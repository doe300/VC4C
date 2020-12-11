
#include "Peephole.h"

#include "../InstructionWalker.h"
#include "../Locals.h"
#include "../Method.h"
#include "../Profiler.h"
#include "../Values.h"
#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "Combiner.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

using RegisterMap = FastMap<const Local*, Register>;

static InstructionWalker lookAhead(InstructionWalker it)
{
    it.nextInMethod();
    while(!it.isEndOfMethod())
    {
        if(it.has() && it->mapsToASMInstruction())
            break;
        it.nextInMethod();
    }
    return it;
}

static InstructionWalker lookAheadInBlock(InstructionWalker it)
{
    it.nextInBlock();
    while(!it.isEndOfBlock())
    {
        if(it.has() && it->mapsToASMInstruction())
            break;
        it.nextInBlock();
    }
    return it;
}

static InstructionWalker lookBackInBlock(InstructionWalker it)
{
    it.previousInBlock();
    while(!it.isEndOfBlock() && !it.isStartOfBlock())
    {
        if(it.has() && it->mapsToASMInstruction())
            break;
        it.previousInBlock();
    }
    return it;
}

static InstructionWalker findSingleReader(InstructionWalker it, InstructionWalker endIt, const Local* loc,
    const RegisterMap& registerMap, const Register& conflictingRegister)
{
    auto locIt = registerMap.find(loc);
    if(locIt == registerMap.end())
        // not found, abort
        return endIt;

    while(!(it = lookAhead(it)).isEndOfBlock())
    {
        if(it->writesLocal(loc))
            return endIt;
        if(it->readsLocal(loc))
            return it;
        bool readsRegister = false;
        bool isAborted = false;
        it.forAllInstructions([&](const intermediate::IntermediateInstruction& inst) {
            for(const auto& arg : inst.getArguments())
            {
                if(auto reg = arg.checkRegister())
                    readsRegister = readsRegister || *reg == locIt->second;
                else if(auto loc = arg.checkLocal())
                {
                    auto argLocIt = registerMap.find(loc);
                    if(argLocIt == registerMap.end())
                        // local not mapped to register -> unknown state -> abort
                        isAborted = true;
                    else
                        readsRegister = readsRegister || argLocIt->second == locIt->second;
                }
            }
            if(inst.writesRegister(conflictingRegister))
                // overwrites input register -> abort
                isAborted = true;
            if(auto outLoc = inst.checkOutputLocal())
            {
                auto outIt = registerMap.find(outLoc);
                if(outIt == registerMap.end() || outIt->second == conflictingRegister)
                    // overwrites input register -> abort
                    isAborted = true;
            }
        });

        if(readsRegister)
            return isAborted ? endIt : it;
    }
    return endIt;
}

static bool canRemoveInstructionBetween(
    InstructionWalker prevIt, InstructionWalker nextIt, const RegisterMap& registerMap)
{
    if(!prevIt.has() || !nextIt.has() || nextIt.isEndOfMethod())
        return false;

    bool isAborted = false;
    SortedSet<Register> writtenRegisters;
    prevIt.forAllInstructions([&](const intermediate::IntermediateInstruction& inst) {
        if(auto reg = inst.checkOutputRegister())
            writtenRegisters.emplace(*reg);
        else if(auto loc = inst.checkOutputLocal())
        {
            auto locIt = registerMap.find(loc);
            if(locIt == registerMap.end())
                isAborted = true;
            else
                writtenRegisters.emplace(locIt->second);
        }
    });

    if(isAborted)
        return false;

    SortedSet<Register> readRegisters;
    nextIt.forAllInstructions([&](const intermediate::IntermediateInstruction& inst) {
        for(const auto& arg : inst.getArguments())
        {
            if(auto reg = arg.checkRegister())
                readRegisters.emplace(*reg);
            else if(auto loc = arg.checkLocal())
            {
                auto locIt = registerMap.find(loc);
                if(locIt == registerMap.end())
                    isAborted = true;
                else
                    readRegisters.emplace(locIt->second);
            }
        }
    });

    if(isAborted)
        return false;

    for(const auto& reg : writtenRegisters)
    {
        bool isRead = readRegisters.find(reg) != readRegisters.end();
        if(!isRead && (reg == REG_REPLICATE_ALL || reg == REG_REPLICATE_QUAD))
            isRead = readRegisters.find(REG_ACC5) != readRegisters.end();
        if(isRead && reg.isGeneralPurpose())
            return false;
        if(isRead && (prevIt->hasPackMode() || nextIt->hasUnpackMode()))
            return false;
        if(isRead && nextIt.get<intermediate::VectorRotation>())
            return false;
    }

    return !intermediate::needsDelay(prevIt.get(), nextIt.get());
}

static Optional<Register> getSingleRegister(
    const intermediate::IntermediateInstruction& inst, const RegisterMap& registerMap)
{
    SortedSet<Register> usedRegisters;
    inst.forUsedLocals([&usedRegisters, &registerMap](
                           const Local* loc, LocalUse::Type type, const intermediate::IntermediateInstruction&) {
        auto locIt = registerMap.find(loc);
        if(locIt != registerMap.end())
            usedRegisters.emplace(locIt->second);
        else
            // to be on the safe side, treat "not-mapped" locals as mapping to NOP which disables removal of
            // instruction for now
            usedRegisters.emplace(REG_NOP);
    });
    return usedRegisters.size() == 1 ? *usedRegisters.begin() : Optional<Register>{};
}

static const intermediate::Operation* getSimpleLocalMove(const intermediate::Operation* op)
{
    if(op && (op->op == OP_OR || op->op == OP_V8MIN) && op->isSimpleOperation() && !op->hasConditionalExecution() &&
        op->getSecondArg() == op->getFirstArg() && op->readsLocal() && op->checkOutputLocal())
        return op;
    return nullptr;
}

void optimizations::removeObsoleteInstructions(
    const Module& module, Method& kernel, const Configuration& config, const RegisterMap& registerMap)
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << logging::endl;
        logging::debug() << "Running peephole pass: RemoveInstructions" << logging::endl;
    });

    PROFILE_COUNTER(
        vc4c::profiler::COUNTER_OPTIMIZATION + 7010, "PeepholeRemoveInstructions (before)", kernel.countInstructions());
    PROFILE_START(PeepholeRemoveInstructions);

    auto it = kernel.walkAllInstructions();
    auto lastInstruction = it;
    while(!it.isEndOfMethod())
    {
        if(it.isEndOfBlock() || !it.has() || !it->mapsToASMInstruction())
        {
            it.nextInMethod();
            continue;
        }
        auto nop = it.get<intermediate::Nop>();
        auto combined = it.get<intermediate::CombinedOperation>();
        if(it->hasDecoration(intermediate::InstructionDecorations::MANDATORY_DELAY) &&
            (!nop || nop->type != intermediate::DelayType::WAIT_REGISTER) && !combined)
        {
            // The instruction is mapped, but we just don't touch it. We need to skip this for NOPs otherwise, we cannot
            // remove any of them. Also skip this for combined operations, since we do not completely remove them and
            // therefore do not violate the delay.
            lastInstruction = it;
            it.nextInMethod();
            continue;
        }
        auto nextIt = lookAhead(it);
        if(nop && nop->type == intermediate::DelayType::WAIT_REGISTER && !nop->hasSideEffects() &&
            !nextIt.isEndOfMethod() && nextIt.has() &&
            canRemoveInstructionBetween(lastInstruction, nextIt, registerMap))
        {
            /*
             * This NOP was inserted to split-read-after writes (mostly for longer-living locals, so they can be mapped
             * to physical registers), but the locals are mapped to accumulators, so there is no need for the NOP
             * anymore.
             */
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Remove NOP inserted for register-delay which is not needed between '"
                    << lastInstruction->to_string() << "' and '" << nextIt->to_string() << "': " << it->to_string()
                    << logging::endl);
            it.erase();
            continue;
        }
        auto move = it.get<intermediate::MoveOperation>();
        if(move && move->isSimpleMove() && !move->hasConditionalExecution() && move->checkOutputLocal() &&
            move->getSource().checkLocal())
        {
            auto reg = getSingleRegister(*move, registerMap);
            if(reg && canRemoveInstructionBetween(lastInstruction, nextIt, registerMap))
            {
                // Input and output are mapped to the same register and there is no need for a delay instruction between
                // the previous and next instruction -> remove the move completely
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Removing simple move with input and output mapped to the same register: "
                        << move->to_string() << " (register " << reg->to_string() << ')' << logging::endl);
                it.erase();
                continue;
            }
            auto moveOut = move->checkOutputLocal();
            auto moveIn = move->getSource().checkLocal();
            auto inIt = registerMap.find(moveIn);
            auto outIt = registerMap.find(moveOut);
            if(moveOut && moveIn && moveOut->getSingleWriter() == move &&
                moveOut->countUsers(LocalUse::Type::READER) == 1 && inIt != registerMap.end() &&
                outIt != registerMap.end() && inIt->second.file == outIt->second.file &&
                (outIt->second != REG_ACC5 ||
                    move->hasDecoration(intermediate::InstructionDecorations::IDENTICAL_ELEMENTS)))
            {
                /*
                 * A simple move from local to local where the output is written and read exactly once. Furthermore, the
                 * in/out locals are mapped to registers in the same register file -> where one can be used, the other
                 * one can be used instead -> we can remove the move and directly use its input. Moves to the
                 * replication register are only removed if the input is already a splat value.
                 */
                if(!nextIt.isEndOfMethod() && it.getBasicBlock() == nextIt.getBasicBlock() &&
                    nextIt->readsLocal(moveOut) && canRemoveInstructionBetween(lastInstruction, nextIt, registerMap))
                {
                    /*
                     * Next instruction is the single reader of the move output and there is no need for a delay
                     * between the previous and the next instruction -> we can remove the move and directly use its
                     * input in the next instruction's input. For now only check for both instructions in the same
                     * block, since otherwise we would need to check for register re-usage in case of loops.
                     */
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing simple move with input and output mapped to the same register file and single "
                               "read in next instruction: "
                            << move->to_string() << " (registers " << inIt->second.to_string() << " and "
                            << outIt->second.to_string() << ')' << logging::endl);
                    it.erase();
                    nextIt->replaceLocal(moveOut, moveIn, LocalUse::Type::READER);
                    continue;
                }
                auto readerIt = findSingleReader(it, it.getBasicBlock()->walkEnd(), moveOut, registerMap, inIt->second);
                if(!readerIt.isEndOfBlock() && readerIt->readsLocal(moveOut) && !readerIt->hasUnpackMode() &&
                    !readerIt.get<intermediate::VectorRotation>() && !nextIt.isEndOfMethod() &&
                    canRemoveInstructionBetween(lastInstruction, nextIt, registerMap))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing simple move with input and output mapped to the same register file and single "
                               "read a following instruction: "
                            << move->to_string() << " (registers " << inIt->second.to_string() << " and "
                            << outIt->second.to_string() << ')' << logging::endl);
                    it.erase();
                    readerIt->replaceLocal(moveOut, moveIn, LocalUse::Type::READER);
                    continue;
                }
            }
        }
        if(combined)
        {
            const intermediate::Operation* simpleMoveOp = nullptr;
            if(auto first = getSimpleLocalMove(combined->getFirstOp()))
                simpleMoveOp = first;
            else if(auto second = getSimpleLocalMove(combined->getSecondOp()))
                simpleMoveOp = second;
            if(auto reg = simpleMoveOp ? getSingleRegister(*simpleMoveOp, registerMap) : Optional<Register>{})
            {
                /*
                 * The combined operations contains a simple move where the input and output register is the same -> we
                 * can remove this move.
                 * This does not directly benefit us performance-wise, but allows for more instructions to be combined
                 * in a successive peephole-combination optimization step.
                 */
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Removing simple move with input and output mapped to the same register '"
                        << simpleMoveOp->to_string() << "' (register " << reg->to_string()
                        << ") as part of a combined instruction: " << combined->to_string() << logging::endl);
                if(combined->getFirstOp() == simpleMoveOp)
                    it.reset(combined->splitUp().second.release()->copyExtrasFrom(combined));
                else if(combined->getSecondOp() == simpleMoveOp)
                    it.reset(combined->splitUp().first.release()->copyExtrasFrom(combined));
            }
        }
        lastInstruction = it;
        it.nextInMethod();
    }

    PROFILE_END(PeepholeRemoveInstructions);
    PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_OPTIMIZATION + 7011, "PeepholeRemoveInstructions (after)",
        kernel.countInstructions(), vc4c::profiler::COUNTER_OPTIMIZATION + 7010);
}

void optimizations::combineRegisterMappedOperations(const Module& module, Method& kernel, const Configuration& config,
    const FastMap<const Local*, Register>& registerMap)
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << logging::endl;
        logging::debug() << "Running peephole pass: CombineInstructions" << logging::endl;
    });

    PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + 7020, "PeepholeCombineInstructions (before)",
        kernel.countInstructions());
    PROFILE_START(PeepholeCombineInstructions);

    for(auto& block : kernel)
    {
        auto it = block.walk();
        auto nextIt = lookAheadInBlock(it);
        while(!it.isEndOfBlock() && !nextIt.isEndOfBlock())
        {
            if(!it.has() || !it->mapsToASMInstruction() || !nextIt.has() || !nextIt->mapsToASMInstruction())
            {
                it.nextInBlock();
                nextIt = lookAheadInBlock(it);
                continue;
            }

            bool conditionsMet = true;
            auto move = it.get<intermediate::MoveOperation>();
            auto op = it.get<intermediate::Operation>();
            auto nextMove = nextIt.get<intermediate::MoveOperation>();
            auto nextOp = nextIt.get<intermediate::Operation>();
            if((!move && !op) || (!nextMove && !nextOp))
                conditionsMet = false;

            if(conditionsMet)
            {
                MergeConditionData data{&registerMap};
                conditionsMet = std::all_of(MERGE_CONDITIONS.begin(), MERGE_CONDITIONS.end(),
                    [op, nextOp, move, nextMove, &data](
                        const MergeCondition& cond) -> bool { return cond(op, nextOp, move, nextMove, data); });
            }
            if(conditionsMet)
            {
                auto checkIt = lookAheadInBlock(nextIt);
                if(checkIt.isEndOfBlock() || !checkIt.has() || !canRemoveInstructionBetween(it, checkIt, registerMap))
                    conditionsMet = false;
            }
            if(conditionsMet)
            {
                auto checkIt = lookBackInBlock(it);
                if(checkIt.isEndOfBlock() || !checkIt.has() ||
                    !canRemoveInstructionBetween(checkIt, nextIt, registerMap))
                    conditionsMet = false;
            }

            if(conditionsMet)
                combineOperationsInner(it, nextIt);

            it.nextInBlock();
            nextIt = lookAheadInBlock(it);
        }
    }
    PROFILE_END(PeepholeCombineInstructions);
    PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_OPTIMIZATION + 7021, "PeepholeCombineInstructions (after)",
        kernel.countInstructions(), vc4c::profiler::COUNTER_OPTIMIZATION + 7020);
}
