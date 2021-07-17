/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Combiner.h"

#include "../InstructionWalker.h"
#include "../analysis/MemoryAnalysis.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "Eliminator.h"
#include "log.h"

#include <algorithm>
#include <cstdlib>
#include <memory>

// TODO combine y = (x >> n) << n with and
// same for y = (x << n) >> n (at least of n constant)
// TODO for "exact" shifts, y == (y >> n) << n!
// need to find a case of shifts first, where the exact flag is set!

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

// Taken from https://stackoverflow.com/questions/2835469/how-to-perform-rotate-shift-in-c?noredirect=1&lq=1
constexpr static uint32_t rotate_left_halfword(uint32_t value, uint8_t shift) noexcept
{
    return (value << shift) | (value >> (16 - shift));
}

static_assert((rotate_left_halfword(0x12340000 >> 16, 4) << 16) == 0x23410000, "");

static const std::string combineLoadLiteralsThreshold = "combine-load-threshold";

bool optimizations::simplifyBranches(const Module& module, Method& method, const Configuration& config)
{
    bool hasChanged = false;
    for(auto it = method.walkAllInstructions(); !it.isEndOfBlock(); it.nextInMethod())
    {
        if(Branch* thisBranch = it.get<Branch>())
        {
            bool skippedOtherBranch = false;
            // eliminates branches to the next instruction to save up to 4 instructions (1 branch + 3 NOP)
            // eliminate branches to the next instruction, such branches are e.g. introduced by method-inlining
            auto nextIt = it.copy().nextInMethod();
            if(!nextIt.isEndOfMethod())
            {
                auto otherBranch = nextIt.get<Branch>();
                if(otherBranch && otherBranch->branchCondition.isInversionOf(thisBranch->branchCondition))
                {
                    // if the following branch has the inverted condition (either-or-branch), it can be skipped, to
                    // allow removing of the original branch to the following blocks anyway
                    nextIt.nextInMethod();
                    // but we cannot allow the removal of duplicate branches to the same target, since the branch would
                    // not have fallen-through for branch condition not met, since we have another branch right here
                    // that handles that case
                    skippedOtherBranch = true;
                }
            }
            if(!nextIt.isEndOfMethod())
            {
                intermediate::BranchLabel* label = nextIt.get<intermediate::BranchLabel>();
                if(label != nullptr && label->getLabel() == thisBranch->getSingleTargetLabel())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing branch to next instruction: " << thisBranch->to_string() << logging::endl);
                    it = it.erase();
                    // don't skip next instruction
                    it.previousInMethod();
                    // skip second part below
                    continue;
                }
            }
            // skip all following labels
            while(!nextIt.isEndOfMethod() && nextIt.get<BranchLabel>())
                nextIt.nextInMethod();
            if(nextIt.isEndOfMethod())
                return hasChanged;
            if(Branch* nextBranch = nextIt.get<Branch>())
            {
                if(skippedOtherBranch || thisBranch->getSingleTargetLabel() != nextBranch->getSingleTargetLabel())
                    continue;
                // for now, only remove unconditional branches
                if(!thisBranch->isUnconditional() || !nextBranch->isUnconditional())
                    continue;
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Removing duplicate branch to same target: " << thisBranch->to_string() << logging::endl);
                it = it.erase();
                // don't skip next instruction
                it.previousInMethod();
                hasChanged = true;
            }
        }
    }
    return hasChanged;
}

bool MergeConditionData::isPeepholeRun() const noexcept
{
    return registerMap;
}

static bool isSimpleMoveOfZero(const IntermediateInstruction* inst)
{
    return inst && inst->isSimpleMove() && (inst->getMoveSource() & &Value::getLiteralValue) == 0_lit;
}

const std::vector<MergeCondition> optimizations::MERGE_CONDITIONS = {
    // check both instructions are actually mapped to machine code
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        if(firstOp != nullptr && !firstOp->mapsToASMInstruction())
            return false;
        if(firstMove != nullptr && !firstMove->mapsToASMInstruction())
            return false;
        if(secondOp != nullptr && !secondOp->mapsToASMInstruction())
            return false;
        if(secondMove != nullptr && !secondMove->mapsToASMInstruction())
            return false;
        return true;
    },
    // check neither instruction is a vector rotation
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        return (firstMove == nullptr || dynamic_cast<VectorRotation*>(firstMove) == nullptr) &&
            (secondMove == nullptr || dynamic_cast<VectorRotation*>(secondMove) == nullptr);
    },
    // check at most one instruction is a mandatory delay
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        unsigned numDelays = 0;
        if((check<IntermediateInstruction>(firstOp) | check<IntermediateInstruction>(firstMove))
                ->hasDecoration(InstructionDecorations::MANDATORY_DELAY))
            ++numDelays;
        if((check<IntermediateInstruction>(secondOp) | check<IntermediateInstruction>(secondMove))
                ->hasDecoration(InstructionDecorations::MANDATORY_DELAY))
            ++numDelays;
        return numDelays <= 1;
    },
    // check both instructions use different ALUs
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        if(firstOp != nullptr && secondOp != nullptr)
        {
            if((firstOp->op.runsOnAddALU() && firstOp->op.runsOnMulALU()) ||
                (secondOp->op.runsOnAddALU() && secondOp->op.runsOnMulALU()))
                // both instructions can be mapped to use different ALUs
                return true;
            if(firstOp->op.runsOnAddALU() && secondOp->op.runsOnAddALU())
                return false;
            else if(firstOp->op.runsOnMulALU() && secondOp->op.runsOnMulALU())
                return false;
        }
        return true;
    },
    // check reads from or writes to special registers
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        if(firstOp != nullptr && (firstOp->checkOutputRegister() || firstOp->readsRegister()))
            return false;
        else if(firstMove != nullptr && (firstMove->checkOutputRegister() || firstMove->readsRegister()))
            return false;
        else if(secondOp != nullptr && (secondOp->checkOutputRegister() || secondOp->readsRegister()))
            return false;
        else if(secondMove != nullptr && (secondMove->checkOutputRegister() || secondMove->readsRegister()))
            return false;
        return true;
    },
    // check second operation using the result of the first
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        Value outFirst(UNDEFINED_VALUE);
        if(firstOp != nullptr && firstOp->getOutput())
            outFirst = firstOp->getOutput().value();
        else if(firstMove != nullptr && firstMove->getOutput())
            outFirst = firstMove->getOutput().value();
        if(outFirst.type != TYPE_UNKNOWN)
        {
            if(secondOp != nullptr)
            {
                if(secondOp->getFirstArg() == outFirst)
                    return false;
                else if(secondOp->getSecondArg() == outFirst)
                    return false;
            }
            if(secondMove != nullptr)
            {
                if(secondMove->getSource() == outFirst)
                    return false;
            }
        }
        return true;
    },
    // check operations use same output and do not have inverted conditions. If the first write is unconditional and has
    // no side-effects, make it conditional on the inversion of the second write.
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        Value outFirst(UNDEFINED_VALUE);
        ConditionCode condFirst = COND_ALWAYS;
        bool firstSideEffects = false;
        if(firstOp != nullptr && firstOp->getOutput())
        {
            outFirst = firstOp->getOutput().value();
            condFirst = firstOp->getCondition();
            firstSideEffects = firstOp->hasSideEffects();
        }
        else if(firstMove != nullptr && firstMove->getOutput())
        {
            outFirst = firstMove->getOutput().value();
            condFirst = firstMove->getCondition();
            firstSideEffects = firstMove->hasSideEffects();
        }
        Value outSecond(UNDEFINED_VALUE);
        ConditionCode condSecond = COND_ALWAYS;
        if(secondOp != nullptr && secondOp->getOutput())
        {
            outSecond = secondOp->getOutput().value();
            condSecond = secondOp->getCondition();
        }
        else if(secondMove != nullptr && secondMove->getOutput())
        {
            outSecond = secondMove->getOutput().value();
            condSecond = secondMove->getCondition();
        }
        if(outFirst == outSecond || (outFirst.checkLocal() && outSecond.checkLocal() == outFirst.local()))
        {
            if(!condSecond.isInversionOf(condFirst))
            {
                if(condFirst == COND_ALWAYS && !firstSideEffects)
                {
                    // first write is unconditional and second conditional -> rewrite first write to be conditional on
                    // inversion of second write, so we can combine them.
                    (firstOp ? dynamic_cast<ExtendedInstruction*>(firstOp) : firstMove)
                        ->setCondition(condSecond.invert());
                }
                else
                    return false;
            }
        }
        return true;
    },
    // check operations write to registers on same physical register file ("peephole mode" only)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        if(!data.isPeepholeRun())
            return true;

        auto firstInst = firstOp ? dynamic_cast<UnpackingInstruction*>(firstOp) : firstMove;
        auto secondInst = secondOp ? dynamic_cast<UnpackingInstruction*>(secondOp) : secondMove;

        auto firstOutReg = firstInst->checkOutputRegister();
        if(auto loc = firstInst->checkOutputLocal())
        {
            auto locIt = data.registerMap->find(loc);
            if(locIt == data.registerMap->end())
                return false;
            firstOutReg = locIt->second;
        }

        auto secondOutReg = secondInst->checkOutputRegister();
        if(auto loc = secondInst->checkOutputLocal())
        {
            auto locIt = data.registerMap->find(loc);
            if(locIt == data.registerMap->end())
                return false;
            secondOutReg = locIt->second;
        }

        if(!firstOutReg || !secondOutReg ||
            (firstOutReg->file == secondOutReg->file && firstOutReg->isGeneralPurpose() &&
                secondOutReg->isGeneralPurpose()))
            // fail if we write to two physical registers on the same file, even if it is the same register
            return false;

        if(firstOutReg->file == RegisterFile::ACCUMULATOR && secondOutReg->file == RegisterFile::ACCUMULATOR &&
            *firstOutReg == *secondOutReg && !firstInst->getCondition().isInversionOf(secondInst->getCondition()))
            // fail if we write the same accumulator, unless we have inverted conditions for writing
            return false;

        return true;
    },
    // check first operation sets flags and second operation depends on them
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        SetFlag setsFlags = SetFlag::DONT_SET;
        bool usesFlags = false;
        ConditionCode firstCond = COND_ALWAYS;
        ConditionCode secondCond = COND_ALWAYS;
        if(firstOp != nullptr)
        {
            setsFlags = firstOp->getFlags();
            firstCond = firstOp->getCondition();
        }
        else if(firstMove != nullptr)
        {
            setsFlags = firstMove->getFlags();
            firstCond = firstMove->getCondition();
        }
        if(secondOp != nullptr)
        {
            usesFlags = secondOp->hasConditionalExecution();
            secondCond = secondOp->getCondition();
        }
        else if(secondMove != nullptr)
        {
            usesFlags = secondMove->hasConditionalExecution();
            secondCond = secondMove->getCondition();
        }
        if(setsFlags == SetFlag::SET_FLAGS && usesFlags)
        {
            // special case, e.g. mov.ifz x, bool true, mov.ifnz x, bool false
            if(firstCond != COND_ALWAYS && secondCond != COND_ALWAYS)
            {
                return firstCond.isInversionOf(secondCond);
            }
        }
        return setsFlags == SetFlag::DONT_SET || !usesFlags;
    },
    // check MUL ALU sets flags (flags would be set by ADD ALU)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        ConditionCode firstCond = firstOp ? firstOp->getCondition() : firstMove->getCondition();
        ConditionCode secondCond = secondOp ? secondOp->getCondition() : secondMove->getCondition();
        if(firstCond.isInversionOf(secondCond))
        {
            // if they have inverted conditions, the ADD ALU can't set the flags, the MUL ALU is supposed to
            return true;
        }
        // XXX handle v8adds, can be on ADD and MUL ALU. Would need to make sure, flag-setting instruction is on ADD ALU
        if(firstOp != nullptr && firstOp->op.runsOnMulALU() && firstOp->doesSetFlag())
            return false;
        else if(secondOp != nullptr && secondOp->op.runsOnMulALU() && secondOp->doesSetFlag())
            return false;
        return true;
    },
    // check maximum 1 literal value is used (both can use the same literal value)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        // TODO This could be optimized, if we known which literals will be extracted to extra load instructions.
        // XXX Need then to check only for literals which are converted to small immediate values in-place.
        Optional<Literal> literal;
        // If one literal user is a move of 0, we can rewrite it with a XOR of one of the arguments of the other
        // instruction
        MoveOperation* simpleMoveOfZero = nullptr;
        if(firstOp != nullptr)
        {
            if(auto lit = firstOp->getFirstArg().getLiteralValue())
                literal = lit;
            if(auto lit = (firstOp->getSecondArg() & &Value::getLiteralValue))
                literal = lit;
        }
        if(firstMove != nullptr)
        {
            if(auto lit = firstMove->getSource().getLiteralValue())
                literal = lit;
            if(isSimpleMoveOfZero(firstMove))
                simpleMoveOfZero = firstMove;
        }
        if(literal)
        {
            if(secondOp != nullptr)
            {
                if(secondOp->getFirstArg().getLiteralValue() && !secondOp->getFirstArg().hasLiteral(*literal))
                {
                    if(simpleMoveOfZero)
                        literal = secondOp->getFirstArg().getLiteralValue();
                    else
                        return false;
                }
                if((secondOp->getSecondArg() & &Value::getLiteralValue) &&
                    !secondOp->assertArgument(1).hasLiteral(*literal))
                {
                    if(simpleMoveOfZero)
                        literal = secondOp->getSecondArg() & &Value::getLiteralValue;
                    else
                        return false;
                }
            }
            if(secondMove != nullptr)
            {
                if(secondMove->getSource().getLiteralValue() && !secondMove->getSource().hasLiteral(*literal))
                {
                    if(simpleMoveOfZero)
                        literal = secondMove->getSource().getLiteralValue();
                    else if(isSimpleMoveOfZero(secondMove))
                        simpleMoveOfZero = secondMove;
                    else
                        return false;
                }
            }
        }
        else if(isSimpleMoveOfZero(secondMove))
            simpleMoveOfZero = secondMove;

        if(simpleMoveOfZero && literal && literal->unsignedInt() != 0)
        {
            if(data.isPeepholeRun())
                // don't do the rewrite for the "peephole mode" for now, to not make it too complex, especially since
                // in a further check below this is also not regarded
                return false;
            // XOR needs to be on add ALU, so check whether other instruction can be on mul ALU
            if(simpleMoveOfZero == firstMove && ((secondOp && secondOp->op.runsOnMulALU()) || secondMove))
                data.rewriteSimpleMoveOfZero = firstMove;
            else if(simpleMoveOfZero == secondMove && ((firstOp && firstOp->op.runsOnMulALU()) || firstMove))
                data.rewriteSimpleMoveOfZero = secondMove;
            else
                return false;
        }

        return true;
    },
    // check a maximum of 2 inputs are read from ("normal" mode)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        if(data.isPeepholeRun())
            // skip this particular check, we have the "peephole mode" version just below
            return true;
        std::pair<Value, Value> inputs = std::make_pair(UNDEFINED_VALUE, UNDEFINED_VALUE);
        if(firstOp != nullptr)
        {
            inputs.first = firstOp->getFirstArg();
            if(auto arg1 = firstOp->getSecondArg())
                inputs.second = *arg1;
        }
        if(firstMove != nullptr && data.rewriteSimpleMoveOfZero != firstMove)
            inputs.first = firstMove->getSource();
        if(secondOp != nullptr)
        {
            if(secondOp->getFirstArg() != inputs.first)
            {
                if(inputs.second.type == TYPE_UNKNOWN)
                    inputs.second = secondOp->getFirstArg();
                else if(secondOp->getFirstArg() != inputs.second)
                    return false;
            }
            if(secondOp->getSecondArg() && secondOp->assertArgument(1) != inputs.first)
            {
                if(inputs.second.type == TYPE_UNKNOWN)
                    inputs.second = secondOp->assertArgument(1);
                else if(secondOp->assertArgument(1) != inputs.second)
                    return false;
            }
        }
        if(secondMove != nullptr && data.rewriteSimpleMoveOfZero != secondMove)
        {
            if(secondMove->getSource() != inputs.first)
            {
                if(inputs.second.type == TYPE_UNKNOWN)
                    inputs.second = secondMove->getSource();
                else if(secondMove->getSource() != inputs.second)
                    return false;
            }
        }
        return true;
    },
    // check a maximum of 4 inputs are read from ("pephole mode")
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        if(!data.isPeepholeRun())
            // skip this particular check, we have the "normal" version just above
            return true;

        /*
         * For "peephole mode", we already know the registers a local is mapped to and can therefore be more
         * accurate with this check. More precisely, we need to check that:
         * - at most 4 distinct inputs are used
         * - at most a single input is located at physical register file A or B respectively
         * - at most one small immediate value or register on physical register file B is read
         */
        auto firstInst = firstOp ? dynamic_cast<UnpackingInstruction*>(firstOp) : firstMove;
        auto secondInst = secondOp ? dynamic_cast<UnpackingInstruction*>(secondOp) : secondMove;
        SortedSet<Register> allRegisters;
        FastSet<SmallImmediate> allImmediates;
        bool hasError = false;
        auto extractRegister = [&](const Value& val) {
            if(auto reg = val.checkRegister())
                allRegisters.emplace(*reg);
            else if(auto imm = val.checkImmediate())
                allImmediates.emplace(*imm);
            else if(auto loc = val.checkLocal())
            {
                auto locIt = data.registerMap->find(loc);
                if(locIt != data.registerMap->end())
                    allRegisters.emplace(locIt->second);
                else
                    hasError = true;
            }
            else
                hasError = true;
        };
        for(const auto& arg : firstInst->getArguments())
            extractRegister(arg);
        for(const auto& arg : secondInst->getArguments())
            extractRegister(arg);

        if(hasError || allImmediates.size() > 1 || (allImmediates.size() + allRegisters.size()) > 4)
            return false;
        if(std::count_if(allRegisters.begin(), allRegisters.end(),
               [](const Register& reg) -> bool { return reg.file == RegisterFile::PHYSICAL_A; }) > 1)
            return false;
        if(allImmediates.size() +
                static_cast<std::size_t>(std::count_if(allRegisters.begin(), allRegisters.end(),
                    [](const Register& reg) -> bool { return reg.file == RegisterFile::PHYSICAL_B; })) >
            1)
            return false;

        return true;
    },
    // check at most 1 signal (including IMMEDIATE) is set, same for Un-/Pack
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        Signaling firstSignal = SIGNAL_NONE;
        Signaling secondSignal = SIGNAL_NONE;

        Unpack firstUnpack = UNPACK_NOP;
        Unpack secondUnpack = UNPACK_NOP;

        Pack firstPack = PACK_NOP;
        Pack secondPack = PACK_NOP;
        ConditionCode firstCond = firstOp ? firstOp->getCondition() : firstMove->getCondition();
        ConditionCode secondCond = secondOp ? secondOp->getCondition() : secondMove->getCondition();
        bool invertedConditions = firstCond.isInversionOf(secondCond);
        if(firstOp != nullptr)
        {
            firstSignal = firstOp->getSignal();
            if(firstOp->getFirstArg().getLiteralValue() || (firstOp->getSecondArg() & &Value::getLiteralValue))
                firstSignal = SIGNAL_ALU_IMMEDIATE;
            firstPack = firstOp->getPackMode();
            firstUnpack = firstOp->getUnpackMode();
        }
        if(firstMove != nullptr)
        {
            firstSignal = firstMove->getSignal();
            if(firstMove->getSource().getLiteralValue() && data.rewriteSimpleMoveOfZero != firstMove)
                firstSignal = SIGNAL_ALU_IMMEDIATE;
            firstPack = firstMove->getPackMode();
            firstUnpack = firstMove->getUnpackMode();
        }
        if(secondOp != nullptr)
        {
            secondSignal = secondOp->getSignal();
            if(secondOp->getFirstArg().getLiteralValue() || (secondOp->getSecondArg() & &Value::getLiteralValue))
                secondSignal = SIGNAL_ALU_IMMEDIATE;
            secondPack = secondOp->getPackMode();
            secondUnpack = secondOp->getUnpackMode();
        }
        if(secondMove != nullptr)
        {
            secondSignal = secondMove->getSignal();
            if(secondMove->getSource().getLiteralValue() && data.rewriteSimpleMoveOfZero != secondMove)
                secondSignal = SIGNAL_ALU_IMMEDIATE;
            secondPack = secondMove->getPackMode();
            secondUnpack = secondMove->getUnpackMode();
        }
        // only one signal can be fired (independent of ALU conditions)
        if(firstSignal != SIGNAL_NONE && secondSignal != SIGNAL_NONE && firstSignal != secondSignal)
            return false;
        // both instructions need to have the SAME pack-mode (including NOP), unless they have inverted conditions
        if(firstPack != secondPack && !invertedConditions)
            return false;
        // if both have a pack-mode set (excluding NOP), it must be the same, since we can only set one pack-mode per
        // (combined) instruction
        if(firstPack.hasEffect() && secondPack.hasEffect() && firstPack != secondPack)
            return false;
        // can only unpack from reg-file A -> 1 input
        // XXX since we cannot know which input is on register-file A, for now we don't combine unpack modes, unless
        // there is only one input and both use the same unpack mode
        if((firstUnpack.hasEffect() || secondUnpack.hasEffect()) &&
            !(firstUnpack == secondUnpack && firstMove && secondMove &&
                firstMove->getSource() == secondMove->getSource()))
            return false;
        // the pack-mode must match the ALU, can only apply mul pack modes if the other operation can execute on add ALU
        // and vice versa
        if(firstPack.hasEffect() && firstPack.supportsMulALU() != (secondMove || secondOp->op.runsOnAddALU()))
            return false;
        if(secondPack.hasEffect() && secondPack.supportsMulALU() != (firstMove || firstOp->op.runsOnAddALU()))
            return false;
        return true;
    },
    // check not two different boolean values which both are used in conditional jump
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove,
        MergeConditionData& data) -> bool {
        // since boolean values are combined with ELEM_NUMBER (reg-file A) before conditional branch, they cannot be on
        // reg-file A  combining two of those can cause register-association errors
        // TODO find a way to fix this
        if(((firstOp != nullptr && firstOp->checkOutputLocal() && firstOp->getOutput()->type == TYPE_BOOL) ||
               (firstMove != nullptr && firstMove->checkOutputLocal() && firstMove->getOutput()->type == TYPE_BOOL)))
        {
            if(((secondOp != nullptr && secondOp->checkOutputLocal() && secondOp->getOutput()->type == TYPE_BOOL) ||
                   (secondMove != nullptr && secondMove->checkOutputLocal() &&
                       secondMove->getOutput()->type == TYPE_BOOL)))
            {
                // this is not an issue, if the output of the two instructions is the same in which case it can be
                // assigned to register-file B
                return (firstOp != nullptr ? firstOp->getOutput() : firstMove->getOutput()).value() ==
                    (secondOp != nullptr ? secondOp->getOutput() : secondMove->getOutput()).value();
            }
        }
        return true;
    }};

bool optimizations::combineOperationsInner(InstructionWalker it, InstructionWalker nextIt)
{
    auto move = it.get<intermediate::MoveOperation>();
    auto op = it.get<intermediate::Operation>();
    auto nextMove = nextIt.get<intermediate::MoveOperation>();
    auto nextOp = nextIt.get<intermediate::Operation>();

    // move supports both ADD and MUL ALU
    // if merge, make "move" to other op-code or x x / v8max x x
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Merging instructions " << it->to_string() << " and " << nextIt->to_string() << logging::endl);
    if(op && nextOp)
    {
        it.reset(std::make_unique<CombinedOperation>(
            staticPointerCast<Operation>(it.release()), staticPointerCast<Operation>(nextIt.release())));
        nextIt.erase();
    }
    else if(op && nextMove)
    {
        if(auto newMove = nextMove->combineWith(op->op))
        {
            newMove->copyExtrasFrom(*nextMove);
            it.reset(
                std::make_unique<CombinedOperation>(staticPointerCast<Operation>(it.release()), std::move(newMove)));
            nextIt.erase();
        }
        else
        {
            logging::warn() << "Error combining move-operation '" << nextMove->to_string()
                            << "' with: " << op->to_string() << logging::endl;
            return false;
        }
    }
    else if(move && nextOp)
    {
        if(auto newMove = move->combineWith(nextOp->op))
        {
            newMove->copyExtrasFrom(*move);
            it.reset(std::make_unique<CombinedOperation>(
                std::move(newMove), staticPointerCast<Operation>(nextIt.release())));
            nextIt.erase();
        }
        else
        {
            logging::warn() << "Error combining move-operation '" << move->to_string()
                            << "' with: " << nextOp->to_string() << logging::endl;
            return false;
        }
    }
    else if(move && nextMove)
    {
        bool firstOnMul = (move->hasPackMode() && move->getPackMode().supportsMulALU()) ||
            (nextMove->hasPackMode() && !nextMove->getPackMode().supportsMulALU()) || nextMove->doesSetFlag();
        auto newMove0 = move->combineWith(firstOnMul ? OP_ADD : OP_MUL24);
        auto newMove1 = nextMove->combineWith(firstOnMul ? OP_MUL24 : OP_ADD);
        if(newMove0 && newMove1)
        {
            newMove0->copyExtrasFrom(*move);
            newMove1->copyExtrasFrom(*nextMove);
            it.reset(std::make_unique<CombinedOperation>(std::move(newMove0), std::move(newMove1)));
            nextIt.erase();
        }
        else
        {
            logging::warn() << "Error combining move-operation '" << move->to_string()
                            << "' with: " << nextMove->to_string() << logging::endl;
            return false;
        }
    }
    else
        throw CompilationError(
            CompilationStep::OPTIMIZER, "Unhandled combination, type", (it->to_string() + ", ") + nextIt->to_string());

    if(CombinedOperation* comb = it.get<CombinedOperation>())
    {
        // move instruction usable on both ALUs to the free ALU
        if(comb->getFirstOp()->op.runsOnAddALU() && comb->getFirstOp()->op.runsOnMulALU())
        {
            OpCode code = comb->getFirstOp()->op;
            if(comb->getSecondOp()->op.runsOnAddALU())
                code.opAdd = 0;
            else // by default (e.g. both run on both ALUs), map to ADD ALU
                code.opMul = 0;
            comb->getFirstOp()->op = code;
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Fixing operation available on both ALUs to " << (code.opAdd == 0 ? "MUL" : "ADD")
                    << " ALU: " << comb->getFirstOp()->to_string() << logging::endl);
        }
        if(comb->getSecondOp()->op.runsOnAddALU() && comb->getSecondOp()->op.runsOnMulALU())
        {
            OpCode code = comb->getSecondOp()->op;
            if(comb->getFirstOp()->op.runsOnMulALU())
                code.opMul = 0;
            else // by default (e.g. both run on both ALUs), map to MUL ALU
                code.opAdd = 0;
            comb->getSecondOp()->op = code;
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Fixing operation available on both ALUs to " << (code.opAdd == 0 ? "MUL" : "ADD")
                    << " ALU: " << comb->getSecondOp()->to_string() << logging::endl);
        }

        // mark combined instruction as delay, if one of the combined instructions is
        if(comb->getFirstOp()->hasDecoration(InstructionDecorations::MANDATORY_DELAY) ||
            comb->getSecondOp()->hasDecoration(InstructionDecorations::MANDATORY_DELAY))
            comb->addDecorations(InstructionDecorations::MANDATORY_DELAY);
        return true;
    }

    return false;
}

bool optimizations::combineOperations(const Module& module, Method& method, const Configuration& config)
{
    // TODO can combine operation x and y if y is something like (result of x & 0xFF/0xFFFF) -> pack-mode
    bool hasChanged = false;
    for(BasicBlock& bb : method)
    {
        auto it = bb.walk();
        while(!it.isEndOfBlock() && !it.copy().nextInBlock().isEndOfBlock())
        {
            MoveOperation* move = it.get<MoveOperation>();
            if(move != nullptr)
            {
                //- remove moves where getSource() is not written to afterwards -> set destination = getSource()
                // rewrite all following instructions using the original destination
            }
            Operation* op = it.get<Operation>();
            if(op != nullptr || move != nullptr)
            {
                IntermediateInstruction* instr = it.get();
                auto nextIt = it.copy().nextInBlock();
                Operation* nextOp = nextIt.get<Operation>();
                MoveOperation* nextMove = nextIt.get<MoveOperation>();
                if(nextOp != nullptr || nextMove != nullptr)
                {
                    IntermediateInstruction* nextInstr = nextIt.get();
                    //- combine add/mul instructions, where:
                    /*
                     * - combined instructions use at least 2 accumulators, or share getSource()-registers, so that only
                     * 2 getSource() registers are required
                     * - the instructions do not depend one-on-another (e.g. out of first is in of second)
                     * - both instructions write to different locals (or to same local and have inverted conditions)
                     * - MUL instruction does not set flags (otherwise flags would be applied for ADD output)
                     * - only one instruction uses a literal (or the literal is the same)
                     * - both set signals (including immediate ALU operation)
                     * For now, may be removed (with exceptions):
                     * - neither of these instructions read/write from special registers
                     *   otherwise this could cause reading two UNIFORMS at once / writing VPM/VPM_ADDR at once
                     */
                    // TODO a written-to register MUST not be read in the next instruction (check instruction
                    // before/after combined) (unless within local range)
                    MergeConditionData data{};
                    bool conditionsMet = std::all_of(MERGE_CONDITIONS.begin(), MERGE_CONDITIONS.end(),
                        [op, nextOp, move, nextMove, &data](
                            const MergeCondition& cond) -> bool { return cond(op, nextOp, move, nextMove, data); });
                    if(instr->checkOutputLocal() && nextInstr->checkOutputLocal())
                    {
                        // extra check, only combine writes to the same local, if local is only used within the next
                        // instruction  this is required, since we cannot write to a physical register from both ALUs,
                        // so the local needs to be on an accumulator
                        if(instr->getOutput()->local() == nextInstr->getOutput()->local() &&
                            !nextIt.getBasicBlock()->isLocallyLimited(
                                nextIt, instr->getOutput()->local(), config.additionalOptions.accumulatorThreshold))
                            conditionsMet = false;
                    }
                    if(instr->checkOutputLocal() || nextInstr->checkOutputLocal())
                    {
                        // also check that if the next instruction is a vector rotation, neither of the locals is being
                        // rotated there  since vector rotations can't rotate vectors which have been written in the
                        // instruction directly preceding it (true for both full-vector and per-quad rotations)
                        auto checkIt = nextIt.copy().nextInBlock();
                        if(!checkIt.isEndOfBlock() && checkIt->getVectorRotation())
                        {
                            if(instr->checkOutputLocal() && checkIt->readsLocal(instr->checkOutputLocal()))
                                conditionsMet = false;
                            if(nextInstr->checkOutputLocal() && checkIt->readsLocal(nextInstr->checkOutputLocal()))
                                conditionsMet = false;
                        }
                        // the next instruction MUST NOT unpack a value written to in one of the combined instructions
                        // equally, neither of the combined instructions is allowed to pack a value read in the
                        // following instructions
                        if(!checkIt.isEndOfBlock())
                        {
                            if(checkIt->hasUnpackMode())
                            {
                                if(std::any_of(checkIt->getArguments().begin(), checkIt->getArguments().end(),
                                       [instr, nextInstr](const Value& val) -> bool {
                                           return val.checkLocal() &&
                                               (instr->writesLocal(val.local()) || nextInstr->writesLocal(val.local()));
                                       }))
                                {
                                    conditionsMet = false;
                                }
                            }
                            if(instr->hasPackMode() && instr->checkOutputLocal() &&
                                checkIt->readsLocal(instr->getOutput()->local()))
                                conditionsMet = false;
                            if(nextInstr->hasPackMode() && nextInstr->checkOutputLocal() &&
                                checkIt->readsLocal(nextInstr->getOutput()->local()))
                                conditionsMet = false;
                        }
                        // run previous checks also for the previous (before instr) instruction
                        // this time with inverted checks (since the order is inverted)
                        checkIt = it.copy().previousInBlock();
                        if(!checkIt.isStartOfBlock() && checkIt->checkOutputLocal())
                        {
                            if(checkIt->hasPackMode() &&
                                (instr->readsLocal(checkIt->getOutput()->local()) ||
                                    nextInstr->readsLocal(checkIt->getOutput()->local())))
                                conditionsMet = false;
                            if(instr->hasUnpackMode() && instr->readsLocal(checkIt->getOutput()->local()))
                                conditionsMet = false;
                            if(nextInstr->hasUnpackMode() && nextInstr->readsLocal(checkIt->getOutput()->local()))
                                conditionsMet = false;
                        }
                    }

                    if(conditionsMet && data.rewriteSimpleMoveOfZero)
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Rewriting simple move of zero to XOR of other argument to enable combination: "
                                << data.rewriteSimpleMoveOfZero->to_string() << logging::endl);
                        if(data.rewriteSimpleMoveOfZero == move)
                        {
                            Value arg = nextInstr->assertArgument(0);
                            if(auto litArg = nextInstr->findLiteralArgument())
                                // prefer the literal argument for XOR'ing for easier readability
                                arg = *litArg;
                            op = &it.reset(
                                createWithExtras<Operation>(*move, OP_XOR, move->getOutput().value(), arg, arg));
                            move = nullptr;
                            instr = op;
                        }
                        else if(data.rewriteSimpleMoveOfZero == nextMove)
                        {
                            Value arg = instr->assertArgument(0);
                            if(auto litArg = instr->findLiteralArgument())
                                // prefer the literal argument for XOR'ing for easier readability
                                arg = *litArg;
                            nextOp = &nextIt.reset(createWithExtras<Operation>(
                                *nextMove, OP_XOR, nextMove->getOutput().value(), arg, arg));
                            nextMove = nullptr;
                            nextInstr = nextOp;
                        }
                        else
                            // would be very very weird to run into here
                            conditionsMet = false;
                    }

                    if(conditionsMet && combineOperationsInner(it, nextIt))
                        hasChanged = true;
                }
            }
            it.nextInBlock();
        }
    }

    return hasChanged;
}

static Optional<Literal> getSourceLiteral(InstructionWalker it)
{
    if(it.get<LoadImmediate>() && it.get<LoadImmediate>()->type == LoadType::REPLICATE_INT32)
    {
        return it.get<LoadImmediate>()->getImmediate();
    }
    else if(auto sourceLiteral = it->getMoveSource() & &Value::getLiteralValue)
    {
        // for literal sources, any possible applied rotation has no effect, so we can accept them here
        return sourceLiteral;
    }
    else if(auto op = it.get<Operation>())
    {
        const auto val = op->precalculate(2).first;
        if(val)
            return val->getLiteralValue();
    }
    return {};
}

static Optional<Register> getSourceConstantRegister(InstructionWalker it)
{
    if(it->getVectorRotation())
        // XXX would need to check for same (constant) offset too!
        return {};
    if(it.get<MoveOperation>() && (it->readsRegister(REG_ELEMENT_NUMBER) || it->readsRegister(REG_QPU_NUMBER)))
    {
        return it.get<MoveOperation>()->getSource().reg();
    }
    return {};
}

static bool canReplaceConstantLoad(
    InstructionWalker it, const InstructionWalker start, const InstructionWalker match, std::size_t stepsLeft)
{
    InstructionWalker pos = it.copy();
    // check whether the instruction last loading the same literal is at most ACCUMULATOR_THRESHOLD_HINT instructions
    // before this one
    while(stepsLeft > 0 && !pos.isStartOfBlock() && pos != start)
    {
        if(pos.get() == match.get())
            return true;
        pos.previousInBlock();
        --stepsLeft;
    }
    return false;
}

bool optimizations::combineLoadingConstants(const Module& module, Method& method, const Configuration& config)
{
    std::size_t threshold = config.additionalOptions.combineLoadThreshold;
    bool hasChanged = false;

    for(BasicBlock& block : method)
    {
        FastMap<uint32_t, InstructionWalker> lastConstantWriter;
        FastMap<Register, InstructionWalker> lastLoadRegister;
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(it.get() && it->checkOutputLocal() && !it->hasConditionalExecution() &&
                it->getOutput()->local()->countUsers(LocalUse::Type::WRITER) == 1 &&
                // TODO also combine is both ranges are not locally limited and overlap for the most part
                // (or at least if one range completely contains the other range)
                block.isLocallyLimited(it, it->getOutput()->local(), config.additionalOptions.accumulatorThreshold))
            {
                if(Optional<Literal> literal = getSourceLiteral(it))
                {
                    auto immIt = lastConstantWriter.find(literal->unsignedInt());
                    if(immIt != lastConstantWriter.end() && !it->hasSideEffects() &&
                        canReplaceConstantLoad(it, block.walk(), immIt->second, threshold))
                    {
                        auto oldLocal = it->getOutput()->local();
                        auto newLocal = immIt->second->getOutput()->local();
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Removing duplicate loading of literal: " << it->to_string() << logging::endl);
                        // Local#forUsers can't be used here, since we modify the list of users via
                        // LocalUser#replaceLocal
                        FastSet<const LocalUser*> readers = oldLocal->getUsers(LocalUse::Type::READER);
                        for(const LocalUser* reader : readers)
                            const_cast<LocalUser*>(reader)->replaceLocal(oldLocal, newLocal);
                        it.erase();
                        hasChanged = true;
                        continue;
                    }
                    else
                        lastConstantWriter[literal->unsignedInt()] = it;
                }
                if(auto reg = getSourceConstantRegister(it))
                {
                    auto regIt = lastLoadRegister.find(*reg);
                    if(regIt != lastLoadRegister.end() && !it->hasSideEffects() &&
                        canReplaceConstantLoad(it, block.walk(), regIt->second, threshold))
                    {
                        auto oldLocal = it->getOutput()->local();
                        auto newLocal = regIt->second->getOutput()->local();
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Removing duplicate loading of register: " << it->to_string() << logging::endl);
                        // Local#forUsers can't be used here, since we modify the list of users via
                        // LocalUser#replaceLocal
                        FastSet<const LocalUser*> readers = oldLocal->getUsers(LocalUse::Type::READER);
                        for(const LocalUser* reader : readers)
                            const_cast<LocalUser*>(reader)->replaceLocal(oldLocal, newLocal);
                        it.erase();
                        hasChanged = true;
                        continue;
                    }
                    else
                        lastLoadRegister[*reg] = it;
                }
            }
            it.nextInBlock();
        }
    }

    return hasChanged;
}

InstructionWalker optimizations::combineSelectionWithZero(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(it.isEndOfBlock() || it.isEndOfMethod())
        return it;
    InstructionWalker nextIt = it.copy().nextInBlock();
    if(nextIt.isEndOfBlock() || nextIt.isEndOfMethod())
        return it;
    if(it.get() == nullptr || nextIt.get() == nullptr)
        return it;
    if(!it.get<MoveOperation>() || !nextIt.get<MoveOperation>())
        return it;
    if(!it->getOutput() || !nextIt->getOutput())
        return it;
    MoveOperation* move = it.get<MoveOperation>();
    MoveOperation* nextMove = nextIt.get<MoveOperation>();
    if(it->hasSideEffects() || nextIt->hasSideEffects())
        return it;
    if(it->getOutput().value() != nextIt->getOutput().value())
        return it;
    auto extendedInst = it.get<intermediate::ExtendedInstruction>();
    auto nextExtendedInst = nextIt.get<intermediate::ExtendedInstruction>();
    if(!it->hasConditionalExecution() || !nextIt->hasConditionalExecution() || !extendedInst || !nextExtendedInst ||
        !extendedInst->getCondition().isInversionOf(nextExtendedInst->getCondition()))
        return it;
    // For conditional moves, the source could be written conditionally
    for(auto inst : {move, nextMove})
    {
        if(auto loc = inst->getSource().checkLocal())
        {
            // For now, don't combine if either source is written conditionally
            // TODO improve by removing the condition of the source and allow combining
            // XXX actually would need to check if the condition is the same as the condition for the moves
            auto writers = loc->getUsers(LocalUse::Type::WRITER);
            if(std::any_of(writers.begin(), writers.end(),
                   [](const LocalUser* writer) -> bool { return writer->hasConditionalExecution(); }))
                return it;
        }
    }
    // we have two consecutive moves to the same value, without side-effects and inverted conditions.
    // additionally, one of the moves writes a zero-vale
    if(move->getSource().hasLiteral(0_lit) && !nextMove->getSource().hasLiteral(0_lit))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Rewriting selection of either zero or " << nextMove->getSource().to_string()
                << " using only one input" << logging::endl);
        it.reset(createWithExtras<Operation>(
            *move, OP_XOR, move->getOutput().value(), nextMove->getSource(), nextMove->getSource()));
        // to process this instruction again (e.g. loading literals)
        it.previousInBlock();
    }
    else if(nextMove->getSource().hasLiteral(0_lit))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Rewriting selection of either " << move->getSource().to_string() << " or zero using only one input"
                << logging::endl);
        nextIt.reset(createWithExtras<Operation>(
            *nextMove, OP_XOR, nextMove->getOutput().value(), move->getSource(), move->getSource()));
    }
    return it;
}

bool optimizations::combineVectorRotations(const Module& module, Method& method, const Configuration& config)
{
    bool hasChanged = false;
    for(BasicBlock& block : method)
    {
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            VectorRotation* rot = it.get<VectorRotation>();
            if(!rot)
            {
                it.nextInBlock();
                continue;
            }

            if(rot->getOffset() == VECTOR_ROTATE_R5)
            {
                // check whether we rotate by r5 which is set to a constant value (e.g. when rewritten by some
                // other optimization) and rewrite rotation to rotation by this static offset
                auto writer = block.findLastWritingOfRegister(it, REG_ACC5);
                Optional<Value> staticOffset = NO_VALUE;
                if(writer && (staticOffset = (*writer)->precalculate(2).first))
                {
                    auto rotationInfo = rot->getVectorRotation();
                    if(staticOffset == INT_ZERO ||
                        /* since 4 divides 16, this is also valid for per-quad rotation */
                        (rotationInfo->isFullRotationAllowed() && staticOffset->hasLiteral(16_lit)) ||
                        (!rotationInfo->isFullRotationAllowed() && staticOffset->hasLiteral(4_lit)))
                    {
                        // NOTE: offset of 16 can occur for downwards rotations a << (16 - x) when the actual
                        // rotation x is zero.
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Replacing vector rotation by offset of zero with move: " << it->to_string()
                                << logging::endl);
                        auto newMove = &it.reset(
                            createWithExtras<MoveOperation>(*rot, rot->getOutput().value(), rot->getSource()));
                        if(it->getSignal() == SIGNAL_ALU_IMMEDIATE && !it->assertArgument(0).checkVector() &&
                            !it->assertArgument(0).getLiteralValue())
                            // need to remove the "ALU immediate" signal which might be added from the vector rotation
                            // offset, unless we move a literal value, otherwise the register-file B gets mapped to a
                            // literal instead
                            newMove->setSignaling(SIGNAL_NONE);
                        hasChanged = true;
                        continue;
                    }
                    else if(staticOffset->getLiteralValue() &&
                        staticOffset->getLiteralValue()->unsignedInt() < NATIVE_VECTOR_SIZE)
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Rewriting vector rotation to use constant offset: " << it->to_string()
                                << logging::endl);
                        rot->replaceValue(ROTATION_REGISTER,
                            Value(SmallImmediate::fromRotationOffset(
                                      static_cast<uint8_t>(staticOffset->getLiteralValue()->unsignedInt())),
                                TYPE_INT8),
                            LocalUse::Type::READER);
                        hasChanged = true;
                        continue;
                    }
                }
            }

            if(rot->getSource().checkLocal() && !rot->hasUnpackMode() && !rot->getSignal().hasSideEffects())
            {
                auto writer = dynamic_cast<const LoadImmediate*>(rot->getSource().getSingleWriter());
                if(writer && !writer->hasPackMode())
                {
                    // we rotate the result of a load -> rewrite to rotate the load instead.
                    if(writer->type == LoadType::REPLICATE_INT32)
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Replacing rotation of constant load with constant load: " << rot->to_string()
                                << logging::endl);
                        auto newLoad = std::make_unique<LoadImmediate>(it->getOutput().value(), writer->getImmediate());
                        newLoad->copyExtrasFrom(*rot, true);
                        it.reset(std::move(newLoad));
                        hasChanged = true;
                        continue;
                    }
                    else if(rot->type == RotationType::FULL && rot->getOffset() != VECTOR_ROTATE_R5)
                    {
                        // rotate upper and lower parts by given offset and reset rotation with load!
                        auto lit = writer->assertArgument(0).getLiteralValue().value().unsignedInt();
                        auto offset = rot->getOffset().getRotationOffset();
                        auto upper = rotate_left_halfword(lit >> 16, *offset) << 16;
                        auto lower = rotate_left_halfword(lit & 0xFFFF, *offset);
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Replacing rotation of masked load with rotated masked load: " << rot->to_string()
                                << logging::endl);
                        auto newLoad =
                            std::make_unique<LoadImmediate>(it->getOutput().value(), upper | lower, writer->type);
                        newLoad->copyExtrasFrom(*rot, true);
                        it.reset(std::move(newLoad));
                        hasChanged = true;
                        continue;
                    }
                    // TODO support for per-quad rotation
                }
            }
            if(rot->getSource().getSingleWriter())
            {
                auto writer = rot->getSource().getSingleWriter();
                if(writer && (writer->precalculate().first & &Value::getLiteralValue))
                {
                    // we don't directly take the literal source, so that the writing move can apply its pack mode (and
                    // this instruction can apply its unpack mode)
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Replacing rotation of constant operation with move: " << rot->to_string()
                            << logging::endl);
                    auto newMove = &it.reset(
                        createWithExtras<MoveOperation>(*rot, it->getOutput().value(), writer->getOutput().value()));
                    if(it->getSignal() == SIGNAL_ALU_IMMEDIATE)
                        // need to remove the "ALU immediate" signal which might be added from the vector rotation
                        // offset, unless we move a literal value, otherwise the register-file B gets mapped to a
                        // literal instead
                        newMove->setSignaling(SIGNAL_NONE);
                    hasChanged = true;
                    continue;
                }
            }

            if(!rot->hasUnpackMode() && !has_flag(rot->getSideEffects(), SideEffectType::REGISTER_READ))
            {
                if(rot->getSource().checkLocal() && rot->getOffset() != VECTOR_ROTATE_R5)
                {
                    if(auto firstRot = dynamic_cast<const VectorRotation*>(rot->getSource().getSingleWriter()))
                    {
                        if(!firstRot->hasSideEffects() && firstRot->getOffset() != VECTOR_ROTATE_R5 &&
                            (rot->type == firstRot->type || rot->type == RotationType::ANY ||
                                firstRot->type == RotationType::ANY))
                        {
                            auto firstIt = it.getBasicBlock()->findWalkerForInstruction(firstRot, it);
                            if(firstIt)
                            {
                                hasChanged = true;
                                /*
                                 * Can combine the offsets of two rotations,
                                 * - if the only source of a vector rotation is only written once,
                                 * - the source of the input is another vector rotation,
                                 * - both rotations only use immediate offsets,
                                 * - neither rotation has any side effects and
                                 * - both rotations are of the same type (full-vector or per-quad)
                                 */
                                const uint8_t offset = (rot->getOffset().getRotationOffset().value() +
                                                           firstRot->getOffset().getRotationOffset().value()) %
                                    (!rot->getVectorRotation()->isFullRotationAllowed() ? 4 : 16);
                                if(offset == 0)
                                {
                                    CPPLOG_LAZY(logging::Level::DEBUG,
                                        log << "Replacing unnecessary vector rotations " << firstRot->to_string()
                                            << " and " << rot->to_string() << " with single move" << logging::endl);
                                    auto newMove = &it.reset(createWithExtras<MoveOperation>(
                                        *rot, rot->getOutput().value(), firstRot->getSource()));
                                    it->copyExtrasFrom(*firstRot);
                                    if(it->getSignal() == SIGNAL_ALU_IMMEDIATE &&
                                        !it->assertArgument(0).checkVector() &&
                                        !it->assertArgument(0).getLiteralValue())
                                        // need to remove the "ALU immediate" signal which might be added from the
                                        // vector rotation offset, unless we move a literal value, otherwise the
                                        // register-file B gets mapped to a literal instead
                                        newMove->setSignaling(SIGNAL_NONE);

                                    if(!(*firstIt)->hasSideEffects() &&
                                        !firstRot->getOutput()->local()->hasUsers(LocalUse::Type::READER))
                                        // only remove first rotation if it does not have a second user
                                        firstIt->erase();
                                }
                                else
                                {
                                    CPPLOG_LAZY(logging::Level::DEBUG,
                                        log << "Combining vector rotations " << firstRot->to_string() << " and "
                                            << rot->to_string() << " to a single rotation with offset "
                                            << static_cast<unsigned>(offset) << logging::endl);
                                    it.reset(createWithExtras<VectorRotation>(*rot, rot->getOutput().value(),
                                        firstRot->getSource(), SmallImmediate::fromRotationOffset(offset),
                                        rot->type == RotationType::ANY ? firstRot->type : rot->type));
                                    it->copyExtrasFrom(*firstRot);
                                    if(!(*firstIt)->hasSideEffects() &&
                                        !firstRot->getOutput()->local()->hasUsers(LocalUse::Type::READER))
                                        // only remove first rotation if it does not have a second user
                                        firstIt->erase();
                                }
                            }
                        }
                    }
                }
            }
            it.nextInBlock();
        }
    }
    return hasChanged;
}

InstructionWalker optimizations::combineArithmeticOperations(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    auto op = it.get<Operation>();
    if(!op)
        return it;
    if(it->getArguments().size() != 2)
        return it;
    if(it->hasConditionalExecution() || !op->isSimpleOperation())
        return it;
    if(!it->readsLocal())
        return it;

    // exactly one local and one literal operand
    Value literalArg = UNDEFINED_VALUE;
    Value localArg = UNDEFINED_VALUE;
    if(it->getArguments()[0].getConstantValue() & &Value::getLiteralValue)
    {
        literalArg = *it->getArguments()[0].getConstantValue();
        localArg = it->getArguments()[1];
    }
    else if(it->getArguments()[1].getConstantValue() & &Value::getLiteralValue)
    {
        literalArg = *it->getArguments()[1].getConstantValue();
        localArg = it->getArguments()[0];
    }
    else
        // even if we could combine, we cannot save an instruction (since we cannot pre-calculate anything)
        return it;

    auto singleWriter = localArg.getSingleWriter();
    if(!singleWriter || singleWriter->getArguments().size() != 2)
        return it;

    auto writerOp = dynamic_cast<const Operation*>(singleWriter);
    if(writerOp == nullptr || writerOp->op != op->op)
        // not the same operation
        return it;

    if(!writerOp->isSimpleOperation() || singleWriter->hasConditionalExecution())
        return it;

    if(singleWriter->getOutput()->local()->countUsers(LocalUse::Type::READER) != 1)
        // we cannot remove or modify the writer, so abort here
        return it;

    Value otherLiteralArg = UNDEFINED_VALUE;
    Value origArg = UNDEFINED_VALUE;
    if(singleWriter->getArguments()[0].getConstantValue() & &Value::getLiteralValue)
    {
        otherLiteralArg = *singleWriter->getArguments()[0].getConstantValue();
        origArg = singleWriter->getArguments()[1];
    }
    else if(singleWriter->getArguments()[1].getConstantValue() & &Value::getLiteralValue)
    {
        otherLiteralArg = *singleWriter->getArguments()[1].getConstantValue();
        origArg = singleWriter->getArguments()[0];
    }
    else
        // same as above, we cannot pre-calculate anything
        return it;

    if(!op->op.isCommutative() && (it->getArgument(0) != localArg || singleWriter->getArgument(0) != origArg))
        // we cannot transform e.g. (a op b) op c to b op (a op c)
        return it;

    Optional<Value> precalc = NO_VALUE;
    if(op->op.isAssociative())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Combining associative operations " << singleWriter->to_string() << " and " << it->to_string()
                << logging::endl);
        precalc = op->op(literalArg, otherLiteralArg).first;
    }
    else if(op->op == OP_SHL || op->op == OP_SHR || op->op == OP_ASR || op->op == OP_ROR)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Combining shifts " << singleWriter->to_string() << " and " << it->to_string() << logging::endl);
        precalc = OP_ADD(literalArg, otherLiteralArg).first;
        // If the new offset would exceed the 31, then the result is not true anymore (since the ALU only takes the last
        // 5 bits of the offset). E.g. (a >> 30) >> 30 = a >> 60 != a >> (60 & 0x1F)
        if(precalc & &Value::getLiteralValue && (precalc & &Value::getLiteralValue)->unsignedInt() >= 32)
        {
            if(op->op == OP_ASR)
                // a >>> 31 and a >>> 123 have the same effect, everything except the high bit is shifted away, so
                // replace with valid shift resulting in the same value
                precalc = Value(Literal(31), TYPE_INT8);
            else if(op->op == OP_SHL || op->op == OP_SHR)
            {
                // a >> 34 and a << 35 is always zero, so replace with instruction always setting zero
                op->op = OP_AND;
                precalc = INT_ZERO;
            }
            // For rotation, the result is the same for e.g. a >>< 123 and a >>< (123 & 0x1F), so no need to change
            // anything here
        }
    }
    auto lastIt = it.getBasicBlock()->findWalkerForInstruction(singleWriter, it);
    if(lastIt)
    {
        lastIt->reset(
            createWithExtras<Operation>(*it.get(), op->op, it->getOutput().value(), origArg, precalc.value()));
        it.erase();
        // don't skip next instruction
        it.previousInBlock();
    }
    return it;
}

static Optional<std::pair<Value, InstructionDecorations>> combineAdditions(
    Method& method, InstructionWalker referenceIt, FastMap<Value, InstructionDecorations>& addedValues)
{
    Optional<std::pair<Value, InstructionDecorations>> prevResult;
    auto valIt = addedValues.begin();
    while(valIt != addedValues.end())
    {
        if(prevResult)
        {
            auto newResult = method.addNewLocal(prevResult->first.type);
            auto newFlags = intersect_flags(prevResult->second, valIt->second);
            assign(referenceIt, newResult) = (as_signed{prevResult->first} + as_signed{valIt->first}, newFlags);
            prevResult = std::make_pair(newResult, newFlags);
        }
        else
            prevResult = std::make_pair(valIt->first, valIt->second);
        valIt = addedValues.erase(valIt);
    }
    return prevResult;
}

// static void rewriteIndexCalculation(Method& method, analysis::MemoryAccessRange& range)
// {
//     // 3. combine the additions so work-group uniform and non-uniform values are added
//     // separately
//     auto insertIt = range.typeSizeShift ? range.typeSizeShift.value() : range.baseAddressAdd;
//     auto firstVal = combineAdditions(method, insertIt, range.groupUniformAddressParts);
//     auto secondVal = combineAdditions(method, insertIt, range.dynamicAddressParts);
//     Optional<std::pair<Value, InstructionDecorations>> resultVal;
//     if(!range.groupUniformAddressParts.empty() || !range.dynamicAddressParts.empty())
//         throw CompilationError(CompilationStep::OPTIMIZER, "Too many values remaining",
//             std::to_string(range.groupUniformAddressParts.size() + range.dynamicAddressParts.size()));
//     if(firstVal && secondVal)
//     {
//         // add work-group uniform and variable part
//         resultVal = std::make_pair(
//             method.addNewLocal(range.memoryObject->type), intersect_flags(firstVal->second, secondVal->second));
//         insertIt.emplace(new Operation(OP_ADD, resultVal->first, firstVal->first, secondVal->first));
//         insertIt->addDecorations(resultVal->second);
//     }
//     else if(firstVal)
//         resultVal = firstVal;
//     else if(secondVal)
//         resultVal = secondVal;
//     if(range.typeSizeShift)
//         const_cast<intermediate::Operation*>(range.typeSizeShift)->setArgument(0, std::move(resultVal->first));
//     else
//         // TODO replace index variable with new index variable
//         throw CompilationError(
//             CompilationStep::OPTIMIZER, "Not yet implemented, no shift in address calculation", range.to_string());

//     CPPLOG_LAZY(logging::Level::DEBUG,
//         log << "Rewrote address-calculation with indices "
//             << (firstVal ? (firstVal->first.to_string() + " (" + toString(firstVal->second) + ")") : "") << " and "
//             << (secondVal ? (secondVal->first.to_string() + " (" + toString(secondVal->second) + ")") : "")
//             << logging::endl);
// }

bool optimizations::cacheWorkGroupDMAAccess(const Module& module, Method& method, const Configuration& config)
{
    if(method.metaData.getFixedWorkGroupSize() == 1u)
    {
        // no need to do anything, if there is only 1 work-item
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Skipping work-group caching for work-groups of fixed item count of 1" << logging::endl);
        return false;
    }
    auto memoryAccessRanges = analysis::determineAccessRanges(method);
    for(auto& pair : memoryAccessRanges)
    {
        bool allUniformPartsEqual;
        analysis::ValueRange offsetRange;
        std::tie(allUniformPartsEqual, offsetRange) = analysis::checkWorkGroupUniformParts(pair.second, false);
        if(!allUniformPartsEqual)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot cache memory location " << pair.first->to_string()
                    << " in VPM, since the work-group uniform parts of the address calculations differ, which "
                       "is not yet supported!"
                    << logging::endl);
            continue;
        }
        if(offsetRange.getRange() >= config.availableVPMSize)
        {
            // this also checks for any over/underflow when converting the range to unsigned int in the next steps
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot cache memory location " << pair.first->to_string()
                    << " in VPM, the accessed range is too big: " << offsetRange.to_string() << logging::endl);
            continue;
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Memory location " << pair.first->to_string() << " is accessed via DMA in the dynamic range "
                << offsetRange.to_string() << logging::endl);

        auto accessedType = method.createArrayType(
            pair.first->type, static_cast<unsigned>(offsetRange.getRange() + 1 /* bounds of range are inclusive! */));

        // TODO the local is not correct, at least not if there is a work-group uniform offset
        auto vpmArea = method.vpm->addArea(pair.first, accessedType);
        if(vpmArea == nullptr)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Memory location " << pair.first->to_string() << " with dynamic access range "
                    << offsetRange.to_string() << " cannot be cached in VPM, since it does not fit" << logging::endl);
            continue;
        }

        // TODO insert load memory area into VPM at start of kernel (after all the required offsets/indices are
        // calculated), can be skipped if memory area is write-only
        // TODO calculate address from base address and work-group uniform parts
        // TODO insert store VPM into memory area at end of kernel
        // TODO rewrite memory accesses to only access the correct VPM area

        // for(auto& entry : pair.second)
        //     rewriteIndexCalculation(method, entry);

        // TODO now, combine access to memory with VPM access
        // need to make sure, only 1 kernel accesses RAM/writes the configuration, how?
        // -> need some lightweight synchronization (e.g. status value in VPM?? One kernel would need to
        // poll!!)
        // TODO if minValue  != 0, need then to deduct it from the group-uniform address too!
        // use base pointer as memory pointer (for read/write-back) and offset as VPM offset. maximum
        // offset is the number of elements to copy/cache

        // TODO insert initial read from DMA, final write to DMA
        // even for writes, need to read, since memory in between might be untouched?

        // TODO if it can be proven that all values in the range are guaranteed to be written (and not read before),
        // we can skip the initial loading. This guarantee needs to hold across all work-items in a group!
    }

    // XXX
    return eliminateDeadCode(module, method, config);
}
