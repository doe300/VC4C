/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Eliminator.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../analysis/AvailableExpressionAnalysis.h"
#include "../analysis/DataDependencyGraph.h"
#include "../periphery/SFU.h"
#include "log.h"

#include <algorithm>
#include <list>
#include <map>

using namespace vc4c;
using namespace vc4c::optimizations;

bool optimizations::eliminateDeadCode(const Module& module, Method& method, const Configuration& config)
{
    // TODO (additionally or instead of this) walk through locals, check whether they are never read and writings have
    // no side-effects  then walk through all writings of such locals and remove them (example:
    // ./testing/test_vpm_write.cl)
    bool hasChanged = false;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        intermediate::IntermediateInstruction* instr = it.get();
        intermediate::Operation* op = it.get<intermediate::Operation>();
        intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
        intermediate::LoadImmediate* load = it.get<intermediate::LoadImmediate>();

        // fail-fast on all not-supported instruction types
        // also skip all instructions writing to non-locals (registers)
        if((op || move || load) && instr->hasValueType(ValueType::LOCAL))
        {
            // check whether the output of an instruction is never read
            // only check for ALU-operations and loads, if no flags are set and no special signals are sent
            if(!instr->hasSideEffects())
            {
                const Local* dest = instr->getOutput()->local();
                // check whether local is
                // a) no parameter ??
                if(!dest->is<Parameter>())
                {
                    // b) never read at all
                    // must check from the start, because in SPIR-V, locals can be read before they are written to (e.g.
                    // in phi-node and branch backwards)
                    bool isRead = !dest->getUsers(LocalUse::Type::READER).empty();
                    if(!isRead)
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Removing instruction " << instr->to_string() << ", since its output is never read"
                                << logging::endl);
                        it.erase();
                        // if we removed this instruction, maybe the previous one can be removed too??
                        it.previousInBlock();
                        hasChanged = true;
                        continue;
                    }
                }
            }
            if(move != nullptr)
            {
                if(move->getSource().checkLocal() && move->getOutput()->checkLocal() && move->isSimpleMove())
                {
                    // if for a move, neither the input-local nor the output-local are written to afterwards,
                    // XXX or the input -local is only written after the last use of the output-local
                    // both locals can be the same and the move can be removed

                    auto inLoc = move->getSource().local();
                    auto outLoc = move->getOutput()->local();
                    // for instruction added by phi-elimination, the result could have been written to (with a different
                    // source) previously, so check
                    bool isWrittenTo = !outLoc->getUsers(LocalUse::Type::WRITER).empty();
                    if(!isWrittenTo && inLoc->type == outLoc->type)
                    {
                        // TODO what if both locals are written before (and used differently), possible??
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Merging locals " << inLoc->to_string() << " and " << outLoc->to_string()
                                << " since they contain the same value" << logging::endl);
                        outLoc->forUsers(LocalUse::Type::READER, [inLoc, outLoc](const LocalUser* instr) -> void {
                            // change outLoc to inLoc
                            bool outLocFound = false;
                            for(std::size_t i = 0; i < instr->getArguments().size(); ++i)
                            {
                                Value tmp = instr->assertArgument(i);
                                if(tmp.hasLocal(outLoc))
                                {
                                    tmp = Value(inLoc, tmp.type);
                                    const_cast<LocalUser*>(instr)->setArgument(i, std::move(tmp));
                                    outLocFound = true;
                                }
                            }
                            if(!outLocFound)
                            {
                                throw CompilationError(
                                    CompilationStep::OPTIMIZER, "Unsupported case of instruction merging!");
                            }
                        });
                        // skip ++it, so next instructions is looked at too
                        it.erase();
                        hasChanged = true;
                        continue;
                    }
                }
                else if(move->getSource().hasRegister(REG_UNIFORM) && !move->signal.hasSideEffects())
                {
                    // if the added work-group info UNIFORMs are never read, we can remove then (and their flag)
                    const Local* dest = instr->getOutput()->local();
                    bool isRead = !dest->getUsers(LocalUse::Type::READER).empty();
                    if(!isRead)
                    {
                        using FuncType = decltype(&KernelUniforms::setGlobalDataAddressUsed);
                        FuncType disableFunc = nullptr;
                        if(dest->name == Method::WORK_DIMENSIONS)
                            disableFunc = &KernelUniforms::setWorkDimensionsUsed;
                        else if(dest->name == Method::GLOBAL_DATA_ADDRESS)
                            disableFunc = &KernelUniforms::setGlobalDataAddressUsed;
                        else if(dest->name == Method::GLOBAL_OFFSET_X)
                            disableFunc = &KernelUniforms::setGlobalOffsetXUsed;
                        else if(dest->name == Method::GLOBAL_OFFSET_Y)
                            disableFunc = &KernelUniforms::setGlobalOffsetYUsed;
                        else if(dest->name == Method::GLOBAL_OFFSET_Z)
                            disableFunc = &KernelUniforms::setGlobalOffsetZUsed;
                        else if(dest->name == Method::GROUP_ID_X)
                            disableFunc = &KernelUniforms::setGroupIDXUsed;
                        else if(dest->name == Method::GROUP_ID_Y)
                            disableFunc = &KernelUniforms::setGroupIDYUsed;
                        else if(dest->name == Method::GROUP_ID_Z)
                            disableFunc = &KernelUniforms::setGroupIDZUsed;
                        else if(dest->name == Method::LOCAL_IDS)
                            disableFunc = &KernelUniforms::setLocalIDsUsed;
                        else if(dest->name == Method::LOCAL_SIZES)
                            disableFunc = &KernelUniforms::setLocalSizesUsed;
                        else if(dest->name == Method::NUM_GROUPS_X)
                            disableFunc = &KernelUniforms::setNumGroupsXUsed;
                        else if(dest->name == Method::NUM_GROUPS_Y)
                            disableFunc = &KernelUniforms::setNumGroupsYUsed;
                        else if(dest->name == Method::NUM_GROUPS_Z)
                            disableFunc = &KernelUniforms::setNumGroupsZUsed;

                        if(disableFunc)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Removing read of work-group UNIFORM, since it is never used: "
                                    << move->to_string() << logging::endl);
                            // disable work-group UNIFORM from method
                            (method.metaData.uniformsUsed.*disableFunc)(false);
                            it.erase();
                            hasChanged = true;
                            continue;
                        }
                    }
                }
            }
        }
        // remove unnecessary writes to special purpose registers
        if((op || move || load) && instr->hasValueType(ValueType::REGISTER) && !instr->hasSideEffects())
        {
            // check whether the register output is actually used. This depends on the kind of register
            // Having an unused rotation offset write can happen, e.g. if the value is zero and the rotation gets
            // rewritten to a move (in #combineVectorRotations)
            bool isUsed = true;
            if(instr->writesRegister(REG_ACC5) || instr->writesRegister(REG_REPLICATE_QUAD) ||
                instr->writesRegister(REG_REPLICATE_ALL))
            {
                auto checkIt = it.copy().nextInBlock();
                while(!checkIt.isEndOfBlock())
                {
                    if(checkIt.get() &&
                        (checkIt->writesRegister(REG_ACC5) || checkIt->writesRegister(REG_REPLICATE_QUAD) ||
                            checkIt->writesRegister(REG_REPLICATE_ALL)))
                    {
                        // register is written before it is read!
                        isUsed = false;
                        break;
                    }
                    if(checkIt.get() &&
                        (checkIt->readsRegister(REG_ACC5) || checkIt->readsRegister(REG_REPLICATE_QUAD) ||
                            checkIt->readsRegister(REG_REPLICATE_ALL)))
                    {
                        // register is used
                        break;
                    }
                    checkIt.nextInBlock();
                }
            }
            // TODO same for SFU/TMU?! Or do they always trigger side effects?

            if(!isUsed)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Removing write to special purpose register which is never used: " << it->to_string()
                        << logging::endl);
                it.erase();
                hasChanged = true;
                continue;
            }
        }
        it.nextInMethod();
    }
    // remove unused locals. This is actually not required, but gives us some feedback about the effect of this
    // optimization
    method.cleanLocals();
    return hasChanged;
}

InstructionWalker optimizations::simplifyOperation(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(auto op = it.get<intermediate::Operation>())
    {
        if(op->isSimpleOperation())
        {
            // TODO could actually allow for setflags! At least replacing, not removing
            // improve by pre-calculating first and second arguments
            const Value firstArg = (op->getFirstArg().getSingleWriter() != nullptr ?
                    op->getFirstArg().getSingleWriter()->precalculate(3).first :
                    NO_VALUE)
                                       .value_or(op->getFirstArg());
            const Optional<Value> secondArg =
                (op->getSecondArg() && op->assertArgument(1).getSingleWriter() != nullptr ?
                        op->assertArgument(1).getSingleWriter()->precalculate(3).first :
                        op->getSecondArg());

            Optional<Value> rightIdentity = OpCode::getRightIdentity(op->op);
            Optional<Value> leftIdentity = OpCode::getLeftIdentity(op->op);
            Optional<Value> rightAbsorbing = OpCode::getRightAbsorbingElement(op->op);
            Optional<Value> leftAbsorbing = OpCode::getLeftAbsorbingElement(op->op);

            // one of the operands is the absorbing element, operation can be replaced with move
            if(leftAbsorbing && firstArg.hasLiteral(leftAbsorbing->getLiteralValue().value()))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing obsolete " << op->to_string() << " with move 1" << logging::endl);
                it.reset((new intermediate::MoveOperation(
                              op->getOutput().value(), leftAbsorbing.value(), op->conditional, op->setFlags))
                             ->addDecorations(it->decoration));
            }
            else if(rightAbsorbing && secondArg && secondArg->hasLiteral(rightAbsorbing->getLiteralValue().value()))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing obsolete " << op->to_string() << " with move 2" << logging::endl);
                it.reset((new intermediate::MoveOperation(
                              op->getOutput().value(), rightAbsorbing.value(), op->conditional, op->setFlags))
                             ->addDecorations(it->decoration));
            }
            // both operands are the same and the operation is self-inverse <=> f(a, a) = 0
            else if(op->op.isSelfInverse() && firstArg == secondArg && firstArg.type.getElementType() != TYPE_BOOL)
            {
                // do not replace xor true, true, since this is almost always combined with or true, true for inverted
                // condition
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing obsolete " << op->to_string() << " with move 7" << logging::endl);
                it.reset((new intermediate::MoveOperation(op->getOutput().value(),
                              Value(Literal(0u), op->getOutput()->type), op->conditional, op->setFlags))
                             ->addDecorations(it->decoration));
            }
            // writes into the input -> can be removed, if it doesn't do anything
            else if(op->getOutput() && op->getOutput().value() == op->getFirstArg())
            {
                // check whether second-arg exists and does nothing
                if(rightIdentity && secondArg && secondArg->hasLiteral(rightIdentity->getLiteralValue().value()))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG, log << "Removing obsolete " << op->to_string() << logging::endl);
                    it.erase();
                    // don't skip next instruction
                    it.previousInBlock();
                }
                else if(op->op.isIdempotent() && secondArg && secondArg.value() == firstArg)
                {
                    CPPLOG_LAZY(logging::Level::DEBUG, log << "Removing obsolete " << op->to_string() << logging::endl);
                    it.erase();
                    // don't skip next instruction
                    it.previousInBlock();
                }
            }
            else if(op->getOutput() && op->getSecondArg() && op->getOutput().value() == op->assertArgument(1))
            {
                // check whether first-arg does nothing
                if(leftIdentity && firstArg.hasLiteral(leftIdentity->getLiteralValue().value()))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG, log << "Removing obsolete " << op->to_string() << logging::endl);
                    it.erase();
                    // don't skip next instruction
                    it.previousInBlock();
                }
                else if(op->op.isIdempotent() && secondArg && secondArg.value() == firstArg &&
                    !firstArg.checkRegister() && !firstArg.isUndefined())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG, log << "Removing obsolete " << op->to_string() << logging::endl);
                    it.erase();
                    // don't skip next instruction
                    it.previousInBlock();
                }
            }
            else // writes to another local -> can be replaced with move
            {
                // check whether second argument exists and does nothing
                if(rightIdentity && secondArg && secondArg->hasLiteral(rightIdentity->getLiteralValue().value()))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Replacing obsolete " << op->to_string() << " with move 3" << logging::endl);
                    it.reset((new intermediate::MoveOperation(
                                  op->getOutput().value(), op->getFirstArg(), op->conditional, op->setFlags))
                                 ->addDecorations(it->decoration));
                }
                // check whether first argument does nothing
                else if(leftIdentity && secondArg && firstArg.hasLiteral(leftIdentity->getLiteralValue().value()))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Replacing obsolete " << op->to_string() << " with move 4" << logging::endl);
                    it.reset((new intermediate::MoveOperation(
                                  op->getOutput().value(), op->assertArgument(1), op->conditional, op->setFlags))
                                 ->addDecorations(it->decoration));
                }
                // check whether operation does not really calculate anything
                else if(op->op.isIdempotent() && secondArg && secondArg.value() == firstArg &&
                    !firstArg.checkRegister() && !firstArg.isUndefined())
                {
                    logging::logLazy(logging::Level::DEBUG, [&]() {
                        logging::debug() << secondArg.value().to_string() << " - " << firstArg.to_string()
                                         << logging::endl;
                        logging::debug() << "Replacing obsolete " << op->to_string() << " with move 5" << logging::endl;
                    });
                    it.reset((new intermediate::MoveOperation(
                                  op->getOutput().value(), op->assertArgument(1), op->conditional, op->setFlags))
                                 ->addDecorations(it->decoration));
                }
            }
        }
        // TODO trunc to int32/float
    }
    else if(auto move = it.get<intermediate::MoveOperation>())
    {
        if(move->getSource() == move->getOutput().value() && move->isSimpleMove())
        {
            // skip copying to same, if no flags/signals/pack and unpack-modes are set
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Removing obsolete " << move->to_string() << logging::endl);
            it.erase();
            // don't skip next instruction
            it.previousInBlock();
        }
        if(it.get<intermediate::VectorRotation>() && move->getSource().isLiteralValue())
        {
            // replace rotation of constant with move
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replacing obsolete " << move->to_string() << " with move 6" << logging::endl);
            it.reset((new intermediate::MoveOperation(
                          move->getOutput().value(), move->getSource(), move->conditional, move->setFlags))
                         ->addDecorations(it->decoration));
        }
    }

    return it;
}

InstructionWalker optimizations::foldConstants(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    intermediate::Operation* op = it.get<intermediate::Operation>();
    if(op != nullptr && !op->hasUnpackMode())
    {
        // calculations with literals can be pre-calculated
        if(op->getFirstArg().getLiteralValue() && (!op->getSecondArg() || op->assertArgument(1).getLiteralValue()))
        {
            if(op->conditional != COND_ALWAYS && op->op == OP_XOR && op->getSecondArg() == op->getFirstArg())
            {
                // skip "xor ?, true, true", so it can be optimized (combined with "move ?, true") afterwards
                // also skip any "xor ?, val, val", since they are created on purpose (by combineSelectionWithZero to
                // allow for combination with the other case)
                return it;
            }
            const Optional<Value> value = op->precalculate(3).first;
            if(value)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing '" << op->to_string() << "' with constant value: " << value.to_string()
                        << logging::endl);
                it.reset((new intermediate::MoveOperation(op->getOutput().value(), value.value()))->copyExtrasFrom(op));
            }
        }
    }
    return it;
}

static void mapPhi(const intermediate::PhiNode& node, Method& method, InstructionWalker it)
{
    while(!it.isStartOfBlock())
    {
        it.previousInBlock();
    }
    const Local* label = it.get<intermediate::BranchLabel>()->getLabel();
    for(const auto& pair : node.getValuesForLabels())
    {
        BasicBlock* bb = method.findBasicBlock(pair.first);
        if(bb == nullptr)
        {
            logging::error() << "Cannot map phi-node to label: " << pair.first->name << logging::endl;
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to map all phi-options to valid basic-blocks",
                pair.first->to_string());
        }
        // make sure, moves are inserted before the outgoing branches
        InstructionWalker it = bb->walkEnd();
        ConditionCode jumpCondition = COND_ALWAYS;
        Value condition(UNDEFINED_VALUE);
        while(it.copy().previousInBlock().get<intermediate::Branch>())
        {
            it.previousInBlock();
            if(it.get<intermediate::Branch>()->getTarget() == label)
            {
                jumpCondition = it->conditional;
                condition = it.get<intermediate::Branch>()->getCondition();
            }
        }
        // Since originally the value of the PHI node is set after the jump (at the start of the destination basic
        // block)  and we have conditional branches "jump to A or B", we need to only set the value if we take the
        // (conditional) branch jumping to this basic block.

        if(jumpCondition != COND_ALWAYS)
        {
            // Since the correct flags for the branch might not be set, we need to set them here.
            // Also, don't "or" with element number, since we might need to set the flags for more than the first
            // SIMD-element, this way, we set it for all
            it.emplace(new intermediate::MoveOperation(NOP_REGISTER, condition, COND_ALWAYS, SetFlag::SET_FLAGS));
            it.nextInBlock();
        }
        it.emplace((new intermediate::MoveOperation(node.getOutput().value(), pair.second, jumpCondition))
                       ->copyExtrasFrom(&node)
                       ->addDecorations(add_flag(node.decoration, intermediate::InstructionDecorations::PHI_NODE)));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Inserting into end of basic-block '" << pair.first->name << "': " << it->to_string()
                << logging::endl);
    }

    // set reference of local to original reference, if always the same for all possible sources
    if(node.hasValueType(ValueType::LOCAL))
    {
        const Local* ref = nullptr;
        for(const auto& pair : node.getValuesForLabels())
        {
            if(!pair.second.checkLocal())
                // cannot set universal reference
                return;
            if(pair.second.hasLocal(node.getOutput()->local()) ||
                pair.second.local()->getBase(true) == node.getOutput()->local())
                // phi node references its own result, ignore
                continue;
            if(ref != nullptr &&
                !(pair.second.local()->getBase(true) == ref || pair.second.local()->reference.first == ref))
                // references differ
                return;
            ref = pair.second.local()->getBase(true);
        }

        node.getOutput()->local()->reference.first = const_cast<Local*>(ref);
        node.getOutput()->local()->reference.second = ANY_ELEMENT;
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "PHI output: " << node.getOutput()->to_string(true, true) << logging::endl);
    }
}

void optimizations::eliminatePhiNodes(const Module& module, Method& method, const Configuration& config)
{
    // Search for all phi-nodes and insert all mapped instructions to the end of the corresponding basic block
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(auto phiNode = it.get<intermediate::PhiNode>())
        {
            // 2) map the phi-node to the move-operations per predecessor-label
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Eliminating phi-node by inserting moves: " << it->to_string() << logging::endl);
            mapPhi(*phiNode, method, it);
            it.erase();
        }
        else
            it.nextInMethod();
    }
}

InstructionWalker optimizations::eliminateReturn(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(it.get<intermediate::Return>())
    {
        const Local* target = method.findLocal(BasicBlock::LAST_BLOCK);
        if(target == nullptr)
        {
            target = method.findOrCreateLocal(TYPE_LABEL, BasicBlock::LAST_BLOCK);
            method.appendToEnd(new intermediate::BranchLabel(*target));
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Replacing return in kernel-function with branch to end-label" << logging::endl);
        it.reset(new intermediate::Branch(target, COND_ALWAYS, BOOL_TRUE));
    }
    return it;
}

static bool isNoReadBetween(InstructionWalker first, InstructionWalker second, Register reg)
{
    first.nextInBlock();
    while(!first.isEndOfBlock() && first != second)
    {
        // just to be sure (e.g. for reading TMU/SFU/VPM), check triggering load of r4 and releasing of mutex too
        if(first->readsRegister(reg) || first->writesRegister(reg) || first->signal.triggersReadOfR4() ||
            first->writesRegister(REG_MUTEX))
            return false;
        // for reading VPM, check also VPM read setup
        if(reg.isVertexPipelineMemory() && first->hasValueType(ValueType::REGISTER) &&
            first->getOutput()->reg().isVertexPipelineMemory())
            return false;
        first.nextInBlock();
    }
    return true;
}

/* TODO
 * this propagation should work among basic blocks.
 * but we need very keen to unsafe-case
 *
 *     A    Move propagation of an instruction in C may dangerous if an instruction in D is rewritten.
 *    / \   But, the propagation A to B and C should work.
 *   /   \
 *  B    C
 *  \    /
 *   \  /
 *    D
 */
bool optimizations::propagateMoves(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    auto replaced = false;
    while(!it.isEndOfMethod())
    {
        auto const op = it.get<intermediate::MoveOperation>();

        // just copy of value
        // should not work like:
        //
        // - mov.setf null, r0
        // - mov r0, r1 with pack, unpack
        // - mov r0, r4 // TODO r4 can be propagate unless signal or the use of sfu isn't issued
        // - mov r0, r5
        // - mov r0, vpm
        // - mov r0, unif
        //
        // very side-effects are mattered here.
        //
        // - mov.setf r0, r1
        // - mov r0, r1, load_tmu0
        if(op && !it.get<intermediate::VectorRotation>() && !op->hasConditionalExecution() && !op->hasPackMode() &&
            !op->hasUnpackMode() && op->getOutput().has_value() &&
            (!op->getSource().checkRegister() || !op->getSource().reg().hasSideEffectsOnRead()) &&
            (!op->getOutput().value().checkRegister()))
        {
            auto it2 = it.copy().nextInBlock();
            auto oldValue = op->getOutput().value();
            const auto& newValue = op->getSource();
            // only continue iterating as long as there is a read of the local left
            FastSet<const LocalUser*> remainingLocalReads = oldValue.checkLocal() ?
                oldValue.local()->getUsers(LocalUse::Type::READER) :
                FastSet<const LocalUser*>{};
            // registers fixed to physical file B cannot be combined with literal
            bool skipLiteralReads = newValue.checkRegister() && newValue.reg().file == RegisterFile::PHYSICAL_B;
            while(!it2.isEndOfBlock() && !remainingLocalReads.empty())
            {
                bool replacedThisInstruction = false;
                if(!skipLiteralReads || !it2->readsLiteral())
                {
                    for(auto arg : it2->getArguments())
                    {
                        if(arg == oldValue && !arg.checkLiteral() && !arg.checkImmediate())
                        {
                            replaced = true;
                            replacedThisInstruction = true;
                            it2->replaceValue(oldValue, newValue, LocalUse::Type::READER);
                            remainingLocalReads.erase(it2.get());
                        }
                    }
                }

                if(replacedThisInstruction)
                    foldConstants(module, method, it2, config);

                if(it2->getOutput().has_value() && it2->getOutput().value() == oldValue)
                    break;

                it2.nextInBlock();
            }
        }

        it.nextInMethod();
    }

    return replaced;
}

bool optimizations::eliminateRedundantMoves(const Module& module, Method& method, const Configuration& config)
{
    /*
     * XXX can be improved to move UNIFORM reads,
     * iff in same/first block and no reorder of UNIFORM values/UNIFORM pointer is not re-set.
     * Problem: initially there are reads of UNIFORM between write and usage, even if they could also be removed
     * -> would need to run this optimization from end-to-front (additionally to front-to-end?)
     *
     * behavior can be tested on CTS: api/test_api min_max_constant_args
     */

    bool flag = false;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(it.get<intermediate::MoveOperation>() &&
            !it->hasDecoration(intermediate::InstructionDecorations::PHI_NODE) && !it->hasPackMode() &&
            !it->hasUnpackMode() && it->conditional == COND_ALWAYS && !it.get<intermediate::VectorRotation>())
        {
            // skip PHI-nodes, since they are read in another basic block (and the output is written more than once
            // anyway) as well as modification of the value, conditional execution and vector-rotations
            const intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();

            // the source is written and read only once
            bool sourceUsedOnce = move->getSource().getSingleWriter() != nullptr &&
                move->getSource().local()->getUsers(LocalUse::Type::READER).size() == 1;
            // the destination is written and read only once (and not in combination with a literal value, to not
            // introduce register conflicts)
            bool destUsedOnce = move->hasValueType(ValueType::LOCAL) && move->getOutput()->getSingleWriter() == move &&
                move->getOutput()->local()->getUsers(LocalUse::Type::READER).size() == 1;
            bool destUsedOnceWithoutLiteral = destUsedOnce &&
                !(*move->getOutput()->local()->getUsers(LocalUse::Type::READER).begin())->readsLiteral();

            auto sourceWriter = (move->getSource().getSingleWriter() != nullptr) ?
                it.getBasicBlock()->findWalkerForInstruction(move->getSource().getSingleWriter(), it) :
                Optional<InstructionWalker>{};
            auto destinationReader = (move->hasValueType(ValueType::LOCAL) &&
                                         move->getOutput()->local()->getUsers(LocalUse::Type::READER).size() == 1) ?
                it.getBasicBlock()->findWalkerForInstruction(
                    *move->getOutput()->local()->getUsers(LocalUse::Type::READER).begin(),
                    it.getBasicBlock()->walkEnd()) :
                Optional<InstructionWalker>{};

            if(move->getSource() == move->getOutput().value() && move->isSimpleMove())
            {
                if(move->signal == SIGNAL_NONE)
                {
                    CPPLOG_LAZY(
                        logging::Level::DEBUG, log << "Removing obsolete move: " << it->to_string() << logging::endl);
                    it.erase();
                    flag = true;
                }
                else
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing obsolete move with nop: " << it->to_string() << logging::endl);
                    auto nop = new intermediate::Nop(intermediate::DelayType::WAIT_REGISTER, move->signal);
                    it.reset(nop);
                    flag = true;
                }
            }

            else if(!it->hasSideEffects() && sourceUsedOnce && destUsedOnceWithoutLiteral && destinationReader &&
                move->getSource().type == move->getOutput()->type)
            {
                // if the source is written only once and the destination is read only once, we can replace the uses of
                // the output with the input
                // XXX we need to check the type equality, since otherwise Reordering might re-order the reading before
                // the writing (if the local is written as type A and read as type B)
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Removing obsolete move by replacing uses of the output with the input: " << it->to_string()
                        << logging::endl);
                (*destinationReader)
                    ->replaceValue(move->getOutput().value(), move->getSource(), LocalUse::Type::READER);
                if((*destinationReader).get<intermediate::MoveOperation>())
                    (*destinationReader)->addDecorations(intermediate::forwardDecorations(it->decoration));
                it.erase();
                // to not skip the next instruction
                it.previousInBlock();
                flag = true;
            }
            else if(it->hasValueType(ValueType::REGISTER) && sourceUsedOnce && sourceWriter &&
                (!(*sourceWriter)->hasSideEffects()
                    // FIXME this re-orders UNIFORM reads (e.g. in test_branches.cl) ||
                    // !((*sourceWriter)->signal.hasSideEffects() || (*sourceWriter)->doesSetFlag()))
                    ) &&
                !it->signal.hasSideEffects() &&
                // TODO don't know why this does not work (maybe because of some other optimization applied to the
                // result?), but rewriting moves to rotation registers screw up the
                // TestVectorFunctions#testShuffle2Vector16 test
                !it->writesRegister(REG_REPLICATE_ALL) && !it->writesRegister(REG_REPLICATE_QUAD))
            {
                // if the source is only used once (by this move) and the destination is a register, we can replace this
                // move by the operation calculating the source  This optimization can save almost one instruction per
                // VPM write/VPM address write
                // TODO This could potentially lead to far longer usage-ranges for operands of sourceWriter and
                // therefore to register conflicts
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing obsolete move with instruction calculating its source: " << it->to_string()
                        << logging::endl);
                auto output = it->getOutput();
                auto setFlags = it->setFlags;
                auto sourceDecorations = intermediate::forwardDecorations((*sourceWriter)->decoration);
                it.reset(sourceWriter->release());
                sourceWriter->erase();
                it->setOutput(std::move(output));
                it->setSetFlags(setFlags);
                it->addDecorations(sourceDecorations);
                flag = true;
            }
            else if(move->getSource().checkRegister() && destUsedOnce &&
                (destUsedOnceWithoutLiteral || has_flag(move->getSource().reg().file, RegisterFile::PHYSICAL_ANY) ||
                    has_flag(move->getSource().reg().file, RegisterFile::ACCUMULATOR)) &&
                destinationReader && !move->signal.hasSideEffects() && move->setFlags == SetFlag::DONT_SET &&
                !(*destinationReader)->hasUnpackMode() && (*destinationReader)->conditional == COND_ALWAYS &&
                !(*destinationReader)->readsRegister(move->getSource().reg()) &&
                isNoReadBetween(it, destinationReader.value(), move->getSource().reg()) &&
                /* Tests have shown that an instruction cannot read and write VPM at the same time */
                (!move->getSource().hasRegister(REG_VPM_IO) ||
                    !(*destinationReader)->getOutput()->hasRegister(REG_VPM_IO)))
            {
                // if the source is a register, the output is only used once, this instruction has no signals/sets no
                // flags, the output consumer does not also read this move's source and there is no read of the source
                // between the move and the consumer, the consumer can directly use the register moved here
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing obsolete move by inserting the source into the instruction consuming its result: "
                        << it->to_string() << logging::endl);
                const Value newInput(move->getSource().reg(), move->getOutput()->type);
                const Local* oldLocal = move->getOutput()->local();
                for(std::size_t i = 0; i < (*destinationReader)->getArguments().size(); ++i)
                {
                    if((*destinationReader)->assertArgument(i).hasLocal(oldLocal))
                        (*destinationReader)->setArgument(i, std::move(newInput));
                }
                if(oldLocal->residesInMemory() && (*destinationReader)->hasValueType(ValueType::LOCAL) &&
                    (*destinationReader)->getOutput()->local()->getBase(true) ==
                        (*destinationReader)->getOutput()->local())
                    (*destinationReader)->getOutput()->local()->reference =
                        std::make_pair(const_cast<Local*>(oldLocal), ANY_ELEMENT);
                it.erase();
                flag = true;
                // to not skip the next instruction
                it.previousInBlock();
            }
        }
        it.nextInMethod();
    }

    return flag;
}

bool optimizations::eliminateRedundantBitOp(const Module& module, Method& method, const Configuration& config)
{
    bool replaced = false;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        auto op = it.get<intermediate::Operation>();
        if(op && op->isSimpleOperation())
        {
            if(op->op == OP_AND)
            {
                // and v1, v2, v3 => and v1, v2, v4
                // and v4, v1, v2    mov v4, v1
                //
                // and v1, v2, v3 => and v1, v2, v3
                // or  v4, v1, v2    mov v4, v2
                auto foundAnd = [&](Local* out, Local* in, InstructionWalker walker) {
                    auto it = walker.copy().nextInBlock();
                    while(!it.isEndOfBlock())
                    {
                        auto op2 = it.get<intermediate::Operation>();
                        if(op2 && op2->op == OP_AND && op2->readsLocal(out) && op2->readsLocal(in))
                        {
                            auto mov = new intermediate::MoveOperation(
                                op2->getOutput().value(), out->createReference(), op2->conditional, op2->setFlags);
                            it.reset(mov);
                        }
                        else if(op2 && op2->op == OP_OR && op2->readsLocal(out) && op2->readsLocal(in))
                        {
                            auto mov = new intermediate::MoveOperation(
                                op2->getOutput().value(), in->createReference(), op2->conditional, op2->setFlags);
                            it.reset(mov);
                        }

                        it.nextInBlock();
                    }
                };

                const auto& arg0 = op->assertArgument(0);
                const auto& arg1 = op->assertArgument(1);

                if(op->getOutput() && op->getOutput()->checkLocal())
                {
                    auto out = op->getOutput().value().local();

                    if(arg0.checkLocal())
                        foundAnd(out, arg0.local(), it);
                    if(arg1.checkLocal())
                        foundAnd(out, arg1.local(), it);
                }
            };

            if(op->op == OP_OR)
            {
                // or  v1, v2, v3 => or  v1, v2, v4
                // and v4, v1, v2    mov v4, v2
                //
                // or  v1, v2, v3 => or  v1, v2, v3
                // or  v4, v1, v2    mov v4, v1
                auto foundOr = [&](Local* out, Local* in, InstructionWalker walker) {
                    auto it = walker.copy().nextInBlock();
                    while(!it.isEndOfBlock())
                    {
                        auto op2 = it.get<intermediate::Operation>();
                        if(op2 && op2->op == OP_AND && op2->readsLocal(out) && op2->readsLocal(in))
                        {
                            auto mov = new intermediate::MoveOperation(
                                op2->getOutput().value(), in->createReference(), op2->conditional, op2->setFlags);
                            replaced = true;
                            it.reset(mov);
                        }
                        else if(op2 && op2->op == OP_OR && op2->readsLocal(out) && op2->readsLocal(in))
                        {
                            auto mov = new intermediate::MoveOperation(
                                op2->getOutput().value(), out->createReference(), op2->conditional, op2->setFlags);
                            replaced = true;
                            it.reset(mov);
                        }

                        it.nextInBlock();
                    }
                };

                const auto& arg0 = op->assertArgument(0);
                const auto& arg1 = op->assertArgument(1);

                if(op->getOutput() && op->getOutput()->checkLocal())
                {
                    auto out = op->getOutput()->local();

                    if(arg0.checkLocal())
                        foundOr(out, arg0.local(), it);
                    if(arg1.checkLocal())
                        foundOr(out, arg1.local(), it);
                }
            }
        }

        it.nextInMethod();
    }

    return replaced;
}

bool optimizations::eliminateCommonSubexpressions(const Module& module, Method& method, const Configuration& config)
{
    bool replacedSomething = false;
    for(auto& block : method)
    {
        // we do not run the whole analysis in front, but only the next step to save on memory usage
        // For that purpose, we also override the previous expressions on every step
        analysis::AvailableExpressionAnalysis::Cache cache{};
        analysis::AvailableExpressions expressions{};
        FastMap<const Local*, Expression> calculatingExpressions{};

        for(auto it = block.walk(); !it.isEndOfBlock(); it.nextInBlock())
        {
            if(!it.has())
                continue;
            Optional<Expression> expr;
            std::tie(expressions, expr) = analysis::AvailableExpressionAnalysis::analyzeAvailableExpressions(
                it.get(), expressions, cache, config.additionalOptions.maxCommonExpressionDinstance);
            if(expr)
            {
                Expression newExpr = expr.value();
                if(it->hasValueType(ValueType::LOCAL))
                {
                    calculatingExpressions.erase(it->getOutput()->local());
                    calculatingExpressions.emplace(it->getOutput()->local(), expr.value());
                }
                auto exprIt = expressions.find(expr.value());
                // replace instruction with matching expression, if the expression is not constant (no use replacing
                // loading of constants with copies of a local initialized with a constant)
                if(exprIt != expressions.end() && exprIt->second.first != it.get() && !expr->getConstantExpression())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Found common subexpression: " << it->to_string() << " is the same as "
                            << exprIt->second.first->to_string() << logging::endl);
                    it.reset(new intermediate::MoveOperation(
                        it->getOutput().value(), exprIt->second.first->getOutput().value()));
                    replacedSomething = true;
                }
                else if((newExpr = expr->combineWith(calculatingExpressions)) != expr)
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Rewriting expression '" << expr->to_string() << "' to '" << newExpr.to_string() << "'"
                            << logging::endl);

                    if(exprIt != expressions.end() && exprIt->second.first == it.get())
                        // reset this expression, since the mapped instruction will be overwritten
                        expressions.erase(exprIt);

                    if(newExpr.code.numOperands == 1)
                        it.reset(new intermediate::Operation(newExpr.code, it->getOutput().value(), newExpr.arg0));
                    else
                        it.reset(new intermediate::Operation(
                            newExpr.code, it->getOutput().value(), newExpr.arg0, newExpr.arg1.value()));
                    it->setUnpackMode(newExpr.unpackMode);
                    it->setPackMode(newExpr.packMode);
                    it->addDecorations(newExpr.deco);

                    if(it->hasValueType(ValueType::LOCAL))
                        calculatingExpressions.at(it->getOutput()->local()) = newExpr;
                    replacedSomething = true;
                }
            }
        }
    }
    return replacedSomething;
}

InstructionWalker optimizations::rewriteConstantSFUCall(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(!it.has() || !it->hasValueType(ValueType::REGISTER) || !it->getOutput()->reg().isSpecialFunctionsUnit())
        return it;

    auto constantValue = it->precalculate(3).first;
    auto result = constantValue ? periphery::precalculateSFU(it->getOutput()->reg(), constantValue.value()) : NO_VALUE;
    if(result)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Replacing SFU call with constant input '" << it->to_string()
                << "' to move of result: " << result->to_string() << logging::endl);

        // remove this instruction, 2 NOPs (with SFU type) and rewrite the result
        it.erase();
        unsigned numDelays = 2;
        while(numDelays != 0 && !it.isEndOfBlock())
        {
            if(it.get<intermediate::Nop>() &&
                it.get<const intermediate::Nop>()->type == intermediate::DelayType::WAIT_SFU)
            {
                it.erase();
                --numDelays;
            }
            else
                it.nextInBlock();
        }

        if(it.isEndOfBlock())
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find both NOPs for waiting for SFU result");

        while(!it.isEndOfBlock())
        {
            if(it->readsRegister(REG_SFU_OUT))
            {
                it.reset((new intermediate::MoveOperation(it->getOutput().value(), result.value()))
                             ->copyExtrasFrom(it.get()));
                break;
            }
            else
                it.nextInBlock();
        }

        if(it.isEndOfBlock())
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find the reading of the SFU result");

        // to not skip optimizing the resulting instruction
        it.previousInBlock();
    }
    return it;
}
