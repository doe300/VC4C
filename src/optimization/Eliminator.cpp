/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Eliminator.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../analysis/DataDependencyGraph.h"
#include "log.h"

#include <algorithm>
#include <list>
#include <map>

using namespace vc4c;
using namespace vc4c::optimizations;

void optimizations::eliminateDeadStore(const Module& module, Method& method, const Configuration& config)
{
    // TODO (additionally or instead of this) walk through locals, check whether they are never read and writings have
    // no side-effects  then walk through all writings of such locals and remove them (example:
    // ./testing/test_vpm_write.cl)
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        intermediate::IntermediateInstruction* instr = it.get();
        // fail-fast on all not-supported instruction types
        // also skip all instructions writing to non-locals (registers)
        if(it.get() && !it.has<intermediate::Branch>() && !it.has<intermediate::BranchLabel>() &&
            !it.has<intermediate::SemaphoreAdjustment>() && instr->hasValueType(ValueType::LOCAL))
        {
            intermediate::Operation* op = it.get<intermediate::Operation>();
            intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
            intermediate::LoadImmediate* load = it.get<intermediate::LoadImmediate>();

            // check whether the output of an instruction is never read
            // only check for ALU-operations and loads, if no flags are set and no special signals are sent
            if((move != nullptr || op != nullptr || load != nullptr) && !instr->hasSideEffects())
            {
                const Local* dest = instr->getOutput()->local;
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
                        logging::debug() << "Removing instruction " << instr->to_string()
                                         << ", since its output is never read" << logging::endl;
                        it.erase();
                        // if we removed this instruction, maybe the previous one can be removed too??
                        it.previousInBlock();
                        continue;
                    }
                }
            }
            if(move != nullptr)
            {
                if(move->getSource().hasType(ValueType::LOCAL) && move->getOutput()->hasType(ValueType::LOCAL) &&
                    !move->hasConditionalExecution() && !move->hasPackMode() && !move->hasSideEffects() &&
                    dynamic_cast<intermediate::VectorRotation*>(move) == nullptr)
                {
                    // if for a move, neither the input-local nor the output-local are written to afterwards,
                    // XXX or the input -local is only written after the last use of the output-local
                    // both locals can be the same and the move can be removed

                    const Local* inLoc = move->getSource().local;
                    const Local* outLoc = move->getOutput()->local;
                    // for instruction added by phi-elimination, the result could have been written to (with a different
                    // source) previously, so check
                    bool isWrittenTo = !outLoc->getUsers(LocalUse::Type::WRITER).empty();
                    if(!isWrittenTo && inLoc->type == outLoc->type)
                    {
                        // TODO what if both locals are written before (and used differently), possible??
                        logging::debug() << "Merging locals " << inLoc->to_string() << " and " << outLoc->to_string()
                                         << " since they contain the same value" << logging::endl;
                        outLoc->forUsers(LocalUse::Type::READER, [inLoc, outLoc](const LocalUser* instr) -> void {
                            // change outLoc to inLoc
                            bool outLocFound = false;
                            for(std::size_t i = 0; i < instr->getArguments().size(); ++i)
                            {
                                Value tmp = instr->assertArgument(i);
                                if(tmp.hasLocal(outLoc))
                                {
                                    tmp = Value(inLoc, tmp.type);
                                    const_cast<LocalUser*>(instr)->setArgument(i, tmp);
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
                        continue;
                    }
                }
            }
        }
        it.nextInMethod();
    }
    // remove unused locals. This is actually not required, but gives us some feedback about the effect of this
    // optimization
    method.cleanLocals();
}

bool optimizations::eliminateUselessInstruction(
    const Module& module, Method& method, InstructionWalker& it, const Configuration& config)
{
    bool replaced = false;
    intermediate::Operation* op = it.get<intermediate::Operation>();
    intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
    if(op != nullptr)
    {
        if(!op->hasSideEffects() && !op->hasPackMode() && !op->hasUnpackMode())
        {
            // improve by pre-calculating first and second arguments
            const Value firstArg =
                (op->getFirstArg().getSingleWriter() != nullptr ? op->getFirstArg().getSingleWriter()->precalculate(3) :
                                                                  NO_VALUE)
                    .value_or(op->getFirstArg());
            const Optional<Value> secondArg =
                (op->getSecondArg() && op->assertArgument(1).getSingleWriter() != nullptr ?
                        op->assertArgument(1).getSingleWriter()->precalculate(3) :
                        NO_VALUE)
                    .orOther(op->getSecondArg());

            Optional<Value> rightIdentity = OpCode::getRightIdentity(op->op);
            Optional<Value> leftIdentity = OpCode::getLeftIdentity(op->op);
            Optional<Value> rightAbsorbing = OpCode::getRightAbsorbingElement(op->op);
            Optional<Value> leftAbsorbing = OpCode::getLeftAbsorbingElement(op->op);

            // one of the operands is the absorbing element, operation can be replaced with move
            if(leftAbsorbing && firstArg.hasLiteral(leftAbsorbing->getLiteralValue().value()))
            {
                logging::debug() << "Replacing obsolete " << op->to_string() << " with move" << logging::endl;
                it.reset(new intermediate::MoveOperation(
                    op->getOutput().value(), leftAbsorbing.value(), op->conditional, op->setFlags));
                replaced = true;
            }
            else if(rightAbsorbing && secondArg && secondArg->hasLiteral(rightAbsorbing->getLiteralValue().value()))
            {
                logging::debug() << "Replacing obsolete " << op->to_string() << " with move" << logging::endl;
                it.reset(new intermediate::MoveOperation(
                    op->getOutput().value(), rightAbsorbing.value(), op->conditional, op->setFlags));
                replaced = true;
            }
            // writes into the input -> can be removed, if it doesn't do anything
            else if(op->getOutput() && op->getOutput().value() == op->getFirstArg())
            {
                // check whether second-arg exists and does nothing
                if(rightIdentity && secondArg && secondArg->hasLiteral(rightIdentity->getLiteralValue().value()))
                {
                    logging::debug() << "Removing obsolete " << op->to_string() << logging::endl;
                    it.erase();
                    // don't skip next instruction
                    it.previousInBlock();
                    replaced = true;
                }
            }
            else if(op->getOutput() && op->getSecondArg() && op->getOutput().value() == op->assertArgument(1))
            {
                // check whether first-arg does nothing
                if(leftIdentity && firstArg.hasLiteral(leftIdentity->getLiteralValue().value()))
                {
                    logging::debug() << "Removing obsolete " << op->to_string() << logging::endl;
                    it.erase();
                    // don't skip next instruction
                    it.previousInBlock();
                    replaced = true;
                }
            }
            else // writes to another local -> can be replaced with move
            {
                // check whether second argument exists and does nothing
                if(rightIdentity && secondArg && secondArg->hasLiteral(rightIdentity->getLiteralValue().value()))
                {
                    logging::debug() << "Replacing obsolete " << op->to_string() << " with move" << logging::endl;
                    it.reset(new intermediate::MoveOperation(
                        op->getOutput().value(), op->getFirstArg(), op->conditional, op->setFlags));
                    replaced = true;
                }
                // check whether first argument does nothing
                else if(leftIdentity && secondArg && firstArg.hasLiteral(leftIdentity->getLiteralValue().value()))
                {
                    logging::debug() << "Replacing obsolete " << op->to_string() << " with move" << logging::endl;
                    it.reset(new intermediate::MoveOperation(
                        op->getOutput().value(), op->assertArgument(1), op->conditional, op->setFlags));
                    replaced = true;
                }
            }
        }
        // TODO trunc to int32/float
    }
    else if(move != nullptr)
    {
        if(move->getSource() == move->getOutput().value() && !move->hasSideEffects() && !move->hasPackMode() &&
            !move->hasUnpackMode() && !it.has<intermediate::VectorRotation>())
        {
            // skip copying to same, if no flags/signals/pack and unpack-modes are set
            logging::debug() << "Removing obsolete " << move->to_string() << logging::endl;
            it.erase();
            // don't skip next instruction
            it.previousInBlock();
            replaced = true;
        }
        if(it.has<intermediate::VectorRotation>() && move->getSource().isLiteralValue())
        {
            // replace rotation of constant with move
            logging::debug() << "Replacing obsolete " << move->to_string() << " with move" << logging::endl;
            it.reset(new intermediate::MoveOperation(
                move->getOutput().value(), move->getSource(), move->conditional, move->setFlags));
            replaced = true;
        }
    }

    return replaced;
}

InstructionWalker optimizations::eliminateUselessBranch(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // eliminates branches to the next instruction to save up to 4 instructions (1 branch + 3 NOP)
    intermediate::Branch* branch = it.get<intermediate::Branch>();
    if(branch != nullptr)
    {
        // eliminate branches to the next instruction, such branches are e.g. introduced by method-inlining
        auto nextIt = it.copy().nextInMethod();
        // FIXME removing conditional branches to next instruction hangs QPU (e.g. because of successive PHI-writes not
        // being skipped?)
        //		while(!nextIt.isEndOfMethod())
        if(!nextIt.isEndOfMethod())
        {
            intermediate::BranchLabel* label = nextIt.get<intermediate::BranchLabel>();
            //			intermediate::Branch* br = nextIt.get<intermediate::Branch>();
            if(label != nullptr)
            {
                if(label->getLabel() == branch->getTarget())
                {
                    logging::debug() << "Removing branch to next instruction: " << branch->to_string() << logging::endl;
                    it = it.erase();
                    // don't skip next instruction
                    it.previousInMethod();
                }
                //				break;
            }
            /*			else if(br != nullptr)
                        {
                            //if the following branch has the same condition with inverted flags (either-or-branch), it
               can be skipped if(!(br->conditional.isInversionOf(branch->conditional) && br->getCondition() ==
               branch->getCondition()))
                            {
                                //otherwise, abort this optimization
                                break;
                            }
                        }
                        nextIt.nextInMethod();
            */
        }
    }
    return it;
}

bool optimizations::calculateConstantInstruction(
    const Module& module, Method& method, InstructionWalker& it, const Configuration& config)
{
    bool replaced = false;
    intermediate::Operation* op = it.get<intermediate::Operation>();
    if(op != nullptr && !op->hasUnpackMode())
    {
        // calculations with literals can be pre-calculated
        if(op->getFirstArg().getLiteralValue() && (!op->getSecondArg() || op->assertArgument(1).getLiteralValue()))
        {
            if(op->conditional != COND_ALWAYS && op->opCode == "xor" && op->getSecondArg().is(op->getFirstArg()))
            {
                // skip "xor ?, true, true", so it can be optimized (combined with "move ?, true") afterwards
                // also skip any "xor ?, val, val", since they are created on purpose (by combineSelectionWithZero to
                // allow for combination with the other case)
                return false;
            }
            const Optional<Value> value = op->precalculate(3);
            if(value)
            {
                logging::debug() << "Replacing '" << op->to_string() << "' with constant value: " << value.to_string()
                                 << logging::endl;
                it.reset((new intermediate::MoveOperation(op->getOutput().value(), value.value()))->copyExtrasFrom(op));
                replaced = true;
            }
        }
    }

    return replaced;
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
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to map all phi-options to valid basic-blocks");
        }
        logging::debug() << "Inserting 'move' into end of basic-block: " << pair.first->name << logging::endl;
        // make sure, moves are inserted before the outgoing branches
        InstructionWalker it = bb->end();
        ConditionCode jumpCondition = COND_ALWAYS;
        Value condition(UNDEFINED_VALUE);
        while(it.copy().previousInBlock().has<intermediate::Branch>())
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
    }
}

void optimizations::eliminatePhiNodes(const Module& module, Method& method, const Configuration& config)
{
    // Search for all phi-nodes and insert all mapped instructions to the end of the corresponding basic block
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        const intermediate::PhiNode* phiNode = it.get<intermediate::PhiNode>();
        if(phiNode != nullptr)
        {
            // 2) map the phi-node to the move-operations per predecessor-label
            logging::debug() << "Eliminating phi-node by inserting moves: " << it->to_string() << logging::endl;
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
    if(it.has<intermediate::Return>())
    {
        const Local* target = method.findLocal(BasicBlock::LAST_BLOCK);
        if(target == nullptr)
        {
            target = method.findOrCreateLocal(TYPE_LABEL, BasicBlock::LAST_BLOCK);
            method.appendToEnd(new intermediate::BranchLabel(*target));
        }
        logging::debug() << "Replacing return in kernel-function with branch to end-label" << logging::endl;
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
        first.nextInBlock();
    }
    return true;
}

bool optimizations::translateToMove(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    bool flag = false;
    while(!it.isEndOfMethod())
    {
        auto const op = it.get<intermediate::Operation>();
        if(op &&
            (op->op == OP_AND || op->op == OP_OR || op->op == OP_V8MAX || op->op == OP_V8MIN || op->op == OP_MAX ||
                op->op == OP_MIN) &&
            op->getFirstArg() == op->assertArgument(1))
        {
            auto move = new intermediate::MoveOperation(
                op->getOutput().value(), op->getFirstArg(), op->conditional, op->setFlags);
            logging::debug() << "Replacing obsolete instruction with move: " << op->to_string() << logging::endl;
            it.reset(move);
            flag = true;
        }

        it.nextInMethod();
    }

    return flag;
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
        if(op && !it.has<intermediate::VectorRotation>() && !op->hasConditionalExecution() && !op->hasPackMode() &&
            !op->hasUnpackMode() && op->getOutput().has_value() && (op->getSource().valueType != ValueType::REGISTER) &&
            (op->getOutput().value().valueType != ValueType::REGISTER))
        {
            auto it2 = it.copy().nextInBlock();
            auto oldValue = op->getOutput().value();
            const auto& newValue = op->getSource();
            while(!it2.isEndOfBlock())
            {
                bool replacedThisInstruction = false;
                for(auto arg : it2->getArguments())
                {
                    if(arg == oldValue && arg.valueType != ValueType::LITERAL &&
                        arg.valueType != ValueType::SMALL_IMMEDIATE)
                    {
                        replaced = true;
                        replacedThisInstruction = true;
                        it2->replaceValue(oldValue, newValue, LocalUse::Type::READER);
                    }
                }

                if(replacedThisInstruction)
                    calculateConstantInstruction(module, method, it2, config);

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
    bool flag = false;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(it.has<intermediate::MoveOperation>() &&
            !it->hasDecoration(intermediate::InstructionDecorations::PHI_NODE) && !it->hasPackMode() &&
            !it->hasUnpackMode() && it->conditional == COND_ALWAYS && !it.has<intermediate::VectorRotation>())
        {
            // skip PHI-nodes, since they are read in another basic block (and the output is written more than once
            // anyway) as well as modification of the value, conditional execution and vector-rotations
            const intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();

            // the source is written and read only once
            bool sourceUsedOnce = move->getSource().getSingleWriter() != nullptr &&
                move->getSource().local->getUsers(LocalUse::Type::READER).size() == 1;
            // the destination is written and read only once (and not in combination with a literal value, to not
            // introduce register conflicts)
            bool destUsedOnce = move->hasValueType(ValueType::LOCAL) && move->getOutput()->getSingleWriter() == move &&
                move->getOutput()->local->getUsers(LocalUse::Type::READER).size() == 1;
            bool destUsedOnceWithoutLiteral =
                destUsedOnce && !(*move->getOutput()->local->getUsers(LocalUse::Type::READER).begin())->readsLiteral();

            auto sourceWriter = (move->getSource().getSingleWriter() != nullptr) ?
                it.getBasicBlock()->findWalkerForInstruction(move->getSource().getSingleWriter(), it) :
                Optional<InstructionWalker>{};
            auto destinationReader = (move->hasValueType(ValueType::LOCAL) &&
                                         move->getOutput()->local->getUsers(LocalUse::Type::READER).size() == 1) ?
                it.getBasicBlock()->findWalkerForInstruction(
                    *move->getOutput()->local->getUsers(LocalUse::Type::READER).begin(), it.getBasicBlock()->end()) :
                Optional<InstructionWalker>{};

            if(!move->hasPackMode() && !move->hasUnpackMode() && move->getSource() == move->getOutput().value() &&
                !move->doesSetFlag() && !move->hasSideEffects())
            {
                if(move->signal == SIGNAL_NONE)
                {
                    logging::debug() << "Removing obsolete move: " << it->to_string() << logging::endl;
                    it.erase();
                    flag = true;
                }
                else
                {
                    logging::debug() << "Removing obsolete move with nop: " << it->to_string() << logging::endl;
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
                logging::debug() << "Removing obsolete move by replacing uses of the output with the input: "
                                 << it->to_string() << logging::endl;
                (*destinationReader)
                    ->replaceValue(move->getOutput().value(), move->getSource(), LocalUse::Type::READER);
                it.erase();
                // to not skip the next instruction
                it.previousInBlock();
                flag = true;
            }
            else if(it->hasValueType(ValueType::REGISTER) && sourceUsedOnce && sourceWriter &&
                (!(*sourceWriter)->hasSideEffects() ||
                    !((*sourceWriter)->signal.hasSideEffects() || (*sourceWriter)->doesSetFlag())) &&
                !it->signal.hasSideEffects())
            {
                // if the source is only used once (by this move) and the destination is a register, we can replace this
                // move by the operation calculating the source  This optimization can save almost one instruction per
                // VPM write/VPM address write
                // TODO This could potentially lead to far longer usage-ranges for operands of sourceWriter and
                // therefore to register conflicts
                logging::debug() << "Replacing obsolete move with instruction calculating its source: "
                                 << it->to_string() << logging::endl;
                auto output = it->getOutput();
                auto setFlags = it->setFlags;
                it.reset(sourceWriter->release());
                sourceWriter->erase();
                it->setOutput(output);
                it->setSetFlags(setFlags);
                flag = true;
            }
            else if(move->getSource().hasType(ValueType::REGISTER) && destUsedOnce &&
                (destUsedOnceWithoutLiteral || has_flag(move->getSource().reg.file, RegisterFile::PHYSICAL_ANY) ||
                    has_flag(move->getSource().reg.file, RegisterFile::ACCUMULATOR)) &&
                destinationReader && !move->signal.hasSideEffects() && move->setFlags == SetFlag::DONT_SET &&
                !(*destinationReader)->hasUnpackMode() && (*destinationReader)->conditional == COND_ALWAYS &&
                !(*destinationReader)->readsRegister(move->getSource().reg) &&
                isNoReadBetween(it, destinationReader.value(), move->getSource().reg))
            {
                // if the source is a register, the output is only used once, this instruction has no signals/sets no
                // flags, the output consumer does not also read this move's source and there is no read of the source
                // between the move and the consumer, the consumer can directly use the register moved here
                logging::debug()
                    << "Replacing obsolete move by inserting the source into the instruction consuming its result: "
                    << it->to_string() << logging::endl;
                const Value newInput(move->getSource().reg, move->getOutput()->type);
                const Local* oldLocal = move->getOutput()->local;
                for(std::size_t i = 0; i < (*destinationReader)->getArguments().size(); ++i)
                {
                    if((*destinationReader)->assertArgument(i).hasLocal(oldLocal))
                        (*destinationReader)->setArgument(i, newInput);
                }
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
        if(it.get() && !it->hasSideEffects() && !it->hasPackMode() && !it->hasUnpackMode())
        {
            auto op = it.get<intermediate::Operation>();
            if(op && op->op == OP_AND && !op->hasUnpackMode() && !op->hasPackMode())
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
                auto out = op->getOutput().value().local;

                if(arg0.valueType == ValueType::LOCAL)
                    foundAnd(out, arg0.local, it);
                if(arg1.valueType == ValueType::LOCAL)
                    foundAnd(out, arg1.local, it);
            };

            if(op && op->op == OP_OR && !op->hasUnpackMode() && !op->hasPackMode())
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
                auto out = op->getOutput().value().local;

                if(arg0.valueType == ValueType::LOCAL)
                    foundOr(out, arg0.local, it);
                if(arg1.valueType == ValueType::LOCAL)
                    foundOr(out, arg1.local, it);
            }
        }

        it.nextInMethod();
    }

    return replaced;
}
