/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Combiner.h"

#include "../InstructionWalker.h"
#include "../analysis/ValueRange.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "Eliminator.h"
#include "log.h"

#include <algorithm>
#include <cstdlib>
#include <memory>

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

static const std::string combineLoadLiteralsThreshold = "combine-load-threshold";

bool optimizations::simplifyBranches(const Module& module, Method& method, const Configuration& config)
{
    bool hasChanged = false;
    for(auto it = method.walkAllInstructions(); !it.isEndOfBlock(); it.nextInMethod())
    {
        Branch* thisBranch = it.get<Branch>();
        if(thisBranch != nullptr)
        {
            // eliminates branches to the next instruction to save up to 4 instructions (1 branch + 3 NOP)
            // eliminate branches to the next instruction, such branches are e.g. introduced by method-inlining
            auto nextIt = it.copy().nextInMethod();
            // FIXME removing conditional branches to next instruction hangs QPU (e.g. because of successive PHI-writes
            // not being skipped?)
            //		while(!nextIt.isEndOfMethod())
            if(!nextIt.isEndOfMethod())
            {
                intermediate::BranchLabel* label = nextIt.get<intermediate::BranchLabel>();
                // intermediate::Branch* br = nextIt.get<intermediate::Branch>();
                if(label != nullptr && label->getLabel() == thisBranch->getTarget())
                {
                    logging::debug() << "Removing branch to next instruction: " << thisBranch->to_string()
                                     << logging::endl;
                    it = it.erase();
                    // don't skip next instruction
                    it.previousInMethod();
                    // skip second part below
                    continue;
                }
                /*			else if(br != nullptr)
                            {
                                //if the following branch has the same condition with inverted flags (either-or-branch),
                   it can be skipped if(!(br->conditional.isInversionOf(branch->conditional) && br->getCondition() ==
                   branch->getCondition()))
                                {
                                    //otherwise, abort this optimization
                                    break;
                                }
                            }
                            nextIt.nextInMethod();
                */
            }
            // skip all following labels
            while(!nextIt.isEndOfMethod() && nextIt.has<BranchLabel>())
                nextIt.nextInMethod();
            if(nextIt.isEndOfMethod())
                return hasChanged;
            Branch* nextBranch = nextIt.get<Branch>();
            if(nextBranch != nullptr)
            {
                if(thisBranch->getTarget() != nextBranch->getTarget())
                    continue;
                // for now, only remove unconditional branches
                if(!thisBranch->isUnconditional() || !nextBranch->isUnconditional())
                    continue;
                logging::debug() << "Removing duplicate branch to same target: " << thisBranch->to_string()
                                 << logging::endl;
                it = it.erase();
                // don't skip next instruction
                it.previousInMethod();
                hasChanged = true;
            }
        }
    }
    return hasChanged;
}

using MergeCondition = std::function<bool(Operation*, Operation*, MoveOperation*, MoveOperation*)>;
static const std::vector<MergeCondition> mergeConditions = {
    // check both instructions can be combined and are actually mapped to machine code
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        if(firstOp != nullptr && !(firstOp->canBeCombined && firstOp->mapsToASMInstruction()))
            return false;
        if(firstMove != nullptr && !(firstMove->canBeCombined && firstMove->mapsToASMInstruction()))
            return false;
        if(secondOp != nullptr && !(secondOp->canBeCombined && secondOp->mapsToASMInstruction()))
            return false;
        if(secondMove != nullptr && !(secondMove->canBeCombined && secondMove->mapsToASMInstruction()))
            return false;
        return true;
    },
    // check neither instruction is a vector rotation
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        return (firstMove == nullptr || dynamic_cast<VectorRotation*>(firstMove) == nullptr) &&
            (secondMove == nullptr || dynamic_cast<VectorRotation*>(secondMove) == nullptr);
    },
    // check both instructions use different ALUs
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
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
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        if(firstOp != nullptr &&
            (firstOp->hasValueType(ValueType::REGISTER) || firstOp->getFirstArg().hasRegister() ||
                (firstOp->getSecondArg() && firstOp->assertArgument(1).hasRegister())))
            return false;
        else if(firstMove != nullptr &&
            (firstMove->hasValueType(ValueType::REGISTER) || firstMove->getSource().hasRegister()))
            return false;
        else if(secondOp != nullptr &&
            (secondOp->hasValueType(ValueType::REGISTER) || secondOp->getFirstArg().hasRegister() ||
                (secondOp->getSecondArg() && secondOp->assertArgument(1).hasRegister())))
            return false;
        else if(secondMove != nullptr &&
            (secondMove->hasValueType(ValueType::REGISTER) || secondMove->getSource().hasRegister()))
            return false;
        return true;
    },
    // check second operation using the result of the first
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
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
    // check operations use same output and do not have inverted conditions
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        Value outFirst(UNDEFINED_VALUE);
        ConditionCode condFirst = COND_ALWAYS;
        if(firstOp != nullptr && firstOp->getOutput())
        {
            outFirst = firstOp->getOutput().value();
            condFirst = firstOp->conditional;
        }
        else if(firstMove != nullptr && firstMove->getOutput())
        {
            outFirst = firstMove->getOutput().value();
            condFirst = firstMove->conditional;
        }
        Value outSecond(UNDEFINED_VALUE);
        ConditionCode condSecond = COND_ALWAYS;
        if(secondOp != nullptr && secondOp->getOutput())
        {
            outSecond = secondOp->getOutput().value();
            condSecond = secondOp->conditional;
        }
        else if(secondMove != nullptr && secondMove->getOutput())
        {
            outSecond = secondMove->getOutput().value();
            condSecond = secondMove->conditional;
        }
        if(outFirst == outSecond ||
            (outFirst.hasLocal() && outSecond.hasLocal() && outSecond.hasLocal(outFirst.local())))
        {
            if(!condSecond.isInversionOf(condFirst))
                return false;
        }
        return true;
    },
    // check first operation sets flags and second operation depends on them
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        SetFlag setsFlags = SetFlag::DONT_SET;
        bool usesFlags = false;
        ConditionCode firstCond = COND_ALWAYS;
        ConditionCode secondCond = COND_ALWAYS;
        if(firstOp != nullptr)
        {
            setsFlags = firstOp->setFlags;
            firstCond = firstOp->conditional;
        }
        else if(firstMove != nullptr)
        {
            setsFlags = firstMove->setFlags;
            firstCond = firstMove->conditional;
        }
        if(secondOp != nullptr)
        {
            usesFlags = secondOp->hasConditionalExecution();
            secondCond = secondOp->conditional;
        }
        else if(secondMove != nullptr)
        {
            usesFlags = secondMove->hasConditionalExecution();
            secondCond = secondMove->conditional;
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
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        ConditionCode firstCond = firstOp ? firstOp->conditional : firstMove->conditional;
        ConditionCode secondCond = secondOp ? secondOp->conditional : secondMove->conditional;
        if(firstCond.isInversionOf(secondCond))
        {
            // if they have inverted conditions, the ADD ALU can't set the flags, the MUL ALU is supposed to
            return true;
        }
        // XXX handle v8adds, can be on ADD and MUL ALU. Would need to make sure, flag-setting instruction is on ADD ALU
        if(firstOp != nullptr && firstOp->op.runsOnMulALU() && firstOp->setFlags == SetFlag::SET_FLAGS)
            return false;
        else if(secondOp != nullptr && secondOp->op.runsOnMulALU() && secondOp->setFlags == SetFlag::SET_FLAGS)
            return false;
        return true;
    },
    // check maximum 1 immediate value is used (both can use the same immediate value)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        Optional<SmallImmediate> immediate;
        if(firstOp != nullptr)
        {
            if(firstOp->getFirstArg().hasImmediate())
                immediate = firstOp->getFirstArg().immediate();
            if(firstOp->getSecondArg() && firstOp->assertArgument(1).hasImmediate())
                immediate = firstOp->assertArgument(1).immediate();
        }
        if(firstMove != nullptr)
        {
            if(firstMove->getSource().hasImmediate())
                immediate = firstMove->getSource().immediate();
        }
        if(immediate)
        {
            if(secondOp != nullptr)
            {
                if(secondOp->getFirstArg().hasImmediate() && !secondOp->getFirstArg().hasImmediate(immediate.value()))
                    return false;
                if(secondOp->getSecondArg() && secondOp->assertArgument(1).hasImmediate() &&
                    !secondOp->assertArgument(1).hasImmediate(immediate.value()))
                    return false;
            }
            if(secondMove != nullptr)
            {
                if(secondMove->getSource().hasImmediate() && !secondMove->getSource().hasImmediate(immediate.value()))
                    return false;
            }
        }
        return true;
    },
    // check a maximum of 2 inputs are read from
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        std::pair<Value, Value> inputs = std::make_pair(UNDEFINED_VALUE, UNDEFINED_VALUE);
        if(firstOp != nullptr)
        {
            inputs.first = firstOp->getFirstArg();
            if(firstOp->getSecondArg())
                inputs.second = firstOp->assertArgument(1);
        }
        if(firstMove != nullptr)
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
        if(secondMove != nullptr)
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
    // check at most 1 signal (including IMMEDIATE) is set, same for Un-/Pack
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        Signaling firstSignal = SIGNAL_NONE;
        Signaling secondSignal = SIGNAL_NONE;

        Unpack firstUnpack = UNPACK_NOP;
        Unpack secondUnpack = UNPACK_NOP;

        Pack firstPack = PACK_NOP;
        Pack secondPack = PACK_NOP;
        ConditionCode firstCond = firstOp ? firstOp->conditional : firstMove->conditional;
        ConditionCode secondCond = secondOp ? secondOp->conditional : secondMove->conditional;
        bool invertedConditions = firstCond.isInversionOf(secondCond);
        if(firstOp != nullptr)
        {
            firstSignal = firstOp->signal;
            if(firstOp->getFirstArg().getLiteralValue() ||
                (firstOp->getSecondArg() && firstOp->assertArgument(1).getLiteralValue()))
                firstSignal = SIGNAL_ALU_IMMEDIATE;
            firstPack = firstOp->packMode;
            firstUnpack = firstOp->unpackMode;
        }
        if(firstMove != nullptr)
        {
            firstSignal = firstMove->signal;
            if(firstMove->getSource().getLiteralValue())
                firstSignal = SIGNAL_ALU_IMMEDIATE;
            firstPack = firstMove->packMode;
            firstUnpack = firstMove->unpackMode;
        }
        if(secondOp != nullptr)
        {
            secondSignal = secondOp->signal;
            if(secondOp->getFirstArg().getLiteralValue() ||
                (secondOp->getSecondArg() && secondOp->assertArgument(1).getLiteralValue()))
                secondSignal = SIGNAL_ALU_IMMEDIATE;
            secondPack = secondOp->packMode;
            secondUnpack = secondOp->unpackMode;
        }
        if(secondMove != nullptr)
        {
            secondSignal = secondMove->signal;
            if(secondMove->getSource().getLiteralValue())
                secondSignal = SIGNAL_ALU_IMMEDIATE;
            secondPack = secondMove->packMode;
            secondUnpack = secondMove->unpackMode;
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
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool {
        // since boolean values are combined with ELEM_NUMBER (reg-file A) before conditional branch, they cannot be on
        // reg-file A  combining two of those can cause register-association errors
        // TODO find a way to fix this
        if(((firstOp != nullptr && firstOp->hasValueType(ValueType::LOCAL) &&
                firstOp->getOutput()->type == TYPE_BOOL) ||
               (firstMove != nullptr && firstMove->hasValueType(ValueType::LOCAL) &&
                   firstMove->getOutput()->type == TYPE_BOOL)))
        {
            if(((secondOp != nullptr && secondOp->hasValueType(ValueType::LOCAL) &&
                    secondOp->getOutput()->type == TYPE_BOOL) ||
                   (secondMove != nullptr && secondMove->hasValueType(ValueType::LOCAL) &&
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
                    bool conditionsMet = std::all_of(mergeConditions.begin(), mergeConditions.end(),
                        [op, nextOp, move, nextMove](
                            const MergeCondition& cond) -> bool { return cond(op, nextOp, move, nextMove); });
                    if(instr->hasValueType(ValueType::LOCAL) && nextInstr->hasValueType(ValueType::LOCAL))
                    {
                        // extra check, only combine writes to the same local, if local is only used within the next
                        // instruction  this is required, since we cannot write to a physical register from both ALUs,
                        // so the local needs to be on an accumulator
                        if(instr->getOutput()->local() == nextInstr->getOutput()->local() &&
                            !nextIt.getBasicBlock()->isLocallyLimited(
                                nextIt, instr->getOutput()->local(), config.additionalOptions.accumulatorThreshold))
                            conditionsMet = false;
                    }
                    if(instr->hasValueType(ValueType::LOCAL) || nextInstr->hasValueType(ValueType::LOCAL))
                    {
                        // also check that if the next instruction is a vector rotation, neither of the locals is being
                        // rotated there  since vector rotations can't rotate vectors which have been written in the
                        // instruction directly preceding it
                        auto checkIt = nextIt.copy().nextInBlock();
                        if(!checkIt.isEndOfBlock() && checkIt.has<VectorRotation>())
                        {
                            const Value& src = checkIt.get<VectorRotation>()->getSource();
                            if(instr->hasValueType(ValueType::LOCAL) && instr->getOutput() == src)
                                conditionsMet = false;
                            if(nextInstr->hasValueType(ValueType::LOCAL) && nextInstr->getOutput() == src)
                                conditionsMet = false;
                        }
                        // the next instruction MUST NOT unpack a value written to in one of the combined instructions
                        // equally, neither of the combined instructions is allowed to pack a value read in the
                        // following instructions
                        if(!checkIt.isEndOfBlock())
                        {
                            if(checkIt->unpackMode.hasEffect())
                            {
                                if(std::any_of(checkIt->getArguments().begin(), checkIt->getArguments().end(),
                                       [instr, nextInstr](const Value& val) -> bool {
                                           return val.hasLocal() &&
                                               (instr->writesLocal(val.local()) || nextInstr->writesLocal(val.local()));
                                       }))
                                {
                                    conditionsMet = false;
                                }
                            }
                            if(instr->packMode.hasEffect() && instr->hasValueType(ValueType::LOCAL) &&
                                checkIt->readsLocal(instr->getOutput()->local()))
                                conditionsMet = false;
                            if(nextInstr->packMode.hasEffect() && nextInstr->hasValueType(ValueType::LOCAL) &&
                                checkIt->readsLocal(nextInstr->getOutput()->local()))
                                conditionsMet = false;
                        }
                        // run previous checks also for the previous (before instr) instruction
                        // this time with inverted checks (since the order is inverted)
                        checkIt = it.copy().previousInBlock();
                        if(!checkIt.isStartOfBlock() && checkIt->hasValueType(ValueType::LOCAL))
                        {
                            if(checkIt->packMode.hasEffect() &&
                                (instr->readsLocal(checkIt->getOutput()->local()) ||
                                    nextInstr->readsLocal(checkIt->getOutput()->local())))
                                conditionsMet = false;
                            if(instr->unpackMode.hasEffect() && instr->readsLocal(checkIt->getOutput()->local()))
                                conditionsMet = false;
                            if(nextInstr->unpackMode.hasEffect() &&
                                nextInstr->readsLocal(checkIt->getOutput()->local()))
                                conditionsMet = false;
                        }
                    }

                    if(conditionsMet)
                    {
                        hasChanged = true;
                        // move supports both ADD and MUL ALU
                        // if merge, make "move" to other op-code or x x / v8max x x
                        logging::debug() << "Merging instructions " << instr->to_string() << " and "
                                         << nextInstr->to_string() << logging::endl;
                        if(op != nullptr && nextOp != nullptr)
                        {
                            it.reset(new CombinedOperation(
                                dynamic_cast<Operation*>(it.release()), dynamic_cast<Operation*>(nextIt.release())));
                            nextIt.erase();
                        }
                        else if(op != nullptr && nextMove != nullptr)
                        {
                            Operation* newMove = nextMove->combineWith(op->op);
                            if(newMove != nullptr)
                            {
                                it.reset(new CombinedOperation(dynamic_cast<Operation*>(it.release()), newMove));
                                nextIt.erase();
                            }
                            else
                                logging::warn() << "Error combining move-operation '" << nextMove->to_string()
                                                << "' with: " << op->to_string() << logging::endl;
                        }
                        else if(move != nullptr && nextOp != nullptr)
                        {
                            Operation* newMove = move->combineWith(nextOp->op);
                            if(newMove != nullptr)
                            {
                                it.reset(new CombinedOperation(newMove, dynamic_cast<Operation*>(nextIt.release())));
                                nextIt.erase();
                            }
                            else
                                logging::warn() << "Error combining move-operation '" << move->to_string()
                                                << "' with: " << nextOp->to_string() << logging::endl;
                        }
                        else if(move != nullptr && nextMove != nullptr)
                        {
                            bool firstOnMul = (move->packMode.hasEffect() && move->packMode.supportsMulALU()) ||
                                (nextMove->packMode.hasEffect() && !nextMove->packMode.supportsMulALU()) ||
                                nextMove->doesSetFlag();
                            Operation* newMove0 = move->combineWith(firstOnMul ? OP_ADD : OP_MUL24);
                            Operation* newMove1 = nextMove->combineWith(firstOnMul ? OP_MUL24 : OP_ADD);
                            if(newMove0 != nullptr && newMove1 != nullptr)
                            {
                                it.reset(new CombinedOperation(newMove0, newMove1));
                                nextIt.erase();
                            }
                            else
                                logging::warn() << "Error combining move-operation '" << move->to_string()
                                                << "' with: " << nextMove->to_string() << logging::endl;
                        }
                        else
                            throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled combination, type",
                                (instr->to_string() + ", ") + nextInstr->to_string());
                        if(it.get<CombinedOperation>() != nullptr)
                        {
                            // move instruction usable on both ALUs to the free ALU
                            CombinedOperation* comb = it.get<CombinedOperation>();
                            if(comb->getFirstOp()->op.runsOnAddALU() && comb->getFirstOp()->op.runsOnMulALU())
                            {
                                OpCode code = comb->getFirstOp()->op;
                                if(comb->getSecondOP()->op.runsOnAddALU())
                                    code.opAdd = 0;
                                else // by default (e.g. both run on both ALUs), map to ADD ALU
                                    code.opMul = 0;
                                dynamic_cast<Operation*>(comb->op1.get())->op = code;
                                logging::debug() << "Fixing operation available on both ALUs to "
                                                 << (code.opAdd == 0 ? "MUL" : "ADD")
                                                 << " ALU: " << comb->op1->to_string() << logging::endl;
                            }
                            if(comb->getSecondOP()->op.runsOnAddALU() && comb->getSecondOP()->op.runsOnMulALU())
                            {
                                OpCode code = comb->getSecondOP()->op;
                                if(comb->getFirstOp()->op.runsOnMulALU())
                                    code.opMul = 0;
                                else // by default (e.g. both run on both ALUs), map to MUL ALU
                                    code.opAdd = 0;
                                dynamic_cast<Operation*>(comb->op2.get())->op = code;
                                logging::debug() << "Fixing operation available on both ALUs to "
                                                 << (code.opAdd == 0 ? "MUL" : "ADD")
                                                 << " ALU: " << comb->op2->to_string() << logging::endl;
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

static Optional<Literal> getSourceLiteral(InstructionWalker it)
{
    if(it.has<LoadImmediate>() && it.get<LoadImmediate>()->type == LoadType::REPLICATE_INT32)
    {
        return it.get<LoadImmediate>()->getImmediate();
    }
    else if(it.has<MoveOperation>() && it->readsLiteral())
    {
        return it.get<MoveOperation>()->getSource().getLiteralValue();
    }
    else if(it.has<Operation>())
    {
        const auto val = it.get<Operation>()->precalculate(2);
        if(val)
            return val->getLiteralValue();
    }
    return {};
}

static bool canReplaceLiteralLoad(
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

bool optimizations::combineLoadingLiterals(const Module& module, Method& method, const Configuration& config)
{
    std::size_t threshold = config.additionalOptions.combineLoadThreshold;
    bool hasChanged = false;

    for(BasicBlock& block : method)
    {
        FastMap<uint32_t, InstructionWalker> lastLoadImmediate;
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(it.get() && it->hasValueType(ValueType::LOCAL) && !it->hasConditionalExecution() &&
                it->getOutput()->local()->getUsers(LocalUse::Type::WRITER).size() == 1 &&
                // TODO also combine is both ranges are not locally limited and overlap for the most part
                // (or at least if one range completely contains the other range)
                block.isLocallyLimited(it, it->getOutput()->local(), config.additionalOptions.accumulatorThreshold))
            {
                Optional<Literal> literal = getSourceLiteral(it);
                if(literal)
                {
                    auto immIt = lastLoadImmediate.find(literal->unsignedInt());
                    if(immIt != lastLoadImmediate.end() &&
                        canReplaceLiteralLoad(it, block.walk(), immIt->second, threshold))
                    {
                        Local* oldLocal = it->getOutput()->local();
                        Local* newLocal = immIt->second->getOutput()->local();
                        logging::debug() << "Removing duplicate loading of local: " << it->to_string() << logging::endl;
                        // Local#forUsers can't be used here, since we modify the list of users via
                        // LocalUser#replaceLocal
                        FastSet<const LocalUser*> readers = oldLocal->getUsers(LocalUse::Type::READER);
                        for(const LocalUser* reader : readers)
                        {
                            const_cast<LocalUser*>(reader)->replaceLocal(oldLocal, newLocal);
                        };
                        it.erase();
                        hasChanged = true;
                        continue;
                    }
                    else
                        lastLoadImmediate[literal->unsignedInt()] = it;
                }
            }
            it.nextInBlock();
        }
    }

    return hasChanged;
}

bool optimizations::unrollWorkGroups(const Module& module, Method& method, const Configuration& config)
{
    /*
     * Kernel Loop Optimization:
     *
     * Block for re-running the kernel:
     * In a loop with a count specified in GROUP_LOOP_SIZE, the kernel is re-run.
     * This saves some of the overhead for changing the group-id and re-starting the kernel via a mailbox call.
     *
     * In return, for every loop iteration there needs to be a UNIFORM with the count of remaining loop iterations
     * left. Or just a non-zero value for all but the last and a zero-value for the last iteration. Additionally,
     * all UNIFORMs need to be re-loaded!!
     */
    const Local* startLabel = method.findOrCreateLocal(TYPE_LABEL, BasicBlock::DEFAULT_BLOCK);

    // add conditional jump to end of kernel, to jump back to the beginning
    auto lastBlock = method.findBasicBlock(method.findLocal(BasicBlock::LAST_BLOCK));
    if(!lastBlock)
        throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find the default last block!");
    const Local* loopSize = method.findOrCreateLocal(TYPE_INT32, Method::GROUP_LOOP_SIZE);
    InstructionWalker it = lastBlock->walk().nextInBlock();
    assign(it, loopSize->createReference()) =
        (UNIFORM_REGISTER, InstructionDecorations::UNSIGNED_RESULT, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    it.emplace(new Branch(startLabel, COND_ZERO_CLEAR, loopSize->createReference()));

    return true;
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
    if(!it.has<MoveOperation>() || !nextIt.has<MoveOperation>())
        return it;
    if(!it->getOutput() || !nextIt->getOutput())
        return it;
    MoveOperation* move = it.get<MoveOperation>();
    MoveOperation* nextMove = nextIt.get<MoveOperation>();
    if(it->hasSideEffects() || nextIt->hasSideEffects())
        return it;
    if(it->getOutput().value() != nextIt->getOutput().value())
        return it;
    if(it->conditional == COND_ALWAYS || nextIt->conditional == COND_ALWAYS ||
        !it->conditional.isInversionOf(nextIt->conditional))
        return it;
    // For conditional moves, the source could be written conditionally
    // TODO improve by removing the condition of the source and allow combining
    // XXX actually would need to check if the condition is the same as the condition for the moves
    if(it->hasConditionalExecution() || nextIt->hasConditionalExecution())
        return it;
    // we have two consecutive moves to the same value, without side-effects and inverted conditions.
    // additionally, one of the moves writes a zero-vale
    if(move->getSource().hasLiteral(INT_ZERO.literal()) && !nextMove->getSource().hasLiteral(INT_ZERO.literal()))
    {
        logging::debug() << "Rewriting selection of either zero or " << nextMove->getSource().to_string()
                         << " using only one input" << logging::endl;
        it.reset((new Operation(OP_XOR, move->getOutput().value(), nextMove->getSource(), nextMove->getSource()))
                     ->copyExtrasFrom(move));
        // to process this instruction again (e.g. loading literals)
        it.previousInBlock();
    }
    else if(nextMove->getSource().hasLiteral(INT_ZERO.literal()))
    {
        logging::debug() << "Rewriting selection of either " << move->getSource().to_string()
                         << " or zero using only one input" << logging::endl;
        nextIt.reset((new Operation(OP_XOR, nextMove->getOutput().value(), move->getSource(), move->getSource()))
                         ->copyExtrasFrom(nextMove));
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
            if(it.has<VectorRotation>() && !it->hasSideEffects())
            {
                VectorRotation* rot = it.get<VectorRotation>();
                if(rot->getSource().hasLocal() && rot->getOffset().hasImmediate() &&
                    rot->getOffset().immediate() != VECTOR_ROTATE_R5)
                {
                    const LocalUser* writer = rot->getSource().getSingleWriter();
                    if(writer != nullptr)
                    {
                        const VectorRotation* firstRot = dynamic_cast<const VectorRotation*>(writer);
                        if(firstRot != nullptr && !firstRot->hasSideEffects() && firstRot->getOffset().hasImmediate() &&
                            firstRot->getOffset().immediate() != VECTOR_ROTATE_R5)
                        {
                            auto firstIt = it.getBasicBlock()->findWalkerForInstruction(firstRot, it);
                            if(firstIt)
                            {
                                hasChanged = true;
                                /*
                                 * Can combine the offsets of two rotations,
                                 * - if the only source of a vector rotation is only written once,
                                 * - the source of the input is another vector rotation,
                                 * - both rotations only use immediate offsets and
                                 * - neither rotation has any side effects
                                 */
                                const uint8_t offset =
                                    (rot->getOffset().immediate().getRotationOffset().value() +
                                        firstRot->getOffset().immediate().getRotationOffset().value()) %
                                    16;
                                if(offset == 0)
                                {
                                    logging::debug()
                                        << "Replacing unnecessary vector rotations " << firstRot->to_string() << " and "
                                        << rot->to_string() << " with single move" << logging::endl;
                                    it.reset((new MoveOperation(rot->getOutput().value(), firstRot->getSource()))
                                                 ->copyExtrasFrom(rot));
                                    it->copyExtrasFrom(firstRot);
                                    firstIt->erase();
                                }
                                else
                                {
                                    logging::debug()
                                        << "Combining vector rotations " << firstRot->to_string() << " and "
                                        << rot->to_string() << " to a single rotation with offset "
                                        << static_cast<unsigned>(offset) << logging::endl;
                                    it.reset((new VectorRotation(rot->getOutput().value(), firstRot->getSource(),
                                                  Value(SmallImmediate::fromRotationOffset(offset), TYPE_INT8)))
                                                 ->copyExtrasFrom(rot));
                                    it->copyExtrasFrom(firstRot);
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

InstructionWalker optimizations::combineSameFlags(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(it.get() == nullptr || it->setFlags != SetFlag::SET_FLAGS)
        return it;
    // only combine writing into NOP-register for now
    if(it->getOutput() && it->getOutput().value() != NOP_REGISTER)
        return it;
    // only remove this setting of flags, if we have no other side effects and don't execute conditionally
    if(it->signal.hasSideEffects() || it->conditional != COND_ALWAYS)
        return it;
    // only combine setting flags from moves (e.g. for PHI-nodes) for now
    if(it.get<intermediate::MoveOperation>() == nullptr)
        return it;
    const Value src = it.get<intermediate::MoveOperation>()->getSource();

    InstructionWalker checkIt = it.copy().previousInBlock();
    while(!checkIt.isStartOfBlock())
    {
        if(checkIt.get() && checkIt->setFlags == SetFlag::SET_FLAGS)
        {
            if(checkIt.get<intermediate::MoveOperation>() != nullptr &&
                checkIt.get<intermediate::MoveOperation>()->getSource() == src && checkIt->conditional == COND_ALWAYS)
            {
                logging::debug() << "Removing duplicate setting of same flags: " << it->to_string() << logging::endl;
                it.erase();
                // don't skip next instruction
                it.previousInBlock();
            }
            // otherwise some other flags are set -> cancel
            return it;
        }
        checkIt.previousInBlock();
    }

    return it;
}

InstructionWalker optimizations::combineArithmeticOperations(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    auto op = it.get<Operation>();
    if(!op)
        return it;
    if(it->getArguments().size() != 2)
        return it;
    if(!it->readsLiteral())
        // even if we could combine, we cannot save an instruction (since we cannot pre-calculate anything)
        return it;
    if(it->hasConditionalExecution() || !op->isSimpleOperation())
        return it;
    if(!std::any_of(it->getArguments().begin(), it->getArguments().end(),
           [](const Value& val) -> bool { return val.hasLocal(); }))
        return it;

    // exactly one local and one literal operand
    const auto& literalArg = it->getArguments()[0].isLiteralValue() ? it->getArguments()[0] : it->getArguments()[1];
    const auto& localArg = it->getArguments()[0].isLiteralValue() ? it->getArguments()[1] : it->getArguments()[0];

    auto singleWriter = localArg.getSingleWriter();
    if(!singleWriter || !singleWriter->readsLiteral())
        // same as above, we cannot pre-calculate anything
        return it;

    auto writerOp = dynamic_cast<const Operation*>(singleWriter);
    if(writerOp == nullptr || writerOp->op != op->op)
        // not the same operation
        return it;

    if(!writerOp->isSimpleOperation() || singleWriter->hasConditionalExecution())
        return it;

    if(singleWriter->getOutput()->local()->getUsers(LocalUse::Type::READER).size() != 1)
        // we cannot remove or modify the writer, so abort here
        return it;

    const auto& otherLiteralArg = singleWriter->getArguments()[0].isLiteralValue() ? singleWriter->getArguments()[0] :
                                                                                     singleWriter->getArguments()[1];
    const auto& origArg = singleWriter->getArguments()[0].isLiteralValue() ? singleWriter->getArguments()[1] :
                                                                             singleWriter->getArguments()[0];

    if(!op->op.isCommutative() && (it->getArgument(0) != localArg || singleWriter->getArgument(0) != origArg))
        // we cannot transform e.g. (a op b) op c to b op (a op c)
        return it;

    Optional<Value> precalc = NO_VALUE;
    if(op->op.isAssociative())
    {
        logging::debug() << "Combining associative operations " << singleWriter->to_string() << " and "
                         << it->to_string() << logging::endl;
        precalc = op->op(literalArg, otherLiteralArg);
    }
    else if(op->op == OP_SHL || op->op == OP_SHR || op->op == OP_ASR || op->op == OP_ROR)
    {
        logging::debug() << "Combining shifts " << singleWriter->to_string() << " and " << it->to_string()
                         << logging::endl;
        precalc = OP_ADD(literalArg, otherLiteralArg);
    }
    auto lastIt = it.getBasicBlock()->findWalkerForInstruction(singleWriter, it).value();
    lastIt.reset(new Operation(op->op, it->getOutput().value(), origArg, precalc.value()));
    it.erase();
    // don't skip next instruction
    it.previousInBlock();
    return it;
}

static bool isGroupUniform(const Local* local)
{
    auto writers = local->getUsers(LocalUse::Type::WRITER);
    return std::all_of(writers.begin(), writers.end(), [](const LocalUser* instr) -> bool {
        return instr->hasDecoration(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    });
}

static bool isWorkGroupUniform(const Value& val)
{
    return val.hasImmediate() || val.hasLiteral() ||
        (val.hasLocal() && isGroupUniform(val.local()))
        // XXX this is not true for the local ID UNIFORM
        || (val.hasRegister(REG_UNIFORM));
}

static FastMap<Value, InstructionDecorations> findDirectLevelAdditionInputs(const Value& val)
{
    FastMap<Value, InstructionDecorations> result;
    auto writer = val.getSingleWriter();
    if(writer == nullptr || writer->hasDecoration(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
    {
        // we have no need to split up work-group uniform values any more detailed
        auto deco = writer ? writer->decoration : InstructionDecorations::NONE;
        result.emplace(val,
            add_flag(deco,
                val.hasImmediate() || val.hasLiteral() ? InstructionDecorations::WORK_GROUP_UNIFORM_VALUE :
                                                         InstructionDecorations::NONE));
        if(val.hasImmediate() && val.immediate().getIntegerValue() >= 0)
            result[val] = add_flag(result[val], InstructionDecorations::UNSIGNED_RESULT);
        else if(val.hasLiteral() && val.literal().signedInt() >= 0)
            result[val] = add_flag(result[val], InstructionDecorations::UNSIGNED_RESULT);
        else if(val.hasRegister() && val.reg() == REG_UNIFORM)
            // XXX this is not true for the local ID UNIFORM, which should never be checked here (since the actual ID
            // needs always be extracted via non-adds)
            result[val] = add_flag(result[val], InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        return result;
    }
    auto op = dynamic_cast<const Operation*>(writer);
    bool onlySideEffectIsReadingUniform = op && op->hasSideEffects() && !op->doesSetFlag() &&
        !op->signal.hasSideEffects() &&
        !(op->hasValueType(ValueType::REGISTER) && op->getOutput()->reg().hasSideEffectsOnWrite()) &&
        std::all_of(op->getArguments().begin(), op->getArguments().end(), [](const Value& arg) -> bool {
            return !arg.hasRegister() || arg.reg() == REG_UNIFORM || !arg.reg().hasSideEffectsOnRead();
        });
    if(op && op->op == OP_ADD && !op->hasConditionalExecution() &&
        (!op->hasSideEffects() || onlySideEffectIsReadingUniform) && !op->hasPackMode() && !op->hasUnpackMode())
    {
        FastMap<Value, InstructionDecorations> args;
        for(const auto& arg : op->getArguments())
        {
            auto tmp = findDirectLevelAdditionInputs(arg);
            args.insert(tmp.begin(), tmp.end());
        }
        return args;
    }
    result.emplace(val, writer->decoration);
    return result;
}

// represents analysis data for the range of memory accessed per memory object
struct MemoryAccessRange
{
    const Local* memoryObject = nullptr;
    // the instruction writing the address to VPR_ADDR or VPW_ADDR
    InstructionWalker addressWrite{};
    // the instruction adding the offset to the base pointer, could be the same as addressWrite
    InstructionWalker baseAddressAdd{};
    // the instruction converting the address offset from element offset to byte offset
    Optional<InstructionWalker> typeSizeShift{};
    // the work-group uniform parts of which the address offset is calculated from
    FastMap<Value, InstructionDecorations> groupUniformAddressParts{};
    // the dynamic parts of which the address offset is calculated from
    FastMap<Value, InstructionDecorations> dynamicAddressParts{};
    // the maximum range (in elements!) the memory is accessed in
    analysis::IntegerRange offsetRange{0, 0};

    std::string to_string() const
    {
        return (addressWrite->to_string() +
                   (addressWrite->writesRegister(REG_VPM_DMA_LOAD_ADDR) ? " - read " : " - write ")) +
            (memoryObject->to_string() +
                (groupUniformAddressParts.empty() ? " with" : " with work-group uniform offset and") +
                " dynamic element range [") +
            (std::to_string(offsetRange.minValue) + ", ") + (std::to_string(offsetRange.maxValue) + "]");
    }
};

struct LocalUsageOrdering
{
    bool operator()(const Local* l1, const Local* l2) const
    {
        // prefer more usages over less usages
        // TODO is this the correct way to do this? E.g. is there one usage per memory access?
        return l1->getUsers(LocalUse::Type::READER).size() > l2->getUsers(LocalUse::Type::READER).size() || l1 < l2;
    }
};

using AccessRanges = OrderedMap<const Local*, FastAccessList<MemoryAccessRange>, LocalUsageOrdering>;

static AccessRanges determineAccessRanges(Method& method)
{
    // TODO if we cannot find an access range for a local, we cannot combine any other access ranges for this global!
    AccessRanges result;
    for(BasicBlock& block : method)
    {
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(it.has() && (it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            {
                // 1. find writes to VPM DMA addresses with work-group uniform part in address values
                if(std::any_of(it->getArguments().begin(), it->getArguments().end(), isWorkGroupUniform) ||
                    it.has<MoveOperation>())
                {
                    if(it.has<MoveOperation>() && it->assertArgument(0).hasLocal() &&
                        (it->assertArgument(0).local()->is<Parameter>() || it->assertArgument(0).local()->is<Global>()))
                    {
                        // direct write of address (e.g. all work items write to the same location
                        logging::debug() << "DMA address is directly set to a parameter/global address, cannot be "
                                            "optimized by caching multiple accesses: "
                                         << it->to_string() << logging::endl;
                        it.nextInBlock();
                        continue;
                    }
                    MemoryAccessRange range;
                    range.addressWrite = it;
                    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
                    // base-pointer is found correctly
                    auto trackIt = it;
                    if(it.has<MoveOperation>() && it->assertArgument(0).getSingleWriter())
                    {
                        auto walker =
                            it.getBasicBlock()->findWalkerForInstruction(it->assertArgument(0).getSingleWriter(), it);
                        if(!walker)
                        {
                            logging::debug() << "Unhandled case, address is calculated in a different basic-block: "
                                             << it->to_string() << logging::endl;
                            it.nextInBlock();
                            continue;
                        }
                        else
                            trackIt = walker.value();
                    }

                    auto variableArg = std::find_if_not(
                        trackIt->getArguments().begin(), trackIt->getArguments().end(), isWorkGroupUniform);
                    if(variableArg != trackIt->getArguments().end() && variableArg->getSingleWriter() != nullptr)
                    {
                        // 2. rewrite address so all work-group uniform parts are combined and all variable parts and
                        // added in the end
                        // TODO is this the correct criteria? We could also handle only base-pointer + local_id, for
                        // example
                        logging::debug() << "Found VPM DMA address write with work-group uniform operand: "
                                         << it->to_string() << logging::endl;
                        Value varArg = *variableArg;
                        // 2.1 jump over final addition of base address if it is a parameter
                        if(trackIt.has<Operation>() && trackIt.get<const Operation>()->op == OP_ADD)
                        {
                            const auto& arg0 = trackIt->assertArgument(0);
                            const auto& arg1 = trackIt->assertArgument(1);
                            if(arg0.hasLocal() &&
                                (arg0.local()->is<Parameter>() || arg0.local()->is<Global>() ||
                                    arg0.local()->name == Method::GLOBAL_DATA_ADDRESS))
                            {
                                range.memoryObject = arg0.local();
                                varArg = arg1;
                            }
                            else if(arg1.hasLocal() &&
                                (arg1.local()->is<Parameter>() || arg1.local()->is<Global>() ||
                                    arg1.local()->name == Method::GLOBAL_DATA_ADDRESS))
                            {
                                range.memoryObject = arg1.local();
                                varArg = arg0;
                            }
                            else if(arg0.hasRegister(REG_UNIFORM))
                            {
                                // e.g. reading of uniform for parameter is replaced by reading uniform here (if
                                // parameter only used once)
                                range.memoryObject = trackIt->getOutput()->local()->getBase(true);
                                varArg = arg1;
                            }
                            else if(arg1.hasRegister(REG_UNIFORM))
                            {
                                range.memoryObject = trackIt->getOutput()->local()->getBase(true);
                                varArg = arg0;
                            }
                            else
                            {
                                throw CompilationError(CompilationStep::OPTIMIZER,
                                    "Unhandled case of memory access: ", trackIt->to_string());
                            }
                            range.baseAddressAdd = trackIt;
                        }
                        else
                        {
                            logging::debug()
                                << "Cannot optimize further, since add of base-address and pointer was not found: "
                                << it->to_string() << logging::endl;
                            it.nextInBlock();
                            continue;
                        }
                        auto writer = varArg.getSingleWriter();
                        // 2.2 jump over shl (if any) and remember offset
                        if(dynamic_cast<const Operation*>(writer) &&
                            dynamic_cast<const Operation*>(writer)->op == OP_SHL)
                        {
                            if(!writer->assertArgument(1).getLiteralValue() ||
                                (1u << writer->assertArgument(1).getLiteralValue()->unsignedInt()) !=
                                    it->assertArgument(0).type.getElementType().getPhysicalWidth())
                            {
                                // Abort, since the offset shifted does not match the type-width of the element type
                                logging::debug()
                                    << "Cannot optimize further, since shift-offset does not match type size: "
                                    << it->to_string() << " and " << writer->to_string() << logging::endl;
                                it.nextInBlock();
                                continue;
                            }
                            range.typeSizeShift = trackIt.getBasicBlock()->findWalkerForInstruction(writer, trackIt);
                            varArg = writer->assertArgument(0);
                            // TODO is never read. Remove or use?
                            writer = varArg.getSingleWriter();
                        }
                        // 2.3 collect all directly neighboring (and directly referenced) additions
                        // result is now: finalAdd + (sum(addedValues) << shiftFactor)
                        auto addressParts = findDirectLevelAdditionInputs(varArg);
                        if(addressParts.size() < 2)
                        {
                            // could not determine multiple inputs to add, abort
                            it.nextInBlock();
                            continue;
                        }
                        // 2.4 calculate the maximum dynamic offset
                        for(const auto& val : addressParts)
                        {
                            if(!has_flag(val.second, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
                            {
                                range.dynamicAddressParts.emplace(val);
                                if(val.first.hasLocal())
                                {
                                    auto singleRange = analysis::ValueRange::getValueRange(val.first, &method);
                                    range.offsetRange.minValue += singleRange.getIntRange()->minValue;
                                    range.offsetRange.maxValue += singleRange.getIntRange()->maxValue;
                                }
                                else
                                    throw CompilationError(CompilationStep::OPTIMIZER,
                                        "Unhandled value for memory access offset", val.first.to_string());
                            }
                            else
                                range.groupUniformAddressParts.emplace(val);
                        }
                        logging::debug() << range.to_string() << logging::endl;
                        result[range.memoryObject].emplace_back(range);
                    }
                }
            }
            it.nextInBlock();
        }
    }
    return result;
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
            referenceIt.emplace(new Operation(OP_ADD, newResult, prevResult->first, valIt->first));
            referenceIt->addDecorations(newFlags);
            referenceIt.nextInBlock();
            prevResult = std::make_pair(newResult, newFlags);
        }
        else
            prevResult = std::make_pair(valIt->first, valIt->second);
        valIt = addedValues.erase(valIt);
    }
    return prevResult;
}

static std::pair<bool, analysis::IntegerRange> checkWorkGroupUniformParts(
    FastAccessList<MemoryAccessRange>& accessRanges)
{
    analysis::IntegerRange offsetRange{std::numeric_limits<int>::max(), std::numeric_limits<int>::min()};
    const auto& firstUniformAddresses = accessRanges.front().groupUniformAddressParts;
    FastMap<Value, InstructionDecorations> differingUniformParts;
    bool allUniformPartsEqual = true;
    for(auto& entry : accessRanges)
    {
        if(entry.groupUniformAddressParts != firstUniformAddresses)
        {
            allUniformPartsEqual = false;
            for(const auto& pair : entry.groupUniformAddressParts)
            {
                if(firstUniformAddresses.find(pair.first) == firstUniformAddresses.end())
                    differingUniformParts.emplace(pair);
            }
            for(const auto& pair : firstUniformAddresses)
                if(entry.groupUniformAddressParts.find(pair.first) == entry.groupUniformAddressParts.end())
                    differingUniformParts.emplace(pair);
        }
        offsetRange.minValue = std::min(offsetRange.minValue, entry.offsetRange.minValue);
        offsetRange.maxValue = std::max(offsetRange.maxValue, entry.offsetRange.maxValue);
    }
    if(!allUniformPartsEqual)
    {
        if(std::all_of(differingUniformParts.begin(), differingUniformParts.end(),
               [](const std::pair<Value, InstructionDecorations>& part) -> bool {
                   return part.first.getLiteralValue().has_value();
               }))
        {
            // all work-group uniform values which differ between various accesses of the same local are literal
            // values. We can use this knowledge to still allow caching the local, by converting the literals to
            // dynamic offsets
            for(auto& entry : accessRanges)
            {
                auto it = entry.groupUniformAddressParts.begin();
                while(it != entry.groupUniformAddressParts.end())
                {
                    if(differingUniformParts.find(it->first) != differingUniformParts.end())
                    {
                        entry.offsetRange.minValue += it->first.getLiteralValue()->signedInt();
                        entry.offsetRange.maxValue += it->first.getLiteralValue()->signedInt();
                        entry.dynamicAddressParts.emplace(*it);
                        it = entry.groupUniformAddressParts.erase(it);
                    }
                    else
                        ++it;
                }
            }
            return checkWorkGroupUniformParts(accessRanges);
        }
        else
            return std::make_pair(false, analysis::IntegerRange{});
    }
    return std::make_pair(true, offsetRange);
}

static void rewriteIndexCalculation(Method& method, MemoryAccessRange& range)
{
    // 3. combine the additions so work-group uniform and non-uniform values are added
    // separately
    auto insertIt = range.typeSizeShift ? range.typeSizeShift.value() : range.baseAddressAdd;
    auto firstVal = combineAdditions(method, insertIt, range.groupUniformAddressParts);
    auto secondVal = combineAdditions(method, insertIt, range.dynamicAddressParts);
    Optional<std::pair<Value, InstructionDecorations>> resultVal;
    if(!range.groupUniformAddressParts.empty() || !range.dynamicAddressParts.empty())
        throw CompilationError(CompilationStep::OPTIMIZER, "Too many values remaining",
            std::to_string(range.groupUniformAddressParts.size() + range.dynamicAddressParts.size()));
    if(firstVal && secondVal)
    {
        // add work-group uniform and variable part
        resultVal = std::make_pair(
            method.addNewLocal(range.memoryObject->type), intersect_flags(firstVal->second, secondVal->second));
        insertIt.emplace(new Operation(OP_ADD, resultVal->first, firstVal->first, secondVal->first));
        insertIt->addDecorations(resultVal->second);
    }
    else if(firstVal)
        resultVal = firstVal;
    else if(secondVal)
        resultVal = secondVal;
    if(range.typeSizeShift)
        (*range.typeSizeShift)->setArgument(0, resultVal->first);
    else
        // TODO replace index variable with new index variable
        throw CompilationError(
            CompilationStep::OPTIMIZER, "Not yet implemented, no shift in address calculation", range.to_string());

    logging::debug() << "Rewrote address-calculation with indices "
                     << (firstVal ? (firstVal->first.to_string() + " (" + toString(firstVal->second) + ")") : "")
                     << " and "
                     << (secondVal ? (secondVal->first.to_string() + " (" + toString(secondVal->second) + ")") : "")
                     << logging::endl;
}

bool optimizations::cacheWorkGroupDMAAccess(const Module& module, Method& method, const Configuration& config)
{
    auto memoryAccessRanges = determineAccessRanges(method);
    for(auto& pair : memoryAccessRanges)
    {
        bool allUniformPartsEqual;
        analysis::IntegerRange offsetRange;
        std::tie(allUniformPartsEqual, offsetRange) = checkWorkGroupUniformParts(pair.second);
        if(!allUniformPartsEqual)
        {
            logging::debug() << "Cannot cache memory location " << pair.first->to_string()
                             << " in VPM, since the work-group uniform parts of the address calculations differ, which "
                                "is not yet supported!"
                             << logging::endl;
            continue;
        }
        if((offsetRange.maxValue - offsetRange.minValue) >= config.availableVPMSize ||
            (offsetRange.maxValue < offsetRange.minValue))
        {
            // this also checks for any over/underflow when converting the range to unsigned int in the next steps
            logging::debug() << "Cannot cache memory location " << pair.first->to_string()
                             << " in VPM, the accessed range is too big: [" << offsetRange.minValue << ", "
                             << offsetRange.maxValue << "]" << logging::endl;
            continue;
        }
        logging::debug() << "Memory location " << pair.first->to_string()
                         << " is accessed via DMA in the dynamic range [" << offsetRange.minValue << ", "
                         << offsetRange.maxValue << "]" << logging::endl;

        auto accessedType = pair.first->type.toArrayType(static_cast<unsigned>(
            offsetRange.maxValue - offsetRange.minValue + 1 /* bounds of range are inclusive! */));

        // TODO the local is not correct, at least not if there is a work-group uniform offset
        auto vpmArea = method.vpm->addArea(pair.first, accessedType, false);
        if(vpmArea == nullptr)
        {
            logging::debug() << "Memory location " << pair.first->to_string() << " with dynamic access range ["
                             << offsetRange.minValue << ", " << offsetRange.maxValue
                             << "] cannot be cached in VPM, since it does not fit" << logging::endl;
            continue;
        }

        // TODO insert load memory area into VPM at start of kernel (after all the required offsets/indices are
        // calculated)
        // TODO calculate address from base address and work-group uniform parts
        // TODO insert store VPM into memory area at end of kernel
        // TODO rewrite memory accesses to only access the correct VPM area

        for(auto& entry : pair.second)
            rewriteIndexCalculation(method, entry);

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
