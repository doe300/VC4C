/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Combiner.h"

#include "../InstructionWalker.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <cstdlib>
#include <memory>

// TODO combine y = (x >> n) << n with and
// same for y = (x << n) >> n (at least of n constant)

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

static Optional<Register> getSourceConstantRegister(InstructionWalker it)
{
    if(it.has<MoveOperation>() && (it->readsRegister(REG_ELEMENT_NUMBER) || it->readsRegister(REG_QPU_NUMBER)))
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
        FastMap<uint32_t, InstructionWalker> lastLoadImmediate;
        FastMap<Register, InstructionWalker> lastLoadRegister;
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
                        canReplaceConstantLoad(it, block.walk(), immIt->second, threshold))
                    {
                        Local* oldLocal = it->getOutput()->local();
                        Local* newLocal = immIt->second->getOutput()->local();
                        logging::debug() << "Removing duplicate loading of literal: " << it->to_string()
                                         << logging::endl;
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
                auto reg = getSourceConstantRegister(it);
                if(reg)
                {
                    auto regIt = lastLoadRegister.find(*reg);
                    if(regIt != lastLoadRegister.end() &&
                        canReplaceConstantLoad(it, block.walk(), regIt->second, threshold))
                    {
                        Local* oldLocal = it->getOutput()->local();
                        Local* newLocal = regIt->second->getOutput()->local();
                        logging::debug() << "Removing duplicate loading of register: " << it->to_string()
                                         << logging::endl;
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
                        lastLoadRegister[*reg] = it;
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
        precalc = op->op(literalArg, otherLiteralArg).first;
    }
    else if(op->op == OP_SHL || op->op == OP_SHR || op->op == OP_ASR || op->op == OP_ROR)
    {
        logging::debug() << "Combining shifts " << singleWriter->to_string() << " and " << it->to_string()
                         << logging::endl;
        precalc = OP_ADD(literalArg, otherLiteralArg).first;
    }
    auto lastIt = it.getBasicBlock()->findWalkerForInstruction(singleWriter, it);
    if(lastIt)
    {
        lastIt->reset(new Operation(op->op, it->getOutput().value(), origArg, precalc.value()));
        it.erase();
        // don't skip next instruction
        it.previousInBlock();
    }
    return it;
}

InstructionWalker optimizations::combineFlagWithOutput(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(!it.has() || !it->doesSetFlag())
        // does not set flags
        return it;
    if(!it->writesRegister(REG_NOP))
        // already has output
        return it;
    if(!it.has<intermediate::MoveOperation>())
        // only combine setting flags from moves (e.g. for PHI-nodes) for now
        return it;
    const auto& in = it->assertArgument(0);
    if(it->signal.hasSideEffects() || it->hasConditionalExecution() ||
        (in.hasRegister() && in.reg().hasSideEffectsOnRead()))
        // only remove this setting of flags, if we have no other side effects and don't execute conditionally
        return it;
    const auto checkFunc = [&](InstructionWalker checkIt) -> bool {
        return checkIt.has() && !checkIt->doesSetFlag() && !checkIt->hasSideEffects() && checkIt.has<MoveOperation>() &&
            (checkIt->assertArgument(0) == in ||
                (in.getLiteralValue() && checkIt->assertArgument(0).hasLiteral(*in.getLiteralValue())));
    };

    // look into the future until we hit the instruction requiring the flag
    InstructionWalker resultIt = it.getBasicBlock()->walkEnd();
    auto checkIt = it.copy().nextInBlock();
    while(!checkIt.isEndOfBlock())
    {
        // this is usually only executed few times, since setting of flags and usage thereof are close to each other
        if(checkIt.has() && (checkIt->hasConditionalExecution() || checkIt->doesSetFlag()))
            break;
        if(checkFunc(checkIt))
        {
            resultIt = checkIt;
            break;
        }
        checkIt.nextInBlock();
    }

    // look back only a few instructions
    if(resultIt.isEndOfBlock())
    {
        checkIt = it.copy().previousInBlock();
        while(!checkIt.isStartOfBlock())
        {
            // TODO limit number of steps?

            if(checkIt.has() &&
                (checkIt->hasConditionalExecution() || checkIt->doesSetFlag() || checkIt->getOutput() == in))
                break;
            if(checkFunc(checkIt))
            {
                resultIt = checkIt;
                break;
            }
            checkIt.previousInBlock();
        }
    }
    if(!resultIt.isEndOfBlock())
    {
        logging::debug() << "Combining move to set flags '" << it->to_string()
                         << "' with move to output: " << resultIt->to_string() << logging::endl;
        resultIt->setSetFlags(SetFlag::SET_FLAGS);
        // TODO decorations? If input is the same, most decorations are also the same?! Also, setflags usually don't
        // have that many!?
        it.erase();
        // don't skip next instruction
        it.previousInBlock();
    }
    return it;
}

bool optimizations::combineVPMSetupWrites(const Module& module, Method& method, const Configuration& config)
{
    bool changedInstructions = false;
    for(auto& bb : method)
    {
        Optional<periphery::VPRDMASetup> lastDMAReadSetup;
        Optional<periphery::VPRStrideSetup> lastReadStrideSetup;
        Optional<periphery::VPWDMASetup> lastDMAWriteSetup;
        Optional<periphery::VPWStrideSetup> lastWriteStrideSetup;
        // NOTE: cannot simply remove successive VPM read/write setup, since a) the VPM address is auto-incremented and
        // b) the read setup specifies the exact number of rows to read

        auto it = bb.walk();
        while(!it.isEndOfBlock())
        {
            if(it.has() && (it->writesRegister(REG_VPM_IN_SETUP) || it->writesRegister(REG_VPM_OUT_SETUP)))
            {
                bool readSetup = it->writesRegister(REG_VPM_IN_SETUP);
                // this can for now only optimize constant setups
                auto setupBits = it->getArgument(0) ? it->assertArgument(0).getLiteralValue() : Optional<Literal>{};
                if(setupBits && (it.has<MoveOperation>() || it.has<LoadImmediate>()))
                {
                    if(readSetup)
                    {
                        auto setup = periphery::VPRSetup::fromLiteral(setupBits->unsignedInt());
                        if(setup.isDMASetup())
                        {
                            if(lastDMAReadSetup == setup.dmaSetup)
                            {
                                logging::debug()
                                    << "Removing duplicate writing of same DMA read setup: " << it->to_string()
                                    << logging::endl;
                                it = it.erase();
                                changedInstructions = true;
                                continue;
                            }
                            lastDMAReadSetup = setup.dmaSetup;
                        }
                        else if(setup.isStrideSetup())
                        {
                            if(lastReadStrideSetup == setup.strideSetup)
                            {
                                logging::debug()
                                    << "Removing duplicate writing of same DMA read stride setup: " << it->to_string()
                                    << logging::endl;
                                it = it.erase();
                                changedInstructions = true;
                                continue;
                            }
                            lastReadStrideSetup = setup.strideSetup;
                        }
                    }
                    else
                    {
                        auto setup = periphery::VPWSetup::fromLiteral(setupBits->unsignedInt());
                        if(setup.isDMASetup())
                        {
                            if(lastDMAWriteSetup == setup.dmaSetup)
                            {
                                logging::debug()
                                    << "Removing duplicate writing of same DMA write setup: " << it->to_string()
                                    << logging::endl;
                                it = it.erase();
                                changedInstructions = true;
                                continue;
                            }
                            lastDMAWriteSetup = setup.dmaSetup;
                        }
                        else if(setup.isStrideSetup())
                        {
                            if(lastWriteStrideSetup == setup.strideSetup)
                            {
                                logging::debug()
                                    << "Removing duplicate writing of same DMA write stride setup: " << it->to_string()
                                    << logging::endl;
                                it = it.erase();
                                changedInstructions = true;
                                continue;
                            }
                            lastWriteStrideSetup = setup.strideSetup;
                        }
                    }
                }
                else if(it.has<Operation>() &&
                    std::none_of(
                        it->getArguments().begin(), it->getArguments().end(), [readSetup](const Value& arg) -> bool {
                            auto writer = arg.getSingleWriter();
                            auto val = writer ? writer->precalculate(1) : arg;
                            if(val && val->getLiteralValue())
                                if(readSetup &&
                                    periphery::VPRSetup::fromLiteral(val->getLiteralValue()->unsignedInt())
                                        .isGenericSetup())
                                    return true;
                            if(!readSetup &&
                                periphery::VPWSetup::fromLiteral(val->getLiteralValue()->unsignedInt())
                                    .isGenericSetup())
                                return true;
                            return false;
                        }))
                {
                    // we have a VPM IO setup write which we do not know to be a generic setup.
                    // to be on the safe side, clear last setups
                    lastDMAReadSetup = {};
                    lastDMAWriteSetup = {};
                    lastReadStrideSetup = {};
                    lastWriteStrideSetup = {};
                }
            }
            it.nextInBlock();
        }
    }
    return changedInstructions;
}
