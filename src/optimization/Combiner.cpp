/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Combiner.h"
#include "log.h"
#include "helper.h"
#include "../intermediate/Helper.h"
#include "../InstructionWalker.h"

#include <stdlib.h>
#include <algorithm>
#include <memory>

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;

InstructionWalker optimizations::combineDuplicateBranches(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	/*
	 * label: %91
	 * br %103
	 * label: %92
	 * br %103
	 * label: %93
	 * br %103
	 * label: %94
	 * br %103
	 * label: %95
	 * br %103
	 *
	 * is converted into:
	 *
	 * label: %91
	 * label: %92
	 * label: %93
	 * label: %94
	 * label: %95
	 * br %103
	 */
	Branch* thisBranch = it.get<Branch>();
	if(thisBranch != nullptr)
	{

		//skip all following labels
		InstructionWalker nextIt = it.copy().nextInMethod();
		while(!nextIt.isEndOfMethod() && nextIt.has<BranchLabel>())
			nextIt.nextInMethod();
		if(nextIt.isEndOfMethod())
			return it;
		Branch* nextBranch = nextIt.get<Branch>();
		if(nextBranch != nullptr)
		{
			if(thisBranch->getTarget() != nextBranch->getTarget())
				return it;
			//for now, only remove unconditional branches
			if(!thisBranch->isUnconditional() || !nextBranch->isUnconditional())
				return it;
			logging::debug() << "Removing duplicate branch to same target: " << thisBranch->to_string() << logging::endl;
			it = it.erase();
			//don't skip next instruction
			it.previousInMethod();
		}
	}
	return it;
}

using MergeCondition = std::function<bool(Operation*, Operation*, MoveOperation*, MoveOperation*)>;
static const std::vector<MergeCondition> mergeConditions = {
	//check both instructions can be combined and are actually mapped to machine code
	[](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
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
	//check neither instruction is a vector rotation
	[](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
		return (firstMove == nullptr || dynamic_cast<VectorRotation*>(firstMove) == nullptr) && (secondMove == nullptr || dynamic_cast<VectorRotation*>(secondMove) == nullptr);
	},
    //check both instructions use different ALUs
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
        if(firstOp != nullptr && secondOp != nullptr)
        {
            const auto firstCodes = toOpCode(firstOp->opCode);
            const auto secondCodes = toOpCode(secondOp->opCode);
            if(firstCodes.first != OPADD_NOP && secondCodes.first != OPADD_NOP)
                return false;
            else if(firstCodes.second != OPMUL_NOP && secondCodes.second != OPMUL_NOP)
                return false;
        }
        return true;
    },
    //check reads from or writes to special registers
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
        if(firstOp != nullptr && (firstOp->hasValueType(ValueType::REGISTER) || firstOp->getFirstArg().hasType(ValueType::REGISTER) || (firstOp->getSecondArg() && firstOp->getSecondArg().get().hasType(ValueType::REGISTER))))
            return false;
        else if(firstMove != nullptr && (firstMove->hasValueType(ValueType::REGISTER) || firstMove->getSource().hasType(ValueType::REGISTER)))
            return false;
        else if(secondOp != nullptr && (secondOp->hasValueType(ValueType::REGISTER) || secondOp->getFirstArg().hasType(ValueType::REGISTER) || (secondOp->getSecondArg() && secondOp->getSecondArg().get().hasType(ValueType::REGISTER))))
            return false;
        else if(secondMove != nullptr && (secondMove->hasValueType(ValueType::REGISTER) || secondMove->getSource().hasType(ValueType::REGISTER)))
            return false;
        return true;
    },
    //check second operation using the result of the first
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
        Value outFirst(UNDEFINED_VALUE);
        if(firstOp != nullptr && firstOp->getOutput())
            outFirst = firstOp->getOutput();
        else if(firstMove != nullptr && firstMove->getOutput())
            outFirst = firstMove->getOutput();
        if(outFirst.type != TYPE_UNKNOWN)
        {
            if(secondOp != nullptr)
            {
                if(secondOp->getFirstArg() == outFirst)
                    return false;
                else if(secondOp->getSecondArg() && secondOp->getSecondArg().get() == outFirst)
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
    //check operations use same output and do not have inverted conditions
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
        Value outFirst(UNDEFINED_VALUE);
        ConditionCode condFirst = COND_ALWAYS;
        if(firstOp != nullptr && firstOp->getOutput())
        {
            outFirst = firstOp->getOutput();
            condFirst = firstOp->conditional;
        }
        else if(firstMove != nullptr && firstMove->getOutput())
        {
            outFirst = firstMove->getOutput();
            condFirst = firstMove->conditional;
        }
        Value outSecond(UNDEFINED_VALUE);
        ConditionCode condSecond = COND_ALWAYS;
        if(secondOp != nullptr && secondOp->getOutput())
        {
            outSecond = secondOp->getOutput();
            condSecond = secondOp->conditional;
        }
        else if(secondMove != nullptr && secondMove->getOutput())
        {
            outSecond = secondMove->getOutput();
            condSecond = secondMove->conditional;
        }
        if(outFirst == outSecond || (outFirst.hasType(ValueType::LOCAL) && outSecond.hasType(ValueType::LOCAL) && outSecond.hasLocal(outFirst.local)))
        {
            if(!condSecond.isInversionOf(condFirst))
                return false;
        }
        return true;
    },
    //check first operation sets flags and second operation depends on them
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
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
        	//special case, e.g. mov.ifz x, bool true, mov.ifnz x, bool false
        	if(firstCond != COND_ALWAYS && secondCond != COND_ALWAYS)
        	{
        		return firstCond.isInversionOf(secondCond);
        	}
        }
        return setsFlags == SetFlag::DONT_SET || !usesFlags;
    },
    //check MUL ALU sets flags (flags would be set by ADD ALU)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
    	ConditionCode firstCond = firstOp ? firstOp->conditional : firstMove->conditional;
		ConditionCode secondCond = secondOp ? secondOp->conditional : secondMove->conditional;
		if(firstCond.isInversionOf(secondCond))
		{
			//if they have inverted conditions, the ADD ALU can't set the flags, the MUL ALU is supposed to
			return true;
		}
        if(firstOp != nullptr && toOpCode(firstOp->opCode).second != OPMUL_NOP && firstOp->setFlags == SetFlag::SET_FLAGS)
            return false;
        else if(secondOp != nullptr && toOpCode(secondOp->opCode).second != OPMUL_NOP && secondOp->setFlags == SetFlag::SET_FLAGS)
            return false;
        return true;
    },
    //check maximum 1 immediate value is used (both can use the same immediate value)
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
        Optional<SmallImmediate> immediate(false, 0);
        if(firstOp != nullptr)
        {
            if(firstOp->getFirstArg().hasType(ValueType::SMALL_IMMEDIATE))
            	immediate = firstOp->getFirstArg().immediate;
            if(firstOp->getSecondArg() && firstOp->getSecondArg().get().hasType(ValueType::SMALL_IMMEDIATE))
            	immediate = firstOp->getSecondArg().get().immediate;
        }
        if(firstMove != nullptr)
        {
            if(firstMove->getSource().hasType(ValueType::SMALL_IMMEDIATE))
            	immediate = firstMove->getSource().immediate;
        }
        if(immediate)
        {
            if(secondOp != nullptr)
            {
                if(secondOp->getFirstArg().hasType(ValueType::SMALL_IMMEDIATE) && !secondOp->getFirstArg().hasImmediate(immediate))
                    return false;
                if(secondOp->getSecondArg() && secondOp->getSecondArg().get().hasType(ValueType::SMALL_IMMEDIATE) && !secondOp->getSecondArg().get().hasImmediate(immediate))
                    return false;
            }
            if(secondMove != nullptr)
            {
                if(secondMove->getSource().hasType(ValueType::SMALL_IMMEDIATE) && !secondMove->getSource().hasImmediate(immediate))
                    return false;
            }
        }
        return true;
    },
    //check a maximum of 2 inputs are read from
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
        std::pair<Value, Value> inputs = std::make_pair(UNDEFINED_VALUE, UNDEFINED_VALUE);
        if(firstOp != nullptr)
        {
            inputs.first = firstOp->getFirstArg();
            if(firstOp->getSecondArg())
                inputs.second = firstOp->getSecondArg();
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
            if(secondOp->getSecondArg() && secondOp->getSecondArg().get() != inputs.first)
            {
            	if(inputs.second.type == TYPE_UNKNOWN)
					inputs.second = secondOp->getSecondArg();
            	else if(secondOp->getSecondArg().get() != inputs.second)
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
    //check at most 1 signal (including IMMEDIATE) is set, same for Un-/Pack
    [](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
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
        	if(firstOp->getFirstArg().hasType(ValueType::LITERAL) || (firstOp->getSecondArg() && firstOp->getSecondArg().get().hasType(ValueType::LITERAL)))
				firstSignal = SIGNAL_ALU_IMMEDIATE;
        	firstPack = firstOp->packMode;
        	firstUnpack = firstOp->unpackMode;
        }
        if(firstMove != nullptr)
        {
        	firstSignal = firstMove->signal;
        	if(firstMove->getSource().hasType(ValueType::LITERAL))
        		firstSignal = SIGNAL_ALU_IMMEDIATE;
        	firstPack = firstMove->packMode;
        	firstUnpack = firstMove->unpackMode;
        }
        if(secondOp != nullptr)
        {
        	secondSignal = secondOp->signal;
        	if(secondOp->getFirstArg().hasType(ValueType::LITERAL) || (secondOp->getSecondArg() && secondOp->getSecondArg().get().hasType(ValueType::LITERAL)))
        		secondSignal = SIGNAL_ALU_IMMEDIATE;
        	secondPack = secondOp->packMode;
        	secondUnpack = secondOp->unpackMode;
        }
        if(secondMove != nullptr)
        {
        	secondSignal = secondMove->signal;
        	if(secondMove->getSource().hasType(ValueType::LITERAL))
        		secondSignal = SIGNAL_ALU_IMMEDIATE;
        	secondPack = secondMove->packMode;
        	secondUnpack = secondMove->unpackMode;
        }
        //only one signal can be fired (independent of ALU conditions)
        if(firstSignal != SIGNAL_NONE && secondSignal != SIGNAL_NONE && firstSignal != secondSignal)
        	return false;
        //both instructions need to have the SAME pack-mode (including NOP), unless they have inverted conditions
		if(firstPack != secondPack && !invertedConditions)
			return false;
		//can only unpack from reg-file A -> 1 input
		//TODO check if both use same input, then allow (or have inverted conditions)
		if(firstUnpack != UNPACK_NOP && secondUnpack != UNPACK_NOP)
			return false;
        return true;
    },
	//check not two different boolean values which both are used in conditional jump
	[](Operation* firstOp, Operation* secondOp, MoveOperation* firstMove, MoveOperation* secondMove) -> bool{
    	//since boolean values are combined with ELEM_NUMBER (reg-file A) before conditional branch, they cannot be on reg-file A
    	//combining two of those can cause register-association errors
    	//TODO find a way to fix this
    	if(((firstOp != nullptr && firstOp->hasValueType(ValueType::LOCAL) && firstOp->getOutput().get().type == TYPE_BOOL) || (firstMove != nullptr && firstMove->hasValueType(ValueType::LOCAL) && firstMove->getOutput().get().type == TYPE_BOOL)))
    	{
    		if(((secondOp != nullptr && secondOp->hasValueType(ValueType::LOCAL) && secondOp->getOutput().get().type == TYPE_BOOL) || (secondMove != nullptr && secondMove->hasValueType(ValueType::LOCAL) && secondMove->getOutput().get().type == TYPE_BOOL)))
    		{
    			return false;
    		}
    	}
    	return true;
    }
};

void optimizations::combineOperations(const Module& module, Method& method, const Configuration& config)
{
	//TODO can combine operation x and y if y is something like (result of x & 0xFF/0xFFFF) -> pack-mode
	for(BasicBlock& bb : method.getBasicBlocks())
	{
		auto it = bb.begin();
		while(!it.isEndOfBlock() && !it.copy().nextInBlock().isEndOfBlock())
		{
			MoveOperation* move = it.get<MoveOperation>();
			if(move != nullptr)
			{
				//- remove moves where getSource() is not written to afterwards -> set destination = getSource()
				//rewrite all following instructions using the original destination
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
					 * - combined instructions use at least 2 accumulators, or share getSource()-registers, so that only 2 getSource() registers are required
					 * - the instructions do not depend one-on-another (e.g. out of first is in of second)
					 * - both instructions write to different locals (or to same local and have inverted conditions)
					 * - MUL instruction does not set flags (otherwise flags would be applied for ADD output)
					 * - only one instruction uses a literal (or the literal is the same)
					 * - both set signals (including immediate ALU operation)
					 * For now, may be removed (with exceptions):
					 * - neither of these instructions read/write from special registers
					 *   otherwise this could cause reading two UNIFORMS at once / writing VPM/VPM_ADDR at once
					 */
					//TODO a written-to register MUST not be read in the next instruction (check instruction before/after combined) (unless within local range)
					bool conditionsMet = std::all_of(mergeConditions.begin(), mergeConditions.end(), [op, nextOp, move, nextMove](const MergeCondition& cond) -> bool
					{
						return cond(op, nextOp, move, nextMove);
					});
					if(instr->hasValueType(ValueType::LOCAL) && nextInstr->hasValueType(ValueType::LOCAL))
					{
						//extra check, only combine writes to the same local, if local is only used within the next instruction
						//this is required, since we cannot write to a physical register from both ALUs, so the local needs to be on an accumulator
						if(instr->getOutput().get().local == nextInstr->getOutput().get().local && !nextIt.getBasicBlock()->isLocallyLimited(nextIt, instr->getOutput().get().local))
							conditionsMet = false;
					}
					if(instr->hasValueType(ValueType::LOCAL) || nextInstr->hasValueType(ValueType::LOCAL))
					{
						//also check that if the next instruction is a vector rotation, neither of the locals is being rotated there
						//since vector rotations can't rotate vectors which have been written in the instruction directly preceding it
						auto checkIt = nextIt.copy().nextInBlock();
						if(!checkIt.isEndOfBlock() && checkIt.has<VectorRotation>())
						{
							const Value src = checkIt.get<VectorRotation>()->getSource();
							if(instr->hasValueType(ValueType::LOCAL) && instr->getOutput().get() == src)
								conditionsMet = false;
							if(nextInstr->hasValueType(ValueType::LOCAL) && nextInstr->getOutput().get() == src)
								conditionsMet = false;
						}
					}
					if(conditionsMet)
					{
						//move supports both ADD and MUL ALU
						//if merge, make "move" to other op-code or x x / v8max x x
						logging::debug() << "Merging instructions " << instr->to_string() << " and " << nextInstr->to_string() << logging::endl;
						if(op != nullptr && nextOp != nullptr)
						{
							it.reset(new CombinedOperation(dynamic_cast<Operation*>(it.release()), dynamic_cast<Operation*>(nextIt.release())));
							nextIt.erase();
						}
						else if(op != nullptr && nextMove != nullptr)
						{
							Operation* newMove = nextMove->combineWith(op->opCode);
							if(newMove != nullptr)
							{
								it.reset(new CombinedOperation(dynamic_cast<Operation*>(it.release()), newMove));
								nextIt.erase();
							}
							else
								logging::warn() << "Error combining move-operation '" << nextMove->to_string() << "' with: " << op->to_string() << logging::endl;
						}
						else if(move != nullptr && nextOp != nullptr)
						{
							Operation* newMove = move->combineWith(nextOp->opCode);
							if(newMove != nullptr)
							{
								it.reset(new CombinedOperation(newMove, dynamic_cast<Operation*>(nextIt.release())));
								nextIt.erase();
							}
							else
								logging::warn() << "Error combining move-operation '" << move->to_string() << "' with: " << nextOp->to_string() << logging::endl;
						}
						else if(move != nullptr && nextMove != nullptr)
						{
							Operation* newMove0 = move->combineWith("mul24");
							Operation* newMove1 = nextMove->combineWith("add");
							if(newMove0 != nullptr && newMove1 != nullptr)
							{
								it.reset(new CombinedOperation(newMove0, newMove1));
								nextIt.erase();
							}
							else
								logging::warn() << "Error combining move-operation '" << move->to_string() << "' with: " << nextMove->to_string() << logging::endl;
						}
						else
							throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled combination, type", (instr->to_string() + ", ") + nextInstr->to_string());
					}
				}
			}
			it.nextInBlock();
		}
	}
}

static Optional<Literal> getSourceLiteral(InstructionWalker it)
{
	if(it.has<LoadImmediate>())
	{
		return it.get<LoadImmediate>()->getImmediate();
	}
	else if(it.has<MoveOperation>() && it.get<MoveOperation>()->getSource().hasType(ValueType::LITERAL))
	{
		return it.get<MoveOperation>()->getSource().literal;
	}
	else if(it.has<MoveOperation>() && it.get<MoveOperation>()->getSource().hasType(ValueType::SMALL_IMMEDIATE))
	{
		return Literal(static_cast<int64_t>(it.get<MoveOperation>()->getSource().immediate.getIntegerValue()));
	}
	else if(it.has<Operation>())
	{
		const auto val = it.get<Operation>()->precalculate(2);
		if(val.hasValue)
			return val.get().literal;
	}
	return Optional<Literal>(false, static_cast<int64_t>(0));
}

static bool canReplaceLiteralLoad(InstructionWalker it, const InstructionWalker start, const InstructionWalker match)
{
	std::size_t stepsLeft = ACCUMULATOR_THRESHOLD_HINT;
	InstructionWalker pos = it;
	//check whether the instruction last loading the same literal is at most ACCUMULATOR_THRESHOLD_HINT instructions before this one
	while(stepsLeft > 0 && !pos.isStartOfBlock() && pos != start)
	{
		if(pos.get() == match.get())
			return true;
		pos.previousInBlock();
		--stepsLeft;
	}
	return false;
}

void optimizations::combineLoadingLiterals(const Module& module, Method& method, const Configuration& config)
{
	for(BasicBlock& block : method.getBasicBlocks())
	{
		FastMap<int64_t, InstructionWalker> lastLoadImmediate;
		InstructionWalker it = block.begin();
		while(!it.isEndOfBlock())
		{
			if(it->hasValueType(ValueType::LOCAL) && it->getOutput().get().local->getUsers(LocalUser::Type::WRITER).size() == 1 && block.isLocallyLimited(it, it->getOutput().get().local))
			{
				Optional<Literal> literal = getSourceLiteral(it);
				if(literal.hasValue)
				{
					if(lastLoadImmediate.find(literal.get().integer) != lastLoadImmediate.end() && canReplaceLiteralLoad(it, block.begin(), lastLoadImmediate.at(literal.get().integer)))
					{
						Local* oldLocal = it->getOutput().get().local;
						Local* newLocal = lastLoadImmediate.at(literal.get().integer)->getOutput().get().local;
						logging::debug() << "Removing duplicate loading of local: " << it->to_string() << logging::endl;
						//Local#forUsers can't be used here, since we modify the list of users via LocalUser#replaceLocal
						FastSet<const LocalUser*> readers = oldLocal->getUsers(LocalUser::Type::READER);
						for(const LocalUser* reader : readers)
						{
							const_cast<LocalUser*>(reader)->replaceLocal(oldLocal, newLocal);
						};
						it.erase();
						continue;
					}
					else
						lastLoadImmediate[literal.get().integer] =  it;
				}
			}
			it.nextInBlock();
		}
	}
}

void optimizations::unrollWorkGroups(const Module& module, Method& method, const Configuration& config)
{
	/*
	 * Kernel Loop Optimization:
	 *
	 * Block for re-running the kernel:
	 * In a loop with a count specified in GROUP_LOOP_SIZE, the kernel is re-run.
	 * This saves some of the overhead for changing the group-id and re-starting the kernel via a mailbox call.
	 *
	 * In return, for every loop iteration there needs to be a UNIFORM with the count of remaining loop iterations left.
	 * Or just a non-zero value for all but the last and a zero-value for the last iteration.
	 * Additionally, all UNIFORMs need to be re-loaded!!
	 */
	const Local* startLabel = method.findOrCreateLocal(TYPE_LABEL, BasicBlock::DEFAULT_BLOCK);

	//add conditional jump to end of kernel, to jump back to the beginning
	const Local* loopSize = method.findOrCreateLocal(TYPE_INT32, Method::GROUP_LOOP_SIZE);
	method.appendToEnd(new MoveOperation(loopSize->createReference(), UNIFORM_REGISTER));
	method.appendToEnd(new Branch(startLabel, COND_ZERO_CLEAR, loopSize->createReference()));
}


InstructionWalker optimizations::combineSelectionWithZero(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
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
	if(!it->getOutput().hasValue || !nextIt->getOutput().hasValue)
		return it;
	MoveOperation* move = it.get<MoveOperation>();
	MoveOperation* nextMove = nextIt.get<MoveOperation>();
	if(it->hasSideEffects() || nextIt->hasSideEffects())
		return it;
	if(it->getOutput().get() != nextIt->getOutput().get())
		return it;
	if(it->conditional == COND_ALWAYS || nextIt->conditional == COND_ALWAYS || !it->conditional.isInversionOf(nextIt->conditional))
		return it;
	//we have two consecutive moves to the same value, without side-effects and inverted conditions.
	//additionally, one of the moves writes a zero-vale
	if(move->getSource().hasLiteral(INT_ZERO.literal))
	{
		logging::debug() << "Rewriting selection of either zero or " << nextMove->getSource().to_string() << " using only one input" << logging::endl;
		it.reset((new Operation("xor", move->getOutput(), nextMove->getSource(), nextMove->getSource()))->copyExtrasFrom(move));
		//to process this instruction again (e.g. loading literals)
		it.previousInBlock();
	}
	else if(nextMove->getSource().hasLiteral(INT_ZERO.literal))
	{
		logging::debug() << "Rewriting selection of either " << move->getSource().to_string() << " or zero using only one input" << logging::endl;
		nextIt.reset((new Operation("xor", nextMove->getOutput(), move->getSource(), move->getSource()))->copyExtrasFrom(nextMove));
	}
	return it;
}

void optimizations::combineVectorRotations(const Module& module, Method& method, const Configuration& config)
{
	for(BasicBlock& block : method.getBasicBlocks())
	{
		InstructionWalker it = block.begin();
		while(!it.isEndOfBlock())
		{
			if(it.has<VectorRotation>() && !it->hasSideEffects())
			{
				VectorRotation* rot = it.get<VectorRotation>();
				if(rot->getSource().hasType(ValueType::LOCAL) && rot->getOffset().hasType(ValueType::SMALL_IMMEDIATE) && rot->getOffset().immediate != VECTOR_ROTATE_R5)
				{
					const LocalUser* writer = rot->getSource().local->getSingleWriter();
					if(writer != nullptr)
					{
						const VectorRotation* firstRot = dynamic_cast<const VectorRotation*>(writer);
						if(firstRot != nullptr && !firstRot->hasSideEffects() && firstRot->getOffset().hasType(ValueType::SMALL_IMMEDIATE) && firstRot->getOffset().immediate != VECTOR_ROTATE_R5)
						{
							auto firstIt = it.getBasicBlock()->findWalkerForInstruction(firstRot, it);
							if(firstIt.hasValue)
							{
								/*
								 * Can combine the offsets of two rotations,
								 * - if the only source of a vector rotation is only written once,
								 * - the source of the input is another vector rotation,
								 * - both rotations only use immediate offsets and
								 * - neither rotation has any side effects
								 */
								const uint8_t offset = (rot->getOffset().immediate.getRotationOffset().get() + firstRot->getOffset().immediate.getRotationOffset().get()) % 16;
								if(offset == 0)
								{
									logging::debug() << "Replacing unnecessary vector rotations " << firstRot->to_string() << " and " << rot->to_string() << " with single move" << logging::endl;
									it.reset((new MoveOperation(rot->getOutput(), firstRot->getSource()))->copyExtrasFrom(rot));
									it->copyExtrasFrom(firstRot);
									firstIt.get().erase();
								}
								else
								{
									logging::debug() << "Combining vector rotations " << firstRot->to_string() << " and " << rot->to_string() << " to a single rotation with offset " << static_cast<unsigned>(offset) << logging::endl;
									it.reset((new VectorRotation(rot->getOutput(), firstRot->getSource(), Value(SmallImmediate::fromRotationOffset(offset), TYPE_INT8)))->copyExtrasFrom(rot));
									it->copyExtrasFrom(firstRot);
									firstIt.get().erase();
								}
							}
						}
					}
				}
			}
			it.nextInBlock();
		}
	}
}
