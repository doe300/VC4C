/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Reordering.h"

#include "../intermediate/Helper.h"
#include "../Profiler.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;

/*
 * Finds the last instruction before the (list of) NOP(s) that is not a NOP -> the reason for the insertion of NOPs
 */
static InstructionWalker findPreviousInstruction(BasicBlock& basicBlock, const InstructionWalker pos)
{
	PROFILE_START(findPreviousInstruction);
	auto it = pos;
	while(!it.isStartOfBlock())
	{
		if(it.get() != nullptr && it->mapsToASMInstruction() && it->getOutput().hasValue)
			break;
		it.previousInBlock();
	}
	PROFILE_END(findPreviousInstruction);
	return it;
}

/*
 * Finds an instruction within the basic block that does not access any of the given values
 */
static InstructionWalker findInstructionNotAccessing(BasicBlock& basicBlock, const InstructionWalker pos, FastSet<Value>& excludedValues)
{
	std::size_t instructionsLeft = REPLACE_NOP_MAX_INSTRUCTIONS_TO_CHECK;
	auto it = pos;
	while(instructionsLeft > 0 && !it.isEndOfBlock())
	{
		if(it.get() == nullptr)
		{
			//skip already replaced instructions
			it.nextInBlock();
			--instructionsLeft;
			continue;
		}
		bool validReplacement = true;
		PROFILE_START(checkExcludedValues);
		if(it->getOutput().hasValue && excludedValues.find(it->getOutput().get()) != excludedValues.end())
		{
			validReplacement = false;
		}
		for(const Value& arg : it->getArguments())
		{
			if(excludedValues.find(arg) != excludedValues.end())
			{
				validReplacement = false;
				break;
			}
		}
		PROFILE_END(checkExcludedValues);
		if(validReplacement && it->writesRegister(REG_MUTEX))
		{
			//never move MUTEX_RELEASE
			validReplacement = false;
			//Allow instructions to be moved over MUTEX_RELEASE to replace the VPM wait nops
		}
		if(validReplacement && it->readsRegister(REG_MUTEX))
		{
			//Re-ordering MUTEX_ACQUIRE would extend the critical section (maybe a lot!), so don't move it
			validReplacement = false;
			//Also, never move anything out of (or over) the critical section
			return basicBlock.end();

		}
		//for now, skip everything setting and using flags/signals
		if(validReplacement && (it->hasConditionalExecution() || it->hasSideEffects()))
		{
			validReplacement = false;
		}
		if(validReplacement && (it.has<Branch>() || it.has<BranchLabel>() || it.has<MemoryBarrier>()))
		{
			//NEVER RE-ORDER BRANCHES, LABELS OR BARRIERS!
			validReplacement = false;
		}
		if(validReplacement && it.has<Nop>())
		{
			//replacing NOP with NOP will violate the delay (e.g. for branches, SFU)
			validReplacement = false;
		}
		if(validReplacement && !it->mapsToASMInstruction())
		{
			//skip every instruction, which is not mapped to machine code, since otherwise the delay for the NOP will be violated
			validReplacement = false;
		}
		if(validReplacement)
		{
			logging::debug() << "Found instruction not using any of the excluded values (" << to_string<Value, FastSet<Value>>(excludedValues) << "): " << it->to_string() << logging::endl;
			break;
		}

		//otherwise add all outputs by instructions in between (the NOP and the replacement), since they could be used as input in the following instructions
		if(it->getOutput() && !it->writesRegister(REG_NOP))
		{
			excludedValues.insert(it->getOutput().get());
			//make sure, SFU/TMU calls are not moved over other SFU/TMU calls
			//this prevents nop-sfu-... from being replaced with sfu-sfu-...
			if(it->writesRegister(REG_SFU_EXP2) || it->writesRegister(REG_SFU_LOG2) || it->writesRegister(REG_SFU_RECIP)
					|| it->writesRegister(REG_SFU_RECIP_SQRT) || it->writesRegister(REG_TMU0_ADDRESS) || it->writesRegister(REG_TMU1_ADDRESS))
			{
				excludedValues.emplace(Value(REG_SFU_EXP2, TYPE_FLOAT));
				excludedValues.emplace(Value(REG_SFU_LOG2, TYPE_FLOAT));
				excludedValues.emplace(Value(REG_SFU_OUT, TYPE_FLOAT));
				excludedValues.emplace(Value(REG_SFU_RECIP, TYPE_FLOAT));
				excludedValues.emplace(Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT));
				excludedValues.emplace(Value(REG_TMU0_ADDRESS, TYPE_VOID.toPointerType()));
				excludedValues.emplace(Value(REG_TMU1_ADDRESS, TYPE_VOID.toPointerType()));
			}
		}
		--instructionsLeft;
		it.nextInBlock();
	}
	if(instructionsLeft == 0)
		it = basicBlock.end();
	return it;
}

/*
 * Finds a suitable instruction within this basic block to replace the NOP with, without violating the reason for the NOP.
 * Also, this instruction MUST not be dependent on any instruction in between the NOP and the replacement-instruction
 */
static InstructionWalker findReplacementCandidate(BasicBlock& basicBlock, const InstructionWalker pos, const DelayType nopReason)
{
	PROFILE_START(findReplacementCandidate);
	FastSet<Value> excludedValues;
	InstructionWalker replacementIt = basicBlock.end();
	switch(nopReason)
	{
		case DelayType::BRANCH_DELAY:
			//This type of NOPs do not yet exist (they are created in CodeGenerator)
			PROFILE_END(findReplacementCandidate);
			return basicBlock.end();
		case DelayType::THREAD_END:
			//there are no more instructions after THREND
			PROFILE_END(findReplacementCandidate);
			return basicBlock.end();
		case DelayType::WAIT_VPM:
		case DelayType::WAIT_REGISTER:
		{
			//can insert any instruction which does not access the given register/local
			const InstructionWalker lastInstruction = findPreviousInstruction(basicBlock, pos);
			if(lastInstruction.isStartOfBlock())
			{
				//this can e.g. happen, if the vector rotation is the first instruction in a basic block
				//TODO for now, we can't handle this case, since there may be several writing instructions jumping to the block
				logging::debug() << "Can't find reason for NOP in block: " << basicBlock.begin()->to_string() << logging::endl;
				return basicBlock.end();
			}
			excludedValues.insert(lastInstruction->getOutput().get());
			if(lastInstruction->writesRegister(REG_VPM_IN_ADDR))
			{
				excludedValues.emplace(Value(REG_VPM_IN_BUSY, TYPE_UNKNOWN));
				excludedValues.emplace(Value(REG_VPM_IO, TYPE_UNKNOWN));
			}
			if(lastInstruction->writesRegister(REG_VPM_OUT_ADDR))
			{
				excludedValues.emplace(Value(REG_VPM_OUT_BUSY, TYPE_UNKNOWN));
				excludedValues.emplace(Value(REG_VPM_IO, TYPE_UNKNOWN));
			}
			PROFILE_START(findInstructionNotAccessing);
			replacementIt = findInstructionNotAccessing(basicBlock, pos, excludedValues);
			PROFILE_END(findInstructionNotAccessing);
			break;
		}
		case DelayType::WAIT_SFU:
		case DelayType::WAIT_TMU:
		{
			//can insert any instruction which doesn't access SFU/TMU or accumulator r4
			excludedValues.emplace(Value(REG_SFU_EXP2, TYPE_FLOAT));
			excludedValues.emplace(Value(REG_SFU_LOG2, TYPE_FLOAT));
			excludedValues.emplace(Value(REG_SFU_OUT, TYPE_FLOAT));
			excludedValues.emplace(Value(REG_SFU_RECIP, TYPE_FLOAT));
			excludedValues.emplace(Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT));
			excludedValues.emplace(Value(REG_TMU0_ADDRESS, TYPE_VOID.toPointerType()));
			excludedValues.emplace(Value(REG_TMU1_ADDRESS, TYPE_VOID.toPointerType()));
			PROFILE_START(findInstructionNotAccessing);
			replacementIt = findInstructionNotAccessing(basicBlock, pos, excludedValues);
			PROFILE_END(findInstructionNotAccessing);
			break;
		}
		case DelayType::WAIT_UNIFORM:
			//TODO could reorder, as long as we do not access uniforms ??
			PROFILE_END(findReplacementCandidate);
			return basicBlock.end();
	}
	PROFILE_END(findReplacementCandidate);
	return replacementIt;
}

InstructionWalker optimizations::moveInstructionUp(InstructionWalker dest, InstructionWalker it)
{
	/*
	 * a b c d e f
	 * f b c d e a
	 * f a c d e b
	 * f a b d e c
	 * f a b c e d
	 * f a b c d e
	 */
//	InstructionsIterator next = dest;
//	while(next != it)
//	{
//		std::iter_swap(next, it);
//		++next;
//	}

	/*!
	 * a b c d e f
	 * f a b c d e nil
	 * f a b c d e
	 */
	auto res = dest.emplace(it.release());
	it.erase();
	return res;
}

static void replaceNOPs(BasicBlock& basicBlock, Method& method)
{
	InstructionWalker it = basicBlock.begin();
	while(!it.isEndOfBlock())
	{
		const Nop* nop = it.get<Nop>();
		//only replace NOPs without side-effects (e.g. signal)
		if(nop != nullptr && !nop->hasSideEffects())
		{
			InstructionWalker replacementIt = findReplacementCandidate(basicBlock, it, nop->type);
			if(!replacementIt.isEndOfBlock())
			{
				// replace NOP with instruction, reset instruction at position (do not yet erase, otherwise iterators are wrong!)
				logging::debug() << "Replacing NOP with: " << replacementIt->to_string() << logging::endl;
				bool cannotBeCombined = !it->canBeCombined;
				it.reset(replacementIt.release());
				if(cannotBeCombined)
					it->canBeCombined = false;
			}
			else if(nop->type == DelayType::WAIT_VPM)
			{
				//nops inserted to wait for VPM to finish can be removed again,
				//since the wait-instruction will correctly wait the remaining number of instructions
				it.erase();
				//to not skip the next nop
				it.previousInBlock();
			}
		}
		it.nextInBlock();
	}
}

void optimizations::splitReadAfterWrites(const Module& module, Method& method, const Configuration& config)
{
	//try to split up consecutive instructions writing/reading to the same local (so less locals are forced to accumulators) by inserting NOPs
	//the NOP then can be replaced with other instructions by the next optimization (#reorderWithinBasicBlocks)
	auto it = method.walkAllInstructions();
	InstructionWalker lastInstruction = it;
	const Local* lastWrittenTo = nullptr;
	//skip the first instruction, since we start the check at the read (and need to look back at the write)
	it.nextInMethod();
	while(!it.isEndOfMethod())
	{
		//skip already replaced instructions
		if(it.get() != nullptr)
		{
			if(lastWrittenTo != nullptr)
			{
				if(it->readsLocal(lastWrittenTo))
				{
					//only insert instruction, if local is used afterwards (and not just in the next few instructions)
					//or the pack-mode of the previous instruction is set, since in that case, the register-file A MUST be used, so it cannot be read in the next instruction
					//or the unpack-mode of this instruction is set, since in that case, the register-file A MUST be used, so it cannot be written to in the previous instruction
					//also vector-rotations MUST be on accumulator, but the input MUST NOT be written in the previous instruction, so they are also split up
					if(lastInstruction->hasPackMode() || it->hasUnpackMode() || it.has<VectorRotation>() || !lastInstruction.getBasicBlock()->isLocallyLimited(lastInstruction, lastWrittenTo))
					{
						logging::debug() << "Inserting NOP to split up read-after-write before: " << it->to_string() << logging::endl;
						//emplacing after the last instruction instead of before this one fixes errors with wrote-label-read, which then becomes
						//write-nop-label-read instead of write-label-nop-read and the combiner can find a reason for the NOP
						lastInstruction.copy().nextInBlock().emplace(new Nop(DelayType::WAIT_REGISTER));
					}
				}
			}
			if(it->mapsToASMInstruction())
			{
				//ignoring instructions not mapped to machine code, e.g. labels will also check for write-label-read
				lastWrittenTo = it->hasValueType(ValueType::LOCAL) ? it->getOutput().get().local : nullptr;
				lastInstruction = it;
			}

			if(it->readsRegister(REG_VPM_IN_WAIT) || it->readsRegister(REG_VPM_OUT_WAIT) || it->readsRegister(REG_VPM_IO))
			{
				//TODO constant + x * nrows
				unsigned numDelays = 0;
				if(it->readsRegister(REG_VPM_IN_WAIT))
					numDelays = 6; //XXX 8
				else if(it->readsRegister(REG_VPM_OUT_WAIT))
					numDelays = 10; //XXX 12
				//TODO else insert delay only before first read!
				for(unsigned i = 0; i < numDelays; ++i)
				{
					it.emplace(new Nop(DelayType::WAIT_VPM));
					it.nextInBlock();
				}
			}
		}
		it.nextInMethod();
	}
}

void optimizations::reorderWithinBasicBlocks(const Module& module, Method& method, const Configuration& config)
{
    /*
     * TODO re-order instructions to:
     * 2. combine instructions(try to pair instruction from ADD and MUL ALU together, or moves)
     * 3. split up VPM setup and wait VPM wait, so the delay can be used productively (only possible if we allow reordering over mutex-release).
     *    How many instructions to try to insert? 3?
     */
	for(BasicBlock& block : method.getBasicBlocks())
	{
		// remove NOPs by inserting instructions which do not violate the reason for the NOP
		PROFILE(replaceNOPs, block, method);
	}

	//after all re-orders are done, remove empty instructions
	method.cleanEmptyInstructions();
}

InstructionWalker optimizations::moveRotationSourcesToAccumulators(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	//makes sure, all sources for vector-rotations have a usage-range small enough to be on an accumulator
	if(it.has<VectorRotation>() && it.get<VectorRotation>()->getSource().hasType(ValueType::LOCAL))
	{
		const Local* loc = it.get<VectorRotation>()->getSource().local;
		InstructionWalker writer = it.copy().previousInBlock();
		while(!writer.isStartOfBlock())
		{
			if(writer.has() && writer->hasValueType(ValueType::LOCAL) && writer->getOutput().get().hasLocal(loc))
				break;
			writer.previousInBlock();
		}
		//if the local is either written in another block or the usage-range exceeds the accumulator threshold, move to temporary
		if(writer.isStartOfBlock() || !writer.getBasicBlock()->isLocallyLimited(writer, loc))
		{
			InstructionWalker mapper = it.copy().previousInBlock();
			//insert mapper before first NOP
			while(mapper.copy().previousInBlock().has<Nop>())
				mapper.previousInBlock();
			logging::debug() << "Moving source of vector-rotation to temporary for: " << it->to_string() << logging::endl;
			const Value tmp = method.addNewLocal(loc->type, "%vector_rotation");
			mapper.emplace(new MoveOperation(tmp, loc->createReference()));
			it->replaceLocal(loc, tmp.local, LocalUser::Type::READER);
			return writer;
		}
	}
	return it;
}
