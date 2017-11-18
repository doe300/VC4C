/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SEMAPHOREINSTRUCTION_H
#define SEMAPHOREINSTRUCTION_H

#include "Instruction.h"

namespace vc4c
{
	namespace qpu_asm
	{

		class SemaphoreInstruction: public Instruction
		{
		public:
			SemaphoreInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws, Address addOut, Address mulOut, bool increment, Semaphore semaphore);
			~SemaphoreInstruction() override = default;

			std::string toASMString() const override;

			BITFIELD_ENTRY(Pack, Pack, 52, Quintuple)
			BITFIELD_ENTRY(AddCondition, ConditionCode, 49, Triple)
			BITFIELD_ENTRY(MulCondition, ConditionCode, 46, Triple)
			BITFIELD_ENTRY(SetFlag, SetFlag, 45, Bit)
			BITFIELD_ENTRY(WriteSwap, WriteSwap, 44, Bit)
			BITFIELD_ENTRY(AddOut, Address, 38, Sextuple)
			BITFIELD_ENTRY(MulOut, Address, 32, Sextuple)

			BITFIELD_ENTRY(IncrementSemaphore, bool, 4, Bit)
			BITFIELD_ENTRY(Semaphore, Semaphore, 0, Quadruple)
		};
	} // namespace qpu_asm
} // namespace vc4c

#endif /* SEMAPHOREINSTRUCTION_H */

