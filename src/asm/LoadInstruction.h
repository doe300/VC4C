/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LOADINSTRUCTION_H
#define LOADINSTRUCTION_H

#include "Instruction.h"
namespace vc4c
{

	namespace qpu_asm
	{

		class LoadInstruction: public Instruction
		{
		public:
			LoadInstruction();
			LoadInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws, const Address addOut, const Address mulOut, const uint32_t value);
			LoadInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws, const Address addOut, const Address mulOut, const int16_t value0, int16_t value1);
			LoadInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws, const Address addOut, const Address mulOut, const uint16_t value0, uint16_t value1);
			~LoadInstruction();

			std::string toASMString() const override;

			BITFIELD_ENTRY(Pack, Pack, 52, Quintuple)
			BITFIELD_ENTRY(AddCondition, ConditionCode, 49, Triple)
			BITFIELD_ENTRY(MulCondition, ConditionCode, 46, Triple)
			BITFIELD_ENTRY(SetFlag, SetFlag, 45, Bit)
			BITFIELD_ENTRY(WriteSwap, WriteSwap, 44, Bit)
			BITFIELD_ENTRY(AddOut, Address, 38, Sextuple)
			BITFIELD_ENTRY(MulOut, Address, 32, Sextuple)

			BITFIELD_ENTRY(ImmediateInt, uint32_t, 0, Int)
			BITFIELD_ENTRY(ImmediateShort0, uint16_t, 16, Short)
			BITFIELD_ENTRY(ImmediateShort1, uint16_t, 0, Short)
			BITFIELD_ENTRY(ImmediateSignedShort0, int16_t, 16, SignedShort)
			BITFIELD_ENTRY(ImmediateSignedShort1, int16_t, 0, SignedShort)
		};
	}
}

#endif /* LOADINSTRUCTION_H */

