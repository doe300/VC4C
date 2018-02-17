/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef ALUINSTRUCTION_H
#define ALUINSTRUCTION_H

#include "Instruction.h"

namespace vc4c
{
	struct SmallImmediate;

	namespace qpu_asm
	{

		class ALUInstruction : public Instruction
		{
		public:

			explicit ALUInstruction(uint64_t code) : Instruction(code) { }
			ALUInstruction(Signaling sig, Unpack unpack, Pack pack,
					ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
					Address addOut, Address mulOut, const OpCode& mul, const OpCode& add,
					Address addInA, Address addInB,
					InputMultiplex muxAddA, InputMultiplex muxAddB, InputMultiplex muxMulA, InputMultiplex muxMulB);
			ALUInstruction(Unpack unpack, Pack pack,
					ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
					Address addOut, Address mulOut, const OpCode& mul, const OpCode& add,
					Address addInA, SmallImmediate addInB,
					InputMultiplex muxAddA, InputMultiplex muxAddB, InputMultiplex muxMulA, InputMultiplex muxMulB);
			~ALUInstruction() override = default;

			std::string toASMString() const override;
			bool isValidInstruction() const override;

			BITFIELD_ENTRY(Unpack, Unpack, 57, Triple)
			BITFIELD_ENTRY(Pack, Pack, 52, Quintuple)
			BITFIELD_ENTRY(AddCondition, ConditionCode, 49, Triple)
			BITFIELD_ENTRY(MulCondition, ConditionCode, 46, Triple)
			BITFIELD_ENTRY(SetFlag, SetFlag, 45, Bit)

			BITFIELD_ENTRY(Multiplication, unsigned char, 29, Triple)
			BITFIELD_ENTRY(Addition, unsigned char, 24, Quintuple)
			BITFIELD_ENTRY(InputA, Address, 18, Sextuple)
			BITFIELD_ENTRY(InputB, Address, 12, Sextuple)
			BITFIELD_ENTRY(AddMultiplexA, InputMultiplex, 9, Triple)
			BITFIELD_ENTRY(AddMultiplexB, InputMultiplex, 6, Triple)
			BITFIELD_ENTRY(MulMultiplexA, InputMultiplex, 3, Triple)
			BITFIELD_ENTRY(MulMultiplexB, InputMultiplex, 0, Triple)
		};
	} // namespace qpu_asm
} // namespace vc4c


#endif /* ALUINSTRUCTION_H */

