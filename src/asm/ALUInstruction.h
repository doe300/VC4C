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
	namespace qpu_asm
	{

		class ALUInstruction : public Instruction
		{
		public:

			ALUInstruction();
			ALUInstruction(const Signaling sig, const Unpack unpack, const Pack pack,
					const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws,
					const Address addOut, const Address mulOut, const OpCode& mul, const OpCode& add,
					const Address addInA, const Address addInB,
					const InputMutex muxAddA, const InputMutex muxAddB, const InputMutex muxMulA, const InputMutex muxMulB);
			ALUInstruction(const Unpack unpack, const Pack pack,
					const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws,
					const Address addOut, const Address mulOut, const OpCode& mul, const OpCode& add,
					const Address addInA, const SmallImmediate addInB,
					const InputMutex muxAddA, const InputMutex muxAddB, const InputMutex muxMulA, const InputMutex muxMulB);
			virtual ~ALUInstruction();

			std::string toASMString() const override;

			BITFIELD_ENTRY(Sig, Signaling, 60, Quadruple)
			BITFIELD_ENTRY(Unpack, Unpack, 57, Triple)
			BITFIELD_ENTRY(Pack, Pack, 52, Quintuple)
			BITFIELD_ENTRY(AddCondition, ConditionCode, 49, Triple)
			BITFIELD_ENTRY(MulCondition, ConditionCode, 46, Triple)
			BITFIELD_ENTRY(SetFlag, SetFlag, 45, Bit)
			BITFIELD_ENTRY(WriteSwap, WriteSwap, 44, Bit)
			BITFIELD_ENTRY(AddOut, Address, 38, Sextuple)
			BITFIELD_ENTRY(MulOut, Address, 32, Sextuple)

			BITFIELD_ENTRY(Multiplication, unsigned char, 29, Triple)
			BITFIELD_ENTRY(Addition, unsigned char, 24, Quintuple)
			BITFIELD_ENTRY(InputA, Address, 18, Sextuple)
			BITFIELD_ENTRY(InputB, Address, 12, Sextuple)
			BITFIELD_ENTRY(AddMutexA, InputMutex, 9, Triple)
			BITFIELD_ENTRY(AddMutexB, InputMutex, 6, Triple)
			BITFIELD_ENTRY(MulMutexA, InputMutex, 3, Triple)
			BITFIELD_ENTRY(MulMutexB, InputMutex, 0, Triple)
		};
	}
}


#endif /* ALUINSTRUCTION_H */

