/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef BRANCHINSTRUCTION_H
#define BRANCHINSTRUCTION_H

#include "Instruction.h"

namespace vc4c
{
	namespace qpu_asm
	{

		class BranchInstruction: public Instruction
		{
		public:
			BranchInstruction(BranchCond cond, BranchRel relative, BranchReg addRegister, Address branchRegister, Address addOut, Address mulOut, int32_t offset);
			~BranchInstruction() override = default;

			std::string toASMString() const override;

			BITFIELD_ENTRY(BranchCondition, BranchCond, 52, Quadruple)
			BITFIELD_ENTRY(BranchRelative, BranchRel, 51, Bit)
			BITFIELD_ENTRY(AddRegister, BranchReg, 50, Bit)
			BITFIELD_ENTRY(RegisterAddress, Address, 45, Quintuple)
			BITFIELD_ENTRY(AddOut, Address, 38, Sextuple)
			BITFIELD_ENTRY(MulOut, Address, 32, Sextuple)

			BITFIELD_ENTRY(Immediate, int32_t, 0, SignedInt)
		};
	} // namespace qpu_asm
} // namespace vc4c



#endif /* BRANCHINSTRUCTION_H */

