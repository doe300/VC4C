/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "BranchInstruction.h"
#include "../Values.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

BranchInstruction::BranchInstruction(const BranchCond cond, const BranchRel relative, const BranchReg addRegister,
    const Address branchRegister, const Address addOut, const Address mulOut, const int32_t offset)
{
    setEntry(OpBranch::BRANCH, 60, MASK_Quadruple);
    setBranchCondition(cond);
    setBranchRelative(relative);
    setAddRegister(addRegister);
    setRegisterAddress(branchRegister);
    setAddOut(addOut);
    setMulOut(mulOut);
    setImmediate(offset);
}

std::string BranchInstruction::toASMString() const
{
    auto s = std::string("br") + (getBranchRelative() == BranchRel::BRANCH_RELATIVE ? "r" : "a") +
        ((getBranchCondition() == BranchCond::ALWAYS ? "" : std::string(".") + toString(getBranchCondition())) + " ") +
        (getAddOut() != REG_NOP.num ? Register{RegisterFile::PHYSICAL_A, getAddOut()}.to_string(true, false) + ", " :
                                      "") +
        (getMulOut() != REG_NOP.num ? Register{RegisterFile::PHYSICAL_B, getMulOut()}.to_string(true, false) + ", " :
                                      "") +
        (getBranchRelative() == BranchRel::BRANCH_RELATIVE ? "(pc+4) + " : "") +
        std::to_string(getImmediate() / 8 /* byte-index -> instruction-index */) +
        (getAddRegister() == BranchReg::BRANCH_REG ?
                std::string(" + ") + Register{RegisterFile::PHYSICAL_A, getRegisterAddress()}.to_string(true, true) :
                "");
    return s;
}
