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

LCOV_EXCL_START
std::string BranchInstruction::toASMString() const
{
    std::string output = "";
    if(getAddOut() != REG_NOP.num)
        output += toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ";
    if(getMulOut() != REG_NOP.num)
        output += toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getMulOut()) + ", ";

    std::string target = "";
    if(getBranchRelative() == BranchRel::BRANCH_RELATIVE)
        target += "(pc+4) + ";
    target += std::to_string(getImmediate() / 8 /* byte-index -> instruction-index */);
    if(getAddRegister() == BranchReg::BRANCH_REG)
        target += " + " + toInputRegister(InputMultiplex::REGA, getAddOut(), getMulOut(), false);

    auto s = std::string("br") + (getBranchRelative() == BranchRel::BRANCH_RELATIVE ? "r" : "a") +
        ((getBranchCondition() == BRANCH_ALWAYS ? "" : std::string(".") + getBranchCondition().to_string()) + " ") +
        output + target;
    return s;
}
LCOV_EXCL_STOP
