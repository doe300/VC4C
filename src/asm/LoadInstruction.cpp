/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LoadInstruction.h"

#include "../SIMDVector.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../periphery/VPM.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

LoadInstruction::LoadInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
    Address addOut, Address mulOut, uint32_t value)
{
    setType(OpLoad::LOAD_IMM_32);
    setPack(pack);
    setAddCondition(condAdd);
    setMulCondition(condMul);
    setSetFlag(sf);
    setWriteSwap(ws);
    setAddOut(addOut);
    setMulOut(mulOut);
    setImmediateInt(value);
}

LoadInstruction::LoadInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
    Address addOut, Address mulOut, int16_t value0, int16_t value1)
{
    setType(OpLoad::LOAD_SIGNED);
    setPack(pack);
    setAddCondition(condAdd);
    setMulCondition(condMul);
    setSetFlag(sf);
    setWriteSwap(ws);
    setAddOut(addOut);
    setMulOut(mulOut);
    setImmediateSignedShort0(value0);
    setImmediateSignedShort1(value1);
}

LoadInstruction::LoadInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
    Address addOut, Address mulOut, uint16_t value0, uint16_t value1)
{
    setType(OpLoad::LOAD_UNSIGNED);
    setPack(pack);
    setAddCondition(condAdd);
    setMulCondition(condMul);
    setSetFlag(sf);
    setWriteSwap(ws);
    setAddOut(addOut);
    setMulOut(mulOut);
    setImmediateShort0(value0);
    setImmediateShort1(value1);
}

LCOV_EXCL_START
std::string LoadInstruction::toASMString() const
{
    if(getType() == OpLoad::LOAD_UNSIGNED)
    {
        auto s = std::string("ldui") + (toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag()) + " ") +
            ((toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") +
                intermediate::LoadImmediate::toLoadedValues(
                    getImmediateInt(), intermediate::LoadType::PER_ELEMENT_UNSIGNED)
                    .to_string(true));
        return s;
    }
    if(getType() == OpLoad::LOAD_SIGNED)
    {
        auto s = std::string("ldsi") + (toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag()) + " ") +
            ((toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") +
                intermediate::LoadImmediate::toLoadedValues(
                    getImmediateInt(), intermediate::LoadType::PER_ELEMENT_SIGNED)
                    .to_string(true));
        return s;
    }
    std::string valString;
    if(getAddOut() == REG_VPM_OUT_SETUP.num)
    {
        if(getWriteSwap() == WriteSwap::DONT_SWAP)
            // VPR setup
            valString = periphery::VPRSetup::fromLiteral(getImmediateInt()).to_string();
        else
            // VPW setup
            valString = periphery::VPWSetup::fromLiteral(getImmediateInt()).to_string();
    }
    else
        valString = std::to_string(getImmediateInt());
    auto s = std::string("ldi") + (toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag()) + " ") +
        (toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") + valString;
    return s;
}
LCOV_EXCL_STOP
