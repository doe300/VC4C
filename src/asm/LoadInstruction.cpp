/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LoadInstruction.h"

#include "../periphery/VPM.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

LoadInstruction::LoadInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, 
                                 const SetFlag sf, const WriteSwap ws, 
                                 const Address addOut, const Address mulOut, const uint32_t value)
{
	setEntry(OpLoad::LOAD_IMM_32, 57, MASK_Septuple);
    setPack(pack);
    setAddCondition(condAdd);
    setMulCondition(condMul);
    setSetFlag(sf);
    setWriteSwap(ws);
    setAddOut(addOut);
    setMulOut(mulOut);
    setImmediateInt(value);
}

LoadInstruction::LoadInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, 
                                 const SetFlag sf, const WriteSwap ws, const Address addOut, const Address mulOut, 
                                 const int16_t value0, int16_t value1)
{
	setEntry(OpLoad::LOAD_SIGNED, 57, MASK_Septuple);
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

LoadInstruction::LoadInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, 
                                 const SetFlag sf, const WriteSwap ws, const Address addOut, const Address mulOut, 
                                 const uint16_t value0, uint16_t value1)
{
    setEntry(OpLoad::LOAD_UNSIGNED, 57, MASK_Septuple);
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

std::string LoadInstruction::toASMString() const
{
	if(getEntry<uint8_t>(57, MASK_Septuple) == static_cast<uint8_t>(OpLoad::LOAD_SIGNED))
	{
		auto s = std::string("ldi") + (toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag()) + " ") +
		            ((toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") + std::to_string(getImmediateSignedShort0()) + ", ") + std::to_string(getImmediateSignedShort1());
		return addComment(s);
	}
	if(getEntry<uint8_t>(57, MASK_Septuple) == static_cast<uint8_t>(OpLoad::LOAD_UNSIGNED))
	{
		auto s = std::string("ldi") + (toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag()) + " ") +
				 ((toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") + std::to_string(getImmediateShort0()) + ", ") + std::to_string(getImmediateShort1());
		return addComment(s);
	}
	std::string valString;
	if(getAddOut() == REG_VPM_OUT_SETUP.num)
	{
		if(getWriteSwap() == WriteSwap::DONT_SWAP)
			//VPR setup
			valString = periphery::VPRSetup::fromLiteral(getImmediateInt()).to_string();
		else
			//VPW setup
			valString = periphery::VPWSetup::fromLiteral(getImmediateInt()).to_string();
	}
	else
		valString = std::to_string(getImmediateInt());
    auto s = std::string("ldi") + (toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag()) + " ") +
			 (toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") + valString;
	return addComment(s);
}

bool LoadInstruction::isValidInstruction() const
{
	if(getSig() != SIGNAL_LOAD_IMMEDIATE)
		return false;
	return getEntry<OpLoad>(57, MASK_Septuple) == OpLoad::LOAD_IMM_32 || getEntry<OpLoad>(57, MASK_Septuple) == OpLoad::LOAD_SIGNED || getEntry<OpLoad>(57, MASK_Septuple) == OpLoad::LOAD_UNSIGNED;
}
