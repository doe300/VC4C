/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ALUInstruction.h"

#include "../Values.h"

#include <stdexcept>

using namespace vc4c;
using namespace vc4c::qpu_asm;

ALUInstruction::ALUInstruction(const Signaling sig, const Unpack unpack, const Pack pack,
                               const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws,
                               const Address addOut, const Address mulOut,
                               const OpCode& mul, const OpCode& add, const Address addInA, const Address addInB,
                               const InputMutex muxAddA, const InputMutex muxAddB, const InputMutex muxMulA, const InputMutex muxMulB)
{
    this->setSig(sig);
    this->setUnpack(unpack);
    this->setPack(pack);
    this->setAddCondition(condAdd);
    this->setMulCondition(condMul);
    this->setSetFlag(sf);
    this->setWriteSwap(ws);
    this->setAddOut(addOut);
    this->setMulOut(mulOut);

    this->setMultiplication(mul.opMul);
    this->setAddition(add.opAdd);
    this->setInputA(addInA);
    this->setInputB(addInB);
    this->setAddMutexA(muxAddA);
    this->setAddMutexB(muxAddB);
    this->setMulMutexA(muxMulA);
    this->setMulMutexB(muxMulB);

    if(( mul != OP_NOP && !mul.runsOnMulALU()) || (add != OP_NOP && !add.runsOnAddALU()))
    	throw CompilationError(CompilationStep::CODE_GENERATION, "Opcode specified for wrong ALU");
}

ALUInstruction::ALUInstruction(const Unpack unpack, const Pack pack, 
                               const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws, 
                               const Address addOut, const Address mulOut, const OpCode& mul, const OpCode& add, const Address addInA, const SmallImmediate addInB,
                               const InputMutex muxAddA, const InputMutex muxAddB, const InputMutex muxMulA, const InputMutex muxMulB)
{
    this->setSig(SIGNAL_ALU_IMMEDIATE);
    this->setUnpack(unpack);
    this->setPack(pack);
    this->setAddCondition(condAdd);
    this->setMulCondition(condMul);
    this->setSetFlag(sf);
    this->setWriteSwap(ws);
    this->setAddOut(addOut);
    this->setMulOut(mulOut);

    this->setMultiplication(mul.opMul);
    this->setAddition(add.opAdd);
    this->setInputA(addInA);
    this->setInputB(static_cast<Address>(addInB));
    this->setAddMutexA(muxAddA);
    this->setAddMutexB(muxAddB);
    this->setMulMutexA(muxMulA);
    this->setMulMutexB(muxMulB);

    if(( mul != OP_NOP && !mul.runsOnMulALU()) || (add != OP_NOP && !add.runsOnAddALU()))
		throw CompilationError(CompilationStep::CODE_GENERATION, "Opcode specified for wrong ALU");
}

std::string ALUInstruction::toASMString() const
{
	const OpCode& opAdd = OpCode::toOpCode(getAddition(), false);
	const OpCode& opMul = OpCode::toOpCode(getMultiplication(), true);
    std::string addPart;
    std::string mulPart;
    bool hasImmediate = getSig() == SIGNAL_ALU_IMMEDIATE;
    std::string addArgs, mulArgs;
    bool addCanUnpack = false;
    bool mulCanUnpack = false;
    if(opAdd.numOperands > 0)
    {
    	addArgs.append(", ").append(toInputRegister(getAddMutexA(), getInputA(), getInputB(), hasImmediate));
    	addCanUnpack |= getAddMutexA() == InputMutex::REGA;
    	addCanUnpack |= getAddMutexA() == InputMutex::ACC4;
    }
    if(opAdd.numOperands > 1)
    {
		addArgs.append(", ").append(toInputRegister(getAddMutexB(), getInputA(), getInputB(), hasImmediate));
		addCanUnpack |= getAddMutexB() == InputMutex::REGA;
		addCanUnpack |= getAddMutexB() == InputMutex::ACC4;
    }
    if(opMul.numOperands > 0)
    {
    	mulArgs.append(", ").append(toInputRegister(getMulMutexA(), getInputA(), getInputB(), hasImmediate));
    	mulCanUnpack |= getMulMutexA() == InputMutex::REGA;
    	mulCanUnpack |= getMulMutexA() == InputMutex::ACC4;
    }
    if(opMul.numOperands > 1)
    {
		mulArgs.append(", ").append(toInputRegister(getMulMutexB(), getInputA(), getInputB(), hasImmediate));
		mulCanUnpack |= getMulMutexB() == InputMutex::REGA;
		mulCanUnpack |= getMulMutexB() == InputMutex::ACC4;
    }
    addPart = std::string(opAdd.name) + (toExtrasString(getSig(), getAddCondition(), getSetFlag(), getUnpack(), getPack(), getWriteSwap() == WriteSwap::DONT_SWAP, addCanUnpack) + " ") + (opAdd != OP_NOP ? toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) : "") + addArgs;
    mulPart = std::string(opMul.name) + (toExtrasString(getSig(), getMulCondition(), getSetFlag(), getUnpack(), getPack(), getWriteSwap() == WriteSwap::SWAP, mulCanUnpack) + " ") + (opMul != OP_NOP ? toOutputRegister(getWriteSwap() == WriteSwap::SWAP, getMulOut()) : "") + mulArgs;
    if(getMulMutexA() != InputMutex::REGA && getMulMutexA() != InputMutex::REGB && getMulMutexB() != InputMutex::REGA && getMulMutexB() != InputMutex::REGB && getSig() == SIGNAL_ALU_IMMEDIATE && (opAdd == OP_NOP || (getAddMutexA() != InputMutex::REGB && getAddMutexB() != InputMutex::REGB)))
    {
    	//both inputs for mul are accumulators, an immediate value is used
    	//and the ADD ALU executes NOP or both inputs from the ADD ALU are not on register-file B
    	// -> vector rotation
    	mulPart.append(" ").append(static_cast<SmallImmediate>(getInputB()).toString());
    }
    if(opAdd != OP_NOP)
    {
        if(opMul != OP_NOP)
        {
            return (addPart + "; ") + mulPart;
        }
        return addPart;
    }
    if(opMul != OP_NOP)
        return mulPart;
    return addPart;
}

bool ALUInstruction::isValidInstruction() const
{
	switch(getSig().value)
	{
		case SIGNAL_BRANCH.value:
		case SIGNAL_LOAD_IMMEDIATE.value:
			return false;
		default:
			return true;
	}
}
