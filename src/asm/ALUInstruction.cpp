/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <stdexcept>

#include "ALUInstruction.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

ALUInstruction::ALUInstruction()
{
}

ALUInstruction::ALUInstruction(const Signaling sig, const Unpack unpack, const Pack pack,
                               const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws,
                               const Address addOut, const Address mulOut,
                               const OpMul mul, const OpAdd add, const Address addInA, const Address addInB,
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

    this->setMultiplication(mul);
    this->setAddition(add);
    this->setInputA(addInA);
    this->setInputB(addInB);
    this->setAddMutexA(muxAddA);
    this->setAddMutexB(muxAddB);
    this->setMulMutexA(muxMulA);
    this->setMulMutexB(muxMulB);
}

ALUInstruction::ALUInstruction(const Unpack unpack, const Pack pack, 
                               const ConditionCode condAdd, const ConditionCode condMul, const SetFlag sf, const WriteSwap ws, 
                               const Address addOut, const Address mulOut, const OpMul mul, const OpAdd add, const Address addInA, const SmallImmediate addInB, 
                               const InputMutex muxAddA, const InputMutex muxAddB, const InputMutex muxMulA, const InputMutex muxMulB)
{
    this->setSig(Signaling::ALU_IMMEDIATE);
    this->setUnpack(unpack);
    this->setPack(pack);
    this->setAddCondition(condAdd);
    this->setMulCondition(condMul);
    this->setSetFlag(sf);
    this->setWriteSwap(ws);
    this->setAddOut(addOut);
    this->setMulOut(mulOut);

    this->setMultiplication(mul);
    this->setAddition(add);
    this->setInputA(addInA);
    this->setInputB(static_cast<Address>(addInB));
    this->setAddMutexA(muxAddA);
    this->setAddMutexB(muxAddB);
    this->setMulMutexA(muxMulA);
    this->setMulMutexB(muxMulB);
}


ALUInstruction::~ALUInstruction()
{
}

std::string ALUInstruction::toASMString() const
{
    std::string addPart;
    std::string mulPart;
    bool hasImmediate = getSig() == Signaling::ALU_IMMEDIATE;
    std::string addArgs, mulArgs;
    if(getAddition().numOperands > 0)
    	addArgs.append(", ").append(toInputRegister(getAddMutexA(), getInputA(), getInputB(), hasImmediate));
    if(getAddition().numOperands > 1)
		addArgs.append(", ").append(toInputRegister(getAddMutexB(), getInputA(), getInputB(), hasImmediate));
    if(getMultiplication().numOperands > 0)
    	mulArgs.append(", ").append(toInputRegister(getMulMutexA(), getInputA(), getInputB(), hasImmediate));
    if(getMultiplication().numOperands > 1)
		mulArgs.append(", ").append(toInputRegister(getMulMutexB(), getInputA(), getInputB(), hasImmediate));
    addPart = std::string(getAddition().name) + (toExtrasString(getSig(), getAddCondition(), getSetFlag(), getUnpack(), getPack()) + " ") + (getAddition() != OPADD_NOP ? toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) : "") + addArgs;
    mulPart = std::string(getMultiplication().name) + (toExtrasString(getSig(), getMulCondition(), getSetFlag(), getUnpack(), getPack()) + " ") + (getMultiplication() != OPMUL_NOP ? toOutputRegister(getWriteSwap() == WriteSwap::SWAP, getMulOut()) : "") + mulArgs;
    if(getMulMutexA() != InputMutex::REGA && getMulMutexA() != InputMutex::REGB && getMulMutexB() != InputMutex::REGA && getMulMutexB() != InputMutex::REGB && getSig() == Signaling::ALU_IMMEDIATE && (getAddition() == OPADD_NOP || (getAddMutexA() != InputMutex::REGB && getAddMutexB() != InputMutex::REGB)))
    {
    	//both inputs for mul are accumulators, an immediate value is used
    	//and the ADD ALU executes NOP or both inputs from the ADD ALU are not on register-file B
    	// -> vector rotation
    	mulPart.append(" ").append(static_cast<SmallImmediate>(getInputB()).toString());
    }
    if(getAddition() != OPADD_NOP)
    {
        if(getMultiplication() != OPMUL_NOP)
        {
            return (addPart + "; ") + mulPart;
        }
        return addPart;
    }
    if(getMultiplication() != OPMUL_NOP)
        return mulPart;
    return addPart;
}

std::pair<OpAdd, OpMul> vc4c::toOpCode(const std::string& opCode)
{
    if(opCode.compare("nop") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_NOP);
    if(opCode.compare("fadd") == 0)
        return std::make_pair(OPADD_FADD, OPMUL_NOP);
    if(opCode.compare("fsub") == 0)
        return std::make_pair(OPADD_FSUB, OPMUL_NOP);
    if(opCode.compare("fmin") == 0)
        return std::make_pair(OPADD_FMIN, OPMUL_NOP);
    if(opCode.compare("fmax") == 0)
        return std::make_pair(OPADD_FMAX, OPMUL_NOP);
    if(opCode.compare("fminabs") == 0)
        return std::make_pair(OPADD_FMINABS, OPMUL_NOP);
    if(opCode.compare("fmaxabs") == 0)
        return std::make_pair(OPADD_FMAXABS, OPMUL_NOP);
    if(opCode.compare("ftoi") == 0)
        return std::make_pair(OPADD_FTOI, OPMUL_NOP);
    if(opCode.compare("itof") == 0)
        return std::make_pair(OPADD_ITOF, OPMUL_NOP);
    if(opCode.compare("add") == 0)
        return std::make_pair(OPADD_ADD, OPMUL_NOP);
    if(opCode.compare("sub") == 0)
        return std::make_pair(OPADD_SUB, OPMUL_NOP);
    if(opCode.compare("shr") == 0)
        return std::make_pair(OPADD_SHR, OPMUL_NOP);
    if(opCode.compare("asr") == 0)
        return std::make_pair(OPADD_ASR, OPMUL_NOP);
    if(opCode.compare("ror") == 0)
        return std::make_pair(OPADD_ROR, OPMUL_NOP);
    if(opCode.compare("shl") == 0)
        return std::make_pair(OPADD_SHL, OPMUL_NOP);
    if(opCode.compare("min") == 0)
        return std::make_pair(OPADD_MIN, OPMUL_NOP);
    if(opCode.compare("max") == 0)
        return std::make_pair(OPADD_MAX, OPMUL_NOP);
    if(opCode.compare("and") == 0)
        return std::make_pair(OPADD_AND, OPMUL_NOP);
    if(opCode.compare("or") == 0)
        return std::make_pair(OPADD_OR, OPMUL_NOP);
    if(opCode.compare("xor") == 0)
        return std::make_pair(OPADD_XOR, OPMUL_NOP);
    if(opCode.compare("not") == 0)
        return std::make_pair(OPADD_NOT, OPMUL_NOP);
    if(opCode.compare("clz") == 0)
        return std::make_pair(OPADD_CLZ, OPMUL_NOP);
    if(opCode.compare("v8adds") == 0)
        return std::make_pair(OPADD_V8ADDS, OPMUL_NOP);
    if(opCode.compare("v8subs") == 0)
        return std::make_pair(OPADD_V8SUBS, OPMUL_NOP);
    
    if(opCode.compare("fmul") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_FMUL);
    if(opCode.compare("mul24") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_MUL24);
    if(opCode.compare("v8muld") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_V8MULD);
    if(opCode.compare("v8min") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_V8MIN);
    if(opCode.compare("v8max") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_V8MAX);
    if(opCode.compare("v8adds") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_V8ADDS);
    if(opCode.compare("v8subs") == 0)
        return std::make_pair(OPADD_NOP, OPMUL_V8SUBS);

    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid op-code", opCode);
}

