/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Instruction.h"

#include "ALUInstruction.h"
#include "BranchInstruction.h"
#include "LoadInstruction.h"
#include "SemaphoreInstruction.h"
#include "../Values.h"

#include <cstdbool>
#include <memory>

using namespace vc4c;
using namespace vc4c::qpu_asm;

Instruction::Instruction() : Bitfield(0)
{

}

Instruction::Instruction(uint64_t code) : Bitfield(code)
{

}

Instruction::~Instruction()
{

}

uint64_t Instruction::toBinaryCode() const
{
    return value;
}

std::string Instruction::toHexString(bool withAssemblerCode) const
{
    uint64_t binaryCode = toBinaryCode();
    if(withAssemblerCode)
    {
        return (std::string(qpu_asm::toHexString(binaryCode)) + "//") + toASMString();
    }
    return qpu_asm::toHexString(binaryCode);
}

Instruction* Instruction::readFromBinary(uint64_t binary)
{
	std::unique_ptr<Instruction> inst;

	inst.reset(new ALUInstruction(binary));
	if(inst->isValidInstruction())
		return inst.release();

	inst.reset(new BranchInstruction(binary));
	if(inst->isValidInstruction())
		return inst.release();

	inst.reset(new LoadInstruction(binary));
	if(inst->isValidInstruction())
		return inst.release();

	inst.reset(new SemaphoreInstruction(binary));
	if(inst->isValidInstruction())
		return inst.release();

	return nullptr;
}

std::string Instruction::toInputRegister(const InputMultiplex mux, const Address regA, const Address regB, const bool hasImmediate)
{
    if(mux == InputMultiplex::ACC0)
        return "r0";
    if(mux == InputMultiplex::ACC1)
        return "r1";
    if(mux == InputMultiplex::ACC2)
        return "r2";
    if(mux == InputMultiplex::ACC3)
        return "r3";
    if(mux == InputMultiplex::ACC4)
        return "r4";
    if(mux == InputMultiplex::ACC5)
        return "r5";
    //general register-file
    if(mux == InputMultiplex::REGA)
    {
        const Register tmp{RegisterFile::PHYSICAL_A, regA};
        return tmp.to_string(true, true);
    }
    else if(hasImmediate)
    {
        //is immediate value
        return static_cast<SmallImmediate>(regB).to_string();
    }
    else
    {
        const Register tmp{RegisterFile::PHYSICAL_B, regB};
        return tmp.to_string(true, true);
    }
}

std::string Instruction::toOutputRegister(bool regFileA, const Address reg)
{
    const Register tmp{regFileA ? RegisterFile::PHYSICAL_A : RegisterFile::PHYSICAL_B, reg};
    return tmp.to_string(true, false);
}

std::string Instruction::toExtrasString(const Signaling sig, const ConditionCode cond, const SetFlag flags, const Unpack unpack, const Pack pack, bool usesOutputA, bool usesInputAOrR4)
{
    std::string result("");
    if(sig != SIGNAL_NONE && sig != SIGNAL_ALU_IMMEDIATE)
        result += std::string(".") + sig.to_string();
    if(cond != COND_ALWAYS)
        result += std::string(".") + cond.to_string();
    if(flags == SetFlag::SET_FLAGS)
        result += std::string(".") + toString(flags);
    if(usesInputAOrR4 && unpack != UNPACK_NOP)
    	result += std::string(".") + unpack.to_string();
    if(usesOutputA && pack != PACK_NOP)
    	result += std::string(".") + pack.to_string();
    return result;
}

std::string Instruction::addComment(std::string s) const {
	std::string r;
	if (! comment.empty())
		r = s + " // " + comment;
	else
		r = s;

	if (! previousComment.empty())
		r = "// " + previousComment + "\n" + r;
	return r;
}

std::string qpu_asm::toHexString(const uint64_t code)
{
	//lower half before upper half
	char buffer[64];
	snprintf(buffer, sizeof(buffer), "0x%08x, 0x%08x, ", static_cast<uint32_t>(code & 0xFFFFFFFFLL), static_cast<uint32_t>((code & 0xFFFFFFFF00000000LL) >> 32));
	return buffer;
}
