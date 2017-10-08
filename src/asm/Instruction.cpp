/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <stdbool.h>

#include "Instruction.h"
#include "../Values.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

Instruction::Instruction() : Bitfield(0)
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

std::string Instruction::toInputRegister(const InputMutex mutex, const Address regA, const Address regB, const bool hasImmediate)
{
    if(mutex == InputMutex::ACC0)
        return "r0";
    if(mutex == InputMutex::ACC1)
        return "r1";
    if(mutex == InputMutex::ACC2)
        return "r2";
    if(mutex == InputMutex::ACC3)
        return "r3";
    if(mutex == InputMutex::ACC4)
        return "r4";
    if(mutex == InputMutex::ACC5)
        return "r5";
    //general register-file
    if(mutex == InputMutex::REGA)
    {
        const Register tmp{RegisterFile::PHYSICAL_A, regA};
        return tmp.to_string(true, true);
    }
    else if(hasImmediate)
    {
        //is immediate value
        return static_cast<SmallImmediate>(regB).toString();
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

std::string Instruction::toExtrasString(const Signaling sig, const ConditionCode cond, const SetFlag flags, const Unpack unpack, const Pack pack)
{
    std::string result("");
    if(sig != Signaling::NO_SIGNAL && sig != Signaling::ALU_IMMEDIATE)
        result += std::string(".") + toString(sig);
    if(cond != COND_ALWAYS)
        result += std::string(".") + cond.toString();
    if(flags == SetFlag::SET_FLAGS)
        result += std::string(".") + toString(flags);
    if(unpack != UNPACK_NOP)
    	result += std::string(".") + unpack.toString();
    if(pack != PACK_NOP)
    	result += std::string(".") + pack.toString();
    return result;
}

std::string qpu_asm::toHexString(const uint64_t code)
{
	//lower half before upper half
	char buffer[64];
	snprintf(buffer, sizeof(buffer), "0x%08x, 0x%08x, ", static_cast<uint32_t>(code & 0xFFFFFFFFLL), static_cast<uint32_t>((code & 0xFFFFFFFF00000000LL) >> 32));
	return buffer;
}
