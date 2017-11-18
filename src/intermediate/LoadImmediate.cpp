/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../asm/LoadInstruction.h"
#include "log.h"

#include <cstdbool>
#include <memory>

using namespace vc4c;
using namespace vc4c::intermediate;

LoadImmediate::LoadImmediate(const Value& dest, const Literal& source, const ConditionCode& cond, const SetFlag setFlags) :
IntermediateInstruction({true, dest}, cond, setFlags)
{
    //32-bit integers are loaded through all SIMD-elements!
    // "[...] write either a 32-bit immediate across the entire SIMD array" (p. 33)
	setImmediate(source);
}

std::string LoadImmediate::to_string() const
{
    return (getOutput().get().to_string(true) + " = loadi ") + getArgument(0).to_string() + createAdditionalInfoString();
}

IntermediateInstruction* LoadImmediate::copyFor(Method& method, const std::string& localPrefix) const
{
    return (new LoadImmediate(renameValue(method, getOutput(), localPrefix), getArgument(0).get().literal, conditional, setFlags))->copyExtrasFrom(this);
}

Optional<Value> LoadImmediate::precalculate(const std::size_t numIterations) const
{
	return getArgument(0);
}

qpu_asm::Instruction* LoadImmediate::convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    const Register outReg = getOutput().get().hasType(ValueType::REGISTER) ? getOutput().get().reg : registerMapping.at(getOutput().get().local);
    const ConditionCode conditional0 = outReg.num == REG_NOP.num ? COND_NEVER : this->conditional;
    return new qpu_asm::LoadInstruction(PACK_NOP, conditional0, COND_NEVER, setFlags,
                               outReg.file == RegisterFile::PHYSICAL_A ? WriteSwap::DONT_SWAP : WriteSwap::SWAP, outReg.num, REG_NOP.num, getArgument(0).get().literal.toImmediate());
}

Literal LoadImmediate::getImmediate() const
{
	return getArgument(0).get().literal;
}

void LoadImmediate::setImmediate(const Literal& value)
{
	setArgument(0, Value(value, value.type == LiteralType::INTEGER ? TYPE_INT32 : (value.type == LiteralType::REAL ? TYPE_FLOAT : TYPE_BOOL)));
}
