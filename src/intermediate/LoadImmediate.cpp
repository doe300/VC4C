/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../SIMDVector.h"
#include "../asm/LoadInstruction.h"
#include "../helper.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <bitset>
#include <cstdbool>
#include <memory>

using namespace vc4c;
using namespace vc4c::intermediate;

LoadImmediate::LoadImmediate(const Value& dest, const Literal& source, ConditionCode cond, const SetFlag setFlags) :
    IntermediateInstruction(dest, cond, setFlags), type(LoadType::REPLICATE_INT32)
{
    // 32-bit integers are loaded through all SIMD-elements!
    // "[...] write either a 32-bit immediate across the entire SIMD array" (p. 33)
    setImmediate(source);
}

LoadImmediate::LoadImmediate(const Value& dest, uint32_t mask, LoadType type, ConditionCode cond, SetFlag setFlags) :
    IntermediateInstruction(dest, cond, setFlags), type(type)
{
    setArgument(0, Value(Literal(mask), TYPE_INT32));
}

LCOV_EXCL_START
std::string LoadImmediate::to_string() const
{
    switch(type)
    {
    case LoadType::REPLICATE_INT32:
        if(getOutput()->hasRegister(REG_VPM_IN_SETUP) ||
            has_flag(decoration, InstructionDecorations::VPM_READ_CONFIGURATION))
            return (getOutput()->to_string(true) + " = loadi ") +
                periphery::VPRSetup::fromLiteral(getImmediate().unsignedInt()).to_string() +
                createAdditionalInfoString();
        if(getOutput()->hasRegister(REG_VPM_OUT_SETUP) ||
            has_flag(decoration, InstructionDecorations::VPM_WRITE_CONFIGURATION))
            return (getOutput()->to_string(true) + " = loadi ") +
                periphery::VPWSetup::fromLiteral(getImmediate().unsignedInt()).to_string() +
                createAdditionalInfoString();
        return (getOutput()->to_string(true) + " = loadi ") + assertArgument(0).to_string() +
            createAdditionalInfoString();
    case LoadType::PER_ELEMENT_SIGNED:

        return (getOutput()->to_string(true) + " = loadsi ") + TYPE_INT32.toVectorType(16).to_string() + " " +
            toLoadedValues(assertArgument(0).literal().unsignedInt(), LoadType::PER_ELEMENT_SIGNED).to_string() +
            createAdditionalInfoString();
    case LoadType::PER_ELEMENT_UNSIGNED:
        return (getOutput()->to_string(true) + " = loadui ") + TYPE_INT8.toVectorType(16).to_string() + " " +
            toLoadedValues(assertArgument(0).literal().unsignedInt(), LoadType::PER_ELEMENT_UNSIGNED).to_string() +
            createAdditionalInfoString();
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled type of load", std::to_string(static_cast<unsigned>(type)));
}
LCOV_EXCL_STOP

IntermediateInstruction* LoadImmediate::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    switch(type)
    {
    case LoadType::REPLICATE_INT32:
        return (new LoadImmediate(renameValue(method, getOutput().value(), localPrefix, localMapping),
                    assertArgument(0).literal(), conditional, setFlags))
            ->copyExtrasFrom(this);
    case LoadType::PER_ELEMENT_SIGNED:
    case LoadType::PER_ELEMENT_UNSIGNED:
        return (new LoadImmediate(renameValue(method, getOutput().value(), localPrefix, localMapping),
                    assertArgument(0).literal().unsignedInt(), type, conditional, setFlags))
            ->copyExtrasFrom(this);
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled type of load", std::to_string(static_cast<unsigned>(type)));
}

PrecalculatedValue LoadImmediate::precalculate(const std::size_t numIterations) const
{
    switch(type)
    {
    case LoadType::REPLICATE_INT32:
        return PrecalculatedValue{getArgument(0), ElementFlags::fromValue(assertArgument(0))};
    case LoadType::PER_ELEMENT_SIGNED:
    {
        // since there are only a few possible combinations (4 per type), using the global vector store is okay here
        auto val = GLOBAL_VECTOR_HOLDER.storeVector(
            toLoadedValues(assertArgument(0).literal().unsignedInt(), type), TYPE_INT32.toVectorType(16));
        return PrecalculatedValue{val, VectorFlags::fromValue(val)};
    }
    case LoadType::PER_ELEMENT_UNSIGNED:
    {
        // since there are only a few possible combinations (4 per type), using the global vector store is okay here
        auto val = GLOBAL_VECTOR_HOLDER.storeVector(
            toLoadedValues(assertArgument(0).literal().unsignedInt(), type), TYPE_INT8.toVectorType(16));
        return PrecalculatedValue{val, VectorFlags::fromValue(val)};
    }
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled type of load", std::to_string(static_cast<unsigned>(type)));
}

qpu_asm::DecoratedInstruction LoadImmediate::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    const Register outReg =
        getOutput()->checkRegister() ? getOutput()->reg() : registerMapping.at(getOutput()->local());
    const ConditionCode conditional0 =
        (outReg.num == REG_NOP.num && setFlags == SetFlag::DONT_SET) ? COND_NEVER : this->conditional;
    qpu_asm::LoadInstruction res(PACK_NOP, conditional0, COND_NEVER, setFlags,
        outReg.file == RegisterFile::PHYSICAL_A ? WriteSwap::DONT_SWAP : WriteSwap::SWAP, outReg.num, REG_NOP.num,
        assertArgument(0).literal().toImmediate());
    switch(type)
    {
    case LoadType::REPLICATE_INT32:
        res.setType(OpLoad::LOAD_IMM_32);
        break;
    case LoadType::PER_ELEMENT_SIGNED:
        res.setType(OpLoad::LOAD_SIGNED);
        break;
    case LoadType::PER_ELEMENT_UNSIGNED:
        res.setType(OpLoad::LOAD_UNSIGNED);
        break;
    default:
        throw CompilationError(
            CompilationStep::GENERAL, "Unhandled type of load", std::to_string(static_cast<unsigned>(type)));
    }

    return res;
}

bool LoadImmediate::isNormalized() const
{
    return true;
}

Literal LoadImmediate::getImmediate() const
{
    if(type != LoadType::REPLICATE_INT32)
        throw CompilationError(CompilationStep::GENERAL, "Cannot query immediate value from masked load", to_string());
    return assertArgument(0).literal();
}

void LoadImmediate::setImmediate(const Literal& value, DataType type)
{
    if(this->type != LoadType::REPLICATE_INT32)
        throw CompilationError(CompilationStep::GENERAL, "Cannot set immediate value for masked load", to_string());
    setArgument(0, Value(value, type));
}

SIMDVector LoadImmediate::toLoadedValues(uint32_t mask, vc4c::intermediate::LoadType type)
{
    std::bitset<32> bits(mask);
    SIMDVector values;
    if(type == LoadType::PER_ELEMENT_UNSIGNED)
    {
        for(std::size_t i = 0; i < 16; ++i)
        {
            values[i] = Literal(static_cast<unsigned>(bits.test(i + 16)) * 2u + static_cast<unsigned>(bits.test(i)));
        }
    }
    else if(type == LoadType::PER_ELEMENT_SIGNED)
    {
        for(std::size_t i = 0; i < 16; ++i)
        {
            values[i] = Literal(static_cast<int>(bits.test(i + 16)) * -2 + static_cast<int>(bits.test(i)));
        }
    }
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Unhandled type of masked load", std::to_string(static_cast<unsigned>(type)));
    return values;
}

uint32_t LoadImmediate::fromLoadedValues(const SIMDVector& values, LoadType type)
{
    if(type == LoadType::PER_ELEMENT_SIGNED)
    {
        std::bitset<32> mask;
        for(std::size_t i = 0; i < values.size(); ++i)
        {
            auto elemVal = values[i].signedInt();
            if(elemVal < -2 || elemVal > 1)
                throw CompilationError(
                    CompilationStep::GENERAL, "Invalid element to load via signed element load", values[i].to_string());
            mask.set(i + 16, elemVal < 0);
            mask.set(i, elemVal & 1);
        }
        return static_cast<uint32_t>(mask.to_ulong());
    }
    if(type == LoadType::PER_ELEMENT_UNSIGNED)
    {
        std::bitset<32> mask;
        for(std::size_t i = 0; i < values.size(); ++i)
        {
            auto elemVal = values[i].unsignedInt();
            if(elemVal > 3)
                throw CompilationError(
                    CompilationStep::GENERAL, "Invalid element to load via signed element load", values[i].to_string());
            mask.set(i + 16, elemVal >> 1);
            mask.set(i, elemVal & 1);
        }
        return static_cast<uint32_t>(mask.to_ulong());
    }
    throw CompilationError(CompilationStep::GENERAL, "Invalid load type", std::to_string(static_cast<unsigned>(type)));
}

bool LoadImmediate::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherLoad = dynamic_cast<const LoadImmediate*>(&other))
        return type == otherLoad->type;
    return false;
}
