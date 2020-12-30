/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "OpCodes.h"

#include "../HalfType.h"
#include "../SIMDVector.h"
#include "../Values.h"
#include "../analysis/ValueRange.h"
#include "../intrinsics/Operators.h"
#include "CompilationError.h"

#include <cmath>
#include <functional>
#include <map>
#include <random>

using namespace vc4c;

LCOV_EXCL_START
std::string ConditionCode::to_string() const
{
    switch(*this)
    {
    case COND_ALWAYS:
        return "";
    case COND_CARRY_CLEAR:
        return "ifcc";
    case COND_CARRY_SET:
        return "ifc";
    case COND_NEGATIVE_CLEAR:
        return "ifnc";
    case COND_NEGATIVE_SET:
        return "ifn";
    case COND_NEVER:
        return "never";
    case COND_ZERO_CLEAR:
        return "ifzc";
    case COND_ZERO_SET:
        return "ifz";
    }
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "Unsupported condition", std::to_string(static_cast<unsigned>(value)));
}
LCOV_EXCL_STOP

ConditionCode ConditionCode::invert() const
{
    switch(*this)
    {
    case COND_ALWAYS:
        return COND_NEVER;
    case COND_CARRY_CLEAR:
        return COND_CARRY_SET;
    case COND_CARRY_SET:
        return COND_CARRY_CLEAR;
    case COND_NEGATIVE_CLEAR:
        return COND_NEGATIVE_SET;
    case COND_NEGATIVE_SET:
        return COND_NEGATIVE_CLEAR;
    case COND_NEVER:
        return COND_ALWAYS;
    case COND_ZERO_CLEAR:
        return COND_ZERO_SET;
    case COND_ZERO_SET:
        return COND_ZERO_CLEAR;
    }
    throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported conditions", to_string());
}

bool ConditionCode::isInversionOf(const ConditionCode other) const
{
    return other == invert();
}

BranchCond ConditionCode::toBranchCondition(bool requireAllSet) const
{
    switch(value)
    {
    case COND_ALWAYS.value:
        return BRANCH_ALWAYS;
    case COND_CARRY_CLEAR.value:
        return requireAllSet ? BRANCH_ANY_C_CLEAR : BRANCH_ALL_C_CLEAR;
    case COND_CARRY_SET.value:
        return requireAllSet ? BRANCH_ALL_C_SET : BRANCH_ANY_C_SET;
    case COND_NEGATIVE_CLEAR.value:
        return requireAllSet ? BRANCH_ANY_N_CLEAR : BRANCH_ALL_N_CLEAR;
    case COND_NEGATIVE_SET.value:
        return requireAllSet ? BRANCH_ALL_N_SET : BRANCH_ANY_N_SET;
    case COND_ZERO_CLEAR.value:
        return requireAllSet ? BRANCH_ANY_Z_CLEAR : BRANCH_ALL_Z_CLEAR;
    case COND_ZERO_SET.value:
        return requireAllSet ? BRANCH_ALL_Z_SET : BRANCH_ANY_Z_SET;
    }
    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid condition for branch", to_string());
}

LCOV_EXCL_START
std::string Signaling::to_string() const
{
    switch(*this)
    {
    case SIGNAL_LOAD_ALPHA:
        return "loada";
    case SIGNAL_ALU_IMMEDIATE:
        return "imm";
    case SIGNAL_BRANCH:
        return "br";
    case SIGNAL_LOAD_COLOR:
        return "loadc";
    case SIGNAL_LOAD_COLOR_END:
        return "loadc_end";
    case SIGNAL_LOAD_COVERAGE:
        return "loadcov";
    case SIGNAL_THREAD_SWITCH_LAST:
        return "lthrsw";
    case SIGNAL_LOAD_IMMEDIATE:
        return "load_imm";
    case SIGNAL_LOAD_TMU0:
        return "load_tmu0";
    case SIGNAL_LOAD_TMU1:
        return "load_tmu1";
    case SIGNAL_NONE:
        return "";
    case SIGNAL_END_PROGRAM:
        return "thrend";
    case SIGNAL_UNLOCK_SCORE:
        return "scoreu";
    case SIGNAL_SOFT_BREAK:
        return "bkpt";
    case SIGNAL_SWITCH_THREAD:
        return "thrsw";
    case SIGNAL_WAIT_FOR_SCORE:
        return "scorew";
    }
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "Unsupported signal", std::to_string(static_cast<unsigned>(value)));
}
LCOV_EXCL_STOP

bool Signaling::hasSideEffects() const noexcept
{
    return *this != SIGNAL_NONE && *this != SIGNAL_ALU_IMMEDIATE && *this != SIGNAL_LOAD_IMMEDIATE;
}

bool Signaling::triggersReadOfR4() const noexcept
{
    return *this == SIGNAL_LOAD_ALPHA || *this == SIGNAL_LOAD_COLOR || *this == SIGNAL_LOAD_COLOR_END ||
        *this == SIGNAL_LOAD_COVERAGE || *this == SIGNAL_LOAD_TMU0 || *this == SIGNAL_LOAD_TMU1;
}

LCOV_EXCL_START
std::string Unpack::to_string() const
{
    // http://maazl.de/project/vc4asm/doc/extensions.html#pack
    switch(*this)
    {
    case UNPACK_NOP:
    case UNPACK_NOP_PM:
        return "";
    case UNPACK_16A_32:
        return "sextLow16to32";
    case UNPACK_16B_32:
        return "sextHigh16to32";
    case UNPACK_8888_32:
        return "replMSB";
    case UNPACK_8A_32:
        return "zextByte0To32";
    case UNPACK_8B_32:
        return "zextByte1To32";
    case UNPACK_8C_32:
        return "zextByte2To32";
    case UNPACK_8D_32:
        return "zextByte3To32";
    case UNPACK_R4_16A_32:
        return "r4HalfLowToFloat";
    case UNPACK_R4_16B_32:
        return "r4HalfHighToFloat";
    case UNPACK_R4_ALPHA_REPLICATE:
        return "r4ReplAlpha";
    case UNPACK_R4_COLOR0:
        return "r4Byte0ToFloat";
    case UNPACK_R4_COLOR1:
        return "r4Byte1ToFloat";
    case UNPACK_R4_COLOR2:
        return "r4Byte2ToFloat";
    case UNPACK_R4_COLOR3:
        return "r4Byte3ToFloat";
    }
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "Unsupported unpack-mode", std::to_string(static_cast<unsigned>(value)));
}
LCOV_EXCL_STOP

static Literal unpackLiteral(Unpack mode, Literal literal, bool isFloatOperation)
{
    if(literal.isUndefined())
        return literal;
    switch(mode)
    {
    case UNPACK_NOP:
        return literal;
    case UNPACK_16A_32:
    {
        // signed conversion -> truncate to unsigned short, bit-cast to signed short and sign-extend
        uint16_t lowWord = static_cast<uint16_t>(literal.unsignedInt());
        if(isFloatOperation)
            return Literal(static_cast<float>(half_t(lowWord)));
        int16_t lowWordSigned = bit_cast<uint16_t, int16_t>(lowWord);
        return Literal(static_cast<int32_t>(lowWordSigned));
    }
    case UNPACK_16B_32:
    {
        // signed conversion -> truncate to unsigned short, bit-cast to signed short and sign-extend
        uint16_t highWord = static_cast<uint16_t>(literal.unsignedInt() >> 16);
        if(isFloatOperation)
            return Literal(static_cast<float>(half_t(highWord)));
        int16_t highWordSigned = bit_cast<uint16_t, int16_t>(highWord);
        return Literal(static_cast<int32_t>(highWordSigned));
    }
    case UNPACK_R4_ALPHA_REPLICATE:
        FALL_THROUGH
    case UNPACK_8888_32:
    {
        // unsigned cast required to guarantee cutting off the value
        uint8_t lsb = static_cast<uint8_t>(literal.unsignedInt() >> 24u);
        return Literal((static_cast<uint32_t>(lsb) << 24) | (static_cast<uint32_t>(lsb) << 16) |
            (static_cast<uint32_t>(lsb) << 8) | lsb);
    }
    case UNPACK_8A_32:
    {
        if(isFloatOperation)
            return unpackLiteral(UNPACK_R4_COLOR0, literal, isFloatOperation);
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte0 = static_cast<uint8_t>(literal.unsignedInt());
        return Literal(static_cast<uint32_t>(byte0));
    }
    case UNPACK_8B_32:
    {
        if(isFloatOperation)
            return unpackLiteral(UNPACK_R4_COLOR1, literal, isFloatOperation);
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte1 = static_cast<uint8_t>(literal.unsignedInt() >> 8);
        return Literal(static_cast<uint32_t>(byte1));
    }
    case UNPACK_8C_32:
    {
        if(isFloatOperation)
            return unpackLiteral(UNPACK_R4_COLOR2, literal, isFloatOperation);
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte2 = static_cast<uint8_t>(literal.unsignedInt() >> 16);
        return Literal(static_cast<uint32_t>(byte2));
    }
    case UNPACK_8D_32:
    {
        if(isFloatOperation)
            return unpackLiteral(UNPACK_R4_COLOR3, literal, isFloatOperation);
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte3 = static_cast<uint8_t>(literal.unsignedInt() >> 24);
        return Literal(static_cast<uint32_t>(byte3));
    }
    case UNPACK_R4_16A_32:
    {
        uint16_t lowWord = static_cast<uint16_t>(literal.unsignedInt());
        return Literal(static_cast<float>(half_t(lowWord)));
    }
    case UNPACK_R4_16B_32:
    {
        uint16_t highWord = static_cast<uint16_t>(literal.unsignedInt() >> 16);
        return Literal(static_cast<float>(half_t(highWord)));
    }
    case UNPACK_R4_COLOR0:
    {
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte0 = static_cast<uint8_t>(literal.unsignedInt());
        return Literal(static_cast<float>(byte0) / 255.0f);
    }
    case UNPACK_R4_COLOR1:
    {
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte1 = static_cast<uint8_t>(literal.unsignedInt() >> 8);
        return Literal(static_cast<float>(byte1) / 255.0f);
    }
    case UNPACK_R4_COLOR2:
    {
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte2 = static_cast<uint8_t>(literal.unsignedInt() >> 16);
        return Literal(static_cast<float>(byte2) / 255.0f);
    }
    case UNPACK_R4_COLOR3:
    {
        // unsigned cast required to guarantee cutting off the value
        uint8_t byte3 = static_cast<uint8_t>(literal.unsignedInt() >> 24);
        return Literal(static_cast<float>(byte3) / 255.0f);
    }
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unsupported unpack-mode", std::to_string(static_cast<unsigned>(mode.value)));
}

Optional<Value> Unpack::operator()(const Value& val) const
{
    // TODO are the r4 unpack values additional or instead-of the "normal" ones?
    if(!hasEffect())
        return val;
    // we never can pack complex types (even pointer, there are always 32-bit)
    if(!val.type.isSimpleType())
        return NO_VALUE;
    if(auto vector = val.checkVector())
    {
        // unpack vectors per element
        auto elemType = val.type.toVectorType(1);
        SIMDVector result = vector->transform(
            [&](Literal lit) -> Literal { return unpackLiteral(*this, lit, elemType.isFloatingType()); });
        return SIMDVectorHolder::storeVector(std::move(result), val.type, vector->getStorage());
    }
    // can only unpack literals
    if(auto lit = val.getLiteralValue())
        return Value(unpackLiteral(*this, *lit, val.type.isFloatingType()), val.type);
    return NO_VALUE;
}

SIMDVector Unpack::operator()(const SIMDVector& val, bool isFloatOperation) const
{
    if(!hasEffect())
        return val;
    return val.transform([&](Literal lit) -> Literal { return unpackLiteral(*this, lit, isFloatOperation); });
}

analysis::ValueRange Unpack::operator()(const analysis::ValueRange& range, bool isFloatOperation) const
{
    using namespace analysis;
    switch(*this)
    {
    case UNPACK_NOP:
    case UNPACK_NOP_PM:
        return range;
    case UNPACK_16A_32:
        if(isFloatOperation)
            return UNPACK_R4_16A_32(range, isFloatOperation);
        if(range.fitsIntoRange(RANGE_SHORT))
            return range;
        // XXX since the unpack mode does not saturate, having a larger range is UB, I guess
        return ValueRange(TYPE_INT32);
    case UNPACK_8A_32:
        if(isFloatOperation)
            return UNPACK_R4_COLOR0(range, isFloatOperation);
        if(range.fitsIntoRange(RANGE_UCHAR))
            return range;
        // XXX since the unpack mode does not saturate, having a larger range is UB, I guess
        return ValueRange(TYPE_INT32);
    case UNPACK_R4_16A_32:
        if(range.fitsIntoRange(RANGE_HALF))
            return range;
        // XXX since the unpack mode does not saturate, having a larger range is UB, I guess
        return ValueRange(TYPE_FLOAT);
    case UNPACK_R4_COLOR0:
        if(range.fitsIntoRange(RANGE_UCHAR))
            return range / 255.0;
        // XXX since the unpack mode does not saturate, having a larger range is UB, I guess
        return ValueRange(TYPE_FLOAT);
    }
    return ValueRange{isFloatOperation ? TYPE_FLOAT : TYPE_INT32};
}

const Unpack Unpack::unpackTo32Bit(DataType type)
{
    if(type.getScalarBitCount() >= DataType::WORD)
        return UNPACK_NOP;
    if(type.getScalarBitCount() == DataType::HALF_WORD)
        return UNPACK_16A_32;
    if(type.getScalarBitCount() == DataType::BYTE)
        return UNPACK_8A_32;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled type-width for unpack-modes", type.to_string());
}

bool Unpack::isPMBitSet() const noexcept
{
    // check whether pm bit set
    return value & 0x1;
}

bool Unpack::hasEffect() const noexcept
{
    // exclude "normal" NOP and NOP with pm bit set
    return value != 0 && value != 1;
}

BitMask Unpack::getInputMask() const
{
    if(!hasEffect())
        return BITMASK_ALL;
    if(*this == UNPACK_16A_32 || *this == UNPACK_R4_16A_32)
        return BitMask{0x0000FFFF};
    if(*this == UNPACK_16B_32 || *this == UNPACK_R4_16B_32)
        return BitMask{0xFFFF0000};
    if(*this == UNPACK_8A_32 || *this == UNPACK_R4_COLOR0)
        return BitMask{0x000000FF};
    if(*this == UNPACK_8B_32 || *this == UNPACK_R4_COLOR1)
        return BitMask{0x0000FF00};
    if(*this == UNPACK_8C_32 || *this == UNPACK_R4_COLOR2)
        return BitMask{0x00FF0000};
    if(*this == UNPACK_8D_32 || *this == UNPACK_R4_COLOR3 || *this == UNPACK_8888_32 ||
        *this == UNPACK_R4_ALPHA_REPLICATE)
        return BitMask{0xFF000000};
    throw CompilationError(CompilationStep::GENERAL, "Cannot determine mask for unknown unpack mode", to_string());
}

BitMask Unpack::operator()(BitMask mask, bool isFloatOperation) const
{
    if(!hasEffect())
        return mask;
    auto outputMask = BITMASK_ALL;
    if(!isFloatOperation &&
        (*this == UNPACK_8A_32 || *this == UNPACK_8B_32 || *this == UNPACK_8C_32 || *this == UNPACK_8D_32))
        outputMask = BitMask{0x000000FF};

    return getInputMask() & mask ? outputMask : BITMASK_NONE;
}

LCOV_EXCL_START
std::string Pack::to_string() const
{
    // http://maazl.de/project/vc4asm/doc/extensions.html#pack
    switch(*this)
    {
    case PACK_NOP:
    case PACK_NOP_PM:
        return "";
    case PACK_32_16A:
        return "trunc32toLow16";
    case PACK_32_16A_S:
        return "sat16ToLow16";
    case PACK_32_16B:
        return "trunc32ToHigh16";
    case PACK_32_16B_S:
        return "sat16ToHigh16";
    case PACK_32_32:
        return "sat";
    case PACK_32_8888:
        return "replLSB";
    case PACK_32_8888_S:
        return "replLSBSat";
    case PACK_32_8A:
        return "truncLSBToByte0";
    case PACK_32_8A_S:
        return "satLSBToByte0";
    case PACK_32_8B:
        return "truncLSBToByte1";
    case PACK_32_8B_S:
        return "satLSBToByte1";
    case PACK_32_8C:
        return "truncLSBToByte2";
    case PACK_32_8C_S:
        return "satLSBToByte2";
    case PACK_32_8D:
        return "truncLSBToByte3";
    case PACK_32_8D_S:
        return "satLSBToByte3";
    case PACK_MUL_GRAY_REPLICATE:
        return "mulFloatToReplLSB";
    case PACK_MUL_COLOR0:
        return "mulFloatToByte0";
    case PACK_MUL_COLOR1:
        return "mulFloatToByte1";
    case PACK_MUL_COLOR2:
        return "mulFloatToByte2";
    case PACK_MUL_COLOR3:
        return "mulFloatToByte3";
    }
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "Unsupported pack-mode", std::to_string(static_cast<unsigned>(value)));
}
LCOV_EXCL_STOP

Literal packLiteral(Pack mode, Literal literal, bool isFloatOperation, const ElementFlags& flags)
{
    if(literal.isUndefined())
        return literal;

    if(mode.hasEffect() && mode != PACK_32_32 && !isFloatOperation && flags.overflow != FlagStatus::UNDEFINED)
        // Tests have shown that (at least for integer operations), all pack modes apply a 32-bit saturation first.
        literal = packLiteral(PACK_32_32, literal, false, flags);

    switch(mode)
    {
    case PACK_NOP:
        return literal;
    case PACK_32_16A:
        if(isFloatOperation)
            return Literal(static_cast<uint16_t>(half_t(literal.real())));
        if(flags.overflow == FlagStatus::SET &&
            (literal.unsignedInt() == 0x7FFFFFFFu || literal.unsignedInt() == 0x80000000u))
            // Tests have shown that for 32-bit overflow, the values 0x7FFFFFFF and 0x80000000 get truncated to 0x7FFF
            // and 0x8000 respectively
            return literal.unsignedInt() == 0x7FFFFFFFu ? Literal(0x7FFF) : Literal(0x8000);
        return Literal(literal.unsignedInt() & 0xFFFF);
    case PACK_32_16A_S:
        if(isFloatOperation)
            // TODO no saturation?
            return Literal(static_cast<uint16_t>(half_t(literal.real())));
        if(flags.overflow == FlagStatus::SET)
            // On full 32-bit integer overflow, the return value is sometimes zero!
            return Literal(0u);
        return Literal(saturate<int16_t>(literal.signedInt()) & 0xFFFF);
    case PACK_32_16B:
        if(isFloatOperation)
            return Literal(static_cast<uint16_t>(half_t(literal.real())) << 16);
        if(flags.overflow == FlagStatus::SET &&
            (literal.unsignedInt() == 0x7FFFFFFFu || literal.unsignedInt() == 0x80000000u))
            // Tests have shown that for 32-bit overflow, the values 0x7FFFFFFF and 0x80000000 get truncated to 0x7FFF
            // and 0x8000 respectively
            return literal.unsignedInt() == 0x7FFFFFFFu ? Literal(0x7FFF) : Literal(0x8000);
        return Literal((literal.unsignedInt() & 0xFFFF) << 16);
    case PACK_32_16B_S:
        if(isFloatOperation)
            // TODO no saturation?
            return Literal(static_cast<uint16_t>(half_t(literal.real())) << 16);
        if(flags.overflow == FlagStatus::SET)
            // On full 32-bit integer overflow, the return value is sometimes zero!
            return Literal(0u);
        // need to bitcast here, since left shifting negative values is undefined behavior
        return Literal(bit_cast<int32_t, uint32_t>(saturate<int16_t>(literal.signedInt())) << 16);
    case PACK_32_32:
        // this depends on signed integer overflow (to determine overflow and then saturate)
        switch(flags.overflow)
        {
        case FlagStatus::CLEAR:
            return literal;
        case FlagStatus::SET:
            /*
             * Rationale:
             * add and sub can only add up to 1 bit:
             * - for signed positive overflow, the MSB is set (negative)
             * - for signed negative overflow, the MSB depends on the second MSB values?
             * TODO not correct for signed negative overflow?
             */
            return flags.carry == FlagStatus::SET ? Literal(0x80000000u) : Literal(0x7FFFFFFFu);
        case FlagStatus::UNDEFINED:
        default:
            throw CompilationError(CompilationStep::GENERAL, "Cannot saturate on unknown overflow flags",
                Value(literal, isFloatOperation ? TYPE_FLOAT : TYPE_INT32).to_string());
        }
    case PACK_32_8888:
        return Literal(((literal.unsignedInt() & 0xFF) << 24) | ((literal.unsignedInt() & 0xFF) << 16) |
            ((literal.unsignedInt() & 0xFF) << 8) | (literal.unsignedInt() & 0xFF));
    case PACK_32_8888_S:
        return Literal((saturate<uint8_t>(literal.signedInt()) << 24) | (saturate<uint8_t>(literal.signedInt()) << 16) |
            (saturate<uint8_t>(literal.signedInt()) << 8) | saturate<uint8_t>(literal.signedInt()));
    case PACK_32_8A:
        return Literal(literal.unsignedInt() & 0xFF);
    case PACK_32_8A_S:
        return Literal(saturate<uint8_t>(literal.signedInt()));
    case PACK_32_8B:
        return Literal((literal.unsignedInt() & 0xFF) << 8);
    case PACK_32_8B_S:
        return Literal(saturate<uint8_t>(literal.signedInt()) << 8);
    case PACK_32_8C:
        return Literal((literal.unsignedInt() & 0xFF) << 16);
    case PACK_32_8C_S:
        return Literal(saturate<uint8_t>(literal.signedInt()) << 16);
    case PACK_32_8D:
        return Literal((literal.unsignedInt() & 0xFF) << 24);
    case PACK_32_8D_S:
        return Literal(saturate<uint8_t>(literal.signedInt()) << 24);
    case PACK_MUL_GRAY_REPLICATE:
    {
        auto tmp = static_cast<uint32_t>(literal.real() * 255.0f) & 0xFF;
        return Literal(tmp << 24 | tmp << 16 | tmp << 8 | tmp);
    }
    case PACK_MUL_COLOR0:
    {
        auto tmp = static_cast<uint32_t>(literal.real() * 255.0f) & 0xFF;
        return Literal(tmp);
    }
    case PACK_MUL_COLOR1:
    {
        auto tmp = static_cast<uint32_t>(literal.real() * 255.0f) & 0xFF;
        return Literal(tmp << 8);
    }
    case PACK_MUL_COLOR2:
    {
        auto tmp = static_cast<uint32_t>(literal.real() * 255.0f) & 0xFF;
        return Literal(tmp << 16);
    }
    case PACK_MUL_COLOR3:
    {
        auto tmp = static_cast<uint32_t>(literal.real() * 255.0f) & 0xFF;
        return Literal(tmp << 24);
    }
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unsupported pack-mode", std::to_string(static_cast<unsigned>(mode.value)));
}

Optional<Value> Pack::operator()(const Value& val, const VectorFlags& flags) const
{
    // TODO are the mul pack modes additional or instead-of the "normal" ones? Can mul ALU also use"normal" pack mode?
    if(!hasEffect())
        return val;
    // we never can pack complex types (even pointer, there are always 32-bit)
    if(!val.type.isSimpleType())
        return NO_VALUE;
    if(auto vector = val.checkVector())
    {
        // pack vectors per element
        auto elemType = val.type.toVectorType(1);
        SIMDVector result;
        for(std::size_t i = 0; i < vector->size(); ++i)
        {
            auto elem = (*vector)[i];
            result[i] = packLiteral(*this, elem, elemType.isFloatingType(), flags[i]);
        }
        return SIMDVectorHolder::storeVector(std::move(result), val.type, vector->getStorage());
    }
    // can only pack literals
    if(auto lit = val.getLiteralValue())
        return Value(packLiteral(*this, *lit, val.type.isFloatingType(), flags[0]), val.type);
    return NO_VALUE;
}

SIMDVector Pack::operator()(const SIMDVector& val, const VectorFlags& flags, bool isFloatOperation) const
{
    if(!hasEffect())
        return val;
    SIMDVector result;
    for(std::size_t i = 0; i < val.size(); ++i)
        result[i] = packLiteral(*this, val[i], isFloatOperation, flags[i]);
    return result;
}

analysis::ValueRange Pack::operator()(const analysis::ValueRange& range, bool isFloatOperation) const
{
    using namespace analysis;
    switch(*this)
    {
    case PACK_NOP:
    case PACK_NOP_PM:
    case PACK_32_32:
        return range;
        // TODO support non-saturating/truncating cases into LSB(s)
    case PACK_32_16A:
        if(isFloatOperation)
        {
            if(range.fitsIntoRange(RANGE_HALF))
                return range;
            // XXX since the pack mode does not saturate, having a larger range is UB, I guess
            return RANGE_HALF;
        }
        if(range.fitsIntoRange(ValueRange(0.0, static_cast<double>(std::numeric_limits<uint16_t>::max()))))
            // since we might need to cut off the upper bits, for now only handle if they are zero anyways!
            return range;
        // TODO is this correct?? Is the conversion supposed to be signed or unsigned??
        return ValueRange(TYPE_INT32);
    case PACK_32_8A:
        if(range.fitsIntoRange(RANGE_UCHAR))
            return range;
        return RANGE_UCHAR;
    case PACK_32_16A_S:
        if(isFloatOperation)
        {
            if(range.fitsIntoRange(RANGE_HALF))
                return range;
            // XXX since the pack mode does not saturate, having a larger range is UB, I guess
            return RANGE_HALF;
        }
        return ValueRange(static_cast<double>(saturate<int16_t>(static_cast<int64_t>(range.minValue))),
            static_cast<double>(saturate<int16_t>(static_cast<int64_t>(range.maxValue))));
    case PACK_32_8A_S:
        return ValueRange(static_cast<double>(saturate<uint8_t>(static_cast<int64_t>(range.minValue))),
            static_cast<double>(saturate<uint8_t>(static_cast<int64_t>(range.maxValue))));
    case PACK_MUL_COLOR0:
        // FIXME is this correct/as expected?? Since the actual value range is still the same [0, 1] (same for unpack
        // mode)
        if(range.fitsIntoRange(ValueRange(0.0, 1.0)))
            return range * 255.0;
        return RANGE_UCHAR;
    }
    return ValueRange{isFloatOperation ? TYPE_FLOAT : TYPE_INT32};
}

bool Pack::isPMBitSet() const noexcept
{
    // check for pm bit set
    return value & 0x10;
}

bool Pack::hasEffect() const noexcept
{
    // exclude "normal" NOP and NOP with pm bit set
    return value != 0 && value != 0x10;
}

BitMask Pack::getOutputMask() const
{
    if(!hasEffect())
        return BITMASK_ALL;
    if(*this == PACK_32_32 || *this == PACK_32_8888 || *this == PACK_32_8888_S || *this == PACK_MUL_GRAY_REPLICATE)
        return BITMASK_ALL;
    if(*this == PACK_32_16A || *this == PACK_32_16A_S)
        return BitMask{0x0000FFFF};
    if(*this == PACK_32_16B || *this == PACK_32_16B_S)
        return BitMask{0xFFFF0000};
    if(*this == PACK_32_8A || *this == PACK_32_8A_S || *this == PACK_MUL_COLOR0)
        return BitMask{0x000000FF};
    if(*this == PACK_32_8B || *this == PACK_32_8B_S || *this == PACK_MUL_COLOR1)
        return BitMask{0x0000FF00};
    if(*this == PACK_32_8C || *this == PACK_32_8C_S || *this == PACK_MUL_COLOR2)
        return BitMask{0x00FF0000};
    if(*this == PACK_32_8D || *this == PACK_32_8D_S || *this == PACK_MUL_COLOR3)
        return BitMask{0xFF000000};
    throw CompilationError(CompilationStep::GENERAL, "Cannot determine mask for unknown pack mode", to_string());
}

BitMask Pack::operator()(BitMask mask) const
{
    if(!hasEffect())
        return mask;
    if(*this == PACK_32_16A)
        return mask.getLowerHalfWord() ? BitMask{0x0000FFFF} : BITMASK_NONE;
    if(*this == PACK_32_16B)
        return mask.getLowerHalfWord() ? BitMask{0xFFFF0000} : BITMASK_NONE;
    if(*this == PACK_32_8888)
        return mask.getByte0() ? BITMASK_ALL : BITMASK_NONE;
    if(*this == PACK_32_8A)
        return mask.getByte0() ? BitMask{0x000000FF} : BITMASK_NONE;
    if(*this == PACK_32_8B)
        return mask.getByte0() ? BitMask{0x0000FF00} : BITMASK_NONE;
    if(*this == PACK_32_8C)
        return mask.getByte0() ? BitMask{0x00FF0000} : BITMASK_NONE;
    if(*this == PACK_32_8D)
        return mask.getByte0() ? BitMask{0xFF000000} : BITMASK_NONE;
    if(*this == PACK_32_16A_S)
        return mask ? BitMask{0x0000FFFF} : BITMASK_NONE;
    if(*this == PACK_32_16B_S)
        return mask ? BitMask{0xFFFF0000} : BITMASK_NONE;
    if(*this == PACK_32_8A_S || *this == PACK_MUL_COLOR0)
        return mask ? BitMask{0x000000FF} : BITMASK_NONE;
    if(*this == PACK_32_8B_S || *this == PACK_MUL_COLOR1)
        return mask ? BitMask{0x0000FF00} : BITMASK_NONE;
    if(*this == PACK_32_8C_S || *this == PACK_MUL_COLOR2)
        return mask ? BitMask{0x00FF0000} : BITMASK_NONE;
    if(*this == PACK_32_8D_S || *this == PACK_MUL_COLOR3)
        return mask ? BitMask{0xFF000000} : BITMASK_NONE;

    return BITMASK_ALL;
}

LCOV_EXCL_START
std::string vc4c::toString(const SetFlag flag)
{
    switch(flag)
    {
    case SetFlag::DONT_SET:
        return "";
    case SetFlag::SET_FLAGS:
        return "setf";
    }
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "Unsupported set-flags flag", std::to_string(static_cast<unsigned>(flag)));
}
LCOV_EXCL_STOP

bool vc4c::isFlagSetByMulALU(unsigned char opAdd, unsigned char opMul) noexcept
{
    // despite what the Broadcom specification states, only using mul ALU if add ALU executes nop.
    return opAdd == OP_NOP.opAdd && opMul != OP_NOP.opMul;
}

bool ElementFlags::matchesCondition(ConditionCode cond) const
{
    switch(cond.value)
    {
    case COND_ALWAYS.value:
        return true;
    case COND_CARRY_CLEAR.value:
    {
        if(carry == FlagStatus::UNDEFINED)
            throw CompilationError(CompilationStep::GENERAL, "Reading undefined carry flags");
        return carry == FlagStatus::CLEAR;
    }
    case COND_CARRY_SET.value:
    {
        if(carry == FlagStatus::UNDEFINED)
            throw CompilationError(CompilationStep::GENERAL, "Reading undefined carry flags");
        return carry == FlagStatus::SET;
    }
    case COND_NEGATIVE_CLEAR.value:
    {
        if(negative == FlagStatus::UNDEFINED)
            throw CompilationError(CompilationStep::GENERAL, "Reading undefined negative flags");
        return negative == FlagStatus::CLEAR;
    }
    case COND_NEGATIVE_SET.value:
    {
        if(negative == FlagStatus::UNDEFINED)
            throw CompilationError(CompilationStep::GENERAL, "Reading undefined negative flags");
        return negative == FlagStatus::SET;
    }
    case COND_NEVER.value:
        return false;
    case COND_ZERO_CLEAR.value:
    {
        if(zero == FlagStatus::UNDEFINED)
            throw CompilationError(CompilationStep::GENERAL, "Reading undefined zero flags");
        return zero == FlagStatus::CLEAR;
    }
    case COND_ZERO_SET.value:
    {
        if(zero == FlagStatus::UNDEFINED)
            throw CompilationError(CompilationStep::GENERAL, "Reading undefined zero flags");
        return zero == FlagStatus::SET;
    }
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled condition code", cond.to_string());
}
LCOV_EXCL_START
static std::string toFlagString(FlagStatus flag, char flagChar)
{
    if(flag == FlagStatus::CLEAR)
        return "-";
    if(flag == FlagStatus::SET)
        return std::string(&flagChar, 1);
    return "?";
}

std::string ElementFlags::to_string() const
{
    return toFlagString(zero, 'z') + toFlagString(negative, 'n') + toFlagString(carry, 'c') +
        toFlagString(overflow, 'o');
}
LCOV_EXCL_STOP

ElementFlags ElementFlags::fromValue(const Value& val) noexcept
{
    if(auto lit = val.getLiteralValue())
    {
        return ElementFlags::fromLiteral(*lit);
    }
    return ElementFlags{};
}

ElementFlags ElementFlags::fromLiteral(Literal lit) noexcept
{
    ElementFlags flags;
    // for both unsigned and float, the MSB is the sign and MBS(x) == 1 means x < 0
    flags.negative = lit.signedInt() < 0 ? FlagStatus::SET : FlagStatus::CLEAR;
    // for signed, unsigned and float, zero is all bits zero
    flags.zero = lit.unsignedInt() == 0 ? FlagStatus::SET : FlagStatus::CLEAR;
    // literals can never have carry/32-bit overflow set
    flags.carry = flags.overflow = FlagStatus::CLEAR;
    return flags;
}

bool VectorFlags::matchesCondition(BranchCond cond) const
{
    switch(cond)
    {
    case BRANCH_ALWAYS:
        return true;
    case BRANCH_ALL_C_CLEAR:
        return std::all_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_CARRY_CLEAR); });
    case BRANCH_ALL_C_SET:
        return std::all_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_CARRY_SET); });
    case BRANCH_ALL_N_CLEAR:
        return std::all_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_NEGATIVE_CLEAR); });
    case BRANCH_ALL_N_SET:
        return std::all_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_NEGATIVE_SET); });
    case BRANCH_ALL_Z_CLEAR:
        return std::all_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_ZERO_CLEAR); });
    case BRANCH_ALL_Z_SET:
        return std::all_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_ZERO_SET); });
    case BRANCH_ANY_C_CLEAR:
        return std::any_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_CARRY_CLEAR); });
    case BRANCH_ANY_C_SET:
        return std::any_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_CARRY_SET); });
    case BRANCH_ANY_N_CLEAR:
        return std::any_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_NEGATIVE_CLEAR); });
    case BRANCH_ANY_N_SET:
        return std::any_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_NEGATIVE_SET); });
    case BRANCH_ANY_Z_CLEAR:
        return std::any_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_ZERO_CLEAR); });
    case BRANCH_ANY_Z_SET:
        return std::any_of(begin(), end(),
            [](const ElementFlags& element) -> bool { return element.matchesCondition(COND_ZERO_SET); });
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled branch condition", cond.to_string());
}

std::string VectorFlags::to_string() const
{
    return "{" + vc4c::to_string<ElementFlags>(*this) + "}";
}

VectorFlags VectorFlags::fromValue(const Value& val)
{
    if(auto lit = val.getLiteralValue())
        return ElementFlags::fromLiteral(*lit);
    VectorFlags flags{};
    if(auto vec = val.checkVector())
    {
        for(unsigned i = 0; i < vec->size(); ++i)
            flags[i] = ElementFlags::fromLiteral((*vec)[i]);
    }
    return flags;
}

bool OpCode::operator==(const OpCode& right) const noexcept
{
    if(this == &right)
        return true;
    if(opAdd > 0 && opAdd == right.opAdd)
        return true;
    if(opMul > 0 && opMul == right.opMul)
        return true;
    if(opAdd == 0 && opMul == 0 && right.opAdd == 0 && right.opMul == 0)
        return true;
    return false;
}

bool OpCode::operator!=(const OpCode& right) const noexcept
{
    return !(*this == right);
}

bool OpCode::operator<(const OpCode& right) const noexcept
{
    return opAdd < right.opAdd || opMul < right.opMul;
}

// Taken from https://stackoverflow.com/questions/2835469/how-to-perform-rotate-shift-in-c?noredirect=1&lq=1
CONST static unsigned int rotate_right(unsigned int value, int shift) noexcept
{
    if((shift &= 31) == 0)
        return value;
    return (value >> shift) | (value << (32 - shift));
}

// TODO somehow use the FlagBehavior here?
static std::pair<Optional<Literal>, ElementFlags> setFlags(Literal lit)
{
    auto flags = ElementFlags::fromLiteral(lit);
    // we don't know the flags for carry and 32-bit overflow here, since we don't actually have a plain literal, but an
    // operation return value which might set those.
    flags.carry = flags.overflow = FlagStatus::UNDEFINED;
    return std::make_pair(std::forward<Literal>(lit), flags);
}

static std::pair<Optional<Literal>, ElementFlags> setFlags(Literal lit, bool is32BitOverflow)
{
    auto tmp = setFlags(lit);
    tmp.second.carry = is32BitOverflow ? FlagStatus::SET : FlagStatus::CLEAR;
    return tmp;
}

static std::pair<Optional<Literal>, ElementFlags> setFlags(Literal lit, bool is32BitOverflow, bool isSignedOverflow)
{
    auto tmp = setFlags(lit, is32BitOverflow);
    tmp.second.overflow = isSignedOverflow ? FlagStatus::SET : FlagStatus::CLEAR;
    return tmp;
}

static bool checkMinMaxCarry(const Literal& arg0, const Literal& arg1)
{
    // VideoCore IV sets carry flag for fmin/fmax/fminabs/fmaxabs(a, b) if a > b
    // VideoCore IV considers NaN > Inf for min/fmax/fminabs/fmaxabs

    if(std::isnan(arg0.real()) && std::isnan(arg1.real()))
        // works, since the bit-representation is ordered same as integers
        return arg0.signedInt() > arg1.signedInt();
    if(std::isnan(arg0.real()))
        // -NaN < all < NaN
        return !std::signbit(arg0.real());
    if(std::isnan(arg1.real()))
        // -NaN < all < NaN
        return std::signbit(arg1.real());
    return arg0.real() > arg1.real();
}

static constexpr float infinity(bool sign)
{
    return sign ? -std::numeric_limits<float>::infinity() : std::numeric_limits<float>::infinity();
}

static PrecalculatedLiteral calcLiteral(const OpCode& code, Literal firstLit, Literal secondLit)
{
    switch(code.opAdd)
    {
    case OP_FADD.opAdd:
    {
        // -0 seems to be treated generally as +0
        auto firstArg = firstLit.real() == -0.0f ? 0.0f : firstLit.real();
        auto secondArg = secondLit.real() == -0.0f ? 0.0f : secondLit.real();
        if(std::isnan(secondArg))
            // for addition with NaN, the sign of the NaN is taken
            // this also applies if both operands are NaNs
            return setFlags(Literal(infinity(std::signbit(secondArg))), !std::signbit(secondArg));
        if(std::isnan(firstArg))
            // for addition with NaN, the sign of the NaN is taken
            return setFlags(Literal(infinity(std::signbit(firstArg))), !std::signbit(firstArg));
        auto tmp = FlushDenormsAndRoundToZero<float, float, std::plus<float>>{}(firstArg, secondArg);
        return setFlags(Literal(tmp), tmp > 0.0f);
    }
    case OP_FSUB.opAdd:
    {
        // -0 seems to be treated generally as +0
        auto firstArg = firstLit.real() == -0.0f ? 0.0f : firstLit.real();
        auto secondArg = secondLit.real() == -0.0f ? 0.0f : secondLit.real();
        if(std::isnan(secondArg))
            // for subtraction with NaN, the sign of the NaN is inverted
            // x - NaN -> -Inf, x - -NaN -> +Inf
            // this also applies if both operands are NaNs
            return setFlags(Literal(infinity(!std::signbit(secondArg))), std::signbit(secondArg));
        if(std::isnan(firstArg))
            // for subtraction with NaN, the sign of the NaN is taken
            return setFlags(Literal(infinity(std::signbit(firstArg))), !std::signbit(firstArg));
        auto tmp = FlushDenormsAndRoundToZero<float, float, std::minus<float>>{}(firstArg, secondArg);
        return setFlags(Literal(tmp), tmp > 0.0f);
    }
    case OP_FMIN.opAdd:
    {
        if(std::isnan(firstLit.real()))
        {
            if(std::signbit(firstLit.real()))
                // -NaN is less then everything else
                return setFlags(firstLit, checkMinMaxCarry(firstLit, secondLit));
            return setFlags(secondLit, checkMinMaxCarry(firstLit, secondLit));
        }
        if(std::isnan(secondLit.real()))
        {
            if(std::signbit(secondLit.real()))
                // -NaN is less then everything else
                return setFlags(secondLit, checkMinMaxCarry(firstLit, secondLit));
            return setFlags(firstLit, checkMinMaxCarry(firstLit, secondLit));
        }
        static_assert(std::min(std::numeric_limits<float>::infinity(), 0.0f) == 0.0f, "");
        static_assert(std::min(-std::numeric_limits<float>::infinity(), 0.0f) != 0.0f, "");
        return setFlags(
            Literal(std::min(firstLit.real(), secondLit.real())), firstLit.real() > secondLit.real(), false);
    }
    case OP_FMAX.opAdd:
    {
        if(std::isnan(firstLit.real()))
        {
            if(std::signbit(firstLit.real()))
                // -NaN is less then everything else
                return setFlags(secondLit, checkMinMaxCarry(firstLit, secondLit));
            return setFlags(firstLit, checkMinMaxCarry(firstLit, secondLit));
        }
        if(std::isnan(secondLit.real()))
        {
            if(std::signbit(secondLit.real()))
                // -NaN is less then everything else
                return setFlags(firstLit, checkMinMaxCarry(firstLit, secondLit));
            return setFlags(secondLit, checkMinMaxCarry(firstLit, secondLit));
        }
        if((firstLit.real() == 0.0f || firstLit.real() == -0.0f) &&
            (secondLit.real() == -0.0f || secondLit.real() == 0.0f))
            // test have shown that both zeroes are treated equal, with the second operand being returned
            return setFlags(secondLit, false);
        static_assert(std::max(std::numeric_limits<float>::infinity(), 0.0f) != 0.0f, "");
        static_assert(std::max(-std::numeric_limits<float>::infinity(), 0.0f) == 0.0f, "");
        return setFlags(
            Literal(std::max(firstLit.real(), secondLit.real())), firstLit.real() > secondLit.real(), false);
    }
    case OP_FMINABS.opAdd:
    {
        auto carryFlag = checkMinMaxCarry(Literal(std::abs(firstLit.real())), Literal(std::abs(secondLit.real())));
        if(std::isnan(firstLit.real()))
            return setFlags(Literal(std::abs(secondLit.real())), carryFlag);
        if(std::isnan(secondLit.real()))
            return setFlags(Literal(std::abs(firstLit.real())), carryFlag);
        return setFlags(Literal(std::min(std::abs(firstLit.real()), std::abs(secondLit.real()))), carryFlag, false);
    }
    case OP_FMAXABS.opAdd:
    {
        auto carryFlag = checkMinMaxCarry(Literal(std::abs(firstLit.real())), Literal(std::abs(secondLit.real())));
        if(std::isnan(firstLit.real()))
            return setFlags(Literal(std::abs(firstLit.real())), carryFlag);
        if(std::isnan(secondLit.real()))
            return setFlags(Literal(std::abs(secondLit.real())), carryFlag);
        if(std::isinf(firstLit.real()))
            return setFlags(Literal(std::abs(firstLit.real())), carryFlag);
        if(std::isinf(secondLit.real()))
            return setFlags(Literal(std::abs(secondLit.real())), carryFlag);
        return setFlags(Literal(std::max(std::fabs(firstLit.real()), std::fabs(secondLit.real()))), carryFlag, false);
    }
    case OP_FTOI.opAdd:
        // Converts unsigned values > 2^31 to unsigned integer and most out of bounds [INT_MIN, UINT_MAX] to 0.
        if(firstLit.real() == std::numeric_limits<float>::max() ||
            firstLit.real() == std::numeric_limits<float>::lowest())
            // somehow FLT_MIN and FLT_MAX are converted to INT_MIN
            return setFlags(Literal(std::numeric_limits<int32_t>::min()), false);
        if(std::isnan(firstLit.real()) || std::isinf(firstLit.real()) ||
            std::abs(firstLit.real()) > static_cast<float>(std::numeric_limits<int64_t>::max()) ||
            std::abs(static_cast<int64_t>(firstLit.real())) > std::numeric_limits<int32_t>::max())
            return setFlags(Literal(0u), false);
        return setFlags(Literal(static_cast<int32_t>(firstLit.real())), false);
    case OP_ITOF.opAdd:
    {
        auto tmp = RoundToZeroConversion<int32_t, float>{}(firstLit.signedInt());
        return setFlags(Literal(tmp), firstLit.signedInt() > 0);
    }
    case OP_ADD.opAdd:
    {
        auto extendedVal =
            static_cast<uint64_t>(firstLit.unsignedInt()) + static_cast<uint64_t>(secondLit.unsignedInt());
        auto signedVal = static_cast<int64_t>(firstLit.signedInt()) + static_cast<int64_t>(secondLit.signedInt());
        // use unsigned addition to have defined overflow wrap behavior, the actual calculation is identical
        return setFlags(Literal(firstLit.unsignedInt() + secondLit.unsignedInt()), extendedVal > uint64_t{0xFFFFFFFF},
            signedVal > static_cast<int64_t>(std::numeric_limits<int32_t>::max()) ||
                signedVal < static_cast<int64_t>(std::numeric_limits<int32_t>::min()));
    }
    case OP_SUB.opAdd:
    {
        auto extendedVal = static_cast<int64_t>(firstLit.signedInt()) - static_cast<int64_t>(secondLit.signedInt());
        // use unsigned subtraction to have defined overflow wrap behavior, the actual calculation is identical
        return setFlags(Literal(firstLit.unsignedInt() - secondLit.unsignedInt()),
            (firstLit.signedInt() >= 0 && secondLit.signedInt() < 0 && extendedVal != 0) ||
                (firstLit.signedInt() >= 0 && secondLit.signedInt() > 0 && extendedVal < 0) ||
                (firstLit.signedInt() < 0 && secondLit.signedInt() < 0 && extendedVal < 0),
            extendedVal > static_cast<int64_t>(std::numeric_limits<int32_t>::max()) ||
                extendedVal < static_cast<int64_t>(std::numeric_limits<int32_t>::min()));
    }
    case OP_SHR.opAdd:
    {
        // Tests have shown that on VC4 all shifts (asr, shr, shl) only take the last 5 bits of the offset (modulo 32)
        auto offset = secondLit.unsignedInt() & 0x1F;
        // carry is set iff the -1st bit is set. If only other bits (-2nd, -3rd, etc.) are shifted out of the value, the
        // carry flag is not set
        auto shiftLoss = offset != 0 && ((firstLit.unsignedInt() >> (offset - 1)) & 1);
        return setFlags(Literal(firstLit.unsignedInt() >> offset), shiftLoss);
    }
    case OP_ASR.opAdd:
    {
        // Tests have shown that on VC4 all shifts (asr, shr, shl) only take the last 5 bits of the offset (modulo 32)
        auto offset = secondLit.unsignedInt() & 0x1F;
        // carry is set iff the -1st bit is set. If only other bits (-2nd, -3rd, etc.) are shifted out of the value, the
        // carry flag is not set.
        auto shiftLoss = offset != 0 && ((firstLit.unsignedInt() >> (offset - 1)) & 1);
        return setFlags(intrinsics::asr(firstLit, secondLit), shiftLoss, false);
    }
    case OP_ROR.opAdd:
        return setFlags(Literal(rotate_right(firstLit.unsignedInt(), secondLit.signedInt())), false);
    case OP_SHL.opAdd:
    {
        // Tests have shown that on VC4 all shifts (asr, shr, shl) only take the last 5 bits of the offset (modulo 32)
        auto offset = secondLit.unsignedInt() & 0x1F;
        // carry is set iff the 32st bit is set. If only other bits (33rd, 35th, etc.) are shifted out of the value, the
        // carry flag is not set.
        auto extendedResult = static_cast<uint64_t>(firstLit.unsignedInt()) << offset;
        auto shiftLoss = ((extendedResult >> 32) & 1) == 1;
        return setFlags(Literal(firstLit.unsignedInt() << offset), shiftLoss);
    }
    case OP_MIN.opAdd:
        return setFlags(Literal(std::min(firstLit.signedInt(), secondLit.signedInt())),
            firstLit.signedInt() > secondLit.signedInt(), false);
    case OP_MAX.opAdd:
        return setFlags(Literal(std::max(firstLit.signedInt(), secondLit.signedInt())),
            firstLit.signedInt() > secondLit.signedInt(), false);
    case OP_AND.opAdd:
        return setFlags(Literal(firstLit.unsignedInt() & secondLit.unsignedInt()), false, false);
    case OP_OR.opAdd:
        return setFlags(Literal(firstLit.unsignedInt() | secondLit.unsignedInt()), false, false);
    case OP_XOR.opAdd:
        return setFlags(Literal(firstLit.unsignedInt() ^ secondLit.unsignedInt()), false, false);
    case OP_NOT.opAdd:
        return setFlags(Literal(~firstLit.unsignedInt()), false);
    case OP_CLZ.opAdd:
        return setFlags(intrinsics::clz(firstLit), false, false);
    }

    switch(code.opMul)
    {
    case OP_FMUL.opMul:
    {
        // -0 seems to be treated generally as +0
        auto firstArg = flushDenorms(firstLit.real() == -0.0f ? 0.0f : firstLit.real());
        auto secondArg = flushDenorms(secondLit.real() == -0.0f ? 0.0f : secondLit.real());
        auto eitherSign = std::signbit(firstArg) != std::signbit(secondArg);
        if(firstArg == 0.0f || secondArg == 0.0f)
            // multiplication with zero beats any NaN/Inf considerations
            return setFlags(Literal(0.0f));
        if(std::isnan(firstArg) || std::isnan(secondArg))
            return setFlags(Literal(infinity(eitherSign)), !eitherSign);
        return setFlags(Literal(firstArg * secondArg));
    }
    case OP_MUL24.opMul:
    {
        auto extendedVal = static_cast<uint64_t>(firstLit.unsignedInt() & 0xFFFFFFu) *
            static_cast<uint64_t>(secondLit.unsignedInt() & 0xFFFFFFu);
        return setFlags(Literal((firstLit.unsignedInt() & 0xFFFFFF) * (secondLit.unsignedInt() & 0xFFFFFF)),
            extendedVal > static_cast<uint64_t>(0xFFFFFFFFul));
    }
    }

    if(code == OP_V8ADDS || code == OP_V8SUBS || code == OP_V8MAX || code == OP_V8MIN || code == OP_V8MULD)
    {
        std::array<uint32_t, 4> bytesA, bytesB, bytesOut;
        bytesA[0] = firstLit.unsignedInt() & 0xFF;
        bytesA[1] = firstLit.unsignedInt() >> 8 & 0xFF;
        bytesA[2] = firstLit.unsignedInt() >> 16 & 0xFF;
        bytesA[3] = firstLit.unsignedInt() >> 24 & 0xFF;
        bytesB[0] = secondLit.unsignedInt() & 0xFF;
        bytesB[1] = secondLit.unsignedInt() >> 8 & 0xFF;
        bytesB[2] = secondLit.unsignedInt() >> 16 & 0xFF;
        bytesB[3] = secondLit.unsignedInt() >> 24 & 0xFF;
        std::function<uint32_t(uint32_t, uint32_t)> func = [&](uint32_t, uint32_t) -> uint32_t {
            throw CompilationError(CompilationStep::GENERAL, "Unhandled op-code", code.name);
        };
        if(code == OP_V8ADDS)
            func = [](uint32_t a, uint32_t b) -> uint32_t { return std::min(a + b, 255u); };
        if(code == OP_V8SUBS)
            func = [](uint32_t a, uint32_t b) -> uint32_t {
                return static_cast<uint32_t>(std::max(std::min(static_cast<int32_t>(a - b), 255), 0));
            };
        if(code == OP_V8MAX)
            func = [](uint32_t a, uint32_t b) -> uint32_t { return std::max(a, b); };
        if(code == OP_V8MIN)
            func = [](uint32_t a, uint32_t b) -> uint32_t { return std::min(a, b); };
        if(code == OP_V8MULD)
            func = [](uint32_t a, uint32_t b) -> uint32_t { return (a * b + 127) / 255; };

        std::transform(bytesA.begin(), bytesA.end(), bytesB.begin(), bytesOut.begin(), func);
        uint32_t result = ((bytesOut[3] & 0xFF) << 24) | ((bytesOut[2] & 0xFF) << 16) | ((bytesOut[1] & 0xFF) << 8) |
            (bytesOut[0] & 0xFF);
        return setFlags(Literal(result), false);
    }

    return std::make_pair(Optional<Literal>{}, ElementFlags{});
}

PrecalculatedValue OpCode::operator()(const Value& firstOperand, const Optional<Value>& secondOperand) const
{
    if(numOperands > 1 && !secondOperand)
        return std::make_pair(NO_VALUE, VectorFlags{});

    if(numOperands == 1 && firstOperand.isUndefined())
        // returns an undefined value (of the correct type)
        return std::make_pair(
            (acceptsFloat == returnsFloat) ? Value(firstOperand.type) : UNDEFINED_VALUE, VectorFlags{});
    if(numOperands == 2 && secondOperand->isUndefined())
        // returns an undefined value (of the correct type)
        return std::make_pair((acceptsFloat == returnsFloat && firstOperand.type == secondOperand->type) ?
                Value(firstOperand.type) :
                UNDEFINED_VALUE,
            VectorFlags{});

    // extract the literal value behind the operands
    if(!firstOperand.getLiteralValue() && !firstOperand.checkVector())
        return std::make_pair(NO_VALUE, VectorFlags{});
    Optional<Value> secondVal =
        !secondOperand || (secondOperand->getLiteralValue() || secondOperand->checkVector()) ? secondOperand : NO_VALUE;
    if(numOperands > 1 && !secondVal)
        return std::make_pair(NO_VALUE, VectorFlags{});

    auto firstVector = firstOperand.checkVector();
    auto secondVector = secondVal ? secondVal->checkVector() : nullptr;

    // do not calculate vector rotations
    if(firstOperand.checkImmediate() && firstOperand.immediate().isVectorRotation())
        return std::make_pair(NO_VALUE, VectorFlags{});
    if(numOperands > 1 && secondVal->checkImmediate() && secondVal->immediate().isVectorRotation())
        return std::make_pair(NO_VALUE, VectorFlags{});

    // both (used) values are literals (or literal containers)
    bool calcPerComponent =
        (firstVector && !firstVector->getAllSame()) || (numOperands > 1 && secondVector && !secondVector->getAllSame());
    DataType resultType = firstOperand.type;
    if(numOperands > 1 &&
        (secondVal->type.getVectorWidth() > resultType.getVectorWidth() ||
            secondVal->type.containsType(firstOperand.type)))
        resultType = secondVal->type;

    // at least one used value is a container, need to calculate component-wise
    if(calcPerComponent)
    {
        SIMDVector res;
        VectorFlags flags;
        auto elementType = resultType.getElementType();
        for(unsigned char i = 0; i < res.size(); ++i)
        {
            PrecalculatedValue tmp{NO_VALUE, {}};
            if(numOperands == 1)
                tmp = operator()(
                    firstVector ? (*firstVector)[i] : *firstOperand.getLiteralValue(), UNDEFINED_LITERAL, elementType);
            else
                tmp = operator()(firstVector ? (*firstVector)[i] : *firstOperand.getLiteralValue(),
                    secondVector ? (*secondVector)[i] : *secondVal->getLiteralValue(), elementType);
            if(!tmp.first)
                // result could not be calculated for a single component of the vector, abort
                return std::make_pair(NO_VALUE, VectorFlags{});
            res[i] = *std::move(tmp.first)->getLiteralValue();
            flags[i] = tmp.second[0];
        }
        // TODO this could lead to a lot of vectors in the global storage!
        return std::make_pair(SIMDVectorHolder::storeVector(
                                  std::move(res), resultType, (firstVector ? firstVector : secondVector)->getStorage()),
            flags);
    }

    if(firstOperand.isUndefined() || (numOperands > 1 && secondVal && secondVal->isUndefined()))
        return std::make_pair(UNDEFINED_VALUE, VectorFlags{});

    // TODO throws if first element is no literal
    const Literal firstLit = firstOperand.getLiteralValue() ? *firstOperand.getLiteralValue() : (*firstVector)[0];
    const Literal secondLit = (!secondVal || numOperands == 1) ?
        Literal(0u) :
        secondVal->getLiteralValue() ? *secondVal->getLiteralValue() : (*secondVector)[0];
    return operator()(firstLit, secondLit, resultType);
}

PrecalculatedValue OpCode::operator()(Literal firstOperand, Literal secondOperand, DataType resultType) const
{
    if(numOperands >= 1 && firstOperand.isUndefined())
        // returns an undefined value (of the correct type)
        return std::make_pair(returnsFloat ? Value(TYPE_FLOAT) : Value(resultType), VectorFlags{});
    if(numOperands == 2 && secondOperand.isUndefined())
        // returns an undefined value (of the correct type)
        return std::make_pair(returnsFloat ? Value(TYPE_FLOAT) : Value(resultType), VectorFlags{});

    auto tmp = calcLiteral(*this, firstOperand, secondOperand);
    if(!tmp.first)
        return std::make_pair(NO_VALUE, VectorFlags{});

    resultType = opAdd == OP_FTOI.opAdd ? TYPE_INT32 : resultType;
    resultType = opAdd == OP_ITOF.opAdd ? TYPE_FLOAT : resultType;
    return std::make_pair(Value(*tmp.first, resultType), tmp.second);
}

// Since e.g. for emulation we always calculate the whole SIMD-vector even so we might only care about some of the
// elements, we will have a lot of calculations with undefined operands. To still be able to run the emulations
// while at the same time not to truncate the undefined literal to zero (which could be an expected value), replace
// all undefined operands with a random value. For the pre-calculation we propagate the undefined operands correctly
// in the OpCode::operator() taking Literal arguments.
static Literal resolveUndefined(Literal operand, bool isFloat)
{
    static std::default_random_engine engine{};
    static std::uniform_int_distribution<uint32_t> distribution{};
    if(operand.isUndefined())
    {
        operand = Literal(distribution(engine));
        operand.type = LiteralType::REAL;
    }
    return operand;
}

PrecalculatedVector OpCode::operator()(const SIMDVector& firstOperand, const SIMDVector& secondOperand) const
{
    if(numOperands >= 1 && firstOperand.isUndefined())
        // returns an undefined vector
        return std::make_pair(SIMDVector{}, VectorFlags{});
    if(numOperands == 2 && secondOperand.isUndefined())
        // returns an undefined vector
        return std::make_pair(SIMDVector{}, VectorFlags{});
    SIMDVector res;
    VectorFlags flags;
    for(unsigned char i = 0; i < res.size(); ++i)
    {
        PrecalculatedLiteral tmp{Optional<Literal>{}, {}};
        if(numOperands == 1)
            tmp = calcLiteral(*this, resolveUndefined(firstOperand[i], acceptsFloat), UNDEFINED_LITERAL);
        else
            tmp = calcLiteral(*this, resolveUndefined(firstOperand[i], acceptsFloat),
                resolveUndefined(secondOperand[i], acceptsFloat));
        if(!tmp.first)
            // result could not be calculated for a single component of the vector, abort
            return std::make_pair(Optional<SIMDVector>{}, VectorFlags{});
        res[i] = *tmp.first;
        flags[i] = tmp.second;
    }
    return std::make_pair(res, flags);
}

analysis::ValueRange OpCode::operator()(
    const analysis::ValueRange& firstRange, const analysis::ValueRange& secondRange) const
{
    using namespace analysis;

    switch(opAdd)
    {
    case OP_FADD.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_FLOAT;
        return ValueRange{firstRange.minValue + secondRange.minValue, firstRange.maxValue + secondRange.maxValue};
    case OP_FSUB.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_FLOAT;
        return ValueRange{firstRange.minValue - secondRange.maxValue, firstRange.maxValue - secondRange.minValue};
    case OP_FMIN.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_FLOAT;
        return ValueRange{
            std::min(firstRange.minValue, secondRange.minValue), std::min(firstRange.maxValue, secondRange.maxValue)};
    case OP_FMAX.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_FLOAT;
        return ValueRange{
            std::max(firstRange.minValue, secondRange.minValue), std::max(firstRange.maxValue, secondRange.maxValue)};
    case OP_FMINABS.opAdd:
    {
        if(!firstRange || !secondRange)
            return RANGE_FLOAT;
        auto firstAbs = firstRange.toAbsoluteRange();
        auto secondAbs = secondRange.toAbsoluteRange();
        return ValueRange{
            std::min(firstAbs.minValue, secondAbs.minValue), std::min(firstAbs.maxValue, secondAbs.maxValue)};
    }
    case OP_FMAXABS.opAdd:
    {
        if(!firstRange || !secondRange)
            return RANGE_FLOAT;
        auto firstAbs = firstRange.toAbsoluteRange();
        auto secondAbs = secondRange.toAbsoluteRange();
        return ValueRange{
            std::max(firstAbs.minValue, secondAbs.minValue), std::max(firstAbs.maxValue, secondAbs.maxValue)};
    }
    case OP_FTOI.opAdd:
        if(!firstRange)
            return RANGE_INT;
        return ValueRange{static_cast<double>(saturate<int32_t>(static_cast<int64_t>(firstRange.minValue))),
            static_cast<double>(saturate<int32_t>(static_cast<int64_t>(firstRange.maxValue)))};
    case OP_ITOF.opAdd:
        if(!firstRange)
            return RANGE_FLOAT;
        return ValueRange{static_cast<double>(firstRange.minValue), static_cast<double>(firstRange.maxValue)};
    case OP_ADD.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_INT;
        return ValueRange{firstRange.minValue + secondRange.minValue, firstRange.maxValue + secondRange.maxValue};
    case OP_SUB.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_INT;
        return ValueRange{firstRange.minValue - secondRange.maxValue, firstRange.maxValue - secondRange.minValue};
    case OP_SHR.opAdd:
        if(!firstRange && !secondRange)
            return RANGE_UINT;
        if(firstRange && firstRange.minValue < 0.0)
            return RANGE_UINT;
        if(firstRange && secondRange && secondRange.isUnsigned() &&
            (static_cast<int64_t>(secondRange.minValue) / 32) == (static_cast<int64_t>(secondRange.maxValue) / 32))
        {
            // if there is no modulo 32 overflow within the values of the second range, we can use the following:
            // [a, b] >> [c, d] -> [a >> d, b >> c]
            auto minOffset = static_cast<int64_t>(secondRange.minValue) % 32;
            auto minFactor = static_cast<double>(1 << minOffset);
            auto maxOffset = static_cast<int64_t>(secondRange.maxValue) % 32;
            auto maxFactor = static_cast<double>(1 << maxOffset);
            return ValueRange(std::trunc(firstRange.minValue / maxFactor), std::trunc(firstRange.maxValue / minFactor));
        }
        if(secondRange && secondRange.getSingletonValue() && secondRange.isUnsigned())
        {
            // [a, b] >> const -> [a / 2^const, b / 2^const]
            // the case for first range given is already handled above
            // second operand is taken modulo 32!!
            auto offset = static_cast<int64_t>(*secondRange.getSingletonValue()) % 32;
            auto div = static_cast<double>(1 << offset);
            return RANGE_UINT / div;
        }
        if(firstRange && firstRange.isUnsigned())
            // [a, b] >> [?, ?] -> [0, b]
            return ValueRange{0.0, std::trunc(firstRange.maxValue)};
        return RANGE_UINT;
    case OP_SHL.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_UINT;
        if(firstRange && firstRange.minValue < 0.0)
            return RANGE_UINT;
        if(firstRange && firstRange.isUnsigned())
        {
            if(firstRange.getSingletonValue() == 0.0)
                // [0, 0] >> [?, ?] > [0, 0]
                return ValueRange(0.0, 0.0);
            if(secondRange && secondRange.isUnsigned() &&
                (static_cast<int64_t>(secondRange.minValue) / 32) == (static_cast<int64_t>(secondRange.maxValue) / 32))
            {
                // if there is no modulo 32 overflow within the values of the second range, we can use the following:
                // [a, b] << [c, d] -> [a << c, b << d]
                auto minOffset = static_cast<int64_t>(secondRange.minValue) % 32;
                auto newMin = static_cast<int64_t>(firstRange.minValue) << minOffset;
                auto maxOffset = static_cast<int64_t>(secondRange.maxValue) % 32;
                auto newMax = static_cast<int64_t>(firstRange.maxValue) << maxOffset;
                return ValueRange(
                    static_cast<double>(saturate<uint32_t>(newMin)), static_cast<double>(saturate<uint32_t>(newMax)));
            }
            // [a, b] << [?, ?] -> [a, UINT_MAX]
            return ValueRange(
                std::trunc(firstRange.minValue), static_cast<double>(std::numeric_limits<uint32_t>::max()));
        }
        return RANGE_UINT;
    case OP_MIN.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_INT;
        return ValueRange{
            std::min(firstRange.minValue, secondRange.minValue), std::min(firstRange.maxValue, secondRange.maxValue)};
    case OP_MAX.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_INT;
        return ValueRange{
            std::max(firstRange.minValue, secondRange.minValue), std::max(firstRange.maxValue, secondRange.maxValue)};
    case OP_AND.opAdd:
        if(!firstRange || !secondRange)
            return RANGE_UINT;
        if(firstRange.minValue < 0 || secondRange.minValue < 0)
            return RANGE_UINT;
        return ValueRange{0, std::min(firstRange.maxValue, secondRange.maxValue)};
    case OP_CLZ.opAdd:
        // XXX could try to determine maximum number of leading zeroes from input
        return ValueRange{0.0, 32.0};
    }

    if(opMul == OP_FMUL.opMul)
    {
        if(!firstRange || !secondRange)
            return ValueRange{TYPE_FLOAT};
        auto options = {firstRange.minValue * secondRange.minValue, firstRange.minValue * secondRange.maxValue,
            firstRange.maxValue * secondRange.minValue, firstRange.maxValue * secondRange.maxValue};
        return ValueRange{std::min(options), std::max(options)};
    }
    if(opMul == OP_MUL24.opMul)
    {
        if(!firstRange || !secondRange)
            return RANGE_UINT;
        if(firstRange.minValue < 0.0 || secondRange.minValue < 0.0)
            return RANGE_UINT;
        if(firstRange.maxValue >= static_cast<double>(0xFFFFFFu) ||
            secondRange.maxValue >= static_cast<double>(0xFFFFFFu))
            return RANGE_UINT;

        auto options = {firstRange.minValue * secondRange.minValue, firstRange.minValue * secondRange.maxValue,
            firstRange.maxValue * secondRange.minValue, firstRange.maxValue * secondRange.maxValue};
        return ValueRange{std::min(options), std::max(options)};
    }

    return ValueRange(returnsFloat ? TYPE_FLOAT : TYPE_INT32);
}

OperationBitMasks OpCode::operator()(BitMask firstMask, BitMask secondMask) const
{
    if(opAdd == OP_CLZ.opAdd)
        // result is in [0, 32]
        return {BitMask{0x0000003F}, BITMASK_ALL, BITMASK_NONE};

    if(opAdd == OP_AND.opAdd)
        // only bits which are set in both operands can be set in result
        return {firstMask & secondMask, BITMASK_ALL, BITMASK_ALL};

    if(opAdd == OP_OR.opAdd || opAdd == OP_XOR.opAdd)
        // only bits which are set in either operand can be set in result
        return {firstMask | secondMask, BITMASK_ALL, BITMASK_ALL};

    if(opAdd == OP_SHL.opAdd || opAdd == OP_SHR.opAdd || opAdd == OP_ASR.opAdd)
        return {BITMASK_ALL, BITMASK_ALL, BitMask{0x0000001F}};

    if(opAdd == OP_V8ADDS.opAdd || opAdd == OP_V8SUBS.opAdd || opMul == OP_V8ADDS.opMul || opMul == OP_V8SUBS.opMul ||
        opMul == OP_V8MULD.opMul || opMul == OP_V8MAX.opMul || opMul == OP_V8MIN.opMul)
    {
        // parts are saturated/calculated per byte, so if a byte is not set at all, there cannot be any output there
        BitMask resultMask{(firstMask.getByte0() || secondMask.getByte0() ? 0xFF : 0) |
            (firstMask.getByte1() || secondMask.getByte1() ? 0xFF00 : 0) |
            (firstMask.getByte2() || secondMask.getByte2() ? 0xFF0000 : 0) |
            (firstMask.getByte3() || secondMask.getByte3() ? 0xFF000000 : 0)};
        return {resultMask, BITMASK_ALL, BITMASK_ALL};
    }

    if(opMul == OP_MUL24.opMul)
        return {BITMASK_ALL, BitMask{0x00FFFFFF}, BitMask{0x00FFFFFF}};

    // default to needing all bits and writing all of them
    return {BITMASK_ALL, BITMASK_ALL, numOperands > 1 ? BITMASK_ALL : BITMASK_NONE};
}

const OpCode& OpCode::toOpCode(const std::string& name)
{
    const OpCode& code = findOpCode(name);
    if(code == OP_NOP && name != "nop")
        throw CompilationError(CompilationStep::GENERAL, "No machine code operation for this op-code", name);
    return code;
}

static const std::multimap<std::string, OpCode> opCodes = {{OP_ADD.name, OP_ADD}, {OP_AND.name, OP_AND},
    {OP_ASR.name, OP_ASR}, {OP_CLZ.name, OP_CLZ}, {OP_FADD.name, OP_FADD}, {OP_FMAX.name, OP_FMAX},
    {OP_FMAXABS.name, OP_FMAXABS}, {OP_FMIN.name, OP_FMIN}, {OP_FMINABS.name, OP_FMINABS}, {OP_FMUL.name, OP_FMUL},
    {OP_FSUB.name, OP_FSUB}, {OP_FTOI.name, OP_FTOI}, {OP_ITOF.name, OP_ITOF}, {OP_MAX.name, OP_MAX},
    {OP_MIN.name, OP_MIN}, {OP_MUL24.name, OP_MUL24}, {OP_NOP.name, OP_NOP}, {OP_NOT.name, OP_NOT}, {OP_OR.name, OP_OR},
    {OP_ROR.name, OP_ROR}, {OP_SHL.name, OP_SHL}, {OP_SHR.name, OP_SHR}, {OP_SUB.name, OP_SUB},
    {OP_V8ADDS.name, OP_V8ADDS}, {OP_V8MAX.name, OP_V8MAX}, {OP_V8MIN.name, OP_V8MIN}, {OP_V8MULD.name, OP_V8MULD},
    {OP_V8SUBS.name, OP_V8SUBS}, {OP_XOR.name, OP_XOR}};

// NOTE: The indices MUST correspond to the op-codes!
static const std::array<OpCode, 32> addCodes = {OP_NOP, OP_FADD, OP_FSUB, OP_FMIN, OP_FMAX, OP_FMINABS, OP_FMAXABS,
    OP_FTOI, OP_ITOF, OP_NOP, OP_NOP, OP_NOP, OP_ADD, OP_SUB, OP_SHR, OP_ASR, OP_ROR, OP_SHL, OP_MIN, OP_MAX, OP_AND,
    OP_OR, OP_XOR, OP_NOT, OP_CLZ, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_V8ADDS, OP_V8SUBS};

// NOTE: The indices MUST correspond to the op-codes!
static const std::array<OpCode, 8> mulCodes = {
    OP_NOP, OP_FMUL, OP_MUL24, OP_V8MULD, OP_V8MIN, OP_V8MAX, OP_V8ADDS, OP_V8SUBS};

bool OpCode::isIdempotent() const noexcept
{
    return *this == OP_AND || *this == OP_FMAX || *this == OP_FMIN || *this == OP_MAX || *this == OP_MIN ||
        *this == OP_OR || *this == OP_V8MAX || *this == OP_V8MIN;
}

bool OpCode::isAssociative() const noexcept
{
    return *this == OP_ADD || *this == OP_AND || *this == OP_FADD || *this == OP_FMAX || *this == OP_FMAXABS ||
        *this == OP_FMIN || *this == OP_FMINABS || *this == OP_FMUL || *this == OP_MAX || *this == OP_MIN ||
        *this == OP_OR || *this == OP_V8MAX || *this == OP_V8MIN || *this == OP_XOR;
}

bool OpCode::isCommutative() const noexcept
{
    return *this == OP_ADD || *this == OP_AND || *this == OP_FADD || *this == OP_FMAX || *this == OP_FMAXABS ||
        *this == OP_FMIN || *this == OP_FMINABS || *this == OP_FMUL || *this == OP_MAX || *this == OP_MIN ||
        *this == OP_MUL24 || *this == OP_OR || *this == OP_V8ADDS || *this == OP_V8MAX || *this == OP_V8MIN ||
        *this == OP_V8MULD || *this == OP_XOR;
}

bool OpCode::isLeftDistributiveOver(const OpCode& other) const noexcept
{
    if(*this == OP_FMUL)
        return other == OP_FADD || other == OP_FSUB;
    if(*this == OP_FADD)
        return other == OP_FMIN || other == OP_FMAX;
    if(*this == OP_ADD)
        return other == OP_MIN || other == OP_MAX;
    if(*this == OP_AND)
        return other == OP_OR || other == OP_XOR;
    return false;
}

bool OpCode::isRightDistributiveOver(const OpCode& other) const noexcept
{
    if(*this == OP_FMUL)
        return other == OP_FADD || other == OP_FSUB;
    if(*this == OP_FADD)
        return other == OP_FMIN || other == OP_FMAX;
    if(*this == OP_ADD)
        return other == OP_MIN || other == OP_MAX;
    if(*this == OP_AND)
        return other == OP_OR || other == OP_XOR;
    return false;
}

bool OpCode::isSelfInverse() const noexcept
{
    return *this == OP_NOT || *this == OP_SUB || *this == OP_FSUB || *this == OP_XOR;
}

const OpCode& OpCode::toOpCode(const unsigned char opCode, const bool isMulALU)
{
    if(opCode == 0)
        return OP_NOP;
    if(isMulALU)
        return mulCodes.at(opCode);
    else
        return addCodes.at(opCode);
}

const OpCode& OpCode::findOpCode(const std::string& name)
{
    auto it = opCodes.find(name);
    if(it != opCodes.end())
        return it->second;
    return OP_NOP;
}

Optional<Value> OpCode::getLeftIdentity(const OpCode& code)
{
    if(code == OP_ADD)
        return INT_ZERO;
    if(code == OP_AND)
        return VALUE_ALL_BITS_SET;
    if(code == OP_FADD)
        return FLOAT_ZERO;
    if(code == OP_FMIN)
        return FLOAT_NAN;
    if(code == OP_FMAX)
        // -Inf
        return Value(Literal(0xFF800000), TYPE_FLOAT);
    if(code == OP_FMUL)
        return FLOAT_ONE;
    if(code == OP_MUL24)
        return INT_ONE;
    if(code == OP_OR)
        return INT_ZERO;
    if(code == OP_XOR)
        return INT_ZERO;
    return NO_VALUE;
}

Optional<Value> OpCode::getRightIdentity(const OpCode& code)
{
    if(code == OP_ADD)
        return INT_ZERO;
    if(code == OP_AND)
        return VALUE_ALL_BITS_SET;
    if(code == OP_ASR)
        return INT_ZERO;
    if(code == OP_FADD)
        return FLOAT_ZERO;
    if(code == OP_FMIN)
        return FLOAT_NAN;
    if(code == OP_FMUL)
        return FLOAT_ONE;
    if(code == OP_FSUB)
        return FLOAT_ZERO;
    if(code == OP_MUL24)
        return INT_ONE;
    if(code == OP_OR)
        return INT_ZERO;
    if(code == OP_ROR)
        return INT_ZERO;
    if(code == OP_SHL)
        return INT_ZERO;
    if(code == OP_SHR)
        return INT_ZERO;
    if(code == OP_SUB)
        return INT_ZERO;
    if(code == OP_XOR)
        return INT_ZERO;
    return NO_VALUE;
}

Optional<Value> OpCode::getLeftAbsorbingElement(const OpCode& code)
{
    if(code == OP_AND)
        return INT_ZERO;
    if(code == OP_ASR)
        // XXX actually all bits set too
        return INT_ZERO;
    if(code == OP_FMAX)
        return FLOAT_NAN;
    if(code == OP_FMAXABS)
        return FLOAT_NAN;
    if(code == OP_FMINABS)
        return FLOAT_ZERO;
    if(code == OP_FMUL)
        return FLOAT_ZERO;
    if(code == OP_MUL24)
        return INT_ZERO;
    if(code == OP_OR)
        return VALUE_ALL_BITS_SET;
    if(code == OP_ROR)
        // XXX actually all bits set too
        return INT_ZERO;
    if(code == OP_SHL)
        return INT_ZERO;
    if(code == OP_SHR)
        return INT_ZERO;
    if(code == OP_V8MIN)
        return INT_ZERO;
    if(code == OP_V8MAX)
        return VALUE_ALL_BITS_SET;
    if(code == OP_V8MULD)
        return INT_ZERO;
    return NO_VALUE;
}

Optional<Value> OpCode::getRightAbsorbingElement(const OpCode& code)
{
    if(code == OP_AND)
        return INT_ZERO;
    if(code == OP_FMAX)
        return FLOAT_NAN;
    if(code == OP_FMAXABS)
        return FLOAT_NAN;
    if(code == OP_FMINABS)
        return FLOAT_ZERO;
    if(code == OP_FMUL)
        return FLOAT_ZERO;
    if(code == OP_MUL24)
        return INT_ZERO;
    if(code == OP_OR)
        return VALUE_ALL_BITS_SET;
    if(code == OP_V8MIN)
        return INT_ZERO;
    if(code == OP_V8MAX)
        return VALUE_ALL_BITS_SET;
    if(code == OP_V8MULD)
        return INT_ZERO;
    return NO_VALUE;
}

LCOV_EXCL_START
std::string BranchCond::to_string() const
{
    switch(*this)
    {
    case BRANCH_ALL_C_CLEAR:
        return "ifallcc";
    case BRANCH_ALL_C_SET:
        return "ifallc";
    case BRANCH_ALL_N_CLEAR:
        return "ifallnc";
    case BRANCH_ALL_N_SET:
        return "ifalln";
    case BRANCH_ALL_Z_CLEAR:
        return "ifallzc";
    case BRANCH_ALL_Z_SET:
        return "ifallz";
    case BRANCH_ALWAYS:
        return "";
    case BRANCH_ANY_C_CLEAR:
        return "ifanycc";
    case BRANCH_ANY_C_SET:
        return "ifanyc";
    case BRANCH_ANY_N_CLEAR:
        return "ifanync";
    case BRANCH_ANY_N_SET:
        return "ifanyn";
    case BRANCH_ANY_Z_CLEAR:
        return "ifanyzc";
    case BRANCH_ANY_Z_SET:
        return "ifanyz";
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Invalid branch-condition", std::to_string(static_cast<unsigned>(value)));
}
LCOV_EXCL_STOP

BranchCond BranchCond::invert() const
{
    switch(*this)
    {
    case BRANCH_ALL_C_CLEAR:
        return BRANCH_ANY_C_SET;
    case BRANCH_ALL_C_SET:
        return BRANCH_ANY_C_CLEAR;
    case BRANCH_ALL_N_CLEAR:
        return BRANCH_ANY_N_SET;
    case BRANCH_ALL_N_SET:
        return BRANCH_ANY_N_CLEAR;
    case BRANCH_ALL_Z_CLEAR:
        return BRANCH_ANY_Z_SET;
    case BRANCH_ALL_Z_SET:
        return BRANCH_ANY_Z_CLEAR;
    case BRANCH_ANY_C_CLEAR:
        return BRANCH_ALL_C_SET;
    case BRANCH_ANY_C_SET:
        return BRANCH_ALL_C_CLEAR;
    case BRANCH_ANY_N_CLEAR:
        return BRANCH_ALL_N_SET;
    case BRANCH_ANY_N_SET:
        return BRANCH_ALL_N_CLEAR;
    case BRANCH_ANY_Z_CLEAR:
        return BRANCH_ALL_Z_SET;
    case BRANCH_ANY_Z_SET:
        return BRANCH_ALL_Z_CLEAR;
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Invalid branch-condition", std::to_string(static_cast<unsigned>(value)));
}

bool BranchCond::isInversionOf(BranchCond other) const
{
    if(*this == BRANCH_ALWAYS || other == BRANCH_ALWAYS)
        // there is no "branch never"
        return false;
    return other == invert();
}

ConditionCode BranchCond::toConditionCode() const
{
    switch(*this)
    {
    case BRANCH_ALL_C_CLEAR:
    case BRANCH_ANY_C_CLEAR:
        return COND_CARRY_CLEAR;
    case BRANCH_ALL_C_SET:
    case BRANCH_ANY_C_SET:
        return COND_CARRY_SET;
    case BRANCH_ALL_N_CLEAR:
    case BRANCH_ANY_N_CLEAR:
        return COND_NEGATIVE_CLEAR;
    case BRANCH_ALL_N_SET:
    case BRANCH_ANY_N_SET:
        return COND_NEGATIVE_SET;
    case BRANCH_ALL_Z_CLEAR:
    case BRANCH_ANY_Z_CLEAR:
        return COND_ZERO_CLEAR;
    case BRANCH_ALL_Z_SET:
    case BRANCH_ANY_Z_SET:
        return COND_ZERO_SET;
    case BRANCH_ALWAYS:
        return COND_ALWAYS;
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Invalid branch-condition", std::to_string(static_cast<unsigned>(value)));
}
