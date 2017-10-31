/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "OpCodes.h"

#include "CompilationError.h"
#include "../Values.h"

#include <vector>

using namespace vc4c;


std::string ConditionCode::toString() const
{
	switch (*this)
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
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported condition", std::to_string(static_cast<unsigned>(value)));
}

ConditionCode ConditionCode::invert() const
{
	switch (*this)
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
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported conditions!");
}

bool ConditionCode::isInversionOf(const ConditionCode other) const
{
	return other ==  invert();
}

BranchCond ConditionCode::toBranchCondition() const
{
    switch(value)
    {
    case COND_ALWAYS.value:
        return BranchCond::ALWAYS;
    case COND_CARRY_CLEAR.value:
        return BranchCond::ALL_C_CLEAR;
    case COND_CARRY_SET.value:
        return BranchCond::ALL_C_SET;
    case COND_NEGATIVE_CLEAR.value:
        return BranchCond::ALL_N_CLEAR;
    case COND_NEGATIVE_SET.value:
        return BranchCond::ALL_N_SET;
    case COND_ZERO_CLEAR.value:
        return BranchCond::ALL_Z_CLEAR;
    case COND_ZERO_SET.value:
        return BranchCond::ALL_Z_SET;
    }
    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid condition for branch", toString());
}

std::string vc4c::toString(const Signaling signal)
{
	switch (signal)
	{
		case Signaling::ALPHA_LOAD:
			return "loada";
		case Signaling::ALU_IMMEDIATE:
			return "imm";
		case Signaling::BRANCH:
			return "br";
		case Signaling::COLOR_LOAD:
			return "loadc";
		case Signaling::COLOR_LOAD_END:
			return "loadc_end";
		case Signaling::COVERAGE_LOAD:
			return "loadcov";
		case Signaling::LAST_THREAD_SWITCH:
			return "lthrsw";
		case Signaling::LOAD_IMMEDIATE:
			return "load_imm";
		case Signaling::LOAD_TMU0:
			return "load_tmu0";
		case Signaling::LOAD_TMU1:
			return "load_tmu1";
		case Signaling::NO_SIGNAL:
			return "";
		case Signaling::PROGRAM_END:
			return "thrend";
		case Signaling::SCORE_UNLOCK:
			return "scoreu";
		case Signaling::SOFT_BREAK:
			return "bkpt";
		case Signaling::THREAD_SWITCH:
			return "thrsw";
		case Signaling::WAIT_FOR_SCORE:
			return "scorew";
	}
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported signal", std::to_string(static_cast<unsigned>(signal)));
}

std::string SmallImmediate::toString() const
{
	if (value <= 15)
		// 0, ..., 15
		return (std::to_string(static_cast<int>(value)) + " (") + std::to_string(static_cast<int>(value)) + ")";
	if (value <= 31)
		// -16, ..., -1
		return (std::to_string(static_cast<int>(value) - 32) + " (") + std::to_string(static_cast<int>(value)) + ")";
	if (value <= 39)
		// 1.0, ..., 128.0
		return (std::to_string(static_cast<float>(1 << (static_cast<int>(value) - 32))) + " (") + std::to_string(static_cast<int>(value)) + ")";
	if (value <= 47)
		// 1/256, ..., 1/2
		return (std::to_string(1.0f / (1 << (48 - static_cast<int>(value)))) + " (") + std::to_string(static_cast<int>(value)) + ")";
	if (value == 48)
		return "<< r5";
	if (value <= 63)
		return std::string("<< ") + std::to_string(static_cast<int>(value) - 48);
	throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid small immediate value", std::to_string(static_cast<unsigned>(value)));
}

Optional<char> SmallImmediate::getIntegerValue() const
{
	if (value <= 15)
		// 0, ..., 15
		return static_cast<char>(value);
	if (value <= 31)
		// -16, ..., -1
		return static_cast<char>(32 - static_cast<char>(value));
	return {};
}

Optional<float> SmallImmediate::getFloatingValue() const
{
	if (value >= 32 && value <= 39)
		// 1.0, ..., 128.0
		return static_cast<float>(1 << (static_cast<unsigned>(value) - 32));
	if (value >= 40 && value <= 47)
		// 1/256, ..., 1/2
		return 1.0f / (1 << (48 - static_cast<unsigned>(value)));
	return {};
}

bool SmallImmediate::isVectorRotation() const
{
	return value >= 48 && value <= 63;
}

Optional<unsigned char> SmallImmediate::getRotationOffset() const
{
	if(!isVectorRotation())
		return {};
	if(*this == VECTOR_ROTATE_R5)
		return {};
	return static_cast<unsigned char>(value - VECTOR_ROTATE_R5.value);
}

Optional<Literal> SmallImmediate::toLiteral() const
{
	if(getIntegerValue().hasValue)
		return Literal(static_cast<long>(getIntegerValue().get()));
	if(getFloatingValue().hasValue)
		return Literal(getFloatingValue().get());
	return Optional<Literal>(false, 0L);
}

SmallImmediate SmallImmediate::fromRotationOffset(unsigned char offset)
{
	if(offset == 0 || offset > 15)
		//Offset of 0 would result in the use of r5 register
		throw CompilationError(CompilationStep::GENERAL, "Invalid vector rotation offset", std::to_string(static_cast<int>(offset)));
    return static_cast<SmallImmediate>(offset + VECTOR_ROTATE_R5);
}

std::string Unpack::toString() const
{
	//http://maazl.de/project/vc4asm/doc/extensions.html#pack
	switch (*this)
	{
		case UNPACK_NOP:
			return "";
		case UNPACK_16A_32:
			return "upLow16to32";
		case UNPACK_16B_32:
			return "upHigh16to32";
		case UNPACK_8888_32:
			return "replMSByte";
		case UNPACK_8A_32:
			return "upByte0To32";
		case UNPACK_8B_32:
			return "upByte1To32";
		case UNPACK_8C_32:
			return "upByte2To32";
		case UNPACK_8D_32:
			return "upByte3To32";
	}
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported unpack-mode", std::to_string(static_cast<unsigned>(value)));
}

const Unpack Unpack::unpackTo32Bit(const DataType& type)
{
	if(type.getScalarBitCount() >= 32)
		return UNPACK_NOP;
	if(type.getScalarBitCount() == 16)
		return UNPACK_16A_32;
	if(type.getScalarBitCount() == 8)
		return UNPACK_8A_32;
	throw CompilationError(CompilationStep::GENERAL, "Unhandled type-width for unpack-modes", type.to_string());
}

std::string Pack::toString() const
{
	//http://maazl.de/project/vc4asm/doc/extensions.html#pack
	switch (*this)
	{
		case PACK_NOP:
			return "";
		case PACK_32_16A:
			return "p32toLow16";
		case PACK_32_16A_S:
			return "sat16ToLow16";
		case PACK_32_16B:
			return "p32ToHigh16";
		case PACK_32_16B_S:
			return "sat16ToHigh16";
		case PACK_32_32:
			return "sat";
		case PACK_32_8888:
			return "replLSByte";
		case PACK_32_8888_S:
			return "replLSByteSat";
		case PACK_32_8A:
			return "pLSByteToByte0";
		case PACK_32_8A_S:
			return "sat8ToByte0";
		case PACK_32_8B:
			return "pLSByteToByte1";
		case PACK_32_8B_S:
			return "sat8ToByte1";
		case PACK_32_8C:
			return "pLSByteToByte2";
		case PACK_32_8C_S:
			return "sat8ToByte2";
		case PACK_32_8D:
			return "pLSByteToByte3";
		case PACK_32_8D_S:
			return "sat8ToByte3";
	}
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported pack-mode", std::to_string(static_cast<unsigned>(value)));
}

Optional<Value> Pack::pack(const Value& val) const
{
	//we never can pack complex types (even pointer, there are always 32-bit)
	if(val.type.complexType)
		return NO_VALUE;
	//for now, we can't pack floats
	if(val.type.isFloatingType())
		return NO_VALUE;
	//can only pack literals
	if(!val.hasType(ValueType::LITERAL) && !val.hasType(ValueType::SMALL_IMMEDIATE))
		return NO_VALUE;
	switch (*this)
	{
		case PACK_NOP:
			return val;
		case PACK_32_16A:
			return Value(Literal(val.literal.integer & 0xFFFF), val.type);
		case PACK_32_16A_S:
			return Value(Literal(saturate<int16_t>(val.literal.integer)), val.type);
		case PACK_32_16B:
			return Value(Literal((val.literal.integer & 0xFFFF) << 16), val.type);
		case PACK_32_16B_S:
			return NO_VALUE;
		case PACK_32_32:
			return Value(Literal(saturate<int32_t>(val.literal.integer)), val.type);
		case PACK_32_8888:
			return Value(Literal(((val.literal.integer & 0xFF) << 24) | ((val.literal.integer & 0xFF) << 16) | ((val.literal.integer & 0xFF) << 8) | (val.literal.integer & 0xFF)), val.type);
		case PACK_32_8888_S:
			return Value(Literal((saturate<uint8_t>(val.literal.integer) << 24) | (saturate<uint8_t>(val.literal.integer) << 16) | (saturate<uint8_t>(val.literal.integer) << 8) | saturate<uint8_t>(val.literal.integer)), val.type);
		case PACK_32_8A:
			return Value(Literal(val.literal.integer & 0xFF), val.type);
		case PACK_32_8A_S:
			return Value(Literal(saturate<uint8_t>(val.literal.integer)), val.type);
		case PACK_32_8B:
			return Value(Literal((val.literal.integer & 0xFF) << 8), val.type);
		case PACK_32_8B_S:
			return Value(Literal(saturate<uint8_t>(val.literal.integer) << 8), val.type);
		case PACK_32_8C:
			return Value(Literal((val.literal.integer & 0xFF) << 16), val.type);
		case PACK_32_8C_S:
			return Value(Literal(saturate<uint8_t>(val.literal.integer) << 16), val.type);
		case PACK_32_8D:
			return Value(Literal((val.literal.integer & 0xFF) << 24), val.type);
		case PACK_32_8D_S:
			return Value(Literal(saturate<uint8_t>(val.literal.integer) << 24), val.type);
	}
	throw CompilationError(CompilationStep::GENERAL, "Unsupported pack-mode", std::to_string(static_cast<unsigned>(value)));
}

std::string vc4c::toString(const SetFlag flag)
{
	switch (flag)
	{
		case SetFlag::DONT_SET:
			return "";
		case SetFlag::SET_FLAGS:
			return "setf";
	}
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported set-flags flag", std::to_string(static_cast<unsigned>(flag)));
}

static std::vector<OpAdd> opAdds = { OPADD_NOP, OPADD_FADD, OPADD_FSUB, OPADD_FMIN, OPADD_FMAX, OPADD_FMINABS, OPADD_FMAXABS, OPADD_FTOI, OPADD_ITOF,
		OPADD_NOP, OPADD_NOP, OPADD_NOP,
		OPADD_ADD, OPADD_SUB, OPADD_SHR, OPADD_ASR, OPADD_ROR, OPADD_SHL, OPADD_MIN, OPADD_MAX, OPADD_AND, OPADD_OR, OPADD_XOR, OPADD_NOT, OPADD_CLZ,
		OPADD_NOP, OPADD_NOP, OPADD_NOP, OPADD_NOP, OPADD_NOP,
		OPADD_V8ADDS, OPADD_V8SUBS
};

OpAdd::OpAdd(const unsigned char opCode) : name(opAdds.at(opCode).name), opCode(opCode), numOperands(opAdds.at(opCode).numOperands)
{
}

bool OpAdd::operator ==(const OpAdd& right) const
{
	return this->opCode == right.opCode;
}

bool OpAdd::operator !=(const OpAdd& right) const
{
	return this->opCode != right.opCode;
}

OpAdd::operator unsigned char() const
{
	return opCode;
}

const OpAdd& OpAdd::toOpCode(const std::string& opCode)
{
	for(const OpAdd& op : opAdds)
	{
		if(opCode.compare(op.name) == 0)
			return op;
	}
	throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid op-code", opCode);
}

static std::vector<OpMul> opMuls = { OPMUL_NOP, OPMUL_FMUL, OPMUL_MUL24, OPMUL_V8MULD, OPMUL_V8MIN, OPMUL_V8MAX, OPMUL_V8ADDS, OPMUL_V8SUBS};

OpMul::OpMul(const unsigned char opCode) : name(opMuls.at(opCode).name), opCode(opCode), numOperands(opMuls.at(opCode).numOperands)
{
}

bool OpMul::operator ==(const OpMul& right) const
{
	return this->opCode == right.opCode;
}

bool OpMul::operator !=(const OpMul& right) const
{
	return this->opCode != right.opCode;
}

OpMul::operator unsigned char() const
{
	return opCode;
}

const OpMul& OpMul::toOpCode(const std::string& opCode)
{
	for(const OpMul& op : opMuls)
	{
		if(opCode.compare(op.name) == 0)
			return op;
	}
	throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid op-code", opCode);
}

std::string vc4c::toString(const BranchCond cond)
{
    switch(cond)
    {
        case BranchCond::ALL_C_CLEAR:
            return "ifallcc";
        case BranchCond::ALL_C_SET:
            return "ifallc";
        case BranchCond::ALL_N_CLEAR:
            return "ifallnc";
        case BranchCond::ALL_N_SET:
            return "ifalln";
        case BranchCond::ALL_Z_CLEAR:
            return "ifallzc";
        case BranchCond::ALL_Z_SET:
            return "ifallz";
        case BranchCond::ALWAYS:
            return "";
        case BranchCond::ANY_C_CLEAR:
            return "ifanycc";
        case BranchCond::ANY_C_SET:
            return "ifanyc";
        case BranchCond::ANY_N_CLEAR:
            return "ifanync";
        case BranchCond::ANY_N_SET:
            return "ifanyn";
        case BranchCond::ANY_Z_CLEAR:
            return "ifanyzc";
        case BranchCond::ANY_Z_SET:
            return "ifanyz";
    }
    throw CompilationError(CompilationStep::GENERAL, "Invalid branch-condition!");
}
