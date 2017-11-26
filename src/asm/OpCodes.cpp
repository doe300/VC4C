/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "OpCodes.h"

#include "../Values.h"
#include "CompilationError.h"

#include <map>

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
        return BranchCond::ANY_C_SET;
    case COND_NEGATIVE_CLEAR.value:
        return BranchCond::ALL_N_CLEAR;
    case COND_NEGATIVE_SET.value:
        return BranchCond::ANY_N_SET;
    case COND_ZERO_CLEAR.value:
        return BranchCond::ALL_Z_CLEAR;
    case COND_ZERO_SET.value:
        return BranchCond::ANY_Z_SET;
    }
    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid condition for branch", toString());
}

std::string Signaling::toString() const
{
	switch (*this)
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
	throw CompilationError(CompilationStep::CODE_GENERATION, "Unsupported signal", std::to_string(static_cast<unsigned>(value)));
}

bool Signaling::hasSideEffects() const
{
	return *this != SIGNAL_NONE && *this != SIGNAL_ALU_IMMEDIATE && *this != SIGNAL_LOAD_IMMEDIATE;
}

std::string Unpack::toString() const
{
	//http://maazl.de/project/vc4asm/doc/extensions.html#pack
	switch (*this)
	{
		case UNPACK_NOP:
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

bool OpCode::operator==(const OpCode& right) const
{
	if(opAdd > 0 && opAdd == right.opAdd)
		return true;
	if(opMul > 0 && opMul == right.opMul)
		return true;
	if(opAdd == 0 && opMul == 0 && right.opAdd == 0 && right.opMul == 0)
		return true;
	return false;
}

bool OpCode::operator!=(const OpCode& right) const
{
	return !(*this == right);
}

bool OpCode::operator<(const OpCode& right) const
{
	return opAdd < right.opAdd || opMul < right.opMul;
}

const OpCode& OpCode::toOpCode(const std::string& name)
{
	const OpCode& code = findOpCode(name);
	if(code == OP_NOP && name.compare("nop") != 0)
		throw CompilationError(CompilationStep::GENERAL, "No machine code operation for this op-code", name);
	return code;
}

static const std::multimap<std::string, OpCode> opCodes = {
		{OP_ADD.name, OP_ADD}, {OP_AND.name, OP_AND}, {OP_ASR.name, OP_ASR}, {OP_CLZ.name, OP_CLZ},
		{OP_FADD.name, OP_FADD}, {OP_FMAX.name, OP_FMAX}, {OP_FMAXABS.name, OP_FMAXABS}, {OP_FMIN.name, OP_FMIN},
		{OP_FMINABS.name, OP_FMINABS}, {OP_FMUL.name, OP_FMUL}, {OP_FSUB.name, OP_FSUB}, {OP_FTOI.name, OP_FTOI},
		{OP_ITOF.name, OP_ITOF}, {OP_MAX.name, OP_MAX}, {OP_MIN.name, OP_MIN}, {OP_MUL24.name, OP_MUL24},
		{OP_NOP.name, OP_NOP}, {OP_NOT.name, OP_NOT}, {OP_OR.name, OP_OR}, {OP_ROR.name, OP_ROR},
		{OP_SHL.name, OP_SHL}, {OP_SHR.name, OP_SHR}, {OP_SUB.name, OP_SUB}, {OP_V8ADDS.name, OP_V8ADDS},
		{OP_V8MAX.name, OP_V8MAX}, {OP_V8MIN.name, OP_V8MIN}, {OP_V8MULD.name, OP_V8MULD}, {OP_V8SUBS.name, OP_V8SUBS},
		{OP_XOR.name, OP_XOR}
};

static const std::map<unsigned char, OpCode> addCodes = {
		{OP_ADD.opAdd, OP_ADD}, {OP_AND.opAdd, OP_AND}, {OP_ASR.opAdd, OP_ASR}, {OP_CLZ.opAdd, OP_CLZ},
		{OP_FADD.opAdd, OP_FADD}, {OP_FMAX.opAdd, OP_FMAX}, {OP_FMAXABS.opAdd, OP_FMAXABS}, {OP_FMIN.opAdd, OP_FMIN},
		{OP_FMINABS.opAdd, OP_FMINABS}, {OP_FSUB.opAdd, OP_FSUB}, {OP_FTOI.opAdd, OP_FTOI}, {OP_ITOF.opAdd, OP_ITOF},
		{OP_MAX.opAdd, OP_MAX}, {OP_MIN.opAdd, OP_MIN}, {OP_NOP.opAdd, OP_NOP}, {OP_NOT.opAdd, OP_NOT},
		{OP_OR.opAdd, OP_OR}, {OP_ROR.opAdd, OP_ROR}, {OP_SHL.opAdd, OP_SHL}, {OP_SHR.opAdd, OP_SHR},
		{OP_SUB.opAdd, OP_SUB}, {OP_V8ADDS.opAdd, OP_V8ADDS}, {OP_V8SUBS.opAdd, OP_V8SUBS}, {OP_XOR.opAdd, OP_XOR}
};

static const std::map<unsigned char, OpCode> mulCodes = {
		{OP_FMUL.opMul, OP_FMUL}, {OP_MUL24.opMul, OP_MUL24}, {OP_NOP.opMul, OP_NOP}, {OP_V8ADDS.opMul, OP_V8ADDS},
		{OP_V8MAX.opMul, OP_V8MAX}, {OP_V8MIN.opMul, OP_V8MIN}, {OP_V8MULD.opMul, OP_V8MULD}, {OP_V8SUBS.opMul, OP_V8SUBS}
};

const OpCode& OpCode::toOpCode(const unsigned char opCode, const bool isMulALU)
{
	if(opCode == 0)
		return OP_NOP;
	if(isMulALU)
	{
		auto it = mulCodes.find(opCode);
		if(it != mulCodes.end())
			return it->second;
	}
	else
	{
		auto it = addCodes.find(opCode);
		if(it != addCodes.end())
			return it->second;
	}
	throw CompilationError(CompilationStep::GENERAL, "No machine code operation for this op-code", std::to_string(static_cast<unsigned>(opCode)));
}

const OpCode& OpCode::findOpCode(const std::string& name)
{
	auto it = opCodes.find(name);
	if(it != opCodes.end())
		return it->second;
	return OP_NOP;
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
