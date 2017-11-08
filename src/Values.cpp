/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Values.h"
#include "Locals.h"
#include "CompilationError.h"
#include "intermediate/IntermediateInstruction.h"

#include <limits>

using namespace vc4c;

std::string vc4c::toString(const RegisterFile file)
{
	std::string fileName;
	if(file == RegisterFile::ANY)
		return "any";
	if(file == RegisterFile::NONE)
		return "none";
	if(has_flag(file, RegisterFile::ACCUMULATOR))
		fileName.append("acc");
	if(has_flag(file, RegisterFile::PHYSICAL_A))
		fileName.append(fileName.empty() ? "" : ",").append("A");
	if(has_flag(file, RegisterFile::PHYSICAL_B))
		fileName.append(fileName.empty() ? "" : ",").append("B");

	return fileName;
}

bool vc4c::isFixed(const RegisterFile file)
{
	return file == RegisterFile::ACCUMULATOR || file == RegisterFile::PHYSICAL_A || file == RegisterFile::PHYSICAL_B;
}

Register::Register() : file(RegisterFile::NONE), num(SIZE_MAX)
{

}

bool Register::isGeneralPurpose() const
{
    return num < 32;
}

std::string Register::to_string(bool specialNames, bool readAccess) const
{
    if(specialNames)
    {
        if(readAccess)
        {
            if(num == 32)
                return "unif";
            if(num == 35)
                return "varying";
            if(num == 36)
            	//previously "tmu_in|sfu_in", but TMU is not used
                return "sfu_in";
            if(num == 37)
                return "rep";
            if(num == 38)
                return file == RegisterFile::PHYSICAL_A ? "elem_num" : "qpu_num";
            if(num == 39)
                return "-";
            if(num == 48)
                return "vpm";
            if(num == 49)
                return file == RegisterFile::PHYSICAL_A ? "vpr_busy" : "vpw_busy";
            if(num == 50)
                return file == RegisterFile::PHYSICAL_A ? "vpr_wait" : "vpw_wait";
            if(num == 51)
                return "mutex_acq";
        }
        else
        {
            if(num == 37)
                return file == RegisterFile::PHYSICAL_A ? "rep_quad" : "rep_all";
            if(num == 38)
                return "irq";
            if(num == 39)
                return "-";
            if(num == 48)
                return "vpm";
            if(num == 49)
                return file == RegisterFile::PHYSICAL_A ? "vpr_setup" : "vpw_setup";
            if(num == 50)
                return file == RegisterFile::PHYSICAL_A ? "vpr_addr" : "vpw_addr";
            if(num == 51)
                return "mutex_rel";
            if(num == 52)
                return "sfu_recip";
            if(num == 53)
                return "sfu_rsqrt";
            if(num == 54)
                return "sfu_exp";
            if(num == 55)
                return "sfu_log";
            if(num == 56)
                return "tmu0s";
            if(num == 57)
                return "tmu0t";
            if(num == 58)
                return "tmu0r";
            if(num == 59)
                return "tmu0b";
            if(num == 60)
                return "tmu1s";
            if(num == 61)
                return "tmu1t";
            if(num == 62)
                return "tmu1r";
            if(num == 63)
                return "tmu1b";

        }
        if(getAccumulatorNumber() != INVALID_ACCUMULATOR)
        {
            return std::string("r") + std::to_string(getAccumulatorNumber());
        }
    }
    return std::string(file == RegisterFile::PHYSICAL_A ? "ra" : (file == RegisterFile::PHYSICAL_B ? "rb" : "rx")) + std::to_string(num);
}

int Register::getAccumulatorNumber() const
{
    switch(num)
    {
    case 32: return 0;
    case 33: return 1;
    case 34: return 2;
    case 35: return 3;
    case 36: return 4;
    case 37: return 5;
    default:
        return -1;
    }
}

bool Register::operator<(const Register& right) const
{
    const auto tmp = static_cast<unsigned char>(file) < static_cast<unsigned char>(right.file);
    if(tmp != 0)
        return tmp;
    return num < right.num;
}

bool Register::operator>(const Register& right) const
{
	const auto tmp = static_cast<unsigned char>(file) > static_cast<unsigned char>(right.file);
	if(tmp != 0)
		return tmp;
	return num > right.num;
}

bool Register::operator==(const Register& right) const
{
	if(this == &right)
		return true;
    return num == right.num && file == right.file;
}

bool Register::isAccumulator() const
{
    return (num >= 32 && num <= 37) || file == RegisterFile::ACCUMULATOR;
}

bool Register::isVertexPipelineMemory() const
{
    return num >= 48 && num <= 50;
}

bool Register::isSpecialFunctionsUnit() const
{
    return num >= 52 && num <= 55;
}

bool Register::hasSideEffectsOnRead() const
{
	if(!isReadable())
		return false;
	if(num == 32 || num == 35 || num == 36) /* UNIFORM, VARYING and SFU/TMU read */
		return true;
	if(num >= 48 && num <= 50) /* VPM read, busy, wait */
		return true;
	if(num == 51) /* mutex acquire */
		return true;
	return false;
}

bool Register::hasSideEffectsOnWrite() const
{
	if(!isWriteable())
		return false;
	if(num == 36) /* TMU noswap */
		return true;
	if(num == 38) /* host interrupt */
		return true;
	if(num == 40) /* UNIFORM address */
		return true;
	if(num >= 41 && num <= 47) /* Tile buffer setup and value writes */
		return true;
	if(num >= 48 && num <= 50) /* VPM setup */
		return true;
	if(num == 51) /* mutex release */
		return true;
	if(num >= 52 && num <= 55) /* SFU calls */
		return true;
	if(num >= 56 && num <= 63) /* TMU setup */
		return true;
	return false;
}

bool Register::isReadable() const
{
	if(num == 40 /* UNIFORM address */ || (num >= 43 && num <= 47) /* TLB setup */ ||
			num >= 52 /* SFU, TMU write */)
		return false;
	return true;
}

bool Register::isWriteable() const
{
	return true;
}

Literal::Literal(const int64_t integer) : integer(integer), type(LiteralType::INTEGER)
{

}

Literal::Literal(const uint64_t integer) : integer(integer), type(LiteralType::INTEGER)
{

}

Literal::Literal(const double real): integer(bit_cast<double, int64_t>(real)), type(LiteralType::REAL)
{

}

Literal::Literal(const bool flag) : integer(flag), type(LiteralType::BOOL)
{

}

bool Literal::operator==(const Literal& other) const
{
	if(this == &other)
		return true;
    if(type != other.type)
        return false;
    switch(type)
    {
    case LiteralType::BOOL:
        return isTrue() == other.isTrue();
    case LiteralType::INTEGER:
        return integer == other.integer;
    case LiteralType::REAL:
        return real() == other.real();
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled literal type!");
}

bool Literal::operator<(const Literal& other) const
{
	return integer < other.integer;
}

const std::string Literal::to_string() const
{
    switch(type)
    {
    case LiteralType::BOOL:
        return (isTrue() ? "true" : "false");
    case LiteralType::INTEGER:
        return std::to_string(integer);
    case LiteralType::REAL:
        return std::to_string(real());
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled literal type!");
}

bool Literal::isTrue() const
{
    if(type == LiteralType::BOOL || type == LiteralType::INTEGER)
        return integer == 1;
    return false;
}

double Literal::real() const
{
	return bit_cast<int64_t, double>(integer);
}

uint32_t Literal::toImmediate() const
{
    //https://stackoverflow.com/questions/12342926/casting-float-to-int-bitwise-in-c
    union {
        int32_t i;
        uint32_t u;
        float f;
    } tmp;
    switch(type)
    {
        case LiteralType::BOOL:
            return isTrue();
        case LiteralType::INTEGER:
            if(static_cast<long>(std::numeric_limits<unsigned int>::max()) > 0 /* on Raspberry Pi this is -1, because long seems to only have 4 bytes too */
            		&& integer > static_cast<long>(std::numeric_limits<unsigned int>::max()))
                throw CompilationError(CompilationStep::GENERAL, "Immediate out of range", std::to_string(integer));
            if(integer > std::numeric_limits<int>::max())
                tmp.u = static_cast<uint32_t>(integer);
            else
                tmp.i = static_cast<int32_t>(integer);
            return tmp.u;
        case LiteralType::REAL:
            tmp.f = static_cast<float>(real());
            return tmp.u;
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled literal type!");
}

bool ContainerValue::isAllSame(const Optional<Literal>& value) const
{
	if(elements.size() == 0)
	{
		return true;
	}
	const Literal singleValue = value ? value.get() : elements.at(0).literal;
	for(const Value& element : elements)
	{
		if(element.isUndefined())
			//if items are UNDEFINED, ignore them, since maybe the remaining items all have the same value
			continue;
		if(!element.hasLiteral(singleValue))
			return false;
	}
	return true;
}

bool ContainerValue::isElementNumber(bool withOffset) const
{
	if(elements.size() == 0)
	{
		return true;
	}
	const int64_t offset = withOffset ? elements.at(0).literal.integer : 0;
	for(std::size_t i = 0; i < elements.size(); ++i)
	{
		if(elements.at(i).isUndefined())
			//if items are UNDEFINED, ignore them, since maybe the remaining items correspond to the element-number
			continue;
		if(!elements.at(i).hasType(ValueType::LITERAL))
			throw CompilationError(CompilationStep::GENERAL, "Invalid container element", elements.at(i).to_string());
		if(elements.at(i).literal.integer != static_cast<int64_t>(i) + offset)
			return false;
	}
	return true;
}

Value::Value(const Literal& lit, const DataType& type) : literal(lit), type(type), valueType(ValueType::LITERAL)
{

}

Value::Value(const Register& reg, const DataType& type) : reg(reg), type(type), valueType(ValueType::REGISTER)
{

}

Value::Value(const ContainerValue& container, const DataType& type) : local(), type(type), valueType(ValueType::CONTAINER), container(container)
{

}

Value::Value(const Value& val) : local(val.local),  type(val.type), valueType(val.valueType)
{
    if (val.hasType(ValueType::LITERAL))
        literal = val.literal;
    else if (val.hasType(ValueType::REGISTER))
        reg = val.reg;
    else if (val.hasType(ValueType::CONTAINER))
        container = val.container;
    else if(val.hasType(ValueType::SMALL_IMMEDIATE))
    	immediate = val.immediate;
    else if(!val.hasType(ValueType::LOCAL) && !val.hasType(ValueType::UNDEFINED))
        throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

Value::Value(Value&& val) : local(val.local), type(val.type), valueType(val.valueType)
{
    if (val.hasType(ValueType::LITERAL))
        literal = val.literal;
    else if (val.hasType(ValueType::REGISTER))
        reg = val.reg;
    else if (val.hasType(ValueType::CONTAINER))
        container = val.container;
    else if(val.hasType(ValueType::SMALL_IMMEDIATE))
    	immediate = val.immediate;
    else if(!val.hasType(ValueType::LOCAL) && !val.hasType(ValueType::UNDEFINED))
        throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

Value::Value(const Local* local, const DataType& type) : local(const_cast<Local*>(local)), type(type), valueType(ValueType::LOCAL)
{

}

Value::Value(const DataType& type) : local(), type(type), valueType(ValueType::UNDEFINED)
{

}

Value::Value(const SmallImmediate& immediate, const DataType& type) : immediate(immediate), type(type), valueType(ValueType::SMALL_IMMEDIATE)
{

}

bool Value::operator==(const Value& other) const
{
	if(this == &other)
		return true;
    if(valueType != other.valueType)
        return false;
    if(type != other.type)
        return false;
    switch(valueType)
    {
    case ValueType::CONTAINER:
        return container.elements == other.container.elements;
    case ValueType::LITERAL:
        return other.hasLiteral(literal);
    case ValueType::LOCAL:
        return local == other.local;
    case ValueType::REGISTER:
        return reg == other.reg;
    case ValueType::UNDEFINED:
        return true;
    case ValueType::SMALL_IMMEDIATE:
    	return other.hasImmediate(immediate);
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

Value Value::getCompoundPart(int index) const
{
    if(hasType(ValueType::CONTAINER))
    {
        return container.elements.at(index);
    }
    else if(hasType(ValueType::LOCAL))
    {
        return Value(local, type.getElementType());
    }
    else if(hasType(ValueType::UNDEFINED))
    {
        return UNDEFINED_VALUE;
    }
    else if(hasType(ValueType::REGISTER))
    {
        //would only be valid for IO-registers, writing all values sequentially??
        return Value(reg, type.getElementType());
    }
    throw CompilationError(CompilationStep::GENERAL, "Can't get part of non-compound value", to_string(false));
}

bool Value::hasType(const ValueType type) const
{
    return valueType == type;
}

bool Value::hasLocal(const Local* local) const
{
    return hasType(ValueType::LOCAL) && checkPointer(this->local) == checkPointer(local);
}

bool Value::hasRegister(const Register& reg) const
{
    return hasType(ValueType::REGISTER) && this->reg == reg;
}

bool Value::hasLiteral(const Literal& lit) const
{
	if(hasType(ValueType::SMALL_IMMEDIATE))
		return (immediate.getIntegerValue().hasValue && immediate.getIntegerValue().get() == lit.integer) ||
				(immediate.getFloatingValue().hasValue && immediate.getFloatingValue().get() == lit.real());
    return hasType(ValueType::LITERAL) && this->literal == lit;
}

bool Value::hasImmediate(const SmallImmediate& immediate) const
{
	if(hasType(ValueType::LITERAL))
		return (immediate.getIntegerValue().hasValue && immediate.getIntegerValue().get() == literal.integer) ||
				(immediate.getFloatingValue().hasValue && immediate.getFloatingValue().get() == literal.real());
	return hasType(ValueType::SMALL_IMMEDIATE) && this->immediate == immediate;
}

bool Value::isUndefined() const
{
    return hasType(ValueType::UNDEFINED);
}

bool Value::isZeroInitializer() const
{
    return (hasType(ValueType::LITERAL) && literal.integer == 0) ||
    		(hasType(ValueType::SMALL_IMMEDIATE) && immediate.value == 0) ||
			(hasType(ValueType::CONTAINER) && std::all_of(container.elements.begin(), container.elements.end(), [](const Value& val) -> bool {return val.isZeroInitializer();}));
}

bool Value::isLiteralValue() const
{
	return hasType(ValueType::LITERAL) || hasType(ValueType::SMALL_IMMEDIATE);
}

std::string Value::to_string(const bool writeAccess, bool withLiterals) const
{
    const std::string typeName = (type.typeName.empty() ? "unknown" : type.to_string()) + ' ';
    switch(valueType)
    {
    case ValueType::LITERAL:
        return typeName + literal.to_string();
    case ValueType::CONTAINER:
    {
    	if(withLiterals)
    	{
    		std::string tmp;
    		const std::string pre = type.isVectorType() ? "<" : type.getArrayType().hasValue ? "[" : "{";
    		const std::string post = type.isVectorType() ? ">" : type.getArrayType().hasValue ? "]" : "}";
    		for(const Value& element : container.elements)
    			tmp.append(element.to_string(writeAccess, withLiterals)).append(", ");
    		return typeName + pre + tmp.substr(0, tmp.length() - 2) + post;
    	}
    	if(isZeroInitializer())
    		return typeName + "zeronitializer";
        return typeName + std::string("container with ") + std::to_string(container.elements.size()) + " elements";
    }
    case ValueType::LOCAL:
        return typeName + local->name;
    case ValueType::REGISTER:
        return std::string("register ") + reg.to_string(true, !writeAccess);
    case ValueType::UNDEFINED:
        return typeName + "undefined";
    case ValueType::SMALL_IMMEDIATE:
		return typeName + immediate.toString();
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

bool Value::isWriteable() const
{
	switch(valueType)
	{
		case ValueType::CONTAINER:
		case ValueType::LITERAL:
		case ValueType::UNDEFINED:
		case ValueType::SMALL_IMMEDIATE:
			return false;
		case ValueType::LOCAL:
			return true;
		case ValueType::REGISTER:
			return reg.isWriteable();
	}
	throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

bool Value::isReadable() const
{
	switch(valueType)
	{
		case ValueType::CONTAINER:
		case ValueType::LITERAL:
		case ValueType::UNDEFINED:
		case ValueType::LOCAL:
		case ValueType::SMALL_IMMEDIATE:
			return true;
		case ValueType::REGISTER:
			return reg.isReadable();
	}
	throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}
const Value& Value::assertWriteable() const
{
	if(isWriteable())
		return *this;
	throw CompilationError(CompilationStep::GENERAL, "Cannot write to a read-only value", to_string(false));
}

Value& Value::assertWriteable()
{
	if(isWriteable())
		return *this;
	throw CompilationError(CompilationStep::GENERAL, "Cannot write to a read-only value", to_string(false));
}

const Value& Value::assertReadable() const
{
	if(isReadable())
		return *this;
	throw CompilationError(CompilationStep::GENERAL, "Cannot read from a write-only value", to_string(false));
}

Value& Value::assertReadable()
{
	if(isReadable())
		return *this;
	throw CompilationError(CompilationStep::GENERAL, "Cannot read from a write-only value", to_string(false));
}

Value Value::createZeroInitializer(const DataType& type)
{
	if(type.isScalarType() || type.getPointerType().hasValue)
		return INT_ZERO;
	Value val(ContainerValue(), type);
	if(type.isVectorType())
	{
		for(unsigned i = 0; i < type.num; i++)
		{
			val.container.elements.push_back(INT_ZERO);
		}
	}
	else if(type.getArrayType().hasValue)
	{
		for(unsigned i = 0; i < type.getArrayType().get()->size; i++)
		{
			val.container.elements.push_back(createZeroInitializer(type.getElementType()));
		}
	}
	else if(type.getStructType().hasValue)
	{
		for(unsigned i = 0; i < type.getStructType().get()->elementTypes.size(); i++)
		{
			val.container.elements.push_back(createZeroInitializer(type.getElementType(i)));
		}
	}
	else
		throw CompilationError(CompilationStep::GENERAL, "Unhandled type for zero-initializer", type.to_string());
	return val;
}

std::size_t vc4c::hash<vc4c::Value>::operator()(vc4c::Value const& val) const noexcept
{
	return std::hash<std::string>::operator()(val.to_string());
}
