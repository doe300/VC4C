/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Values.h"

#include "CompilationError.h"
#include "Locals.h"
#include "intermediate/IntermediateInstruction.h"

#include <limits>

using namespace vc4c;

constexpr Literal tombstone_traits<Literal>::tombstone;
constexpr SmallImmediate tombstone_traits<SmallImmediate>::tombstone;

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

bool Register::isGeneralPurpose() const
{
    return num < 32;
}

std::string Register::to_string(bool specialNames, bool readAccess) const
{
    if(specialNames)
    {
        if(readAccess && file != RegisterFile::ACCUMULATOR)
        {
            if(num == 32)
                return "unif";
            if(num == 35)
                return "varying";
            if(num == 36)
                return "sfu_tmu_in";
            if(num == 37)
                return "rep";
            if(num == 38)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "elem_num";
                if(file == RegisterFile::PHYSICAL_B)
                    return "qpu_num";
            }
            if(num == 39)
                return "-";
            if(num == 48)
                return "vpm";
            if(num == 49)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_busy";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_busy";
            }
            if(num == 50)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_wait";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_wait";
            }
            if(num == 51)
                return "mutex_acq";
        }
        else
        {
            if(num == 36 && file != RegisterFile::ACCUMULATOR)
                return "tmu_noswap";
            if(num == 37)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "rep_quad|r5";
                if(file == RegisterFile::PHYSICAL_B)
                    return "rep_all|r5";
            }
            if(num == 38)
                return "irq";
            if(num == 39)
                return "-";
            if(num == 48)
                return "vpm";
            if(num == 49)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_setup";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_setup";
            }
            if(num == 50)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_addr";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_addr";
            }
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
    return std::string(file == RegisterFile::PHYSICAL_A ? "ra" : (file == RegisterFile::PHYSICAL_B ? "rb" : "rx")) +
        std::to_string(num);
}

int Register::getAccumulatorNumber() const
{
    switch(num)
    {
    case 32:
        return 0;
    case 33:
        return 1;
    case 34:
        return 2;
    case 35:
        return 3;
    case 36:
        return 4;
    case 37:
        return 5;
    default:
        return -1;
    }
}

bool Register::operator<(Register right) const
{
    const auto tmp = static_cast<unsigned char>(file) < static_cast<unsigned char>(right.file);
    return tmp || num < right.num;
}

bool Register::operator>(Register right) const
{
    const auto tmp = static_cast<unsigned char>(file) > static_cast<unsigned char>(right.file);
    return tmp || num > right.num;
}

bool Register::operator==(Register right) const
{
    if(this == &right)
        return true;
    return num == right.num && file == right.file;
}

bool Register::isAccumulator() const
{
    return (num >= 32 && num <= 37) || file == RegisterFile::ACCUMULATOR;
}

bool Register::isTileBuffer() const
{
    return num >= 43 && num <= 47;
}

bool Register::isVertexPipelineMemory() const
{
    return num >= 48 && num <= 50;
}

bool Register::isSpecialFunctionsUnit() const
{
    return num >= 52 && num <= 55;
}

bool Register::isTextureMemoryUnit() const
{
    return num >= 56;
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
    if(num == 40 /* UNIFORM address */ || (num >= 43 && num <= 47) /* TLB setup */ || num >= 52 /* SFU, TMU write */)
        return false;
    return true;
}

bool Register::isWriteable() const
{
    return true;
}

bool Register::triggersReadOfR4() const
{
    return isSpecialFunctionsUnit();
    // TODO TMU S coordinates trigger the loading/processing of the (texture) value, but the signal  is needed to
    // load it into r4
    // || (num == 56 || num == 60) /* TMU S coordinates */;
}

bool Literal::operator==(const Literal& other) const
{
    if(this == &other)
        return true;
    // checks for bit-equality
    return u == other.u;
}

bool Literal::operator<(const Literal& other) const
{
    return u < other.u;
}

std::string Literal::to_string() const
{
    switch(type)
    {
    case LiteralType::BOOL:
        return (isTrue() ? "true" : "false");
    case LiteralType::INTEGER:
        return std::to_string(i);
    case LiteralType::REAL:
        return std::to_string(real());
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled literal type!");
}

bool Literal::isTrue() const
{
    if(type == LiteralType::BOOL || type == LiteralType::INTEGER)
        return u == 1;
    return false;
}

float Literal::real() const
{
    return f;
}

int32_t Literal::signedInt() const
{
    return i;
}

uint32_t Literal::unsignedInt() const
{
    return u;
}

uint32_t Literal::toImmediate() const
{
    return u;
}

std::string SmallImmediate::to_string() const
{
    if(value <= 15)
        // 0, ..., 15
        return (std::to_string(static_cast<int>(value)) + " (") + std::to_string(static_cast<int>(value)) + ")";
    if(value <= 31)
        // -16, ..., -1
        return (std::to_string(static_cast<int>(value) - 32) + " (") + std::to_string(static_cast<int>(value)) + ")";
    if(value <= 39)
        // 1.0, ..., 128.0
        return (std::to_string(static_cast<float>(1 << (static_cast<int>(value) - 32))) + " (") +
            std::to_string(static_cast<int>(value)) + ")";
    if(value <= 47)
        // 1/256, ..., 1/2
        return (std::to_string(1.0f / static_cast<float>(1 << (48 - static_cast<int>(value)))) + " (") +
            std::to_string(static_cast<int>(value)) + ")";
    if(value == 48)
        return "<< r5";
    if(value <= 63)
        return std::string("<< ") + std::to_string(static_cast<int>(value) - 48);
    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid small immediate value",
        std::to_string(static_cast<unsigned>(value)));
}

Optional<int32_t> SmallImmediate::getIntegerValue() const
{
    if(value <= 15)
        // 0, ..., 15
        return static_cast<int32_t>(value);
    if(value <= 31)
        // -16, ..., -1
        return static_cast<int32_t>(value) - 32;
    return {};
}

Optional<float> SmallImmediate::getFloatingValue() const
{
    if(value >= 32 && value <= 39)
        // 1.0, ..., 128.0
        return static_cast<float>(1 << (static_cast<unsigned>(value) - 32));
    if(value >= 40 && value <= 47)
        // 1/256, ..., 1/2
        return 1.0f / static_cast<float>(1 << (48 - static_cast<unsigned>(value)));
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
    if(auto intVal = getIntegerValue())
        return Literal(*intVal);
    if(auto floatVal = getFloatingValue())
        return Literal(*floatVal);
    return {};
}

Optional<SmallImmediate> SmallImmediate::fromInteger(signed char val)
{
    if(val < -16 || val > 15)
        return {};
    if(val < 0)
        return SmallImmediate(static_cast<unsigned char>(32 + val));
    return SmallImmediate(static_cast<unsigned char>(val));
}

SmallImmediate SmallImmediate::fromRotationOffset(unsigned char offset)
{
    if(offset == 0 || offset > 15)
        // Offset of 0 would result in the use of r5 register
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid vector rotation offset", std::to_string(static_cast<int>(offset)));
    return static_cast<SmallImmediate>(static_cast<unsigned char>(offset + VECTOR_ROTATE_R5));
}

bool ContainerValue::hasOnlyScalarElements() const
{
    return std::all_of(elements.begin(), elements.end(), toFunction(&Value::isLiteralValue));
}

bool ContainerValue::isAllSame() const
{
    if(elements.empty())
        return true;

    const Value singleValue = elements[0];
    for(const Value& element : elements)
    {
        if(element.isUndefined())
            // if items are UNDEFINED, ignore them, since maybe the remaining items all have the same value
            continue;
        if(element != singleValue)
            return false;
    }
    return true;
}

bool ContainerValue::isElementNumber(bool withOffset) const
{
    if(elements.empty())
        return true;
    if(std::any_of(elements.begin(), elements.end(), [](const Value& val) -> bool { return !val.getLiteralValue(); }))
        return false;
    const int32_t offset = withOffset ? elements[0].getLiteralValue()->signedInt() : 0;
    for(std::size_t i = 0; i < elements.size(); ++i)
    {
        if(elements[i].isUndefined())
            // if items are UNDEFINED, ignore them, since maybe the remaining items correspond to the element-number
            continue;
        if(elements[i].getLiteralValue()->signedInt() != static_cast<int32_t>(i) + offset)
            return false;
    }
    return true;
}

bool ContainerValue::isUndefined() const
{
    for(const Value& elem : elements)
    {
        if(!elem.isUndefined())
            return false;
    }
    return true;
}

Value::Value(const Literal& lit, const DataType& type) noexcept : data(lit), type(type) {}

Value::Value(Register reg, const DataType& type) noexcept : data(reg), type(type) {}

Value::Value(const ContainerValue& container, const DataType& type) : data(container), type(type) {}

Value::Value(const Local* local, const DataType& type) noexcept : data(const_cast<Local*>(local)), type(type) {}

Value::Value(const DataType& type) noexcept : data(VariantNamespace::monostate{}), type(type) {}

Value::Value(SmallImmediate immediate, const DataType& type) noexcept : data(immediate), type(type) {}

bool Value::operator==(const Value& other) const
{
    if(this == &other)
        return true;
    if(data.index() != other.data.index())
        return false;
    if(auto reg = checkRegister())
        return other.hasRegister(*reg);
    if(auto lit = checkLiteral())
        return other.hasLiteral(*lit);
    if(auto loc = checkLocal())
        return other.hasLocal(loc);
    if(auto container = checkContainer())
        return other.container().elements == container->elements;
    if(auto imm = checkImmediate())
        return other.hasImmediate(*imm);
    if(isUndefined())
        return other.isUndefined();
    throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

Value Value::getCompoundPart(std::size_t index) const
{
    if(auto container = checkContainer())
        return container->elements.at(index);
    if(auto loc = checkLocal())
        return Value(loc, type.getElementType());
    if(auto reg = checkRegister())
        // would only be valid for IO-registers, writing all values sequentially??
        return Value(*reg, type.getElementType());
    if(isUndefined())
        return UNDEFINED_VALUE;
    throw CompilationError(CompilationStep::GENERAL, "Can't get part of non-compound value", to_string(false));
}

bool Value::hasLocal(const Local* local) const
{
    return checkLocal() && checkPointer(this->local()) == checkPointer(local);
}

bool Value::hasRegister(Register reg) const
{
    return checkRegister() && this->reg() == reg;
}

bool Value::hasLiteral(const Literal& lit) const
{
    if(auto imm = checkImmediate())
        return imm->getIntegerValue() == lit.signedInt() || imm->getFloatingValue() == lit.real();
    return checkLiteral() && this->literal() == lit;
}

bool Value::hasImmediate(SmallImmediate immediate) const
{
    if(auto lit = checkLiteral())
        return immediate.getIntegerValue() == lit->signedInt() || immediate.getFloatingValue() == lit->real();
    return checkImmediate() && this->immediate() == immediate;
}

bool Value::isUndefined() const
{
    return VariantNamespace::holds_alternative<VariantNamespace::monostate>(data) ||
        (checkContainer() && container().isUndefined()) || (checkLocal() && local() == nullptr);
}

bool Value::isZeroInitializer() const
{
    return (checkLiteral() && literal().unsignedInt() == 0) || (checkImmediate() && immediate().value == 0) ||
        (checkContainer() &&
            std::all_of(container().elements.begin(), container().elements.end(),
                [](const Value& val) -> bool { return val.isZeroInitializer(); }));
}

bool Value::isLiteralValue() const
{
    return checkLiteral() || checkImmediate();
}

Optional<Literal> Value::getLiteralValue() const
{
    if(auto lit = checkLiteral())
        return *lit;
    if(auto imm = checkImmediate())
        return imm->toLiteral();
    return {};
}

std::string Value::to_string(const bool writeAccess, bool withLiterals) const
{
    const std::string typeName = (type.isUnknown() ? "unknown" : type.to_string()) + ' ';
    if(auto lit = checkLiteral())
        return typeName + lit->to_string();
    if(auto container = checkContainer())
    {
        if(withLiterals)
        {
            std::string tmp;
            const std::string pre = type.isVectorType() ? "<" : type.getArrayType() ? "[" : "{";
            const std::string post = type.isVectorType() ? ">" : type.getArrayType() ? "]" : "}";
            for(const Value& element : container->elements)
                tmp.append(element.to_string(writeAccess, withLiterals)).append(", ");
            return typeName + pre + tmp.substr(0, tmp.length() - 2) + post;
        }
        if(isZeroInitializer())
            return typeName + "zerointializer";
        return typeName + std::string("container with ") + std::to_string(container->elements.size()) + " elements";
    }
    if(auto loc = checkLocal())
        return withLiterals ? loc->to_string(true) : (typeName + loc->name);
    if(auto reg = checkRegister())
        return std::string("register ") + reg->to_string(true, !writeAccess);
    if(auto imm = checkImmediate())
        return typeName + imm->to_string();
    if(isUndefined())
        return typeName + "undefined";
    throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type!");
}

bool Value::isWriteable() const
{
    return checkLocal() || (checkRegister() && reg().isWriteable());
}

bool Value::isReadable() const
{
    return !(checkRegister() && !reg().isReadable());
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

const LocalUser* Value::getSingleWriter() const
{
    if(auto loc = checkLocal())
        return loc->getSingleWriter();
    return nullptr;
}

Value Value::createZeroInitializer(const DataType& type)
{
    if(type.isScalarType() || type.getPointerType())
        return INT_ZERO;
    Value val(ContainerValue(), type);
    if(type.isVectorType())
    {
        for(unsigned i = 0; i < type.getVectorWidth(); i++)
        {
            val.container().elements.push_back(INT_ZERO);
        }
    }
    else if(auto arrayType = type.getArrayType())
    {
        for(unsigned i = 0; i < arrayType->size; i++)
        {
            val.container().elements.push_back(createZeroInitializer(arrayType->elementType));
        }
    }
    else if(auto structType = type.getStructType())
    {
        for(unsigned i = 0; i < structType->elementTypes.size(); i++)
        {
            val.container().elements.push_back(createZeroInitializer(type.getElementType(i)));
        }
    }
    else
        throw CompilationError(CompilationStep::GENERAL, "Unhandled type for zero-initializer", type.to_string());
    return val;
}

std::size_t std::hash<vc4c::ContainerValue>::operator()(vc4c::ContainerValue const& val) const noexcept
{
    static const std::hash<Value> elementHash;
    return std::accumulate(val.elements.begin(), val.elements.end(), static_cast<std::size_t>(0),
        [&](std::size_t s, const Value& val) -> std::size_t { return s + elementHash(val); });
}

std::size_t std::hash<vc4c::Value>::operator()(vc4c::Value const& val) const noexcept
{
    std::hash<DataType> typeHash;
    std::hash<Variant<Literal, Register, Local*, SmallImmediate, ContainerValue, VariantNamespace::monostate>> dataHash;
    return typeHash(val.type) ^ dataHash(val.data);
}
