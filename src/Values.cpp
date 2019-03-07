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

const Value vc4c::ELEMENT_NUMBERS(
    SIMDVector({Literal(0), Literal(1), Literal(2), Literal(3), Literal(4), Literal(5), Literal(6), Literal(7),
        Literal(8), Literal(9), Literal(10), Literal(11), Literal(12), Literal(13), Literal(14), Literal(15)}),
    TYPE_INT8.toVectorType(16));

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

bool vc4c::isFixed(const RegisterFile file) noexcept
{
    return file == RegisterFile::ACCUMULATOR || file == RegisterFile::PHYSICAL_A || file == RegisterFile::PHYSICAL_B;
}

bool Register::isGeneralPurpose() const noexcept
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

int Register::getAccumulatorNumber() const noexcept
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

bool Register::operator<(Register right) const noexcept
{
    const auto tmp = static_cast<unsigned char>(file) < static_cast<unsigned char>(right.file);
    return tmp || num < right.num;
}

bool Register::operator>(Register right) const noexcept
{
    const auto tmp = static_cast<unsigned char>(file) > static_cast<unsigned char>(right.file);
    return tmp || num > right.num;
}

bool Register::operator==(Register right) const noexcept
{
    if(this == &right)
        return true;
    return num == right.num && file == right.file;
}

bool Register::isAccumulator() const noexcept
{
    return (num >= 32 && num <= 37) || file == RegisterFile::ACCUMULATOR;
}

bool Register::isTileBuffer() const noexcept
{
    return num >= 43 && num <= 47;
}

bool Register::isVertexPipelineMemory() const noexcept
{
    return num >= 48 && num <= 50;
}

bool Register::isSpecialFunctionsUnit() const noexcept
{
    return num >= 52 && num <= 55;
}

bool Register::isTextureMemoryUnit() const noexcept
{
    return num >= 56;
}

bool Register::hasSideEffectsOnRead() const noexcept
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

bool Register::hasSideEffectsOnWrite() const noexcept
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

bool Register::isReadable() const noexcept
{
    if(num == 40 /* UNIFORM address */ || (num >= 43 && num <= 47) /* TLB setup */ || num >= 52 /* SFU, TMU write */)
        return false;
    return true;
}

bool Register::isWriteable() const noexcept
{
    return true;
}

bool Register::triggersReadOfR4() const noexcept
{
    return isSpecialFunctionsUnit();
    // TODO TMU S coordinates trigger the loading/processing of the (texture) value, but the signal  is needed to
    // load it into r4
    // || (num == 56 || num == 60) /* TMU S coordinates */;
}

bool Literal::operator==(const Literal& other) const noexcept
{
    if(this == &other)
        return true;
    if(type == LiteralType::TOMBSTONE)
        return other.type == LiteralType::TOMBSTONE;
    // checks for bit-equality
    return u == other.u;
}

bool Literal::operator<(const Literal& other) const noexcept
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
    case LiteralType::TOMBSTONE:
        return "undefined";
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled literal type!");
}

bool Literal::isTrue() const noexcept
{
    if(type == LiteralType::BOOL || type == LiteralType::INTEGER)
        return u == 1;
    return false;
}

float Literal::real() const noexcept
{
    return f;
}

int32_t Literal::signedInt() const noexcept
{
    return i;
}

uint32_t Literal::unsignedInt() const noexcept
{
    return u;
}

uint32_t Literal::toImmediate() const noexcept
{
    return u;
}

bool Literal::isUndefined() const noexcept
{
    return type == LiteralType::TOMBSTONE;
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

Optional<int32_t> SmallImmediate::getIntegerValue() const noexcept
{
    if(value <= 15)
        // 0, ..., 15
        return static_cast<int32_t>(value);
    if(value <= 31)
        // -16, ..., -1
        return static_cast<int32_t>(value) - 32;
    return {};
}

Optional<float> SmallImmediate::getFloatingValue() const noexcept
{
    if(value >= 32 && value <= 39)
        // 1.0, ..., 128.0
        return static_cast<float>(1 << (static_cast<unsigned>(value) - 32));
    if(value >= 40 && value <= 47)
        // 1/256, ..., 1/2
        return 1.0f / static_cast<float>(1 << (48 - static_cast<unsigned>(value)));
    return {};
}

bool SmallImmediate::isVectorRotation() const noexcept
{
    return value >= 48 && value <= 63;
}

Optional<unsigned char> SmallImmediate::getRotationOffset() const noexcept
{
    if(!isVectorRotation())
        return {};
    if(*this == VECTOR_ROTATE_R5)
        return {};
    return static_cast<unsigned char>(value - VECTOR_ROTATE_R5.value);
}

Optional<Literal> SmallImmediate::toLiteral() const noexcept
{
    if(auto intVal = getIntegerValue())
        return Literal(*intVal);
    if(auto floatVal = getFloatingValue())
        return Literal(*floatVal);
    return {};
}

Optional<SmallImmediate> SmallImmediate::fromInteger(signed char val) noexcept
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

bool SIMDVector::isAllSame() const noexcept
{
    Literal firstElement = elements[0];
    return std::all_of(elements.begin(), elements.end(),
        // if items are UNDEFINED, ignore them, since maybe the remaining items all have the same value
        [=](Literal lit) -> bool { return lit.isUndefined() || lit == firstElement; });
}

bool SIMDVector::isElementNumber(bool withOffset) const noexcept
{
    const int32_t offset = withOffset ? elements[0].signedInt() : 0;
    for(std::size_t i = 0; i < elements.size(); ++i)
    {
        if(elements[i].signedInt() != static_cast<int32_t>(i) + offset)
            return false;
    }
    return true;
}

bool SIMDVector::isUndefined() const
{
    return std::all_of(elements.begin(), elements.end(), [](Literal lit) -> bool { return lit.isUndefined(); });
}

void SIMDVector::forAllElements(const std::function<void(Literal)>& consumer) const
{
    std::for_each(elements.begin(), elements.end(), consumer);
}

SIMDVector SIMDVector::transform(const std::function<Literal(Literal)>& transformOp) const&
{
    SIMDVector copy;
    for(unsigned i = 0; i < elements.size(); ++i)
    {
        copy.elements[i] = transformOp(elements[i]);
    }
    return copy;
}

SIMDVector SIMDVector::transform(const std::function<Literal(Literal)>& transformOp) &&
{
    std::transform(elements.begin(), elements.end(), elements.begin(), transformOp);
    return std::move(*this);
}

SIMDVector SIMDVector::rotate(uint8_t offset) const&
{
    SIMDVector copy(*this);
    //"Rotates the order of the elements in the range [first,last), in such a way that the element pointed by middle
    // becomes the new first element."
    offset = (NATIVE_VECTOR_SIZE - offset);
    std::rotate(copy.begin(), copy.begin() + offset, copy.end());
    return copy;
}

SIMDVector SIMDVector::rotate(uint8_t offset) &&
{
    //"Rotates the order of the elements in the range [first,last), in such a way that the element pointed by middle
    // becomes the new first element."
    offset = (NATIVE_VECTOR_SIZE - offset);
    std::rotate(begin(), begin() + offset, end());
    return std::move(*this);
}

Value::Value(const Literal& lit, DataType type) noexcept : data(lit), type(type) {}

Value::Value(Register reg, DataType type) noexcept : data(reg), type(type) {}

Value::Value(SIMDVector&& vector, DataType type) : data(std::move(vector)), type(type) {}

Value::Value(const Local* local, DataType type) noexcept : data(const_cast<Local*>(local)), type(type) {}

Value::Value(DataType type) noexcept : data(VariantNamespace::monostate{}), type(type) {}

Value::Value(SmallImmediate immediate, DataType type) noexcept : data(immediate), type(type) {}

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
    if(auto vec = checkVector())
        return other.vector() == *vec;
    if(auto imm = checkImmediate())
        return other.hasImmediate(*imm);
    if(isUndefined())
        return other.isUndefined();
    throw CompilationError(CompilationStep::GENERAL, "Unhandled value-type", to_string());
}

Value Value::getCompoundPart(std::size_t index) const
{
    if(auto vec = checkVector())
        return Value(vec->at(index), type.toVectorType(1));
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
    if(auto lit = checkLiteral())
    {
        return lit->isUndefined();
    }
    if(VariantNamespace::holds_alternative<Local*>(data))
    {
        return VariantNamespace::get<Local*>(data) == nullptr;
    }
    if(VariantNamespace::holds_alternative<VariantNamespace::monostate>(data))
    {
        return true;
    }
    if(auto vector = checkVector())
    {
        return vector->isUndefined();
    }
    return false;
}

bool Value::isZeroInitializer() const
{
    if(auto vec = checkVector())
    {
        return vec->at(0).unsignedInt() == 0 && vec->isAllSame();
    }
    if(auto lit = getLiteralValue())
    {
        return lit->unsignedInt() == 0;
    }
    return false;
}

bool Value::isLiteralValue() const noexcept
{
    return checkLiteral() || checkImmediate();
}

Optional<Literal> Value::getLiteralValue() const noexcept
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
    if(auto vec = checkVector())
    {
        if(withLiterals)
        {
            std::string tmp;
            for(const auto& lit : *vec)
                tmp.append(lit.to_string()).append(", ");
            return typeName + "<" + tmp.substr(0, tmp.length() - 2) + ">";
        }
        if(isZeroInitializer())
            return typeName + "zerointializer";
        return typeName + "SIMD vector";
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

Value Value::createZeroInitializer(DataType type)
{
    if(type.isScalarType() || type.getPointerType())
        return INT_ZERO;
    if(type.isVectorType())
    {
        return Value(SIMDVector(Literal{0u}), type);
    }
    // TODO do we ever have to create array- and struct zero initializers ??
    // if(auto arrayType = type.getArrayType())
    // {
    //     ContainerValue val(arrayType->size);
    //     for(unsigned i = 0; i < arrayType->size; i++)
    //     {
    //         val.elements.push_back(createZeroInitializer(arrayType->elementType));
    //     }
    //     return Value(std::move(val), type);
    // }
    // if(auto structType = type.getStructType())
    // {
    //     ContainerValue val(structType->elementTypes.size());
    //     for(unsigned i = 0; i < structType->elementTypes.size(); i++)
    //     {
    //         val.elements.push_back(createZeroInitializer(type.getElementType(static_cast<int>(i))));
    //     }
    //     return Value(std::move(val), type);
    // }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled type for zero-initializer", type.to_string());
}

std::size_t std::hash<vc4c::SIMDVector>::operator()(vc4c::SIMDVector const& val) const noexcept
{
    static const std::hash<Literal> elementHash;
    return std::accumulate(val.begin(), val.end(), static_cast<std::size_t>(0),
        [&](std::size_t s, const Literal& val) -> std::size_t { return s + elementHash(val); });
}

std::size_t std::hash<vc4c::Value>::operator()(vc4c::Value const& val) const noexcept
{
    std::hash<DataType> typeHash;
    std::hash<Variant<Literal, Register, Local*, SmallImmediate, SIMDVector, VariantNamespace::monostate>> dataHash;
    return typeHash(val.type) ^ dataHash(val.data);
}
