/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Values.h"

#include "CompilationError.h"
#include "Locals.h"
#include "SIMDVector.h"
#include "intermediate/IntermediateInstruction.h"

using namespace vc4c;

constexpr Literal tombstone_traits<Literal>::tombstone;
constexpr SmallImmediate tombstone_traits<SmallImmediate>::tombstone;

static SIMDVector ELEMENT_NUMBER_VECTOR(
    {Literal(0), Literal(1), Literal(2), Literal(3), Literal(4), Literal(5), Literal(6), Literal(7), Literal(8),
        Literal(9), Literal(10), Literal(11), Literal(12), Literal(13), Literal(14), Literal(15)});
static SIMDVector ZEROES_VECTOR(Literal(0));

const Value vc4c::ELEMENT_NUMBERS(&ELEMENT_NUMBER_VECTOR, TYPE_INT8.toVectorType(16));

bool Literal::operator==(const Literal& other) const noexcept
{
    if(this == &other)
        return true;
    if(type == LiteralType::TOMBSTONE || other.type == LiteralType::TOMBSTONE)
        // if either is tombstone, but must be tombstone
        return type == other.type;
    if(type == LiteralType::LONG_LEADING_ONES || other.type == LiteralType::LONG_LEADING_ONES)
        return type == other.type && u == other.u;
    // checks for bit-equality
    return u == other.u;
}

bool Literal::operator<(const Literal& other) const noexcept
{
    return u < other.u;
}

LCOV_EXCL_START
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
    case LiteralType::LONG_LEADING_ONES:
        return std::to_string(bit_cast<uint64_t, int64_t>(uint64_t{0xFFFFFFFF00000000} | u));
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled literal type!");
}
LCOV_EXCL_STOP

bool Literal::isTrue() const noexcept
{
    return u == 1;
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

BitMask Literal::getBitMask() const noexcept
{
    return isUndefined() ? BITMASK_ALL : BitMask{unsignedInt()};
}

Optional<Literal> vc4c::toLongLiteral(uint64_t val)
{
    auto upper = val >> 32u;
    auto lower = static_cast<uint32_t>(val & 0xFFFFFFFFu);
    if(upper == 0)
        return Literal(lower);
    if(upper == 0xFFFFFFFF)
    {
        Literal tmp(lower);
        tmp.type = LiteralType::LONG_LEADING_ONES;
        return tmp;
    }
    return {};
}

LCOV_EXCL_START
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
        return (std::to_string(static_cast<float>(1u << (static_cast<int>(value) - 32))) + " (") +
            std::to_string(static_cast<int>(value)) + ")";
    if(value <= 47)
        // 1/256, ..., 1/2
        return (std::to_string(1.0f / static_cast<float>(1u << (48 - static_cast<int>(value)))) + " (") +
            std::to_string(static_cast<int>(value)) + ")";
    if(value == 48)
        return "<< r5";
    if(value <= 63)
        return std::string("<< ") + std::to_string(static_cast<int>(value) - 48);
    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid small immediate value",
        std::to_string(static_cast<unsigned>(value)));
}
LCOV_EXCL_STOP

Optional<int32_t> SmallImmediate::getIntegerValue() const noexcept
{
    if(value <= 15)
        // 0, ..., 15
        return static_cast<int32_t>(value);
    if(value <= 31)
        // -16, ..., -1
        return static_cast<int32_t>(value) - 32;
    /*
     * The SmallImmediate values representing vector rotations also provide integer values when read.
     * I.e. "rotation by r5" (immediate 48) reads as -16, "rotation by 1" (immediate 49) as -15 and so on until
     * "rotation by 15" (immediate 63) reads as -1.
     */
    if(isVectorRotation())
        return -64 + static_cast<int32_t>(value);
    return {};
}

Optional<float> SmallImmediate::getFloatingValue() const noexcept
{
    if(value >= 32 && value <= 39)
        // 1.0, ..., 128.0
        return static_cast<float>(1u << (static_cast<unsigned>(value) - 32));
    if(value >= 40 && value <= 47)
        // 1/256, ..., 1/2
        return 1.0f / static_cast<float>(1u << (48 - static_cast<unsigned>(value)));
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

Value::Value(const Literal& lit, DataType type) noexcept : data(lit), type(type) {}

Value::Value(Register reg, DataType type) noexcept : data(reg), type(type) {}

Value::Value(const SIMDVector* vector, DataType type) : data(vector), type(type) {}

Value::Value(Local* local, DataType type) noexcept : data(local), type(type) {}

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
        return vec->getAllSame() == Literal{0u};
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
    if(auto vector = checkVector())
        return vector->isUndefined() ? UNDEFINED_LITERAL : vector->getAllSame();
    return {};
}

LCOV_EXCL_START
std::string Value::to_string(bool writeAccess, bool withLiterals) const
{
    const std::string typeName = (type.isUnknown() ? "unknown" : type.to_string()) + ' ';
    if(auto lit = checkLiteral())
        return typeName + lit->to_string();
    if(auto vec = checkVector())
        return typeName + vec->to_string(withLiterals);
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
LCOV_EXCL_STOP

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

bool Value::isUnsignedInteger() const
{
    if(!type.isIntegralType())
        return false;
    if(auto reg = checkRegister())
        return reg->isUnsignedInteger();
    if(auto lit = getLiteralValue())
        return lit->signedInt() > 0;
    if(auto local = checkLocal())
    {
        return local->residesInMemory() ||
            local->allUsers(LocalUse::Type::WRITER, [](const intermediate::IntermediateInstruction* instr) -> bool {
                return instr->hasDecoration(vc4c::intermediate::InstructionDecorations::UNSIGNED_RESULT) ||
                    intermediate::isGroupBuiltin(instr->decoration, true);
            });
    }
    if(auto vec = checkVector())
    {
        return std::all_of(vec->begin(), vec->end(), [](const Literal& lit) -> bool { return lit.signedInt() > 0; });
    }
    return false;
}

Optional<Value> Value::getConstantValue(bool transitive) const
{
    if(checkLiteral())
        return *this;
    if(checkImmediate())
        return *this;
    if(checkVector())
        return *this;
    auto reg = checkRegister();
    if(reg && (*reg == REG_ELEMENT_NUMBER || *reg == REG_QPU_NUMBER))
        return *this;
    if(auto loc = checkLocal())
        return (transitive && loc->getSingleWriter()) ? loc->getSingleWriter()->precalculate().first : NO_VALUE;
    return isUndefined() ? *this : NO_VALUE;
}

Optional<Value> Value::createZeroInitializer(DataType type)
{
    if(type.isScalarType() || type.getPointerType())
        return Value(Literal(0u), type);
    if(type.isVectorType())
    {
        return Value(&ZEROES_VECTOR, type);
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
    return NO_VALUE;
}

bool Value::isAllSame() const
{
    if(isUndefined())
        return true;
    if(VariantNamespace::get_if<Literal>(&data))
        return true;
    if(auto reg = VariantNamespace::get_if<Register>(&data))
        return *reg == REG_UNIFORM || *reg == REG_QPU_NUMBER;
    if(auto imm = VariantNamespace::get_if<SmallImmediate>(&data))
        // XXX what values do the vector rotations actually have?
        return !imm->isVectorRotation();
    if(auto vec = VariantNamespace::get_if<const SIMDVector*>(&data))
        return *vec && ((*vec)->getAllSame() || (*vec)->isUndefined());
    if(auto loc = VariantNamespace::get_if<Local*>(&data))
    {
        auto writer = (*loc)->getSingleWriter();
        return writer && writer->hasDecoration(intermediate::InstructionDecorations::IDENTICAL_ELEMENTS);
    }
    return false;
}

BitMask Value::getReadMask() const noexcept
{
    if(auto lit = getLiteralValue())
        return lit->getBitMask();
    if(auto reg = checkRegister())
        return reg->getReadMask();
    if(auto vec = checkVector())
        return vec->getBitMask();

    return BITMASK_ALL;
}

std::size_t std::hash<vc4c::Value>::operator()(vc4c::Value const& val) const noexcept
{
    // NOTE: Cannot apply hash of type here, since
    // 1) Values with same content but different type are considered equal (see operator==) and
    // 2) otherwise for FastSet or FastMap e.g. periphery-register-values are not considered equal, if they differ only
    // in type
    std::hash<decltype(val.data)> dataHash;
    return dataHash(val.data);
}
