/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegisterLoweredMemory.h"

#include "../Expression.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "logger.h"

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

RegisterCacheEntry::RegisterCacheEntry(const Value& cacheRegister, const Value& addr, DataType valueType) :
    loweredRegister(cacheRegister), byteOffset(addr), valueType(valueType)
{
    if(!cacheRegister.checkLocal() || !cacheRegister.type.isSimpleType())
        throw CompilationError(
            CompilationStep::GENERAL, "Can only cache memory in locals of simple type", cacheRegister.to_string());
}

RegisterCacheEntry::~RegisterCacheEntry() noexcept = default;

LCOV_EXCL_START
std::string RegisterCacheEntry::to_string() const
{
    return "register cache entry accessing " + valueType.to_string() + " in " + loweredRegister.to_string() +
        " at byte-offset of " + byteOffset.to_string();
}
LCOV_EXCL_STOP

static InstructionWalker insertWriteLoweredRegisterInner(
    Method& method, InstructionWalker it, const Value& src, const Value& addressOffset, const Value& loweredRegister)
{
    auto entry = std::make_shared<RegisterCacheEntry>(loweredRegister, addressOffset, src.type);
    it.emplace(std::make_unique<intermediate::CacheAccessInstruction>(MemoryOperation::WRITE, src, entry));
    it.nextInBlock();
    return it;
}

InstructionWalker periphery::insertWriteLoweredRegister(
    Method& method, InstructionWalker it, const Value& src, const Value& addressOffset, const Value& loweredRegister)
{
    return insertWriteLoweredRegisterInner(method, it, src, addressOffset, loweredRegister);
}

static InstructionWalker insertReadLoweredRegisterInner(Method& method, InstructionWalker it, const Value& dest,
    const Value& addressOffset, const Value& loweredRegister, DataType destType)
{
    auto entry = std::make_shared<RegisterCacheEntry>(loweredRegister, addressOffset, destType);
    it.emplace(std::make_unique<intermediate::CacheAccessInstruction>(MemoryOperation::READ, dest, entry));
    it.nextInBlock();
    return it;
}

InstructionWalker periphery::insertReadLoweredRegister(
    Method& method, InstructionWalker it, const Value& dest, const Value& addressOffset, const Value& loweredRegister)
{
    if(auto destData = Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
    {
        it = insertReadLoweredRegisterInner(
            method, it, destData->lower->createReference(), addressOffset, loweredRegister, dest.type);
        auto highAddressOffset = assign(it, addressOffset.type) = addressOffset + 4_val;
        return insertReadLoweredRegisterInner(
            method, it, destData->upper->createReference(), highAddressOffset, loweredRegister, dest.type);
    }
    return insertReadLoweredRegisterInner(method, it, dest, addressOffset, loweredRegister, dest.type);
}

static Value precalculateOffset(const Value& byteOffset)
{
    if(auto constant = byteOffset.getConstantValue())
        return *constant;
    auto writer = byteOffset.getSingleWriter();
    auto expr = writer ? Expression::createRecursiveExpression(*writer) : nullptr;
    if(auto constant = expr ? expr->getConstantExpression() : NO_VALUE)
        return *constant;
    return byteOffset;
}

static NODISCARD InstructionWalker insertByteToElementAndSubOffset(
    InstructionWalker it, Value& outElementOffset, Value& outSubOffset, const RegisterCacheEntry& entry)
{
    unsigned elementTypeSize = 0;
    auto containerType = entry.loweredRegister.type;
    if(containerType.isSimpleType() || containerType.getArrayType())
        // default case for storing scalar values, vectors or arrays of scalar value
        elementTypeSize = containerType.getElementType().getInMemoryWidth();
    else if(containerType.getPointerType())
        // special case for e.g. storing pointer values
        elementTypeSize = TYPE_VOID_POINTER.getInMemoryWidth();
    else
        throw CompilationError(CompilationStep::NORMALIZER,
            "Failed to calculate element type width for register-lowered allocation", containerType.to_string());
    auto byteOffset = precalculateOffset(entry.byteOffset);
    outElementOffset = assign(it, TYPE_INT8, "%element_offset") = byteOffset / Literal(elementTypeSize);
    outSubOffset = assign(it, TYPE_INT8, "%sub_offset") = byteOffset % Literal(elementTypeSize);
    return it;
}

static DataType convertLargeContainerSmallDataType(
    DataType containerType, DataType valueType, bool addSingleElement = false)
{
    // create a vector with the same element type as the container but the same total bit-width as the value type
    auto factor = containerType.getScalarBitCount() / valueType.getScalarBitCount();
    // we need float conversions, since there can be vector-width which are not multiples of 2 (e.g. 3)
    auto vectorWidth = static_cast<float>(valueType.getVectorWidth()) / static_cast<float>(factor);
    auto realVectorWidth = static_cast<uint8_t>(std::ceil(vectorWidth) + (addSingleElement ? 1u : 0u));
    return containerType.getElementType().toVectorType(realVectorWidth);
}

static DataType convertSmallContainerLargeDataType(
    DataType containerType, DataType valueType, bool addSingleElement = false)
{
    // create a vector with the same element type as the container but the same total bit-width as the value type
    auto factor = valueType.getScalarBitCount() / containerType.getScalarBitCount();
    // we need float conversions, since there can be vector-width which are not multiples of 2 (e.g. 3)
    auto vectorWidth = static_cast<float>(valueType.getVectorWidth()) * static_cast<float>(factor);
    auto realVectorWidth = static_cast<uint8_t>(std::ceil(vectorWidth) + (addSingleElement ? 1u : 0u));
    return containerType.getElementType().toVectorType(realVectorWidth);
}

static NODISCARD InstructionWalker lowerRegisterRead(Method& method, InstructionWalker it,
    const CacheAccessInstruction& readInstruction, const RegisterCacheEntry& entry)
{
    auto elementOffset = UNDEFINED_VALUE;
    auto subOffset = UNDEFINED_VALUE;
    it = insertByteToElementAndSubOffset(it, elementOffset, subOffset, entry);

    const auto& dest = readInstruction.getData();
    auto containerType = entry.loweredRegister.type;
    if(entry.valueType.getScalarBitCount() == containerType.getScalarBitCount())
        // simple vector extraction
        it = insertVectorExtraction(it, method, entry.loweredRegister, elementOffset, dest);
    else if(entry.valueType.getScalarBitCount() < containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert simple vector extraction and bit-cast
        // create a vector with the same element type as the container but the same total bit-width as the output type
        auto tmpValue = method.addNewLocal(convertLargeContainerSmallDataType(containerType, entry.valueType));
        it = insertVectorExtraction(it, method, entry.loweredRegister, elementOffset, tmpValue);
        it = insertBitcast(it, method, tmpValue, dest);
    }
    else if(entry.valueType.getScalarBitCount() < containerType.getScalarBitCount() &&
        entry.valueType.getVectorWidth() <=
            (NATIVE_VECTOR_SIZE - containerType.getScalarBitCount() / entry.valueType.getScalarBitCount()))
    {
        /*
         * If we were to read e.g. 16 chars from an int16 container from an arbitrary in-element sub-offset, we would
         * need to read some more elements into a second vector and then combine them, since we might drop a few entries
         * at the beginning and still need to have in the end 16 elements. So exclude this more complex case here.
         */
        auto tmpInput = method.addNewLocal(convertLargeContainerSmallDataType(containerType, entry.valueType, true));
        it = insertVectorExtraction(it, method, entry.loweredRegister, elementOffset, tmpInput);
        // for intN container and shortM elements, we can have an sub-offset of 1 short, thus we need to extract M + 1
        // shorts, for charM elements, we can have up to 3 chars as sub-offset, so need to extract M + 3 chars
        auto maxSubOffset = (containerType.getScalarBitCount() / entry.valueType.getScalarBitCount()) - 1u;
        if(auto literalSubOffset = subOffset.getLiteralValue())
            // if we have a static sub-offset, we can reduce the numbers of elements required to be extracted further
            maxSubOffset = std::min(maxSubOffset, literalSubOffset->unsignedInt());
        auto vectorWidth = static_cast<uint8_t>(entry.valueType.getVectorWidth() + maxSubOffset);
        auto tmpOutput = method.addNewLocal(entry.valueType.toVectorType(vectorWidth));
        // TODO we could save a lot of instructions (at least for the constant sub-offset case) if we can forward the
        // sub-offset to the bit-cast, e.g. for a sub-offset of 2 bytes the first 2 bytes of the first element don't
        // need to be extracted at all. Since we vector-rotate them to the upper elements, it is harder to reason later,
        // that we do not care about them, unless we hard-enforce the vector-width as number of used elements...
        it = insertBitcast(it, method, tmpInput, tmpOutput);
        auto elementOffset = assign(it, subOffset.type) =
            subOffset / Literal(entry.valueType.getElementType().getLogicalWidth());
        it = insertVectorRotation(it, tmpOutput, elementOffset, dest, Direction::DOWN);
    }
    else if(entry.valueType.getScalarBitCount() > containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert simple vector extraction and bit-cast
        // create a vector with the same element type as the container but the same total bit-width as the output type
        auto tmpValue = method.addNewLocal(convertSmallContainerLargeDataType(containerType, entry.valueType));
        it = insertVectorExtraction(it, method, entry.loweredRegister, elementOffset, tmpValue);
        it = insertBitcast(it, method, tmpValue, dest, InstructionDecorations::NONE,
            entry.valueType.getScalarBitCount() > 32 ? 2u : 1u);
    }
    else
    {
        logging::error() << "Unhandled lowering of reading from register-cached memory: " << readInstruction.to_string()
                         << " (offsets " << elementOffset.to_string() << " and " << subOffset.to_string() << ')'
                         << logging::endl;
        throw CompilationError(CompilationStep::GENERAL,
            "Reading register-lowered memory with data of larger type is not yet implemented",
            readInstruction.to_string());
    }
    it.erase();
    return it;
}

static NODISCARD InstructionWalker lowerRegisterWrite(Method& method, InstructionWalker it,
    const CacheAccessInstruction& writeInstruction, const RegisterCacheEntry& entry)
{
    auto elementOffset = UNDEFINED_VALUE;
    auto subOffset = UNDEFINED_VALUE;
    it = insertByteToElementAndSubOffset(it, elementOffset, subOffset, entry);

    auto src = writeInstruction.getData();
    // restore original type, to e.g. for insertion of zero not insert "i8 0", but "i32 0"
    src.type = entry.valueType;

    auto containerType = entry.loweredRegister.type;
    if(src.type.getScalarBitCount() == containerType.getScalarBitCount())
        // simple vector insertion
        it = insertVectorInsertion(it, method, entry.loweredRegister, elementOffset, src);
    else if(src.type.getScalarBitCount() < containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert bit-cast and simple vector insertion
        auto tmpValue = method.addNewLocal(convertLargeContainerSmallDataType(containerType, src.type));
        it = insertBitcast(it, method, src, tmpValue);
        it = insertVectorInsertion(it, method, entry.loweredRegister, elementOffset, tmpValue);
    }
    else
    {
        logging::error() << "Unhandled lowering of writing into register-cached memory: "
                         << writeInstruction.to_string() << " (offsets " << elementOffset.to_string() << " and "
                         << subOffset.to_string() << ')' << logging::endl;
        throw CompilationError(CompilationStep::GENERAL,
            "Writing register-lowered memory with data of different type is not yet implemented",
            writeInstruction.to_string());
    }
    it.erase();
    return it;
}

InstructionWalker periphery::lowerRegisterAccess(Method& method, InstructionWalker it)
{
    auto accessInstruction = it.get<CacheAccessInstruction>();
    auto cacheEntry = (check(accessInstruction) & &MemoryAccessInstruction::getRegisterCacheEntry).value_or(nullptr);

    if(!cacheEntry)
        return it;

    if(accessInstruction->op == MemoryOperation::READ)
        return lowerRegisterRead(method, it, *accessInstruction, *cacheEntry);
    if(accessInstruction->op == MemoryOperation::WRITE)
        return lowerRegisterWrite(method, it, *accessInstruction, *cacheEntry);

    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled access to lowered register", accessInstruction->to_string());
}
