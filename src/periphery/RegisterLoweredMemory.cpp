/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegisterLoweredMemory.h"

#include "../Expression.h"
#include "../SIMDVector.h"
#include "../analysis/MemoryAnalysis.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../normalization/LiteralValues.h"
#include "../normalization/LongOperations.h"
#include "logger.h"

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

RegisterCacheEntry::RegisterCacheEntry(
    const Value& cacheRegister, const Value& addr, DataType valueType, MultiPartDataAccess valueAccess) :
    loweredRegister(cacheRegister),
    byteOffset(addr), valueType(valueType), valuePartAccess(valueAccess)
{
    if(!cacheRegister.checkLocal() || !cacheRegister.type.isSimpleType())
        throw CompilationError(
            CompilationStep::GENERAL, "Can only cache memory in locals of simple type", cacheRegister.to_string());
}

RegisterCacheEntry::~RegisterCacheEntry() noexcept = default;

LCOV_EXCL_START
std::string RegisterCacheEntry::to_string() const
{
    std::string part = valuePartAccess == MultiPartDataAccess::LOWER_PART ?
        "lower part of " :
        (valuePartAccess == MultiPartDataAccess::UPPER_PART ? "upper part of " : "");
    return "register cache entry accessing " + part + valueType.to_string() + " in " + loweredRegister.to_string() +
        " at byte-offset of " + byteOffset.to_string();
}
LCOV_EXCL_STOP

Value RegisterCacheEntry::precalculateOffset() const
{
    if(auto constant = byteOffset.getConstantValue())
        return *constant;
    auto writer = byteOffset.getSingleWriter();
    auto expr = writer ? Expression::createRecursiveExpression(*writer) : nullptr;
    if(auto constant = expr ? expr->getConstantExpression() : NO_VALUE)
        return *constant;
    return byteOffset;
}

static InstructionWalker insertWriteLoweredRegisterInner(Method& method, InstructionWalker it, const Value& src,
    const Value& addressOffset, const Value& loweredRegister, DataType srcType,
    MultiPartDataAccess partAccess = MultiPartDataAccess::SINGLE_PART)
{
    auto entry = std::make_shared<RegisterCacheEntry>(loweredRegister, addressOffset, srcType, partAccess);
    it.emplace(std::make_unique<intermediate::CacheAccessInstruction>(MemoryOperation::WRITE, src, entry));
    it.nextInBlock();
    return it;
}

InstructionWalker periphery::insertWriteLoweredRegister(
    Method& method, InstructionWalker it, const Value& src, const Value& addressOffset, const Value& loweredRegister)
{
    if(auto srcData = Local::getLocalData<MultiRegisterData>(src.checkLocal()))
    {
        it = insertWriteLoweredRegisterInner(method, it, srcData->lower->createReference(), addressOffset,
            loweredRegister, src.type, MultiPartDataAccess::LOWER_PART);
        auto highAddressOffset = assign(it, addressOffset.type) = addressOffset + 4_val;
        return insertWriteLoweredRegisterInner(method, it, srcData->upper->createReference(), highAddressOffset,
            loweredRegister, src.type, MultiPartDataAccess::UPPER_PART);
    }
    return insertWriteLoweredRegisterInner(method, it, src, addressOffset, loweredRegister, src.type);
}

static InstructionWalker insertReadLoweredRegisterInner(Method& method, InstructionWalker it, const Value& dest,
    const Value& addressOffset, const Value& loweredRegister, DataType destType,
    MultiPartDataAccess partAccess = MultiPartDataAccess::SINGLE_PART)
{
    auto entry = std::make_shared<RegisterCacheEntry>(loweredRegister, addressOffset, destType, partAccess);
    it.emplace(std::make_unique<intermediate::CacheAccessInstruction>(MemoryOperation::READ, dest, entry));
    it.nextInBlock();
    return it;
}

InstructionWalker periphery::insertReadLoweredRegister(
    Method& method, InstructionWalker it, const Value& dest, const Value& addressOffset, const Value& loweredRegister)
{
    if(auto destData = Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
    {
        it = insertReadLoweredRegisterInner(method, it, destData->lower->createReference(), addressOffset,
            loweredRegister, dest.type, MultiPartDataAccess::LOWER_PART);
        auto highAddressOffset = assign(it, addressOffset.type) = addressOffset + 4_val;
        return insertReadLoweredRegisterInner(method, it, destData->upper->createReference(), highAddressOffset,
            loweredRegister, dest.type, MultiPartDataAccess::UPPER_PART);
    }
    return insertReadLoweredRegisterInner(method, it, dest, addressOffset, loweredRegister, dest.type);
}

InstructionWalker periphery::insertFillLoweredRegister(Method& method, InstructionWalker it, const Value& src,
    const Value& addressOffset, const Value& numCopies, const Value& loweredRegister)
{
    // NOTE: In contrast to READ/WRITE, we directly insert the code for FILL due to a) cache access not supporting
    // multi-element access and b) we currently can't optimize (e.g. combine) FILLs anyway. XXX Should be unified at
    // some point!
    auto staticAddressOffset = addressOffset.getConstantValue() & &Value::getLiteralValue;
    if(!staticAddressOffset)
    {
        auto writer = analysis::getSingleWriter(addressOffset);
        auto expr = writer ? Expression::createRecursiveExpression(*writer) : nullptr;
        staticAddressOffset = expr ? expr->getConstantExpression() & &Value::getLiteralValue : Optional<Literal>{};
    }
    auto staticNumCopies = numCopies.getConstantValue() & &Value::getLiteralValue;
    if(!staticNumCopies)
    {
        auto writer = analysis::getSingleWriter(numCopies);
        auto expr = writer ? Expression::createRecursiveExpression(*writer) : nullptr;
        staticNumCopies = expr ? expr->getConstantExpression() & &Value::getLiteralValue : Optional<Literal>{};
    }
    auto loweredRegisterElementWidth = loweredRegister.type.getElementType().getLogicalWidth();
    if(src.type.isScalarType() && src.type.getScalarBitCount() == loweredRegister.type.getScalarBitCount())
    {
        if(staticAddressOffset == 0_lit && staticNumCopies == Literal(loweredRegister.type.getVectorWidth()))
        {
            // we fill the whole vector
            return insertReplication(it, src, loweredRegister);
        }

        auto replicatedValue = method.addNewLocal(
            src.type.toVectorType(static_cast<uint8_t>(
                staticNumCopies.value_or(Literal(static_cast<uint32_t>(NATIVE_VECTOR_SIZE))).unsignedInt())),
            "%fill_register_splat");
        it = insertReplication(it, src, replicatedValue);
        Value elementOffset = UNDEFINED_VALUE;
        if(staticAddressOffset)
            elementOffset = Value(Literal(staticAddressOffset->unsignedInt() / loweredRegisterElementWidth), TYPE_INT8);
        else
            elementOffset = assign(it, TYPE_INT8, "%fill_register_offset") =
                addressOffset / Literal(loweredRegisterElementWidth);
        return insertVectorInsertion(it, method, loweredRegister, elementOffset, replicatedValue, numCopies);
    }
    if(src.type.isScalarType() && src.type.getScalarBitCount() < loweredRegister.type.getScalarBitCount())
    {
        if(staticAddressOffset && staticAddressOffset->unsignedInt() % loweredRegisterElementWidth == 0 &&
            staticNumCopies &&
            staticNumCopies->unsignedInt() % (loweredRegisterElementWidth / src.type.getLogicalWidth()) == 0)
        {
            // the inserted data size is a multiple of the lowered register element width and aligned to the lowered
            // register element alignment, thus we can replicate the value across a single lowered register and then
            // treat identical to above (for filling same-width elements)
            Value replicatedElement = UNDEFINED_VALUE;
            if(auto srcLiteral = src.getConstantValue() & &Value::getLiteralValue)
            {
                uint64_t value = srcLiteral->unsignedInt();
                for(uint8_t typeWidth = src.type.getScalarBitCount();
                    typeWidth < loweredRegister.type.getScalarBitCount(); typeWidth *= 2u)
                {
                    auto lower = value & ((uint64_t{1} << typeWidth) - 1u);
                    value = lower | (lower << typeWidth);
                }
                if(auto lit = toLongLiteral(value))
                    replicatedElement = Value(*lit, loweredRegister.type.getElementType());
                else
                {
                    replicatedElement = method.addNewLocal(loweredRegister.type.getElementType());
                    it = normalization::insertLongLoad(it, method, *replicatedElement.local(), value);
                }
            }
            else
            {
                replicatedElement = src;
                for(uint8_t typeWidth = src.type.getScalarBitCount();
                    typeWidth < loweredRegister.type.getScalarBitCount(); typeWidth *= 2u)
                {
                    auto lower = assign(it, loweredRegister.type.getElementType()) =
                        replicatedElement & Value(Literal((1u << typeWidth) - 1u), TYPE_INT32);
                    auto upper = assign(it, loweredRegister.type.getElementType()) = lower
                        << Value(Literal(typeWidth), TYPE_INT8);
                    replicatedElement = assign(it, loweredRegister.type.getElementType(), "%fill_register_replicated") =
                        lower | upper;
                }
            }
            auto numResizedCopies =
                staticNumCopies->unsignedInt() / (loweredRegisterElementWidth / src.type.getLogicalWidth());
            if(staticAddressOffset == 0_lit && numResizedCopies >= loweredRegister.type.getVectorWidth() &&
                replicatedElement.getLiteralValue())
            {
                // simply load the replicated value in all vector elements
                assign(it, loweredRegister) = load(replicatedElement.getLiteralValue().value());
                return it;
            }
            return insertFillLoweredRegister(method, it, replicatedElement, addressOffset,
                Value(Literal(numResizedCopies), TYPE_INT8), loweredRegister);
        }
    }

    logging::error() << "Unimplemented filling of " << numCopies.to_string() << " copies of " << src.to_string()
                     << " into " << loweredRegister.to_string() << " at offset " << addressOffset.to_string()
                     << logging::endl;
    throw CompilationError(
        CompilationStep::NORMALIZER, "General case for filling register-lowered memory is not yet implemented");
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
    auto byteOffset = entry.precalculateOffset();
    if(elementTypeSize == 1)
    {
        outElementOffset = assign(it, TYPE_INT8, "%element_offset") = byteOffset;
        outSubOffset = 0_val;
    }
    else
    {
        outElementOffset = assign(it, TYPE_INT8, "%element_offset") = byteOffset / Literal(elementTypeSize);
        outSubOffset = assign(it, TYPE_INT8, "%sub_offset") = byteOffset % Literal(elementTypeSize);
    }
    return it;
}

static NODISCARD InstructionWalker insertByteToElementAndSubOffsetAndSelectedPart(InstructionWalker it,
    Value& outElementOffset, Value& outSubOffset, Value& outSelectedPart, const RegisterCacheEntry& entry,
    const MultiRegisterData& loweredRegisters, bool isRead)
{
    auto byteOffset = entry.precalculateOffset();

    /*
     * Byte offset in 64-bit 2 register) container:
     *
     * - byte-offset 0 -> lower part, element 0, sub-element 0
     * - byte-offset 4 -> upper part, element 0, sub-element 0
     * - byte-offset 7 -> upper part, element 0, sub-element 3
     * - byte-offset 9 -> lower part, element 1, sub-element 1
     * - byte offset 13 -> upper part, element 2, sub-element 1
     *
     * => byte-offset / 4 % 2 ? lower part : upper part
     * => byte-offset / 8 -> element
     * => byte-offset % 4 -> sub-element offset
     */
    if(auto constantOffset = byteOffset.getLiteralValue())
    {
        outSelectedPart = ((constantOffset->unsignedInt() / 4) % 2 ? loweredRegisters.upper : loweredRegisters.lower)
                              ->createReference();
        outElementOffset = Value(Literal(constantOffset->unsignedInt() / 8), TYPE_INT8);
        outSubOffset = Value(Literal(constantOffset->unsignedInt() % 4), TYPE_INT8);
    }
    else if(isRead && entry.valueType.isScalarType())
    {
        outSelectedPart = assign(it, loweredRegisters.lower->type) = loweredRegisters.lower->createReference();
        auto tmp = assign(it, TYPE_INT8) = byteOffset / 4_lit;
        assignNop(it) = (tmp % 2_lit, SetFlag::SET_FLAGS);
        assign(it, outSelectedPart) = (loweredRegisters.upper->createReference(), COND_ZERO_CLEAR);
        outElementOffset = assign(it, TYPE_INT8) = byteOffset / 8_lit;
        outSubOffset = assign(it, TYPE_INT8) = byteOffset % 4_lit;
    }
    else if(entry.valuePartAccess == MultiPartDataAccess::LOWER_PART)
    {
        // accessing a whole lower data part in a 64-bit multi-part lowered register is always correctly aligned to the
        // part bounds
        outSelectedPart = loweredRegisters.lower->createReference();
        outElementOffset = assign(it, TYPE_INT8) = byteOffset / 8_lit;
        outSubOffset = assign(it, TYPE_INT8) = byteOffset % 4_lit;
    }
    else if(entry.valuePartAccess == MultiPartDataAccess::UPPER_PART)
    {
        // accessing a whole upper data part in a 64-bit multi-part lowered register is always correctly aligned to the
        // part bounds
        outSelectedPart = loweredRegisters.upper->createReference();
        auto removedOffset = assign(it, TYPE_INT8) = byteOffset - 4_val;
        outElementOffset = assign(it, TYPE_INT8) = removedOffset / 8_lit;
        outSubOffset = assign(it, TYPE_INT8) = removedOffset % 4_lit;
    }
    else
        throw CompilationError(CompilationStep::NORMALIZER,
            "General calculation of multi-register container access parts is not yet implemented");
    // TODO the part returned here is only for the element (part of the first) access!
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
    auto selectedPart = entry.loweredRegister;
    if(auto multiParts = Local::getLocalData<MultiRegisterData>(entry.loweredRegister.checkLocal()))
        it = insertByteToElementAndSubOffsetAndSelectedPart(
            it, elementOffset, subOffset, selectedPart, entry, *multiParts, true);
    else
        it = insertByteToElementAndSubOffset(it, elementOffset, subOffset, entry);

    const auto& dest = readInstruction.getData();
    auto containerType = entry.loweredRegister.type;
    if(entry.valueType.getScalarBitCount() == containerType.getScalarBitCount())
        // simple vector extraction
        it = insertVectorExtraction(it, method, selectedPart, elementOffset, dest);
    else if(entry.valueType.getScalarBitCount() < containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert simple vector extraction and bit-cast
        // create a vector with the same element type as the container but the same total bit-width as the output type
        auto tmpValue = method.addNewLocal(
            convertLargeContainerSmallDataType(containerType, entry.valueType), "%load_register_tmp");
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
        auto tmpInput = method.addNewLocal(
            convertLargeContainerSmallDataType(selectedPart.type, entry.valueType, true), "%load_register_input");
        it = insertVectorExtraction(it, method, selectedPart, elementOffset, tmpInput);
        // for intN container and shortM elements, we can have an sub-offset of 1 short, thus we need to extract M + 1
        // shorts, for charM elements, we can have up to 3 chars as sub-offset, so need to extract M + 3 chars
        auto maxSubOffset = (containerType.getScalarBitCount() / entry.valueType.getScalarBitCount()) - 1u;
        if(auto literalSubOffset = subOffset.getLiteralValue())
            // if we have a static sub-offset, we can reduce the numbers of elements required to be extracted further
            maxSubOffset = std::min(maxSubOffset, literalSubOffset->unsignedInt());
        auto vectorWidth = static_cast<uint8_t>(entry.valueType.getVectorWidth() + maxSubOffset);
        auto tmpOutput = method.addNewLocal(entry.valueType.toVectorType(vectorWidth), "%load_register_output");
        // TODO we could save a lot of instructions (at least for the constant sub-offset case) if we can forward the
        // sub-offset to the bit-cast, e.g. for a sub-offset of 2 bytes the first 2 bytes of the first element don't
        // need to be extracted at all. Since we vector-rotate them to the upper elements, it is harder to reason later,
        // that we do not care about them, unless we hard-enforce the vector-width as number of used elements...
        it = insertBitcast(it, method, tmpInput, tmpOutput);
        auto elementOffset = assign(it, subOffset.type, "%load_register_offset") =
            subOffset / Literal(entry.valueType.getElementType().getLogicalWidth());
        it = insertVectorRotation(it, tmpOutput, elementOffset, dest, Direction::DOWN);
    }
    else if(entry.valueType.getScalarBitCount() > containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert simple vector extraction and bit-cast
        // create a vector with the same element type as the container but the same total bit-width as the output type
        auto tmpValue = method.addNewLocal(
            convertSmallContainerLargeDataType(containerType, entry.valueType), "%load_register_tmp");
        it = insertVectorExtraction(it, method, selectedPart, elementOffset, tmpValue);
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

static Value createFirstLastElementMask(DataType sourceType, DataType containerType)
{
    if(sourceType.getLogicalWidth() > containerType.getElementType().getLogicalWidth())
        return 0xFFFFFFFF_val;
    uint32_t mask = 0;
    for(uint8_t i = 0; i < sourceType.getVectorWidth(); ++i)
        mask |= sourceType.getScalarWidthMask() << (i * sourceType.getScalarBitCount());
    return Value(Literal(mask), TYPE_INT32);
}

static NODISCARD InstructionWalker lowerRegisterWrite(Method& method, InstructionWalker it,
    const CacheAccessInstruction& writeInstruction, const RegisterCacheEntry& entry)
{
    auto elementOffset = UNDEFINED_VALUE;
    auto subOffset = UNDEFINED_VALUE;
    auto selectedPart = entry.loweredRegister;
    if(auto multiParts = Local::getLocalData<MultiRegisterData>(entry.loweredRegister.checkLocal()))
        it = insertByteToElementAndSubOffsetAndSelectedPart(
            it, elementOffset, subOffset, selectedPart, entry, *multiParts, false);
    else
        it = insertByteToElementAndSubOffset(it, elementOffset, subOffset, entry);

    auto src = writeInstruction.getData();
    if(src.checkVector())
    {
        // lower vector assembly, since the normalization step did already run
        src = method.addNewLocal(src.type);
        it.emplace(std::make_unique<MoveOperation>(src, writeInstruction.getData()));
        it = normalization::handleContainer(method.module, method, it, {});
        while(!it.isEndOfBlock() && !it.get<CacheAccessInstruction>())
            it.nextInBlock();
    }
    // restore original type, to e.g. for insertion of zero not insert "i8 0", but "i32 0"
    src.type = entry.valueType;

    auto containerType = entry.loweredRegister.type;
    if(entry.valueType.getScalarBitCount() == containerType.getScalarBitCount())
        // simple vector insertion
        it = insertVectorInsertion(it, method, selectedPart, elementOffset, src);
    else if(entry.valueType.isScalarType() && entry.valueType.getScalarBitCount() == 32 &&
        containerType.getScalarBitCount() > 32)
        // simple insertion of a whole single lower/upper element
        it = insertVectorInsertion(it, method, selectedPart, elementOffset, src);
    else if(entry.valueType.getScalarBitCount() < containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert bit-cast and simple vector insertion
        auto tmpValue = method.addNewLocal(
            convertLargeContainerSmallDataType(containerType, entry.valueType), "%store_register_tmp");
        it = insertBitcast(it, method, src, tmpValue);
        if(entry.valueType.getLogicalWidth() % containerType.getElementType().getLogicalWidth() == 0)
            // We fill an number of elements completely, so we can directly insert into the container
            it = insertVectorInsertion(it, method, entry.loweredRegister, elementOffset, tmpValue);
        else
        {
            // We do not completely fill the last container element, so we need to manually apply a sub-element mask to
            // retain the contents in the bytes not intended to be written
            auto numRemainingBytes =
                entry.valueType.getLogicalWidth() % containerType.getElementType().getLogicalWidth();
            auto lastIndex = entry.valueType.getLogicalWidth() / containerType.getElementType().getLogicalWidth();
            auto partialMask = assign(it, containerType.getElementType(), "%store_register_partial_mask") =
                Value(Literal((1u << (numRemainingBytes * 8)) - 1u), TYPE_INT32);
            auto partialValue = assign(it, containerType, "%store_register_partial") = tmpValue & partialMask;
            auto retainMask = assign(it, partialMask.type) = ~partialMask;
            auto previousValue = assign(it, containerType, "%store_register_partial") =
                entry.loweredRegister & retainMask;
            auto lastElementCond = assignNop(it) = selectSIMDElement(static_cast<uint8_t>(lastIndex));
            assign(it, tmpValue) = (partialValue | previousValue, lastElementCond);
            it = insertVectorInsertion(it, method, entry.loweredRegister, elementOffset, tmpValue);
        }
    }
    else if(entry.valueType.getScalarBitCount() < containerType.getScalarBitCount())
    {
        auto tmpInput = method.addNewLocal(src.type, "%store_register_input");
        auto inputElementOffset = assign(it, subOffset.type) =
            subOffset / Literal(entry.valueType.getElementType().getLogicalWidth());
        it = insertVectorRotation(it, src, inputElementOffset, tmpInput, Direction::UP);
        auto tmpValue = method.addNewLocal(
            convertLargeContainerSmallDataType(containerType, entry.valueType, true), "%store_register_tmp");
        it = insertBitcast(it, method, tmpInput, tmpValue, InstructionDecorations::NONE, 1u /* element stride */,
            true /* zero output */);
        /*
         * Do not directly insert into the container, just move into place for now. This is required since we have
         * sub-element offsets and therefore need to make sure the previous content of the partially written elements
         * for the not explicitly written parts is retained. E.g. for writing char2 into an int, we need to retain the
         * previous value of the upper 2 bytes.
         */
        auto tmpContainer = assign(it, entry.loweredRegister.type, "%store_register_output") = INT_ZERO;
        it = insertVectorInsertion(it, method, tmpContainer, elementOffset, tmpValue);
        /*
         * Create mask for all the elements and sub-elements actually written. For this we set all wholly covered
         * elements and need special handling for the first element and one-after-the-elements, since they might be
         * partially written.
         */
        SIMDVector fullElementMask{0_lit};
        auto writeMask = assign(it, containerType, "%store_register_mask") = INT_ZERO;
        for(uint8_t i = 1; i < tmpValue.type.getVectorWidth() - 1u; ++i)
            fullElementMask[i] = 0xFFFFFFFF_lit;
        if(fullElementMask.getAllSame() != 0_lit)
            writeMask = assign(it, containerType, "%store_register_mask") =
                load(LoadImmediate::fromLoadedValues(fullElementMask, LoadType::PER_ELEMENT_SIGNED),
                    LoadType::PER_ELEMENT_SIGNED);
        auto shiftOffset = assign(it, TYPE_INT8) = subOffset * 8_lit;
        auto specialMask = createFirstLastElementMask(entry.valueType, containerType);
        // do not overwrite the first subOffset bytes from the first element
        auto firstElementMask = assign(it, TYPE_INT32) = specialMask << shiftOffset;
        auto firstElementCond = assignNop(it) = selectSIMDElement(0);
        assign(it, writeMask) = (firstElementMask, firstElementCond);
        // do not overwrite the last bytes from the last element
        // if we write 1 source element the last (second) container element is not written at all, if we write 3 source
        // elements, the last container element is written to a maximum of (container type element size / source element
        // type size) - 1, e.g. lower 16-bit for 32-bit container and 16-bit source and lower 3 bytes for 32-bit
        // container and 8-bit source.
        // store 4 * N + 1 char in intM at sub-offset 1: first mask = 0xFFFFFF00, last mask = 0x0000FFFF
        // store 4 * N + 1 char in intM at sub-offset 3: first mask = 0xFF000000, last mask = 0x00000000
        // store 4 * N + 3 char in intM at sub-offset 2: first mask = 0xFFFF0000, last mask = 0x000000FF
        // store 2 * N + 1 char in shortM at sub-offset 1: first mask = 0xFF00, last mask = 0x0000
        // store 2 * N short in intM at sub-offset 1: first mask = 0xFFFF0000, last mask = 0x0000FFFF
        // -> first element offset = sub-offset (in bytes) * size of byte
        // -> first element mask = all upper bytes in container element
        // -> last element offset = (bits in container element type - (sub-offset (in bytes) * size of byte + remaining
        // element type bits not fitting into container element)
        // -> last element mask = all lower bytes in container element
        auto maxPossibleBits =
            ((entry.valueType.getLogicalWidth()) % containerType.getElementType().getLogicalWidth()) * 8u;
        maxPossibleBits = maxPossibleBits == 0 ? 32u : maxPossibleBits;
        auto tmpOffset = assign(it, TYPE_INT8) = shiftOffset + Value(Literal(maxPossibleBits), TYPE_INT8);
        // if the offset becomes negative (e.g. for writing char3 at 3 byte offset into an int-vector, giving 32 - 48 =
        // -16), the handling of SHR (i.e. just taking the lower 5 bits, giving -16 ^ 31 = 16) still gives the correct
        tmpOffset = assign(it, TYPE_INT8) =
            (Value(Literal(containerType.getScalarBitCount()), TYPE_INT8) - tmpOffset, SetFlag::SET_FLAGS);
        // if the byte-offset + the number of bytes written exceeds the container type-size, we need to adapt the last
        // element mask to not fully omit the last element. E.g.
        // - writing char3 into int-vector at byte-offset 2 gives a mask of 0xFFFFFFFF >> -8 = 0xFF
        // - writing char3 into int-vector at byte-offset 3 gives a mask of 0xFFFFFFFF >> -16 = 0xFFFF
        specialMask = assign(it, TYPE_INT32) = specialMask;
        assign(it, specialMask) = (Value(Literal(containerType.getScalarWidthMask()), TYPE_INT32), COND_NEGATIVE_SET);
        auto lastElementMask = assign(it, TYPE_INT32) = as_unsigned{specialMask} >> tmpOffset;
        assign(it, lastElementMask) = (INT_ZERO, COND_ZERO_SET);
        auto lastElementCond = assignNop(it) =
            selectSIMDElement(static_cast<uint8_t>(tmpValue.type.getVectorWidth() - 1u));
        assign(it, writeMask) = (lastElementMask, lastElementCond);
        auto adjustedWriteMask = method.addNewLocal(containerType, "%store_register_mask");
        it = insertVectorRotation(it, writeMask, elementOffset, adjustedWriteMask, Direction::UP);
        auto retainMask = assign(it, adjustedWriteMask.type, "%store_register_mask") = ~adjustedWriteMask;
        auto retainContainer = assign(it, entry.loweredRegister.type) = entry.loweredRegister & retainMask;
        tmpContainer = assign(it, tmpContainer.type) = tmpContainer & adjustedWriteMask;
        assign(it, entry.loweredRegister) = retainContainer | tmpContainer;
    }
    else if(entry.valueType.getScalarBitCount() > containerType.getScalarBitCount() && subOffset == 0_val)
    {
        // no in-element sub-offset, so insert bit-cast and simple vector insertion
        auto tmpValue = method.addNewLocal(
            convertSmallContainerLargeDataType(containerType, entry.valueType), "%store_register_tmp");
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
