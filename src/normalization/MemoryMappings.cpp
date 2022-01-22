/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryMappings.h"

#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../intrinsics/WorkItems.h"
#include "../periphery/RegisterLoweredMemory.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

using MemoryMapper = FunctionPointer<InstructionWalker(Method&, InstructionWalker, MemoryInstruction*,
    const tools::SmallSortedPointerSet<const MemoryInfo*>&, const tools::SmallSortedPointerSet<const MemoryInfo*>&)>;

static InstructionWalker invalidMapping(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    throw CompilationError(CompilationStep::NORMALIZER, "Invalid memory access", mem->to_string());
}

static InstructionWalker lowerMemoryReadOnlyToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker lowerMemoryReadWriteToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker lowerMemoryCopyToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker lowerMemoryReadToVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker lowerMemoryWriteToVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker loadMemoryViaTMU(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker accessMemoryInRAMViaVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);
static InstructionWalker mapMemoryCopy(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);

/* clang-format off */
static constexpr MemoryMapper MAPPERS[6][4] = {
    /* READ,                         WRITE,                          COPY (from),                   FILL */
    {lowerMemoryReadOnlyToRegister,  invalidMapping,                 lowerMemoryReadOnlyToRegister, invalidMapping},                 /* QPU_REGISTER_READONLY */
    {lowerMemoryReadWriteToRegister, lowerMemoryReadWriteToRegister, lowerMemoryCopyToRegister,     lowerMemoryReadWriteToRegister}, /* QPU_REGISTER_READWRITE */
    {lowerMemoryReadToVPM,           lowerMemoryWriteToVPM,          mapMemoryCopy,                 lowerMemoryWriteToVPM},          /* VPM_PER_QPU */
    {lowerMemoryReadToVPM,           lowerMemoryWriteToVPM,          mapMemoryCopy,                 lowerMemoryWriteToVPM},          /* VPM_SHARED_ACCESS */
    {loadMemoryViaTMU,               invalidMapping,                 mapMemoryCopy,                 invalidMapping},                 /* RAM_LOAD_TMU */
    {accessMemoryInRAMViaVPM,        accessMemoryInRAMViaVPM,        mapMemoryCopy,                 accessMemoryInRAMViaVPM},        /* RAM_READ_WRITE_VPM */
};
/* clang-format on */

LCOV_EXCL_START
std::string MemoryInfo::to_string() const
{
    switch(type)
    {
    case MemoryAccessType::QPU_REGISTER_READONLY:
        return "read-only register " + mappedRegisterOrConstant.to_string();
    case MemoryAccessType::QPU_REGISTER_READWRITE:
        return "register " + mappedRegisterOrConstant.to_string();
    case MemoryAccessType::VPM_PER_QPU:
        return "private VPM area " + (area ? area->to_string() : "(null)");
    case MemoryAccessType::VPM_SHARED_ACCESS:
        return "shared VPM area " + (area ? area->to_string() : "(null)");
    case MemoryAccessType::RAM_LOAD_TMU:
        return "read-only memory access via TMU" + std::string(tmuFlag ? "0" : "1");
    case MemoryAccessType::RAM_READ_WRITE_VPM:
        return "read-write memory access via VPM" + (area ? " (cached in " + area->to_string() + ")" : "");
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled memory info type", std::to_string(static_cast<uint32_t>(type)));
}
LCOV_EXCL_STOP

InstructionWalker normalization::mapMemoryAccess(Method& method, InstructionWalker it,
    intermediate::MemoryInstruction* mem, const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    auto& typeInfos = mem->op == MemoryOperation::READ || mem->op == MemoryOperation::COPY ? srcInfos : destInfos;
    if(typeInfos.empty())
        throw CompilationError(CompilationStep::NORMALIZER, "Can't map memory access without valid memory access type",
            mem ? mem->to_string() : it->to_string());
    auto type = (*typeInfos.begin())->type;
    for(const auto& info : typeInfos)
    {
        if(info->type != type)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Can't map conditional memory accesses of different memory access types together",
                mem ? mem->to_string() : it->to_string());
    }
    return MAPPERS[static_cast<unsigned>(type)][static_cast<unsigned>(mem->op)](method, it, mem, srcInfos, destInfos);
}

// FIXME remove once all mappers support multiple sources/destinations
#define ASSERT_SINGLE_SOURCE(type)                                                                                     \
    if(srcInfos.size() != 1)                                                                                           \
    {                                                                                                                  \
        if(mem)                                                                                                        \
            logging::error() << "Memory instruction: " << mem->to_string() << logging::endl;                           \
        logging::error() << "Accessed memory locations: " << to_string<const MemoryInfo*>(srcInfos) << logging::endl;  \
        throw CompilationError(                                                                                        \
            CompilationStep::NORMALIZER, "This type of memory mapping does not yet support multiple sources", type);   \
    }                                                                                                                  \
    const MemoryInfo& srcInfo = **srcInfos.begin()

#define ASSERT_SINGLE_DESTINATION(type)                                                                                \
    if(destInfos.size() != 1)                                                                                          \
    {                                                                                                                  \
        if(mem)                                                                                                        \
            logging::error() << "Memory instruction: " << mem->to_string() << logging::endl;                           \
        logging::error() << "Accessed memory locations: " << to_string<const MemoryInfo*>(srcInfos) << logging::endl;  \
        throw CompilationError(CompilationStep::NORMALIZER,                                                            \
            "This type of memory mapping does not yet support multiple destinations", type);                           \
    }                                                                                                                  \
    const MemoryInfo& destInfo = **destInfos.begin()

static bool copiesWholeRegister(const Value& numEntries, const DataType& elementType, const DataType& registerType)
{
    // for copying of byte* where actually the whole vector is copied
    return numEntries.getLiteralValue() &&
        numEntries.getLiteralValue()->unsignedInt() * elementType.getLogicalWidth() == registerType.getLogicalWidth();
}

static tools::SmallSortedPointerSet<const Local*> getBaseAddresses(
    const tools::SmallSortedPointerSet<const MemoryInfo*>& memoryInfos)
{
    tools::SmallSortedPointerSet<const Local*> result;
    for(const auto& info : memoryInfos)
        result.emplace(info->local);
    return result;
}

static FastMap<const Local*, Value> getBaseAddressesAndContainers(
    const tools::SmallSortedPointerSet<const MemoryInfo*>& memoryInfos)
{
    FastMap<const Local*, Value> result;
    for(const auto& info : memoryInfos)
        result.emplace(info->local, info->mappedRegisterOrConstant.value());
    return result;
}

/*
 * There are several cases of memory lowered into registers:
 * - constant memory with constant index (direct value determinable) -> map to direct value
 * - constant memory which fits into register but dynamic index -> map to register, index by vector rotation
 * - private memory which fits into register -> map to register
 * - private memory where the type can be converted to fit into register -> map to register + index by vector
 * rotation
 */

/*
 * Lowers access to a constant memory location into a register.
 *
 * This can be done for constant memory locations.
 *
 * NOTE: This is the best optimization for memory access and should be preferred, where applicable.
 */
static InstructionWalker lowerMemoryReadOnlyToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    ASSERT_SINGLE_SOURCE("lowerMemoryReadOnlyToRegister");
    if(mem->op != MemoryOperation::READ && mem->op != MemoryOperation::COPY)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Cannot perform a non-read operation on constant memory", mem->to_string());

    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToElementOffset(
        it, method, tmpIndex, getBaseAddressesAndContainers(srcInfos), mem, mem->getSource());
    // TODO check whether index is guaranteed to be in range [0, 16[
    auto elementType = srcInfo.convertedRegisterOrAreaType ? *srcInfo.convertedRegisterOrAreaType :
                                                             srcInfo.mappedRegisterOrConstant->type.getElementType();

    auto wholeRegister = srcInfo.convertedRegisterOrAreaType &&
        copiesWholeRegister(
            mem->getNumEntries(), mem->getDestinationElementType(), *srcInfo.convertedRegisterOrAreaType);
    if(!srcInfo.convertedRegisterOrAreaType && srcInfo.mappedRegisterOrConstant &&
        srcInfo.mappedRegisterOrConstant->getConstantValue(false))
        // check additionally, whether we copy the whole constant "vector"
        wholeRegister |= copiesWholeRegister(
            mem->getNumEntries(), mem->getDestinationElementType(), srcInfo.mappedRegisterOrConstant->type);
    Value tmpVal(UNDEFINED_VALUE);
    if(mem->op == MemoryOperation::COPY && wholeRegister)
        // there is no need to calculate the index, if we copy the whole object
        tmpVal = srcInfo.convertedRegisterOrAreaType ? Value(*srcInfo.convertedRegisterOrAreaType) :
                                                       srcInfo.mappedRegisterOrConstant.value();
    else
    {
        tmpVal = method.addNewLocal(elementType, "%lowered_constant");
        it = insertVectorExtraction(it, method, *srcInfo.mappedRegisterOrConstant, tmpIndex, tmpVal);
    }

    if(srcInfo.mappedRegisterOrConstant && !srcInfo.mappedRegisterOrConstant->checkLocal())
    {
        if(mem->op == MemoryOperation::COPY)
        {
            if(!wholeRegister && mem->getNumEntries() != INT_ONE)
            {
                logging::error() << mem->getSource().to_string() << " - " << srcInfo.to_string() << " -> "
                                 << mem->getDestination().to_string() << " - "
                                 << to_string<const MemoryInfo*>(destInfos) << logging::endl;
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Mapping copy with more than 1 entry is not yet implemented", mem->to_string());
            }
            MemoryInstruction* memFill = nullptr;
            if(wholeRegister && srcInfo.mappedRegisterOrConstant->isAllSame() &&
                srcInfo.mappedRegisterOrConstant->type.getArrayType())
            {
                // XXX This is a work-around for when we manage to "lower" e.g. an i8[128] zeroinitializer into a
                // read-only register, which at least happens for SPIR-V. Since we cannot actually write the array to
                // VPM/etc., we write every single entry, which will then be rewritten to a larger data type as needed
                // (e.g. 2 uint16 writes for the i8[128] example).
                auto arrayType = srcInfo.mappedRegisterOrConstant->type.getArrayType();
                tmpVal.type = arrayType->elementType;
                memFill = &it.reset(std::make_unique<MemoryInstruction>(MemoryOperation::FILL,
                    Value(mem->getDestination()), std::move(tmpVal), Value(Literal(arrayType->size), TYPE_INT32)));
            }
            else
                memFill = &it.reset(std::make_unique<MemoryInstruction>(
                    MemoryOperation::WRITE, Value(mem->getDestination()), std::move(tmpVal)));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced memory copy from constant memory to memory write of constant value: "
                    << it->to_string() << logging::endl);
            return mapMemoryAccess(method, it, memFill, srcInfos, destInfos);
        }
        if(mem->op == MemoryOperation::READ)
        {
            it.reset(std::make_unique<MoveOperation>(mem->getDestination(), tmpVal));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced loading of constant memory with constant literal: " << it->to_string()
                    << logging::endl);
            return it;
        }
    }
    if(srcInfo.convertedRegisterOrAreaType)
    {
        if(mem->op == MemoryOperation::READ)
        {
            it.reset(std::make_unique<MoveOperation>(mem->getDestination(), tmpVal));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced loading of constant memory with vector rotation of register: " << it->to_string()
                    << logging::endl);
            return it;
        }
        if(mem->op == MemoryOperation::COPY)
        {
            if(!wholeRegister && mem->getNumEntries() != INT_ONE)
            {
                logging::error() << mem->getSource().to_string() << " - " << srcInfo.to_string() << " -> "
                                 << mem->getDestination().to_string() << " - "
                                 << to_string<const MemoryInfo*>(destInfos) << logging::endl;
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Mapping copy with more than 1 entry is not yet implemented", mem->to_string());
            }
            auto memWrite = &it.reset(std::make_unique<MemoryInstruction>(
                MemoryOperation::WRITE, Value(mem->getDestination()), std::move(tmpVal)));
            it = mapMemoryAccess(method, it, memWrite, srcInfos, destInfos);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced copying from constant memory with vector rotation and writing of memory: "
                    << it->to_string() << logging::endl);
            return it;
        }
    }
    if(auto constant = getConstantElementValue(mem->getSource()))
    {
        if(mem->op == MemoryOperation::COPY)
        {
            if(mem->getNumEntries() != INT_ONE)
            {
                logging::error() << mem->getSource().to_string() << " - " << srcInfo.to_string() << " -> "
                                 << mem->getDestination().to_string() << " - "
                                 << to_string<const MemoryInfo*>(destInfos) << logging::endl;
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Mapping copy with more than 1 entry is not yet implemented", mem->to_string());
            }
            // since a copy always involves another memory object, this rewrite is picked up when the other
            // object is processed
            auto memWrite = &it.reset(std::make_unique<MemoryInstruction>(
                MemoryOperation::WRITE, Value(mem->getDestination()), *std::move(constant)));
            it = mapMemoryAccess(method, it, memWrite, srcInfos, destInfos);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced memory copy from constant memory to memory write of constant value: "
                    << it->to_string() << logging::endl);
            return it;
        }
        else
        {
            it.reset(std::make_unique<MoveOperation>(mem->getOutput().value(), *constant));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced loading of constant memory with constant literal: " << it->to_string()
                    << logging::endl);
            return it;
        }
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case of lowering constant memory to register", mem->to_string());
}

/*
 * Maps memory access to the given local into moves from/to the given register
 *
 * NOTE: This is the best optimization for memory access and should always be preferred.
 * NOTE: This optimization cannot be applied if changes made to the lowered register need to be reflected to other QPUs.
 */
static InstructionWalker lowerMemoryReadWriteToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    if(mem->op == MemoryOperation::READ)
    {
        for(const auto& entry : srcInfos)
            if(!entry->mappedRegisterOrConstant)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Cannot map memory location to register without mapping register specified", mem->to_string());
    }
    else
    {
        for(const auto& entry : destInfos)
            if(!entry->mappedRegisterOrConstant)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Cannot map memory location to register without mapping register specified", mem->to_string());
    }

    if(mem->op == MemoryOperation::READ)
    {
        Value offset = UNDEFINED_VALUE;
        Value selectedContainer = UNDEFINED_VALUE;
        it = insertAddressToOffsetAndContainer(
            it, method, offset, getBaseAddressesAndContainers(srcInfos), selectedContainer, mem, mem->getSource());
        if(auto src = check(selectedContainer.getSingleWriter()) & &LocalUser::getMoveSource)
            selectedContainer = *src;
        it = periphery::insertReadLoweredRegister(method, it, mem->getDestination(), offset, selectedContainer);
    }
    else if(mem->op == MemoryOperation::WRITE)
    {
        ASSERT_SINGLE_DESTINATION("lowerMemoryReadWriteToRegister");
        // TODO copying the container does not work, since we need to write back...
        Value offset = UNDEFINED_VALUE;
        it = insertAddressToOffset(it, method, offset, getBaseAddresses(destInfos), mem, mem->getDestination());
        it = periphery::insertWriteLoweredRegister(
            method, it, mem->getSource(), offset, destInfo.mappedRegisterOrConstant.value());
    }
    else if(mem->op == MemoryOperation::FILL && mem->getSource().type.isScalarType())
    {
        ASSERT_SINGLE_DESTINATION("lowerMemoryReadWriteToRegister");
        it = insertReplication(it, mem->getSource(), destInfo.mappedRegisterOrConstant.value());
    }
    else
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Replaced access to stack allocation '" << it->to_string()
            << "' with: " << it.copy().previousInBlock()->to_string() << logging::endl);
    return it.erase();
}

static InstructionWalker lowerMemoryCopyToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    ASSERT_SINGLE_SOURCE("lowerMemoryCopyToRegister");
    ASSERT_SINGLE_DESTINATION("lowerMemoryCopyToRegister");
    if(mem->op != MemoryOperation::COPY)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
    if(destInfo.type == MemoryAccessType::QPU_REGISTER_READONLY)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Copy into read-only registers is not supported", mem->to_string());

    auto wholeRegister = copiesWholeRegister(
        mem->getNumEntries(), mem->getDestinationElementType(), *srcInfo.convertedRegisterOrAreaType);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Mapping copy with register-mapped memory: " << mem->to_string() << logging::endl);

    Value offset = UNDEFINED_VALUE;
    it = insertAddressToOffset(it, method, offset, getBaseAddresses(srcInfos), mem, mem->getSource());
    if(srcInfo.mappedRegisterOrConstant)
    {
        Value tmp(UNDEFINED_VALUE);
        Value numEntries = INT_ONE;
        if(wholeRegister)
            tmp = *srcInfo.mappedRegisterOrConstant;
        else
        {
            if(mem->getNumEntries() != INT_ONE)
            {
                if(auto lit = mem->getNumEntries().getLiteralValue())
                {
                    // TODO is this correct?
                    // NOTE: copied entry type could be byte, which actual type is half-word or word!
                    auto typeFactor = static_cast<uint32_t>(
                        srcInfo.mappedRegisterOrConstant->type.getElementType().getScalarBitCount() /
                        mem->getSourceElementType().getScalarBitCount());
                    if((lit->unsignedInt() % typeFactor) != 0)
                        throw CompilationError(CompilationStep::NORMALIZER,
                            "Copied number of bytes is not a multiple of the actual register type", mem->to_string());
                    auto numElements = lit->unsignedInt() / typeFactor;
                    if(numElements == 0 || numElements > NATIVE_VECTOR_SIZE)
                    {
                        logging::error() << "Cannot copy " << numElements << " elements of "
                                         << mem->getSourceElementType().to_string() << " from "
                                         << mem->getSource().to_string() << " into "
                                         << mem->getDestination().to_string() << logging::endl;
                        throw CompilationError(
                            CompilationStep::NORMALIZER, "Invalid copied number of elements", mem->to_string());
                    }
                    tmp = method.addNewLocal(
                        srcInfo.mappedRegisterOrConstant->type.toVectorType(static_cast<uint8_t>(numElements)));
                }
                // TODO only add if above is tested and works!
                // else
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Mapping copy with a dynamic number of entries is not yet implemented", mem->to_string());
            }
            else
                tmp = method.addNewLocal(mem->getSourceElementType());
            it = periphery::insertReadLoweredRegister(method, it, tmp, offset, *srcInfo.mappedRegisterOrConstant);
        }
        auto memWrite = &it.reset(std::make_unique<MemoryInstruction>(
            MemoryOperation::WRITE, Value(mem->getDestination()), std::move(tmp), std::move(numEntries)));
        return mapMemoryAccess(method, it, memWrite, srcInfos, destInfos);
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
}

static InstructionWalker insertToInVPMAreaOffset(Method& method, InstructionWalker it, Value& out,
    const MemoryInfo& info, const MemoryInstruction* mem, const Value& ptrValue)
{
    if(info.ranges)
    {
        auto range = std::find_if(info.ranges->begin(), info.ranges->end(),
            [&](const MemoryAccessRange& range) -> bool { return range.addressWrite.base() == it; });
        if(range == info.ranges->end())
            throw CompilationError(CompilationStep::NORMALIZER,
                "Failed to find memory access range for VPM cached memory access", mem->to_string());
        return insertAddressToWorkItemSpecificOffset(it, method, out, const_cast<MemoryAccessRange&>(*range));
    }
    return insertAddressToStackOffset(it, method, out, info.local, info.type, mem, ptrValue);
}

/*
 * Tries to map the given memory location into VPM
 *
 * This is applicable for private (stack) or local memory.
 *
 * NOTE: A memory location can only be lowered into VPM if all access to it can be lowered to VPM
 * NOTE: This is to be preferred over keeping the memory location in RAM
 */
static InstructionWalker lowerMemoryReadToVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    ASSERT_SINGLE_SOURCE("lowerMemoryReadToVPM");
    // Need to make sure addressing is still correct!
    if(srcInfo.type == MemoryAccessType::VPM_PER_QPU && !srcInfo.local->is<StackAllocation>())
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of per-QPU memory buffer", srcInfo.local->to_string());
    if(!srcInfo.area)
        throw CompilationError(CompilationStep::NORMALIZER, "Cannot lower into VPM without VPM area", mem->to_string());

    if(srcInfo.type == MemoryAccessType::VPM_PER_QPU)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping read of stack allocation into VPM: " << mem->to_string() << logging::endl);
    else
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping read of shared local memory into VPM: " << mem->to_string() << logging::endl);

    Value inAreaOffset = UNDEFINED_VALUE;
    it = insertToInVPMAreaOffset(method, it, inAreaOffset, srcInfo, mem, mem->getSource());
    if(mem->op == MemoryOperation::READ)
    {
        it =
            method.vpm->insertReadVPM(method, it, mem->getDestination(), *srcInfo.area, mem->guardAccess, inAreaOffset);
        return it.erase();
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case to lower reading of memory into VPM", mem->to_string());
}

static InstructionWalker lowerMemoryWriteToVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    bool allAreasArePerQPU = true;
    bool allAreasAreShared = true;
    for(auto destInfo : destInfos)
    {
        if(destInfo->type == MemoryAccessType::VPM_PER_QPU && !destInfo->local->is<StackAllocation>())
            throw CompilationError(
                CompilationStep::NORMALIZER, "Unhandled case of per-QPU memory buffer", destInfo->local->to_string());
        if(!destInfo->area)
            throw CompilationError(
                CompilationStep::NORMALIZER, "Cannot lower into VPM without VPM area", mem->to_string());
        if(destInfo->type != MemoryAccessType::VPM_PER_QPU)
            allAreasArePerQPU = false;
        if(destInfo->type != MemoryAccessType::VPM_SHARED_ACCESS)
            allAreasAreShared = false;
    }

    if(allAreasArePerQPU)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping write to stack allocation into VPM: " << mem->to_string() << logging::endl);
    else if(allAreasAreShared)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping write to shared local memory into VPM: " << mem->to_string() << logging::endl);
    else
        throw CompilationError(CompilationStep::NORMALIZER,
            "Cannot lower conditional access to per-QPU and shared VPM memory", mem->to_string());

    ASSERT_SINGLE_DESTINATION("lowerMemoryWriteToVPM");

    Value inAreaOffset = UNDEFINED_VALUE;
    it = insertToInVPMAreaOffset(method, it, inAreaOffset, destInfo, mem, mem->getDestination());
    if(mem->op == MemoryOperation::WRITE)
    {
        it = method.vpm->insertWriteVPM(method, it, mem->getSource(), *destInfo.area, mem->guardAccess, inAreaOffset);
        return it.erase();
    }
    if(mem->op == MemoryOperation::FILL)
    {
        if(!mem->getNumEntries().getLiteralValue())
        {
            throw CompilationError(CompilationStep::NORMALIZER,
                "Filling dynamically sized VPM area is not yet implemented", mem->to_string());
        }
        if(mem->getSource().type == TYPE_INT8)
        {
            // if we fill single bytes, combine them to some vector type to not have to write so many single bytes
            auto vpmType = periphery::getBestVectorSize(mem->getNumEntries().getLiteralValue()->unsignedInt());
            // 1. replicate byte across word
            auto fillWord = assign(it, TYPE_INT32) = (mem->getSource(), UNPACK_8A_32);
            // 2. replicate word across all vector elements
            auto fillVector = method.addNewLocal(TYPE_INT32.toVectorType(16), "%memory_fill");
            it = insertReplication(it, fillWord, fillVector);
            // 3. calculate base offset for per-QPU memory area
            Value inAreaOffset = UNDEFINED_VALUE;
            it = insertToInVPMAreaOffset(method, it, inAreaOffset, destInfo, mem, mem->getDestination());
            // 3. write vector to VPM
            auto vpmTypeSize = Literal(vpmType.first.getInMemoryWidth());
            if(mem->guardAccess)
            {
                it.emplace(std::make_unique<MutexLock>(MutexAccess::LOCK));
                it.nextInBlock();
            }
            for(unsigned i = 0; i < vpmType.second; ++i)
            {
                auto byteOffset = assign(it, TYPE_INT32) = Value(Literal(i), TYPE_INT32) * vpmTypeSize;
                byteOffset = assign(it, TYPE_INT32) = inAreaOffset + byteOffset;
                it = method.vpm->insertWriteVPM(method, it, fillVector, *destInfo.area, false, byteOffset);
            }
            if(mem->guardAccess)
            {
                it.emplace(std::make_unique<MutexLock>(MutexAccess::RELEASE));
                it.nextInBlock();
            }
            return it.erase();
        }
        logging::error() << "Destination: " << destInfo.local->to_string() << " - " << mem->getNumEntries().to_string()
                         << " - " << mem->getSource().to_string() << " - "
                         << (destInfo.area ? destInfo.area->to_string() : "") << logging::endl;
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case to lower writing of memory into VPM", mem->to_string());
}

/*
 * Maps a single memory read to a TMU load
 *
 * NOTE: Memory locations loaded via TMU MUST NOT be written to by the same kernel (even on a different QPU)!
 */
static InstructionWalker loadMemoryViaTMU(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Loading from read-only memory via TMU: " << mem->to_string() << logging::endl);
    if(mem->op == MemoryOperation::READ)
    {
        // we have to use the same TMU for all loads. To minimize cache misses, select the TMU which is used by the most
        // sources selected
        uint32_t numTMU0 = 0;
        uint32_t numTMU1 = 0;
        for(auto srcInfo : srcInfos)
        {
            if(auto param = srcInfo->local->as<Parameter>())
                param->decorations = add_flag(param->decorations, ParameterDecorations::INPUT);
            ++(srcInfo->tmuFlag ? numTMU0 : numTMU1);
        }
        // prefer TMU1 here, since statistically TMU0 will be used more often
        const auto& tmu = numTMU0 > numTMU1 ? periphery::TMU0 : periphery::TMU1;
        it = periphery::insertReadVectorFromTMU(method, it, mem->getDestination(), mem->getSource(), tmu);
        return it.erase();
    }
    throw CompilationError(CompilationStep::NORMALIZER, "Unhandled case to read from memory via TMU", mem->to_string());
}

/*
 * Maps a memory access instruction to an instruction accessing RAM through VPM.
 *
 * NOTE: At least one of the operands of the instruction to be mapped must be located in RAM
 * NOTE: this is the least optimal mapping possible and should avoided if possible.
 */
static InstructionWalker accessMemoryInRAMViaVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Mapping access to memory located in RAM: " << mem->to_string() << logging::endl);
    switch(mem->op)
    {
    case MemoryOperation::FILL:
    {
        if(mem->guardAccess)
        {
            it.emplace(std::make_unique<MutexLock>(MutexAccess::LOCK));
            it.nextInBlock();
        }
        auto numCopies = mem->getNumEntries().getLiteralValue();
        if(numCopies && mem->getSource().type == TYPE_INT8)
        {
            // if we fill single bytes, combine them to some vector type to not have to write so many single bytes
            auto vpmType = periphery::getBestVectorSize(numCopies->unsignedInt());
            // 1. replicate byte across word
            auto fillWord = assign(it, TYPE_INT32) = (mem->getSource(), UNPACK_8A_32);
            // 2. replicate word across all vector elements
            auto fillVector = method.addNewLocal(vpmType.first, "%memory_fill");
            it = insertReplication(it, fillWord, fillVector);
            // 3. write vector to VPM and then fill the memory
            it = method.vpm->insertFillDMA(
                method, it, mem->getDestination(), fillVector, Value(Literal(vpmType.second), TYPE_INT32), false);
        }
        else
            // Fill dynamically sized memory
            // TODO This is usually the result of an (llvm.)memset(...) instruction, which always writes a certain
            // number of single bytes, which is very inefficient!
            it = method.vpm->insertFillDMA(
                method, it, mem->getDestination(), mem->getSource(), mem->getNumEntries(), false);
        if(mem->guardAccess)
        {
            it.emplace(std::make_unique<MutexLock>(MutexAccess::RELEASE));
            it.nextInBlock();
        }
        for(auto destInfo : destInfos)
        {
            if(auto param = destInfo->local->as<Parameter>())
                param->decorations = add_flag(param->decorations, ParameterDecorations::OUTPUT);
        }
        break;
    }
    case MemoryOperation::READ:
    {
        it = periphery::insertReadDMA(method, it, mem->getDestination(), mem->getSource(), mem->guardAccess);
        for(auto srcInfo : srcInfos)
        {
            if(auto param = srcInfo->local->as<Parameter>())
                param->decorations = add_flag(param->decorations, ParameterDecorations::INPUT);
        }

        break;
    }
    case MemoryOperation::WRITE:
    {
        it = periphery::insertWriteDMA(method, it, mem->getSource(), mem->getDestination(), mem->guardAccess);
        for(auto destInfo : destInfos)
        {
            if(auto param = destInfo->local->as<Parameter>())
                param->decorations = add_flag(param->decorations, ParameterDecorations::OUTPUT);
        }
        break;
    }
    default:
        throw CompilationError(CompilationStep::NORMALIZER, "Unhandled case of accessing RAM", mem->to_string());
    }
    // remove MemoryInstruction
    // since a copy may have another iterator to it, do not remove the element, just clear it
    // the empty instruction is cleaned up in #combineVPMAccess
    return it.erase();
}

static InstructionWalker mapMemoryCopy(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
    const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos)
{
    /*
     * Handled cases:
     *
     * From\To |     VPM      |        RAM           |
     * VPM     | read + write |      DMA write       |
     * RAM     |   DMA read   | DMA read + DMA write |
     *
     */

    // srcInRegister is handled by another function
    bool destInRegister = std::all_of(destInfos.begin(), destInfos.end(),
        [](const MemoryInfo* info) { return info->type == MemoryAccessType::QPU_REGISTER_READWRITE; });
    bool srcInVPM = std::all_of(srcInfos.begin(), srcInfos.end(), [](const MemoryInfo* info) {
        return info->type == MemoryAccessType::VPM_PER_QPU || info->type == MemoryAccessType::VPM_SHARED_ACCESS;
    });
    bool srcInRAM = std::all_of(srcInfos.begin(), srcInfos.end(), [](const MemoryInfo* info) {
        return info->type == MemoryAccessType::RAM_LOAD_TMU || info->type == MemoryAccessType::RAM_READ_WRITE_VPM;
    });
    bool srcAreas = std::all_of(srcInfos.begin(), srcInfos.end(), [](const MemoryInfo* info) { return info->area; });
    bool destInVPM = std::all_of(destInfos.begin(), destInfos.end(), [](const MemoryInfo* info) {
        return info->type == MemoryAccessType::VPM_PER_QPU || info->type == MemoryAccessType::VPM_SHARED_ACCESS;
    });
    bool destInRAM = std::all_of(destInfos.begin(), destInfos.end(), [](const MemoryInfo* info) {
        return info->type == MemoryAccessType::RAM_LOAD_TMU || info->type == MemoryAccessType::RAM_READ_WRITE_VPM;
    });
    bool destAreas = std::all_of(destInfos.begin(), destInfos.end(), [](const MemoryInfo* info) { return info->area; });
    bool destConvertedRegisters = std::all_of(
        destInfos.begin(), destInfos.end(), [](const MemoryInfo* info) { return info->convertedRegisterOrAreaType; });

    for(auto srcInfo : srcInfos)
    {
        if(auto param = srcInfo->local->as<Parameter>())
            param->decorations = add_flag(param->decorations, ParameterDecorations::INPUT);
    }
    for(auto destInfo : destInfos)
    {
        if(auto param = destInfo->local->as<Parameter>())
            param->decorations = add_flag(param->decorations, ParameterDecorations::OUTPUT);
    }

    // for some/all copies, LLVM generates memcpy of i8* to i8* with the number of bytes as number of elements. We need
    // to convert it back to the actual number of elements of the given type
    auto numEntries = mem->getNumEntries();
    Optional<DataType> vpmRowType{};
    if(numEntries.getLiteralValue() && srcAreas && mem->getSourceElementType() == TYPE_INT8)
    {
        ASSERT_SINGLE_SOURCE("mapMemoryCopy");
        auto origType = srcInfo.local->type.getElementType();
        auto numBytes = numEntries.getLiteralValue()->unsignedInt();
        if(numBytes != origType.getInMemoryWidth())
        {
            logging::error() << "Trying to copy " << numBytes << " bytes of " << origType.to_string() << logging::endl;
            throw CompilationError(CompilationStep::NORMALIZER,
                "Byte-wise partial copy from VPM is not yet implemented", mem->to_string());
        }
        if(auto array = origType.getArrayType())
        {
            numEntries = Value(Literal(array->size), TYPE_INT32);
            vpmRowType = array->elementType;
        }
        else if(origType.isScalarType() || origType.isVectorType())
        {
            numEntries = INT_ONE;
            vpmRowType = origType;
        }
        else
            throw CompilationError(
                CompilationStep::NORMALIZER, "Unsupported element type for memory copy into VPM", mem->to_string());
    }

    if(numEntries.getLiteralValue() && destAreas && mem->getDestinationElementType() == TYPE_INT8)
    {
        ASSERT_SINGLE_DESTINATION("mapMemoryCopy");
        auto origType = destInfo.local->type.getElementType();
        auto numBytes = numEntries.getLiteralValue()->unsignedInt();
        if(numBytes != origType.getInMemoryWidth())
        {
            logging::error() << "Trying to copy " << numBytes << " bytes of " << origType.to_string() << logging::endl;
            throw CompilationError(
                CompilationStep::NORMALIZER, "Byte-wise partial copy to VPM is not yet implemented", mem->to_string());
        }
        if(auto array = origType.getArrayType())
        {
            numEntries = Value(Literal(array->size), TYPE_INT32);
            vpmRowType = array->elementType;
        }
        else if(origType.isScalarType() || origType.isVectorType())
        {
            numEntries = INT_ONE;
            vpmRowType = origType;
        }
        else
            throw CompilationError(
                CompilationStep::NORMALIZER, "Unsupported element type for memory copy into VPM", mem->to_string());
    }

    if(srcInVPM && destInVPM)
    {
        // copy from VPM into VPM -> VPM read + VPM write
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping copy from/to VPM to VPM read and VPM write: " << mem->to_string() << logging::endl);

        if(numEntries != INT_ONE)
            // TODO could for static count insert that number of reads/writes, for dynamic need a loop!
            throw CompilationError(CompilationStep::NORMALIZER,
                "Copying within VPM with more than 1 entries is not yet implemented", mem->to_string());
        if(mem->guardAccess)
        {
            it.emplace(std::make_unique<MutexLock>(MutexAccess::LOCK));
            it.nextInBlock();
        }
        auto tmpVal = method.addNewLocal(mem->getSourceElementType(), "%vpm_copy_tmp");
        auto memRead = &it.emplace(std::make_unique<MemoryInstruction>(
            MemoryOperation::READ, Value(tmpVal), Value(mem->getSource()), Value(numEntries), false));
        it = mapMemoryAccess(method, it, memRead, srcInfos, destInfos);
        auto memWrite = &it.reset(std::make_unique<MemoryInstruction>(
            MemoryOperation::WRITE, Value(mem->getDestination()), std::move(tmpVal), Value(numEntries), false));
        it = mapMemoryAccess(method, it, memWrite, srcInfos, destInfos);
        if(mem->guardAccess)
        {
            it.emplace(std::make_unique<MutexLock>(MutexAccess::RELEASE));
            it.nextInBlock();
        }
        return it;
    }
    else if(srcInVPM && destInRAM)
    {
        // copy from VPM into RAM -> DMA write
        ASSERT_SINGLE_SOURCE("mapMemoryCopy");
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping copy from VPM into RAM to DMA write: " << mem->to_string() << logging::endl);
        Value inAreaOffset = UNDEFINED_VALUE;
        it = insertToInVPMAreaOffset(method, it, inAreaOffset, srcInfo, mem, mem->getSource());
        auto destType = method.createPointerType(vpmRowType.value_or(mem->getDestinationElementType()),
            mem->getDestination().type.getPointerType()->addressSpace);
        it = method.vpm->insertWriteRAM(method, it, Value(mem->getDestination().local(), destType),
            vpmRowType.value_or(mem->getSourceElementType()), *srcInfo.area, mem->guardAccess, inAreaOffset,
            numEntries);
        return it.erase();
    }
    else if(srcInRAM && destInVPM)
    {
        // copy from RAM into VPM -> DMA read
        ASSERT_SINGLE_DESTINATION("mapMemoryCopy");
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping copy from RAM into VPM to DMA read: " << mem->to_string() << logging::endl);
        Value inAreaOffset = UNDEFINED_VALUE;
        it = insertToInVPMAreaOffset(method, it, inAreaOffset, destInfo, mem, mem->getDestination());
        auto srcType = method.createPointerType(
            vpmRowType.value_or(mem->getSourceElementType()), mem->getSource().type.getPointerType()->addressSpace);
        it = method.vpm->insertReadRAM(method, it, Value(mem->getSource().local(), srcType),
            vpmRowType.value_or(mem->getDestinationElementType()), *destInfo.area, mem->guardAccess, inAreaOffset,
            numEntries);
        return it.erase();
    }
    else if(srcInRAM && destInRAM)
    {
        // copy from RAM into RAM -> DMA read + DMA write
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Mapping copy from RAM into RAM to DMA read and DMA write: " << mem->to_string() << logging::endl);
        if(!numEntries.isLiteralValue())
            it = method.vpm->insertCopyRAMDynamic(
                method, it, mem->getDestination(), mem->getSource(), numEntries, mem->guardAccess);
        else
        {
            uint64_t numBytes = numEntries.getLiteralValue()->unsignedInt() *
                (mem->getSourceElementType().getScalarBitCount() * mem->getSourceElementType().getVectorWidth()) / 8;
            if(numBytes > std::numeric_limits<unsigned>::max())
                throw CompilationError(
                    CompilationStep::OPTIMIZER, "Cannot copy more than 4GB of data", mem->to_string());

            it = method.vpm->insertCopyRAM(
                method, it, mem->getDestination(), mem->getSource(), static_cast<unsigned>(numBytes), mem->guardAccess);
        }
        return it.erase();
    }
    else if(destInRegister && destConvertedRegisters)
    {
        // copy from VPM/RAM into register -> read from VPM/RAM + write to register
        ASSERT_SINGLE_DESTINATION("mapMemoryCopy");
        if(copiesWholeRegister(numEntries, mem->getSourceElementType(), *destInfo.convertedRegisterOrAreaType))
        {
            // e.g. for copying 32 bytes into float[8] register -> just read 1 float16 vector
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Mapping copy of whole register from VPM/RAM into register to read from VPM/RAM: "
                    << mem->to_string() << logging::endl);
            auto memRead = &it.reset(
                std::make_unique<MemoryInstruction>(MemoryOperation::READ, Value(*destInfo.mappedRegisterOrConstant),
                    Value(mem->getSource().local(), method.createPointerType(*destInfo.convertedRegisterOrAreaType)),
                    Value(INT_ONE), mem->guardAccess));
            return mapMemoryAccess(method, it, memRead, srcInfos, destInfos);
        }
        else if(numEntries.getLiteralValue() &&
            (numEntries.getLiteralValue()->unsignedInt() * mem->getSourceElementType().getLogicalWidth()) <=
                TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE).getLogicalWidth())
        {
            // general case, read whole row via TMU/VPM and insert only actually used elements
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Mapping partial copy of read only RAM into register: " << mem->to_string() << logging::endl);
            // e.g. if we copy 2 entries of int2, we need to copy 4 SIMD elements
            // also if we copy 20 entries of i8, we need to copy 5 SIMD elements of i32!
            auto numElements =
                (numEntries.getLiteralValue()->unsignedInt() * mem->getSourceElementType().getLogicalWidth());

            if(mem->guardAccess)
            {
                it.emplace(std::make_unique<MutexLock>(MutexAccess::LOCK));
                it.nextInBlock();
            }
            auto destRegisterElementType = destInfo.convertedRegisterOrAreaType->getElementType();
            Value tmp = UNDEFINED_VALUE;
            if(numElements % destRegisterElementType.getLogicalWidth() == 0 &&
                (numElements / destRegisterElementType.getLogicalWidth()) <= NATIVE_VECTOR_SIZE)
            {
                // simple case, we can directly copy elements of the lowered register type
                numElements = numElements / destRegisterElementType.getLogicalWidth();
                tmp = method.addNewLocal(
                    destRegisterElementType.toVectorType(static_cast<uint8_t>(numElements)), "%mem_read_tmp");
            }
            else
            {
                // copy e.g. copying 1 byte into an int vector, need to combine the byte with the rest of the word for
                // the correct element, which is handled in the register-lowered memory functions
                tmp = method.addNewLocal(mem->getSourceElementType());
            }
            auto memRead = &it.emplace(std::make_unique<MemoryInstruction>(
                MemoryOperation::READ, Value(tmp), Value(mem->getSource()), Value(INT_ONE), false));
            it = mapMemoryAccess(method, it, memRead, srcInfos, destInfos);
            auto memWrite = &it.reset(std::make_unique<MemoryInstruction>(
                MemoryOperation::WRITE, Value(mem->getDestination()), std::move(tmp), Value(INT_ONE), false));
            it = mapMemoryAccess(method, it, memWrite, srcInfos, destInfos);
            if(mem->guardAccess)
            {
                it.emplace(std::make_unique<MutexLock>(MutexAccess::RELEASE));
                it.nextInBlock();
            }
            return it;
        }
        else
        {
            // copy an dynamic (or constant but too big fitting) area of VPM/RAM (via TMU or VPM) to register
            ASSERT_SINGLE_SOURCE("mapMemoryCopy");
            logging::error() << to_string<const MemoryInfo*>(srcInfos) << " - " << srcInfo.to_string() << logging::endl;
            logging::error() << to_string<const MemoryInfo*>(destInfos) << " - " << destInfo.to_string()
                             << logging::endl;
            if(auto writer = mem->getNumEntries().getSingleWriter())
                logging::error() << writer->to_string() << logging::endl;
            throw CompilationError(CompilationStep::NORMALIZER, "Needs to be re-written", mem->to_string());
        }
    }
    else
    {
        LCOV_EXCL_START
        for(const auto& srcInfo : srcInfos)
            logging::error() << "Source: " << (srcInfo->local ? srcInfo->local->to_string() : "?") << " - "
                             << static_cast<unsigned>(srcInfo->type) << " - "
                             << (srcInfo->area ? srcInfo->area->to_string() : "") << logging::endl;

        for(const auto& destInfo : destInfos)
            logging::error() << "Destination: " << (destInfo->local ? destInfo->local->to_string() : "?") << " - "
                             << static_cast<unsigned>(destInfo->type) << " - "
                             << (destInfo->area ? destInfo->area->to_string() : "") << logging::endl;

        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case for handling memory copy", mem->to_string());
        LCOV_EXCL_STOP
    }
}

static InstructionWalker insertWriteBackCode(
    Method& method, InstructionWalker it, const Local* memoryArea, const MemoryInfo* info)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Inserting code to write back data cached in VPM area '" << info->area->to_string()
            << "' to: " << memoryArea->to_string() << logging::endl);

    // We either loaded the previous contents of the RAM region into the VPM area at beginning of the work-group or have
    // proven that the whole VPM area is overwritten by the work-items. So we can simply copy the whole VPM area to RAM.
    // Since this code is only executed for the first work-item while the rest is blocked, we do not have to use the
    // mutex.

    // the address is the work-group constant offset to the base address!
    auto memoryOffset = method.addNewLocal(memoryArea->type, "%cache_uniform_offset");
    std::vector<MemoryAccessRange> tmpRanges = info->ranges.value();
    auto tmp = analysis::checkWorkGroupUniformParts(tmpRanges);
    if(!tmp.first)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Cannot insert write-back code for cached local with different work-group uniform parts",
            memoryArea->to_string());
    it = insertAddressToWorkGroupUniformOffset(it, method, memoryOffset, tmpRanges.at(0));
    // TODO how to get number and type of elements? E.g. for char8 per row, don't write upper garbage of rows...
    // TODO add checks whether the ranges/limits/types are all acceptable 8e.g. in range)!
    auto memoryAddress = assign(it, memoryArea->type, "%cache_base_address") =
        memoryArea->createReference() + memoryOffset;
    Value numEntries = UNDEFINED_VALUE;
    {
        // FIXME this is only correct if every work-item writes a single entry. tmp.second only lists the maximum number
        // of entries written. TODO how to reliably get the actual number of entries written by all work-items?
        // calculate the scalar local size
        auto localSizeX = method.addNewLocal(TYPE_INT8, "%local_size_x");
        auto* tmpCall = &it.emplace(std::make_unique<MethodCall>(
            Value(localSizeX), std::string(intrinsics::FUNCTION_NAME_LOCAL_SIZE), std::vector<Value>{0_val}));
        auto oldIt = it;
        it.nextInBlock();
        intrinsics::intrinsifyWorkItemFunction(method, typeSafe(oldIt, *tmpCall));
        auto localSizeY = method.addNewLocal(TYPE_INT8, "%local_size_y");
        tmpCall = &it.emplace(std::make_unique<MethodCall>(
            Value(localSizeY), std::string(intrinsics::FUNCTION_NAME_LOCAL_SIZE), std::vector<Value>{1_val}));
        oldIt = it;
        it.nextInBlock();
        intrinsics::intrinsifyWorkItemFunction(method, typeSafe(oldIt, *tmpCall));
        auto localSizeZ = method.addNewLocal(TYPE_INT8, "%local_size_z");
        tmpCall = &it.emplace(std::make_unique<MethodCall>(
            Value(localSizeZ), std::string(intrinsics::FUNCTION_NAME_LOCAL_SIZE), std::vector<Value>{2_val}));
        oldIt = it;
        it.nextInBlock();
        intrinsics::intrinsifyWorkItemFunction(method, typeSafe(oldIt, *tmpCall));

        // local_size_scalar = local_size_z * local_size_y * local_size_x
        auto tmp = assign(it, TYPE_INT8, "%local_size_scalar") = mul24(localSizeZ, localSizeY);
        numEntries = assign(it, TYPE_INT8, "%local_size_scalar") = mul24(tmp, localSizeX);
    }
    auto elementType = info->area->originalAddress->type.getElementType();
    return method.vpm->insertWriteRAM(
        method, it, memoryAddress, elementType, *info->area, false /* no mutex required */, INT_ZERO, numEntries);
}

void normalization::insertCacheSynchronizationCode(
    Method& method, const FastMap<const Local*, CacheMemoryData>& cachedLocals)
{
    if(std::any_of(cachedLocals.begin(), cachedLocals.end(),
           [](const auto& cacheEntry) -> bool { return cacheEntry.second.insertWriteBack; }))
    {
        // insert control-flow barrier block at end of kernel with cache write-back code
        auto lastBlock = method.findBasicBlock(BasicBlock::LAST_BLOCK);
        auto newLabel = method.addNewLocal(TYPE_LABEL, "%cache_write_back").local();
        auto it = method.emplaceLabel(lastBlock->walk(), std::make_unique<intermediate::BranchLabel>(*newLabel));
        auto newBlock = it.getBasicBlock();
        it.nextInBlock();
        intrinsics::insertControlFlowBarrier(method, it, [&](InstructionWalker blockIt) -> InstructionWalker {
            for(const auto& entry : cachedLocals)
                blockIt = insertWriteBackCode(method, blockIt, entry.first, entry.second.info);
            return blockIt;
        });
        intermediate::redirectAllBranches(*lastBlock, *newBlock);
        method.flags = add_flag(method.flags, MethodFlags::TRAILING_CONTROL_FLOW_BARRIER);
    }
}
