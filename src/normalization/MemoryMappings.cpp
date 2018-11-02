/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryMappings.h"

#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/operators.h"
#include "../periphery/TMU.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

using MemoryMapper = InstructionWalker (*)(
    Method&, InstructionWalker, MemoryInstruction*, const MemoryInfo&, const MemoryInfo&);

static InstructionWalker invalidMapping(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    throw CompilationError(CompilationStep::NORMALIZER, "Invalid memory access", mem->to_string());
}

static InstructionWalker lowerMemoryReadOnlyToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker lowerMemoryReadWriteToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker lowerMemoryCopyToRegister(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker lowerMemoryReadToVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker lowerMemoryWriteToVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker loadMemoryViaTMU(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker accessMemoryInRAMViaVPM(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
static InstructionWalker mapMemoryCopy(Method& method, InstructionWalker it, MemoryInstruction* mem,
    const MemoryInfo& srcInfo, const MemoryInfo& destInfo);

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

InstructionWalker normalization::mapMemoryAccess(Method& method, InstructionWalker it,
    intermediate::MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    auto type = mem->op == MemoryOperation::READ || mem->op == MemoryOperation::COPY ? srcInfo.type : destInfo.type;
    return MAPPERS[static_cast<unsigned>(type)][static_cast<unsigned>(mem->op)](method, it, mem, srcInfo, destInfo);
}

static bool copiesWholeRegister(const Value& numEntries, const DataType& elementType, const DataType& registerType)
{
    // for copying of byte* where actually the whole vector is copied
    return numEntries.getLiteralValue() &&
        numEntries.getLiteralValue()->unsignedInt() * elementType.getPhysicalWidth() == registerType.getPhysicalWidth();
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
static InstructionWalker lowerMemoryReadOnlyToRegister(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    if(mem->op != MemoryOperation::READ && mem->op != MemoryOperation::COPY)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Cannot perform a non-read operation on constant memory", mem->to_string());

    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToElementOffset(it, method, tmpIndex, srcInfo.local, *srcInfo.mappedRegisterOrConstant, mem);
    // TODO check whether index is guaranteed to be in range [0, 16[
    auto elementType = srcInfo.convertedRegisterType ? *srcInfo.convertedRegisterType :
                                                       srcInfo.mappedRegisterOrConstant->type.getElementType();
    auto wholeRegister =
        copiesWholeRegister(mem->getNumEntries(), mem->getDestinationElementType(), *srcInfo.convertedRegisterType);
    Value tmpVal(UNDEFINED_VALUE);
    if(mem->op == MemoryOperation::COPY && wholeRegister)
        // there is no need to calculate the index, if we copy the whole object
        tmpVal = *srcInfo.convertedRegisterType;
    else
    {
        tmpVal = method.addNewLocal(elementType, "%lowered_constant");
        it = insertVectorExtraction(it, method, *srcInfo.mappedRegisterOrConstant, tmpIndex, tmpVal);
    }

    if(srcInfo.mappedRegisterOrConstant && !srcInfo.mappedRegisterOrConstant->hasLocal())
    {
        if(mem->op == MemoryOperation::COPY)
        {
            if(!wholeRegister && mem->getNumEntries() != INT_ONE)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Lowering copy with more than 1 entry is not yet implemented", mem->to_string());
            it.reset(new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), tmpVal));
            logging::debug() << "Replaced memory copy from constant memory to memory write of constant value: "
                             << it->to_string() << logging::endl;
            return mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
        }
        if(mem->op == MemoryOperation::READ)
        {
            it.reset(new MoveOperation(mem->getDestination(), tmpVal));
            logging::debug() << "Replaced loading of constant memory with constant literal: " << it->to_string()
                             << logging::endl;
            return it;
        }
    }
    if(srcInfo.convertedRegisterType)
    {
        if(mem->op == MemoryOperation::READ)
        {
            it.reset(new MoveOperation(mem->getDestination(), tmpVal));
            logging::debug() << "Replaced loading of constant memory with vector rotation of register: "
                             << it->to_string() << logging::endl;
            return it;
        }
        if(mem->op == MemoryOperation::COPY)
        {
            if(!wholeRegister && mem->getNumEntries() != INT_ONE)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Lowering copy with more than 1 entry is not yet implemented", mem->to_string());
            it.reset(new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), tmpVal));
            it = mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
            logging::debug() << "Replaced copying from constant memory with vector rotation and writing of memory: "
                             << it->to_string() << logging::endl;
            return it;
        }
    }
    auto constant = getConstantValue(mem->getSource());
    if(constant)
    {
        if(mem->op == MemoryOperation::COPY)
        {
            if(mem->getNumEntries() != INT_ONE)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Lowering copy with more than 1 entry is not yet implemented", mem->to_string());
            // since a copy always involves another memory object, this rewrite is picked up when the other
            // object is processed
            it.reset(new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), *constant));
            it = mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
            logging::debug() << "Replaced memory copy from constant memory to memory write of constant value: "
                             << it->to_string() << logging::endl;
            return it;
        }
        else
        {
            it.reset(new MoveOperation(mem->getOutput().value(), *constant));
            logging::debug() << "Replaced loading of constant memory with constant literal: " << it->to_string()
                             << logging::endl;
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
static InstructionWalker lowerMemoryReadWriteToRegister(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    const auto& loweredInfo = mem->op == MemoryOperation::READ ? srcInfo : destInfo;
    if(!loweredInfo.mappedRegisterOrConstant)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Cannot map memory location to register without mapping register specified", mem->to_string());
    const auto& loweredRegister = loweredInfo.mappedRegisterOrConstant.value();
    const auto local = loweredInfo.local;
    Value tmpIndex = UNDEFINED_VALUE;
    // TODO does this also handle whole-object access (e.g. for scalar/vector memory areas) ??
    // TODO check whether index is guaranteed to be in range [0, 16[
    it = insertAddressToElementOffset(it, method, tmpIndex, local, loweredRegister, mem);
    if(mem->op == MemoryOperation::READ)
    {
        it = insertVectorExtraction(it, method, loweredRegister, tmpIndex, mem->getDestination());
    }
    else if(mem->op == MemoryOperation::WRITE)
    {
        // TODO need special handling for inserting multiple elements to set all new elements
        it = insertVectorInsertion(it, method, loweredRegister, tmpIndex, mem->getSource());
    }
    else if(mem->op == MemoryOperation::FILL && mem->getSource().type.isScalarType())
    {
        it = insertReplication(it, mem->getSource(), loweredRegister);
    }
    else
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
    logging::debug() << "Replaced access to stack allocation '" << it->to_string()
                     << "' with: " << it.copy().previousInBlock()->to_string() << logging::endl;
    return it.erase();
}

static InstructionWalker lowerMemoryCopyToRegister(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    if(srcInfo.local == destInfo.local)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Copy from and to same register lowered memory area is not supported", mem->to_string());
    if(mem->op != MemoryOperation::COPY)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
    if(destInfo.type == MemoryType::QPU_REGISTER_READONLY)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Copy into read-only registers is not supported", mem->to_string());

    auto wholeRegister =
        copiesWholeRegister(mem->getNumEntries(), mem->getDestinationElementType(), *srcInfo.convertedRegisterType);

    if(!wholeRegister && mem->getNumEntries() != INT_ONE)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Lowering copy with more than 1 entry is not yet implemented", mem->to_string());

    logging::debug() << "Lowering copy with register-mapped memory: " << mem->to_string() << logging::endl;

    Value tmpIndex = UNDEFINED_VALUE;
    if(srcInfo.mappedRegisterOrConstant)
    {
        // TODO check whether index is guaranteed to be in range [0, 16[
        Value tmp(UNDEFINED_VALUE);
        if(wholeRegister)
            tmp = *srcInfo.mappedRegisterOrConstant;
        else
        {
            tmp = method.addNewLocal(mem->getSourceElementType());
            it = insertVectorExtraction(it, method, *srcInfo.mappedRegisterOrConstant, tmpIndex, tmp);
        }
        it.reset(new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), tmp));
        return mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
    }
    if(destInfo.mappedRegisterOrConstant)
    {
        // TODO is this ever called?? copying into register (from anywhere should be handled smewhere else)
        throw CompilationError(CompilationStep::NORMALIZER,
            "lowerMemoryCopyToRegister should not be called to copy into register", mem->to_string());
        // TODO need special handling for inserting multiple elements to set all new elements
        auto tmp = method.addNewLocal(mem->getDestinationElementType());
        it.emplace(new MemoryInstruction(MemoryOperation::READ, tmp, mem->getSource()));
        it = mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
        it = insertVectorInsertion(it, method, *destInfo.mappedRegisterOrConstant, tmpIndex, mem->getSource());
        return it.erase();
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
}

static InstructionWalker insertToInVPMAreaOffset(
    Method& method, InstructionWalker it, Value& out, const MemoryInfo& info, const MemoryInstruction* mem)
{
    if(info.ranges)
    {
        auto range = std::find_if(info.ranges->begin(), info.ranges->end(),
            [&](const MemoryAccessRange& range) -> bool { return range.memoryInstruction == it; });
        if(range == info.ranges->end())
            throw CompilationError(CompilationStep::NORMALIZER,
                "Failed to find memory access range for VPM cached memory access", mem->to_string());
        return insertAddressToWorkItemSpecificOffset(it, method, out, const_cast<MemoryAccessRange&>(*range));
    }
    return insertAddressToStackOffset(it, method, out, info.local, info.type, mem);
}

/*
 * Tries to map the given memory location into VPM
 *
 * This is applicable for private (stack) or local memory.
 *
 * NOTE: A memory location can only be lowered into VPM if all access to it can be lowered to VPM
 * NOTE: This is to be preferred over keeping the memory location in RAM
 */
static InstructionWalker lowerMemoryReadToVPM(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    // Need to make sure addressing is still correct!
    if(srcInfo.type == MemoryType::VPM_PER_QPU && !srcInfo.local->is<StackAllocation>())
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of per-QPU memory buffer", srcInfo.local->to_string());
    if(!srcInfo.area)
        throw CompilationError(CompilationStep::NORMALIZER, "Cannot lower into VPM without VPM area", mem->to_string());

    if(srcInfo.type == MemoryType::VPM_PER_QPU)
        logging::debug() << "Lowering read of stack allocation into VPM: " << mem->to_string() << logging::endl;
    else
        logging::debug() << "Lowering read of shared local memory into VPM: " << mem->to_string() << logging::endl;

    Value inAreaOffset = UNDEFINED_VALUE;
    it = insertToInVPMAreaOffset(method, it, inAreaOffset, srcInfo, mem);
    if(mem->op == MemoryOperation::READ)
    {
        it = method.vpm->insertReadVPM(method, it, mem->getDestination(), srcInfo.area, false, inAreaOffset);
        return it.erase();
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case to lower reading of memory into VPM", mem->to_string());
}

static InstructionWalker lowerMemoryWriteToVPM(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    if(destInfo.type == MemoryType::VPM_PER_QPU && !destInfo.local->is<StackAllocation>())
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of per-QPU memory buffer", destInfo.local->to_string());
    if(!destInfo.area)
        throw CompilationError(CompilationStep::NORMALIZER, "Cannot lower into VPM without VPM area", mem->to_string());

    if(destInfo.type == MemoryType::VPM_PER_QPU)
        logging::debug() << "Lowering write to stack allocation into VPM: " << mem->to_string() << logging::endl;
    else
        logging::debug() << "Lowering write to shared local memory into VPM: " << mem->to_string() << logging::endl;

    Value inAreaOffset = UNDEFINED_VALUE;
    it = insertToInVPMAreaOffset(method, it, inAreaOffset, destInfo, mem);
    if(mem->op == MemoryOperation::WRITE)
    {
        it = method.vpm->insertWriteVPM(method, it, mem->getSource(), destInfo.area, false, inAreaOffset);
        return it.erase();
    }
    if(mem->op == MemoryOperation::FILL)
    {
        logging::error() << "Destination: " << destInfo.local->to_string() << " - "
                         << static_cast<unsigned>(destInfo.type) << " - "
                         << (destInfo.area ? destInfo.area->to_string() : "") << logging::endl;
        throw CompilationError(
            CompilationStep::NORMALIZER, "Filling VPM area is not yet implemented", mem->to_string());
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case to lower writing of memory into VPM", mem->to_string());
}

/*
 * Maps a single memory read to a TMU load
 *
 * NOTE: Memory locations loaded via TMU MUST NOT be written to by the same kernel (even on a different QPU)!
 */
static InstructionWalker loadMemoryViaTMU(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    logging::debug() << "Loading from read-only memory via TMU: " << mem->to_string() << logging::endl;
    if(mem->op == MemoryOperation::READ)
    {
        it = periphery::insertReadVectorFromTMU(
            method, it, mem->getDestination(), mem->getSource(), srcInfo.tmuFlag ? periphery::TMU1 : periphery::TMU0);
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
static InstructionWalker accessMemoryInRAMViaVPM(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
{
    logging::debug() << "Mapping access to memory located in RAM: " << mem->to_string() << logging::endl;
    switch(mem->op)
    {
    case MemoryOperation::FILL:
    {
        if(!mem->getNumEntries().isLiteralValue())
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Filling dynamically sized memory is not yet implemented", mem->to_string());
        uint64_t numCopies = mem->getNumEntries().getLiteralValue()->unsignedInt();
        if(numCopies > std::numeric_limits<unsigned>::max())
            throw CompilationError(CompilationStep::OPTIMIZER, "Cannot fill more than 4GB of data", mem->to_string());
        // TODO could optimize (e.g. for zero-initializers) by writing several bytes at once
        it = method.vpm->insertWriteVPM(method, it, mem->getSource(), nullptr, false);
        it = method.vpm->insertFillRAM(method, it, mem->getDestination(), mem->getSourceElementType(),
            static_cast<unsigned>(numCopies), nullptr, false);
        auto* dest = mem->getDestination().hasLocal() ? mem->getDestination().local()->getBase(true) : nullptr;
        if(dest && dest->is<Parameter>())
            const_cast<Parameter*>(dest->as<const Parameter>())->decorations =
                add_flag(dest->as<const Parameter>()->decorations, ParameterDecorations::OUTPUT);
        break;
    }
    case MemoryOperation::READ:
    {
        it = periphery::insertReadDMA(
            method, it, mem->getDestination(), mem->getSource(), true /* need to lock mutex for shared scratch area */);
        auto* src = mem->getSource().hasLocal() ? mem->getSource().local()->getBase(true) : nullptr;
        if(src && src->is<Parameter>())
            const_cast<Parameter*>(src->as<const Parameter>())->decorations =
                add_flag(src->as<const Parameter>()->decorations, ParameterDecorations::INPUT);
        break;
    }
    case MemoryOperation::WRITE:
    {
        it = periphery::insertWriteDMA(
            method, it, mem->getSource(), mem->getDestination(), true /* need to lock mutex for shared scratch area */);
        auto* dest = mem->getDestination().hasLocal() ? mem->getDestination().local()->getBase(true) : nullptr;
        if(dest && dest->is<Parameter>())
            const_cast<Parameter*>(dest->as<const Parameter>())->decorations =
                add_flag(dest->as<const Parameter>()->decorations, ParameterDecorations::OUTPUT);
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

static InstructionWalker mapMemoryCopy(
    Method& method, InstructionWalker it, MemoryInstruction* mem, const MemoryInfo& srcInfo, const MemoryInfo& destInfo)
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
    bool destInRegister = destInfo.type == MemoryType::QPU_REGISTER_READWRITE;
    bool srcInVPM = srcInfo.type == MemoryType::VPM_PER_QPU || srcInfo.type == MemoryType::VPM_SHARED_ACCESS;
    bool srcInRAM = srcInfo.type == MemoryType::RAM_LOAD_TMU || srcInfo.type == MemoryType::RAM_READ_WRITE_VPM;
    bool destInVPM = destInfo.type == MemoryType::VPM_PER_QPU || destInfo.type == MemoryType::VPM_SHARED_ACCESS;
    bool destInRAM = destInfo.type == MemoryType::RAM_LOAD_TMU || destInfo.type == MemoryType::RAM_READ_WRITE_VPM;

    auto* src = mem->getSource().hasLocal() ? mem->getSource().local()->getBase(true) : nullptr;
    if(src && src->is<Parameter>())
        const_cast<Parameter*>(src->as<const Parameter>())->decorations =
            add_flag(src->as<const Parameter>()->decorations, ParameterDecorations::INPUT);
    auto* dest = mem->getDestination().hasLocal() ? mem->getDestination().local()->getBase(true) : nullptr;
    if(dest && dest->is<Parameter>())
        const_cast<Parameter*>(dest->as<const Parameter>())->decorations =
            add_flag(dest->as<const Parameter>()->decorations, ParameterDecorations::OUTPUT);

    if(srcInVPM && destInVPM)
    {
        // copy from VPM into VPM -> VPM read + VPM write
        logging::debug() << "Mapping copy from/to VPM to VPM read and VPM write: " << mem->to_string() << logging::endl;

        if(mem->getNumEntries() != INT_ONE)
            // TODO could for static count insert that number of reads/writes, for dynamic need a loop!
            throw CompilationError(CompilationStep::NORMALIZER,
                "Copying within VPM with more than 1 entries is not yet implemented", mem->to_string());
        auto tmpVal = method.addNewLocal(mem->getSourceElementType(), "%vpm_copy_tmp");
        it.emplace(new MemoryInstruction(MemoryOperation::READ, tmpVal, mem->getSource()));
        it = mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
        it.reset(new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), tmpVal));
        return mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
    }
    else if(srcInVPM && destInRAM)
    {
        // copy from VPM into RAM -> DMA write
        logging::debug() << "Mapping copy from VPM into RAM to DMA write: " << mem->to_string() << logging::endl;
        Value inAreaOffset = UNDEFINED_VALUE;
        it = insertToInVPMAreaOffset(method, it, inAreaOffset, srcInfo, mem);
        it = method.vpm->insertWriteRAM(method, it, mem->getDestination(), mem->getSourceElementType(), srcInfo.area,
            false, inAreaOffset, mem->getNumEntries());
        return it.erase();
    }
    else if(srcInRAM && destInVPM)
    {
        // copy from RAM into VPM -> DMA read
        logging::debug() << "Mapping copy from RAM into VPM to DMA read: " << mem->to_string() << logging::endl;
        Value inAreaOffset = UNDEFINED_VALUE;
        it = insertToInVPMAreaOffset(method, it, inAreaOffset, destInfo, mem);
        it = method.vpm->insertReadRAM(method, it, mem->getSource(), mem->getSourceElementType(), destInfo.area, false,
            inAreaOffset, mem->getNumEntries());
        return it.erase();
    }
    else if(srcInRAM && destInRAM)
    {
        // copy from RAM into RAM -> DMA read + DMA write
        if(!mem->getNumEntries().isLiteralValue())
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Copying dynamically sized memory within RAM is not yet implemented", mem->to_string());
        uint64_t numBytes = mem->getNumEntries().getLiteralValue()->unsignedInt() *
            (mem->getSourceElementType().getScalarBitCount() * mem->getSourceElementType().getVectorWidth()) / 8;
        if(numBytes > std::numeric_limits<unsigned>::max())
            throw CompilationError(CompilationStep::OPTIMIZER, "Cannot copy more than 4GB of data", mem->to_string());
        logging::debug() << "Mapping copy from RAM into RAM to DMA read and DMA write: " << mem->to_string()
                         << logging::endl;
        it = method.vpm->insertCopyRAM(
            method, it, mem->getDestination(), mem->getSource(), static_cast<unsigned>(numBytes));
        return it.erase();
    }
    else if(destInRegister && destInfo.convertedRegisterType)
    {
        // copy from VPM/RAM into register -> read from VPM/RAM + write to register
        logging::debug() << "Mapping copy from VPM/RAM into register to read from VPM/RAM and register insertion: "
                         << mem->to_string() << logging::endl;
        // TODO some general version
        if(copiesWholeRegister(mem->getNumEntries(), mem->getSourceElementType(), *destInfo.convertedRegisterType))
        {
            // e.g. for copying 32 bytes into float[8] register -> just read 1 float16 vector
            it.reset(new MemoryInstruction(MemoryOperation::READ, *destInfo.mappedRegisterOrConstant,
                Value(mem->getSource().local(), destInfo.convertedRegisterType->toPointerType())));
            return mapMemoryAccess(method, it, it.get<MemoryInstruction>(), srcInfo, destInfo);
        }
    }
    else
    {
        logging::error() << "Source: " << (srcInfo.local ? srcInfo.local->to_string() : "?") << " - "
                         << static_cast<unsigned>(srcInfo.type) << " - "
                         << (srcInfo.area ? srcInfo.area->to_string() : "") << logging::endl;

        logging::error() << "Destination: " << (destInfo.local ? destInfo.local->to_string() : "?") << " - "
                         << static_cast<unsigned>(destInfo.type) << " - "
                         << (destInfo.area ? destInfo.area->to_string() : "") << logging::endl;

        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case for handling memory copy", mem->to_string());
    }

    throw CompilationError(CompilationStep::NORMALIZER, "Need to be re-written", mem->to_string());
}
