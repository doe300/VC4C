/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AddressCalculation.h"

#include "../InstructionWalker.h"
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <functional>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::normalization;
using namespace vc4c::operators;

MemoryAccessType normalization::toMemoryAccessType(periphery::VPMUsage usage)
{
    switch(usage)
    {
    case periphery::VPMUsage::SCRATCH:
    case periphery::VPMUsage::LOCAL_MEMORY:
        return MemoryAccessType::VPM_SHARED_ACCESS;
    case periphery::VPMUsage::REGISTER_SPILLING:
    case periphery::VPMUsage::STACK:
        return MemoryAccessType::VPM_PER_QPU;
    }
    throw CompilationError(CompilationStep::NORMALIZER,
        "Unknown VPM usage type to map to memory type: ", std::to_string(static_cast<int>(usage)));
}

/**
 * Calculates the offset (in bytes) from the given address to the given baseAddress and converts negative values to
 * large positive values.
 *
 * By unconditionally resetting the highest bit, "small" negative numbers will be converted to "large" positive numbers.
 *
 * Since we can only address 1GB of memory anyway, the largest negative number that can be created is -1GB
 * (-1073741824). When toggling the highest bit, this results in 1GB which is the largest value. Similar is true for all
 * lower negative numbers, e.g. -42 -> 2147483606 being larger than any actual supported address.
 */
NODISCARD static InstructionWalker insertAddressToAbsoluteOffset(InstructionWalker it, Method& method, Value& out,
    const Local* baseAddress, const intermediate::MemoryInstruction* mem, const Value& ptrValue)
{
    auto tmp = assign(it, ptrValue.type, "%conditional_address_offset") = ptrValue - baseAddress->createReference();
    out = assign(it, ptrValue.type) = (tmp & 0x7FFFFFFF_val);
    return it;
}

/**
 * Functional type to be used to insert code in the address selection
 *
 * @param it the current instruction walker position
 * @param loc the base address that is currently processed
 * @param cond the condition in which the given base address is chosen. Any additional value selection should use the
 * same condition
 *
 * @return the new instruction walker position
 */
using AdditionalBehavior = std::function<InstructionWalker(InstructionWalker it, const Local* loc, ConditionCode cond)>;

NODISCARD static InstructionWalker insertAddressToOffsetInner(InstructionWalker it, Method& method, Value& out,
    const tools::SmallSortedPointerSet<const Local*>& baseAddresses, const MemoryInstruction* mem,
    const Value& ptrValue, const AdditionalBehavior& additionalBehavior)
{
    auto indexOp = dynamic_cast<const Operation*>(ptrValue.getSingleWriter());
    if(!indexOp)
    {
        // for stores, the store itself is also a write instruction
        auto writers = ptrValue.local()->getUsers(LocalUse::Type::WRITER);
        if(writers.size() == 2 && writers.find(mem) != writers.end())
        {
            writers.erase(mem);
            indexOp = dynamic_cast<const Operation*>(*writers.begin());
        }
    }
    if(baseAddresses.size() == 1)
    {
        // default case, always used for unconditional memory access
        auto baseAddress = (*baseAddresses.begin());
        if(ptrValue.hasLocal(baseAddress))
            // trivial case, the offset is zero
            out = INT_ZERO;
        else if(indexOp && indexOp->readsLocal(baseAddress) && indexOp->op == OP_ADD)
            // for simple version where the index is base address + offset, simple use the offset directly
            out =
                indexOp->getFirstArg().hasLocal(baseAddress) ? indexOp->getSecondArg().value() : indexOp->getFirstArg();
        else
            // for more complex versions, calculate offset by subtracting base address from result
            // address
            out = assign(it, baseAddress->type, "%pointer_diff") = ptrValue - baseAddress->createReference();
        if(additionalBehavior)
            // only 1 candidate, run for this always
            it = additionalBehavior(it, baseAddress, COND_ALWAYS);
        return it;
    }

    // we need to find the base address from the given candidates which the pointer value refers to.
    // This is the largest base address smaller than the given pointer value
    auto addressIt = baseAddresses.begin();
    Value minimumOffset(UNDEFINED_VALUE);
    it = insertAddressToAbsoluteOffset(it, method, minimumOffset, *addressIt, mem, ptrValue);
    auto baseAddress = assign(it, (*addressIt)->type) = (*addressIt)->createReference();
    if(additionalBehavior)
        // the first address is always selected
        it = additionalBehavior(it, *addressIt, COND_ALWAYS);

    for(++addressIt; addressIt != baseAddresses.end(); ++addressIt)
    {
        Value offset(UNDEFINED_VALUE);
        it = insertAddressToAbsoluteOffset(it, method, offset, *addressIt, mem, ptrValue);

        // determine the lower, but still positive offset of the current and the tracked minimum offset
        auto cond = assignNop(it) = as_signed{offset} < as_signed{minimumOffset};
        assign(it, minimumOffset) = (offset, cond);
        assign(it, baseAddress) = ((*addressIt)->createReference(), cond);
        if(additionalBehavior)
            // only run if the condition is given
            it = additionalBehavior(it, *addressIt, cond);
    }
    out = assign(it, ptrValue.type, "%pointer_diff") = minimumOffset;
    return it;
}

InstructionWalker normalization::insertAddressToOffset(InstructionWalker it, Method& method, Value& out,
    const tools::SmallSortedPointerSet<const Local*>& baseAddresses, const intermediate::MemoryInstruction* mem,
    const Value& ptrValue)
{
    return insertAddressToOffsetInner(it, method, out, baseAddresses, mem, ptrValue, {});
}

InstructionWalker normalization::insertAddressToStackOffset(InstructionWalker it, Method& method, Value& out,
    const Local* baseAddress, MemoryAccessType type, const MemoryInstruction* mem, const Value& ptrValue)
{
    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToOffset(it, method, tmpIndex, {baseAddress}, mem, ptrValue);
    if(type == MemoryAccessType::VPM_PER_QPU)
    {
        // size of one stack-frame in bytes
        auto stackByteSize = periphery::VPM::getVPMStorageType(baseAddress->type.getElementType()).getInMemoryWidth();
        // add offset of stack-frame
        Value stackOffset = method.addNewLocal(TYPE_VOID_POINTER, "%stack_offset");
        assign(it, stackOffset) = mul24(Value(Literal(stackByteSize), TYPE_INT16), Value(REG_QPU_NUMBER, TYPE_INT8));
        out = assign(it, TYPE_VOID_POINTER, "%stack_offset") = tmpIndex + stackOffset;
    }
    else
    {
        out = tmpIndex;
    }
    return it;
}

InstructionWalker normalization::insertAddressToElementOffset(InstructionWalker it, Method& method, Value& out,
    const FastMap<const Local*, Value>& baseAddressesAndContainers, Value& outContainer, const MemoryInstruction* mem,
    const Value& ptrValue)
{
    tools::SmallSortedPointerSet<const Local*> baseAddresses;
    auto containerType = baseAddressesAndContainers.begin()->second.type;
    for(const auto& entry : baseAddressesAndContainers)
    {
        baseAddresses.emplace(entry.first);
        if(entry.second.type != containerType)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Calculating conditional address into register/constant-lowered memory areas with different types is "
                "not yet implemented",
                it->to_string());
    }

    Value tmpIndex = UNDEFINED_VALUE;
    outContainer = method.addNewLocal(containerType, "%conditional_container");
    it = insertAddressToOffsetInner(it, method, tmpIndex, baseAddresses, mem, ptrValue,
        [&](InstructionWalker it, const Local* loc, ConditionCode cond) -> InstructionWalker {
            // additionally to selecting the base address, we also need to select the lowered container
            assign(it, outContainer) = (baseAddressesAndContainers.at(loc), cond);
            return it;
        });
    // the index (as per index calculation) is in bytes, but we need index in elements, so divide by element size
    out = assign(it, TYPE_VOID_POINTER, "%element_offset") =
        tmpIndex / Literal(containerType.getElementType().getInMemoryWidth());
    return it;
}

static Optional<std::pair<Value, InstructionDecorations>> combineAdditions(
    Method& method, InstructionWalker referenceIt, FastMap<Value, InstructionDecorations>& addedValues)
{
    if(addedValues.empty())
        return {};
    Optional<std::pair<Value, InstructionDecorations>> prevResult;
    auto valIt = addedValues.begin();
    while(valIt != addedValues.end())
    {
        if(prevResult)
        {
            auto newFlags = intersect_flags(prevResult->second, valIt->second);
            auto newResult = assign(referenceIt, prevResult->first.type) = (prevResult->first + valIt->first, newFlags);
            prevResult = std::make_pair(newResult, newFlags);
        }
        else
            prevResult = std::make_pair(valIt->first, valIt->second);
        valIt = addedValues.erase(valIt);
    }
    return prevResult;
}

InstructionWalker normalization::insertAddressToWorkItemSpecificOffset(
    InstructionWalker it, Method& method, Value& out, analysis::MemoryAccessRange& range)
{
    if(range.constantOffset)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Calculating work-item specific offset with constant part is not yet implemented", range.to_string());
    auto dynamicParts = combineAdditions(method, it, range.dynamicAddressParts);
    if(!dynamicParts)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Failed to calculate dynamic parts of work-item specific offset", range.to_string());
    out = dynamicParts->first;
    if(range.typeSizeShift)
        out = assign(it, dynamicParts->first.type) =
            (dynamicParts->first << range.typeSizeShift->assertArgument(1), dynamicParts->second);
    return it;
}
