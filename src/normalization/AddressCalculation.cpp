/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AddressCalculation.h"

#include "../Locals.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/operators.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::normalization;
using namespace vc4c::operators;

MemoryType normalization::toMemoryType(periphery::VPMUsage usage)
{
    switch(usage)
    {
    case periphery::VPMUsage::SCRATCH:
    case periphery::VPMUsage::LOCAL_MEMORY:
    case periphery::VPMUsage::MEMORY_CACHE:
        return MemoryType::VPM_SHARED_ACCESS;
    case periphery::VPMUsage::REGISTER_SPILLING:
    case periphery::VPMUsage::STACK:
        return MemoryType::VPM_PER_QPU;
    }
    throw CompilationError(CompilationStep::NORMALIZER,
        "Unknown VPM usage type to map to memory type: ", std::to_string(static_cast<int>(usage)));
}

InstructionWalker normalization::insertAddressToOffset(
    InstructionWalker it, Method& method, Value& out, const Local* baseAddress, const MemoryInstruction* mem)
{
    auto ptrVal = mem->op == MemoryOperation::READ ? mem->getSource() : mem->getDestination();
    auto indexOp = dynamic_cast<const Operation*>(ptrVal.getSingleWriter());
    if(!indexOp)
    {
        // for stores, the store itself is also a write instruction
        auto writers = ptrVal.local()->getUsers(LocalUse::Type::WRITER);
        if(writers.size() == 2 && writers.find(mem) != writers.end())
        {
            writers.erase(mem);
            indexOp = dynamic_cast<const Operation*>(*writers.begin());
        }
    }
    if(ptrVal.hasLocal(baseAddress))
    {
        // trivial case, the offset is zero
        out = INT_ZERO;
    }
    else if(indexOp && indexOp->readsLocal(baseAddress) && indexOp->op == OP_ADD)
    {
        // for simple version where the index is base address + offset, simple use the offset directly
        out = indexOp->getFirstArg().hasLocal(baseAddress) ? indexOp->getSecondArg().value() : indexOp->getFirstArg();
    }
    else
    {
        // for more complex versions, calculate offset by subtracting base address from result
        // address
        out = assign(it, baseAddress->type, "%pointer_diff") = ptrVal - baseAddress->createReference();
    }
    return it;
}

InstructionWalker normalization::insertAddressToStackOffset(InstructionWalker it, Method& method, Value& out,
    const Local* baseAddress, MemoryType type, const MemoryInstruction* mem)
{
    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToOffset(it, method, tmpIndex, baseAddress, mem);
    if(type == MemoryType::VPM_PER_QPU)
    {
        // size of one stack-frame in bytes
        auto stackByteSize = periphery::VPM::getVPMStorageType(baseAddress->type.getElementType()).getPhysicalWidth();
        // add offset of stack-frame
        Value stackOffset = method.addNewLocal(TYPE_VOID.toPointerType(), "%stack_offset");
        Value tmp = method.addNewLocal(baseAddress->type);
        assign(it, stackOffset) = mul24(Value(Literal(stackByteSize), TYPE_INT16), Value(REG_QPU_NUMBER, TYPE_INT8));
        out = assign(it, TYPE_VOID.toPointerType(), "%stack_offset") = tmpIndex + stackOffset;
    }
    else
    {
        out = tmpIndex;
    }
    return it;
}

InstructionWalker normalization::insertAddressToElementOffset(InstructionWalker it, Method& method, Value& out,
    const Local* baseAddress, const Value& container, const MemoryInstruction* mem)
{
    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToOffset(it, method, tmpIndex, baseAddress, mem);
    // the index (as per index calculation) is in bytes, but we need index in elements, so divide by element size
    out = assign(it, TYPE_VOID.toPointerType(), "%element_offset") =
        tmpIndex / Literal(container.type.getElementType().getPhysicalWidth());
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
            auto newResult = method.addNewLocal(prevResult->first.type);
            auto newFlags = intersect_flags(prevResult->second, valIt->second);
            referenceIt.emplace(new Operation(OP_ADD, newResult, prevResult->first, valIt->first));
            referenceIt->addDecorations(newFlags);
            referenceIt.nextInBlock();
            prevResult = std::make_pair(newResult, newFlags);
        }
        else
            prevResult = std::make_pair(valIt->first, valIt->second);
        valIt = addedValues.erase(valIt);
    }
    return prevResult;
}

std::string MemoryAccessRange::to_string() const
{
    return (memoryInstruction->to_string() +
               (memoryInstruction->writesRegister(REG_VPM_DMA_LOAD_ADDR) ? " - read " : " - write ")) +
        (memoryObject->to_string() +
            (groupUniformAddressParts.empty() ? " with" : " with work-group uniform offset and") +
            " dynamic element range [") +
        (std::to_string(offsetRange.minValue) + ", ") + (std::to_string(offsetRange.maxValue) + "]");
}

InstructionWalker normalization::insertAddressToWorkItemSpecificOffset(
    InstructionWalker it, Method& method, Value& out, MemoryAccessRange& range)
{
    auto dynamicParts = combineAdditions(method, it, range.dynamicAddressParts);
    out = dynamicParts->first;
    if(range.typeSizeShift)
        out = assign(it, dynamicParts->first.type) =
            (dynamicParts->first << (*range.typeSizeShift)->assertArgument(1), dynamicParts->second);
    return it;
}

InstructionWalker normalization::insertAddressToWorkGroupUniformOffset(
    InstructionWalker it, Method& method, Value& out, MemoryAccessRange& range)
{
    auto uniformParts = combineAdditions(method, it, range.groupUniformAddressParts);
    out = uniformParts->first;
    if(range.typeSizeShift)
        out = assign(it, uniformParts->first.type) =
            (uniformParts->first << (*range.typeSizeShift)->assertArgument(1), uniformParts->second);
    static auto checkPointer = [](const Value& arg) -> bool { return arg.type.isPointerType(); };
    if(std::all_of(
           range.baseAddressAdd->getArguments().begin(), range.baseAddressAdd->getArguments().end(), checkPointer) ||
        std::none_of(
            range.baseAddressAdd->getArguments().begin(), range.baseAddressAdd->getArguments().end(), checkPointer))
        throw CompilationError(CompilationStep::NORMALIZER, "Cannot determine base address of addition",
            range.baseAddressAdd->to_string());
    auto baseAddrIt = std::find_if(
        range.baseAddressAdd->getArguments().begin(), range.baseAddressAdd->getArguments().end(), checkPointer);
    out = assign(it, range.baseAddressAdd->getOutput()->type) =
        (*baseAddrIt + out, uniformParts->second, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    return it;
}

bool LocalUsageOrdering::operator()(const Local* l1, const Local* l2) const
{
    // prefer more usages over less usages
    // since there is always only 1 writer for the local address, we prefer this over only counting readers for
    // performance reasons
    // TODO is this the correct way to do this? E.g. is there one usage per memory access?
    if(l1->getUsers().size() > l2->getUsers().size())
        return true;
    if(l1->getUsers().size() == l2->getUsers().size())
        return l1 < l2;
    return false;
}