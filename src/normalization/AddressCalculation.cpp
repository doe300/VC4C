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

static BaseAndOffset findOffset(const Value& val)
{
    if(!val.hasLocal())
        return BaseAndOffset();
    const LocalUser* writer = val.getSingleWriter();
    if(writer != nullptr)
    {
        const Optional<Value> offset = writer->precalculate(8);
        if(offset && offset->isLiteralValue())
        {
            return BaseAndOffset(NO_VALUE, offset->getLiteralValue()->signedInt());
        }
    }
    return BaseAndOffset();
}

BaseAndOffset normalization::findBaseAndOffset(const Value& val)
{
    // TODO add support for offsets via getlocal/global_id, etc.
    // need to the set base to addr + offset and the offset to the offset of the offset (e.g. param[get_local_id(0) +
    // 7])  but how to determine?
    if(!val.hasLocal())
        return BaseAndOffset();
    if(val.local()->is<Parameter>() || val.local()->is<Global>() || val.local()->is<StackAllocation>())
        return BaseAndOffset(val, 0);

    // follow the references
    const Local* ref = val.local()->getBase(false);
    if(ref != val.local())
        return findBaseAndOffset(ref->createReference());
    if(val.local()->reference.first != nullptr && val.local()->reference.second != ANY_ELEMENT)
        return BaseAndOffset(val.local()->reference.first->createReference(), val.local()->reference.second);

    const auto writers = val.local()->getUsers(LocalUse::Type::WRITER);
    if(writers.size() != 1)
        return BaseAndOffset();

    // The reader can be one of several valid cases:
    // 1. a move from another local -> need to follow the move
    if(dynamic_cast<const MoveOperation*>(*writers.begin()) != nullptr)
        return findBaseAndOffset(dynamic_cast<const MoveOperation*>(*writers.begin())->getSource());
    const auto& args = (*writers.begin())->getArguments();
    // 2. an addition with a local and a literal -> the local is the base, the literal the offset
    if(dynamic_cast<const Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
        std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.hasLocal(); }) &&
        std::any_of(
            args.begin(), args.end(), [](const Value& arg) -> bool { return arg.getLiteralValue().has_value(); }))
    {
        return BaseAndOffset(
            std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.hasLocal(); })
                ->local()
                ->getBase(false)
                ->createReference(),
            static_cast<int32_t>((*std::find_if(args.begin(), args.end(),
                                      [](const Value& arg) -> bool { return arg.getLiteralValue().has_value(); }))
                                     .getLiteralValue()
                                     ->signedInt() /
                val.type.getElementType().getPhysicalWidth()));
    }

    // 3. an addition with two locals -> one is the base, the other the calculation of the literal
    if(dynamic_cast<const Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
        std::all_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.hasLocal(); }))
    {
        const auto offset0 = findOffset(args[0]);
        const auto offset1 = findOffset(args[1]);
        if(offset0.offset && args[1].hasLocal())
            return BaseAndOffset(args[1].local()->getBase(false)->createReference(),
                static_cast<int32_t>(offset0.offset.value() / val.type.getElementType().getPhysicalWidth()));
        if(offset1.offset && args[0].hasLocal())
            return BaseAndOffset(args[0].local()->getBase(false)->createReference(),
                static_cast<int32_t>(offset1.offset.value() / val.type.getElementType().getPhysicalWidth()));
    }
    /*
        if(writers.size() == 1)
        {
            // couldn't find literal offset for any direct base, try with arbitrary values
            ref = val.local->getBase(true);
            Optional<Value> offset = NO_VALUE;
            for(const auto& arg : (*writers.begin())->getArguments())
            {
                if(ref != nullptr && arg.hasLocal() && arg.local->getBase(false) == ref)
                    // skip finding the same base again
                    continue;
                auto tmp = findBaseAndOffset(arg);
                if(tmp.base && tmp.base->local->getBase(true) == ref && tmp.offset.is(0))
                    // this parameter is the base itself, is already handled
                    continue;
                // TODO how to combine the offsets?
                // TODO also need to handle non-addition of offsets (e.g. ptr = base + (offset + size * i))
                logging::debug() << "Found offset of " << tmp.base.to_string() << " + "
                                 << (tmp.offset ? tmp.offset.value() : -1) << logging::endl;
                logging::debug() << "Found offset of " << tmp.base.to_string() << " with expression: "
                                 << vc4c::Expression::createExpression(*(*writers.begin())).to_string() <<
       logging::endl;
            }
            // TODO why is this called twice? The whole function, from outside
            logging::debug() << "Found base and non-literal offset: " << ref->to_string() << " - " << offset.to_string()
                             << logging::endl;
            if(ref && (ref->residesInMemory() || (ref->is<Parameter>() && ref->type.isPointerType())))
                return BaseAndOffset(ref->createReference(), {});
        }
    */

    return BaseAndOffset();
}

MemoryType normalization::toMemoryType(periphery::VPMUsage usage)
{
    switch(usage)
    {
    case periphery::VPMUsage::SCRATCH:
    case periphery::VPMUsage::LOCAL_MEMORY:
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