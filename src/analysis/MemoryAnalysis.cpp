
#include "MemoryAnalysis.h"

#include "../Expression.h"
#include "../GlobalValues.h"
#include "../Profiler.h"
#include "log.h"

#include <cmath>
#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

Optional<ValueRange> MemoryAccessRange::getDynamicAccessByteRange() const
{
    auto offsetRange = ValueRange::getValueRange(dynamicOffset);
    if(accessRange)
        return offsetRange | (offsetRange + *accessRange - 1.0 /* upper bound is inclusive */);
    return {};
}

Optional<ValueRange> MemoryAccessRange::getDynamicAccessElementRange() const
{
    if(accessElementType.isUnknown())
        return {};
    if(auto byteRange = getDynamicAccessByteRange())
    {
        auto tmp = *byteRange / accessElementType.getLogicalWidth();
        return ValueRange(std::floor(tmp.minValue), std::floor(tmp.maxValue));
    }
    return {};
}

static uint32_t getAlignment(const SubExpression& part)
{
    if(auto expr = part.checkExpression())
    {
        if(auto constant = expr->getConstantExpression() & &Value::getLiteralValue)
            return constant.value_or(Literal(1)).unsignedInt();
        if(expr->code == Expression::FAKEOP_UMUL)
        {
            auto constant = (expr->arg0.getConstantExpression() & &Value::getLiteralValue) |
                (expr->arg1.getConstantExpression() & &Value::getLiteralValue);
            return constant.value_or(Literal(1)).unsignedInt();
        }
    }
    if(auto val = part.checkValue())
        return val->getLiteralValue().value_or(Literal(1)).unsignedInt();

    return 1;
}

uint32_t MemoryAccessRange::getAccessAlignment(uint32_t assumedBaseAlignment) const
{
    uint32_t baseAlignment =
        assumedBaseAlignment != 0 ? assumedBaseAlignment : baseAddress->type.getElementType().getInMemoryAlignment();
    uint32_t workGroupAlignment = getAlignment(groupUniformOffset);
    uint32_t dynamicAlignment = getAlignment(dynamicOffset);

    if(workGroupAlignment != 0 && dynamicAlignment != 0)
        return gcd(baseAlignment, gcd(workGroupAlignment, dynamicAlignment));
    if(workGroupAlignment != 0)
        return gcd(baseAlignment, workGroupAlignment);
    if(dynamicAlignment != 0)
        return gcd(baseAlignment, dynamicAlignment);
    return baseAlignment;
}

static bool hasNoOverlaps(const ValueRange& accessRange, const Expression& strideExpression)
{
    // accesses cannot overlap of the stride between them is larger (or equals) than the maximum number of bytes
    // accessed
    if(strideExpression.code != Expression::FAKEOP_UMUL || !strideExpression.hasConstantOperand())
        return false;
    auto accessStride = (strideExpression.arg0.getConstantExpression() | strideExpression.arg1.getConstantExpression())
                            .value()
                            .literal()
                            .unsignedInt();
    return static_cast<double>(accessStride) >= std::max(accessRange.minValue, accessRange.maxValue);
}

bool MemoryAccessRange::hasNoOverlapBetweenAccesses() const
{
    if(!accessRange || !dynamicOffset.checkExpression())
        return false;
    return hasNoOverlaps(*accessRange, *dynamicOffset.checkExpression());
}

bool MemoryAccessRange::mayOverlap(const MemoryAccessRange& other) const
{
    if(!accessRange || !other.accessRange)
        return true;
    if(!dynamicOffset.checkExpression() || !other.dynamicOffset.checkExpression())
        return true;
    if(groupUniformOffset != other.groupUniformOffset)
        return true;
    return !hasNoOverlaps(*accessRange, *other.dynamicOffset.checkExpression()) ||
        !hasNoOverlaps(*other.accessRange, *dynamicOffset.checkExpression());
}

LCOV_EXCL_START
std::string MemoryAccessRange::to_string() const
{
    std::stringstream ss;

    ss << addressWrite->to_string();
    if(addressWrite->writesRegister(REG_VPM_DMA_LOAD_ADDR) ||
        addressWrite.get()->op == intermediate::MemoryOperation::READ)
        ss << " - read ";
    else if(addressWrite->writesRegister(REG_VPM_DMA_STORE_ADDR) ||
        addressWrite.get()->op == intermediate::MemoryOperation::WRITE)
        ss << " - write ";
    else
        ss << " - access ";

    if(accessRange)
        ss << accessRange->to_string(false) << " bytes of ";

    ss << baseAddress->to_string();

    if(!accessElementType.isUnknown())
        ss << " (as " << accessElementType.to_string()
           << " elements, aligned at " + std::to_string(getAccessAlignment()) + " bytes)";

    if(groupUniformOffset)
        ss << " with work-group uniform offset " << groupUniformOffset.to_string();

    if(dynamicOffset)
        ss << " with dynamic offset " << dynamicOffset.to_string();

    return ss.str();
}
LCOV_EXCL_STOP

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

const intermediate::IntermediateInstruction* analysis::getSingleWriter(
    const Value& val, const intermediate::IntermediateInstruction* defaultInst)
{
    auto loc = val.checkLocal();
    if(!loc)
        return nullptr;

    const intermediate::IntermediateInstruction* writer = nullptr;
    for(const auto& w : loc->getUsers(LocalUse::Type::WRITER))
    {
        if(auto memoryInstruction = dynamic_cast<const intermediate::MemoryInstruction*>(w))
        {
            // Store memory instructions count as writers, so ignore them. We explicitly check for "store" (including
            // "fill" and "copy" into), since the pointer value we are searching the writer for might be written by
            // loading a pointer-to-pointer value (as i.e. occurs a lot if the clang front-end is run without any
            // optimization).
            // Or in other words, if the local's memory location is written to, we skip it as writer. If the local
            // itself is written to (i.e. its value is loaded from a memory location), this is a local writer.
            if(memoryInstruction->op != intermediate::MemoryOperation::READ)
                continue;
        }
        if(writer)
        {
            writer = nullptr;
            break;
        }
        writer = w;
    }
    if(!writer)
    {
        // if the value is the parameter/global directly, the address write might not yet exist
        if(loc->residesInMemory())
            return defaultInst;
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case, value does not have exactly 1 writer for '" << val.to_string()
                << "': " << (defaultInst ? defaultInst->to_string() : "(null)") << logging::endl);
        return {};
    }
    return writer;
}

static Optional<ValueRange> getAccessWidthElementRange(const intermediate::MemoryInstruction& memInst)
{
    if(auto constant = memInst.getNumEntries().getConstantValue())
        return ValueRange::getValueRangeFlat(*constant, true);
    if(auto writer = memInst.getNumEntries().getSingleWriter())
    {
        if(auto expr = Expression::createRecursiveExpression(*writer, 8))
            return ValueRange::getValueRange(*expr);
    }
    // TODO error or try to determine differently (e.g. lifetime bounds, array bounds, etc...)
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Could not determine number of accessed entries, falling back to undefined access range: "
            << memInst.to_string() << logging::endl);
    return ValueRange{};
}

static const Local* getBaseAddress(const Local* loc)
{
    if(!loc)
        return nullptr;
    if(loc->residesInMemory() || loc->is<Parameter>())
        return loc;
    if(loc->is<BuiltinLocal>() && loc->as<BuiltinLocal>()->builtinType == BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)
        return loc;

    // the address is determined by phi-nodes
    if(loc->type.getPointerType() && loc->allUsers(LocalUse::Type::WRITER, [](const LocalUser* user) {
           return user && user->hasDecoration(intermediate::InstructionDecorations::PHI_NODE);
       }))
        return loc;
    // the address is determined by some condition, e.g. via ?:-operator
    if(loc->type.getPointerType() && loc->allUsers(LocalUse::Type::WRITER, [](const LocalUser* user) {
           return user && user->hasConditionalExecution();
       }))
        return loc;

    if(auto writer = getSingleWriter(loc->createReference()))
    {
        if(auto source = writer->getMoveSource() & &Value::checkLocal)
            return getBaseAddress(source);
    }
    return nullptr;
}

static Optional<MemoryAccessRange> determineAccessRange(Method& method,
    const intermediate::IntermediateInstruction& inst, TypedInstructionWalker<intermediate::MemoryInstruction> memIt,
    FastMap<const Local*, ValueRange>& knownRanges, const Local* checkBaseAddress = nullptr)
{
    // 1. find writes to memory addresses with work-group uniform part in address values
    if(auto memInst = dynamic_cast<const intermediate::MemoryInstruction*>(&inst))
    {
        const Local* checkLocal = nullptr;
        DataType elementType = TYPE_UNKNOWN;
        switch(memInst->op)
        {
        case intermediate::MemoryOperation::READ:
            checkLocal = memInst->getSource().checkLocal();
            elementType = memInst->getDestinationElementType();
            break;
        case intermediate::MemoryOperation::FILL:
        case intermediate::MemoryOperation::WRITE:
            checkLocal = memInst->getDestination().checkLocal();
            elementType = memInst->getSourceElementType();
            break;
        case intermediate::MemoryOperation::COPY:
            if(checkBaseAddress &&
                (memInst->getSource().hasLocal(checkBaseAddress) ||
                    memInst->getDestination().hasLocal(checkBaseAddress)))
            {
                // is read for either input or output
                checkLocal = checkBaseAddress;
                elementType = memInst->getSourceElementType();
            }
            break;
        }
        if(checkLocal && checkLocal->residesInMemory() && (!checkBaseAddress || checkLocal == checkBaseAddress))
        {
            // direct write of address (e.g. all work items access the same location)
            MemoryAccessRange range;
            range.addressWrite = memIt;
            range.baseAddress = checkLocal;
            range.accessElementType = elementType;
            if(auto elementRange = getAccessWidthElementRange(*memInst))
                range.accessRange = *elementRange * elementType.getLogicalWidth();
            range.groupUniformOffset = INT_ZERO;
            range.dynamicOffset = INT_ZERO;
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found memory access without offset: " << range.to_string() << logging::endl);
            return range;
        }
    }
    auto memInst = memIt.get();
    auto moveSourceLocal = inst.getMoveSource() & &Value::checkLocal;
    auto addressExpression = Expression::createRecursiveExpression(inst, 8);
    if(addressExpression && addressExpression->isMoveExpression())
    {
        // for some very special cases where the instruction is not a move but an add %base, 0
        auto loc = addressExpression->arg0.checkLocal(true);
        if(loc && loc->residesInMemory())
            moveSourceLocal = loc;
    }
    if(moveSourceLocal)
        // either resolves to memory location, phi/conditional memory address write or NULL
        moveSourceLocal = getBaseAddress(moveSourceLocal);
    if(memInst && moveSourceLocal)
    {
        // direct write of address (e.g. all work items access the same location)
        MemoryAccessRange range;
        range.addressWrite = memIt;
        range.baseAddress = moveSourceLocal->getBase(false);
        range.accessElementType = memInst->op == intermediate::MemoryOperation::READ ?
            memInst->getDestinationElementType() :
            memInst->getSourceElementType();
        if(auto elementRange = getAccessWidthElementRange(*memInst))
            range.accessRange = *elementRange * range.accessElementType.getLogicalWidth();
        range.groupUniformOffset = INT_ZERO;
        range.dynamicOffset = INT_ZERO;
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Found memory access without offset: " << range.to_string() << logging::endl);
        return range;
    }
    if(!addressExpression || addressExpression->code != OP_ADD)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine address expression for memory access: " << inst.to_string() << logging::endl);
        return {};
    }

    MemoryAccessRange range;
    range.addressWrite = memIt;

    auto addParts = addressExpression->getAssociativeParts();
    for(const auto& addPart : addParts)
    {
        auto loc = addPart.checkLocal(true);
        if((range.baseAddress = getBaseAddress(loc)))
        {
            // Subtract the base address to get the offset expression
            auto partIt = std::find(addParts.begin(), addParts.end(), addPart);
            if(partIt != addParts.end())
            {
                addParts.erase(partIt);
                while(addParts.size() > 1)
                {
                    addParts.front() =
                        std::make_shared<Expression>(OP_ADD, addParts.front(), addParts.back())->combineWith({});
                    addParts.pop_back();
                }
                if(auto expr = addParts.front().checkExpression())
                    addressExpression = expr;
                else
                    addressExpression =
                        std::make_shared<Expression>(OP_V8MIN, addParts.front(), addParts.front())->combineWith({});
            }
            else
                addressExpression = std::make_shared<Expression>(OP_SUB, addressExpression, addPart)->combineWith({});

            break;
        }
    }

    if(!range.baseAddress)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine base memory address of memory access: " << inst.to_string() << logging::endl);
        return {};
    }

    std::tie(range.dynamicOffset, range.groupUniformOffset) = addressExpression->splitIntoDynamicAndConstantPart(true,
        add_flag(add_flag(ExpressionOptions::ALLOW_FAKE_OPS, ExpressionOptions::RECURSIVE,
                     ExpressionOptions::STOP_AT_BUILTINS),
            ExpressionOptions::SPLIT_GROUP_BUILTINS));

    if(auto elementRange = memInst ? getAccessWidthElementRange(*memInst) : Optional<ValueRange>{})
    {
        DataType elementType = memInst->op == intermediate::MemoryOperation::READ ?
            memInst->getDestinationElementType() :
            memInst->getSourceElementType();
        range.accessRange = *elementRange * elementType.getLogicalWidth();
        range.accessElementType = elementType;
    }

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Found memory access range: " << range.to_string() << logging::endl);
    return range;
}

static Optional<MemoryAccessRange> findAccessRange(Method& method, const Value& val, const Local* baseAddr,
    TypedInstructionWalker<intermediate::MemoryInstruction> accessIt,
    const intermediate::IntermediateInstruction* defaultInst, FastMap<const Local*, ValueRange>& knownRanges)
{
    if(auto writer = getSingleWriter(val, defaultInst))
        // if there is a single address writer, take that one
        return determineAccessRange(method, *writer, accessIt, knownRanges, baseAddr);
    // TODO how to determine access range for a memory location for conditionally written address??
    return {};
}

FastAccessList<MemoryAccessRange> analysis::determineAccessRanges(Method& method, const Local* baseAddr,
    FastMap<TypedInstructionWalker<intermediate::MemoryInstruction>, const Local*>& accessInstructions)
{
    PROFILE_SCOPE(DetermineAccessRanges);
    // NOTE: If we cannot find one access range for a local, we cannot combine any other access ranges for this local!
    FastAccessList<MemoryAccessRange> result;
    FastMap<const Local*, ValueRange> knownRanges;
    for(const auto& entry : accessInstructions)
    {
        const auto memInstr = entry.first.get();
        switch(memInstr->op)
        {
        case intermediate::MemoryOperation::READ:
        {
            if(auto res = findAccessRange(method, memInstr->getSource(), baseAddr, entry.first, memInstr, knownRanges))
            {
                result.emplace_back(std::move(res).value());
                break;
            }

            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine access range for memory read: " << entry.first->to_string()
                    << logging::endl);
            return FastAccessList<MemoryAccessRange>{};
        }
        case intermediate::MemoryOperation::WRITE:
        case intermediate::MemoryOperation::FILL:
        {
            if(auto res =
                    findAccessRange(method, memInstr->getDestination(), baseAddr, entry.first, memInstr, knownRanges))
            {
                result.emplace_back(std::move(res).value());
                break;
            }
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine access range for memory write/fill: " << entry.first->to_string()
                    << logging::endl);
            return FastAccessList<MemoryAccessRange>{};
        }
        case intermediate::MemoryOperation::COPY:
        {
            Value matchingAddress = UNDEFINED_VALUE;
            if(memInstr->getSource().local()->getBase(true) == entry.second)
                matchingAddress = memInstr->getSource();
            else if(memInstr->getDestination().local()->getBase(true) == entry.second)
                matchingAddress = memInstr->getDestination();
            else
                throw CompilationError(CompilationStep::GENERAL, "Failed to find address referring to memory location",
                    memInstr->to_string() + " and " + baseAddr->to_string());

            if(auto res = findAccessRange(method, matchingAddress, baseAddr, entry.first, memInstr, knownRanges))
            {
                result.emplace_back(std::move(res).value());
                break;
            }
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine access range for local '" << baseAddr->to_string()
                    << "' in memory copy: " << entry.first->to_string() << logging::endl);
            return FastAccessList<MemoryAccessRange>{};
        }
        }
    }
    return result;
}

static std::vector<SubExpression> getAdditionParts(const SubExpression& in)
{
    if(auto expr = in.checkExpression())
    {
        if(expr->code != OP_ADD)
            return {expr};
        return expr->getAssociativeParts();
    }
    return {in};
}

static SubExpression combine(OpCode code, const SubExpression& one, const SubExpression& other)
{
    if(!one && code == OP_ADD)
        return other;
    if(!other && code == OP_ADD)
        return one;
    return std::make_shared<Expression>(code, one ? SubExpression{one} : INT_ZERO, other)->combineWith({});
}

Optional<IdenticalWorkGroupUniformPartsResult> analysis::checkWorkGroupUniformParts(
    FastAccessList<MemoryAccessRange>& accessRanges)
{
    analysis::ValueRange accessRange{};
    const auto& firstUniformAddresses = getAdditionParts(accessRanges.front().groupUniformOffset);
    std::vector<SubExpression> differingUniformParts;
    bool allUniformPartsEqual = true;
    Optional<DataType> elementType;
    for(auto& entry : accessRanges)
    {
        auto entryParts = getAdditionParts(entry.groupUniformOffset);
        if(entryParts != firstUniformAddresses)
        {
            allUniformPartsEqual = false;
            for(const auto& part : entryParts)
            {
                if(std::find(firstUniformAddresses.begin(), firstUniformAddresses.end(), part) ==
                    firstUniformAddresses.end())
                    differingUniformParts.emplace_back(part);
            }
            for(const auto& part : firstUniformAddresses)
                if(std::find(entryParts.begin(), entryParts.end(), part) == entryParts.end())
                    differingUniformParts.emplace_back(part);
        }
        if(auto range = entry.getDynamicAccessElementRange())
        {
            if(accessRange)
                accessRange |= *range;
            else
                // for the first entry, the combined range is still unknown/undefined
                accessRange = *range;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot cache memory location with unknown accessed range: " << entry.to_string()
                    << logging::endl);
            return {};
        }
        if(entry.accessElementType.isUnknown())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot cache memory location with unknown accessed element type: " << entry.to_string()
                    << logging::endl);
            return {};
        }
        else if(elementType && elementType != entry.accessElementType)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot cache memory location accessed with different element types: " << entry.to_string()
                    << logging::endl);
            return {};
        }
        elementType = entry.accessElementType;
    }
    if(!allUniformPartsEqual)
    {
        if(std::all_of(differingUniformParts.begin(), differingUniformParts.end(),
               [](const auto& part) -> bool { return part.getLiteralValue().has_value(); }))
        {
            // all work-group uniform values which differ between various accesses of the same local are literal
            // values. We can use this knowledge to still allow caching the local, by converting the literals to
            // dynamic offsets
            for(auto& entry : accessRanges)
            {
                for(const auto& part : getAdditionParts(entry.groupUniformOffset))
                {
                    if(std::find(differingUniformParts.begin(), differingUniformParts.end(), part) !=
                        differingUniformParts.end())
                    {
                        entry.dynamicOffset = combine(OP_ADD, entry.dynamicOffset, part);
                        entry.groupUniformOffset = combine(OP_SUB, entry.groupUniformOffset, part);
                    }
                }
            }
            return checkWorkGroupUniformParts(accessRanges);
        }
        else
            return {};
    }
    if(!elementType)
        return {};
    return IdenticalWorkGroupUniformPartsResult{accessRange, *elementType};
}
