
#include "MemoryAnalysis.h"

#include "../Expression.h"
#include "../GlobalValues.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::analysis;

LCOV_EXCL_START
std::string MemoryAccessRange::to_string() const
{
    std::string exprPart{};
    if(addressExpression)
        exprPart = typeSizeShift ? (" with element offset expression: " + addressExpression->to_string()) :
                                   (baseAddressAdd ? " with byte offset expression: " + addressExpression->to_string() :
                                                     (" with address expression: " + addressExpression->to_string()));

    return (addressWrite->to_string() +
               (addressWrite->writesRegister(REG_VPM_DMA_LOAD_ADDR) ? " - read " : " - write ")) +
        (memoryObject->to_string() +
            (groupUniformAddressParts.empty() ? " with" : " with work-group uniform offset and") +
            " dynamic elements") +
        (offsetRange ? (" in range [" + (std::to_string(offsetRange->minValue) + ", ") +
                           (std::to_string(offsetRange->maxValue) + "]")) :
                       " with indeterminate range") +
        (constantOffset ? " offset by " + constantOffset->to_string() : "") + exprPart;
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

static bool isGroupUniform(const Local* local)
{
    auto writers = local->getUsers(LocalUse::Type::WRITER);
    return std::all_of(writers.begin(), writers.end(), [](const LocalUser* instr) -> bool {
        return instr->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    });
}

static bool isWorkGroupUniform(const Value& val)
{
    return val.checkImmediate() || val.checkLiteral() ||
        (val.checkLocal() && isGroupUniform(val.local()))
        // XXX this is not true for the local ID UNIFORM
        || (val.hasRegister(REG_UNIFORM));
}

static FastMap<Value, intermediate::InstructionDecorations> findDirectLevelAdditionInputs(const Value& val)
{
    using namespace vc4c::intermediate;
    FastMap<Value, InstructionDecorations> result;
    auto writer = val.getSingleWriter();
    if(writer == nullptr || writer->hasDecoration(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
    {
        // we have no need to split up work-group uniform values any more detailed
        auto deco = writer ? writer->decoration : InstructionDecorations::NONE;
        result.emplace(val,
            add_flag(deco,
                val.checkImmediate() || val.checkLiteral() ? InstructionDecorations::WORK_GROUP_UNIFORM_VALUE :
                                                             InstructionDecorations::NONE));
        if(val.checkImmediate() && val.immediate().getIntegerValue() >= 0)
            result[val] = add_flag(result[val], InstructionDecorations::UNSIGNED_RESULT);
        else if(val.checkLiteral() && val.literal().signedInt() >= 0)
            result[val] = add_flag(result[val], InstructionDecorations::UNSIGNED_RESULT);
        else if(val.checkRegister() && val.reg() == REG_UNIFORM)
            // XXX this is not true for the local ID UNIFORM, which should never be checked here (since the actual ID
            // needs always be extracted via non-ADDs, e.g. ANDs)
            result[val] = add_flag(result[val], InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        return result;
    }
    auto move = dynamic_cast<const MoveOperation*>(writer);
    if(move && move->isSimpleMove())
        return findDirectLevelAdditionInputs(move->getSource());

    auto op = dynamic_cast<const Operation*>(writer);
    bool onlySideEffectIsReadingUniform = op && op->getSideEffects() == SideEffectType::REGISTER_READ &&
        std::all_of(op->getArguments().begin(), op->getArguments().end(), [](const Value& arg) -> bool {
            return !arg.checkRegister() || arg.reg() == REG_UNIFORM || !arg.reg().hasSideEffectsOnRead();
        });
    if(op && op->op == OP_ADD && !op->hasConditionalExecution() &&
        (!op->hasSideEffects() || onlySideEffectIsReadingUniform) && !op->hasPackMode() && !op->hasUnpackMode())
    {
        FastMap<Value, InstructionDecorations> args;
        for(const auto& arg : op->getArguments())
        {
            auto tmp = findDirectLevelAdditionInputs(arg);
            args.insert(tmp.begin(), tmp.end());
        }
        return args;
    }
    result.emplace(val, writer->decoration);
    return result;
}

static Optional<MemoryAccessRange> determineAccessRange(
    Method& method, InstructionWalker it, InstructionWalker memIt, FastMap<const Local*, InstructionWalker>& indexCache)
{
    // 1. find writes to VPM DMA addresses with work-group uniform part in address values
    if(auto memInst = it.get<intermediate::MemoryInstruction>())
    {
        if(memInst->getSource().checkLocal() &&
            (memInst->getSource().local()->is<Parameter>() || memInst->getSource().local()->is<Global>()))
        {
            // direct write of address (e.g. all work items access the same location)
            MemoryAccessRange range;
            range.addressWrite = memIt;
            range.memoryObject = memInst->getSource().local();
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found memory access without offset: " << range.to_string() << logging::endl);
            return range;
        }
    }
    if(it.get<intermediate::MoveOperation>() && it->assertArgument(0).checkLocal() &&
        (it->assertArgument(0).local()->is<Parameter>() || it->assertArgument(0).local()->is<Global>()))
    {
        // direct write of address (e.g. all work items access the same location)
        // XXX if the memory is __local and the width of the writes is known, can lower into VPM (e.g. for data
        // exchange between work-items). But then the __local memory should be set small enough to fit in the VPM
        // completely, which is already handled at this point (see above).
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "DMA address is directly set to a parameter/global address, cannot be "
                   "optimized by caching multiple accesses: "
                << it->to_string() << logging::endl);
        return {};
    }
    MemoryAccessRange range;
    range.addressWrite = memIt;
    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
    // base-pointer is found correctly
    auto trackIt = it;
    if(it.get<intermediate::MoveOperation>() && it->assertArgument(0).getSingleWriter())
    {
        auto walker = it.getBasicBlock()->findWalkerForInstruction(it->assertArgument(0).getSingleWriter(), it);
        if(!walker)
        {
            // TODO this does not work for MemoryInstruction mapping, since the instructions are not in order of
            // execution!
            auto cacheIt = indexCache.find(it->assertArgument(0).checkLocal());
            if(cacheIt != indexCache.end())
                walker = cacheIt->second;
        }
        if(!walker)
        {
            // XXX this is actually no problem (other than finding the iterator)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Unhandled case, address is calculated in a different basic-block: " << it->to_string()
                    << logging::endl);
            return {};
        }
        else
            trackIt = walker.value();
    }

    auto variableArg =
        std::find_if_not(trackIt->getArguments().begin(), trackIt->getArguments().end(), isWorkGroupUniform);
    if(variableArg != trackIt->getArguments().end() && variableArg->getSingleWriter() != nullptr)
    {
        // 2. rewrite address so all work-group uniform parts are combined and all variable parts and
        // added in the end
        // TODO is this the correct criteria? We could also handle only base-pointer + local_id, for
        // example
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Found VPM DMA address write with work-group uniform operand: " << it->to_string() << logging::endl);
        Value varArg = *variableArg;
        if(!trackIt.isEndOfBlock() && trackIt->checkOutputLocal() &&
            indexCache.find(trackIt->checkOutputLocal()) == indexCache.end())
            indexCache.emplace(trackIt->checkOutputLocal(), trackIt);

        // 2.1 jump over final addition of base address if it is a parameter
        while(trackIt.get<intermediate::Operation>() && trackIt.get<const intermediate::Operation>()->op == OP_ADD)
        {
            const auto& arg0 = trackIt->assertArgument(0);
            const auto& arg1 = trackIt->assertArgument(1);
            if(arg0.checkLocal() &&
                (arg0.local()->is<Parameter>() || arg0.local()->is<Global>() ||
                    arg0.local()->name == Method::GLOBAL_DATA_ADDRESS))
            {
                range.memoryObject = arg0.local();
                varArg = arg1;
            }
            else if(arg1.checkLocal() &&
                (arg1.local()->is<Parameter>() || arg1.local()->is<Global>() ||
                    arg1.local()->name == Method::GLOBAL_DATA_ADDRESS))
            {
                range.memoryObject = arg1.local();
                varArg = arg0;
            }
            else if(arg0.hasRegister(REG_UNIFORM))
            {
                // e.g. reading of uniform for parameter is replaced by reading uniform here (if
                // parameter only used once)
                range.memoryObject = trackIt->getOutput()->local()->getBase(true);
                varArg = arg1;
            }
            else if(arg1.hasRegister(REG_UNIFORM))
            {
                range.memoryObject = trackIt->getOutput()->local()->getBase(true);
                varArg = arg0;
            }
            else if(auto constant = arg0.getConstantValue(true))
            {
                // this is an add of a constant, the actual base-address add might be the source of the
                // other argument
                if(auto writer = arg1.getSingleWriter())
                {
                    if(auto tmp = trackIt.getBasicBlock()->findWalkerForInstruction(writer, trackIt))
                    {
                        range.constantOffset = constant;
                        trackIt = *tmp;
                        continue;
                    }
                }
            }
            else if(auto constant = arg1.getConstantValue(true))
            {
                // this is an add of a constant, the actual base-address add might be the source of the
                // other argument
                if(auto writer = arg0.getSingleWriter())
                {
                    if(auto tmp = trackIt.getBasicBlock()->findWalkerForInstruction(writer, trackIt))
                    {
                        range.constantOffset = constant;
                        trackIt = *tmp;
                        continue;
                    }
                }
            }
            else
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Unhandled case of memory access: " << trackIt->to_string() << logging::endl);
                return {};
            }
            range.baseAddressAdd = dynamic_cast<const intermediate::Operation*>(trackIt.get());
            break;
        }
        if(!range.memoryObject || !range.baseAddressAdd)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot optimize further, since add of base-address and pointer was not found: "
                    << it->to_string() << logging::endl);
            return {};
        }
        auto writer = varArg.getSingleWriter();
        // 2.2 jump over shl (if any) and remember offset
        if(dynamic_cast<const intermediate::Operation*>(writer) &&
            dynamic_cast<const intermediate::Operation*>(writer)->op == OP_SHL)
        {
            if(!writer->assertArgument(1).getLiteralValue() ||
                (1u << writer->assertArgument(1).getLiteralValue()->unsignedInt()) !=
                    it->assertArgument(0).type.getElementType().getLogicalWidth())
            {
                // Abort, since the offset shifted does not match the type-width of the element type
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Cannot optimize further, since shift-offset does not match type size: " << it->to_string()
                        << " and " << writer->to_string() << logging::endl);
                return {};
            }
            range.typeSizeShift = dynamic_cast<const intermediate::Operation*>(writer);
            varArg = writer->assertArgument(0);
        }
        // 2.3 collect all directly neighboring (and directly referenced) additions
        // result is now: finalAdd + (sum(addedValues) << shiftFactor)
        auto addressParts = findDirectLevelAdditionInputs(varArg);
        // 2.4 calculate the maximum dynamic offset
        for(const auto& val : addressParts)
        {
            if(!has_flag(val.second, intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
            {
                range.dynamicAddressParts.emplace(val);
                if(val.first.checkLocal())
                {
                    if(auto singleRange = analysis::ValueRange::getValueRangeRecursive(val.first, &method))
                    {
                        if(range.offsetRange)
                        {
                            range.offsetRange->minValue += singleRange.minValue;
                            range.offsetRange->maxValue += singleRange.maxValue;
                        }
                    }
                    else
                    {
                        // we might have an determinate access range before, but this address part, we don't know the
                        // range -> indeterminate range
                        logging::debug() << "Could not determine access range for address part '"
                                         << val.first.to_string() << "' for memory access: " << memIt->to_string()
                                         << logging::endl;
                        range.offsetRange = {};
                    }
                }
                else
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Unhandled value for memory access offset", val.first.to_string());
            }
            else
                range.groupUniformAddressParts.emplace(val);
        }
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Found memory access range: " << range.to_string() << logging::endl);
        return range;
    }
    return {};
}

AccessRanges analysis::determineAccessRanges(Method& method)
{
    // TODO if we cannot find an access range for a local, we cannot combine any other access ranges for this globally!
    AccessRanges result;
    // cache already found write walkers in case the output is reused in a different block
    // TODO remove this when we can find walker for any instruction across blocks fast
    FastMap<const Local*, InstructionWalker> indexCache;
    for(BasicBlock& block : method)
    {
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(it.has() && (it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            {
                if(auto range = determineAccessRange(method, it, it, indexCache))
                    result[range->memoryObject].emplace_back(std::move(range).value());
            }
            it.nextInBlock();
        }
    }
    return result;
}

static Optional<InstructionWalker> findSingleWriter(InstructionWalker it, const Value& val)
{
    const intermediate::IntermediateInstruction* writer = nullptr;
    for(const auto& w : val.local()->getUsers(LocalUse::Type::WRITER))
    {
        if(dynamic_cast<const intermediate::MemoryInstruction*>(w))
            // store memory instructions count as writers, so ignore them
            continue;
        if(writer)
        {
            writer = nullptr;
            break;
        }
        writer = w;
    }
    if(!writer)
    {
        if(auto loc = val.checkLocal())
        {
            // if the value is the parameter/global directly, the address write might not yet exist
            if(loc->is<Parameter>() || loc->is<Global>())
                return it;
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case, value does not have exactly 1 writer for '" << val.to_string()
                << "': " << it->to_string() << logging::endl);
        return {};
    }
    auto writerIt = it.getBasicBlock()->findWalkerForInstruction(writer, it);
    if(!writerIt)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case, address is calculated in a different basic-block: " << it->to_string()
                << logging::endl);
        return {};
    }
    return writerIt;
}

FastAccessList<MemoryAccessRange> analysis::determineAccessRanges(
    Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // cache already found write walkers in case the output is reused in a different block
    // TODO remove this when we can find walker for any instruction across blocks fast
    FastMap<const Local*, InstructionWalker> indexCache;
    // NOTE: If we cannot find one access range for a local, we cannot combine any other access ranges for this local!
    FastAccessList<MemoryAccessRange> result;
    for(auto it : access.accessInstructions)
    {
        const auto memInstr = it.get<intermediate::MemoryInstruction>();
        switch(memInstr->op)
        {
        case intermediate::MemoryOperation::READ:
        {
            if(auto writerIt = findSingleWriter(it, memInstr->getSource()))
            {
                if(auto res = determineAccessRange(method, writerIt.value(), it, indexCache))
                {
                    result.emplace_back(std::move(res).value());
                    break;
                }
            }
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine access range for memory read: " << it->to_string() << logging::endl);
            return FastAccessList<MemoryAccessRange>{};
        }
        case intermediate::MemoryOperation::WRITE:
        case intermediate::MemoryOperation::FILL:
        {
            // TODO for fill (and copy below) need to heed number of elements!
            if(auto writerIt = findSingleWriter(it, memInstr->getDestination()))
            {
                if(auto res = determineAccessRange(method, writerIt.value(), it, indexCache))
                {
                    result.emplace_back(std::move(res).value());
                    break;
                }
            }
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine access range for memory write/fill: " << it->to_string() << logging::endl);
            return FastAccessList<MemoryAccessRange>{};
        }
        case intermediate::MemoryOperation::COPY:
        {
            Value matchingAddress = UNDEFINED_VALUE;
            if(memInstr->getSource().local()->getBase(true) == baseAddr)
                matchingAddress = memInstr->getSource();
            else if(memInstr->getDestination().local()->getBase(true) == baseAddr)
                matchingAddress = memInstr->getDestination();
            else
                throw CompilationError(CompilationStep::GENERAL, "Failed to find address referring to address",
                    memInstr->to_string() + " and " + baseAddr->to_string());

            if(auto writerIt = findSingleWriter(it, matchingAddress))
            {
                if(auto res = determineAccessRange(method, writerIt.value(), it, indexCache))
                {
                    result.emplace_back(std::move(res.value()));
                    break;
                }
            }
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine access range for local '" << baseAddr->to_string()
                    << "' in memory copy: " << it->to_string() << logging::endl);
            return FastAccessList<MemoryAccessRange>{};
        }
        }
    }
    return result;
}
