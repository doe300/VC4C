
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

    auto mode = (addressWrite->writesRegister(REG_VPM_DMA_LOAD_ADDR) ||
                (addressWrite.get<intermediate::MemoryInstruction>() &&
                    addressWrite.get<intermediate::MemoryInstruction>()->op == intermediate::MemoryOperation::READ) ?
            " - read " :
            (addressWrite->writesRegister(REG_VPM_DMA_STORE_ADDR) ||
                        (addressWrite.get<intermediate::MemoryInstruction>() &&
                            addressWrite.get<intermediate::MemoryInstruction>()->op ==
                                intermediate::MemoryOperation::WRITE) ?
                    " - write " :
                    " - access "));
    return addressWrite->to_string() + mode +
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

static const intermediate::IntermediateInstruction* getSingleWriter(
    const Value& val, const intermediate::IntermediateInstruction* defaultInst)
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
            if(loc->is<Parameter>() || loc->residesInMemory())
                return defaultInst;
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case, value does not have exactly 1 writer for '" << val.to_string()
                << "': " << defaultInst->to_string() << logging::endl);
        return {};
    }
    return writer;
}

static Optional<ValueRange> addNumEntries(const Local* baseAddress, Optional<ValueRange> range,
    const intermediate::MemoryInstruction* memInst, const Local* local)
{
    const auto& numEntries = memInst->getNumEntries();
    if(numEntries == INT_ONE)
        return range;
    if(!range || !*range)
        return range;

    double typeFactor = 1;
    if(local && baseAddress && local->type.getElementType() == baseAddress->type.getElementType())
        // there is no type conversion between the memory access and the actually stored type
        // -> simply add the number of entries
        typeFactor = 1.0;
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Determining number of accessed entries with differing base and accessed types is not yet "
                   "supported: "
                << memInst->to_string() << logging::endl);
        return RANGE_UINT;
    }

    if(auto lit = numEntries.getConstantValue() & &Value::getLiteralValue)
    {
        auto maxOffset = static_cast<double>(lit->unsignedInt() - 1u) * typeFactor;
        return ValueRange{range->minValue, range->maxValue + maxOffset};
    }
    if(auto writer = numEntries.getSingleWriter())
    {
        if(auto expr = Expression::createRecursiveExpression(*writer))
        {
            if(auto lit = expr->getConstantExpression() & &Value::getLiteralValue)
            {
                auto maxOffset = static_cast<double>(lit->unsignedInt() - 1u) * typeFactor;
                return ValueRange{range->minValue, range->maxValue + maxOffset};
            }
            if(auto tmpRange = ValueRange::getValueRange(*expr))
            {
                auto maxOffset = (tmpRange.maxValue - 1.0) * typeFactor;
                return ValueRange{range->minValue, range->maxValue + maxOffset};
            }
        }
    }
    // TODO error or try to determine differently (e.g. lifetime bounds, array bounds, etc...)
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Could not determine number of accessed entries, falling back to undefined access range: "
            << memInst->to_string() << logging::endl);
    return ValueRange{};
}

static bool findMemoryObjectAndBaseAddressAdd(
    MemoryAccessRange& range, Optional<Value>& varArg, const intermediate::IntermediateInstruction*& trackInst)
{
    // TODO directly use expression?? this resolves all the moves.
    // check 2 layers of adds: base-address, constant offset and index offset (here check type-size shift and actual
    // index offset)
    while(dynamic_cast<const intermediate::Operation*>(trackInst) &&
        dynamic_cast<const intermediate::Operation*>(trackInst)->op == OP_ADD)
    {
        const auto& arg0 = trackInst->assertArgument(0);
        const auto& arg1 = trackInst->assertArgument(1);
        if(arg0.checkLocal() &&
            (arg0.local()->is<Parameter>() || arg0.local()->residesInMemory() ||
                arg0.local()->name == Method::GLOBAL_DATA_ADDRESS))
        {
            range.memoryObject = arg0.local();
            varArg = arg1;
        }
        else if(arg1.checkLocal() &&
            (arg1.local()->is<Parameter>() || arg1.local()->residesInMemory() ||
                arg1.local()->name == Method::GLOBAL_DATA_ADDRESS))
        {
            range.memoryObject = arg1.local();
            varArg = arg0;
        }
        else if(arg0.hasRegister(REG_UNIFORM))
        {
            // e.g. reading of uniform for parameter is replaced by reading uniform here (if
            // parameter only used once)
            range.memoryObject = trackInst->getOutput()->local()->getBase(true);
            varArg = arg1;
        }
        else if(arg1.hasRegister(REG_UNIFORM))
        {
            range.memoryObject = trackInst->getOutput()->local()->getBase(true);
            varArg = arg0;
        }
        else if(auto constant = arg0.getConstantValue(true))
        {
            // this is an add of a constant, the actual base-address add might be the source of the
            // other argument
            if(auto writer = getSingleWriter(arg1, nullptr))
            {
                range.constantOffset = constant;
                trackInst = writer;
                continue;
            }
        }
        else if(auto constant = arg1.getConstantValue(true))
        {
            // this is an add of a constant, the actual base-address add might be the source of the
            // other argument
            if(auto writer = getSingleWriter(arg0, nullptr))
            {
                range.constantOffset = constant;
                trackInst = writer;
                continue;
            }
        }
        else
        {
            // we cannot directly determine the required values, try with simplified expression...
            auto expr = Expression::createRecursiveExpression(*trackInst, 8);
            bool isHandled = false;
            if(expr && expr->code == OP_ADD)
            {
                auto firstLoc = expr->arg0.checkLocal();
                auto secondLoc = expr->arg1.checkLocal();
                if(firstLoc &&
                    (firstLoc->is<Parameter>() || firstLoc->residesInMemory() ||
                        firstLoc->name == Method::GLOBAL_DATA_ADDRESS))
                {
                    range.memoryObject = firstLoc;
                    varArg = arg1;
                    isHandled = true;
                }
                else if(secondLoc &&
                    (secondLoc->is<Parameter>() || secondLoc->residesInMemory() ||
                        secondLoc->name == Method::GLOBAL_DATA_ADDRESS))
                {
                    range.memoryObject = secondLoc;
                    varArg = arg0;
                    isHandled = true;
                }
                range.addressExpression = expr;
            }
            if(!isHandled)
                return false;
        }
        range.baseAddressAdd = dynamic_cast<const intermediate::Operation*>(trackInst);
        break;
    }
    return true;
}

static bool findTypeSizeShift(
    MemoryAccessRange& range, Optional<Value>& varArg, const intermediate::IntermediateInstruction& inst)
{
    if(varArg)
    {
        auto writer = varArg->getSingleWriter();
        if(dynamic_cast<const intermediate::Operation*>(writer) &&
            dynamic_cast<const intermediate::Operation*>(writer)->op == OP_SHL)
        {
            auto secondLiteral = writer->assertArgument(1).getConstantValue() & &Value::getLiteralValue;
            if(!secondLiteral ||
                (1u << secondLiteral->unsignedInt()) != inst.assertArgument(0).type.getElementType().getLogicalWidth())
                // Abort, since the offset shifted does not match the type-width of the element type
                return false;
            range.typeSizeShift = dynamic_cast<const intermediate::Operation*>(writer);
            varArg = writer->assertArgument(0);
        }
    }
    return true;
}

static Optional<MemoryAccessRange> determineAccessRange(
    Method& method, const intermediate::IntermediateInstruction& inst, InstructionWalker memIt)
{
    // 1. find writes to VPM DMA addresses with work-group uniform part in address values
    if(auto memInst = dynamic_cast<const intermediate::MemoryInstruction*>(&inst))
    {
        auto checkLocal = memInst->op == intermediate::MemoryOperation::READ ?
            memInst->getSource().checkLocal() :
            memInst->op == intermediate::MemoryOperation::WRITE ? memInst->getDestination().checkLocal() : nullptr;
        if(checkLocal && (checkLocal->is<Parameter>() || checkLocal->residesInMemory()))
        {
            // direct write of address (e.g. all work items access the same location)
            MemoryAccessRange range;
            range.addressWrite = memIt;
            range.memoryObject = checkLocal;
            range.offsetRange = addNumEntries(range.memoryObject, ValueRange{0.0, 0.0}, memInst, checkLocal);
            if(auto writer = checkLocal->getSingleWriter())
                range.addressExpression = Expression::createRecursiveExpression(*writer);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found memory access without offset: " << range.to_string() << logging::endl);
            return range;
        }
    }
    auto memInst = memIt.get<intermediate::MemoryInstruction>();
    if(memInst && dynamic_cast<const intermediate::MoveOperation*>(&inst) && inst.assertArgument(0).checkLocal() &&
        (inst.assertArgument(0).local()->is<Parameter>() || inst.assertArgument(0).local()->residesInMemory()))
    {
        // direct write of address (e.g. all work items access the same location)
        MemoryAccessRange range;
        range.addressWrite = memIt;
        range.memoryObject = inst.assertArgument(0).local()->getBase(false);
        range.offsetRange =
            addNumEntries(range.memoryObject, ValueRange{0.0, 0.0}, memInst, inst.assertArgument(0).local());
        range.addressExpression = Expression::createRecursiveExpression(inst);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "DMA address is directly set to a parameter/global address: " << range.to_string() << logging::endl);
        return range;
    }
    MemoryAccessRange range;
    range.addressWrite = memIt;
    range.offsetRange = ValueRange{0.0, 0.0};
    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
    // base-pointer is found correctly
    auto trackInst = &inst;
    if(dynamic_cast<const intermediate::MoveOperation*>(&inst) && inst.assertArgument(0).getSingleWriter())
        trackInst = inst.assertArgument(0).getSingleWriter();

    Optional<Value> varArg;
    auto variableArg =
        std::find_if_not(trackInst->getArguments().begin(), trackInst->getArguments().end(), isWorkGroupUniform);
    if(variableArg != trackInst->getArguments().end() && variableArg->getSingleWriter() != nullptr)
        varArg = *variableArg;

    // 2. rewrite address so all work-group uniform parts are combined and all variable parts and
    // added in the end
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Found VPM DMA address write with work-group uniform operand: " << inst.to_string() << logging::endl);

    // 2.1 jump over final addition of base address if it is a parameter
    if(!findMemoryObjectAndBaseAddressAdd(range, varArg, trackInst))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case of memory access: " << trackInst->to_string() << logging::endl);
        return {};
    }
    if(!range.memoryObject || !range.baseAddressAdd)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Address add of base-address and pointer was not found: " << inst.to_string() << logging::endl);
        return {};
    }
    // 2.2 jump over shl (if any) and remember offset
    if(!findTypeSizeShift(range, varArg, inst))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Address shift-offset does not match type size: " << inst.to_string() << " and "
                << (varArg & &Value::getSingleWriter ? varArg->getSingleWriter()->to_string() : "(no writer)")
                << logging::endl);
        return {};
    }
    // 2.3 collect all directly neighboring (and directly referenced) additions
    // result is now: finalAdd + (sum(addedValues) << shiftFactor)
    auto addressPartsSource = varArg ? *varArg : trackInst ? trackInst->getOutput().value() : inst.getOutput().value();
    auto addressParts = findDirectLevelAdditionInputs(addressPartsSource);
    if(auto writer = addressPartsSource.getSingleWriter())
        range.addressExpression = Expression::createRecursiveExpression(*writer, 8);
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
                    if(range.offsetRange && *range.offsetRange)
                    {
                        range.offsetRange->minValue += singleRange.minValue;
                        range.offsetRange->maxValue += singleRange.maxValue;
                    }
                }
                else
                {
                    // we might have an determinate access range before, but this address part, we don't know the
                    // range -> indeterminate range
                    logging::debug() << "Could not determine access range for address part '" << val.first.to_string()
                                     << "' for memory access: " << memIt->to_string() << logging::endl;
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

    range.offsetRange = addNumEntries(range.memoryObject, range.offsetRange, memInst, nullptr /* TODO */);
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Found memory access range: " << range.to_string() << logging::endl);
    return range;
}

AccessRanges analysis::determineAccessRanges(Method& method)
{
    // TODO if we cannot find an access range for a local, we cannot combine any other access ranges for this globally!
    AccessRanges result;
    for(BasicBlock& block : method)
    {
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(it.has() && (it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            {
                if(auto range = determineAccessRange(method, *it.get(), it))
                    result[range->memoryObject].emplace_back(std::move(range).value());
            }
            it.nextInBlock();
        }
    }
    return result;
}

FastAccessList<MemoryAccessRange> analysis::determineAccessRanges(
    Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // NOTE: If we cannot find one access range for a local, we cannot combine any other access ranges for this local!
    FastAccessList<MemoryAccessRange> result;
    for(auto it : access.accessInstructions)
    {
        const auto memInstr = it.get<intermediate::MemoryInstruction>();
        switch(memInstr->op)
        {
        case intermediate::MemoryOperation::READ:
        {
            if(auto writer = getSingleWriter(memInstr->getSource(), memInstr))
            {
                if(auto res = determineAccessRange(method, *writer, it))
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
            if(auto writer = getSingleWriter(memInstr->getDestination(), memInstr))
            {
                if(auto res = determineAccessRange(method, *writer, it))
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

            if(auto writer = getSingleWriter(matchingAddress, memInstr))
            {
                if(auto res = determineAccessRange(method, *writer, it))
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
