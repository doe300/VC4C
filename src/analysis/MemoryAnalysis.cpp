
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
        " in range " + offsetRange.to_string() + (constantOffset ? " offset by " + constantOffset->to_string() : "") +
        exprPart;
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
    return local->allUsers(LocalUse::Type::WRITER, [](const LocalUser* instr) -> bool {
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
    if(writer->isSimpleMove())
        return findDirectLevelAdditionInputs(writer->getMoveSource().value());

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

const intermediate::IntermediateInstruction* analysis::getSingleWriter(
    const Value& val, const intermediate::IntermediateInstruction* defaultInst)
{
    const intermediate::IntermediateInstruction* writer = nullptr;
    for(const auto& w : val.local()->getUsers(LocalUse::Type::WRITER))
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
        if(auto loc = val.checkLocal())
        {
            // if the value is the parameter/global directly, the address write might not yet exist
            if(loc->residesInMemory())
                return defaultInst;
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case, value does not have exactly 1 writer for '" << val.to_string()
                << "': " << (defaultInst ? defaultInst->to_string() : "(null)") << logging::endl);
        return {};
    }
    return writer;
}

static Optional<ValueRange> addNumEntries(const Local* baseAddress, Optional<ValueRange> range,
    const intermediate::MemoryInstruction* memInst, const Local* local)
{
    const auto& numEntries = memInst ? memInst->getNumEntries() : UNDEFINED_VALUE;
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
                << (memInst ? memInst->to_string() : "(none)") << logging::endl);
        return RANGE_UINT;
    }

    if(auto lit = numEntries.getConstantValue() & &Value::getLiteralValue)
    {
        auto maxOffset = static_cast<double>(lit->unsignedInt() - 1u) * typeFactor;
        return *range | (*range + maxOffset);
    }
    if(auto writer = numEntries.getSingleWriter())
    {
        if(auto expr = Expression::createRecursiveExpression(*writer))
        {
            if(auto lit = expr->getConstantExpression() & &Value::getLiteralValue)
            {
                auto maxOffset = static_cast<double>(lit->unsignedInt() - 1u) * typeFactor;
                return *range | (*range + maxOffset);
            }
            if(auto tmpRange = ValueRange::getValueRange(*expr))
            {
                auto maxOffset = (tmpRange.maxValue - 1.0) * typeFactor;
                return *range | (*range + maxOffset);
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
            (arg0.local()->residesInMemory() ||
                (arg0.local()->is<BuiltinLocal>() &&
                    arg0.local()->as<BuiltinLocal>()->builtinType == BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)))
        {
            range.memoryObject = arg0.local();
            varArg = arg1;
        }
        else if(arg1.checkLocal() &&
            (arg1.local()->residesInMemory() ||
                (arg1.local()->is<BuiltinLocal>() &&
                    arg1.local()->as<BuiltinLocal>()->builtinType == BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)))
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
            if(auto writer = getSingleWriter(arg1))
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
            if(auto writer = getSingleWriter(arg0))
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
                auto firstLoc = expr->arg0.checkLocal(true);
                auto secondLoc = expr->arg1.checkLocal(true);
                if(firstLoc &&
                    (firstLoc->residesInMemory() ||
                        (firstLoc->is<BuiltinLocal>() &&
                            firstLoc->as<BuiltinLocal>()->builtinType == BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)))
                {
                    range.memoryObject = firstLoc;
                    varArg = arg1;
                    isHandled = true;
                }
                else if(secondLoc &&
                    (secondLoc->residesInMemory() ||
                        (secondLoc->is<BuiltinLocal>() &&
                            secondLoc->as<BuiltinLocal>()->builtinType == BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)))
                {
                    range.memoryObject = secondLoc;
                    varArg = arg0;
                    isHandled = true;
                }
            }
            // e.g. there is no offset at all, then the expression shrinks to a move of the base address
            else if(expr && expr->isMoveExpression() && expr->arg0.checkValue() & &Value::checkLocal)
            {
                auto loc = expr->arg0.checkValue()->local();
                if(loc->residesInMemory() ||
                    (loc->is<BuiltinLocal>() &&
                        loc->as<BuiltinLocal>()->builtinType == BuiltinLocal::Type::GLOBAL_DATA_ADDRESS))
                {
                    range.memoryObject = loc;
                    range.constantOffset = INT_ZERO;
                    varArg = INT_ZERO;
                    isHandled = true;
                }
            }
            if(isHandled)
                range.addressExpression = expr;
            else
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
            auto type = inst.assertArgument(0).type.getElementType();
            bool shiftIsElementTypeSize =
                secondLiteral && (1u << secondLiteral->unsignedInt()) == type.getLogicalWidth();
            bool shiftIsArrayElementTypeSize = secondLiteral && type.getArrayType() &&
                (1u << secondLiteral->unsignedInt()) == type.getArrayType()->elementType.getLogicalWidth();
            if(!shiftIsElementTypeSize && !shiftIsArrayElementTypeSize)
                // Abort, since the offset shifted does not match the type-width of the element type
                return false;
            range.typeSizeShift = dynamic_cast<const intermediate::Operation*>(writer);
            varArg = writer->assertArgument(0);
        }
    }
    return true;
}

static Optional<MemoryAccessRange> determineAccessRange(Method& method,
    const intermediate::IntermediateInstruction& inst, InstructionWalker memIt,
    FastMap<const Local*, ValueRange>& knownRanges, const Local* checkBaseAddress = nullptr)
{
    // 1. find writes to memory addresses with work-group uniform part in address values
    if(auto memInst = dynamic_cast<const intermediate::MemoryInstruction*>(&inst))
    {
        const Local* checkLocal = nullptr;
        switch(memInst->op)
        {
        case intermediate::MemoryOperation::READ:
            checkLocal = memInst->getSource().checkLocal();
            break;
        case intermediate::MemoryOperation::FILL:
        case intermediate::MemoryOperation::WRITE:
            checkLocal = memInst->getDestination().checkLocal();
            break;
        case intermediate::MemoryOperation::COPY:
            if(checkBaseAddress &&
                (memInst->getSource().hasLocal(checkBaseAddress) ||
                    memInst->getDestination().hasLocal(checkBaseAddress)))
                // is read for either input or output
                checkLocal = checkBaseAddress;
            break;
        }
        if(checkLocal && checkLocal->residesInMemory() && (!checkBaseAddress || checkLocal == checkBaseAddress))
        {
            // direct write of address (e.g. all work items access the same location)
            MemoryAccessRange range;
            range.addressWrite = memIt;
            range.memoryObject = checkLocal;
            range.offsetRange = addNumEntries(range.memoryObject, ValueRange{0.0}, memInst, checkLocal);
            if(auto writer = checkLocal->getSingleWriter())
                range.addressExpression = Expression::createRecursiveExpression(*writer);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found memory access without offset: " << range.to_string() << logging::endl);
            return range;
        }
    }
    auto memInst = memIt.get<intermediate::MemoryInstruction>();
    auto moveSourceLocal = inst.getMoveSource() & &Value::checkLocal;
    if(memInst && moveSourceLocal && moveSourceLocal->residesInMemory())
    {
        // direct write of address (e.g. all work items access the same location)
        MemoryAccessRange range;
        range.addressWrite = memIt;
        range.memoryObject = inst.assertArgument(0).local()->getBase(false);
        range.offsetRange = addNumEntries(range.memoryObject, ValueRange{0.0}, memInst, inst.assertArgument(0).local());
        range.addressExpression = Expression::createRecursiveExpression(inst);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Memory address is directly set to a parameter/global address: " << range.to_string()
                << logging::endl);
        return range;
    }
    MemoryAccessRange range;
    range.addressWrite = memIt;
    range.offsetRange = ValueRange{0.0};
    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
    // base-pointer is found correctly
    auto trackInst = &inst;
    if(auto writer = inst.getMoveSource() & &Value::getSingleWriter)
        trackInst = writer;

    Optional<Value> varArg;
    auto variableArg =
        std::find_if_not(trackInst->getArguments().begin(), trackInst->getArguments().end(), isWorkGroupUniform);
    if(variableArg != trackInst->getArguments().end() && variableArg->getSingleWriter() != nullptr)
        varArg = *variableArg;

    // 2. rewrite address so all work-group uniform parts are combined and all variable parts and
    // added in the end
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Found memory address write with work-group uniform operand: " << inst.to_string() << logging::endl);

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
            if(auto loc = val.first.checkLocal())
            {
                if(auto singleRange = analysis::ValueRange::getValueRangeRecursive(val.first, &method, knownRanges))
                {
                    if(range.offsetRange && *range.offsetRange)
                        *range.offsetRange += singleRange;
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
    FastMap<const Local*, ValueRange> knownRanges;
    for(BasicBlock& block : method)
    {
        InstructionWalker it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(it.has() && (it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            {
                if(auto range = determineAccessRange(method, *it.get(), it, knownRanges))
                    result[range->memoryObject].emplace_back(range.value());
            }
            it.nextInBlock();
        }
    }
    return result;
}

static Optional<MemoryAccessRange> findAccessRange(Method& method, const Value& val, const Local* baseAddr,
    InstructionWalker accessIt, const intermediate::IntermediateInstruction* defaultInst,
    FastMap<const Local*, ValueRange>& knownRanges)
{
    if(auto writer = getSingleWriter(val, defaultInst))
        // if there is a single address writer, take that one
        return determineAccessRange(method, *writer, accessIt, knownRanges, baseAddr);
    // TODO how to determine access range for a memory location for conditionally written address??
    return {};
}

FastAccessList<MemoryAccessRange> analysis::determineAccessRanges(
    Method& method, const Local* baseAddr, FastMap<InstructionWalker, const Local*>& accessInstructions)
{
    // NOTE: If we cannot find one access range for a local, we cannot combine any other access ranges for this local!
    FastAccessList<MemoryAccessRange> result;
    FastMap<const Local*, ValueRange> knownRanges;
    for(const auto& entry : accessInstructions)
    {
        const auto memInstr = entry.first.get<intermediate::MemoryInstruction>();
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

std::pair<bool, analysis::ValueRange> analysis::checkWorkGroupUniformParts(
    FastAccessList<MemoryAccessRange>& accessRanges, bool allowConstantOffsets)
{
    analysis::ValueRange offsetRange{};
    const auto& firstUniformAddresses = accessRanges.front().groupUniformAddressParts;
    FastMap<Value, intermediate::InstructionDecorations> differingUniformParts;
    bool allUniformPartsEqual = true;
    for(auto& entry : accessRanges)
    {
        if(entry.groupUniformAddressParts != firstUniformAddresses)
        {
            allUniformPartsEqual = false;
            for(const auto& pair : entry.groupUniformAddressParts)
            {
                if(firstUniformAddresses.find(pair.first) == firstUniformAddresses.end())
                    differingUniformParts.emplace(pair);
            }
            for(const auto& pair : firstUniformAddresses)
                if(entry.groupUniformAddressParts.find(pair.first) == entry.groupUniformAddressParts.end())
                    differingUniformParts.emplace(pair);
        }
        if(auto range = entry.offsetRange)
        {
            if(offsetRange)
                offsetRange |= *range;
            else
                // for the first entry, the combined range is still unknown/undefined
                offsetRange = *range;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot cache memory location with unknown accessed range" << logging::endl);
            return std::make_pair(false, analysis::ValueRange{});
        }
        if(!allowConstantOffsets && entry.constantOffset)
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Constant address offsets are not yet supported" << logging::endl);
            return std::make_pair(false, analysis::ValueRange{});
        }
    }
    if(!allUniformPartsEqual)
    {
        if(std::all_of(differingUniformParts.begin(), differingUniformParts.end(),
               [](const auto& part) -> bool { return part.first.getLiteralValue().has_value(); }))
        {
            // all work-group uniform values which differ between various accesses of the same local are literal
            // values. We can use this knowledge to still allow caching the local, by converting the literals to
            // dynamic offsets
            for(auto& entry : accessRanges)
            {
                auto it = entry.groupUniformAddressParts.begin();
                while(it != entry.groupUniformAddressParts.end())
                {
                    if(differingUniformParts.find(it->first) != differingUniformParts.end())
                    {
                        if(entry.offsetRange)
                            *entry.offsetRange += it->first.getLiteralValue()->signedInt();
                        else
                            // TODO correct??
                            entry.offsetRange =
                                analysis::ValueRange{static_cast<double>(it->first.getLiteralValue()->signedInt())};

                        entry.dynamicAddressParts.emplace(*it);
                        it = entry.groupUniformAddressParts.erase(it);
                    }
                    else
                        ++it;
                }
            }
            return checkWorkGroupUniformParts(accessRanges);
        }
        else
            return std::make_pair(false, analysis::ValueRange{});
    }
    return std::make_pair(true, offsetRange);
}
