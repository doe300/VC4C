
#include "MemoryAnalysis.h"

#include "../GlobalValues.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::analysis;

LCOV_EXCL_START
std::string MemoryAccessRange::to_string() const
{
    return (addressWrite->to_string() +
               (addressWrite->writesRegister(REG_VPM_DMA_LOAD_ADDR) ? " - read " : " - write ")) +
        (memoryObject->to_string() +
            (groupUniformAddressParts.empty() ? " with" : " with work-group uniform offset and") +
            " dynamic element range [") +
        (std::to_string(offsetRange.minValue) + ", ") + (std::to_string(offsetRange.maxValue) + "]") +
        (constantOffset ? " offset by " + constantOffset->to_string() : "");
}
LCOV_EXCL_STOP

bool LocalUsageOrdering::operator()(const Local* l1, const Local* l2) const
{
    // prefer more usages over less usages
    // TODO is this the correct way to do this? E.g. is there one usage per memory access?
    return l1->getUsers(LocalUse::Type::READER).size() > l2->getUsers(LocalUse::Type::READER).size() || l1 < l2;
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
            // needs always be extracted via non-adds)
            result[val] = add_flag(result[val], InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        return result;
    }
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
                // 1. find writes to VPM DMA addresses with work-group uniform part in address values
                if(std::any_of(it->getArguments().begin(), it->getArguments().end(), isWorkGroupUniform) ||
                    it.get<intermediate::MoveOperation>())
                {
                    if(it.get<intermediate::MoveOperation>() && it->assertArgument(0).checkLocal() &&
                        (it->assertArgument(0).local()->is<Parameter>() || it->assertArgument(0).local()->is<Global>()))
                    {
                        // direct write of address (e.g. all work items access the same location)
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "DMA address is directly set to a parameter/global address, cannot be "
                                   "optimized by caching multiple accesses: "
                                << it->to_string() << logging::endl);
                        it.nextInBlock();
                        continue;
                    }
                    MemoryAccessRange range;
                    range.addressWrite = it;
                    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
                    // base-pointer is found correctly
                    auto trackIt = it;
                    if(it.get<intermediate::MoveOperation>() && it->assertArgument(0).getSingleWriter())
                    {
                        auto walker =
                            it.getBasicBlock()->findWalkerForInstruction(it->assertArgument(0).getSingleWriter(), it);
                        if(!walker)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Unhandled case, address is calculated in a different basic-block: "
                                    << it->to_string() << logging::endl);
                            it.nextInBlock();
                            continue;
                        }
                        else
                            trackIt = walker.value();
                    }

                    auto variableArg = std::find_if_not(
                        trackIt->getArguments().begin(), trackIt->getArguments().end(), isWorkGroupUniform);
                    if(variableArg != trackIt->getArguments().end() && variableArg->getSingleWriter() != nullptr)
                    {
                        // 2. rewrite address so all work-group uniform parts are combined and all variable parts and
                        // added in the end
                        // TODO is this the correct criteria? We could also handle only base-pointer + local_id, for
                        // example
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Found VPM DMA address write with work-group uniform operand: " << it->to_string()
                                << logging::endl);
                        Value varArg = *variableArg;
                        // 2.1 jump over final addition of base address if it is a parameter
                        while(trackIt.get<intermediate::Operation>() &&
                            trackIt.get<const intermediate::Operation>()->op == OP_ADD)
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
                                    log << "Unhandled case of memory access: " << trackIt->to_string()
                                        << logging::endl);
                                it.nextInBlock();
                                break;
                            }
                            range.baseAddressAdd = trackIt;
                            break;
                        }
                        if(!range.memoryObject || range.baseAddressAdd.isEndOfBlock())
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Cannot optimize further, since add of base-address and pointer was not found: "
                                    << it->to_string() << logging::endl);
                            it.nextInBlock();
                            continue;
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
                                    log << "Cannot optimize further, since shift-offset does not match type size: "
                                        << it->to_string() << " and " << writer->to_string() << logging::endl);
                                it.nextInBlock();
                                continue;
                            }
                            range.typeSizeShift = trackIt.getBasicBlock()->findWalkerForInstruction(writer, trackIt);
                            varArg = writer->assertArgument(0);
                        }
                        // 2.3 collect all directly neighboring (and directly referenced) additions
                        // result is now: finalAdd + (sum(addedValues) << shiftFactor)
                        auto addressParts = findDirectLevelAdditionInputs(varArg);
                        if(addressParts.size() < 2)
                        {
                            // could not determine multiple inputs to add, abort
                            it.nextInBlock();
                            continue;
                        }
                        // 2.4 calculate the maximum dynamic offset
                        for(const auto& val : addressParts)
                        {
                            if(!has_flag(val.second, intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
                            {
                                range.dynamicAddressParts.emplace(val);
                                if(val.first.checkLocal())
                                {
                                    auto singleRange = analysis::ValueRange::getValueRange(val.first, &method);
                                    range.offsetRange.minValue += singleRange.getIntRange()->minValue;
                                    range.offsetRange.maxValue += singleRange.getIntRange()->maxValue;
                                }
                                else
                                    throw CompilationError(CompilationStep::OPTIMIZER,
                                        "Unhandled value for memory access offset", val.first.to_string());
                            }
                            else
                                range.groupUniformAddressParts.emplace(val);
                        }
                        CPPLOG_LAZY(logging::Level::DEBUG, log << range.to_string() << logging::endl);
                        result[range.memoryObject].emplace_back(range);
                    }
                }
            }
            it.nextInBlock();
        }
    }
    return result;
}
