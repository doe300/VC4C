/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryMappings.h"

#include "../Profiler.h"
#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;

using MappingCheck = MemoryInfo (*)(Method& method, const Local*, MemoryAccess&);

static MemoryInfo canLowerToRegisterReadOnly(Method& method, const Local* baseAddr, MemoryAccess& access);
static MemoryInfo canLowerToRegisterReadWrite(Method& method, const Local* baseAddr, MemoryAccess& access);
static MemoryInfo canLowerToPrivateVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access);
static MemoryInfo canLowerToSharedVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access);
static MemoryInfo canMapToTMUReadOnly(Method& method, const Local* baseAddr, MemoryAccess& access);
static MemoryInfo canMapToDMAReadWrite(Method& method, const Local* baseAddr, MemoryAccess& access);

static constexpr MappingCheck CHECKS[6] = {
    canLowerToRegisterReadOnly,  /* QPU_REGISTER_READONLY */
    canLowerToRegisterReadWrite, /* QPU_REGISTER_READWRITE */
    canLowerToPrivateVPMArea,    /* VPM_PER_QPU */
    canLowerToSharedVPMArea,     /* VPM_SHARED_ACCESS */
    canMapToTMUReadOnly,         /* RAM_LOAD_TMU */
    canMapToDMAReadWrite         /* RAM_READ_WRITE_VPM */
};

MemoryInfo normalization::checkMemoryMapping(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    return CHECKS[static_cast<unsigned>(access.preferred)](method, baseAddr, access);
}

Optional<Value> normalization::getConstantValue(const Value& source)
{
    // can only read from constant global data, so the global is always the source
    const Global* global = source.local()->getBase(true)->as<Global>();
    if(source.local()->reference.second >= 0 && global->value.hasContainer())
        // fixed index
        return global->value.container().elements.at(source.local()->reference.second);
    else if(global->value.isLiteralValue())
        // scalar value
        return global->value;
    else if(global->value.isZeroInitializer())
        // all entries are the same
        return Value::createZeroInitializer(global->value.type.getElementType());
    else if(global->value.isUndefined())
        // all entries are undefined
        return Value(global->value.type.getElementType());
    else if(global->value.hasContainer() && global->value.container().isElementNumber())
        return ELEMENT_NUMBER_REGISTER;
    else if(global->value.hasContainer() && global->value.container().isAllSame())
        // all entries are the same
        return global->value.container().elements.at(0);
    return NO_VALUE;
}

/*
 * Tries to convert the array-type pointed to by the given local to a vector-type to fit into a single register.
 *
 * For this conversion to succeed, the array-element type must be a scalar of bit-width <= 32-bit and the size of the
 * array known to be less or equals to 16.
 */
static Optional<DataType> convertSmallArrayToRegister(const Local* local)
{
    const Local* base = local->getBase(true);
    if(base->type.getPointerType())
    {
        const auto& baseType = base->type.getPointerType().value()->elementType;
        auto arrayType = baseType.getArrayType();
        if(arrayType && arrayType.value()->size <= NATIVE_VECTOR_SIZE && arrayType.value()->elementType.isScalarType())
            return arrayType.value()->elementType.toVectorType(static_cast<uint8_t>(arrayType.value()->size));
    }
    return {};
}

static bool isMemoryOnlyRead(const Local* local)
{
    auto base = local->getBase(true);
    if(base->is<Parameter>() && has_flag(base->as<const Parameter>()->decorations, ParameterDecorations::READ_ONLY))
        return true;

    if(base->is<Global>() && base->as<Global>()->isConstant)
        return true;

    if(base->type.getPointerType() && base->type.getPointerType().value()->addressSpace == AddressSpace::CONSTANT)
        return true;

    // TODO also check for no actual writes. Need to heed index-calculation from base!
    return false;
}

// Finds the next instruction writing the given value into memory
static InstructionWalker findNextValueStore(
    InstructionWalker it, const Value& src, std::size_t limit, const Local* sourceLocation)
{
    while(!it.isEndOfBlock() && limit > 0)
    {
        auto memInstr = it.get<MemoryInstruction>();
        if(memInstr != nullptr && memInstr->op == MemoryOperation::WRITE && memInstr->getSource() == src)
        {
            return it;
        }
        if(memInstr != nullptr && memInstr->getDestination().local()->getBase(true) == sourceLocation)
        {
            // there is some other instruction writing into the memory we read, it could have been changed -> abort
            // TODO can we be more precise and abort only if the same index is written?? How to determine??
            return it.getBasicBlock()->walkEnd();
        }
        if(it.has<MemoryBarrier>() || it.has<Branch>() || it.has<MutexLock>() || it.has<SemaphoreAdjustment>())
            break;
        it.nextInBlock();
        --limit;
    }
    return it.getBasicBlock()->walkEnd();
}

std::pair<MemoryAccessMap, FastSet<InstructionWalker>> normalization::determineMemoryAccess(Method& method)
{
    // TODO lower local/private struct-elements into VPM?! At least for single structs
    logging::debug() << "Determining memory access for kernel: " << method.name << logging::endl;
    MemoryAccessMap mapping;
    FastSet<InstructionWalker> allWalkers;
    for(const auto& param : method.parameters)
    {
        if(!param.type.isPointerType())
            continue;
        const auto* pointerType = param.type.getPointerType().value();
        if(pointerType->addressSpace == AddressSpace::CONSTANT)
        {
            logging::debug() << "Constant parameter '" << param.to_string() << "' will be read from RAM via TMU"
                             << logging::endl;
            mapping[&param].preferred = MemoryType::RAM_LOAD_TMU;
            // fall-back, e.g. for memory copy
            mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
        }
        else if(pointerType->addressSpace == AddressSpace::GLOBAL)
        {
            if(isMemoryOnlyRead(&param))
            {
                logging::debug() << "Global parameter '" << param.to_string()
                                 << "' without any write access will be read from RAM via TMU" << logging::endl;
                mapping[&param].preferred = MemoryType::RAM_LOAD_TMU;
                // fall-back, e.g. for memory copy
                mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
            }
            else
            {
                logging::debug() << "Global parameter '" << param.to_string()
                                 << "' which is written to will be stored in RAM and accessed via VPM" << logging::endl;
                mapping[&param].preferred = MemoryType::RAM_READ_WRITE_VPM;
                mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
            }
        }
        else if(pointerType->addressSpace == AddressSpace::LOCAL)
        {
            logging::debug() << "Local parameter '" << param.to_string()
                             << "' will be stored in RAM and accessed via VPM" << logging::endl;
            mapping[&param].preferred = MemoryType::RAM_READ_WRITE_VPM;
            mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
        }
        else
            throw CompilationError(
                CompilationStep::NORMALIZER, "Invalid address space for pointer parameter", param.to_string(true));
    }

    InstructionWalker it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(it.has<MemoryInstruction>())
        {
            // convert read-then-write to copy
            auto memInstr = it.get<const MemoryInstruction>();
            if(memInstr->op == MemoryOperation::READ && !memInstr->hasConditionalExecution() &&
                memInstr->getDestination().local()->getUsers(LocalUse::Type::READER).size() == 1)
            {
                auto nextIt = findNextValueStore(
                    it, memInstr->getDestination(), 16 /* TODO */, memInstr->getSource().local()->getBase(true));
                auto nextMemInstr = nextIt.isEndOfBlock() ? nullptr : nextIt.get<const MemoryInstruction>();
                if(nextMemInstr != nullptr && !nextIt->hasConditionalExecution() &&
                    nextMemInstr->op == MemoryOperation::WRITE &&
                    nextMemInstr->getSource().getSingleWriter() == memInstr &&
                    nextMemInstr->getSourceElementType().getPhysicalWidth() ==
                        memInstr->getDestinationElementType().getPhysicalWidth())
                {
                    // TODO also extend so value read, not modified and stored (if used otherwise) is replaced with load
                    // (for other uses) and copy -> supports other type sizes
                    logging::debug()
                        << "Found reading of memory where the sole usage writes the value back into memory: "
                        << memInstr->to_string() << logging::endl;
                    logging::debug() << "Replacing manual copy of memory with memory copy instruction for write: "
                                     << nextMemInstr->to_string() << logging::endl;

                    const Value src = memInstr->getSource();
                    it.erase();
                    nextIt.reset(new MemoryInstruction(
                        MemoryOperation::COPY, nextMemInstr->getDestination(), src, nextMemInstr->getNumEntries()));
                    // continue with the next instruction after the read in the next iteration
                    continue;
                }
            }
            for(const auto local : memInstr->getMemoryAreas())
            {
                if(mapping.find(local) != mapping.end())
                {
                    // local was already processed
                    mapping[local].accessInstructions.emplace(it);
                    continue;
                }
                mapping[local].accessInstructions.emplace(it);
                if(local->is<StackAllocation>())
                {
                    if(local->type.isSimpleType() || convertSmallArrayToRegister(local))
                    {
                        logging::debug() << "Small stack value '" << local->to_string()
                                         << "' will be stored in a register" << logging::endl;
                        mapping[local].preferred = MemoryType::QPU_REGISTER_READWRITE;
                        // we cannot pack an array into a VPM cache line, since always all 16 elements are read/written
                        // and we would overwrite the other elements
                        mapping[local].fallback =
                            local->type.isSimpleType() ? MemoryType::VPM_PER_QPU : MemoryType::RAM_READ_WRITE_VPM;
                    }
                    else if(!local->type.getElementType().getStructType())
                    {
                        logging::debug() << "Stack value '" << local->to_string()
                                         << "' will be stored in VPM per QPU (with fall-back to RAM via VPM)"
                                         << logging::endl;
                        mapping[local].preferred = MemoryType::VPM_PER_QPU;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                    else
                    {
                        logging::debug() << "Struct stack value '" << local->to_string()
                                         << "' will be stored in RAM per QPU (via VPM)" << logging::endl;
                        mapping[local].preferred = MemoryType::RAM_READ_WRITE_VPM;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                }
                else if(local->is<Global>())
                {
                    if(isMemoryOnlyRead(local))
                    {
                        // global buffer
                        if(getConstantValue(memInstr->getSource()))
                        {
                            logging::debug() << "Constant element of constant buffer '" << local->to_string()
                                             << "' will be stored in a register " << logging::endl;
                            mapping[local].preferred = MemoryType::QPU_REGISTER_READONLY;
                            mapping[local].fallback = MemoryType::RAM_LOAD_TMU;
                        }
                        else if(convertSmallArrayToRegister(local))
                        {
                            logging::debug() << "Small constant buffer '" << local->to_string()
                                             << "' will be stored in a register" << logging::endl;
                            mapping[local].preferred = MemoryType::QPU_REGISTER_READONLY;
                            mapping[local].fallback = MemoryType::RAM_LOAD_TMU;
                        }
                        else
                        {
                            logging::debug() << "Constant buffer '" << local->to_string()
                                             << "' will be read from RAM via TMU" << logging::endl;
                            mapping[local].preferred = MemoryType::RAM_LOAD_TMU;
                            // fall-back, e.g. for memory copy
                            mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                        }
                    }
                    else if(!local->type.getElementType().getStructType())
                    {
                        // local buffer
                        logging::debug() << "Local buffer '" << local->to_string()
                                         << "' will be stored in VPM (with fall-back to RAM via VPM)" << logging::endl;
                        mapping[local].preferred = MemoryType::VPM_SHARED_ACCESS;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                    else
                    {
                        // local buffer
                        logging::debug() << "Local struct '" << local->to_string() << "' will be stored in RAM via VPM"
                                         << logging::endl;
                        mapping[local].preferred = MemoryType::RAM_READ_WRITE_VPM;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                }
                else
                    // parameters MUST be handled before and there is no other type of memory objects
                    throw CompilationError(
                        CompilationStep::NORMALIZER, "Invalid local type for memory area", local->to_string(true));
            }
            if(it.has())
                allWalkers.emplace(it);
        }
        it.nextInMethod();
    }

    return std::make_pair(std::move(mapping), std::move(allWalkers));
}

static MemoryInfo canLowerToRegisterReadOnly(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // a) the global is a constant scalar/vector which fits into a single register
    auto constant = getConstantValue(baseAddr->createReference());
    if(constant)
    {
        return MemoryInfo{baseAddr, MemoryType::QPU_REGISTER_READONLY, nullptr, {}, constant};
    }
    // b) the global in a constant array small enough to be rewritten to fit into a single register (e.g. int[8])
    auto convertedType = convertSmallArrayToRegister(baseAddr);
    if(convertedType)
    {
        // convert int[8] to int8
        Value convertedValue(baseAddr->as<Global>()->value);
        convertedValue.type = *convertedType;
        return MemoryInfo{baseAddr, MemoryType::QPU_REGISTER_READONLY, nullptr, {}, convertedValue, convertedType};
    }
    // c) the global is a constant where all accesses have constant indices and therefore all accessed elements can be
    // determined at compile time
    if(std::all_of(
           access.accessInstructions.begin(), access.accessInstructions.end(), [&](InstructionWalker it) -> bool {
               return getConstantValue(it.get<const intermediate::MemoryInstruction>()->getSource()).has_value();
           }))
        return MemoryInfo{baseAddr, MemoryType::QPU_REGISTER_READONLY};

    // cannot lower to constant register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canLowerToRegisterReadWrite(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // a) the private memory fits into a single register
    if(baseAddr->type.isScalarType())
        return MemoryInfo{baseAddr, MemoryType::QPU_REGISTER_READWRITE, nullptr, {},
            method.addNewLocal(baseAddr->type, "%lowered_stack")};
    // b) the private memory is small enough to be rewritten to fit into a single register (e.g. int[4])
    auto convertedType = convertSmallArrayToRegister(baseAddr);
    if(convertedType)
        return MemoryInfo{baseAddr, MemoryType::QPU_REGISTER_READWRITE, nullptr, {},
            method.addNewLocal(*convertedType, "%lowered_stack"), convertedType};

    // cannot lower to register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canLowerToPrivateVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    auto area =
        method.vpm->addArea(baseAddr, baseAddr->type.getElementType(), true, method.metaData.getWorkGroupSize());
    if(area)
        return MemoryInfo{baseAddr, MemoryType::VPM_PER_QPU, area, {}, NO_VALUE, convertSmallArrayToRegister(baseAddr)};

    // cannot lower to register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canLowerToSharedVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    auto area = method.vpm->addArea(baseAddr, baseAddr->type.getElementType(), false);
    if(area)
        return MemoryInfo{
            baseAddr, MemoryType::VPM_SHARED_ACCESS, area, {}, NO_VALUE, convertSmallArrayToRegister(baseAddr)};

    // cannot lower to register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canMapToTMUReadOnly(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // TODO for better performance, the TMU flag should alternate in according to the order of usage (first read use
    // TMU0, second read use TMU1, ...)
    static thread_local bool tmuFlag = true;
    tmuFlag = !tmuFlag;
    return MemoryInfo{baseAddr, MemoryType::RAM_LOAD_TMU, nullptr, {}, {}, {}, tmuFlag};
}

static FastAccessList<MemoryAccessRange> determineAccessRanges(
    Method& method, const Local* baseAddr, MemoryAccess& access);
static const periphery::VPMArea* checkCacheMemoryAccessRanges(
    Method& method, const Local* baseAddr, FastAccessList<MemoryAccessRange>& accesRanges);

static MemoryInfo canMapToDMAReadWrite(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    PROFILE_START(DetermineAccessRanges);
    auto ranges = determineAccessRanges(method, baseAddr, access);
    PROFILE_END(DetermineAccessRanges);

    if(!ranges.empty())
    {
        auto area = checkCacheMemoryAccessRanges(method, baseAddr, ranges);
        if(area)
            // TODO also need to mark for initial load/write-back
            return MemoryInfo{baseAddr, MemoryType::VPM_SHARED_ACCESS, area, std::move(ranges)};
    }
    return MemoryInfo{baseAddr, MemoryType::RAM_READ_WRITE_VPM};
}

static bool isGroupUniform(const Local* local)
{
    auto writers = local->getUsers(LocalUse::Type::WRITER);
    return std::all_of(writers.begin(), writers.end(), [](const LocalUser* instr) -> bool {
        return instr->hasDecoration(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    });
}

static bool isWorkGroupUniform(const Value& val)
{
    return val.hasImmediate() || val.hasLiteral() ||
        (val.hasLocal() && isGroupUniform(val.local()))
        // XXX this is not true for the local ID UNIFORM
        || (val.hasRegister(REG_UNIFORM));
}

static FastMap<Value, InstructionDecorations> findDirectLevelAdditionInputs(const Value& val)
{
    FastMap<Value, InstructionDecorations> result;
    auto writer = val.getSingleWriter();
    if(writer == nullptr || writer->hasDecoration(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
    {
        // we have no need to split up work-group uniform values any more detailed
        auto deco = writer ? writer->decoration : InstructionDecorations::NONE;
        result.emplace(val,
            add_flag(deco,
                val.hasImmediate() || val.hasLiteral() ? InstructionDecorations::WORK_GROUP_UNIFORM_VALUE :
                                                         InstructionDecorations::NONE));
        if(val.hasImmediate() && val.immediate().getIntegerValue() >= 0)
            result[val] = add_flag(result[val], InstructionDecorations::UNSIGNED_RESULT);
        else if(val.hasLiteral() && val.literal().signedInt() >= 0)
            result[val] = add_flag(result[val], InstructionDecorations::UNSIGNED_RESULT);
        else if(val.hasRegister() && val.reg() == REG_UNIFORM)
            // XXX this is not true for the local ID UNIFORM, which should never be checked here (since the actual ID
            // needs always be extracted via non-ADDs, e.g. ANDs)
            result[val] = add_flag(result[val], InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        return result;
    }
    auto move = dynamic_cast<const MoveOperation*>(writer);
    if(move && !dynamic_cast<const VectorRotation*>(writer))
        return findDirectLevelAdditionInputs(move->getSource());

    auto op = dynamic_cast<const Operation*>(writer);
    bool onlySideEffectIsReadingUniform = op && op->hasSideEffects() && !op->doesSetFlag() &&
        !op->signal.hasSideEffects() &&
        !(op->hasValueType(ValueType::REGISTER) && op->getOutput()->reg().hasSideEffectsOnWrite()) &&
        std::all_of(op->getArguments().begin(), op->getArguments().end(), [](const Value& arg) -> bool {
            return !arg.hasRegister() || arg.reg() == REG_UNIFORM || !arg.reg().hasSideEffectsOnRead();
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

static Optional<MemoryAccessRange> determineAccessRange(Method& method, InstructionWalker it, InstructionWalker memIt)
{
    // 1. find writes to VPM DMA addresses with work-group uniform part in address values
    if(it.has<MoveOperation>() && it->assertArgument(0).hasLocal() &&
        (it->assertArgument(0).local()->is<Parameter>() || it->assertArgument(0).local()->is<Global>()))
    {
        // direct write of address (e.g. all work items write to the same location)
        // XXX if the memory is __local and the width of the writes is known, can lower into VPM (e.g. for data
        // exchange between work-items). But then the __local memory should be set small enough to fit in the VPM
        // completely, which is already handled at this point.
        logging::debug() << "DMA address is directly set to a parameter/global address, cannot be "
                            "optimized by caching multiple accesses: "
                         << it->to_string() << logging::endl;
        return {};
    }
    MemoryAccessRange range;
    range.memoryInstruction = memIt;
    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
    // base-pointer is found correctly
    auto trackIt = it;
    if(it.has<MoveOperation>() && it->assertArgument(0).getSingleWriter())
    {
        auto walker = it.getBasicBlock()->findWalkerForInstruction(it->assertArgument(0).getSingleWriter(), it);
        if(!walker)
        {
            // TODO this is actually no problem (other than finding the iterator), is it?
            logging::debug() << "Unhandled case, address is calculated in a different basic-block: " << it->to_string()
                             << logging::endl;
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
        logging::debug() << "Found VPM DMA address write with work-group uniform operand: " << it->to_string()
                         << logging::endl;
        Value varArg = *variableArg;
        // 2.1 jump over final addition of base address if it is a parameter
        if(trackIt.has<Operation>() && trackIt.get<const Operation>()->op == OP_ADD)
        {
            const auto& arg0 = trackIt->assertArgument(0);
            const auto& arg1 = trackIt->assertArgument(1);
            if(arg0.hasLocal() &&
                (arg0.local()->is<Parameter>() || arg0.local()->is<Global>() ||
                    arg0.local()->name == Method::GLOBAL_DATA_ADDRESS))
            {
                range.memoryObject = arg0.local();
                varArg = arg1;
            }
            else if(arg1.hasLocal() &&
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
            else
            {
                throw CompilationError(
                    CompilationStep::OPTIMIZER, "Unhandled case of memory access: ", trackIt->to_string());
            }
            range.baseAddressAdd = trackIt;
        }
        else
        {
            logging::debug() << "Cannot optimize further, since add of base-address and pointer was not found: "
                             << it->to_string() << logging::endl;
            return {};
        }
        auto writer = varArg.getSingleWriter();
        // 2.2 jump over shl (if any) and remember offset
        if(dynamic_cast<const Operation*>(writer) && dynamic_cast<const Operation*>(writer)->op == OP_SHL)
        {
            if(!writer->assertArgument(1).getLiteralValue() ||
                (1u << writer->assertArgument(1).getLiteralValue()->unsignedInt()) !=
                    it->assertArgument(0).type.getElementType().getPhysicalWidth())
            {
                // Abort, since the offset shifted does not match the type-width of the element type
                logging::debug() << "Cannot optimize further, since shift-offset does not match type size: "
                                 << it->to_string() << " and " << writer->to_string() << logging::endl;
                return {};
            }
            range.typeSizeShift = trackIt.getBasicBlock()->findWalkerForInstruction(writer, trackIt);
            varArg = writer->assertArgument(0);
            writer = varArg.getSingleWriter();
        }
        // 2.3 collect all directly neighboring (and directly referenced) additions
        // result is now: finalAdd + (sum(addedValues) << shiftFactor)
        auto addressParts = findDirectLevelAdditionInputs(varArg);
        // 2.4 calculate the maximum dynamic offset
        for(const auto& val : addressParts)
        {
            if(!has_flag(val.second, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
            {
                range.dynamicAddressParts.emplace(val);
                if(val.first.hasLocal())
                {
                    auto singleRange = analysis::ValueRange::getValueRange(val.first, &method);
                    range.offsetRange.minValue += singleRange.getIntRange()->minValue;
                    range.offsetRange.maxValue += singleRange.getIntRange()->maxValue;
                }
                else
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Unhandled value for memory access offset", val.first.to_string());
            }
            else
                range.groupUniformAddressParts.emplace(val);
        }
        logging::debug() << range.to_string() << logging::endl;
        return range;
    }
    return {};
}

static Optional<InstructionWalker> findSingleWriter(InstructionWalker it, const Value& val)
{
    const IntermediateInstruction* writer = nullptr;
    for(const auto& w : val.local()->getUsers(LocalUse::Type::WRITER))
    {
        if(dynamic_cast<const MemoryInstruction*>(w))
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
        logging::debug() << "Unhandled case, value does not have exactly 1 writer: " << it->to_string()
                         << logging::endl;
        return {};
    }
    auto writerIt = it.getBasicBlock()->findWalkerForInstruction(writer, it);
    if(!writerIt)
    {
        logging::debug() << "Unhandled case, address is calculated in a different basic-block: " << it->to_string()
                         << logging::endl;
        return {};
    }
    return writerIt;
}

static FastAccessList<MemoryAccessRange> determineAccessRanges(
    Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // FIXME re-enable check for caching once rest works again
    return FastAccessList<MemoryAccessRange>{};
    // NOTE: If we cannot find one access range for a local, we cannot combine any other access ranges for this local!
    static const auto copiedFromCheck = [](const InstructionWalker& it) -> bool {
        return it.get<const MemoryInstruction>()->op == MemoryOperation::COPY;
    };
    FastAccessList<MemoryAccessRange> result;
    for(auto it : access.accessInstructions)
    {
        const auto memInstr = it.get<MemoryInstruction>();
        switch(memInstr->op)
        {
        case MemoryOperation::READ:
        {
            auto writerIt = findSingleWriter(it, memInstr->getSource());
            if(writerIt)
            {
                auto res = determineAccessRange(method, writerIt.value(), it);
                if(res)
                    result.emplace_back(std::move(res.value()));
                else
                    return FastAccessList<MemoryAccessRange>{};
            }
            break;
        }
        case MemoryOperation::WRITE:
        case MemoryOperation::FILL:
        {
            auto writerIt = findSingleWriter(it, memInstr->getDestination());
            if(writerIt)
            {
                auto res = determineAccessRange(method, writerIt.value(), it);
                if(res)
                    result.emplace_back(std::move(res.value()));
                else
                    return FastAccessList<MemoryAccessRange>{};
            }
            break;
        }
        case MemoryOperation::COPY:
        {
            auto writerIt = findSingleWriter(it, memInstr->getSource());
            if(writerIt &&
                // special handling for memory which is only copied from (never read/written), since no extra space
                // is required
                !std::all_of(access.accessInstructions.begin(), access.accessInstructions.end(), copiedFromCheck))
            {
                auto res = determineAccessRange(method, writerIt.value(), it);
                if(res)
                    result.emplace_back(std::move(res.value()));
                else
                    return FastAccessList<MemoryAccessRange>{};
            }
            writerIt = findSingleWriter(it, memInstr->getDestination());
            if(writerIt)
            {
                auto res = determineAccessRange(method, writerIt.value(), it);
                if(res)
                    result.emplace_back(std::move(res.value()));
                else
                    return FastAccessList<MemoryAccessRange>{};
            }
            break;
        }
        }
    }
    return result;
}

static std::pair<bool, analysis::IntegerRange> checkWorkGroupUniformParts(
    FastAccessList<MemoryAccessRange>& accessRanges)
{
    analysis::IntegerRange offsetRange{std::numeric_limits<int>::max(), std::numeric_limits<int>::min()};
    const auto& firstUniformAddresses = accessRanges.front().groupUniformAddressParts;
    FastMap<Value, InstructionDecorations> differingUniformParts;
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
        offsetRange.minValue = std::min(offsetRange.minValue, entry.offsetRange.minValue);
        offsetRange.maxValue = std::max(offsetRange.maxValue, entry.offsetRange.maxValue);
    }
    if(!allUniformPartsEqual)
    {
        if(std::all_of(differingUniformParts.begin(), differingUniformParts.end(),
               [](const std::pair<Value, InstructionDecorations>& part) -> bool {
                   return part.first.getLiteralValue().has_value();
               }))
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
                        entry.offsetRange.minValue += it->first.getLiteralValue()->signedInt();
                        entry.offsetRange.maxValue += it->first.getLiteralValue()->signedInt();
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
            return std::make_pair(false, analysis::IntegerRange{});
    }
    return std::make_pair(true, offsetRange);
}

static const periphery::VPMArea* checkCacheMemoryAccessRanges(
    Method& method, const Local* baseAddr, FastAccessList<MemoryAccessRange>& memoryAccessRanges)
{
    auto maxNumVectors = method.vpm->getMaxCacheVectors(TYPE_INT32, true);
    GroupedAccessRanges result;

    bool allUniformPartsEqual;
    analysis::IntegerRange offsetRange;
    std::tie(allUniformPartsEqual, offsetRange) = checkWorkGroupUniformParts(memoryAccessRanges);
    if(!allUniformPartsEqual)
    {
        logging::debug() << "Cannot cache memory location " << baseAddr->to_string()
                         << " in VPM, since the work-group uniform parts of the address calculations differ, which "
                            "is not yet supported!"
                         << logging::endl;
        return nullptr;
    }
    if((offsetRange.maxValue - offsetRange.minValue) >= maxNumVectors || (offsetRange.maxValue < offsetRange.minValue))
    {
        // this also checks for any over/underflow when converting the range to unsigned int in the next steps
        logging::debug() << "Cannot cache memory location " << baseAddr->to_string()
                         << " in VPM, the accessed range is too big: [" << offsetRange.minValue << ", "
                         << offsetRange.maxValue << "]" << logging::endl;
        return nullptr;
    }
    logging::debug() << "Memory location " << baseAddr->to_string() << " is accessed via DMA in the dynamic range ["
                     << offsetRange.minValue << ", " << offsetRange.maxValue << "]" << logging::endl;

    // TODO correct type?? Shouldn't it be baseAddr->type.getElmentType().toArrayType(...??
    auto accessedType = baseAddr->type.toArrayType(
        static_cast<unsigned>(offsetRange.maxValue - offsetRange.minValue + 1 /* bounds of range are inclusive! */));

    // XXX the local is not correct, at least not if there is a work-group uniform offset, but since all work-items
    // use the same work-group offset, it doesn't matter
    auto vpmArea = method.vpm->addArea(baseAddr, accessedType, false);
    if(vpmArea == nullptr)
    {
        logging::debug() << "Memory location " << baseAddr->to_string() << " with dynamic access range ["
                         << offsetRange.minValue << ", " << offsetRange.maxValue
                         << "] cannot be cached in VPM, since it does not fit" << logging::endl;
        return nullptr;
    }
    return vpmArea;
}