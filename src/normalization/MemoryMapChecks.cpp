/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryMappings.h"

#include "../GlobalValues.h"
#include "../Profiler.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

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
    if(auto literal = global->initialValue.getScalar())
        // scalar value
        return Value(*literal, global->initialValue.type.getElementType());
    if(global->initialValue.isZeroInitializer())
        // all entries are the same
        return Value::createZeroInitializer(global->initialValue.type.getElementType());
    if(global->initialValue.isUndefined())
        // all entries are undefined
        return Value(global->initialValue.type.getElementType());
    auto globalContainer = global->initialValue.getCompound();
    if(global->initialValue.isAllSame())
        // all entries are the same
        return globalContainer->at(0).toValue();
    if(globalContainer && source.local()->reference.second >= 0)
        // fixed index
        return globalContainer->at(source.local()->reference.second).toValue();
    if(auto val = global->initialValue.toValue())
    {
        if(auto vector = val->checkVector())
        {
            return vector->isElementNumber() ? ELEMENT_NUMBER_REGISTER : NO_VALUE;
        }
    }
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
    if(auto ptrType = base->type.getPointerType())
    {
        const auto& baseType = ptrType->elementType;
        auto arrayType = baseType.getArrayType();
        if(arrayType && arrayType->size <= NATIVE_VECTOR_SIZE && arrayType->elementType.isScalarType())
            return arrayType->elementType.toVectorType(static_cast<uint8_t>(arrayType->size));
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

    if(base->type.getPointerType() && base->type.getPointerType()->addressSpace == AddressSpace::CONSTANT)
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
        if(it.get<MemoryBarrier>() || it.get<Branch>() || it.get<MutexLock>() || it.get<SemaphoreAdjustment>())
            break;
        it.nextInBlock();
        --limit;
    }
    return it.getBasicBlock()->walkEnd();
}

std::pair<MemoryAccessMap, FastSet<InstructionWalker>> normalization::determineMemoryAccess(Method& method)
{
    // TODO lower local/private struct-elements into VPM?! At least for single structs
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Determining memory access for kernel: " << method.name << logging::endl);
    MemoryAccessMap mapping;
    FastSet<InstructionWalker> allWalkers;
    for(const auto& param : method.parameters)
    {
        if(!param.type.getPointerType())
            continue;
        auto pointerType = param.type.getPointerType();
        if(pointerType->addressSpace == AddressSpace::CONSTANT)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Constant parameter '" << param.to_string() << "' will be read from RAM via TMU"
                    << logging::endl);
            mapping[&param].preferred = MemoryAccessType::RAM_LOAD_TMU;
            // fall-back, e.g. for memory copy
            mapping[&param].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
        }
        else if(pointerType->addressSpace == AddressSpace::GLOBAL)
        {
            if(isMemoryOnlyRead(&param))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Global parameter '" << param.to_string()
                        << "' without any write access will be read from RAM via TMU" << logging::endl);
                mapping[&param].preferred = MemoryAccessType::RAM_LOAD_TMU;
                // fall-back, e.g. for memory copy
                mapping[&param].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
            }
            else
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Global parameter '" << param.to_string()
                        << "' which is written to will be stored in RAM and accessed via VPM" << logging::endl);
                mapping[&param].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
                mapping[&param].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
            }
        }
        else if(pointerType->addressSpace == AddressSpace::LOCAL)
        {
            // TODO if last access index is known and fits into VPM, set for VPM-or-RAM
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local parameter '" << param.to_string() << "' will be stored in RAM and accessed via VPM"
                    << logging::endl);
            mapping[&param].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
            mapping[&param].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
        }
        else
            throw CompilationError(
                CompilationStep::NORMALIZER, "Invalid address space for pointer parameter", param.to_string(true));
    }

    InstructionWalker it = method.walkAllInstructions();
    // The 64 bit store instructions which need to be split up in two 32 bit store instructions and the locals for the
    // lower and upper words
    FastMap<const MemoryInstruction*, std::pair<const Local*, const Local*>> rewrite64BitStoresTo32;
    while(!it.isEndOfMethod())
    {
        if(auto memInstr = it.get<MemoryInstruction>())
        {
            auto rewriteParts = rewrite64BitStoresTo32.find(memInstr);
            if(rewriteParts != rewrite64BitStoresTo32.end())
            {
                // rewrite the 64-bit store to 2 32-bit stores
                auto lowerIndex = Value(memInstr->getDestination().local(), method.createPointerType(TYPE_INT32));
                it.emplace(new MemoryInstruction(
                    MemoryOperation::WRITE, Value(lowerIndex), rewriteParts->second.first->createReference()));
                it->copyExtrasFrom(memInstr);
                auto startIt = it;
                it.nextInBlock();
                auto upperIndex = assign(it, lowerIndex.type) = lowerIndex + 4_val;
                if(auto ref = lowerIndex.local()->reference.first)
                    upperIndex.local()->reference.first = ref;
                it.emplace(new MemoryInstruction(
                    MemoryOperation::WRITE, std::move(upperIndex), rewriteParts->second.second->createReference()));
                it->copyExtrasFrom(memInstr);
                it.nextInBlock();

                rewrite64BitStoresTo32.erase(rewriteParts);
                it.erase();

                // continue with exactly the first store instruction
                memInstr = startIt.get<MemoryInstruction>();
                it = startIt;
            }
            else if(memInstr->op == MemoryOperation::READ && !memInstr->hasConditionalExecution() &&
                memInstr->getDestination().local()->getUsers(LocalUse::Type::READER).size() == 1)
            {
                // convert read-then-write to copy
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
                    LCOV_EXCL_START
                    CPPLOG_LAZY_BLOCK(
                        logging::Level::DEBUG, {
                            logging::debug()
                                << "Found reading of memory where the sole usage writes the value back into memory: "
                                << memInstr->to_string() << logging::endl;
                            logging::debug()
                                << "Replacing manual copy of memory with memory copy instruction for write: "
                                << nextMemInstr->to_string() << logging::endl;
                        });
                    LCOV_EXCL_STOP

                    auto src = memInstr->getSource();
                    it.erase();
                    nextIt.reset(new MemoryInstruction(MemoryOperation::COPY, Value(nextMemInstr->getDestination()),
                        std::move(src), Value(nextMemInstr->getNumEntries())));
                    // continue with the next instruction after the read in the next iteration
                    continue;
                }
            }
            // fall-through on purpose, since the below block also handles cases which match the first condition of the
            // above block, but not the second (e.g. read and write of 64-bit struct in different blocks)
            if(memInstr->op == MemoryOperation::READ && !memInstr->hasConditionalExecution() &&
                memInstr->getDestination().type.getElementType() == TYPE_INT64 && memInstr->getSource().checkLocal() &&
                memInstr->getSource().local()->getBase(true)->type.getElementType().getStructType())
            {
                // This is a "read of a 64-bit integer value from a memory area which refers to a struct pointer"
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found reading of 64-bit integer bit-cast from struct pointer: " << memInstr->to_string()
                        << logging::endl);
                bool canBeSplitUp = true;
                FastSet<const MoveOperation*> movesToRewrite;
                FastSet<const Operation*> shiftsToRewrite;
                FastSet<const MemoryInstruction*> storesToRewrite;

                // Check whether all uses are used in a supported way (e.g. actually only access of the lower 32 bit or
                // copy the whole value)
                for(auto user : memInstr->getDestination().local()->getUsers(LocalUse::Type::READER))
                {
                    if(auto move = dynamic_cast<const MoveOperation*>(user))
                    {
                        // check for truncation from 64 to 32 bits
                        if(move->getOutput()->type.getScalarBitCount() != 32)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Unsupported move, aborting rewrite: " << move->to_string() << logging::endl);
                            canBeSplitUp = false;
                            break;
                        }
                        // moves which truncate the 64 bit integer to 32 bit are accepted
                        movesToRewrite.emplace(move);
                    }
                    else if(auto memAccess = dynamic_cast<const MemoryInstruction*>(user))
                    {
                        // memory writes accesses using this value are accepted
                        if(memAccess->op != MemoryOperation::WRITE)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Unsupported memory access, aborting rewrite: " << memAccess->to_string()
                                    << logging::endl);
                            canBeSplitUp = false;
                            break;
                        }
                        storesToRewrite.emplace(memAccess);
                    }
                    else if(auto op = dynamic_cast<const Operation*>(user))
                    {
                        if(op->op == OP_SHR && op->getSecondArg())
                        {
                            auto shiftOffset = op->assertArgument(1).getLiteralValue();
                            if(auto writer = op->assertArgument(1).getSingleWriter())
                            {
                                auto precalc = writer->precalculate();
                                shiftOffset = precalc.first ? precalc.first->getLiteralValue() : shiftOffset;
                            }

                            if(shiftOffset == 32_lit)
                            {
                                // shift exactly 1 word -> reads upper word
                                shiftsToRewrite.emplace(op);
                            }
                            else
                            {
                                CPPLOG_LAZY(logging::Level::DEBUG,
                                    log << "Unsupported shift of 64 bit integer, aborting rewrite: "
                                        << user->to_string() << logging::endl);
                                canBeSplitUp = false;
                            }
                        }
                        else
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Unsupported operation with 64 bit integer, aborting rewrite: "
                                    << user->to_string() << logging::endl);
                            canBeSplitUp = false;
                        }
                    }
                    else
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Unsupported access to 64 bit integer, aborting rewrite: " << user->to_string()
                                << logging::endl);
                        canBeSplitUp = false;
                    }
                }
                if(canBeSplitUp)
                {
                    // split load into 2 loads (upper and lower word), mark stores for conversion
                    // TODO move both inserted loads (and also both inserted stores) into one mutex block (load/store 2
                    // values at once)
                    auto origLocal = memInstr->getDestination();
                    auto lowerLocal = method.addNewLocal(TYPE_INT32, origLocal.local()->name + ".lower");
                    auto upperLocal = method.addNewLocal(TYPE_INT32, origLocal.local()->name + ".upper");

                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Splitting '" << origLocal.to_string() << "' into '" << lowerLocal.to_string()
                            << "' and '" << upperLocal.to_string() << '\'' << logging::endl);

                    auto lowerIndex = Value(memInstr->getSource().local(), method.createPointerType(TYPE_INT32));
                    it.emplace(new MemoryInstruction(MemoryOperation::READ, Value(lowerLocal), Value(lowerIndex)));
                    it->copyExtrasFrom(memInstr);
                    auto startIt = it;
                    it.nextInBlock();
                    auto upperIndex = assign(it, lowerIndex.type) = lowerIndex + 4_val;
                    if(auto ref = lowerIndex.local()->reference.first)
                        upperIndex.local()->reference.first = ref;
                    it.emplace(new MemoryInstruction(MemoryOperation::READ, Value(upperLocal), std::move(upperIndex)));
                    it->copyExtrasFrom(memInstr);
                    it.nextInBlock();

                    for(auto store : storesToRewrite)
                    {
                        // mark all stores for rewrite
                        rewrite64BitStoresTo32.emplace(store,
                            std::pair<const Local*, const Local*>{lowerLocal.checkLocal(), upperLocal.checkLocal()});
                    }

                    for(auto move : movesToRewrite)
                        // replace all truncations with usage of lower word
                        const_cast<MoveOperation*>(move)->replaceValue(origLocal, lowerLocal, LocalUse::Type::READER);

                    for(auto shift : shiftsToRewrite)
                    {
                        // replace all shifts of the upper word to lower with upper word
                        // use a shift by zero, so we do not need to access the instruction walker
                        const_cast<Operation*>(shift)->setArgument(0, upperLocal);
                        const_cast<Operation*>(shift)->setArgument(1, 0_val);
                    }

                    it.erase();

                    // continue with exactly the first load instruction
                    memInstr = startIt.get<MemoryInstruction>();
                    it = startIt;
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
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Small stack value '" << local->to_string() << "' will be stored in a register"
                                << logging::endl);
                        mapping[local].preferred = MemoryAccessType::QPU_REGISTER_READWRITE;
                        // we cannot pack an array into a VPM cache line, since always all 16 elements are read/written
                        // and we would overwrite the other elements
                        mapping[local].fallback = local->type.isSimpleType() ? MemoryAccessType::VPM_PER_QPU :
                                                                               MemoryAccessType::RAM_READ_WRITE_VPM;
                    }
                    else if(!local->type.getElementType().getStructType())
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Stack value '" << local->to_string()
                                << "' will be stored in VPM per QPU (with fall-back to RAM via VPM)" << logging::endl);
                        mapping[local].preferred = MemoryAccessType::VPM_PER_QPU;
                        mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
                    }
                    else
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Struct stack value '" << local->to_string()
                                << "' will be stored in RAM per QPU (via VPM)" << logging::endl);
                        mapping[local].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
                        mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
                    }
                }
                else if(local->is<Global>())
                {
                    if(isMemoryOnlyRead(local))
                    {
                        // global buffer
                        if(getConstantValue(memInstr->getSource()))
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Constant element of constant buffer '" << local->to_string()
                                    << "' will be stored in a register " << logging::endl);
                            mapping[local].preferred = MemoryAccessType::QPU_REGISTER_READONLY;
                            mapping[local].fallback = MemoryAccessType::RAM_LOAD_TMU;
                        }
                        else if(convertSmallArrayToRegister(local))
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Small constant buffer '" << local->to_string()
                                    << "' will be stored in a register" << logging::endl);
                            mapping[local].preferred = MemoryAccessType::QPU_REGISTER_READONLY;
                            mapping[local].fallback = MemoryAccessType::RAM_LOAD_TMU;
                        }
                        else
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Constant buffer '" << local->to_string() << "' will be read from RAM via TMU"
                                    << logging::endl);
                            mapping[local].preferred = MemoryAccessType::RAM_LOAD_TMU;
                            // fall-back, e.g. for memory copy
                            mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
                        }
                    }
                    else if(!local->type.getElementType().getStructType())
                    {
                        // local buffer
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Local buffer '" << local->to_string()
                                << "' will be stored in VPM (with fall-back to RAM via VPM)" << logging::endl);
                        mapping[local].preferred = MemoryAccessType::VPM_SHARED_ACCESS;
                        mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
                    }
                    else
                    {
                        // local buffer
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Local struct '" << local->to_string() << "' will be stored in RAM via VPM"
                                << logging::endl);
                        mapping[local].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
                        mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
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
        return MemoryInfo{baseAddr, MemoryAccessType::QPU_REGISTER_READONLY, nullptr, {}, constant};
    }
    // b) the global in a constant array small enough to be rewritten to fit into a single register (e.g. int[8])
    if(auto convertedType = convertSmallArrayToRegister(baseAddr))
    {
        // convert int[8] to int8
        if(auto convertedValue = baseAddr->as<Global>()->initialValue.toValue())
        {
            convertedValue->type = *convertedType;
            return MemoryInfo{
                baseAddr, MemoryAccessType::QPU_REGISTER_READONLY, nullptr, {}, convertedValue, convertedType};
        }
    }
    // c) the global is a constant where all accesses have constant indices and therefore all accessed elements can be
    // determined at compile time
    if(std::all_of(
           access.accessInstructions.begin(), access.accessInstructions.end(), [&](InstructionWalker it) -> bool {
               return getConstantValue(it.get<const intermediate::MemoryInstruction>()->getSource()).has_value();
           }))
        return MemoryInfo{baseAddr, MemoryAccessType::QPU_REGISTER_READONLY};

    // cannot lower to constant register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canLowerToRegisterReadWrite(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // a) the private memory fits into a single register
    if(baseAddr->type.isScalarType())
        return MemoryInfo{baseAddr, MemoryAccessType::QPU_REGISTER_READWRITE, nullptr, {},
            method.addNewLocal(baseAddr->type, "%lowered_stack")};
    // b) the private memory is small enough to be rewritten to fit into a single register (e.g. int[4])
    auto convertedType = convertSmallArrayToRegister(baseAddr);
    if(convertedType)
        return MemoryInfo{baseAddr, MemoryAccessType::QPU_REGISTER_READWRITE, nullptr, {},
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
        return MemoryInfo{
            baseAddr, MemoryAccessType::VPM_PER_QPU, area, {}, NO_VALUE, convertSmallArrayToRegister(baseAddr)};

    // cannot lower to register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canLowerToSharedVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    auto area = method.vpm->addArea(baseAddr, baseAddr->type.getElementType(), false);
    if(area)
        return MemoryInfo{
            baseAddr, MemoryAccessType::VPM_SHARED_ACCESS, area, {}, NO_VALUE, convertSmallArrayToRegister(baseAddr)};

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
    return MemoryInfo{baseAddr, MemoryAccessType::RAM_LOAD_TMU, nullptr, {}, {}, {}, tmuFlag};
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
            return MemoryInfo{baseAddr, MemoryAccessType::VPM_SHARED_ACCESS, area, std::move(ranges)};
    }
    return MemoryInfo{baseAddr, MemoryAccessType::RAM_READ_WRITE_VPM};
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
    return val.checkImmediate() || val.checkLiteral() ||
        (val.checkLocal() && isGroupUniform(val.local()))
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
    if(move && !dynamic_cast<const VectorRotation*>(writer))
        return findDirectLevelAdditionInputs(move->getSource());

    auto op = dynamic_cast<const Operation*>(writer);
    bool onlySideEffectIsReadingUniform = op && op->hasSideEffects() && !op->doesSetFlag() &&
        !op->signal.hasSideEffects() &&
        !(op->hasValueType(ValueType::REGISTER) && op->getOutput()->reg().hasSideEffectsOnWrite()) &&
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

static Optional<MemoryAccessRange> determineAccessRange(Method& method, InstructionWalker it, InstructionWalker memIt)
{
    // 1. find writes to VPM DMA addresses with work-group uniform part in address values
    if(it.get<MoveOperation>() && it->assertArgument(0).checkLocal() &&
        (it->assertArgument(0).local()->is<Parameter>() || it->assertArgument(0).local()->is<Global>()))
    {
        // direct write of address (e.g. all work items write to the same location)
        // XXX if the memory is __local and the width of the writes is known, can lower into VPM (e.g. for data
        // exchange between work-items). But then the __local memory should be set small enough to fit in the VPM
        // completely, which is already handled at this point.
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "DMA address is directly set to a parameter/global address, cannot be "
                   "optimized by caching multiple accesses: "
                << it->to_string() << logging::endl);
        return {};
    }
    MemoryAccessRange range;
    range.memoryInstruction = memIt;
    // if the instruction is a move, handle/skip it here, so the add with the shifted offset +
    // base-pointer is found correctly
    auto trackIt = it;
    if(it.get<MoveOperation>() && it->assertArgument(0).getSingleWriter())
    {
        auto walker = it.getBasicBlock()->findWalkerForInstruction(it->assertArgument(0).getSingleWriter(), it);
        if(!walker)
        {
            // TODO this is actually no problem (other than finding the iterator), is it?
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
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Found VPM DMA address write with work-group uniform operand: " << it->to_string() << logging::endl);
        Value varArg = *variableArg;
        // 2.1 jump over final addition of base address if it is a parameter
        if(trackIt.get<Operation>() && trackIt.get<const Operation>()->op == OP_ADD)
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
            else
            {
                throw CompilationError(
                    CompilationStep::OPTIMIZER, "Unhandled case of memory access: ", trackIt->to_string());
            }
            range.baseAddressAdd = trackIt;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot optimize further, since add of base-address and pointer was not found: "
                    << it->to_string() << logging::endl);
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
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Cannot optimize further, since shift-offset does not match type size: " << it->to_string()
                        << " and " << writer->to_string() << logging::endl);
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
                if(val.first.checkLocal())
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
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Unhandled case, value does not have exactly 1 writer: " << it->to_string() << logging::endl);
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
        LCOV_EXCL_START
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Cannot cache memory location " << baseAddr->to_string()
                << " in VPM, since the work-group uniform parts of the address calculations differ, which "
                   "is not yet supported!"
                << logging::endl);
        LCOV_EXCL_STOP
        return nullptr;
    }
    if((offsetRange.maxValue - offsetRange.minValue) >= maxNumVectors || (offsetRange.maxValue < offsetRange.minValue))
    {
        // this also checks for any over/underflow when converting the range to unsigned int in the next steps
        LCOV_EXCL_START
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Cannot cache memory location " << baseAddr->to_string()
                << " in VPM, the accessed range is too big: [" << offsetRange.minValue << ", " << offsetRange.maxValue
                << "]" << logging::endl);
        LCOV_EXCL_STOP
        return nullptr;
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Memory location " << baseAddr->to_string() << " is accessed via DMA in the dynamic range ["
            << offsetRange.minValue << ", " << offsetRange.maxValue << "]" << logging::endl);

    // TODO correct type?? Shouldn't it be baseAddr->type.getElmentType().toArrayType(...??
    auto accessedType = method.createArrayType(baseAddr->type,
        static_cast<unsigned>(offsetRange.maxValue - offsetRange.minValue + 1 /* bounds of range are inclusive! */));

    // XXX the local is not correct, at least not if there is a work-group uniform offset, but since all work-items
    // use the same work-group offset, it doesn't matter
    auto vpmArea = method.vpm->addArea(baseAddr, accessedType, false);
    if(vpmArea == nullptr)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Memory location " << baseAddr->to_string() << " with dynamic access range [" << offsetRange.minValue
                << ", " << offsetRange.maxValue << "] cannot be cached in VPM, since it does not fit" << logging::endl);
        return nullptr;
    }
    return vpmArea;
}