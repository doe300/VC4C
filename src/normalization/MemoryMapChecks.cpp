/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryMappings.h"

#include "../GlobalValues.h"
#include "../Profiler.h"
#include "../SIMDVector.h"
#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <array>

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

static constexpr std::array<MappingCheck, 6> CHECKS = {
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

Optional<Value> normalization::getConstantContainerValue(const Value& source)
{
    // can only read from constant global data, so the global is always the source
    const Global* global = source.local()->getBase(true)->as<Global>();
    if(!global)
        return NO_VALUE;
    if(auto literal = global->initialValue.getScalar())
        // scalar value
        return Value(*literal, global->initialValue.type);
    if(global->initialValue.isZeroInitializer())
    {
        // all entries are zero (or undefined)
        if(auto zeroInitializer = Value::createZeroInitializer(global->initialValue.type))
            return zeroInitializer;
    }
    if(global->initialValue.isUndefined())
        // all entries are undefined
        return Value(global->initialValue.type);
    // all entries are the same
    auto value = global->initialValue.toValue();
    if(value)
    {
        if(value->checkVector() && value->checkVector()->isElementNumber())
            return ELEMENT_NUMBER_REGISTER;
        // rewrite array fitting into vector to vector for better handling (esp. for lowered registers)
        auto arrayType = value->type.getArrayType();
        if(arrayType && arrayType->size <= NATIVE_VECTOR_SIZE)
            value->type = arrayType->elementType.toVectorType(static_cast<uint8_t>(arrayType->size));
    }
    return value;
}

Optional<Value> normalization::getConstantElementValue(const Value& source)
{
    // can only read from constant global data, so the global is always the source
    const Global* global = source.local()->getBase(true)->as<Global>();
    if(!global)
        return NO_VALUE;
    if(auto container = getConstantContainerValue(source))
    {
        if(container->isUndefined())
            return Value(container->type.getElementType());
        if(auto lit = container->getLiteralValue())
            return Value(*lit, container->type.getElementType());
        if(auto vec = container->checkVector())
        {
            if(auto lit = vec->getAllSame())
                return Value(*lit, container->type.getElementType());
            if(vec->isElementNumber())
                return ELEMENT_NUMBER_REGISTER;
        }
        if(auto reg = container->checkRegister())
            return container;
    }
    auto globalContainer = global->initialValue.getCompound();
    auto sourceData = source.local()->get<ReferenceData>();
    if(globalContainer && sourceData && sourceData->offset >= 0)
        // fixed index
        // TODO this might be wrong! The offset is not necessarily the offset directly into the global, it could be a
        // sub-offset!
        return globalContainer->at(static_cast<std::size_t>(sourceData->offset)).toValue();
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
    // TODO can't we also lower e.g. uint4[4] into 1 register? Shouldn't be a problem if element access is implemented
    // correctly, since the anyway can use vloadN/vstoreN to access vectors of any size. Just need to make sure not to
    // set whole vector when storing first element!
    // TODO for gentype3/vload3/vstore3, need to make sure to either use padded or not-padded size for all access!
    const Local* base = local->getBase(true);
    if(auto ptrType = base->type.getPointerType())
    {
        const auto& baseType = ptrType->elementType;
        auto arrayType = baseType.getArrayType();
        if(arrayType && arrayType->size <= NATIVE_VECTOR_SIZE && arrayType->elementType.isScalarType())
            return arrayType->elementType.toVectorType(static_cast<uint8_t>(arrayType->size));
        if(auto pointerType = baseType.getPointerType())
            // pointer to pointer (the content is a pointer) fits into register
            return baseType;
        if(baseType.isSimpleType())
            // any simple type fits into register
            return baseType;
    }
    return {};
}

/**
 * Follow all locals derived from the given local (e.g. to calculate address offsets) and see whether any of them is
 * used in a memory write instruction.
 */
static bool hasMemoryWrites(const Local* local)
{
    FastSet<const Local*> closed;
    FastSet<const Local*> open{local};
    while(!open.empty())
    {
        auto loc = *open.begin();
        open.erase(open.begin());
        closed.emplace(loc);

        bool isWrittenTo = false;
        // memory store/fill/etc. instructions "write" the written-to address, so we have to check for address "writers"
        loc->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
            if(auto memInst = dynamic_cast<const MemoryInstruction*>(writer))
                isWrittenTo = true;
        });
        if(isWrittenTo)
            return true;

        loc->forUsers(LocalUse::Type::READER, [&](const LocalUser* reader) {
            if(auto memInst = dynamic_cast<const MemoryInstruction*>(reader))
                // do not continue tracking the read value (or the copied-to address)
                return;
            if(auto out = reader->checkOutputLocal())
            {
                if(closed.find(out) == closed.end())
                    open.emplace(out);
            }
        });
    }

    return false;
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

    // as a fallback, check for actual memory writes
    return !hasMemoryWrites(local);
}

// Finds the next instruction writing the given value into memory
static NODISCARD InstructionWalker findNextValueStore(
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

static NODISCARD std::pair<InstructionWalker, InstructionWalker> insert64BitWrite(Method& method, InstructionWalker it,
    Local* address, Value lower, Value upper, MemoryInstruction* origInstruction = nullptr)
{
    if(origInstruction->guardAccess)
    {
        it.emplace(new MutexLock(MutexAccess::LOCK));
        it.nextInBlock();
    }
    auto lowerIndex = Value(address, method.createPointerType(TYPE_INT32));
    it.emplace(
        new MemoryInstruction(MemoryOperation::WRITE, Value(lowerIndex), std::move(lower), Value(INT_ONE), false));
    if(origInstruction)
        it->copyExtrasFrom(origInstruction);
    auto startIt = it;
    it.nextInBlock();
    auto upperIndex = assign(it, lowerIndex.type) = lowerIndex + 4_val;
    if(auto data = lowerIndex.local()->get<ReferenceData>())
        upperIndex.local()->set(ReferenceData(*data));
    it.emplace(
        new MemoryInstruction(MemoryOperation::WRITE, std::move(upperIndex), std::move(upper), Value(INT_ONE), false));
    if(origInstruction)
        it->copyExtrasFrom(origInstruction);
    it.nextInBlock();
    if(origInstruction->guardAccess)
    {
        it.emplace(new MutexLock(MutexAccess::RELEASE));
        it.nextInBlock();
    }
    return std::make_pair(it, startIt);
}

static Optional<FastSet<const IntermediateInstruction*>> checkAllWritersArePhiNodesOrSelections(
    const Local* local, const tools::SmallSortedPointerSet<const IntermediateInstruction*>& excludedInstructions)
{
    bool allWritersArePhiNodes = true;
    bool allWritersAreSelections = true;
    FastSet<const IntermediateInstruction*> phiNodes;
    FastSet<const IntermediateInstruction*> selections;
    local->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
        if(excludedInstructions.find(writer) != excludedInstructions.end())
            // skip the (possible write) we are currently processing
            return;
        if(writer->hasDecoration(InstructionDecorations::PHI_NODE))
        {
            if(writer->assertArgument(0).checkLocal())
                phiNodes.emplace(writer);
            else
                allWritersArePhiNodes = false;
            allWritersAreSelections = false;
        }
        else if(writer->getMoveSource() && writer->hasConditionalExecution())
        {
            // TODO more precise check? E.g. for same setting of flag?
            if(writer->assertArgument(0).checkLocal())
                selections.emplace(writer);
            else
                allWritersAreSelections = false;
            allWritersArePhiNodes = false;
        }
        else
        {
            allWritersArePhiNodes = false;
            allWritersAreSelections = false;
        }
    });
    if(!phiNodes.empty() && allWritersArePhiNodes)
        return phiNodes;
    if(!selections.empty() && allWritersAreSelections)
        return selections;
    return {};
}

// Memory is accessed conditionally (either address X or address Y, ...), e.g. for phi-nodes of select-statements
// (?:-operator).
struct ConditionalMemoryAccess
{
    // The instruction doing the conditional write, e.g. the phi-node or the conditional move for a select-statement
    const IntermediateInstruction* conditionalWrite;
    Variant<const Local*, Value> source;

    inline bool operator==(const ConditionalMemoryAccess& other) const noexcept
    {
        return conditionalWrite == other.conditionalWrite;
    }

    std::string to_string() const
    {
        if(auto loc = VariantNamespace::get_if<const Local*>(&source))
            return (*loc)->to_string();
        return VariantNamespace::get<Value>(source).to_string();
    }
};

namespace std
{
    template <>
    struct hash<ConditionalMemoryAccess> : private std::hash<const IntermediateInstruction*>
    {
        inline std::size_t operator()(const ConditionalMemoryAccess& access) const noexcept
        {
            return std::hash<const IntermediateInstruction*>::operator()(access.conditionalWrite);
        }
    };

} // namespace std

static void mapConditionalOrPhiWrites(const FastSet<const IntermediateInstruction*>& conditonalWrites,
    FastSet<const ConditionalMemoryAccess*>& localAccesses, FastSet<ConditionalMemoryAccess>& conditionalAddressWrites)
{
    for(auto writer : conditonalWrites)
    {
        if(const Local* loc = writer->assertArgument(0).local())
        {
            loc = loc->getBase(true);
            if(loc->residesInMemory() || loc->is<Parameter>())
            {
                auto tmp = conditionalAddressWrites.emplace(ConditionalMemoryAccess{writer, loc});
                localAccesses.emplace(&*tmp.first);
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "\tconditional write " << writer->to_string() << " of memory area: " << loc->to_string(true)
                        << logging::endl);
                continue;
            }
            if(loc->getSingleWriter() &&
                (loc->getSingleWriter()->hasDecoration(InstructionDecorations::PHI_NODE) ||
                    loc->getSingleWriter()->hasConditionalExecution()))
            {
                auto writer = loc->getSingleWriter();
                auto sourceValue = writer->getMoveSource().value_or(writer->getOutput().value());
                auto sourceWriter = check(sourceValue.getSingleWriter()) | check(writer);
                auto tmp = conditionalAddressWrites.emplace(ConditionalMemoryAccess{writer, sourceValue});
                localAccesses.emplace(&*tmp.first);
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "\tconditional write " << writer->to_string()
                        << " of address write: " << sourceWriter->to_string() << logging::endl);
                continue;
            }
        }
        if(writer->hasDecoration(InstructionDecorations::PHI_NODE) || writer->hasConditionalExecution())
        {
            auto sourceValue = writer->getMoveSource().value_or(writer->getOutput().value());
            auto sourceWriter = check(sourceValue.getSingleWriter()) | check(writer);
            auto tmp = conditionalAddressWrites.emplace(ConditionalMemoryAccess{writer, sourceValue});
            localAccesses.emplace(&*tmp.first);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "\tconditional write " << writer->to_string()
                    << " of address write: " << sourceWriter->to_string() << logging::endl);
            continue;
        }
        throw CompilationError(CompilationStep::NORMALIZER,
            "Unhandled case of conditional address write used as source for memory access", writer->to_string());
    }
}

static MemoryAccessType getFallbackType(MemoryAccessType preferredType)
{
    switch(preferredType)
    {
    case MemoryAccessType::QPU_REGISTER_READONLY:
        return MemoryAccessType::RAM_LOAD_TMU;
    case MemoryAccessType::QPU_REGISTER_READWRITE:
        return MemoryAccessType::VPM_PER_QPU;
    case MemoryAccessType::VPM_PER_QPU:
    case MemoryAccessType::VPM_SHARED_ACCESS:
    case MemoryAccessType::RAM_LOAD_TMU:
    case MemoryAccessType::RAM_READ_WRITE_VPM:
        return MemoryAccessType::RAM_READ_WRITE_VPM;
    }
    throw CompilationError(CompilationStep::NORMALIZER, "Cannot determine fallback for unknown memory access type",
        std::to_string(static_cast<unsigned>(preferredType)));
}

static void addConditionalLocalMapping(const Local* loc, const ConditionalMemoryAccess& conditionalWrite,
    FastMap<const ConditionalMemoryAccess*, MemoryAccess*>& conditionalAccesses,
    FastMap<const Local*, FastSet<const Local*>>& conditionalLocalMappings, MemoryAccessMap& mapping)
{
    auto mappingIt = mapping.find(loc);
    if(mappingIt == mapping.end())
        // TODO what to do if the memory area is not yet mapped (i.e. only accessed via phi-nodes)?
        // Would need to run the checks/mappings recursive?!
        throw CompilationError(CompilationStep::NORMALIZER,
            "Memory which is only accessed via conditional address writes is not supported yet", loc->to_string());
    conditionalAccesses.emplace(&conditionalWrite, &mappingIt->second);
    conditionalLocalMappings[conditionalWrite.conditionalWrite->checkOutputLocal()].emplace(loc);
}

static const Local* determineSingleMemoryAreaMapping(MemoryAccessMap& mapping, InstructionWalker it, const Local* local,
    const intermediate::MemoryInstruction* memInstr,
    FastMap<InstructionWalker, std::pair<const Local*, FastSet<const ConditionalMemoryAccess*>>>&
        conditionalWrittenMemoryAccesses,
    FastSet<ConditionalMemoryAccess>& conditionalAddressWrites)
{
    if(mapping.find(local) != mapping.end())
    {
        // local was already processed
        mapping[local].accessInstructions.emplace(it, local);
        return local;
    }
    if(local->is<StackAllocation>())
    {
        mapping[local].accessInstructions.emplace(it, local);
        if(local->type.isSimpleType() || convertSmallArrayToRegister(local))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Small stack value '" << local->to_string() << "' can be stored in a register" << logging::endl);
            mapping[local].preferred = MemoryAccessType::QPU_REGISTER_READWRITE;
            // we cannot pack an array into a VPM cache line, since always all 16 elements are read/written
            // and we would overwrite the other elements
            mapping[local].fallback =
                local->type.isSimpleType() ? MemoryAccessType::VPM_PER_QPU : MemoryAccessType::RAM_READ_WRITE_VPM;
        }
        else if(!local->type.getElementType().getStructType())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Stack value '" << local->to_string()
                    << "' can be stored in VPM per QPU (with fall-back to RAM via VPM)" << logging::endl);
            mapping[local].preferred = MemoryAccessType::VPM_PER_QPU;
            mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Struct stack value '" << local->to_string() << "' can be stored in RAM per QPU (via VPM)"
                    << logging::endl);
            mapping[local].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
            mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
        }
    }
    else if(local->is<Global>())
    {
        mapping[local].accessInstructions.emplace(it, local);
        if(isMemoryOnlyRead(local))
        {
            // global buffer
            if(getConstantElementValue(memInstr->getSource()))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Constant element of constant buffer '" << local->to_string()
                        << "' can be stored in a register " << logging::endl);
                mapping[local].preferred = MemoryAccessType::QPU_REGISTER_READONLY;
                mapping[local].fallback = MemoryAccessType::RAM_LOAD_TMU;
            }
            else if(convertSmallArrayToRegister(local))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Small constant buffer '" << local->to_string() << "' can be stored in a register"
                        << logging::endl);
                mapping[local].preferred = MemoryAccessType::QPU_REGISTER_READONLY;
                mapping[local].fallback = MemoryAccessType::RAM_LOAD_TMU;
            }
            else
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Constant buffer '" << local->to_string() << "' can be read from RAM via TMU"
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
                    << "' can be stored in VPM (with fall-back to RAM via VPM)" << logging::endl);
            mapping[local].preferred = MemoryAccessType::VPM_SHARED_ACCESS;
            mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
        }
        else
        {
            // local buffer
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local struct '" << local->to_string() << "' can be stored in RAM via VPM" << logging::endl);
            mapping[local].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
            mapping[local].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
        }
    }
    else if(auto conditonalWrites = checkAllWritersArePhiNodesOrSelections(local, {it.get(), memInstr}))
    {
        // If the local is a PHI result from different memory addresses, we can still map it, if the
        // source addresses have the same access type. Since the "original" access types of the sources
        // might not yet be determined and the source of a phi-node might be another phi-node, we first
        // collect all the information and in the end find the greatest common memory access type for
        // all memory accesses using the same phi-node. The same is true for memory addresses written via
        // select-statements (?:-operators).

        auto& entry = conditionalWrittenMemoryAccesses
                          .emplace(it, std::pair<const Local*, FastSet<const ConditionalMemoryAccess*>>{})
                          .first->second;
        entry.first = local;
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Conditionally accessed local '" << local->to_string()
                << "' will not be mapped, these sources can be mapped instead: " << logging::endl);

        mapConditionalOrPhiWrites(*conditonalWrites, entry.second, conditionalAddressWrites);

        // delete the original (wrong) mapping, since it is not a proper memory area
        mapping.erase(local);
    }
    else
    {
        // We come here for all uncommon types of memory access, e.g. accessing pointers loaded from memory (e.g. stack)
        // or accessing pointers which have been converted from/to integer values (and therefore have no ReferenceData
        // chain to the original memory area).

        // To still be able to map them, try to get to some memory area by following the local's value source and try
        // again to find underlying memory area
        auto sourceLocal = local;
        while(auto writer = analysis::getSingleWriter(sourceLocal->createReference()))
        {
            if(auto source = writer->getMoveSource())
            {
                if(auto loc = source->checkLocal())
                    sourceLocal = loc;
                else
                    break;
            }
            else if(auto op = dynamic_cast<const Operation*>(writer))
            {
                // check for base-address + offset addition using integers
                if(op->op == OP_ADD)
                {
                    auto firstArg = getSourceValue(op->getFirstArg());
                    auto secondArg = getSourceValue(op->assertArgument(1));

                    // XXX The "one argument is pointer the other not" check is not completely true, but the most
                    // fitting one. In theory, they both could be pointers or the actual base pointer could be reachable
                    // by the non-pointer argument.
                    if(firstArg.checkLocal() &&
                        (secondArg.getConstantValue() ||
                            (firstArg.type.getPointerType() && !secondArg.type.getPointerType())))
                        sourceLocal = firstArg.local();
                    else if(secondArg.checkLocal() &&
                        (firstArg.getConstantValue() ||
                            (!firstArg.type.getPointerType() && secondArg.type.getPointerType())))
                        sourceLocal = secondArg.local();
                    else
                        break;
                }
                else
                    break;
            }
            else
                break;
        }

        auto writer = dynamic_cast<const intermediate::MemoryInstruction*>(
            analysis::getSingleWriter(sourceLocal->createReference()));
        if(sourceLocal && writer && writer->op == intermediate::MemoryOperation::READ)
        {
            // Some addresses are loaded from stack-allocated pointer-to-pointer values (i.e. this is the default if all
            // optimizations are disabled in the clang front-end). To handle this, we check if this is the case and if
            // so, try to determine the (hopefully single) value actually written to that stack allocation.
            // NOTE: There is no code (and no need for any code) that checks whether the memory is actually a stack
            // allocation!
            auto stackAllocation = writer->getSource().local();
            FastSet<const intermediate::IntermediateInstruction*> stackAllocationWriters;
            stackAllocation->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* user) {
                if(auto stackWriter = dynamic_cast<const intermediate::MemoryInstruction*>(getSourceInstruction(user)))
                    stackAllocationWriters.emplace(stackWriter);
            });
            if(stackAllocationWriters.size() == 1)
            {
                // this instruction writes the original pointer value to the stack allocated pointer-to-pointer, which
                // is then read again to retrieve the pointer value we are currently trying to determine its origin for.
                auto singleWriter = (*stackAllocationWriters.begin());
                if(auto loc =
                        dynamic_cast<const intermediate::MemoryInstruction*>(singleWriter)->getSource().checkLocal())
                    sourceLocal = loc;
                else
                    // TODO we cannot actually access any memory not provided by a parameter/stack allocation/global
                    // data, so this might very fast become UB. But we can still handle the "invalid" address itself, so
                    // at this point there is no error yet!
                    logging::warn() << "Found single stack-allocation writer, but source is not a local: "
                                    << singleWriter->to_string() << logging::endl;
            }
        }

        if(sourceLocal)
            sourceLocal = sourceLocal->getBase(true);
        if(sourceLocal && local != sourceLocal)
        {
            // try again to map the local to an (existing) memory area
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine underlying memory area for '" << local->to_string()
                    << "', trying again for its single source: " << sourceLocal->to_string() << logging::endl);
            auto memoryLocal = determineSingleMemoryAreaMapping(
                mapping, it, sourceLocal, memInstr, conditionalWrittenMemoryAccesses, conditionalAddressWrites);
            // if we returned here, then we managed to (conditionally) map the local to a memory area. So edit the
            // current local to reference the inner most local
            if(local->type.getPointerType() && !local->get<LocalData>() && memoryLocal &&
                memoryLocal->type.getPointerType())
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Single source '" << sourceLocal->to_string()
                        << "' is mapped to memory, applying base to local '" << local->to_string()
                        << "': " << memoryLocal->to_string(true) << logging::endl);
                // add the access instruction to the actually accessed memory instruction
                auto innerMappingIt = mapping.find(memoryLocal);
                if(innerMappingIt != mapping.end())
                    innerMappingIt->second.accessInstructions.emplace(it, local);
                const_cast<Local*>(local)->set(ReferenceData(*memoryLocal, ANY_ELEMENT));
            }
            return memoryLocal;
        }

        // FIXME this is called e.g. for pointers dereferenced which are loaded from memory (from
        // pointers-to-pointers), e.g. for ./testing/bullet/jointSolver.cl (or for basically all pointers if clang is
        // run without optimizations)
        // FIXME this is also called for pointers converted from/to integers, e.g. for
        // TestMemoryAccess#CASTPOINTER
        CPPLOG_LAZY_BLOCK(logging::Level::ERROR, {
            logging::error() << "Failed to find memory area for local: " << local->to_string(true) << logging::endl;
            local->forUsers(LocalUse::Type::WRITER,
                [](const auto* writer) { logging::error() << "\tWriter: " << writer->to_string() << logging::endl; });
            local->forUsers(LocalUse::Type::READER,
                [](const auto* reader) { logging::error() << "\tReader: " << reader->to_string() << logging::endl; });
        });

        throw CompilationError(
            CompilationStep::NORMALIZER, "Invalid local type for memory area", local->to_string(true));
    }
    return local;
}

MemoryAccessInfo normalization::determineMemoryAccess(Method& method)
{
    // TODO lower local/private struct-elements into VPM?! At least for single structs
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Determining memory access for kernel: " << method.name << logging::endl);
    MemoryAccessMap mapping;
    FastSet<InstructionWalker> allWalkers;
    FastMap<InstructionWalker, std::pair<const Local*, FastSet<const ConditionalMemoryAccess*>>>
        conditionalWrittenMemoryAccesses;
    FastSet<ConditionalMemoryAccess> conditionalAddressWrites;
    for(const auto& param : method.parameters)
    {
        if(!param.type.getPointerType())
            continue;
        auto pointerType = param.type.getPointerType();
        if(pointerType->addressSpace == AddressSpace::CONSTANT)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Constant parameter '" << param.to_string() << "' can be read from RAM via TMU"
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
                        << "' without any write access can be read from RAM via TMU" << logging::endl);
                mapping[&param].preferred = MemoryAccessType::RAM_LOAD_TMU;
                // fall-back, e.g. for memory copy
                mapping[&param].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
            }
            else
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Global parameter '" << param.to_string()
                        << "' which is written to can be stored in RAM and accessed via VPM" << logging::endl);
                mapping[&param].preferred = MemoryAccessType::RAM_READ_WRITE_VPM;
                mapping[&param].fallback = MemoryAccessType::RAM_READ_WRITE_VPM;
            }
        }
        else if(pointerType->addressSpace == AddressSpace::LOCAL)
        {
            // TODO if last access index is known and fits into VPM, set for VPM-or-RAM
            // TODO need to make sure the correct size is used (by default, lowering to VPM uses the pointed-to-type
            // which could be wrong!)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local parameter '" << param.to_string() << "' can be stored in RAM and accessed via VPM"
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
                InstructionWalker startIt;
                std::tie(it, startIt) = insert64BitWrite(method, it, memInstr->getDestination().local(),
                    rewriteParts->second.first->createReference(), rewriteParts->second.second->createReference(),
                    memInstr);
                rewrite64BitStoresTo32.erase(rewriteParts);
                it.erase();

                // continue with exactly the first store instruction
                memInstr = startIt.get<MemoryInstruction>();
                it = startIt;
            }
            else if(memInstr->op == MemoryOperation::WRITE && !memInstr->hasConditionalExecution() &&
                memInstr->getSource().type.getElementType() == TYPE_INT64)
            {
                // we can lower some more 64-bit writes, if the values can either be written statically (e.g. a
                // constant) or it is guaranteed that the upper word is zero (e.g. converted from 32-bit word).
                auto source = intermediate::getSourceValue(memInstr->getSource());
                // NOTE: we only check for exactly 32-bit, since otherwise VPM would only write the actually used bits,
                // not all 32
                auto lowerSource = source.type.getScalarBitCount() == 32 ? source : source.getConstantValue();
                if(lowerSource)
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Converting write of 64-bit '" << memInstr->getSource().to_string()
                            << "' where the upper part is guaranteed to be zero to a write of the lower part and zero"
                            << logging::endl);

                    // fix type of constant
                    if(lowerSource->type == TYPE_INT64)
                        lowerSource->type = TYPE_INT32;

                    // rewrite the 64-bit store to 2 32-bit stores
                    InstructionWalker startIt;
                    std::tie(it, startIt) = insert64BitWrite(method, it, memInstr->getDestination().local(),
                        *lowerSource, Value(Literal(0u), TYPE_INT32), memInstr);
                    it.erase();

                    // continue with exactly the first store instruction
                    memInstr = startIt.get<MemoryInstruction>();
                    it = startIt;
                }
            }
            else if(memInstr->op == MemoryOperation::READ && !memInstr->hasConditionalExecution() &&
                memInstr->getDestination().local()->countUsers(LocalUse::Type::READER) == 1)
            {
                // convert read-then-write to copy
                auto nextIt = findNextValueStore(
                    it, memInstr->getDestination(), 16 /* TODO */, memInstr->getSource().local()->getBase(true));
                auto nextMemInstr = nextIt.isEndOfBlock() ? nullptr : nextIt.get<const MemoryInstruction>();
                if(nextMemInstr != nullptr && !nextIt->hasConditionalExecution() &&
                    nextMemInstr->op == MemoryOperation::WRITE &&
                    nextMemInstr->getSource().getSingleWriter() == memInstr &&
                    nextMemInstr->getSourceElementType().getInMemoryWidth() ==
                        memInstr->getDestinationElementType().getInMemoryWidth())
                {
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
                    auto dest = nextMemInstr->getDestination();

                    if(nextMemInstr->getSourceElementType().getInMemoryWidth() !=
                            nextMemInstr->getDestinationElementType().getInMemoryWidth() ||
                        memInstr->getSourceElementType().getInMemoryWidth() !=
                            memInstr->getDestinationElementType().getInMemoryWidth())
                    {
                        // we read/write a different data type than the pointer type, this can e.g. happen when
                        // combining vloadN/vstoreN into single copy, where the actual types are vectors, but the
                        // pointers are to scalars.
                        // -> need to find the actual type by using the element types of the in-register type
                        auto valueType = method.createPointerType(nextMemInstr->getSourceElementType());
                        {
                            auto tmp = method.addNewLocal(valueType);
                            if(auto data = Local::getLocalData<ReferenceData>(src.checkLocal()))
                                tmp.local()->set(ReferenceData(*data));
                            assign(nextIt, tmp) = src;
                            src = tmp;
                        }
                        {
                            auto tmp = method.addNewLocal(valueType);
                            if(auto data = Local::getLocalData<ReferenceData>(dest.checkLocal()))
                                tmp.local()->set(ReferenceData(*data));
                            assign(nextIt, tmp) = dest;
                            dest = tmp;
                        }
                    }
                    it.erase();
                    nextIt.reset(new MemoryInstruction(MemoryOperation::COPY, Value(std::move(dest)), std::move(src),
                        Value(nextMemInstr->getNumEntries()), nextMemInstr->guardAccess));
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
                FastSet<const IntermediateInstruction*> movesToRewrite;
                FastSet<const Operation*> shiftsToRewrite;
                FastSet<const MemoryInstruction*> storesToRewrite;

                // Check whether all uses are used in a supported way (e.g. actually only access of the lower 32 bit or
                // copy the whole value)
                for(auto user : memInstr->getDestination().local()->getUsers(LocalUse::Type::READER))
                {
                    if(user->getMoveSource())
                    {
                        // check for truncation from 64 to 32 bits
                        if(user->getOutput()->type.getScalarBitCount() != 32)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Unsupported move, aborting rewrite: " << user->to_string() << logging::endl);
                            canBeSplitUp = false;
                            break;
                        }
                        // moves which truncate the 64 bit integer to 32 bit are accepted
                        movesToRewrite.emplace(user);
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
                    auto origLocal = memInstr->getDestination();
                    Value lowerLocal = UNDEFINED_VALUE;
                    Value upperLocal = UNDEFINED_VALUE;
                    if(auto data = origLocal.local()->get<MultiRegisterData>())
                    {
                        lowerLocal = data->lower->createReference();
                        upperLocal = data->upper->createReference();
                    }
                    else
                    {
                        // TODO this should never be met, should it?!
                        // TODO if this is never called, can also replace the rewrite64BitStoresTo32 with just checking
                        // for LongLocals?
                        lowerLocal = method.addNewLocal(TYPE_INT32, origLocal.local()->name + ".lower");
                        upperLocal = method.addNewLocal(TYPE_INT32, origLocal.local()->name + ".upper");
                    }

                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Splitting '" << origLocal.to_string() << "' into '" << lowerLocal.to_string()
                            << "' and '" << upperLocal.to_string() << '\'' << logging::endl);

                    if(memInstr->guardAccess)
                    {
                        it.emplace(new MutexLock(MutexAccess::LOCK));
                        it.nextInBlock();
                    }
                    auto lowerIndex = Value(memInstr->getSource().local(), method.createPointerType(TYPE_INT32));
                    it.emplace(new MemoryInstruction(
                        MemoryOperation::READ, Value(lowerLocal), Value(lowerIndex), Value(INT_ONE), false));
                    it->copyExtrasFrom(memInstr);
                    auto startIt = it;
                    it.nextInBlock();
                    auto upperIndex = assign(it, lowerIndex.type) = lowerIndex + 4_val;
                    if(auto data = lowerIndex.local()->get<ReferenceData>())
                        upperIndex.local()->set(ReferenceData(*data));
                    it.emplace(new MemoryInstruction(
                        MemoryOperation::READ, Value(upperLocal), std::move(upperIndex), Value(INT_ONE), false));
                    it->copyExtrasFrom(memInstr);
                    it.nextInBlock();
                    if(memInstr->guardAccess)
                    {
                        it.emplace(new MutexLock(MutexAccess::RELEASE));
                        it.nextInBlock();
                    }

                    for(auto store : storesToRewrite)
                    {
                        // mark all stores for rewrite
                        rewrite64BitStoresTo32.emplace(store,
                            std::pair<const Local*, const Local*>{lowerLocal.checkLocal(), upperLocal.checkLocal()});
                    }

                    for(auto move : movesToRewrite)
                        // replace all truncations with usage of lower word
                        const_cast<IntermediateInstruction*>(move)->replaceValue(
                            origLocal, lowerLocal, LocalUse::Type::READER);

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
                determineSingleMemoryAreaMapping(
                    mapping, it, local, memInstr, conditionalWrittenMemoryAccesses, conditionalAddressWrites);
            if(it.has())
                allWalkers.emplace(it);
        }
        it.nextInMethod();
    }

    // 1. map phi-nodes to source access types
    FastMap<const ConditionalMemoryAccess*, MemoryAccess*> conditionalAccesses;
    FastMap<const Local*, FastSet<const Local*>> conditionalLocalMappings;
    for(const auto& conditionalWrite : conditionalAddressWrites)
    {
        if(auto source = VariantNamespace::get_if<Value>(&conditionalWrite.source))
        {
            // check whether conditional source is a conditionally read memory location
            if(source->checkLocal() && (source->local()->residesInMemory() || source->local()->is<Parameter>()))
            {
                logging::debug() << "Using source of conditional address write '" << source->to_string()
                                 << "' as source of main address write: "
                                 << conditionalWrite.conditionalWrite->to_string() << logging::endl;
                addConditionalLocalMapping(
                    source->local(), conditionalWrite, conditionalAccesses, conditionalLocalMappings, mapping);
                continue;
            }
            // the (possibly recursive) conditional memory address writes referenced by this conditional memory access
            // write
            FastSet<ConditionalMemoryAccess> recursiveAccesses;
            auto writers = source->local()->getUsers(LocalUse::Type::WRITER);
            for(const auto& innerConditionalWrite : conditionalAddressWrites)
            {
                if(writers.find(innerConditionalWrite.conditionalWrite) != writers.end())
                    recursiveAccesses.emplace(innerConditionalWrite);
            }
            if(recursiveAccesses.size() != writers.size())
            {
                for(const auto writer : writers)
                    logging::error() << "Address writer: " << writer->to_string() << logging::endl;
                for(const auto& access : recursiveAccesses)
                    logging::error() << "Conditional memory access: " << access.to_string() << logging::endl;
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Failed to find recursive conditional address write for all conditional address write sources!");
            }
            // If all the conditional address writes used as input by this conditional address write are either
            // a) recursively referencing back the output of this write or
            // b) referencing an actual memory location,
            // we can replace the sources of this conditional write with the actual memory locations accessed by our
            // source instructions.
            for(const auto& recursiveAccess : recursiveAccesses)
            {
                const Local* sourceLocal = nullptr;
                if(auto innerSource = VariantNamespace::get_if<Value>(&recursiveAccess.source))
                {
                    if(innerSource->checkLocal() &&
                        (innerSource->local()->residesInMemory() || innerSource->local()->is<Parameter>()))
                        sourceLocal = innerSource->local();
                    else if(!innerSource->hasLocal(conditionalWrite.conditionalWrite->checkOutputLocal()))
                    {
                        logging::error() << "Recursive address write diverges: " << innerSource->to_string()
                                         << logging::endl;
                        throw CompilationError(CompilationStep::NORMALIZER,
                            "Memory accessed via divergent recursive conditional address writes is not supported "
                            "yet",
                            conditionalWrite.conditionalWrite->to_string());
                    }
                    else
                        // this is the recursive branch
                        continue;
                }
                else
                    sourceLocal = VariantNamespace::get<const Local*>(recursiveAccess.source);

                // if the recursive conditional address write source is a local, we handle it as input of our
                // outer conditional address write
                logging::debug() << "Using source of recursive address write '" << sourceLocal->to_string()
                                 << "' as source of main address write: "
                                 << conditionalWrite.conditionalWrite->to_string() << logging::endl;
                addConditionalLocalMapping(
                    sourceLocal, conditionalWrite, conditionalAccesses, conditionalLocalMappings, mapping);
            }
        }
        else
        {
            auto loc = VariantNamespace::get<const Local*>(conditionalWrite.source);
            addConditionalLocalMapping(loc, conditionalWrite, conditionalAccesses, conditionalLocalMappings, mapping);
        }
    }

    // 2. map memory access instructions to phi-nodes, determine best common access type
    for(const auto& conditionalAccessedMemory : conditionalWrittenMemoryAccesses)
    {
        FastSet<MemoryAccessType> preferredTypes;
        FastSet<MemoryAccessType> fallbackTypes;
        for(const auto* access : conditionalAccessedMemory.second.second)
        {
            auto& memoryAccess = *conditionalAccesses.at(access);
            memoryAccess.accessInstructions.emplace(
                conditionalAccessedMemory.first, conditionalAccessedMemory.second.first);
            preferredTypes.emplace(memoryAccess.preferred);
            fallbackTypes.emplace(memoryAccess.fallback);
            // Disable caching any conditionally accessed memory area in VPM (if the final access type turns out to be
            // RAM_READ_WRITE_VPM. Otherwise, we would need to lower and cache all memory areas accessed in the same
            // conditional access and waterfall that...
            memoryAccess.canBeCachedInVPM = false;
        }

        while(preferredTypes.size() > 1)
        {
            // try to fall back to the fall-back memory access types, until we have one type that fits all.
            // since all memory access types in the end fall back to memory access via DMA, we can repeat this fix-up
            // until there is only a single memory access type left
            for(const auto* access : conditionalAccessedMemory.second.second)
            {
                auto& memoryAccess = *conditionalAccesses.at(access);
                // Some other memory location prefers our fallback type, so use it also. Don't fall back (yet) if other
                // memory locations can fall back to our preferred type, since this might be the memory access type
                // everyone can agree on.
                if(fallbackTypes.find(memoryAccess.preferred) == fallbackTypes.end() &&
                    preferredTypes.find(memoryAccess.fallback) != preferredTypes.end())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Falling back to memory access type ' " << toString(memoryAccess.fallback)
                            << "' to match other conditional memory accesses for location: " << access->to_string()
                            << logging::endl);
                    memoryAccess.preferred = memoryAccess.fallback;
                    memoryAccess.fallback = getFallbackType(memoryAccess.preferred);
                }
            }

            // check again
            preferredTypes.clear();
            fallbackTypes.clear();
            for(const auto* access : conditionalAccessedMemory.second.second)
            {
                auto& memoryAccess = *conditionalAccesses.at(access);
                preferredTypes.emplace(memoryAccess.preferred);
                fallbackTypes.emplace(memoryAccess.fallback);
            }
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Conditionally written memory address '" << conditionalAccessedMemory.second.first->to_string()
                << "' will access all source memory locations as " << normalization::toString(*preferredTypes.begin())
                << logging::endl);
    }

    return MemoryAccessInfo{std::move(mapping), std::move(allWalkers), std::move(conditionalLocalMappings)};
}

static MemoryInfo canLowerToRegisterReadOnly(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // a) the global is a constant scalar/vector which fits into a single register
    if(auto constant = getConstantContainerValue(baseAddr->createReference()))
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
    // c) the global is a constant where all accesses are direct accesses (i.e. not via a conditional address write) and
    // have constant indices and therefore all accessed elements can be determined at compile time
    if(std::all_of(access.accessInstructions.begin(), access.accessInstructions.end(), [&](const auto& entry) -> bool {
           return baseAddr == entry.second &&
               getConstantElementValue(entry.first.template get<const intermediate::MemoryInstruction>()->getSource())
                   .has_value();
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
    {
        if(auto stackAllocation = baseAddr->as<StackAllocation>())
            stackAllocation->isLowered = true;
        return MemoryInfo{baseAddr, MemoryAccessType::QPU_REGISTER_READWRITE, nullptr, {},
            method.addNewLocal(baseAddr->type, "%lowered_stack")};
    }
    // b) the private memory is small enough to be rewritten to fit into a single register (e.g. int[4])
    if(auto convertedType = convertSmallArrayToRegister(baseAddr))
    {
        if(auto stackAllocation = baseAddr->as<StackAllocation>())
            stackAllocation->isLowered = true;
        return MemoryInfo{baseAddr, MemoryAccessType::QPU_REGISTER_READWRITE, nullptr, {},
            method.addNewLocal(*convertedType, "%lowered_stack"), convertedType};
    }

    // cannot lower to register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo canLowerToPrivateVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    // FIXME enable once storing with element (non-vector) offset into VPM is implemented
    // Retest: OpenCL-CTS/vload_private, OpenCL_CTS/vstore_private, emulate-memory, test_async_copy
    auto area = static_cast<periphery::VPMArea*>(nullptr);
    // TODO method.vpm->addArea(baseAddr, baseAddr->type.getElementType(), true, method.metaData.getWorkGroupSize());
    if(area)
    {
        // mark stack allocation as lowered to VPM to skip reserving a stack area
        if(auto stackAllocation = baseAddr->as<StackAllocation>())
            stackAllocation->isLowered = true;
        return MemoryInfo{
            baseAddr, MemoryAccessType::VPM_PER_QPU, area, {}, NO_VALUE, convertSmallArrayToRegister(baseAddr)};
    }

    // cannot lower to register, use fall-back
    access.preferred = access.fallback;
    return checkMemoryMapping(method, baseAddr, access);
}

static MemoryInfo toSharedVPMArea(const Local* baseAddr, const periphery::VPMArea* area,
    Optional<std::vector<MemoryAccessRange>>&& ranges, Optional<DataType>&& areaType)
{
    if(auto parameter = baseAddr->as<Parameter>())
        // mark parameter as lowered
        parameter->isLowered = true;
    return MemoryInfo{
        baseAddr, MemoryAccessType::VPM_SHARED_ACCESS, area, std::move(ranges), NO_VALUE, std::move(areaType)};
}

static MemoryInfo canLowerToSharedVPMArea(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    auto elementType = baseAddr->type.getElementType();
    bool hasStructContent =
        elementType.getStructType() || (elementType.getArrayType() && elementType.getElementType().getStructType());
    if(!hasStructContent)
    {
        if(auto area = method.vpm->addArea(baseAddr, baseAddr->type.getElementType()))
            return toSharedVPMArea(baseAddr, area, {}, convertSmallArrayToRegister(baseAddr));
    }

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

static const periphery::VPMArea* checkCacheMemoryAccessRanges(
    Method& method, const Local* baseAddr, FastAccessList<MemoryAccessRange>& memoryAccessRanges, bool isCached);

static MemoryInfo canMapToDMAReadWrite(Method& method, const Local* baseAddr, MemoryAccess& access)
{
    PROFILE_START(DetermineAccessRanges);
    auto ranges = analysis::determineAccessRanges(method, baseAddr, access.accessInstructions);
    PROFILE_END(DetermineAccessRanges);

    const periphery::VPMArea* area = nullptr;
    bool isLocalMemory = baseAddr->type.getPointerType()->addressSpace == AddressSpace::LOCAL;
    // we can't lower structs and we cannot (yet) write multiple rows of 64-bit data
    auto memoryDataType = baseAddr->type.getElementType();
    bool isCacheableType = memoryDataType.isSimpleType() && memoryDataType.getScalarBitCount() <= 32;
    bool hasOnlyCacheableAccesses = isLocalMemory ||
        std::none_of(access.accessInstructions.begin(), access.accessInstructions.end(),
            [](const std::pair<InstructionWalker, const Local*>& pair) -> bool {
                // XXX for now, don't cache any copy between RAM and RAM, since we will have to rewrite the layout
                // inside the VPM (or read/write initially with the correct/not-packed layout)
                // XXX also for now, don't cache any 64-bit access, since we cannot (yet) write multiple rows of 64-bit
                // data
                auto memoryInst = pair.first.get<intermediate::MemoryInstruction>();
                return memoryInst &&
                    (memoryInst->op == intermediate::MemoryOperation::COPY ||
                        memoryInst->getDestinationElementType().getScalarBitCount() > 32 ||
                        memoryInst->getSourceElementType().getScalarBitCount() > 32);
            });
    if(!ranges.empty() && baseAddr->type.getPointerType() && isCacheableType && hasOnlyCacheableAccesses &&
        (isLocalMemory || access.canBeCachedInVPM))
        area = checkCacheMemoryAccessRanges(method, baseAddr, ranges, !isLocalMemory);
    if(area && isLocalMemory)
        // for local/private memory, there is no need for initial load/write-back
        return toSharedVPMArea(baseAddr, area, std::move(ranges), {});
    return MemoryInfo{baseAddr, MemoryAccessType::RAM_READ_WRITE_VPM, area, std::move(ranges)};
}

static const periphery::VPMArea* checkCacheMemoryAccessRanges(
    Method& method, const Local* baseAddr, FastAccessList<MemoryAccessRange>& memoryAccessRanges, bool isCached)
{
    auto maxNumVectors = method.vpm->getMaxCacheVectors(TYPE_INT32, true);
    GroupedAccessRanges result;

    bool allUniformPartsEqual = false;
    analysis::ValueRange offsetRange{};
    std::tie(allUniformPartsEqual, offsetRange) = analysis::checkWorkGroupUniformParts(memoryAccessRanges);
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
    if(offsetRange.getRange() >= maxNumVectors)
    {
        // this also checks for any over/underflow when converting the range to unsigned int in the next steps
        LCOV_EXCL_START
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Cannot cache memory location " << baseAddr->to_string()
                << " in VPM, the accessed range is too big: " << offsetRange.to_string() << logging::endl);
        LCOV_EXCL_STOP
        return nullptr;
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Memory location " << baseAddr->to_string() << " is accessed via DMA in the dynamic range "
            << offsetRange.to_string() << logging::endl);

    // TODO correct type?? Shouldn't it be baseAddr->type.getElmentType().toArrayType(...??
    auto accessedType = method.createArrayType(
        baseAddr->type, static_cast<unsigned>(offsetRange.getRange() + 1 /* bounds of range are inclusive! */));

    // XXX the local is not correct, at least not if there is a work-group uniform offset, but since all work-items
    // use the same work-group offset, it doesn't matter
    auto vpmArea = method.vpm->addArea(baseAddr, accessedType, false, isCached);
    if(vpmArea == nullptr)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Memory location " << baseAddr->to_string() << " with dynamic access range "
                << offsetRange.to_string() << " cannot be cached in VPM, since it does not fit" << logging::endl);
        return nullptr;
    }
    return vpmArea;
}
