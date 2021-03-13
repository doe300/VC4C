/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../GlobalValues.h"
#include "../asm/Instruction.h"
#include "../periphery/CacheEntry.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"

#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;

static void checkMemoryLocation(const Value& val)
{
    if(!val.checkLocal() || !val.type.getPointerType())
        throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to be a pointer local", val.to_string());
}

static void checkLocalValue(const Value& val)
{
    if(val.checkLocal() && (val.type.getPointerType() || val.type.getArrayType()))
        // NOTE: This check explicitly allows for values referencing a memory location while being a non-pointer type
        // (e.g. for bit-cast pointers to integer)!
        throw CompilationError(
            CompilationStep::LLVM_2_IR, "Operand needs to be a local value (local, register)", val.to_string());
}

static void checkSingleValue(const Value& val)
{
    if(val.getLiteralValue() == Literal(1u))
        return;
    throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to the constant one", val.to_string());
}

MemoryInstruction::MemoryInstruction(
    const MemoryOperation op, Value&& dest, Value&& src, Value&& numEntries, bool useMutex) :
    IntermediateInstruction(std::move(dest)),
    op(op), guardAccess(useMutex)
{
    setArgument(0, std::move(src));
    setArgument(1, std::move(numEntries));

    if(assertArgument(1) != INT_ONE)
    {
        if(op != MemoryOperation::COPY && op != MemoryOperation::FILL)
            throw CompilationError(
                CompilationStep::LLVM_2_IR, "Can only use the entry count for copying or filling memory", to_string());
    }
}

LCOV_EXCL_START
std::string MemoryInstruction::to_string() const
{
    auto lockString = guardAccess ? " (guarded)" : " (unguarded)";
    switch(op)
    {
    case MemoryOperation::COPY:
        return std::string("copy ") + (getNumEntries().to_string() + " entries from ") +
            (getSource().to_string() + " into ") + getDestination().to_string() + lockString;
    case MemoryOperation::FILL:
        return std::string("fill ") + (getDestination().to_string() + " with ") +
            (getNumEntries().to_string() + " copies of ") + getSource().to_string() + lockString;
    case MemoryOperation::READ:
        return (getDestination().to_string() + " = load memory at ") + getSource().to_string() + lockString;
    case MemoryOperation::WRITE:
        return std::string("store ") + (getSource().to_string() + " into ") + getDestination().to_string() + lockString;
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}

qpu_asm::DecoratedInstruction MemoryInstruction::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const
{
    throw CompilationError(CompilationStep::OPTIMIZER, "There should be no more memory operations", to_string());
}

bool MemoryInstruction::isNormalized() const
{
    return false;
}
LCOV_EXCL_STOP

SideEffectType MemoryInstruction::getSideEffects() const
{
    return add_flag(IntermediateInstruction::getSideEffects(), SideEffectType::MEMORY_ACCESS);
}

IntermediateInstruction* MemoryInstruction::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return (new MemoryInstruction(op, renameValue(method, getDestination(), localPrefix, localMapping),
                renameValue(method, getSource(), localPrefix, localMapping),
                renameValue(method, getNumEntries(), localPrefix, localMapping), guardAccess))
        ->copyExtrasFrom(this);
}

const Value& MemoryInstruction::getSource() const
{
    return assertArgument(0);
}

const Value& MemoryInstruction::getDestination() const
{
    return getOutput().value();
}

const Value& MemoryInstruction::getNumEntries() const
{
    return assertArgument(1);
}

DataType MemoryInstruction::getSourceElementType(bool sizedType) const
{
    switch(op)
    {
    case MemoryOperation::COPY:
    {
        checkMemoryLocation(getSource());
        DataType elementType = getSource().type.getElementType();
        if(!sizedType)
            // simple pointed-to type
            return elementType;
        // sized pointed-to type
        if(!getNumEntries().isLiteralValue())
            throw CompilationError(CompilationStep::GENERAL,
                "Cannot calculate type-size from dynamically sized memory-operation", to_string());
        return elementType.toArrayType(getNumEntries().getLiteralValue()->unsignedInt());
    }
    case MemoryOperation::FILL:
        // local value
        checkLocalValue(getSource());
        return getSource().type;
    case MemoryOperation::READ:
        // pointed-to type
        checkMemoryLocation(getSource());
        checkSingleValue(getNumEntries());
        return getSource().type.getElementType();
    case MemoryOperation::WRITE:
        // local value
        checkLocalValue(getSource());
        checkSingleValue(getNumEntries());
        return getSource().type;
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}

DataType MemoryInstruction::getDestinationElementType(bool sizedType) const
{
    switch(op)
    {
    case MemoryOperation::COPY:
        FALL_THROUGH
    case MemoryOperation::FILL:
    {
        checkMemoryLocation(getDestination());
        DataType elementType = getDestination().type.getElementType();
        if(!sizedType)
            // simple pointed-to type
            return elementType;
        // sized pointed-to type
        if(!getNumEntries().isLiteralValue())
            throw CompilationError(CompilationStep::GENERAL,
                "Cannot calculate type-size from dynamically sized memory-operation", to_string());
        return elementType.toArrayType(getNumEntries().getLiteralValue()->unsignedInt());
    }
    case MemoryOperation::READ:
        // local value
        checkLocalValue(getDestination());
        checkSingleValue(getNumEntries());
        return getDestination().type;
    case MemoryOperation::WRITE:
        // pointed-to type
        checkMemoryLocation(getDestination());
        checkSingleValue(getNumEntries());
        return getDestination().type.getElementType();
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}

FastSet<const Local*> MemoryInstruction::getMemoryAreas() const
{
    FastSet<const Local*> res;
    switch(op)
    {
    case MemoryOperation::COPY:
        checkMemoryLocation(getSource());
        checkMemoryLocation(getDestination());
        res.emplace(getSource().local()->getBase(true));
        res.emplace(getDestination().local()->getBase(true));
        break;
    case MemoryOperation::FILL:
        checkMemoryLocation(getDestination());
        res.emplace(getDestination().local()->getBase(true));
        break;
    case MemoryOperation::READ:
        checkMemoryLocation(getSource());
        res.emplace(getSource().local()->getBase(true));
        break;
    case MemoryOperation::WRITE:
        checkMemoryLocation(getDestination());
        res.emplace(getDestination().local()->getBase(true));
        break;
    }
    return res;
}

bool MemoryInstruction::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherMem = dynamic_cast<const MemoryInstruction*>(&other))
        return op == otherMem->op && guardAccess == otherMem->guardAccess;
    return false;
}

MemoryAccessInstruction::MemoryAccessInstruction(
    MemoryOperation op, const std::shared_ptr<periphery::CacheEntry>& cacheEntry) :
    IntermediateInstruction(),
    op(op), cache(cacheEntry)
{
    cache->accesses.emplace(this);
}

MemoryAccessInstruction::~MemoryAccessInstruction()
{
    cache->accesses.erase(this);
}

qpu_asm::DecoratedInstruction MemoryAccessInstruction::convertToAsm(
    const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping,
    std::size_t instructionIndex) const
{
    throw CompilationError(CompilationStep::OPTIMIZER, "There should be no more mmeory access operations", to_string());
}

IntermediateInstruction* MemoryAccessInstruction::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    throw CompilationError(
        CompilationStep::GENERAL, "Memory access instructions cannot be copied for new", to_string());
}

bool MemoryAccessInstruction::isNormalized() const
{
    return true;
}

SideEffectType MemoryAccessInstruction::getSideEffects() const
{
    return SideEffectType::MEMORY_ACCESS;
}

std::shared_ptr<periphery::TMUCacheEntry> MemoryAccessInstruction::getTMUCacheEntry() const
{
    return std::dynamic_pointer_cast<periphery::TMUCacheEntry>(cache);
}

std::shared_ptr<periphery::VPMCacheEntry> MemoryAccessInstruction::getVPMCacheEntry() const
{
    return std::dynamic_pointer_cast<periphery::VPMCacheEntry>(cache);
}

bool MemoryAccessInstruction::isLoadAccess() const noexcept
{
    return op == MemoryOperation::READ;
}

bool MemoryAccessInstruction::isStoreAccess() const noexcept
{
    return op != MemoryOperation::READ;
}

bool MemoryAccessInstruction::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherMem = dynamic_cast<const RAMAccessInstruction*>(&other))
        return op == otherMem->op && &cache == &otherMem->cache;
    return false;
}

RAMAccessInstruction::RAMAccessInstruction(MemoryOperation op, const Value& memoryAddress,
    const std::shared_ptr<periphery::CacheEntry>& cacheEntry, const Value& numEntries) :
    MemoryAccessInstruction(op, cacheEntry)
{
    // we always read the address, even if we write its content
    setArgument(0, memoryAddress);
    setArgument(1, numEntries);

    // to make the old code work with the new instruction types
    if(auto tmuCacheEntry = std::dynamic_pointer_cast<periphery::TMUCacheEntry>(cacheEntry))
        setOutput(tmuCacheEntry->tmu.getAddress(memoryAddress.type));
    else if(auto vpmCacheEntry = std::dynamic_pointer_cast<periphery::VPMCacheEntry>(cacheEntry))
        setOutput(
            Value(op == MemoryOperation::READ ? REG_VPM_DMA_LOAD_ADDR : REG_VPM_DMA_STORE_ADDR, memoryAddress.type));

    checkMemoryLocation(memoryAddress);
    checkLocalValue(numEntries);
    if(op == MemoryOperation::COPY || op == MemoryOperation::FILL)
        throw CompilationError(CompilationStep::GENERAL, "Invalid memory operation for accessing RAM");
    if(cache->isReadOnly() && op == MemoryOperation::WRITE)
        throw CompilationError(CompilationStep::GENERAL, "Cannot write to read-only cache");
}

LCOV_EXCL_START
std::string RAMAccessInstruction::to_string() const
{
    if(op == MemoryOperation::READ)
        return "load " + getNumEntries().to_string() + " entries from memory at " + getMemoryAddress().to_string() +
            " into " + cache->to_string() + createAdditionalInfoString();
    if(op == MemoryOperation::WRITE)
        return "store " + getNumEntries().to_string() + " entries from " + cache->to_string() + " into memory at " +
            getMemoryAddress().to_string() + createAdditionalInfoString();
    throw CompilationError(
        CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}
LCOV_EXCL_STOP

const Value& RAMAccessInstruction::getMemoryAddress() const
{
    return assertArgument(0);
}

const Value& RAMAccessInstruction::getNumEntries() const
{
    return assertArgument(1);
}

void RAMAccessInstruction::setMemoryAddress(const Value& address)
{
    setArgument(0, address);
}

void RAMAccessInstruction::setNumEntries(const Value& numEntries)
{
    setArgument(1, numEntries);
}

bool RAMAccessInstruction::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherMem = dynamic_cast<const RAMAccessInstruction*>(&other))
        return MemoryAccessInstruction::innerEquals(other);
    return false;
}

CacheAccessInstruction::CacheAccessInstruction(
    MemoryOperation op, const Value& data, const std::shared_ptr<periphery::CacheEntry>& cacheEntry) :
    MemoryAccessInstruction(op, cacheEntry)
{
    if(op == MemoryOperation::WRITE)
        setArgument(0, data);
    else if(op == MemoryOperation::READ)
        setOutput(data);
    else
        throw CompilationError(CompilationStep::GENERAL, "Invalid memory operation for accessing cache");

    // to make the old code work with the new instruction types
    if(auto tmuCacheEntry = std::dynamic_pointer_cast<periphery::TMUCacheEntry>(cacheEntry))
    {
        if(op == MemoryOperation::WRITE)
            throw CompilationError(CompilationStep::GENERAL, "Cannot write to TMU cache");
        signal = tmuCacheEntry->tmu.signal;
        setArgument(0, Value(REG_TMU_OUT, data.type));
    }
    else if(auto vpmCacheEntry = std::dynamic_pointer_cast<periphery::VPMCacheEntry>(cacheEntry))
    {
        if(op == MemoryOperation::WRITE)
            setOutput(Value(REG_VPM_IO, data.type));
        else if(op == MemoryOperation::READ)
            setArgument(0, Value(REG_VPM_IO, data.type));
    }

    checkLocalValue(data);
}

LCOV_EXCL_START
std::string CacheAccessInstruction::to_string() const
{
    if(op == MemoryOperation::READ)
        return getDestination().to_string() + " = load" + (upperWord ? " upper word" : "") + " from " +
            cache->to_string() + createAdditionalInfoString();
    if(op == MemoryOperation::WRITE)
        return "store " + getSource().to_string() + " into " + (upperWord ? "upper word of " : "") +
            cache->to_string() + createAdditionalInfoString();
    throw CompilationError(
        CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}
LCOV_EXCL_STOP

const Value& CacheAccessInstruction::getData() const
{
    if(op == MemoryOperation::READ)
        return getOutput().value();
    return assertArgument(0);
}

Optional<Value> CacheAccessInstruction::getSource() const
{
    return getArgument(0);
}

Optional<Value> CacheAccessInstruction::getDestination() const
{
    return getOutput();
}

bool CacheAccessInstruction::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherMem = dynamic_cast<const CacheAccessInstruction*>(&other))
        return MemoryAccessInstruction::innerEquals(other);
    return false;
}
