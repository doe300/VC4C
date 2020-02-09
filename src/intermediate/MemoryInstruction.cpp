/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../GlobalValues.h"
#include "../asm/Instruction.h"

#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;

static bool isDerivedFromMemory(const Local* local)
{
    auto base = local->getBase(true);
    if(base->residesInMemory() || base->is<Parameter>())
        return true;
    bool allSourcesDerivedFromMemory = true;

    local->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* user) {
        if(dynamic_cast<const intermediate::MoveOperation*>(user) != nullptr && user->getArgument(0)->checkLocal())
        {
            if(!isDerivedFromMemory(user->getArgument(0)->local()))
                allSourcesDerivedFromMemory = false;
        }
        else if(dynamic_cast<const intermediate::MemoryInstruction*>(user) != nullptr)
            return;
        else if(auto op = dynamic_cast<const intermediate::Operation*>(user))
        {
            if(op->op != OP_ADD && op->op != OP_SUB)
                allSourcesDerivedFromMemory = false;
            else if(op->getFirstArg().checkLocal() && op->getFirstArg().type.getPointerType() &&
                !op->assertArgument(1).type.getPointerType())
            {
                return;
            }
            else if(op->assertArgument(1).checkLocal() && !op->getFirstArg().type.getPointerType() &&
                op->assertArgument(1).type.getPointerType())
            {
                return;
            }
            else
                allSourcesDerivedFromMemory = false;
        }
        else
        {
            // unknown / not handled operation, assume worst
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Unhandled source of pointer: " << user->to_string() << logging::endl);
            allSourcesDerivedFromMemory = false;
        }
    });

    return allSourcesDerivedFromMemory;
}

static void checkMemoryLocation(const Value& val)
{
    if(!val.type.getPointerType())
        throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to be a pointer", val.to_string());
    /*
     * TODO some memory locations are not recognized:
     * Pointers with a dynamically set memory location cannot be recognized at compile-time and therefore this check
     * will always fail for them
     */
    if(!val.checkLocal() || !isDerivedFromMemory(val.local()))
        throw CompilationError(CompilationStep::LLVM_2_IR,
            "Operand needs to refer to a memory location or a parameter containing one", val.to_string());
}

static void checkLocalValue(const Value& val)
{
    if(val.checkLocal() &&
        (val.local()->residesInMemory() ||
            (val.local()->is<Parameter>() && (val.local()->type.getPointerType() || val.local()->type.getArrayType()))))
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

    if(numEntries != INT_ONE)
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
LCOV_EXCL_STOP

qpu_asm::DecoratedInstruction MemoryInstruction::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const
{
    throw CompilationError(CompilationStep::OPTIMIZER, "There should be no more memory operations", to_string());
}

bool MemoryInstruction::isNormalized() const
{
    return false;
}

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
