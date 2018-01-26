/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

using namespace vc4c;
using namespace vc4c::intermediate;

static void checkMemoryLocation(const Value& val)
{
	if(!val.type.getPointerType())
		throw CompilationError(CompilationStep::LLVM_2_IR, "Parameter needs to be a pointer", val.to_string());
	if(!val.hasType(ValueType::LOCAL) || !(val.local->getBase(true)->residesInMemory() || val.local->getBase(true)->is<Parameter>()))
		throw CompilationError(CompilationStep::LLVM_2_IR, "Parameter needs to refer to a memory location or a parameter containing one", val.to_string());
}

static void checkLocalValue(const Value& val)
{
	if(val.hasType(ValueType::LOCAL) && (val.local->residesInMemory() || val.local->is<Parameter>()))
		throw CompilationError(CompilationStep::LLVM_2_IR, "Parameter needs to be a local value (local, register)", val.to_string());
}

static void checkSingleValue(const Value& val)
{
	if(val.getLiteralValue().is(Literal(static_cast<int64_t>(1))))
		return;
	throw CompilationError(CompilationStep::LLVM_2_IR, "Parameter needs to the constant one", val.to_string());
}

MemoryInstruction::MemoryInstruction(const MemoryOperation op, const Value dest, const Value src, const Value numEntries) : IntermediateInstruction(dest),
		op(op)
{
	setArgument(0, src);
	setArgument(1, numEntries);
	switch(op)
	{
		case MemoryOperation::COPY:
			checkMemoryLocation(src);
			checkMemoryLocation(dest);
			checkLocalValue(numEntries);
			break;
		case MemoryOperation::FILL:
			checkLocalValue(src);
			checkMemoryLocation(dest);
			checkLocalValue(numEntries);
			break;
		case MemoryOperation::READ:
			checkMemoryLocation(src);
			checkLocalValue(dest);
			checkSingleValue(numEntries);
			break;
		case MemoryOperation::WRITE:
			checkLocalValue(src);
			checkMemoryLocation(dest);
			checkSingleValue(numEntries);
	}
}

std::string MemoryInstruction::to_string() const
{
	switch(op)
	{
		case MemoryOperation::COPY:
			return std::string("copy ") + (getNumEntries().to_string() + " from ") + (getSource().to_string() + " into ") + getDestination().to_string();
		case MemoryOperation::FILL:
			return std::string("fill ") + (getDestination().to_string() + " with ") + (getNumEntries().to_string() + " copies of ") + getSource().to_string();
		case MemoryOperation::READ:
			return std::string("read ") + (getSource().to_string() + " into ") + getDestination().to_string();
		case MemoryOperation::WRITE:
			return std::string("write ") + (getSource().to_string() + " into ") + getDestination().to_string();
	}
	throw CompilationError(CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}

qpu_asm::Instruction* MemoryInstruction::convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const
{
	throw CompilationError(CompilationStep::OPTIMIZER, "There should be no more memory operations", to_string());
}

IntermediateInstruction* MemoryInstruction::copyFor(Method& method, const std::string& localPrefix) const
{
	return (new MemoryInstruction(op, renameValue(method, getDestination(), localPrefix), renameValue(method, getSource(), localPrefix), renameValue(method, getNumEntries(), localPrefix)))->copyExtrasFrom(this);
}

const Value& MemoryInstruction::getSource() const
{
	return getArguments().at(0);
}

const Value& MemoryInstruction::getDestination() const
{
	return getOutput().value();
}

const Value& MemoryInstruction::getNumEntries() const
{
	return getArguments().at(1);
}

static bool canMoveIntoVPM(const Value& val, bool isMemoryAddress)
{
	if(isMemoryAddress)
	{
		//Checks whether the memory-address can be lowered into VPM
		if(!val.hasType(ValueType::LOCAL))
			throw CompilationError(CompilationStep::GENERAL, "Cannot access memory-address without a local", val.to_string());
		const Local* base = val.local->getBase(true);
		if(base->is<Global>())
			/*
			 * Constant globals can be moved into VPM (actually completely into constant values), since they do not change.
			 * Non-constant globals on the other side cannot be moved to the VPM, since they might lose their values in the next work-group.
			 * Local memory is mapped by LLVM into globals with __local address space, but can be lowered to VPM, since it is only used within one work-group
			 */
			return base->as<Global>()->isConstant || base->type.getPointerType().ifPresent([](const PointerType* ptr) -> bool { return ptr->addressSpace == AddressSpace::LOCAL;});
		if(base->is<Parameter>())
			/*
			 * Since parameter are used outside of the kernel execution (host-side), they cannot be lowered into VPM.
			 * The only exception are __local parameter, which are not used outside of the work-group and can therefore handled as local values.
			 */
			return base->type.getPointerType().value()->addressSpace == AddressSpace::LOCAL;
		if(base->is<StackAllocation>())
			//the stack can always be lowered into VPM
			return true;
		//for any other value, do not lower
		return false;
	}

	/*
	 * Checks whether the local value (local, register) can be lifted into VPM.
	 * This can be useful for operations performing memory-copy without QPU-side access to skip the steps of loading into QPU and writing back to VPM.
	 */
	if(!val.hasType(ValueType::LOCAL))
		//any non-local cannot be moved to VPM
		return false;

	return std::all_of(val.local->getUsers().begin(), val.local->getUsers().end(), [](const std::pair<const LocalUser*, LocalUse>& pair) -> bool
	{
		//TODO correct?
		return dynamic_cast<const MemoryInstruction*>(pair.first) != nullptr;
	});
}

bool MemoryInstruction::canMoveSourceIntoVPM() const
{
	return canMoveIntoVPM(getSource(), op == MemoryOperation::COPY || op == MemoryOperation::READ);
}

bool MemoryInstruction::canMoveDestinationIntoVPM() const
{
	return canMoveIntoVPM(getDestination(), op != MemoryOperation::WRITE);
}

DataType MemoryInstruction::getSourceElementType(bool sizedType) const
{
	switch(op)
	{
		case MemoryOperation::COPY:
		{
			DataType elementType = getSource().type.getElementType();
			if(!sizedType)
				//simple pointed-to type
				return elementType;
			//sized pointed-to type
			if(!getNumEntries().isLiteralValue())
				throw CompilationError(CompilationStep::GENERAL, "Cannot calculate type-size from dynamically sized memory-operation", to_string());
			std::shared_ptr<ComplexType> complex(new ArrayType(elementType, static_cast<unsigned>(getNumEntries().getLiteralValue()->integer)));
			return DataType((elementType.to_string() + "[") + std::to_string(getNumEntries().getLiteralValue()->integer) + "]", 1, complex);
		}
		case MemoryOperation::FILL:
			//local value
			return getSource().type;
		case MemoryOperation::READ:
			//pointed-to type
			return getSource().type.getElementType();
		case MemoryOperation::WRITE:
			//local value
			return getSource().type;
	}
	throw CompilationError(CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}

DataType MemoryInstruction::getDestinationElementType(bool sizedType) const
{
	switch(op)
	{
		case MemoryOperation::COPY:
			//fall-through on purpose
		case MemoryOperation::FILL:
		{
			DataType elementType = getDestination().type.getElementType();
			if(!sizedType)
				//simple pointed-to type
				return elementType;
			//sized pointed-to type
			if(!getNumEntries().isLiteralValue())
				throw CompilationError(CompilationStep::GENERAL, "Cannot calculate type-size from dynamically sized memory-operation", to_string());
			std::shared_ptr<ComplexType> complex(new ArrayType(elementType, static_cast<unsigned>(getNumEntries().getLiteralValue()->integer)));
			return DataType((elementType.to_string() + "[") + std::to_string(getNumEntries().getLiteralValue()->integer) + "]", 1, complex);
		}
		case MemoryOperation::READ:
			//local value
			return getDestination().type;
		case MemoryOperation::WRITE:
			//pointed-to type
			return getDestination().type.getElementType();
	}
	throw CompilationError(CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}
