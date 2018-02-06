/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../periphery/VPM.h"

using namespace vc4c;
using namespace vc4c::intermediate;

/*
 * TODO some memory locations are not recognized:
 * - when pointer is set via PHI-node
 * - pointers loaded from stack allocations storing pointers to pointers
 */

static void checkMemoryLocation(const Value& val)
{
	if(!val.type.getPointerType())
		throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to be a pointer", val.to_string());
	if(!val.hasType(ValueType::LOCAL) || !(val.local->getBase(true)->residesInMemory() || val.local->getBase(true)->is<Parameter>()))
		throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to refer to a memory location or a parameter containing one", val.to_string());
}

static void checkLocalValue(const Value& val)
{
	if(val.hasType(ValueType::LOCAL) && (val.local->residesInMemory() || (val.local->is<Parameter>() && (val.local->type.getPointerType() || val.local->type.getArrayType()))))
		throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to be a local value (local, register)", val.to_string());
}

static void checkSingleValue(const Value& val)
{
	if(val.getLiteralValue().is(Literal(static_cast<int64_t>(1))))
		return;
	throw CompilationError(CompilationStep::LLVM_2_IR, "Operand needs to the constant one", val.to_string());
}

MemoryInstruction::MemoryInstruction(const MemoryOperation op, const Value dest, const Value src, const Value numEntries) : IntermediateInstruction(dest),
		op(op)
{
	setArgument(0, src);
	setArgument(1, numEntries);
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
			return (getDestination().to_string() + " = load memory at ") + getSource().to_string();
		case MemoryOperation::WRITE:
			return std::string("store ") + (getSource().to_string() + " into ") + getDestination().to_string();
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
		checkMemoryLocation(val);
		//Checks whether the memory-address can be lowered into VPM
		const Local* base = val.local->getBase(true);
		if(base->type.getElementType().getStructType() || (base->type.getElementType().getArrayType() && base->type.getElementType().getArrayType().value()->elementType.getStructType()))
			//cannot lower structs/arrays of structs into VPM
			return false;
		const DataType inVPMType = periphery::VPM::getVPMStorageType(base->type.getElementType());
		if(inVPMType.getPhysicalWidth() > VPM_DEFAULT_SIZE)
			return false;
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
			 * XXX The only exception are __local parameter, which are not used outside of the work-group and can therefore handled as local values.
			 */
			return false;
		if(base->is<StackAllocation>())
			//the stack can always be lowered into VPM (if it fits!)
			return (inVPMType.getPhysicalWidth() * 12 /* number of stacks */) < VPM_DEFAULT_SIZE;
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
		//TODO enable if handled correctly by optimizations (e.g. combination of read/write into copy)
		return false; //return dynamic_cast<const MemoryInstruction*>(pair.first) != nullptr;
	});
}

bool MemoryInstruction::canMoveSourceIntoVPM() const
{
	if(op == MemoryOperation::READ || op == MemoryOperation::WRITE)
		checkSingleValue(getNumEntries());
	return canMoveIntoVPM(getSource(), op == MemoryOperation::COPY || op == MemoryOperation::READ);
}

bool MemoryInstruction::canMoveDestinationIntoVPM() const
{
	if(op == MemoryOperation::READ || op == MemoryOperation::WRITE)
		checkSingleValue(getNumEntries());
	return canMoveIntoVPM(getDestination(), op != MemoryOperation::READ);
}

bool MemoryInstruction::accessesConstantGlobal() const
{
	switch(op)
	{
		case MemoryOperation::COPY:
			checkMemoryLocation(getSource());
			checkMemoryLocation(getDestination());
			return (getSource().local->getBase(true)->is<Global>() && getSource().local->getBase(true)->as<Global>()->isConstant) || (getDestination().local->getBase(true)->is<Global>() && getDestination().local->getBase(true)->as<Global>()->isConstant);
		case MemoryOperation::FILL:
			checkMemoryLocation(getDestination());
			return getDestination().local->getBase(true)->is<Global>() && getDestination().local->getBase(true)->as<Global>()->isConstant;
		case MemoryOperation::READ:
			checkMemoryLocation(getSource());
			return getSource().local->getBase(true)->is<Global>() && getSource().local->getBase(true)->as<Global>()->isConstant;
		case MemoryOperation::WRITE:
			checkMemoryLocation(getDestination());
			return getDestination().local->getBase(true)->is<Global>() && getDestination().local->getBase(true)->as<Global>()->isConstant;
	}
	return false;
}

bool MemoryInstruction::accessesStackAllocation() const
{
	switch(op)
	{
		case MemoryOperation::COPY:
			checkMemoryLocation(getSource());
			checkMemoryLocation(getDestination());
			return getSource().local->getBase(true)->is<StackAllocation>() || getDestination().local->getBase(true)->is<StackAllocation>();
		case MemoryOperation::FILL:
			checkMemoryLocation(getDestination());
			return getDestination().local->getBase(true)->is<StackAllocation>();
		case MemoryOperation::READ:
			checkMemoryLocation(getSource());
			return getSource().local->getBase(true)->is<StackAllocation>();
		case MemoryOperation::WRITE:
			checkMemoryLocation(getDestination());
			return getDestination().local->getBase(true)->is<StackAllocation>();
	}
	return false;
}

static bool isGlobalWithLocalAddressSpace(const Local* local)
{
	return local->is<Global>() && local->type.getPointerType().value()->addressSpace == AddressSpace::LOCAL;
}

bool MemoryInstruction::accessesLocalMemory() const
{
	switch(op)
	{
		case MemoryOperation::COPY:
			checkMemoryLocation(getSource());
			checkMemoryLocation(getDestination());
			return isGlobalWithLocalAddressSpace(getSource().local->getBase(true)) || isGlobalWithLocalAddressSpace(getDestination().local->getBase(true));
		case MemoryOperation::FILL:
			checkMemoryLocation(getDestination());
			return isGlobalWithLocalAddressSpace(getDestination().local->getBase(true));
		case MemoryOperation::READ:
			checkMemoryLocation(getSource());
			return isGlobalWithLocalAddressSpace(getSource().local->getBase(true));
		case MemoryOperation::WRITE:
			checkMemoryLocation(getDestination());
			return isGlobalWithLocalAddressSpace(getDestination().local->getBase(true));
	}
	return false;
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
			checkLocalValue(getSource());
			return getSource().type;
		case MemoryOperation::READ:
			//pointed-to type
			checkMemoryLocation(getSource());
			checkSingleValue(getNumEntries());
			return getSource().type.getElementType();
		case MemoryOperation::WRITE:
			//local value
			checkLocalValue(getSource());
			checkSingleValue(getNumEntries());
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
			checkMemoryLocation(getDestination());
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
			checkLocalValue(getDestination());
			checkSingleValue(getNumEntries());
			return getDestination().type;
		case MemoryOperation::WRITE:
			//pointed-to type
			checkMemoryLocation(getDestination());
			checkSingleValue(getNumEntries());
			return getDestination().type.getElementType();
	}
	throw CompilationError(CompilationStep::GENERAL, "Unknown memory operation type", std::to_string(static_cast<unsigned>(op)));
}

FastSet<const Local*> MemoryInstruction::getMemoryAreas() const
{
	FastSet<const Local*> res;
	switch(op)
	{
		case MemoryOperation::COPY:
			checkMemoryLocation(getSource());
			checkMemoryLocation(getDestination());
			res.emplace(getSource().local->getBase(true));
			res.emplace(getDestination().local->getBase(true));
			break;
		case MemoryOperation::FILL:
			checkMemoryLocation(getDestination());
			res.emplace(getDestination().local->getBase(true));
			break;
		case MemoryOperation::READ:
			checkMemoryLocation(getSource());
			res.emplace(getSource().local->getBase(true));
			break;
		case MemoryOperation::WRITE:
			checkMemoryLocation(getDestination());
			res.emplace(getDestination().local->getBase(true));
			break;
	}
	return res;
}
