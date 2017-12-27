/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TMU.h"

#include "../InstructionWalker.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::periphery;

const TMU periphery::TMU0{REG_TMU0_COORD_S_U_X, REG_TMU0_COORD_T_V_Y, REG_TMU0_COORD_R_BORDER_COLOR, REG_TMU0_COORD_B_LOD_BIAS, SIGNAL_LOAD_TMU0};
const TMU periphery::TMU1{REG_TMU1_COORD_S_U_X, REG_TMU1_COORD_T_V_Y, REG_TMU1_COORD_R_BORDER_COLOR, REG_TMU1_COORD_B_LOD_BIAS, SIGNAL_LOAD_TMU1};

static InstructionWalker insertCalculateAddressOffsets(Method& method, InstructionWalker it, const Value& baseAddress, const DataType& type, Value& outputAddress)
{
	if(type.num == 1)
	{
		outputAddress = baseAddress;
		return it;
	}
	/*
	 * we need to set the addresses in this way:
	 *
	 * element 0: base-address + sizeof(type) * 0
	 * element 1: base-address + sizeof(type) * 1
	 * element 2: base-address + sizeof(type) * 2
	 * ...
	 *
	 * any element not in use (e.g. 5 to 15 for 4-element vector) needs to be set to 0
	 */
	const Value addressOffsets = method.addNewLocal(TYPE_INT32.toVectorType(type.num), "%address_offset");
	//XXX actually this is baseAddr.type * type.num, but we can't have vectors of pointers
	outputAddress = method.addNewLocal(TYPE_INT32.toVectorType(type.num), "%tmu_address");

	//addressOffsets = sizeof(type) * elem_num
	it.emplace(new intermediate::Operation(OP_MUL24, addressOffsets, Value(Literal(static_cast<int64_t>(type.getScalarBitCount()) / 8), TYPE_INT8), ELEMENT_NUMBER_REGISTER));
	it.nextInBlock();
	//outputAddress = (elem_num < type.num) ? baseAddress + addressOffsets : 0
	it.emplace(new intermediate::Operation(OP_SUB, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(Literal(static_cast<int64_t>(type.num)), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
	it.nextInBlock();
	//XXX rewrite, so it can be combined with next instruction
	//TODO or generally check if we can rewrite "mov out, 0" in way so we can combine it with next/previous instruction (e.g. by using "xor out, x, x" or "v8subs out, x, x")
	it.emplace(new intermediate::MoveOperation(outputAddress, INT_ZERO, COND_NEGATIVE_CLEAR));
	it.nextInBlock();
	it.emplace(new intermediate::Operation(OP_ADD, outputAddress, baseAddress, addressOffsets, COND_NEGATIVE_SET));
	it.nextInBlock();
	return it;
}

/*
 * Loading via TMU has 2 limitations:
 * - the address needs to be 32-bit aligned (the last bits of the address are ignored)
 * - only 32-bit values can be read
 *
 * For 32-bit types, simply calculating the addresses of the other vector-elements by adding the type-size to the base address is correct.
 * For non 32-bit types, this is not quite correct, e.g. a loading of 4 short-values actually looks like this:
 *
 * Assumption: base-address is 4-Byte aligned:
 * element 0: base-address + sizeof(type) * 0 -> loads element 1 | element 0
 * element 1: base-address + sizeof(type) * 1 -> loads element 1 | element 0
 * element 2: base-address + sizeof(type) * 2 -> loads element 3 | element 2
 * element 3: base-address + sizeof(type) * 3 -> loads element 3 | element 2
 *
 * Assumption: base-address is not 4-Byte aligned:
 * element 0: base-address + sizeof(type) * 0 -> loads element 0 | out-of-bounds
 * element 1: base-address + sizeof(type) * 1 -> loads element 2 | element 1
 * element 2: base-address + sizeof(type) * 2 -> loads element 2 | element 1
 * element 3: base-address + sizeof(type) * 3 -> loads out-of-bounds | element 3
 * NOTE: out-of-bounds words cannot lie outside of reserved memory (if data does), since memory is always 4 Byte aligned and multiples of 4 Byte
 */
static InstructionWalker insertExtractHalfWordElements(Method& method, InstructionWalker it, const Value& dest, const Value& src, const Value& addressVector)
{
	//1) for every address, check if it is aligned to 4 Byte <-> address & 0b11 == 0
	it.emplace(new intermediate::Operation(OP_AND, NOP_REGISTER, addressVector, Value(Literal(static_cast<int64_t>(3)), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
	it.nextInBlock();
	//2) extract short values
	//2.1) if the element address is aligned correctly, use lower half-word, otherwise use upper-half word
	//2.2) otherwise, use lower half-word from odd elements and upper half-word from even elements

	//tmp = address & 0b11 ? src >> 16 : src
	const Value tmp = method.addNewLocal(dest.type, "%tmp_result");
	it.emplace(new intermediate::Operation(OP_SHR, tmp, src, Value(Literal(static_cast<int64_t>(16)), TYPE_INT8), COND_ZERO_CLEAR));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(tmp, src, COND_ZERO_SET));
	it.nextInBlock();

	//dest = tmp & 0xFFFF
	it.emplace(new intermediate::Operation(OP_AND, dest, tmp, Value(Literal(TYPE_INT16.getScalarWidthMask()), TYPE_INT32)));
	it.nextInBlock();
	return it;
}

/*
 * For problem description, see #insertExtractHalfWordElements
 *
 * Algorithm for single-byte types:
 * 1. shift elements right by offset
 * 2. AND with 0xFF to discard the other elements
 *
 * The offset to shift by is calculated as following:
 * element address has offset of x to alignment of 4 Byte: elem = src >> (8 * x)
 */
static InstructionWalker insertExtractByteElements(Method& method, InstructionWalker it, const Value& dest, const Value& src, const Value& addressVector)
{
	//alignmentOffset = address & 0b11
	const Value alignmentOffset = method.addNewLocal(dest.type, "%alignment_offset");
	it.emplace(new intermediate::Operation(OP_AND, alignmentOffset, addressVector, Value(Literal(static_cast<int64_t>(3)), TYPE_INT8)));
	it.nextInBlock();

	//shiftOffset = alignmentOffset * 8
	const Value shiftOffset = method.addNewLocal(dest.type, "%shift_offset");
	it.emplace(new intermediate::Operation(OP_MUL24, shiftOffset, alignmentOffset, Value(Literal(static_cast<int64_t>(8)), TYPE_INT8)));
	it.nextInBlock();

	//tmp = src >> shiftOffset
	const Value tmp = method.addNewLocal(dest.type, "%tmp_result");
	it.emplace(new intermediate::Operation(OP_SHR, tmp, src, shiftOffset));
	it.nextInBlock();

	//dest = tmp & 0xFF
	it.emplace(new intermediate::Operation(OP_AND, dest, tmp, Value(Literal(TYPE_INT8.getScalarWidthMask()), TYPE_INT32)));
	it.nextInBlock();
	return it;
}

InstructionWalker periphery::insertReadVectorFromTMU(Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu)
{
	if(dest.type.complexType && !dest.type.getPointerType())
		throw CompilationError(CompilationStep::GENERAL, "Reading of this type via TMU is not (yet) implemented", dest.type.to_string());

	Value addresses(UNDEFINED_VALUE);
	it = insertCalculateAddressOffsets(method, it, addr, dest.type, addresses);

	//"General-memory lookups are performed by writing to just the s-parameter, using the absolute memory address" (page 41)
	//1) write address to TMU_S register
	it.emplace(new intermediate::MoveOperation(tmu.getAddress(addr.type), addresses));
	it.nextInBlock();
	//2) trigger loading of TMU
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(tmu.signal);
	it.nextInBlock();
	//3) read value from R4
	//FIXME in both cases, result values are unsigned!! (Same behavior as for VPM?!)
	if(dest.type.getScalarBitCount() == 8)
	{
		const Value tmp = method.addNewLocal(TYPE_INT32.toVectorType(dest.type.num), "%tmu_result");
		it.emplace(new intermediate::MoveOperation(tmp, TMU_READ_REGISTER));
		it.nextInBlock();
		return insertExtractByteElements(method, it, dest, tmp, addresses);
	}
	else if(dest.type.getScalarBitCount() == 16)
	{
		const Value tmp = method.addNewLocal(TYPE_INT32.toVectorType(dest.type.num), "%tmu_result");
		it.emplace(new intermediate::MoveOperation(tmp, TMU_READ_REGISTER));
		it.nextInBlock();
		return insertExtractHalfWordElements(method, it, dest, tmp, addresses);
	}
	else
	{
		it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker periphery::insertGeneralReadTMU(Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu)
{
	//"General-memory lookups are performed by writing to just the s-parameter, using the absolute memory address" (page 41)
	//1) write address to TMU_S register
	it.emplace(new intermediate::MoveOperation(tmu.getAddress(addr.type), addr));
	it.nextInBlock();
	//2) trigger loading of TMU
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(tmu.signal);
	it.nextInBlock();
	//3) read value from R4
	it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
	it.nextInBlock();
	return it;
}

InstructionWalker periphery::insertReadTMU(Method& method, InstructionWalker it, const Value& image, const Value& dest, const Value& xCoord, const Optional<Value>& yCoord, const TMU& tmu)
{
	if(!image.hasType(ValueType::LOCAL))
		throw CompilationError(CompilationStep::GENERAL, "Cannot access image-configuration for non-local image", image.to_string());
	const Global* imageConfig = method.findGlobal(ImageType::toImageConfigurationName(image.local->name));
	if(imageConfig == nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Failed to find the image-configuration for", image.to_string());
	if(!xCoord.type.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the x-axis", xCoord.to_string());
	if(yCoord && !yCoord->type.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the y-axis", yCoord.to_string());

	// 1. set the UNIFORM pointer to point to the configurations for the image about to be read
	it.emplace(new intermediate::MoveOperation(Value(REG_UNIFORM_ADDRESS, TYPE_INT32.toVectorType(16).toPointerType()), imageConfig->createReference()));
	it.nextInBlock();
	// 2. need to wait 2 instructions for UNIFORM-pointer to be changed
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_UNIFORM));
	it.nextInBlock();
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_UNIFORM));
	it.nextInBlock();
	// 3. write the TMU addresses
	if(yCoord)
	{
		it.emplace(new intermediate::MoveOperation(tmu.getYCoord(yCoord->type), yCoord.value()));
		it.nextInBlock();
	}
	else
	{
		//for 1D-images, we only have an x-coordinate, but if we only write the TMU_S register, general TMU lookup is used!
		//so we write a dummy y-coordinate with a value of zero, to select the first row
		it.emplace(new intermediate::MoveOperation(tmu.getYCoord(), INT_ZERO));
		it.nextInBlock();
	}
	it.emplace(new intermediate::MoveOperation(tmu.getXCoord(xCoord.type), xCoord));
	it.nextInBlock();
	// 4. trigger loadtmu
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(tmu.signal);
	it.nextInBlock();
	// 5. read from r4 (stalls 9 to 20 cycles)
	it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
	it.nextInBlock();
	// 6. TODO reset UNIFORM pointer? for next work-group iteration, or disable when used with images?

	return it;
}
