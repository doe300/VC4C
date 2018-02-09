/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "VPM.h"

#include "../Profiler.h"
#include "log.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::intermediate;

//"[...] of 32-bit words,  [...]"
static constexpr unsigned VPM_WORD_WIDTH = sizeof(unsigned);
//"[...] 16 words wide [...]"
static constexpr unsigned VPM_NUM_COLUMNS = 16;
//"[...] maximum height of 64 words [...]"
static constexpr unsigned VPM_NUM_ROWS = 64;
//Store all data horizontally
static constexpr bool IS_HORIZONTAL = true;
//Store all data packed
static constexpr bool IS_PACKED = true;

/*
 * The naming of the setup-parameters are adopted from http://maazl.de/project/vc4asm/doc/vc4.qinc.html#VPM
 */

static std::string getVPMSizeName(uint8_t size)
{
	switch(size)
	{
		case 0: // 8 bit
			return "16 bytes";
		case 1: // 16 bit
			return "16 half-words";
		case 2: // 32 bit
			return "16 words";
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(static_cast<unsigned>(size)));
	}
}

static uint8_t getVPMSize(const DataType& paramType)
{
    //documentation, table 32 (page 57)
	switch(paramType.getScalarBitCount())
	{
		case 8:
			return 0;
		case 16:
			return 1;
		case 32:
			return 2;
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(paramType.getScalarBitCount()));
	}
}

static std::string getVPMModeName(uint8_t mode)
{
	if(mode >= 4)
		return " bytes";
	if(mode >= 2)
		return " half-words";
	return " words";
}

static std::string toAddressAndModeString(uint8_t size, uint16_t address, bool isHorizontal, bool isPacked)
{
	std::string res;

	res.append(isHorizontal ? "h" : "v");
	unsigned typeSize = 0;
	unsigned xCoord = 0;
	unsigned yCoord = 0;
	switch(size)
	{
		case 0: // 8 bit
			typeSize = 8;
			xCoord = address & 0x3;
			yCoord = address >> 2;
			break;
		case 1: // 16 bit
			typeSize = 16;
			xCoord = address & 0x1;
			yCoord = address >> 1;
			break;
		case 2: // 32 bit
			typeSize = 32;
			yCoord = address;
			break;
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(static_cast<unsigned>(size)));
	}

	res.append(std::to_string(typeSize));
	if(typeSize != 32)
		res.append(isPacked ? "p" : "l");
	res.append("(").append(std::to_string(yCoord));
	if(typeSize != 32)
		res.append(",").append(std::to_string(xCoord));
	res.append(")");

	return res;
}

static std::string toStride(uint8_t stride, uint8_t size)
{
	switch(size)
	{
		case 0: // 8 bit
			return stride >= 4 ? (std::to_string(stride / 4) + " rows") : (std::to_string(stride * 16) + " bytes");
		case 1: // 16 bit
			return stride >= 2 ? (std::to_string(stride / 2) + " rows") : (std::to_string(stride * 16) + " half-words");
		case 2: // 32 bit
			return (std::to_string(stride) + " rows");
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(static_cast<unsigned>(size)));
	}
}

std::string VPWGenericSetup::to_string() const
{
	return std::string("vpm_setup(size: ") + (getVPMSizeName(getSize()) + ", stride: ") + (toStride(getStride(), getSize()) + ", address: ") + toAddressAndModeString(getSize(), getAddress(), getHorizontal(), !getLaned()) + ")";
}

std::string VPWDMASetup::to_string() const
{
	//TODO byte/half-word offset (VPR too)
	return std::string("vdw_setup(rows: ") + (std::to_string(getUnits()) + ", elements: ") + std::to_string(getDepth()) + (getVPMModeName(getMode()) + ", address: ") + toAddressAndModeString(getVPMSize(TYPE_INT32), getVPMBase(), getHorizontal(), true) + ")";
}

std::string VPWStrideSetup::to_string() const
{
	return (std::string("vdw_setup(stride: ") + std::to_string(getStride())) + ")";
}

std::string VPWSetup::to_string() const
{
	if(isGenericSetup())
		return genericSetup.to_string();
	if(isDMASetup())
		return dmaSetup.to_string();
	return strideSetup.to_string();
}

std::string VPRGenericSetup::to_string() const
{
	return std::string("vpm_setup(num: ") + (std::to_string(getNumber()) + ", size: ") + (getVPMSizeName(getSize()) + ", stride: ") + (toStride(getStride(), getSize()) + ", address: ") + toAddressAndModeString(getSize(), getAddress(), getHorizontal(), !getLaned()) + ")";
}

std::string VPRDMASetup::to_string() const
{
	//TODO VPM/memory pitch
	return std::string("vdr_setup(rows: ") + (std::to_string(getNumberRows()) + ", elements: ") + std::to_string(getRowLength()) + (getVPMModeName(getMode()) + ", address: ") + toAddressAndModeString(getVPMSize(TYPE_INT32), getAddress(), !getVertical(), true) + ")";
}

std::string VPRStrideSetup::to_string() const
{
	return (std::string("vdr_setup(stride: ") + std::to_string(getStride())) + ")";
}

std::string VPRSetup::to_string() const
{
	if(isGenericSetup())
		return genericSetup.to_string();
	if(isDMASetup())
		return dmaSetup.to_string();
	return strideSetup.to_string();
}

/*
 * Returns the divisor for the column-offset applicable for the given type.
 * This is also the number of columns used by a 16-element vector of the given scalar type.
 *
 * Examples (assuming horizontal mode and packed data):
 * 8-bit (vector) types can have a byte-offset of 0 - 3 which translate to column-offsets of 0, 4, 8 and 12
 * and means, that the values need to be positioned at the column 0, 4, 8 or 12
 *
 * Usage:
 * alignment correct = column % column-divisor == 0
 * byte/half-word-offset = column / column-divisor
 */
static unsigned char getColumnDivisor(const DataType& type)
{
	/*
	 * Since we currently use one VPM row for a single vector,
	 * all values are positioned at the beginning of the row (row-index 0)
	 */
	if(true)
		return 16;

	/*
	 * General solution
	 *
	 * Returns 4 for byte types, 8 for half-word types and 1 for word-types
	 */
	return static_cast<unsigned char>(VPM_NUM_COLUMNS / (TYPE_INT32.getScalarBitCount() / type.getScalarBitCount()));
}

/*
 * Checks whether the given area fits into the VPM area
 */
static bool checkIndices(const DataType& type, unsigned char rowIndex, unsigned char columnIndex, unsigned char numRows, unsigned char numColumns)
{
	logging::debug() << static_cast<unsigned>(columnIndex) << " " << static_cast<unsigned>(numColumns) << " " <<
			static_cast<unsigned>(rowIndex) << " " << static_cast<unsigned>(numRows) << logging::endl;
	if((columnIndex + numColumns) > VPM_NUM_COLUMNS)
		return false;
	if((rowIndex + numRows) > VPM_NUM_ROWS)
		return false;
	if((columnIndex % getColumnDivisor(type)) != 0)
		return false;
	return true;
}

static uint8_t getVPMDMAMode(const DataType& paramType)
{
    //documentation, table 34 (page 58) / table 36 (page 59)
    //The offset is added initially onto the address, so don't set it (it will skip to write the first byte(s)/half word)
	switch(paramType.getScalarBitCount())
	{
		case 8:
			return 4;	//use byte-wise addressing with offset 1 byte
		case 16:
			return 2;	//use half-word wise addressing with offset of a half word
		case 32:
			return 0;
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(paramType.getScalarBitCount()));
	}
}

InstructionWalker periphery::insertReadDMA(Method& method, InstructionWalker it, const Value& dest, const Value& addr, const bool useMutex)
{
	if(useMutex)
	{
		//acquire mutex
		it.emplace( new MutexLock(MutexAccess::LOCK));
		it.nextInBlock();
	}

	it = method.vpm->insertReadRAM(it, addr, dest.type, nullptr, false);
	it = method.vpm->insertReadVPM(method, it, dest, nullptr, false);

	if(useMutex)
	{
		//free mutex
		it.emplace( new MutexLock(MutexAccess::RELEASE));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker periphery::insertWriteDMA(Method& method, InstructionWalker it, const Value& src, const Value& addr, const bool useMutex)
{
	if(useMutex)
	{
		//acquire mutex
		it.emplace( new MutexLock(MutexAccess::LOCK));
		it.nextInBlock();
	}

	it = method.vpm->insertWriteVPM(method, it, src, nullptr, false);
	it = method.vpm->insertWriteRAM(it, addr, src.type, nullptr, false);

	if(useMutex)
	{
		//free mutex
		it.emplace( new MutexLock(MutexAccess::RELEASE));
		it.nextInBlock();
	}
	return it;
}

std::pair<DataType, uint8_t> periphery::getBestVectorSize(const int64_t numBytes)
{
	for(uint8_t numElements = 16; numElements > 0; --numElements)
	{
		//16, 15, 14, ... elements of type...
		for(uint8_t typeSize = 4; typeSize > 0; typeSize /= 2)
		{
			//4 bytes (int), 2 bytes(short), 1 byte (char)
			if(numBytes % (numElements * typeSize) == 0)
			{
				DataType result = typeSize == 4 ? TYPE_INT32 : typeSize == 2 ? TYPE_INT16 : TYPE_INT8;
				result.num = numElements;
				uint8_t numVectors = static_cast<uint8_t>(numBytes / (static_cast<int64_t>(numElements) * static_cast<int64_t>(typeSize)));
				return std::make_pair(result, numVectors);
			}
		}
	}
	throw CompilationError(CompilationStep::LLVM_2_IR, "Failed to find element- and vector-sizes matching the given amount of bytes", std::to_string(numBytes));
}

/*
 * Calculates the address of the data in the VPM in the format used by the QPU-side access (read/write VPM)
 */
static uint8_t calculateQPUSideAddress(const DataType& type, unsigned char rowIndex, unsigned char columnIndex)
{
	//see Broadcom spec, pages 57, 58 and figure 8 (page 54)
	//Y coord is the multiple of 16 * 32-bit (= 64 Byte)
	//B coord is the byte [0, 1, 2, 3] in the word
	//H coord is the half-word [0, 1] in the word

	//check alignment
	if(!checkIndices(type, rowIndex, columnIndex, 1, getColumnDivisor(type)))
		throw CompilationError(CompilationStep::GENERAL, "Invalid alignment in VPM for type", type.to_string());

	if(type.getScalarBitCount() == 32)
		//"ADDR[5:0] = Y[5:0]"
		return static_cast<uint8_t>(rowIndex);
	else if(type.getScalarBitCount() == 16)
		// "ADDR[6:0] = Y[5:0] | H[0]"
		return static_cast<uint8_t>(rowIndex << 1) | static_cast<uint8_t>(columnIndex / getColumnDivisor(type));
	else if(type.getScalarBitCount() == 8)
		// "ADDR[7:0] = Y[5:0] | B[1:0]"
		return static_cast<uint8_t>(rowIndex << 2) | static_cast<uint8_t>(columnIndex / getColumnDivisor(type));
	else
		throw CompilationError(CompilationStep::GENERAL, "Invalid bit-width to store in VPM", type.to_string());
}

static InstructionWalker calculateElementOffset(Method& method, InstructionWalker it, const DataType& elementType, const Value& inAreaOffset, Value& elementOffset)
{
	if(inAreaOffset.getLiteralValue())
	{
		//e.g. 32-bit type, 4 byte offset -> 1 32-bit vector offset
		//e.g. byte4 type, 4 byte offset -> 1 byte-vector offset
		//e.g. half-word8 type, 32 byte offset -> 2 half-word vector offset
		elementOffset = Value(Literal(inAreaOffset.getLiteralValue()->signedInt() / elementType.getPhysicalWidth()), inAreaOffset.type);
	}
	else
	{
		//e.g. 32-bit type, 4 byte offset -> shr by 2 (= division by 4)
		elementOffset = method.addNewLocal(TYPE_INT32, "%vpm_element_offset");
		it.emplace(new Operation(OP_SHR, elementOffset, inAreaOffset, Value(Literal(static_cast<int32_t>(std::log2(elementType.getPhysicalWidth()))), TYPE_INT8)));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker VPM::insertReadVPM(Method& method, InstructionWalker it, const Value& dest, const VPMArea* area, bool useMutex, const Value& inAreaOffset)
{
	const DataType vpmStorageType = getVPMStorageType(dest.type);
	if(area != nullptr)
		area->checkAreaSize(vpmStorageType.getPhysicalWidth());
	else
		//a single vector can only use a maximum of 1 row
		updateScratchSize(1);

	it = insertLockMutex(it, useMutex);
	//1) configure reading from VPM into QPU
	const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
	const VPRSetup genericSetup(realArea.toReadSetup(dest.type));
	if(inAreaOffset == INT_ZERO)
	{
		it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(genericSetup.value)));
		it.nextInBlock();
	}
	else
	{
		//this is the offset in byte -> calculate the offset in elements of destination-type

		//1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
		Value elementOffset = UNDEFINED_VALUE;
		calculateElementOffset(method, it, dest.type, inAreaOffset, elementOffset);
		//2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
		//3) write setup with dynamic address
		it.emplace(new Operation(OP_ADD, VPM_IN_SETUP_REGISTER, Value(Literal(genericSetup.value), TYPE_INT32), elementOffset));
		it.nextInBlock();
	}
	//2) read value from VPM
	it.emplace( new MoveOperation(dest, VPM_IO_REGISTER));
	it.nextInBlock();
	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertWriteVPM(Method& method, InstructionWalker it, const Value& src, const VPMArea* area, bool useMutex, const Value& inAreaOffset)
{
	const DataType vpmStorageType = getVPMStorageType(src.type);
	if(area != nullptr)
		area->checkAreaSize(vpmStorageType.getPhysicalWidth());
	else
		//a single vector can only use a maximum of 1 row
		updateScratchSize(1);

	it = insertLockMutex(it, useMutex);
	//1. configure writing from QPU into VPM
	const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
	const VPWSetup genericSetup(realArea.toWriteSetup(src.type));
	if(inAreaOffset == INT_ZERO)
	{
		it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(genericSetup.value)));
		it.nextInBlock();
	}
	else
	{
		//this is the offset in byte -> calculate the offset in elements of destination-type

		//1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
		Value elementOffset = UNDEFINED_VALUE;
		calculateElementOffset(method, it, src.type, inAreaOffset, elementOffset);
		//2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
		//3) write setup with dynamic address
		it.emplace(new Operation(OP_ADD, VPM_IN_SETUP_REGISTER, Value(Literal(genericSetup.value), TYPE_INT32), elementOffset));
		it.nextInBlock();
	}
	//2. write data to VPM
	it.emplace(new MoveOperation(VPM_IO_REGISTER, src));
	it.nextInBlock();
	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertReadRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, const VPMArea* area, bool useMutex)
{
	if(area != nullptr)
		area->checkAreaSize(getVPMStorageType(type).getPhysicalWidth());
	else
		//a single vector can only use a maximum of 1 row
		updateScratchSize(1);

	if(memoryAddress.hasType(ValueType::LOCAL) && memoryAddress.local != nullptr)
	{
		//set the type of the parameter, if we can determine it
		if(memoryAddress.local->as<Parameter>() != nullptr)
			memoryAddress.local->as<Parameter>()->decorations = add_flag(memoryAddress.local->as<Parameter>()->decorations, ParameterDecorations::INPUT);
		if(memoryAddress.local->reference.first != nullptr && memoryAddress.local->reference.first->as<Parameter>() != nullptr)
			memoryAddress.local->reference.first->as<Parameter>()->decorations = add_flag(memoryAddress.local->reference.first->as<Parameter>()->decorations, ParameterDecorations::INPUT);
	}

	it = insertLockMutex(it, useMutex);
	//for some additional information, see
	//http://maazl.de/project/vc4asm/doc/VideoCoreIV-addendum.html

	//initialize VPM DMA for reading from host
	const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
	const VPRSetup dmaSetup(realArea.toReadDMASetup(type));
	it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(dmaSetup.value)));
	it.nextInBlock();
	const VPRSetup strideSetup(VPRStrideSetup(static_cast<uint16_t>(type.getPhysicalWidth())));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(strideSetup.value)));
	it.nextInBlock();

	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
	//-> write output-argument base address + offset/index into VPM_ADDR
	it.emplace( new MoveOperation(VPM_IN_ADDR_REGISTER, memoryAddress));
	it.nextInBlock();
	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
	it.emplace( new MoveOperation(NOP_REGISTER, VPM_IN_WAIT_REGISTER));
	it.nextInBlock();

	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertWriteRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, const VPMArea* area, bool useMutex)
{
	if(area != nullptr)
		area->checkAreaSize(getVPMStorageType(type).getPhysicalWidth());
	else
		//a single vector can only use a maximum of 1 row
		updateScratchSize(1);

	if(memoryAddress.hasType(ValueType::LOCAL) && memoryAddress.local != nullptr)
	{
		//set the type of the parameter, if we can determine it
		if(memoryAddress.local->as<Parameter>() != nullptr)
			memoryAddress.local->as<Parameter>()->decorations = add_flag(memoryAddress.local->as<Parameter>()->decorations, ParameterDecorations::OUTPUT);
		if(memoryAddress.local->reference.first != nullptr && memoryAddress.local->reference.first->as<Parameter>() != nullptr)
			memoryAddress.local->reference.first->as<Parameter>()->decorations = add_flag(memoryAddress.local->reference.first->as<Parameter>()->decorations, ParameterDecorations::OUTPUT);
	}

	it = insertLockMutex(it, useMutex);

	//initialize VPM DMA for writing to host
	const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
	const VPWSetup dmaSetup(realArea.toWriteDMASetup(type));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(dmaSetup.value)));
	it.nextInBlock();
	//set stride to zero
	const VPWSetup strideSetup(VPWStrideSetup(0));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(strideSetup.value)));
	it.nextInBlock();

	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
	//-> write output-argument base address + offset/index into VPM_ADDR
	it.emplace( new MoveOperation(VPM_OUT_ADDR_REGISTER, memoryAddress));
	it.nextInBlock();
	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
	it.emplace( new MoveOperation(NOP_REGISTER, VPM_OUT_WAIT_REGISTER));
	it.nextInBlock();

	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertCopyRAM(Method& method, InstructionWalker it, const Value& destAddress, const Value& srcAddress, const unsigned numBytes, const VPMArea* area, bool useMutex)
{
	const auto size = getBestVectorSize(numBytes);
	if(area != nullptr)
		area->checkAreaSize(size.first.getPhysicalWidth());
	else
		updateScratchSize(1);

	it = insertLockMutex(it, useMutex);

	it = insertReadRAM(it, srcAddress, size.first, area, false);
	it = insertWriteRAM(it, destAddress, size.first, area, false);

	for(unsigned i = 1; i < size.second; ++i)
	{
		const Value tmpSource = method.addNewLocal(srcAddress.type, "%mem_copy_addr");
		const Value tmpDest = method.addNewLocal(destAddress.type, "%mem_copy_addr");

		//increment offset from base address
		it.emplace(new Operation(OP_ADD, tmpSource, srcAddress, Value(Literal(i * size.first.getPhysicalWidth()), TYPE_INT8)));
		it.nextInBlock();
		it.emplace(new Operation(OP_ADD, tmpDest, destAddress, Value(Literal(i * size.first.getPhysicalWidth()), TYPE_INT8)));
		it.nextInBlock();

		it = insertReadRAM(it, tmpSource, size.first, area, false);
		it = insertWriteRAM(it, tmpDest, size.first, area, false);
	}
	it = insertUnlockMutex(it, useMutex);

	return it;
}

InstructionWalker VPM::insertFillRAM(Method& method, InstructionWalker it, const Value& memoryAddress, const DataType& type, const unsigned numCopies, const VPMArea* area, bool useMutex)
{
	if(numCopies == 0)
		return it;

	if(area != nullptr)
		area->checkAreaSize(type.getPhysicalWidth());
	else
		updateScratchSize(1);

	it = insertLockMutex(it, useMutex);
	it = insertWriteRAM(it, memoryAddress, type, area, false);
	for(unsigned i = 1; i < numCopies; ++i)
	{
		const Value tmpDest = method.addNewLocal(memoryAddress.type, "%mem_fill_addr");

		//increment offset from base address
		it.emplace(new Operation(OP_ADD, tmpDest, memoryAddress, Value(Literal(i * type.getPhysicalWidth()), TYPE_INT8)));
		it.nextInBlock();

		it = insertWriteRAM(it, tmpDest, type, area, false);
	}
	it = insertUnlockMutex(it, useMutex);

	return it;
}

void VPMArea::checkAreaSize(const unsigned requestedSize) const
{
	if(requestedSize > (numRows * VPM_NUM_COLUMNS * VPM_WORD_WIDTH)) //TODO rewrite packed/not packed!
		throw CompilationError(CompilationStep::GENERAL, "VPM area has not enough space available", std::to_string(requestedSize));
}

bool VPMArea::operator<(const VPMArea& other) const
{
	return rowOffset < other.rowOffset;
}

bool VPMArea::requiresSpacePerQPU() const
{
	return usageType == VPMUsage::REGISTER_SPILLING || usageType == VPMUsage::STACK;
}

DataType VPMArea::getElementType() const
{
	switch(usageType)
	{
		case VPMUsage::SCRATCH:
			//is not known
			return TYPE_UNKNOWN;
		case VPMUsage::LOCAL_MEMORY:
			//element-type of local assigned to this area
			return originalAddress->type.getElementType();
		case VPMUsage::REGISTER_SPILLING:
			//all registers have the same size
			return TYPE_INT32.toVectorType(16);
		case VPMUsage::STACK:
			//is not known
			return TYPE_UNKNOWN;
	}
	return TYPE_UNKNOWN;
}

uint8_t VPMArea::getElementsInRow(const DataType& elementType) const
{
	DataType type = (elementType.isUnknown() ? getElementType() : elementType).toVectorType(1);
	if(type.isUnknown())
		throw CompilationError(CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type");

	if(!canBePackedIntoRow())
		//if we cannot pack multiple vectors into one row, a row holds 1 vector (16 elements)
		return static_cast<uint8_t>(NATIVE_VECTOR_SIZE);
	//otherwise, a row has 64 Bytes of data, so we can calculate the number of elements fitting
	return static_cast<uint8_t>((VPM_NUM_COLUMNS * VPM_WORD_WIDTH * 8) / type.getScalarBitCount());
}

bool VPMArea::canBeAccessedViaDMA() const
{
	return usageType == VPMUsage::SCRATCH || getElementType().num == NATIVE_VECTOR_SIZE;
}

bool VPMArea::canBePackedIntoRow() const
{
	return !canBeAccessedViaDMA() || getElementType().num == NATIVE_VECTOR_SIZE;
}

VPWGenericSetup VPMArea::toWriteSetup(const DataType& elementType) const
{
	DataType type = elementType.isUnknown() ? getElementType() : elementType;
	if(type.isUnknown())
		throw CompilationError(CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type");

	//if we can pack into a single row, do so. Otherwise set stride to beginning of next row
	const uint8_t stride = canBePackedIntoRow() ? 1 : static_cast<uint8_t>(TYPE_INT32.getScalarBitCount() / type.getScalarBitCount());
	VPWGenericSetup setup(getVPMSize(type), stride, calculateQPUSideAddress(type, rowOffset, 0));
	setup.setHorizontal(IS_HORIZONTAL);
	setup.setLaned(!IS_PACKED);
	return setup;
}

VPWDMASetup VPMArea::toWriteDMASetup(const DataType& elementType, uint8_t numValues) const
{
	DataType type = elementType.isUnknown() ? getElementType() : elementType;
	if(type.isUnknown())
		throw CompilationError(CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type");

	//by "default", one value per row, so we need to store the number of values as number of rows
	uint8_t rowDepth = type.getVectorWidth(true);
	uint8_t numRows = numValues;
	if(canBePackedIntoRow())
	{
		//if we have the row packed, we need to calculate the row-width from the maximum row-width and the number of elements
		const unsigned totalNumElements = type.getVectorWidth(true) * numValues;
		const uint8_t elementsPerRow = getElementsInRow(type);
		if((totalNumElements > elementsPerRow) && (totalNumElements % elementsPerRow != 0))
			throw CompilationError(CompilationStep::GENERAL, "Cannot store a number of values which is not a multiple of the row-length ");
		if(totalNumElements > elementsPerRow)
		{
			rowDepth = elementsPerRow;
			numRows = static_cast<uint8_t>(totalNumElements / elementsPerRow);
		}
		else
		{
			rowDepth = static_cast<uint8_t>(totalNumElements);
			numRows = 1;
		}
	}

	VPWDMASetup setup(getVPMDMAMode(type), rowDepth, numRows);
	setup.setHorizontal(IS_HORIZONTAL);
	setup.setWordRow(rowOffset);

	return setup;
}

VPRGenericSetup VPMArea::toReadSetup(const DataType& elementType, uint8_t numValues) const
{
	DataType type = elementType.isUnknown() ? getElementType() : elementType;
	if(type.isUnknown())
		throw CompilationError(CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type");

	//if we can pack into a single row, do so. Otherwise set stride to beginning of next row
	const uint8_t stride = canBePackedIntoRow() ? 1 : static_cast<uint8_t>(TYPE_INT32.getScalarBitCount() / type.getScalarBitCount());
	VPRGenericSetup setup(getVPMSize(type), stride, numValues, calculateQPUSideAddress(type, rowOffset, 0));
	setup.setHorizontal(IS_HORIZONTAL);
	setup.setLaned(!IS_PACKED);
	return setup;
}

VPRDMASetup VPMArea::toReadDMASetup(const DataType& elementType, uint8_t numValues) const
{
	DataType type = elementType.isUnknown() ? getElementType() : elementType;
	if(type.isUnknown())
		throw CompilationError(CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type");
	if(numValues > 16)
		throw CompilationError(CompilationStep::GENERAL, "Cannot read more than 16 rows via DMA into VPW at a time");

	//If the data is packed, have a pitch of 1 unit (e.g. 1 byte/half-word/word offset depending on type)
	//otherwise, always jump to the next row
	const uint8_t vpmPitch = canBePackedIntoRow() ? 1 : TYPE_INT32.getScalarBitCount() / type.getScalarBitCount();
	VPRDMASetup setup(getVPMDMAMode(type), type.getVectorWidth(true) % 16 /* 0 => 16 */, numValues % 16 /* 0 => 16 */, vpmPitch % 16 /* 0 => 16 */);
	setup.setWordRow(rowOffset);
	setup.setVertical(!IS_HORIZONTAL);
	return setup;
}

VPM::VPM(const unsigned totalVPMSize) : maximumVPMSize(std::min(VPM_DEFAULT_SIZE, totalVPMSize)), areas(), isScratchLocked(false)
{
	//set a size of at least 1 row, so if no scratch is used, the first area has an offset of != 0 and therefore is different than the scratch-area
	areas.emplace(VPMArea{VPMUsage::SCRATCH, 0, 1, nullptr});
}

const VPMArea& VPM::getScratchArea()
{
	return *areas.begin();
}

const VPMArea* VPM::findArea(const Local* local)
{
	for(const VPMArea& area : areas)
		if(area.originalAddress == local)
			return &area;
	return nullptr;
}

const VPMArea* VPM::addArea(const Local* local, const DataType& elementType, bool isStackArea, unsigned numStacks)
{
	//Since we can only read/write in packages of 16-element vectors on the QPU-side, we need to reserve enough space for 16-element vectors (even if we do not use all of the elements)
	DataType inVPMType = getVPMStorageType(elementType);

	unsigned requestedSize = inVPMType.getPhysicalWidth() * (isStackArea ? numStacks : 1);
	uint8_t numRows = static_cast<unsigned char>(requestedSize / (VPM_NUM_COLUMNS * VPM_WORD_WIDTH) + (requestedSize % (VPM_NUM_COLUMNS * VPM_WORD_WIDTH) != 0));
	const VPMArea* area = findArea(local);
	if(area != nullptr && area->numRows >= numRows)
		return area;

	//lock scratch area, so it cannot expand over reserved VPM areas
	isScratchLocked = true;

	//find free consecutive space in VPM with the requested size and return it
	uint8_t rowOffset = 0;
	for(const VPMArea& area : areas)
	{
		if(rowOffset + numRows > area.rowOffset)
		{
			//if the new area doesn't fit before the current one, place it after
			rowOffset = static_cast<unsigned char>(area.rowOffset + area.numRows);
		}
	}

	//check if we can fit at the end
	if(rowOffset + numRows < VPM_NUM_ROWS)
	{
		//for now align all new VPM areas at the beginning of a column
		auto it = areas.emplace(VPMArea{isStackArea ? VPMUsage::STACK : VPMUsage::LOCAL_MEMORY, rowOffset, numRows, local});
		logging::debug() << "Allocating " << numRows << " rows (per 64 byte) of VPM cache starting at row " << rowOffset << " for local: " << local->to_string(false) << (isStackArea ? std::string("(") + std::to_string(numStacks) + " stacks )" : "") << logging::endl;
		PROFILE_COUNTER(9010, "VPM cache size", requestedSize);
		return &(*it.first);
	}

	//no more (big enough) free space on VPM
	return nullptr;
}

unsigned VPM::getMaxCacheVectors(const DataType& type, bool writeAccess) const
{
	if(writeAccess)
		return std::min(63u, (maximumVPMSize / 16) / (type.getScalarBitCount() / 8));
	return std::min(15u, (maximumVPMSize / 16) / (type.getScalarBitCount() / 8));
}

void VPM::updateScratchSize(unsigned char requestedRows)
{
	if(requestedRows > VPM_NUM_ROWS)
		throw CompilationError(CompilationStep::GENERAL, "The requested size of the scratch area exceeds the total VPM size", std::to_string(requestedRows));

	if(getScratchArea().numRows < requestedRows)
	{
		if(isScratchLocked)
			throw CompilationError(CompilationStep::GENERAL, "Size of the scratch area is already locked");
		logging::debug() << "Increased the scratch size to " << requestedRows << " rows (" << requestedRows * 64 << " bytes)" << logging::endl;
		const_cast<unsigned char&>(getScratchArea().numRows) = requestedRows;
	}
}

InstructionWalker VPM::insertLockMutex(InstructionWalker it, bool useMutex) const
{
	if(useMutex)
	{
		//acquire mutex
		it.emplace( new MutexLock(MutexAccess::LOCK));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker VPM::insertUnlockMutex(InstructionWalker it, bool useMutex) const
{
	if(useMutex)
	{
		//free mutex
		it.emplace( new MutexLock(MutexAccess::RELEASE));
		it.nextInBlock();
	}
	return it;
}

VPMInstructions periphery::findRelatedVPMInstructions(InstructionWalker anyVPMInstruction, bool isVPMRead)
{
	const auto predAddressWrite = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool
	{
		if(isVPMRead)
			return inst->writesRegister(REG_VPM_IN_ADDR);
		return inst->writesRegister(REG_VPM_OUT_ADDR);
	};
	const auto predDMASetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool
	{
		if(dynamic_cast<const intermediate::LoadImmediate*>(inst) == nullptr)
			return false;
		if(isVPMRead)
			return inst->writesRegister(REG_VPM_IN_SETUP) && VPRSetup::fromLiteral(inst->getArgument(0)->getLiteralValue().value().unsignedInt()).isDMASetup();
		return inst->writesRegister(REG_VPM_OUT_SETUP) && VPWSetup::fromLiteral(inst->getArgument(0)->getLiteralValue().value().unsignedInt()).isDMASetup();
	};
	const auto predDMAWait = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool
	{
		if(isVPMRead)
			return inst->readsRegister(REG_VPM_IN_WAIT);
		return inst->readsRegister(REG_VPM_OUT_WAIT);
	};
	const auto predGenericSetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool
	{
		if(dynamic_cast<const intermediate::LoadImmediate*>(inst) == nullptr)
			return false;
		if(isVPMRead)
			return inst->writesRegister(REG_VPM_IN_SETUP) && VPRSetup::fromLiteral(inst->getArgument(0)->getLiteralValue().value().unsignedInt()).isGenericSetup();
		return inst->writesRegister(REG_VPM_OUT_SETUP) && VPWSetup::fromLiteral(inst->getArgument(0)->getLiteralValue().value().unsignedInt()).isGenericSetup();
	};
	const auto predStrideSetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool
	{
		if(dynamic_cast<const intermediate::LoadImmediate*>(inst) == nullptr)
			return false;
		if(isVPMRead)
			return inst->writesRegister(REG_VPM_IN_SETUP) && VPRSetup::fromLiteral(inst->getArgument(0)->getLiteralValue().value().unsignedInt()).isStrideSetup();
		return inst->writesRegister(REG_VPM_OUT_SETUP) && VPWSetup::fromLiteral(inst->getArgument(0)->getLiteralValue().value().unsignedInt()).isStrideSetup();
	};
	const auto predVPMAccess = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool
	{
		if(isVPMRead)
			return inst->readsRegister(REG_VPM_IO);
		return inst->writesRegister(REG_VPM_IO);
	};

	VPMInstructions result;


	//TODO could this select the wrong instructions for multiple VPM accesses within a single mutex-lock block?
	//XXX are multiple VPM accesses within a mutex-lock even possible without combining the setups and addresses?
	auto it = anyVPMInstruction;
	while(!it.isStartOfBlock())
	{
		//only look up to the next mutex (un)lock
		if(it.has<intermediate::MutexLock>())
			break;
		if(it.has())
		{
			if(!result.addressWrite && predAddressWrite(it.get()))
				result.addressWrite = it;
			if(!result.dmaSetup && predDMASetup(it.get()))
				result.dmaSetup = it;
			if(!result.dmaWait && predDMAWait(it.get()))
				result.dmaWait = it;
			if(!result.genericVPMSetup && predGenericSetup(it.get()))
				result.genericVPMSetup = it;
			if(!result.strideSetup && predStrideSetup(it.get()))
				result.strideSetup = it;
			if(!result.vpmAccess && predVPMAccess(it.get()))
				result.vpmAccess = it;
		}
		it.previousInBlock();
	}

	it = anyVPMInstruction;
	while(!it.isEndOfBlock())
	{
		//only look up to the next mutex (un)lock
		if(it.has<intermediate::MutexLock>())
			break;
		if(it.has())
		{
			if(!result.addressWrite && predAddressWrite(it.get()))
				result.addressWrite = it;
			if(!result.dmaSetup && predDMASetup(it.get()))
				result.dmaSetup = it;
			if(!result.dmaWait && predDMAWait(it.get()))
				result.dmaWait = it;
			if(!result.genericVPMSetup && predGenericSetup(it.get()))
				result.genericVPMSetup = it;
			if(!result.strideSetup && predStrideSetup(it.get()))
				result.strideSetup = it;
			if(!result.vpmAccess && predVPMAccess(it.get()))
				result.vpmAccess = it;
		}
		it.nextInBlock();
	}

	return result;
}

DataType VPM::getVPMStorageType(const DataType& type)
{
	DataType inVPMType = TYPE_UNKNOWN;
	if(type.getArrayType())
	{
		//e.g. short2[17] -> short16[17]
		//also int4[1][2] -> int16[1][2]
		ArrayType* array = new ArrayType(getVPMStorageType(type.getElementType()), type.getArrayType().value()->size);
		inVPMType = DataType((array->elementType.to_string() + "[") + std::to_string(array->size) + "]", 1, std::shared_ptr<ComplexType>(array));
	}
	else if(type.getPointerType())
		//e.g. int* -> int16
		inVPMType = TYPE_INT32.toVectorType(16);
	else if(type.complexType)
		throw CompilationError(CompilationStep::GENERAL, "Unhandled element-type to cache in VPM", type.to_string());
	else
		//e.g. char3 -> char16
		inVPMType = type.toVectorType(16);
	return inVPMType;
}
