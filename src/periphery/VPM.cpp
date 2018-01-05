/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "VPM.h"

#include "log.h"

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::intermediate;

static int64_t getVPMSize(const DataType& paramType)
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
			throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid parameter type-size", std::to_string(paramType.getScalarBitCount()));
	}
}

static int64_t getVPMDMAMode(const DataType& paramType)
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
			throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid parameter type-size", std::to_string(paramType.getScalarBitCount()));
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
	it = method.vpm->insertReadVPM(it, dest, nullptr, false);

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

	it = method.vpm->insertWriteVPM(it, src, nullptr, false);
	it = method.vpm->insertWriteRAM(it, addr, src.type, nullptr, false);

	if(useMutex)
	{
		//free mutex
		it.emplace( new MutexLock(MutexAccess::RELEASE));
		it.nextInBlock();
	}
	return it;
}

std::pair<DataType, uint8_t> getBestVectorSize(const int64_t numBytes)
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

static uint8_t calculateAddress(const DataType& type, unsigned byteOffset)
{
	//see Broadcom spec, pages 57, 58 and figure 8 (page 54)
	//Y coord is the multiple of 16 * 32-bit (= 64 Byte)
	//B coord is the byte [0, 1, 2, 3] in the word
	//H coord is the half-word [0, 1] in the word
	//the stride is always 32-bit and the pack-mode is always laned
	//XXX for now, we always address in size of 16-element vectors, since there is now way to address the upper-half of an 16-element vector (correct?)

	//check alignment
	if((byteOffset * 8) % (16 * type.getScalarBitCount()) != 0)
		throw CompilationError(CompilationStep::GENERAL, "Invalid alignment in VPM for type", type.to_string());

	//TODO correct?? needs testing!
	uint8_t yCoord = static_cast<uint8_t>(byteOffset / 64);
	uint8_t remainder = byteOffset % 64;
	if(type.getScalarBitCount() == 32)
		//"ADDR[5:0] = Y[5:0]"
		return yCoord;
	else if(type.getScalarBitCount() == 16)
		// "ADDR[6:0] = Y[5:0] | H[0]"
		return static_cast<uint8_t>(yCoord << 1) | static_cast<uint8_t>(remainder / 32);
	else if(type.getScalarBitCount() == 8)
		// "ADDR[7:0] = Y[5:0] | B[1:0]"
		return static_cast<uint8_t>(yCoord << 2) | static_cast<uint8_t>(remainder / 16);
	else
		throw CompilationError(CompilationStep::GENERAL, "Invalid bit-width to store in VPM", type.to_string());
}

static unsigned calculateOffset(const VPMArea* area)
{
	if(area != nullptr)
		return area->baseOffset;
	return 0;
}

InstructionWalker VPM::insertReadVPM(InstructionWalker it, const Value& dest, const VPMArea* area, bool useMutex)
{
	if(area != nullptr)
		area->checkAreaSize(dest.type.getPhysicalWidth());
	else
		updateScratchSize(dest.type.getPhysicalWidth());

	it = insertLockMutex(it, useMutex);
	//1) configure reading from VPM into QPU
	const auto size = getVPMSize(dest.type);
	const VPRSetup genericSetup(VPRGenericSetup(size, TYPE_INT32.getScalarBitCount() / dest.type.getScalarBitCount(), 1, calculateAddress(dest.type, calculateOffset(area))));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<int64_t>(genericSetup))));
	it.nextInBlock();
	//2) read value from VPM
	it.emplace( new MoveOperation(dest, VPM_IO_REGISTER));
	it.nextInBlock();
	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertWriteVPM(InstructionWalker it, const Value& src, const VPMArea* area, bool useMutex)
{
	if(area != nullptr)
		area->checkAreaSize(src.type.getPhysicalWidth());
	else
		updateScratchSize(src.type.getPhysicalWidth());

	it = insertLockMutex(it, useMutex);
	//1. configure writing from QPU into VPM
	const int64_t vpmSize = getVPMSize(src.type);
	const VPWSetup genericSetup(VPWGenericSetup(vpmSize, TYPE_INT32.getScalarBitCount() / src.type.getScalarBitCount(), calculateAddress(src.type, calculateOffset(area))));
	it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<int64_t>(genericSetup))));
	it.nextInBlock();
	//2. write data to VPM
	it.emplace(new MoveOperation(VPM_IO_REGISTER, src));
	it.nextInBlock();
	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertReadRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, const VPMArea* area, bool useMutex)
{
	if(area != nullptr)
		area->checkAreaSize(type.getPhysicalWidth());
	else
		updateScratchSize(type.getPhysicalWidth());

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
	const int64_t dmaMode = getVPMDMAMode(type);
	VPRSetup dmaSetup(VPRDMASetup(dmaMode, type.getVectorWidth(true) % 16 /* 0 => 16 */, 1 % 16 /* 0 => 16 */));
	dmaSetup.dmaSetup.setAddress(static_cast<uint16_t>(calculateOffset(area)));
	it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<int64_t>(dmaSetup))));
	it.nextInBlock();
	const VPRSetup strideSetup(VPRStrideSetup(static_cast<uint16_t>(type.getPhysicalWidth())));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<int64_t>(strideSetup))));
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
		area->checkAreaSize(type.getPhysicalWidth());
	else
		updateScratchSize(type.getPhysicalWidth());

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
	const int64_t dmaMode = getVPMDMAMode(type);
	VPWSetup dmaSetup(VPWDMASetup(dmaMode, type.getVectorWidth(true), 1 % 128 /* 0 => 128 */));
	dmaSetup.dmaSetup.setVPMBase(static_cast<uint16_t>(calculateOffset(area)));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<int64_t>(dmaSetup))));
	it.nextInBlock();
	//set stride to zero
	const VPWSetup strideSetup(VPWStrideSetup(0));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<int64_t>(strideSetup))));
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
		updateScratchSize(size.first.getPhysicalWidth());

	it = insertLockMutex(it, useMutex);

	it = insertReadRAM(it, srcAddress, size.first, area, false);
	it = insertWriteRAM(it, destAddress, size.first, area, false);

	for(uint8_t i = 1; i < size.second; ++i)
	{
		const Value tmpSource = method.addNewLocal(srcAddress.type, "%mem_copy_addr");
		const Value tmpDest = method.addNewLocal(destAddress.type, "%mem_copy_addr");

		//increment offset from base address
		it.emplace(new Operation(OP_ADD, tmpSource, srcAddress, Value(Literal(static_cast<int64_t>(i * size.first.getPhysicalWidth())), TYPE_INT8)));
		it.nextInBlock();
		it.emplace(new Operation(OP_ADD, tmpDest, destAddress, Value(Literal(static_cast<int64_t>(i * size.first.getPhysicalWidth())), TYPE_INT8)));
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
		updateScratchSize(type.getPhysicalWidth());

	it = insertLockMutex(it, useMutex);
	it = insertWriteRAM(it, memoryAddress, type, area, false);
	for(unsigned i = 1; i < numCopies; ++i)
	{
		const Value tmpDest = method.addNewLocal(memoryAddress.type, "%mem_fill_addr");

		//increment offset from base address
		it.emplace(new Operation(OP_ADD, tmpDest, memoryAddress, Value(Literal(static_cast<int64_t>(i * type.getPhysicalWidth())), TYPE_INT8)));
		it.nextInBlock();

		it = insertWriteRAM(it, tmpDest, type, area, false);
	}
	it = insertUnlockMutex(it, useMutex);

	return it;
}

void VPMArea::checkAreaSize(const unsigned requestedSize) const
{
	if(requestedSize > size)
		throw CompilationError(CompilationStep::GENERAL, "VPM area has not enough space available", std::to_string(requestedSize));
}

bool VPMArea::operator<(const VPMArea& other) const
{
	return baseOffset < other.baseOffset;
}

bool VPMArea::requiresSpacePerQPU() const
{
	return usageType == VPMUsage::REGISTER_SPILLING;
}

unsigned VPMArea::getTotalSize() const
{
	return (requiresSpacePerQPU() ? 12 : 1) * size;
}

VPM::VPM(const unsigned totalVPMSize) : maximumVPMSize(totalVPMSize), areas(), isScratchLocked(false)
{
	areas.emplace(VPMArea{VPMUsage::GENERAL_DMA, 0, 0, nullptr});
}

const VPMArea& VPM::getScratchArea()
{
	return *areas.begin();
}

const VPMArea* VPM::findArea(const Local* local)
{
	for(const VPMArea& area : areas)
		if(area.dmaAddress == local)
			return &area;
	return nullptr;
}

const VPMArea* VPM::addArea(const Local* local, unsigned requestedSize, bool alignToBack)
{
	const VPMArea* area = findArea(local);
	if(area != nullptr && area->size >= requestedSize)
		return area;

	//lock scratch area, so it cannot expand over reserved VPM areas
	isScratchLocked = true;

	//find free consecutive space in VPM with the requested size and return it
	unsigned baseOffset = 0;
	for(const VPMArea& area : areas)
	{
		if(baseOffset + requestedSize < area.baseOffset)
			//if the new area doesn't fit before the current one, place it after
			baseOffset = area.baseOffset + area.size;
	}

	//check if we can fit at the end
	if(baseOffset + requestedSize < maximumVPMSize)
	{
		auto it = areas.emplace(VPMArea{VPMUsage::SPECIFIC_DMA, baseOffset, requestedSize, local});
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

void VPM::updateScratchSize(unsigned requestedSize)
{
	if(isScratchLocked)
		throw CompilationError(CompilationStep::GENERAL, "Size of the scratch area is already locked");
	if(requestedSize > maximumVPMSize)
		throw CompilationError(CompilationStep::GENERAL, "The requested size of the scratch area exceeds the total VPM size", std::to_string(requestedSize));

	if(getScratchArea().size < requestedSize)
		logging::debug() << "Increased the scratch size to " << requestedSize << " bytes" << logging::endl;

	//TODO is this correct?
	//Since we do not store all data completely packed, do we?
	const_cast<unsigned&>(getScratchArea().size) = std::max(getScratchArea().size, requestedSize);
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
