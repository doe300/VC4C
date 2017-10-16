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

static long getVPMSize(const DataType& paramType)
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

static long getVPMDMAMode(const DataType& paramType)
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

/*
static InstructionWalker insertConfigureVPRDMA(InstructionWalker it, const DataType& destType, const uint8_t numVectors = 1)
{
	//for some additional information, see
	//http://maazl.de/project/vc4asm/doc/VideoCoreIV-addendum.html

	//initialize VPM DMA for reading from host
	const long dmaMode = getVPMDMAMode(destType);
	const VPRSetup dmaSetup(VPRDMASetup(dmaMode, destType.getVectorWidth(true) % 16 / * 0 => 16 * /, numVectors % 16 / * 0 => 16 * /));
	it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(dmaSetup))));
	it.nextInBlock();
	const VPRSetup strideSetup(VPRStrideSetup(destType.getPhysicalWidth()));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(strideSetup))));
	return it.nextInBlock();
}
*/

InstructionWalker periphery::insertReadDMA(Method& method, InstructionWalker it, const Value& dest, const Value& addr, const bool useMutex)
{
/*
	if(addr.hasType(ValueType::LOCAL))
	{
		//set the type of the parameter, if we can determine it
		if(dynamic_cast<const Parameter*>(addr.local) != nullptr)
			dynamic_cast<Parameter*>(addr.local)->decorations = add_flag(dynamic_cast<const Parameter*>(addr.local)->decorations, ParameterDecorations::INPUT);
		if(addr.local->reference.first != nullptr && dynamic_cast<const Parameter*>(addr.local->reference.first) != nullptr)
			dynamic_cast<Parameter*>(addr.local->reference.first)->decorations = add_flag(dynamic_cast<const Parameter*>(addr.local->reference.first)->decorations, ParameterDecorations::INPUT);
	}

    if(useMutex)
    {
        //acquire mutex
        it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
        it.nextInBlock();
    }
    / *
     * Reading memory has two step: (see https://www.raspberrypi.org/forums/viewtopic.php?p=1143940&sid=dc1d1cceda5f0b407b2bfca9ae3422ec#p1143940)
     * 1. reading from memory into VPM via DMA
     * 2. reading from VPM into QPU
     *
     * So we need these steps:
     * 1. configure VPM DMA
     * 2. set memory address
     * 3. wait for DMA to finish
     * 4. configure VPR
     * 5. read value
     * /
    //1) configure VPR DMA to read from memory into VPM
    it = insertConfigureVPRDMA(it, dest.type);
	//2) write input-argument base address + offset/index into VPM_ADDR
	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
	it.emplace(new MoveOperation(VPM_IN_ADDR_REGISTER, addr));
	it.nextInBlock();
	//3) wait for DMA to finish loading into VPM
	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
	it.emplace( new MoveOperation(NOP_REGISTER, VPM_IN_WAIT_REGISTER));
	it.nextInBlock();
	//4) configure reading from VPM into QPU
	const auto size = getVPMSize(dest.type);
	const VPRSetup genericSetup(VPRGenericSetup(size, TYPE_INT32.getScalarBitCount() / dest.type.getScalarBitCount()));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(genericSetup))));
	it.nextInBlock();
	//5) read value from VPM
	it.emplace( new MoveOperation(dest, VPM_IO_REGISTER));
	it.nextInBlock();

    if(useMutex)
    {
        //free mutex
        it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
        it.nextInBlock();
    }
    return it;
*/
	if(useMutex)
	{
		//acquire mutex
		it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
		it.nextInBlock();
	}

	it = method.vpm->insertReadRAM(it, addr, dest.type, false);
	it = method.vpm->insertReadVPM(it, dest, false);

	if(useMutex)
	{
		//free mutex
		it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
		it.nextInBlock();
	}
	return it;
}

//static InstructionWalker insertConfigureDynamicVPRDMA(InstructionWalker it, Method& method, const Value& numComponents, const Value& componentWidth, const Value& numVectors = INT_ONE)
//{
//	const uint32_t VPM_READ_DMA_SETUP = 0 | (1 << 31) | (0 << 24) /* | (destType.num % 16 << 20) */ /* | (numVectors % 16 << 16) */ | (1 << 12) | (0 << 11);
//	const Value dmaTemp = method.addNewLocal(TYPE_INT32, "%vpm_dma");
//	const Value dmaTemp1 = method.addNewLocal(TYPE_INT32, "%vpm_dma");
//	it.emplace( new Operation("and", dmaTemp, numComponents, Value(Literal(0xFFL), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace( new Operation("shl", dmaTemp, dmaTemp, Value(Literal(20L), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace( new Operation("or", dmaTemp1, Value(Literal(static_cast<long>(VPM_READ_DMA_SETUP)), TYPE_INT32), dmaTemp));
//	it.nextInBlock();
//	Value dmaTemp2(UNDEFINED_VALUE);
//	if(numVectors.hasType(ValueType::LITERAL))
//	{
//		dmaTemp2 = Value(Literal((numVectors.literal.integer % 16) << 16), TYPE_INT32);
//	}
//	else
//	{
//		dmaTemp2 = method.addNewLocal(TYPE_INT32, "%vpm_dma");
//		it.emplace( new Operation("and", dmaTemp2, numVectors, Value(Literal(0xFFL), TYPE_INT8)));
//		it.nextInBlock();
//		it.emplace( new Operation("shl", dmaTemp2, dmaTemp2, Value(Literal(16L), TYPE_INT8)));
//		it.nextInBlock();
//	}
//	it.emplace( new Operation("or", dmaTemp1, dmaTemp1, dmaTemp2));
//	it.nextInBlock();
//	//this does the same as getVPMDMAMode
//	// 1 Byte => 4, 2 Bytes => 2, 4 Bytes => 0 -> 8 >> # Bytes
//	const Value dmaMode = method.addNewLocal(TYPE_INT8, "%vpm_dma_mode");
//	it.emplace( new Operation("shr", dmaMode, Value(Literal(8L), TYPE_INT8), componentWidth));
//	it.nextInBlock();
//	it.emplace( new Operation("shl", dmaMode, dmaMode, Value(Literal(28L), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace( new Operation("or", VPM_IN_SETUP_REGISTER, dmaTemp1, dmaMode));
//	it.nextInBlock();
//	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>( 9 << 28))));
//	it.nextInBlock();
//	return it;
//}
//
//InstructionWalker periphery::insertReadDMADynamic(InstructionWalker it, Method& method, const Value& dest, const Value& addr, const Value& componentWidth, const Value& numComponents, const bool useMutex)
//{
//	if(useMutex)
//	{
//		//acquire mutex
//		it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
//		it.nextInBlock();
//	}
//	//1) configure loading from memory into VPM via DMA
//	it = insertConfigureDynamicVPRDMA(it, method, numComponents, componentWidth);
//	//2) write input-argument base address + offset/index into VPM_ADDR
//	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
//	it.emplace( new MoveOperation(VPM_IN_ADDR_REGISTER, addr));
//	it.nextInBlock();
//	//3) wait for DMA to finish loading
//	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
//	it.emplace( new MoveOperation(NOP_REGISTER, VPM_IN_WAIT_REGISTER));
//	it.nextInBlock();
//	//4) configure reading from VPM into QPU
//	//Configures the address and mode to read from
//	const Value vpmSize = method.addNewLocal(TYPE_INT32, "%vpm_size");
//	//this does the same as getVPMSize
//	// 1 Byte => 0, 2 Bytes => 1, 4 Bytes => 2 -> # Bytes >> 1
//	it.emplace( new Operation("shr", vpmSize, componentWidth, Value(Literal(1L), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace( new Operation("shl", vpmSize, vpmSize, Value(Literal(8L), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace(new Operation("or", VPM_IN_SETUP_REGISTER, Value(Literal(static_cast<long>(1 << 20 | 1 << 12 | 1 << 11 | 0 << 10 /* | vpmSize << 8 */)), TYPE_INT32), vpmSize));
//	it.nextInBlock();
//	//5) read value from VPM
//	it.emplace(new MoveOperation(dest, VPM_IO_REGISTER));
//	it.nextInBlock();
//	if(useMutex)
//	{
//		//free mutex
//		it.emplace(new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
//		it.nextInBlock();
//	}
//	return it;
//}

/*
static InstructionWalker insertConfigureVPW(InstructionWalker it, const DataType& sourceType, const uint8_t numVectors = 1)
{
	//initialize VPM DMA for writing to host
	const long dmaMode = getVPMDMAMode(sourceType);
	const VPWSetup dmaSetup(VPWDMASetup(dmaMode, sourceType.getVectorWidth(true), numVectors % 128 / * 0 => 128 * /));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(dmaSetup))));
	it.nextInBlock();
	//set stride to zero
	const VPWSetup strideSetup(VPWStrideSetup(0));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(strideSetup))));
	it.nextInBlock();
	//general VPM configuration
	const long vpmSize = getVPMSize(sourceType);
	const VPWSetup genericSetup(VPWGenericSetup(vpmSize, TYPE_INT32.getScalarBitCount()/sourceType.getScalarBitCount() / * 0 => 64 * /));
	it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(genericSetup))));
	it.nextInBlock();
	return it;
}
*/

InstructionWalker periphery::insertWriteDMA(Method& method, InstructionWalker it, const Value& src, const Value& addr, const bool useMutex)
{
/*
	if(addr.hasType(ValueType::LOCAL))
	{
		//set the type of the parameter, if we can determine it
		if(dynamic_cast<const Parameter*>(addr.local) != nullptr)
			dynamic_cast<Parameter*>(addr.local)->decorations = add_flag(dynamic_cast<const Parameter*>(addr.local)->decorations, ParameterDecorations::OUTPUT);
		if(addr.local->reference.first != nullptr && dynamic_cast<const Parameter*>(addr.local->reference.first) != nullptr)
			dynamic_cast<Parameter*>(addr.local->reference.first)->decorations = add_flag(dynamic_cast<const Parameter*>(addr.local->reference.first)->decorations, ParameterDecorations::OUTPUT);
	}

    if(useMutex)
    {
        //acquire mutex
        it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
        it.nextInBlock();
    }
    //1) (re-)configure VPM
    it = insertConfigureVPW(it, src.type);
    //2) write value to VPM out-register
    it.emplace(new MoveOperation(VPM_IO_REGISTER, src));
    it.nextInBlock();
    //"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
    //3) -> write output-argument base address + offset/index into VPM_ADDR
    it.emplace( new MoveOperation(VPM_OUT_ADDR_REGISTER, addr));
    it.nextInBlock();
    //"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
    it.emplace( new MoveOperation(NOP_REGISTER, VPM_OUT_WAIT_REGISTER));
    it.nextInBlock();
    if(useMutex)
    {
        //free mutex
        it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
        it.nextInBlock();
    }
    return it;
*/
	if(useMutex)
	{
		//acquire mutex
		it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
		it.nextInBlock();
	}

	it = method.vpm->insertWriteVPM(it, src, false);
	it = method.vpm->insertWriteRAM(it, addr, src.type, false);

	if(useMutex)
	{
		//free mutex
		it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
		it.nextInBlock();
	}
	return it;
}

//static InstructionWalker insertConfigureDynamicVPW(InstructionWalker it, Method& method, const Value& numComponents, const Value& componentWidth, const Value& numVectors = INT_ONE)
//{
//	//general VPM configuration
//	/*
//	 * - write horizontal
//	 * - write packed (for sizes < 32-bit, they are written into the same 32-bit word)
//	 * - set size according to output data-type
//	 */
//	const Value vpmSize = method.addNewLocal(TYPE_INT32, "%vpm_size");
//	//this does the same as getVPMSize
//	// 1 Byte => 0, 2 Bytes => 1, 4 Bytes => 2 -> # Bytes >> 1
//	it.emplace( new Operation("shr", vpmSize, componentWidth, Value(Literal(1L), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace(  new Operation("shl", vpmSize, vpmSize, Value(Literal(8L), TYPE_INT8)));
//	it.nextInBlock();
//	it.emplace( new Operation("or", VPM_OUT_SETUP_REGISTER, Value(Literal(static_cast<long>(1 << 11 | 0 << 10 /* | vpmSize << 8 */ | 1 << 20)), TYPE_INT32), vpmSize));
//	it.nextInBlock();
//	//initialize VPM DMA for writing to host
//	/*
//	 * See Table 34: (page. 58)
//	 * - configure DMA mode
//	 * - 1 row (write for every output/QPU separate)
//	 * - set row depth according to the number of elements in the output-vector
//	 * - write horizontally
//	 * - set mode according to output data-type (0 for 32-bit, 2-3 for 16-bit (half word offset), 4-7 for 8-bit (byte offset)
//	 */
//	const uint32_t VPM_WRITE_DMA_SETUP = 0 | (2 << 30) | (1 << 23) /* | (sourceType.num << 16) */ | (1 << 14);
//	const Value dmaTemp = method.addNewLocal(TYPE_INT32, "%vpm_dma");
//	it.emplace(  new Operation("shl", dmaTemp, numComponents, Value(Literal(16L), TYPE_INT8)));
//	it.nextInBlock();
//	//this does the same as getVPMDMAMode
//	// 1 Byte => 4, 2 Bytes => 2, 4 Bytes => 0 -> 8 >> # Bytes
//	const Value dmaMode = method.addNewLocal(TYPE_INT8, "%vpm_dma_mode");
//	it.emplace(  new Operation("shr", dmaMode, Value(Literal(8L), TYPE_INT8), componentWidth));
//	it.nextInBlock();
//	it.emplace( new Operation("or", dmaTemp, dmaTemp, dmaMode));
//	it.nextInBlock();
//	it.emplace( new Operation("or", VPM_OUT_SETUP_REGISTER, Value(Literal(static_cast<long>(VPM_WRITE_DMA_SETUP)), TYPE_INT32), dmaTemp));
//	it.nextInBlock();
//	//set stride to zero
//	/*
//	 * See Table 35 (page 59):
//	 * - configure DMA stride, set to zero
//	 */
//	const uint32_t VPM_WRITE_STRIDE_SETUP = 0 | (3 << 30);
//	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(VPM_WRITE_STRIDE_SETUP))));
//	it.nextInBlock();
//	return it;
//}
//
//InstructionWalker periphery::insertWriteDMADynamic(InstructionWalker it, Method& method, const Value& src, const Value& addr, const Value& componentWidth, const Value& numComponents, const bool useMutex)
//{
//	if(useMutex)
//	{
//		//acquire mutex
//		it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
//		it.nextInBlock();
//	}
//	//1) (re-)configure VPM
//	it = insertConfigureDynamicVPW(it, method, numComponents, componentWidth);
//	//2) write value to VPM out-register
//	it.emplace( new MoveOperation(VPM_IO_REGISTER, src));
//	it.nextInBlock();
//	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
//	//3) -> write output-argument base address + offset/index into VPM_ADDR
//	it.emplace( new MoveOperation(VPM_OUT_ADDR_REGISTER, addr));
//	it.nextInBlock();
//	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
//	it.emplace( new MoveOperation(NOP_REGISTER, VPM_OUT_WAIT_REGISTER));
//	it.nextInBlock();
//	if(useMutex)
//	{
//		//free mutex
//		it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
//		it.nextInBlock();
//	}
//	return it;
//}

//InstructionWalker periphery::insertCopyDMA(InstructionWalker it, Method& method, const Value& srcAddr, const Value& destAddr, const DataType& vectorType, const uint8_t numEntries, const bool useMutex)
//{
//	//TODO Replace with calls to insertLoadVPM/insertStoreVPM, since they are correct/can be optimized
//	//TODO could rewrite, so QPUs do not need to access data, simply load RAM -> VPM and then store VPM -> RAM
//	if(numEntries > 16)
//		//XXX split into two parts, need to adapt src/dest addresses for upper half
//		throw CompilationError(CompilationStep::LLVM_2_IR, "Can only copy 16 elements at a time");
//	if(useMutex)
//	{
//		//acquire mutex
//		it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
//		it.nextInBlock();
//	}
//
//	const Value tmp = method.addNewLocal(vectorType, "%copy_tmp");
//	//1) configure VPR DMA to read from memory into VPM
//	it = insertConfigureVPRDMA(it, tmp.type, numEntries);
//	//2) write input-argument base address + offset/index into VPM_ADDR
//	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
//	it.emplace( new MoveOperation(VPM_IN_ADDR_REGISTER, srcAddr));
//	it.nextInBlock();
//	//3) configure VPW
//	it = insertConfigureVPW(it, tmp.type, numEntries);
//	//4) wait for DMA to finish loading into VPM
//	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
//	it.emplace( new MoveOperation(NOP_REGISTER, VPM_IN_WAIT_REGISTER));
//	it.nextInBlock();
//	//5) configure reading from VPM into QPU
//	const auto size = getVPMSize(tmp.type);
//	const VPRSetup genericVPRSetup(VPRGenericSetup(size, TYPE_INT32.getScalarBitCount() / tmp.type.getScalarBitCount(), numEntries % 16 /* 0 => 16 */));
//	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(genericVPRSetup.value))));
//	it.nextInBlock();
//	//6) read values from VPM and store into VPW
//	for(std::size_t i = 0; i < numEntries; ++i)
//	{
//		it.emplace( new MoveOperation(tmp, VPM_IO_REGISTER));
//		it.nextInBlock();
//		it.emplace( new MoveOperation(VPM_IO_REGISTER, tmp));
//		it.nextInBlock();
//	}
//	//7) write VPW to memory and wait for it to be finished
//	//"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or VCD_ST_ADDR register" (p. 56)
//	it.emplace( new MoveOperation(VPM_OUT_ADDR_REGISTER, destAddr));
//	it.nextInBlock();
//	//"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
//	it.emplace( new MoveOperation(NOP_REGISTER, VPM_OUT_WAIT_REGISTER));
//	it.nextInBlock();
//
//	if(useMutex)
//	{
//		//free mutex
//		it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
//		it.nextInBlock();
//	}
//	return it;
//}

std::pair<DataType, uint8_t> getBestVectorSize(const long numBytes)
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
				uint8_t numVectors = numBytes / (numElements * typeSize);
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
	uint8_t yCoord = byteOffset / 64;
	uint8_t remainder = byteOffset % 64;
	if(type.getScalarBitCount() == 32)
		//"ADDR[5:0] = Y[5:0]"
		return yCoord;
	else if(type.getScalarBitCount() == 16)
		// "ADDR[6:0] = Y[5:0] | H[0]"
		return (yCoord << 1) | (remainder / 32);
	else if(type.getScalarBitCount() == 8)
		// "ADDR[7:0] = Y[5:0] | B[1:0]"
		return (yCoord << 2) | (remainder / 16);
	else
		throw CompilationError(CompilationStep::GENERAL, "Invalid bit-width to store in VPM", type.to_string());
}

InstructionWalker VPM::insertReadVPM(InstructionWalker it, const Value& dest, bool useMutex)
{
	updateScratchSize(dest.type.getPhysicalWidth());
	it = insertLockMutex(it, useMutex);
	//1) configure reading from VPM into QPU
	const auto size = getVPMSize(dest.type);
	const VPRSetup genericSetup(VPRGenericSetup(size, TYPE_INT32.getScalarBitCount() / dest.type.getScalarBitCount(), calculateAddress(dest.type, 0)));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(genericSetup))));
	it.nextInBlock();
	//2) read value from VPM
	it.emplace( new MoveOperation(dest, VPM_IO_REGISTER));
	it.nextInBlock();
	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertWriteVPM(InstructionWalker it, const Value& src, bool useMutex)
{
	updateScratchSize(src.type.getPhysicalWidth());
	it = insertLockMutex(it, useMutex);
	//1. configure writing from QPU into VPM
	const long vpmSize = getVPMSize(src.type);
	const VPWSetup genericSetup(VPWGenericSetup(vpmSize, TYPE_INT32.getScalarBitCount() / src.type.getScalarBitCount(), calculateAddress(src.type, 0)));
	it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(genericSetup))));
	it.nextInBlock();
	//2. write data to VPM
	it.emplace(new MoveOperation(VPM_IO_REGISTER, src));
	it.nextInBlock();
	it = insertUnlockMutex(it, useMutex);
	return it;
}

InstructionWalker VPM::insertReadRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, bool useMutex)
{
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
	const long dmaMode = getVPMDMAMode(type);
	VPRSetup dmaSetup(VPRDMASetup(dmaMode, type.getVectorWidth(true) % 16 /* 0 => 16 */, 1 % 16 /* 0 => 16 */));
	dmaSetup.dmaSetup.setAddress(0);
	it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(dmaSetup))));
	it.nextInBlock();
	const VPRSetup strideSetup(VPRStrideSetup(type.getPhysicalWidth()));
	it.emplace( new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(static_cast<long>(strideSetup))));
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

InstructionWalker VPM::insertWriteRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, bool useMutex)
{
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
	const long dmaMode = getVPMDMAMode(type);
	VPWSetup dmaSetup(VPWDMASetup(dmaMode, type.getVectorWidth(true), 1 % 128 /* 0 => 128 */));
	dmaSetup.dmaSetup.setVPMBase(0);
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(dmaSetup))));
	it.nextInBlock();
	//set stride to zero
	const VPWSetup strideSetup(VPWStrideSetup(0));
	it.emplace( new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(static_cast<long>(strideSetup))));
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

InstructionWalker VPM::insertCopyRAM(Method& method, InstructionWalker it, const Value& destAddress, const Value& srcAddress, const unsigned numBytes, bool useMutex)
{
	const auto size = getBestVectorSize(numBytes);
	updateScratchSize(size.first.getPhysicalWidth());

	it = insertLockMutex(it, useMutex);

	it = insertReadRAM(it, srcAddress, size.first, false);
	it = insertWriteRAM(it, destAddress, size.first, false);

	for(uint8_t i = 1; i < size.second; ++i)
	{
		const Value tmpSource = method.addNewLocal(srcAddress.type, "%mem_copy_addr");
		const Value tmpDest = method.addNewLocal(destAddress.type, "%mem_copy_addr");

		//increment offset from base address
		it.emplace(new Operation("add", tmpSource, srcAddress, Value(Literal(static_cast<long>(i * size.first.getPhysicalWidth())), TYPE_INT8)));
		it.nextInBlock();
		it.emplace(new Operation("add", tmpDest, destAddress, Value(Literal(static_cast<long>(i * size.first.getPhysicalWidth())), TYPE_INT8)));
		it.nextInBlock();

		it = insertReadRAM(it, tmpSource, size.first, false);
		it = insertWriteRAM(it, tmpDest, size.first, false);
	}
	it = insertUnlockMutex(it, useMutex);

	return it;
}

VPM::VPM(const unsigned totalVPMSize) : maximumVPMSize(totalVPMSize), areas(), isScratchLocked(false)
{
	areas.push_back(VPMArea{VPMUsage::GENERAL_DMA, 0, 0, nullptr});
}

VPMArea& VPM::getScratchArea()
{
	return areas.front();
}

VPMArea* VPM::findArea(const Local* local)
{
	for(VPMArea& area : areas)
		if(area.dmaAddress == local)
			return &area;
	return nullptr;
}

VPMArea* VPM::addArea(const Local* local, unsigned requestedSize, bool alignToBack)
{
	VPMArea* area = findArea(local);
	if(area != nullptr && area->size >= requestedSize)
		return area;
	//TODO find free consecutive space in VPM with the requested size and return it

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
	getScratchArea().size = std::max(getScratchArea().size, requestedSize);
}

InstructionWalker VPM::insertLockMutex(InstructionWalker it, bool useMutex) const
{
	if(useMutex)
	{
		//acquire mutex
		it.emplace( new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker VPM::insertUnlockMutex(InstructionWalker it, bool useMutex) const
{
	if(useMutex)
	{
		//free mutex
		it.emplace( new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
		it.nextInBlock();
	}
	return it;
}
