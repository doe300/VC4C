/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "VPM.h"

#include "../Profiler.h"
#include "../intermediate/operators.h"
#include "log.h"

#include <cmath>
#include <iomanip>

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

//"[...] of 32-bit words,  [...]"
static constexpr unsigned VPM_WORD_WIDTH = sizeof(unsigned);
//"[...] 16 words wide [...]"
static constexpr unsigned VPM_NUM_COLUMNS = 16;
//"[...] maximum height of 64 words [...]"
static constexpr unsigned VPM_NUM_ROWS = 64;
// Store all data horizontally
static constexpr bool IS_HORIZONTAL = true;
// Store all data packed
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
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(static_cast<unsigned>(size)));
    }
}

static uint8_t getVPMSize(DataType paramType)
{
    // documentation, table 32 (page 57)
    switch(paramType.getScalarBitCount())
    {
    case 8:
        return 0;
    case 16:
        return 1;
    case 32:
        return 2;
    default:
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(paramType.getScalarBitCount()));
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
    // TODO vertical address is wrong, has y, x and word-part components
    unsigned typeSize = 0;
    unsigned xCoord = 0;
    unsigned yCoord = 0;
    switch(size)
    {
    case 0: // 8 bit
        typeSize = 8;
        xCoord = address & 0x3;
        yCoord = (address >> 2) & 0x3F;
        break;
    case 1: // 16 bit
        typeSize = 16;
        xCoord = address & 0x1;
        yCoord = (address >> 1) & 0x3F;
        break;
    case 2: // 32 bit
        typeSize = 32;
        yCoord = address & 0x3F;
        break;
    default:
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(static_cast<unsigned>(size)));
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

static std::string toDMAAddressAndModeString(uint16_t address, bool isHorizontal)
{
    std::string res;

    res.append(isHorizontal ? "h" : "v");
    unsigned typeSize = 32;
    unsigned xCoord = address & 0xF;
    unsigned yCoord = (address >> 4) & 0x3F;

    res.append(std::to_string(typeSize));
    res.append("(").append(std::to_string(yCoord));
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
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(static_cast<unsigned>(size)));
    }
}

/*
 * Reverts the common pattern of having a value of 0 meaning the limit by replacing zero with the limit
 */
template <typename T>
static constexpr T demodulo(T val, T limit)
{
    return val == 0 ? limit : val;
}

std::string VPWGenericSetup::to_string() const
{
    return std::string("vpm_setup(size: ") + (getVPMSizeName(getSize()) + ", stride: ") +
        (toStride(demodulo(getStride(), uint8_t{64}), getSize()) + ", address: ") +
        toAddressAndModeString(getSize(), getAddress(), getHorizontal(), !getLaned()) + ")";
}

std::string VPWDMASetup::to_string() const
{
    // TODO byte/half-word offset (VPR too)
    return std::string("vdw_setup(rows: ") + (std::to_string(demodulo(getUnits(), uint8_t{128})) + ", elements: ") +
        std::to_string(demodulo(getDepth(), uint8_t{128})) + (getVPMModeName(getMode()) + ", address: ") +
        toDMAAddressAndModeString(getVPMBase(), getHorizontal()) + ")";
}

std::string VPWStrideSetup::to_string() const
{
    return (std::string("vdw_setup(memory stride: ") + std::to_string(getStride())) + " bytes)";
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
    return std::string("vpm_setup(num: ") + (std::to_string(demodulo(getNumber(), uint8_t{16})) + ", size: ") +
        (getVPMSizeName(getSize()) + ", stride: ") +
        (toStride(demodulo(getStride(), uint8_t{64}), getSize()) + ", address: ") +
        toAddressAndModeString(getSize(), getAddress(), getHorizontal(), !getLaned()) + ")";
}

std::string VPRDMASetup::to_string() const
{
    // TODO VPM/memory pitch
    return std::string("vdr_setup(rows: ") + (std::to_string(demodulo(getNumberRows(), uint8_t{16})) + ", elements: ") +
        std::to_string(demodulo(getRowLength(), uint8_t{16})) + (getVPMModeName(getMode()) + ", address: ") +
        toDMAAddressAndModeString(getAddress(), !getVertical()) + ")";
}

std::string VPRStrideSetup::to_string() const
{
    return (std::string("vdr_setup(memory pitch: ") + std::to_string(getPitch())) + " bytes)";
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
static unsigned char getColumnDivisor(DataType type)
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
static bool checkIndices(
    DataType type, unsigned char rowIndex, unsigned char columnIndex, unsigned char numRows, unsigned char numColumns)
{
    if((columnIndex + numColumns) > VPM_NUM_COLUMNS)
        return false;
    if((rowIndex + numRows) > VPM_NUM_ROWS)
        return false;
    if((columnIndex % getColumnDivisor(type)) != 0)
        return false;
    return true;
}

static uint8_t getVPMDMAMode(DataType paramType)
{
    // documentation, table 34 (page 58) / table 36 (page 59)
    // The offset is added initially onto the address, so don't set it (it will skip to write the first byte(s)/half
    // word)
    switch(paramType.getScalarBitCount())
    {
    case 8:
        return 4; // use byte-wise addressing with offset 1 byte
    case 16:
        return 2; // use half-word wise addressing with offset of a half word
    case 32:
        return 0;
    default:
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid parameter type-size", std::to_string(paramType.getScalarBitCount()));
    }
}

InstructionWalker periphery::insertReadDMA(
    Method& method, InstructionWalker it, const Value& dest, const Value& addr, const bool useMutex)
{
    if(useMutex)
    {
        // acquire mutex
        it.emplace(new MutexLock(MutexAccess::LOCK));
        it.nextInBlock();
    }

    it = method.vpm->insertReadRAM(method, it, addr, dest.type, nullptr, false);
    it = method.vpm->insertReadVPM(method, it, dest, nullptr, false);

    if(useMutex)
    {
        // free mutex
        it.emplace(new MutexLock(MutexAccess::RELEASE));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker periphery::insertWriteDMA(
    Method& method, InstructionWalker it, const Value& src, const Value& addr, const bool useMutex)
{
    if(useMutex)
    {
        // acquire mutex
        it.emplace(new MutexLock(MutexAccess::LOCK));
        it.nextInBlock();
    }

    it = method.vpm->insertWriteVPM(method, it, src, nullptr, false);
    it = method.vpm->insertWriteRAM(method, it, addr, src.type, nullptr, false);

    if(useMutex)
    {
        // free mutex
        it.emplace(new MutexLock(MutexAccess::RELEASE));
        it.nextInBlock();
    }
    return it;
}

std::pair<DataType, uint8_t> periphery::getBestVectorSize(const int64_t numBytes)
{
    for(uint8_t numElements = 16; numElements > 0; --numElements)
    {
        // 16, 15, 14, ... elements of type...
        for(uint8_t typeSize = 4; typeSize > 0; typeSize /= 2)
        {
            // 4 bytes (int), 2 bytes(short), 1 byte (char)
            if(numBytes % (numElements * typeSize) == 0)
            {
                DataType result = typeSize == 4 ? TYPE_INT32 : typeSize == 2 ? TYPE_INT16 : TYPE_INT8;
                result = result.toVectorType(numElements);
                uint8_t numVectors = static_cast<uint8_t>(
                    numBytes / (static_cast<int64_t>(numElements) * static_cast<int64_t>(typeSize)));
                return std::make_pair(result, numVectors);
            }
        }
    }
    throw CompilationError(CompilationStep::LLVM_2_IR,
        "Failed to find element- and vector-sizes matching the given amount of bytes", std::to_string(numBytes));
}

/*
 * Calculates the address of the data in the VPM in the format used by the QPU-side access (read/write VPM)
 */
static uint8_t calculateQPUSideAddress(DataType type, unsigned char rowIndex, unsigned char columnIndex)
{
    // see Broadcom spec, pages 57, 58 and figure 8 (page 54)
    // Y coord is the multiple of 16 * 32-bit (= 64 Byte)
    // B coord is the byte [0, 1, 2, 3] in the word
    // H coord is the half-word [0, 1] in the word

    // check alignment
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

static NODISCARD InstructionWalker calculateElementOffset(
    Method& method, InstructionWalker it, DataType elementType, const Value& inAreaOffset, Value& elementOffset)
{
    // e.g. 32-bit type, 4 byte offset -> 1 32-bit element offset
    // e.g. byte4 type, 4 byte offset -> 1 byte-element offset
    // e.g. half-word8 type, 32 byte offset -> 2 half-word element offset
    if(inAreaOffset == INT_ZERO)
        elementOffset = INT_ZERO;
    else
        elementOffset = assign(it, TYPE_INT16, "%vpm_element_offset") =
            inAreaOffset / Literal(elementType.getPhysicalWidth());
    return it;
}

InstructionWalker VPM::insertReadVPM(Method& method, InstructionWalker it, const Value& dest, const VPMArea* area,
    bool useMutex, const Value& inAreaOffset)
{
    const DataType vpmStorageType = getVPMStorageType(dest.type);
    if(area != nullptr)
        area->checkAreaSize(vpmStorageType.getPhysicalWidth());
    else
        // a single vector can only use a maximum of 1 row
        updateScratchSize(1);

    it = insertLockMutex(it, useMutex);
    // 1) configure reading from VPM into QPU
    const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
    const VPRSetup genericSetup(realArea.toReadSetup(dest.type));
    if(inAreaOffset == INT_ZERO)
    {
        it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(genericSetup.value)));
        it->addDecorations(InstructionDecorations::VPM_READ_CONFIGURATION);
        it.nextInBlock();
    }
    else
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffset(method, it, dest.type, inAreaOffset, elementOffset);
        // 2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
        // 3) write setup with dynamic address
        assign(it, VPM_IN_SETUP_REGISTER) = (Value(Literal(genericSetup.value), TYPE_INT32) + elementOffset,
            InstructionDecorations::VPM_READ_CONFIGURATION);
    }
    // 2) read value from VPM
    assign(it, dest) = VPM_IO_REGISTER;
    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertWriteVPM(Method& method, InstructionWalker it, const Value& src, const VPMArea* area,
    bool useMutex, const Value& inAreaOffset)
{
    const DataType vpmStorageType = getVPMStorageType(src.type);
    if(area != nullptr)
        area->checkAreaSize(vpmStorageType.getPhysicalWidth());
    else
        // a single vector can only use a maximum of 1 row
        updateScratchSize(1);

    it = insertLockMutex(it, useMutex);
    // 1. configure writing from QPU into VPM
    const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
    const VPWSetup genericSetup(realArea.toWriteSetup(src.type));
    if(inAreaOffset == INT_ZERO)
    {
        it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(genericSetup.value)));
        it->addDecorations(InstructionDecorations::VPM_WRITE_CONFIGURATION);
        it.nextInBlock();
    }
    else
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffset(method, it, src.type, inAreaOffset, elementOffset);
        // 2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
        // 3) write setup with dynamic address
        assign(it, VPM_OUT_SETUP_REGISTER) = (Value(Literal(genericSetup.value), TYPE_INT32) + elementOffset,
            InstructionDecorations::VPM_WRITE_CONFIGURATION);
    }
    // 2. write data to VPM
    assign(it, VPM_IO_REGISTER) = src;
    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertReadRAM(Method& method, InstructionWalker it, const Value& memoryAddress, DataType type,
    const VPMArea* area, bool useMutex, const Value& inAreaOffset, const Value& numEntries)
{
    if(area != nullptr)
        area->checkAreaSize(getVPMStorageType(type).getPhysicalWidth());
    else
        // a single vector can only use a maximum of 1 row
        updateScratchSize(1);

    if(auto local = memoryAddress.checkLocal())
    {
        // set the type of the parameter, if we can determine it
        if(auto param = local->as<Parameter>())
            memoryAddress.local()->as<Parameter>()->decorations =
                add_flag(param->decorations, ParameterDecorations::INPUT);
        if(local->reference.first != nullptr && local->reference.first->as<Parameter>() != nullptr)
            local->reference.first->as<Parameter>()->decorations =
                add_flag(local->reference.first->as<Parameter>()->decorations, ParameterDecorations::INPUT);
    }

    auto rowCount = numEntries.getLiteralValue() ? numEntries.getLiteralValue()->unsignedInt() : 0;
    if(rowCount > 16)
        throw CompilationError(CompilationStep::GENERAL, "Cannot read more than 16 entries at a time from RAM via DMA",
            numEntries.to_string());

    it = insertLockMutex(it, useMutex);
    // for some additional information, see
    // http://maazl.de/project/vc4asm/doc/VideoCoreIV-addendum.html

    // initialize VPM DMA for reading from host
    const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
    const VPRSetup dmaSetup(realArea.toReadDMASetup(type, static_cast<uint8_t>(rowCount)));
    Value dmaSetupBits(Literal(dmaSetup.value), TYPE_INT32);
    if(inAreaOffset != INT_ZERO)
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffset(method, it, memoryAddress.type.getElementType(), inAreaOffset, elementOffset);
        // 2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
        if(!realArea.canBePackedIntoRow())
            // need to modify offset to point to next row, not next element in same row
            elementOffset = assign(it, TYPE_INT32, "%vpm_row_offset") = elementOffset << 4_val;
        // 3) write setup with dynamic address
        dmaSetupBits = assign(it, TYPE_INT32, "%vpr_setup") =
            (dmaSetupBits + elementOffset, InstructionDecorations::VPM_READ_CONFIGURATION);
    }
    if(!numEntries.getLiteralValue())
    {
        // we need to dynamically set the number of elements to be read

        // TODO this assumes 1 row = 1 entry, is this always correct?

        // only 0-15 is supported, where value of 0 means 16 rows
        // TODO this calculation treats a source of 0 or any multiple of 16 as 16 (and truncates all the higher counts)
        auto numRows = assign(it, TYPE_INT8, "%vpr_setup_rows") = numEntries & 0xF_val;
        auto numRowsShifted = assign(it, TYPE_INT8, "%vpr_setup_rows") = numRows << 16_val;
        dmaSetupBits = assign(it, TYPE_INT32, "%vpr_setup") = dmaSetupBits + numRowsShifted;
    }
    assign(it, VPM_IN_SETUP_REGISTER) = (dmaSetupBits, InstructionDecorations::VPM_READ_CONFIGURATION);

    const VPRSetup strideSetup(VPRStrideSetup(0));
    it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(strideSetup.value)));
    it->addDecorations(InstructionDecorations::VPM_READ_CONFIGURATION);
    it.nextInBlock();

    //"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or
    // VCD_ST_ADDR register" (p. 56)
    //-> write output-argument base address + offset/index into VPM_ADDR
    assign(it, VPM_DMA_LOAD_ADDR_REGISTER) = memoryAddress;
    //"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
    assign(it, NOP_REGISTER) = VPM_DMA_LOAD_WAIT_REGISTER;

    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertWriteRAM(Method& method, InstructionWalker it, const Value& memoryAddress, DataType type,
    const VPMArea* area, bool useMutex, const Value& inAreaOffset, const Value& numEntries)
{
    if(area != nullptr)
        area->checkAreaSize(getVPMStorageType(type).getPhysicalWidth());
    else
        // a single vector can only use a maximum of 1 row
        updateScratchSize(1);

    // TODO is the calculation of the size to copy correct? We are mixing different types (e.g. byte from memory
    // instruction, consecutive memory area) with type for VPM area (rows which might not be filled completely). Same
    // for reading RAM!

    auto rowCount = numEntries.getLiteralValue() ? numEntries.getLiteralValue()->unsignedInt() : 0;
    if(rowCount > 128)
        throw CompilationError(CompilationStep::GENERAL,
            "Cannot write more than 128 entries at a time into RAM via DMA", numEntries.to_string());

    if(auto local = memoryAddress.checkLocal())
    {
        // set the type of the parameter, if we can determine it
        if(auto param = local->as<Parameter>())
            memoryAddress.local()->as<Parameter>()->decorations =
                add_flag(param->decorations, ParameterDecorations::OUTPUT);
        if(local->reference.first != nullptr && local->reference.first->as<Parameter>() != nullptr)
            local->reference.first->as<Parameter>()->decorations =
                add_flag(local->reference.first->as<Parameter>()->decorations, ParameterDecorations::OUTPUT);
    }

    it = insertLockMutex(it, useMutex);

    // initialize VPM DMA for writing to host
    const VPMArea& realArea = area != nullptr ? *area : getScratchArea();
    const VPWSetup dmaSetup(realArea.toWriteDMASetup(type, static_cast<uint8_t>(rowCount)));
    Value dmaSetupBits(Literal(dmaSetup.value), TYPE_INT32);
    if(inAreaOffset != INT_ZERO)
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffset(method, it, memoryAddress.type.getElementType(), inAreaOffset, elementOffset);
        // 2) dynamically calculate new VPM address from base and offset (shift and add offset to setup-value)
        if(!realArea.canBePackedIntoRow())
            // need to modify offset to point to next row, not next element in same row
            elementOffset = assign(it, TYPE_INT32, "%vpm_row_offset") = elementOffset << 4_val;
        Value shiftedOffset = assign(it, TYPE_INT32) = elementOffset << 3_val;
        // 3) write setup with dynamic address
        dmaSetupBits = assign(it, TYPE_INT32, "%vpw_setup") =
            (Value(Literal(dmaSetup.value), TYPE_INT32) + shiftedOffset,
                InstructionDecorations::VPM_WRITE_CONFIGURATION);
    }
    if(!numEntries.getLiteralValue())
    {
        // we need to dynamically set the number of elements to be written

        // TODO this assumes 1 row = 1 entry, is this always correct?

        // only 0-128 is supported, where value of 0 means 128 rows
        // TODO this calculation treats a source of 0 or any multiple of 128 as 128 (and truncates all the higher
        // counts)
        auto numRows = assign(it, TYPE_INT8, "%vpw_setup_rows") = numEntries & 0x7F_val;
        auto numRowsShifted = assign(it, TYPE_INT8, "%vpw_setup_rows") = numRows << 23_val;
        dmaSetupBits = assign(it, TYPE_INT32, "%vpw_setup") = dmaSetupBits + numRowsShifted;
    }
    assign(it, VPM_OUT_SETUP_REGISTER) = (dmaSetupBits, InstructionDecorations::VPM_WRITE_CONFIGURATION);

    // set stride to zero
    const VPWSetup strideSetup(VPWStrideSetup(0));
    it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(strideSetup.value)));
    it->addDecorations(InstructionDecorations::VPM_WRITE_CONFIGURATION);
    it.nextInBlock();

    //"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or
    // VCD_ST_ADDR register" (p. 56)
    //-> write output-argument base address + offset/index into VPM_ADDR
    assign(it, VPM_DMA_STORE_ADDR_REGISTER) = memoryAddress;
    //"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
    assign(it, NOP_REGISTER) = VPM_DMA_STORE_WAIT_REGISTER;

    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertCopyRAM(Method& method, InstructionWalker it, const Value& destAddress,
    const Value& srcAddress, const unsigned numBytes, const VPMArea* area, bool useMutex)
{
    const auto size = getBestVectorSize(numBytes);
    if(area != nullptr)
        area->checkAreaSize(size.first.getPhysicalWidth());
    else
        updateScratchSize(1);

    it = insertLockMutex(it, useMutex);

    it = insertReadRAM(method, it, srcAddress, size.first, area, false);
    it = insertWriteRAM(method, it, destAddress, size.first, area, false);

    for(unsigned i = 1; i < size.second; ++i)
    {
        // increment offset from base address
        Value tmpSource = assign(it, srcAddress.type, "%mem_copy_addr") =
            srcAddress + Value(Literal(i * size.first.getPhysicalWidth()), TYPE_INT8);
        Value tmpDest = assign(it, destAddress.type, "%mem_copy_addr") =
            destAddress + Value(Literal(i * size.first.getPhysicalWidth()), TYPE_INT8);

        it = insertReadRAM(method, it, tmpSource, size.first, area, false);
        it = insertWriteRAM(method, it, tmpDest, size.first, area, false);
    }
    it = insertUnlockMutex(it, useMutex);

    return it;
}

InstructionWalker VPM::insertFillRAM(Method& method, InstructionWalker it, const Value& memoryAddress, DataType type,
    const unsigned numCopies, const VPMArea* area, bool useMutex)
{
    if(numCopies == 0)
        return it;

    if(area != nullptr)
        area->checkAreaSize(type.getPhysicalWidth());
    else
        updateScratchSize(1);

    it = insertLockMutex(it, useMutex);
    it = insertWriteRAM(method, it, memoryAddress, type, area, false);
    for(unsigned i = 1; i < numCopies; ++i)
    {
        // increment offset from base address
        Value tmpDest = assign(it, memoryAddress.type, "%mem_fill_addr") =
            memoryAddress + Value(Literal(i * type.getPhysicalWidth()), TYPE_INT8);
        it = insertWriteRAM(method, it, tmpDest, type, area, false);
    }
    it = insertUnlockMutex(it, useMutex);

    return it;
}

void VPMArea::checkAreaSize(const unsigned requestedSize) const
{
    if(requestedSize > (numRows * VPM_NUM_COLUMNS * VPM_WORD_WIDTH)) // TODO rewrite packed/not packed!
        throw CompilationError(
            CompilationStep::GENERAL, "VPM area has not enough space available", std::to_string(requestedSize));
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
        // is not known
        return TYPE_UNKNOWN;
    case VPMUsage::LOCAL_MEMORY:
        // element-type of local assigned to this area
        return originalAddress->type.getElementType();
    case VPMUsage::REGISTER_SPILLING:
        // all registers have the same size
        return TYPE_INT32.toVectorType(16);
    case VPMUsage::STACK:
        // is not known
        return TYPE_UNKNOWN;
    }
    return TYPE_UNKNOWN;
}

uint8_t VPMArea::getElementsInRow(DataType elementType) const
{
    DataType type = (elementType.isUnknown() ? getElementType() : elementType).toVectorType(1);
    if(type.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", elementType.to_string());

    if(!canBePackedIntoRow())
        // if we cannot pack multiple vectors into one row, a row holds 1 vector (16 elements)
        return static_cast<uint8_t>(NATIVE_VECTOR_SIZE);
    // otherwise, a row has 64 Bytes of data, so we can calculate the number of elements fitting
    return static_cast<uint8_t>((VPM_NUM_COLUMNS * VPM_WORD_WIDTH * 8) / type.getScalarBitCount());
}

bool VPMArea::canBeAccessedViaDMA() const
{
    return usageType == VPMUsage::SCRATCH || getElementType().getVectorWidth() == NATIVE_VECTOR_SIZE;
}

bool VPMArea::canBePackedIntoRow() const
{
    // TODO proper calculation (or pass in constructor!)
    return /* !canBeAccessedViaDMA() || */ getElementType().getVectorWidth() == NATIVE_VECTOR_SIZE;
}

VPWGenericSetup VPMArea::toWriteSetup(DataType elementType) const
{
    DataType type = elementType.isUnknown() ? getElementType() : elementType;
    if(type.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", elementType.to_string());

    // if we can pack into a single row, do so. Otherwise set stride to beginning of next row
    const uint8_t stride =
        canBePackedIntoRow() ? 1 : static_cast<uint8_t>(TYPE_INT32.getScalarBitCount() / type.getScalarBitCount());
    VPWGenericSetup setup(getVPMSize(type), stride, calculateQPUSideAddress(type, rowOffset, 0));
    setup.setHorizontal(IS_HORIZONTAL);
    setup.setLaned(!IS_PACKED);
    return setup;
}

VPWDMASetup VPMArea::toWriteDMASetup(DataType elementType, uint8_t numRows) const
{
    DataType type = elementType.isUnknown() ? getElementType() : elementType;
    if(type.getScalarBitCount() > 32)
        // converts e.g. 64.bit integer to 2x 32.bit integer
        type = DataType(32, type.getVectorWidth() * type.getScalarBitCount() / 32, type.isFloatingType());
    if(type.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", elementType.to_string());

    // by "default", one value per row, so we need to store the number of values as number of rows
    uint8_t rowDepth = type.getVectorWidth(true);
    if(canBePackedIntoRow())
    {
        // if we have the row packed, we need to calculate the row-width from the maximum row-width and the number of
        // elements
        const unsigned totalNumElements = type.getVectorWidth(true) * numRows;
        const uint8_t elementsPerRow = getElementsInRow(type);
        if((totalNumElements > elementsPerRow) && (totalNumElements % elementsPerRow != 0))
            throw CompilationError(CompilationStep::GENERAL,
                "Cannot store a number of values which is not a multiple of the row-length ", elementType.to_string());
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

VPRGenericSetup VPMArea::toReadSetup(DataType elementType, uint8_t numRows) const
{
    DataType type = elementType.isUnknown() ? getElementType() : elementType;
    if(type.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", elementType.to_string());

    // if we can pack into a single row, do so. Otherwise set stride to beginning of next row
    const uint8_t stride =
        canBePackedIntoRow() ? 1 : static_cast<uint8_t>(TYPE_INT32.getScalarBitCount() / type.getScalarBitCount());
    VPRGenericSetup setup(getVPMSize(type), stride, numRows, calculateQPUSideAddress(type, rowOffset, 0));
    setup.setHorizontal(IS_HORIZONTAL);
    setup.setLaned(!IS_PACKED);
    return setup;
}

VPRDMASetup VPMArea::toReadDMASetup(DataType elementType, uint8_t numRows) const
{
    DataType type = elementType.isUnknown() ? getElementType() : elementType;
    if(type.getScalarBitCount() > 32)
        // converts e.g. 64.bit integer to 2x 32.bit integer
        type = DataType(32, type.getVectorWidth() * type.getScalarBitCount() / 32, type.isFloatingType());
    if(type.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", elementType.to_string());
    if(numRows > 16)
        throw CompilationError(CompilationStep::GENERAL, "Cannot read more than 16 rows via DMA into VPW at a time",
            std::to_string(numRows));

    // If the data is packed, have a pitch of 1 unit (e.g. 1 byte/half-word/word offset depending on type)
    // otherwise, always jump to the next row
    const uint8_t vpmPitch = canBePackedIntoRow() ? 1 : TYPE_INT32.getScalarBitCount() / type.getScalarBitCount();
    VPRDMASetup setup(getVPMDMAMode(type), type.getVectorWidth(true) % 16 /* 0 => 16 */, numRows % 16 /* 0 => 16 */,
        vpmPitch % 16 /* 0 => 16 */);
    setup.setWordRow(rowOffset);
    setup.setVertical(!IS_HORIZONTAL);
    return setup;
}

static std::string toUsageString(VPMUsage usage, const Local* local)
{
    switch(usage)
    {
    case VPMUsage::LOCAL_MEMORY:
        return (local ? local->to_string() : "(nullptr)") + " (lowered)";
    case VPMUsage::REGISTER_SPILLING:
        return "register spilling";
    case VPMUsage::SCRATCH:
        return "scratch area";
    case VPMUsage::STACK:
        return "stack" + (local ? " " + local->to_string() : "");
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled VPM usage type", std::to_string(static_cast<unsigned>(usage)));
}

std::string VPMArea::to_string() const
{
    return toUsageString(usageType, originalAddress) + ", rows[" + std::to_string(static_cast<unsigned>(rowOffset)) +
        ", " + std::to_string(static_cast<unsigned>(rowOffset + numRows)) + "[";
}

VPM::VPM(const unsigned totalVPMSize) : maximumVPMSize(std::min(VPM_DEFAULT_SIZE, totalVPMSize)), areas(VPM_NUM_ROWS)
{
    // set a size of at least 1 row, so if no scratch is used, the first area has an offset of != 0 and therefore is
    // different than the scratch-area
    areas[0] = std::make_shared<VPMArea>(VPMArea{VPMUsage::SCRATCH, 0, 1, nullptr});
}

const VPMArea& VPM::getScratchArea() const
{
    return **areas.begin();
}

const VPMArea* VPM::findArea(const Local* local)
{
    for(const auto& area : areas)
        if(area && area->originalAddress == local)
            return area.get();
    return nullptr;
}

const VPMArea* VPM::addArea(const Local* local, DataType elementType, bool isStackArea, unsigned numStacks)
{
    // Since we can only read/write in packages of 16-element vectors on the QPU-side, we need to reserve enough space
    // for 16-element vectors (even if we do not use all of the elements)
    DataType inVPMType = getVPMStorageType(elementType);

    unsigned requestedSize = inVPMType.getPhysicalWidth() * (isStackArea ? numStacks : 1);
    if(requestedSize > maximumVPMSize)
        // does not fit, independent of packing of rows
        return nullptr;
    uint8_t numRows = static_cast<unsigned char>(
        requestedSize / (VPM_NUM_COLUMNS * VPM_WORD_WIDTH) + (requestedSize % (VPM_NUM_COLUMNS * VPM_WORD_WIDTH) != 0));
    const VPMArea* area = findArea(local);
    if(area != nullptr && area->numRows >= numRows)
        return area;

    // find free consecutive space in VPM with the requested size and return it
    // to keep the remaining space free for scratch, we start allocating space from the end of the VPM
    Optional<unsigned> rowOffset;
    uint8_t numFreeRows = 0;
    for(auto i = areas.size() - 1; i > 0 /* index 0 is always reserved for scratch */; --i)
    {
        if(areas[i])
        {
            // row is already reserved
            numFreeRows = 0;
            continue;
        }
        else
            ++numFreeRows;
        if(numFreeRows >= numRows)
        {
            rowOffset = static_cast<unsigned>(i);
            break;
        }
    }
    if(!rowOffset)
        // no more (big enough) free space on VPM
        return nullptr;

    // for now align all new VPM areas at the beginning of a row
    auto ptr = std::make_shared<VPMArea>(VPMArea{isStackArea ? VPMUsage::STACK : VPMUsage::LOCAL_MEMORY,
        static_cast<uint8_t>(rowOffset.value()), numRows, local});
    for(auto i = rowOffset.value(); i < (rowOffset.value() + numRows); ++i)
        areas[i] = ptr;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Allocating " << numRows << " rows (per 64 byte) of VPM cache starting at row " << rowOffset.value()
            << " for local: " << local->to_string(false)
            << (isStackArea ? std::string("(") + std::to_string(numStacks) + " stacks )" : "") << logging::endl);
    PROFILE_COUNTER(vc4c::profiler::COUNTER_GENERAL + 90, "VPM cache size", requestedSize);
    return ptr.get();
}

unsigned VPM::getMaxCacheVectors(DataType type, bool writeAccess) const
{
    unsigned numFreeRows = 0;
    // can possible use up all rows up to the first area
    for(const auto& area : areas)
    {
        if(area && area->usageType != VPMUsage::SCRATCH)
            break;
        ++numFreeRows;
    }

    if(writeAccess)
        return std::min(std::min(63u, (maximumVPMSize / 16) / (type.getScalarBitCount() / 8)), numFreeRows);
    return std::min(std::min(15u, (maximumVPMSize / 16) / (type.getScalarBitCount() / 8)), numFreeRows);
}

void VPM::updateScratchSize(unsigned char requestedRows)
{
    if(requestedRows > VPM_NUM_ROWS)
        throw CompilationError(CompilationStep::GENERAL,
            "The requested size of the scratch area exceeds the total VPM size", std::to_string(requestedRows));
    if(getMaxCacheVectors(TYPE_INT32, true) < requestedRows)
        throw CompilationError(CompilationStep::GENERAL,
            "The requested size of the scratch area exceeds the available VPM size", std::to_string(requestedRows));

    if(getScratchArea().numRows < requestedRows)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Increased the scratch size to " << requestedRows << " rows (" << requestedRows * 64 << " bytes)"
                << logging::endl);
        const_cast<unsigned char&>(getScratchArea().numRows) = requestedRows;
        // fill areas with scratch
        for(unsigned i = 1; i < requestedRows; ++i)
            areas[i] = areas[0];
    }
}

InstructionWalker VPM::insertLockMutex(InstructionWalker it, bool useMutex) const
{
    if(useMutex)
    {
        // acquire mutex
        it.emplace(new MutexLock(MutexAccess::LOCK));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker VPM::insertUnlockMutex(InstructionWalker it, bool useMutex) const
{
    if(useMutex)
    {
        // free mutex
        it.emplace(new MutexLock(MutexAccess::RELEASE));
        it.nextInBlock();
    }
    return it;
}

VPMInstructions periphery::findRelatedVPMInstructions(InstructionWalker anyVPMInstruction, bool isVPMRead)
{
    const auto predAddressWrite = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(isVPMRead)
            return inst->writesRegister(REG_VPM_DMA_LOAD_ADDR);
        return inst->writesRegister(REG_VPM_DMA_STORE_ADDR);
    };
    const auto predDMASetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(dynamic_cast<const intermediate::LoadImmediate*>(inst) == nullptr)
            return false;
        if(isVPMRead)
            return inst->writesRegister(REG_VPM_IN_SETUP) &&
                VPRSetup::fromLiteral(inst->assertArgument(0).getLiteralValue().value().unsignedInt()).isDMASetup();
        return inst->writesRegister(REG_VPM_OUT_SETUP) &&
            VPWSetup::fromLiteral(inst->assertArgument(0).getLiteralValue().value().unsignedInt()).isDMASetup();
    };
    const auto predDMAWait = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(isVPMRead)
            return inst->readsRegister(REG_VPM_DMA_LOAD_WAIT);
        return inst->readsRegister(REG_VPM_DMA_STORE_WAIT);
    };
    const auto predGenericSetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(dynamic_cast<const intermediate::LoadImmediate*>(inst) == nullptr)
            return false;
        if(isVPMRead)
            return inst->writesRegister(REG_VPM_IN_SETUP) &&
                VPRSetup::fromLiteral(inst->assertArgument(0).getLiteralValue().value().unsignedInt()).isGenericSetup();
        return inst->writesRegister(REG_VPM_OUT_SETUP) &&
            VPWSetup::fromLiteral(inst->assertArgument(0).getLiteralValue().value().unsignedInt()).isGenericSetup();
    };
    const auto predStrideSetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(dynamic_cast<const intermediate::LoadImmediate*>(inst) == nullptr)
            return false;
        if(isVPMRead)
            return inst->writesRegister(REG_VPM_IN_SETUP) &&
                VPRSetup::fromLiteral(inst->assertArgument(0).getLiteralValue().value().unsignedInt()).isStrideSetup();
        return inst->writesRegister(REG_VPM_OUT_SETUP) &&
            VPWSetup::fromLiteral(inst->assertArgument(0).getLiteralValue().value().unsignedInt()).isStrideSetup();
    };
    const auto predVPMAccess = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(isVPMRead)
            return inst->readsRegister(REG_VPM_IO);
        return inst->writesRegister(REG_VPM_IO);
    };

    VPMInstructions result;

    // TODO could this select the wrong instructions for multiple VPM accesses within a single mutex-lock block?
    // XXX are multiple VPM accesses within a mutex-lock even possible without combining the setups and addresses?
    auto it = anyVPMInstruction;
    while(!it.isStartOfBlock())
    {
        // only look up to the next mutex (un)lock
        if(it.get<intermediate::MutexLock>())
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
        // only look up to the next mutex (un)lock
        if(it.get<intermediate::MutexLock>())
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

DataType VPM::getVPMStorageType(DataType type)
{
    DataType inVPMType = TYPE_UNKNOWN;
    if(auto arrayType = type.getArrayType())
    {
        // e.g. short2[17] -> short16[17]
        // also int4[1][2] -> int16[1][2]
        inVPMType = getVPMStorageType(arrayType->elementType).toArrayType(arrayType->size);
    }
    else if(type.getPointerType())
        // e.g. int* -> int16
        inVPMType = TYPE_INT32.toVectorType(16);
    else if(!type.isSimpleType())
        throw CompilationError(CompilationStep::GENERAL, "Unhandled element-type to cache in VPM", type.to_string());
    else
        // e.g. char3 -> char16
        inVPMType = type.toVectorType(16);
    return inVPMType;
}

static void writeArea(std::wostream& s, const std::string& name, unsigned width)
{
    auto sub = name.substr(0, width - 1) + "|";
    s << std::setw(static_cast<int>(width)) << sub;
}

void VPM::dumpUsage() const
{
    static const unsigned outputWidth = 128;

    CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
        logging::debug() << "VPM usage: "
                         << std::accumulate(areas.begin(), areas.end(), 0u,
                                [](unsigned sum, const std::shared_ptr<VPMArea>& area) -> unsigned {
                                    return sum + (area != nullptr);
                                })
                         << " of " << VPM_NUM_ROWS << " rows:" << logging::endl;

        std::shared_ptr<VPMArea> lastArea;
        unsigned numEmpty = 0;
        auto& stream = logging::debug() << "|";
        for(const auto& area : areas)
        {
            if(area == lastArea)
                continue;
            if(!area)
            {
                ++numEmpty;
                continue;
            }
            if(numEmpty > 0)
            {
                writeArea(stream, "", (numEmpty * outputWidth) / VPM_NUM_ROWS);
                numEmpty = 0;
            }
            lastArea = area;
            writeArea(stream, toUsageString(area->usageType, area->originalAddress),
                (area->numRows * outputWidth) / VPM_NUM_ROWS);
        }
        if(numEmpty > 0)
            writeArea(stream, "", (numEmpty * outputWidth) / VPM_NUM_ROWS);
        stream << logging::endl;
    });
}
