/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "VPM.h"

#include "../Profiler.h"
#include "../intermediate/Helper.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "log.h"

#include <atomic>
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

LCOV_EXCL_START
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
LCOV_EXCL_STOP

static uint8_t getVPMSize(uint8_t scalarBitCount)
{
    // documentation, table 32 (page 57)
    switch(scalarBitCount)
    {
    case 8:
        return 0;
    case 16:
        return 1;
    case 32:
        return 2;
    default:
        throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size",
            std::to_string(static_cast<unsigned>(scalarBitCount)));
    }
}

LCOV_EXCL_START
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
        xCoord = address & 0x3u;
        yCoord = (address >> 2u) & 0x3Fu;
        break;
    case 1: // 16 bit
        typeSize = 16;
        xCoord = address & 0x1u;
        yCoord = (address >> 1u) & 0x3Fu;
        break;
    case 2: // 32 bit
        typeSize = 32;
        yCoord = address & 0x3Fu;
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

static std::string toVPitchString(uint8_t vpitch, uint8_t mode)
{
    vpitch = demodulo(vpitch, uint8_t{16});

    if(mode >= 4) // 8-bit witdh
    {
        auto y = (vpitch >> 2) & 0x3;
        auto b = vpitch & 0x3;
        return "(" + std::to_string(y) + ", " + std::to_string(b) + ")";
    }
    if(mode >= 2) // 16-bit witdh
    {
        auto y = (vpitch >> 1) & 0x3;
        auto h = vpitch & 0x1;
        return "(" + std::to_string(y) + ", " + std::to_string(h) + ")";
    }
    return std::to_string(vpitch);
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
    return "vdw_setup(" + std::string(getHorizontal() ? "rows: " : "columns: ") +
        std::to_string(demodulo(getUnits(), uint8_t{128})) + ", " + (getHorizontal() ? "columns: " : "rows: ") +
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
    return "vdr_setup(" + std::string(getVertical() ? "columns: " : "rows: ") +
        std::to_string(demodulo(getNumberRows(), uint8_t{16})) + ", " + (getVertical() ? "rows: " : "columns: ") +
        std::to_string(demodulo(getRowLength(), uint8_t{16})) + (getVPMModeName(getMode()) + ", address: ") +
        toDMAAddressAndModeString(getAddress(), !getVertical()) +
        ", vpitch: " + toVPitchString(getVPitch(), getMode()) + ")";
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
LCOV_EXCL_STOP

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
static unsigned char getColumnDivisor(uint8_t scalarBitCount)
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
    return static_cast<unsigned char>(VPM_NUM_COLUMNS / (TYPE_INT32.getScalarBitCount() / scalarBitCount));
}

/*
 * Checks whether the given area fits into the VPM area
 */
static bool checkIndices(uint8_t scalarBitCount, unsigned char rowIndex, unsigned char columnIndex,
    unsigned char numRows, unsigned char numColumns)
{
    if((columnIndex + numColumns) > VPM_NUM_COLUMNS)
        return false;
    if((rowIndex + numRows) > VPM_NUM_ROWS)
        return false;
    if((columnIndex % getColumnDivisor(scalarBitCount)) != 0)
        return false;
    return true;
}

static uint8_t getVPMDMAMode(uint8_t scalarBitCount)
{
    // documentation, table 34 (page 58) / table 36 (page 59)
    // The offset is added initially onto the address, so don't set it (it will skip to write the first byte(s)/half
    // word)
    switch(scalarBitCount)
    {
    case 8:
        return 4; // use byte-wise addressing with offset 1 byte
    case 16:
        return 2; // use half-word wise addressing with offset of a half word
    case 32:
        return 0;
    default:
        throw CompilationError(CompilationStep::GENERAL, "Invalid parameter type-size",
            std::to_string(static_cast<unsigned>(scalarBitCount)));
    }
}

InstructionWalker periphery::insertReadDMA(
    Method& method, InstructionWalker it, const Value& dest, const Value& addr, bool useMutex)
{
    if(useMutex)
    {
        // acquire mutex
        it.emplace(new MutexLock(MutexAccess::LOCK));
        it.nextInBlock();
    }

    auto cacheEntry = std::make_shared<VPMCacheEntry>(method.vpm->getScratchArea(), dest.type);
    it = method.vpm->insertReadRAM(method, it, addr, cacheEntry);
    it = method.vpm->insertReadVPM(method, it, dest, cacheEntry);

    if(useMutex)
    {
        // free mutex
        it.emplace(new MutexLock(MutexAccess::RELEASE));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker periphery::insertWriteDMA(
    Method& method, InstructionWalker it, const Value& src, const Value& addr, bool useMutex)
{
    if(useMutex)
    {
        // acquire mutex
        it.emplace(new MutexLock(MutexAccess::LOCK));
        it.nextInBlock();
    }

    auto cacheEntry = std::make_shared<VPMCacheEntry>(method.vpm->getScratchArea(), src.type);
    it = method.vpm->insertWriteVPM(method, it, src, cacheEntry);
    it = method.vpm->insertWriteRAM(method, it, addr, cacheEntry);

    if(useMutex)
    {
        // free mutex
        it.emplace(new MutexLock(MutexAccess::RELEASE));
        it.nextInBlock();
    }
    return it;
}

std::pair<DataType, uint32_t> periphery::getBestVectorSize(uint32_t numBytes)
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
                auto numVectors = static_cast<uint32_t>(
                    numBytes / (static_cast<uint32_t>(numElements) * static_cast<uint32_t>(typeSize)));
                return std::make_pair(result, numVectors);
            }
        }
    }
    throw CompilationError(CompilationStep::GENERAL,
        "Failed to find element- and vector-sizes matching the given amount of bytes", std::to_string(numBytes));
}

InstructionWalker VPM::insertReadVPM(Method& method, InstructionWalker it, const Value& dest, const VPMArea& area,
    bool useMutex, const Value& inAreaOffset)
{
    auto cacheType = getVPMStorageType(dest.type).toVectorType(dest.type.getVectorWidth());
    auto cacheEntry = std::make_shared<VPMCacheEntry>(area, cacheType, inAreaOffset);

    it = insertLockMutex(it, useMutex);
    it = insertReadVPM(method, it, dest, cacheEntry);
    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertWriteVPM(Method& method, InstructionWalker it, const Value& src, const VPMArea& area,
    bool useMutex, const Value& inAreaOffset)
{
    auto cacheType = getVPMStorageType(src.type).toVectorType(src.type.getVectorWidth());
    auto cacheEntry = std::make_shared<VPMCacheEntry>(area, cacheType, inAreaOffset);

    it = insertLockMutex(it, useMutex);
    it = insertWriteVPM(method, it, src, cacheEntry);
    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertReadRAM(Method& method, InstructionWalker it, const Value& memoryAddress, DataType type,
    const VPMArea& area, bool useMutex, const Value& inAreaOffset, const Value& numEntries)
{
    area.checkAreaSize(getVPMStorageType(type).getLogicalWidth());

    if(auto local = memoryAddress.checkLocal())
    {
        // set the type of the parameter, if we can determine it
        if(auto param = local->as<Parameter>())
            memoryAddress.local()->as<Parameter>()->decorations =
                add_flag(param->decorations, ParameterDecorations::INPUT);
        if(auto param = local->getBase(true)->as<Parameter>())
            param->decorations = add_flag(param->decorations, ParameterDecorations::INPUT);
    }

    auto cacheEntry = std::make_shared<VPMCacheEntry>(area, type, inAreaOffset);

    it = insertLockMutex(it, useMutex);
    it = insertReadRAM(method, it, memoryAddress, cacheEntry, numEntries);
    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertWriteRAM(Method& method, InstructionWalker it, const Value& memoryAddress, DataType type,
    const VPMArea& area, bool useMutex, const Value& inAreaOffset, const Value& numEntries)
{
    area.checkAreaSize(getVPMStorageType(type).getLogicalWidth());

    if(auto local = memoryAddress.checkLocal())
    {
        // set the type of the parameter, if we can determine it
        if(auto param = local->as<Parameter>())
            memoryAddress.local()->as<Parameter>()->decorations =
                add_flag(param->decorations, ParameterDecorations::OUTPUT);
        if(auto param = local->getBase(true)->as<Parameter>())
            param->decorations = add_flag(param->decorations, ParameterDecorations::OUTPUT);
    }

    auto cacheEntry = std::make_shared<VPMCacheEntry>(area, type, inAreaOffset);

    it = insertLockMutex(it, useMutex);
    it = insertWriteRAM(method, it, memoryAddress, cacheEntry, numEntries);
    it = insertUnlockMutex(it, useMutex);
    return it;
}

NODISCARD InstructionWalker VPM::insertReadVPM(
    Method& method, InstructionWalker it, const Value& dest, const std::shared_ptr<VPMCacheEntry>& cacheEntry)
{
    if(auto data = Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
    {
        it.emplace(new CacheAccessInstruction(MemoryOperation::READ, data->lower->createReference(), cacheEntry));
        it.nextInBlock();
        it.emplace(new CacheAccessInstruction(MemoryOperation::READ, data->upper->createReference(), cacheEntry));
        it.get<CacheAccessInstruction>()->upperWord = true;
    }
    else
        it.emplace(new CacheAccessInstruction(MemoryOperation::READ, dest, cacheEntry));
    it.nextInBlock();
    return it;
}

NODISCARD InstructionWalker VPM::insertWriteVPM(
    Method& method, InstructionWalker it, const Value& src, const std::shared_ptr<VPMCacheEntry>& cacheEntry)
{
    auto convertedSource = src;
    if(src.getLiteralValue() || src.checkVector())
        // we need this, otherwise i.e. vectors directly written to the VPM are not handled correctly
        convertedSource = assign(it, src.type) = src;
    if(auto data = Local::getLocalData<MultiRegisterData>(convertedSource.checkLocal()))
    {
        it.emplace(new CacheAccessInstruction(MemoryOperation::WRITE, data->lower->createReference(), cacheEntry));
        it.nextInBlock();
        it.emplace(new CacheAccessInstruction(MemoryOperation::WRITE, data->upper->createReference(), cacheEntry));
        it.get<CacheAccessInstruction>()->upperWord = true;
    }
    else
        it.emplace(new CacheAccessInstruction(MemoryOperation::WRITE, convertedSource, cacheEntry));
    it.nextInBlock();
    return it;
}

NODISCARD InstructionWalker VPM::insertReadRAM(Method& method, InstructionWalker it, const Value& memoryAddress,
    const std::shared_ptr<VPMCacheEntry>& cacheEntry, const Value& numEntries)
{
    it.emplace(new RAMAccessInstruction(MemoryOperation::READ, memoryAddress, cacheEntry, numEntries));
    it.nextInBlock();
    return it;
}

NODISCARD InstructionWalker VPM::insertWriteRAM(Method& method, InstructionWalker it, const Value& memoryAddress,
    const std::shared_ptr<VPMCacheEntry>& cacheEntry, const Value& numEntries)
{
    it.emplace(new RAMAccessInstruction(MemoryOperation::WRITE, memoryAddress, cacheEntry, numEntries));
    it.nextInBlock();
    return it;
}

/*
 * Calculates the address of the data in the VPM in the format used by the QPU-side access (read/write VPM)
 */
static uint8_t calculateQPUSideAddress(uint8_t scalarBitCount, unsigned char rowIndex, unsigned char columnIndex)
{
    // see Broadcom spec, pages 57, 58 and figure 8 (page 54)
    // Y coord is the multiple of 16 * 32-bit (= 64 Byte)
    // B coord is the byte [0, 1, 2, 3] in the word
    // H coord is the half-word [0, 1] in the word

    // check alignment
    if(!checkIndices(scalarBitCount, rowIndex, columnIndex, 1, getColumnDivisor(scalarBitCount)))
        throw CompilationError(CompilationStep::GENERAL, "Invalid alignment in VPM for type-size",
            std::to_string(static_cast<unsigned>(scalarBitCount)));

    if(scalarBitCount == 32)
        //"ADDR[5:0] = Y[5:0]"
        return static_cast<uint8_t>(rowIndex);
    else if(scalarBitCount == 16)
        // "ADDR[6:0] = Y[5:0] | H[0]"
        return static_cast<uint8_t>(rowIndex << 1) |
            static_cast<uint8_t>(columnIndex / getColumnDivisor(scalarBitCount));
    else if(scalarBitCount == 8)
        // "ADDR[7:0] = Y[5:0] | B[1:0]"
        return static_cast<uint8_t>(rowIndex << 2) |
            static_cast<uint8_t>(columnIndex / getColumnDivisor(scalarBitCount));
    else
        throw CompilationError(CompilationStep::GENERAL, "Invalid bit-width to store in VPM",
            std::to_string(static_cast<unsigned>(scalarBitCount)));
}

static NODISCARD InstructionWalker calculateElementOffsetInVPM(Method& method, InstructionWalker it,
    DataType scalarType, const Value& vectorWidth, const Value& inAreaOffset, Value& elementOffset, bool dontPack)
{
    // e.g. long4 type, 64 byte offset -> 4 32-bit element offset
    // e.g. 32-bit type, 4 byte offset -> 1 32-bit element offset
    // e.g. byte4 type, 4 byte offset -> 1 byte-element offset
    // e.g. half-word8 type, 32 byte offset -> 2 half-word element offset
    if(inAreaOffset == INT_ZERO)
        elementOffset = INT_ZERO;
    else if(scalarType.getScalarBitCount() > 32)
    {
        auto inMemorySize = assign(it, TYPE_INT8, "%memory_size") =
            vectorWidth * Literal(TYPE_INT32.getScalarBitCount() / 8);
        // NOTE: This only works for in-memory-size is a power of 2!
        auto logMemorySize = method.addNewLocal(TYPE_INT8, "%memory_size");
        it.emplace(new Operation(OP_CLZ, logMemorySize, inMemorySize));
        it.nextInBlock();
        logMemorySize = assign(it, TYPE_INT8, "%memory_size") = (31_val - logMemorySize);
        elementOffset = assign(it, TYPE_INT16, "%vpm_element_offset") = as_unsigned{inAreaOffset} >> logMemorySize;
    }
    else
    {
        auto inMemorySize = assign(it, TYPE_INT8, "%memory_size") =
            vectorWidth * Literal(scalarType.getScalarBitCount() / 8 + (scalarType.getScalarBitCount() % 8 != 0));
        // NOTE: This only works for in-memory-size is a power of 2!
        auto logMemorySize = method.addNewLocal(TYPE_INT8, "%memory_size");
        it.emplace(new Operation(OP_CLZ, logMemorySize, inMemorySize));
        it.nextInBlock();
        logMemorySize = assign(it, TYPE_INT8, "%memory_size") = (31_val - logMemorySize);
        elementOffset = assign(it, TYPE_INT16, "%vpm_element_offset") = as_unsigned{inAreaOffset} >> logMemorySize;
    }

    if(scalarType.getScalarBitCount() < 32 && dontPack)
        // If we disallow packing, make sure all addresses are aligned to a row. Thus, we make sure the address is a
        // multiple of 4(2) for 8-bit (16-bit) types.
        // inAreaOffset = I * sizeof(<element-type * N>)
        // (initial) elementOffset = inAreaOffset / sizeof(<element-type * N>) = I
        // For 8-bit types: (final) elementOffset = I * 4
        // For 16-bit types: (final) elementOffset = I * 2
        elementOffset = assign(it, TYPE_INT16, "%vpm_element_offset") =
            elementOffset * Literal(TYPE_INT32.getScalarBitCount() / scalarType.getScalarBitCount());
    return it;
}

/**
 * The checks are as follows:
 * - in-VPM type is 16-element vector version of "original" type (dest.type)
 * - if "original" type is scalar, every scalar is in 0th element of its own 16-element vector, so just load
 * the vector (fast case, simple divide offset by element size)
 * - if "original" type is vector (of size N) and offset is multiple of that vector size, every entry is the first N
 * elements of its own 16-element vector, so just calculate the offset of the whole entry (fast case, simple divide
 * offset by element size)
 * - otherwise (e.g. offset unknown), we need to assume "unaligned" (not a multiple of the element size) access, so need
 * to access multiple elements and rotate/combine the results
 */
static bool isUnalignedMemoryVPMAccess(const Value& offset, DataType elementType)
{
    return elementType.getVectorWidth() > 1 &&
        (!offset.getLiteralValue() || (offset.getLiteralValue()->unsignedInt() % elementType.getInMemoryWidth()) != 0);
}

InstructionWalker VPM::insertCopyRAM(Method& method, InstructionWalker it, const Value& destAddress,
    const Value& srcAddress, const unsigned numBytes, bool useMutex)
{
    const auto size = getBestVectorSize(numBytes);

    it = insertLockMutex(it, useMutex);

    // TODO use insertReadRAM/insertWriteRAM with multiple entries??
    auto cacheEntry = std::make_shared<VPMCacheEntry>(getScratchArea(), size.first);
    it = insertReadRAM(method, it, srcAddress, cacheEntry);
    it = insertWriteRAM(method, it, destAddress, cacheEntry);

    for(unsigned i = 1; i < size.second; ++i)
    {
        // increment offset from base address
        Value tmpSource = assign(it, srcAddress.type, "%mem_copy_addr") =
            srcAddress + Value(Literal(i * size.first.getInMemoryWidth()), TYPE_INT8);
        Value tmpDest = assign(it, destAddress.type, "%mem_copy_addr") =
            destAddress + Value(Literal(i * size.first.getInMemoryWidth()), TYPE_INT8);

        if(auto data = Local::getLocalData<ReferenceData>(srcAddress.checkLocal()))
            tmpSource.local()->set(ReferenceData(*data->base, ANY_ELEMENT));
        if(auto data = Local::getLocalData<ReferenceData>(destAddress.checkLocal()))
            tmpDest.local()->set(ReferenceData(*data->base, ANY_ELEMENT));

        it = insertReadRAM(method, it, tmpSource, cacheEntry);
        it = insertWriteRAM(method, it, tmpDest, cacheEntry);
    }
    it = insertUnlockMutex(it, useMutex);

    return it;
}

InstructionWalker VPM::insertCopyRAMDynamic(Method& method, InstructionWalker it, const Value& destAddress,
    const Value& srcAddress, const Value& numEntries, bool useMutex)
{
    it = insertLockMutex(it, useMutex);

    // count from maximum to 0 (exclusive)
    auto counter = assign(it, numEntries.type, "%remaining_iterations") =
        (numEntries, InstructionDecorations::PHI_NODE);
    auto& block = intermediate::insertLoop(method, it, counter, "dynamic_dma_copy");
    {
        // inside the loop, a single iteration
        auto inLoopIt = block.walk().nextInBlock();
        auto elementType = destAddress.type.getElementType();
        auto index = assign(inLoopIt, counter.type) = numEntries - counter;
        // XXX does not support more than 2^23 elements
        auto offset = assign(inLoopIt, counter.type) =
            mul24(index, Value(Literal(elementType.getInMemoryWidth()), TYPE_INT32));

        // increment offset from base address
        Value tmpSource = assign(inLoopIt, srcAddress.type, "%mem_copy_addr") = srcAddress + offset;
        Value tmpDest = assign(inLoopIt, destAddress.type, "%mem_copy_addr") = destAddress + offset;

        if(auto local = srcAddress.checkLocal())
        {
            // set the type of the parameter, if we can determine it
            if(auto param = local->as<Parameter>())
                srcAddress.local()->as<Parameter>()->decorations =
                    add_flag(param->decorations, ParameterDecorations::INPUT);
            if(auto param = local->getBase(true)->as<Parameter>())
                param->decorations = add_flag(param->decorations, ParameterDecorations::INPUT);
            if(auto data = local->get<ReferenceData>())
                tmpSource.local()->set(ReferenceData(*data->base, ANY_ELEMENT));
        }
        if(auto local = destAddress.checkLocal())
        {
            // set the type of the parameter, if we can determine it
            if(auto param = local->as<Parameter>())
                destAddress.local()->as<Parameter>()->decorations =
                    add_flag(param->decorations, ParameterDecorations::OUTPUT);
            if(auto param = local->getBase(true)->as<Parameter>())
                param->decorations = add_flag(param->decorations, ParameterDecorations::OUTPUT);
            if(auto data = local->get<ReferenceData>())
                tmpDest.local()->set(ReferenceData(*data->base, ANY_ELEMENT));
        }

        auto cacheEntry = std::make_shared<VPMCacheEntry>(getScratchArea(), elementType);

        inLoopIt = insertReadRAM(method, inLoopIt, tmpSource, cacheEntry);
        inLoopIt = insertWriteRAM(method, inLoopIt, tmpDest, cacheEntry);

        // decrement remaining iterations counter
        assign(inLoopIt, counter) = (counter - INT_ONE, InstructionDecorations::PHI_NODE);
    }

    it.nextInBlock();
    it = insertUnlockMutex(it, useMutex);
    return it;
}

InstructionWalker VPM::insertFillDMA(Method& method, InstructionWalker it, const Value& memoryAddress,
    const Value& source, const Value& numCopies, bool useMutex)
{
    if(numCopies.getLiteralValue() == 0_lit)
        return it;

    auto cacheEntry = std::make_shared<VPMCacheEntry>(getScratchArea(), source.type);

    it = insertLockMutex(it, useMutex);
    it = insertWriteVPM(method, it, source, cacheEntry);
    it = insertWriteRAM(method, it, memoryAddress, cacheEntry);

    if(auto literalCount = (numCopies.getConstantValue() & &Value::getLiteralValue))
    {
        for(unsigned i = 1; i < literalCount->unsignedInt(); ++i)
        {
            // increment offset from base address
            Value tmpDest = assign(it, memoryAddress.type, "%mem_fill_addr") =
                memoryAddress + Value(Literal(i * source.type.getInMemoryWidth()), TYPE_INT8);
            it = insertWriteRAM(method, it, tmpDest, cacheEntry);
        }
    }
    else
    {
        // count from maximum to 0 (exclusive)
        auto counter = assign(it, numCopies.type, "%remaining_iterations") =
            (numCopies, InstructionDecorations::PHI_NODE);
        auto& block = intermediate::insertLoop(method, it, counter, "dynamic_dma_fill");
        {
            // inside the loop, a single iteration
            auto inLoopIt = block.walk().nextInBlock();
            auto index = assign(inLoopIt, counter.type) = numCopies - counter;
            // XXX does not support more than 2^23 elements
            auto offset = assign(inLoopIt, counter.type) =
                mul24(index, Value(Literal(source.type.getInMemoryWidth()), TYPE_INT32));

            // increment offset from base address
            Value tmpDest = assign(inLoopIt, memoryAddress.type, "%mem_fill_addr") = memoryAddress + offset;

            inLoopIt = insertWriteRAM(method, inLoopIt, tmpDest, cacheEntry);

            // decrement remaining iterations counter
            assign(inLoopIt, counter) = (counter - INT_ONE, InstructionDecorations::PHI_NODE);
        }

        it.nextInBlock();
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
    case VPMUsage::RAM_CACHE:
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
    return usageType == VPMUsage::SCRATCH || usageType == VPMUsage::RAM_CACHE ||
        getElementType().getVectorWidth() == NATIVE_VECTOR_SIZE;
}

bool VPMArea::canBePackedIntoRow() const
{
    // TODO proper calculation (or pass in constructor!)
    // FIXME if we allow packing, QPU->VPM->RAM (simple) works, but RAM-> (single) VPM-> (all) RAM does not! If we
    // disallow it, other way round
    // FIXME also uses path for unaligned memory access which has a lot of overhead!
    // return /* !canBeAccessedViaDMA() || */ getElementType().getVectorWidth() == NATIVE_VECTOR_SIZE &&
    //     usageType != VPMUsage::RAM_CACHE;
    return false;
}

static DataType simplifyComplexTypes(DataType type)
{
    if(auto arrayType = type.getArrayType())
        // treat single-element array as the single element
        return arrayType->size == 1 ? arrayType->elementType : type;
    if(auto structType = type.getStructType())
        // treat single-element struct as the single element
        return structType->elementTypes.size() == 1 ? structType->elementTypes.front() : type;
    if(auto ptrType = type.getPointerType())
        // treat pointer as "normal" integer
        return TYPE_INT32;
    return type;
}

/*
 * For 64-bit vector writes, the vector of lower words and the vector of upper words are stored in two consecutive
 * VPM rows.
 * When writing to RAM, we need to interleave the two rows to alternately write lower and upper word for each
 * element.
 *
 * VPM:
 * [elem 0 low, elem 1 low, ..., elem N low]
 * [elem 0 up , elem 1 up , ..., elem N up ]
 *
 * RAM:
 * [elem 0 low, elem0 up, elem 1 low, elem 1 up, ..., elem N low, elem N up]
 *
 * This can be achieved by using Vertical mode with a Depth of 2 (rows) and N Units (where N is the vector-width).
 *
 * FIXME 64-bit type are now stored laned while the rest is packed, this makes pointer reinterpretation to
 * different-sized types impossible!
 */

/**
 * Generates a QPU-to-VPM write setup for accessing the base-address of this VPM area for elements of the
 * given data-type.
 *
 * If the data-type is set to unknown, the element-type of the local associated with this area is used
 */
NODISCARD static VPWGenericSetup toWriteSetup(const VPMArea& area, DataType scalarType)
{
    if(scalarType.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", scalarType.to_string());

    uint8_t scalarBitCount = scalarType.getScalarBitCount();

    // 64-bit integer vectors are stored as 2 rows of 32-bit integer vectors in VPM
    scalarBitCount = std::min(scalarBitCount, uint8_t{32});

    // if we can pack into a single row, do so. Otherwise set stride to beginning of next row
    const uint8_t stride =
        area.canBePackedIntoRow() ? 1 : static_cast<uint8_t>(TYPE_INT32.getScalarBitCount() / scalarBitCount);
    VPWGenericSetup setup(
        getVPMSize(scalarBitCount), stride, calculateQPUSideAddress(scalarBitCount, area.rowOffset, 0));
    setup.setHorizontal(IS_HORIZONTAL);
    setup.setLaned(!IS_PACKED);
    return setup;
}

/**
 * Generates a VPM-to-RAM DMA write setup for storing the contents of the VPM area into RAM with the given
 * element-type and number of rows of the given type.
 *
 * If the data-type is set to unknown, the element-type of the local associated with this area is used
 */
NODISCARD static InstructionWalker insertWriteDMASetup(InstructionWalker it, Value& setup, const VPMArea& area,
    DataType scalarType, const Value& vectorWidth, Value numRows)
{
    if(scalarType.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", scalarType.to_string());

    auto is64BitType = scalarType.getScalarBitCount() == 64;
    if(scalarType.getScalarBitCount() > 32)
    {
        // 64-bit integer vectors are stored as 2 rows of 32-bit integer vectors in VPM
        scalarType = DataType{32, scalarType.getVectorWidth(), scalarType.isFloatingType()};
        if(!numRows.hasLiteral(1_lit))
            // TODO we need to issue multiple setups, since we need to interleave the Nth 32-bit element of every other
            // row after Nth element of the row before, instead of one row after the other or one column after the other
            throw CompilationError(
                CompilationStep::GENERAL, "Writing multiple rows of 64-bit vectors into DMA is not supported yet");
        numRows = 2_val;
    }

    // by "default", one value per row, so we need to store the number of values as number of rows
    auto rowDepth = vectorWidth;
    if(area.canBePackedIntoRow())
    {
        // if we have the row packed, we need to calculate the row-width from the maximum row-width and the number of
        // elements
        auto totalNumElements = assign(it, vectorWidth.type, "%vpm_num_elements") = mul24(vectorWidth, numRows);
        const uint8_t elementsPerRow = area.getElementsInRow(scalarType);
        auto needsMoreRows = assignNop(it) =
            as_signed{totalNumElements} > as_signed{Value(Literal(elementsPerRow), TYPE_INT8)};
        rowDepth = assign(it, TYPE_INT8, "%vpm_dma_depth") = (Value(Literal(elementsPerRow), TYPE_INT8), needsMoreRows);
        numRows = assign(it, TYPE_INT8, "%vpm_dma_rows") = (totalNumElements / Literal(elementsPerRow), needsMoreRows);
        assign(it, rowDepth) = (totalNumElements, needsMoreRows.invert());
    }

    auto toStaticValue = [](const Value& val) -> uint8_t {
        return static_cast<uint8_t>((val.getConstantValue() & &Value::getLiteralValue).value_or(0_lit).unsignedInt());
    };

    auto depth = getSourceValue(rowDepth);
    depth = depth.getConstantValue().value_or(depth);
    auto units = getSourceValue(numRows);
    units = units.getConstantValue().value_or(units);
    auto horizontal = IS_HORIZONTAL;
    if(is64BitType)
    {
        // invert Depth and Units, since we also read from VPM vertical
        std::swap(depth, units);
        horizontal = !IS_HORIZONTAL;
    }
    VPWDMASetup setupBits(getVPMDMAMode(scalarType.getScalarBitCount()), toStaticValue(depth) % 128 /* 0 => 128 */,
        toStaticValue(units) % 128 /* 0 => 128 */);
    setupBits.setHorizontal(horizontal);
    setupBits.setWordRow(area.rowOffset);

    setup = Value(Literal(VPWSetup(setupBits).value), TYPE_INT32);

    // handle dynamic depths/units
    if(!depth.getConstantValue())
    {
        // TODO truncates any value more than 128, is this a problem?
        auto dynamicDepth = assign(it, TYPE_INT8, "%vpm_dma_depth") = (depth % 128_lit);
        dynamicDepth = assign(it, TYPE_INT32, "%vpm_dma_depth") = (dynamicDepth << 16_val);
        setup = assign(it, TYPE_INT32, "%vpm_dma_setup") = setup | dynamicDepth;
    }
    if(!units.getConstantValue())
    {
        // TODO truncates any value more than 128, is this a problem?
        auto dynamicUnits = assign(it, TYPE_INT8, "%vpm_dma_units") = (units % 128_lit);
        dynamicUnits = assign(it, TYPE_INT32, "%vpm_dma_units") = (dynamicUnits << 23_val);
        setup = assign(it, TYPE_INT32, "%vpm_dma_setup") = setup | dynamicUnits;
    }

    return it;
}

/**
 * Generates a VPM-to-QPU read setup for accessing the base-address of this VPM area for the given number of
 * rows of the given data-type.
 *
 * If the data-type is set to unknown, the default element-type of this area is used
 */
NODISCARD static VPRGenericSetup toReadSetup(const VPMArea& area, DataType scalarType, uint8_t numRows = 1)
{
    if(scalarType.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", scalarType.to_string());

    uint8_t scalarBitCount = scalarType.getScalarBitCount();

    if(scalarBitCount > 32)
    {
        // 64-bit integer vectors are stored as 2 rows of 32-bit integer vectors in VPM
        scalarBitCount = 32;
        numRows = 2 * numRows;
    }

    // if we can pack into a single row, do so. Otherwise set stride to beginning of next row
    const uint8_t stride =
        area.canBePackedIntoRow() ? 1 : static_cast<uint8_t>(TYPE_INT32.getScalarBitCount() / scalarBitCount);
    VPRGenericSetup setup(
        getVPMSize(scalarBitCount), stride, numRows, calculateQPUSideAddress(scalarBitCount, area.rowOffset, 0));
    setup.setHorizontal(IS_HORIZONTAL);
    setup.setLaned(!IS_PACKED);
    return setup;
}

/**
 * Generates a RAM-to-VPM DMA read setup for loading the contents of a memory address into this VPM area
 * given the element-type and number of rows of the given type.
 *
 * If the data-type is set to unknown, the default element-type of this area is used
 */
NODISCARD static InstructionWalker insertReadDMASetup(InstructionWalker it, Value& setup, const VPMArea& area,
    DataType scalarType, const Value& vectorWidth, Value numRows)
{
    if(scalarType.isUnknown())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot generate VPW setup for unknown type", scalarType.to_string());

    auto is64BitType = scalarType.getScalarBitCount() == 64;
    if(scalarType.getScalarBitCount() > 32)
    {
        // 64-bit integer vectors are stored as 2 rows of 32-bit integer vectors in VPM
        scalarType = DataType{32, scalarType.getVectorWidth(), scalarType.isFloatingType()};
        if(!numRows.hasLiteral(1_lit))
            // TODO we need to issue multiple setups (unless vector-width is 16 or long vectors are packed, in which
            // case the Depth can just be set to vector-width * numRows)
            throw CompilationError(
                CompilationStep::GENERAL, "Writing multiple rows of 64-bit vectors into DMA is not supported yet");
        numRows = 2_val;
    }

    auto toStaticValue = [](const Value& val) -> uint8_t {
        return static_cast<uint8_t>((val.getConstantValue() & &Value::getLiteralValue).value_or(0_lit).unsignedInt());
    };

    // If the data is packed, have a pitch of 1 unit (e.g. 1 byte/half-word/word offset depending on type)
    // otherwise, always jump to the next row
    const uint8_t vpmPitch =
        area.canBePackedIntoRow() ? 1 : TYPE_INT32.getScalarBitCount() / scalarType.getScalarBitCount();

    auto rowLength = getSourceValue(vectorWidth);
    rowLength = rowLength.getConstantValue().value_or(rowLength);
    numRows = getSourceValue(numRows);
    numRows = numRows.getConstantValue().value_or(numRows);
    bool vertical = !IS_HORIZONTAL;
    if(is64BitType)
    {
        // invert length and number of rows, since we also write to VPM vertical
        std::swap(rowLength, numRows);
        vertical = IS_HORIZONTAL;
    }
    VPRDMASetup setupBits(getVPMDMAMode(scalarType.getScalarBitCount()), toStaticValue(rowLength) % 16 /* 0 => 16 */,
        toStaticValue(numRows) % 16 /* 0 => 16 */, vpmPitch % 16 /* 0 => 16 */);
    setupBits.setVertical(vertical);
    setupBits.setWordRow(area.rowOffset);

    setup = Value(Literal(VPRSetup(setupBits).value), TYPE_INT32);

    // handle dynamic row length/number
    if(!rowLength.getLiteralValue())
    {
        // TODO truncates any value more than 16, is this a problem?
        auto dynamicRowLength = assign(it, TYPE_INT8, "%vpm_dma_row_length") = (rowLength % 16_lit);
        dynamicRowLength = assign(it, TYPE_INT32, "%vpm_dma_row_length") = (dynamicRowLength << 20_val);
        setup = assign(it, TYPE_INT32, "%vpm_dma_setup") = setup | dynamicRowLength;
    }
    if(!numRows.getLiteralValue())
    {
        // TODO truncates any value more than 16, is this a problem?
        auto dynamicNumRows = assign(it, TYPE_INT8, "%vpm_dma_num_rows") = (numRows % 16_lit);
        dynamicNumRows = assign(it, TYPE_INT32, "%vpm_dma_num_rows") = (dynamicNumRows << 16_val);
        setup = assign(it, TYPE_INT32, "%vpm_dma_setup") = setup | dynamicNumRows;
    }

    return it;
}

LCOV_EXCL_START
static std::string toUsageString(VPMUsage usage, const Local* local)
{
    switch(usage)
    {
    case VPMUsage::RAM_CACHE:
        return (local ? local->to_string() : "(nullptr)") + " (cached)";
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
LCOV_EXCL_STOP

// running counter, for visual distinction of the VPM cache entries only
static std::atomic_uint vpmCacheEntryCounter{0};

VPMCacheEntry::VPMCacheEntry(const VPMArea& area, DataType type, const Value& innerOffset) :
    index(vpmCacheEntryCounter++), area(area), inAreaOffset(innerOffset), elementType(type),
    dynamicVectorWidth(Value(Literal(type.getVectorWidth()), TYPE_INT8))
{
    area.checkAreaSize(type.getLogicalWidth());
}

VPMCacheEntry::~VPMCacheEntry() noexcept = default;

LCOV_EXCL_START
std::string VPMCacheEntry::to_string() const
{
    return "VPM cache entry " + std::to_string(index) + " (" + elementType.to_string() +
        (!dynamicVectorWidth.isUndefined() ? " with " + dynamicVectorWidth.to_string() + " elements" : "") +
        " and offset " + inAreaOffset.to_string() + " to base " + area.to_string() + ")";
}
LCOV_EXCL_STOP

DataType VPMCacheEntry::getScalarType() const
{
    if(elementType.isUnknown())
        return simplifyComplexTypes(area.getElementType()).getElementType();
    return simplifyComplexTypes(elementType).getElementType();
}

Value VPMCacheEntry::getVectorWidth() const
{
    if(dynamicVectorWidth.isUndefined())
        return Value(Literal(elementType.getVectorWidth()), TYPE_INT8);
    return dynamicVectorWidth;
}

DataType VPMCacheEntry::getVectorType() const
{
    if(auto lit = getVectorWidth().getLiteralValue())
        return getScalarType().toVectorType(static_cast<uint8_t>(lit->unsignedInt()));
    // dynamic size -> use maximum size
    return getScalarType().toVectorType(NATIVE_VECTOR_SIZE);
}

void VPMCacheEntry::setStaticElementCount(uint8_t numElements)
{
    elementType = elementType.toVectorType(numElements);
    dynamicVectorWidth = Value(Literal(numElements), TYPE_INT8);
}

void VPMCacheEntry::setDynamicElementCount(const Value& numElements)
{
    elementType = elementType.toVectorType(1);
    dynamicVectorWidth = numElements;
}

VPM::VPM(const unsigned totalVPMSize) : maximumVPMSize(std::min(VPM_DEFAULT_SIZE, totalVPMSize)), areas(VPM_NUM_ROWS)
{
    // set a size of at least 2 row (for 64-bit data), so if no scratch is used, the first area has an offset of != 0
    // and therefore is different than the scratch-area
    auto scratch = std::make_shared<VPMArea>(VPMUsage::SCRATCH, 0, 2, nullptr);
    areas[0] = areas[1] = scratch;
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

const VPMArea* VPM::addArea(
    const Local* local, DataType elementType, bool isStackArea, bool isMemoryCache, unsigned numStacks)
{
    // Since we can only read/write in packages of 16-element vectors on the QPU-side, we need to reserve enough space
    // for 16-element vectors (even if we do not use all of the elements)
    DataType inVPMType = getVPMStorageType(elementType);

    unsigned requestedSize = inVPMType.getLogicalWidth() * (isStackArea ? numStacks : 1);
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
    auto ptr = std::make_shared<VPMArea>(
        isStackArea ? VPMUsage::STACK : (isMemoryCache ? VPMUsage::RAM_CACHE : VPMUsage::LOCAL_MEMORY),
        static_cast<uint8_t>(rowOffset.value()), numRows, local);
    for(auto i = rowOffset.value(); i < (rowOffset.value() + numRows); ++i)
        areas[i] = ptr;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Allocating " << numRows << " rows (per 64 byte) of VPM cache starting at row " << rowOffset.value()
            << " for local: " << local->to_string(false)
            << (isStackArea ? std::string(" (") + std::to_string(numStacks) + " stacks)" : "") << logging::endl);
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

DataType VPM::getVPMStorageType(DataType elemenType)
{
    DataType inVPMType = TYPE_UNKNOWN;
    if(auto arrayType = elemenType.getArrayType())
    {
        // e.g. short2[17] -> short16[17]
        // also int4[1][2] -> int16[1][2]
        inVPMType = getVPMStorageType(arrayType->elementType).toArrayType(arrayType->size);
    }
    else if(elemenType.getPointerType())
        // e.g. int* -> int16
        inVPMType = TYPE_INT32.toVectorType(16);
    else if(!elemenType.isSimpleType())
        throw CompilationError(
            CompilationStep::GENERAL, "Unhandled element-type to cache in VPM", elemenType.to_string());
    else
        // e.g. char3 -> char16
        inVPMType = elemenType.toVectorType(16);
    return inVPMType;
}

LCOV_EXCL_START
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
LCOV_EXCL_STOP

NODISCARD static InstructionWalker lowerReadVPM(
    Method& method, InstructionWalker it, const CacheAccessInstruction& access, const VPMCacheEntry& cacheEntry)
{
    // try to get constant offset value
    auto internalOffset = getSourceValue(cacheEntry.inAreaOffset);
    internalOffset = internalOffset.getConstantValue(true).value_or(internalOffset);

    // 1) configure reading from VPM into QPU
    const auto& dataType = cacheEntry.getVectorType();
    VPRSetup genericSetup(toReadSetup(cacheEntry.area, cacheEntry.getScalarType()));
    if(dataType.getScalarBitCount() == 64)
    {
        // only read a single row, since we insert separate setups for both words
        // FIXME this is way less efficient then loading both rows at once!
        genericSetup.genericSetup.setNumber(genericSetup.genericSetup.getNumber() / 2);
        if(access.upperWord)
            genericSetup.genericSetup.setWordRow(genericSetup.genericSetup.getWordRow() + 1u);
    }
    Value outputValue = VPM_IO_REGISTER;
    if(internalOffset == INT_ZERO)
    {
        it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(genericSetup.value)));
        it->addDecorations(InstructionDecorations::VPM_READ_CONFIGURATION);
        it.nextInBlock();
    }
    else if(isUnalignedMemoryVPMAccess(internalOffset, dataType))
    {
        // TODO make sure this block is only used where really really required!
        // TODO if inAreaOffset guaranteed to lie within one row, skip loading of second?!
        // TODO 64-bit version
        /*
         * In OpenCL, vectors are aligned to the alignment of the element type (e.g. a char16 vector is char aligned).
         * More accurately: they can be loaded/stored from any address aligned to the element type!
         * This is at least true when loaded via vloadN.
         * Since loading vectors from VPM is always aligned to the whole vector (actually the whole 16-element vector),
         * we need to do some manual post-processing to assemble a correct result vector.
         *
         * Goal: Load a N-element vector V from row X with an offset of I bytes, where I is aligned to the element type
         * of V.
         * - Calculate base VPM address: B = X + I / sizeof(V)
         * - Load 2 <E16> (16-element vectors of correct element type E) vectors L and U from VPM address B and B + 1
         * - Calculate rotation factor: R = (I % sizeof(V)) / sizeof(E)
         * - Rotate result of first load downwards by R elements: L = rotate(L, R)
         * - Rotate result of second load upwards by R elements: U = rotate(U, R)
         * - Copy elements [0, N-R] from the first vector L into the result
         * - Copy elements [N-R, N] from the second vector U into the result
         */
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffsetInVPM(method, it, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(),
            internalOffset, elementOffset, !cacheEntry.area.canBePackedIntoRow());
        genericSetup.genericSetup.setNumber(2);
        assign(it, VPM_IN_SETUP_REGISTER) = (Value(Literal(genericSetup.value), TYPE_INT32) + elementOffset,
            InstructionDecorations::VPM_READ_CONFIGURATION);
        // Example:
        // VPM has (address B) 0, 1, 2, 3, 4, 5, 6, 7, ..., (address B + 1) 16, 17, 18, 19, 20, ..., 31
        // Vector size N = 16, rotation factor R = 11
        // L = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
        auto lowerPart = assign(it, dataType, "%vpm.unaligned.lower") = VPM_IO_REGISTER;
        // U = [16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]
        auto upperPart = assign(it, dataType, "%vpm.unaligned.upper") = VPM_IO_REGISTER;
        auto rotationOffset = assign(it, TYPE_INT8) = internalOffset % Literal(dataType.getInMemoryWidth());
        rotationOffset = assign(it, TYPE_INT8, "%vpm.unaligned.offset") =
            rotationOffset / Literal(dataType.getElementType().getInMemoryWidth());
        Value lowerRotated = method.addNewLocal(dataType, "%vpm.unaligned.lower");
        Value upperRotated = method.addNewLocal(dataType, "%vpm.unaligned.upper");
        // L = [11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        it = insertVectorRotation(it, lowerPart, rotationOffset, lowerRotated, Direction::DOWN);
        // V = [11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        outputValue = assign(it, dataType, "%vpm.unaligned.result") = lowerRotated;
        // U = [27, 28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]
        it = insertVectorRotation(it, upperPart, rotationOffset, upperRotated, Direction::DOWN);
        Value rotationOffsetReplicated = method.addNewLocal(TYPE_INT8.toVectorType(16));
        it = insertReplication(it, rotationOffset, rotationOffsetReplicated);
        // N-R = 5
        auto selectionOffset = assign(it, TYPE_INT8.toVectorType(16)) =
            cacheEntry.getVectorWidth() - rotationOffsetReplicated;
        // flags = [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        assign(it, NOP_REGISTER) = (ELEMENT_NUMBER_REGISTER - selectionOffset, SetFlag::SET_FLAGS);
        // V = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]
        assign(it, outputValue) = (upperRotated, COND_NEGATIVE_CLEAR);
    }
    else
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffsetInVPM(method, it, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(),
            internalOffset, elementOffset, !cacheEntry.area.canBePackedIntoRow());
        // 2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
        // 3) write setup with dynamic address
        assign(it, VPM_IN_SETUP_REGISTER) = (Value(Literal(genericSetup.value), TYPE_INT32) + elementOffset,
            InstructionDecorations::VPM_READ_CONFIGURATION);
    }
    // 2) read value from VPM
    assign(it, access.getData()) = outputValue;
    it.erase();
    return it;
}

NODISCARD static InstructionWalker lowerWriteVPM(
    Method& method, InstructionWalker it, const CacheAccessInstruction& access, const VPMCacheEntry& cacheEntry)
{
    // try to get constant offset value
    auto internalOffset = getSourceValue(cacheEntry.inAreaOffset);
    internalOffset = internalOffset.getConstantValue(true).value_or(internalOffset);

    // 1. configure writing from QPU into VPM
    const auto& dataType = cacheEntry.getVectorType();
    VPWSetup genericSetup(toWriteSetup(cacheEntry.area, cacheEntry.getScalarType()));
    if(dataType.getScalarBitCount() == 64)
    {
        if(access.upperWord)
            genericSetup.genericSetup.setWordRow(genericSetup.genericSetup.getWordRow() + 1u);
    }
    if(internalOffset == INT_ZERO)
    {
        it.emplace(new LoadImmediate(VPM_OUT_SETUP_REGISTER, Literal(genericSetup.value)));
        it->addDecorations(InstructionDecorations::VPM_WRITE_CONFIGURATION);
        it.nextInBlock();
    }
    else if(isUnalignedMemoryVPMAccess(internalOffset, dataType))
    {
        // TODO make sure this block is only used where really really required!
        // TODO if inAreaOffset guaranteed to lie within one row, skip loading of second?!
        // TODO 64-bit version
        /*
         * In OpenCL, vectors are aligned to the alignment of the element type (e.g. a char16 vector is char
         * aligned). More accurately: they can be loaded/stored from any address aligned to the element type! This
         * is at least true when stored via vstoreN. Since storing vectors into VPM is always aligned to the whole
         * vector (actually the whole 16-element vector), we need to do some manual post-processing to assemble a
         * correct result vector. For storing, we actually need to load the old value, insert the changed data at
         * the given position and store the new value back to not overwrite valid data (since we always write 16
         * elements, even if we don't want to).
         *
         * Goal: Store a N-element vector V into row X with an offset of I bytes, where I is aligned to the element
         * type of V.
         * - Calculate base VPM address: B = X + I / sizeof(V)
         * - Load 2 <E16> (16-element vectors of correct element type E) vectors L and U from VPM address B and B +
         * 1
         * - Calculate rotation factor: R = (I % sizeof(V)) / sizeof(E)
         * - Rotate new data vector V upwards by R elements: V = rotate(V, R)
         * - Copy elements [N-R, N] from the rotated new data vector V into first vector L
         * - Copy elements [0, N-R] from the rotated new data vector V into first vector U
         * - Write 2 <E16> vectors L and U into VPM address B and B + 1
         */
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffsetInVPM(method, it, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(),
            internalOffset, elementOffset, !cacheEntry.area.canBePackedIntoRow());
        {
            VPRSetup genericReadSetup(toReadSetup(cacheEntry.area, cacheEntry.getScalarType()));
            // XXX this might lead to reading 64th (as in one-past-the-end) VPM row, if the original addressed row is
            // the 63th (last row) while reading the second row. This should not be a problem, since the address wraps
            // at 64 rows, thus we read the 0th row, but never actually access its data.
            genericReadSetup.genericSetup.setNumber(2);
            assign(it, VPM_IN_SETUP_REGISTER) = (Value(Literal(genericReadSetup.value), TYPE_INT32) + elementOffset,
                InstructionDecorations::VPM_READ_CONFIGURATION);
        }
        // Example:
        // VPM has (address B) 0, 1, 2, 3, 4, 5, 6, 7, ..., (address B + 1) 16, 17, 18, 19, 20, ..., 31
        // Vector size N = 16, rotation factor R = 11
        // Vector data V = [40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55]
        // L = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
        auto lowerPart = assign(it, dataType, "%vpm.unaligned.orig.lower") = VPM_IO_REGISTER;
        // U = [16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]
        auto upperPart = assign(it, dataType, "%vpm.unaligned.orig.upper") = VPM_IO_REGISTER;
        auto rotationOffset = assign(it, TYPE_INT8) = internalOffset % Literal(dataType.getInMemoryWidth());
        rotationOffset = assign(it, TYPE_INT8, "%vpm.unaligned.offset") =
            rotationOffset / Literal(dataType.getElementType().getInMemoryWidth());
        Value srcRotated = method.addNewLocal(dataType, "%vpm.unaligned.new");
        // V = [45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44]
        it = insertVectorRotation(it, access.getData(), rotationOffset, srcRotated, Direction::UP);
        Value rotationOffsetReplicated = method.addNewLocal(TYPE_INT8.toVectorType(16));
        it = insertReplication(it, rotationOffset, rotationOffsetReplicated);
        rotationOffsetReplicated = assign(it, rotationOffsetReplicated.type) = rotationOffsetReplicated - 1_val;
        // flags = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5]
        assign(it, NOP_REGISTER) = (rotationOffsetReplicated - ELEMENT_NUMBER_REGISTER, SetFlag::SET_FLAGS);
        // L = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 40, 41, 42, 43, 44]
        assign(it, lowerPart) = (srcRotated, COND_NEGATIVE_SET);
        // U = [45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 27, 28, 29, 30, 31]
        assign(it, upperPart) = (srcRotated, COND_NEGATIVE_CLEAR);
        assign(it, VPM_OUT_SETUP_REGISTER) = (Value(Literal(genericSetup.value), TYPE_INT32) + elementOffset,
            InstructionDecorations::VPM_WRITE_CONFIGURATION);
        assign(it, VPM_IO_REGISTER) = lowerPart;
        assign(it, VPM_IO_REGISTER) = upperPart;
        it.erase();
        return it;
    }
    else
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        it = calculateElementOffsetInVPM(method, it, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(),
            internalOffset, elementOffset, !cacheEntry.area.canBePackedIntoRow());
        // 2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
        // 3) write setup with dynamic address
        assign(it, VPM_OUT_SETUP_REGISTER) = (Value(Literal(genericSetup.value), TYPE_INT32) + elementOffset,
            InstructionDecorations::VPM_WRITE_CONFIGURATION);
    }
    // 2. write data to VPM
    assign(it, VPM_IO_REGISTER) = access.getData();
    it.erase();
    return it;
}

NODISCARD static InstructionWalker lowerReadRAM(
    Method& method, InstructionWalker it, const RAMAccessInstruction& access, const VPMCacheEntry& cacheEntry)
{
    auto numEntries = getSourceValue(access.getNumEntries());
    numEntries = numEntries.getConstantValue().value_or(numEntries);

    // for some additional information, see
    // http://maazl.de/project/vc4asm/doc/VideoCoreIV-addendum.html

    // initialize VPM DMA for reading from host
    Value dmaSetupBits = UNDEFINED_VALUE;
    // TODO this assumes 1 row = 1 entry, is this always correct?
    it = insertReadDMASetup(
        it, dmaSetupBits, cacheEntry.area, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(), numEntries);

    if(cacheEntry.inAreaOffset != INT_ZERO)
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        // If we cannot pack the row, the data accessed from the QPUs is aligned to the beginning of a separate row per
        // element. To also align the data transferred via DMA to that, we do not align to a row of the correct type in
        // the call to #calculateElementOffsetInVPM, but instead below force alignment to 32-bit row.
        it = calculateElementOffsetInVPM(method, it, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(),
            cacheEntry.inAreaOffset, elementOffset, true);
        // 2) dynamically calculate new VPM address from base and offset (add offset to setup-value)
        if(!cacheEntry.area.canBePackedIntoRow())
            // need to modify offset to point to next row, not next element in same row
            elementOffset = assign(it, TYPE_INT32, "%vpm_row_offset") = elementOffset << 4_val;
        // 3) write setup with dynamic address
        dmaSetupBits = assign(it, TYPE_INT32, "%vpr_setup") =
            (dmaSetupBits + elementOffset, InstructionDecorations::VPM_READ_CONFIGURATION);
    }
    assign(it, VPM_IN_SETUP_REGISTER) = (dmaSetupBits, InstructionDecorations::VPM_READ_CONFIGURATION);

    VPRSetup strideSetup(VPRStrideSetup(0));
    if(numEntries != INT_ONE)
    {
        // NOTE: This for read the pitch (start-to-start) and for write the stride (end-to-start) is set, we need to set
        // this to the data size, but not required for write setup!
        // TODO if we have dynamic vector size, we can't do this, since we cannot statically determine the stride!
        strideSetup.strideSetup = VPRStrideSetup(static_cast<uint16_t>(cacheEntry.getVectorType().getInMemoryWidth()));
    }
    it.emplace(new LoadImmediate(VPM_IN_SETUP_REGISTER, Literal(strideSetup.value)));
    it->addDecorations(InstructionDecorations::VPM_READ_CONFIGURATION);
    it.nextInBlock();

    //"the actual DMA load or store operation is initiated by writing the memory address to the VCD_LD_ADDR or
    // VCD_ST_ADDR register" (p. 56)
    //-> write output-argument base address + offset/index into VPM_ADDR
    assign(it, VPM_DMA_LOAD_ADDR_REGISTER) = access.getMemoryAddress();
    //"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
    assign(it, NOP_REGISTER) = VPM_DMA_LOAD_WAIT_REGISTER;

    it.erase();
    return it;
}

NODISCARD static InstructionWalker lowerWriteRAM(
    Method& method, InstructionWalker it, const RAMAccessInstruction& access, const VPMCacheEntry& cacheEntry)
{
    // TODO is the calculation of the size to copy correct? We are mixing different types (e.g. byte from memory
    // instruction, consecutive memory area) with type for VPM area (rows which might not be filled completely). Same
    // for reading RAM!

    auto numEntries = getSourceValue(access.getNumEntries());
    numEntries = numEntries.getConstantValue().value_or(numEntries);

    // initialize VPM DMA for writing to host
    Value dmaSetupBits = UNDEFINED_VALUE;
    // TODO this assumes 1 row = 1 entry, is this always correct?
    it = insertWriteDMASetup(
        it, dmaSetupBits, cacheEntry.area, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(), numEntries);

    if(cacheEntry.inAreaOffset != INT_ZERO)
    {
        // this is the offset in byte -> calculate the offset in elements of destination-type

        // 1) convert offset in bytes to offset in elements (!! VPM stores vector-size of 16!!)
        Value elementOffset = UNDEFINED_VALUE;
        // If we cannot pack the row, the data accessed from the QPUs is aligned to the beginning of a separate row per
        // element. To also align the data transferred via DMA to that, we do not align to a row of the correct type in
        // the call to #calculateElementOffsetInVPM, but instead below force alignment to 32-bit row.
        it = calculateElementOffsetInVPM(method, it, cacheEntry.getScalarType(), cacheEntry.getVectorWidth(),
            cacheEntry.inAreaOffset, elementOffset, true);
        // 2) dynamically calculate new VPM address from base and offset (shift and add offset to setup-value)
        if(!cacheEntry.area.canBePackedIntoRow())
            // need to modify offset to point to next row, not next element in same row
            elementOffset = assign(it, TYPE_INT32, "%vpm_row_offset") = elementOffset << 4_val;
        Value shiftedOffset = assign(it, TYPE_INT32) = elementOffset << 3_val;
        // 3) write setup with dynamic address
        dmaSetupBits = assign(it, TYPE_INT32, "%vpw_setup") =
            (dmaSetupBits + shiftedOffset, InstructionDecorations::VPM_WRITE_CONFIGURATION);
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
    assign(it, VPM_DMA_STORE_ADDR_REGISTER) = access.getMemoryAddress();
    //"A new DMA load or store operation cannot be started until the previous one is complete" (p. 56)
    assign(it, NOP_REGISTER) = VPM_DMA_STORE_WAIT_REGISTER;

    it.erase();
    return it;
}

static InstructionWalker lowerAccessRAM(
    Method& method, InstructionWalker it, const RAMAccessInstruction& access, const VPMCacheEntry& cacheEntry)
{
    if(access.op == MemoryOperation::READ)
        return lowerReadRAM(method, it, access, cacheEntry);
    if(access.op == MemoryOperation::WRITE)
        return lowerWriteRAM(method, it, access, cacheEntry);
    throw CompilationError(
        CompilationStep::NORMALIZER, "Invalid memory operation for VPM DMA access", access.to_string());
}

static InstructionWalker lowerAccessCache(
    Method& method, InstructionWalker it, const CacheAccessInstruction& access, const VPMCacheEntry& cacheEntry)
{
    if(access.op == MemoryOperation::READ)
        return lowerReadVPM(method, it, access, cacheEntry);
    if(access.op == MemoryOperation::WRITE)
        return lowerWriteVPM(method, it, access, cacheEntry);
    throw CompilationError(
        CompilationStep::NORMALIZER, "Invalid memory operation for VPM cache access", access.to_string());
}

InstructionWalker periphery::lowerVPMAccess(Method& method, InstructionWalker it)
{
    auto vpmCacheEntry =
        (check(it.get<MemoryAccessInstruction>()) & &MemoryAccessInstruction::getVPMCacheEntry).value_or(nullptr);

    if(!vpmCacheEntry)
        return it;

    if(auto ramAccess = it.get<RAMAccessInstruction>())
        it = lowerAccessRAM(method, it, *ramAccess, *vpmCacheEntry);
    else if(auto cacheAccess = it.get<CacheAccessInstruction>())
        it = lowerAccessCache(method, it, *cacheAccess, *vpmCacheEntry);
    return it;
}
