/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TMU.h"

#include "../GlobalValues.h"
#include "../InstructionWalker.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"

#include <atomic>

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::operators;

// running counter, for visual distinction of the TMU cache entries only
static std::atomic_uint32_t tmuCacheEntryCounter{0};

TMUCacheEntry::TMUCacheEntry(const TMU& tmu, const Value& addr, DataType originalType) :
    index(tmuCacheEntryCounter++), tmu(tmu), addresses(addr),
    numVectorElements(originalType.getPointerType() ? 1 : originalType.getVectorWidth()),
    elementStrideInBytes(originalType.getScalarBitCount() / 8)
{
    if(!originalType.isSimpleType() && !originalType.getPointerType())
        throw CompilationError(
            CompilationStep::GENERAL, "Reading of this type via TMU is not implemented", originalType.to_string());
}

TMUCacheEntry::~TMUCacheEntry() noexcept = default;

LCOV_EXCL_START
std::string TMUCacheEntry::to_string() const
{
    return "TMU" + std::to_string(static_cast<unsigned>(getTMUIndex())) + " cache entry " + std::to_string(index) +
        " (" + std::to_string(static_cast<unsigned>(numVectorElements)) + " elements with stride of " +
        std::to_string(elementStrideInBytes) + " byte)";
}
LCOV_EXCL_STOP

uint8_t TMUCacheEntry::getTMUIndex() const noexcept
{
    return tmu.signal == SIGNAL_LOAD_TMU0 ? 0 : 1;
}

const intermediate::RAMAccessInstruction* TMUCacheEntry::getRAMReader() const
{
    auto memoryAccesses = getMemoryAccesses();
    return memoryAccesses.size() == 1 ? *memoryAccesses.begin() : nullptr;
}

const intermediate::CacheAccessInstruction* TMUCacheEntry::getCacheReader() const
{
    auto cacheAccesses = getQPUAccesses();
    return cacheAccesses.size() == 1 ? *cacheAccesses.begin() : nullptr;
}

const TMU periphery::TMU0{REG_TMU0_COORD_S_U_X, REG_TMU0_COORD_T_V_Y, REG_TMU0_COORD_R_BORDER_COLOR,
    REG_TMU0_COORD_B_LOD_BIAS, SIGNAL_LOAD_TMU0};
const TMU periphery::TMU1{REG_TMU1_COORD_S_U_X, REG_TMU1_COORD_T_V_Y, REG_TMU1_COORD_R_BORDER_COLOR,
    REG_TMU1_COORD_B_LOD_BIAS, SIGNAL_LOAD_TMU1};

static NODISCARD InstructionWalker insertCalculateAddressOffsets(Method& method, InstructionWalker it,
    const Value& baseAddress, uint8_t numElements, uint32_t elementStrideInBytes, const Value& outputAddress)
{
    // since the base address might be a single pointer, we need to replicate it for the upper vector elements to read
    // the correct address
    Value replicatedAddress = baseAddress;
    if(!baseAddress.isAllSame())
    {
        replicatedAddress =
            method.addNewLocal(method.createPointerType(TYPE_INT32.toVectorType(numElements)), "%replicated_address");
        it = intermediate::insertReplication(it, baseAddress, replicatedAddress);
    }

    if(numElements == 1)
    {
        // We don't actually need to load anything into the upper SIMD vector elements. But since we cannot "not load
        // anything" for single elements, we just load the same data into all elements, which at least requires only a
        // single TMU cache miss.
        // To guarantee that we actually load the same address (and not some random value which happened to be in the
        // address register), we also use the replicated address for the scalar loads.
        assign(it, outputAddress) = replicatedAddress;
        return it;
    }
    /*
     * we need to set the addresses in this way:
     *
     * element 0: base-address + type-size * 0
     * element 1: base-address + type-size * 1
     * element 2: base-address + type-size * 2
     * ...
     */
    const Value addressOffsets = method.addNewLocal(TYPE_INT32.toVectorType(numElements), "%address_offset");

    // addressOffsets = sizeof(type) * elem_num
    assign(it, addressOffsets) = ELEMENT_NUMBER_REGISTER * Literal(elementStrideInBytes);
    // We don't actually need to load anything into the upper SIMD vector elements. But since we cannot "not load
    // anything" for single elements, we just load the successive data into the upper elements. Since the data is most
    // likely anyway on the same TMU cache line and/or will be queried in a successive (work-group) loop iteration, this
    // gives us little overhead.
    assign(it, outputAddress) = replicatedAddress + addressOffsets;
    return it;
}

/*
 * Loading via TMU has 2 limitations:
 * - the address needs to be 32-bit aligned (the last bits of the address are ignored)
 * - only 32-bit values can be read
 *
 * For 32-bit types, simply calculating the addresses of the other vector-elements by adding the type-size to the base
 * address is correct. For non 32-bit types, this is not quite correct, e.g. a loading of 4 short-values actually looks
 * like this:
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
 * NOTE: out-of-bounds words cannot lie outside of reserved memory (if data does), since memory is always 4 Byte aligned
 * and multiples of 4 Byte
 */
static NODISCARD InstructionWalker insertExtractHalfWordElements(
    Method& method, InstructionWalker it, const Value& dest, const Value& src, const Value& addressVector)
{
    // 1) for every address, check if it is aligned to 4 Byte <-> address & 0b11 == 0
    assign(it, NOP_REGISTER) = (addressVector & 3_val, SetFlag::SET_FLAGS);
    // 2) extract short values
    // 2.1) if the element address is aligned correctly, use lower half-word, otherwise use upper-half word
    // 2.2) otherwise, use lower half-word from odd elements and upper half-word from even elements

    // tmp = address & 0b11 ? src >> 16 : src
    const Value tmp = method.addNewLocal(dest.type, "%tmu_result");
    assign(it, tmp) = (as_unsigned{src} >> 16_val, COND_ZERO_CLEAR);
    assign(it, tmp) = (src, COND_ZERO_SET);

    // dest = tmp & 0xFFFF
    assign(it, dest) = tmp & Value(Literal(TYPE_INT16.getScalarWidthMask()), TYPE_INT32);
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
static NODISCARD InstructionWalker insertExtractByteElements(
    Method& method, InstructionWalker it, const Value& dest, const Value& src, const Value& addressVector)
{
    // alignmentOffset = address & 0b11
    Value alignmentOffset = assign(it, dest.type, "%alignment_offset") = addressVector & 3_val;

    // shiftOffset = alignmentOffset * 8
    Value shiftOffset = assign(it, dest.type, "%shift_offset") = mul24(alignmentOffset, 8_val);

    // tmp = src >> shiftOffset
    Value tmp = assign(it, dest.type, "%tmu_result") = as_unsigned{src} >> shiftOffset;

    // dest = tmp & 0xFF
    assign(it, dest) = tmp & Value(Literal(TYPE_INT8.getScalarWidthMask()), TYPE_INT32);
    return it;
}

/*
 * For 64-bit read, do the following:
 * - read 2*N (where N is vector-size) 32-bit elements, if possible (e.g. N <= 8) in one go
 * - rotate upper parts to "upper" locals, rotate lower parts to "lower" locals
 *
 * For 64-bit loads, we do not have the same problem as for bytes/half-words above with the alignment, since all 64-bit
 * aligned addresses are already correctly aligned for 32-bit addresses.
 */
static NODISCARD InstructionWalker insertReadLongVectorFromTMU(
    Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu)
{
    using namespace intermediate;

    auto outputData = Local::getLocalData<MultiRegisterData>(dest.checkLocal());
    if(!outputData)
        throw CompilationError(
            CompilationStep::GENERAL, "Can only read 64-bit value from TMU into long local", dest.to_string());

    /*
     * This works:
     * tmu0s = <address>
     * tmu0s = <address>
     * (load_tmu0)
     * <lower> = r4
     * (load_tmu0)
     * <upper> = r4
     *
     * This works:
     * tmu0s = <address>
     * (load_tmu0)
     * tmu0s = <address>
     * <lower> = r4
     * (load_tmu0)
     * <upper> = r4
     *
     * This does not work:
     * tmu0s = <address>
     * (load_tmu0)
     * tmu0s = <address>
     * (load_tmu0)
     * <lower> = r4
     * <upper> = r4
     * (Both receive the result of the last load_tmu0, since it overrides any previous value in r4)
     */

    auto lowerEntry =
        tmu.createEntry(method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%tmu_address"), dest.type);
    auto upperEntry =
        tmu.createEntry(method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%tmu_address"), dest.type);

    // load the lower N 32-bit elements (where N is the result vector size)
    // using the 64-bit destination type, the address offsets guarantee the upper part to be skipped, e.g.
    // element 0 loads from address + 0, element 1 from address + 8, ..., element N from address + 8 * N
    it.emplace(new RAMAccessInstruction(MemoryOperation::READ, addr, lowerEntry));
    it.nextInBlock();
    // load the upper N 32-bit elements (where N is the result vector size)
    // using the 64-bit destination type, the address offsets guarantee the lower part to be skipped, e.g.
    // element 0 loads from address + 4, element 1 from address + 12, ..., element N from address + 4 + 8 * N
    auto tmpAddress = assign(it, addr.type, "%tmu_upper_addr") = addr + 4_val;
    it.emplace(new RAMAccessInstruction(MemoryOperation::READ, tmpAddress, upperEntry));
    it.nextInBlock();

    // read the lower and upper elements into the result variables
    it.emplace(new CacheAccessInstruction(MemoryOperation::READ, outputData->lower->createReference(), lowerEntry));
    it.nextInBlock();

    it.emplace(new CacheAccessInstruction(MemoryOperation::READ, outputData->upper->createReference(), upperEntry));
    it.nextInBlock();

    return it;
}

InstructionWalker periphery::insertReadVectorFromTMU(
    Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu)
{
    using namespace intermediate;
    if(!dest.type.isSimpleType() && !dest.type.getPointerType())
        throw CompilationError(
            CompilationStep::GENERAL, "Reading of this type via TMU is not (yet) implemented", dest.type.to_string());

    if(dest.type.getScalarBitCount() == 64)
        return insertReadLongVectorFromTMU(method, it, dest, addr, tmu);

    // NOTE: actually this is baseAddr.type * type.num, but we can't have vectors of pointers
    auto entry =
        tmu.createEntry(method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%tmu_address"), dest.type);
    it.emplace(new RAMAccessInstruction(MemoryOperation::READ, addr, entry));
    it.nextInBlock();
    it.emplace(new CacheAccessInstruction(MemoryOperation::READ, dest, entry));
    it.nextInBlock();
    return it;
}

NODISCARD static InstructionWalker lowerReadRAM(Method& method, InstructionWalker it,
    const intermediate::RAMAccessInstruction& access, const TMUCacheEntry& cacheEntry)
{
    it = insertCalculateAddressOffsets(method, it, access.getMemoryAddress(), cacheEntry.numVectorElements,
        cacheEntry.elementStrideInBytes, cacheEntry.addresses);

    //"General-memory lookups are performed by writing to just the s-parameter, using the absolute memory address" (page
    // 41)  1) write address to TMU_S register
    assign(it, cacheEntry.tmu.getAddress(access.getMemoryAddress().type)) = cacheEntry.addresses;

    return it.erase();
}

NODISCARD static InstructionWalker lowerReadCache(Method& method, InstructionWalker it,
    const intermediate::CacheAccessInstruction& access, const TMUCacheEntry& cacheEntry)
{
    const auto& dest = access.getData();

    // 2) trigger loading of TMU
    nop(it, intermediate::DelayType::WAIT_TMU, cacheEntry.tmu.signal);
    it.copy().previousInBlock()->addDecorations(intermediate::InstructionDecorations::MANDATORY_DELAY);
    // 3) read value from R4
    // NOTE: in both cases, result values are unsigned (as in zero-, not sign-extended)!! (Same behavior as for VPM?!)
    if(dest.type.getScalarBitCount() <= 8)
    {
        Value tmp = assign(it, TYPE_INT32.toVectorType(dest.type.getVectorWidth()), "%tmu_result") = TMU_READ_REGISTER;
        it = insertExtractByteElements(method, it, dest, tmp, cacheEntry.addresses);
    }
    else if(dest.type.getScalarBitCount() <= 16)
    {
        Value tmp = assign(it, TYPE_INT32.toVectorType(dest.type.getVectorWidth()), "%tmu_result") = TMU_READ_REGISTER;
        it = insertExtractHalfWordElements(method, it, dest, tmp, cacheEntry.addresses);
    }
    else if(dest.type.getScalarBitCount() <= 32)
        assign(it, dest) = TMU_READ_REGISTER;
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot read values larger than 32-bit via TMU", dest.to_string());

    return it.erase();
}

InstructionWalker periphery::lowerTMURead(Method& method, InstructionWalker it)
{
    using namespace intermediate;

    auto tmuCacheEntry =
        (check(it.get<MemoryAccessInstruction>()) & &MemoryAccessInstruction::getTMUCacheEntry).value_or(nullptr);

    if(!tmuCacheEntry)
        return it;

    if(auto ramAccess = it.get<RAMAccessInstruction>())
        //"General-memory lookups are performed by writing to just the s-parameter, using the absolute memory address"
        //(page 41)
        // 1) write address to TMU_S register
        it = lowerReadRAM(method, it, *ramAccess, *tmuCacheEntry);
    else if(auto cacheAccess = it.get<CacheAccessInstruction>())
        // 2) trigger loading of TMU
        // 3) read value from R4
        it = lowerReadCache(method, it, *cacheAccess, *tmuCacheEntry);
    return it;
}
