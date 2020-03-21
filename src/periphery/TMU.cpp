/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TMU.h"

#include "../GlobalValues.h"
#include "../InstructionWalker.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::operators;

const TMU periphery::TMU0{REG_TMU0_COORD_S_U_X, REG_TMU0_COORD_T_V_Y, REG_TMU0_COORD_R_BORDER_COLOR,
    REG_TMU0_COORD_B_LOD_BIAS, SIGNAL_LOAD_TMU0};
const TMU periphery::TMU1{REG_TMU1_COORD_S_U_X, REG_TMU1_COORD_T_V_Y, REG_TMU1_COORD_R_BORDER_COLOR,
    REG_TMU1_COORD_B_LOD_BIAS, SIGNAL_LOAD_TMU1};

static NODISCARD InstructionWalker insertCalculateAddressOffsets(
    Method& method, InstructionWalker it, const Value& baseAddress, DataType type, Value& outputAddress)
{
    if(type.isScalarType())
    {
        // for scalar loads, we can save us all that effort, since we already have the address in element 0 and there is
        // no offset for element zero. So just set the address for all other elements to 0 to not load anything there.
        outputAddress = assign(it, method.createPointerType(type.getElementType().toVectorType(type.getVectorWidth())),
            "%tmu_address") = 0_val;
        auto cond = assignNop(it) = selectSIMDElement(0);
        assign(it, outputAddress) = (baseAddress, cond);
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
    const Value addressOffsets = method.addNewLocal(TYPE_INT32.toVectorType(type.getVectorWidth()), "%address_offset");
    // NOTE: actually this is baseAddr.type * type.num, but we can't have vectors of pointers
    outputAddress = method.addNewLocal(TYPE_INT32.toVectorType(type.getVectorWidth()), "%tmu_address");

    // since the base address might be a single pointer, we need to replicate it for the upper vector elements to read
    // the correct address
    Value replicatedAddress = method.addNewLocal(
        method.createPointerType(type.getElementType().toVectorType(type.getVectorWidth())), "%replicated_address");
    it = intermediate::insertReplication(it, baseAddress, replicatedAddress);

    // addressOffsets = sizeof(type) * elem_num
    assign(it, addressOffsets) = ELEMENT_NUMBER_REGISTER * Literal(static_cast<uint32_t>(type.getScalarBitCount()) / 8);
    // outputAddress = (elem_num < type.num) ? baseAddress + addressOffsets : 0
    assign(it, NOP_REGISTER) =
        (ELEMENT_NUMBER_REGISTER - Value(Literal(static_cast<int32_t>(type.getVectorWidth())), TYPE_INT8),
            SetFlag::SET_FLAGS);
    // XXX rewrite, so it can be combined with next instruction
    // TODO or generally check if we can rewrite "mov out, 0" in way so we can combine it with next/previous instruction
    // (e.g. by using "xor out, x, x" or "v8subs out, x, x")
    assign(it, outputAddress) = (0_val, COND_NEGATIVE_CLEAR);
    assign(it, outputAddress) = (replicatedAddress + addressOffsets, COND_NEGATIVE_SET);
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
    auto outputData = Local::getLocalData<MultiRegisterData>(dest.checkLocal());
    if(!outputData)
        throw CompilationError(
            CompilationStep::GENERAL, "Can only read 64-bit value from TMU into long local", dest.to_string());

    // load the lower N 32-bit elements (where N is the result vector size)
    // using the 64-bit destination type, the address offsets guarantee the upper part to be skipped, e.g.
    // element 0 loads from address + 0, element 1 from address + 8, ..., element N from address + 8 * N
    Value lowerAddresses(UNDEFINED_VALUE);
    it = insertCalculateAddressOffsets(method, it, addr, dest.type, lowerAddresses);

    // load the upper N 32-bit elements (where N is the result vector size)
    // using the 64-bit destination type, the address offsets guarantee the lower part to be skipped, e.g.
    // element 0 loads from address + 4, element 1 from address + 12, ..., element N from address + 4 + 8 * N
    auto tmpAddress = assign(it, addr.type, "%tmu_upper_addr") = addr + 4_val;
    Value upperAddresses(UNDEFINED_VALUE);
    it = insertCalculateAddressOffsets(method, it, tmpAddress, dest.type, upperAddresses);

    assign(it, tmu.getAddress(addr.type)) = lowerAddresses;
    nop(it, intermediate::DelayType::WAIT_TMU, tmu.signal);
    // TODO do we get more performance when first writing both addresses and then triggering both loads?
    assign(it, tmu.getAddress(addr.type)) = upperAddresses;
    nop(it, intermediate::DelayType::WAIT_TMU, tmu.signal);

    // read the lower and upper elements into the result variables
    assign(it, outputData->lower->createReference()) = TMU_READ_REGISTER;
    assign(it, outputData->upper->createReference()) = TMU_READ_REGISTER;

    return it;
}

InstructionWalker periphery::insertReadVectorFromTMU(
    Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu)
{
    if(!dest.type.isSimpleType() && !dest.type.getPointerType())
        throw CompilationError(
            CompilationStep::GENERAL, "Reading of this type via TMU is not (yet) implemented", dest.type.to_string());

    if(dest.type.getScalarBitCount() == 64)
        return insertReadLongVectorFromTMU(method, it, dest, addr, tmu);

    Value addresses(UNDEFINED_VALUE);
    it = insertCalculateAddressOffsets(method, it, addr, dest.type, addresses);

    //"General-memory lookups are performed by writing to just the s-parameter, using the absolute memory address" (page
    // 41)  1) write address to TMU_S register
    assign(it, tmu.getAddress(addr.type)) = addresses;
    // 2) trigger loading of TMU
    nop(it, intermediate::DelayType::WAIT_TMU, tmu.signal);
    // 3) read value from R4
    // FIXME in both cases, result values are unsigned (as in zero-, not sign-extended)!! (Same behavior as for VPM?!)
    if(dest.type.getScalarBitCount() <= 8)
    {
        Value tmp = assign(it, TYPE_INT32.toVectorType(dest.type.getVectorWidth()), "%tmu_result") = TMU_READ_REGISTER;
        return insertExtractByteElements(method, it, dest, tmp, addresses);
    }
    else if(dest.type.getScalarBitCount() <= 16)
    {
        Value tmp = assign(it, TYPE_INT32.toVectorType(dest.type.getVectorWidth()), "%tmu_result") = TMU_READ_REGISTER;
        return insertExtractHalfWordElements(method, it, dest, tmp, addresses);
    }
    else if(dest.type.getScalarBitCount() <= 32)
        assign(it, dest) = TMU_READ_REGISTER;
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot read values larger than 32-bit via TMU", dest.to_string());
    return it;
}

InstructionWalker periphery::insertReadTMU(Method& method, InstructionWalker it, const Value& image, const Value& dest,
    const Value& xCoord, const Optional<Value>& yCoord, const TMU& tmu)
{
    if(!image.checkLocal())
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot access image-configuration for non-local image", image.to_string());
    const Global* imageConfig =
        method.findGlobal(ImageType::toImageConfigurationName(image.local()->getBase(false)->name));
    if(imageConfig == nullptr)
        throw CompilationError(
            CompilationStep::GENERAL, "Failed to find the image-configuration for", image.to_string());
    if(!xCoord.type.isFloatingType())
        throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the x-axis",
            xCoord.to_string());
    if(yCoord && !yCoord->type.isFloatingType())
        throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the y-axis",
            yCoord.to_string());

    // 1. set the UNIFORM pointer to point to the configurations for the image about to be read
    assign(
        it, Value(REG_UNIFORM_ADDRESS, method.createPointerType(TYPE_INT32.toVectorType(16), AddressSpace::GLOBAL))) =
        imageConfig->createReference();
    // 2. need to wait 2 instructions for UNIFORM-pointer to be changed
    nop(it, intermediate::DelayType::WAIT_UNIFORM);
    nop(it, intermediate::DelayType::WAIT_UNIFORM);
    // 3. write the TMU addresses
    if(yCoord)
    {
        assign(it, tmu.getYCoord(yCoord->type)) = yCoord.value();
    }
    else
    {
        // for 1D-images, we only have an x-coordinate, but if we only write the TMU_S register, general TMU lookup is
        // used!  so we write a dummy y-coordinate with a value of zero, to select the first row
        assign(it, tmu.getYCoord()) = 0_val;
    }
    assign(it, tmu.getXCoord(xCoord.type)) = xCoord;
    // 4. trigger loadtmu
    nop(it, intermediate::DelayType::WAIT_TMU, tmu.signal);
    // 5. read from r4 (stalls 9 to 20 cycles)
    assign(it, dest) = TMU_READ_REGISTER;
    // 6. TODO reset UNIFORM pointer? for next work-group iteration, or disable when used with images?
    return it;
}
