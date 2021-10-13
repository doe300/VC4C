/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TypeConversions.h"

#include "Helper.h"
#include "VectorHelper.h"
#include "operators.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

/**
 * Inserts a bit-cast from a source to a destination with the same element bit-width, but with omitting of elements.
 *
 * Since the element stride is not 1 (i.e. we skip elements, e.g. for intN to lower/upper part of longN conversion), we
 * cannot just simply copy, but instead have to rotate every element into place.
 */
static NODISCARD InstructionWalker insertStridedBitcast(
    InstructionWalker it, Method& method, const Value& src, const Value& dest, uint32_t elementStride, bool zeroOutput)
{
    auto destination = method.addNewLocal(dest.type, "%bit_cast_result");
    if(zeroOutput)
        assign(it, destination) = INT_ZERO;

    for(unsigned i = 0; i < dest.type.getVectorWidth(); ++i)
    {
        // This allows us to e.g. skip every second word for casting 32-bit word to lower word of 64-bit value
        unsigned sourceIndex = i * elementStride;

        const Value tmp = method.addNewLocal(dest.type, "%bit_cast");
        // the vector-rotation to element 0 and then to the destination element should be combined by optimization-step
        // #combineVectorRotations
        if(i == 0 && !zeroOutput)
            // assign destination with first element, so register-allocation finds unconditional write
            assign(it, destination) = src;
        else
        {
            it = insertVectorExtraction(it, method, src, Value(Literal(sourceIndex), TYPE_INT8), tmp);
            it = insertVectorInsertion(it, method, destination, Value(Literal(i), TYPE_INT8), tmp);
        }
    }

    it.emplace(std::make_unique<MoveOperation>(dest, destination));
    return it;
}

/**
 * Inserts a bit-cast where the destination element-type is larger than the source element-type, combining multiple
 * elements into a single one.
 *
 * This also means, the source vector has more elements (of smaller type-size) than the destination vector
 */
static NODISCARD InstructionWalker insertCombiningBitcast(
    InstructionWalker it, Method& method, const Value& src, const Value& dest, uint32_t elementStride, bool zeroOutput)
{
    // the number of source elements to combine in a single destination element
    unsigned sizeFactor = dest.type.getScalarBitCount() / src.type.getScalarBitCount();
    // the number of bits to shift per element
    auto shift = src.type.getScalarBitCount();

    /*
     * By shifting and ANDing whole source vector, we save a few instructions for sources with more than 1 element
     *
     * E.g. short4 -> int2 can be written as
     * (short4 & 0xFFFF) << 0 -> int2 (lower half-words in elements 0 and 2)
     * (short4 & 0xFFFF) << 16 -> int2 (upper half-words in element 1 and 3)
     * -> we only need 2 shifts and 2 ANDs instead of 4 (per element)
     */

    Value truncatedSource = assign(it, src.type, "%bit_cast") =
        src & Value(Literal(src.type.getScalarWidthMask()), TYPE_INT32);

    std::vector<Value> shiftedTruncatedVectors;
    shiftedTruncatedVectors.reserve(sizeFactor);
    for(unsigned i = 0; i < sizeFactor; ++i)
    {
        shiftedTruncatedVectors.emplace_back(
            method.addNewLocal(dest.type.toVectorType(src.type.getVectorWidth()), "%bit_cast"));
        const Value& result = shiftedTruncatedVectors.back();
        if(i == 0)
            // no need to shift for 0th element
            assign(it, result) = truncatedSource;
        else
            assign(it, result) = truncatedSource << Value(Literal(shift * i), TYPE_INT8);
    }

    /*
     * The up to 8 destination elements are now distributed across the shiftedTruncatedVectors (stvs) as follows:
     *
     * Size-factor of 2:
     * stv0[0] | stv1[1], stv0[2] | stv1[3], stv0[4] | stv1[5], ...
     *
     * Size-factor of 4;
     * stv0[0] | stv1[1] | stv2[2] | stv3[3], stv0[4] | stv1[5] | stv2[6] | stv3[7], ...
     *
     * To simplify the assembly of the destination, we rotate the vectors, so their element-numbers align
     */

    std::vector<Value> rotatedVectors;
    rotatedVectors.reserve(shiftedTruncatedVectors.size());
    for(unsigned i = 0; i < shiftedTruncatedVectors.size(); ++i)
    {
        if(i == 0)
            // no need to rotate
            rotatedVectors.emplace_back(shiftedTruncatedVectors.front());
        else
        {
            rotatedVectors.emplace_back(
                method.addNewLocal(dest.type.toVectorType(src.type.getVectorWidth()), "%bit_cast"));
            const Value& result = rotatedVectors.back();
            it = insertVectorRotation(
                it, shiftedTruncatedVectors[i], Value(Literal(i), TYPE_INT8), result, Direction::DOWN);
        }
    }

    /*
     * The up to 8 destination elements are now distributed across the rotatedVectors (rvs) as follows:
     *
     * Size-factor of 2:
     * rv0[0] | rv1[0], rv0[2] | rv1[2], rv0[4] | rv1[4], ...
     *
     * Size-factor of 4;
     * rv0[0] | rv1[0] | rv2[0] | rv3[0], rv0[4] | rv1[4] | rv2[4] | rv3[4], ...
     *
     * In the next step, we OR the separate vectors to a single one
     */
    Value combinedVector = INT_ZERO;
    for(const Value& rv : rotatedVectors)
    {
        Value newCombinedVector = assign(it, dest.type.toVectorType(src.type.getVectorWidth()), "%bit_cast") =
            combinedVector | rv;
        combinedVector = newCombinedVector;
    }

    /*
     * Now, we have the destination elements as follows:
     *
     * Size-factor of 2:
     * cv[0], cv[2], cv[4], cv[6], ...
     *
     * Size-factor of 4;
     * cv[0], cv[4], cv[8], cv[12], ...
     *
     * Finally, we rotate the single elements to fit their position in the destination
     */
    return insertStridedBitcast(it, method, combinedVector, dest, elementStride * sizeFactor, zeroOutput);
}

/**
 * Inserts a bit-cast where the destination element-type is smaller than the source element-type, splitting a single
 * element into several ones.
 *
 * This also means, the source vector has less elements (of larger type-size) than the destination vector
 */
static NODISCARD InstructionWalker insertSplittingBitcast(
    InstructionWalker it, Method& method, const Value& src, const Value& dest, uint32_t elementStride, bool zeroOutput)
{
    if(elementStride != 1)
        throw CompilationError(
            CompilationStep::GENERAL, "Splitting bit-cast with element stride is not yet implemented");
    // the number of destination elements to extract from a single source element
    unsigned sizeFactor = src.type.getScalarBitCount() / dest.type.getScalarBitCount();

    /*
     * By shifting and ANDing whole source vector, we save a few instructions for sources with more than 1 element
     *
     * E.g. int2 -> short4 can be written as
     * (int2 >> 0) & 0xFFFF -> short4 (lower half-words)
     * (int2 >> 16) & 0xFFFF -> short4 (upper half-words)
     * -> we only need 1 shift and 1 AND instead of 4 (per element)
     */
    std::vector<Value> shiftedTruncatedVectors;
    shiftedTruncatedVectors.reserve(sizeFactor);
    auto srcLongData = Local::getLocalData<MultiRegisterData>(src.checkLocal());
    for(unsigned i = 0; i < sizeFactor; ++i)
    {
        shiftedTruncatedVectors.emplace_back(
            method.addNewLocal(dest.type.toVectorType(src.type.getVectorWidth()), "%bit_cast"));
        const Value& result = shiftedTruncatedVectors.back();
        auto srcVal = src;
        auto shiftOffset = dest.type.getScalarBitCount() * i;
        if(srcLongData)
        {
            // need to correctly take the lower or upper part for 64-bit locals and adapt the shift offset
            srcVal = (i >= (sizeFactor / 2) ? srcLongData->upper : srcLongData->lower)->createReference();
            shiftOffset -= i >= (sizeFactor / 2) ? 32 : 0;
        }
        Value tmpShifted = srcVal;
        if(shiftOffset != 0)
            // no need to shift for 0th element
            tmpShifted = assign(it, result.type, "%bit_cast") =
                as_unsigned{srcVal} >> Value(Literal(shiftOffset), TYPE_INT8);
        if(i == (sizeFactor - 1u))
            // no need to AND for upper-most element
            assign(it, result) = tmpShifted;
        else
            assign(it, result) = tmpShifted & Value(Literal(dest.type.getScalarWidthMask()), TYPE_INT32);
    }

    /*
     * The up to 16 destination elements are now distributed across the shiftedTruncatedVectors (stvs) as follows:
     *
     * Size-factor of 2:
     * stv0[0], stv1[0], stv0[1], stv1[1], stv0[2], ...
     *
     * Size-factor of 4;
     * stv0[0], stv1[0], stv2[0], stv3[0], stv0[1], ...
     *
     * So we need to assemble the destination vector from these vectors
     */

    const Value destination = method.addNewLocal(dest.type, "%bit_cast_result");
    if(zeroOutput)
        assign(it, destination) = INT_ZERO;

    for(unsigned i = 0; i < dest.type.getVectorWidth(); ++i)
    {
        const Value& stv = shiftedTruncatedVectors.at(i % shiftedTruncatedVectors.size());
        unsigned sourceElement = static_cast<unsigned>(i / shiftedTruncatedVectors.size());

        // need to fix-up the type to single element for vector insertion to handle it as such
        const Value tmp = method.addNewLocal(dest.type.getElementType(), "%bit_cast");
        // the vector-rotation to element 0 and then to the destination element should be combined by optimization-step
        // #combineVectorRotations
        it = insertVectorExtraction(it, method, stv, Value(Literal(sourceElement), TYPE_INT8), tmp);
        if(i == 0 && !zeroOutput)
            // assign destination with first element, so register-allocation finds unconditional write
            assign(it, destination) = tmp;
        else
            it = insertVectorInsertion(it, method, destination, Value(Literal(i), TYPE_INT8), tmp);
    }

    it.emplace(std::make_unique<MoveOperation>(dest, destination));
    return it;
}

InstructionWalker intermediate::insertBitcast(InstructionWalker it, Method& method, const Value& src, const Value& dest,
    InstructionDecorations deco, uint32_t elementStride, bool zeroOutput)
{
    if(src.isUndefined())
        it.emplace(std::make_unique<intermediate::MoveOperation>(dest, UNDEFINED_VALUE));
    else if(src.isZeroInitializer())
        it.emplace(std::make_unique<intermediate::MoveOperation>(dest, INT_ZERO));
    else if(src.type.getScalarBitCount() < dest.type.getScalarBitCount())
        it = insertCombiningBitcast(it, method, src, dest, elementStride, zeroOutput);
    else if(src.type.getScalarBitCount() > dest.type.getScalarBitCount())
        it = insertSplittingBitcast(it, method, src, dest, elementStride, zeroOutput);
    else if(elementStride != 1u)
        it = insertStridedBitcast(it, method, src, dest, elementStride, zeroOutput);
    else
        // bit-casts with types of same vector-size (and therefore same element-size) are simple moves
        it.emplace(std::make_unique<intermediate::MoveOperation>(dest, src)).addDecorations(deco);

    // last step: map destination to source (if bit-cast of pointers)
    if(dest.checkLocal() && src.checkLocal() && dest.type.getPointerType() && src.type.getPointerType())
        // this helps recognizing lifetime-starts of bit-cast stack-allocations
        const_cast<Local*>(dest.local())->set(ReferenceData(*src.local(), 0));
    it->addDecorations(deco);
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertZeroExtension(InstructionWalker it, Method& method, const Value& src,
    const Value& dest, bool allowLiteral, const ConditionCode conditional, const SetFlag setFlags)
{
    if(src.type.getScalarBitCount() == 32 && dest.type.getScalarBitCount() <= 32)
    {
        //"extend" to smaller type
        auto& newMove = it.emplace(std::make_unique<MoveOperation>(dest, src, conditional, setFlags));
        switch(dest.type.getScalarBitCount())
        {
        case 8:
            newMove.setPackMode(PACK_INT_TO_CHAR_TRUNCATE);
            break;
        case 16:
            newMove.setPackMode(PACK_INT_TO_SHORT_TRUNCATE);
            break;
        case 32:
            // no pack mode
            break;
        default:
            throw CompilationError(
                CompilationStep::GENERAL, "Invalid type-width for zero-extension", dest.type.to_string());
        }
    }
    else if(dest.type.getScalarBitCount() > 32 && Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
    {
        auto out = dest.local()->get<MultiRegisterData>();
        if(src.type.getScalarBitCount() < 32)
        {
            // extend to 32-bit integer first
            it = insertZeroExtension(
                it, method, src, out->lower->createReference(), allowLiteral, conditional, setFlags);
        }
        else
            assign(it, out->lower->createReference()) = (src, conditional, setFlags);
        // set upper word to all zeros
        it.emplace(std::make_unique<MoveOperation>(out->upper->createReference(), INT_ZERO));
        it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    }
    else if(dest.type.getScalarBitCount() >= 32 && src.type.getScalarBitCount() >= 32)
    {
        // do nothing, is just a move, since we truncate the 64-bit integers anyway
        it.emplace(std::make_unique<MoveOperation>(dest, src, conditional, setFlags));
    }
    else if(src.checkRegister() &&
        (has_flag(src.reg().file, RegisterFile::PHYSICAL_A) || has_flag(src.reg().file, RegisterFile::ACCUMULATOR)) &&
        src.type.getScalarBitCount() == 8)
    {
        // if we zero-extend from register-file A, use unpack-modes
        // this is applied e.g. for unpacking parameters in code-generation, since the source is UNIFORM
        it.emplace(std::make_unique<MoveOperation>(dest, src, conditional, setFlags))
            .setUnpackMode(UNPACK_CHAR_TO_INT_ZEXT);
    }
    else if(allowLiteral)
    {
        it.emplace(std::make_unique<Operation>(
            OP_AND, dest, src, Value(Literal(src.type.getScalarWidthMask()), TYPE_INT32), conditional, setFlags));
    }
    else
    {
        const Value tmp = method.addNewLocal(TYPE_INT32, "%zext");
        assign(it, tmp) = load(Literal(src.type.getScalarWidthMask()));
        it.emplace(std::make_unique<Operation>(OP_AND, dest, src, tmp, conditional, setFlags));
    }

    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    auto writer = src.getSingleWriter();
    if(writer)
        it->addDecorations(intermediate::forwardDecorations(writer->decoration));
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertSignExtension(InstructionWalker it, Method& method, const Value& src,
    const Value& dest, bool allowLiteral, const ConditionCode conditional, const SetFlag setFlags)
{
    if(dest.type.getScalarBitCount() > 32 && Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
    {
        auto out = dest.local()->get<MultiRegisterData>();
        if(src.type.getScalarBitCount() < 32)
        {
            // extend to 32-bit integer first
            it = insertSignExtension(
                it, method, src, out->lower->createReference(), allowLiteral, conditional, SetFlag::DONT_SET);
        }
        else
            assign(it, out->lower->createReference()) = (src, conditional, setFlags);
        auto offset = 31_val;
        if(!allowLiteral)
        {
            offset = method.addNewLocal(TYPE_INT8, "%sext");
            assign(it, offset) = load(31_lit);
        }
        // replicate the high bit across the whole word to be written to the upper word
        assign(it, out->upper->createReference()) =
            (as_signed{out->lower->createReference()} >> offset, conditional, setFlags);
        it.previousInBlock();
    }
    else if(dest.type.getScalarBitCount() >= 32 && src.type.getScalarBitCount() >= 32)
    {
        // do nothing, is just a move, since we truncate the 64-bit integers anyway
        it.emplace(std::make_unique<MoveOperation>(dest, src, conditional, setFlags));
    }
    else if(src.checkRegister() &&
        (has_flag(src.reg().file, RegisterFile::PHYSICAL_A) || has_flag(src.reg().file, RegisterFile::ACCUMULATOR)) &&
        src.type.getScalarBitCount() == 16)
    {
        // if we sign-extend from register-file A, use unpack-modes
        // this is applied e.g. for unpacking parameters in code-generation, since the source is UNIFORM
        it.emplace(std::make_unique<MoveOperation>(dest, src, conditional, setFlags))
            .setUnpackMode(UNPACK_SHORT_TO_INT_SEXT);
    }
    else
    {
        // out = asr(shl(in, bit_diff) bit_diff)
        // where bit_diff is the difference to full 32-bit
        Literal diffLit(static_cast<int32_t>(32 - src.type.getScalarBitCount()));
        Value widthDiff(diffLit, TYPE_INT8);

        if(!allowLiteral)
        {
            Value tmp = method.addNewLocal(TYPE_INT8, "%sext");
            assign(it, tmp) = load(diffLit);
            widthDiff = tmp;
        }

        Value tmp = assign(it, TYPE_INT32, "%sext") = (src << widthDiff, conditional);
        it.emplace(std::make_unique<Operation>(OP_ASR, dest, tmp, widthDiff, conditional, setFlags));
        // we shifted the exact same number of bits to the left before, so all N trailing bits are zero
        it->addDecorations(InstructionDecorations::EXACT_OPERATION);
    }

    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertSaturation(
    InstructionWalker it, Method& method, const Value& src, const Value& dest, ConversionType type)
{
    // saturation = clamping to min/max of type
    //-> dest = max(min(src, destType.max), destType.min)
    //-> or via pack-modes

    if(!dest.type.isSimpleType() || dest.type.isFloatingType())
        throw CompilationError(CompilationStep::GENERAL, "Invalid target type for saturation", dest.type.to_string());

    bool isInputSigned = type == ConversionType::SIGNED_TO_SIGNED || type == ConversionType::SIGNED_TO_UNSIGNED;
    bool isOutputSigned = type == ConversionType::UNSIGNED_TO_SIGNED || type == ConversionType::SIGNED_TO_SIGNED;

    if(auto lit = src.getLiteralValue())
    {
        switch(dest.type.getScalarBitCount())
        {
        case 8:
            it.emplace(std::make_unique<MoveOperation>(dest,
                           Value(isOutputSigned ? Literal(saturate<int8_t>(lit->signedInt())) :
                                                  Literal(saturate<uint8_t>(lit->unsignedInt())),
                               dest.type)))
                .addDecorations(
                    isOutputSigned ? InstructionDecorations::NONE : InstructionDecorations::UNSIGNED_RESULT);
            return it;
        case 16:
            it.emplace(std::make_unique<MoveOperation>(dest,
                           Value(isOutputSigned ? Literal(saturate<int16_t>(lit->signedInt())) :
                                                  Literal(saturate<uint16_t>(lit->unsignedInt())),
                               dest.type)))
                .addDecorations(
                    isOutputSigned ? InstructionDecorations::NONE : InstructionDecorations::UNSIGNED_RESULT);
            return it;
        case 32:
            it.emplace(std::make_unique<MoveOperation>(dest,
                           Value(isOutputSigned ? Literal(saturate<int32_t>(lit->signedInt())) :
                                                  Literal(saturate<uint32_t>(lit->unsignedInt())),
                               dest.type)))
                .addDecorations(
                    isOutputSigned ? InstructionDecorations::NONE : InstructionDecorations::UNSIGNED_RESULT);
            return it;
        default:
            throw CompilationError(
                CompilationStep::GENERAL, "Invalid target type for saturation", dest.type.to_string());
        }
    }
    else // saturation can be easily done via pack-modes
    {
        // FIXME does not handle 64-bit inputs correctly!
        if(dest.type.getScalarBitCount() == 8)
        {
            Value tmpSrc = src;
            if(!isInputSigned)
                // if unsigned and MSB is set, will interpret as negative signed -> mask off MSB
                tmpSrc = assign(it, src.type) = (src & 0x7FFFFFFF_val, InstructionDecorations::UNSIGNED_RESULT);
            if(isOutputSigned)
            {
                // dest = min(max(src, -128), 127)
                auto tmp = assign(it, src.type) =
                    max(as_signed{tmpSrc}, as_signed{Value(Literal(std::numeric_limits<int8_t>::min()), TYPE_INT8)});
                it.emplace(std::make_unique<Operation>(
                    OP_MIN, dest, tmp, Value(Literal(std::numeric_limits<int8_t>::max()), TYPE_INT8)));
                return it;
            }
            else
            {
                it.emplace(std::make_unique<MoveOperation>(dest, tmpSrc))
                    .setPackMode(PACK_INT_TO_UNSIGNED_CHAR_SATURATE)
                    .addDecorations(InstructionDecorations::UNSIGNED_RESULT);
                return it;
            }
        }
        else if(dest.type.getScalarBitCount() == 16)
        {
            Value tmpSrc = src;
            if(!isInputSigned)
                // if unsigned and MSB is set, will interpret as negative signed -> mask off MSB
                tmpSrc = assign(it, src.type) = (src & 0x7FFFFFFF_val, InstructionDecorations::UNSIGNED_RESULT);
            if(isOutputSigned)
            {
                it.emplace(std::make_unique<MoveOperation>(dest, tmpSrc))
                    .setPackMode(PACK_INT_TO_SIGNED_SHORT_SATURATE);
                return it;
            }
            else
            {
                auto tmp = assign(it, src.type) =
                    max(as_signed{tmpSrc}, as_signed{Value(Literal(std::numeric_limits<uint16_t>::min()), TYPE_INT16)});
                it.emplace(std::make_unique<Operation>(
                    OP_MIN, dest, tmp, Value(Literal(std::numeric_limits<uint16_t>::max()), TYPE_INT16)));
                return it;
            }
        }

        if(isOutputSigned == isInputSigned)
        {
            // signed -> signed or unsigned -> unsigned => move
            it.emplace(std::make_unique<MoveOperation>(dest, src))
                .addDecorations(
                    isOutputSigned ? InstructionDecorations::NONE : InstructionDecorations::UNSIGNED_RESULT);
            return it;
        }

        if(isInputSigned && !isOutputSigned)
        {
            // signed -> unsigned => dest = max(src, 0)
            it.emplace(std::make_unique<Operation>(OP_MAX, dest, src, INT_ZERO))
                .addDecorations(InstructionDecorations::UNSIGNED_RESULT);
            return it;
        }
        if(!isInputSigned && isOutputSigned)
        {
            // unsigned -> signed => dest = MSB(src) ? INT_MAX : src
            auto negativeCond = assignNop(it) = as_signed{src} < as_signed{INT_ZERO};
            assign(it, dest) = (0x7FFFFFFF_val, negativeCond, InstructionDecorations::UNSIGNED_RESULT);
            it.emplace(std::make_unique<MoveOperation>(dest, src, negativeCond.invert()))
                .addDecorations(InstructionDecorations::UNSIGNED_RESULT);
            return it;
        }
        throw CompilationError(CompilationStep::GENERAL, "Saturation to this type is not yet supported",
            "from " + src.type.to_string() + " to " + dest.type.to_string());
    }
}

InstructionWalker intermediate::insertTruncate(
    InstructionWalker it, Method& method, const Value& src, const Value& dest)
{
    if(dest.type.getScalarBitCount() >= src.type.getScalarBitCount())
        //"truncate" to larger type, simply move
        assign(it, dest) = src;
    else
        assign(it, dest) = src & Value(Literal(dest.type.getScalarWidthMask()), TYPE_INT32);

    return it;
}

static InstructionWalker insertFloatToHalfRounding(FloatRoundingMode roundingMode, InstructionWalker it,
    Value& positiveOverflowValue, Value& negativeOverflowValue, const Value& floatExponent, const Value& floatMantissa,
    const Value& signBit, Value& normalResult, Value& denormalResult)
{
    switch(roundingMode)
    {
    case FloatRoundingMode::RINT:
    {
        // overflow handling
        positiveOverflowValue = 0x7C00_val;
        negativeOverflowValue = 0xFC00_val;
        // rounding handling (pack mode rounds away from zero)
        // rint only rounds up if the low bits are > the half (2^12) or exactly the half and the LSB is set (odd)
        // => revert rounding away from zero for low bits <= half (but not zero)
        auto lowBits = assign(it) = (floatMantissa & 0x1FFF_val, SetFlag::SET_FLAGS);
        auto offset = assign(it) = 1_val;
        assign(it, offset) = (0_val, COND_ZERO_SET);
        auto roundDown = assignNop(it) = as_signed{lowBits} <= as_signed{0x1000_val};
        assign(it, normalResult) = (as_signed{normalResult} - as_signed{offset}, roundDown);
        // round denormal values away from zero if low shifted off mantissa bits are > the half or exactly the half
        // and the LSB is set (odd)
        // we need to check the lowest -(13 + exponent - 112 (float bias - half bias)) bits, so shift the others out
        // of the word: 9 (sign + mantissa) + 10 (retained mantissa) - (113 - exponent)
        auto shiftOffset = assign(it) = as_signed{floatExponent} - as_signed{94_val};
        auto cutoff = assign(it) = (floatMantissa << shiftOffset);
        auto denormalOffset = assign(it) = as_unsigned{cutoff} >> 31_val;
        // check mantissa is odd
        assignNop(it) = (denormalResult & 1_val, SetFlag::SET_FLAGS);
        assign(it, denormalResult) = (as_signed{denormalResult} + as_signed{denormalOffset}, COND_ZERO_CLEAR);
        assign(it, denormalOffset) = (0_val, COND_ZERO_CLEAR); // to make sure the offset is not applied twice
        // check cut-off is more than half-way towards to the next value
        assignNop(it) = (cutoff << 1_val, SetFlag::SET_FLAGS);
        assign(it, denormalResult) = (as_signed{denormalResult} + as_signed{denormalOffset}, COND_ZERO_CLEAR);
        break;
    }
    case FloatRoundingMode::TRUNC:
        // overflow handling
        positiveOverflowValue = 0x7BFF_val;
        negativeOverflowValue = 0xFBFF_val;
        // rounding handling (pack mode rounds away from zero)
        // if we have a bit in the lower part set, the pack mode did round away from zero, so revert this
        assignNop(it) = (floatMantissa & 0x1FFF_val, SetFlag::SET_FLAGS);
        assign(it, normalResult) = (as_signed{normalResult} - as_signed{1_val}, COND_ZERO_CLEAR);
        // denormal values are already truncated to zero in our custom code
        break;
    case FloatRoundingMode::CEIL:
    {
        // overflow handling
        positiveOverflowValue = 0x7C00_val;
        negativeOverflowValue = 0xFBFF_val;
        // rounding handling (pack mode rounds away from zero)
        // => revert rounding if the sign bit is set (and we did actually round == low bits are not zero)
        auto offset = assign(it) = as_unsigned{signBit} >> 15_val;
        assignNop(it) = (floatMantissa & 0x1FFF_val, SetFlag::SET_FLAGS);
        assign(it, normalResult) = (as_signed{normalResult} - as_signed{offset}, COND_ZERO_CLEAR);
        // for denormal values, round for positive value and shifted off mantissa bits not zero
        auto denormalOffset = assign(it) = offset ^ 1_val;
        // we need to check the lowest -(13 + exponent - 112 (float bias - half bias)) bits, so shift the others out
        // of the word: 9 (sign + mantissa) + 10 (retained mantissa) - (113 - exponent)
        auto shiftOffset = assign(it) = as_signed{floatExponent} - as_signed{94_val};
        assignNop(it) = (floatMantissa << shiftOffset, SetFlag::SET_FLAGS);
        assign(it, denormalResult) = (as_signed{denormalResult} + as_signed{denormalOffset}, COND_ZERO_CLEAR);
        break;
    }
    case FloatRoundingMode::FLOOR:
    {
        // overflow handling
        positiveOverflowValue = 0x7BFF_val;
        negativeOverflowValue = 0xFC00_val;
        // rounding handling (pack mode rounds away from zero)
        // => revert rounding if the sign bit is clear (and we did actually round == low bits are not zero)
        assignNop(it) = (floatMantissa & 0x1FFF_val, SetFlag::SET_FLAGS);
        auto denormalOffset = assign(it) = as_unsigned{signBit} >> 15_val;
        auto offset = assign(it) = denormalOffset ^ 1_val;
        assign(it, normalResult) = (as_signed{normalResult} - as_signed{offset}, COND_ZERO_CLEAR);
        // for denormal values, round for negative value and shifted off mantissa bits not zero
        // we need to check the lowest -(13 + exponent - 112 (float bias - half bias)) bits, so shift the others out
        // of the word: 9 (sign + mantissa) + 10 (retained mantissa) - (113 - exponent)
        auto shiftOffset = assign(it) = as_signed{floatExponent} - as_signed{94_val};
        assignNop(it) = (floatMantissa << shiftOffset, SetFlag::SET_FLAGS);
        assign(it, denormalResult) = (as_signed{denormalResult} + as_signed{denormalOffset}, COND_ZERO_CLEAR);
        break;
    }
    }
    return it;
}

InstructionWalker intermediate::insertFloatingPointConversion(
    InstructionWalker it, Method& method, const Value& src, const Value& dest, FloatRoundingMode roundingMode)
{
    if(src.type.getScalarBitCount() == dest.type.getScalarBitCount())
        assign(it, dest) = src;
    else if(src.type.getScalarBitCount() == 16 && dest.type.getScalarBitCount() == 32)
    {
        /*
         * The half -> float unpack mode does not handle denormal values correctly, so we need to manually convert them:
         *
         * 1. mask off sign-bit, count leading zeroes of mantissa
         * 2. set exponent to 127 - 15 - leading zeroes of mantissa
         * 3. left-align mantissa and convert explicit high-bit to implicit bit
         *    unpack mode already shifts mantissa 13 to left (23 bit float mantissa - 10 bit half mantissa)
         * 4. assemble denormal value
         * 5. decide on whether to use denormal or default/normal value
         */
        auto UNSIGNED = InstructionDecorations::UNSIGNED_RESULT;
        auto normalResult = assign(it, dest.type, "%normal_result") =
            (max(as_float{src}, as_float{src}), UNPACK_HALF_TO_FLOAT);

        auto unsignedResult = assign(it) = (normalResult & 0x7FFFFFFF_val, UNSIGNED);
        auto leadingZeroes = assign(it) = clz(unsignedResult);
        // leading zeroes in mantissa = clz(32-bit value) - 9 (sign + exponent)
        auto leadingZeroesPlusOne = assign(it, leadingZeroes.type) = leadingZeroes - 8_val;
        // exponent = 127 - 15 - (clz - 9) = 112 - (clz - 9) = 113 - (clz - 8)
        auto denormalExponent = assign(it, "denormal_exponent") = (113_val - leadingZeroesPlusOne, UNSIGNED);
        // mantissa = mantissa << leading zeroes in mantissa + 1 (implicit high-bit)
        auto denormalMantissa = assign(it) = (unsignedResult << leadingZeroesPlusOne, UNSIGNED);
        denormalMantissa = assign(it, "%denormal_mantissa") = (denormalMantissa & 0x7FFFFF_val, UNSIGNED);

        auto denormalResult = assign(it, dest.type, "%denormal_result") = normalResult & 0x80000000_val;
        auto cond = assignNop(it) = as_unsigned{unsignedResult} != as_unsigned{0_val};
        denormalExponent = assign(it) = denormalExponent << 23_val;
        assign(it, denormalResult) = (denormalResult | denormalExponent, cond);
        assign(it, denormalResult) = (denormalResult | denormalMantissa, cond);
        assign(it, dest) = normalResult;
        cond = assignNop(it) = as_signed{leadingZeroesPlusOne} >= as_signed{1_val};
        assign(it, dest) = (denormalResult, cond);
    }
    else if(src.type.getScalarBitCount() == 32 && dest.type.getScalarBitCount() == 16)
    {
        auto normalResult = assign(it, dest.type, "%normal_result") =
            (max(as_float{src}, as_float{src}), PACK_FLOAT_TO_HALF_TRUNCATE);
        /*
         * The float -> half pack mode does not handle some cases correctly, so we need to fix them up. Namely the
         * VideoCore IV GPU has following deviating behavior:
         *
         * - All (half) denormal values and -0 are flushed to +0
         *   -> distinguish them (+ rounding)
         * - +/- NaN is converted to +/- Inf
         *   -> distinguish them
         * - any out-of-range value is converted to +/- Inf
         *   -> convert to HALF_MAX/-HALF_MAX depending on rounding mode
         * - rounding is always done away from zero
         *   -> fix-up depending on rounding mode
         *
         * => Need to manually apply changes if:
         * - Output is +0 and input is not +0
         * - Output is +/- Inf and input is not +/- Inf
         * - Any of the lower 13 input bits are set (depending on rounding mode)
         */
        auto vectorWidth = dest.type.getVectorWidth();
        auto UNSIGNED = InstructionDecorations::UNSIGNED_RESULT;
        auto unsignedBits = assign(it) = (src & 0x7FFFFFFF_val, UNSIGNED);
        auto floatExponent = assign(it, "%float_exponent") = (as_unsigned{unsignedBits} >> 23_val, UNSIGNED);
        auto signBit = assign(it) = src & 0x80000000_val;
        signBit = assign(it, TYPE_INT16.toVectorType(vectorWidth)) = as_unsigned{signBit} >> 16_val;
        auto floatMantissa = assign(it, "%float_mantissa") = (unsignedBits & 0x007FFFFF_val, SetFlag::SET_FLAGS);

        // handling of +/-Inf and +/- NaN
        auto realInfResult = assign(it, dest.type, "%inf_nan_result") = signBit | 0x7C00_val;
        // all NaNs are equal, so it does not matter which code we return
        assign(it, realInfResult) = (realInfResult | 0x200_val, COND_ZERO_CLEAR);

        // handling of +/- 0 and (half) denormal value
        auto denormalResult = assign(it, dest.type) = signBit;
        // make implicit leading 1 in mantissa explicit for normal -> denormal values
        auto denormalMantissa = assign(it) = floatMantissa | 0x800000_val;
        // shift the mantissa by 14 (normal mantissa length difference + implicit/explicit leading bit) - (exponent -
        // 127 (float bias) + 15(half bias))
        auto shiftOffset = assign(it) = as_signed{126_val} - as_signed{floatExponent};
        denormalMantissa = assign(it, dest.type, "denormal_mantissa") =
            (as_unsigned{denormalMantissa} >> shiftOffset, SetFlag::SET_FLAGS);
        denormalResult = assign(it, dest.type, "%denormal_result") = denormalResult | denormalMantissa;

        // apply rounding and provide out-of-bounds values
        auto positiveOverflowValue = UNDEFINED_VALUE;
        auto negativeOverflowValue = UNDEFINED_VALUE;
        it = insertFloatToHalfRounding(roundingMode, it, positiveOverflowValue, negativeOverflowValue, floatExponent,
            floatMantissa, signBit, normalResult, denormalResult);

        // handling of |finite value| > HALF_MAX
        auto isSigned = assignNop(it) = as_unsigned{signBit} != as_unsigned{0_val};
        auto realOverflowResult = assign(it, dest.type, "%overflow_result") = positiveOverflowValue;
        assign(it, realOverflowResult) = (negativeOverflowValue, isSigned);

        assign(it, dest) = normalResult;
        // any float exponent <= 112 (127 float bias - 15 half bias) is a denormal (or zero) half value
        auto isDenormalOrZero = assignNop(it) = as_signed{floatExponent} <= as_signed{112_val};
        assign(it, dest) = (denormalResult, isDenormalOrZero);
        auto isOverflow = assignNop(it) = as_signed{floatExponent} > as_signed{142_val};
        assign(it, dest) = (realOverflowResult, isOverflow);
        // this overwrites the isOverflow check, so it has to come afterwards
        auto isInfNaN = assignNop(it) = as_unsigned{floatExponent} == as_unsigned{0xFF_val};
        assign(it, dest) = (realInfResult, isInfNaN);
    }
    else
        throw CompilationError(CompilationStep::GENERAL, "Unsupported floating-point conversion",
            src.to_string() + " to " + dest.to_string());
    return it;
}

InstructionWalker intermediate::insertFloatToIntegerSaturation(
    InstructionWalker it, Method& method, const Value& src, const Value& dest, int32_t minInt, uint32_t maxInt)
{
    auto maxFloat = Value(Literal(static_cast<float>(maxInt)), TYPE_FLOAT);
    auto maxInteger = Value(Literal(maxInt), dest.type);
    auto minFloat = Value(Literal(static_cast<float>(minInt)), TYPE_FLOAT);
    auto minInteger = Value(Literal(minInt), dest.type);

    // default -> dest = ftoi(src)
    assign(it, dest) = ftoi(src);
    // src >= itof(max) -> dest = max
    auto overflowCond = assignNop(it) = as_float{src} >= as_float{maxFloat};
    assign(it, dest) = (maxInteger, overflowCond);
    // src <= itof(min) -> dest = min
    auto underflowCond = assignNop(it) = as_float{src} <= as_float{minFloat};
    assign(it, dest) = (minInteger, underflowCond);
    return it;
}

InstructionWalker intermediate::insertUnsignedToFloatConversion(
    InstructionWalker it, Method& method, const Value& src, const Value& dest)
{
    if(src.type.getScalarBitCount() < 32)
    {
        // make sure, leading bits are zeroes
        const uint32_t mask = src.type.getScalarWidthMask();
        auto tmp = assign(it, dest.type, "%uitofp") = (src & Value(Literal(mask), TYPE_INT32));
        assign(it, dest) = itof(tmp);
    }
    else if(src.type.getScalarBitCount() > 32)
    {
        auto parts = Local::getLocalData<MultiRegisterData>(src.checkLocal());
        if(!parts)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Can't convert long to floating value without having multi-register data", it->to_string());

        auto upperPart = method.addNewLocal(dest.type);
        it = insertUnsignedToFloatConversion(it, method, parts->upper->createReference(), upperPart);
        auto lowerPart = method.addNewLocal(dest.type);
        it = insertUnsignedToFloatConversion(it, method, parts->lower->createReference(), lowerPart);
        auto tmp = assign(it, upperPart.type) = upperPart * Value(Literal(std::pow(2.0f, 32.0f)), TYPE_FLOAT);
        assign(it, dest) = tmp + lowerPart;
    }
    else // 32-bits
    {
        // TODO sometimes is off by 1 ULP, e.g. for 1698773569 gets 1698773504 instead of expected 1698773632
        // uitof(x) = y * uitof(x/y) + uitof(x & |y|), where |y| is the bits for y
        auto tmpInt = assign(it, src.type) = src / 2_lit;
        auto tmpFloat = method.addNewLocal(dest.type);
        assign(it, tmpFloat) = itof(tmpInt);
        auto tmpFloat2 = assign(it, tmpFloat.type) = tmpFloat * Value(Literal(2.0f), TYPE_FLOAT);
        auto tmpInt2 = assign(it, src.type) = src % 2_lit;
        auto tmpFloat3 = method.addNewLocal(dest.type);
        assign(it, tmpFloat3) = itof(tmpInt2);
        assign(it, dest) = as_float{tmpFloat2} + as_float{tmpFloat3};
    }
    return it;
}

InstructionWalker intermediate::insertSignedToFloatConversion(
    InstructionWalker it, Method& method, const Value& src, const Value& dest)
{
    if(src.type.getScalarBitCount() > 32)
    {
        auto parts = Local::getLocalData<MultiRegisterData>(src.checkLocal());
        if(!parts)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Can't convert long to floating value without having multi-register data", it->to_string());

        auto upperPart = assign(it, dest.type) = itof(parts->upper->createReference());
        upperPart = assign(it, upperPart.type) = upperPart * Value(Literal(std::pow(2.0f, 32.0f)), TYPE_FLOAT);
        /*
         * Case handling:
         * - Upper part positive -> normal flow (upper part to float * 2^32 + unsigned lower part to float)
         * - Upper part all zeroes -> normal flow (lower part is zero-extended and already handled as unsigned)
         * - Upper part negative -> inverted flow (upper part to float * 2^32 - |lower part| to float)
         * - Upper part all ones -> ignore upper part
         */
        auto lowerAbs = method.addNewLocal(parts->lower->type);
        Value dummySign = UNDEFINED_VALUE;
        it = insertMakePositive(it, method, parts->lower->createReference(), lowerAbs, dummySign);
        auto lowerInput = assign(it, parts->lower->type) = parts->lower->createReference();
        auto cond = assignNop(it) = as_signed{parts->upper->createReference()} < as_signed{0_val};
        assign(it, lowerInput) = (lowerAbs, cond);

        // convert the actual (absolute value of the) lower part
        auto lowerPart = method.addNewLocal(dest.type);
        it = intermediate::insertUnsignedToFloatConversion(it, method, lowerInput, lowerPart);

        // if upper part was negative, subtract, otherwise add the parts together
        cond = assignNop(it) = as_signed{parts->upper->createReference()} < as_signed{0_val};
        assign(it, lowerPart) = (-as_float{lowerPart}, cond);
        // if upper part is all ones, do not add it (since it is only the sign, no value)
        cond = assignNop(it) = as_signed{parts->upper->createReference()} == as_signed{0xFFFFFFFF_val};
        assign(it, upperPart) = (FLOAT_ZERO, cond);
        assign(it, dest) = as_float{upperPart} + as_float{lowerPart};
        return it;
    }

    // for non 32-bit types, need to sign-extend
    auto intValue = src;
    if(src.type.getScalarBitCount() < 32)
    {
        intValue = method.addNewLocal(TYPE_INT32, "%sitofp");
        it = insertSignExtension(it, method, src, intValue, true);
    }

    assign(it, dest) = itof(intValue);
    return it;
}
