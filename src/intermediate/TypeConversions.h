/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TYPE_CONVERSIONS_H
#define TYPE_CONVERSIONS_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace intermediate
    {
        NODISCARD InstructionWalker insertBitcast(InstructionWalker it, Method& method, const Value& src,
            const Value& dest, InstructionDecorations deco = InstructionDecorations::NONE, uint32_t elementStride = 1u,
            bool zeroOutput = false);
        NODISCARD InstructionWalker insertZeroExtension(InstructionWalker it, Method& method, const Value& src,
            const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS,
            SetFlag setFlags = SetFlag::DONT_SET);
        NODISCARD InstructionWalker insertSignExtension(InstructionWalker it, Method& method, const Value& src,
            const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS,
            SetFlag setFlags = SetFlag::DONT_SET);

        enum class ConversionType
        {
            SIGNED_TO_SIGNED,
            UNSIGNED_TO_UNSIGNED,
            SIGNED_TO_UNSIGNED,
            UNSIGNED_TO_SIGNED
        };

        enum class FloatRoundingMode : uint8_t
        {
            // round towards nearest even
            RINT,
            // round towards positive infinity
            CEIL,
            // round towards negative infinity
            FLOOR,
            // round towards zero (this is the default as defined by CL_FP_ROUND_TO_ZERO in our VC4CL device)
            TRUNC
        };

        NODISCARD InstructionWalker insertSaturation(
            InstructionWalker it, Method& method, const Value& src, const Value& dest, ConversionType type);
        NODISCARD InstructionWalker insertTruncate(
            InstructionWalker it, Method& method, const Value& src, const Value& dest);
        NODISCARD InstructionWalker insertFloatingPointConversion(InstructionWalker it, Method& method,
            const Value& src, const Value& dest, FloatRoundingMode roundingMode = FloatRoundingMode::TRUNC);
        NODISCARD InstructionWalker insertFloatToIntegerSaturation(InstructionWalker it, Method& method,
            const Value& src, const Value& dest, int32_t minInt = std::numeric_limits<int32_t>::min(),
            uint32_t maxInt = std::numeric_limits<int32_t>::max());
        NODISCARD InstructionWalker insertUnsignedToFloatConversion(
            InstructionWalker it, Method& method, const Value& src, const Value& dest);
        NODISCARD InstructionWalker insertSignedToFloatConversion(
            InstructionWalker it, Method& method, const Value& src, const Value& dest);
    } /* namespace intermediate */

} /* namespace vc4c */

#endif /* TYPE_CONVERSIONS_H */
