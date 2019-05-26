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
            const Value& dest, InstructionDecorations deco = InstructionDecorations::NONE);
        NODISCARD InstructionWalker insertZeroExtension(InstructionWalker it, Method& method, const Value& src,
            const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS,
            SetFlag setFlags = SetFlag::DONT_SET);
        NODISCARD InstructionWalker insertSignExtension(InstructionWalker it, Method& method, const Value& src,
            const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS,
            SetFlag setFlags = SetFlag::DONT_SET);
        NODISCARD InstructionWalker insertSaturation(
            InstructionWalker it, Method& method, const Value& src, const Value& dest, bool isSigned);
        NODISCARD InstructionWalker insertTruncate(
            InstructionWalker it, Method& method, const Value& src, const Value& dest);
        NODISCARD InstructionWalker insertFloatingPointConversion(
            InstructionWalker it, Method& method, const Value& src, const Value& dest);
    } /* namespace intermediate */

} /* namespace vc4c */

#endif /* TYPE_CONVERSIONS_H */
