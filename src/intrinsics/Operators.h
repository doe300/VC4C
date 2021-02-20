/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPERATORS_H
#define OPERATORS_H

#include "../InstructionWalker.h"

#include <cfenv>
#include <cmath>
#include <limits>

namespace vc4c
{
    namespace intrinsics
    {
        NODISCARD InstructionWalker intrinsifySignedIntegerMultiplication(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op);
        bool canOptimizeMultiplicationWithBinaryMethod(const intermediate::IntrinsicOperation& op);
        NODISCARD InstructionWalker intrinsifyUnsignedIntegerMultiplication(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op);
        NODISCARD InstructionWalker intrinsifyLongMultiplication(
            Method& method, InstructionWalker it, const intermediate::IntrinsicOperation& op);
        /**
         * Returns 64-bit result of full 32-bit multiplication, storing the upper part in the output of the given method
         * call and the (optional) lower part into the given additional value.
         */
        NODISCARD InstructionWalker intrinsifyIntegerToLongMultiplication(Method& method, InstructionWalker it,
            const intermediate::MethodCall* call, Optional<Value> lowResult = NO_VALUE);
        NODISCARD InstructionWalker intrinsifyIntegerMultiplicationViaBinaryMethod(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op);
        NODISCARD InstructionWalker intrinsifySignedIntegerDivision(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op, bool useRemainder = false);
        NODISCARD InstructionWalker intrinsifyUnsignedIntegerDivision(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op, bool useRemainder = false);
        NODISCARD InstructionWalker intrinsifySignedIntegerDivisionByConstant(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op, bool useRemainder = false);
        NODISCARD InstructionWalker intrinsifyUnsignedIntegerDivisionByConstant(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op, bool useRemainder = false);
        NODISCARD InstructionWalker intrinsifyIntegerDivisionByFloatingDivision(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op, bool useRemainder = false);

        NODISCARD InstructionWalker intrinsifyFloatingDivision(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op, bool fullRangeDivision = true);

        /*
         * Implementations for on-host calculations
         */

        Literal asr(Literal left, Literal right);
        Literal clz(Literal val);
        /*
         * OpSMod: "Signed modulo operation of Operand 1 modulo Operand 2. The sign of a non-0 result comes from
         * Operand 2."
         */
        Literal smod(DataType type, const Literal& numerator, const Literal& denominator);
        /*
         * OpSRem: "Signed remainder operation of Operand 1 divided by Operand 2. The sign of a non-0 result comes from
         * Operand 1."
         */
        Literal srem(DataType type, const Literal& numerator, const Literal& denominator);
        /*
         * OpFMod: "Floating-point remainder operation of Operand 1 divided by Operand 2. The sign of a non-0 result
         * comes from Operand 2."
         */
        Literal fmod(DataType type, const Literal& numerator, const Literal& denominator);
        /*
         * OpFRem: "Floating-point remainder operation of Operand 1 divided by Operand 2. The sign of a non-0 result
         * comes from	Operand 1."
         */
        Literal frem(DataType type, const Literal& numerator, const Literal& denominator);

    } // namespace intrinsics

    constexpr float flushDenorms(float result)
    {
        if(std::abs(result) < std::numeric_limits<float>::min())
            // denorms seem to be always flushed to +0.0
            return 0.0f;
        return result;
    }

    /**
     * Helper type for float calculations to flush denormal values to zero and set the truncate-to-zero rounding mode
     * used by the VideoCore IV GPU.
     */
    template <typename R, typename T, typename F = R(T, T)>
    struct FlushDenormsAndRoundToZero
    {
    };

    template <typename F>
    struct FlushDenormsAndRoundToZero<float, float, F>
    {
        explicit FlushDenormsAndRoundToZero(F f = {}) : func(f) {}

        float operator()(float arg0, float arg1) const
        {
            auto origMode = fegetround();
            // emulate the VideoCore IV rounding mode, truncate to zero
            fesetround(FE_TOWARDZERO);
            auto tmp = func(flushDenorms(arg0), flushDenorms(arg1));
            fesetround(origMode);
            return flushDenorms(tmp);
        }

        F func;
    };

    template <typename F>
    struct FlushDenormsAndRoundToZero<float, int32_t, F>
    {
        explicit FlushDenormsAndRoundToZero(F f = {}) : func(f) {}

        float operator()(int32_t arg0, int32_t arg1) const
        {
            auto origMode = fegetround();
            // emulate the VideoCore IV rounding mode, truncate to zero
            fesetround(FE_TOWARDZERO);
            auto tmp = func(arg0, arg1);
            fesetround(origMode);
            return flushDenorms(tmp);
        }

        F func;
    };

    /**
     * Helper type to apply type conversion using the truncate-to-zero rounding mode used by the VideoCore IV GPU.
     */
    template <typename In, typename Out>
    struct RoundToZeroConversion
    {
        Out operator()(In val) const
        {
            auto origMode = fegetround();
            // emulate the VideoCore IV rounding mode, truncate to zero
            fesetround(FE_TOWARDZERO);
            auto tmp = static_cast<Out>(val);
            fesetround(origMode);
            return tmp;
        }
    };
} // namespace vc4c

#endif /* OPERATORS_H */
