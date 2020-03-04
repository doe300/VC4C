/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPERATORS_H
#define OPERATORS_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace intrinsics
    {
        NODISCARD InstructionWalker intrinsifySignedIntegerMultiplication(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op);
        bool canOptimizeMultiplicationWithBinaryMethod(const intermediate::IntrinsicOperation& op);
        NODISCARD InstructionWalker intrinsifyUnsignedIntegerMultiplication(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op);
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

        NODISCARD InstructionWalker intrinsifyFloatingDivision(
            Method& method, InstructionWalker it, intermediate::IntrinsicOperation& op);

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
} // namespace vc4c

#endif /* OPERATORS_H */
