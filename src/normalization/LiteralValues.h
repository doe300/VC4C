/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LITERALVALUES_H
#define LITERALVALUES_H

#include "../Values.h"

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;
    struct Configuration;

    namespace normalization
    {
        /*
         * Handles the use of literal containers (e.g. literal constant vectors) by inserting the single elements at
         * their positions.
         *
         * Also optimizes some special cases, like rotation of literal vector or uniform vectors (all elements are the
         * same)
         */
        InstructionWalker handleContainer(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);
        /*
         * Handles the use of literal operands by either converting them to SmallImmediates or loading them into
         * temporary locals as 32-bit literal values
         *
         * Example:
         *   %3 = add 17, %2
         *
         * is converted to:
         *   %immediate = loadi 17
         *   %3 = add %immediate, %2
         *
         * NOTE: This optimization-step is required for the further compilation to work correctly
         */
        InstructionWalker handleImmediate(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);
        /*
         * Facilitates the register-mapping by rewriting operations, where one operand is a SmallImmediate and the other
         * is a local which is unlikely to be mapped to accumulators by introducing a temporary local which is then used
         * together with the SmallImmediate and should be mapped to an accumulator.
         *
         * Example:
         *   %2 = add 1, %1
         *
         * is converted to:
         *   %use_with_literal = %1
         *   %2 = add 1, %use_with_literal
         *
         * NOTE: Skipping this step results in many (possibly long-living and often-used) locals being forced to
         * physical register-file A, since the physical file B is used for the SmallImmediate
         */
        void handleUseWithImmediate(Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /**
         * Returns the small immediate representation of the given literal, if it can be represented as such
         */
        Optional<SmallImmediate> toImmediate(Literal lit);
    } // namespace normalization
} // namespace vc4c

#endif /* LITERALVALUES_H */
