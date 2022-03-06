/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef REORDERING_H
#define REORDERING_H

#include <cstdint>

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace optimizations
    {
        /*
         * Splits up writing of a local directly followed by an instruction reading it if the local is unlikely to be
         * mapped to an accumulator by inserting nop-instructions. This optimization-pass on its own is actually an
         * de-optimization, but is required for the #reorderWithinBasicBlocks pass to work properly.
         */
        std::size_t splitReadAfterWrites(const Module& module, Method& method, const Configuration& config);

        /*
         * Removes nop-instructions inserted for various reasons (waiting on TMU, SFU, splitting up read-after-write) by
         * replacing them with other instructions from within the same basic block. The selection of the instruction to
         * insert guarantees semantic equivalence by checking all value and periphery dependencies.
         *
         * Example:
         *   %5 = add %3, %4
         *   nop
         *   %6 = sub %2, %5
         *   %7 = mul24 %2, %3
         *
         * is converted to:
         *   %5 = add %3, %4
         *   %7 = mul24 %2, %3
         *   %6 = sub %2, %5
         *
         * NOTE: This optimization is a very limited implementation of instruction-scheduling and should be replaced by
         * a more general version which can actually re-order instructions
         */
        std::size_t reorderWithinBasicBlocks(const Module& module, Method& method, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c

#endif /* REORDERING_H */
