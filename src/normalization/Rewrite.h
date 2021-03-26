/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_REWRITE_H
#define VC4C_REWRITE_H

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;
    struct Configuration;

    namespace normalization
    {
        /**
         * Splits up register conflicts when values fixed to a given register are used together (e.g. via Unpack mode)
         *
         * Example:
         *   %2 = %1 (pack)
         *   %4 = %5 (pack)
         *   %3 = add %2, %1
         *
         * is converted to:
         *   %2 = %1 (pack)
         *   %4 = %5 (pack)
         *   %tmp = %2
         *   %3 = add %tmp, %1
         *
         */
        InstructionWalker splitRegisterConflicts(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /*
         * Extends the branches (up to now represented by a single instruction) by
         * inserting instructions setting the necessary flags (if required)
         * and the subsequent delay-instructions required to empty the pipeline
         *
         * Example:
         *   br %103
         *
         * is converted to:
         *   br %103
         *   nop
         *   nop
         *   nop
         */
        void extendBranches(const Module& module, Method& method, const Configuration& config);

        void eliminatePhiNodes(const Module& module, Method& method, const Configuration& config);

        /**
         * Prevents register-mapping errors by guaranteeing the source of a vector-rotation to be mappable to an
         * accumulator. To do this, long-living used in a vector-rotation are moved to a temporary local which then can
         * be mapped to an accumulator.
         */
        InstructionWalker moveRotationSourcesToAccumulators(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);
    } // namespace normalization
} // namespace vc4c

#endif /* VC4C_REWRITE_H */
