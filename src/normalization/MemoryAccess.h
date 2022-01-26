/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPTIMIZATION_MEMORYACCESS_H
#define OPTIMIZATION_MEMORYACCESS_H

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;
    struct Configuration;

    namespace normalization
    {
        /*
         * Replaces the address of global data with the corresponding offset from the GLOBAL_DATA_ADDRESS value
         */
        void accessGlobalData(Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /*
         * Handles stack allocations:
         * - calculates the offsets from the start of one QPU's "stack"
         * - removes the life-time instructions
         * - maps the addresses to offsets from global-data pointer (see #accessGlobalData)
         *
         * NOTE: This optimization-pass is required for the compiler to handle stack-allocations correctly
         */
        void resolveStackAllocation(Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /*
         * Maps the memory-instructions to instructions actually performing the memory-access (e.g. TMU, VPM access).
         *
         * This optimization-step also contains most of the optimizations for accessing VPM/RAM.
         */
        void mapMemoryAccess(const Module& module, Method& method, const Configuration& config);
    } // namespace normalization
} // namespace vc4c

#endif /* OPTIMIZATION_MEMORYACCESS_H */
