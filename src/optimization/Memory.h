/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_OPTIMIZATION_MEMORY_H
#define VC4C_OPTIMIZATION_MEMORY_H

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace optimizations
    {
        /**
         * Maps all intermediate memory access instructions to the actual hardware instructions used to perform the
         * given memory access.
         *
         * NOTE: This is not actually an optimization (but rather a normalization) step, but for allowing some
         * optimizations to access the more detailed information provided by MemoryAccessInstructions and other
         * optimizations to rewrite the low-level access functions requires this to be run in between the optimization
         * steps.
         */
        bool lowerMemoryAccess(const Module& module, Method& method, const Configuration& config);

        /**
         * Tries to find and group memory accesses to reduce the number of accesses while increasing utilization.
         */
        bool groupMemoryAccess(const Module& module, Method& method, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c

#endif /* VC4C_OPTIMIZATION_MEMORY_H */