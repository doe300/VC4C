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
         * Combine consecutive configuration of VPW/VPR with the same settings
         *
         * In detail, this combines VPM read/writes of uniform type of access (read or write), uniform data-type and
         * consecutive memory-addresses
         *
         * NOTE: Combining VPM accesses merges their mutex-lock blocks which can cause other QPUs to stall for a long
         * time. Also, this optimization currently only supports access memory <-> QPU, data exchange between only
         * memory and VPM are not optimized
         */
        bool groupVPMAccess(const Module& module, Method& method, const Configuration& config);

        /**
         * Tries to find TMU loads within loops where we can pre-calculate the address for loads in the next loop
         * iteration and thus we can pre-fetch the data loaded for the next loop iteration into the TMU FIFO.
         */
        bool prefetchTMULoads(const Module& module, Method& method, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c

#endif /* VC4C_OPTIMIZATION_MEMORY_H */