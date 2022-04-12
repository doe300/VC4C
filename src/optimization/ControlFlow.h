/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_OPTIMIZATION_CONTROLFLOW_H
#define VC4C_OPTIMIZATION_CONTROLFLOW_H

#include <cstdint>

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace optimizations
    {
        /*
         * Adds the start- and stop-segment to the kernel code
         *
         * The start-segment contains the code to read the kernel-parameters as well as the "hidden" parameters
         * (work-item and work-group info, address of global data, etc.) The stop-segment contains instructions to
         * trigger the host-interrupt to notify VC4CL that this execution is finished and generates a signal to let the
         * QPu know the same
         */
        void addStartStopSegment(const Module& module, Method& method, const Configuration& config);

        /*
         * Move constant load operations in (nested) loops to the block before head block of the outer-most loop.
         */
        std::size_t moveLoopInvariantCode(const Module& module, Method& method, const Configuration& config);

        /*
         * Concatenates "adjacent" basic blocks if the preceding block has only one successor and the succeeding block
         * has only one predecessor.
         *
         * This occurs usually at least 2 times: for start-of function and first block, last block and end-of-function.
         *
         * Example:
         *   label: %start_of_function
         *   xxx
         *   label: %entry
         *   yyy
         *
         * is converted to:
         *   label: %start_of_function
         *   xxx
         *   yyy
         *
         */
        std::size_t mergeAdjacentBasicBlocks(const Module& module, Method& method, const Configuration& config);

        /*
         * Reorders basic blocks so as much transitions as possible can be replaced by implicit transitions in
         * #simplifyBranches.
         *
         * Example:
         *   br %4
         *   label: %3
         *   [...]
         *   br %5
         *   label: %4
         *   [...]
         *   br %3
         *   label: %5
         *
         * is converted to:
         *   br %4
         *   label: %4
         *   [...]
         *   br %3
         *   label: %3
         *   [...]
         *   br %5
         *   label: %5
         */
        std::size_t reorderBasicBlocks(const Module& module, Method& method, const Configuration& config);

        /**
         * Extends kernel code to be able to run for all work-groups without the need to return to host-code.
         *
         * Inserts a control-flow loop per dimension (X, Y, Z) of the group id around the kernel code. Depending on the
         * current and maximum group id (per dimension), the kernel code is repeated for the next group.
         *
         * To not require duplication of the UNIFORM values host-side (and also improve UNIFORM cache hits), the UNIFORM
         * pointer is reset after every kernel execution to the same UNIFORM values used for the previous execution.
         *
         * Also moves the group id variables to the start of the kernel to be only loaded once.
         *
         * Generates:
         * label: %start_of_kernel
         * %gid_x = 0
         * %gid_y = 0
         * %gid_z = 0
         *
         * label: %start_of_kernel_content
         *
         * [... real kernel content]
         *
         * %uniform_ptr = uniform
         * %max_gid_x = uniform
         * %max_gid_y = uniform
         * %max_gid_z = uniform
         * uniform_address = %uniform_ptr
         *
         * %gid_x += 1
         * br %start_of_kernel_content if %gid_x < %max_gid_x
         *
         * label: ...
         * %gid_x = 0
         * %gid_y += 1
         * br %start_of_kernel_content if %gid_y < %max_gid_y
         *
         * label: ...
         * %gix_y = 0
         * %gid_z += 1
         * br %start_of_kernel_content if %gid_z < %max_gid_z
         *
         * label: %end_of_kernel
         * [.. stop segment]
         *
         *
         *
         * NOTE: As of this step, there are several control-flow loop around the whole kernel code.
         * NOTE: This optimization step might increase register pressure!
         */
        std::size_t addWorkGroupLoop(const Module& module, Method& method, const Configuration& config);

    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_OPTIMIZATION_CONTROLFLOW_H */
