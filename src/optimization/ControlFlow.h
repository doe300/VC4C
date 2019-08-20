/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_OPTIMIZATION_CONTROLFLOW_H
#define VC4C_OPTIMIZATION_CONTROLFLOW_H

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace optimizations
    {
        /*
         * Tries to find loops which then can be vectorized by combining multiple iterations into one.
         *
         * NOTE: Currently only works with "standard" for-range loops and needs to be enabled explicitly in the
         * Configuration
         */
        bool vectorizeLoops(const Module& module, Method& method, const Configuration& config);

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
        bool removeConstantLoadInLoops(const Module& module, Method& method, const Configuration& config);

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
        bool mergeAdjacentBasicBlocks(const Module& module, Method& method, const Configuration& config);

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
        bool reorderBasicBlocks(const Module& module, Method& method, const Configuration& config);

        /**
         * Simplifies selected conditional control-flow patterns (e.g. if-else, switch-case) by rewriting the control
         * flow to conditional execution.
         *
         * Example:
         *   - = xor.setf %a, 15
         *   br.ifz %1
         *   - = xor.setf %a, 42
         *   br.ifz %2
         *   br %3
         *   [...]
         *   label: %1
         *   %c = 13
         *   br %4
         *   label: %2
         *   %b = ...
         *   %c = mul24 %b, 112
         *   br %4
         *   label: %3
         *   [...]
         *   %c = 42
         *   br %4
         *   [...]
         *   label: %4
         *
         * is converted to:
         *   [...]
         *   %c = 42
         *   - = xor.setf %a, 15
         *   %c = 13 (ifz)
         *   - = xor.setf %a, 42
         *   %b = ...
         *   %c = mul24.ifz %b, 112
         *   br %4
         *   [...]
         *   label: %4
         */
        bool simplifyConditionalBlocks(const Module& module, Method& method, const Configuration& config);

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
        bool addWorkGroupLoop(const Module& module, Method& method, const Configuration& config);

    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_OPTIMIZATION_CONTROLFLOW_H */
