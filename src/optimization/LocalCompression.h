/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LOCAL_COMPRESSION_H
#define VC4C_LOCAL_COMPRESSION_H

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace optimizations
    {
        /*
         * Compresses the locals for work-group information (local/global sizes and dimensions) by combining several
         * locals into the elements of a single register. this optimization reduces the amount of locals required.
         *
         * NOTE: This optimization increases the number of execution cycles required to run a kernel, but lowers the
         * pressure on registers.
         *
         * Example:
         *   %num_groups_x = uniform
         *   %num_groups_y = uniform
         *   %num_groups_z = uniform
         *   ...
         *   %a = %num_groups_y * %y
         *
         * is converted to:
         *   - = xor elem_num, 0 (setf)
         *   %num_groups = uniform (ifz)
         *   - = xor elem_num, 1 (setf)
         *   %num_groups = uniform (ifz)
         *   - = xor elem_num, 2 (setf)
         *   %num_groups = uniform (ifz)
         *   ...
         *   %tmp = %num_groups >> 1
         *   %a = %tmp * %y
         *
         */
        bool compressWorkGroupLocals(const Module& module, Method& method, const Configuration& config);
    } /* namespace optimizations */
} /* namespace vc4c */
#endif /* VC4C_LOCAL_COMPRESSION_H */
