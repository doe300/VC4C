/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_REGISTER_FIXES_H
#define VC4C_REGISTER_FIXES_H

#include "../helper.h"

#include <functional>
#include <string>
#include <vector>

namespace vc4c
{
    struct Configuration;
    class Method;
    namespace qpu_asm
    {
        class GraphColoring;

        enum class FixupResult
        {
            // Colored graph is valid, register fix-up can be aborted
            ALL_FIXED,
            // Some fixes were applied and the colored graph needs to be regenerated before the next step is run
            FIXES_APPLIED_RECREATE_GRAPH,
            // Only fixes that do not modify the colored graph have been applied
            FIXES_APPLIED_KEEP_GRAPH,
            // No fixes were applied, colored graph was not changed, next step should be run
            NOTHING_FIXED
        };

        struct RegisterFixupStep
        {
            std::string name;
            FunctionPointer<FixupResult(Method&, const Configuration&, GraphColoring&)> step;

            FixupResult operator()(Method& method, const Configuration& config, GraphColoring& coloredGraph) const
            {
                return step(method, config, coloredGraph);
            }
        };

        /**
         * The different steps to run for trying to fix register association errors.
         *
         * The general flow is like this:
         * 1. color local graph and check for errors
         * 1.1 if there are no errors, end
         * 2. run the current fix-up step
         * 3. if necessary, completely recreate the colored graph
         * 4. increment current fix-up step and go back to 1.
         */
        extern const std::vector<RegisterFixupStep> FIXUP_STEPS;

        /**
         * Reduces register pressure by grouping pointer parameter into the elements of vector locals.
         *
         * NOTE: Since this modification increases the number of instructions noticeably, certain conditions are checked
         * (e.g. parameters not accessed too often or on loops), which can lead to some pointer parameters not being
         * grouped!
         *
         * Example:
         *   %in = reg uniform
         *   %out = reg uniform
         *   [...]
         *   %in_addr = %in
         *   [...]
         *   %out_addr = %out
         *
         * is converted to:
         *   - = xor reg elem_num, 0 (setf)
         *   %param_group = reg uniform (ifz)
         *   - = xor reg elem_num, 1 (setf)
         *   %param_group reg uniform (ifz)
         *   [...]
         *   %in_addr = %param_group >> 0
         *   [...]
         *   %out_addr = %param_group >> 1
         *
         * Returns whether at least one group of parameters was created and therefore instructions and parameter
         * livenesses changed.
         */
        FixupResult groupParameters(Method& method, const Configuration& config, GraphColoring& coloredGraph);

        /**
         * Reduces register pressure by grouping scalar and pointer locals into the elements of vector locals.
         *
         * NOTE: This modification increases the number of instructions drastically!
         *
         * Example:
         *   %in = reg uniform
         *   %out = reg uniform
         *   [...]
         *   %in_addr = %in
         *   [...]
         *   %out_addr = %out
         *
         * is converted to:
         *   - = xor reg elem_num, 0 (setf)
         *   %param_group = reg uniform (ifz)
         *   - = xor reg elem_num, 1 (setf)
         *   %param_group reg uniform (ifz)
         *   [...]
         *   %in_addr = %param_group >> 0
         *   [...]
         *   %out_addr = %param_group >> 1
         *
         * Returns whether at least one group of scalar locals was created and therefore instructions and local
         * livenesses changed.
         */
        FixupResult groupScalarLocals(
            Method& method, const Configuration& config, const GraphColoring& coloredGraph, bool runConservative);

        /**
         * Reduces register pressure by moving constant calculations closer to their usages.
         *
         * This fix-up may partially undo some optimizations, e.g. by moving constant loads back to their original
         * position inside a loop they were hoisted from.
         *
         * NOTE: Since delays have already been handled at this point, some instructions will be replaced by a NOP
         * instead of being completely removed when being moved away!
         *
         * Example:
         *   %a = ldi 42
         *   [...]
         *   %b = %a xor 15
         *
         * is converted to:
         *   [...]
         *   %a = ldi 42
         *   %b = %a xor 15
         */
        FixupResult rematerializeConstants(Method& method, const Configuration& config, GraphColoring& coloredGraph);

        /**
         * Reduces register pressure by spilling long-living and rarely used locals into VPM cache rows.
         *
         * NOTE: This fix-up could greatly reduce performance, since it introduces VPM accesses.
         *
         */
        FixupResult spillLocals(Method& method, const Configuration& config, GraphColoring& coloredGraph);

    } // namespace qpu_asm

} // namespace vc4c

#endif /* VC4C_REGISTER_FIXES_H */
