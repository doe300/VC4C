/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTRINSICS_H
#define INTRINSICS_H

#include "../InstructionWalker.h"
#include "config.h"

#include <memory>
#include <string>

namespace vc4c
{
    namespace optimizations
    {
        /*
         * Replaces calls to intrinsic function with their implementation.
         *
         * Also replaced unsupported (arithmetic) operations (e.g. mul, udiv, etc.) with instructions emulating their
         * behavior (software-implementation of operators) and replaced comparison-operators with instructions executing
         * the comparison
         */
        InstructionWalker intrinsify(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c
#endif /* INTRINSICS_H */
