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
    namespace intrinsics
    {
        /*
         * Replaces calls to intrinsic function with their implementation.
         *
         * Also replaced unsupported (arithmetic) operations (e.g. mul, udiv, etc.) with instructions emulating their
         * behavior (software-implementation of operators) and replaced comparison-operators with instructions executing
         * the comparison
         */
        void intrinsify(Module& module, Method& method, InstructionWalker it, const Configuration& config);
    } // namespace intrinsics
} // namespace vc4c
#endif /* INTRINSICS_H */
