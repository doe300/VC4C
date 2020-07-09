/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTRINSICS_OTHERS_H
#define INTRINSICS_OTHERS_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace intrinsics
    {
        /**
         * Intrinsifies the call to the barrier(...) OpenCL C function.
         */
        NODISCARD InstructionWalker intrinsifyBarrier(
            Method& method, InstructionWalker it, const intermediate::MethodCall* callSite);
    } // namespace intrinsics
} // namespace vc4c
#endif /* INTRINSICS_OTHERS_H */
