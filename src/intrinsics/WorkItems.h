/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTRINSICS_OTHERS_H
#define INTRINSICS_OTHERS_H

#include "../InstructionWalker.h"

#include <string>

namespace vc4c
{
    namespace intrinsics
    {
        extern const std::string FUNCTION_NAME_LOCAL_SIZE;
        extern const std::string FUNCTION_NAME_LOCAL_ID;
        extern const std::string FUNCTION_NAME_NUM_DIMENSIONS;
        extern const std::string FUNCTION_NAME_NUM_GROUPS;
        extern const std::string FUNCTION_NAME_GROUP_ID;
        extern const std::string FUNCTION_NAME_GLOBAL_OFFSET;
        extern const std::string FUNCTION_NAME_GLOBAL_SIZE;
        extern const std::string FUNCTION_NAME_GLOBAL_ID;

        /**
         * Intrinsifies the call to one of the work-item intrinsic functions listed above
         */
        bool intrinsifyWorkItemFunction(Method& method, InstructionWalker it);

        /**
         * Intrinsifies the call to the barrier(...) OpenCL C function.
         */
        NODISCARD InstructionWalker intrinsifyBarrier(
            Method& method, InstructionWalker it, const intermediate::MethodCall* callSite);

        void insertControlFlowBarrier(Method& method, InstructionWalker it);
    } // namespace intrinsics
} // namespace vc4c
#endif /* INTRINSICS_OTHERS_H */
