/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTRINSICS_OTHERS_H
#define INTRINSICS_OTHERS_H

#include "../InstructionWalker.h"

#include <functional>
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
        extern const std::string FUNCTION_NAME_LOCAL_LINEAR_ID;
        extern const std::string FUNCTION_NAME_GLOBAL_LINEAR_ID;

        /**
         * Intrinsifies the call to one of the work-item intrinsic functions listed above
         */
        bool intrinsifyWorkItemFunction(Method& method, TypedInstructionWalker<intermediate::MethodCall> it);

        /**
         * Intrinsifies the call to the barrier(...) OpenCL C function.
         */
        NODISCARD InstructionWalker intrinsifyBarrier(
            Method& method, TypedInstructionWalker<intermediate::MethodCall> it);

        /**
         * Inserts a control-flow barrier similar to the barrier(...) OpenCL C function at the given position.
         *
         * The optional function is called to insert code into the code path of the first work-item (local ID zero) at
         * the point where all other work-items are blocked on one of the semaphores. This can be used e.g. to pre-load
         * or write-back data from/to a cache.
         *
         * NOTE: The function inserting the optional code to be executed by the first work-item only may be executed
         * multiple times to insert code into multiple blocks!
         */
        void insertControlFlowBarrier(Method& method, InstructionWalker it,
            const std::function<InstructionWalker(InstructionWalker)>& insertFirstWorkItemOnlyCode = {});
    } // namespace intrinsics
} // namespace vc4c
#endif /* INTRINSICS_OTHERS_H */
