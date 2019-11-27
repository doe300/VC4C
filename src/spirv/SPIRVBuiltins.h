/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_SPIRV_BUILTINS
#define VC4C_SPIRV_BUILTINS 1
#ifdef SPIRV_FRONTEND

#include "../Locals.h"

#include "spirv/unified1/spirv.hpp11"

#include <mutex>

namespace vc4c
{
    class InstructionWalker;
    class Module;
    class Method;
    struct Configuration;

    namespace spirv2qasm
    {
        /**
         * Custom local type to represent a SPIR-V built-in value (e.g. local IDs) located in "constant memory".
         *
         * NOTE: This type is to be used by the SPIR-V front-end only and only to correctly lower the built-ins!
         */
        struct SPIRVBuiltin : public Local
        {
            explicit SPIRVBuiltin(
                spv::BuiltIn builtin, DataType type, const std::string& name, std::string&& intrinsic, bool withArg) :
                Local(type, name),
                builtInId(builtin), intrinsicFunction(std::move(intrinsic)), hasDimensionalArgument(withArg)
            {
            }

            ~SPIRVBuiltin() noexcept override;

            spv::BuiltIn builtInId;
            std::string intrinsicFunction;
            bool hasDimensionalArgument;

        protected:
            mutable std::mutex usersLock;
            RAIILock getUsersLock() const override;
        };

        // get_work_dim - scalar integer
        extern SPIRVBuiltin BUILTIN_WORK_DIMENSIONS;
        // get_global_size - int3 vector
        extern SPIRVBuiltin BUILTIN_GLOBAL_SIZE;
        // get_global_id - int3 vector
        extern SPIRVBuiltin BUILTIN_GLOBAL_ID;
        // get_local_size - int3 vector
        extern SPIRVBuiltin BUILTIN_LOCAL_SIZE;
        // get_local_id - int3 vector
        extern SPIRVBuiltin BUILTIN_LOCAL_ID;
        // get_num_groups - int3 vector
        extern SPIRVBuiltin BUILTIN_NUM_GROUPS;
        // get_group_id - int3 vector
        extern SPIRVBuiltin BUILTIN_GROUP_ID;
        // get_global_offset - int3 vector
        extern SPIRVBuiltin BUILTIN_GLOBAL_OFFSET;

        // The magic constant indicating an intermediate::IntrinsicOperation which for reading a built-in value
        extern std::string BUILTIN_INTRINSIC;

        /**
         * Lowers the "loading" of OpenCL C work-item functions from SPIR-V constant memory into the "normal" intrinsic
         * functions.
         *
         * E.g. when using the SPIRV-LLVM translator, the call to get_global_id(X) will produce:
         *   [...]
         *   OpDecorate %a BuiltIn GlobalInvocationId
         *   [...]
         *   %a = OpVariable <int3> <constant>
         *   [...]
         *   %b = OpLoad <int3> %a
         *   %c = OpCompositeExtract <int> %b X
         *
         * ... which is converted by the SPIR-V front-end to:
         *   [...]
         *   %b = BuiltInGlobalId
         *   %c = %b >> X
         *
         * ... which will be lowered by this function to:
         *   [...]
         *   %b = vc4cl_global_id(X)
         *   %tmp = %b << X
         *   %c = %tmp >> X
         *
         * ... which is then intrinsified like a "normal" function call
         *
         * NOTE: This normalization step needs to run before the intrinsic step!
         */
        void lowerBuiltins(Module& module, Method& method, InstructionWalker it, const Configuration& config);

    } // namespace spirv2qasm
} // namespace vc4c

#endif
#endif /* VC4C_SPIRV_BUILTINS */
