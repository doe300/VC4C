/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_SPIRV_BUILTINS
#define VC4C_SPIRV_BUILTINS 1

#include "../Locals.h"

namespace spv
{
    enum class BuiltIn : unsigned;
} // namespace spv

namespace vc4c
{
    class InstructionWalker;
    class Module;
    class Method;
    struct Configuration;

    namespace spirv
    {
        /**
         * Custom local type to represent a SPIR-V built-in value (e.g. local IDs) located in "constant memory".
         *
         * NOTE: This type is to be used by the SPIR-V front-end only and only to correctly lower the built-ins!
         */
        using SPIRVBuiltin = MarkerLocal<std::pair<std::string, bool>>;

        // The magic constant indicating an intermediate::IntrinsicOperation which for reading a built-in value
        extern std::string BUILTIN_INTRINSIC;

        /**
         * Returns the local representing the underlying built-in value or NULL if the built-in is not mapped
         */
        const SPIRVBuiltin* mapToBuiltinLocal(spv::BuiltIn builtin);

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

    } // namespace spirv
} // namespace vc4c

#endif /* VC4C_SPIRV_BUILTINS */
