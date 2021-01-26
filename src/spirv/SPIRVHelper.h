/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRVHELPER_H
#define SPIRVHELPER_H

#include "spirv/unified1/OpenCL.std.h"
#include "spirv/unified1/spirv.hpp11"

#include "../Locals.h"

#include <sstream>

namespace vc4c
{
    class Module;

    namespace spirv
    {
        enum class ParseResultCode
        {
            SUCCESS,
            UNSUPPORTED,
            INTERNAL_ERROR
        };

        std::string getOpenCLMethodName(uint32_t instructionID);

        ParseResultCode checkCapability(spv::Capability cap);
        ParseResultCode checkExtension(const std::string& extension);
        Optional<uint32_t> getDecoration(
            const std::vector<std::pair<spv::Decoration, uint32_t>>& decorations, spv::Decoration deco);
        std::vector<uint32_t> getDecorations(
            const std::vector<std::pair<spv::Decoration, uint32_t>>& decorations, spv::Decoration deco);
        void setParameterDecorations(
            Parameter& param, const std::vector<std::pair<spv::Decoration, uint32_t>>& decorations);
        DataType getIntegerType(uint32_t bitWidth, uint32_t signedness);
        AddressSpace toAddressSpace(spv::StorageClass storageClass);

        std::vector<uint32_t> readStreamOfWords(std::istream& in);

        std::string demangleFunctionName(const std::string& name);

        void addFunctionAliases(Module& module);

    } // namespace spirv
} // namespace vc4c

#endif /* SPIRVHELPER_H */
