/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRV_LEXER_H
#define SPIRV_LEXER_H

// needs to be defined before the first inclusion of the SPIR-V header
#define SPV_ENABLE_UTILITY_CODE

#include "SPIRVParserBase.h"

#include "../Optional.h"
#include "../helper.h"
#include "CompilationError.h"

#include "spirv/unified1/spirv.hpp11"

#include <functional>
#include <iostream>
#include <vector>

namespace vc4c
{
    namespace spirv
    {
        using EndinanessConverter = FunctionPointer<uint32_t(uint32_t)>;

        struct ModuleOperation final : ParsedInstruction
        {
            ~ModuleOperation() noexcept override;

            spv::Op getOpcode() const noexcept override;
            uint32_t getResultId() const noexcept override;
            uint32_t getTypeId() const noexcept override;
            std::size_t getNumWords() const noexcept override;
            uint32_t getWord(std::size_t wordIndex) const override;
            std::vector<uint32_t> parseArguments(std::size_t startIndex) const override;

            std::string readLiteralString(std::size_t operandIndex) const override;

            spv::Op opCode;
            uint32_t typeId;
            uint32_t resultId;
            std::vector<uint32_t> words;
        };

        class SPIRVLexer final : public SPIRVParserBase
        {
        public:
            explicit SPIRVLexer(const precompilation::TypedCompilationData<SourceType::SPIRV_BIN>& input) :
                SPIRVParserBase(input)
            {
            }

            ~SPIRVLexer() override;

        private:
            std::vector<uint32_t> assembleTextToBinary(const std::vector<uint32_t>& module) override
            {
                throw CompilationError(
                    CompilationStep::PARSER, "Assembling SPIR-V text to binary is not supported by this front-end!");
            }

            void doParse(const std::vector<uint32_t>& module) override;

            std::pair<EndinanessConverter, std::vector<uint32_t>::const_iterator> parseHeader(
                const std::vector<uint32_t>& input);
            bool parseBody(const std::vector<uint32_t>& input, std::vector<uint32_t>::const_iterator startIt,
                EndinanessConverter convertEndianess);
        };
    } // namespace spirv
} // namespace vc4c

#endif /* SPIRV_LEXER_H */
