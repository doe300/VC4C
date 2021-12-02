/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRV_TOOLS_PARSER_H
#define SPIRV_TOOLS_PARSER_H

#include "SPIRVParserBase.h"

#include "tool_paths.h"

#ifdef SPIRV_TOOLS_FRONTEND

#include "spirv-tools/libspirv.hpp"

namespace vc4c
{
    namespace spirv
    {
        struct SPIRVToolsInstruction final : ParsedInstruction
        {
            SPIRVToolsInstruction(const spv_parsed_instruction_t* inst) : inst(inst) {}
            ~SPIRVToolsInstruction() noexcept override;

            spv::Op getOpcode() const noexcept override;
            uint32_t getResultId() const noexcept override;
            uint32_t getTypeId() const noexcept override;
            std::size_t getNumWords() const noexcept override;
            uint32_t getWord(std::size_t wordIndex) const override;
            std::vector<uint32_t> parseArguments(std::size_t startIndex) const override;

            std::string readLiteralString(std::size_t operandIndex) const override;

            const spv_parsed_instruction_t* inst;
        };

        class SPIRVToolsParser final : public SPIRVParserBase
        {
        public:
            explicit SPIRVToolsParser(std::istream& input = std::cin, bool isSPIRVText = false) :
                SPIRVParserBase(input, isSPIRVText)
            {
            }
            explicit SPIRVToolsParser(std::vector<uint32_t>&& input, bool isSPIRVText = false) :
                SPIRVParserBase(std::move(input), isSPIRVText)
            {
            }
            ~SPIRVToolsParser() override;

        protected:
            std::vector<uint32_t> assembleTextToBinary(const std::vector<uint32_t>& module) override;
            void doParse(const std::vector<uint32_t>& module) override;
        };

        void linkSPIRVModules(const std::vector<std::istream*>& inputModules, std::ostream& output);

    } // namespace spirv
} // namespace vc4c
#else
namespace vc4c
{
    namespace spirv
    {
        class SPIRVToolsParser final : public SPIRVParserBase
        {
        public:
            explicit SPIRVToolsParser(std::istream& input = std::cin, bool isSPIRVText = false)
            {
                throw CompilationError(CompilationStep::GENERAL, "SPIR-V Tools is not available!");
            }

            ~SPIRVToolsParser() override = default;

        protected:
            void doParse(const std::vector<uint32_t>& module) override
            {
                throw CompilationError(CompilationStep::GENERAL, "SPIR-V Tools is not available!");
            }

            std::vector<uint32_t> assembleTextToBinary(const std::vector<uint32_t>& module) override
            {
                throw CompilationError(CompilationStep::PARSER, "SPIR-V Tools is not available!");
            }
        };

        inline void linkSPIRVModules(const std::vector<std::istream*>& inputModules, std::ostream& output)
        {
            throw CompilationError(CompilationStep::LINKER, "SPIRV-Tools linker is not available!");
        }

    } // namespace spirv
} // namespace vc4c
#endif /* SPIRV_TOOLS_FRONTEND */
#endif /* SPIRV_TOOLS_PARSER_H */