/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRV_PARSER_BASE_H
#define SPIRV_PARSER_BASE_H

#include "../Parser.h"
#include "../performance.h"
#include "CompilationError.h"
#include "SPIRVOperation.h"

#include "spirv/unified1/spirv.hpp11"

#include <array>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace vc4c
{
    namespace spirv
    {
        enum class ParseResultCode;

        struct ParsedInstruction
        {
            virtual ~ParsedInstruction() noexcept;

            virtual spv::Op getOpcode() const noexcept = 0;
            virtual uint32_t getResultId() const noexcept = 0;
            virtual uint32_t getTypeId() const noexcept = 0;
            virtual std::size_t getNumWords() const noexcept = 0;
            virtual uint32_t getWord(std::size_t wordIndex) const = 0;
            virtual std::vector<uint32_t> parseArguments(std::size_t startIndex) const = 0;

            virtual std::string readLiteralString(std::size_t operandIndex) const = 0;
        };

        class SPIRVParserBase : public Parser
        {
        public:
            explicit SPIRVParserBase(std::istream& input = std::cin, bool isSPIRVText = false);
            ~SPIRVParserBase() override;

            void parse(Module& module) override;

            ParseResultCode parseHeader(uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound);
            ParseResultCode parseInstruction(const ParsedInstruction& parsed_instruction);
            intermediate::InstructionDecorations toInstructionDecorations(uint32_t id);

        protected:
            using Decoration = std::pair<spv::Decoration, uint32_t>;

            /*
             * Sampled images are not a run-time type,
             * but only to assign sampler to image at compile-time
             */
            struct SampledImage
            {
                uint32_t image;
                uint32_t sampler;
            };

            enum class MetaDataType : unsigned char
            {
                WORK_GROUP_SIZES,
                WORK_GROUP_SIZES_HINT,
                VECTOR_TYPE_HINT
            };

            // whether the input is SPIR-V text representation
            const bool isTextInput;
            // all global methods in the module
            MethodMapping methods;
            // the input stream
            std::istream& input;
            // the currently processed method, only valid while parsing
            SPIRVMethod* currentMethod;
            // the global mapping of ID -> constants
            ConstantMapping constantMappings;
            // the mapping of ID -> global/stack allocated data
            LocalMapping memoryAllocatedData;
            // the global mapping of ID -> type
            TypeMapping typeMappings;
            // the global mapping of ID -> sampled images
            FastMap<uint32_t, SampledImage> sampledImages;
            // the global mapping of ID -> decorations (applied to this ID)
            FastMap<uint32_t, std::vector<Decoration>> decorationMappings;
            // mapping of locals to their types
            LocalTypeMapping localTypes;
            // the global list of instructions, each instruction stores its own reference to the method it is in
            std::vector<std::unique_ptr<SPIRVOperation>> instructions;
            // the global mapping of kernel ID -> meta-data
            FastMap<uint32_t, std::map<MetaDataType, std::array<uint32_t, 3>>> metadataMappings;
            // the global list of custom strings (e.g. kernel original parameter type names)
            FastAccessList<std::string> strings;
            // the global mapping of ID -> name for this ID (e.g. type-, function-name)
            FastMap<uint32_t, std::string> names;
            // the global mapping of ID -> extended instruction set consumer
            std::unordered_map<uint32_t, ParseResultCode (SPIRVParserBase::*)(const ParsedInstruction&)>
                extensionConsumers;
            // to relay names of unsupported operations
            std::string errorExtra;

            Module* module;

            ParseResultCode parseDecoration(const ParsedInstruction& parsed_instruction, uint32_t value);

            ParseResultCode consumeOpenCLInstruction(const ParsedInstruction& instruction);
            ParseResultCode consumeDebugInfoInstruction(const ParsedInstruction& instruction);
            ParseResultCode consumeOpenCLDebugInfoInstruction(const ParsedInstruction& instruction);
            ParseResultCode consumeNonSemanticInstruction(const ParsedInstruction& instruction);

            virtual std::vector<uint32_t> assembleTextToBinary(const std::vector<uint32_t>& module) = 0;
            virtual void doParse(const std::vector<uint32_t>& module) = 0;
        };
    } // namespace spirv
} // namespace vc4c

#endif /* SPIRV_PARSER_BASE_H */
