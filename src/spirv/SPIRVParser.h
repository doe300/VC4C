/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRV_PARSER_H
#define SPIRV_PARSER_H

#include <iostream>

#include "../Parser.h"
#include "../performance.h"
#include "CompilationError.h"

#ifdef SPIRV_HEADER

#include SPIRV_HEADER
#include SPIRV_PARSER_HEADER
#include "SPIRVOperation.h"

namespace vc4c
{
	namespace spirv2qasm
	{
		class SPIRVParser: public Parser
		{
		public:
			SPIRVParser(std::istream& input = std::cin, const bool isSPIRVText = false);

			void parse(Module& module) override;

			spv_result_t parseHeader(spv_endianness_t endian, uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound, uint32_t reserved);
			spv_result_t parseInstruction(const spv_parsed_instruction_t* parsed_instruction);
			spv_result_t parseDecoration(const spv_parsed_instruction_t* parsed_instruction);
			intermediate::InstructionDecorations toInstructionDecorations(const uint32_t id);

		private:
			using Decoration = std::pair<SpvDecoration, uint32_t>;

			/*
			 * Sampled images are not a run-time type,
			 * but only to assign sampler to image at compile-time
			 */
			struct SampledImage
			{
				uint32_t image;
				uint32_t sampler;
			};

			//whether the input is SPIR-V text representation
			const bool isTextInput;
			//all global methods in the module
			std::map<uint32_t, SPIRVMethod> methods;
			//the input stream
			std::istream& input;
			//the currently processed method, only valid while parsing
			SPIRVMethod* currentMethod;
			//the global mapping of ID -> constants
			std::map<uint32_t, Value> constantMappings;
			//the global mapping of ID -> global data
			std::map<uint32_t, Global*> globalData;
			//the global mapping of ID -> type
			std::map<uint32_t, DataType> typeMappings;
			//the global mapping of ID -> sampled images
			std::map<uint32_t, SampledImage> sampledImages;
			//the global mapping of ID -> decorations (applied to this ID)
			std::map<uint32_t, std::vector<Decoration>> decorationMappings;
			//mapping of locals to their types
			std::map<uint32_t, uint32_t> localTypes;
			//the global list of instructions, each instruction stores its own reference to the method it is in
			std::vector<std::unique_ptr<SPIRVOperation>> instructions;
			//the global mapping of kernel ID -> meta-data
			std::map<uint32_t, std::map<MetaDataType, std::vector<std::string>>>metadataMappings;
			//the global mapping of ID -> name for this ID (e.g. type-, function-name)
			std::map<uint32_t, std::string> names;

			Module* module;

			std::pair<spv_result_t, Optional<Value>> calculateConstantOperation(const spv_parsed_instruction_t* instruction);
		};
	}
}
#else
namespace vc4c
{

namespace spirv2qasm
{
	class SPIRVParser : public Parser
	{
	public:
		SPIRVParser(std::istream& input = std::cin, const bool isSPIRVText = false)
		{
			throw CompilationError(CompilationStep::GENERAL, "SPIR-V frontend is not active!");
		}

		std::vector<Method*> parse() override
		{
			throw CompilationError(CompilationStep::GENERAL, "SPIR-V frontend is not active!");
		}

		std::vector<Method*> getKernelFunctions() const override
		{
			throw CompilationError(CompilationStep::GENERAL, "SPIR-V frontend is not active!");
		}
		std::vector<Local> getGlobalData() const override
		{
			throw CompilationError(CompilationStep::GENERAL, "SPIR-V frontend is not active!");
		}
	};
}
}
#endif

#endif /* SPIRV_PARSER_H */

