/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRV_PARSER_H
#define SPIRV_PARSER_H

#include "../Parser.h"
#include "CompilationError.h"

#include <array>
#include <iostream>

#ifdef SPIRV_FRONTEND

#include "spirv/unified1/spirv.h"
#include "spirv-tools/libspirv.hpp"

#include "../performance.h"
#include "SPIRVOperation.h"

namespace vc4c
{
	namespace spirv2qasm
	{
		class SPIRVParser: public Parser
		{
		public:
			explicit SPIRVParser(std::istream& input = std::cin, bool isSPIRVText = false);
			~SPIRVParser() override = default;

			void parse(Module& module) override;

			spv_result_t parseHeader(spv_endianness_t endian, uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound, uint32_t reserved);
			spv_result_t parseInstruction(const spv_parsed_instruction_t* parsed_instruction);
			spv_result_t parseDecoration(const spv_parsed_instruction_t* parsed_instruction, uint32_t value);
			intermediate::InstructionDecorations toInstructionDecorations(uint32_t id);

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
			MethodMapping methods;
			//the input stream
			std::istream& input;
			//the currently processed method, only valid while parsing
			SPIRVMethod* currentMethod;
			//the global mapping of ID -> constants
			ConstantMapping constantMappings;
			//the mapping of ID -> global/stack allocated data
			AllocationMapping memoryAllocatedData;
			//the global mapping of ID -> type
			TypeMapping typeMappings;
			//the global mapping of ID -> sampled images
			FastMap<uint32_t, SampledImage> sampledImages;
			//the global mapping of ID -> decorations (applied to this ID)
			FastMap<uint32_t, std::vector<Decoration>> decorationMappings;
			//mapping of locals to their types
			LocalTypeMapping localTypes;
			//the global list of instructions, each instruction stores its own reference to the method it is in
			std::vector<std::unique_ptr<SPIRVOperation>> instructions;
			//the global mapping of kernel ID -> meta-data
			FastMap<uint32_t, std::map<MetaDataType, std::array<uint32_t, 3>>> metadataMappings;
			//the global mapping of ID -> name for this ID (e.g. type-, function-name)
			FastMap<uint32_t, std::string> names;

			Module* module;

			std::pair<spv_result_t, Optional<Value>> calculateConstantOperation(const spv_parsed_instruction_t* instruction);
		};
	} // namespace spirv2qasm
} // namespace vc4c
#else
namespace vc4c
{
	namespace spirv2qasm
	{
		class SPIRVParser : public Parser
		{
		public:
			explicit SPIRVParser(std::istream& input = std::cin, bool isSPIRVText = false)
			{
				throw CompilationError(CompilationStep::GENERAL, "SPIR-V frontend is not active!");
			}
			~SPIRVParser() override = default;

			void parse(Module& module) override
			{
				throw CompilationError(CompilationStep::GENERAL, "SPIR-V frontend is not active!");
			}
		};
	} // namespace spirv2qasm
} // namespace vc4c
#endif

#endif /* SPIRV_PARSER_H */

