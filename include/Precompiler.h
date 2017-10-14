/*
 * Header for the pre-compiler allowing programmatic access to the LLVM/SPIRV-LLVM binaries for converting OpenCL C source code to LLVM-IR/SPIR-V
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PRECOMPILER_H
#define PRECOMPILER_H

#include "./config.h"
#include <iostream>
#include <memory>
#include <unordered_map>

#include "helper.h"

namespace vc4c
{
	enum class SourceType
	{
	    UNKNOWN = 0,
	    OPENCL_C = 1,
	    LLVM_IR_TEXT = 2,
		LLVM_IR_BIN = 3,
	    SPIRV_BIN = 4,
	    SPIRV_TEXT = 5,
	    QPUASM_HEX = 6,
	    QPUASM_BIN = 7
	};

	class Precompiler
	{
	public:
		Precompiler(std::istream& input, const SourceType inputType, const Optional<std::string> inputFile = {});

	#if defined SPIRV_CLANG_PATH
		void run(std::unique_ptr<std::istream>& output, const SourceType outputType = SourceType::SPIRV_BIN, const std::string& options = "", Optional<std::string> outputFile = {});
	#elif defined CLANG_PATH
		void run(std::unique_ptr<std::istream>& output, const SourceType outputType = SourceType::LLVM_IR_TEXT, const std::string& options = "", Optional<std::string> outputFile = {});
	#else
		void run(std::unique_ptr<std::istream>& output, const SourceType outputType, const std::string& options = "", Optional<std::string> outputFile = {});
	#endif

		static SourceType getSourceType(std::istream& stream);

		static void linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs, std::ostream& output);

	private:
		std::istream& input;
		const SourceType inputType;
		const Optional<std::string> inputFile;
	};
}

#endif /* PRECOMPILER_H */
