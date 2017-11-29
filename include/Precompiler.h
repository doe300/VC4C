/*
 * Header for the pre-compiler allowing programmatic access to the LLVM/SPIRV-LLVM binaries for converting OpenCL C source code to LLVM-IR/SPIR-V
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PRECOMPILER_H
#define PRECOMPILER_H

#include "config.h"
#include "helper.h"

#include <iostream>
#include <memory>
#include <unordered_map>

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

	class TemporaryFile : private NonCopyable
	{
	public:
		/*
		 * Creates and manages a new empty temporary file
		 */
		explicit TemporaryFile(const std::string& fileTemplate = "/tmp/vc4c-XXXXXX");
		/*
		 * Creates and manages a new temporary file with fixed file-name and initial content
		 */
		explicit TemporaryFile(const std::string& fileName, std::istream& data);
		/*
		 * Creates and manages a new temporary file with fixed file-name and initial content
		 */
		explicit TemporaryFile(const std::string& fileName, const std::vector<char>& data);
		TemporaryFile(const TemporaryFile&) = delete;
		TemporaryFile(TemporaryFile&& other) noexcept;
		~TemporaryFile();

		TemporaryFile& operator=(const TemporaryFile&) = delete;
		TemporaryFile& operator=(TemporaryFile&&) = delete;

		void openOutputStream(std::unique_ptr<std::ostream>& ptr) const;
		void openInputStream(std::unique_ptr<std::istream>& ptr) const;

		const std::string fileName;
	};

	class Precompiler
	{
	public:
		Precompiler(std::istream& input, SourceType inputType, Optional<std::string> inputFile = {});

		/*
		 * Runs the pre-compilation from the source-type passed to the constructor to the output-type specified.
		 *
		 * NOTE: pre-compilation without an output-file specified may result in deleting the symlink /dev/stdout and is therefore discouraged!
		 * See also: github issue 3 (https://github.com/doe300/VC4C/issues/3)
		 */
	#if defined SPIRV_CLANG_PATH and defined SPIRV_LLVM_SPIRV_PATH and defined SPIRV_PARSER_HEADER
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::SPIRV_BIN, const std::string& options = "", Optional<std::string> outputFile = {});
	#elif defined CLANG_PATH
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::LLVM_IR_TEXT, const std::string& options = "", Optional<std::string> outputFile = {});
	#else
		void run(std::unique_ptr<std::istream>& output, SourceType outputType, const std::string& options = "", Optional<std::string> outputFile = {});
	#endif

		static SourceType getSourceType(std::istream& stream);

		static void linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs, std::ostream& output);

	private:
		std::istream& input;
		const SourceType inputType;
		const Optional<std::string> inputFile;
	};
} // namespace vc4c

#endif /* PRECOMPILER_H */
