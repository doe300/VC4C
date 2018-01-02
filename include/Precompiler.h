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
	/*
	 * The type of input-code determined for the input
	 */
	enum class SourceType
	{
		/*
		 * Type was not (yet) determined
		 */
	    UNKNOWN = 0,
		/*
		 * OpenCL C source-code
		 */
	    OPENCL_C = 1,
		/*
		 * LLVM IR in textual representation
		 */
	    LLVM_IR_TEXT = 2,
		/*
		 * LLVM IR bit-code
		 */
		LLVM_IR_BIN = 3,
		/*
		 * SPIR-V in binary representation
		 */
	    SPIRV_BIN = 4,
		/*
		 * SPIR-V in textual representation
		 */
	    SPIRV_TEXT = 5,
		/*
		 * generated machine code in hexadecimal representation
		 */
	    QPUASM_HEX = 6,
		/*
		 * generated machine code in binary representation
		 */
	    QPUASM_BIN = 7
	};

	/*
	 * RAII object to manage a temporary file.
	 *
	 * This class guarantees the temporary file to be deleted even if the compilation is cancelled by throwing an exception.
	 */
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

	/*
	 * The pre-compiler manages and executes the conversion of the input from a various of supported types to a type which can be read by one of the
	 * configured compiler front-ends.
	 */
	class Precompiler
	{
	public:
		Precompiler(std::istream& input, SourceType inputType, const Optional<std::string>& inputFile = {});

		/*
		 * Runs the pre-compilation from the source-type passed to the constructor to the output-type specified.
		 */
	#if defined SPIRV_CLANG_PATH and defined SPIRV_LLVM_SPIRV_PATH and defined SPIRV_PARSER_HEADER
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::SPIRV_BIN, const std::string& options = "", Optional<std::string> outputFile = {});
	#elif defined CLANG_PATH
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::LLVM_IR_TEXT, const std::string& options = "", Optional<std::string> outputFile = {});
	#else
		void run(std::unique_ptr<std::istream>& output, SourceType outputType, const std::string& options = "", Optional<std::string> outputFile = {});
	#endif

		/*
		 * Determines the type of code stored in the given stream.
		 *
		 * NOTE: This function reads from the stream but resets the cursor back to the beginning.
		 */
		static SourceType getSourceType(std::istream& stream);

		/*
		 * Links multiple source-code files using a linker provided by the pre-compilers
		 */
		static void linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs, std::ostream& output);

	private:
		std::istream& input;
		const SourceType inputType;
		const Optional<std::string> inputFile;
	};
} // namespace vc4c

#endif /* PRECOMPILER_H */
