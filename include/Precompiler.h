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

	bool isSupportedByFrontend(SourceType inputType, Frontend frontend);

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
	#ifdef USE_LLVM_LIBRARY
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::LLVM_IR_BIN, const std::string& options = "", Optional<std::string> outputFile = {});
	#elif defined SPIRV_CLANG_PATH and defined SPIRV_LLVM_SPIRV_PATH and defined SPIRV_PARSER_HEADER
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::SPIRV_BIN, const std::string& options = "", Optional<std::string> outputFile = {});
	#elif defined CLANG_PATH
		void run(std::unique_ptr<std::istream>& output, SourceType outputType = SourceType::LLVM_IR_TEXT, const std::string& options = "", Optional<std::string> outputFile = {});
	#else
		void run(std::unique_ptr<std::istream>& output, SourceType outputType, const std::string& options = "", Optional<std::string> outputFile = {});
	#endif

		/*
		 * Helper-function to easily pre-compile a single input with the given configuration into the given output.
		 *
		 * \param input The input stream
		 * \param output The output-stream
		 * \param config The configuration to use for compilation
		 * \param options Specify additional compiler-options to pass onto the pre-compiler
		 * \param inputFile Can be used by the compiler to speed-up compilation (e.g. by running the pre-compiler with these files instead of needing to write input to a temporary file)
		 * \param outputFile The optional output-file to write the pre-compiled code into. If this is specified, the code is compiled into the file, otherwise the output stream is filled with the compiled code
		 */
		static void precompile(std::istream& input,std::unique_ptr<std::istream>& output, Configuration config = {}, const std::string& options = "", const Optional<std::string>& inputFile = {}, Optional<std::string> outputFile = {});

		/*
		 * Determines the type of code stored in the given stream.
		 *
		 * NOTE: This function reads from the stream but resets the cursor back to the beginning.
		 */
		static SourceType getSourceType(std::istream& stream);

		/*
		 * Links multiple source-code files using a linker provided by the pre-compilers.
		 *
		 * Returns the SourceType of the linked module
		 */
		static SourceType linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs, std::ostream& output);

		const SourceType inputType;
		const Optional<std::string> inputFile;
	private:
		std::istream& input;
	};
} // namespace vc4c

#endif /* PRECOMPILER_H */
