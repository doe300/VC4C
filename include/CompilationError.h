/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMPILATIONERROR_H
#define COMPILATIONERROR_H

#include <stdexcept>

namespace vc4c
{
	enum class CompilationStep
	{
	    GENERAL,
		LINKER,
	    SCANNER,
	    PARSER,
	    LLVM_2_IR,
	    OPTIMIZER,
	    LABEL_REGISTER_MAPPING,
	    CODE_GENERATION,
	    PRECOMPILATION,
		VERIFIER
	};

	class CompilationError : public std::runtime_error
	{
	public:
	    CompilationError(CompilationStep step, const std::string& message);
	    CompilationError(CompilationStep step, std::size_t line, const std::string& message);
	    CompilationError(CompilationStep step, const std::string& type, const std::string& message);
	    CompilationError(const CompilationError&) = default;
	    CompilationError(CompilationError&&) noexcept = default;
	    ~CompilationError() override;

	    CompilationError& operator=(const CompilationError&) = default;
	    CompilationError& operator=(CompilationError&&) noexcept = default;
	};
} // namespace vc4c

#endif /* COMPILATIONERROR_H */

