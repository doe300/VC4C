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
    /*
     * Enumeration determining the compilation-step the error was thrown in
     */
    enum class CompilationStep
    {
        /*
         * The current step cannot be assigned to a single compilation-step
         */
        GENERAL,
        /*
         * Error while linking input-data
         */
        LINKER,
        /*
         * Error while scanning textual input
         */
        SCANNER,
        /*
         * Error in the front-end parser being used
         */
        PARSER,
        /*
         * Error converting the front-end internal representation to the "general" IR
         */
        LLVM_2_IR,
        /*
         * Error in one of the normalization steps
         */
        NORMALIZER,
        /*
         * Error in one of the optimization/transformation steps
         */
        OPTIMIZER,
        /*
         * Error mapping labels to their position or locals to their registers
         */
        LABEL_REGISTER_MAPPING,
        /*
         * Error generation the outpur machine-code
         */
        CODE_GENERATION,
        /*
         * Error in pre-compilation (OpenCL to SPIR-V/LLVM IR)
         */
        PRECOMPILATION
    };

    /*
     * Base class for all errors throws during compilation for errors in the input/processing or unsupported cases
     */
    class CompilationError : public std::runtime_error
    {
    public:
        CompilationError(CompilationStep step, const std::string& message);
        CompilationError(CompilationStep step, const std::string& message, const std::string& object);
        CompilationError(const CompilationError&) = default;
        CompilationError(CompilationError&&) = default; // RPi cross-compiler throws on noexcept
        ~CompilationError() noexcept override;

        CompilationError& operator=(const CompilationError&) = default;
        CompilationError& operator=(CompilationError&&) = default; // RPi cross-compiler throws on noexcept

        /*
         * Logs a back-trace for the given executing function.
         *
         * NOTE: This function is a no-op if NDEBUG is set or the compiler is not GCC
         */
        static void logBacktrace();
    };
} // namespace vc4c

#endif /* COMPILATIONERROR_H */
