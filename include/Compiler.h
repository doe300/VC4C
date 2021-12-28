/*
 * Header for the public C++-interface to programmatically access the compiler
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMPILER_H
#define COMPILER_H

#include "config.h"

#include <iostream>
#include <map>
#include <memory>

namespace vc4c
{
    /*!
     * The log-level
     * (see cpplog log-levels)
     */
    enum class LogLevel : unsigned char
    {
        DEBUG = 'D',
        INFO = 'I',
        // something is not as expected
        WARNING = 'W',
        // program-error
        ERROR = 'E',
        // critical software--error
        SEVERE = 'S'
    };

    // Declared in Precompiler.h
    class CompilationData;

    template <typename T>
    class Optional;

    /*
     * Base class for the compilation process
     */
    class Compiler
    {
    public:
        /**
         * Helper-function to easily compile a single input with the given configuration into the given output.
         *
         * \param input The input data
         * \param config The configuration to use for compilation
         * \param options Specify additional compiler-options to pass onto the pre-compiler
         * \param outputFile Can be given to force the compiler to write the output data into that particular file.
         * \return the output data as well as the number of bytes written (only meaningful for binary output-mode)
         */
        static std::pair<CompilationData, std::size_t> compile(const CompilationData& input,
            const Configuration& config = {}, const std::string& options = "", const std::string& outputFile = "");
        [[deprecated]] static std::pair<CompilationData, std::size_t> compile(const CompilationData& input,
            const Configuration& config, const std::string& options, const Optional<std::string>& outputFile);
    };

    /*
     * Sets the logger and its level to be used for any upcoming (pre-)compilation or emulation.
     *
     * This defaults to logging to the console
     *
     * NOTE: The logger is only used for any upcoming API calls on this thread and any worker threads/tasks spawned by
     * this thread and does not affect any parallel or already running API calls.
     */
    void setLogger(std::wostream& outputStream, bool coloredOutput, LogLevel level = LogLevel::WARNING);
} // namespace vc4c

#endif /* COMPILER_H */
