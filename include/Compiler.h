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
#include "helper.h"

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
		//something is not as expected
		WARNING = 'W',
		//program-error
		ERROR = 'E',
		//critical software--error
		SEVERE = 'S'
	};

	/*
	 * Base class for the compilation process
	 */
	class Compiler
	{
	public:
	    Compiler(std::istream& stream, std::ostream& output);

	    /*
	     * Runs the actual compilation
	     *
	     * Returns the number of bytes written (only meaningful for binary output-mode)
	     */
	    std::size_t convert();

	    /*
	     * Returns the current configuration, can be used to modify it
	     */
	    Configuration& getConfiguration();
	    const Configuration& getConfiguration() const;

	    /*
	     * Helper-function to easily compile a single input with the given configuration into the given output.
	     *
	     * \param input The input stream
	     * \param output The output-stream
	     * \param config The configuration to use for compilation
	     * \param options Specify additional compiler-options to pass onto the pre-compiler
	     * \param inputFile Can be used by the compiler to speed-up compilation (e.g. by running the pre-compiler with this file instead of needing to write input to a temporary file)
	     * \return the number of bytes written (only meaningful for binary output-mode)
	     */
	    static std::size_t compile(std::istream& input, std::ostream& output, Configuration config = {}, const std::string& options = "", const Optional<std::string>& inputFile = {});

	private:
	    std::istream& input;
	    std::ostream& output;
	    Configuration config;
	};

	/*
	 * Sets the global logger and its level
	 * This defaults to logging to the console
	 */
	void setLogger(std::wostream& outputStream, bool coloredOutput, LogLevel level = LogLevel::WARNING);
} // namespace vc4c

#endif /* COMPILER_H */

