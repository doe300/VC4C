/* 
 * Header for the public C++-interface to programmatically access the compiler
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMPILER_H
#define COMPILER_H

#include "./config.h"
#include <iostream>
#include <memory>
#include <map>

#include "Precompiler.h"

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

	class Compiler
	{
	public:
	    Compiler(std::istream& stream, std::ostream& output);

	    std::size_t convert();

	    Configuration& getConfiguration();
	    const Configuration& getConfiguration() const;

	    static std::size_t compile(std::istream& input, std::ostream& output, const Configuration config = {}, const std::string& options = "", const Optional<std::string>& inputFile = {});

	private:
	    std::istream& input;
	    std::ostream& output;
	    Configuration config;
	};

	/*
	 * Sets the global logger
	 * This defaults to logging to the console
	 */
	void setLogger(std::wostream& outputStream, const bool coloredOutput, const LogLevel level = LogLevel::WARNING);
}

#endif /* COMPILER_H */

