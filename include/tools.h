/*
 * Contains the public functions to access additional tools not part of the compiler itself, like the built-in emulator
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TOOLS_H
#define VC4C_TOOLS_H

#include "helper.h"

#include <array>
#include <iostream>
#include <limits>

namespace vc4c
{
	namespace tools
	{
		/*
		 * Work-group configuration
		 */
		struct WorkGroupConfig
		{
			uint32_t dimensions = 3;
			std::array<uint32_t, 3> localSizes = {1, 1, 1};
			std::array<uint32_t, 3> numGroups = {1, 1, 1};
			std::array<uint32_t, 3> globalOffsets = {0, 0, 0};
		};

		/*
		 * Data container for all configuration required to emulate a kernel-execution
		 */
		struct EmulationData
		{
			/*
			 * The module to use, either the path to the compiled module-file (in binary format!) or the module-data itself
			 */
			std::pair<std::string, std::istream*> module;
			/*
			 * The name of the kernel to execute
			 */
			std::string kernelName;
			/*
			 * The parameters, in-order.
			 * A parameter is either the word directly read by the kernel or a memory-buffer for pointer parameters
			 *
			 * NOTE: All parameters (incl. output parameters) need to be resized to the actual space required.
			 * Also, the output values are NOT stored back into the parameter, but need to be read via an extra function
			 */
			std::vector<std::pair<uint32_t, Optional<std::vector<uint32_t>>>> parameter;
			/*
			 * The work-group configuration to run the execution with
			 */
			WorkGroupConfig workGroup;
			/*
			 * The maximum number of cycles to execute before terminating the emulation
			 */
			uint32_t maxEmulationCycles = std::numeric_limits<uint32_t>::max();

			std::size_t calcParameterSize() const;
			uint32_t calcNumWorkItems() const;
		};

		/*
		 * The result of the emulation
		 */
		struct EmulationResult
		{
			/*
			 * The input data for this emulation. This is the data passed to the emulator
			 */
			const EmulationData& input;
			/*
			 * Whether the emulation terminated by successfully completing the execution (true) or by exceeding the execution limit (false)
			 */
			bool executionSuccessful;
			/*
			 * The final contents of the parameter passed to the emulation (e.g. for output-parameter).
			 */
			std::vector<std::pair<uint32_t, Optional<std::vector<uint32_t>>>> results;
		};

		/*
		 * Runs the emulation and returns the result.
		 *
		 * NOTE: This function may throw a CompilationError if anything in the emulation goes horrible wrong (e.g. an invalid or unsupported operation was performed)
		 */
		EmulationResult emulate(const EmulationData& data);

	} /* namespace tools */
} /* namespace vc4c */



#endif /* VC4C_TOOLS_H */
