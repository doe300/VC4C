/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include <memory>
#include <vector>
#include <ostream>
#include <map>
#ifdef MULTI_THREADED
#include <mutex>
#endif

#include "../Module.h"
#include "Instruction.h"
#include <config.h>

namespace vc4c
{
	namespace qpu_asm
	{

		class CodeGenerator
		{
		public:
			CodeGenerator(const Module& module, const Configuration& config = { });

			/*
			 * NOTE: Instruction to Assembler mapping can be run in parallel for different methods,
			 * so no static or non-constant global data can be used
			 */
			const FastModificationList<std::unique_ptr<qpu_asm::Instruction>>& generateInstructions(Method& method);

			std::size_t writeOutput(std::ostream& stream);

		private:
			Configuration config;
			const Module& module;
			std::map<Method*, FastModificationList<std::unique_ptr<qpu_asm::Instruction>>> allInstructions;
#ifdef MULTI_THREADED
		std::mutex instructionsLock;
#endif

	};
}
}



#endif /* CODEGENERATOR_H */

