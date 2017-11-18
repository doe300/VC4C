/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include "Instruction.h"
#include "config.h"

#include <map>
#include <memory>
#include <ostream>
#include <vector>
#ifdef MULTI_THREADED
#include <mutex>
#endif

namespace vc4c
{
	class Method;
	class Module;

	namespace qpu_asm
	{

		class CodeGenerator
		{
		public:
			explicit CodeGenerator(const Module& module, const Configuration& config = { });

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
	} // namespace qpu_asm
} // namespace vc4c



#endif /* CODEGENERATOR_H */

