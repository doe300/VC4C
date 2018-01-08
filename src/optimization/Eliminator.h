/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef ELIMINATOR_H
#define ELIMINATOR_H

#include "config.h"

namespace vc4c
{
	class Method;
	class Module;
	class InstructionWalker;

	namespace optimizations
	{
		void eliminateDeadStore(const Module& module, Method& method, const Configuration& config);
		void eliminatePhiNodes(const Module& module, Method& method, const Configuration& config);
		void eliminateLoadingOfConstants(const Module& module, Method& method, const Configuration& config);

		InstructionWalker eliminateUselessInstruction(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		InstructionWalker calculateConstantInstruction(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		InstructionWalker eliminateUselessBranch(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		InstructionWalker eliminateReturn(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c
#endif /* ELIMINATOR_H */

