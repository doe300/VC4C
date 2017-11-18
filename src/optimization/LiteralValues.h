/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LITERALVALUES_H
#define LITERALVALUES_H

#include "config.h"

namespace vc4c
{
	class Method;
	class Module;
	class InstructionWalker;

	namespace optimizations
	{
		InstructionWalker handleContainer(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		InstructionWalker handleImmediate(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		InstructionWalker handleUseWithImmediate(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c

#endif /* LITERALVALUES_H */
