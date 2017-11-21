/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMBINER_H
#define COMBINER_H

#include "config.h"

namespace vc4c
{
	class Method;
	class Module;
	class InstructionWalker;

	namespace optimizations
	{
		/*
		 * Combine successive branches to the same label into a single branch
		 */
		InstructionWalker combineDuplicateBranches(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		/*
		 * Combine ALU-instructions which (can) use different ALUs into a single instruction accessing both ALUs
		 */
		void combineOperations(const Module& module, Method& method, const Configuration& config);

		/*
		 * Combines the loading of the same literal within a small range in basic blocks
		 */
		void combineLoadingLiterals(const Module& module, Method& method, const Configuration& config);

		/*
		 * Adds a branch from the end to the start to allow for running several kernels (from several work-groups) in one execution.
		 * Since the kernels have different group-IDs, all instructions (including loading of parameters) are repeated.
		 * Otherwise, all parameters would need to reserve their registers over the whole range of the program, which would fail a lot of kernels.
		 */
		void unrollWorkGroups(const Module& module, Method& method, const Configuration& config);

		/*
		 * Prepares selections (successive writes to same value with inverted conditions) which write to a local, have no side-effects and one of the sources is zero
		 * for combination, by rewriting the zero-write to xor-ing the other value
		 */
		InstructionWalker combineSelectionWithZero(const Module& module, Method& method, InstructionWalker it, const Configuration& config);

		/*
		 * Combines vector several consecutive rotations with the same data
		 */
		void combineVectorRotations(const Module& module, Method& method, const Configuration& config);

		/*
		 * Combines successive setting of the same flag (e.g. introduced by PHI-nodes)
		 */
		InstructionWalker combineSameFlags(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c
#endif /* COMBINER_H */

