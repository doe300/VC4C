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
		 *
		 * Example:
		 *   label: %91
		 *   br %103
		 *   label: %92
		 *   br %103
		 *   label: %93
		 *   br %103
		 *   label: %94
		 *   br %103
		 *   label: %95
		 *   br %103
		 *
		 * is converted into:
		 *   label: %91
		 *   label: %92
		 *   label: %93
		 *   label: %94
		 *   label: %95
		 *   br %103
		 */
		InstructionWalker combineDuplicateBranches(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
		/*
		 * Combine ALU-instructions which (can) use different ALUs into a single instruction accessing both ALUs.
		 * There are two types of instruction-pairs which can be combined:
		 * - The instructions do not depend on one another, their relative order unimportant (e.g. output of first instruction is not read in second one)
		 * - Both instructions write to the same output but with inverted conditions (see example)
		 *
		 * Example (source taken from #combineSelectionWithZero):
		 *   %5 = %11 (ifz)
		 *   %5 = xor %11, %11 (ifzc)
		 *
		 * is converted to:
		 *   %5 = xor %11, %11 (ifzc) and %5 = v8min %11, %11 (ifz)
		 *
		 * NOTE: As of this point, the instruction-type CombinedInstruction can occur within a basic block!
		 * Also, only moves and ALU instructions are combined at the moment
		 */
		void combineOperations(const Module& module, Method& method, const Configuration& config);

		/*
		 * Combines the loading of the same literal within a small range in a single basic block
		 *
		 * Example:
		 *   %3 = loadi 123456
		 *   ...
		 *   %7 = loadi 123456
		 *   %8 = mul24 %3, %4
		 *   %9 = add %7, %5
		 *
		 * is converted to:
		 *   %3 = loadi 123456
		 *   ...
		 *   %8 = mul24 %3, %4
		 *   %9 = add %3, %5
		 */
		void combineLoadingLiterals(const Module& module, Method& method, const Configuration& config);

		/*
		 * Adds a branch from the end to the start to allow for running several kernels (from several work-groups) in one execution.
		 * Since the kernels have different group-IDs, all instructions (including loading of parameters) are repeated.
		 * Otherwise, all parameters would need to reserve their registers over the whole range of the program, which would fail register-mapping for a lot of kernels.
		 *
		 * NOTE: As of this step, there is a control-flow loop around the whole kernel code
		 */
		void unrollWorkGroups(const Module& module, Method& method, const Configuration& config);

		/*
		 * Prepares selections (successive writes to same value with inverted conditions) which write to a local, have no side-effects and one of the sources is zero
		 * for combination, by rewriting the zero-write to xor-ing the other value
		 *
		 * Example:
		 *   %5 = %11 (ifz)
		 *   %5 = 0 (ifzc)
		 *
		 * is converted to:
		 *   %5 = %11 (ifz)
		 *   %5 = xor %11, %11 (ifzc)
		 */
		InstructionWalker combineSelectionWithZero(const Module& module, Method& method, InstructionWalker it, const Configuration& config);

		/*
		 * Combines vector several consecutive rotations with the same data
		 *
		 * Example:
		 *   %4 = %3 << 2
		 *   %5 = %4 << 6
		 *
		 * is converted to:
		 *   %5 = %3 << 8
		 *
		 * NOTE: This optimization currently only works for constant rotation offsets.
		 */
		void combineVectorRotations(const Module& module, Method& method, const Configuration& config);

		/*
		 * Combines successive setting of the same flag (e.g. introduced by PHI-nodes)
		 *
		 * Example:
		 *   - = %3 (setf)
		 *   ...
		 *   - = %3 (setf)
		 *
		 * is converted to:
		 *   - = %3 (setf)
		 *   ...
		 *
		 * NOTE: Currently, only moves into nop-register are combined, but in an extended optimization-step any two instructions setting flags for the same value and with at most one output could be combined.
		 */
		InstructionWalker combineSameFlags(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c
#endif /* COMBINER_H */

