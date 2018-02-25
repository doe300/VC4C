/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_OPTIMIZATION_CONTROLFLOW_H
#define VC4C_OPTIMIZATION_CONTROLFLOW_H

#include "../Module.h"

namespace vc4c
{
	namespace optimizations
	{
		/*
		 * Tries to find loops which then can be vectorized by combining multiple iterations into one.
		 *
		 * NOTE: Currently only works with "standard" for-range loops and needs to be enabled explicitly in the Configuration
		 */
		void vectorizeLoops(const Module& module, Method& method, const Configuration& config);

		/*
		 * Extends the branches (up to now represented by a single instruction) by
		 * inserting instructions setting the necessary flags (if required)
		 * and the subsequent delay-instructions required to empty the pipeline
		 *
		 * Example:
		 *   br %103
		 *
		 * is converted to:
		 *   br %103
		 *   nop
		 *   nop
		 *   nop
		 */
		void extendBranches(const Module& module, Method& method, const Configuration& config);

		/*
		 * Adds the start- and stop-segment to the kernel code
		 *
		 * The start-segment contains the code to read the kernel-parameters as well as the "hidden" parameters (work-item and work-group info, address of global data, etc.)
		 * The stop-segment contains instructions to trigger the host-interrupt to notify VC4CL that this execution is finished and generates a signal to let the QPu know the same
		 */
		void addStartStopSegment(const Module& module, Method& method, const Configuration& config);

		/*
		 * Remove constant load in loops
		 */
		void removeConstantLoadInLoops(const Module& module, Method& method, const Configuration& config);

	} /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_OPTIMIZATION_CONTROLFLOW_H */
