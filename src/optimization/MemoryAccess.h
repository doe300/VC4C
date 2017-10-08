/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPTIMIZATION_MEMORYACCESS_H
#define OPTIMIZATION_MEMORYACCESS_H

#include <config.h>
#include "../Module.h"

namespace vc4c
{
	namespace optimizations
	{
		/*
		 * Combine consecutive configuration of VPW/VPR with the same settings
		 */
		void combineVPMAccess(const Module& module, Method& method, const Configuration& config);

		InstructionWalker accessGlobalData(const Module& module, Method& method, InstructionWalker it, const Configuration& config);

		/*
		 * Spills long-living locals which are rarely read into the VPM to be cached there.
		 * Also splits the uses before and after being spilled into several locals
		 */
		void spillLocals(const Module& module, Method& method, const Configuration& config);
	}
}

#endif /* OPTIMIZATION_MEMORYACCESS_H */
