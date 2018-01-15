/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPTIMIZATION_MEMORYACCESS_H
#define OPTIMIZATION_MEMORYACCESS_H

#include "config.h"

namespace vc4c
{
	class Method;
	class Module;
	class InstructionWalker;

	namespace optimizations
	{
		/*
		 * Combine consecutive configuration of VPW/VPR with the same settings
		 *
		 * In detail, this combines VPM read/writes of uniform type of access (read or write), uniform data-type and consecutive memory-addresses
		 *
		 * NOTE: Combining VPM accesses merges their mutex-lock blocks which can cause other QPUs to stall for a long time.
		 * Also, this optimization currently only supports access memory <-> QPU, data exchange between only memory and VPM are not optimized
		 */
		void combineVPMAccess(const Module& module, Method& method, const Configuration& config);

		/*
		 * Replaces the address of global data with the corresponding offset from the GLOBAL_DATA_ADDRESS value
		 */
		InstructionWalker accessGlobalData(const Module& module, Method& method, InstructionWalker it, const Configuration& config);

		/*
		 * Spills long-living locals which are rarely read into the VPM to be cached there.
		 * Also splits the uses before and after being spilled into several locals
		 *
		 * NOTE: This step currently only runs the analysis of spill-candidates, no actual spilling is performed
		 */
		void spillLocals(const Module& module, Method& method, const Configuration& config);

		//TODO (Optional optimization): lower stack allocations into registers (e.g. for vectors/scalars)
		/*
		 * Handles stack allocations:
		 * - calculates the offsets from the start of one QPU's "stack"
		 * - removes the life-time instructions
		 * - maps the addresses to offsets from global-data pointer (see #accessGlobalData)
		 *
		 * NOTE: This optimization-pass is required for the compiler to handle stack-allocations correctly
		 */
		void resolveStackAllocations(const Module& module, Method& method, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c

#endif /* OPTIMIZATION_MEMORYACCESS_H */
