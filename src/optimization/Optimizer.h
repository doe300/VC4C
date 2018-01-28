/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include "config.h"

#include <functional>
#include <set>

namespace vc4c
{
	class Method;
	class Module;
	class InstructionWalker;

	namespace optimizations
	{
		/*
		 * An OptimizationPass usually walks over all instructions within a single method
		 */
		struct OptimizationPass
		{
		public:
			/*
			 * NOTE: Optimizations can be run in parallel, so no static or global variables can be set.
			 * The optimizations are only run in parallel for different methods, so any access to the method is thread-safe
			 */
			using Pass = std::function<void(const Module&, Method&, const Configuration&)>;

			OptimizationPass(const std::string& name, const Pass pass, std::size_t index);

			bool operator<(const OptimizationPass& other) const;
			void operator()(const Module& module, Method& method, const Configuration& config) const;
			bool operator==(const OptimizationPass& other) const;

			std::string name;
			std::size_t index;
		private:
			Pass pass;
		};

		/*
		 * An OptimizationStep handles a single instruction per invocation
		 */
		struct OptimizationStep
		{
		public:
			/*
			 * NOTE: Optimizations can be run in parallel, so no static or global variables can be set.
			 * The optimizations are only run in parallel for different methods, so any access to the method is thread-safe
			 */
			using Step = std::function<InstructionWalker(const Module&, Method&, InstructionWalker, const Configuration&)>;

			OptimizationStep(const std::string& name, const Step step, std::size_t index);

			bool operator<(const OptimizationStep& other) const;
			InstructionWalker operator()(const Module& module, Method& method, InstructionWalker it, const Configuration& config) const;
			bool operator==(const OptimizationStep& other) const;

			std::string name;
			std::size_t index;
		private:
			Step step;
		};

		/*
		 * List of pre-defined optimization passes
		 */
		//maps all memory-accessing instructions to instructions actually performing the hardware memory-access
		extern const OptimizationPass MAP_MEMORY_ACCESS;
		//runs all the single-step optimizations. Combining them results in fewer iterations over the instructions
		extern const OptimizationPass RUN_SINGLE_STEPS;
		//combines loadings of the same literal value within a small range of a basic block
		extern const OptimizationPass COMBINE_LITERAL_LOADS;
		//handles stack-allocations by calculating their offsets and indices
		extern const OptimizationPass RESOLVE_STACK_ALLOCATIONS;
		//spills long-living, rarely written locals into the VPM
		extern const OptimizationPass SPILL_LOCALS;
		//combines duplicate vector rotations, e.g. introduced by vector-shuffle into a single rotation
		extern const OptimizationPass COMBINE_ROTATIONS;
		//eliminates useless instructions (dead store, move to same, add with zero, ...)
		extern const OptimizationPass ELIMINATE;
		//vectorizes loops
		extern const OptimizationPass VECTORIZE;
		//more like a de-optimization. Splits read-after-writes (except if the local is used only very locally), so the reordering and register-allocation have an easier job
		extern const OptimizationPass SPLIT_READ_WRITES;
		//re-order instructions to eliminate more NOPs and stall cycles
		extern const OptimizationPass REORDER;
		//run peep-hole optimization to combine ALU-operations
		extern const OptimizationPass COMBINE;
		//add (runtime-configurable) loop over the whole kernel execution, allowing for skipping some of the syscall overhead for kernels with many work-groups
		extern const OptimizationPass UNROLL_WORK_GROUPS;
		//extends the branches by adding the conditional execution and the delay-nops
		extern const OptimizationPass EXTEND_BRANCHES;
		//adds the start- and stop-segments to the beginning and end of the kernel
		extern const OptimizationPass ADD_START_STOP_SEGMENT;

		/*
		 * The default optimization passes consist of all passes listed above.
		 * NOTE: Some of the passes are REQUIRED and the compilation will fail, if they are removed.
		 * Other passes are not technically required, but e.g. make register-allocation a lot easier, thus improving the chance of successful register allocation greatly.
		 */
		extern const std::set<OptimizationPass> DEFAULT_PASSES;

		class Optimizer
		{
		public:
			Optimizer(const Configuration& config = { }, const std::set<OptimizationPass>& passes = DEFAULT_PASSES);

			void optimize(Module& module) const;

			void addPass(const OptimizationPass& pass);
			void removePass(const OptimizationPass& pass);

		private:
			Configuration config;
			std::set<OptimizationPass> passes;
		};
	} // namespace optimizations
} // namespace vc4c
#endif /* OPTIMIZER_H */

