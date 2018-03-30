/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_DEPENDENCY_GRAPH_H
#define VC4C_DEPENDENCY_GRAPH_H

#include "../Graph.h"

namespace vc4c
{
	namespace intermediate
	{
		class IntermediateInstruction;
	}

	class BasicBlock;

	enum class DependencyType
	{
		// flow (true) dependence, read-after-write. The instruction reading a value depends on the value being written
		// before
		FLOW = 1 << 0,
		// anti dependence, write-after-read. The instruction writing a value "depends" on the value being read before
		ANTI = 1 << 1,
		// output dependence, write-after-write. The instruction writing a value "depends" on another instruction
		// writing the same value before
		OUTPUT = 1 << 2,
		// the instruction depends on the other instruction reading the same value beforehand (e.g. for some registers)
		READ_ORDER = 1 << 3,
		// true signal dependence. The instruction "using" the effect of a signal depends on this signal being triggered
		// before
		CONSUME_SIGNAL = 1 << 4,
		// anti signal dependence. The instruction firing a signal "depends" on any previous signal being consumed
		// before
		OVERWRITE_SIGNAL = 1 << 5,
		// output signal dependence. The instruction firing a signal "depends" on the other instruction firing its
		// signal beforehand
		// XXX required?
		SIGNAL_ORDER = 1 << 6,
		// true condition dependence. The instruction using conditional execution depends on the flags being set before
		CONDITIONAL = 1 << 7,
		// anti condition dependence. The instruction setting flags "depends" on any conditional execution being
		// executed before
		OVERWRITE_FLAGS = 1 << 8,
		// output condition dependence. The instruction setting flags "depends" on the neighboring instruction setting
		// its flags beforehand
		// XXX required?
		FLAG_ORDER = 1 << 9,
		// the semaphore needs to be changed after any previous semaphore is modified
		SEMAPHORE_ORDER = 1 << 10,
		// the instructions need to be executed in the order specified via the dependencies due to hardware periphery
		PERIPHERY_ORDER = 1 << 11,
		// the instruction depends on the mutex being locked
		MUTEX_LOCK = 1 << 12

		// TODO add reverse dependencies:
		// These can be used as "outgoing dependencies" to select the next instruction (not depending on anything),
		// which has the most other instructions depending on it
		// also could be used to go to "dependents and remove this instruction as dependency"
	};

	struct DependencyNode;

	/*
	 * A single dependency between two instructions within a basic block
	 *
	 * NOTE: Dependencies are only backwards, e.g. an instruction can only depend on a previous instruction
	 */
	struct Dependency
	{
		DependencyType type;

		/*
		 * The number of cycles delay which should/must be between the two instructions of this dependency
		 */
		unsigned numDelayCycles;
		/*
		 * Determines whether the delay in numDelayCycles is a recommendation (for optimal performance) or a must
		 */
		bool isMandatoryDelay;
		/*
		 * Whether this is a reverse dependency (e.g. in reverse direction=
		 */
		bool isReverse;

		/*
		 * Return whether the instruction in this node depends on the neighbor associated (needs to be executed
		 * afterwards)
		 */
		bool isIncomingDependency() const;
		/*
		 * Returns whether the instruction on this node has the associated neighbor as dependent (needs to be executed
		 * before the neighbor)
		 */
		bool isOutgoingDependency() const;

		/*
		 * Rates the current distance between the two instructions and returns a value determining the acceptance of
		 * this distance:
		 * - returns a value of 0 if there is no mandatory or recommended delay between the two instructions
		 * - returns a value of 0 if the recommended/mandatory delay is smaller than the current distance
		 * - returns UINT_MAX, if the delay is mandatory and the current distance is not large enough
		 * - returns the difference of the recommended delay to the current distance, otherwise
		 *
		 * -> The lower the value, the more acceptable this current distance is
		 */
		unsigned rateDelay(unsigned currentDistance) const;

		/*
		 * Returns whether the instruction for the given node can be inserted between the producer and the consumer of
		 * this dependency (between the two instructions linked by this dependency)
		 *
		 * NOTE: This method does not check for value dependencies!
		 */
		bool canBeInserted(const DependencyNode& node) const;
	};

	struct DependencyNode : public Node<const intermediate::IntermediateInstruction*, Dependency>
	{
		using Base = Node<const intermediate::IntermediateInstruction*, Dependency>;

		explicit DependencyNode(const intermediate::IntermediateInstruction* key) : Base(key) {}

		/*
		 * Returns whether this instruction depends on any other instruction within the same basic block to be executed
		 * before
		 */
		bool hasIncomingDependencies() const;

		/*
		 * Returns whether this instructions requires any other instruction within the same basic block to be executed
		 * afterwards
		 */
		bool hasOutGoingDependencies() const;

		/*
		 * Returns the node setting the flags consumed by this instruction.
		 *
		 * If the instruction does not consume flags, nullptr is returned.
		 */
		const DependencyNode* getFlagsSetter() const;

		/*
		 * Returns the node triggering the signal consumed by this instruction.
		 *
		 * If the instruction does not consume signals, nullptr is returned.
		 */
		const DependencyNode* getSignalTrigger() const;

		/*
		 * Returns the node consuming the signal triggered by this instruction.
		 *
		 * Returns nullptr if this instruction does not trigger a consumable signal.
		 */
		const DependencyNode* getSignalConsumer() const;
	};

	/*
	 * Graph representing the dependencies between instructions within a single basic block
	 */
	class DependencyGraph : public Graph<const intermediate::IntermediateInstruction*, DependencyNode>
	{
	public:
		static DependencyGraph createGraph(const BasicBlock& block);
	};
}

#endif /* VC4C_DEPENDENCY_GRAPH_H */