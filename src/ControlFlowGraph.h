/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_CONTROLFLOWGRAPH_H
#define VC4C_CONTROLFLOWGRAPH_H

#include "Graph.h"

#include "InstructionWalker.h"
#include "Module.h"

namespace vc4c
{
	/*
	 * A relation in the control-flow-graph represents a transition between two basic blocks.
	 *
	 * Every transition is represented by two relations (one in direction of the transition and one in the reverse direction)
	 */
	struct CFGRelation
	{
		//whether this relation represents a reverse (from label to the branch jumping to it)
		bool reverseRelation;

		//the last instruction before the change of basic-block (e.g. the branch or last instruction in block)
		InstructionWalker predecessor;

		//whether the change of basic block is implicit (e.g. by fall-through without any branch away)
		bool isImplicit;

		bool operator==(const CFGRelation& other) const;

		inline bool isReverseRelation() const
		{
			return reverseRelation;
		}

		inline bool isForwardRelation() const
		{
			return !reverseRelation;
		}

		/*
		 * Returns the condition for taking this basic-block transition (e.g. the condition-code and boolean variable for a conditional branch)
		 *
		 * Returns (COND_ALWAYS, UNDEFINED_VALUE) for unconditional transitions
		 */
		std::pair<ConditionCode, Value> getBranchConditions() const;
	};

	using CFGNode = Node<BasicBlock*, CFGRelation>;
	bool operator<(const CFGNode& one, const CFGNode& other);

	/*
	 * A loop in the control-flow represented by the basic-blocks taking part in it
	 *
	 * NOTE: A control-flow loop can only be used within the life-time of the ControlFlowGraph it is created from!
	 */
	struct ControlFlowLoop : public FastAccessList<CFGNode*>
	{
		/*
		 * Returns the basic-block in the CFG preceding the first node in the loop, the node from which the loop is entered.
		 */
		const CFGNode* findPredecessor() const;

		/*
		 * Returns the basic-block in the CFG following the last node in the loop, the node into which this loop exits into.
		 */
		const CFGNode* findSuccessor() const;

		/*
		 * Returns the InstructionWalker for the given instruction, if it is within the loop.
		 */
		Optional<InstructionWalker> findInLoop(const intermediate::IntermediateInstruction* inst) const;
	};

	/*
	 * The control-flow graph (CFG) represents the order/relation of basic-blocks by connecting basic-blocks which can follow directly after one another (e.g. by branching or fall-through)
	 */
	class ControlFlowGraph : public Graph<BasicBlock*, CFGNode>
	{
	public:

		/*
		 * Returns the node which represents the first basic-block being executed by the QPU
		 */
		CFGNode& getStartOfControlFlow();

		/*
		 * Finds all loops in the CFG
		 */
		FastAccessList<ControlFlowLoop> findLoops();

		/*
		 * Creates the CFG from the basic-blocks within the given method
		 */
		static ControlFlowGraph createCFG(Method& method);

	private:
		/*
		 * This is a modified version of the Tarjan's Algorithm to find strongly connected components taken from https://www.geeksforgeeks.org/tarjan-algorithm-find-strongly-connected-components/
		 *
		 * A recursive function that finds and prints strongly connected components using DFS traversal
		 * node --> The node to be visited next
		 * discoveryTimes --> Stores discovery times of visited nodes
		 * lowestReachable --> earliest visited node (the node with minimum discovery time) that can be reached from subtree rooted with current node
		 * stack --> To store all the connected ancestors (could be part of SCC)
		 */
		ControlFlowLoop findLoopsHelper(CFGNode* node, FastMap<CFGNode*, int>& discoveryTimes, FastMap<CFGNode*, int>& lowestReachable, RandomModificationList<CFGNode*>& stack, int& time);
	};

	enum class DataDependencyType
	{
		//flow (true) dependence, read-after-write. The instruction reading a value depends on the value being written before
		FLOW = 1,
		//anti dependence, write-after-read. The instruction writing a value "depends" on the value being read before
		ANTI = 2,
		//output dependence, write-after-write. The instruction writing a value "depends" on another instruction writing the same value before
		OUTPUT = 4,
		//the dependency is on a phi-node and is therefore depending on the branch the block was entered by. Any other dependency is "constant", the depending value is not changed in different basic blocks
		PHI = 8
	};

	/*
	 * A dependency between two basic blocks consists of the list of depending locals and their types of dependence.
	 */
	using DataDependency = FastMap<Local*, DataDependencyType>;

	struct DataDependencyNode : public Node<BasicBlock*, DataDependency>
	{
		using Base = Node<BasicBlock*, DataDependency>;

		explicit DataDependencyNode(BasicBlock* key) : Base(key) { }

		bool dependsOnBlock(const BasicBlock& bb, const DataDependencyType type = DataDependencyType::FLOW) const;
		bool hasExternalDependencies(const Local* local, const DataDependencyType type = DataDependencyType::FLOW) const;
		FastSet<const Local*> getAllExternalDependencies(const DataDependencyType type = DataDependencyType::FLOW) const;
	};

	/*
	 * The data-dependency graph represents the data-dependencies between basic-blocks.
	 *
	 * A data-dependency is e.g. a local being written-to in block A and read in block B.
	 * Data-dependencies within a single basic block are ignored.
	 */
	class DataDependencyGraph : public Graph<BasicBlock*, DataDependencyNode>
	{
	public:
		/*
		 * At least for vectorizing/unrolling loops, we only care for dependencies on phi-nodes!
		 * Any other data dependency is either
		 * - a local dependency (not necessarily basic-block local!) within the loop or
		 * - a "constant" dependency which is set somewhere before the loop and never changed in the loop body
		 */
		static DataDependencyGraph createDependencyGraph(Method& method);
	};

} /* namespace vc4c */

#endif /* VC4C_CONTROLFLOWGRAPH_H */
