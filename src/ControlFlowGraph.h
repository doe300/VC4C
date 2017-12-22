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
	using ControlFlowLoop = FastAccessList<CFGNode*>;

	class ControlFlowGraph : public Graph<BasicBlock*, CFGNode>
	{
	public:

		CFGNode& getStartOfControlFlow();

		FastAccessList<ControlFlowLoop> findLoops();

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

} /* namespace vc4c */

#endif /* VC4C_CONTROLFLOWGRAPH_H */
