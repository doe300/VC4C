/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlowGraph.h"
#include "DebugGraph.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"

#include "log.h"

using namespace vc4c;

bool CFGRelation::operator==(const CFGRelation& other) const
{
	return reverseRelation == other.reverseRelation && predecessor == other.predecessor && isImplicit == other.isImplicit;
}

std::pair<ConditionCode, Value> CFGRelation::getBranchConditions() const
{
	if(predecessor.has<intermediate::Branch>())
	{
		const intermediate::Branch* br = predecessor.get<const intermediate::Branch>();

		if(br->conditional != COND_ALWAYS)
		{
			ConditionCode cond = br->conditional;
			const Value& on = br->getCondition();

			//if the transition is implicit, it happens if the previous branch is not taken
			return std::make_pair(isImplicit ? cond.invert() : cond, on);
		}
	}
	return std::make_pair(COND_ALWAYS, UNDEFINED_VALUE);
}

bool vc4c::operator<(const CFGNode& one, const CFGNode& other)
{
	//sorts the CFG nodes by the order of the basic blocks
	//THIS IS IN REVERSE ORDER ON PURPOSE!
	const std::string& firstLabel = one.key->getLabel()->getLabel()->name;
	const std::string& secondLabel = other.key->getLabel()->getLabel()->name;

	if(firstLabel == BasicBlock::DEFAULT_BLOCK)
		return false;
	if(firstLabel == BasicBlock::LAST_BLOCK)
		return secondLabel == BasicBlock::LAST_BLOCK ? false : true;

	return firstLabel > secondLabel;
}

const CFGNode* ControlFlowLoop::findPredecessor() const
{
	const CFGNode* predecessor = nullptr;
	for(const CFGNode* node : *this)
	{
		node->forAllNeighbors([](const CFGRelation& rel) -> bool {return rel.isReverseRelation();}, [this,&predecessor](const CFGNode* neighbor, const CFGRelation& rel) -> void
		{
			if(std::find(begin(), end(), neighbor) == end())
			{
				//the relation is backwards and node is not within this loop -> predecessor
				if(predecessor != nullptr)
					throw CompilationError(CompilationStep::GENERAL, "Found multiple predecessors for CFG loop", neighbor->key->getLabel()->to_string());

				logging::debug() << "Found predecessor for CFG loop: " << neighbor->key->getLabel()->to_string() << logging::endl;
				predecessor = neighbor;
			}
		});
	}
	return predecessor;
}

const CFGNode* ControlFlowLoop::findSuccessor() const
{
	const CFGNode* successor = nullptr;
	for(const CFGNode* node : *this)
	{
		node->forAllNeighbors([](const CFGRelation& rel) -> bool {return rel.isForwardRelation();}, [this,&successor](const CFGNode* neighbor, const CFGRelation& rel) -> void
		{
			if(std::find(begin(), end(), neighbor) == end())
			{
				//the relation is forward and node is not within this loop -> successor
				if(successor != nullptr)
					throw CompilationError(CompilationStep::GENERAL, "Found multiple successors for CFG loop", neighbor->key->getLabel()->to_string());

				logging::debug() << "Found successor for CFG loop: " << neighbor->key->getLabel()->to_string() << logging::endl;
				successor = neighbor;
			}
		});
	}
	return successor;
}

Optional<InstructionWalker> ControlFlowLoop::findInLoop(const intermediate::IntermediateInstruction* inst) const
{
	for(const CFGNode* node : *this)
	{
		auto it = node->key->findWalkerForInstruction(inst, node->key->end());
		if(it)
			return it;
	}
	return {};
}

CFGNode& ControlFlowGraph::getStartOfControlFlow()
{
	//TODO return node without any predecessors?
	return assertNode(&(*begin()->first->method.begin()));
}

CFGNode& ControlFlowGraph::getEndOfControlFlow()
{
	//return node without any successor,
	//if there are multiple (or none), throw
	CFGNode* candidate = nullptr;
	for(auto& pair : *this)
	{
		CFGNode& node = pair.second;
		if(std::none_of(node.getNeighbors().begin(), node.getNeighbors().end(), [](const auto& pair) -> bool { return pair.second.isForwardRelation(); }))
		{
			if(candidate != nullptr)
			{
				logging::error() << "Candidate: " << candidate->key->getLabel()->to_string() << logging::endl;
				logging::error() << "Candidate: " << node.key->getLabel()->to_string() << logging::endl;
				throw CompilationError(CompilationStep::GENERAL, "Found more than one CFG node without successors!");
			}
			candidate = &node;
		}
	}
	
	if(candidate == nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Found no CFG node without successors!");
	return *candidate;
}

struct CFGNodeSorter : public std::less<CFGNode*>
{
	bool operator()(const CFGNode* x, const CFGNode* y) const
	{
		return vc4c::operator <(*x, *y);
	}
};

FastAccessList<ControlFlowLoop> ControlFlowGraph::findLoops()
{
	FastAccessList<ControlFlowLoop> loops;
	loops.reserve(8);

	FastMap<const CFGNode*, int> discoveryTimes;
	FastMap<const CFGNode*, int> lowestReachable;
	RandomModificationList<const CFGNode*> stack;
	//a time of 0 means not initialized yet
	int time = 1;

	// Call the recursive helper function to find strongly
	// connected components in DFS tree with node 'i'

	//we need the nodes sorted by the order of the basic blocks
	OrderedSet<const CFGNode*, CFGNodeSorter> orderedNodes;
	for(auto& pair : *this)
		orderedNodes.emplace(&pair.second);

	for(const CFGNode* node : orderedNodes)
	{
		if(discoveryTimes[node] == 0)
		{
			auto loop = findLoopsHelper(node, discoveryTimes, lowestReachable, stack, time);
			if(loop.size() > 1)
				loops.emplace_back(std::move(loop));
		}
		if(node->getNeighbors().find(const_cast<CFGNode*>(node)) != node->getNeighbors().end())
		{
			//extra case, loop with single block
			ControlFlowLoop loop;
			loop.emplace_back(node);
			loops.emplace_back(loop);
		}
	}

	for(const auto& loop : loops)
	{
		logging::debug() << "Found a control-flow loop: ";
		for(auto it = loop.rbegin(); it != loop.rend(); ++it)
			logging::debug() << (*it)->key->getLabel()->to_string() << " -> ";
		logging::debug() << logging::endl;
	}

	return loops;
}

ControlFlowGraph ControlFlowGraph::createCFG(Method& method)
{
	PROFILE_START(createCFG);
	ControlFlowGraph graph;

	for(BasicBlock& bb : method)
	{
		bb.forPredecessors([&bb, &graph](InstructionWalker it) -> void
		{
			//this transition is implicit if the previous instruction is not a branch at all or a conditional branch to somewhere else (then the transition happens if the condition is not met)
			bool isImplicit = !it.has<intermediate::Branch>() || (it.get<intermediate::Branch>()->conditional != COND_ALWAYS && it.get<intermediate::Branch>()->getTarget() != bb.getLabel()->getLabel());
			//forward connection
			graph.getOrCreateNode(it.getBasicBlock()).addNeighbor(&graph.getOrCreateNode(&bb), CFGRelation{false, it, isImplicit});
			//backward connection
			graph.getOrCreateNode(&bb).addNeighbor(&graph.getOrCreateNode(it.getBasicBlock()), CFGRelation{true, it, isImplicit});
		});
	}

#ifdef DEBUG_MODE
	auto nameFunc = [](const BasicBlock* bb) -> std::string {return bb->getLabel()->getLabel()->name;};
	auto edgeLabelFunc = [](const CFGRelation& r) -> std::string {return r.isReverseRelation() || r.isImplicit || !r.predecessor.has<intermediate::Branch>() ? "" : std::string("br ") + r.predecessor->conditional.to_string();};
	DebugGraph<BasicBlock*, CFGRelation>::dumpGraph<ControlFlowGraph>(graph, "/tmp/vc4c-cfg.dot", true, nameFunc, toFunction(&CFGRelation::isReverseRelation), edgeLabelFunc);
#endif

	PROFILE_END(createCFG);
	return graph;
}

ControlFlowLoop ControlFlowGraph::findLoopsHelper(const CFGNode* node, FastMap<const CFGNode*, int>& discoveryTimes, FastMap<const CFGNode*, int>& lowestReachable, RandomModificationList<const CFGNode*>& stack, int& time)
{
	// Initialize discovery time and low value
	discoveryTimes[node] = lowestReachable[node] = ++time;
	stack.push_back(node);

	// Go through all vertices adjacent to this
	node->forAllNeighbors(toFunction(&CFGRelation::isForwardRelation), [this, node, &discoveryTimes, &lowestReachable, &stack, &time](const CFGNode* next, const CFGRelation& rel) -> void
	{
		const CFGNode* v = next;
		// If v is not visited yet, then recur for it
		if (discoveryTimes[v] == 0)
		{
			findLoopsHelper(v, discoveryTimes, lowestReachable, stack, time);

			// Check if the subtree rooted with 'v' has a
			// connection to one of the ancestors of 'u'
			// Case 1 (per above discussion on Disc and Low value)
			lowestReachable[node]  = std::min(lowestReachable[node], lowestReachable[v]);
		}

		// Update low value of 'u' only of 'v' is still in stack
		// (i.e. it's a back edge, not cross edge).
		// Case 2 (per above discussion on Disc and Low value)
		else if (std::find(stack.begin(), stack.end(), v) != stack.end())
			lowestReachable[node]  = std::min(lowestReachable[node], discoveryTimes[v]);
	});

	ControlFlowLoop loop;
	loop.reserve(stack.size());

	// head node found, pop the stack and return an SCC
	if (lowestReachable[node] == discoveryTimes[node])
	{
		while (stack.back() != node)
		{
			loop.push_back(stack.back());
			stack.pop_back();
		}
		loop.push_back(stack.back());
		stack.pop_back();
	}

	return loop;
}
