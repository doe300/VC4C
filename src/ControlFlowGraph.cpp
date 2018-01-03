/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlowGraph.h"
#include "DebugGraph.h"
#include "InstructionWalker.h"
#include "Profiler.h"

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

	//XXX correct??
	return firstLabel > secondLabel;
}

CFGNode& ControlFlowGraph::getStartOfControlFlow()
{
	return assertNode(&begin()->first->method.basicBlocks.front());
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

	FastMap<CFGNode*, int> discoveryTimes;
	FastMap<CFGNode*, int> lowestReachable;
	RandomModificationList<CFGNode*> stack;
	//a time of 0 means not initialized yet
	int time = 1;

	// Call the recursive helper function to find strongly
	// connected components in DFS tree with node 'i'

	//we need the nodes sorted by the order of the basic blocks
	OrderedSet<CFGNode*, CFGNodeSorter> orderedNodes;
	for(auto& pair : *this)
		orderedNodes.emplace(&pair.second);

	for(auto& node : orderedNodes)
	{
		if(discoveryTimes[node] == 0)
		{
			auto loop = findLoopsHelper(node, discoveryTimes, lowestReachable, stack, time);
			if(loop.size() > 1)
				loops.emplace_back(std::move(loop));
		}
		if(node->getNeighbors().find(node) != node->getNeighbors().end())
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

	for(BasicBlock& bb : method.getBasicBlocks())
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
	auto edgeLabelFunc = [](const CFGRelation& r) -> std::string {return r.isReverseRelation() || r.isImplicit || !r.predecessor.has<intermediate::Branch>() ? "" : std::string("br ") + r.predecessor->conditional.toString();};
	DebugGraph<BasicBlock*, CFGRelation>::dumpGraph<ControlFlowGraph>(graph, "/tmp/vc4c-cfg.dot", true, nameFunc, toFunction(&CFGRelation::isReverseRelation), edgeLabelFunc);
#endif

	PROFILE_END(createCFG);
	return graph;
}

ControlFlowLoop ControlFlowGraph::findLoopsHelper(CFGNode* node, FastMap<CFGNode*, int>& discoveryTimes, FastMap<CFGNode*, int>& lowestReachable, RandomModificationList<CFGNode*>& stack, int& time)
{
	// Initialize discovery time and low value
	discoveryTimes[node] = lowestReachable[node] = ++time;
	stack.push_back(node);

	// Go through all vertices adjacent to this
	node->forAllNeighbors(toFunction(&CFGRelation::isForwardRelation), [this, node, &discoveryTimes, &lowestReachable, &stack, &time](const CFGNode* next, const CFGRelation& rel) -> void
	{
		CFGNode* v = const_cast<CFGNode*>(next);
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

bool DataDependencyNode::dependsOnBlock(const BasicBlock& bb, const DataDependencyType type) const
{
	for(const auto& neighbor : getNeighbors())
	{
		if(neighbor.first->key == &bb && std::any_of(neighbor.second.begin(), neighbor.second.end(), [&type](const std::pair<Local*, DataDependencyType>& pair) -> bool { return has_flag(pair.second, type);}))
			return true;
	}
	return false;
}

bool DataDependencyNode::hasExternalDependencies(const Local* local, const DataDependencyType type) const
{
	for(const auto& neighbor : getNeighbors())
	{
		if(std::any_of(neighbor.second.begin(), neighbor.second.end(), [local,&type](const std::pair<Local*, DataDependencyType>& pair) -> bool { return pair.first == local && has_flag(pair.second, type);}))
			return true;
	}
	return false;
}

FastSet<const Local*> DataDependencyNode::getAllExternalDependencies(const DataDependencyType type) const
{
	FastSet<const Local*> results;

	for(const auto& neighbor : getNeighbors())
	{
		for(const auto& dependency : neighbor.second)
		{
			if(has_flag(dependency.second, type))
				results.emplace(dependency.first);
		}
	}

	return results;
}

using InstructionMapping = FastMap<const LocalUser*, InstructionWalker>;

static InstructionMapping mapInstructionsToPosition(Method& method)
{
	InstructionMapping mapping;

	auto it = method.walkAllInstructions();
	while(!it.isEndOfMethod())
	{
		mapping.emplace(it.get(), it);
		it.nextInMethod();
	}

	return mapping;
}

static void findDependencies(BasicBlock& bb, DataDependencyGraph& graph, InstructionMapping& mapping)
{
	auto it = bb.begin();
	while(!it.isEndOfBlock())
	{
		it->forUsedLocals([it, &bb, &mapping, &graph](const Local* local, LocalUser::Type type) -> void
		{
			if(has_flag(type, LocalUser::Type::READER))
			{
				local->forUsers(LocalUser::Type::WRITER, [local, &bb, &mapping, &graph](const LocalUser* user) -> void
				{
					auto& instIt = mapping.at(user);

					//add local to relation (may not yet exist)
					if(instIt.getBasicBlock() != &bb || has_flag(instIt->decoration, intermediate::InstructionDecorations::PHI_NODE))
					{
						auto& neighborDependencies = graph.getOrCreateNode(&bb).getNeighbors()[&graph.getOrCreateNode(instIt.getBasicBlock())];
						neighborDependencies[const_cast<Local*>(local)] = add_flag(neighborDependencies[const_cast<Local*>(local)], DataDependencyType::FLOW);
					}
					if(has_flag(instIt->decoration, intermediate::InstructionDecorations::PHI_NODE))
					{
						auto& neighborDependencies = graph.getOrCreateNode(&bb).getNeighbors()[&graph.getOrCreateNode(instIt.getBasicBlock())];
						neighborDependencies[const_cast<Local*>(local)] = add_flag(neighborDependencies[const_cast<Local*>(local)], DataDependencyType::PHI);
					}
				});
			}
			//XXX do we care for these at all?
			if(has_flag(type, LocalUser::Type::WRITER))
			{
				local->forUsers(LocalUser::Type::READER, [it, local, &bb, &mapping, &graph](const LocalUser* user) -> void
				{
					auto& instIt = mapping.at(user);

					//add local to relation (may not yet exist)
					if(instIt.getBasicBlock() != &bb || has_flag(it->decoration, intermediate::InstructionDecorations::PHI_NODE))
					{
						auto& neighborDependencies = graph.getOrCreateNode(&bb).getNeighbors()[&graph.getOrCreateNode(instIt.getBasicBlock())];
						neighborDependencies[const_cast<Local*>(local)] = add_flag(neighborDependencies[const_cast<Local*>(local)], DataDependencyType::ANTI);
					}
				});
			}
		});
		it.nextInBlock();
	}
}

static std::string toEdgeLabel(const DataDependency& dependency)
{
	std::string label;

	for(const auto& pair : dependency)
	{
		label.append(" ").append(pair.first->name);
	}

	return label.empty() ? "" : label.substr(1);
}

DataDependencyGraph DataDependencyGraph::createDependencyGraph(Method& method)
{
	InstructionMapping mapping = mapInstructionsToPosition(method);
	DataDependencyGraph graph;
	for(auto& block : method.getBasicBlocks())
	{
		findDependencies(block, graph, mapping);
	}

#ifdef DEBUG_MODE
	auto nameFunc = [](const BasicBlock* bb) -> std::string {return bb->getLabel()->getLabel()->name;};
	auto weakEdgeFunc = [](const DataDependency& dep) -> bool { return std::all_of(dep.begin(), dep.end(), [](const std::pair<Local*, DataDependencyType>& pair) -> bool { return !has_flag(pair.second, DataDependencyType::FLOW);});};
	DebugGraph<BasicBlock*, DataDependency>::dumpGraph<DataDependencyGraph>(graph, "/tmp/vc4c-data-dependencies.dot", true, nameFunc, weakEdgeFunc, toEdgeLabel);
#endif

	return graph;
}
