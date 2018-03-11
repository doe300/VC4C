/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DataDependencyGraph.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "DebugGraph.h"

#include "log.h"

using namespace vc4c;

bool DataDependencyNode::dependsOnBlock(const BasicBlock &bb, const DataDependencyType type) const
{
	for(const auto &neighbor : getNeighbors())
	{
		if(neighbor.first->key == &bb &&
			std::any_of(neighbor.second.begin(), neighbor.second.end(),
				[&type](const std::pair<Local *, DataDependencyType> &pair) -> bool {
					return has_flag(pair.second, type);
				}))
			return true;
	}
	return false;
}

bool DataDependencyNode::hasExternalDependencies(const Local *local, const DataDependencyType type) const
{
	for(const auto &neighbor : getNeighbors())
	{
		if(std::any_of(neighbor.second.begin(), neighbor.second.end(),
			   [local, &type](const std::pair<Local *, DataDependencyType> &pair) -> bool {
				   return pair.first == local && has_flag(pair.second, type);
			   }))
			return true;
	}
	return false;
}

FastSet<const Local *> DataDependencyNode::getAllExternalDependencies(const DataDependencyType type) const
{
	FastSet<const Local *> results;

	for(const auto &neighbor : getNeighbors())
	{
		for(const auto &dependency : neighbor.second)
		{
			if(has_flag(dependency.second, type))
				results.emplace(dependency.first);
		}
	}

	return results;
}

using InstructionMapping = FastMap<const LocalUser *, InstructionWalker>;

static InstructionMapping mapInstructionsToPosition(Method &method)
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

static void findDependencies(BasicBlock &bb, DataDependencyGraph &graph, InstructionMapping &mapping)
{
	auto it = bb.begin();
	while(!it.isEndOfBlock())
	{
		it->forUsedLocals([it, &bb, &mapping, &graph](const Local *local, LocalUse::Type type) -> void {
			if(has_flag(type, LocalUse::Type::READER))
			{
				local->forUsers(LocalUse::Type::WRITER, [local, &bb, &mapping, &graph](const LocalUser *user) -> void {
					auto &instIt = mapping.at(user);

					// add local to relation (may not yet exist)
					if(instIt.getBasicBlock() != &bb ||
						instIt->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
					{
						auto &neighborDependencies =
							graph.getOrCreateNode(&bb).getNeighbors()[&graph.getOrCreateNode(instIt.getBasicBlock())];
						neighborDependencies[const_cast<Local *>(local)] =
							add_flag(neighborDependencies[const_cast<Local *>(local)], DataDependencyType::FLOW);
					}
					if(instIt->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
					{
						auto &neighborDependencies =
							graph.getOrCreateNode(&bb).getNeighbors()[&graph.getOrCreateNode(instIt.getBasicBlock())];
						neighborDependencies[const_cast<Local *>(local)] =
							add_flag(neighborDependencies[const_cast<Local *>(local)], DataDependencyType::PHI);
					}
				});
			}
			if(has_flag(type, LocalUse::Type::WRITER))
			{
				local->forUsers(
					LocalUse::Type::READER, [it, local, &bb, &mapping, &graph](const LocalUser *user) -> void {
						auto &instIt = mapping.at(user);

						// add local to relation (may not yet exist)
						if(instIt.getBasicBlock() != &bb ||
							it->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
						{
							auto &neighborDependencies =
								graph.getOrCreateNode(&bb)
									.getNeighbors()[&graph.getOrCreateNode(instIt.getBasicBlock())];
							neighborDependencies[const_cast<Local *>(local)] =
								add_flag(neighborDependencies[const_cast<Local *>(local)], DataDependencyType::ANTI);
						}
					});
			}
		});
		it.nextInBlock();
	}
}

#ifdef DEBUG_MODE
static std::string toEdgeLabel(const DataDependency &dependency)
{
	std::string label;

	for(const auto &pair : dependency)
	{
		label.append(" ").append(pair.first->name);
	}

	return label.empty() ? "" : label.substr(1);
}
#endif

DataDependencyGraph DataDependencyGraph::createDependencyGraph(Method &method)
{
	InstructionMapping mapping = mapInstructionsToPosition(method);
	DataDependencyGraph graph;
	for(auto &block : method)
	{
		findDependencies(block, graph, mapping);
	}

#ifdef DEBUG_MODE
	auto nameFunc = [](const BasicBlock *bb) -> std::string { return bb->getLabel()->getLabel()->name; };
	auto weakEdgeFunc = [](const DataDependency &dep) -> bool {
		return std::all_of(dep.begin(), dep.end(), [](const std::pair<Local *, DataDependencyType> &pair) -> bool {
			return !has_flag(pair.second, DataDependencyType::FLOW);
		});
	};
	DebugGraph<BasicBlock *, DataDependency>::dumpGraph<DataDependencyGraph>(
		graph, "/tmp/vc4c-data-dependencies.dot", true, nameFunc, weakEdgeFunc, toEdgeLabel);
#endif

	return graph;
}