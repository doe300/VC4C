/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DataDependencyGraph.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "ControlFlowGraph.h"
#include "DebugGraph.h"
#include "LivenessAnalysis.h"

#include "log.h"

using namespace vc4c;

FastSet<const Local*> DataDependencyNodeBase::getAllIncomingDependencies() const
{
    const auto* self = reinterpret_cast<const DataDependencyNode*>(this);
    FastSet<const Local*> results;
    self->forAllIncomingEdges([&](const DataDependencyNode& neighbor, const DataDependencyEdge& edge) -> bool {
        auto it = edge.data.find(neighbor.key);
        if(it != edge.data.end())
        {
            for(auto& dependency : it->second)
            {
                if(has_flag(dependency.second, DataDependencyType::FLOW) ||
                    has_flag(dependency.second, DataDependencyType::TRANSITIVE))
                    results.emplace(dependency.first);
            }
        }
        return true;
    });

    return results;
}

FastSet<const Local*> DataDependencyNodeBase::getAllOutgoingDependencies() const
{
    const auto* self = reinterpret_cast<const DataDependencyNode*>(this);
    FastSet<const Local*> results;
    self->forAllOutgoingEdges([&](const DataDependencyNode& neighbor, const DataDependencyEdge& edge) -> bool {
        auto it = edge.data.find(self->key);
        if(it != edge.data.end())
        {
            for(const auto& dependency : it->second)
            {
                if(has_flag(dependency.second, DataDependencyType::FLOW) ||
                    has_flag(dependency.second, DataDependencyType::TRANSITIVE))
                    results.emplace(dependency.first);
            }
        }
        return true;
    });

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
        if(auto combInstr = it.get<intermediate::CombinedOperation>())
        {
            if(combInstr->op1)
                mapping.emplace(combInstr->op1.get(), it);
            if(combInstr->op2)
                mapping.emplace(combInstr->op2.get(), it);
        }
        it.nextInMethod();
    }

    return mapping;
}

static void findDependencies(BasicBlock& bb, DataDependencyGraph& graph, InstructionMapping& mapping)
{
    for(const auto& inst : bb)
    {
        if(!inst)
            continue;
        inst->forUsedLocals([&bb, &mapping, &graph](const Local* local, LocalUse::Type type,
                                const intermediate::IntermediateInstruction& inst) -> void {
            if(has_flag(type, LocalUse::Type::READER) && !local->type.isLabelType())
            {
                local->forUsers(LocalUse::Type::WRITER, [local, &bb, &mapping, &graph](const LocalUser* user) -> void {
                    auto& instIt = mapping.at(user);

                    // add local to relation (may not yet exist)
                    if(instIt.getBasicBlock() != &bb ||
                        instIt->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                    {
                        auto type = DataDependencyType::FLOW;
                        if(instIt->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                            type = add_flag(type, DataDependencyType::PHI);

                        auto& neighbor = graph.getOrCreateNode(instIt.getBasicBlock());
                        auto& neighborDependencies =
                            graph.getOrCreateNode(&bb).getOrCreateEdge(&neighbor).addInput(neighbor).data;
                        neighborDependencies[instIt.getBasicBlock()][const_cast<Local*>(local)] =
                            add_flag(neighborDependencies[instIt.getBasicBlock()][const_cast<Local*>(local)], type);
                    }
                });
            }
            if(has_flag(type, LocalUse::Type::WRITER) && !local->type.isLabelType())
            {
                local->forUsers(
                    LocalUse::Type::READER, [&inst, local, &bb, &mapping, &graph](const LocalUser* user) -> void {
                        auto& instIt = mapping.at(user);

                        // add local to relation (may not yet exist)
                        if(instIt.getBasicBlock() != &bb ||
                            inst.hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                        {
                            auto& node = graph.getOrCreateNode(&bb);
                            auto& neighborDependencies =
                                node.getOrCreateEdge(&graph.getOrCreateNode(instIt.getBasicBlock()))
                                    .addInput(node)
                                    .data;
                            neighborDependencies[&bb][const_cast<Local*>(local)] = add_flag(
                                neighborDependencies[&bb][const_cast<Local*>(local)], DataDependencyType::ANTI);
                        }
                    });
            }
        });
    }
}

static void makeTransitive(
    const ControlFlowGraph& cfg, DataDependencyGraph& graph, const analysis::GlobalLivenessAnalysis& analysis)
{
    for(auto& cfgNode : cfg.getNodes())
    {
        auto& startLiveLocals = analysis.getLocalAnalysis(*cfgNode.first).getStartResult();
        auto& node = graph.getOrCreateNode(cfgNode.first);

        // a basic block has all incoming live locals as dependencies to all direct successor blocks
        cfgNode.second.forAllIncomingEdges([&](const CFGNode& predecessor, const CFGEdge&) -> bool {
            auto& predecessorNode = graph.getOrCreateNode(predecessor.key);
            auto& edgeData = node.getOrCreateEdge(&predecessorNode).addInput(predecessorNode).data[predecessorNode.key];
            for(const auto& loc : startLiveLocals)
            {
                // TODO add "normal" data dependency graph for
                // 1) distinguish between direct and transitive dependencies
                // 2) flow and anti/phi dependencies
                edgeData[const_cast<Local*>(loc)] = add_flag(edgeData[const_cast<Local*>(loc)],
                    add_flag(DataDependencyType::FLOW, DataDependencyType::TRANSITIVE));
            }
            return true;
        });
    }
}

#ifdef DEBUG_MODE
LCOV_EXCL_START
static std::string toEdgeLabel(const DataDependency& dependency)
{
    std::string label;

    if(dependency.size() == 2)
    {
        auto it = dependency.begin();
        for(const auto& pair : it->second)
            label.append(" ").append(pair.first->name);
        label.append(" <-> ");
        ++it;
        for(const auto& pair : it->second)
            label.append(" ").append(pair.first->name);
    }
    else if(dependency.size() == 1)
        for(const auto& pair : dependency.begin()->second)
            label.append(" ").append(pair.first->name);

    return label.empty() ? "" : label.substr(1);
}
LCOV_EXCL_STOP
#endif

std::unique_ptr<DataDependencyGraph> DataDependencyGraph::createDependencyGraph(Method& method)
{
    PROFILE_START(createDataDependencyGraph);
    InstructionMapping mapping = mapInstructionsToPosition(method);
    std::unique_ptr<DataDependencyGraph> graph(new DataDependencyGraph(method.size()));
    for(auto& block : method)
    {
        findDependencies(block, *graph, mapping);
    }

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const BasicBlock* bb) -> std::string { return bb->getLabel()->getLabel()->name; };
        auto weakEdgeFunc = [](const DataDependency& dep) -> bool { return false; };
        DebugGraph<BasicBlock*, DataDependency, DataDependencyEdge::Directed>::dumpGraph<DataDependencyGraph>(
            *graph, "/tmp/vc4c-data-dependencies.dot", nameFunc, weakEdgeFunc, toEdgeLabel);
    });
    LCOV_EXCL_STOP
#endif

    PROFILE_END(createDataDependencyGraph);
    return graph;
}

std::unique_ptr<DataDependencyGraph> DataDependencyGraph::createTransitiveDependencyGraph(Method& method)
{
    PROFILE_START(createTransitiveDependencyGraph);
    std::unique_ptr<DataDependencyGraph> graph(new DataDependencyGraph(method.size()));
    analysis::GlobalLivenessAnalysis gla(false);
    gla(method);
    makeTransitive(method.getCFG(), *graph, gla);

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const BasicBlock* bb) -> std::string { return bb->getLabel()->getLabel()->name; };
        auto weakEdgeFunc = [](const DataDependency& dep) -> bool { return false; };
        DebugGraph<BasicBlock*, DataDependency, DataDependencyEdge::Directed>::dumpGraph<DataDependencyGraph>(
            *graph, "/tmp/vc4c-data-dependencies-transitive.dot", nameFunc, weakEdgeFunc, toEdgeLabel);
    });
    LCOV_EXCL_STOP
#endif

    PROFILE_END(createTransitiveDependencyGraph);
    return graph;
}
