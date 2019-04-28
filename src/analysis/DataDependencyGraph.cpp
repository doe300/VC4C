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

// TODO use LocalUsageAnalysis??!

bool DataDependencyNodeBase::dependsOnBlock(const BasicBlock& bb, const DataDependencyType type) const
{
    const auto* self = reinterpret_cast<const DataDependencyNode*>(this);
    bool hasDependency = false;
    // TODO is checking here for only incoming edges correct? Could there be dependencies in both directions?
    self->forAllIncomingEdges([&](const DataDependencyNode& neighbor, const DataDependencyEdge& edge) -> bool {
        if(neighbor.key == &bb && std::any_of(edge.data.begin(), edge.data.end(), [&type](const auto& pair) -> bool {
               return has_flag(pair.second, type);
           }))
        {
            hasDependency = true;
            return false;
        }
        return true;
    });
    return hasDependency;
}

bool DataDependencyNodeBase::hasExternalDependencies(const Local* local, const DataDependencyType type) const
{
    const auto* self = reinterpret_cast<const DataDependencyNode*>(this);
    bool hasDependency = false;
    // TODO same here, is checking here for only incoming edges correct? Could there be dependencies in both directions?
    self->forAllIncomingEdges([&](const DataDependencyNode& neighbor, const DataDependencyEdge& edge) -> bool {
        if(std::any_of(edge.data.begin(), edge.data.end(),
               [local, &type](const auto& pair) -> bool { return pair.first == local && has_flag(pair.second, type); }))
        {
            hasDependency = true;
            return false;
        }
        return true;
    });
    return hasDependency;
}

FastSet<const Local*> DataDependencyNodeBase::getAllExternalDependencies(const DataDependencyType type) const
{
    const auto* self = reinterpret_cast<const DataDependencyNode*>(this);
    FastSet<const Local*> results;
    // TODO same here, is checking here for only incoming edges correct? Could there be dependencies in both directions?
    self->forAllIncomingEdges([&](const DataDependencyNode& neighbor, const DataDependencyEdge& edge) -> bool {
        for(const auto& dependency : edge.data)
        {
            if(has_flag(dependency.second, type))
                results.emplace(dependency.first);
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
        inst->forUsedLocals([&inst, &bb, &mapping, &graph](const Local* local, LocalUse::Type type) -> void {
            if(has_flag(type, LocalUse::Type::READER) && !local->type.isLabelType())
            {
                local->forUsers(LocalUse::Type::WRITER, [local, &bb, &mapping, &graph](const LocalUser* user) -> void {
                    auto& instIt = mapping.at(user);

                    // add local to relation (may not yet exist)
                    if(instIt.getBasicBlock() != &bb ||
                        instIt->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                    {
                        auto& neighborDependencies =
                            graph.getOrCreateNode(&bb)
                                .getOrCreateEdge(&graph.getOrCreateNode(instIt.getBasicBlock()))
                                .data;
                        neighborDependencies[const_cast<Local*>(local)] =
                            add_flag(neighborDependencies[const_cast<Local*>(local)], DataDependencyType::FLOW);
                    }
                    if(instIt->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                    {
                        auto& neighborDependencies =
                            graph.getOrCreateNode(&bb)
                                .getOrCreateEdge(&graph.getOrCreateNode(instIt.getBasicBlock()))
                                .data;
                        neighborDependencies[const_cast<Local*>(local)] =
                            add_flag(neighborDependencies[const_cast<Local*>(local)], DataDependencyType::PHI);
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
                            inst->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                        {
                            auto& neighborDependencies =
                                graph.getOrCreateNode(&bb)
                                    .getOrCreateEdge(&graph.getOrCreateNode(instIt.getBasicBlock()))
                                    .data;
                            neighborDependencies[const_cast<Local*>(local)] =
                                add_flag(neighborDependencies[const_cast<Local*>(local)], DataDependencyType::ANTI);
                        }
                    });
            }
        });
    }
}

#ifdef DEBUG_MODE
LCOV_EXCL_START
static std::string toEdgeLabel(const DataDependency& dependency)
{
    std::string label;

    for(const auto& pair : dependency)
    {
        label.append(" ").append(pair.first->name);
    }

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
        auto weakEdgeFunc = [](const DataDependency& dep) -> bool {
            return std::all_of(dep.begin(), dep.end(),
                [](const auto& pair) -> bool { return !has_flag(pair.second, DataDependencyType::FLOW); });
        };
        DebugGraph<BasicBlock*, DataDependency, DataDependencyEdge::Directed>::dumpGraph<DataDependencyGraph>(
            *graph, "/tmp/vc4c-data-dependencies.dot", nameFunc, weakEdgeFunc, toEdgeLabel);
    });
    LCOV_EXCL_STOP
#endif

    PROFILE_END(createDataDependencyGraph);
    return graph;
}
