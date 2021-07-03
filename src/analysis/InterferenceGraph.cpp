/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "InterferenceGraph.h"

#include "../Method.h"
#include "../Profiler.h"
#include "ControlFlowGraph.h"
#include "DebugGraph.h"
#include "LivenessAnalysis.h"

using namespace vc4c;
using namespace vc4c::analysis;

FastSet<InterferenceNode*> InterferenceGraph::findOverfullNodes(std::size_t numNeighbors)
{
    FastSet<InterferenceNode*> results;

    for(auto& node : nodes)
    {
        if(node.second.getEdgesSize() >= numNeighbors)
            results.emplace(&node.second);
    }

    return results;
}

std::unique_ptr<InterferenceGraph> InterferenceGraph::createGraph(
    Method& method, const GlobalLivenessAnalysis* globalLivenessAnalysis)
{
    PROFILE_START(createInterferenceGraph);
    std::unique_ptr<InterferenceGraph> graph_ptr(new InterferenceGraph(method.getNumLocals()));
    auto& graph = *graph_ptr;

    GlobalLivenessAnalysis tmpAnalysis(true);
    if(!globalLivenessAnalysis)
        tmpAnalysis(method);
    auto& livenessAnalysis = globalLivenessAnalysis ? *globalLivenessAnalysis : tmpAnalysis;

    PROFILE_START(LivenessToInterference);
    for(auto& block : method)
    {
        if(block.empty())
            // empty block means no changes in live locals -> no changes in interference
            continue;

        auto& blockAnalysis = livenessAnalysis.getLocalAnalysis(block);
        auto& livenessChanges = livenessAnalysis.getChanges(block);
        // to update only the interference for local lifetime changes, we re-create the changes given from the
        // LivenessChangesAnalysis on the list of tracked live locals.
        auto& liveLocals = blockAnalysis.getEndResult();
        FastMap<const Local*, InterferenceNode*> liveNodes(liveLocals.size());
        for(auto loc : liveLocals)
            liveNodes.emplace(loc, &graph.getOrCreateNode(loc));
        // NOTE: iterate in reverse order to be able to track the changes in live locals (which are also generated in
        // reverse order) correctly
        for(auto it = block.rbegin(); it != block.rend(); ++it)
        {
            // combined operations can write multiple locals
            const auto combInstr = dynamic_cast<const intermediate::CombinedOperation*>(it->get());
            if(combInstr && combInstr->getFirstOp() && combInstr->getSecondOp())
            {
                auto firstOut = combInstr->getFirstOp()->checkOutputLocal();
                auto secondOut = combInstr->getSecondOp()->checkOutputLocal();
                if(firstOut && secondOut && firstOut != secondOut)
                {
                    graph.getOrCreateNode(firstOut)
                        .getOrCreateEdge(&graph.getOrCreateNode(secondOut), InterferenceType::USED_TOGETHER)
                        .data = InterferenceType::USED_TOGETHER;
                }
            }
            // instructions in general can read multiple locals
            // we have a maximum of 4 locals per (combined) instruction
            FastSet<InterferenceNode*> localsRead(4);
            (*it)->forReadLocals([&](const Local* loc, const intermediate::IntermediateInstruction& inst) {
                if(!loc->type.isLabelType())
                    localsRead.emplace(&graph.getOrCreateNode(loc));
            });
            if(localsRead.size() > 1)
            {
                for(auto locIt = localsRead.begin(); locIt != localsRead.end(); ++locIt)
                {
                    auto& firstNode = **locIt;
                    auto locIt2 = locIt;
                    for(++locIt2; locIt2 != localsRead.end(); ++locIt2)
                    {
                        firstNode.getOrCreateEdge(*locIt2, InterferenceType::USED_TOGETHER).data =
                            InterferenceType::USED_TOGETHER;
                    }
                }
            }

            auto& changes = livenessChanges.getResult(it->get());
            // Most live locals for one instruction are also live for the previous/next instruction (the only changes
            // are the one given by the LivenessChangeAnalysis). Therefore, we only need to create a new edge for all
            // newly added live locals (times all existing live locals), instead of all live locals times all live
            // locals.

            for(auto loc : changes.removedLocals)
                liveNodes.erase(loc);

            for(auto loc : changes.addedLocals)
            {
                auto& firstNode = graph.getOrCreateNode(loc);
                for(auto node : liveNodes)
                {
                    if(node.second != &firstNode)
                        // the node could already be in the list (e.g. when read multiple times) and creating an edge
                        // between a node and itself would cause allocation errors.
                        firstNode.getOrCreateEdge(node.second, InterferenceType::USED_SIMULTANEOUSLY);
                }
                liveNodes.emplace(loc, &firstNode);
            }
        }
    }
    PROFILE_END(LivenessToInterference);

    PROFILE_END(createInterferenceGraph);

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const Local* loc) -> std::string { return loc->name; };
        auto edgeFunc = [](InterferenceType type) -> bool { return !has_flag(type, InterferenceType::USED_TOGETHER); };
        DebugGraph<const Local*, InterferenceType, Directionality::UNDIRECTED>::dumpGraph(
            graph, "/tmp/vc4c-interference.dot", nameFunc, edgeFunc);
    });
    LCOV_EXCL_STOP
#endif
    return graph_ptr;
}
