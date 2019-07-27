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

std::unique_ptr<InterferenceGraph> InterferenceGraph::createGraph(Method& method)
{
    // TODO try to optimize
    PROFILE_START(createInterferenceGraph);
    std::unique_ptr<InterferenceGraph> graph(new InterferenceGraph(method.getNumLocals()));

    FastMap<BasicBlock*, LivenessAnalysis> livenesses;
    livenesses.reserve(method.getCFG().getNodes().size());
    for(auto& block : method)
    {
        auto& blockAnalysis = livenesses[&block];
        blockAnalysis(block);
    }

    // tracks local life-ranges stemming from local being used in succeeding blocks (and written in this block or
    // before)
    FastMap<const intermediate::IntermediateInstruction*, FastSet<const Local*>> additionalLocals;
    for(auto& pair : livenesses)
    {
        if(!pair.second.getStartResult().empty())
        {
            // there are dependencies from other blocks. For each local, walk the CFG back until we meet the write or
            // until we meet another liveness-range of the local
            // TODO is there a better/more efficient way? Can't we combine the walking for all locals from start? Since
            // predecessor blocks are the same
            for(auto& local : pair.second.getStartResult())
            {
                FastSet<BasicBlock*> blocksVisited;
                InstructionVisitor v{[&](InstructionWalker& it) -> InstructionVisitResult {
                                         // skip the own label itself
                                         if(it.get() == pair.first->getLabel())
                                             return InstructionVisitResult::CONTINUE;
                                         if(it.get<intermediate::BranchLabel>())
                                         {
                                             if(it.getBasicBlock()->isStartOfMethod())
                                                 // do not repeat work-group loop
                                                 return InstructionVisitResult::STOP_BRANCH;
                                             if(blocksVisited.find(it.getBasicBlock()) != blocksVisited.end())
                                                 // loop, abort after one iteration
                                                 return InstructionVisitResult::STOP_BRANCH;
                                             blocksVisited.emplace(it.getBasicBlock());
                                         }

                                         auto addLocalsIt = additionalLocals.find(it.get());
                                         auto& lives = livenesses.at(it.getBasicBlock()).getResult(it.get());
                                         if(it->writesLocal(local) || lives.find(local) != lives.end() ||
                                             (addLocalsIt != additionalLocals.end() &&
                                                 addLocalsIt->second.find(local) != addLocalsIt->second.end()))
                                             return InstructionVisitResult::STOP_BRANCH;
                                         additionalLocals[it.get()].emplace(local);
                                         return InstructionVisitResult::CONTINUE;
                                     },
                    false, true};
                v.visitReverse(pair.first->walk(), &method.getCFG());
            }
        }
    }

    for(auto& block : method)
    {
        auto& blockAnalysis = livenesses[&block];
        for(const auto& inst : block)
        {
            // combined operations can write multiple locals
            const auto combInstr = dynamic_cast<const intermediate::CombinedOperation*>(inst.get());
            if(combInstr && combInstr->op1 && combInstr->op1->checkOutputLocal() && combInstr->op2 &&
                combInstr->op2->checkOutputLocal() &&
                combInstr->op1->getOutput()->local() != combInstr->op2->getOutput()->local())
            {
                graph->getOrCreateNode(combInstr->op1->getOutput()->local())
                    .getOrCreateEdge(
                        &graph->getOrCreateNode(combInstr->op2->getOutput()->local()), InterferenceType::USED_TOGETHER)
                    .data = InterferenceType::USED_TOGETHER;
            }
            // instructions in general can read multiple locals
            FastSet<Local*> localsRead;
            // we have a maximum of 4 locals per (combined) instruction
            localsRead.reserve(4);
            inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                if(has_flag(type, LocalUse::Type::READER) && !loc->type.isLabelType())
                    localsRead.emplace(const_cast<Local*>(loc));
            });
            if(localsRead.size() > 1)
            {
                for(auto locIt = localsRead.begin(); locIt != localsRead.end(); ++locIt)
                {
                    auto& firstNode = graph->getOrCreateNode(*locIt);
                    auto locIt2 = locIt;
                    for(++locIt2; locIt2 != localsRead.end(); ++locIt2)
                    {
                        firstNode.getOrCreateEdge(&graph->getOrCreateNode(*locIt2), InterferenceType::USED_TOGETHER)
                            .data = InterferenceType::USED_TOGETHER;
                    }
                }
            }

            auto blockResults = blockAnalysis.getResult(inst.get());
            auto addIt = additionalLocals.find(inst.get());
            if(addIt != additionalLocals.end())
                blockResults.insert(addIt->second.begin(), addIt->second.end());

            for(auto locIt = blockResults.begin(); locIt != blockResults.end(); ++locIt)
            {
                auto& firstNode = graph->getOrCreateNode(const_cast<Local*>(*locIt));
                auto locIt2 = locIt;
                for(++locIt2; locIt2 != blockResults.end(); ++locIt2)
                {
                    firstNode.getOrCreateEdge(
                        &graph->getOrCreateNode(const_cast<Local*>(*locIt2)), InterferenceType::USED_SIMULTANEOUSLY);
                }
            }
        }
    }

    PROFILE_END(createInterferenceGraph);

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const Local* loc) -> std::string { return loc->name; };
        auto edgeFunc = [](InterferenceType type) -> bool { return !has_flag(type, InterferenceType::USED_TOGETHER); };
        DebugGraph<Local*, InterferenceType, Directionality::UNDIRECTED>::dumpGraph(
            *graph, "/tmp/vc4c-interference.dot", nameFunc, edgeFunc);
    });
    LCOV_EXCL_STOP
#endif
    return graph;
}
