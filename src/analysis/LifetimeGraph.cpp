/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LifetimeGraph.h"

#include "../InstructionWalker.h"
#include "../Module.h"
#include "../Profiler.h"
#include "DebugGraph.h"

#include "log.h"

using namespace vc4c;
using namespace vc4c::analysis;

/*
 * Whether this local is relevant to the analysis (e.g. could be lowered to the VPM)
 */
static bool isRelevant(const Local& local)
{
    if(local.is<Global>() || local.is<Parameter>())
        return local.type.getPointerType() && local.type.getPointerType()->addressSpace != AddressSpace::GENERIC &&
            local.type.getPointerType()->addressSpace != AddressSpace::GLOBAL;
    if(local.is<StackAllocation>())
        return true;
    return false;
}

static void overlapLocals(LifetimeGraph& graph, const FastSet<const Local*>& liveLocals)
{
    for(const Local* l1 : liveLocals)
    {
        auto& node = graph.getOrCreateNode(l1);
        node.reserveEdgesSize(liveLocals.size() - 1u);
        for(const Local* l2 : liveLocals)
        {
            if(l1 != l2)
            {
                auto& node2 = graph.getOrCreateNode(l2);
                node.addEdge(&node2, {});
            }
        }
    }
}

std::unique_ptr<LifetimeGraph> LifetimeGraph::createLifetimeGraph(Method& method)
{
    PROFILE_START(createLifetimeGraph);
    // memory objects which are live (used) at this moment
    FastSet<const Local*> liveLocals;

    std::unique_ptr<LifetimeGraph> graph(new LifetimeGraph());

    // add all globals and parameters (unless they are have __global address space)
    for(const Global& global : method.module.globalData)
    {
        if(isRelevant(global))
            liveLocals.emplace(&global);
    }
    for(const Parameter& param : method.parameters)
    {
        if(isRelevant(param))
            liveLocals.emplace(&param);
    }

    // mark all globals/parameters as overlapping
    overlapLocals(*graph, liveLocals);

    // TODO how to handle stack allocations without explicit lifetime boundaries?? E.g. for
    // testing/local_private_sotrage.cl
    method.forAllInstructions([&liveLocals, &graph](const intermediate::IntermediateInstruction& inst) -> void {
        if(auto lifetimeInst = dynamic_cast<const intermediate::LifetimeBoundary*>(&inst))
        {
            const Local* local = lifetimeInst->getStackAllocation().local();
            if(lifetimeInst->isLifetimeEnd)
                liveLocals.erase(local);
            else
            {
                liveLocals.emplace(local);
                // new local, mark all currently live locals as overlapping
                overlapLocals(*graph, liveLocals);
            }
        }
        // TODO this is not completely correct, would need to follow the control-flow from start to stop(s)
    });

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const Local* loc) -> std::string { return loc->name; };
        DebugGraph<const Local*, LifetimeRelation, Directionality::UNDIRECTED>::dumpGraph<LifetimeGraph>(
            *graph, "/tmp/vc4c-lifetimes.dot", nameFunc);
    });
    LCOV_EXCL_STOP
#endif

    PROFILE_END(createLifetimeGraph);
    return graph;
}

struct StackNodeSorter
{
    bool operator()(const LifetimeNode* n1, const LifetimeNode* n2) const
    {
        return n1->key->as<StackAllocation>()->size > n2->key->as<StackAllocation>()->size ||
            n1->key->as<StackAllocation>()->alignment > n2->key->as<StackAllocation>()->alignment || n1->key < n2->key;
    }
};

static void assignOffset(const StackAllocation* s, FastSet<LifetimeNode*>& processedNodes,
    SortedSet<LifetimeNode*, StackNodeSorter>::iterator it, std::size_t offset)
{
    s->offset = offset;
    processedNodes.emplace(*it);
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Assigned stack-allocated object " << s->to_string() << " to stack offset " << offset << logging::endl);
}

unsigned LifetimeGraph::calculateRequiredStackSize()
{
    SortedSet<LifetimeNode*, StackNodeSorter> stackNodes;

    for(std::pair<const Local* const, LifetimeNode>& node : nodes)
    {
        if(node.first->is<StackAllocation>())
            stackNodes.emplace(&node.second);
    }

    if(stackNodes.empty())
        return 0;

    // all stack allocations are sorted by descending size and alignment
    //-> any node which is used concurrently with a previous node can be placed at the same memory-area
    std::size_t currentSize = 0;
    FastSet<LifetimeNode*> processedNodes;
    for(auto it = stackNodes.begin(); it != stackNodes.end(); ++it)
    {
        if(processedNodes.find(*it) != processedNodes.end())
            // already processed, skip
            continue;

        // set stack-offset and add to processed nodes
        // TODO alignment
        assignOffset((*it)->key->as<StackAllocation>(), processedNodes, it, currentSize);

        // check all other outstanding nodes whether they can be assigned to the same offset
        for(auto it2 = it; it2 != stackNodes.end(); ++it2)
        {
            if(processedNodes.find(*it2) != processedNodes.end())
                // already processed, skip
                continue;
            if((*it)->isAdjacent(*it2))
                // life-times overlay, skip
                continue;
            // TODO alignment?!
            assignOffset((*it2)->key->as<StackAllocation>(), processedNodes, it, currentSize);
        }

        currentSize += (*it)->key->as<StackAllocation>()->size;
    }

    return static_cast<unsigned>(currentSize);
}
