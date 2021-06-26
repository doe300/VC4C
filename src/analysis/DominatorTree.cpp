/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DominatorTree.h"

#include "../Profiler.h"
#include "DebugGraph.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::analysis;

bool DominatorTreeNodeBase::dominates(const DominatorTreeNodeBase& other) const
{
    return other.isDominatedBy(*this);
}

bool DominatorTreeNodeBase::isDominatedBy(const DominatorTreeNodeBase& other) const
{
    auto self = reinterpret_cast<const DominatorTreeNode*>(this);
    auto dominator = self;
    while((dominator = dominator->getSinglePredecessor()))
    {
        if(dominator == &other)
            return true;
    }
    return false;
}

FastSet<const DominatorTreeNodeBase*> DominatorTreeNodeBase::getDominators() const
{
    FastSet<const DominatorTreeNodeBase*> dominators;
    auto self = reinterpret_cast<const DominatorTreeNode*>(this);
    auto dominator = self;
    while((dominator = dominator->getSinglePredecessor()))
        dominators.emplace(dominator);
    return dominators;
}

FastSet<const DominatorTreeNodeBase*> DominatorTreeNodeBase::getDominatedNodes() const
{
    FastSet<const DominatorTreeNodeBase*> dominatedNodes;
    auto self = reinterpret_cast<const DominatorTreeNode*>(this);
    self->forAllOutgoingEdges([&](const DominatorTreeNode& successor, const auto& edge) -> bool {
        dominatedNodes.emplace(&successor);
        auto tmp = successor.getDominatedNodes();
        dominatedNodes.insert(tmp.begin(), tmp.end());
        return true;
    });
    return dominatedNodes;
}

static std::unique_ptr<DominatorTree> createTreeInner(
    ControlFlowGraph& cfg, FastSet<const CFGNode*> (*getCandidates)(const CFGNode& node), const std::string& treeName)
{
    PROFILE_SCOPE(createDominatorTree);
    std::unique_ptr<DominatorTree> tree(new DominatorTree(cfg.getNodes().size()));

    FastMap<const CFGNode*, FastSet<const CFGNode*>> predecessors;
    FastMap<const CFGNode*, FastAccessList<const CFGNode*>> dominatorChains;

    // 1. handle direct predecessors and single direct dominators
    for(const auto& node : cfg.getNodes())
    {
        auto& entry = tree->getOrCreateNode(&node.second);
        auto tmp = getCandidates(node.second);

        if(tmp.empty())
        {
            // node has no predecessors, therefore no dominators, nothing further to do
            dominatorChains.emplace(&node.second, FastAccessList<const CFGNode*>{nullptr});
        }
        else if(tmp.size() == 1)
        {
            // the one candidate is the dominator
            tree->getOrCreateNode(*tmp.begin()).addEdge(&entry, {});
            auto it = dominatorChains.emplace(&node.second, FastAccessList<const CFGNode*>{*tmp.begin()}).first;

            // try to extend the dominator chain with an existing one
            auto domIt = dominatorChains.find(*tmp.begin());
            if(domIt != dominatorChains.end())
                it->second.insert(it->second.end(), domIt->second.begin(), domIt->second.end());
        }
        else
            predecessors.emplace(&node.second, std::move(tmp));
    }

    // 2. check whether we can resolve transitive dominators
    while(!predecessors.empty())
    {
        // extend all dominator chains by appending other known chains
        for(auto& chain : dominatorChains)
        {
            if(!chain.second.empty() && chain.second.back())
            {
                auto chainIt = dominatorChains.find(chain.second.back());
                if(chainIt != dominatorChains.end())
                    chain.second.insert(chain.second.end(), chainIt->second.begin(), chainIt->second.end());
            }
        }

        auto it = predecessors.begin();
        while(it != predecessors.end())
        {
            auto& pendingNode = *it;
            // need to find (if possible yet) for all predecessors the one node where the dominator chain (path in the
            // dominator tree) of all predecessors merge. E.g. for node A, predecessor B with dominators B -> C -> D ->
            // E and predecessor F with dominators F -> D -> E, need to find D.
            FastMap<const CFGNode*, FastAccessList<const CFGNode*>*> predecessorDominatorChains;
            bool allPredecessorsProcessed = true;
            for(auto predecessor : pendingNode.second)
            {
                auto domIt = dominatorChains.find(predecessor);
                if(domIt != dominatorChains.end())
                    predecessorDominatorChains.emplace(predecessor, &domIt->second);
                else
                {
                    allPredecessorsProcessed = false;
                    break;
                }
            }

            if(allPredecessorsProcessed)
            {
                // try to connect the dominator chains to a point where we have a single dominator
                auto chainIt = predecessorDominatorChains.begin();
                // initially take the first chain
                auto dominatorCandidates = *chainIt->second;
                // prepend the first predecessor itself to the chain, since it might be in the dominator chain of
                // another predecessor
                dominatorCandidates.insert(dominatorCandidates.begin(), chainIt->first);
                bool loopAborted = false;
                for(++chainIt; chainIt != predecessorDominatorChains.end(); ++chainIt)
                {
                    // find the point in both chains, where they merge (if any)
                    auto domIt =
                        std::find_if(dominatorCandidates.begin(), dominatorCandidates.end(), [&](const CFGNode* node) {
                            // also check for the other predecessor itself
                            return chainIt->first == node ||
                                std::find(chainIt->second->begin(), chainIt->second->end(), node) !=
                                chainIt->second->end();
                        });
                    if(domIt == dominatorCandidates.end())
                    {
                        loopAborted = true;
                        break;
                    }
                    // remove all preceeding dominator candidates, since they are not valid anymore
                    dominatorCandidates.erase(dominatorCandidates.begin(), domIt);
                }

                if(!loopAborted && !dominatorCandidates.empty())
                {
                    // we found or new dominator
                    if(auto first = dominatorCandidates.front())
                        tree->assertNode(first).addEdge(&tree->assertNode(pendingNode.first), {});
                    dominatorChains.emplace(pendingNode.first, std::move(dominatorCandidates));
                    it = predecessors.erase(it);
                    continue;
                }
            }
            ++it;
        }
    }

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const CFGNode* node) -> std::string { return node->key->to_string(); };
        DebugGraph<const CFGNode*, DominationRelation, Directionality::DIRECTED>::dumpGraph<DominatorTree>(
            *tree, "/tmp/vc4c-" + treeName + ".dot", nameFunc);
    });
    LCOV_EXCL_STOP
#endif
    return tree;
}

static FastSet<const CFGNode*> getDominatorCandidates(const CFGNode& node)
{
    // check all incoming edges that are not back edges
    FastSet<const CFGNode*> possibleDominators;
    std::size_t numIncomingEdges = 0;
    node.forAllIncomingEdges([&](const CFGNode& predecessor, const CFGEdge& edge) -> bool {
        ++numIncomingEdges;
        if(!edge.data.isBackEdge(predecessor.key))
            possibleDominators.emplace(&predecessor);
        return true;
    });

    // if there is only exactly 1 incoming edge, this is our dominator, even if we jump back to it at some point
    if(numIncomingEdges == 1)
        node.forAllIncomingEdges([&](const CFGNode& predecessor, const CFGEdge& edge) -> bool {
            possibleDominators.emplace(&predecessor);
            return true;
        });

    // don't use the node itself as dominator (e.g. for single-block loop)
    possibleDominators.erase(&node);

    return possibleDominators;
}

std::unique_ptr<DominatorTree> DominatorTree::createDominatorTree(ControlFlowGraph& cfg)
{
    return createTreeInner(cfg, getDominatorCandidates, "dominators");
}

static FastSet<const CFGNode*> getPostdominatorCandidates(const CFGNode& node)
{
    // check all outgoing edges that are not back edges or bidirectional (e.g. for small loops)
    FastSet<const CFGNode*> possiblePostdominators;
    std::size_t numOutgoingEdges = 0;
    node.forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
        ++numOutgoingEdges;
        if(!edge.data.isBackEdge(node.key) && !edge.data.isWorkGroupLoop && edge.getDirection() != Direction::BOTH)
            possiblePostdominators.emplace(&successor);
        return true;
    });

    // if there is only exactly 1 outgoing edge, this is our postdominator, even if we jump back to it at some point
    if(numOutgoingEdges == 1)
        node.forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
            possiblePostdominators.emplace(&successor);
            return true;
        });

    // don't use the node itself as postdominator (e.g. for single-block loop)
    possiblePostdominators.erase(&node);

    return possiblePostdominators;
}

std::unique_ptr<DominatorTree> DominatorTree::createPostdominatorTree(ControlFlowGraph& cfg)
{
    return createTreeInner(cfg, getPostdominatorCandidates, "postdominators");
}
