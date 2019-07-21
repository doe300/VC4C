
#include "ControlFlowLoop.h"

#include "ControlFlowGraph.h"
#include "logger.h"

using namespace vc4c;

const CFGNode* ControlFlowLoop::findPredecessor() const
{
    const CFGNode* predecessor = nullptr;
    for(const CFGNode* node : *this)
    {
        node->forAllIncomingEdges([this, &predecessor](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(std::find(begin(), end(), &neighbor) == end())
            {
                // the relation is backwards and node is not within this loop -> predecessor
                if(predecessor != nullptr)
                    // TODO testing/boost-compute/test_accumulator.cl throws errors here, because it has multiple
                    // predecessors (in kernel "reduce")! How to handle them?
                    throw CompilationError(CompilationStep::GENERAL, "Found multiple predecessors for CFG loop",
                        neighbor.key->to_string());

                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found predecessor for CFG loop: " << neighbor.key->to_string() << logging::endl);
                predecessor = &neighbor;
            }
            return true;
        });
    }
    return predecessor;
}

FastAccessList<const CFGNode*> ControlFlowLoop::findPredecessors() const
{
    FastAccessList<const CFGNode*> predecessors;
    for(const CFGNode* node : *this)
    {
        node->forAllIncomingEdges([this, &predecessors](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(std::find(begin(), end(), &neighbor) == end())
            {
                predecessors.push_back(&neighbor);
            }
            return true;
        });
    }
    return predecessors;
}

const CFGNode* ControlFlowLoop::findSuccessor() const
{
    const CFGNode* successor = nullptr;
    for(const CFGNode* node : *this)
    {
        node->forAllOutgoingEdges([this, &successor](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(std::find(begin(), end(), &neighbor) == end())
            {
                // the relation is forward and node is not within this loop -> successor
                if(successor != nullptr)
                    throw CompilationError(
                        CompilationStep::GENERAL, "Found multiple successors for CFG loop", neighbor.key->to_string());

                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found successor for CFG loop: " << neighbor.key->to_string() << logging::endl);
                successor = &neighbor;
            }
            return true;
        });
    }
    return successor;
}

Optional<InstructionWalker> ControlFlowLoop::findInLoop(const intermediate::IntermediateInstruction* inst) const
{
    for(const CFGNode* node : *this)
    {
        if(auto it = node->key->findWalkerForInstruction(inst, node->key->walkEnd()))
            return it;
    }
    return {};
}

bool ControlFlowLoop::includes(const ControlFlowLoop& other) const
{
    if(*this == other)
        return false;

    for(auto otherItr : other)
    {
        auto thisItr = std::find_if(begin(), end(), [&](const CFGNode* node) { return node->key == otherItr->key; });
        if(thisItr == end())
        {
            return false;
        }
    }

    return true;
}

LoopInclusionTreeNodeBase::~LoopInclusionTreeNodeBase() noexcept = default;

std::unique_ptr<LoopInclusionTree> vc4c::createLoopInclusingTree(FastAccessList<ControlFlowLoop>& loops)
{
    std::unique_ptr<LoopInclusionTree> inclusionTree(new LoopInclusionTree());
    for(auto& loop1 : loops)
    {
        for(auto& loop2 : loops)
        {
            if(loop1.includes(loop2))
            {
                auto& node1 = inclusionTree->getOrCreateNode(&loop1);
                auto& node2 = inclusionTree->getOrCreateNode(&loop2);
                if(!node1.isAdjacent(&node2))
                {
                    node1.addEdge(&node2, {});
                }
            }
        }
    }
    // Remove extra relations.
    for(auto& loop : loops)
    {
        auto& currentNode = inclusionTree->getOrCreateNode(&loop);

        // Remove parent nodes except node which has longest path to the root node.
        LoopInclusionTreeNodeBase* longestNode = nullptr;
        int longestLength = -1;
        currentNode.forAllIncomingEdges([&](LoopInclusionTreeNode& parent, LoopInclusionTreeEdge&) -> bool {
            auto parentNode = &parent;
            int length = static_cast<int>(parentNode->longestPathLengthToRoot());

            if(length > longestLength)
            {
                longestNode = parentNode;
                longestLength = length;
            }

            return true;
        });

        if(longestNode)
        {
            std::vector<LoopInclusionTreeNode*> nonConnectedEdges;

            currentNode.forAllIncomingEdges([&](LoopInclusionTreeNode& other, LoopInclusionTreeEdge&) -> bool {
                auto otherNode = &other;
                if(longestNode != otherNode)
                {
                    // To avoid finishing loop
                    nonConnectedEdges.push_back(otherNode);
                }
                return true;
            });

            for(auto& otherNode : nonConnectedEdges)
            {
                otherNode->removeAsNeighbor(&currentNode);
            }
        }
    }

    return inclusionTree;
}
