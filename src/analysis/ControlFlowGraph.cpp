/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlowGraph.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "DebugGraph.h"
#include "DominatorTree.h"

#include "log.h"

#include <numeric>
#include <queue>
#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

bool CFGRelation::operator==(const CFGRelation& other) const
{
    return predecessors == other.predecessors;
}

LCOV_EXCL_START
std::string CFGRelation::getLabel() const
{
    std::string extra = isWorkGroupLoop ? " (wgl)" : "";
    if(backEdgeSource)
        extra += " (back edge from " + backEdgeSource->getLabel()->getLabel()->name + ")";
    if(std::all_of(predecessors.begin(), predecessors.end(), [](const auto& pair) -> bool { return !pair.second; }))
    {
        return "" + std::move(extra);
    }
    if(predecessors.empty())
        return "" + std::move(extra);
    const auto converter = [](const std::pair<const BasicBlock*, Optional<InstructionWalker>>& pair) -> std::string {
        auto branch = pair.second ? pair.second->get<intermediate::Branch>() : nullptr;
        if(branch)
            return "br " + branch->branchCondition.to_string() + (branch->isDynamicBranch() ? "(dyn)" : "");
        return "";
    };
    return std::accumulate(predecessors.begin(), predecessors.end(), std::string{},
               [&](const std::string& s, const auto& pair) -> std::string {
                   return (s.empty() ? s : (s + ", ")) + converter(pair);
               }) +
        std::move(extra);
}
LCOV_EXCL_STOP

InstructionWalker CFGRelation::getPredecessor(BasicBlock* source) const
{
    if(const auto& pred = predecessors.at(source))
        return pred.value();
    return source->walkEnd().previousInBlock();
}

bool CFGRelation::isImplicit(const BasicBlock* source) const
{
    const auto& pred = predecessors.at(source);
    return !pred.has_value();
}

bool CFGRelation::isBackEdge(const BasicBlock* source) const
{
    return backEdgeSource == source;
}

bool analysis::operator<(const CFGNode& one, const CFGNode& other)
{
    // sorts the CFG nodes by the order of the basic blocks
    // THIS IS IN REVERSE ORDER ON PURPOSE!
    const std::string& firstLabel = one.key->getLabel()->getLabel()->name;
    const std::string& secondLabel = other.key->getLabel()->getLabel()->name;

    if(firstLabel == BasicBlock::DEFAULT_BLOCK)
        return false;
    if(firstLabel == BasicBlock::LAST_BLOCK)
        return secondLabel != BasicBlock::LAST_BLOCK;

    return firstLabel > secondLabel;
}

CFGNode& ControlFlowGraph::getStartOfControlFlow()
{
    if(nodes.empty())
        throw CompilationError(CompilationStep::GENERAL, "Cannot get start of empty CFG!");
    // TODO return node without any predecessors?
    return assertNode(&(*nodes.begin()->first->getMethod().begin()));
}

CFGNode& ControlFlowGraph::getEndOfControlFlow()
{
    // return node without any successor,
    // if there are multiple (or none), throw
    CFGNode* candidate = nullptr;
    for(auto& pair : nodes)
    {
        CFGNode& node = pair.second;
        if(node.isSink())
        {
            if(candidate != nullptr)
            {
                logging::error() << "Candidate: " << candidate->key->to_string() << logging::endl;
                logging::error() << "Candidate: " << node.key->to_string() << logging::endl;
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
        return analysis::operator<(*x, *y);
    }
};

/**
 * For every header-tail pair (natural loop), walk for every node in the set of nodes dominated by the header
 * depth-first, until:
 * a) a node already known to be in the loop is reached -> node is part of the loop (also, all nodes on its way are
 * known part of the loop)
 * b) a node not part of the dominator set is reached -> this path not part of loop
 * c) a node known to not be part of the loop is reached -> this path not part of loop
 */
static bool walkThroughDominatorSet(const CFGNode* currentNode, const FastSet<const CFGNode*>& dominatorSet,
    ControlFlowLoop& loop, FastSet<const CFGNode*>& notInLoop, FastSet<const CFGNode*>& currentStack)
{
    if(loop.find(currentNode) != loop.end())
        // node is already known to be in the loop
        return true;
    if(notInLoop.find(currentNode) != notInLoop.end())
        return false;
    if(dominatorSet.find(currentNode) == dominatorSet.end())
        // node is not part of the dominator set -> not part of the loop
        return false;
    if(currentStack.find(currentNode) != currentStack.end())
        // this node is already checked (e.g. this is part of an inner loop)
        return false;

    currentStack.emplace(currentNode);
    bool anySuccessorInLoop = false;
    currentNode->forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge&) -> bool {
        if(&successor == currentNode)
            // skip, since we are already in process of checking this node
            return true;
        if(walkThroughDominatorSet(&successor, dominatorSet, loop, notInLoop, currentStack))
        {
            // there is a path to the header within the dominator set -> this node is part of the loop
            anySuccessorInLoop = true;
            loop.emplace(currentNode);
            // no need to continue checking successors for this node
            return false;
        }
        return true;
    });

    if(!anySuccessorInLoop)
        // there is no way from this node to the header within the dominator set, so all successive paths going
        // through this node will also not find the header
        notInLoop.emplace(currentNode);

    currentStack.erase(currentNode);
    return anySuccessorInLoop;
}

FastAccessList<ControlFlowLoop> ControlFlowGraph::findLoops(bool recursively, bool skipWorkGroupLoops)
{
    if(!loops)
        findAllLoops();

    FastAccessList<ControlFlowLoop> result;
    result.reserve(loops->size());
    for(auto& loop : *loops)
    {
        if(skipWorkGroupLoops && loop.isWorkGroupLoop())
            continue;

        // we copy on purpose to allow modifying of the CFG while having the list of previous loops
        result.emplace_back(loop);
    }

    if(!recursively)
    {
        // remove all outer loops
        for(auto it = result.begin(); it != result.end();)
        {
            bool includesLoop = false;
            for(auto& loop : *loops)
            {
                if(&loop != &*it && it->includes(loop))
                {
                    includesLoop = true;
                    break;
                }
            }
            if(includesLoop)
                it = result.erase(it);
            else
                ++it;
        }
    }
    return result;
}

void ControlFlowGraph::findAllLoops()
{
    loops = std::make_unique<FastAccessList<ControlFlowLoop>>();
    auto dominators = getDominatorTree();

    /*
     * Determine loops according to:
     *
     * "A loop is a set of CFG nodes S such that:
     *  - there exists a header node h in that dominates all nodes in S
     *  - from any node in S, there exists a path of directed edges to h."
     *
     * "The natural loop of a back edge (m -> n), where n dominates m, is the set of nodes x such that n dominates x and
     * there is a path from x to m not containing n"
     *
     * Source https://www.cs.princeton.edu/courses/archive/spring03/cs320/notes/loops.pdf#page=20
     * and
     * https://www.cs.colostate.edu/~mstrout/CS553Fall09/Slides/lecture12-control.ppt.pdf#page=6
     */

    PROFILE_SCOPE(findAllLoops);
    // For every CFG node, collect the set of all CFG nodes dominated by that node by walking up the dominator tree and
    // adding every node to the dominated-by set for everyone of its dominators.
    FastMap<const DominatorTreeNode*, FastSet<const CFGNode*>> dominatorSets;
    dominatorSets.reserve(dominators->getNodes().size());
    for(auto& node : dominators->getNodes())
    {
        auto currentNode = &node.second;
        while(auto dominatorNode = currentNode->getSinglePredecessor())
        {
            dominatorSets[dominatorNode].insert(node.first);
            currentNode = dominatorNode;
        }
        // instantiate the entry in the dominator set for single-node inner loops, otherwise they will be skipped
        // completely below
        dominatorSets[&node.second];
    }

    for(const auto& entry : dominatorSets)
    {
        // Since a node can be header for multiple loops, we first determine the possible tails. The tail of a natural
        // loop is the node (in the set of nodes dominated by the header) which is directly connected via the back edge
        // to the header (the node executing the repeat).
        FastMap<const CFGNode*, ControlFlowLoop> tailsToLoops;
        entry.first->key->forAllIncomingEdges([&](const CFGNode& predecessor, const CFGEdge& edge) -> bool {
            if(entry.second.find(&predecessor) != entry.second.end())
            {
                auto it = tailsToLoops.emplace(&predecessor, &edge).first;
                // add tail to the loop, required for the recursive algorithm below to find members of the loop
                it->second.emplace(&predecessor);
            }
            return true;
        });

        for(auto& loop : tailsToLoops)
        {
            // For performance reasons, we keep track of all nodes which are known to not have a path back to the header
            // without leaving the dominator set
            FastSet<const CFGNode*> notInLoop;
            for(const auto& node : entry.second)
            {
                // keep track of our current stack to avoid stack overflow due to infinite recursion
                FastSet<const CFGNode*> stack;
                walkThroughDominatorSet(node, entry.second, loop.second, notInLoop, stack);
            }
            // only here add the header to the loop, to prevent nodes which reach it through another (outer) loop from
            // being added to this (inner) loop
            loop.second.emplace(entry.first->key);
            // add loop to the result
            loops->emplace_back(std::move(loop.second));
        }

        // here handle single-block loops specially, since we do not need to walk through the whole dominator set if we
        // know there is only 1 block in the loop
        entry.first->key->forAllIncomingEdges([&](const CFGNode& predecessor, const CFGEdge& edge) -> bool {
            if(&predecessor == entry.first->key)
            {
                ControlFlowLoop loop(&edge);
                loop.emplace(&predecessor);
                loops->emplace_back(std::move(loop));
            }
            return true;
        });
    }

    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        for(const auto& loop : *loops)
            logging::debug() << "Found a control-flow loop: " << loop.to_string() << logging::endl;
    });
    LCOV_EXCL_STOP
}

std::shared_ptr<DominatorTree> ControlFlowGraph::getDominatorTree()
{
    if(!dominatorTree)
        dominatorTree = DominatorTree::createDominatorTree(*this);
    return dominatorTree;
}

LCOV_EXCL_START
void ControlFlowGraph::dumpGraph(
    const std::string& path, const std::function<std::string(const BasicBlock*)>& labelFunc) const
{
#ifdef DEBUG_MODE
    // XXX to be exact, would need bidirectional arrow [dir="both"] for compact loops
    auto nameFunc = [](const BasicBlock* bb) -> std::string { return bb->getLabel()->getLabel()->name; };
    auto edgeLabelFunc = [](const CFGRelation& r) -> std::string { return r.getLabel(); };
    DebugGraph<BasicBlock*, CFGRelation, CFGEdge::Directed>::dumpGraph<ControlFlowGraph>(
        *this, path, labelFunc ? labelFunc : nameFunc,
        [](const CFGRelation& rel) -> bool {
            return std::all_of(rel.predecessors.begin(), rel.predecessors.end(),
                [](const auto& pair) -> bool { return !pair.second; });
        },
        edgeLabelFunc);
#endif
}
LCOV_EXCL_STOP

static void markDepthFirst(const CFGNode& node, FastSet<const CFGNode*>& visitedNodes,
    FastMap<const CFGNode*, std::size_t>& counters, std::size_t& counter)
{
    if(visitedNodes.find(&node) != visitedNodes.end())
        return;
    visitedNodes.emplace(&node);

    node.forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) {
        markDepthFirst(successor, visitedNodes, counters, counter);
        return true;
    });

    counters[&node] = counter++;
}

static void updateBackEdges(ControlFlowGraph& graph, CFGNode* startOfControlFlow)
{
    PROFILE_SCOPE(updateBackEdges);
    // 1. mark with counter value after traversal of sub-tree
    FastMap<const CFGNode*, std::size_t> counters;
    counters.reserve(graph.getNodes().size());
    std::size_t counter = 0;
    FastSet<const CFGNode*> visitedNodes;
    visitedNodes.reserve(graph.getNodes().size());
    markDepthFirst(*startOfControlFlow, visitedNodes, counters, counter);

    // 2. check all edges whether we have an edge from lower number to higher number
    visitedNodes.clear();
    visitedNodes.reserve(graph.getNodes().size());
    std::queue<CFGNode*> pendingNodes;
    pendingNodes.push(startOfControlFlow);
    while(!pendingNodes.empty())
    {
        auto currentNode = pendingNodes.front();
        pendingNodes.pop();

        if(!visitedNodes.insert(currentNode).second)
            // it may happen that we add a node multiple times to the pending nodes, so for the second and all following
            // calls, just skip it
            continue;

        currentNode->forAllOutgoingEdges([&](CFGNode& next, CFGEdge& edge) -> bool {
            if(&next == currentNode)
                // single-node loop
                edge.data.backEdgeSource = currentNode->key;
            else if(visitedNodes.find(&next) != visitedNodes.end())
                edge.data.backEdgeSource = counters.at(currentNode) < counters.at(&next) ? currentNode->key : nullptr;
            else
            {
                edge.data.backEdgeSource = nullptr;
                pendingNodes.push(&next);
            }
            return true;
        });
    }
}

void ControlFlowGraph::updateOnBlockInsertion(Method& method, BasicBlock& newBlock)
{
    /*
     * 1. insert new node
     * 2. remove fall-through edge from previous to next block
     * 3. add fall-through edge to next block
     * 4. check and add fall-through edge from previous block
     */
    auto nodePtr = findNode(&newBlock);
    if(nodePtr)
        // node is already in, so we do not need to insert it
        return;
    auto& node = getOrCreateNode(&newBlock);
    auto blockIt = std::find_if(
        method.begin(), method.end(), [&](const BasicBlock& block) -> bool { return &block == &newBlock; });
    auto prevBlockIt = blockIt;
    auto nextBlockIt = blockIt;
    ++nextBlockIt;
    if(blockIt != method.begin())
    {
        auto& prevNode = assertNode(&(*(--prevBlockIt)));
        CFGEdge* fallThroughEdge = nullptr;
        prevNode.forAllOutgoingEdges([&](CFGNode& successor, CFGEdge& edge) -> bool {
            if(edge.data.isImplicit(prevNode.key))
            {
                if(fallThroughEdge)
                {
                    logging::error() << fallThroughEdge->data.getPredecessor(prevNode.key)->to_string()
                                     << logging::endl;
                    logging::error() << edge.data.getPredecessor(prevNode.key)->to_string() << logging::endl;

                    throw CompilationError(CompilationStep::GENERAL, "Multiple implicit branches from basic block",
                        prevNode.key->to_string());
                }
                fallThroughEdge = &edge;
            }
            return true;
        });
        if(fallThroughEdge)
        {
            fallThroughEdge->data.predecessors.erase(prevNode.key);
            fallThroughEdge->removeInput(prevNode);
            if(fallThroughEdge->data.predecessors.empty())
                // do not remove any edge in the other direction, if there is one
                prevNode.removeEdge(*fallThroughEdge);
            auto& edge = prevNode.getOrCreateEdge(&node, CFGRelation{}).addInput(prevNode);
            edge.data.predecessors.emplace(prevNode.key, Optional<InstructionWalker>{});
        }
    }
    if((nextBlockIt) != method.end())
    {
        auto& nextNode = assertNode(&(*(nextBlockIt)));
        auto& edge = node.getOrCreateEdge(&nextNode, CFGRelation{}).addInput(node);
        edge.data.predecessors.emplace(node.key, Optional<InstructionWalker>{});
    }

    // clear cached dominator tree and loops
    dominatorTree.reset();
    loops.reset();
}

void ControlFlowGraph::updateOnBlockRemoval(Method& method, BasicBlock& oldBlock)
{
    /*
     * 1. assert on explicit edges to node
     * 2. remove node (if exists)
     * 3. check and add fall-though edge from previous to next node
     */
    auto nodePtr = findNode(&oldBlock);
    if(!nodePtr)
        // node is already removed
        return;
    CFGEdge* fallThroughEdge = nullptr;
    nodePtr->forAllIncomingEdges([&](CFGNode& predecessor, CFGEdge& edge) -> bool {
        if(!edge.data.isImplicit(predecessor.key))
            throw CompilationError(CompilationStep::GENERAL, "Explicit jump to basic block, cannot remove basic block",
                oldBlock.to_string());
        else if(fallThroughEdge)
        {
            logging::error()
                << fallThroughEdge->data.getPredecessor(fallThroughEdge->getOtherNode(*nodePtr).key)->to_string()
                << logging::endl;
            logging::error() << edge.data.getPredecessor(predecessor.key)->to_string() << logging::endl;
            throw CompilationError(
                CompilationStep::GENERAL, "Multiple implicit branches to basic block", oldBlock.to_string());
        }
        else
            fallThroughEdge = &edge;
        return true;
    });
    if(fallThroughEdge)
    {
        auto blockIt = std::find_if(
            method.begin(), method.end(), [&](const BasicBlock& block) -> bool { return &block == &oldBlock; });
        auto nextBlockIt = blockIt;
        if((++nextBlockIt) != method.end())
        {
            auto& nextNode = assertNode(&(*nextBlockIt));
            auto& prevNode = fallThroughEdge->getOtherNode(*nodePtr);
            auto& edge = prevNode.getOrCreateEdge(&nextNode).addInput(prevNode);
            // if there is already an edge (e.g. conditional jump), don't overwrite with fall-through
            edge.data.predecessors.emplace(prevNode.key, Optional<InstructionWalker>{});
        }
    }
    eraseNode(&oldBlock);

    // clear cached dominator tree and loops
    dominatorTree.reset();
    loops.reset();
}

void ControlFlowGraph::updateOnBranchInsertion(Method& method, InstructionWalker it)
{
    /*
     * 1. insert edge from block to destination label
     * 2. check whether branches cover all in this block and remove fall-through edge to next block
     */
    auto& node = assertNode(it.getBasicBlock());
    CFGEdge* fallThroughEdge = nullptr;
    for(auto target : it.get<intermediate::Branch>()->getTargetLabels())
    {
        // may not yet exist, e.g. for forward branches
        auto targetBlock = method.findBasicBlock(target);
        if(!targetBlock)
            throw CompilationError(CompilationStep::GENERAL,
                "Branch target does not belong to any basic block, make sure the block is inserted before the branch",
                it->to_string());
        auto& nextNode = getOrCreateNode(targetBlock);

        auto& edge = node.getOrCreateEdge(&nextNode).addInput(node);
        // overwrite possible fall-through edge
        edge.data.predecessors[node.key] = it;
        edge.data.isWorkGroupLoop = it->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_LOOP);
    }

    node.forAllOutgoingEdges([&](CFGNode& successor, CFGEdge& edge) -> bool {
        if(edge.data.isImplicit(node.key))
        {
            if(fallThroughEdge)
            {
                logging::error() << fallThroughEdge->data.getPredecessor(node.key)->to_string() << logging::endl;
                logging::error() << edge.data.getPredecessor(node.key)->to_string() << logging::endl;

                throw CompilationError(
                    CompilationStep::GENERAL, "Multiple implicit branches from basic block", node.key->to_string());
            }
            fallThroughEdge = &edge;
        }
        return true;
    });

    if(fallThroughEdge && !node.key->fallsThroughToNextBlock(false))
    {
        // we have a fall-through edge left but the block no longer falls through
        fallThroughEdge->data.predecessors.erase(node.key);
        fallThroughEdge->removeInput(node);
        if(fallThroughEdge->data.predecessors.empty())
            // only remove the edge completely, if there was no branch in the other direction
            node.removeEdge(*fallThroughEdge);
    }

    // update back edges
    updateBackEdges(*this, &getStartOfControlFlow());

    // clear cached dominator tree and loops
    dominatorTree.reset();
    loops.reset();
}

void ControlFlowGraph::updateOnBranchRemoval(
    Method& method, BasicBlock& affectedBlock, const FastSet<const Local*>& branchTargets)
{
    /*
     * 1. remove edge from block to destination label
     * 2. check whether branches cover all in this block and add fall-through edge to next block
     */
    auto& node = assertNode(&affectedBlock);
    for(auto branchTarget : branchTargets)
    {
        auto& destNode = assertNode(method.findBasicBlock(branchTarget));

        CFGEdge* branchEdge = nullptr;
        node.forAllOutgoingEdges([&](CFGNode& successor, CFGEdge& edge) -> bool {
            if(successor.key == destNode.key)
            {
                if(branchEdge)
                    throw CompilationError(CompilationStep::GENERAL, "Multiple edges between two basic blocks",
                        branchEdge->data.getLabel());
                branchEdge = &edge;
            }
            return true;
        });

        if(!branchEdge)
            throw CompilationError(
                CompilationStep::GENERAL, "No CFG edge found for branch to target", branchTarget->to_string());

        branchEdge->data.predecessors.erase(node.key);
        branchEdge->removeInput(node);
        if(branchEdge->data.predecessors.empty())
            // we only want to remove the one direction, the jump-back needs to remain
            node.removeEdge(*branchEdge);
    }

    if(node.key->fallsThroughToNextBlock(false))
    {
        auto blockIt = std::find_if(
            method.begin(), method.end(), [&](const BasicBlock& block) -> bool { return &block == node.key; });
        auto nextBlockIt = blockIt;
        if((++nextBlockIt) != method.end())
        {
            auto& nextNode = assertNode(&(*nextBlockIt));
            auto& edge = node.getOrCreateEdge(&nextNode, CFGRelation{}).addInput(node);
            edge.data.predecessors.emplace(node.key, Optional<InstructionWalker>{});
        }
    }

    // clear cached dominator tree and loops
    dominatorTree.reset();
    loops.reset();
}

std::unique_ptr<ControlFlowGraph> ControlFlowGraph::createCFG(Method& method)
{
    PROFILE_SCOPE(createCFG);
    std::unique_ptr<ControlFlowGraph> graph(new ControlFlowGraph(method.size()));

    for(BasicBlock& bb : method)
    {
        bb.forSuccessiveBlocks([&bb, &graph](BasicBlock& successor, InstructionWalker it) {
            // this transition is implicit if the previous instruction is not a branch at all or a conditional branch to
            // somewhere else (then the transition happens if the condition is not met)
            bool isImplicit = true;
            bool isWorkGroupLoop = false;
            if(!it.isEndOfBlock())
            {
                if(auto branch = it.get<intermediate::Branch>())
                {
                    auto targets = branch->getTargetLabels();
                    isImplicit = targets.find(successor.getLabel()->getLabel()) == targets.end();
                    isWorkGroupLoop =
                        !isImplicit && branch->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_LOOP);
                }
            }
            // connection from bb to successor
            auto& node = graph->getOrCreateNode(&bb);
            auto& edge = node.getOrCreateEdge(&graph->getOrCreateNode(&successor), CFGRelation{}).addInput(node);
            edge.data.predecessors.emplace(node.key, isImplicit ? Optional<InstructionWalker>{} : it);
            edge.data.isWorkGroupLoop = isWorkGroupLoop;
        });
    }

    // update back edges
    if(!graph->getNodes().empty())
        updateBackEdges(*graph, &graph->getStartOfControlFlow());

#ifdef DEBUG_MODE
    logging::logLazy(logging::Level::DEBUG, [&]() { graph->dumpGraph("/tmp/vc4c-cfg.dot"); });
#endif
    return graph;
}

std::unique_ptr<ControlFlowGraph> ControlFlowGraph::clone()
{
    std::unique_ptr<ControlFlowGraph> graph(new ControlFlowGraph(nodes.size()));

    for(auto& node : nodes)
    {
        auto& newNode = graph->getOrCreateNode(node.first);
        node.second.forAllIncomingEdges([&newNode, &graph](const CFGNode& source, const CFGEdge& edge) -> bool {
            auto& newSource = graph->getOrCreateNode(source.key);
            auto& newEdge = newSource.getOrCreateEdge(&newNode, CFGRelation{}).addInput(newSource);
            newEdge.data.predecessors.emplace(source.key, edge.data.predecessors.at(source.key));
            newEdge.data.isWorkGroupLoop = edge.data.isWorkGroupLoop;
            return true;
        });
    }

    return graph;
}

static bool traverseDepthFirstHelper(
    const CFGNode& node, const std::function<ControlFlowVisitResult(const CFGNode&)>& consumer)
{
    auto res = consumer(node);

    if(res == ControlFlowVisitResult::STOP_ALL)
        return false;
    if(res == ControlFlowVisitResult::STOP_PATH)
        return true;

    bool continueTraversal = true;
    node.forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
        continueTraversal = traverseDepthFirstHelper(successor, consumer);
        return continueTraversal;
    });
    return continueTraversal;
}

void ControlFlowGraph::traverseDepthFirst(const std::function<ControlFlowVisitResult(const CFGNode&)>& consumer) const
{
    if(getNodes().empty())
        return;
    auto& start = const_cast<ControlFlowGraph&>(*this).getStartOfControlFlow();
    traverseDepthFirstHelper(start, consumer);
}
