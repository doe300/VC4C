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
        if(pair.second && pair.second->get<const intermediate::Branch>())
            return "br " + pair.second->get<const intermediate::Branch>()->conditional.to_string();
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
    return assertNode(&(*nodes.begin()->first->method.begin()));
}

CFGNode& ControlFlowGraph::getEndOfControlFlow()
{
    // return node without any successor,
    // if there are multiple (or none), throw
    CFGNode* candidate = nullptr;
    for(auto& pair : nodes)
    {
        CFGNode& node = pair.second;
        bool anyOutgoingEdges = false;
        node.forAllOutgoingEdges([&anyOutgoingEdges](const CFGNode&, const CFGEdge&) -> bool {
            anyOutgoingEdges = true;
            return false;
        });
        if(!anyOutgoingEdges)
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

FastAccessList<ControlFlowLoop> ControlFlowGraph::findLoops(
    bool recursively, bool skipWorkGroupLoops, const analysis::DominatorTree* dominatorTree)
{
    PROFILE_START(findLoops);
    // use a linked list, since we a) remove entries in the middle and b) need stable pointers for the dominator chains
    FastModificationList<ControlFlowLoop> loops;

    FastMap<const CFGNode*, int> discoveryTimes;
    FastMap<const CFGNode*, int> lowestReachable;
    FastModificationList<const CFGNode*> stack;
    // a time of 0 means not initialized yet
    int time = 1;

    // Call the recursive helper function to find strongly
    // connected components in DFS tree with node 'i'

    // we need the nodes sorted by the order of the basic blocks
    SortedSet<const CFGNode*, CFGNodeSorter> orderedNodes;
    for(auto& pair : nodes)
        orderedNodes.emplace(&pair.second);

    for(const CFGNode* node : orderedNodes)
    {
        if(discoveryTimes[node] == 0)
        {
            if(recursively)
            {
                auto subLoops = findLoopsHelperRecursively(node, discoveryTimes, stack, time);
                if(!subLoops.empty())
                    loops.insert(loops.end(), subLoops.begin(), subLoops.end());
            }
            else
            {
                auto loop = findLoopsHelper(node, discoveryTimes, lowestReachable, stack, time);
                if(loop.size() > 1)
                    loops.emplace_back(std::move(loop));
            }
        }
        if(node->isAdjacent(node))
        {
            // extra case, loop with single block
            ControlFlowLoop loop;
            loop.emplace_back(node);
            loops.emplace_back(std::move(loop));
        }
    }

    // NOTE: since the below loop has O(n^2) complexity, we first filter all work-group loops
    if(skipWorkGroupLoops)
    {
        loops.erase(std::remove_if(loops.begin(), loops.end(),
                        [](const ControlFlowLoop& loop) -> bool { return loop.isWorkGroupLoop(); }),
            loops.end());
    }

    // NOTE: need to merge loops, since for recursive loops or loops with branches inside we get multiple results for
    // same loop with different starting blocks or paths.
    // FIXME for test_barrier.cl (or any big if-else/switch inside loops), this still hangs/takes very long

    // Merge identical loops with different "starting blocks", i.e. the loops have the identical blocks, but are rotated
    // in the ControlFlowLoop structure.
    for(auto it = loops.begin(); it != loops.end(); ++it)
    {
        auto it2 = it;
        ++it2;
        while(it != loops.end() && it2 != loops.end())
        {
            if(*it == *it2)
                it2 = loops.erase(it2);
            else
                ++it2;
        }
    }

    auto dominators = dominatorTree;
    std::unique_ptr<analysis::DominatorTree> tmpTree;
    if(!dominatorTree)
    {
        tmpTree = analysis::DominatorTree::createDominatorTree(*this);
        dominators = tmpTree.get();
    }

    // Merge loops for different branches, e.g. for if-else, switch-case or inner loops taken/not taken
    FastMap<const ControlFlowLoop*, FastAccessList<const analysis::DominatorTreeNode*>> dominatorChains;
    for(auto& loop : loops)
    {
        auto& chain = dominatorChains[&loop];
        chain.reserve(loop.size());
        for(auto entry : loop)
        {
            auto node = dominators->findNode(entry);
            chain.emplace_back(node ? node->getSinglePredecessor() : nullptr);
        }

        // Remove adjacent duplicate dominators, as they e.g. happen for if-without-else, if-else, switch-case and
        // loops. This allows us to simply check for equality of the dominator chains to cover all the cases listed
        // above (instead of only for the if-else case).
        chain.erase(std::unique(chain.begin(), chain.end()), chain.end());
    }

    for(auto it = loops.begin(); it != loops.end(); ++it)
    {
        if(it->empty())
            continue;
        auto& firstDominators = dominatorChains.at(&(*it));
        auto it2 = it;
        ++it2;
        while(it != loops.end() && it2 != loops.end())
        {
            auto& secondDominators = dominatorChains.at(&(*it2));

            if(!firstDominators.empty() && !secondDominators.empty() && firstDominators == secondDominators)
            {
                // since we remove adjacent duplicate dominators in the loop above, this allies for all inner structured
                // control flow (if-without-else, if-else, switch-case, inner loops).
                CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
                    logging::debug() << "Merging loops: " << logging::endl;
                    logging::debug() << "\t" << it->to_string() << logging::endl;
                    logging::debug() << "\t" << it2->to_string() << logging::endl;
                });

                // move all entries from the second loop not present in the first to the first loop
                auto firstIt = it->begin();
                auto secondIt = it2->begin();
                while(secondIt != it2->end())
                {
                    if(*firstIt != *secondIt)
                    {
                        auto findIt = std::find(firstIt, it->end(), *secondIt);
                        if(findIt != it->end())
                            // just skip any blocks in the first which are not in the second
                            firstIt = findIt;
                        else
                            // add block from second to first
                            // NOTE: We do not need to update the dominator chain, since this would only insert the same
                            // dominator as the previous block, which we then anyway would remove again.
                            firstIt = it->insert(firstIt, *secondIt);
                    }
                    ++firstIt;
                    ++secondIt;
                }
                CPPLOG_LAZY(logging::Level::DEBUG, log << "\tinto: " << it->to_string() << logging::endl);
                it2 = loops.erase(it2);
            }
            else
                ++it2;
        }
    }

    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        for(const auto& loop : loops)
            logging::debug() << "Found a control-flow loop: " << loop.to_string() << logging::endl;
    });
    LCOV_EXCL_STOP

    FastAccessList<ControlFlowLoop> result;
    result.reserve(loops.size());
    std::copy(std::make_move_iterator(loops.begin()), std::make_move_iterator(loops.end()), std::back_inserter(result));
    PROFILE_END(findLoops);
    return result;
}

LCOV_EXCL_START
void ControlFlowGraph::dumpGraph(const std::string& path, bool dumpConstantLoadInstructions) const
{
#ifdef DEBUG_MODE
    // XXX to be exact, would need bidirectional arrow [dir="both"] for compact loops
    auto nameFunc = [&dumpConstantLoadInstructions](const BasicBlock* bb) -> std::string {
        if(dumpConstantLoadInstructions)
        {
            std::stringstream ss;
            ss << bb->getLabel()->getLabel()->name << "\\n";
            std::for_each(bb->instructions.begin(), bb->instructions.end(),
                [&ss](const std::unique_ptr<intermediate::IntermediateInstruction>& instr) {
                    if(instr && instr->isConstantInstruction() &&
                        (check(instr->checkOutputLocal()) & &Local::getSingleWriter) == instr.get())
                        ss << instr->to_string() << "\\l";
                });
            return ss.str();
        }
        else
        {
            return bb->getLabel()->getLabel()->name;
        }
    };
    auto edgeLabelFunc = [](const CFGRelation& r) -> std::string { return r.getLabel(); };
    DebugGraph<BasicBlock*, CFGRelation, CFGEdge::Directed>::dumpGraph<ControlFlowGraph>(
        *this, path, nameFunc,
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
    PROFILE_START(updateBackEdges);
    // 1. mark with counter value after traversal of sub-tree
    FastMap<const CFGNode*, std::size_t> counters;
    std::size_t counter = 0;
    FastSet<const CFGNode*> visitedNodes;
    markDepthFirst(*startOfControlFlow, visitedNodes, counters, counter);

    // 2. check all edges whether we have an edge from lower number to higher number
    visitedNodes.clear();
    std::queue<CFGNode*> pendingNodes;
    pendingNodes.push(startOfControlFlow);
    while(!pendingNodes.empty())
    {
        auto currentNode = pendingNodes.front();
        pendingNodes.pop();

        visitedNodes.insert(currentNode);

        currentNode->forAllOutgoingEdges([&](CFGNode& next, CFGEdge& edge) -> bool {
            if(visitedNodes.find(&next) != visitedNodes.end())
                edge.data.backEdgeSource = counters[currentNode] < counters[&next] ? currentNode->key : nullptr;
            else
            {
                edge.data.backEdgeSource = nullptr;
                pendingNodes.push(&next);
            }
            return true;
        });
    }

    PROFILE_END(updateBackEdges);
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
}

void ControlFlowGraph::updateOnBranchInsertion(Method& method, InstructionWalker it)
{
    /*
     * 1. insert edge from block to destination label
     * 2. check whether branches cover all in this block and remove fall-through edge to next block
     */
    auto& node = assertNode(it.getBasicBlock());
    // may not yet exist, e.g. for forward branches
    auto& nextNode = getOrCreateNode(method.findBasicBlock(it.get<intermediate::Branch>()->getTarget()));

    auto& edge = node.getOrCreateEdge(&nextNode).addInput(node);
    // overwrite possible fall-through edge
    edge.data.predecessors[node.key] = it;
    edge.data.isWorkGroupLoop = it->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_LOOP);

    CFGEdge* fallThroughEdge = nullptr;
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
        // we have a fall-through edge left but the block no longer falls through
        node.removeEdge(*fallThroughEdge);

    // update back edges
    updateBackEdges(*this, &getStartOfControlFlow());
}

void ControlFlowGraph::updateOnBranchRemoval(Method& method, BasicBlock& affectedBlock, const Local* branchTarget)
{
    /*
     * 1. remove edge from block to destination label
     * 2. check whether branches cover all in this block and add fall-through edge to next block
     */
    auto& node = assertNode(&affectedBlock);
    auto& destNode = assertNode(method.findBasicBlock(branchTarget));

    CFGEdge* branchEdge = nullptr;
    node.forAllOutgoingEdges([&](CFGNode& successor, CFGEdge& edge) -> bool {
        if(successor.key == destNode.key)
        {
            if(branchEdge)
                throw CompilationError(
                    CompilationStep::GENERAL, "Multiple edges between two basic blocks", branchEdge->data.getLabel());
            branchEdge = &edge;
        }
        return true;
    });

    if(!branchEdge)
        throw CompilationError(
            CompilationStep::GENERAL, "No CFG edge found for branch to target", branchTarget->to_string());

    if(branchEdge->getDirection() == Direction::BOTH)
    {
        // we only want to remove the one direction, the jump-back needs to remain
        // also if reverse direction is implicit, copy as such
        auto reversePred = branchEdge->data.predecessors.at(destNode.key);
        node.removeEdge(*branchEdge);
        destNode.addEdge(&node, CFGRelation{})->data.predecessors.emplace(destNode.key, reversePred);
    }
    else
        node.removeEdge(*branchEdge);

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
}

std::unique_ptr<ControlFlowGraph> ControlFlowGraph::createCFG(Method& method)
{
    PROFILE_START(createCFG);
    std::unique_ptr<ControlFlowGraph> graph(new ControlFlowGraph(method.size()));

    for(BasicBlock& bb : method)
    {
        bb.forPredecessors([&bb, &graph](InstructionWalker it) -> void {
            // this transition is implicit if the previous instruction is not a branch at all or a conditional branch to
            // somewhere else (then the transition happens if the condition is not met)
            bool isImplicit = true;
            bool isWorkGroupLoop = false;
            if(auto branch = it.get<intermediate::Branch>())
            {
                isImplicit = branch->getTarget() != bb.getLabel()->getLabel();
                isWorkGroupLoop = branch->getTarget() == bb.getLabel()->getLabel() &&
                    branch->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_LOOP);
            }
            // connection from it.getBasicBlock() to bb
            auto& node = graph->getOrCreateNode(it.getBasicBlock());
            auto& edge = node.getOrCreateEdge(&graph->getOrCreateNode(&bb), CFGRelation{}).addInput(node);
            edge.data.predecessors.emplace(node.key, isImplicit ? Optional<InstructionWalker>{} : it);
            edge.data.isWorkGroupLoop = isWorkGroupLoop;
        });
    }

    // update back edges
    updateBackEdges(*graph, &graph->getStartOfControlFlow());

#ifdef DEBUG_MODE
    logging::logLazy(logging::Level::DEBUG, [&]() { graph->dumpGraph("/tmp/vc4c-cfg.dot", false); });
#endif

    PROFILE_END(createCFG);
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
    auto& start = const_cast<ControlFlowGraph&>(*this).getStartOfControlFlow();
    traverseDepthFirstHelper(start, consumer);
}

ControlFlowLoop ControlFlowGraph::findLoopsHelper(const CFGNode* node, FastMap<const CFGNode*, int>& discoveryTimes,
    FastMap<const CFGNode*, int>& lowestReachable, FastModificationList<const CFGNode*>& stack, int& time)
{
    // Initialize discovery time and low value
    discoveryTimes[node] = lowestReachable[node] = ++time;
    stack.push_back(node);

    // Go through all vertices adjacent to this
    node->forAllOutgoingEdges([this, node, &discoveryTimes, &lowestReachable, &stack, &time](
                                  const CFGNode& next, const CFGEdge& edge) -> bool {
        const CFGNode* v = &next;
        // If v is not visited yet, then recur for it
        if(discoveryTimes[v] == 0)
        {
            findLoopsHelper(v, discoveryTimes, lowestReachable, stack, time);

            // Check if the subtree rooted with 'v' has a
            // connection to one of the ancestors of 'u'
            // Case 1 (per above discussion on Disc and Low value)
            lowestReachable[node] = std::min(lowestReachable[node], lowestReachable[v]);
        }

        // Update low value of 'u' only of 'v' is still in stack
        // (i.e. it's a back edge, not cross edge).
        // Case 2 (per above discussion on Disc and Low value)
        else if(std::find(stack.begin(), stack.end(), v) != stack.end())
            lowestReachable[node] = std::min(lowestReachable[node], discoveryTimes[v]);
        return true;
    });

    ControlFlowLoop loop;
    loop.reserve(stack.size());

    // head node found, pop the stack and return an SCC
    if(lowestReachable[node] == discoveryTimes[node])
    {
        while(stack.back() != node)
        {
            loop.push_back(stack.back());
            stack.pop_back();
        }
        loop.push_back(stack.back());
        stack.pop_back();
    }

    // for cleanup and easier reading, rotate loop so header is front (if we could determine one)
    if(auto header = loop.getHeader())
    {
        auto it = std::find(loop.rbegin(), loop.rend(), header);
        std::rotate(loop.rbegin(), it, loop.rend());
    }

    return loop;
}

FastAccessList<ControlFlowLoop> ControlFlowGraph::findLoopsHelperRecursively(const CFGNode* node,
    FastMap<const CFGNode*, int>& discoveryTimes, FastModificationList<const CFGNode*>& stack, int& time)
{
    // Initialize discovery time
    discoveryTimes[node] = ++time;
    stack.push_back(node);

    FastAccessList<ControlFlowLoop> loops;

    // Go through all vertices adjacent to this
    node->forAllOutgoingEdges(
        [this, node, &discoveryTimes, &stack, &time, &loops](const CFGNode& next, const CFGEdge& edge) -> bool {
            const CFGNode* v = &next;

            // Create a loop including 'v' only of 'v' is still in stack
            if(std::find(stack.begin(), stack.end(), v) != stack.end() && node != v)
            {
                ControlFlowLoop loop;
                FastModificationList<const CFGNode*> tempStack = stack;

                while(tempStack.back() != v)
                {
                    loop.push_back(tempStack.back());
                    tempStack.pop_back();
                }
                loop.push_back(tempStack.back());
                tempStack.pop_back();

                // for cleanup and easier reading, rotate loop so header is in the back (if we could determine one)
                if(auto header = loop.getHeader())
                {
                    auto it = std::find(loop.rbegin(), loop.rend(), header);
                    std::rotate(loop.rbegin(), it, loop.rend());
                }

                loops.emplace_back(std::move(loop));
            }
            else if(node != v)
            {
                auto subLoops = findLoopsHelperRecursively(v, discoveryTimes, stack, time);
                if(subLoops.size() >= 1)
                    loops.insert(loops.end(), subLoops.begin(), subLoops.end());
            }

            return true;
        });

    stack.pop_back();

    return loops;
}
