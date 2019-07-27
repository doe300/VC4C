/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlowGraph.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "DebugGraph.h"

#include "log.h"

#include <numeric>
#include <sstream>

using namespace vc4c;

bool CFGRelation::operator==(const CFGRelation& other) const
{
    return predecessors == other.predecessors;
}

LCOV_EXCL_START
std::string CFGRelation::getLabel() const
{
    if(std::all_of(predecessors.begin(), predecessors.end(), [](const auto& pair) -> bool { return !pair.second; }))
    {
        return "";
    }
    if(predecessors.empty())
        return "";
    const auto converter = [](const std::pair<BasicBlock*, Optional<InstructionWalker>>& pair) -> std::string {
        if(pair.second && pair.second->get<const intermediate::Branch>())
            return "br " + pair.second->get<const intermediate::Branch>()->conditional.to_string();
        return "";
    };
    return std::accumulate(predecessors.begin(), predecessors.end(), std::string{},
        [&](const std::string& s, const auto& pair) -> std::string {
            return (s.empty() ? s : (s + ", ")) + converter(pair);
        });
}
LCOV_EXCL_STOP

InstructionWalker CFGRelation::getPredecessor(BasicBlock* source) const
{
    if(const auto& pred = predecessors.at(source))
        return pred.value();
    return source->walkEnd().previousInBlock();
}

bool CFGRelation::isImplicit(BasicBlock* source) const
{
    const auto& pred = predecessors.at(source);
    return !pred.has_value();
}

bool vc4c::operator<(const CFGNode& one, const CFGNode& other)
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
        return vc4c::operator<(*x, *y);
    }
};

FastAccessList<ControlFlowLoop> ControlFlowGraph::findLoops(bool recursively)
{
    FastAccessList<ControlFlowLoop> loops;
    loops.reserve(8);

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
                if(subLoops.size() >= 1)
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
            loops.emplace_back(loop);
        }
    }

    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        for(const auto& loop : loops)
        {
            logging::debug() << "Found a control-flow loop: ";
            for(auto it = loop.rbegin(); it != loop.rend(); ++it)
                logging::debug() << (*it)->key->to_string() << " -> ";
            logging::debug() << logging::endl;
        }
    });
    LCOV_EXCL_STOP

    return loops;
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
                    if(instr && instr->isConstantInstruction())
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
            bool isImplicit = !it.get<intermediate::Branch>();
            //|| (it.get<intermediate::Branch>()->conditional != COND_ALWAYS &&
            //        it.get<intermediate::Branch>()->getTarget() != bb.getLabel()->getLabel());
            // connection from it.getBasicBlock() to bb
            auto& node = graph->getOrCreateNode(it.getBasicBlock());
            auto& edge = node.getOrCreateEdge(&graph->getOrCreateNode(&bb), CFGRelation{}).addInput(node);
            edge.data.predecessors.emplace(node.key, isImplicit ? Optional<InstructionWalker>{} : it);
        });
    }

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
            return true;
        });
    }

    return graph;
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

// LoopInclusionTreeNodeBase::LoopInclusionTreeNodeBase(const KeyType key) : Node(key) {}

LoopInclusionTreeNode* vc4c::castToTreeNode(LoopInclusionTreeNodeBase* base)
{
    auto* node = dynamic_cast<LoopInclusionTreeNode*>(base);
    if(node == nullptr)
    {
        throw CompilationError(CompilationStep::OPTIMIZER, "Cannot downcast to LoopInclusionTreeNode.");
    }
    return node;
}

const LoopInclusionTreeNode* vc4c::castToTreeNode(const LoopInclusionTreeNodeBase* base)
{
    auto* node = dynamic_cast<const LoopInclusionTreeNode*>(base);
    if(node == nullptr)
    {
        throw CompilationError(CompilationStep::OPTIMIZER, "Cannot downcast to LoopInclusionTreeNode.");
    }
    return node;
}

LoopInclusionTreeNodeBase* LoopInclusionTreeNodeBase::findRoot(Optional<int> depth)
{
    if(depth && depth.value() == 0)
    {
        return this;
    }

    auto* self = castToTreeNode(this);
    LoopInclusionTreeNodeBase* root = this;
    self->forAllIncomingEdges([&](LoopInclusionTreeNodeBase& parent, LoopInclusionTreeEdge&) -> bool {
        // The root node must be only one
        std::function<int(const int&)> dec = [](const int& d) -> int { return d - 1; };
        root = parent.findRoot(depth & dec);
        return true;
    });
    return root;
}

unsigned int LoopInclusionTreeNodeBase::longestPathLengthToRoot() const
{
    auto* self = castToTreeNode(this);

    if(self->getEdgesSize() == 0)
    {
        // this is root
        return 0;
    }

    unsigned int longestLength = 0;
    self->forAllIncomingEdges([&](const LoopInclusionTreeNodeBase& parent, const LoopInclusionTreeEdge&) -> bool {
        unsigned int length = parent.longestPathLengthToRoot() + 1;
        if(length > longestLength)
        {
            longestLength = length;
        }
        return true;
    });

    return longestLength;
}

bool LoopInclusionTreeNodeBase::hasCFGNodeInChildren(const CFGNode* node) const
{
    auto* self = castToTreeNode(this);

    bool found = false;
    self->forAllOutgoingEdges([&](const LoopInclusionTreeNodeBase& childBase, const LoopInclusionTreeEdge&) -> bool {
        auto* child = castToTreeNode(&childBase);
        auto nodes = child->key;

        auto targetNode = std::find(nodes->begin(), nodes->end(), node);
        if(targetNode != nodes->end())
        {
            found = true;
            return false;
        }
        auto foundInChildren = child->hasCFGNodeInChildren(node);
        if(foundInChildren)
        {
            found = true;
            return false;
        }
        return true;
    });

    return found;
}

std::string LoopInclusionTreeNodeBase::dumpLabel() const
{
    auto* self = castToTreeNode(this);
    return (*self->key->rbegin())->key->getLabel()->to_string();
}
