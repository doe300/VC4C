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

using namespace vc4c;

bool CFGRelation::operator==(const CFGRelation& other) const
{
    return predecessors == other.predecessors && isImplicit == other.isImplicit;
}

std::string CFGRelation::getLabel() const
{
    if(isImplicit.size() == 2 &&
        std::all_of(isImplicit.begin(), isImplicit.end(),
            [](const std::pair<BasicBlock*, bool>& pair) -> bool { return pair.second; }))
    {
        return "";
    }
    if(predecessors.empty())
        return "";
    const auto converter = [](const std::pair<BasicBlock*, InstructionWalker>& pair) -> std::string {
        if(pair.second.has<intermediate::Branch>())
            return "br " + pair.second.get<const intermediate::Branch>()->conditional.to_string();
        return "";
    };
    return std::accumulate(predecessors.begin(), predecessors.end(), std::string{},
        [&](const std::string& s, const std::pair<BasicBlock*, InstructionWalker>& pair) -> std::string {
            return (s.empty() ? s : (s + ", ")) + converter(pair);
        });
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
        return secondLabel == BasicBlock::LAST_BLOCK ? false : true;

    return firstLabel > secondLabel;
}

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
                        neighbor.key->getLabel()->to_string());

                logging::debug() << "Found predecessor for CFG loop: " << neighbor.key->getLabel()->to_string()
                                 << logging::endl;
                predecessor = &neighbor;
            }
            return true;
        });
    }
    return predecessor;
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
                    throw CompilationError(CompilationStep::GENERAL, "Found multiple successors for CFG loop",
                        neighbor.key->getLabel()->to_string());

                logging::debug() << "Found successor for CFG loop: " << neighbor.key->getLabel()->to_string()
                                 << logging::endl;
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
        auto it = node->key->findWalkerForInstruction(inst, node->key->end());
        if(it)
            return it;
    }
    return {};
}

bool ControlFlowLoop::includes(const ControlFlowLoop& other) const
{
    if(*this == other)
        return false;

    auto head = std::find_if(
        this->begin(), this->end(), [&](const CFGNode* node) { return node->key == (*other.begin())->key; });
    if(head == this->end())
    {
        return false;
    }

    auto thisItr = head;
    auto otherItr = other.begin();
    while(thisItr != this->end() && otherItr != other.end() && (*thisItr)->key == (*otherItr)->key)
    {
        ++thisItr;
        ++otherItr;
    }

    return otherItr == other.end();
}

CFGNode& ControlFlowGraph::getStartOfControlFlow()
{
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
                logging::error() << "Candidate: " << candidate->key->getLabel()->to_string() << logging::endl;
                logging::error() << "Candidate: " << node.key->getLabel()->to_string() << logging::endl;
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

FastAccessList<ControlFlowLoop> ControlFlowGraph::findLoops()
{
    FastAccessList<ControlFlowLoop> loops;
    loops.reserve(8);

    FastMap<const CFGNode*, int> discoveryTimes;
    FastMap<const CFGNode*, int> lowestReachable;
    RandomModificationList<const CFGNode*> stack;
    // a time of 0 means not initialized yet
    int time = 1;

    // Call the recursive helper function to find strongly
    // connected components in DFS tree with node 'i'

    // we need the nodes sorted by the order of the basic blocks
    OrderedSet<const CFGNode*, CFGNodeSorter> orderedNodes;
    for(auto& pair : nodes)
        orderedNodes.emplace(&pair.second);

    for(const CFGNode* node : orderedNodes)
    {
        if(discoveryTimes[node] == 0)
        {
            auto loop = findLoopsHelper(node, discoveryTimes, lowestReachable, stack, time);
            if(loop.size() > 1)
                loops.emplace_back(std::move(loop));
        }
        if(node->isAdjacent(node))
        {
            // extra case, loop with single block
            ControlFlowLoop loop;
            loop.emplace_back(node);
            loops.emplace_back(loop);
        }
    }

    for(const auto& loop : loops)
    {
        logging::debug() << "Found a control-flow loop: ";
        for(auto it = loop.rbegin(); it != loop.rend(); ++it)
            logging::debug() << (*it)->key->getLabel()->to_string() << " -> ";
        logging::debug() << logging::endl;
    }

    return loops;
}

void ControlFlowGraph::dumpGraph(const std::string& path) const
{
#ifdef DEBUG_MODE
    // XXX to be exact, would need bidirectional arrow [dir="both"] for compact loops
    auto nameFunc = [](const BasicBlock* bb) -> std::string { return bb->getLabel()->getLabel()->name; };
    auto edgeLabelFunc = [](const CFGRelation& r) -> std::string { return r.getLabel(); };
    DebugGraph<BasicBlock*, CFGRelation, CFGEdge::Directed>::dumpGraph<ControlFlowGraph>(*this, path, nameFunc,
        [](const CFGRelation& rel) -> bool {
            return std::all_of(rel.isImplicit.begin(), rel.isImplicit.end(),
                [](const std::pair<BasicBlock*, bool>& pair) -> bool { return pair.second; });
        },
        edgeLabelFunc);
#endif
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
            bool isImplicit = !it.has<intermediate::Branch>();
            //|| (it.get<intermediate::Branch>()->conditional != COND_ALWAYS &&
            //        it.get<intermediate::Branch>()->getTarget() != bb.getLabel()->getLabel());
            // connection from it.getBasicBlock() to bb
            auto& node = graph->getOrCreateNode(it.getBasicBlock());
            auto& edge = node.getOrCreateEdge(&graph->getOrCreateNode(&bb), CFGRelation{}).addInput(node);
            edge.data.isImplicit.emplace(node.key, isImplicit);
            edge.data.predecessors.emplace(node.key, it);
        });
    }

#ifdef DEBUG_MODE
    graph->dumpGraph("/tmp/vc4c-cfg.dot");
#endif

    PROFILE_END(createCFG);
    return graph;
}

ControlFlowLoop ControlFlowGraph::findLoopsHelper(const CFGNode* node, FastMap<const CFGNode*, int>& discoveryTimes,
    FastMap<const CFGNode*, int>& lowestReachable, RandomModificationList<const CFGNode*>& stack, int& time)
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

LoopInclusionTreeNodeBase* LoopInclusionTreeNodeBase::findRoot()
{
    auto* self = reinterpret_cast<LoopInclusionTreeNode*>(this);
    LoopInclusionTreeNodeBase* root = this;
    self->forAllIncomingEdges([&](LoopInclusionTreeNode& parent, LoopInclusionTreeEdge&) -> bool {
        root = parent.findRoot();
        return true;
    });
    return root;
}
