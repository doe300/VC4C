#ifndef VC4C_CONTROLFLOWLOOP_H
#define VC4C_CONTROLFLOWLOOP_H 1

#include "../Graph.h"
#include "../InstructionWalker.h"
#include "../performance.h"
#include "Optional.h"

#include <memory>
#include <string>

namespace vc4c
{
    class BasicBlock;
    class CFGRelation;

    using CFGNode = Node<BasicBlock*, CFGRelation, Directionality::BIDIRECTIONAL>;

    /*
     * A loop in the control-flow represented by the basic-blocks taking part in it
     *
     * NOTE: A control-flow loop can only be used within the life-time of the ControlFlowGraph it is created from!
     */
    struct ControlFlowLoop : public FastAccessList<const CFGNode*>
    {
        ControlFlowLoop() = default;

        /*
         * Returns the basic-block(s) in the CFG preceding the first node in the loop, the node from which the loop is
         * entered.
         */
        const CFGNode* findPredecessor() const;
        FastAccessList<const CFGNode*> findPredecessors() const;

        /*
         * Returns the basic-block in the CFG following the last node in the loop, the node into which this loop exits
         * into.
         */
        const CFGNode* findSuccessor() const;

        /*
         * Returns the InstructionWalker for the given instruction, if it is within the loop.
         */
        Optional<InstructionWalker> findInLoop(const intermediate::IntermediateInstruction* inst) const;

        /*
         * Returns whether this loop includes other loop and doesn't equal it.
         */
        bool includes(const ControlFlowLoop& other) const;
    };

    /*
     * A relation in the control-flow-loop
     */
    struct LoopInclusion
    {
    };

    struct LoopInclusionTreeNodeBase
    {
        virtual ~LoopInclusionTreeNodeBase() noexcept;

        LoopInclusionTreeNodeBase* findRoot(Optional<int> depth);
        unsigned int longestPathLengthToRoot() const;
        bool hasCFGNodeInChildren(const CFGNode* node) const;

        std::string dumpLabel() const;
    };

    using LoopInclusionTreeNode =
        Node<ControlFlowLoop*, LoopInclusion, Directionality::DIRECTED, LoopInclusionTreeNodeBase>;
    using LoopInclusionTreeEdge = LoopInclusionTreeNode::EdgeType;

    LoopInclusionTreeNode* castToTreeNode(LoopInclusionTreeNodeBase* base);
    const LoopInclusionTreeNode* castToTreeNode(const LoopInclusionTreeNodeBase* base);

    /*
     * The trees represents inclusion relation of control-flow loops. This may have multiple trees.
     */
    using LoopInclusionTree = Graph<ControlFlowLoop*, LoopInclusionTreeNode>;

    /**
     * Create the tree of loop inclusions from the given list of detected control flow loops.
     *
     * A loop A includes another loop B if A includes all basic blocks that are part of B and A != B.
     *
     * Examples:
     *
     * loop A {
     *     loop B {
     *         loop C {
     *         }
     *     }
     *     loop D {
     *     }
     * }
     *
     * to:
     *       +-+
     *  +----+A+
     *  |    +++
     *  |     |
     *  v     v
     * +-+   +++   +-+
     * |D|   |B+-->+C|
     * +-+   +-+   +-+
     */
    std::unique_ptr<LoopInclusionTree> createLoopInclusingTree(FastAccessList<ControlFlowLoop>& loops);
} // namespace vc4c

#endif /* VC4C_CONTROLFLOWLOOP_H */
