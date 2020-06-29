/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_DOMINATOR_TREE_H
#define VC4C_DOMINATOR_TREE_H

#include "../Graph.h"
#include "../Method.h"
#include "ControlFlowGraph.h"

#include <memory>

namespace vc4c
{
    namespace analysis
    {
        // Dummy relation for dominator trees
        struct DominationRelation
        {
        };

        struct DominatorTreeNodeBase
        {
            bool dominates(const DominatorTreeNodeBase& other) const;
            bool isDominatedBy(const DominatorTreeNodeBase& other) const;

            FastSet<const DominatorTreeNodeBase*> getDominators() const;
            FastSet<const DominatorTreeNodeBase*> getDominatedNodes() const;
        };

        using DominatorTreeNode =
            Node<const CFGNode*, DominationRelation, Directionality::DIRECTED, DominatorTreeNodeBase>;

        /**
         * The tree of immediate strict dominators within the given control flow graph.
         *
         * Following definitions are used:
         * * A node D dominates a node N if every path from the start of the control flow to N must go through D.
         * * A node D strictly dominates a node N if D dominates N and D does not equal N.
         * * A node D immediately dominates a node N if D strictly dominates N, but does not strictly dominate any other
         *   node that strictly dominates N (D is the dominator "closest" to N).
         *
         * Adapted from:
         * https://en.wikipedia.org/wiki/Dominator_(graph_theory)
         */
        struct DominatorTree : public Graph<const CFGNode*, DominatorTreeNode>
        {
            explicit DominatorTree(std::size_t numNodes) : Graph(numNodes) {}

            static std::unique_ptr<DominatorTree> createDominatorTree(ControlFlowGraph& cfg);
            static std::unique_ptr<DominatorTree> createPostdominatorTree(ControlFlowGraph& cfg);
        };

    } // namespace analysis
} // namespace vc4c

#endif /* VC4C_DOMINATOR_TREE_H */
