/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "../Graph.h"

#include <memory>

#ifndef VC4C_INTERFERENCE_GRAPH
#define VC4C_INTERFERENCE_GRAPH

namespace vc4c
{
    class Local;
    class Method;

    namespace analysis
    {
        class GlobalLivenessAnalysis;

        /*
         * The type of interference between two locals
         */
        enum class InterferenceType : unsigned char
        {
            /*
             * Two locals are in use at the same time, but do not block each other's register-file (only the specific
             * register)
             */
            USED_SIMULTANEOUSLY = 1,
            /*
             * Two locals are in use (as inputs or outputs) by the same instructions, so they block each other's
             * register-file (at least for physical files A and B)
             */
            USED_TOGETHER = 2
        };

        using InterferenceNode = vc4c::Node<vc4c::Local*, InterferenceType, vc4c::Directionality::UNDIRECTED>;
        using Interference = InterferenceNode::EdgeType;

        /*
         * The interference graph connects locals by how they interfere which each other (are live at the same time)
         */
        class InterferenceGraph : public Graph<vc4c::Local*, InterferenceNode>
        {
        public:
            using NodeType = InterferenceNode;
            using EdgeType = Interference;
            /*
             * Returns a set of nodes which have more than the given number of neighbors
             */
            FastSet<InterferenceNode*> findOverfullNodes(std::size_t numNeighbors);

            static std::unique_ptr<InterferenceGraph> createGraph(
                Method& method, const GlobalLivenessAnalysis* globalLivenessAnalysis = nullptr);

        private:
            explicit InterferenceGraph(std::size_t numLocals) : Graph(numLocals) {}
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_INTERFERENCE_GRAPH */
