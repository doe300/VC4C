/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LIFETIME_GRAPH_H
#define VC4C_LIFETIME_GRAPH_H

#include "../Graph.h"
#include "../Locals.h"
#include "../Method.h"

namespace vc4c
{
    namespace analysis
    {
        struct LifetimeRelation
        {
        };

        using LifetimeNode = Node<const Local*, LifetimeRelation, Directionality::UNDIRECTED>;

        /*
         * Graph of relations of objects residing in memory and their life-time
         *
         * Can be used for e.g. escape analysis or lowering private/local memory into VPM
         */
        class LifetimeGraph : public Graph<const Local*, LifetimeNode>
        {
        public:
            static std::unique_ptr<LifetimeGraph> createLifetimeGraph(Method& method);

            /*
             * Calculates the stack-size required to fit all stack-allocations.
             * Non-overlapping stack-allocations may be placed in the same memory-area
             *
             * NOTE: This calculation is for one QPU!
             */
            unsigned calculateRequiredStackSize();
        };

    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_LIFETIME_GRAPH_H */
