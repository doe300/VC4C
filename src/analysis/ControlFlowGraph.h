/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_CONTROLFLOWGRAPH_H
#define VC4C_CONTROLFLOWGRAPH_H

#include "../Graph.h"
#include "../InstructionWalker.h"
#include "../Method.h"
#include "../tools/SmallMap.h"
#include "ControlFlowLoop.h"

namespace vc4c
{
    namespace analysis
    {
        struct DominatorTree;

        /*
         * A relation in the control-flow-graph represents a transition between two basic blocks.
         *
         * NOTE: This transition can be bi-directional!
         *
         */
        class CFGRelation
        {
        public:
            // the direction (given by the source block) which is the back edge.
            const BasicBlock* backEdgeSource;
            // whether this edge represents a (back-) jump of the work-group loop optimization
            bool isWorkGroupLoop = false;

            bool operator==(const CFGRelation& other) const;

            std::string getLabel() const;

            /**
             * Returns the last instruction executed in the source block (e.g. the branch) before the control flow
             * switches to the destination block.
             */
            InstructionWalker getPredecessor(BasicBlock* source) const;

            /**
             * Returns whether the branch in the given direction (source to destination) is implicit.
             */
            bool isImplicit(const BasicBlock* source) const;

            /**
             * Returns whether the branch in the given direction (source to destination) is a back-edge (e.g. for a
             * loop).
             *
             * I.e. returns whether any code execution path taking this branch must have previously executed the
             * destination block.
             */
            bool isBackEdge(const BasicBlock* source) const;

        private:
            // map of the source block and the predecessor within this block (empty for fall-through)
            tools::SmallSortedPointerMap<const BasicBlock*, Optional<InstructionWalker>> predecessors;

            friend class ControlFlowGraph;
        };

        using CFGNode = Node<BasicBlock*, CFGRelation, Directionality::BIDIRECTIONAL>;
        bool operator<(const CFGNode& one, const CFGNode& other);

        using CFGEdge = CFGNode::EdgeType;

        enum class ControlFlowVisitResult : unsigned char
        {
            // continue to visit the next node
            CONTINUE,
            // stop visiting this path, continue with others, if any
            STOP_PATH,
            // stop completely
            STOP_ALL
        };

        /*
         * The control-flow graph (CFG) represents the order/relation of basic-blocks by connecting basic-blocks which
         * can follow directly after one another (e.g. by branching or fall-through)
         */
        class ControlFlowGraph : public Graph<BasicBlock*, CFGNode>
        {
        public:
            /*
             * Returns the node which represents the first basic-block being executed by the QPU
             */
            CFGNode& getStartOfControlFlow();

            /*
             * Returns the single node which represents the last basic-block being executed
             *
             * NOTE: For non-kernel function there may be multiple last blocks (e.g. containing return-statements) in
             * which case this function will throw!
             */
            CFGNode& getEndOfControlFlow();

            /*
             * Finds all loops in the CFG
             *
             * @param recursively if set to false, only the inner most loops will be returned, otherwise all loops will
             * be returned
             */
            FastAccessList<ControlFlowLoop> findLoops(bool recursively, bool skipWorkGroupLoops = true,
                const analysis::DominatorTree* dominatorTree = nullptr);

            /*
             * Dump this graph as dot file
             */
            void dumpGraph(const std::string& path, bool dumpConstantLoadInstructions) const;

            void updateOnBlockInsertion(Method& method, BasicBlock& newBlock);
            void updateOnBlockRemoval(Method& method, BasicBlock& oldBlock);
            void updateOnBranchInsertion(Method& method, InstructionWalker it);
            void updateOnBranchRemoval(Method& method, BasicBlock& affectedBlock, const FastSet<const Local*>& branchTargets);

            /*
             * Creates the CFG from the basic-blocks within the given method
             */
            static std::unique_ptr<ControlFlowGraph> createCFG(Method& method);

            /*
             * Clone the CFG
             */
            std::unique_ptr<ControlFlowGraph> clone();

            /**
             * Traverses this CFG depth-first applying the given consumer to every node visited.
             *
             * Due to loops inside the control flow, nodes might be visited multiple times.
             *
             * NOTE: It is the responsibility of the consumer to stop the traversal (e.g. on loops) to avoid stack
             * overflows!
             */
            void traverseDepthFirst(const std::function<ControlFlowVisitResult(const CFGNode&)>& consumer) const;

        private:
            explicit ControlFlowGraph(std::size_t numBlocks) : Graph(numBlocks) {}

            friend class Method;
        };
    } // namespace analysis
} /* namespace vc4c */

#endif /* VC4C_CONTROLFLOWGRAPH_H */
