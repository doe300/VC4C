/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_DATA_DEPENDENCY_GRAPH_H
#define VC4C_DATA_DEPENDENCY_GRAPH_H

#include "../Graph.h"
#include "../Method.h"
#include "../performance.h"

namespace vc4c
{
    enum class DataDependencyType : unsigned char
    {
        // flow (true) dependence, read-after-write. The instruction reading a value depends on the value being written
        // before
        FLOW = 1,
        // anti dependence, write-after-read. The instruction writing a value "depends" on the value being read before
        ANTI = 2,
        // the dependency is on a phi-node and is therefore depending on the branch the block was entered by. Any other
        // dependency is "constant", the depending value is not changed in different basic blocks
        PHI = 4
    };

    /*
     * A dependency between two basic blocks consists of the list of depending locals and their types of dependence.
     *
     * Since the dependencies can go either way, we need to distinguish between directions!
     *
     * NOTE: The basic block key is the writing/input block!
     */
    using DataDependency = FastMap<const BasicBlock*, FastMap<Local*, DataDependencyType>>;

    class DataDependencyGraph;

    struct DataDependencyNodeBase
    {
        // Returns all locals written somewhere else and consumed
        FastSet<const Local*> getAllIncomingDependencies() const;
        // Returns all locals written by this node and consumed somewhere else
        FastSet<const Local*> getAllOutgoingDependencies() const;
    };

    using DataDependencyNode = Node<BasicBlock*, DataDependency, Directionality::BIDIRECTIONAL, DataDependencyNodeBase>;
    using DataDependencyEdge = typename DataDependencyNode::EdgeType;

    /*
     * The data-dependency graph represents the data-dependencies between basic-blocks.
     *
     * A data-dependency is e.g. a local being written-to in block A and read in block B.
     * Data-dependencies within a single basic block are ignored.
     */
    class DataDependencyGraph : public Graph<BasicBlock*, DataDependencyNode>
    {
    public:
        /*
         * At least for vectorizing/unrolling loops, we only care for dependencies on phi-nodes!
         * Any other data dependency is either
         * - a local dependency (not necessarily basic-block local!) within the loop or
         * - a "constant" dependency which is set somewhere before the loop and never changed in the loop body
         *
         * NOTE: This graph only associates blocks consuming a local with blocks providing it, independent of whether
         * the blocks are adjacent!
         *
         * Example:
         *   A
         *  / \
         * B   C
         *  \ /
         *   D
         * Assuming block D uses a local written by block A, it will depend on block A and not B or C!
         */
        static std::unique_ptr<DataDependencyGraph> createDependencyGraph(Method& method);

    private:
        explicit DataDependencyGraph(std::size_t numBlocks) : Graph(numBlocks) {}
    };
} // namespace vc4c

#endif /* VC4C_DATA_DEPENDENCY_GRAPH_H */
