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
    enum class DataDependencyType
    {
        // flow (true) dependence, read-after-write. The instruction reading a value depends on the value being written
        // before
        FLOW = 1,
        // anti dependence, write-after-read. The instruction writing a value "depends" on the value being read before
        ANTI = 2,
        // output dependence, write-after-write. The instruction writing a value "depends" on another instruction
        // writing the same value before
        OUTPUT = 4,
        // the dependency is on a phi-node and is therefore depending on the branch the block was entered by. Any other
        // dependency is "constant", the depending value is not changed in different basic blocks
        PHI = 8
    };

    /*
     * A dependency between two basic blocks consists of the list of depending locals and their types of dependence.
     */
    using DataDependency = FastMap<Local*, DataDependencyType>;

    struct DataDependencyNode : public Node<BasicBlock*, DataDependency>
    {
        using Base = Node<BasicBlock*, DataDependency>;

        explicit DataDependencyNode(BasicBlock* key) : Base(key) {}

        bool dependsOnBlock(const BasicBlock& bb, const DataDependencyType type = DataDependencyType::FLOW) const;
        bool hasExternalDependencies(
            const Local* local, const DataDependencyType type = DataDependencyType::FLOW) const;
        FastSet<const Local*> getAllExternalDependencies(
            const DataDependencyType type = DataDependencyType::FLOW) const;
    };

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
         */
        static DataDependencyGraph createDependencyGraph(Method& method);
    };
}

#endif /* VC4C_DATA_DEPENDENCY_GRAPH_H */