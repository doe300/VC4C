/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_DEPENDENCY_GRAPH_H
#define VC4C_DEPENDENCY_GRAPH_H

#include "../Graph.h"

#include <memory>

namespace vc4c
{
    namespace intermediate
    {
        class IntermediateInstruction;
    }

    class BasicBlock;

    namespace analysis
    {
        enum class DependencyType : unsigned short
        {
            // flow (true) dependence, read-after-write. The instruction reading a value depends on the value being
            // written before
            VALUE_READ_AFTER_WRITE = 1 << 0,
            // anti dependence, write-after-read. The instruction writing a value "depends" on the value being read
            // before
            VALUE_WRITE_AFTER_READ = 1 << 1,
            // output dependence, write-after-write. The instruction writing a value "depends" on another instruction
            // writing the same value before
            VALUE_WRITE_AFTER_WRITE = 1 << 2,
            // the instruction depends on the other instruction reading the same value beforehand (e.g. for some
            // registers)
            VALUE_READ_AFTER_READ = 1 << 3,
            // true signal dependence. The instruction "using" the effect of a signal depends on this signal being
            // triggered before
            SIGNAL_READ_AFTER_WRITE = 1 << 4,
            // anti signal dependence. The instruction firing a signal "depends" on any previous signal being consumed
            // before
            SIGNAL_WRITE_AFTER_READ = 1 << 5,
            // output signal dependence. The instruction firing a signal "depends" on the other instruction firing its
            // signal beforehand
            // XXX required?
            SIGNAL_WRITE_AFTER_WRITE = 1 << 6,
            // true condition dependence. The instruction using conditional execution depends on the flags being set
            // before
            FLAGS_READ_AFTER_WRITE = 1 << 7,
            // anti condition dependence. The instruction setting flags "depends" on any conditional execution being
            // executed before
            FLAGS_WRITE_AFTER_READ = 1 << 8,
            // output condition dependence. The instruction setting flags "depends" on the neighboring instruction
            // setting its flags beforehand
            // XXX required?
            FLAGS_WRITE_AFTER_WRITE = 1 << 9,
            // the semaphore needs to be changed after any previous semaphore is modified
            SEMAPHORE_ORDER = 1 << 10,
            // the instructions need to be executed in the order specified via the dependencies due to hardware
            // periphery
            PERIPHERY_ORDER = 1 << 11,
            // the instruction depends on the mutex being locked
            MUTEX_LOCK = 1 << 12,
            // the branch depends on the other instruction being executed before
            BRANCH_ORDER = 1 << 13,
            // thread end instructions have a fixed ordering
            THREAD_END_ORDER = 1 << 14

            // TODO add reverse dependencies:
            // These can be used as "outgoing dependencies" to select the next instruction (not depending on anything),
            // which has the most other instructions depending on it
            // also could be used to go to "dependents and remove this instruction as dependency"
        };

        /*
         * A single dependency between two instructions within a basic block
         *
         * NOTE: Dependencies are only backwards, e.g. an instruction can only depend on a previous instruction
         */
        struct Dependency
        {
            DependencyType type;

            /*
             * The number of cycles delay which should/must be between the two instructions of this dependency
             */
            unsigned numDelayCycles;
            /*
             * Determines whether the delay in numDelayCycles is a recommendation (for optimal performance) or a must
             */
            bool isMandatoryDelay;

            /*
             * Rates the current distance between the two instructions and returns a value determining the acceptance of
             * this distance:
             * - returns a value of 0 if there is no mandatory or recommended delay between the two instructions
             * - returns a value of 0 if the recommended/mandatory delay is smaller than the current distance
             * - returns UINT_MAX, if the delay is mandatory and the current distance is not large enough
             * - returns the difference of the recommended delay to the current distance, otherwise
             *
             * -> The lower the value, the more acceptable this current distance is
             */
            unsigned rateDelay(unsigned currentDistance) const;

            /*
             * Returns whether the instruction for the given node can be inserted between the producer and the consumer
             * of this dependency (between the two instructions linked by this dependency)
             *
             * NOTE: This method does not check for value dependencies!
             */
            bool canBeInserted(const intermediate::IntermediateInstruction* instr) const;
        };

        struct DependencyNodeBase
        {
            using DependencyNode = Node<const intermediate::IntermediateInstruction*, Dependency,
                Directionality::DIRECTED, DependencyNodeBase>;

            /*
             * Returns whether this instruction depends on any other instruction within the same basic block to be
             * executed before
             */
            bool hasIncomingDependencies() const;

            /*
             * Returns whether this instructions requires any other instruction within the same basic block to be
             * executed afterwards
             */
            bool hasOutGoingDependencies() const;

            /*
             * Returns the node setting the flags consumed by this instruction.
             *
             * If the instruction does not consume flags, nullptr is returned.
             */
            const DependencyNode* getFlagsSetter() const;

            /*
             * Returns the node triggering the signal consumed by this instruction.
             *
             * If the instruction does not consume signals, nullptr is returned.
             */
            const DependencyNode* getSignalTrigger() const;

            /*
             * Returns the node consuming the signal triggered by this instruction.
             *
             * Returns nullptr if this instruction does not trigger a consumable signal.
             */
            const DependencyNode* getSignalConsumer() const;

            /**
             * Calculates the length of the critical path before this instruction.
             *
             * The critical path is the longer path of dependencies (and their distances) and therefore its length for
             * the part before this instruction is the number of instructions which MUST be inserted before this
             * instruction can be inserted.
             *
             * @param onlyMandatoryDelay whether to assume a delay of 0 additional instructions for non-mandatory delays
             * (if set to true) or also count preferred delay
             * @param cache the optional cache to read already calculated delays from and to insert all (intermediate
             * and final) calculated values into
             *
             * NOTE: This function calculates the length by traversing all dependencies which takes some time. Therefore
             * the result should be cached.
             */
            std::size_t calculatePrecedingCriticalPathLength(bool onlyMandatoryDelay,
                FastMap<const intermediate::IntermediateInstruction*, std::size_t>* cache = nullptr) const;

            /**
             * Calculates the length of the critical path after this instruction
             *
             * See #calculatePrecedingCriticalPathLength() for the meaning of the return value
             *
             * @param onlyMandatoryDelay whether to assume a delay of 0 additional instructions for non-mandatory delays
             * (if set to true) or also count preferred delay
             * @param cache the optional cache to read already calculated delays from and to insert all (intermediate
             * and final) calculated values into
             *
             * NOTE: This function calculates the length by traversing all dependencies which takes some time. Therefore
             * the result should be cached.
             */
            std::size_t calculateSucceedingCriticalPathLength(bool onlyMandatoryDelay,
                FastMap<const intermediate::IntermediateInstruction*, std::size_t>* cache = nullptr) const;
        };

        using DependencyNode = Node<const intermediate::IntermediateInstruction*, Dependency, Directionality::DIRECTED,
            DependencyNodeBase>;
        using DependencyEdge = typename DependencyNode::EdgeType;

        /*
         * Graph representing the dependencies between instructions within a single basic block
         */
        class DependencyGraph : public Graph<const intermediate::IntermediateInstruction*, DependencyNode>
        {
        public:
            static std::unique_ptr<DependencyGraph> createGraph(const BasicBlock& block);

        private:
            explicit DependencyGraph(std::size_t numInstructions) : Graph(numInstructions) {}
        };
    } // namespace analysis
} // namespace vc4c

#endif /* VC4C_DEPENDENCY_GRAPH_H */
