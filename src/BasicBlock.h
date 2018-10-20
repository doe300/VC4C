/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_BASIC_BLOCK_H
#define VC4C_BASIC_BLOCK_H

#include "Optional.h"
#include "config.h"

#include "Locals.h"
#include "helper.h"
#include "performance.h"

namespace vc4c
{
    namespace intermediate
    {
        class IntermediateInstruction;
        struct BranchLabel;

        using IL = std::unique_ptr<IntermediateInstruction>;
        using InstructionsList = FastModificationList<IL>;
        using InstructionsIterator = InstructionsList::iterator;
        using ConstInstructionsIterator = InstructionsList::const_iterator;
    } // namespace intermediate

    class InstructionWalker;
    class ConstInstructionWalker;
    class Method;

    /*
     * A basic-block is a sequence of continuous instructions within a function body.
     *
     * If an instruction within a basic-block is executed, all instructions within that block are executed.
     * A basic-block always starts with a label, can only contain that one label and cannot have any non-branch
     * instructions behind the first branch.
     *
     * NOTE: This implementation does not follow the above principle exactly: Jump-here-or-there (e.g. SPIR-V
     * OpBranchConditional) instructions are split up into two separate branch-instructions, making it possible for the
     * second branch to not be executed when the block is traversed.
     */
    class BasicBlock : private NonCopyable
    {
    public:
        static const std::string DEFAULT_BLOCK;
        static const std::string LAST_BLOCK;

        BasicBlock(Method& method, intermediate::BranchLabel* label);
        BasicBlock(const BasicBlock&) = delete;
        BasicBlock(BasicBlock&&) = delete;
        ~BasicBlock();

        BasicBlock& operator=(const BasicBlock&) = delete;
        BasicBlock& operator=(BasicBlock&&) = delete;

        /*
         * Whether this basic block has no instructions (except its label)
         */
        bool empty() const;
        /*
         * Returns an iterator to the start of this basic block (points to the label)
         */
        InstructionWalker begin();
        ConstInstructionWalker begin() const;
        /*
         * Returns an iterator to the end of the basic block (points one past the last instruction)
         */
        InstructionWalker end();
        ConstInstructionWalker end() const;
        /*
         * Returns the number of instructions within this block
         */
        std::size_t size() const;

        /*!
         * Checks if all usages of this local are within a certain range from the current instruction within a single
         * basic block
         */
        bool isLocallyLimited(InstructionWalker curIt, const Local* locale, std::size_t threshold) const;

        /*
         * Returns the label for this block
         */
        const intermediate::BranchLabel* getLabel() const;
        // for modifying label value
        intermediate::BranchLabel* getLabel();
        /*
         * Runs the consumer function for every block directly following this
         *
         * NOTE: the consumer may be called several times for a single block
         */
        void forSuccessiveBlocks(const std::function<void(BasicBlock&)>& consumer) const;
        /*
         * Runs the consumer function for every block directly preceding this
         *
         * NOTE: the consumer may be called several times for a single block
         */
        void forPredecessors(const std::function<void(InstructionWalker)>& consumer) const;
        /*
         * Returns whether the control-flow falls through to the next basic block, e.g. there are no unconditional
         * branches away
         */
        bool fallsThroughToNextBlock() const;
        /*
         * Returns the InstructionWalker for the given instruction, if any
         */
        Optional<InstructionWalker> findWalkerForInstruction(
            const intermediate::IntermediateInstruction* instr, InstructionWalker start) const;
        /*
         * Returns the InstructionWalker for the last (as in prior to the given instruction) instruction setting flags
         * within this basic block
         */
        Optional<InstructionWalker> findLastSettingOfFlags(InstructionWalker start) const;
        /*
         * Returns whether this basic-block is the start of the method body
         */
        bool isStartOfMethod() const;
        void dumpInstructions() const;

        Method& getMethod()
        {
            return method;
        }
        const Method& getMethod() const
        {
            return method;
        }

    private:
        Method& method;
        intermediate::InstructionsList instructions;

        friend class ControlFlowGraph;
        friend class InstructionWalker;
        friend class ConstInstructionWalker;
        friend struct InstructionVisitor;
        friend class Method;
    };

    template <typename Scope>
    struct ScopedInstructionWalker;
    using BlockIterator = ScopedInstructionWalker<BasicBlock>;
}

#endif /* VC4C_BASIC_BLOCK */
