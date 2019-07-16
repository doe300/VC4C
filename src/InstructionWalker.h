/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_WALKER_H
#define INSTRUCTION_WALKER_H

#include "intermediate/IntermediateInstruction.h"

#include <functional>
#include <iterator>

namespace vc4c
{
    class BasicBlock;
    class ControlFlowGraph;

    enum class InstructionVisitResult : unsigned char
    {
        // continue to visit the next instruction
        CONTINUE,
        // stop visiting this branch, continue with others, if any
        STOP_BRANCH,
        // stop completely
        STOP_ALL
    };

    /*
     * Visitor-pattern to iterate over instructions in a method
     */
    struct InstructionVisitor
    {
        const std::function<InstructionVisitResult(InstructionWalker&)> op;
        /*
         * Whether to stop at the end/beginning of a basic-block or continue with its successors/predecessors
         */
        const bool stopAtBlock;
        /*
         * Whether to follow jumps (e.g. iterate in order of execution) or linearly iterate over the basic blocks (e.g.
         * in order of appearance)
         */
        const bool followJumps;

        /*
         * Visits start and all following instructions, according to the settings
         *
         * \return true, if the end of the block/method was reached, false, if the visiting operation aborted with
         * STOP_ALL
         */
        bool visit(const InstructionWalker& start) const;
        /*
         * Visits start and all preceding instructions, according to the settings
         *
         * \return true, if the beginning of the block/method was reached, false, of the visiting operation aborted with
         * STOP_ALL
         */
        bool visitReverse(const InstructionWalker& start, ControlFlowGraph* blockGraph = nullptr) const;
    };

    /*
     * Enhanced version of an iterator over instructions within a method.
     *
     * NOTE: Since a InstructionWalker can walk just within a basic-block or over all instructions in a method, the
     * operators ++ and -- are not implemented. The functions to go to the next/previous instruction within a
     * basic-block/the whole method need to be used.
     */
    class InstructionWalker
    {
    public:
        explicit InstructionWalker();
        InstructionWalker(BasicBlock* basicBlock, intermediate::InstructionsIterator pos);
        InstructionWalker(const InstructionWalker&) = default;
        InstructionWalker(InstructionWalker&&) noexcept = default;
        ~InstructionWalker() = default;

        InstructionWalker& operator=(const InstructionWalker&) = default;
        InstructionWalker& operator=(InstructionWalker&&) noexcept = default;

        /*
         * Returns the basic-block the current position belongs to
         */
        BasicBlock* getBasicBlock();

        /*
         * Steps forward to the next instruction within the same basic block
         */
        InstructionWalker& nextInBlock();
        /*
         * Steps backwards to the previous instruction within the same basic block
         */
        InstructionWalker& previousInBlock();
        /*
         * Whether this object points to the end of the basic block (one past the last instruction)
         */
        bool isEndOfBlock() const;
        /*
         * Whether this object points to the beginning of the basic block (the block's label)
         */
        bool isStartOfBlock() const;

        /*
         * Steps forward to the next instruction.
         * If the end of the basic-block is reached, steps to the first instruction of the next basic-block
         */
        InstructionWalker& nextInMethod();
        /*
         * Steps backward to the previous instruction.
         * If the step is taken before the beginning of the current block, steps to the last instruction of the previous
         * block
         */
        InstructionWalker& previousInMethod();
        /*
         * Whether the end of the method (end of the last basic-block) is reached
         */
        bool isEndOfMethod() const;
        /*
         * Whether the beginning of the method (beginning of the first basic-block) is reached
         */
        bool isStartOfMethod() const;

        /*
         * Creates a copy of this object with the same position
         */
        NODISCARD InstructionWalker copy() const;

        bool operator==(const InstructionWalker& other) const;
        inline bool operator!=(const InstructionWalker& other) const
        {
            return !(*this == other);
        }

        /*
         * Returns the instruction stored at this position of the given type
         *
         * Returns a nullptr if there is no instruction stored or it is not of the requested type.
         */
        template <typename T>
        inline T* get()
        {
            return dynamic_cast<T*>(get());
        }

        template <typename T>
        inline const T* get() const
        {
            return dynamic_cast<const T*>(get());
        }

        /*
         * Returns whether the instruction at this position is set.
         *
         * During some optimizations, instructions may be removed without their positions being cleared
         */
        inline bool has() const
        {
            return get() != nullptr;
        }

        inline intermediate::IntermediateInstruction* operator->()
        {
            return get();
        }

        inline const intermediate::IntermediateInstruction* operator->() const
        {
            return get();
        }

        /*
         * Accesses the instruction stored at this position
         *
         * If the position points is invalid (e.g. past the end), an exception is thrown
         */
        intermediate::IntermediateInstruction* get();
        const intermediate::IntermediateInstruction* get() const;
        /*
         * Releases the instruction-object pointed to (see std::unique_ptr::release) without removing this position from
         * the list of instructions
         */
        NODISCARD intermediate::IntermediateInstruction* release();

        /*
         * Replaces the instruction pointed to with the given object
         */
        InstructionWalker& reset(intermediate::IntermediateInstruction* instr);
        /*
         * Erases this position (and the instruction stored), automatically jumping to the next position
         */
        InstructionWalker& erase();
        /*
         * Places the given instruction before this position and jumping to the newly inserted position
         *
         * NOTE: labels cannot be added this way, neither can any instructions be placed at the start of a basic-block
         * (before its label)
         */
        InstructionWalker& emplace(intermediate::IntermediateInstruction* instr);

        /*
         * Executes the given function for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the function is executed for both single instructions
         */
        inline void forAllInstructions(
            const std::function<void(const intermediate::IntermediateInstruction*)>& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                func(combined->getFirstOp());
                func(combined->getSecondOP());
            }
            else
                func(get());
        }

        /*
         * Checks whether the given predicate matches for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is tested for both single instructions
         */
        inline bool allInstructionMatches(
            const std::function<bool(const intermediate::IntermediateInstruction*)>& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) && func(combined->getSecondOP());
            }
            return func(get());
        }

        /*
         * Checks whether the given predicate matches for any instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is checked for both single instructions
         */
        inline bool anyInstructionMatches(
            const std::function<bool(const intermediate::IntermediateInstruction*)>& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) || func(combined->getSecondOP());
            }
            return func(get());
        }

        /*
         * Traverse instructions from the position and replace arguments or output to new one in the same basic block.
         *
         * If `forward` = false, traver reversely.
         * If `stopWhenWritten` = true, finish when it finds a instruction, which is re-assign in it.
         */
        NODISCARD bool replaceValueInBlock(const Value& oldValue, const Value& newValue,
            LocalUse::Type type = LocalUse::Type::READER, bool forward = true, bool stopWhenWritten = true);

    private:
        BasicBlock* basicBlock;
        intermediate::InstructionsIterator pos;

        friend class Method;
        friend class ConstInstructionWalker;
        friend struct tombstone_traits<InstructionWalker>;
    };

    template <>
    struct tombstone_traits<InstructionWalker>
    {
        static constexpr bool is_specialized = true;
        static const InstructionWalker tombstone;

        static constexpr bool isTombstone(const InstructionWalker& val)
        {
            return val.basicBlock == nullptr && val.pos == intermediate::InstructionsIterator{};
        }
    };

    /*
     * Constant version of InstructionWalker, which only allows read-only access to the instructions
     */
    class ConstInstructionWalker
    {
    public:
        explicit ConstInstructionWalker();
        explicit ConstInstructionWalker(InstructionWalker it);
        ConstInstructionWalker(const BasicBlock* basicBlock, intermediate::ConstInstructionsIterator pos);
        ConstInstructionWalker(const ConstInstructionWalker&) = default;
        ConstInstructionWalker(ConstInstructionWalker&&) noexcept = default;
        ~ConstInstructionWalker() = default;

        ConstInstructionWalker& operator=(const ConstInstructionWalker&) = default;
        ConstInstructionWalker& operator=(ConstInstructionWalker&&) noexcept = default;

        /*
         * Returns the basic-block the current position belongs to
         */
        const BasicBlock* getBasicBlock() const;

        /*
         * Steps forward to the next instruction within the same basic block
         */
        ConstInstructionWalker& nextInBlock();
        /*
         * Steps backwards to the previous instruction within the same basic block
         */
        ConstInstructionWalker& previousInBlock();
        /*
         * Whether this object points to the end of the basic block (one past the last instruction)
         */
        bool isEndOfBlock() const;
        /*
         * Whether this object points to the beginning of the basic block (the block's label)
         */
        bool isStartOfBlock() const;

        /*
         * Steps forward to the next instruction.
         * If the end of the basic-block is reached, steps to the first instruction of the next basic-block
         */
        ConstInstructionWalker& nextInMethod();
        /*
         * Steps backward to the previous instruction.
         * If the step is taken before the beginning of the current block, steps to the last instruction of the previous
         * block
         */
        ConstInstructionWalker& previousInMethod();
        /*
         * Whether the end of the method (end of the last basic-block) is reached
         */
        bool isEndOfMethod() const;
        /*
         * Whether the beginning of the method (beginning of the first basic-block) is reached
         */
        bool isStartOfMethod() const;

        /*
         * Creates a copy of this object with the same position
         */
        NODISCARD ConstInstructionWalker copy() const;

        bool operator==(const ConstInstructionWalker& other) const;
        inline bool operator!=(const ConstInstructionWalker& other) const
        {
            return !(*this == other);
        }

        /*
         * Returns the instruction stored at this position of the given type
         */
        template <typename T>
        inline const T* get() const
        {
            return dynamic_cast<T*>(get());
        }

        /*
         * Returns whether the instruction at this position is set.
         *
         * During some optimizations, instructions may be removed without their positions being cleared
         */
        inline bool has() const
        {
            return get() != nullptr;
        }

        /*
         * Whether the current position contains an object of the given type
         */
        template <typename T>
        inline bool has() const
        {
            return get<const T>() != nullptr;
        }

        inline const intermediate::IntermediateInstruction* operator->() const
        {
            return get();
        }

        /*
         * Accesses the instruction stored at this position
         *
         * If the position points is invalid (e.g. past the end), an exception is thrown
         */
        const intermediate::IntermediateInstruction* get() const;

        /*
         * Executes the given function for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the function is executed for both single instructions
         */
        inline void forAllInstructions(
            const std::function<void(const intermediate::IntermediateInstruction*)>& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                func(combined->getFirstOp());
                func(combined->getSecondOP());
            }
            else
                func(get());
        }

        /*
         * Checks whether the given predicate matches for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is tested for both single instructions
         */
        inline bool allInstructionMatches(
            const std::function<bool(const intermediate::IntermediateInstruction*)>& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) && func(combined->getSecondOP());
            }
            return func(get());
        }

        /*
         * Checks whether the given predicate matches for any instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is checked for both single instructions
         */
        inline bool anyInstructionMatches(
            const std::function<bool(const intermediate::IntermediateInstruction*)>& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) || func(combined->getSecondOP());
            }
            return func(get());
        }

    private:
        const BasicBlock* basicBlock;
        intermediate::ConstInstructionsIterator pos;

        friend class Method;
    };

} /* namespace vc4c */

namespace std
{
    template <>
    struct hash<vc4c::InstructionWalker> : public std::hash<const vc4c::intermediate::IntermediateInstruction*>
    {
        inline size_t operator()(const vc4c::InstructionWalker& it) const noexcept
        {
            return std::hash<const vc4c::intermediate::IntermediateInstruction*>::operator()(it.get());
        }
    };

    template <>
    struct hash<vc4c::ConstInstructionWalker> : public std::hash<const vc4c::intermediate::IntermediateInstruction*>
    {
        inline size_t operator()(const vc4c::ConstInstructionWalker& it) const noexcept
        {
            return std::hash<const vc4c::intermediate::IntermediateInstruction*>::operator()(it.get());
        }
    };
} /* namespace std */

#endif /* INSTRUCTION_WALKER_H */
