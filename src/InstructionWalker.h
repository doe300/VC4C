/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_WALKER_H
#define INSTRUCTION_WALKER_H

#include "intermediate/IntermediateInstruction.h"

#include <functional>
#include <memory>

namespace vc4c
{
    namespace intermediate
    {
        using InstructionsList = FastModificationList<std::unique_ptr<IntermediateInstruction>>;
        using InstructionsIterator = InstructionsList::iterator;
        using ConstInstructionsIterator = InstructionsList::const_iterator;
    } // namespace intermediate

    class BasicBlock;

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
        const BasicBlock* getBasicBlock() const;

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
        NODISCARD std::unique_ptr<intermediate::IntermediateInstruction> release();

        /*
         * Replaces the instruction pointed to with the given object
         */
        intermediate::IntermediateInstruction& reset(std::unique_ptr<intermediate::IntermediateInstruction>&& instr);
        template <typename T>
        T& reset(std::unique_ptr<T>&& instr)
        {
            auto ptr = instr.get();
            reset(staticPointerCast<intermediate::IntermediateInstruction>(std::move(instr)));
            return *ptr;
        }
        /*
         * Erases this position (and the instruction stored), automatically jumping to the next position
         */
        InstructionWalker& erase();
        /**
         * Safe version of #erase() which checks if the Instruction (if any) to be erased is marked with
         * InstructionDecorations#MANDATORY_DELAY in which case the instruction is not erased completely, but replaced
         * with a NOP also marked with #MANDATORY_DELAY.
         *
         * Since the instruction might already be moved somewhere else, its decorations might need to be passed
         * explicitly as parameter.
         *
         * @return This instruction walker pointing to the next instruction (as in #erase() if no NOP is inserted, to
         * the instruction after the NOP otherwise).
         */
        InstructionWalker& safeErase(
            intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE);
        /*
         * Places the given instruction before this position and jumping to the newly inserted position
         *
         * NOTE: labels cannot be added this way, neither can any instructions be placed at the start of a basic-block
         * (before its label)
         */
        intermediate::IntermediateInstruction& emplace(std::unique_ptr<intermediate::IntermediateInstruction>&& instr);
        template <typename T>
        T& emplace(std::unique_ptr<T>&& instr)
        {
            auto ptr = instr.get();
            emplace(staticPointerCast<intermediate::IntermediateInstruction>(std::move(instr)));
            return *ptr;
        }

        /*
         * Executes the given function for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the function is executed for both single instructions
         */
        template <typename Func>
        inline void forAllInstructions(const Func& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                if(auto first = combined->getFirstOp())
                    func(*first);
                if(auto second = combined->getSecondOp())
                    func(*second);
            }
            else if(auto ins = get())
                func(*ins);
        }

        /*
         * Checks whether the given predicate matches for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is tested for both single instructions
         */
        template <typename Func>
        inline bool allInstructionMatches(const Func& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) && func(combined->getSecondOp());
            }
            return func(get());
        }

        /*
         * Checks whether the given predicate matches for any instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is checked for both single instructions
         */
        template <typename Func>
        inline bool anyInstructionMatches(const Func& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) || func(combined->getSecondOp());
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

    template <typename T>
    class TypedInstructionWalker;

    template <typename T>
    TypedInstructionWalker<T> typeSafe(InstructionWalker it);
    template <typename T>
    TypedInstructionWalker<T> typeSafe(InstructionWalker it, const T& instruction);

    /**
     *  Special case for instruction walker asserting that the pointed-to instruction exists and is of the template
     * parameter type.
     */
    template <typename T>
    class TypedInstructionWalker
    {
    public:
        explicit TypedInstructionWalker() = default;

        bool has() const
        {
            return it.has();
        }

        T* get()
        {
            return it.get<T>();
        }

        const T* get() const
        {
            return it.get<T>();
        }

        inline T* operator->()
        {
            return get();
        }

        inline const T* operator->() const
        {
            return get();
        }

        bool operator==(const TypedInstructionWalker& other) const
        {
            return it == other.it;
        }

        bool operator!=(const TypedInstructionWalker& other) const
        {
            return it != other.it;
        }

        operator InstructionWalker() const noexcept
        {
            return it;
        }

        InstructionWalker& base() noexcept
        {
            return it;
        }

        InstructionWalker base() const noexcept
        {
            return it;
        }

        InstructionWalker erase()
        {
            return it.erase();
        }

        T& reset(std::unique_ptr<T>&& instr)
        {
            return it.reset(std::move(instr));
        }

        NODISCARD std::unique_ptr<T> release()
        {
            if(auto val = get())
            {
                it.release().release();
                return std::unique_ptr<T>{val};
            }
            return nullptr;
        }

    private:
        InstructionWalker it;

        explicit TypedInstructionWalker(InstructionWalker walker) : it(walker) {}

        friend TypedInstructionWalker typeSafe<>(InstructionWalker it);
        friend TypedInstructionWalker typeSafe<>(InstructionWalker it, const T& instruction);
    };

    template <typename T>
    TypedInstructionWalker<T> typeSafe(InstructionWalker it)
    {
        if(it.isEndOfBlock() || !it.has())
            return TypedInstructionWalker<T>{it};
        if(auto ptr = it.get<T>())
            return TypedInstructionWalker<T>{it};
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid instruction type to pass to typed instruction walker", it->to_string());
    }

    template <typename T>
    TypedInstructionWalker<T> typeSafe(InstructionWalker it, const T& instruction)
    {
        if(it.isEndOfBlock() || !it.has())
            return TypedInstructionWalker<T>{it};
        if(&instruction != it.get())
            throw CompilationError(CompilationStep::GENERAL,
                "Wrong instruction parameter to pass to typed instruction walker", it->to_string());
        return TypedInstructionWalker<T>{it};
    }

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

    template <typename T>
    struct tombstone_traits<TypedInstructionWalker<T>>
    {
        static constexpr bool is_specialized = true;
        static const TypedInstructionWalker<T> tombstone;

        static constexpr bool isTombstone(const TypedInstructionWalker<T>& val)
        {
            return tombstone_traits<InstructionWalker>::isTombstone(val);
        }
    };

    template <typename T>
    const TypedInstructionWalker<T> tombstone_traits<TypedInstructionWalker<T>>::tombstone;

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
        template <typename Func>
        inline void forAllInstructions(const Func& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                if(auto first = combined->getFirstOp())
                    func(*first);
                if(auto second = combined->getSecondOp())
                    func(*second);
            }
            else if(auto ins = get())
                func(*ins);
        }

        /*
         * Checks whether the given predicate matches for all instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is tested for both single instructions
         */
        template <typename Func>
        inline bool allInstructionMatches(const Func& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) && func(combined->getSecondOp());
            }
            return func(get());
        }

        /*
         * Checks whether the given predicate matches for any instructions stored at this position.
         *
         * If the current instruction is a combined instruction, the predicate is checked for both single instructions
         */
        template <typename Func>
        inline bool anyInstructionMatches(const Func& func) const
        {
            auto combined = get<const intermediate::CombinedOperation>();
            if(combined != nullptr)
            {
                return func(combined->getFirstOp()) || func(combined->getSecondOp());
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

    template <typename T>
    struct hash<vc4c::TypedInstructionWalker<T>> : public std::hash<const vc4c::intermediate::IntermediateInstruction*>
    {
        inline size_t operator()(const vc4c::TypedInstructionWalker<T>& it) const noexcept
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
