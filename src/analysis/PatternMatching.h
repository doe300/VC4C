#pragma once

#include "../InstructionWalker.h"
#include "../Values.h"
#include "../Variant.h"
#include "../asm/OpCodes.h"
#include "../intermediate/operators.h"

#include <functional>
#include <vector>

namespace vc4c
{
    struct Expression;

    namespace pattern
    {
        // A fake operation to indicate a move
        static constexpr OpCode FAKEOP_MOV{"mov", 127, 127, 1, false, false, FlagBehavior::NONE};
        // A fake operation to indicate a value load
        static constexpr OpCode FAKEOP_LDI{"ldi", 128, 128, 1, false, false, FlagBehavior::NONE};
        // A fake operation to indicate a vector rotation
        static constexpr OpCode FAKEOP_ROTATE{"rot", 129, 129, 1, false, false, FlagBehavior::NONE};
        // A fake operation to indicate a branch
        static constexpr OpCode FAKEOP_BR{"br", 130, 130, 1, false, false, FlagBehavior::NONE};
        // A fake operation to indicate a mutex access
        static constexpr OpCode FAKEOP_MUTEX{"mutex", 131, 131, 1, false, false, FlagBehavior::NONE};

        // Dummy entries to be used with the capture(...) functions if the caller wants to make sure multiple entries
        // match, but does not care about their actual value.
        static thread_local Value V1 = UNDEFINED_VALUE;
        static thread_local Value V2 = UNDEFINED_VALUE;
        static thread_local Value V3 = UNDEFINED_VALUE;
        static thread_local Value V4 = UNDEFINED_VALUE;
        static thread_local OpCode O1 = OP_NOP;
        static thread_local OpCode O2 = OP_NOP;
        static thread_local ConditionCode C1 = COND_NEVER;
        static thread_local ConditionCode C2 = COND_NEVER;

        /**
         * A placeholder for an entry (e.g. Value or Opcode) to be filled with the object matching the pattern at the
         * given position.
         *
         * Example:
         * Pattern:   $<1> = add i32 15, $<2>
         * Input: i32 %out = add i32 15, i32 %in
         * Gives the values:
         * $<1> = i32 %out
         * $<2> = i32 %in
         */
        template <typename T>
        using Placeholder = std::reference_wrapper<T>;
        using Ignored = VariantNamespace::monostate;

        struct UnaryInstructionPattern;
        struct BinaryInstructionPattern;
        struct InstructionPattern;

        struct ValuePattern
        {
            Variant<Value, Placeholder<Value>, Placeholder<const Local*>, Placeholder<Literal>, Ignored> pattern;

            // Operator-syntax version of combining output pattern (this) with the instruction operation
            NODISCARD InstructionPattern operator=(UnaryInstructionPattern&& unary) &&;
            NODISCARD InstructionPattern operator=(BinaryInstructionPattern&& binary) &&;
            /**
             * Creates an instruction pattern from the given operation
             *
             * NOTE: If the assigned operation wrapper has side effects or sets unsupported fields (e.g. un-/pack
             * modes), an exception will be thrown!
             */
            NODISCARD InstructionPattern operator=(vc4c::operators::OperationWrapper&& op) &&;
        };

        /**
         * Creates a ValuePattern which matches the given value.
         *
         * The given value is read-only and the resulting Pattern will only match a given input if the Value at the
         * given position matches the input value.
         */
        inline ValuePattern match(const Value& val)
        {
            return ValuePattern{val};
        }

        /**
         * Creates a ValuePattern which matches an arbitrary value.
         *
         * If the resulting Pattern matches a given input, the referenced value is replaced with the matching value from
         * the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         *
         */
        inline ValuePattern capture(Value& val)
        {
            return ValuePattern{std::ref(val)};
        }

        /**
         * Creates a ValuePattern which matches an arbitrary local.
         *
         * The resulting Pattern will only match a given input if the Value at the given position is a Local.
         *
         * If the resulting Pattern matches a given input, the referenced local is replaced with the matching local from
         * the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         */
        inline ValuePattern capture(const Local*& local)
        {
            return ValuePattern{std::ref(local)};
        }

        /**
         * Creates a ValuePattern which matches an arbitrary literal value.
         *
         * The resulting Pattern will only match a given input if the Value at the given position is a literal value.
         *
         * If the resulting Pattern matches a given input, the referenced literal value is replaced with the matching
         * literal constant from the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         */
        inline ValuePattern capture(Literal& lit)
        {
            return ValuePattern{std::ref(lit)};
        }

        /**
         * Creates a ValuePattern which matches an arbitrary value.
         */
        inline ValuePattern anyValue()
        {
            return ValuePattern{Ignored{}};
        }

        using OperationPattern = Variant<OpCode, Placeholder<OpCode>, Ignored>;

        /**
         * Creates an OperationPattern which matches the given operation.
         *
         * The given operation is read-only and the resulting Pattern will only match a given input if the operation at
         * the given position matches the input operation.
         */
        inline OperationPattern match(OpCode code)
        {
            return OperationPattern{code};
        }

        /**
         * Creates an OperationPattern which matches an arbitrary operation.
         *
         * If the resulting Pattern matches a given input, the referenced operation is replaced with the matching
         * operation from the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         */
        inline OperationPattern capture(OpCode& code)
        {
            return OperationPattern{std::ref(code)};
        }

        /**
         * Creates a OperationPattern which matches an arbitrary operation.
         */
        inline OperationPattern anyOperation()
        {
            return OperationPattern{Ignored{}};
        }

        // Helper type to support capturing an inverted condition
        struct InvertedCondition
        {
            Placeholder<ConditionCode> cond;
        };

        using ConditionPattern = Variant<ConditionCode, Placeholder<ConditionCode>, InvertedCondition, Ignored>;

        /**
         * Creates an ConditionPattern which matches the given condition code.
         *
         * The given operation is read-only and the resulting Pattern will only match a given input if the condition
         * code at the given position matches the input condition code.
         */
        inline ConditionPattern match(ConditionCode code)
        {
            return ConditionPattern{code};
        }

        /**
         * Creates an ConditionPattern which matches an arbitrary condition code.
         *
         * If the resulting Pattern matches a given input, the referenced condition code is replaced with the matching
         * condition code from the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         */
        inline ConditionPattern capture(ConditionCode& code)
        {
            return ConditionPattern{std::ref(code)};
        }

        /**
         * Creates an ConditionPattern which matches an arbitrary condition code.
         *
         * If the resulting Pattern matches a given input, the referenced condition code is replaced with the reverted
         * condition of the matching condition code from the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         */
        inline ConditionPattern captureInverse(ConditionCode& code)
        {
            return ConditionPattern{InvertedCondition{std::ref(code)}};
        }

        /**
         * Creates a ConditionPattern which matches an arbitrary condition code.
         */
        inline ConditionPattern anyCondition()
        {
            return ConditionPattern{Ignored{}};
        }

        using FlagPattern = Variant<SetFlag, Placeholder<SetFlag>, Ignored>;

        /**
         * Creates an FlagPattern which matches the given flag state.
         *
         * The given operation is read-only and the resulting Pattern will only match a given input if the flag
         * state at the given position matches the input flags.
         */
        inline FlagPattern match(SetFlag flag)
        {
            return FlagPattern{flag};
        }

        /**
         * Creates an FlagPattern which matches an arbitrary flag state.
         *
         * If the resulting Pattern matches a given input, the referenced flag state is replaced with the matching
         * flag state from the input.
         *
         * NOTE: If a pattern contains multiple capture() using the same object, e.g. multiple capture() taking the same
         * local Value, the pattern only matches if the values at the given positions are equal.
         * Example:
         * Pattern:     $<1> = add $<2>, $<2>
         * Input1:  i32 %out = add i32 15, i32 15
         * Input2: i32 %out1 = add i32 17, i32 %in
         * -> Input1 matches, while Input2 does not
         */
        inline FlagPattern capture(SetFlag& flag)
        {
            return FlagPattern{std::ref(flag)};
        }

        /**
         * Creates a FlagPattern which matches an arbitrary flag state.
         */
        inline FlagPattern anyFlags()
        {
            return FlagPattern{Ignored{}};
        }

        /**
         * Pattern to match against a single instruction
         */
        struct InstructionPattern
        {
            ValuePattern output;
            OperationPattern operation;
            ValuePattern firstArgument;
            ValuePattern secondArgument;
            ConditionPattern condition = anyCondition();
            FlagPattern flags = anyFlags();
        };

        // Helper type to store temporary unary instruction
        struct UnaryInstructionPattern
        {
            OperationPattern operation;
            ValuePattern firstArgument;
            ConditionPattern condition = anyCondition();
            FlagPattern flags = anyFlags();
        };

        // Helper type to store temporary binary instruction
        struct BinaryInstructionPattern
        {
            OperationPattern operation;
            ValuePattern firstArgument;
            ValuePattern secondArgument;
            ConditionPattern condition = anyCondition();
            FlagPattern flags = anyFlags();
        };

        inline UnaryInstructionPattern operator,(OperationPattern&& operation, ValuePattern&& arg)
        {
            return UnaryInstructionPattern{std::move(operation), std::move(arg)};
        }

        inline BinaryInstructionPattern operator,(UnaryInstructionPattern&& unary, ValuePattern&& secondArg)
        {
            return BinaryInstructionPattern{std::move(unary.operation), std::move(unary.firstArgument),
                std::move(secondArg), std::move(unary.condition)};
        }

        inline UnaryInstructionPattern operator,(UnaryInstructionPattern&& unary, ConditionPattern&& condition)
        {
            return UnaryInstructionPattern{
                std::move(unary.operation), std::move(unary.firstArgument), std::move(condition)};
        }

        inline BinaryInstructionPattern operator,(BinaryInstructionPattern&& binary, ConditionPattern&& condition)
        {
            return BinaryInstructionPattern{std::move(binary.operation), std::move(binary.firstArgument),
                std::move(binary.secondArgument), std::move(condition)};
        }

        inline UnaryInstructionPattern operator,(UnaryInstructionPattern&& unary, FlagPattern&& flag)
        {
            return UnaryInstructionPattern{std::move(unary.operation), std::move(unary.firstArgument),
                std::move(unary.condition), std::move(flag)};
        }

        inline BinaryInstructionPattern operator,(BinaryInstructionPattern&& binary, FlagPattern&& flag)
        {
            return BinaryInstructionPattern{std::move(binary.operation), std::move(binary.firstArgument),
                std::move(binary.secondArgument), std::move(binary.condition), std::move(flag)};
        }

        /**
         * Pattern to match against multiple instructions
         *
         * Contains optional additional configuration for pattern matching
         */
        struct Pattern
        {
            std::vector<InstructionPattern> parts;

            /**
             * Whether to allow other (unrelated) instructions between the single parts of this pattern.
             * An instruction is only unrelated, if it does not write a value or set flags consumed by one of the
             * following instructions from within the pattern or triggers a signal with side-effects.
             */
            bool allowGaps = true;
        };

        /**
         * Tries to match the given pattern against the given instruction
         *
         * If the instruction matches the pattern, all placeholders in the pattern are set to the matching entries.
         */
        NODISCARD bool matches(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern);

        /**
         * Tries to match the given pattern against the given expression
         *
         * NOTE: For expressions, some values are neither checked nor updated, e.g. the output value and condition code!
         *
         * If the expression matches the pattern, all placeholders in the pattern are set to the matching entries.
         */
        NODISCARD bool matches(const Expression& expr, InstructionPattern& pattern);

        /**
         * Tries to find the given pattern in the current basic block starting at the given position
         *
         * If a matching instruction is found, all placeholders in the pattern are set to the matching entries of that
         * instruction.
         *
         * NOTE: Since the start of the match is returned, the iterator might need to be incremented before calling this
         * function again, otherwise the same instruction will be matched again.
         *
         * @return the iterator for the matching instruction or end-of-block if the pattern did not match any
         * instruction in the current block.
         */
        NODISCARD InstructionWalker search(InstructionWalker start, InstructionPattern& pattern);

        /**
         * Tries to find the given pattern in the current basic block starting at the given position
         *
         * If matching instructions are found, all placeholders in the pattern are set to the matching entries of these
         * instructions.
         *
         * NOTE: Since the start of the match is returned, the iterator might need to be incremented before calling this
         * function again, otherwise the same instructions will be matched again.
         *
         * @return the iterator for the matching instructions or end-of-block if the pattern did not match any
         * instruction in the current block.
         */
        NODISCARD InstructionWalker search(InstructionWalker start, Pattern& pattern);

    } // namespace pattern
} // namespace vc4c
