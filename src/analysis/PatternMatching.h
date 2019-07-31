#pragma once

#include "../InstructionWalker.h"
#include "../Values.h"
#include "../Variant.h"
#include "../asm/OpCodes.h"

#include <functional>
#include <vector>

namespace vc4c
{
    struct Expression;

    namespace intermediate
    {
        class IntermediateInstruction;
    } // namespace intermediate

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

        using ConditionPattern = Variant<ConditionCode, Placeholder<ConditionCode>, Ignored>;

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
         */
        inline ConditionPattern capture(ConditionCode& code)
        {
            return ConditionPattern{std::ref(code)};
        }

        /**
         * Creates a ConditionPattern which matches an arbitrary condition code.
         */
        inline ConditionPattern anyCondition()
        {
            return ConditionPattern{Ignored{}};
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
        };

        // Helper type to store temporary unary instruction
        struct UnaryInstructionPattern
        {
            OperationPattern operation;
            ValuePattern firstArgument;
            ConditionPattern condition = anyCondition();
        };

        // Helper type to store temporary binary instruction
        struct BinaryInstructionPattern
        {
            OperationPattern operation;
            ValuePattern firstArgument;
            ValuePattern secondArgument;
            ConditionPattern condition = anyCondition();
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
