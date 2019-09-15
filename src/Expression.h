/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_EXPRESSION_H
#define VC4C_EXPRESSION_H

#include "Values.h"
#include "asm/OpCodes.h"
#include "intermediate/IntermediateInstruction.h"
#include "intermediate/operators.h"

namespace vc4c
{
    /**
     * An expression is an abstraction of an ALU operation (or load) where only the inputs and the type of operation
     * is considered.
     *
     * Expressions might not have any side-effects or conditional execution!
     */
    struct Expression
    {
        // A fake operation to indicate an unsigned multiplication
        static constexpr OpCode FAKEOP_UMUL{"umul", 132, 132, 2, false, false, FlagBehavior::NONE};

        OpCode code;
        Value arg0;
        Optional<Value> arg1;
        Unpack unpackMode = UNPACK_NOP;
        Pack packMode = PACK_NOP;
        intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;

        /**
         * Tries to create an expression representing the single instruction
         */
        static Optional<Expression> createExpression(const intermediate::IntermediateInstruction& instr);

        /**
         * Tries to create an expression representing the calculation in the given instruction while also regarding the
         * instructions calculating the instruction arguments.
         *
         * If the optional parameter allowFakeOperations is set, the resulting expression might be an operation, which
         * can not be mapped to an instruction, e.g. the fake "umul" defined above.
         */
        static Optional<Expression> createRecursiveExpression(const intermediate::IntermediateInstruction& instr,
            unsigned maxDepth = 6, bool allowFakeOperations = false);

        bool operator==(const Expression& other) const;

        std::string to_string() const;

        bool isMoveExpression() const;
        Optional<Value> getConstantExpression() const;
        bool hasConstantOperand() const;

        /**
         * Creates a combined expression given the input expressions.
         *
         * Expressions can be combined, e.g. if the inputs of this expression can be calculated with one of the
         * arguments and the op-codes can be combined.
         *
         * If the optional parameter allowFakeOperations is set, the resulting expression might be an operation, which
         * can not be mapped to an instruction, e.g. the fake "umul" defined above.
         *
         * Returns a copy of this expression, if no combination could be done
         */
        Expression combineWith(const FastMap<const Local*, Expression>& inputs, bool allowFakeOperations = false) const;
    };

    // Extends the operator syntax to create expressions from it
    namespace operators
    {
        struct ExpressionWrapper
        {
            NODISCARD Expression operator=(OperationWrapper&& op) &&;
        };

        /**
         * Creates an expression from the given operation
         *
         * NOTE: If the assigned operation wrapper has side effects, an exception will be thrown!
         */
        NODISCARD inline ExpressionWrapper expression()
        {
            return ExpressionWrapper{};
        }

        /**
         * Creates an expression from the given operation
         *
         * NOTE: If the given operation wrapper has side effects, an exception will be thrown!
         */
        NODISCARD inline Expression expression(OperationWrapper&& op)
        {
            return ExpressionWrapper{} = std::move(op);
        }
    } // namespace operators
} /* namespace vc4c */

namespace std
{
    template <>
    struct hash<vc4c::Expression>
    {
        size_t operator()(const vc4c::Expression& expr) const noexcept;
    };
} /* namespace std */
#endif /* VC4C_EXPRESSION_H */
