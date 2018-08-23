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
        const OpCode code;
        const Value arg0;
        const Optional<Value> arg1;
        const Unpack unpackMode = UNPACK_NOP;
        const Pack packMode = PACK_NOP;
        const intermediate::InstructionDecorations deco;

        static Optional<Expression> createExpression(const intermediate::IntermediateInstruction& instr);

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
         * Returns a copy of this expression, if no combination could be done
         */
        Expression combineWith(const FastMap<const Local*, Expression>& inputs) const;
    };

    template <>
    struct hash<Expression>
    {
        size_t operator()(const Expression& expr) const noexcept;
    };
}
#endif /* VC4C_EXPRESSION_H */