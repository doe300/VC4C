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
        OpCode code;
        Value arg0;
        Optional<Value> arg1;
        Unpack unpackMode = UNPACK_NOP;
        Pack packMode = PACK_NOP;
        intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;

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
