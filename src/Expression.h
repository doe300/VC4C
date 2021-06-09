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
#include "performance.h"

#include <memory>
#include <string>
#include <vector>

namespace vc4c
{
    struct Expression;

    /*
     * Maps the available locals and the available expression writing into the given local for a given point in
     * the program code. The additional integer value is the distance in instructions from the current position
     * where the expression was written.
     */
    using AvailableExpressions =
        FastMap<std::shared_ptr<Expression>, std::pair<const intermediate::IntermediateInstruction*, unsigned>>;

    // Using shared_ptr allows to copy expressions and also to share subexpressions
    struct SubExpression : private Variant<VariantNamespace::monostate, Value, std::shared_ptr<Expression>>
    {
        using Base = Variant<VariantNamespace::monostate, Value, std::shared_ptr<Expression>>;
        explicit SubExpression() : Base(VariantNamespace::monostate{}) {}
        SubExpression(const Value& val) : Base(val) {}
        SubExpression(Value&& val) : Base(std::move(val)) {}
        SubExpression(const Optional<Value>& val);
        SubExpression(Optional<Value>&& val);
        SubExpression(const std::shared_ptr<Expression>& child) : Base(child) {}

        explicit inline operator bool() const noexcept
        {
            return !VariantNamespace::holds_alternative<VariantNamespace::monostate>(*this);
        }

        bool operator==(const SubExpression& other) const;
        inline bool operator!=(const SubExpression& other) const
        {
            return !(*this == other);
        }

        std::string to_string() const;
        Optional<Value> getConstantExpression() const;
        bool isConstant() const;

        inline Optional<Value> checkValue() const
        {
            if(auto val = VariantNamespace::get_if<Value>(this))
                return *val;
            return NO_VALUE;
        }

        inline std::shared_ptr<Expression> checkExpression() const
        {
            if(auto expr = VariantNamespace::get_if<std::shared_ptr<Expression>>(this))
                return *expr ? (*expr) : nullptr;
            return nullptr;
        }

        inline const Local* checkLocal() const
        {
            if(auto val = VariantNamespace::get_if<Value>(this))
                return val->checkLocal();
            return nullptr;
        }

        inline Optional<Literal> getLiteralValue() const
        {
            if(auto val = VariantNamespace::get_if<Value>(this))
                return val->getLiteralValue();
            return {};
        }

        friend std::hash<vc4c::SubExpression>;
    };

    /**
     * Options to control the generation of expressions
     *
     * This is a bit-mask!
     */
    enum class ExpressionOptions
    {
        NONE = 0,
        // Allow creating of fake (as in not represent-able in machine instructions) operation codes, e.g. simplified
        // unsigned multiplication
        ALLOW_FAKE_OPS = 1,
        // Stop at any built-in work-item and work-group value, e.g. local/global ids, work-group size.
        STOP_AT_BUILTINS = 2,
        // Recursively combine the expressions until it cannot be simplified anymore
        RECURSIVE = 4
    };

    /**
     * An expression is an abstraction of an ALU operation (or load) where only the inputs and the type of operation
     * is considered.
     *
     * Expressions might not have any side-effects or conditional execution!
     */
    struct Expression : public std::enable_shared_from_this<Expression>
    {
        // A fake operation to indicate an unsigned multiplication
        static constexpr OpCode FAKEOP_UMUL{"umul", 132, 132, 2, false, false, FlagBehavior::NONE};

        OpCode code;
        SubExpression arg0;
        SubExpression arg1{};
        Unpack unpackMode = UNPACK_NOP;
        Pack packMode = PACK_NOP;
        intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;
        // The optional value this expression is written to
        const Local* outputValue = nullptr;

        Expression(const OpCode& op, const SubExpression& first, const SubExpression& second = SubExpression{},
            Unpack unpack = UNPACK_NOP, Pack pack = PACK_NOP, intermediate::InstructionDecorations decorations = {},
            const Local* outLoc = nullptr);

        /**
         * Tries to create an expression representing the single instruction
         */
        static std::shared_ptr<Expression> createExpression(const intermediate::IntermediateInstruction& instr);

        /**
         * Tries to create an expression representing the calculation in the given instruction while also regarding the
         * instructions calculating the instruction arguments.
         *
         * If the optional parameter allowFakeOperations is set, the resulting expression might be an operation, which
         * can not be mapped to an instruction, e.g. the fake "umul" defined above.
         */
        static std::shared_ptr<Expression> createRecursiveExpression(const intermediate::IntermediateInstruction& instr,
            unsigned maxDepth = 6,
            ExpressionOptions options = add_flag(
                ExpressionOptions::ALLOW_FAKE_OPS, ExpressionOptions::STOP_AT_BUILTINS, ExpressionOptions::RECURSIVE));

        bool operator==(const Expression& other) const;
        inline bool operator!=(const Expression& other) const
        {
            return !(*this == other);
        }

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
        std::shared_ptr<Expression> combineWith(const FastMap<const Local*, std::shared_ptr<Expression>>& inputs,
            ExpressionOptions options = ExpressionOptions::NONE);

        /**
         * Returns the value this expression converges to (if it converges at all).
         *
         * For the calculation of the limit it is assumed that the expression is calculated an infinite number of times
         * with the output of the previous calculation taken as the single input local of the next calculation.
         *
         * If the optional initial value is given, it will be used as start point of the induction.
         *
         * An expression converges to a given limit, if:
         * - the op-code itself converges (e.g. add converges to +-INT_MAX, or does not converge)
         * - the expression takes exactly 1 non-constant argument
         * - the result converges linearly, e.g. for add/sub. Shl also converges to zero on its first operand, but jumps
         *   between negative and positive values first, thus it is not considered.
         */
        Optional<Value> getConvergenceLimit(Optional<Literal> initialValue = {}) const;

        /**
         * Returns the instruction representing this expression or a nullptr of no such instruction can be formed.
         *
         * NOTE: Only simple expressions (without sub-expressions) or expressions where the optional output values for
         * all sub-expressions are known can be converted to instructions!
         */
        intermediate::IntermediateInstruction* toInstruction(const Value& output) const;

        /**
         * If possible, generates instructions for this and all child expressions, if there a not yet any matching
         * instructions for the (sub-)expressions in the input container.
         *
         * Returns whether instructions were inserted
         */
        NODISCARD bool insertInstructions(
            InstructionWalker& it, const Value& out, const AvailableExpressions& existingExpressions) const;

        inline Expression& addDecorations(intermediate::InstructionDecorations newDeco)
        {
            deco = add_flag(deco, newDeco);
            return *this;
        }

        /**
         * Writes a graphical representation of this (recursive) expression into the given file
         */
        void dumpTree(const std::string& path) const;

        /**
         * Tries to split this expression into a constant and a dynamic part.
         *
         * Adding the resulting subexpressions gives the same result as this expression gives.
         *
         * The constant part is any subexpression returning compile-time constant (or if the boolean parameter is set,
         * also work-group uniform) values.
         * The dynamic part contains the subexpression with all other parts of this expression.
         *
         * NOTE: This can only return the parts for expressions where the two parts can be cleanly separated!
         */
        std::pair<SubExpression, SubExpression> splitIntoDynamicAndConstantPart(
            bool workGroupUniformIsConstant, ExpressionOptions options = ExpressionOptions::NONE);

        /**
         * Returns the list of operands in this expression, iff this expression represents an associative opcode.
         * Otherwise returns an empty list.
         *
         * E.g. for an expression (a + b) + c, the parts a, b and c are returned.
         */
        std::vector<SubExpression> getAssociativeParts() const;
    };

    // Extends the operator syntax to create expressions from it
    namespace operators
    {
        struct ExpressionWrapper
        {
            NODISCARD std::shared_ptr<Expression> operator=(OperationWrapper&& op) &&;
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
        NODISCARD inline std::shared_ptr<Expression> expression(OperationWrapper&& op)
        {
            return ExpressionWrapper{} = std::move(op);
        }
    } // namespace operators
} /* namespace vc4c */

namespace std
{
    template <>
    struct hash<vc4c::SubExpression>
    {
        size_t operator()(const vc4c::SubExpression& expr) const noexcept;
    };

    template <>
    struct hash<vc4c::Expression>
    {
        size_t operator()(const vc4c::Expression& expr) const noexcept;
    };
} /* namespace std */
#endif /* VC4C_EXPRESSION_H */
