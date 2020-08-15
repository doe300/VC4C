/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_OPTIMIZATION_VALUEEXPR
#define VC4C_OPTIMIZATION_VALUEEXPR

#include "../Values.h"

#include <vector>
#include <memory>

namespace vc4c
{
    namespace optimizations
    {
        class ValueExpr
        {
        public:
            // (signed, value)
            using ExpandedExprs = std::vector<std::pair<bool, std::shared_ptr<ValueExpr>>>;

            virtual ~ValueExpr() = default;

            virtual bool operator==(const ValueExpr& other) const = 0;
            inline bool operator!=(const ValueExpr& other) const
            {
                return !(*this == other);
            }

            virtual std::shared_ptr<ValueExpr> replaceLocal(const Value& value, std::shared_ptr<ValueExpr> expr) = 0;

            // expand value expr as liner combination
            // e.g. (a + b) * c = a * c + b * c
            virtual void expand(ExpandedExprs& exprs) = 0;

            virtual Optional<int> getInteger() const = 0;

            virtual std::string to_string() const = 0;
        };

        class ValueBinaryOp : public ValueExpr
        {
        public:
            enum class BinaryOp
            {
                Add,
                Sub,
                Mul,
                Div,
                Other,
            };

            ValueBinaryOp(std::shared_ptr<ValueExpr> left, BinaryOp op, std::shared_ptr<ValueExpr> right) :
                left(left), op(op), right(right)
            {
            }

            bool operator==(const ValueExpr& other) const override;

            std::shared_ptr<ValueExpr> replaceLocal(const Value& value, std::shared_ptr<ValueExpr> expr) override;

            void expand(ExpandedExprs& exprs) override;

            Optional<int> getInteger() const override;

            std::string to_string() const override;

            std::shared_ptr<ValueExpr> left;
            BinaryOp op;
            std::shared_ptr<ValueExpr> right;
        };

        class ValueTerm : public ValueExpr
        {
        public:
            ValueTerm(const Value& value) : value(value) {}

            bool operator==(const ValueExpr& other) const override;

            std::shared_ptr<ValueExpr> replaceLocal(const Value& from, std::shared_ptr<ValueExpr> expr) override;

            void expand(ExpandedExprs& exprs) override;

            Optional<int> getInteger() const override;

            std::string to_string() const override;

            const Value value;
        };

    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_OPTIMIZATION_VALUEEXPR */
