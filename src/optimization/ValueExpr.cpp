/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ValueExpr.h"

#include "../Locals.h"

using namespace vc4c;
using namespace vc4c::optimizations;

bool ValueBinaryOp::operator==(const ValueExpr& other) const
{
    if(auto otherOp = dynamic_cast<const ValueBinaryOp*>(&other))
    {
        return op == otherOp->op && *right == *otherOp->right && *left == *otherOp->left;
    }
    return false;
}

std::shared_ptr<ValueExpr> ValueBinaryOp::replaceLocal(const Value& value, std::shared_ptr<ValueExpr> expr)
{
    return std::make_shared<ValueBinaryOp>(left->replaceLocal(value, expr), op, right->replaceLocal(value, expr));
}

void ValueBinaryOp::expand(ExpandedExprs& exprs)
{
    auto leftNum = left->getInteger();
    auto rightNum = right->getInteger();
    if(leftNum && rightNum)
    {
        int l = leftNum.value_or(0);
        int r = rightNum.value_or(0);
        int num = 0;
        switch(op)
        {
        case BinaryOp::Add:
            num = l + r;
            break;
        case BinaryOp::Sub:
            num = l - r;
            break;
        case BinaryOp::Mul:
            num = l * r;
            break;
        case BinaryOp::Div:
            num = l / r;
            break;
        case BinaryOp::Other:
            break;
        }

        // TODO: Care other types
        auto value = Value(Literal(std::abs(num)), TYPE_INT32);
        std::shared_ptr<ValueExpr> expr = std::make_shared<ValueTerm>(value);
        exprs.push_back(std::make_pair(true, expr));
    }
    else
    {
        switch(op)
        {
        case BinaryOp::Add:
        {
            left->expand(exprs);
            right->expand(exprs);
            break;
        }
        case BinaryOp::Sub:
        {
            left->expand(exprs);

            ExpandedExprs temp;
            right->expand(temp);
            for(auto& e : temp)
            {
                e.first = !e.first;
            }
            exprs.insert(exprs.end(), temp.begin(), temp.end());
            break;
        }
        case BinaryOp::Mul:
        {
            if(leftNum || rightNum)
            {
                int num = 0;
                std::shared_ptr<ValueExpr> expr = nullptr;
                if(leftNum)
                {
                    num = leftNum.value_or(0);
                    expr = right;
                }
                else
                {
                    num = rightNum.value_or(0);
                    expr = left;
                }
                for(int i = 0; i < num; i++)
                {
                    exprs.push_back(std::make_pair(true, expr));
                }
            }
            else
            {
                exprs.push_back(std::make_pair(true, std::make_shared<ValueBinaryOp>(left, op, right)));
            }
            break;
        }
        case BinaryOp::Div:
        {
            exprs.push_back(std::make_pair(true, std::make_shared<ValueBinaryOp>(left, op, right)));
            break;
        }
        case BinaryOp::Other:
            break;
        }
    }
}

Optional<int> ValueBinaryOp::getInteger() const
{
    return Optional<int>();
}

std::string ValueBinaryOp::to_string() const
{
    std::string opStr;
    switch(op)
    {
    case BinaryOp::Add:
        opStr = "+";
        break;
    case BinaryOp::Sub:
        opStr = "-";
        break;
    case BinaryOp::Mul:
        opStr = "*";
        break;
    case BinaryOp::Div:
        opStr = "/";
        break;
    case BinaryOp::Other:
        opStr = "other";
        break;
    }

    return "(" + left->to_string() + " " + opStr + " " + right->to_string() + ")";
}

std::shared_ptr<ValueExpr> optimizations::makeValueBinaryOpFromLocal(Value& left, ValueBinaryOp::BinaryOp binOp, Value& right)
{
    return std::make_shared<ValueBinaryOp>(
            std::make_shared<ValueTerm>(left), binOp, std::make_shared<ValueTerm>(right));
}

bool ValueTerm::operator==(const ValueExpr& other) const
{
    if(auto otherTerm = dynamic_cast<const ValueTerm*>(&other))
        return value == otherTerm->value;
    return false;
}

std::shared_ptr<ValueExpr> ValueTerm::replaceLocal(const Value& from, std::shared_ptr<ValueExpr> expr)
{
    if(auto fromLocal = from.checkLocal())
    {
        if(auto valueLocal = value.checkLocal())
        {
            if(*fromLocal == *valueLocal)
            {
                return expr;
            }
        }
    }
    return std::make_shared<ValueTerm>(value);
}

void ValueTerm::expand(ExpandedExprs& exprs)
{
    exprs.push_back(std::make_pair(true, std::make_shared<ValueTerm>(value)));
}

Optional<int> ValueTerm::getInteger() const
{
    if(auto lit = value.checkLiteral())
    {
        return Optional<int>(lit->signedInt());
    }
    else if(auto imm = value.checkImmediate())
    {
        return imm->getIntegerValue();
    }
    return Optional<int>();
}

std::string ValueTerm::to_string() const
{
    return value.to_string();
}

