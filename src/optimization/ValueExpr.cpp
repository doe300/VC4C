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
    ExpandedExprs leftEE, rightEE;
    left->expand(leftEE);
    right->expand(rightEE);

    auto getInteger = [](const std::pair<bool, std::shared_ptr<ValueExpr>> &v) {
        std::function<Optional<int>(const int&)> addSign = [&](const int& num) {
            return make_optional(v.first ? num : -num);
        };
        return v.second->getInteger() & addSign;
    };

    auto leftNum = (leftEE.size() == 1) ? getInteger(leftEE[0]) : Optional<int>();
    auto rightNum = (rightEE.size() == 1) ? getInteger(rightEE[0]) : Optional<int>();

    auto append = [](ExpandedExprs &ee1, ExpandedExprs &ee2) {
        ee1.insert(ee1.end(), ee2.begin(), ee2.end());
    };

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
            append(exprs, leftEE);
            append(exprs, rightEE);
            break;
        }
        case BinaryOp::Sub:
        {
            append(exprs, leftEE);

            for(auto& e : rightEE)
            {
                e.first = !e.first;
            }
            append(exprs, rightEE);
            break;
        }
        case BinaryOp::Mul:
        {
            if(leftNum || rightNum)
            {
                int num = 0;
                ExpandedExprs *ee = nullptr;
                if(leftNum)
                {
                    num = leftNum.value_or(0);
                    ee = &rightEE;
                }
                else
                {
                    num = rightNum.value_or(0);
                    ee = &leftEE;
                }
                for(int i = 0; i < num; i++)
                {
                    append(exprs, *ee);
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

std::shared_ptr<ValueExpr> optimizations::makeValueBinaryOpFromLocal(
    Value& left, ValueBinaryOp::BinaryOp binOp, Value& right)
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
