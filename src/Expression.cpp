#include "Expression.h"

#include "Profiler.h"
#include "log.h"

using namespace vc4c;

constexpr OpCode Expression::FAKEOP_UMUL;

SubExpression::SubExpression(const Optional<Value>& val) : Base(VariantNamespace::monostate{})
{
    if(val)
        *this = *val;
}

SubExpression::SubExpression(Optional<Value>&& val) : Base(VariantNamespace::monostate{})
{
    if(val)
        *this = std::move(val).value();
}

bool SubExpression::operator==(const SubExpression& other) const
{
    if(index() != other.index())
        return false;
    if(VariantNamespace::get_if<VariantNamespace::monostate>(this))
        return true;
    if(auto val = VariantNamespace::get_if<Value>(this))
        return *val == VariantNamespace::get<Value>(other);
    if(auto expr = VariantNamespace::get_if<std::shared_ptr<Expression>>(this))
    {
        auto otherExpr = VariantNamespace::get_if<std::shared_ptr<Expression>>(&other);
        // we compare by value to make sure we can merge same (but not identical object) subexpressions
        return (!expr && !otherExpr) || (!(*expr) && !(*otherExpr)) || *expr == *otherExpr || **expr == **otherExpr;
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled subexpression type");
}

LCOV_EXCL_START
std::string SubExpression::to_string() const
{
    if(VariantNamespace::get_if<VariantNamespace::monostate>(this))
        return "-";
    if(auto val = VariantNamespace::get_if<Value>(this))
        return val->to_string();
    if(auto expr = VariantNamespace::get_if<std::shared_ptr<Expression>>(this))
    {
        if(auto ptr = *expr)
            return "(" + ptr->to_string() + ")";
        else
            return "-";
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled subexpression type");
}
LCOV_EXCL_STOP

Optional<Value> SubExpression::getConstantExpression() const
{
    if(VariantNamespace::get_if<VariantNamespace::monostate>(this))
        return NO_VALUE;
    if(auto val = VariantNamespace::get_if<Value>(this))
        return *val;
    if(auto expr = VariantNamespace::get_if<std::shared_ptr<Expression>>(this))
    {
        if(auto ptr = *expr)
            return ptr->getConstantExpression();
        return NO_VALUE;
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled subexpression type");
}

static bool isConstantOperand(const Value& op)
{
    return op.getLiteralValue() || op.checkVector();
}

bool SubExpression::isConstant() const
{
    if(auto val = VariantNamespace::get_if<Value>(this))
        return isConstantOperand(*val);
    return false;
}

std::shared_ptr<Expression> Expression::createExpression(const intermediate::IntermediateInstruction& instr)
{
    if(instr.hasSideEffects())
        return {};
    if(instr.hasConditionalExecution())
        return {};
    if(instr.readsRegister(REG_REPLICATE_ALL) || instr.readsRegister(REG_REPLICATE_QUAD))
        // not actually a side-effect, but cannot be combined with any other expression
        return {};
    if(dynamic_cast<const intermediate::Operation*>(&instr) == nullptr &&
        dynamic_cast<const intermediate::MoveOperation*>(&instr) == nullptr &&
        dynamic_cast<const intermediate::LoadImmediate*>(&instr) == nullptr)
        // not an ALU or load operation
        return {};
    if(dynamic_cast<const intermediate::VectorRotation*>(&instr) != nullptr)
        // skip vector rotations
        return {};
    if(dynamic_cast<const intermediate::LoadImmediate*>(&instr) &&
        dynamic_cast<const intermediate::LoadImmediate*>(&instr)->type != intermediate::LoadType::REPLICATE_INT32)
        // skip loading of masked values
        return {};

    auto code = dynamic_cast<const intermediate::Operation*>(&instr) != nullptr ?
        dynamic_cast<const intermediate::Operation&>(instr).op :
        OP_V8MIN;
    return std::make_shared<Expression>(code, instr.getArgument(0).value(),
        instr.getArgument(1) ? instr.getArgument(1) : code == OP_V8MIN ? instr.getArgument(0) : NO_VALUE,
        instr.unpackMode, instr.packMode, instr.decoration);
}

static std::shared_ptr<Expression> createRecursiveExpressionInner(const intermediate::IntermediateInstruction& instr,
    unsigned maxDepth, FastMap<const Local*, std::shared_ptr<Expression>>& parentsCache, ExpressionOptions options)
{
    bool stopAtBuiltin = has_flag(options, ExpressionOptions::STOP_AT_BUILTINS);
    if(auto expr = Expression::createExpression(instr))
    {
        if(stopAtBuiltin && intermediate::isGroupBuiltin(expr->deco, true))
            return expr;
        if(maxDepth > 0)
        {
            for(auto& arg : instr.getArguments())
            {
                if(auto writer = arg.getSingleWriter())
                {
                    if(parentsCache.find(arg.local()) != parentsCache.end())
                        continue;
                    if(auto parentExpr = createRecursiveExpressionInner(*writer, maxDepth - 1, parentsCache, options))
                        parentsCache.emplace(arg.local(), std::move(parentExpr));
                }
            }
            return expr->combineWith(parentsCache, options);
        }
        return expr;
    }
    return {};
}

std::shared_ptr<Expression> Expression::createRecursiveExpression(
    const intermediate::IntermediateInstruction& instr, unsigned maxDepth, ExpressionOptions options)
{
    PROFILE_START(createRecursiveExpression);
    FastMap<const Local*, std::shared_ptr<Expression>> parentsCache;
    auto exp = createRecursiveExpressionInner(instr, maxDepth, parentsCache, options);
    PROFILE_END(createRecursiveExpression);
    return exp;
}

bool Expression::operator==(const Expression& other) const
{
    return code == other.code &&
        ((arg0 == other.arg0 && arg1 == other.arg1) ||
            (code.isCommutative() && arg0 == other.arg1 && arg1 == other.arg0)) &&
        unpackMode == other.unpackMode && packMode == other.packMode && deco == other.deco;
}

LCOV_EXCL_START
std::string Expression::to_string() const
{
    auto basic = std::string(code.name) + " " + arg0.to_string() + ", " + arg1.to_string();
    std::string extra;
    extra += unpackMode.hasEffect() ? unpackMode.to_string() + " " : "";
    extra += packMode.hasEffect() ? packMode.to_string() + " " : "";
    extra += intermediate::toString(deco);
    return basic + (extra.empty() ? "" : (" (" + extra + ')'));
}
LCOV_EXCL_STOP

bool Expression::isMoveExpression() const
{
    return code.numOperands == 2 && code.isIdempotent() && arg1 == arg0 && !unpackMode.hasEffect() &&
        !packMode.hasEffect();
}

Optional<Value> Expression::getConstantExpression() const
{
    if(auto firstVal = arg0.getConstantExpression())
    {
        auto secondVal = arg1.getConstantExpression();
        return code(*firstVal, secondVal).first;
    }
    return NO_VALUE;
}

bool Expression::hasConstantOperand() const
{
    return arg0.isConstant() || arg1.isConstant();
}

static Optional<Value> calculate(const OpCode& code, const Optional<Value>& firstArg, const Optional<Value>& secondArg)
{
    if(firstArg)
        return code(*firstArg, secondArg).first;
    return NO_VALUE;
}

// need special check, since Values with Literal and SmallImmediates do not compare equal
static bool hasValue(const Optional<Value>& val, Optional<Literal> lit)
{
    return val && lit && val->hasLiteral(*lit);
}

std::shared_ptr<Expression> Expression::combineWith(
    const FastMap<const Local*, std::shared_ptr<Expression>>& inputs, ExpressionOptions options)
{
    bool allowFakeOperations = has_flag(options, ExpressionOptions::ALLOW_FAKE_OPS);
    bool stopAtBuiltin = has_flag(options, ExpressionOptions::STOP_AT_BUILTINS);

    auto firstVal = arg0.checkValue();
    auto secondVal = arg1.checkValue();
    auto firstExpr = arg0.checkExpression();
    auto secondExpr = arg1.checkExpression();

    if(stopAtBuiltin && intermediate::isGroupBuiltin(deco, true))
        return shared_from_this();

    if(auto firstLoc = arg0.checkLocal())
    {
        auto it = inputs.find(firstLoc);
        if(it != inputs.end())
        {
            firstVal = NO_VALUE;
            firstExpr = it->second;
            arg0 = firstExpr;
        }
    }
    if(auto secondLoc = arg1.checkLocal())
    {
        auto it = inputs.find(secondLoc);
        if(it != inputs.end())
        {
            secondVal = NO_VALUE;
            secondExpr = it->second;
            arg1 = secondExpr;
        }
    }

    // replace move expressions by their sources, if possible
    if(firstExpr && firstExpr->isMoveExpression())
    {
        if(firstExpr->arg0.checkLocal() && inputs.find(firstExpr->arg0.checkLocal()) != inputs.end())
        {
            // replace left expression by source
            firstVal = NO_VALUE;
            firstExpr = inputs.at(firstExpr->arg0.checkLocal());
            arg0 = firstExpr;
        }
        else
        {
            arg0 = firstExpr->arg0;
            firstVal = arg0.checkValue();
            firstExpr = arg0.checkExpression();
        }
    }

    if(secondExpr && secondExpr->isMoveExpression())
    {
        if(secondExpr->arg0.checkLocal() && inputs.find(secondExpr->arg0.checkLocal()) != inputs.end())
        {
            // replace right expression by source
            secondVal = NO_VALUE;
            secondExpr = inputs.at(secondExpr->arg0.checkLocal());
            arg1 = secondExpr;
        }
        else
        {
            arg1 = secondExpr->arg0;
            secondVal = arg1.checkValue();
            secondExpr = arg1.checkExpression();
        }
    }

    if(unpackMode.hasEffect() || packMode.hasEffect() ||
        (firstExpr != nullptr && (firstExpr->unpackMode.hasEffect() || firstExpr->packMode.hasEffect())) ||
        ((secondExpr != nullptr && (secondExpr->unpackMode.hasEffect() || secondExpr->packMode.hasEffect()))))
        // cannot combine pack modes
        return shared_from_this();

    // "replace" this with source expression
    if(isMoveExpression())
    {
        if(firstExpr)
            return firstExpr->combineWith(inputs);
        if(secondExpr)
            return secondExpr->combineWith(inputs);
    }

    if(code.numOperands == 1 && firstExpr != nullptr)
    {
        if(code.isIdempotent() && firstExpr->code == code)
            // f(f(a)) = f(a)
            return std::make_shared<Expression>(
                code, firstExpr->arg0, NO_VALUE, UNPACK_NOP, PACK_NOP, add_flag(deco, firstExpr->deco));
        // NOTE: ftoi(itof(i)) != i, itof(ftoi(f)) != f, since the truncation/rounding would get lost!
        if(code.isSelfInverse() && code == firstExpr->code)
        {
            // f(f(a)) = a, e.g. not(not(a)) = a
            auto inner = firstExpr->arg0.checkExpression();
            return inner ? inner->addDecorations(deco).shared_from_this() :
                           std::make_shared<Expression>(OP_V8MIN, firstExpr->arg0, firstExpr->arg0, UNPACK_NOP,
                               PACK_NOP, add_flag(deco, firstExpr->deco));
        }
    }

    if(code.numOperands == 2)
    {
        if(code.isIdempotent() && arg0 == arg1)
            // f(a, a) = a
            return firstExpr ? firstExpr->addDecorations(deco).shared_from_this() :
                               std::make_shared<Expression>(OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco);
        if(code.isSelfInverse() && arg0 == arg1)
        {
            // f(a, a) = 0, e.g. a - a = 0
            auto zero = code.returnsFloat ? FLOAT_ZERO : INT_ZERO;
            return std::make_shared<Expression>(OP_V8MIN, zero, zero, UNPACK_NOP, PACK_NOP, deco);
        }
        if(firstVal && hasValue(firstVal, OpCode::getLeftIdentity(code) & &Value::getLiteralValue))
            // f(id, a) = a
            return secondExpr ? secondExpr->addDecorations(deco).shared_from_this() :
                                std::make_shared<Expression>(OP_V8MIN, arg1, arg1, UNPACK_NOP, PACK_NOP, deco);
        if(secondVal && hasValue(secondVal, OpCode::getRightIdentity(code) & &Value::getLiteralValue))
            // f(a, id) = a
            return firstExpr ? firstExpr->addDecorations(deco).shared_from_this() :
                               std::make_shared<Expression>(OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco);
        if(firstVal && hasValue(firstVal, OpCode::getLeftAbsorbingElement(code) & &Value::getLiteralValue))
            // f(absorb, a) = absorb
            return std::make_shared<Expression>(OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco);
        if(secondVal && hasValue(secondVal, OpCode::getRightAbsorbingElement(code) & &Value::getLiteralValue))
            // f(a, absorb) = absorb
            return std::make_shared<Expression>(OP_V8MIN, arg1, arg1, UNPACK_NOP, PACK_NOP, deco);

        if(firstVal && code.isAssociative() && secondExpr && secondExpr->code == code && arg0.isConstant() &&
            secondExpr->arg0.isConstant())
        {
            // f(constA, f(constB, a)) = f(f(constA, constB), a), if associative
            if(auto tmp = calculate(code, firstVal, secondExpr->arg0.checkValue()))
            {
                return std::make_shared<Expression>(code, *tmp, secondExpr->arg1, UNPACK_NOP, PACK_NOP, deco);
            }
        }

        if(secondVal && code.isAssociative() && firstExpr && firstExpr->code == code && arg1.isConstant() &&
            firstExpr->arg1.isConstant())
        {
            // f(f(a, constA), constB) = f(a, f(constA, constB)), if associative
            if(auto tmp = calculate(code, firstExpr->arg1.checkValue(), secondVal))
            {
                return std::make_shared<Expression>(code, firstExpr->arg0, tmp, UNPACK_NOP, PACK_NOP, deco);
            }
        }

        if(firstVal && code.isAssociative() && code.isCommutative() && secondExpr && secondExpr->code == code &&
            arg0.isConstant() && secondExpr->arg1.isConstant())
        {
            // f(constA, f(a, constB)) = f(f(constA, constB), a), if associative and commutative
            if(auto tmp = calculate(code, firstVal, secondExpr->arg1.checkValue()))
            {
                return std::make_shared<Expression>(code, *tmp, secondExpr->arg0, UNPACK_NOP, PACK_NOP, deco);
            }
        }

        if(secondVal && code.isAssociative() && code.isCommutative() && firstExpr && firstExpr->code == code &&
            arg1.isConstant() && firstExpr->arg0.isConstant())
        {
            // f(f(constA, a), constB) = f(f(constA, constB), a), if associative and commutative
            if(auto tmp = calculate(code, firstExpr->arg0.checkValue(), secondVal))
            {
                return std::make_shared<Expression>(code, *tmp, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco);
            }
        }

        if(code.isAssociative() && code.isIdempotent() && secondExpr && secondExpr->code == code &&
            secondExpr->arg0 == arg0)
        {
            // f(a, f(a, b)) = f(a, b), if associative and idempotent
            return std::make_shared<Expression>(code, arg0, secondExpr->arg1, UNPACK_NOP, PACK_NOP, deco);
        }

        if(code.isAssociative() && code.isIdempotent() && firstExpr && firstExpr->code == code &&
            firstExpr->arg1 == arg1)
        {
            // f(f(a, b), b) = f(a, b), if associative and idempotent
            return std::make_shared<Expression>(code, firstExpr->arg0, arg1, UNPACK_NOP, PACK_NOP, deco);
        }

        if(code.isAssociative() && code.isIdempotent() && code.isCommutative() && secondExpr &&
            secondExpr->code == code && secondExpr->arg1 == arg0)
        {
            // f(a, f(b, a)) = f(a, b), if associative, commutative and idempotent
            return std::make_shared<Expression>(code, arg0, secondExpr->arg0, UNPACK_NOP, PACK_NOP, deco);
        }

        if(code.isAssociative() && code.isIdempotent() && code.isCommutative() && firstExpr &&
            firstExpr->code == code && firstExpr->arg0 == arg1)
        {
            // f(f(a, b), a) = f(a, b), if associative, commutative and idempotent
            return std::make_shared<Expression>(code, firstExpr->arg0, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco);
        }

        if(firstExpr && secondExpr && firstExpr->code == secondExpr->code &&
            firstExpr->code.isLeftDistributiveOver(code) && firstExpr->arg0 == secondExpr->arg0)
        {
            // g(f(a, b), f(a, c)) = f(a, g(b, c)), if left distributive
            if(auto tmp = calculate(code, firstExpr->arg1.checkValue(), secondExpr->arg1.checkValue()))
                return std::make_shared<Expression>(firstExpr->code, firstExpr->arg0, tmp, UNPACK_NOP, PACK_NOP, deco);
            // general non-constant case
            auto tmp = std::make_shared<Expression>(code, firstExpr->arg1, secondExpr->arg1);
            return std::make_shared<Expression>(firstExpr->code, firstExpr->arg0, tmp, UNPACK_NOP, PACK_NOP, deco);
        }

        if(firstExpr && secondExpr && firstExpr->code == secondExpr->code &&
            firstExpr->code.isRightDistributiveOver(code) && firstExpr->arg1 == secondExpr->arg1)
        {
            // g(f(a, b), f(c, b)) = f(g(a, c), b), if right distributive
            if(auto tmp = calculate(code, firstExpr->arg0.checkValue(), secondExpr->arg0.checkValue()))
                return std::make_shared<Expression>(firstExpr->code, *tmp, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco);
            // general non-constant case
            auto tmp = std::make_shared<Expression>(code, firstExpr->arg0, secondExpr->arg0);
            return std::make_shared<Expression>(firstExpr->code, tmp, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco);
        }

        // TODO more properties to use (e.g. distributive and commutative?)

        if(code == OP_FADD && arg0 == arg1)
        {
            // doesn't save any instruction, but utilizes mul ALU
            return std::make_shared<Expression>(
                OP_FMUL, arg0, Value(Literal(2.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP, deco);
        }

        // TODO generalize! E.g. for fsub
        if(code == OP_FADD && firstExpr && firstExpr->code == OP_FMUL)
        {
            if(firstExpr->arg0 == arg1 && firstExpr->arg1.getLiteralValue())
                // fadd(fmul(a, constB), a) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg1,
                    Value(Literal(firstExpr->arg1.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, firstExpr->deco));
            if(firstExpr->arg1 == arg1 && firstExpr->arg0.getLiteralValue())
                // fadd(fmul(constB, a), a) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg1,
                    Value(Literal(firstExpr->arg0.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, firstExpr->deco));
        }
        if(code == OP_FADD && secondExpr && secondExpr->code == OP_FMUL)
        {
            if(secondExpr->arg0 == arg0 && secondExpr->arg1.getLiteralValue())
                // fadd(a, fmul(a, constB)) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg0,
                    Value(Literal(secondExpr->arg1.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, secondExpr->deco));
            if(secondExpr->arg1 == arg0 && secondExpr->arg0.getLiteralValue())
                // fadd(a, fmul(constB, a)) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg0,
                    Value(Literal(secondExpr->arg0.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, secondExpr->deco));
        }

        // TODO generalize! E.g. via mapping opcode -> inverse opcode (inverse according to which of the operands?!)
        if(firstExpr && arg1 && firstExpr->arg1 && (arg1 == firstExpr->arg0 || arg1 == firstExpr->arg1))
        {
            // NOTE: This rewrite removes overflow since it removes two operations that could overflow!
            const auto& otherArg = arg1 == firstExpr->arg0 ? firstExpr->arg1 : firstExpr->arg0;
            auto otherExpr = otherArg.checkExpression();
            // (a + b) - a = b
            // (a + b) - b = a
            // (a - b) + b = a
            if((code == OP_SUB && firstExpr->code == OP_ADD) || (code == OP_FSUB && firstExpr->code == OP_FADD) ||
                (code == OP_ADD && firstExpr->code == OP_SUB && arg1 == firstExpr->arg1) ||
                (code == OP_FADD && firstExpr->code == OP_FSUB && arg1 == firstExpr->arg1))
                return otherExpr ?
                    otherExpr->addDecorations(deco).shared_from_this() :
                    std::make_shared<Expression>(OP_V8MIN, otherArg, otherArg, UNPACK_NOP, PACK_NOP, deco);

            // (a - b) - a = -b
            if((code == OP_SUB && firstExpr->code == OP_SUB && arg1 == firstExpr->arg0) ||
                (code == OP_FSUB && firstExpr->code == OP_FSUB && arg1 == firstExpr->arg0))
                return std::make_shared<Expression>(code, INT_ZERO, otherArg, UNPACK_NOP, PACK_NOP, deco);
        }
        if(secondExpr && secondExpr->arg1 && (arg0 == secondExpr->arg0 || arg0 == secondExpr->arg1))
        {
            // NOTE: This rewrite removes overflow since it removes two operations that could overflow!
            const auto& otherArg = arg0 == secondExpr->arg0 ? secondExpr->arg1 : secondExpr->arg0;
            auto otherExpr = otherArg.checkExpression();
            // a + (b - a) = b
            if((code == OP_ADD && secondExpr->code == OP_SUB && arg0 == secondExpr->arg1) ||
                (code == OP_FADD && secondExpr->code == OP_FSUB && arg0 == secondExpr->arg1))
                return otherExpr ?
                    otherExpr->addDecorations(deco).shared_from_this() :
                    std::make_shared<Expression>(OP_V8MIN, otherArg, otherArg, UNPACK_NOP, PACK_NOP, deco);

            // a - (b + a) = -b
            // a - (a + b) = -b
            if((code == OP_SUB && secondExpr->code == OP_ADD) || (code == OP_FSUB && secondExpr->code == OP_FADD))
                return std::make_shared<Expression>(code, INT_ZERO, otherArg, UNPACK_NOP, PACK_NOP, deco);
        }

        // (a << const) + a = a + (a << const) -> a * ((1 << const) + 1)
        if(allowFakeOperations && code == OP_ADD && firstExpr && firstExpr->code == OP_SHL && firstExpr->arg0 == arg1 &&
            (firstExpr->arg1.getLiteralValue()))
        {
            auto factor = (1u << firstExpr->arg1.getLiteralValue()->unsignedInt()) + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }
        if(allowFakeOperations && code == OP_ADD && secondExpr && secondExpr->code == OP_SHL &&
            secondExpr->arg0 == arg0 && (secondExpr->arg1.getLiteralValue()))
        {
            auto factor = (1u << secondExpr->arg1.getLiteralValue()->unsignedInt()) + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }

        // (a << const) - a = a * ((1 << const) - 1)
        if(allowFakeOperations && code == OP_SUB && firstExpr && firstExpr->code == OP_SHL && firstExpr->arg0 == arg1 &&
            firstExpr->arg1.getLiteralValue())
        {
            auto factor = (1u << firstExpr->arg1.getLiteralValue()->unsignedInt()) - 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }

        // (a * constA) << constB = (constA * a) << constB -> a * (constA << constB)
        if(allowFakeOperations && code == OP_SHL && firstExpr && firstExpr->code == FAKEOP_UMUL &&
            (firstExpr->arg1.getLiteralValue()) && (arg1.getLiteralValue()))
        {
            auto factor = firstExpr->arg1.getLiteralValue()->unsignedInt() << arg1.getLiteralValue()->unsignedInt();
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }
        if(allowFakeOperations && code == OP_SHL && firstExpr && firstExpr->code == FAKEOP_UMUL && firstExpr->arg1 &&
            (firstExpr->arg0.getLiteralValue()) && (arg1.getLiteralValue()))
        {
            auto factor = firstExpr->arg0.getLiteralValue()->unsignedInt() << arg1.getLiteralValue()->unsignedInt();
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg1, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }

        // (a * constA) + a = (constA * a) + a = a + (a * constA) = a + (constA * a) = a * (constA + 1)
        if(allowFakeOperations && code == OP_ADD && firstExpr && firstExpr->code == FAKEOP_UMUL &&
            (firstExpr->arg0 == arg1) && (firstExpr->arg1.getLiteralValue()))
        {
            auto factor = firstExpr->arg1.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }
        if(allowFakeOperations && code == OP_ADD && firstExpr && firstExpr->code == FAKEOP_UMUL && firstExpr->arg1 &&
            firstExpr->arg1 == arg1 && (firstExpr->arg0.getLiteralValue()))
        {
            auto factor = firstExpr->arg0.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg1, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }
        if(allowFakeOperations && code == OP_ADD && secondExpr && secondExpr->code == FAKEOP_UMUL &&
            (secondExpr->arg0 == arg0) && (secondExpr->arg1.getLiteralValue()))
        {
            auto factor = secondExpr->arg1.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, secondExpr->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }
        if(allowFakeOperations && code == OP_ADD && secondExpr && secondExpr->code == FAKEOP_UMUL && secondExpr->arg1 &&
            secondExpr->arg1 == arg0 && (secondExpr->arg0.getLiteralValue()))
        {
            auto factor = secondExpr->arg0.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }

        // (a * constA) - a = (constA * a) - a = a * (constA - 1)
        if(allowFakeOperations && code == OP_SUB && firstExpr && firstExpr->code == FAKEOP_UMUL &&
            (firstExpr->arg0 == arg1) && (firstExpr->arg1.getLiteralValue()))
        {
            auto factor = firstExpr->arg1.getLiteralValue()->unsignedInt() - 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }
        if(allowFakeOperations && code == OP_SUB && firstExpr && firstExpr->code == FAKEOP_UMUL && firstExpr->arg1 &&
            firstExpr->arg1 == arg1 && (firstExpr->arg0.getLiteralValue()))
        {
            auto factor = firstExpr->arg0.getLiteralValue()->unsignedInt() - 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, firstExpr->arg1, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco);
        }

        // TODO
        // (a << constA) * constB = constB * (a << constA) -> a * ((1 << constA) * constB)
        // (a * const) + a = a + (a * const) = (const * a) + a = a + (const * a) -> a * (const + 1)
    }

    // in any other case, just build an expression tree
    if((firstExpr && !arg0.checkExpression()) || (secondExpr && !arg1.checkExpression()))
    {
        // there is a first/second expression, which was not set before (e.g. set via the input), set is as first/second
        // expression
        SubExpression newFirst = arg0;
        SubExpression newSecond = arg1;
        if(firstExpr)
            newFirst = SubExpression{firstExpr};
        if(secondExpr)
            newSecond = SubExpression{secondExpr};
        return std::make_shared<Expression>(code, newFirst, newSecond, unpackMode, packMode, deco);
    }

    return shared_from_this();
}

static bool isUnsigned(const SubExpression& sub)
{
    if(auto expr = sub.checkExpression())
        return intermediate::isGroupBuiltin(expr->deco, true) ||
            has_flag(expr->deco, intermediate::InstructionDecorations::UNSIGNED_RESULT);
    if(auto val = sub.checkValue())
        return val->isUnsignedInteger();
    return false;
}

Optional<Value> Expression::getConvergenceLimit(Optional<Literal> initialValue) const
{
    if(auto constant = getConstantExpression())
        return constant;
    if(arg1 && arg1.checkLocal() && arg0.checkLocal() && arg0.checkLocal() != arg1.checkLocal())
        // multiple different locals
        return NO_VALUE;
    bool leftIsLocal = arg0.checkLocal();
    bool rightIsLocal = arg1 && arg1.checkLocal();
    // TODO make use of
    bool leftIsUnsigned = isUnsigned(arg0);
    bool rightIsUnsigned = isUnsigned(arg1);
    auto firstVal = arg0.checkValue();
    auto secondVal = arg1.checkValue();
    Optional<Literal> constantLiteral = ((firstVal ? firstVal->getConstantValue() : NO_VALUE) |
                                            (secondVal ? secondVal->getConstantValue() : NO_VALUE)) &
        &Value::getLiteralValue;

    switch(code.opAdd)
    {
    case OP_FADD.opAdd:
        if(leftIsLocal && rightIsLocal && initialValue)
            // a + a = a * 2 -> +-inf
            return initialValue->real() == 0.0f ? NO_VALUE : (initialValue->real() < 0.0f ? FLOAT_NEG_INF : FLOAT_INF);
        if((leftIsLocal || rightIsLocal) && constantLiteral)
            // a + 0 = 0 + a -> a
            // a + x = x + a -> +-inf
            return constantLiteral->real() == 0.0f ? (initialValue ? Value(*initialValue, TYPE_FLOAT) : NO_VALUE) :
                                                     (constantLiteral->real() < 0.0f ? FLOAT_NEG_INF : FLOAT_INF);
        return NO_VALUE;
    case OP_FSUB.opAdd:
        if(leftIsLocal && rightIsLocal)
            // a - a = 0, 0 - 0 = 0, ...
            return FLOAT_ZERO;
        if(leftIsLocal && constantLiteral)
            // a - 0 -> a
            // a - x -> +-inf
            return constantLiteral->real() == 0.0f ? (initialValue ? Value(*initialValue, TYPE_FLOAT) : NO_VALUE) :
                                                     (constantLiteral->real() < 0.0f ? FLOAT_INF : FLOAT_NEG_INF);
        // x - a = (x-a), x - (x-a) = a, ...
        return NO_VALUE;
    case OP_FMIN.opAdd:
        if(leftIsLocal && rightIsLocal)
            // min(a, a) -> a
            return initialValue ? Value(*initialValue, TYPE_FLOAT) : NO_VALUE;
        if((leftIsLocal || rightIsLocal) && initialValue && constantLiteral)
            // min(a, x) -> min(a, x)
            return Value(Literal(std::min(initialValue->real(), constantLiteral->real())), TYPE_FLOAT);
        return NO_VALUE;
    case OP_FMAX.opAdd:
        if(leftIsLocal && rightIsLocal)
            // max(a, a) -> a
            return initialValue ? Value(*initialValue, TYPE_FLOAT) : NO_VALUE;
        if((leftIsLocal || rightIsLocal) && initialValue && constantLiteral)
            // max(a, x) -> max(a, x)
            return Value(Literal(std::max(initialValue->real(), constantLiteral->real())), TYPE_FLOAT);
        return NO_VALUE;
    case OP_FMINABS.opAdd:
        if(leftIsLocal && rightIsLocal)
            // minabs(a, a) -> abs(a)
            return initialValue ? Value(Literal(std::abs(initialValue->real())), TYPE_FLOAT) : NO_VALUE;
        if((leftIsLocal || rightIsLocal) && initialValue && constantLiteral)
            // minabs(a, x) -> min(abs(a), abs(x))
            return Value(
                Literal(std::min(std::abs(initialValue->real()), std::abs(constantLiteral->real()))), TYPE_FLOAT);
        return NO_VALUE;
    case OP_FMAXABS.opAdd:
        if(leftIsLocal && rightIsLocal)
            // maxabs(a, a) -> abs(a)
            return initialValue ? Value(Literal(std::abs(initialValue->real())), TYPE_FLOAT) : NO_VALUE;
        if((leftIsLocal || rightIsLocal) && initialValue && constantLiteral)
            // maxabs(a, x) -> max(abs(a), abs(x))
            return Value(
                Literal(std::max(std::abs(initialValue->real()), std::abs(constantLiteral->real()))), TYPE_FLOAT);
        return NO_VALUE;
    case OP_ADD.opAdd:
        if(leftIsLocal && rightIsLocal && initialValue)
            // a + a = a * 2 -> INT_MIN/INT_MAX
            return initialValue->signedInt() == 0 ?
                INT_ZERO :
                (initialValue->signedInt() < 0 ? Value(Literal(std::numeric_limits<int32_t>::min()), TYPE_INT32) :
                                                 Value(Literal(std::numeric_limits<int32_t>::max()), TYPE_INT32));
        if((leftIsLocal || rightIsLocal) && constantLiteral)
            // a + 0 = 0 + a -> a
            // a + x = x + a -> INT_MIN/INT_MAX
            return constantLiteral->signedInt() == 0 ?
                (initialValue ? Value(*initialValue, TYPE_INT32) : NO_VALUE) :
                (constantLiteral->signedInt() < 0 ? Value(Literal(std::numeric_limits<int32_t>::min()), TYPE_INT32) :
                                                    Value(Literal(std::numeric_limits<int32_t>::max()), TYPE_INT32));
        return NO_VALUE;
    case OP_SUB.opAdd:
        if(leftIsLocal && rightIsLocal)
            // a - a -> 0
            return INT_ZERO;
        if(leftIsLocal && constantLiteral)
            // a - 0 -> a
            // a - x -> INT_MIN/INT_MAX
            return constantLiteral->signedInt() == 0 ?
                (initialValue ? Value(*initialValue, TYPE_INT32) : NO_VALUE) :
                (constantLiteral->signedInt() < 0 ? Value(Literal(std::numeric_limits<int32_t>::max()), TYPE_INT32) :
                                                    Value(Literal(std::numeric_limits<int32_t>::min()), TYPE_INT32));
        return NO_VALUE;
    case OP_SHR.opAdd:
        if(leftIsLocal && !rightIsLocal && constantLiteral)
            // a >> 0 -> a
            // a >> x -> 0
            return constantLiteral->unsignedInt() == 0 ? (initialValue ? Value(*initialValue, TYPE_INT32) : NO_VALUE) :
                                                         INT_ZERO;
        if(leftIsLocal && rightIsLocal && initialValue && initialValue->unsignedInt() == 0)
            // 0 >> 0 -> 0
            return INT_ZERO;
        // a >> (a % 32), x >> (a % 32) -> ? (since a could be != 0, but (a % 32) is 0)
        return NO_VALUE;
    case OP_ASR.opAdd:
        if(leftIsLocal && rightIsLocal && initialValue)
            // -1 >> -1 (%32) -> -1
            // 0 >> 0 -> 0
            return initialValue->signedInt() == 0 ? INT_ZERO :
                                                    (initialValue->signedInt() == -1 ? INT_MINUS_ONE : NO_VALUE);
        if(leftIsLocal && !rightIsLocal && constantLiteral && initialValue)
            // a >> 0 -> a
            // a >> x -> 0/-1
            return constantLiteral->unsignedInt() == 0 ? Value(*initialValue, TYPE_INT32) :
                                                         initialValue->signedInt() < 0 ? INT_MINUS_ONE : INT_ZERO;
        return NO_VALUE;
    case OP_SHL.opAdd:
        if(leftIsLocal && initialValue)
            // 0 << x -> 0
            // a << 0 -> a
            return (initialValue->unsignedInt() == 0) ?
                INT_ZERO :
                (constantLiteral && constantLiteral->unsignedInt() == 0 ? Value(*initialValue, TYPE_INT32) : NO_VALUE);
        if(rightIsLocal && constantLiteral && constantLiteral->unsignedInt() == 0)
            // 0 << a -> 0
            return INT_ZERO;
        return NO_VALUE;
    case OP_MIN.opAdd:
        if(leftIsLocal && rightIsLocal)
            // min(a, a) -> a
            return initialValue ? Value(*initialValue, TYPE_INT32) : NO_VALUE;
        if((leftIsLocal || rightIsLocal) && initialValue && constantLiteral)
            // min(a, x) -> min(a, x)
            return Value(Literal(std::min(initialValue->signedInt(), constantLiteral->signedInt())), TYPE_INT32);
        return NO_VALUE;
    case OP_MAX.opAdd:
        if(leftIsLocal && rightIsLocal)
            // max(a, a) -> a
            return initialValue ? Value(*initialValue, TYPE_INT32) : NO_VALUE;
        if((leftIsLocal || rightIsLocal) && initialValue && constantLiteral)
            // max(a, x) -> max(a, x)
            return Value(Literal(std::max(initialValue->signedInt(), constantLiteral->signedInt())), TYPE_INT32);
        return NO_VALUE;
    }
    if(code.opMul == FAKEOP_UMUL.opMul)
    {
        if(constantLiteral == Literal(0u))
            // a * 0 -> 0
            return INT_ZERO;
        if(leftIsLocal && rightIsLocal)
            // 0 * 0 -> 0
            // a * a -> INT_MAX
            return initialValue == Literal(0u) ? INT_ZERO :
                                                 Value(Literal(std::numeric_limits<uint32_t>::max()), TYPE_INT32);
        return NO_VALUE;
    }
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Unhandled convergence limit calculation for: " << to_string() << logging::endl);
    return NO_VALUE;
}

intermediate::IntermediateInstruction* Expression::toInstruction(const Value& output) const
{
    if(code.opAdd > 32 || code.opMul > 32)
        // check for fake opcodes
        return nullptr;

    intermediate::IntermediateInstruction* inst;
    auto firstVal = arg0.checkValue();
    auto secondVal = arg1.checkValue();
    if(firstVal && code.numOperands == 1)
        inst = new intermediate::Operation(code, output, *firstVal);
    else if(firstVal && secondVal)
        inst = new intermediate::Operation(code, output, *firstVal, *secondVal);
    else
        // if either input is an expression, there is no instructions generating the values (and we don't know the local
        // to read), so we cannot generate the instruction.
        return nullptr;
    inst->setUnpackMode(unpackMode);
    inst->setPackMode(packMode);
    inst->addDecorations(deco);
    return inst;
}

bool Expression::insertInstructions(
    InstructionWalker& it, const Value& out, const AvailableExpressions& existingExpressions) const
{
    if(code.opAdd > 32 || code.opMul > 32)
        // check for fake opcodes
        return false;

    if(existingExpressions.find(std::const_pointer_cast<Expression>(shared_from_this())) != existingExpressions.end())
        // nothing to do
        return false;

    if(isMoveExpression() && arg0.checkExpression())
        // to not insert moves, but the moved-from data
        return arg0.checkExpression()->insertInstructions(it, out, existingExpressions);

    // TODO this is only 0-level recursive, since we do not have the right locals for intermediate results

    auto leftVal = arg0.checkValue();
    if(auto leftExpr = arg0.checkExpression())
    {
        auto it = existingExpressions.find(leftExpr);
        if(it == existingExpressions.end())
            return false;
        leftVal = it->second.first->getOutput();
    }

    auto rightVal = arg0.checkValue();
    if(auto rightExpr = arg0.checkExpression())
    {
        auto it = existingExpressions.find(rightExpr);
        if(it == existingExpressions.end())
            return false;
        rightVal = it->second.first->getOutput();
    }

    if(!leftVal || (code.numOperands > 1 && !rightVal))
        return false;

    it.emplace(Expression{code, *leftVal, rightVal, unpackMode, packMode, deco}.toInstruction(out));
    it.nextInBlock();
    return true;
}

size_t std::hash<vc4c::SubExpression>::operator()(const vc4c::SubExpression& expr) const noexcept
{
    std::hash<SubExpression::Base> baseHash;
    return baseHash(expr);
}

size_t std::hash<vc4c::Expression>::operator()(const vc4c::Expression& expr) const noexcept
{
    hash<const char*> nameHash;
    hash<SubExpression> valHash;

    return nameHash(expr.code.name) ^ valHash(expr.arg0) ^ (expr.arg1 ? valHash(expr.arg1) : 0) ^
        expr.unpackMode.value ^ expr.packMode.value ^ static_cast<unsigned>(expr.deco);
}

std::shared_ptr<Expression> operators::ExpressionWrapper::operator=(OperationWrapper&& op) &&
{
    if(op.signal.hasSideEffects() || op.setFlags == SetFlag::SET_FLAGS)
        throw CompilationError(CompilationStep::GENERAL, "Expressions cannot have side-effects!");
    return std::make_shared<Expression>(
        op.op, std::move(op.arg0), std::move(op.arg1), op.unpackMode, op.packMode, op.decoration);
}
