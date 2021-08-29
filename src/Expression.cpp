#include "Expression.h"

#include "InstructionWalker.h"
#include "Profiler.h"
#include "analysis/DebugGraph.h"
#include "intermediate/Helper.h"
#include "intermediate/operators.h"
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
    if(auto lit = getLiteralValue())
        // make expressions with same literal value (even if one is literal and other is small immediate) compare equal
        return lit == other.getLiteralValue();
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

const Local* SubExpression::checkLocal(bool includeExpressionOutput) const
{
    if(auto loc = checkLocal())
        return loc;
    if(auto expr = checkExpression())
        return expr->outputValue;
    return nullptr;
}

static bool isWorkGroupUniform(const SubExpression& exp)
{
    auto expr = exp.checkExpression();
    auto local = exp.checkLocal();
    auto builtin = local ? local->as<BuiltinLocal>() : nullptr;
    return (expr && has_flag(expr->deco, intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE)) ||
        (builtin && builtin->isWorkGroupUniform()) || exp.isConstant();
}

static void updateWorkGroupUniformDecoration(Expression& expr)
{
    if(isWorkGroupUniform(expr.arg0) && (expr.code.numOperands == 1 || isWorkGroupUniform(expr.arg1)))
        expr.addDecorations(intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
}

Expression::Expression(const OpCode& op, const SubExpression& first, const SubExpression& second, Unpack unpack,
    Pack pack, intermediate::InstructionDecorations decorations, const Local* outLoc) :
    code(op),
    arg0(first), arg1(second), unpackMode(unpack), packMode(pack), deco(decorations), outputValue(outLoc)
{
    updateWorkGroupUniformDecoration(*this);
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
    if(instr.getVectorRotation())
        // skip vector rotations
        return {};
    if(dynamic_cast<const intermediate::LoadImmediate*>(&instr) &&
        dynamic_cast<const intermediate::LoadImmediate*>(&instr)->type != intermediate::LoadType::REPLICATE_INT32)
        // skip loading of masked values
        return {};

    auto code = dynamic_cast<const intermediate::Operation*>(&instr) != nullptr ?
        dynamic_cast<const intermediate::Operation&>(instr).op :
        OP_V8MIN;
    auto unpackMode = (check(dynamic_cast<const intermediate::UnpackingInstruction*>(&instr)) &
        &intermediate::UnpackingInstruction::getUnpackMode)
                          .value_or(UNPACK_NOP);
    auto packMode = (check(dynamic_cast<const intermediate::ExtendedInstruction*>(&instr)) &
        &intermediate::ExtendedInstruction::getPackMode)
                        .value_or(PACK_NOP);
    return std::make_shared<Expression>(code, instr.getArgument(0).value(),
        instr.getArgument(1) ? instr.getArgument(1) : (code == OP_V8MIN ? instr.getArgument(0) : NO_VALUE), unpackMode,
        packMode, instr.decoration, instr.checkOutputLocal());
}

static std::shared_ptr<Expression> createRecursiveExpressionInner(const intermediate::IntermediateInstruction& instr,
    unsigned maxDepth, FastMap<const Local*, std::shared_ptr<Expression>>& parentsCache, ExpressionOptions options)
{
    bool stopAtBuiltin = has_flag(options, ExpressionOptions::STOP_AT_BUILTINS);
    if(auto expr = Expression::createExpression(instr))
    {
        if(auto constantValue = expr->getConstantExpression())
            expr = std::make_shared<Expression>(OP_V8MIN, *constantValue, *constantValue, expr->unpackMode,
                expr->packMode, expr->deco, expr->outputValue);
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
    PROFILE_SCOPE(createRecursiveExpression);
    FastMap<const Local*, std::shared_ptr<Expression>> parentsCache;
    auto exp = createRecursiveExpressionInner(instr, maxDepth, parentsCache, options);
    return exp;
}

bool Expression::operator==(const Expression& other) const
{
    return code == other.code &&
        ((arg0 == other.arg0 && arg1 == other.arg1) ||
            ((code.isCommutative() || code == FAKEOP_UMUL) && arg0 == other.arg1 && arg1 == other.arg0)) &&
        unpackMode == other.unpackMode && packMode == other.packMode && deco == other.deco;
}

LCOV_EXCL_START
static std::string toExtraString(
    Unpack unpackMode, Pack packMode, intermediate::InstructionDecorations deco, const Local* output)
{
    std::string extra;
    extra += unpackMode.hasEffect() ? unpackMode.to_string() + " " : "";
    extra += packMode.hasEffect() ? packMode.to_string() + " " : "";
    extra += intermediate::toString(deco);
    extra += output ? (" " + output->name) : "";
    return extra;
}

std::string Expression::to_string() const
{
    auto basic =
        std::string(code.name) + " " + arg0.to_string() + (code.numOperands > 1 ? ", " + arg1.to_string() : "");
    auto extra = toExtraString(unpackMode, packMode, deco, outputValue);
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
        auto firstSourceVal = intermediate::getSourceValue(*firstVal);
        auto secondSourceVal = secondVal ? intermediate::getSourceValue(*secondVal) : secondVal;
        if(secondSourceVal && code.opMul == FAKEOP_UMUL.opMul)
        {
            auto firstLit = firstSourceVal.getLiteralValue();
            auto secondLit = secondSourceVal->getLiteralValue();
            if(firstLit && secondLit)
                return Value(Literal(firstLit->unsignedInt() * secondLit->unsignedInt()), TYPE_INT32);
        }
        else
            return code(firstSourceVal, secondSourceVal).first;
    }
    return NO_VALUE;
}

bool Expression::hasConstantOperand() const
{
    return arg0.isConstant() || arg1.isConstant();
}

static Optional<Value> calculate(const OpCode& code, const Optional<Value>& firstArg, const Optional<Value>& secondArg)
{
    if(code == Expression::FAKEOP_UMUL && firstArg && secondArg)
    {
        auto firstSourceLit = intermediate::getSourceValue(*firstArg).getLiteralValue();
        auto secondSourceLit = intermediate::getSourceValue(*secondArg).getLiteralValue();
        if(firstSourceLit && secondSourceLit)
            return Value(Literal(firstSourceLit->unsignedInt() * secondSourceLit->unsignedInt()), TYPE_INT32);
        else
            return NO_VALUE;
    }
    if(firstArg)
    {
        auto firstSourceVal = intermediate::getSourceValue(*firstArg);
        auto secondSourceVal = secondArg ? intermediate::getSourceValue(*secondArg) : secondArg;
        return code(firstSourceVal, secondSourceVal).first;
    }
    return NO_VALUE;
}

// need special check, since Values with Literal and SmallImmediates do not compare equal
static bool hasValue(const Optional<Value>& val, Optional<Literal> lit)
{
    return val && lit && val->hasLiteral(*lit);
}

static std::shared_ptr<Expression> combineIfRecursive(const std::shared_ptr<Expression>& expression,
    const FastMap<const Local*, std::shared_ptr<Expression>>& inputs, ExpressionOptions options)
{
    if(has_flag(options, ExpressionOptions::RECURSIVE))
        return expression->combineWith(inputs, options);
    return expression;
}

static const Local* eitherOutput(const Local* one, const Local* other)
{
    return one ? one : other;
}

static std::shared_ptr<Expression> addOutput(std::shared_ptr<Expression>&& expr, const Local* loc)
{
    expr->outputValue = eitherOutput(expr->outputValue, loc);
    return std::move(expr);
}

static std::shared_ptr<Expression> combineWithInner(
    Expression& expression, const FastMap<const Local*, std::shared_ptr<Expression>>& inputs, ExpressionOptions options)
{
    const auto& FAKEOP_UMUL = Expression::FAKEOP_UMUL;
    bool allowFakeOperations = has_flag(options, ExpressionOptions::ALLOW_FAKE_OPS);
    bool stopAtBuiltin = has_flag(options, ExpressionOptions::STOP_AT_BUILTINS);

    auto& code = expression.code;
    auto& deco = expression.deco;
    auto& arg0 = expression.arg0;
    auto& arg1 = expression.arg1;
    auto firstVal = arg0.checkValue();
    auto secondVal = arg1.checkValue();
    auto firstExpr = arg0.checkExpression();
    auto secondExpr = arg1.checkExpression();
    auto outputValue = expression.outputValue;

    if(stopAtBuiltin && intermediate::isGroupBuiltin(deco, true))
        return expression.shared_from_this();

    if(auto firstLoc = arg0.checkLocal())
    {
        auto it = inputs.find(firstLoc);
        if(it != inputs.end())
        {
            firstVal = NO_VALUE;
            firstExpr = it->second;
            arg0 = firstExpr;
            updateWorkGroupUniformDecoration(expression);
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
            updateWorkGroupUniformDecoration(expression);
        }
    }

    // replace move expressions by their sources, if possible
    if(firstExpr && firstExpr->isMoveExpression())
    {
        auto loc = firstExpr->arg0.checkLocal();
        if(loc && inputs.find(loc) != inputs.end())
        {
            // replace left expression by source
            firstVal = NO_VALUE;
            firstExpr = inputs.at(loc);
            arg0 = firstExpr;
        }
        else
        {
            arg0 = firstExpr->arg0;
            firstVal = arg0.checkValue();
            firstExpr = arg0.checkExpression();
        }
        updateWorkGroupUniformDecoration(expression);
    }

    if(secondExpr && secondExpr->isMoveExpression())
    {
        auto loc = secondExpr->arg0.checkLocal();
        if(loc && inputs.find(loc) != inputs.end())
        {
            // replace right expression by source
            secondVal = NO_VALUE;
            secondExpr = inputs.at(loc);
            arg1 = secondExpr;
        }
        else
        {
            arg1 = secondExpr->arg0;
            secondVal = arg1.checkValue();
            secondExpr = arg1.checkExpression();
        }
        updateWorkGroupUniformDecoration(expression);
    }

    if(stopAtBuiltin && firstExpr && intermediate::isGroupBuiltin(firstExpr->deco, true))
        firstExpr = nullptr;
    if(stopAtBuiltin && secondExpr && intermediate::isGroupBuiltin(secondExpr->deco, true))
        secondExpr = nullptr;

    if(expression.unpackMode.hasEffect() || expression.packMode.hasEffect() ||
        (firstExpr != nullptr && (firstExpr->unpackMode.hasEffect() || firstExpr->packMode.hasEffect())) ||
        ((secondExpr != nullptr && (secondExpr->unpackMode.hasEffect() || secondExpr->packMode.hasEffect()))))
        // cannot combine pack modes
        return expression.shared_from_this();

    // "replace" this with source expression
    if(expression.isMoveExpression())
    {
        if(firstExpr)
            return firstExpr->combineWith(inputs, options);
        if(secondExpr)
            return secondExpr->combineWith(inputs, options);
        return expression.shared_from_this();
    }

    if(code.numOperands == 1 && firstExpr != nullptr)
    {
        if(code.isIdempotent() && firstExpr->code == code)
            // f(f(a)) = f(a)
            // XXX there are no single-argument idempotent operations
            return std::make_shared<Expression>(code, firstExpr->arg0, NO_VALUE, UNPACK_NOP, PACK_NOP,
                add_flag(deco, firstExpr->deco), eitherOutput(outputValue, firstExpr->outputValue));
        // NOTE: ftoi(itof(i)) != i, itof(ftoi(f)) != f, since the truncation/rounding would get lost!
        if(code.isSelfInverse() && code == firstExpr->code)
        {
            // f(f(a)) = a, e.g. not(not(a)) = a
            auto inner = firstExpr->arg0.checkExpression();
            return inner ? inner->addDecorations(deco).shared_from_this() :
                           std::make_shared<Expression>(OP_V8MIN, firstExpr->arg0, firstExpr->arg0, UNPACK_NOP,
                               PACK_NOP, add_flag(deco, firstExpr->deco), outputValue);
        }
    }

    if(code.numOperands == 2)
    {
        if(code.isIdempotent() && arg0 == arg1)
            // f(a, a) = a
            // XXX this is actually hidden by the isMoveExpression() check above
            return addOutput(firstExpr ? firstExpr->addDecorations(deco).shared_from_this() :
                                         std::make_shared<Expression>(OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco),
                outputValue);
        if(code.isSelfInverse() && arg0 == arg1)
        {
            // f(a, a) = 0, e.g. a - a = 0
            auto zero = code.returnsFloat ? FLOAT_ZERO : INT_ZERO;
            return std::make_shared<Expression>(OP_V8MIN, zero, zero, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        if(firstVal &&
            (hasValue(firstVal, code.getLeftIdentity() & &Value::getLiteralValue) ||
                (code == FAKEOP_UMUL && hasValue(firstVal, Literal(1)))))
        {
            // f(id, a) = a
            if(!secondExpr)
                // we disable the expression above to not combine group uniform values, but here we can still replace
                // the current value with a work-group uniform expression
                secondExpr = arg1.checkExpression();
            return addOutput(secondExpr ?
                    secondExpr->addDecorations(deco).shared_from_this() :
                    std::make_shared<Expression>(OP_V8MIN, arg1, arg1, UNPACK_NOP, PACK_NOP, deco),
                outputValue);
        }
        if(secondVal &&
            (hasValue(secondVal, code.getRightIdentity() & &Value::getLiteralValue) ||
                (code == FAKEOP_UMUL && hasValue(secondVal, Literal(1)))))
        {
            // f(a, id) = a
            if(!firstExpr)
                // we disable the expression above to not combine group uniform values, but here we can still replace
                // the current value with a work-group uniform expression
                firstExpr = arg0.checkExpression();
            return addOutput(firstExpr ? firstExpr->addDecorations(deco).shared_from_this() :
                                         std::make_shared<Expression>(OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco),
                outputValue);
        }
        if(firstVal &&
            (hasValue(firstVal, code.getLeftAbsorbingElement() & &Value::getLiteralValue) ||
                (code == FAKEOP_UMUL && hasValue(firstVal, Literal(0)))))
            // f(absorb, a) = absorb
            return std::make_shared<Expression>(OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco, outputValue);
        if(secondVal &&
            (hasValue(secondVal, code.getRightAbsorbingElement() & &Value::getLiteralValue) ||
                (code == FAKEOP_UMUL && hasValue(secondVal, Literal(0)))))
            // f(a, absorb) = absorb
            return std::make_shared<Expression>(OP_V8MIN, arg1, arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);

        if(firstVal && (code.isAssociative() || code == FAKEOP_UMUL) && secondExpr && secondExpr->code == code &&
            arg0.isConstant() && secondExpr->arg0.isConstant())
        {
            // f(constA, f(constB, a)) = f(f(constA, constB), a), if associative
            if(auto tmp = calculate(code, firstVal, secondExpr->arg0.checkValue()))
                return std::make_shared<Expression>(
                    code, *tmp, secondExpr->arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(secondVal && (code.isAssociative() || code == FAKEOP_UMUL) && firstExpr && firstExpr->code == code &&
            arg1.isConstant() && firstExpr->arg1.isConstant())
        {
            // f(f(a, constA), constB) = f(a, f(constA, constB)), if associative
            if(auto tmp = calculate(code, firstExpr->arg1.checkValue(), secondVal))
                return std::make_shared<Expression>(
                    code, firstExpr->arg0, tmp, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(firstVal && ((code.isAssociative() && code.isCommutative()) || code == FAKEOP_UMUL) && secondExpr &&
            secondExpr->code == code && arg0.isConstant() && secondExpr->arg1.isConstant())
        {
            // f(constA, f(a, constB)) = f(f(constA, constB), a), if associative and commutative
            if(auto tmp = calculate(code, firstVal, secondExpr->arg1.checkValue()))
                return std::make_shared<Expression>(
                    code, *tmp, secondExpr->arg0, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(secondVal && ((code.isAssociative() && code.isCommutative()) || code == FAKEOP_UMUL) && firstExpr &&
            firstExpr->code == code && arg1.isConstant() && firstExpr->arg0.isConstant())
        {
            // f(f(constA, a), constB) = f(f(constA, constB), a), if associative and commutative
            if(auto tmp = calculate(code, firstExpr->arg0.checkValue(), secondVal))
                return std::make_shared<Expression>(
                    code, *tmp, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(code.isAssociative() && code.isIdempotent() && secondExpr && secondExpr->code == code &&
            secondExpr->arg0 == arg0)
        {
            // f(a, f(a, b)) = f(a, b), if associative and idempotent
            return std::make_shared<Expression>(code, arg0, secondExpr->arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(code.isAssociative() && code.isIdempotent() && firstExpr && firstExpr->code == code &&
            firstExpr->arg1 == arg1)
        {
            // f(f(a, b), b) = f(a, b), if associative and idempotent
            return std::make_shared<Expression>(code, firstExpr->arg0, arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(code.isAssociative() && code.isIdempotent() && code.isCommutative() && secondExpr &&
            secondExpr->code == code && secondExpr->arg1 == arg0)
        {
            // f(a, f(b, a)) = f(a, b), if associative, commutative and idempotent
            return std::make_shared<Expression>(code, arg0, secondExpr->arg0, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(code.isAssociative() && code.isIdempotent() && code.isCommutative() && firstExpr &&
            firstExpr->code == code && firstExpr->arg0 == arg1)
        {
            // f(f(a, b), a) = f(a, b), if associative, commutative and idempotent
            return std::make_shared<Expression>(
                code, firstExpr->arg0, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(firstExpr && secondExpr && firstExpr->code == secondExpr->code &&
            (firstExpr->code.isLeftDistributiveOver(code) ||
                (allowFakeOperations && firstExpr->code == FAKEOP_UMUL && (code == OP_ADD || code == OP_SUB))) &&
            firstExpr->arg0 == secondExpr->arg0)
        {
            // g(f(a, b), f(a, c)) = f(a, g(b, c)), if left distributive
            if(auto tmp = calculate(code, firstExpr->arg1.checkValue(), secondExpr->arg1.checkValue()))
                return std::make_shared<Expression>(
                    firstExpr->code, firstExpr->arg0, tmp, UNPACK_NOP, PACK_NOP, deco, outputValue);
            // general non-constant case
            auto tmp = std::make_shared<Expression>(code, firstExpr->arg1, secondExpr->arg1);
            tmp = combineIfRecursive(tmp, inputs, options);
            return std::make_shared<Expression>(
                firstExpr->code, firstExpr->arg0, tmp, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(firstExpr && secondExpr && firstExpr->code == secondExpr->code &&
            (firstExpr->code.isRightDistributiveOver(code) ||
                (allowFakeOperations && firstExpr->code == FAKEOP_UMUL && (code == OP_ADD || code == OP_SUB))) &&
            firstExpr->arg1 == secondExpr->arg1)
        {
            // g(f(a, b), f(c, b)) = f(g(a, c), b), if right distributive
            if(auto tmp = calculate(code, firstExpr->arg0.checkValue(), secondExpr->arg0.checkValue()))
                return std::make_shared<Expression>(
                    firstExpr->code, *tmp, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
            // general non-constant case
            auto tmp = std::make_shared<Expression>(code, firstExpr->arg0, secondExpr->arg0);
            tmp = combineIfRecursive(tmp, inputs, options);
            return std::make_shared<Expression>(
                firstExpr->code, tmp, firstExpr->arg1, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        if(firstExpr && secondExpr && firstExpr->code == secondExpr->code &&
            ((firstExpr->code.isCommutative() && firstExpr->code.isRightDistributiveOver(code)) ||
                (allowFakeOperations && firstExpr->code == FAKEOP_UMUL && (code == OP_ADD || code == OP_SUB))) &&
            (firstExpr->arg1 == secondExpr->arg0 || firstExpr->arg0 == secondExpr->arg1))
        {
            // g(f(a, b), f(b, c)) = g(f(a, b), f(c, a)) = f(a, g(b, c)), if distributive and commutative
            auto sameArg = firstExpr->arg1 == secondExpr->arg0 ? firstExpr->arg1 : firstExpr->arg0;
            auto firstOtherArg = sameArg == firstExpr->arg0 ? firstExpr->arg1 : firstExpr->arg0;
            auto secondOtherArg = sameArg == secondExpr->arg0 ? secondExpr->arg1 : secondExpr->arg0;

            if(auto tmp = calculate(code, firstOtherArg.checkValue(), secondOtherArg.checkValue()))
                return std::make_shared<Expression>(
                    firstExpr->code, *tmp, sameArg, UNPACK_NOP, PACK_NOP, deco, outputValue);
            // general non-constant case
            auto tmp = std::make_shared<Expression>(code, firstOtherArg, secondOtherArg);
            tmp = combineIfRecursive(tmp, inputs, options);
            return std::make_shared<Expression>(firstExpr->code, tmp, sameArg, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // TODO more properties to use (e.g. distributive and commutative?)
        // TODO (a + (b * constB)) * constC => (a * constC) + (b * (constB + constC))
        // more general, if f distributive over g: g(f(a, g(b, constB)), constC) = f(g(a, constC), g(b, f(constB,
        // constC))) also if commutative, support other orders of arguments

        if(code == OP_FADD && arg0 == arg1)
        {
            // doesn't save any instruction, but utilizes mul ALU
            return std::make_shared<Expression>(
                OP_FMUL, arg0, Value(Literal(2.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // TODO generalize! E.g. for fsub
        if(code == OP_FADD && firstExpr && firstExpr->code == OP_FMUL)
        {
            if(firstExpr->arg0 == arg1 && firstExpr->arg1.getLiteralValue())
                // fadd(fmul(a, constB), a) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg1,
                    Value(Literal(firstExpr->arg1.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, firstExpr->deco), outputValue);
            if(firstExpr->arg1 == arg1 && firstExpr->arg0.getLiteralValue())
                // fadd(fmul(constB, a), a) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg1,
                    Value(Literal(firstExpr->arg0.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, firstExpr->deco), outputValue);
        }
        if(code == OP_FADD && secondExpr && secondExpr->code == OP_FMUL)
        {
            if(secondExpr->arg0 == arg0 && secondExpr->arg1.getLiteralValue())
                // fadd(a, fmul(a, constB)) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg0,
                    Value(Literal(secondExpr->arg1.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, secondExpr->deco), outputValue);
            if(secondExpr->arg1 == arg0 && secondExpr->arg0.getLiteralValue())
                // fadd(a, fmul(constB, a)) = fmul(a, constB+1)
                return std::make_shared<Expression>(OP_FMUL, arg0,
                    Value(Literal(secondExpr->arg0.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, secondExpr->deco), outputValue);
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
                return addOutput(otherExpr ?
                        otherExpr->addDecorations(deco).shared_from_this() :
                        std::make_shared<Expression>(OP_V8MIN, otherArg, otherArg, UNPACK_NOP, PACK_NOP, deco),
                    outputValue);

            // (a - b) - a = -b
            if((code == OP_SUB && firstExpr->code == OP_SUB && arg1 == firstExpr->arg0) ||
                (code == OP_FSUB && firstExpr->code == OP_FSUB && arg1 == firstExpr->arg0))
                return std::make_shared<Expression>(code, INT_ZERO, otherArg, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        if(secondExpr && secondExpr->arg1 && (arg0 == secondExpr->arg0 || arg0 == secondExpr->arg1))
        {
            // NOTE: This rewrite removes overflow since it removes two operations that could overflow!
            const auto& otherArg = arg0 == secondExpr->arg0 ? secondExpr->arg1 : secondExpr->arg0;
            auto otherExpr = otherArg.checkExpression();
            // a + (b - a) = b
            if((code == OP_ADD && secondExpr->code == OP_SUB && arg0 == secondExpr->arg1) ||
                (code == OP_FADD && secondExpr->code == OP_FSUB && arg0 == secondExpr->arg1))
                return addOutput(otherExpr ?
                        otherExpr->addDecorations(deco).shared_from_this() :
                        std::make_shared<Expression>(OP_V8MIN, otherArg, otherArg, UNPACK_NOP, PACK_NOP, deco),
                    outputValue);

            // a - (b + a) = -b
            // a - (a + b) = -b
            if((code == OP_SUB && secondExpr->code == OP_ADD) || (code == OP_FSUB && secondExpr->code == OP_FADD))
                return std::make_shared<Expression>(code, INT_ZERO, otherArg, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // (a + b) - (a + c) = (b + a) - (c + a) = b - c
        if((code == OP_SUB || code == OP_FSUB) && firstExpr && secondExpr && firstExpr->code == secondExpr->code &&
            firstExpr->code == (code == OP_SUB ? OP_ADD : OP_FADD) &&
            (firstExpr->arg0 == secondExpr->arg0 || firstExpr->arg0 == secondExpr->arg1 ||
                firstExpr->arg1 == secondExpr->arg0 || firstExpr->arg1 == secondExpr->arg1))
        {
            // NOTE: This rewrite removes overflow since it removes two operations that could overflow!
            auto matchingArg = (firstExpr->arg0 == secondExpr->arg0 || firstExpr->arg0 == secondExpr->arg1) ?
                firstExpr->arg0 :
                firstExpr->arg1;

            auto leftArg = matchingArg == firstExpr->arg0 ? firstExpr->arg1 : firstExpr->arg0;
            auto rightArg = matchingArg == secondExpr->arg0 ? secondExpr->arg1 : secondExpr->arg0;
            return std::make_shared<Expression>(code, leftArg, rightArg, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // (a << b) << c = a << (b + c)
        // (a >> b) >> c = a >> (b + c)
        if((code == OP_SHL || code == OP_SHR || code == OP_ASR) && firstExpr && firstExpr->code == code)
        {
            SubExpression offset{};
            if(auto constantOffset = calculate(OP_ADD, firstExpr->arg1.checkValue(), arg1.checkValue()))
                offset = *constantOffset;
            else
                offset =
                    combineIfRecursive(std::make_shared<Expression>(OP_ADD, firstExpr->arg1, arg1), inputs, options);
            return std::make_shared<Expression>(code, firstExpr->arg0, offset, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // (a << const) = a * (1 << const)
        if(allowFakeOperations && code == OP_SHL && secondVal & &Value::getLiteralValue)
        {
            // this step simplifies all handling of shl as multiplication with power of two for any further combinations
            auto factor = 1u << secondVal->getLiteralValue()->unsignedInt();
            return std::make_shared<Expression>(
                FAKEOP_UMUL, arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // (a * constA) + a = (constA * a) + a = a + (a * constA) = a + (constA * a) = a * (constA + 1)
        if(allowFakeOperations && code == OP_ADD && firstExpr && firstExpr->code == FAKEOP_UMUL &&
            (firstExpr->arg0 == arg1) && (firstExpr->arg1.getLiteralValue()))
        {
            auto factor = firstExpr->arg1.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32),
                UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        if(allowFakeOperations && code == OP_ADD && firstExpr && firstExpr->code == FAKEOP_UMUL && firstExpr->arg1 &&
            firstExpr->arg1 == arg1 && (firstExpr->arg0.getLiteralValue()))
        {
            auto factor = firstExpr->arg0.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(FAKEOP_UMUL, firstExpr->arg1, Value(Literal(factor), TYPE_INT32),
                UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        if(allowFakeOperations && code == OP_ADD && secondExpr && secondExpr->code == FAKEOP_UMUL &&
            (secondExpr->arg0 == arg0) && (secondExpr->arg1.getLiteralValue()))
        {
            auto factor = secondExpr->arg1.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(FAKEOP_UMUL, secondExpr->arg0, Value(Literal(factor), TYPE_INT32),
                UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        if(allowFakeOperations && code == OP_ADD && secondExpr && secondExpr->code == FAKEOP_UMUL && secondExpr->arg1 &&
            secondExpr->arg1 == arg0 && (secondExpr->arg0.getLiteralValue()))
        {
            auto factor = secondExpr->arg0.getLiteralValue()->unsignedInt() + 1u;
            return std::make_shared<Expression>(
                FAKEOP_UMUL, arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // (a * constA) - a = (constA * a) - a = a * (constA - 1)
        if(allowFakeOperations && code == OP_SUB && firstExpr && firstExpr->code == FAKEOP_UMUL &&
            (firstExpr->arg0 == arg1) && (firstExpr->arg1.getLiteralValue()))
        {
            auto factor = firstExpr->arg1.getLiteralValue()->unsignedInt() - 1u;
            return std::make_shared<Expression>(FAKEOP_UMUL, firstExpr->arg0, Value(Literal(factor), TYPE_INT32),
                UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        if(allowFakeOperations && code == OP_SUB && firstExpr && firstExpr->code == FAKEOP_UMUL && firstExpr->arg1 &&
            firstExpr->arg1 == arg1 && (firstExpr->arg0.getLiteralValue()))
        {
            auto factor = firstExpr->arg0.getLiteralValue()->unsignedInt() - 1u;
            return std::make_shared<Expression>(FAKEOP_UMUL, firstExpr->arg1, Value(Literal(factor), TYPE_INT32),
                UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // a + (a + b) = a + (b + a) = b + (2 * a)
        if(((allowFakeOperations && code == OP_ADD) || code == OP_FADD) && secondExpr && secondExpr->code == code &&
            (secondExpr->arg0 == arg0 || secondExpr->arg1 == arg0))
        {
            auto mul = std::make_shared<Expression>(
                code == OP_ADD ? FAKEOP_UMUL : OP_FMUL, Value(Literal(2), TYPE_INT8), arg0);
            mul = combineIfRecursive(mul, inputs, options);
            auto& otherArg = secondExpr->arg0 == arg0 ? secondExpr->arg1 : secondExpr->arg0;
            return std::make_shared<Expression>(code, otherArg, mul, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        // (a + b) + a = (b + a) + a = b + (2 * a)
        if(((allowFakeOperations && code == OP_ADD) || code == OP_FADD) && firstExpr && firstExpr->code == code &&
            (firstExpr->arg0 == arg1 || firstExpr->arg1 == arg1))
        {
            auto mul = std::make_shared<Expression>(
                code == OP_ADD ? FAKEOP_UMUL : OP_FMUL, Value(Literal(2), TYPE_INT8), arg1);
            mul = combineIfRecursive(mul, inputs, options);
            auto& otherArg = firstExpr->arg0 == arg1 ? firstExpr->arg1 : firstExpr->arg0;
            return std::make_shared<Expression>(code, otherArg, mul, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // a + ((a * constA) + b) = a + (b + (a * constA) = b + ((constA + 1) * a)
        auto isMulWithConstant = [op{code == OP_ADD ? FAKEOP_UMUL : OP_FMUL}](
                                     const SubExpression& sub, const SubExpression& arg) -> bool {
            auto subExpr = sub.checkExpression();
            return subExpr && subExpr->code == op && (subExpr->arg0 == arg || subExpr->arg1 == arg) &&
                (subExpr->arg0.getLiteralValue() || subExpr->arg1.getLiteralValue());
        };
        if((code == OP_FADD || (allowFakeOperations && code == OP_ADD)) && secondExpr && secondExpr->code == code &&
            (isMulWithConstant(secondExpr->arg0, arg0) || isMulWithConstant(secondExpr->arg1, arg0)))
        {
            auto innerMulOp =
                (isMulWithConstant(secondExpr->arg0, arg0) ? secondExpr->arg0 : secondExpr->arg1).checkExpression();
            auto constValue = innerMulOp->arg0 == arg0 ? innerMulOp->arg1 : innerMulOp->arg0;
            auto newLit = code == OP_FADD ? Literal(constValue.getLiteralValue()->real() + 1.0f) :
                                            Literal(constValue.getLiteralValue()->signedInt() + 1);
            Value newFactor{newLit, constValue.checkValue()->type};
            auto& otherArg = isMulWithConstant(secondExpr->arg0, arg0) ? secondExpr->arg1 : secondExpr->arg0;

            auto mul = std::make_shared<Expression>(code == OP_ADD ? FAKEOP_UMUL : OP_FMUL, newFactor, arg0);
            mul = combineIfRecursive(mul, inputs, options);
            return std::make_shared<Expression>(code, otherArg, mul, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }
        // ((a * constA) + b) + a = (b + (a * constA)) + a = b + ((constA + 1) * a)
        if((code == OP_FADD || (allowFakeOperations && code == OP_ADD)) && firstExpr && firstExpr->code == code &&
            (isMulWithConstant(firstExpr->arg0, arg1) || isMulWithConstant(firstExpr->arg1, arg1)))
        {
            auto innerMulOp =
                (isMulWithConstant(firstExpr->arg0, arg1) ? firstExpr->arg0 : firstExpr->arg1).checkExpression();
            auto constValue = innerMulOp->arg0 == arg1 ? innerMulOp->arg1 : innerMulOp->arg0;
            auto newLit = code == OP_FADD ? Literal(constValue.getLiteralValue()->real() + 1.0f) :
                                            Literal(constValue.getLiteralValue()->signedInt() + 1);
            Value newFactor{newLit, constValue.checkValue()->type};
            auto& otherArg = isMulWithConstant(firstExpr->arg0, arg1) ? firstExpr->arg1 : firstExpr->arg0;

            auto mul = std::make_shared<Expression>(code == OP_ADD ? FAKEOP_UMUL : OP_FMUL, newFactor, arg1);
            mul = combineIfRecursive(mul, inputs, options);
            return std::make_shared<Expression>(code, otherArg, mul, UNPACK_NOP, PACK_NOP, deco, outputValue);
        }

        // a - (b + c) = (a - b) - c
        // a - (b - c) = (a - b) + c
        if((code == OP_FSUB || code == OP_SUB) && secondExpr &&
            (code == OP_FSUB ? (secondExpr->code == OP_FSUB || secondExpr->code == OP_FADD) :
                               (secondExpr->code == OP_SUB || secondExpr->code == OP_ADD)))
        {
            static const SortedMap<uint8_t, OpCode> mapping = {
                {OP_FSUB.opAdd, OP_FADD}, {OP_FADD.opAdd, OP_FSUB}, {OP_SUB.opAdd, OP_ADD}, {OP_ADD.opAdd, OP_SUB}};
            auto innerOp = std::make_shared<Expression>(code, arg0, secondExpr->arg0);
            innerOp = combineIfRecursive(innerOp, inputs, options);
            return std::make_shared<Expression>(mapping.at(secondExpr->code.opAdd), innerOp, secondExpr->arg1,
                expression.unpackMode, expression.packMode, deco, outputValue);
        }
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
        return std::make_shared<Expression>(
            code, newFirst, newSecond, expression.unpackMode, expression.packMode, deco, outputValue);
    }

    return expression.shared_from_this();
}

std::shared_ptr<Expression> Expression::combineWith(
    const FastMap<const Local*, std::shared_ptr<Expression>>& inputs, ExpressionOptions options)
{
    auto prevExpression = this;
    auto tmp = combineWithInner(*this, inputs, options);
    while(has_flag(options, ExpressionOptions::RECURSIVE) && tmp.get() != prevExpression)
    {
        prevExpression = tmp.get();
        tmp = combineWithInner(*tmp, inputs, options);
    }
    return tmp;
}

static Optional<Value> getConvergenceLimit0(const SubExpression& sub)
{
    if(auto val = sub.checkValue())
        return *val;

    if(auto expr = sub.checkExpression())
        return expr->getConvergenceLimit();
    return {};
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
    // TODO make use of work-item/work-group decorations
    // TODO make use of value ranges from sub-expression??
    auto firstVal = arg0.checkValue() | getConvergenceLimit0(arg0);
    auto secondVal = arg1.checkValue() | getConvergenceLimit0(arg1);
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
                                                         (initialValue->signedInt() < 0 ? INT_MINUS_ONE : INT_ZERO);
        return NO_VALUE;
    case OP_SHL.opAdd:
        if(leftIsLocal && constantLiteral && constantLiteral->unsignedInt() == 0)
            // a << 0 -> a
            return initialValue ? Value(*initialValue, TYPE_INT32) : arg0.checkValue();
        if(leftIsLocal && initialValue)
            // 0 << x -> 0
            return (initialValue->unsignedInt() == 0) ? INT_ZERO : NO_VALUE;
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
            // const * const -> const^2
            // a * a -> INT_MAX
            return initialValue ?
                Value(Literal(initialValue->unsignedInt() * initialValue->unsignedInt()), TYPE_INT32) :
                Value(Literal(std::numeric_limits<uint32_t>::max()), TYPE_INT32);
        return NO_VALUE;
    }
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Unhandled convergence limit calculation for: " << to_string() << logging::endl);
    return NO_VALUE;
}

static bool isPowerOfTwo(const Optional<Literal>& lit)
{
    return lit && isPowerTwo(lit->unsignedInt());
}

std::unique_ptr<intermediate::IntermediateInstruction> Expression::toInstruction(const Value& output) const
{
    if(code == FAKEOP_UMUL && (isPowerOfTwo(arg0.getLiteralValue()) || isPowerOfTwo(arg1.getLiteralValue())))
    {
        // a * 2^b = a << b
        auto factorArg = isPowerOfTwo(arg0.getLiteralValue()) ? arg0 : arg1;
        auto otherArg = isPowerOfTwo(arg0.getLiteralValue()) ? arg1 : arg0;
        auto factor = static_cast<unsigned>(std::log2(factorArg.getLiteralValue().value().unsignedInt()));
        Expression tmpExpr{OP_SHL, otherArg, Value(Literal(factor), TYPE_INT32), unpackMode, packMode, deco};
        return tmpExpr.toInstruction(output);
    }
    if(code.opAdd > 32 || code.opMul > 32)
        // check for fake opcodes
        return nullptr;

    std::unique_ptr<intermediate::Operation> inst = nullptr;
    auto firstVal = arg0.checkValue();
    auto secondVal = arg1.checkValue();
    if(auto firstExp = arg0.checkExpression())
        firstVal = firstExp->outputValue ? firstExp->outputValue->createReference() : NO_VALUE;
    if(auto secondExp = arg1.checkExpression())
        secondVal = secondExp->outputValue ? secondExp->outputValue->createReference() : NO_VALUE;

    if(firstVal && code.numOperands == 1)
        inst = std::make_unique<intermediate::Operation>(code, output, *firstVal);
    else if(firstVal && secondVal)
        inst = std::make_unique<intermediate::Operation>(code, output, *firstVal, *secondVal);
    else
        // if either input is an expression (without a known output value), there is no instructions generating the
        // values (and we don't know the local to read), so we cannot generate the instruction.
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
        if(leftExpr->outputValue)
            leftVal = leftExpr->outputValue->createReference();
        else
        {
            auto exprIt = existingExpressions.find(leftExpr);
            if(exprIt == existingExpressions.end())
                return false;
            leftVal = exprIt->second.first->getOutput();
        }
    }

    auto rightVal = arg1.checkValue();
    if(auto rightExpr = arg1.checkExpression())
    {
        if(rightExpr->outputValue)
            rightVal = rightExpr->outputValue->createReference();
        else
        {
            auto exprIt = existingExpressions.find(rightExpr);
            if(exprIt == existingExpressions.end())
                return false;
            rightVal = exprIt->second.first->getOutput();
        }
    }

    if(!leftVal || (code.numOperands > 1 && !rightVal))
        return false;

    if(auto inst = Expression{code, *leftVal, rightVal, unpackMode, packMode, deco}.toInstruction(out))
    {
        it.emplace(std::move(inst));
        it.nextInBlock();
        return true;
    }
    return false;
}

using ExpressionTree = DebugGraph<const void*, empty_base, Directionality::DIRECTED>;

static void dumpExpression(const void* key, const Expression& expr, ExpressionTree& graph);

NODISCARD static const void* dumpSubExpression(const SubExpression& expr, ExpressionTree& graph)
{
    if(auto val = expr.checkValue())
        graph.addNode(&expr, val.to_string());
    else if(auto exp = expr.checkExpression())
        dumpExpression(&expr, *exp, graph);
    else
        graph.addNode(&expr, "-");
    return &expr;
}

static void dumpExpression(const void* key, const Expression& expr, ExpressionTree& graph)
{
    std::string label = expr.code.name;
    auto extra = toExtraString(expr.unpackMode, expr.packMode, expr.deco, expr.outputValue);
    label += (extra.empty() ? "" : (" (" + extra + ')'));
    graph.addNode(key, label);

    auto subId = dumpSubExpression(expr.arg0, graph);
    graph.addEdge(key, subId, false, "", Direction::FIRST_TO_SECOND);
    if(expr.code.numOperands > 1)
    {
        subId = dumpSubExpression(expr.arg1, graph);
        graph.addEdge(key, subId, false, "", Direction::FIRST_TO_SECOND);
    }
}

void Expression::dumpTree(const std::string& path) const
{
    ExpressionTree graph{path, 8};
    dumpExpression(this, *this, graph);
}

static std::pair<SubExpression, SubExpression> toSubExpressionParts(
    const SubExpression& subExpr, bool workGroupUniformIsConstant, ExpressionOptions options)
{
    if(auto expr = subExpr.checkExpression())
        return expr->splitIntoDynamicAndConstantPart(workGroupUniformIsConstant, options);
    if(auto val = subExpr.checkValue())
    {
        if(auto constantVal = val->getConstantValue())
            return std::make_pair(SubExpression{INT_ZERO}, SubExpression{*constantVal});
        auto loc = val->checkLocal();
        if(workGroupUniformIsConstant && loc)
        {
            bool hasWriters = false;
            bool allWritersAreWorkGroupUniform =
                loc->allUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) -> bool {
                    hasWriters = true;
                    return writer &&
                        writer->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
                });
            if(loc->is<Parameter>() || (hasWriters && allWritersAreWorkGroupUniform))
                return std::make_pair(SubExpression{INT_ZERO}, SubExpression{*val});
        }
    }
    return std::make_pair(subExpr, SubExpression{INT_ZERO});
}

static SubExpression precalculateExpression(const std::shared_ptr<Expression>& expr)
{
    if(auto constVal = expr->getConstantExpression())
        return *constVal;
    return expr;
}

std::pair<SubExpression, SubExpression> Expression::splitIntoDynamicAndConstantPart(
    bool workGroupUniformIsConstant, ExpressionOptions options)
{
    if(unpackMode.hasEffect() || packMode.hasEffect())
        return std::make_pair(SubExpression{shared_from_this()}, SubExpression{INT_ZERO});

    if(auto constant = getConstantExpression())
        return std::make_pair(SubExpression{INT_ZERO}, SubExpression{*constant});

    if(workGroupUniformIsConstant && has_flag(deco, intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))
        // this also handles work-group-uniform built-ins (e.g. group_id, etc.)...
        return std::make_pair(SubExpression{INT_ZERO}, SubExpression{shared_from_this()});
    if(has_flag(options, ExpressionOptions::STOP_AT_BUILTINS) && intermediate::isGroupBuiltin(deco, true))
        // ... so this leaves only the non-uniform built-ins
        return std::make_pair(SubExpression{shared_from_this()}, SubExpression{INT_ZERO});

    auto leftParts = toSubExpressionParts(arg0, workGroupUniformIsConstant, options);
    auto rightParts = toSubExpressionParts(arg1, workGroupUniformIsConstant, options);

    if(isMoveExpression())
        return leftParts;

    if(code.numOperands == 1)
        // XXX ftoi and itof can be split up for dynamic/constant parts
        return std::make_pair(SubExpression{shared_from_this()}, SubExpression{INT_ZERO});

    if(code.numOperands == 2)
    {
        // add associativity: (dynA + constA) + (dynB + constB) -> (dynA + dynB) + (constA + constB)
        if(code == OP_ADD)
        {
            auto dynParts =
                std::make_shared<Expression>(code, leftParts.first, rightParts.first)->combineWith({}, options);
            auto constParts =
                std::make_shared<Expression>(code, leftParts.second, rightParts.second)->combineWith({}, options);
            return std::make_pair(
                SubExpression{precalculateExpression(dynParts)}, SubExpression{precalculateExpression(constParts)});
        }

        // mul distributivity over add:
        // (dyn + const) * factor = factor * (dyn + const)
        // if factor only constant: (dyn * constFactor) + (const * constFactor)
        if(code == FAKEOP_UMUL && rightParts.first.checkValue() == INT_ZERO &&
            rightParts.second.checkValue() != INT_ZERO)
        {
            auto dynParts =
                std::make_shared<Expression>(code, leftParts.first, rightParts.second)->combineWith({}, options);
            auto constParts =
                std::make_shared<Expression>(code, leftParts.second, rightParts.second)->combineWith({}, options);
            return std::make_pair(
                SubExpression{precalculateExpression(dynParts)}, SubExpression{precalculateExpression(constParts)});
        }
        if(code == FAKEOP_UMUL && leftParts.first.checkValue() == INT_ZERO && leftParts.second.checkValue() != INT_ZERO)
        {
            auto dynParts =
                std::make_shared<Expression>(code, leftParts.second, rightParts.first)->combineWith({}, options);
            auto constParts =
                std::make_shared<Expression>(code, leftParts.second, rightParts.second)->combineWith({}, options);
            return std::make_pair(
                SubExpression{precalculateExpression(dynParts)}, SubExpression{precalculateExpression(constParts)});
        }

        // shift distributivity over add: (dyn + const) << offset -> (dyn << offset) + (const << offset)
        if((code == OP_SHL || code == OP_SHR) && rightParts.first.checkValue() == INT_ZERO)
        {
            auto dynParts =
                std::make_shared<Expression>(code, leftParts.first, rightParts.second)->combineWith({}, options);
            auto constParts =
                std::make_shared<Expression>(code, leftParts.second, rightParts.second)->combineWith({}, options);
            return std::make_pair(
                SubExpression{precalculateExpression(dynParts)}, SubExpression{precalculateExpression(constParts)});
        }
    }
    return std::make_pair(SubExpression{shared_from_this()}, SubExpression{INT_ZERO});
}

std::vector<SubExpression> Expression::getAssociativeParts() const
{
    if(!code.isAssociative() && code != FAKEOP_UMUL)
        return {};
    if(unpackMode.hasEffect() || packMode.hasEffect())
        return {};

    std::vector<SubExpression> result;

    auto leftExpr = arg0.checkExpression();
    if(leftExpr && leftExpr->code == code)
    {
        // (a op b) op c = a op b op c
        auto leftParts = leftExpr->getAssociativeParts();
        result.reserve(result.size() + leftParts.size());
        std::move(leftParts.begin(), leftParts.end(), std::back_inserter(result));
    }
    else
        result.emplace_back(arg0);

    auto rightExpr = arg1.checkExpression();
    if(rightExpr && rightExpr->code == code)
    {
        // a op (b op c) = a op b op c
        auto rightParts = rightExpr->getAssociativeParts();
        result.reserve(result.size() + rightParts.size());
        std::move(rightParts.begin(), rightParts.end(), std::back_inserter(result));
    }
    else
        result.emplace_back(arg1);

    return result;
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
