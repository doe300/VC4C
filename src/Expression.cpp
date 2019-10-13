#include "Expression.h"

#include "log.h"

using namespace vc4c;

constexpr OpCode Expression::FAKEOP_UMUL;

Optional<Expression> Expression::createExpression(const intermediate::IntermediateInstruction& instr)
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
    return Expression{code, instr.getArgument(0).value(),
        instr.getArgument(1) ? instr.getArgument(1) : code == OP_V8MIN ? instr.getArgument(0) : NO_VALUE,
        instr.unpackMode, instr.packMode, instr.decoration};
}

static Optional<Expression> createRecursiveExpressionInner(const intermediate::IntermediateInstruction& instr,
    unsigned maxDepth, FastMap<const Local*, Expression>& parentsCache, bool allowFakeOperations)
{
    if(auto expr = Expression::createExpression(instr))
    {
        if(maxDepth > 0)
        {
            for(auto& arg : instr.getArguments())
            {
                if(auto writer = arg.getSingleWriter())
                {
                    if(parentsCache.find(arg.local()) != parentsCache.end())
                        continue;
                    if(auto parentExpr =
                            createRecursiveExpressionInner(*writer, maxDepth - 1, parentsCache, allowFakeOperations))
                        parentsCache.emplace(arg.local(), *std::move(parentExpr));
                }
            }
            return expr->combineWith(parentsCache, allowFakeOperations);
        }
        return expr;
    }
    return {};
}

Optional<Expression> Expression::createRecursiveExpression(
    const intermediate::IntermediateInstruction& instr, unsigned maxDepth, bool allowFakeOperations)
{
    FastMap<const Local*, Expression> parentsCache;
    return createRecursiveExpressionInner(instr, maxDepth, parentsCache, allowFakeOperations);
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
    auto basic =
        std::string(code.name) + " " + arg0.to_string() + (arg1 ? std::string(", ") + arg1.to_string() : std::string{});
    std::string extra;
    extra += unpackMode.hasEffect() ? unpackMode.to_string() + " " : "";
    extra += packMode.hasEffect() ? packMode.to_string() + " " : "";
    extra += intermediate::toString(deco);
    return basic + (extra.empty() ? "" : (" (" + extra + ')'));
}
LCOV_EXCL_STOP

bool Expression::isMoveExpression() const
{
    return code.numOperands == 2 && code.isIdempotent() && arg1 == arg0;
}

Optional<Value> Expression::getConstantExpression() const
{
    return code(arg0, arg1).first;
}

static bool isConstantOperand(const Value& op)
{
    return op.getLiteralValue() || op.checkVector();
}

static bool isConstantOperand(const Optional<Value>& op)
{
    return op && isConstantOperand(*op);
}

bool Expression::hasConstantOperand() const
{
    return isConstantOperand(arg0) || isConstantOperand(arg1);
}

Expression Expression::combineWith(const FastMap<const Local*, Expression>& inputs, bool allowFakeOperations) const
{
    const Expression* expr0 = nullptr;
    const Expression* expr1 = nullptr;
    if(arg0.checkLocal() && inputs.find(arg0.local()) != inputs.end())
        expr0 = &inputs.at(arg0.local());
    if(arg1 && arg1->checkLocal() && inputs.find(arg1->local()) != inputs.end())
        expr1 = &inputs.at(arg1->local());
    if(expr0 == nullptr && expr1 == nullptr)
        // no expression can be combined
        return *this;

    // replace move expressions by their sources, if possible
    if(expr0 && expr0->isMoveExpression() && expr0->arg0.checkLocal() &&
        inputs.find(expr0->arg0.local()) != inputs.end())
        // replace left expression by source
        expr0 = &inputs.at(expr0->arg0.local());

    if(expr1 && expr1->isMoveExpression() && expr1->arg0.checkLocal() &&
        inputs.find(expr1->arg0.local()) != inputs.end())
        // replace right expression by source
        expr1 = &inputs.at(expr1->arg0.local());

    if(unpackMode.hasEffect() || packMode.hasEffect() ||
        (expr0 != nullptr && (expr0->unpackMode.hasEffect() || expr0->packMode.hasEffect())) ||
        ((expr1 != nullptr && (expr1->unpackMode.hasEffect() || expr1->packMode.hasEffect()))))
        // cannot combine pack modes
        return *this;

    // "replace" this with source expression
    if(isMoveExpression())
    {
        if(expr0)
            return expr0->combineWith(inputs);
        if(expr1)
            return expr1->combineWith(inputs);
    }

    if(code.numOperands == 1 && expr0 != nullptr)
    {
        if(code.isIdempotent() && expr0->code == code)
            // f(f(a)) = f(a)
            return Expression{code, expr0->arg0, NO_VALUE, UNPACK_NOP, PACK_NOP, add_flag(deco, expr0->deco)};
        // NOTE: ftoi(itof(i)) != i, itof(ftoi(f)) != f, since the truncation/rounding would get lost!
        if(code.isSelfInverse() && code == expr0->code)
            // f(f(a)) = a, e.g. not(not(a)) = a
            return Expression{OP_V8MIN, expr0->arg0, expr0->arg0, UNPACK_NOP, PACK_NOP, add_flag(deco, expr0->deco)};
    }

    if(code.numOperands == 2)
    {
        if(code.isIdempotent() && arg0 == arg1)
            // f(a, a) = a
            return Expression{OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco};
        if(code.isSelfInverse() && arg0 == arg1)
        {
            // f(a, a) = 0, e.g. a - a = 0
            auto zero = code.returnsFloat ? FLOAT_ZERO : INT_ZERO;
            return Expression{OP_V8MIN, zero, zero, UNPACK_NOP, PACK_NOP, deco};
        }
        if(OpCode::getLeftIdentity(code) == arg0)
            // f(id, a) = a
            return Expression{OP_V8MIN, arg1.value(), arg1, UNPACK_NOP, PACK_NOP, deco};
        if(OpCode::getRightIdentity(code) == arg1)
            // f(a, id) = a
            return Expression{OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco};
        if(OpCode::getLeftAbsorbingElement(code) == arg0)
            // f(absorb, a) = absorb
            return Expression{OP_V8MIN, arg0, arg0, UNPACK_NOP, PACK_NOP, deco};
        if(OpCode::getRightAbsorbingElement(code) == arg1)
            // f(a, absorb) = absorb
            return Expression{OP_V8MIN, arg1.value(), arg1, UNPACK_NOP, PACK_NOP, deco};

        if(code.isAssociative() && expr1 && expr1->code == code && isConstantOperand(arg0) &&
            isConstantOperand(expr1->arg0))
        {
            // f(constA, f(constB, a)) = f(f(constA, constB), a), if associative
            if(auto tmp = code(arg0, expr1->arg0).first)
            {
                return Expression{code, *tmp, expr1->arg1, UNPACK_NOP, PACK_NOP, deco};
            }
        }

        if(code.isAssociative() && expr0 && expr0->code == code && isConstantOperand(arg1) &&
            isConstantOperand(expr0->arg1))
        {
            // f(f(a, constA), constB) = f(a, f(constA, constB)), if associative
            if(auto tmp = code(*expr0->arg1, arg1).first)
            {
                return Expression{code, expr0->arg0, tmp, UNPACK_NOP, PACK_NOP, deco};
            }
        }

        if(code.isAssociative() && code.isCommutative() && expr1 && expr1->code == code && isConstantOperand(arg0) &&
            isConstantOperand(expr1->arg1))
        {
            // f(constA, f(a, constB)) = f(f(constA, constB), a), if associative and commutative
            if(auto tmp = code(arg0, expr1->arg1).first)
            {
                return Expression{code, *tmp, expr1->arg0, UNPACK_NOP, PACK_NOP, deco};
            }
        }

        if(code.isAssociative() && code.isCommutative() && expr0 && expr0->code == code && isConstantOperand(arg1) &&
            isConstantOperand(expr0->arg0))
        {
            // f(f(constA, a), constB) = f(f(constA, constB), a), if associative and commutative
            if(auto tmp = code(expr0->arg0, arg1).first)
            {
                return Expression{code, *tmp, expr0->arg1, UNPACK_NOP, PACK_NOP, deco};
            }
        }

        if(code.isAssociative() && code.isIdempotent() && expr1 && expr1->code == code && expr1->arg0 == arg0)
        {
            // f(a, f(a, b)) = f(a, b), if associative and idempotent
            return Expression{code, arg0, expr1->arg1, UNPACK_NOP, PACK_NOP, deco};
        }

        if(code.isAssociative() && code.isIdempotent() && expr0 && expr0->code == code && expr0->arg1 == arg1)
        {
            // f(f(a, b), b) = f(a, b), if associative and idempotent
            return Expression{code, expr0->arg0, arg1, UNPACK_NOP, PACK_NOP, deco};
        }

        if(code.isAssociative() && code.isIdempotent() && code.isCommutative() && expr1 && expr1->code == code &&
            expr1->arg1 == arg0)
        {
            // f(a, f(b, a)) = f(a, b), if associative, commutative and idempotent
            return Expression{code, arg0, expr1->arg0, UNPACK_NOP, PACK_NOP, deco};
        }

        if(code.isAssociative() && code.isIdempotent() && code.isCommutative() && expr0 && expr0->code == code &&
            expr0->arg0 == arg1)
        {
            // f(f(a, b), a) = f(a, b), if associative, commutative and idempotent
            return Expression{code, expr0->arg0, expr0->arg1, UNPACK_NOP, PACK_NOP, deco};
        }

        if(expr0 && expr1 && expr0->code == expr1->code && expr0->code.isLeftDistributiveOver(code) &&
            expr0->arg0 == expr1->arg0)
        {
            // g(f(a, b), f(a, c)) = f(a, g(b, c)), if left distributive
            if(auto tmp = code(*expr0->arg1, expr1->arg1).first)
            {
                return Expression{expr0->code, expr0->arg0, tmp, UNPACK_NOP, PACK_NOP, deco};
            }
            // TODO add general (with non-constant) case? E.g. if expression g(b, c) already exists
        }

        if(expr0 && expr1 && expr0->code == expr1->code && expr0->code.isRightDistributiveOver(code) &&
            expr0->arg1 == expr1->arg1)
        {
            // g(f(a, b), f(c, b)) = f(g(a, c), b), if right distributive
            if(auto tmp = code(expr0->arg0, expr1->arg0).first)
            {
                return Expression{expr0->code, *tmp, expr0->arg1, UNPACK_NOP, PACK_NOP, deco};
            }
        }

        // TODO more properties to use (e.g. distributive and commutative?)

        if(code == OP_FADD && arg0 == arg1)
        {
            // doesn't save any instruction, but utilizes mul ALU
            return Expression{OP_FMUL, arg0, Value(Literal(2.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP, deco};
        }

        // TODO generalize! E.g. for fsub
        if(code == OP_FADD && expr0 && expr0->code == OP_FMUL)
        {
            if(expr0->arg0 == arg1 && expr0->arg1->getLiteralValue())
                // fadd(fmul(a, constB), a) = fmul(a, constB+1)
                return Expression{OP_FMUL, arg1.value(),
                    Value(Literal(expr0->arg1->getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, expr0->deco)};
            if(expr0->arg1 == arg1 && expr0->arg0.getLiteralValue())
                // fadd(fmul(constB, a), a) = fmul(a, constB+1)
                return Expression{OP_FMUL, arg1.value(),
                    Value(Literal(expr0->arg0.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, expr0->deco)};
        }
        if(code == OP_FADD && expr1 && expr1->code == OP_FMUL)
        {
            if(expr1->arg0 == arg0 && expr1->arg1->getLiteralValue())
                // fadd(a, fmul(a, constB)) = fmul(a, constB+1)
                return Expression{OP_FMUL, arg0,
                    Value(Literal(expr1->arg1->getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, expr1->deco)};
            if(expr1->arg1 == arg0 && expr1->arg0.getLiteralValue())
                // fadd(a, fmul(constB, a)) = fmul(a, constB+1)
                return Expression{OP_FMUL, arg0,
                    Value(Literal(expr1->arg0.getLiteralValue()->real() + 1.0f), TYPE_FLOAT), UNPACK_NOP, PACK_NOP,
                    add_flag(deco, expr1->deco)};
        }

        // TODO generalize! E.g. via mapping opcode -> inverse opcode (inverse according to which of the operands?!)
        if(expr0 && arg1 && expr0->arg1 && (arg1 == expr0->arg0 || arg1 == expr0->arg1))
        {
            // NOTE: This rewrite removes overflow since it removes two operations that could overflow!
            const auto& otherArg = arg1 == expr0->arg0 ? *expr0->arg1 : expr0->arg0;
            // (a + b) - a = b
            // (a + b) - b = a
            // (a - b) + b = a
            if((code == OP_SUB && expr0->code == OP_ADD) || (code == OP_FSUB && expr0->code == OP_FADD) ||
                (code == OP_ADD && expr0->code == OP_SUB && arg1 == expr0->arg1) ||
                (code == OP_FADD && expr0->code == OP_FSUB && arg1 == expr0->arg1))
                return Expression{OP_V8MIN, otherArg, otherArg, UNPACK_NOP, PACK_NOP, deco};

            // (a - b) - a = -b
            if((code == OP_SUB && expr0->code == OP_SUB && arg1 == expr0->arg0) ||
                (code == OP_FSUB && expr0->code == OP_FSUB && arg1 == expr0->arg0))
                return Expression{code, INT_ZERO, otherArg, UNPACK_NOP, PACK_NOP, deco};
        }
        if(expr1 && expr1->arg1 && (arg0 == expr1->arg0 || arg0 == expr1->arg1))
        {
            // NOTE: This rewrite removes overflow since it removes two operations that could overflow!
            const auto& otherArg = arg0 == expr1->arg0 ? *expr1->arg1 : expr1->arg0;
            // a + (b - a) = b
            if((code == OP_ADD && expr1->code == OP_SUB && arg0 == expr1->arg1) ||
                (code == OP_FADD && expr1->code == OP_FSUB && arg0 == expr1->arg1))
            {
                return Expression{OP_V8MIN, otherArg, otherArg, UNPACK_NOP, PACK_NOP, deco};
            }
            // a - (b + a) = -b
            // a - (a + b) = -b
            if((code == OP_SUB && expr1->code == OP_ADD) || (code == OP_FSUB && expr1->code == OP_FADD))
                return Expression{code, INT_ZERO, otherArg, UNPACK_NOP, PACK_NOP, deco};
        }

        // (a << const) + a = a + (a << const) -> a * ((1 << const) + 1)
        if(allowFakeOperations && code == OP_ADD && expr0 && expr0->arg0 == arg1 &&
            (expr0->arg1 & &Value::getLiteralValue))
        {
            auto factor = (1u << expr0->arg1->getLiteralValue()->unsignedInt()) + 1u;
            return Expression{FAKEOP_UMUL, expr0->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco};
        }
        if(allowFakeOperations && code == OP_ADD && expr1 && expr1->arg0 == arg0 &&
            (expr1->arg1 & &Value::getLiteralValue))
        {
            auto factor = (1u << expr1->arg1->getLiteralValue()->unsignedInt()) + 1u;
            return Expression{FAKEOP_UMUL, arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco};
        }

        // (a * constA) << constB = (constA * a) << constB -> a * (constA << constB)
        if(allowFakeOperations && code == OP_SHL && expr0 && expr0->code == FAKEOP_UMUL &&
            (expr0->arg1 & &Value::getLiteralValue) && (arg1 & &Value::getLiteralValue))
        {
            auto factor = expr0->arg1->getLiteralValue()->unsignedInt() << arg1->getLiteralValue()->unsignedInt();
            return Expression{FAKEOP_UMUL, expr0->arg0, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco};
        }
        if(allowFakeOperations && code == OP_SHL && expr0 && expr0->code == FAKEOP_UMUL && expr0->arg1 &&
            (expr0->arg0.getLiteralValue()) && (arg1 & &Value::getLiteralValue))
        {
            auto factor = expr0->arg0.getLiteralValue()->unsignedInt() << arg1->getLiteralValue()->unsignedInt();
            return Expression{
                FAKEOP_UMUL, *expr0->arg1, Value(Literal(factor), TYPE_INT32), UNPACK_NOP, PACK_NOP, deco};
        }

        // TODO
        // (a << constA) * constB = constB * (a << constA) -> a * ((1 << constA) * constB)
        // (a * const) + a = a + (a * const) = (const * a) + a = a + (const * a) -> a * (const + 1)
    }

    return *this;
}

Optional<Value> Expression::getConvergenceLimit(Optional<Literal> initialValue) const
{
    if(auto constant = getConstantExpression())
        return constant;
    if(arg1 && arg1->checkLocal() && arg0.checkLocal() && arg0.local() != arg1->local())
        // multiple different locals
        return NO_VALUE;
    bool leftIsLocal = arg0.checkLocal();
    bool rightIsLocal = arg1 && arg1->checkLocal();
    Optional<Literal> constantLiteral =
        (arg0.getConstantValue() | (arg1 ? arg1->getConstantValue() : NO_VALUE)) & &Value::getLiteralValue;

    if(!leftIsLocal && !rightIsLocal)
        return NO_VALUE;

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
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Unhandled convergence limit calculation for: " << to_string() << logging::endl);
    return NO_VALUE;
}

size_t std::hash<vc4c::Expression>::operator()(const vc4c::Expression& expr) const noexcept
{
    hash<const char*> nameHash;
    hash<Value> valHash;

    return nameHash(expr.code.name) ^ valHash(expr.arg0) ^ (expr.arg1 ? valHash(expr.arg1.value()) : 0) ^
        expr.unpackMode.value ^ expr.packMode.value ^ static_cast<unsigned>(expr.deco);
}

Expression operators::ExpressionWrapper::operator=(OperationWrapper&& op) &&
{
    if(op.signal.hasSideEffects() || op.setFlags == SetFlag::SET_FLAGS)
        throw CompilationError(CompilationStep::GENERAL, "Expressions cannot have side-effects!");
    return Expression{op.op, std::move(op.arg0), std::move(op.arg1), op.unpackMode, op.packMode, op.decoration};
}
