#include "Expression.h"

using namespace vc4c;

Optional<Expression> Expression::createExpression(const intermediate::IntermediateInstruction& instr)
{
    if(instr.hasSideEffects())
        return {};
    if(instr.hasConditionalExecution())
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
    return Expression{
        code, instr.getArgument(0).value(), instr.getArgument(1), instr.unpackMode, instr.packMode, instr.decoration};
}

bool Expression::operator==(const Expression& other) const
{
    return code == other.code && arg0 == other.arg0 && arg1 == other.arg1 && unpackMode == other.unpackMode &&
        packMode == other.packMode && deco == other.deco;
}

std::string Expression::to_string() const
{
    return std::string(code.name) + " " + arg0.to_string() +
        (arg1 ? std::string(", ") + arg1.to_string() : std::string{});
}

bool Expression::isMoveExpression() const
{
    return (code == OP_OR || code == OP_V8MAX || code == OP_V8MIN) && arg1 == arg0;
}

Optional<Value> Expression::getConstantExpression() const
{
    return code(arg0, arg1);
}

bool Expression::hasConstantOperand() const
{
    return arg0.getLiteralValue() || arg0.hasContainer() || (arg1 && (arg1->getLiteralValue() || arg1->hasContainer()));
}

Expression Expression::combineWith(const FastMap<const Local*, Expression>& inputs) const
{
    const Expression* expr0 = nullptr;
    const Expression* expr1 = nullptr;
    if(arg0.hasLocal() && inputs.find(arg0.local()) != inputs.end())
        expr0 = &inputs.at(arg0.local());
    if(arg1 && arg1->hasLocal() && inputs.find(arg1->local()) != inputs.end())
        expr1 = &inputs.at(arg1->local());
    if(expr0 == nullptr && expr1 == nullptr)
        // no expression can be combined
        return *this;

    if(unpackMode != UNPACK_NOP || packMode != PACK_NOP ||
        (expr0 != nullptr && (expr0->unpackMode != UNPACK_NOP || expr0->packMode != PACK_NOP)) ||
        ((expr1 != nullptr && (expr1->unpackMode != UNPACK_NOP || expr1->packMode != PACK_NOP))))
        // cannot combine pack modes
        return *this;

    if(code.numOperands == 1 && expr0 != nullptr)
    {
        if(code == OP_FTOI && expr0->code == OP_ITOF)
            // ftoi(itof(i)) = i
            return Expression{OP_V8MIN, expr0->arg0, NO_VALUE, UNPACK_NOP, PACK_NOP, add_flag(deco, expr0->deco)};
        if(code == OP_ITOF && expr0->code == OP_FTOI)
            // itof(ftoi(f)) = f
            return Expression{OP_V8MIN, expr0->arg0, NO_VALUE, UNPACK_NOP, PACK_NOP, add_flag(deco, expr0->deco)};
        if(code == OP_NOT && expr0->code == OP_NOT)
            // not(not(a)) = a
            return Expression{OP_V8MIN, expr0->arg0, NO_VALUE, UNPACK_NOP, PACK_NOP, add_flag(deco, expr0->deco)};
    }

    auto firstArgConstant = arg0.getLiteralValue() || arg0.hasContainer() ?
        Optional<Value>(arg0) :
        expr0 ? expr0->getConstantExpression() : NO_VALUE;

    auto secondArgConstant = arg1 && (arg1->getLiteralValue() || arg1->hasContainer()) ?
        arg1 :
        expr1 ? expr1->getConstantExpression() : NO_VALUE;

    if(static_cast<int>(firstArgConstant || (expr0 && expr0->hasConstantOperand())) +
                static_cast<int>(secondArgConstant || (expr1 && expr1->hasConstantOperand())) <
            2 ||
        !(firstArgConstant || secondArgConstant))
        // we can only combine expressions if there are multiple literal/constant parts to combine (one of them directly
        // in this expression)
        return *this;

    if(code.numOperands == 2)
    {
        if(code == OP_FADD && ((expr0 && expr0->code == OP_FADD) || (expr1 && expr1->code == OP_FADD)))
        {
            // TODO
        }

        // TODO also combine things like (a * 4) + a = a * 5 or (a * 4) - a = a * 3
    }

    return *this;
}

size_t std::hash<vc4c::Expression>::operator()(const vc4c::Expression& expr) const noexcept
{
    hash<const char*> nameHash;
    hash<Value> valHash;

    return nameHash(expr.code.name) ^ valHash(expr.arg0) ^ (expr.arg1 ? valHash(expr.arg1.value()) : 0) ^
        expr.unpackMode.value ^ expr.packMode.value ^ static_cast<unsigned>(expr.deco);
}
