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
    return (code == OP_OR || code == OP_V8MAX || code == OP_V8MIN) && arg1.is(arg0);
}

Optional<Value> Expression::getConstantExpression() const
{
    return code(arg0, arg1);
}

size_t hash<Expression>::operator()(const Expression& expr) const noexcept
{
    hash<const char*> nameHash;
    hash<Value> valHash;

    return nameHash(expr.code.name) ^ valHash(expr.arg0) ^ (expr.arg1 ? valHash(expr.arg1.value()) : 0) ^
        expr.unpackMode.value ^ expr.packMode.value ^ static_cast<unsigned>(expr.deco);
}