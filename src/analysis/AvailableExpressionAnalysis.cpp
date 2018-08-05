/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AvailableExpressionAnalysis.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

size_t hash<analysis::Expression>::operator()(const analysis::Expression& expr) const noexcept
{
    hash<const char*> nameHash;
    hash<Value> valHash;

    return nameHash(expr.code.name) ^ valHash(expr.arg0) ^ (expr.arg1 ? valHash(expr.arg1.value()) : 0) ^
        expr.unpackMode.value ^ expr.packMode.value ^ static_cast<unsigned>(expr.deco);
}

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

AvailableExpressionAnalysis::AvailableExpressionAnalysis() :
    LocalAnalysis(AvailableExpressionAnalysis::analyzeAvailableExpressions, AvailableExpressionAnalysis::to_string)
{
}

AvailableExpressions AvailableExpressionAnalysis::analyzeAvailableExpressions(
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions,
    FastMap<const Local*, FastSet<const Expression*>>& cache)
{
    AvailableExpressions newExpressions(previousExpressions);

    if(instr->hasValueType(ValueType::LOCAL))
    {
        // re-set all expressions using the local written to as input
        auto cacheIt = cache.find(instr->getOutput()->local);
        if(cacheIt != cache.end())
        {
            for(const auto& expr : cacheIt->second)
                newExpressions.erase(*expr);
        }
        auto expr = Expression::createExpression(*instr);
        if(expr)
        {
            // only adds if expression is not already in there
            auto it = newExpressions.emplace(expr.value(), instr);
            if(it.second)
            {
                // add map from input locals to expression (if we really inserted an expression)
                for(const auto loc : instr->getUsedLocals())
                {
                    if(has_flag(loc.second, LocalUse::Type::READER))
                        cache[loc.first].emplace(&it.first->first);
                }
            }
        }
    }

    return newExpressions;
}

std::string AvailableExpressionAnalysis::to_string(const AvailableExpressions& expressions)
{
    if(expressions.empty())
        return "";
    std::stringstream s;
    auto it = expressions.begin();
    s << it->first.to_string();
    ++it;
    for(; it != expressions.end(); ++it)
        s << ", " << it->first.to_string();
    return s.str();
}