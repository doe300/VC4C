/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AvailableExpressionAnalysis.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

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
        auto cacheIt = cache.find(instr->getOutput()->local());
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