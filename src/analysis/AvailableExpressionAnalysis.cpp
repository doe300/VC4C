/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AvailableExpressionAnalysis.h"

#include "../Profiler.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

AvailableExpressionAnalysis::AvailableExpressionAnalysis() :
    LocalAnalysis(
        AvailableExpressionAnalysis::analyzeAvailableExpressionsWrapper, AvailableExpressionAnalysis::to_string)
{
}

std::pair<AvailableExpressions, Optional<Expression>> AvailableExpressionAnalysis::analyzeAvailableExpressions(
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions,
    FastMap<const Local*, FastSet<Expression>>& cache, unsigned maxExpressionDistance)
{
    PROFILE_START(AvailableExpressionAnalysis);
    AvailableExpressions newExpressions(previousExpressions);
    auto it = newExpressions.begin();
    while(it != newExpressions.end())
    {
        if(it->second.second >= maxExpressionDistance)
        {
            // remove all "older" expressions, since we do not care for them anymore
            it = newExpressions.erase(it);
        }
        else
        {
            ++it->second.second;
            ++it;
        }
    }

    Optional<Expression> expr;
    if(instr->hasValueType(ValueType::LOCAL))
    {
        // re-set all expressions using the local written to as input
        auto cacheIt = cache.find(instr->getOutput()->local());
        if(cacheIt != cache.end())
        {
            for(const auto& expr : cacheIt->second)
                newExpressions.erase(expr);
        }
        expr = Expression::createExpression(*instr);
        if(expr)
        {
            // only adds if expression is not already in there
            auto it = newExpressions.emplace(expr.value(), std::make_pair(instr, 0));
            if(it.second)
            {
                // add map from input locals to expression (if we really inserted an expression)
                for(const auto loc : instr->getUsedLocals())
                {
                    if(has_flag(loc.second, LocalUse::Type::READER))
                        cache[loc.first].emplace(it.first->first);
                }
            }
        }
    }
    PROFILE_END(AvailableExpressionAnalysis);
    return std::make_pair(std::move(newExpressions), std::move(expr));
}

AvailableExpressions AvailableExpressionAnalysis::analyzeAvailableExpressionsWrapper(
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions,
    FastMap<const Local*, FastSet<Expression>>& cache)
{
    return analyzeAvailableExpressions(instr, previousExpressions, cache, std::numeric_limits<unsigned>::max()).first;
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