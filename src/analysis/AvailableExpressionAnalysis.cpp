/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AvailableExpressionAnalysis.h"

#include "../Profiler.h"
#include "DebugGraph.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

AvailableExpressionAnalysis::AvailableExpressionAnalysis() :
    LocalAnalysis(
        AvailableExpressionAnalysis::analyzeAvailableExpressionsWrapper, AvailableExpressionAnalysis::to_string)
{
}

std::pair<AvailableExpressions, std::shared_ptr<Expression>> AvailableExpressionAnalysis::analyzeAvailableExpressions(
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions,
    FastMap<const Local*, FastSet<std::shared_ptr<Expression>>>& cache, unsigned maxExpressionDistance)
{
    PROFILE_SCOPE(AvailableExpressionAnalysis);
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

    std::shared_ptr<Expression> expr;
    if(auto loc = instr->checkOutputLocal())
    {
        // re-set all expressions using the local written to as input
        auto cacheIt = cache.find(loc);
        if(cacheIt != cache.end())
        {
            for(const auto& expr : cacheIt->second)
                newExpressions.erase(expr);
        }
        if((expr = Expression::createExpression(*instr)))
        {
            // only adds if expression is not already in there
            auto it = newExpressions.emplace(expr, std::make_pair(instr, 0));
            if(it.second)
            {
                // add map from input locals to expression (if we really inserted an expression)
                for(const auto& loc : instr->getUsedLocals())
                {
                    if(has_flag(loc.second, LocalUse::Type::READER))
                        cache[loc.first].emplace(it.first->first);
                }
            }
        }
    }
    return std::make_pair(std::move(newExpressions), std::move(expr));
}

AvailableExpressions AvailableExpressionAnalysis::analyzeAvailableExpressionsWrapper(
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions,
    FastMap<const Local*, FastSet<std::shared_ptr<Expression>>>& cache)
{
    return analyzeAvailableExpressions(instr, previousExpressions, cache, std::numeric_limits<unsigned>::max()).first;
}

LCOV_EXCL_START
std::string AvailableExpressionAnalysis::to_string(const AvailableExpressions& expressions)
{
    if(expressions.empty())
        return "";
    std::stringstream s;
    auto it = expressions.begin();
    s << it->first->to_string();
    ++it;
    for(; it != expressions.end(); ++it)
        s << ", " << it->first->to_string();
    return s.str();
}
LCOV_EXCL_STOP

#ifndef DEBUG_MODE
void AvailableExpressionAnalysis::dumpGraph(const AvailableExpressions& expressions, const std::string& fileName)
{
    // DebugGraph is not available, no-op
    (void) expressions;
    (void) fileName;
}
#else
void AvailableExpressionAnalysis::dumpGraph(const AvailableExpressions& expressions, const std::string& fileName)
{
    DebugGraph<const void*, uintptr_t, Directionality::DIRECTED, void> graph(fileName, expressions.size());
    for(const auto& pair : expressions)
    {
        const auto& expr = pair.first;
        graph.addNode(expr.get(), expr->code.name);
        if(auto val = expr->arg0.checkValue())
        {
            graph.addNode(&expr->arg0, val.to_string());
            graph.addEdge(expr.get(), &expr->arg0, false, "left", vc4c::Direction::FIRST_TO_SECOND);
        }
        else if(auto child = expr->arg0.checkExpression())
        {
            graph.addNode(child.get(), child->code.name);
            graph.addEdge(expr.get(), child.get(), false, "left", vc4c::Direction::FIRST_TO_SECOND);
        }
        if(auto val = expr->arg1.checkValue())
        {
            graph.addNode(&expr->arg1, val.to_string());
            graph.addEdge(expr.get(), &expr->arg1, false, "right", vc4c::Direction::FIRST_TO_SECOND);
        }
        else if(auto child = expr->arg1.checkExpression())
        {
            graph.addNode(child.get(), child->code.name);
            graph.addEdge(expr.get(), child.get(), false, "right", vc4c::Direction::FIRST_TO_SECOND);
        }

        if(auto loc = pair.second.first->checkOutputLocal())
        {
            graph.addNode(loc, loc->to_string());
            graph.addEdge(loc, expr.get(), true, "", vc4c::Direction::FIRST_TO_SECOND);
        }
    }

    // TODO some expressions are not resolved completely, e.g. if defined in a different basic block, since they are
    // referenced somehow but not in the list of expressions for this block
}
#endif
