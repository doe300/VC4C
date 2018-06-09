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
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions, void* dummy)
{
    AvailableExpressions newExpressions(previousExpressions);

    if(instr->hasValueType(ValueType::LOCAL))
    {
        newExpressions[instr->getOutput()->local] = instr;
    }

    return newExpressions;
}

std::string AvailableExpressionAnalysis::to_string(const AvailableExpressions& expressions)
{
    if(expressions.empty())
        return "";
    std::stringstream s;
    auto it = expressions.begin();
    s << it->first->name;
    ++it;
    for(; it != expressions.end(); ++it)
        s << ", " << it->first->name;
    return s.str();
}