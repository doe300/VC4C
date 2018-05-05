/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "AvailableExpressionAnalysis.h"

using namespace vc4c;
using namespace vc4c::analysis;

AvailableExpressionAnalysis::AvailableExpressionAnalysis() :
    LocalAnalysis(AvailableExpressionAnalysis::analyzeAvailableExpressions, {})
{
}

AvailableExpressions AvailableExpressionAnalysis::analyzeAvailableExpressions(
    const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions)
{
    AvailableExpressions newExpressions(previousExpressions);

    if(instr->hasValueType(ValueType::LOCAL))
    {
        newExpressions[instr->getOutput()->local] = instr;
    }

    return newExpressions;
}