/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LivenessAnalysis.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

LivenessAnalysis::LivenessAnalysis() : LocalAnalysis(LivenessAnalysis::analyzeLiveness, LivenessAnalysis::to_string) {}

FastSet<const Local*> LivenessAnalysis::analyzeLiveness(const intermediate::IntermediateInstruction* instr,
    const FastSet<const Local*>& nextResult, FastSet<const Local*>& conditionalWrites)
{
    FastSet<const Local*> result(nextResult);

    if(instr->hasValueType(ValueType::LOCAL) &&
        !instr->hasDecoration(vc4c::intermediate::InstructionDecorations::ELEMENT_INSERTION))
    {
        if(instr->hasConditionalExecution())
        {
            if(conditionalWrites.find(instr->getOutput()->local) != conditionalWrites.end())
                result.erase(instr->getOutput()->local);
            else
                conditionalWrites.emplace(instr->getOutput()->local);
        }
        else
            result.erase(instr->getOutput()->local);
    }

    for(const Value& arg : instr->getArguments())
    {
        if(arg.hasType(ValueType::LOCAL))
            result.emplace(arg.local);
    }

    return result;
}

std::string LivenessAnalysis::to_string(const FastSet<const Local*>& liveLocals)
{
    if(liveLocals.empty())
        return "";
    std::stringstream s;
    auto it = liveLocals.begin();
    s << (*it)->name;
    ++it;
    for(; it != liveLocals.end(); ++it)
        s << ", " << (*it)->name;
    return s.str();
}

LocalUsageAnalysis::LocalUsageAnalysis() : GlobalAnalysis(LocalUsageAnalysis::analyzeLocalUsage) {}

std::pair<FastSet<const Local*>, FastSet<const Local*>> LocalUsageAnalysis::analyzeLocalUsage(const BasicBlock& block)
{
    FastSet<const Local*> localsWritten;
    FastSet<const Local*> importedLocals;
    FastMap<const Local*, unsigned> localsReadAfterWriting;

    for(auto it = block.begin(); !it.isEndOfBlock(); it.nextInBlock())
    {
        for(const Value& arg : it->getArguments())
        {
            if(arg.hasType(ValueType::LOCAL))
            {
                if(localsWritten.find(arg.local) == localsWritten.end())
                    // read before the first write (if any)
                    importedLocals.emplace(arg.local);
                else
                    ++localsReadAfterWriting[arg.local];
            }
            if(it->hasValueType(ValueType::LOCAL))
                localsWritten.emplace(it->getOutput()->local);
        }
    }

    for(const auto& pair : localsReadAfterWriting)
    {
        if(pair.first->getUsers(LocalUse::Type::READER).size() == pair.second)
            localsWritten.erase(pair.first);
    }

    return std::make_pair(std::move(importedLocals), std::move(localsWritten));
}