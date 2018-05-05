/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LivenessAnalysis.h"

using namespace vc4c;
using namespace vc4c::analysis;

LivenessAnalysis::LivenessAnalysis() : LocalAnalysis(LivenessAnalysis::analyzeLiveness, {}) {}

FastSet<const Local*> LivenessAnalysis::analyzeLiveness(
    const intermediate::IntermediateInstruction* instr, const FastSet<const Local*>& nextResult)
{
    FastSet<const Local*> result(nextResult);

    if(instr->hasValueType(ValueType::LOCAL))
        result.erase(instr->getOutput()->local);

    for(const Value& arg : instr->getArguments())
    {
        if(arg.hasType(ValueType::LOCAL))
            result.emplace(arg.local);
    }

    return result;
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