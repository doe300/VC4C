/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LivenessAnalysis.h"

#include "../Profiler.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

LivenessAnalysis::LivenessAnalysis() : LocalAnalysis(LivenessAnalysis::analyzeLiveness, LivenessAnalysis::to_string) {}

FastSet<const Local*> LivenessAnalysis::analyzeLiveness(const intermediate::IntermediateInstruction* instr,
    const FastSet<const Local*>& nextResult,
    std::pair<FastSet<const Local*>, FastMap<const Local*, ConditionCode>>& cache)
{
    PROFILE_START(LivenessAnalysis);
    auto& conditionalWrites = cache.first;
    auto& conditionalReads = cache.second;

    FastSet<const Local*> result(nextResult);

    if(instr->hasValueType(ValueType::LOCAL) &&
        !instr->hasDecoration(vc4c::intermediate::InstructionDecorations::ELEMENT_INSERTION))
    {
        if(instr->hasConditionalExecution() &&
            !instr->hasDecoration(intermediate::InstructionDecorations::ELEMENT_INSERTION))
        {
            auto condReadIt = conditionalReads.find(instr->getOutput()->local());
            if(condReadIt != conditionalReads.end() && condReadIt->second == instr->conditional &&
                condReadIt->first->getSingleWriter() == instr)
                // the local only exists within a conditional block (e.g. temporary within the same flag)
                result.erase(instr->getOutput()->local());
            else if(conditionalWrites.find(instr->getOutput()->local()) != conditionalWrites.end())
                result.erase(instr->getOutput()->local());
            else
                conditionalWrites.emplace(instr->getOutput()->local());
        }
        else
            result.erase(instr->getOutput()->local());
    }
    auto combInstr = dynamic_cast<const intermediate::CombinedOperation*>(instr);
    if(combInstr)
    {
        if(combInstr->op1)
            result = analyzeLiveness(combInstr->op1.get(), result, cache);
        if(combInstr->op2)
            result = analyzeLiveness(combInstr->op2.get(), result, cache);
    }

    for(const Value& arg : instr->getArguments())
    {
        if(arg.hasLocal() && !arg.local()->type.isLabelType())
        {
            result.emplace(arg.local());
            if(instr->hasConditionalExecution())
            {
                // there exist locals which only exist if a certain condition is met, so check this
                auto condReadIt = conditionalReads.find(arg.local());
                // if the local is read with different conditions, it must exist in any case
                if(condReadIt != conditionalReads.end() && condReadIt->second != instr->conditional)
                    conditionalReads.erase(condReadIt);
                else
                    conditionalReads.emplace(arg.local(), instr->conditional);
            }
        }
    }
    PROFILE_END(LivenessAnalysis);
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

LocalUsageAnalysis::LocalUsageAnalysis() :
    GlobalAnalysis(LocalUsageAnalysis::analyzeLocalUsage, LocalUsageAnalysis::to_string)
{
}

std::pair<FastSet<const Local*>, FastSet<const Local*>> LocalUsageAnalysis::analyzeLocalUsage(const BasicBlock& block)
{
    FastSet<const Local*> localsWritten;
    FastSet<const Local*> importedLocals;
    FastMap<const Local*, unsigned> localsReadAfterWriting;

    auto func = [&](const intermediate::IntermediateInstruction* instr) {
        for(const Value& arg : instr->getArguments())
        {
            if(arg.hasLocal() && !arg.local()->type.isLabelType())
            {
                if(localsWritten.find(arg.local()) == localsWritten.end())
                    // read before the first write (if any)
                    importedLocals.emplace(arg.local());
                else
                    ++localsReadAfterWriting[arg.local()];
            }
        }

        if(instr->hasValueType(ValueType::LOCAL) && !instr->getOutput()->local()->type.isLabelType())
            localsWritten.emplace(instr->getOutput()->local());
    };

    for(auto it = block.begin(); !it.isEndOfBlock(); it.nextInBlock())
    {
        if(it.has<intermediate::CombinedOperation>())
        {
            auto combInst = it.get<const intermediate::CombinedOperation>();
            if(combInst->op1)
                func(combInst->op1.get());
            if(combInst->op2)
                func(combInst->op2.get());
        }
        else
            func(it.get());
    }

    for(const auto& pair : localsReadAfterWriting)
    {
        if(pair.first->getUsers(LocalUse::Type::READER).size() == pair.second)
            localsWritten.erase(pair.first);
    }

    return std::make_pair(std::move(importedLocals), std::move(localsWritten));
}

std::string LocalUsageAnalysis::to_string(const FastSet<const Local*>& locals)
{
    if(locals.empty())
        return "";
    std::stringstream s;
    auto it = locals.begin();
    s << (*it)->name;
    ++it;
    for(; it != locals.end(); ++it)
        s << ", " << (*it)->name;
    return s.str();
}
