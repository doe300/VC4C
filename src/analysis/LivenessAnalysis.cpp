/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LivenessAnalysis.h"

#include "../Profiler.h"
#include "ControlFlowGraph.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

LivenessAnalysis::LivenessAnalysis(FastSet<const Local*>&& outgoingLiveLocals) :
    LocalAnalysis(LivenessAnalysis::analyzeLiveness, LivenessAnalysis::to_string, std::move(outgoingLiveLocals))
{
}

static bool isIfElseWrite(const FastSet<const LocalUser*>& users)
{
    // TODO be exact, would need to check whether the SetFlags instruction is the same for both instructions
    return users.size() == 2 && (*users.begin())->conditional.isInversionOf((*(++users.begin()))->conditional);
}

FastSet<const Local*> LivenessAnalysis::analyzeLiveness(const intermediate::IntermediateInstruction* instr,
    const FastSet<const Local*>& nextResult,
    std::pair<FastSet<const Local*>, FastMap<const Local*, ConditionCode>>& cache)
{
    PROFILE_START(LivenessAnalysis);
    auto& conditionalWrites = cache.first;
    auto& conditionalReads = cache.second;

    FastSet<const Local*> result(nextResult);

    if(instr->checkOutputLocal() &&
        !instr->hasDecoration(vc4c::intermediate::InstructionDecorations::ELEMENT_INSERTION))
    {
        auto out = instr->getOutput()->local();
        if(instr->hasConditionalExecution() &&
            !instr->hasDecoration(intermediate::InstructionDecorations::ELEMENT_INSERTION))
        {
            auto condReadIt = conditionalReads.find(out);
            if(condReadIt != conditionalReads.end() && condReadIt->second == instr->conditional &&
                condReadIt->first->getSingleWriter() == instr)
                // the local only exists within a conditional block (e.g. temporary within the same flag)
                result.erase(out);
            else if(conditionalWrites.find(out) != conditionalWrites.end() &&
                isIfElseWrite(out->getUsers(LocalUse::Type::WRITER)))
                // the local is written in a select statement (if a then write b otherwise c), which is now complete
                // (since the other write is already added to conditionalWrites)
                // NOTE: For conditions (if there are several conditional writes, we need to keep the local)
                result.erase(out);
            else
                conditionalWrites.emplace(out);
        }
        else
            result.erase(out);
    }
    if(auto combInstr = dynamic_cast<const intermediate::CombinedOperation*>(instr))
    {
        if(combInstr->op1)
            result = analyzeLiveness(combInstr->op1.get(), result, cache);
        if(combInstr->op2)
            result = analyzeLiveness(combInstr->op2.get(), result, cache);
    }

    for(const Value& arg : instr->getArguments())
    {
        if(arg.checkLocal() && !arg.local()->type.isLabelType())
        {
            result.emplace(arg.local());
            if(instr->hasConditionalExecution())
            {
                // there exist locals which only exist if a certain condition is met, so check this
                auto condReadIt = conditionalReads.find(arg.local());
                // if the local is read with different conditions, it must exist in any case
                // TODO be exact, would need to check whether the SetFlags instruction is the same for both instructions
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

LCOV_EXCL_START
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
LCOV_EXCL_STOP

/*
 * Algorithm:
 * in reverse order of control flow, starting from end of kernel
 * 1. run local liveness analysis, cache result for block including live locals at end of block
 * 2. for all direct predecessor blocks
 * 2.1 if cached live locals at end of predecessor block already contains all live locals at start of current block,
 * stop
 * 2.2 if edge is work-group loop, stop
 * 2.3 if no cached live locals of predecessor blocks, or new locals, (re-)run step 1 for predecessor block
 */

using LiveLocalsCache = FastMap<const BasicBlock*, FastSet<const Local*>>;

static void runAnalysis(const CFGNode& node, FastMap<const BasicBlock*, std::unique_ptr<LivenessAnalysis>>& results,
    LiveLocalsCache& cachedEndLiveLocals, FastSet<const Local*>&& cacheEntry, const BasicBlock* startOfKernel)
{
    auto& analyzer = results[node.key];
    analyzer.reset(new LivenessAnalysis(std::move(cacheEntry)));
    (*analyzer)(*node.key);
    // copy on purpose, since result could be modified by previous forAllIncomingEdges loop
    auto startLiveLocals = analyzer->getStartResult();

    if(node.key == startOfKernel && node.key->getLabel()->isWorkGroupLoop)
    {
        // skip work-group loop, since they do not modify the live locals
        // Since if the work-group loop is not active, there might be a kernel code loop back to the start, we only
        // abort if the work-group loop is active (in which case the first block will have the flag set).
        return;
    }

    // TODO could breadth-first be faster?
    node.forAllIncomingEdges([&](const CFGNode& predecessor, const CFGEdge&) -> bool {
        // add all live locals from the beginning of this block to the end of the new one and check whether we are done
        // with this predecessor
        auto predCacheIt = cachedEndLiveLocals.find(predecessor.key);
        if(predCacheIt != cachedEndLiveLocals.end())
        {
            auto sizePrevious = predCacheIt->second.size();
            predCacheIt->second.insert(startLiveLocals.begin(), startLiveLocals.end());
            if(predCacheIt->second.size() == sizePrevious)
                // no new locals were added, can abort this node
                return true;
        }
        else
            predCacheIt = cachedEndLiveLocals.emplace(predecessor.key, startLiveLocals).first;

        runAnalysis(
            predecessor, results, cachedEndLiveLocals, FastSet<const Local*>{predCacheIt->second}, startOfKernel);

        return true;
    });
}

void GlobalLivenessAnalysis::operator()(Method& method)
{
    // TODO need to improve for performance, e.g. make sure, we don't do unnecessary block analyses
    PROFILE_START(GlobalLivenessAnalysis);
    auto& cfg = method.getCFG();
    auto& finalNode = cfg.getEndOfControlFlow();

    // The cache of the live locals at the end of a given basic blocks (e.g. consumed by any succeeding block)
    LiveLocalsCache cachedEndLiveLocals(cfg.getNodes().size());
    results.reserve(cfg.getNodes().size());
    runAnalysis(finalNode, results, cachedEndLiveLocals, {}, cfg.getStartOfControlFlow().key);
    PROFILE_END(GlobalLivenessAnalysis);
}

void GlobalLivenessAnalysis::dumpResults(const Method& method) const
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        for(const BasicBlock& block : method)
        {
            logging::debug() << block.to_string() << logging::endl;
            results.at(&block)->dumpResults(block);
        }
    });
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
            if(arg.checkLocal() && !arg.local()->type.isLabelType())
            {
                if(localsWritten.find(arg.local()) == localsWritten.end())
                    // read before the first write (if any)
                    importedLocals.emplace(arg.local());
                else
                    ++localsReadAfterWriting[arg.local()];
            }
        }

        if(instr->checkOutputLocal() && !instr->getOutput()->local()->type.isLabelType())
            localsWritten.emplace(instr->getOutput()->local());
    };

    for(const auto& inst : block)
    {
        if(auto combInst = dynamic_cast<const intermediate::CombinedOperation*>(inst.get()))
        {
            if(combInst->op1)
                func(combInst->op1.get());
            if(combInst->op2)
                func(combInst->op2.get());
        }
        else
            func(inst.get());
    }

    for(const auto& pair : localsReadAfterWriting)
    {
        if(pair.first->getUsers(LocalUse::Type::READER).size() == pair.second)
            localsWritten.erase(pair.first);
    }

    return std::make_pair(std::move(importedLocals), std::move(localsWritten));
}

LCOV_EXCL_START
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
LCOV_EXCL_STOP
