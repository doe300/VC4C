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

static std::unique_ptr<Local> replicateRegisterLocal(new Parameter("%replicate_register", TYPE_INT32.toVectorType(16)));
const Local* analysis::FAKE_REPLICATE_REGISTER = replicateRegisterLocal.get();

LivenessChangesAnalysis::LivenessChangesAnalysis() :
    LocalAnalysis(LivenessChangesAnalysis::analyzeLivenessChanges, LivenessChangesAnalysis::to_string)
{
}

static bool isIfElseWrite(const FastSet<const LocalUser*>& users)
{
    // TODO be exact, would need to check whether the SetFlags instruction is the same for both instructions
    return users.size() == 2 && (*users.begin())->conditional.isInversionOf((*(++users.begin()))->conditional);
}

static bool readsR5(const intermediate::IntermediateInstruction& instr)
{
    return instr.readsRegister(REG_ACC5) || instr.readsRegister(REG_REPLICATE_ALL) ||
        instr.readsRegister(REG_REPLICATE_QUAD);
}

static bool writesR5(const intermediate::IntermediateInstruction& instr)
{
    return instr.writesRegister(REG_ACC5) || instr.writesRegister(REG_REPLICATE_ALL) ||
        instr.writesRegister(REG_REPLICATE_QUAD);
}

static LivenessChanges analyzeLivenessChangesInner(
    const intermediate::IntermediateInstruction& instr, LivenessAnalysisCache& cache, bool trackR5Usage)
{
    PROFILE_START(LivenessChangesAnalysis);
    LivenessChanges changes;
    auto& conditionalWrites = cache.first;
    auto& conditionalReads = cache.second;

    if(dynamic_cast<const intermediate::BranchLabel*>(&instr))
        // labels do not change any lifetime, since we do not track label lifetimes
        return changes;

    auto localConsumer = [&](const Local* loc, LocalUse::Type type, const intermediate::IntermediateInstruction& inst) {
        if(has_flag(type, LocalUse::Type::WRITER) &&
            !inst.hasDecoration(vc4c::intermediate::InstructionDecorations::ELEMENT_INSERTION))
        {
            if(inst.hasConditionalExecution() &&
                !inst.hasDecoration(intermediate::InstructionDecorations::ELEMENT_INSERTION))
            {
                auto condReadIt = conditionalReads.find(loc);
                if(condReadIt != conditionalReads.end() && condReadIt->second == inst.conditional &&
                    condReadIt->first->getSingleWriter() == &inst)
                    // the local only exists within a conditional block (e.g. temporary within the same flag)
                    changes.removedLocals.emplace_back(loc);
                else if(conditionalWrites.find(loc) != conditionalWrites.end() &&
                    isIfElseWrite(loc->getUsers(LocalUse::Type::WRITER)))
                    // the local is written in a select statement (if a then write b otherwise c), which is now complete
                    // (since the other write is already added to conditionalWrites)
                    // NOTE: For conditions (if there are several conditional writes, we need to keep the local)
                    changes.removedLocals.emplace_back(loc);
                else
                    conditionalWrites.emplace(loc);
            }
            else
                changes.removedLocals.emplace_back(loc);
        }
        if(has_flag(type, LocalUse::Type::READER) && !loc->type.isLabelType())
        {
            changes.addedLocals.emplace_back(loc);
            if(inst.hasConditionalExecution())
            {
                // there exist locals which only exist if a certain condition is met, so check this
                auto condReadIt = conditionalReads.find(loc);
                // if the local is read with different conditions, it must exist in any case
                // TODO be exact, would need to check whether the SetFlags instruction is the same for both instructions
                if(condReadIt != conditionalReads.end() && condReadIt->second != inst.conditional)
                    conditionalReads.erase(condReadIt);
                else
                    conditionalReads.emplace(loc, inst.conditional);
            }
        }
    };

    instr.forUsedLocals(localConsumer);
    if(trackR5Usage)
    {
        auto usageConsumer = [&](const intermediate::IntermediateInstruction& inst) {
            LocalUse::Type type = LocalUse::Type::NONE;
            if(writesR5(inst))
                type = add_flag(type, LocalUse::Type::WRITER);
            if(readsR5(inst))
                type = add_flag(type, LocalUse::Type::READER);
            if(type != LocalUse::Type::NONE)
                localConsumer(FAKE_REPLICATE_REGISTER, type, inst);
        };
        if(auto combined = dynamic_cast<const intermediate::CombinedOperation*>(&instr))
        {
            if(auto first = combined->getFirstOp())
                usageConsumer(*first);
            if(auto second = combined->getSecondOP())
                usageConsumer(*second);
        }
        else
            usageConsumer(instr);
    }

    PROFILE_END(LivenessChangesAnalysis);
    return changes;
}

LivenessChanges LivenessChangesAnalysis::analyzeLivenessChanges(const intermediate::IntermediateInstruction* instr,
    const LivenessChanges& previousChanges, LivenessAnalysisCache& cache, bool trackR5Usage)
{
    if(instr)
        return analyzeLivenessChangesInner(*instr, cache, trackR5Usage);
    return {};
}

LCOV_EXCL_START
std::string LivenessChangesAnalysis::to_string(const LivenessChanges& changes)
{
    std::stringstream s;
    auto it = changes.removedLocals.begin();
    if(it != changes.removedLocals.end())
    {
        s << '-' << (*it)->name;
        ++it;
        for(; it != changes.removedLocals.end(); ++it)
            s << ", -" << (*it)->name;
    }

    it = changes.addedLocals.begin();
    if(it != changes.addedLocals.end())
    {
        if(!changes.removedLocals.empty())
            s << ' ';
        s << '+' << (*it)->name;
        ++it;
        for(; it != changes.addedLocals.end(); ++it)
            s << ", +" << (*it)->name;
    }
    return s.str();
}
LCOV_EXCL_STOP

LivenessAnalysis::LivenessAnalysis(FastSet<const Local*>&& outgoingLiveLocals) :
    LocalAnalysis(LivenessAnalysis::analyzeLiveness, LivenessAnalysis::to_string, std::move(outgoingLiveLocals))
{
}

static FastSet<const Local*> updateLiveness(
    FastSet<const Local*>&& nextResult, const LivenessChanges& changes, bool skipAdditions = false);

FastSet<const Local*> LivenessAnalysis::analyzeIncomingLiveLocals(
    const BasicBlock& block, bool trackR5Usage, FastSet<const Local*>&& outgoingLiveLocals)
{
    Cache c;
    auto prevVal = std::move(outgoingLiveLocals);
    auto it = block.end();
    do
    {
        --it;
        if(*it)
            prevVal = updateLiveness(std::move(prevVal), analyzeLivenessChangesInner(*it->get(), c, trackR5Usage));
    } while(it != block.begin());
    return prevVal;
}

void LivenessAnalysis::analyzeWithChanges(const BasicBlock& block, const LivenessChangesAnalysis& analysis)
{
    results.reserve(block.size());
    const auto* prevVal = &initialValue;
    auto it = block.end();
    do
    {
        --it;
        if(*it)
        {
            auto pos = results.emplace(
                it->get(), updateLiveness(FastSet<const Local*>{*prevVal}, analysis.getResult(it->get())));
            prevVal = &(pos.first->second);
        }
    } while(it != block.begin());

    resultAtStart = &results.at(block.begin()->get());
    if(!block.empty())
        resultAtEnd = &results.at((--block.end())->get());
}

void LivenessAnalysis::updateWithChanges(
    const BasicBlock& block, const LivenessChangesAnalysis& analysis, FastSet<const Local*>&& outgoingLiveLocals)
{
    auto prevVal = std::move(outgoingLiveLocals);
    auto it = block.end();
    do
    {
        --it;
        if(*it)
        {
            // Do not handle locals added by the instruction itself, since they are already handled by the initial run.
            // Here, we only care about whether/when the lifetime of the additional locals ends within this block.
            auto tmp = updateLiveness(std::move(prevVal), analysis.getResult(it->get()), true);
            results.at(it->get()).insert(tmp.begin(), tmp.end());
            prevVal = std::move(tmp);
        }
    } while(!prevVal.empty() && it != block.begin());
}

FastSet<const Local*> LivenessAnalysis::analyzeLiveness(const intermediate::IntermediateInstruction* instr,
    const FastSet<const Local*>& nextResult, LivenessAnalysisCache& cache, bool trackR5Usage)
{
    auto changes = instr ? analyzeLivenessChangesInner(*instr, cache, trackR5Usage) : LivenessChanges{};
    return updateLiveness(FastSet<const Local*>{nextResult}, changes);
}

static FastSet<const Local*> updateLiveness(
    FastSet<const Local*>&& nextResult, const LivenessChanges& changes, bool skipAdditions)
{
    PROFILE_START(LivenessAnalysis);

    FastSet<const Local*> result(std::move(nextResult));
    for(auto removed : changes.removedLocals)
        result.erase(removed);
    if(!skipAdditions)
    {
        for(auto added : changes.addedLocals)
            result.emplace(added);
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
    const FastMap<const BasicBlock*, LivenessChangesAnalysis>& changes, LiveLocalsCache& cachedEndLiveLocals,
    FastSet<const Local*>&& cacheEntry, const BasicBlock* startOfKernel)
{
    PROFILE_START(SingleLivenessAnalysis);
    auto& analyzer = results[node.key];
    if(!analyzer)
    {
        analyzer.reset(new LivenessAnalysis(std::move(cacheEntry)));
        analyzer->analyzeWithChanges(*node.key, changes.at(node.key));
    }
    else
        analyzer->updateWithChanges(*node.key, changes.at(node.key), std::move(cacheEntry));
    PROFILE_END(SingleLivenessAnalysis);

    // copy on purpose, since result could be modified by previous forAllIncomingEdges loop
    auto startLiveLocals = analyzer->getStartResult();

    if(node.key == startOfKernel &&
        node.key->getLabel()->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_LOOP))
    {
        // skip work-group loop, since they do not modify the live locals
        // Since if the work-group loop is not active, there might be a kernel code loop back to the start, we only
        // abort if the work-group loop is active (in which case the first block will have the flag set).
        return;
    }

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

        runAnalysis(predecessor, results, changes, cachedEndLiveLocals, FastSet<const Local*>{predCacheIt->second},
            startOfKernel);

        return true;
    });
}

void GlobalLivenessAnalysis::operator()(Method& method)
{
    PROFILE_START(GlobalLivenessAnalysis);
    auto& cfg = method.getCFG();
    auto& finalNode = cfg.getEndOfControlFlow();
    changes.reserve(method.size());
    // precalculate changes in livenesses
    for(const auto& block : method)
    {
        auto it = changes.emplace(&block, LivenessChangesAnalysis{}).first;
        it->second(block, trackR5Usage);
    }

    // The cache of the live locals at the end of a given basic blocks (e.g. consumed by any succeeding block)
    LiveLocalsCache cachedEndLiveLocals(cfg.getNodes().size());
    results.reserve(cfg.getNodes().size());
    runAnalysis(finalNode, results, changes, cachedEndLiveLocals, {}, cfg.getStartOfControlFlow().key);
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
