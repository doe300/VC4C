/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LivenessAnalysis.h"

#include "../Profiler.h"
#include "ControlFlowGraph.h"
#include "ControlFlowLoop.h"

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
    if(users.size() != 2)
        return false;
    auto first = dynamic_cast<const intermediate::ExtendedInstruction*>(*users.begin());
    auto second = dynamic_cast<const intermediate::ExtendedInstruction*>(*(++users.begin()));
    // TODO be exact, would need to check whether the SetFlags instruction is the same for both instructions
    return first && second && first->getCondition().isInversionOf(second->getCondition());
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
    auto& conditionalWrites = cache.conditionalWrites;
    auto& conditionalReads = cache.conditionalReads;
    auto& liveLocals = cache.liveLocals;
    auto& phiWrites = cache.conditionalPhiWrites;

    if(dynamic_cast<const intermediate::BranchLabel*>(&instr))
    {
        // end the liveness range of all phi-nodes (that are still phi-only nodes)
        for(auto loc : phiWrites)
        {
            changes.removedLocals.emplace(loc);
            liveLocals.erase(loc);
        }
        // labels by themselves do not change any lifetime, since we do not track label lifetimes
        return changes;
    }

    auto localConsumer = [&](const Local* loc, LocalUse::Type type, const intermediate::IntermediateInstruction& inst) {
        auto extendedInst = dynamic_cast<const intermediate::ExtendedInstruction*>(&inst);
        if(has_flag(type, LocalUse::Type::WRITER) &&
            !inst.hasDecoration(vc4c::intermediate::InstructionDecorations::ELEMENT_INSERTION))
        {
            if(extendedInst && inst.hasConditionalExecution() &&
                !inst.hasDecoration(intermediate::InstructionDecorations::ELEMENT_INSERTION))
            {
                auto condReadIt = conditionalReads.find(loc);
                if(condReadIt != conditionalReads.end() && condReadIt->second == extendedInst->getCondition() &&
                    condReadIt->first->getSingleWriter() == &inst)
                {
                    // the local only exists within a conditional block (e.g. temporary within the same flag)
                    changes.removedLocals.emplace(loc);
                    phiWrites.erase(loc);
                    liveLocals.erase(loc);
                }
                else if(conditionalWrites.find(loc) != conditionalWrites.end() &&
                    isIfElseWrite(loc->getUsers(LocalUse::Type::WRITER)))
                {
                    // the local is written in a select statement (if a then write b otherwise c), which is now complete
                    // (since the other write is already added to conditionalWrites)
                    // NOTE: For conditions (if there are several conditional writes, we need to keep the local)
                    changes.removedLocals.emplace(loc);
                    phiWrites.erase(loc);
                    liveLocals.erase(loc);
                }
                else if(inst.hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                {
                    /*
                     * Phi-nodes for conditional branches are by default (unless optimized away) marked with the
                     * condition of taking the associated branch. Since there is no write for the opposite condition
                     * inside the same basic block, the above code will not terminate the local live range here.
                     *
                     * The block of this phi-node write is only the direct predecessor of the block consuming the
                     * phi-node (at runtime), iff the branch is taken. Thus, either the phi-node condition (which is the
                     * branch condition) is true, the phi-node is overwritten and the associated branch is taken or the
                     * phi-node condition is false, the phi-node is not overwritten and the branch is not taken.
                     *
                     * Given that the phi-nodes are generated from an LLVM SSA-form IR, a single phi-node local is only
                     * used for a single phi-node (and therefore only across a single block and its predecessors). Also,
                     * the phi-node local is written by phi-node writes inserted into the end of all of the block's
                     * direct predecessors.
                     *
                     * So we assume, that if the block of the phi-node read is reached from this block not via the
                     * phi-node write associated branch, but via some other way, that the phi-node local is always
                     * overwritten along the execution path. This allows us to assume a phi-node write to
                     * unconditionally end the local's live range for this execution path.
                     *
                     * This is not quite true, at least not for single-block loops where the following code would
                     * generate a wrong liveness range:
                     *   %inc = add %i, 1
                     *   [...]
                     *   %i = %inc (phi ifzc)
                     * => %i would have two distinct live ranges, although in the case where the condition for the
                     * assignment is not met, the original value has to be retained!
                     *
                     * To also handle this case, we treat all phi-node writes special and do not immediately end their
                     * liveness (if they are conditional), but instead keep them live to the beginning of the block.
                     *
                     * TODO what guarantees this to be correct instead?? E.g. can we have the same problem for
                     * multi-block loops?
                     */
                    phiWrites.emplace(loc);
                    liveLocals.erase(loc);
                }
                else
                {
                    conditionalWrites.emplace(loc);
                    phiWrites.erase(loc);
                }
            }
            else
            {
                changes.removedLocals.emplace(loc);
                phiWrites.erase(loc);
                liveLocals.erase(loc);
            }
        }
        if(has_flag(type, LocalUse::Type::READER) && !loc->type.isLabelType())
        {
            if(liveLocals.find(loc) == liveLocals.end())
            {
                changes.addedLocals.emplace(loc);
                liveLocals.emplace(loc);
            }
            if(extendedInst && inst.hasConditionalExecution())
            {
                // there exist locals which only exist if a certain condition is met, so check this
                auto condReadIt = conditionalReads.find(loc);
                // if the local is read with different conditions, it must exist in any case
                // TODO be exact, would need to check whether the SetFlags instruction is the same for both instructions
                if(condReadIt != conditionalReads.end() && condReadIt->second != extendedInst->getCondition())
                    conditionalReads.erase(condReadIt);
                else
                    conditionalReads.emplace(loc, extendedInst->getCondition());
            }
            phiWrites.erase(loc);
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
            if(auto second = combined->getSecondOp())
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
    FastSet<const Local*>&& cacheEntry, const BasicBlock* startOfKernel, FastSet<const CFGNode*>& pendingNodes)
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
            if(predCacheIt->second.size() == sizePrevious && results[predecessor.key])
                // no new locals were added and we already processed the node at least once, can abort this node
                return true;
        }
        else
            predCacheIt = cachedEndLiveLocals.emplace(predecessor.key, startLiveLocals).first;

        // we need to (re-)run the analysis on that basic block since the live locals changed
        pendingNodes.emplace(&predecessor);
        return true;
    });
}

static FastSet<const Local*> initializeEndOfBlockLiveLocals(const BasicBlock& block)
{
    /*
     * unnecessary writes (e.g. after the last read without any loop/or other writes are unconditional so they can be
     * actually used at all) brake register association, since they are not regarded!
     *
     * Example:
     * <16 x i32> %lowered_stack.26.upper = register r4
     * [...]
     * <16 x i32> %dec.lower = add <16 x i32> %lowered_stack.26.lower, <16 x i32> -1 (31) (setf group_uniform splat)
     * [...]
     * <16 x i32> %lowered_stack.26.upper = <16 x i32> %dec.upper
     *
     * => The calculated lifetime range is from the middle read to the first write, but the local is live
     * additionally/inclusive from the second write to the end of the block!
     *
     * To fix this, we iterate once over the basic block forward and mark all live-times from write to the next read.
     * This gives us (some non-complete version of) the live locals at the end of the block and guarantees that unread
     * writes are considered live until the end of the block!
     */
    FastSet<const Local*> endLiveLocals;
    for(const auto& it : block)
    {
        if(!it)
            continue;
        for(const auto& loc : it->getUsedLocals())
        {
            if(has_flag(LocalUse::Type::READER, loc.second))
                // we don't care here about the real live-time, only some minimal version, so remove on any (even
                // conditional) read
                endLiveLocals.erase(loc.first);
            if(has_flag(LocalUse::Type::WRITER, loc.second) && !loc.first->type.isLabelType())
                endLiveLocals.emplace(loc.first);
        }
    }
    return endLiveLocals;
}

void GlobalLivenessAnalysis::operator()(Method& method)
{
    PROFILE_START(GlobalLivenessAnalysis);
    auto& cfg = method.getCFG();
    auto& finalNode = cfg.getEndOfControlFlow();
    auto startOfKernel = cfg.getStartOfControlFlow().key;
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
    for(const auto& node : cfg.getNodes())
        // initialize with basic set of locals guaranteed to be still live at the end of the block to avoid some
        // register errors
        cachedEndLiveLocals.emplace(node.first, initializeEndOfBlockLiveLocals(*node.first));
    // to avoid stack overflows, we rerun the pending blocks iteratively, not recursively. So keep track of the basic
    // blocks still to be processed (again).
    FastSet<const CFGNode*> pendingNodes;
    // initial run for the last node
    runAnalysis(finalNode, results, changes, cachedEndLiveLocals, {}, startOfKernel, pendingNodes);
    // repeat until there are no more pending nodes. Since we have a limited number of locals, this can never be an
    // infinite loop. It will always stop, latest when all locals are added to all blocks.
    while(!pendingNodes.empty())
    {
        auto node = *pendingNodes.begin();
        // remove before running the analysis since (e.g. on a 1 block loop) it could be added again with new locals
        pendingNodes.erase(pendingNodes.begin());
        // if the node is in the pendingNodes set, then it is also guaranteed that there is an entry in the
        // cachedEndLiveLocals map, since the map is written before the pendingNodes set.
        auto predCacheIt = cachedEndLiveLocals.find(node->key);
        runAnalysis(*node, results, changes, cachedEndLiveLocals, FastSet<const Local*>{predCacheIt->second},
            startOfKernel, pendingNodes);
    }
    PROFILE_END(GlobalLivenessAnalysis);
}

LCOV_EXCL_START
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
LCOV_EXCL_STOP

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
            if(auto op1 = combInst->getFirstOp())
                func(op1);
            if(auto op2 = combInst->getSecondOp())
                func(op2);
        }
        else
            func(inst.get());
    }

    for(const auto& pair : localsReadAfterWriting)
    {
        if(pair.first->countUsers(LocalUse::Type::READER) == pair.second)
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

bool LocalUsageRange::operator<(const LocalUsageRange& other) const noexcept
{
    if(local < other.local)
        return true;
    if(local > other.local)
        return false;
    if(startIndex < other.startIndex)
        return true;
    if(startIndex > other.startIndex)
        return false;
    return endIndex < other.endIndex;
}

LCOV_EXCL_START
std::string LocalUsageRange::to_string() const
{
    return local->to_string() + " [" + std::to_string(startIndex) + ", " + std::to_string(endIndex) + "] (" +
        std::to_string(numAccesses) + ")";
}
LCOV_EXCL_STOP

bool LocalUtilization::operator<(const LocalUtilization& other) const noexcept
{
    // sort less utilization to front
    if(rating > other.rating)
        return true;
    if(rating < other.rating)
        return false;
    // sort bigger range to the front
    if(numInstructions > other.numInstructions)
        return true;
    if(numInstructions < other.numInstructions)
        return false;
    // sort less accesses to the front
    if(numAccesses < other.numAccesses)
        return true;
    if(numAccesses > other.numAccesses)
        return false;
    return local < other.local;
}

LCOV_EXCL_START
std::string LocalUtilization::to_string() const
{
    return local->to_string() + ", " + std::to_string(numInstructions) + " instructions, " +
        std::to_string(numAccesses) + " accesses, " + std::to_string(numLoops) + " loops -> " + std::to_string(rating);
}
LCOV_EXCL_STOP

static std::size_t calculateRating(std::size_t numInstructions, std::size_t numAccesses, std::size_t numLoops)
{
    // weigh the loops the most, since a loop represents loop-count * #usages usages
    auto usage = numAccesses << numLoops;
    return numInstructions / std::max(usage, std::size_t{1});
}

static LocalUsageRange& mergeUsageRange(FastMap<const Local*, LocalUsageRange>& ranges, LocalUsageRange&& newRange)
{
    auto it = ranges.find(newRange.local);
    if(it != ranges.end())
    {
        it->second.startIndex = std::min(it->second.startIndex, newRange.startIndex);
        it->second.endIndex = std::max(it->second.endIndex, newRange.endIndex);
        it->second.numAccesses = std::max(it->second.numAccesses, newRange.numAccesses);
        it->second.maxUnaccessedRange = std::max(it->second.maxUnaccessedRange, newRange.maxUnaccessedRange);
    }
    else
        it = ranges.emplace(newRange.local, std::move(newRange)).first;
    return it->second;
}

static SortedSet<LocalUsageRange> determineUsageRanges(const BasicBlock& block, const LivenessAnalysis& liveness)
{
    // track the instruction index to have some integer value for our range
    std::size_t instructionIndex = 0;
    FastMap<const Local*, std::size_t> writeIndices;
    FastMap<const Local*, std::size_t> numAccesses;
    FastMap<const Local*, LocalUsageRange> localRanges;
    FastMap<const Local*, std::size_t> lastAccess;
    auto it = block.begin();
    for(auto loc : liveness.getStartResult())
        // all these locals are live from the beginning of the block
        writeIndices.emplace(loc, 0);
    // skip label
    ++it;
    ++instructionIndex;

    while(it != block.end())
    {
        if(!it->get())
        {
            ++it;
            ++instructionIndex;
            continue;
        }

        // track locals accessed in this instruction not mark e.g. locals moved-from (with or op-code) as two usages
        tools::SmallSortedPointerSet<const Local*> usedLocals;

        // We directly use the read/written locals instead of the LivenessChanges, since we anyway need to count all
        // usages and also the LivenessChanges do not handle phi-nodes in a way that is useful to us, e.g. a conditional
        // phi-node write where the other conditional write is in another block does (correctly) not change the local
        // liveness, but needs to be tracked here as write.
        (*it)->forUsedLocals([&](const Local* loc, LocalUse::Type type, const intermediate::IntermediateInstruction&) {
            if(usedLocals.find(loc) == usedLocals.end())
            {
                ++numAccesses[loc];
                usedLocals.emplace(loc);
            }

            if(has_flag(type, LocalUse::Type::READER))
            {
                auto writeIt = writeIndices.find(loc);
                if(writeIt == writeIndices.end())
                    // This should never happen, but we can simply catch it here by inserting a liveness from the
                    // beginning of the block.
                    writeIt = writeIndices.emplace(loc, 0).first;

                // this is the last read, insert range
                mergeUsageRange(localRanges,
                    LocalUsageRange{
                        loc, writeIt->second, instructionIndex, numAccesses[loc], instructionIndex - lastAccess[loc]});
                // remove write entry to start new range with next write
                writeIndices.erase(writeIt);
            }
            if(has_flag(type, LocalUse::Type::WRITER))
            {
                auto writeIt = writeIndices.find(loc);
                if(writeIt == writeIndices.end())
                    // local was not written yet in the block (or all usages read, so removed again), so add
                    // this instruction as reader
                    writeIndices.emplace(loc, instructionIndex);
            }
            lastAccess[loc] = instructionIndex;
        });

        ++it;
        ++instructionIndex;
    }

    // all entries left in the writer-index map have not been (fully) read, so add range to the end of the block
    for(const auto& entry : writeIndices)
        mergeUsageRange(localRanges,
            LocalUsageRange{entry.first, entry.second, instructionIndex, numAccesses[entry.first],
                instructionIndex - lastAccess[entry.first]});

    SortedSet<LocalUsageRange> ranges;
    for(auto& entry : localRanges)
    {
        // skip labels
        if(!entry.first->type.isLabelType())
            ranges.emplace(std::move(entry.second));
    }

    return ranges;
}

void LocalUsageRangeAnalysis::operator()(Method& method)
{
    PROFILE_START(LocalUsageRangeAnalysis);

    std::unique_ptr<GlobalLivenessAnalysis> globalLiveness;
    auto actualLiveness = livenessAnalysis;
    if(!livenessAnalysis)
    {
        globalLiveness.reset(new GlobalLivenessAnalysis(false));
        (*globalLiveness)(method);
        actualLiveness = globalLiveness.get();
    }

    auto loops = method.getCFG().findLoops(true);
    FastMap<const Local*, std::tuple<std::size_t, std::size_t, std::size_t>> intermediateSum;

    for(const auto& block : method)
    {
        auto blockRanges = determineUsageRanges(block, actualLiveness->getLocalAnalysis(block));

        auto numLoops = std::count_if(loops.begin(), loops.end(), [&](const auto& loop) -> bool {
            return std::find_if(loop.begin(), loop.end(), [&](const auto& node) { return node->key == &block; }) !=
                loop.end();
        });

        auto it = detailedRanges.emplace(&block, std::move(blockRanges)).first;
        // combine all ranges to an overall result
        for(const auto& entry : it->second)
        {
            auto& overall = intermediateSum[entry.local];
            std::get<0>(overall) += entry.size();
            std::get<1>(overall) += entry.numAccesses;
            // for loops, only track the locals actually accessed in the loops, not just live during it
            if(entry.numAccesses != 0)
                std::get<2>(overall) += static_cast<std::size_t>(numLoops);
        }
    }

    for(const auto& entry : intermediateSum)
        overallUsages.emplace(LocalUtilization{entry.first, std::get<0>(entry.second), std::get<1>(entry.second),
            std::get<2>(entry.second),
            calculateRating(std::get<0>(entry.second), std::get<1>(entry.second), std::get<2>(entry.second))});

    PROFILE_END(LocalUsageRangeAnalysis);
}

LCOV_EXCL_START
void LocalUsageRangeAnalysis::dumpResults(const Method& method) const
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        for(auto& entry : overallUsages)
            logging::debug() << entry.to_string() << logging::endl;
    });
}
LCOV_EXCL_STOP
