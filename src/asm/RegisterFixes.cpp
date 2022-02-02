/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegisterFixes.h"

#include "../Method.h"
#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/LivenessAnalysis.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../normalization/LiteralValues.h"
#include "../periphery/VPM.h"
#include "GraphColoring.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;
using namespace vc4c::operators;

// TODO make steps: 1. only small fixes, 2. group locals, "spill" to vector, rematerialize, 3. spill into VPM
const std::vector<RegisterFixupStep> qpu_asm::FIXUP_STEPS = {
    // For the first two steps, try to run our in-graph fix-ups
    {"Small rewrites",
        [](Method& method, const Configuration& config, GraphColoring& coloredGraph) -> FixupResult {
            // even though we might have fixed some register errors, these fixed make sure the colored graph is up to
            // date with the changes, so we do not need to recreate it
            return coloredGraph.fixErrors() ? FixupResult::ALL_FIXED : FixupResult::FIXES_APPLIED_KEEP_GRAPH;
        }},
    {"Small rewrites",
        [](Method& method, const Configuration& config, GraphColoring& coloredGraph) -> FixupResult {
            // same as above
            return coloredGraph.fixErrors() ? FixupResult::ALL_FIXED : FixupResult::FIXES_APPLIED_KEEP_GRAPH;
        }},
    // Try to move some constant calculations closer to their usages
    {"Rematerialize constants", rematerializeConstants},
    // Try to group pointer parameters into vectors to save registers used
    {"Group parameters", groupParameters},
    // Try to group any scalar/pointer local into vectors to save registers used
    {"Group scalar locals (conservative)",
        [](Method& method, const Configuration& config, GraphColoring& coloredGraph) -> FixupResult {
            // In the first run, skip e.g. locals which are used inside of (nested) loops
            return groupScalarLocals(method, config, coloredGraph, true);
        }},
    {"Small rewrites",
        [](Method& method, const Configuration& config, GraphColoring& coloredGraph) -> FixupResult {
            // same as above
            return coloredGraph.fixErrors() ? FixupResult::ALL_FIXED : FixupResult::FIXES_APPLIED_KEEP_GRAPH;
        }},
    {"Group scalar locals (aggressive)",
        [](Method& method, const Configuration& config, GraphColoring& coloredGraph) -> FixupResult {
            return groupScalarLocals(method, config, coloredGraph, false);
        }},
    {"Spill locals", spillLocals},
    {"Small rewrites",
        [](Method& method, const Configuration& config, GraphColoring& coloredGraph) -> FixupResult {
            // same as above
            return coloredGraph.fixErrors() ? FixupResult::ALL_FIXED : FixupResult::FIXES_APPLIED_KEEP_GRAPH;
        }},
};

/**
 * We currently have no concept of "vector of pointers", but is some cases we have a pointer value (which is by default
 * a scalar value) where we use the upper vector elements (if the value is a splat value), e.g. to calculate the
 * addresses of the upper TMU elements to load.
 * Since combining multiple "scalar" pointer locals into a single SIMD vector and then extraction the corresponding
 * elements again destroys the splat property, we need to check for such usage.
 */
static bool isUsedAsSplatValue(const Local* loc)
{
    FastSet<const Local*> openSet;
    FastSet<const Local*> closedSet;

    openSet.emplace(loc);

    while(!openSet.empty())
    {
        auto local = *openSet.begin();
        closedSet.emplace(local);
        openSet.erase(openSet.begin());

        bool multipleElementsMayBeUsed = false;

        local->forUsers(LocalUse::Type::READER, [&](const LocalUser* reader) {
            // TODO can be improved by e.g. checking for single-element insertion into other local (if we only
            // insert 0th element, we have no problem)
            if(reader->doesSetFlag())
                multipleElementsMayBeUsed = true;
            else if(reader->checkOutputRegister() & &Register::isTextureMemoryUnit)
                multipleElementsMayBeUsed = true;
            else if(reader->checkOutputRegister() & &Register::isSpecialFunctionsUnit)
                multipleElementsMayBeUsed = true;
            else if(auto outputLoc = reader->checkOutputLocal())
            {
                if(closedSet.find(outputLoc) == closedSet.end())
                    openSet.emplace(outputLoc);
            }
        });

        if(multipleElementsMayBeUsed)
            return true;
    }

    return false;
}

FixupResult qpu_asm::groupParameters(Method& method, const Configuration& config, GraphColoring& coloredGraph)
{
    analysis::LocalUsageRangeAnalysis localUsageRangeAnalysis(&coloredGraph.getLivenessAnalysis());
    localUsageRangeAnalysis(method);

    bool somethingChanged = false;

    FastAccessList<const Parameter*> parametersToBeGrouped;
    for(const auto& entry : localUsageRangeAnalysis.getOverallUsages())
    {
        if(entry.numLoops > 1)
            continue;
        // TODO some better values?
        if(entry.numAccesses > 3)
            // don't group parameter with too many accessed, would need to rotate too often
            continue;
        if(entry.numInstructions <= 32)
            // don't group parameter with too short a usage range, we would likely not gain anything
            continue;
        if(auto param = entry.local->as<Parameter>())
        {
            if(!param->type.getPointerType())
                continue;
            if(!dynamic_cast<const intermediate::ExtendedInstruction*>(param->getSingleWriter()))
                // this should not happen for any parameter, but just to be sure
                continue;
            parametersToBeGrouped.emplace_back(param);
        }
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Trying to group " << parametersToBeGrouped.size() << " parameters..." << logging::endl);

    Optional<Value> groupedValue = NO_VALUE;
    uint8_t groupIndex = 0;

    // Creating this lookup map is more efficient than searching every instruction in the whole kernel function
    FastMap<const Parameter*, std::pair<InstructionWalker, FastAccessList<InstructionWalker>>> instructionMapping;
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            for(auto param : parametersToBeGrouped)
            {
                if(it->writesLocal(param))
                    instructionMapping[param].first = it;
                if(it->readsLocal(param))
                    instructionMapping[param].second.emplace_back(it);
            }
            it.nextInBlock();
        }
    }

    // the parameter at which to stop if a new group would be created
    auto endParam = parametersToBeGrouped.end();
    if(!parametersToBeGrouped.empty() && parametersToBeGrouped.size() < 3)
        endParam = parametersToBeGrouped.begin();
    else
        endParam = parametersToBeGrouped.end() - 3;

    for(auto paramIt = parametersToBeGrouped.begin(); paramIt != parametersToBeGrouped.end(); ++paramIt)
    {
        auto mappingIt = instructionMapping.find(*paramIt);
        if(mappingIt == instructionMapping.end() || mappingIt->second.first.isEndOfBlock() ||
            mappingIt->second.second.empty())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Skipping grouping of parameter: " << (*paramIt)->to_string() << logging::endl);
            continue;
        }

        if(!groupedValue || groupIndex == NATIVE_VECTOR_SIZE)
        {
            if(paramIt >= endParam)
                // don't create an extra group for the last few parameters left
                break;
            // XXX actual type should be <16 x void*>, but can't form vector of pointers
            groupedValue = method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%param_group");
            groupIndex = 0;
        }

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Grouping parameter '" << (*paramIt)->to_string() << "' into " << groupedValue->to_string()
                << " at position: " << static_cast<uint32_t>(groupIndex) << logging::endl);
        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Parameters grouped", 1);

        // Parameter writes always read UNIFORMs which are splat values, so we can simply select the desired element
        auto it = mappingIt->second.first;
        auto cond = assignNop(it) = selectSIMDElement(groupIndex);
        it.get<intermediate::ExtendedInstruction>()->setCondition(cond);
        it->setOutput(groupedValue);
        it->decoration = remove_flag(it->decoration, intermediate::InstructionDecorations::IDENTICAL_ELEMENTS);
        it->addDecorations(intermediate::InstructionDecorations::ELEMENT_INSERTION);

        for(auto& readerIt : mappingIt->second.second)
        {
            // Doing a full vector rotation forces the now long-living parameter group into accumulator!! So create we a
            // temporary copy right away, since we will need to do so anyway
            auto tmpGroup = assign(readerIt, groupedValue->type, std::string{(*paramIt)->name}) = *groupedValue;

            auto tmp = method.addNewLocal((*paramIt)->type, (*paramIt)->name);
            readerIt = intermediate::insertVectorExtraction(
                readerIt, method, tmpGroup, Value(SmallImmediate(groupIndex), TYPE_INT8), tmp);
            readerIt->replaceLocal(*paramIt, tmp);
            if(isUsedAsSplatValue(tmp.local()))
            {
                // we need to recreate the splat property by replicating the value
                auto splatTmp = method.addNewLocal((*paramIt)->type, (*paramIt)->name);
                readerIt = intermediate::insertReplication(readerIt, tmp, splatTmp);
                readerIt->replaceLocal(tmp.local(), splatTmp);
            }
        }

        // paramIt is incremented in the loop header
        ++groupIndex;
        somethingChanged = true;
    }

    return somethingChanged ? FixupResult::FIXES_APPLIED_RECREATE_GRAPH : FixupResult::NOTHING_FIXED;
}

static std::pair<const Local*, uint8_t> reserveGroupSpace(
    Method& method, FastMap<const Local*, std::array<const Local*, NATIVE_VECTOR_SIZE>>& groups, const Local* loc)
{
    for(auto& group : groups)
    {
        for(std::size_t i = 0; i < group.second.size(); ++i)
        {
            if(group.second[i] == nullptr)
            {
                group.second[i] = loc;
                return std::make_pair(group.first, static_cast<uint8_t>(i));
            }
        }
    }
    // insert new group
    auto group = method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%local_group").local();
    groups[group].fill(nullptr);
    groups[group][0] = loc;
    return std::make_pair(group, 0);
}

FixupResult qpu_asm::groupScalarLocals(
    Method& method, const Configuration& config, const GraphColoring& coloredGraph, bool runConservative)
{
    FastMap<const Local*, InstructionWalker> candidateLocals;
    FastMap<const Local*, FastSet<InstructionWalker>> localReaders;
    for(auto& block : method)
    {
        FastMap<const Local*, InstructionWalker> localsWrittenInBlock;
        // the value is a list (as in not-unique) on purpose to keep the count correct for i.e. combined instructions
        FastMap<const Local*, FastAccessList<InstructionWalker>> localsReadInBlock;
        for(auto it = block.walk(); !it.isEndOfBlock(); it.nextInBlock())
        {
            if(!it.get())
                continue;
            it->forUsedLocals(
                [&](const Local* loc, LocalUse::Type use, const intermediate::IntermediateInstruction& instr) {
                    if(loc->type.isLabelType() || (!loc->type.isScalarType() && !loc->type.getPointerType()))
                        // XXX out of simplicity, for now only handle scalar values
                        return;
                    if(has_flag(use, LocalUse::Type::WRITER))
                    {
                        if(!instr.hasConditionalExecution() && loc->getSingleWriter() == &instr &&
                            !instr.hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                            // track only unconditionally written locals with a single writer
                            // XXX otherwise we would need to write-back the updated result into the group vector
                            // also don't track phi-writes, since they are always used immediately after the branch
                            // destination label and therefore don't really have a large usage range
                            localsWrittenInBlock.emplace(loc, it);
                    }
                    if(has_flag(use, LocalUse::Type::READER))
                    {
                        localsReadInBlock[loc].emplace_back(it);
                        localReaders[loc].emplace(it);
                    }
                });
        }

        for(auto& pair : localsWrittenInBlock)
        {
            auto readIt = localsReadInBlock.find(pair.first);
            if(readIt != localsReadInBlock.end())
            {
                if(readIt->second.size() == pair.first->countUsers(LocalUse::Type::READER))
                    // exclude all locals which have all readers inside the block they are written in - locals which are
                    // only live inside a single block
                    // TODO to simplify, could skip all locals which have any readers inside the block they are
                    // written?! this would reduce the number of locals grouped and therefore the number of registers
                    // freed, but also the number of instructions inserted
                    continue;

                if(!readIt->second.empty())
                {
                    // if the local is read inside this and other blocks, do some optimization
                    // 1. remove all reads inside this block from the tracked reads, so we do not spill/rematerialize
                    // inside a single block
                    auto& readers = localReaders[pair.first];
                    for(auto& inst : readIt->second)
                        readers.erase(inst);
                    // 2. move the position to insert the spilling code after the last read
                    auto insertIt = readIt->second.back();
                    if(insertIt.has() && insertIt->doesSetFlag())
                        // ... unless it sets flags, then move before that to not override the flags with out insertion
                        // flags
                        insertIt.previousInBlock();
                    candidateLocals.emplace(pair.first, insertIt);
                }
            }
            candidateLocals.emplace(pair);
        }
    }

    if(runConservative)
    {
        analysis::LocalUsageRangeAnalysis localUsageRangeAnalysis(&coloredGraph.getLivenessAnalysis());
        localUsageRangeAnalysis(method);

        for(const auto& entry : localUsageRangeAnalysis.getOverallUsages())
        {
            auto candIt = candidateLocals.find(entry.local);
            if(candIt == candidateLocals.end())
                continue;
            if(entry.numLoops > 1)
                // skip locals used in nested loops
                candidateLocals.erase(candIt);
            else if(entry.numAccesses > 8)
                // skip locals with too many accesses
                candidateLocals.erase(candIt);
        }
    }

    if(candidateLocals.size() < 4)
        // for 0 and 1 locals this does not help anything at all anyway...
        return FixupResult::NOTHING_FIXED;

    // map of "original" local -> group local (+ index in group) where it is located in
    FastMap<const Local*, std::pair<const Local*, uint8_t>> currentlyGroupedLocals;
    // map of group local -> contained "original" locals and their positions
    FastMap<const Local*, std::array<const Local*, NATIVE_VECTOR_SIZE>> groups;

    for(auto& entry : candidateLocals)
    {
        // allocate an element in our spill register
        auto pos = reserveGroupSpace(method, groups, entry.first);
        currentlyGroupedLocals.emplace(entry.first, pos);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Grouping local '" << entry.first->to_string() << "' into " << pos.first->to_string() << " at index "
                << static_cast<unsigned>(pos.second) << logging::endl);

        // after the local write (or the last read within the writing block), insert the spill code
        // we spill a copy to not force the local to an accumulator (since we do full vector rotation)
        {
            auto spillIt = entry.second.nextInBlock();
            auto tmpValue = assign(spillIt, entry.first->type) = entry.first->createReference();
            spillIt = intermediate::insertVectorInsertion(
                spillIt, method, pos.first->createReference(), Value(SmallImmediate(pos.second), TYPE_INT8), tmpValue);
            if(!spillIt.isEndOfBlock() && spillIt.get() && spillIt->readsLocal(pos.first))
                // if we happen to insert a write just before a read of the same container, insert a NOP to prevent the
                // container to be forced to an accumulator
                nop(spillIt, intermediate::DelayType::WAIT_REGISTER);
        }

        // before all reads (not located in the writing block), insert a dematerialization and use a new temporary
        // local in the read
        auto& readers = localReaders[entry.first];
        for(auto reader : readers)
        {
            auto checkIt = reader.copy().previousInBlock();
            if(!checkIt.isEndOfBlock() && checkIt.get() && checkIt->writesLocal(pos.first))
                // if we happen to insert a read just after a write of the same container, insert a NOP to prevent the
                // container to be forced to an accumulator
                nop(reader, intermediate::DelayType::WAIT_REGISTER);
            // need to extract from temporary container to not force the group container to an accumulator
            auto tmpValue = method.addNewLocal(entry.first->type, entry.first->name);
            auto tmpContainer = assign(reader, pos.first->type) = pos.first->createReference();
            reader = intermediate::insertVectorExtraction(
                reader, method, tmpContainer, Value(SmallImmediate(pos.second), TYPE_INT8), tmpValue);
            if(reader->hasUnpackMode() || reader->readsLiteral() || reader->getVectorRotation())
                // insert a NOP before the  actually reading instruction to allow for e.g. unpack-modes
                nop(reader, intermediate::DelayType::WAIT_REGISTER);
            reader->replaceLocal(entry.first, tmpValue);
            if(isUsedAsSplatValue(tmpValue.local()))
            {
                // we need to recreate the splat property by replicating the value
                auto splatTmp = method.addNewLocal(entry.first->type, entry.first->name);
                reader = intermediate::insertReplication(reader, tmpValue, splatTmp);
                // XXX could improve by only inserting this NOP if required (e.g. new local can't be on accumulator)
                nop(reader, intermediate::DelayType::WAIT_REGISTER);
                reader->replaceLocal(tmpValue.local(), splatTmp);
            }
        }
    }
    return FixupResult::FIXES_APPLIED_RECREATE_GRAPH;
}

FixupResult qpu_asm::rematerializeConstants(Method& method, const Configuration& config, GraphColoring& coloredGraph)
{
    // Simplified version, move constant loads (e.g. as restructured by loop invariant code motion) back close to their
    // usages
    auto it = method.walkAllInstructions();
    FastMap<const Local*, InstructionWalker> constants;
    while(!it.isEndOfMethod())
    {
        // 1. Collect constant loads/calculations which are used exactly once (XXX for simplicity only for now)
        auto constantValue = NO_VALUE;
        if(it.has() && it->isConstantInstruction() && it->checkOutputLocal() &&
            (constantValue = it->precalculate().first) &&
            it->checkOutputLocal()->countUsers(LocalUse::Type::READER) == 1 &&
            // if the local is locally limited, moving it (at most by a few instructions) won't have any big effect
            !it.getBasicBlock()->isLocallyLimited(
                it, it->checkOutputLocal(), config.additionalOptions.accumulatorThreshold))
        {
            constants.emplace(it->checkOutputLocal(), it);
        }

        // 2. Move constant loads/calculations closer to their usages
        // NOTE: This algorithm relies on a constant load being defined (in linear instruction order) before its usage,
        // even across blocks. Except for some rare cases of reordered blocks, this should almost always be true (since
        // blocks are sorted by domination) and safes us from iterating over all instructions twice.
        tools::SmallSortedPointerSet<const Local*> usedConstants;
        it->forReadLocals(
            [&usedConstants, &constants](const Local* loc, const intermediate::IntermediateInstruction& inst) {
                if(constants.find(loc) != constants.end())
                    usedConstants.emplace(loc);
            });
        for(const auto* loc : usedConstants)
        {
            auto constantIt = constants.at(loc);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Moving constant calculation close to its single user: " << constantIt->to_string()
                    << logging::endl);
            auto decorations = it.emplace(constantIt.release()).decoration;
            constantIt.safeErase(decorations);
            it.nextInBlock();
            PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Constants rematerialized", 1);
        }

        it.nextInMethod();
    }

    /*
     * TODO extended version:
     *
     * - allow for moving constant loads with multiple (clustered, e.g. within single block) readers
     *
     * Actual rematerilization:
     * 1. determine constant calculations as only writer to output locals
     * 2. determine all reads of the local and the distance to the calculation
     * 2.x filter locals read across multiple blocks or with already small access range
     * 3. remove instruction (or if is MANDATORY_DELAY, replace with NOP)
     * 4. insert calculating instruction close to usage
     * 4.1 if usage-range is > accumulatorThreshold, keep delay instruction
     *
     * More general:
     * Don't only do for constants, but more general for every operation (where the gain is large enough, e.g. distance
     * is > some factor * users) where all operands are also available close to their users. Remove original
     * calculation, insert close to users (with different locals).
     */

    return constants.empty() ? FixupResult::NOTHING_FIXED : FixupResult::FIXES_APPLIED_RECREATE_GRAPH;
}

/**
 * "Spill cost is the loads and stores needed. Weighted by scope - i.e. avoid inner loops"
 * "The higher the degree of a node to spill the greater the chance that it will help colouring"
 * Source: https://www.inf.ed.ac.uk/teaching/courses/copt/lecture-7.pdf
 *
 * => Prefer spilling locals with smaller rating (lower cost, greater possible gain)
 */
static float calculateRating(
    const LocalUsage& localUsage, const analysis::LoopInclusionTree& inclusionTree, const ColoredNode& node)
{
    uint32_t accumulatedCosts = 0;
    for(const auto& it : localUsage.associatedInstructions)
    {
        uint32_t depth = 0;
        for(const auto& loop : inclusionTree.getNodes())
        {
            if(loop.first->findInLoop(it))
                depth = std::max(depth, loop.second.getLongestPathToRoot());
        }
        accumulatedCosts += 1 + depth;
    }
    // TODO somehow also regard the distance (across blocks) between reads and writes
    return static_cast<float>(accumulatedCosts) / static_cast<float>(node.getEdgesSize());
}

static bool isInMutexLock(InstructionWalker it)
{
    while(!it.isStartOfBlock())
    {
        if(auto mutex = it.get<intermediate::MutexLock>())
            return mutex->locksMutex();
        it.previousInBlock();
    }
    return false;
}

static const LocalUser* findPreviousAccess(
    InstructionWalker it, const tools::SmallSortedPointerMap<const LocalUser*, LocalUse>& users, std::size_t threshold)
{
    it.previousInBlock();
    auto numInstructionsLeft = threshold;
    while(!it.isStartOfBlock() && numInstructionsLeft > 0)
    {
        if(it.has())
        {
            if(users.find(it.get()) != users.end())
                return it.get();
            --numInstructionsLeft;
        }
        it.previousInBlock();
    }
    return nullptr;
}

struct LocalAccessGroup
{
    InstructionWalker mainAccess;
    LocalUse mainAccessUse;
    tools::SmallSortedPointerSet<intermediate::IntermediateInstruction*> additionalReaders;
};

/**
 * Group accesses to the spilled local which are close to each other to not insert unnecessary spill/unspill accesses.
 */
static std::vector<LocalAccessGroup> groupSpillAccesses(
    const tools::SmallSortedPointerMap<const LocalUser*, LocalUse>& users,
    const FastMap<const intermediate::IntermediateInstruction*, InstructionWalker>& instructionMapping,
    std::size_t threshold)
{
    // mapping of access to its previous access
    tools::SmallSortedPointerMap<const LocalUser*, const LocalUser*> accessOrder;
    // mapping of first entry in group to additional readers following shortly after
    FastMap<const LocalUser*, tools::SmallSortedPointerSet<LocalUser*>> groups;

    for(const auto& user : users)
    {
        auto it = instructionMapping.at(user.first);
        if(auto previousAccess = !user.second.writesLocal() ? findPreviousAccess(it, users, threshold) : nullptr)
        {
            accessOrder.emplace(user.first, previousAccess);
            groups[previousAccess].emplace(it.get());
        }
        else
            // single-element group
            groups.emplace(user.first, tools::SmallSortedPointerSet<LocalUser*>{});
    }

    // Since the users are not processed in-order (of the control flow), we need to recursively merge the groups
    bool hasChanges = true;
    while(hasChanges)
    {
        hasChanges = false;
        for(auto& entry : accessOrder)
        {
            auto previousIt = accessOrder.find(entry.second);
            if(previousIt != accessOrder.end())
            {
                hasChanges = true;
                groups.erase(entry.second);
                entry.second = previousIt->second;
                groups[previousIt->second].emplace(const_cast<LocalUser*>(entry.first));
            }
        }
    }
    if(groups.size() <= 1)
    {
        // Sanity check, should never happen due to how we select locals to spill, but just to be sure don't spill
        // locals which are only used in a single group
        return {};
    }
    std::vector<LocalAccessGroup> result;
    result.reserve(groups.size());
    for(auto& group : groups)
    {
        result.emplace_back(
            LocalAccessGroup{instructionMapping.at(group.first), users.at(group.first), std::move(group.second)});
    }
    return result;
}

FixupResult qpu_asm::spillLocals(Method& method, const Configuration& config, GraphColoring& coloredGraph)
{
    static constexpr std::size_t MINIMUM_THRESHOLD = 32; /* TODO some better limit */

    auto loops = method.getCFG().findLoops(true, false);
    auto loopInclusions = analysis::createLoopInclusionTree(loops);

    SortedMap<float, tools::SmallSortedPointerSet<const Local*>> spillCandidates;

    for(const auto& entry : coloredGraph.getLocalUses())
    {
        if(entry.second.associatedInstructions.size() < 2)
            // not used (properly), don't need to be spilled
            continue;
        auto& graphNode = coloredGraph.getGraph().assertNode(entry.first);
        if(graphNode.getEdgesSize() < 32) // XXX better limit?
            // should definitively be colorable
            continue;
        if(entry.first->countUsers(LocalUse::Type::WRITER) > 1)
            // XXX for now don't spill locals written multiple times, makes spilling more complicated
            continue;
        auto firstUseIt = *entry.second.associatedInstructions.begin();
        if(firstUseIt.getBasicBlock()->isLocallyLimited(firstUseIt, entry.first, MINIMUM_THRESHOLD))
            // too small usage range, don't spill
            continue;

        spillCandidates[calculateRating(entry.second, *loopInclusions, graphNode)].emplace(entry.first);
    }

    bool spilledLocals = false;
    bool noMoreSpace = false;

    FastMap<const intermediate::IntermediateInstruction*, InstructionWalker> instructionMapping(
        method.countInstructions());

    FastMap<const periphery::VPMArea*, tools::SmallSortedPointerSet<const ColoredNode*>> spilledAreas;

    for(const auto& entry : spillCandidates)
    {
        for(const auto* loc : entry.second)
        {
            const periphery::VPMArea* spillArea = nullptr;
            const auto& localNode = coloredGraph.getGraph().assertNode(loc);
            for(const auto& entry : spilledAreas)
            {
                // Try to find an already allocated spill are which is not used by any local interfering with the
                // current local ...
                if(std::none_of(entry.second.begin(), entry.second.end(),
                       [&localNode](const ColoredNode* spilledNode) { return localNode.isAdjacent(spilledNode); }))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Reusing spill VPM rows for '" << loc->name << "', previously used by: "
                            << to_string<const ColoredNode*>(
                                   entry.second, [](const ColoredNode* node) { return node->key->to_string(); })
                            << logging::endl);
                    spillArea = entry.first;
                    break;
                }
            }

            if(!spillArea)
                // ... otherwise try to allocate a new spill area
                spillArea = method.vpm->addSpillArea(method.metaData.getMaximumInstancesCount());
            if(!spillArea)
            {
                CPPLOG_LAZY(logging::Level::DEBUG, log << "No more space in VPM, aborting spilling!" << logging::endl);
                noMoreSpace = true;
                break;
            }

            spilledAreas[spillArea].emplace(&localNode);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Spilling local '" << loc->name << "' with rating: " << entry.first << logging::endl);

            if(instructionMapping.empty())
            {
                // create this mapping once to not need to iterate over all instructions again and again
                for(auto it = method.walkAllInstructions(); !it.isEndOfMethod(); it.nextInMethod())
                {
                    if(auto combinedOp = it.get<intermediate::CombinedOperation>())
                    {
                        if(auto op = combinedOp->getFirstOp())
                            instructionMapping.emplace(op, it);
                        if(auto op = combinedOp->getSecondOp())
                            instructionMapping.emplace(op, it);
                    }
                    else if(it.has())
                        instructionMapping.emplace(it.get(), it);
                }
            }

            // since we modify the users, need to create a copy first
            auto accessGroups =
                groupSpillAccesses(loc->getUsers(), instructionMapping, config.additionalOptions.accumulatorThreshold);
            for(auto& group : accessGroups)
            {
                auto it = group.mainAccess;
                bool mutexAlreadyLocked = isInMutexLock(it);
                // Since the qpu_number register is on register-file B, need to split the offset calculation up
                auto qpuNum = assign(it, TYPE_INT8, "%spill_qpu_offset") = Value(REG_QPU_NUMBER, TYPE_INT8);
                auto offset = assign(it, TYPE_INT8, "%spill_qpu_offset") = qpuNum * 64_lit; // 16 elements * 4Byte
                Value groupValue = UNDEFINED_VALUE;
                auto groupValueType =
                    (loc->type.getPointerType() ? TYPE_INT32 : loc->type).toVectorType(NATIVE_VECTOR_SIZE);
                if(group.mainAccessUse.readsLocal())
                {
                    // insert read from VPM before access and replace the local with a temporary
                    groupValue = method.addNewLocal(groupValueType, loc->name, "spill_read");
                    auto beforeIt = it.copy().previousInBlock();
                    it = method.vpm->insertReadVPM(method, it, groupValue, *spillArea, !mutexAlreadyLocked, offset);
                    it->replaceValue(loc->createReference(), groupValue, LocalUse::Type::READER);
                    auto lowerIt = beforeIt;
                    while(lowerIt != it && !lowerIt.isEndOfBlock())
                        lowerIt = periphery::lowerVPMAccess(method, lowerIt).nextInBlock();
                    while(beforeIt != it && !beforeIt.isEndOfBlock())
                        beforeIt =
                            normalization::handleImmediate(method.module, method, beforeIt, config).nextInBlock();
                }
                if(group.mainAccessUse.writesLocal())
                {
                    // insert write to VPM after access and replace the local with a temporary
                    groupValue = method.addNewLocal(groupValueType, loc->name, "spill_write");
                    auto beforeIt = it.copy().previousInBlock();
                    it->replaceValue(loc->createReference(), groupValue, LocalUse::Type::WRITER);
                    it.nextInBlock();
                    it = method.vpm->insertWriteVPM(method, it, groupValue, *spillArea, !mutexAlreadyLocked, offset);
                    auto lowerIt = beforeIt;
                    while(lowerIt != it && !lowerIt.isEndOfBlock())
                        lowerIt = periphery::lowerVPMAccess(method, lowerIt).nextInBlock();
                    while(beforeIt != it && !beforeIt.isEndOfBlock())
                        beforeIt =
                            normalization::handleImmediate(method.module, method, beforeIt, config).nextInBlock();
                }
                for(auto* reader : group.additionalReaders)
                {
                    reader->replaceLocal(loc, groupValue, LocalUse::Type::READER);
                }
            }

            PROFILE_COUNTER_SCOPE(
                vc4c::profiler::COUNTER_BACKEND, "Locals spilled", method.metaData.getMaximumInstancesCount());
            spilledLocals = true;
        }
        if(noMoreSpace)
            break;
    }

    CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, method.vpm->dumpUsage());
    method.dumpInstructions();
    return spilledLocals ? FixupResult::FIXES_APPLIED_RECREATE_GRAPH : FixupResult::NOTHING_FIXED;
}
