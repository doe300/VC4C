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
#include "GraphColoring.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;
using namespace vc4c::operators;

// TODO make steps: 1. only small fixes, 2. group locals, "spill" to vector, rematerialize, 3. spill into VPM
const std::vector<std::pair<std::string, RegisterFixupStep>> qpu_asm::FIXUP_STEPS = {
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
    {"Group scalar locals (conversative)",
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
            if(reader->checkOutputRegister() & &Register::isTextureMemoryUnit)
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
                    candidateLocals.emplace(pair.first, readIt->second.back());
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
            CPPLOG_LAZY(logging::Level::WARNING,
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
