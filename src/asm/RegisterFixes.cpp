/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegisterFixes.h"

#include "../Method.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/LivenessAnalysis.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
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
    // Try to group pointer parameters into vectors to save registers used
    {"Group parameters", groupParameters}};

FixupResult qpu_asm::groupParameters(Method& method, const Configuration& config, const GraphColoring& coloredGraph)
{
    analysis::LocalUsageRangeAnalysis localUsageRangeAnalysis(&coloredGraph.getLivenessAnalysis());
    localUsageRangeAnalysis(method);

    bool somethingChanged = false;

    FastAccessList<const Parameter*> parametersToBeGrouped;
    for(const auto& entry : localUsageRangeAnalysis.getOverallUsages())
    {
        if(entry.numLoops > 0)
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

        auto it = mappingIt->second.first;
        auto cond = assignNop(it) = selectSIMDElement(groupIndex);
        it.get<intermediate::ExtendedInstruction>()->setCondition(cond);
        it->setOutput(groupedValue);

        for(auto& readerIt : mappingIt->second.second)
        {
            // Doing a full vector rotation forces the now long-living parameter group into accumulator!! So create we a
            // temporary copy right away, since we will need to do so anyway
            auto tmpGroup = assign(readerIt, groupedValue->type, std::string{(*paramIt)->name}) = *groupedValue;

            auto tmp = method.addNewLocal((*paramIt)->type, (*paramIt)->name);
            readerIt = intermediate::insertVectorExtraction(
                readerIt, method, tmpGroup, Value(Literal(groupIndex), TYPE_INT8), tmp);
            readerIt->replaceLocal(*paramIt, tmp);
        }

        // paramIt is incremented in the loop header
        ++groupIndex;
        somethingChanged = true;
    }

    return somethingChanged ? FixupResult::FIXES_APPLIED_RECREATE_GRAPH : FixupResult::NOTHING_FIXED;
}
