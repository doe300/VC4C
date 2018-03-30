/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Normalizer.h"

#include "../BackgroundWorker.h"
#include "../InstructionWalker.h"
#include "../Method.h"
#include "../Module.h"
#include "../Profiler.h"
#include "../optimization/Eliminator.h"
#include "Inliner.h"

#include "log.h"

#include <string>
#include <vector>

using namespace vc4c;
using namespace vc4c::normalization;

/*
 * Dummy normalization step which asserts all remaining instructions are normalized
 */
static void checkNormalized(Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(it.has() && !it->isNormalized())
    {
        throw CompilationError(CompilationStep::NORMALIZER, "Not normalized instruction found", it->to_string());
    }
}

// NOTE: The order is on purpose and must not be changed!
const static std::vector<std::pair<std::string, NormalizationStep>> allSteps = {{"CheckNormalized", checkNormalized}};

static void runNormalizationStep(
    const NormalizationStep& step, Module& module, Method& method, const Configuration& config)
{
    for(auto& block : method)
    {
        auto it = block.begin();
        while(!it.isEndOfBlock())
        {
            step(module, method, it, config);
            it.nextInBlock();
        }
    }
}

void Normalizer::normalize(Module& module) const
{
    std::vector<BackgroundWorker> workers;
    workers.reserve(module.getKernels().size());

    // 1. eliminate phi on all methods
    for(auto& method : module)
    {
        // PHI-nodes need to be eliminated before inlining functions
        // since otherwise the phi-node is mapped to the initial label, not to the last label added by the functions
        // (the real end of the original, but split up block)
        PROFILE_COUNTER(
            vc4c::profiler::COUNTER_NORMALIZATION + 1, "Eliminate Phi-nodes (before)", method->countInstructions());
        optimizations::eliminatePhiNodes(module, *method.get(), config);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_NORMALIZATION + 2, "Eliminate Phi-nodes (after)",
            method->countInstructions(), vc4c::profiler::COUNTER_NORMALIZATION + 1);
    }
    // 2. inline kernel-functions
    for(Method* kernelFunc : module.getKernels())
    {
        Method& kernel = *kernelFunc;

        PROFILE_COUNTER(vc4c::profiler::COUNTER_NORMALIZATION + 4, "Inline (before)", kernel.countInstructions());
        inlineMethods(module, kernel, config);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_NORMALIZATION + 5, "Inline (after)",
            kernel.countInstructions(), vc4c::profiler::COUNTER_NORMALIZATION + 4);
    }
    // 3. run other normalization steps on kernel functions
    for(Method* kernelFunc : module.getKernels())
    {
        auto f = [kernelFunc, &module, this]() -> void { normalizeMethod(module, *kernelFunc); };
        workers.emplace(workers.end(), f, "Normalization")->operator()();
    }
    BackgroundWorker::waitForAll(workers);
}

void Normalizer::normalizeMethod(Module& module, Method& method) const
{
    logging::debug() << "-----" << logging::endl;
    logging::info() << "Running normalization passes for: " << method.name << logging::endl;
    std::size_t numInstructions = method.countInstructions();

    PROFILE_START(NormalizationPasses);
    for(const auto& step : allSteps)
    {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: " << step.first << logging::endl;
        PROFILE_START_DYNAMIC(step.first);
        runNormalizationStep(step.second, module, method, config);
        PROFILE_END_DYNAMIC(step.first);
    }
    PROFILE_END(NormalizationPasses);
    logging::info() << logging::endl;
    if(numInstructions != method.countInstructions())
    {
        logging::info() << "Normalization done, changed number of instructions from " << numInstructions << " to "
                        << method.countInstructions() << logging::endl;
    }
    else
    {
        logging::info() << "Normalization done" << logging::endl;
    }
    logging::debug() << "-----" << logging::endl;
}