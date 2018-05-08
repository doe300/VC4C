/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Optimizer.h"

#include "../BackgroundWorker.h"
#include "../Profiler.h"
#include "../intrinsics/Intrinsics.h"
#include "Combiner.h"
#include "ControlFlow.h"
#include "Eliminator.h"
#include "LocalCompression.h"
#include "Reordering.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

OptimizationPass::OptimizationPass(
    const std::string& name, const std::string& parameterName, const Pass& pass, const std::string& description) :
    name(name),
    parameterName(parameterName), description(description), pass(pass)
{
}

bool OptimizationPass::operator()(const Module& module, Method& method, const Configuration& config) const
{
    return pass(module, method, config);
}

OptimizationStep::OptimizationStep(const std::string& name, const Step& step) : name(name), step(step) {}

InstructionWalker OptimizationStep::operator()(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config) const
{
    return step(module, method, it, config);
}

static const std::vector<OptimizationStep> SINGLE_STEPS = {
    // eliminates useless branches (e.g. jumps to the next instruction)
    OptimizationStep("EliminateUselessBranch", eliminateUselessBranch),
    // combine consecutive instructions writing the same local with a value and zero depending on some flags
    OptimizationStep("CombineSelectionWithZero", combineSelectionWithZero),
    // combine successive setting of the same flags
    OptimizationStep("CombineSettingSameFlags", combineSameFlags)};

static bool runSingleSteps(const Module& module, Method& method, const Configuration& config)
{
    auto& s = (logging::debug() << "Running steps: ");
    for(const OptimizationStep& step : SINGLE_STEPS)
        s << step.name << ", ";
    s << logging::endl;

    // since an optimization-step can be run on the result of the previous step,
    // we can't just pass the resulting iterator (pointing behind the optimization result) into the next
    // optimization-step  but since lists do not reallocate elements at inserting/removing, we can re-use the previous
    // iterator
    auto it = method.walkAllInstructions();
    // this construct with previous iterator is required, because the iterator could be invalidated (if the underlying
    // node is removed)
    auto prevIt = it;
    while(!it.isEndOfMethod())
    {
        for(const OptimizationStep& step : SINGLE_STEPS)
        {
            PROFILE_START_DYNAMIC(step.name);
            auto newIt = step(module, method, it, config);
            // we can't just test newIt == it here, since if we replace the content of the iterator instead of deleting
            // it, the iterators are still the same, even if we emplace instructions before
            if(newIt.copy().previousInMethod() != prevIt || newIt != it)
                it = prevIt;
            PROFILE_END_DYNAMIC(step.name);
        }
        it.nextInMethod();
        prevIt = it.copy().previousInMethod();
    }

    // XXX
    return false;
}

static bool generalOptimization(const Module& module, Method& method, const Configuration& config)
{
    using pass = std::function<bool(const Module& module, Method& method, const Configuration& config)>;
    using step =
        std::function<bool(const Module& module, Method& method, InstructionWalker& it, const Configuration& config)>;
    static std::vector<pass> PASS = {translateToMove, eliminateRedundantMoves, eliminateRedundantBitOp, propagateMoves};
    static std::vector<step> SINGLE = {calculateConstantInstruction, eliminateUselessInstruction};
    bool moved;

    for(int i = 0; i < 1000; i++)
    {
        moved = false;
        auto it = method.walkAllInstructions();
        while(!it.isEndOfMethod())
        {
            for(const step& s : SINGLE)
            {
                moved = moved || s(module, method, it, config);
            }
            it.nextInMethod();
        }

        for(const pass& p : PASS)
        {
            moved = moved || p(module, method, config);
        }

        if(!moved)
            break;
    }

    // this is only done when there are no more optimizations to do
    return false;
}

Optimizer::Optimizer(const Configuration& config) : config(config)
{
    auto enabledPasses = getPasses(config.optimizationLevel);
    for(const OptimizationPass& pass : ALL_PASSES)
    {
        if(config.additionalDisabledOptimizations.find(pass.parameterName) !=
            config.additionalDisabledOptimizations.end())
            continue;
        if(config.additionalEnabledOptimizations.find(pass.parameterName) !=
            config.additionalEnabledOptimizations.end())
            passes.emplace_back(&pass);
        if(enabledPasses.find(pass.parameterName) != enabledPasses.end())
            passes.emplace_back(&pass);
    }
}

static void runOptimizationPasses(const Module& module, Method& method, const Configuration& config,
    const std::vector<const OptimizationPass*>& passes)
{
    logging::debug() << "-----" << logging::endl;
    logging::info() << "Running optimization passes for: " << method.name << logging::endl;
    std::size_t numInstructions = method.countInstructions();

    std::size_t index = 0;
    for(const OptimizationPass* pass : passes)
    {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: " << pass->name << logging::endl;
        PROFILE_COUNTER(
            vc4c::profiler::COUNTER_OPTIMIZATION + index, pass->name + " (before)", method.countInstructions());
        PROFILE_START_DYNAMIC(pass->name);
        (*pass)(module, method, config);
        PROFILE_END_DYNAMIC(pass->name);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_OPTIMIZATION + index + 10, pass->name + " (after)",
            method.countInstructions(), vc4c::profiler::COUNTER_OPTIMIZATION + index);
        index += 100;
    }
    logging::info() << logging::endl;
    if(numInstructions != method.countInstructions())
    {
        logging::info() << "Optimizations done, changed number of instructions from " << numInstructions << " to "
                        << method.countInstructions() << logging::endl;
    }
    else
    {
        logging::info() << "Optimizations done" << logging::endl;
    }
    logging::debug() << "-----" << logging::endl;
    method.dumpInstructions();
}

void Optimizer::optimize(Module& module) const
{
    std::vector<BackgroundWorker> workers;
    workers.reserve(module.getKernels().size());
    for(Method* kernelFunc : module.getKernels())
    {
        auto f = [kernelFunc, &module, this]() -> void { runOptimizationPasses(module, *kernelFunc, config, passes); };
        workers.emplace(workers.end(), f, "Optimizer")->operator()();
    }
    BackgroundWorker::waitForAll(workers);
}

const std::vector<OptimizationPass> Optimizer::ALL_PASSES = {
    /*
     * The first optimizations run modify the control-flow of the method.
     * After this block of optimizations is run, the CFG of the method is stable (does not change anymore)
     */
    OptimizationPass("CombineDuplicateBranches", "combine-branches", combineDuplicateBranches,
        "combines successive branches to the same label"),
    OptimizationPass("MergeBasicBlocks", "merge-blocks", mergeAdjacentBasicBlocks,
        "merges adjacent basic blocks if there are no other conflicting transitions"),
    /*
     * The second block executes optimizations only within a single basic block
     */
    OptimizationPass("SingleSteps", "single-steps", runSingleSteps,
        "runs all the single-step optimizations. Combining them results in fewer iterations over the instructions"),
    // OptimizationPass("CompressWorkGroupInfo", "compress-work-group-info", compressWorkGroupLocals,
    //    "compresses work-group info into single local"),
    OptimizationPass("CombineRotations", "combine-rotations", combineVectorRotations,
        "combines duplicate vector rotations, e.g. introduced by vector-shuffle into a single rotation"),
    OptimizationPass("GeneralOptimizations", "general-optimizations", generalOptimization, "TODO"),
    OptimizationPass("EliminateDeadStores", "eliminate-dead-store", eliminateDeadStore,
        "eliminates useless instructions (dead store, move to same, redundant arithmetic operations, ...)"),
    OptimizationPass("VectorizeLoops", "vectorize-loops", vectorizeLoops, "vectorizes loops"),
    OptimizationPass("SplitReadAfterWrites", "split-read-write", splitReadAfterWrites,
        "splits read-after-writes (except if the local is used only very locally), so the reordering and "
        "register-allocation have an easier job"),
    OptimizationPass("CombineLiteralLoads", "combine-loads", combineLoadingLiterals,
        "combines loadings of the same literal value within a small range of a basic block"),
    OptimizationPass("RemoveConstantLoadInLoops", "extract-loads-from-loops", removeConstantLoadInLoops,
        "move constant loads in (nested) loops outside the loops"),
    OptimizationPass("ReorderInstructions", "reorder", reorderWithinBasicBlocks,
        "re-order instructions to eliminate more NOPs and stall cycles"),
    OptimizationPass("CombineALUIinstructions", "combine", combineOperations,
        "run peep-hole optimization to combine ALU-operations")};

std::set<std::string> Optimizer::getPasses(OptimizationLevel level)
{
    std::set<std::string> passes;
    switch(level)
    {
    case OptimizationLevel::FULL:
        passes.emplace("vectorize-loops");
        passes.emplace("extract-loads-from-loops");
        // fall-through on purpose
    case OptimizationLevel::MEDIUM:
        passes.emplace("merge-blocks");
        passes.emplace("combine-rotations");
        passes.emplace("general-optimizations");
        passes.emplace("split-read-write");
        passes.emplace("combine-loads");
        // fall-through on purpose
    case OptimizationLevel::BASIC:
        passes.emplace("combine-branches");
        passes.emplace("eliminate-dead-store");
        passes.emplace("single-steps");
        passes.emplace("reorder");
        passes.emplace("combine");
        // fall-through on purpose
    case OptimizationLevel::NONE:
    default:
        break;
    }

    return passes;
}
