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
#include "Reordering.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

OptimizationPass::OptimizationPass(const std::string& name, const Pass pass, const std::size_t index) :
    name(name), index(index), pass(pass)
{
}

bool OptimizationPass::operator<(const OptimizationPass& other) const
{
    return index < other.index;
}

void OptimizationPass::operator()(const Module& module, Method& method, const Configuration& config) const
{
    pass(module, method, config);
}

bool OptimizationPass::operator==(const OptimizationPass& other) const
{
    return name == other.name && index == other.index;
}

OptimizationStep::OptimizationStep(const std::string& name, const Step step, const std::size_t index) :
    name(name), index(index), step(step)
{
}

bool OptimizationStep::operator<(const OptimizationStep& other) const
{
    return index < other.index;
}

InstructionWalker OptimizationStep::operator()(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config) const
{
    return step(module, method, it, config);
}

bool OptimizationStep::operator==(const OptimizationStep& other) const
{
    return name.compare(other.name) == 0 && index == other.index;
}

static const std::set<OptimizationStep> SINGLE_STEPS = {
    // combined successive branches to the same label (e.g. end of switch-case)
    OptimizationStep("CombineDuplicateBranches", combineDuplicateBranches, 10),
    // eliminates useless branches (e.g. jumps to the next instruction)
    OptimizationStep("EliminateUselessBranch", eliminateUselessBranch, 20),
    // combine consecutive instructions writing the same local with a value and zero depending on some flags
    OptimizationStep("CombineSelectionWithZero", combineSelectionWithZero, 120),
    // combine successive setting of the same flags
    OptimizationStep("CombineSettingSameFlags", combineSameFlags, 130)};

static void runSingleSteps(const Module& module, Method& method, const Configuration& config)
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
}

static void generalOptimization(const Module& module, Method& method, const Configuration& config)
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
}

// need to run before mapping literals
const OptimizationPass optimizations::RUN_SINGLE_STEPS = OptimizationPass("SingleSteps", runSingleSteps, 30);
const OptimizationPass optimizations::COMBINE_ROTATIONS =
    OptimizationPass("CombineRotations", combineVectorRotations, 100);
const OptimizationPass optimizations::GENERAL_OPTIMIZATIONS =
    OptimizationPass("GeneralOptimizations", generalOptimization, 110);
const OptimizationPass optimizations::REMOVE_CONSTANT_LOAD_IN_LOOPS =
    OptimizationPass("RemoveConstantLoadInLoops", removeConstantLoadInLoops, 140);
const OptimizationPass optimizations::ELIMINATE = OptimizationPass("EliminateDeadStores", eliminateDeadStore, 180);
const OptimizationPass optimizations::VECTORIZE = OptimizationPass("VectorizeLoops", vectorizeLoops, 190);
const OptimizationPass optimizations::SPLIT_READ_WRITES =
    OptimizationPass("SplitReadAfterWrites", splitReadAfterWrites, 200);
const OptimizationPass optimizations::COMBINE_LITERAL_LOADS =
    OptimizationPass("CombineLiteralLoads", combineLoadingLiterals, 210);
const OptimizationPass optimizations::REORDER = OptimizationPass("ReorderInstructions", reorderWithinBasicBlocks, 220);
const OptimizationPass optimizations::COMBINE = OptimizationPass("CombineALUIinstructions", combineOperations, 230);
const OptimizationPass optimizations::UNROLL_WORK_GROUPS = OptimizationPass("UnrollWorkGroups", unrollWorkGroups, 240);

const std::set<OptimizationPass> optimizations::DEFAULT_PASSES = {
    RUN_SINGLE_STEPS, /* SPILL_LOCALS, */ COMBINE_LITERAL_LOADS, COMBINE_ROTATIONS, GENERAL_OPTIMIZATIONS, ELIMINATE,
    VECTORIZE, SPLIT_READ_WRITES, REORDER, COMBINE, UNROLL_WORK_GROUPS
    /* , REMOVE_CONSTANT_LOAD_IN_LOOPS
     * TODO in combination with a bug/missing check in register-allocation, this generates invalid code (e.g.
     * testing/test_barrier.cl) More exact: the load is moved outside the loop but the register is re-assigned in the
     * loop having wrong value for successive iterations In register-allocation, need to check for loops and reserve
     * whole loop
     */
};

Optimizer::Optimizer(const Configuration& config, const std::set<OptimizationPass>& passes) :
    config(config), passes(passes)
{
}

static void runOptimizationPasses(
    const Module& module, Method& method, const Configuration& config, const std::set<OptimizationPass>& passes)
{
    logging::debug() << "-----" << logging::endl;
    logging::info() << "Running optimization passes for: " << method.name << logging::endl;
    std::size_t numInstructions = method.countInstructions();

    for(const OptimizationPass& pass : passes)
    {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: " << pass.name << logging::endl;
        PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + pass.index * 10, pass.name + " (before)",
            method.countInstructions());
        PROFILE_START_DYNAMIC(pass.name);
        pass(module, method, config);
        PROFILE_END_DYNAMIC(pass.name);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_OPTIMIZATION + (pass.index + 1) * 10, pass.name + " (after)",
            method.countInstructions(), vc4c::profiler::COUNTER_OPTIMIZATION + pass.index * 10);
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

void Optimizer::addPass(const OptimizationPass& pass)
{
    passes.insert(pass);
}

void Optimizer::removePass(const OptimizationPass& pass)
{
    passes.erase(pass);
}
