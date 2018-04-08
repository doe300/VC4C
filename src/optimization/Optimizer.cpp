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

OptimizationPass::OptimizationPass(const std::string& name, const Pass pass) : name(name), pass(pass) {}

void OptimizationPass::operator()(const Module& module, Method& method, const Configuration& config) const
{
    pass(module, method, config);
}

OptimizationStep::OptimizationStep(const std::string& name, const Step step) : name(name), step(step) {}

InstructionWalker OptimizationStep::operator()(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config) const
{
    return step(module, method, it, config);
}

static const std::vector<OptimizationStep> SINGLE_STEPS = {
    // combined successive branches to the same label (e.g. end of switch-case)
    OptimizationStep("CombineDuplicateBranches", combineDuplicateBranches),
    // eliminates useless branches (e.g. jumps to the next instruction)
    OptimizationStep("EliminateUselessBranch", eliminateUselessBranch),
    // combine consecutive instructions writing the same local with a value and zero depending on some flags
    OptimizationStep("CombineSelectionWithZero", combineSelectionWithZero),
    // combine successive setting of the same flags
    OptimizationStep("CombineSettingSameFlags", combineSameFlags)};

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

Optimizer::Optimizer(const Configuration& config) : config(config)
{
    // runs all the single-step optimizations. Combining them results in fewer iterations over the instructions
    passes.emplace_back("SingleSteps", runSingleSteps);
    // combines duplicate vector rotations, e.g. introduced by vector-shuffle into a single rotation
    passes.emplace_back("CombineRotations", combineVectorRotations);
    passes.emplace_back("GeneralOptimizations", generalOptimization);
    /*
     * TODO in combination with a bug/missing check in register-allocation, this generates invalid code (e.g.
     * testing/test_barrier.cl) More exact: the load is moved outside the loop but the register is re-assigned in the
     * loop having wrong value for successive iterations In register-allocation, need to check for loops and reserve
     * whole loop
     */
    // move constant loads in (nested) loops outside the loops
    // passes.emplace_back("RemoveConstantLoadInLoops", removeConstantLoadInLoops);
    // eliminates useless instructions (dead store, move to same, redundant arithmetic operations, ...)
    passes.emplace_back("EliminateDeadStores", eliminateDeadStore);
    // vectorizes loops
    passes.emplace_back("VectorizeLoops", vectorizeLoops);
    // more like a de-optimization. Splits read-after-writes (except if the local is used only very locally), so the
    // reordering and register-allocation have an easier job
    passes.emplace_back("SplitReadAfterWrites", splitReadAfterWrites);
    // combines loadings of the same literal value within a small range of a basic block
    passes.emplace_back("CombineLiteralLoads", combineLoadingLiterals);
    // re-order instructions to eliminate more NOPs and stall cycles
    passes.emplace_back("ReorderInstructions", reorderWithinBasicBlocks);
    // run peep-hole optimization to combine ALU-operations
    passes.emplace_back("CombineALUIinstructions", combineOperations);
    // add (runtime-configurable) loop over the whole kernel execution, allowing for skipping some of the syscall
    // overhead for kernels with many work-groups
    passes.emplace_back("UnrollWorkGroups", unrollWorkGroups);
}

static void runOptimizationPasses(
    const Module& module, Method& method, const Configuration& config, const std::vector<OptimizationPass>& passes)
{
    logging::debug() << "-----" << logging::endl;
    logging::info() << "Running optimization passes for: " << method.name << logging::endl;
    std::size_t numInstructions = method.countInstructions();

    std::size_t index = 0;
    for(const OptimizationPass& pass : passes)
    {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: " << pass.name << logging::endl;
        PROFILE_COUNTER(
            vc4c::profiler::COUNTER_OPTIMIZATION + index, pass.name + " (before)", method.countInstructions());
        PROFILE_START_DYNAMIC(pass.name);
        pass(module, method, config);
        PROFILE_END_DYNAMIC(pass.name);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_OPTIMIZATION + index + 10, pass.name + " (after)",
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