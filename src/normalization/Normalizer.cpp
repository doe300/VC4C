/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Normalizer.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../Module.h"
#include "../Profiler.h"
#include "../ThreadPool.h"
#include "../intrinsics/Intrinsics.h"
#include "../optimization/ControlFlow.h"
#include "../optimization/Eliminator.h"
#include "../optimization/Reordering.h"
#include "../spirv/SPIRVBuiltins.h"
#include "Inliner.h"
#include "LiteralValues.h"
#include "LongOperations.h"
#include "MemoryAccess.h"
#include "Rewrite.h"

#include "log.h"

#include <string>
#include <vector>

using namespace vc4c;
using namespace vc4c::normalization;

/*
 * Propagate WORK_GROUP_UNIFORM_VALUE decoration through the kernel code
 */
static void propagateGroupUniforms(Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // XXX does not propagate decoration via phi-nodes of back jumps
    static const auto check = [](const Value& arg) -> bool {
        if(arg.checkRegister())
            return arg.hasRegister(REG_UNIFORM) || arg.hasRegister(REG_ELEMENT_NUMBER);
        if(arg.checkImmediate() || arg.checkLiteral() || arg.checkVector())
            return true;
        if(auto local = arg.checkLocal())
        {
            auto writes = local->getUsers(LocalUse::Type::WRITER);
            return local->is<Parameter>() || local->is<Global>() ||
                std::all_of(
                    writes.begin(), writes.end(), [](const intermediate::IntermediateInstruction* instr) -> bool {
                        return instr->hasDecoration(
                            vc4c::intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
                    });
        }
        return false;
    };
    if(it.get<intermediate::Nop>() || it.get<intermediate::BranchLabel>() || it.get<intermediate::MutexLock>() ||
        it.get<intermediate::SemaphoreAdjustment>() || it.get<intermediate::MemoryBarrier>())
        return;
    if(it->hasDecoration(intermediate::InstructionDecorations::BUILTIN_GLOBAL_ID) ||
        it->hasDecoration(intermediate::InstructionDecorations::BUILTIN_LOCAL_ID))
        return;
    if(it->hasConditionalExecution())
    {
        auto flagsIt = it.getBasicBlock()->findLastSettingOfFlags(it);
        if(!(flagsIt && (*flagsIt)->hasDecoration(intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE)))
        {
            // for conditional writes need to check whether condition is met by all work-items (e.g. element insertion)
            return;
        }
    }
    if(std::all_of(it->getArguments().begin(), it->getArguments().end(), check))
        it->addDecorations(intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
}

/*
 * Propagate UNSIGNED_RESULT decoration through the kernel code
 */
static void propagateUnsignedValues(Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // XXX does not propagate decoration via phi-nodes of back jumps
    if(it.get<intermediate::Nop>() || it.get<intermediate::BranchLabel>() || it.get<intermediate::MutexLock>() ||
        it.get<intermediate::SemaphoreAdjustment>() || it.get<intermediate::MemoryBarrier>())
        return;
    if(intermediate::isGroupBuiltin(it->decoration, true))
    {
        it->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT);
        return;
    }
    if(it.get<intermediate::Operation>())
        // TODO to be on the safe side, for now skip operations, since they could convert purely unsigned values to
        // signed ones (e.g. sub)
        return;
    if(it->hasConditionalExecution())
    {
        auto flagsIt = it.getBasicBlock()->findLastSettingOfFlags(it);
        if(!(flagsIt && (*flagsIt)->hasDecoration(intermediate::InstructionDecorations::UNSIGNED_RESULT)))
        {
            // for conditional writes need to check whether condition is met by all work-items (e.g. element insertion)
            return;
        }
    }
    if(std::all_of(it->getArguments().begin(), it->getArguments().end(),
           [](const Value& val) -> bool { return val.isUnsignedInteger(); }))
        it->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT);
}

/*
 * Dummy normalization step which asserts all remaining instructions are normalized
 */
static void checkNormalized(Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    LCOV_EXCL_START
    if(it.has() && !it->isNormalized())
    {
        if(it.get<intermediate::MethodCall>())
        {
            logging::logLazy(logging::Level::WARNING, [&]() {
                logging::error() << "Failed to in-line or intrinsify function-call: " << it->to_string()
                                 << logging::endl;
                logging::warn() << "Candidates:" << logging::endl;
                for(const auto& method : module.methods)
                    logging::warn() << method->returnType.to_string() << " " << method->name << '('
                                    << to_string<Parameter>(method->parameters) << ')' << logging::endl;
            });
        }
        throw CompilationError(CompilationStep::NORMALIZER, "Not normalized instruction found", it->to_string());
    }
    if(it.has())
    {
        it->forUsedLocals([](const Local* loc, LocalUse::Type type, const auto& inst) {
            if(loc && loc->get<MultiRegisterData>())
            {
                throw CompilationError(CompilationStep::NORMALIZER, "Not lowered 64-bit local found", inst.to_string());
            }
        });
    }
    LCOV_EXCL_STOP
}

// NOTE: The order is on purpose and must not be changed!
const static std::vector<std::pair<std::string, NormalizationStep>> initialNormalizationSteps = {
#ifdef SPIRV_FRONTEND
    // fixes "loading" of OpenCL C work-item functions as SPIR-V built-ins. Needs to run before handling intrinsics
    {"LowerSPIRVBuiltins", spirv::lowerBuiltins},
#endif
    // intrinsifies calls to built-ins and unsupported operations
    {"Intrinsics", intrinsics::intrinsify},
    // lowers operations taking or returning 64-bit values
    {"Lower64BitOperations", lowerLongOperation},
    // replaces all remaining returns with jumps to the end of the kernel-function
    {"EliminateReturns", optimizations::eliminateReturn},
    // rewrites the use of literal values to either small-immediate values or loading of literals
    // this first run here is only required, so some loading of literals can be optimized, which is no longer possible
    // after the second run
    {"HandleImmediates", handleImmediate},
    // propagates the instruction decoration whether values are work-group uniform
    {"PropagateGroupUniformValues", propagateGroupUniforms},
    // propagates the unsigned result instruction decoration
    {"PropagateUnsigned", propagateUnsignedValues}};

// these normalization steps are run after the memory access is converted
const static std::vector<std::pair<std::string, NormalizationStep>> initialNormalizationSteps2 = {
    // handles stack-allocations by calculating their offsets and indices
    {"ResolveStackAllocations", resolveStackAllocation},
    // maps access to global data to the offset in the code
    {"MapGlobalDataToAddress", accessGlobalData},
    // moves vector-containers to locals and re-directs all uses to the local
    {"HandleLiteralVector", handleContainer},
    // lowers operations taking or returning 64-bit values. Since other normalization steps might produce 64-bit
    // operations, we rerun this after any other normalization step.
    {"Lower64BitOperations", lowerLongOperation},
    // dummy step which simply checks whether all remaining instructions are normalized
    {"CheckNormalized", checkNormalized}};

const static std::vector<std::pair<std::string, NormalizationStep>> adjustmentSteps = {
    // needs to re-run this, since optimization steps may insert literals
    {"HandleImmediates", handleImmediate},
    // prevents register-conflicts by moving long-living locals into temporaries before being used together with literal
    // values
    {"HandleUseWithImmediate", handleUseWithImmediate},
    // moves all sources of vector-rotations to accumulators (if too large usage-range)
    {"MoveRotationSourcesToAccs", optimizations::moveRotationSourcesToAccumulators},
    // inserts moves to splits up uses of locals fixes to a register-file (e.g. Unpack/Pack) together
    {"SplitRegisterConflicts", splitRegisterConflicts}};
// TODO split read-after-writes?

static void runNormalizationStep(
    const NormalizationStep& step, Module& module, Method& method, const Configuration& config)
{
    for(auto& block : method)
    {
        auto it = block.walk().nextInBlock();
        while(!it.isEndOfBlock())
        {
            auto tmp = it.copy().previousInBlock();
            step(module, method, it, config);
            // TODO make sure, steps only modify the current instruction
            tmp.nextInBlock();
            if(it == tmp)
                it.nextInBlock();
            else
                it = tmp;
        }
    }
}

void Normalizer::normalize(Module& module) const
{
    // 1. eliminate phi on all methods
    for(auto& method : module)
    {
        // PHI-nodes need to be eliminated before inlining functions
        // since otherwise the phi-node is mapped to the initial label, not to the last label added by the functions
        // (the real end of the original, but split up block)
        logging::logLazy(logging::Level::DEBUG, []() {
            logging::debug() << logging::endl;
            logging::debug() << "Running pass: EliminatePhiNodes" << logging::endl;
        });
        PROFILE_COUNTER(
            vc4c::profiler::COUNTER_NORMALIZATION + 1, "Eliminate Phi-nodes (before)", method->countInstructions());
        optimizations::eliminatePhiNodes(module, *method, config);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_NORMALIZATION + 2, "Eliminate Phi-nodes (after)",
            method->countInstructions(), vc4c::profiler::COUNTER_NORMALIZATION + 1);
    }
    auto kernels = module.getKernels();
    // 2. inline kernel-functions
    for(Method* kernelFunc : kernels)
    {
        Method& kernel = *kernelFunc;

        PROFILE_COUNTER(vc4c::profiler::COUNTER_NORMALIZATION + 4, "Inline (before)", kernel.countInstructions());
        PROFILE_START(Inline);
        inlineMethods(module, kernel, config);
        PROFILE_END(Inline);
        PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_NORMALIZATION + 5, "Inline (after)",
            kernel.countInstructions(), vc4c::profiler::COUNTER_NORMALIZATION + 4);
    }
    // 3. run other normalization steps on kernel functions
    const auto f = [&module, this](Method* kernelFunc) -> void { normalizeMethod(module, *kernelFunc); };
    ThreadPool{"Normalization"}.scheduleAll<Method*>(kernels, f);
}

void Normalizer::adjust(Module& module) const
{
    // run adjustment steps on kernel functions
    auto kernels = module.getKernels();
    const auto f = [&module, this](Method* kernelFunc) -> void { adjustMethod(module, *kernelFunc); };
    ThreadPool{"Adjustment"}.scheduleAll<Method*>(kernels, f);
}

void Normalizer::normalizeMethod(Module& module, Method& method) const
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "-----" << logging::endl);
    CPPLOG_LAZY(logging::Level::INFO, log << "Running normalization passes for: " << method.name << logging::endl);
    std::size_t numInstructions = method.countInstructions();

    PROFILE_START(NormalizationPasses);

    for(const auto& step : initialNormalizationSteps)
    {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: " << step.first << logging::endl;
        PROFILE_START_DYNAMIC(step.first);
        runNormalizationStep(step.second, module, method, config);
        PROFILE_END_DYNAMIC(step.first);
    }

    // maps all memory-accessing instructions to instructions actually performing the hardware memory-access
    // this step is called extra, because it needs to be run over all instructions
    logging::logLazy(logging::Level::DEBUG, []() {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: MapMemoryAccess" << logging::endl;
    });
    PROFILE_START(MapMemoryAccess);
    mapMemoryAccess(module, method, config);
    PROFILE_END(MapMemoryAccess);

    // calculate current/final stack offsets after lowering stack-accesses
    method.calculateStackOffsets();

    for(const auto& step : initialNormalizationSteps2)
    {
        logging::logLazy(logging::Level::DEBUG, [&]() {
            logging::debug() << logging::endl;
            logging::debug() << "Running pass: " << step.first << logging::endl;
        });
        PROFILE_START_DYNAMIC(step.first);
        runNormalizationStep(step.second, module, method, config);
        PROFILE_END_DYNAMIC(step.first);
    }

    // adds the start- and stop-segments to the beginning and end of the kernel
    logging::logLazy(logging::Level::DEBUG, []() {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: AddStartStopSegment" << logging::endl;
    });
    PROFILE_START(AddStartStopSegment);
    optimizations::addStartStopSegment(module, method, config);
    PROFILE_END(AddStartStopSegment);

    PROFILE_END(NormalizationPasses);

    LCOV_EXCL_START
    logging::logLazy(logging::Level::INFO, [&]() {
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
    });
    LCOV_EXCL_STOP
}

void Normalizer::adjustMethod(Module& module, Method& method) const
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "-----" << logging::endl);
    CPPLOG_LAZY(logging::Level::INFO, log << "Running adjustment passes for: " << method.name << logging::endl);
    std::size_t numInstructions = method.countInstructions();

    PROFILE_START(AdjustmentPasses);
    method.cleanEmptyInstructions();

    for(const auto& step : adjustmentSteps)
    {
        logging::logLazy(logging::Level::DEBUG, [&]() {
            logging::debug() << logging::endl;
            logging::debug() << "Running pass: " << step.first << logging::endl;
        });
        PROFILE_START_DYNAMIC(step.first);
        runNormalizationStep(step.second, module, method, config);
        PROFILE_END_DYNAMIC(step.first);
    }

    // extends the branches by adding the conditional execution and the delay-nops
    // this step is called extra, because it needs to be run over all instructions
    logging::logLazy(logging::Level::DEBUG, []() {
        logging::debug() << logging::endl;
        logging::debug() << "Running pass: ExtendBranches" << logging::endl;
    });
    PROFILE_START(ExtendBranches);
    optimizations::extendBranches(module, method, config);
    PROFILE_END(ExtendBranches);

    PROFILE_END(AdjustmentPasses);
    LCOV_EXCL_START
    logging::logLazy(logging::Level::INFO, [&]() {
        logging::info() << logging::endl;
        if(numInstructions != method.countInstructions())
        {
            logging::info() << "Adjustment done, changed number of instructions from " << numInstructions << " to "
                            << method.countInstructions() << logging::endl;
        }
        else
        {
            logging::info() << "Adjustment done" << logging::endl;
        }
        logging::debug() << "-----" << logging::endl;
    });
    LCOV_EXCL_STOP
}
