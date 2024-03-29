/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Vector.h"

#include "../Module.h"
#include "../Profiler.h"
#include "../SIMDVector.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/ControlFlowLoop.h"
#include "../analysis/DataDependencyGraph.h"
#include "../analysis/FlagsAnalysis.h"
#include "../analysis/PatternMatching.h"
#include "../intermediate/Helper.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../normalization/LiteralValues.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <cstdlib>
#include <iterator>
#include <limits>

using namespace vc4c;
using namespace vc4c::analysis;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

static InductionVariable extractLoopControl(const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
{
    auto inductionVariables = loop.findInductionVariables(dependencyGraph, true);

    if(inductionVariables.empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Could not find induction variables for loop: " << loop.to_string() << logging::endl);
        return InductionVariable{};
    }
    else if(inductionVariables.size() > 1)
    {
        LCOV_EXCL_START
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Selecting from multiple iteration variables is not supported yet for loop: "
                             << loop.to_string() << logging::endl;
            for(auto& var : inductionVariables)
                logging::debug() << "- " << var.to_string() << logging::endl;
        });
        LCOV_EXCL_STOP
        return InductionVariable{};
    }
    auto& var = *inductionVariables.begin();
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Found induction variable: " << var.to_string() << logging::endl);
    return var;
}

/*
 * For now uses a very simple algorithm:
 * - checks the maximum vector-width used inside the loop
 * - tries to find an optimal factor, which never exceeds 16 elements and divides the number of iterations equally
 */
static unsigned determineVectorizationFactor(const ControlFlowLoop& loop, Optional<unsigned> iterationCount)
{
    unsigned char maxTypeWidth = 1;
    for(const auto& node : loop)
    {
        for(const auto& it : *node->key)
        {
            if(it && it->getOutput())
                // TODO is this check enough?
                maxTypeWidth = std::max(maxTypeWidth, it->getOutput()->type.getVectorWidth());
        }
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Found maximum used vector-width of " << static_cast<unsigned>(maxTypeWidth) << " elements"
            << logging::endl);

    // find the biggest factor fitting into 16 SIMD-elements
    auto factor = static_cast<unsigned>(NATIVE_VECTOR_SIZE / maxTypeWidth);
    while(iterationCount && factor > 0)
    {
        if((*iterationCount % factor) == 0)
            break;
        --factor;
    }
    return factor;
}

/*
 * On the cost-side, we have (as increments):
 * - instructions inserted to construct vectors from scalars
 * - additional delay for writing larger vectors through VPM
 * - memory address is read and written from within loop -> abort
 * - vector rotations -> for now abort
 * - vector foldings after loop
 *
 * On the benefit-side, we have (as factors):
 * - the iterations saved (times the number of instructions in an iteration)
 */
static int calculateCostsVsBenefits(const ControlFlowLoop& loop, const InductionVariable& inductionVariable,
    unsigned vectorizationFactor, unsigned numFoldings, bool isDynamicIterationCount)
{
    // TODO benefits are way off, e.g. for test_vectorization.cl#test4, vectorized version uses 1.5k instead of 29k
    // cycles where this calculation estimates a win of ~400cycles!
    int costs = 0;

    SortedSet<const Local*> readAddresses;
    SortedSet<const Local*> writtenAddresses;

    for(const auto& node : loop)
    {
        for(const auto& it : *node->key)
        {
            if(it)
            {
                if(auto access = dynamic_cast<const RAMAccessInstruction*>(it.get()))
                {
                    // for dynamic iteration counts, we need to insert a mask/dynamic element calculation per memory
                    // access
                    // XXX actually per loop iteration
                    costs += isDynamicIterationCount * (access->getVPMCacheEntry() ? 3 : 2);
                    auto& addresses = access->op == MemoryOperation::READ ? readAddresses : writtenAddresses;
                    if(auto loc = access->getMemoryAddress().checkLocal())
                    {
                        addresses.emplace(loc);
                        if(auto data = loc->get<ReferenceData>())
                            addresses.emplace(data->base);
                        if(loc->residesInMemory())
                        {
                            // we directly access an absolute memory address (without any dynamic offset) inside the
                            // loop, we cannot vectorize this
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Cannot vectorize loops accessing absolute memory location: " << it->to_string()
                                    << logging::endl);
                            return std::numeric_limits<int>::min();
                        }
                    }
                    // Check that the elements accessed are singular for for dynamic iteration count, since we assume we
                    // can dynamically vectorize to up to 16 elements and then accessing 16 * non-singular elements
                    // would no longer fit in the caches.
                    if(auto tmuCacheEntry = access->getTMUCacheEntry())
                    {
                        if(isDynamicIterationCount && tmuCacheEntry->numVectorElements != 1_val)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Cannot dynamically vectorize loop which (possibly) accesses multiple memory "
                                       "elements: "
                                    << access->to_string() << logging::endl);
                            return std::numeric_limits<int>::min();
                        }
                    }
                    if(auto vpmCacheEntry = access->getVPMCacheEntry())
                    {
                        if(isDynamicIterationCount && vpmCacheEntry->getVectorWidth() != 1_val)
                        {
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Cannot dynamically vectorize loop which (possibly) accesses multiple memory "
                                       "elements: "
                                    << access->to_string() << logging::endl);
                            return std::numeric_limits<int>::min();
                        }
                    }
                }
                else if(auto access = dynamic_cast<const CacheAccessInstruction*>(it.get()))
                {
                    // Since for cache access instructions (without a corresponding RAM access instruction), the cache
                    // entry and not the instruction contains the locals (e.g. for addresses/offsets), there is no
                    // LocalUser and therefore the cache access instruction is not vectorized.
                    auto ramAccesses = access->cache->getMemoryAccesses();
                    if(std::none_of(ramAccesses.begin(), ramAccesses.end(),
                           [&loop](const RAMAccessInstruction* inst) { return loop.findInLoop(inst); }))
                    {
                        // XXX for now we do not support this, since we cannot reliably track those uses, so abort
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Cannot vectorize loops containing cache accesses without corresponding RAM "
                                   "accesses: "
                                << it->to_string() << logging::endl);
                        return std::numeric_limits<int>::min();
                    }
                }
                else if(it->getVectorRotation())
                {
                    // abort
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Cannot vectorize loops containing vector rotations: " << it->to_string()
                            << logging::endl);
                    return std::numeric_limits<int>::min();
                }
                else if(dynamic_cast<const intermediate::MemoryBarrier*>(it.get()))
                {
                    // abort
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Cannot vectorize loops containing memory barriers: " << it->to_string()
                            << logging::endl);
                    return std::numeric_limits<int>::min();
                }
                else if(dynamic_cast<const intermediate::CodeAddress*>(it.get()))
                {
                    // abort
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Cannot vectorize loops containing code address calculations: " << it->to_string()
                            << logging::endl);
                    return std::numeric_limits<int>::min();
                }
                else if(dynamic_cast<const intermediate::SemaphoreAdjustment*>(it.get()))
                {
                    // abort
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Cannot vectorize loops containing semaphore calls: " << it->to_string()
                            << logging::endl);
                    return std::numeric_limits<int>::min();
                }
            }

            // TODO check and increase costs
        }
    }

    // constant cost - loading immediate for iteration-step for vector-width > 15 (no longer fitting into small
    // immediate)
    if(inductionVariable.inductionStep->getOutput()->type.getVectorWidth() * vectorizationFactor > 15)
        ++costs;
    // single insertion of element selection for repetition branch
    ++costs;
    // our folding implementation takes 3 * ceil(log2(vector-width)) instructions per folding
    auto foldCosts = 3u * (vc4c::log2(vectorizationFactor) + 1u);
    costs += static_cast<int>(foldCosts * numFoldings);
    // additional calculation of the dynamic element mask
    if(isDynamicIterationCount)
    {
        // replication for non-splat initial assignment
        costs += 2;
        // calculation of dynamic active element count
        costs += 4; // XXX actually per loop iteration
        // additional 1 instruction for masking the LCSSA variable with the element number per folding
        costs += static_cast<int>(numFoldings);
    }

    FastSet<const Local*> readAndWrittenAddresses;
    // NOTE: Cannot pass unordered_set into set_intersection, since it requires its inputs to be sorted!
    std::set_intersection(readAddresses.begin(), readAddresses.end(), writtenAddresses.begin(), writtenAddresses.end(),
        std::inserter(readAndWrittenAddresses, readAndWrittenAddresses.begin()));
    // the references could be null-pointers
    readAndWrittenAddresses.erase(nullptr);
    if(!readAndWrittenAddresses.empty())
    {
        for(const Local* local : readAndWrittenAddresses)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot vectorize loops reading and writing the same memory addresses: " << local->to_string()
                    << logging::endl);
        }
        // abort
        return std::numeric_limits<int>::min();
    }

    // the number of instructions/cycles saved
    auto numInstructions = std::accumulate(loop.begin(), loop.end(), std::size_t{0},
        [](std::size_t sum, const CFGNode* node) -> std::size_t { return sum + node->key->size(); });
    auto benefits = static_cast<int>(numInstructions * vectorizationFactor);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Calculated an cost-vs-benefit rating of " << (benefits - costs)
            << " (estimated number of clock cycles saved, larger is better)" << logging::endl);
    return benefits - costs;
}

struct VectorizedAccess
{
    uint8_t minVectorWidth;
    bool outputVectorized;
    bool inputVectorized;
};

static void scheduleForVectorization(const Local* local,
    FastMap<const intermediate::IntermediateInstruction*, VectorizedAccess>& openInstructions, ControlFlowLoop& loop,
    bool scheduleWriters, uint8_t minVectorWidth,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    local->forUsers(LocalUse::Type::READER, [&](const LocalUser* user) -> void {
        if(closedInstructions.find(user) == closedInstructions.end())
            openInstructions.emplace(user, VectorizedAccess{minVectorWidth, false, true});
        if(user->checkOutputRegister() &&
            (user->getOutput()->reg().isSpecialFunctionsUnit() || user->getOutput()->reg().isTextureMemoryUnit()))
        {
            // need to add the reading of SFU/TMU too
            if(auto optIt = loop.findInLoop(user))
            {
                InstructionWalker it = optIt.value().nextInBlock();
                while(!it.isEndOfBlock())
                {
                    if(it->readsRegister(REG_SFU_OUT) && closedInstructions.find(it.get()) == closedInstructions.end())
                    {
                        openInstructions.emplace(it.get(), VectorizedAccess{minVectorWidth, false, true});
                        break;
                    }

                    it.nextInBlock();
                }
            }
        }
    });
    if(scheduleWriters)
    {
        local->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* user) -> void {
            if(closedInstructions.find(user) == closedInstructions.end())
                openInstructions.emplace(user, VectorizedAccess{minVectorWidth, true, false});
        });
    }
}

static uint8_t getVectorWidth(DataType type)
{
    if(auto ptrType = type.getPointerType())
        return ptrType->elementType.getVectorWidth();
    return type.getVectorWidth();
}

static void removeSplatDecoration(IntermediateInstruction* inst, Optional<InstructionWalker> it)
{
    if(!inst->hasDecoration(InstructionDecorations::IDENTICAL_ELEMENTS))
        return;
    inst->decoration = remove_flag(inst->decoration, InstructionDecorations::IDENTICAL_ELEMENTS);

    if(auto tmuCache =
            (check(dynamic_cast<const RAMAccessInstruction*>(inst)) & &RAMAccessInstruction::getTMUCacheEntry)
                .value_or(nullptr))
    {
        if(!it)
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Cannot remove splat decoration from TMU read without knowing its position");
        auto reader = tmuCache->getCacheReader();
        auto readerIt = it->getBasicBlock()->findWalkerForInstruction(reader);
        if(!readerIt || readerIt->base().isEndOfBlock())
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Failed to find TMU value read for no longer identical TMU address write", inst->to_string());
        removeSplatDecoration(reader, readerIt->base());
    }
    if(auto loc = inst->checkOutputLocal())
        loc->forUsers(LocalUse::Type::READER,
            [=](const LocalUser* reader) { removeSplatDecoration(const_cast<LocalUser*>(reader), it); });
}

static void scheduleForOutputValue(Value& out, intermediate::IntermediateInstruction* inst, Method& method,
    FastMap<const intermediate::IntermediateInstruction*, VectorizedAccess>& openInstructions, uint8_t vectorWidth,
    ControlFlowLoop& loop, FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    if(auto ptrType = out.type.getPointerType())
        // TODO this is only correct if the elements are located in one block (base+0, base+1, base+2...). Is this
        // guaranteed?
        out.type = method.createPointerType(ptrType->elementType.toVectorType(vectorWidth), ptrType->addressSpace);
    else
        out.type = out.type.toVectorType(vectorWidth);
    if(auto loc = out.checkLocal())
    {
        if(auto ptrType = loc->type.getPointerType())
            // TODO see above
            const_cast<DataType&>(loc->type) = method.createPointerType(
                loc->type.getPointerType()->elementType.toVectorType(getVectorWidth(out.type)), ptrType->addressSpace);
        else
            const_cast<DataType&>(loc->type) = loc->type.toVectorType(getVectorWidth(out.type));
        scheduleForVectorization(loc, openInstructions, loop, false, vectorWidth, closedInstructions);
    }
    if(out.hasRegister(REG_REPLICATE_ALL) || out.hasRegister(REG_REPLICATE_QUAD))
    {
        // for replications (e.g. for scalar TMU read), need to un-replicate the values
        // TODO is this true in any case??
        auto replicateIt = loop.findInLoop(inst);
        if(!replicateIt)
            // TODO not actually a problem, we just have to find the instruction walker
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Cannot vectorize replication outside of loop", inst->to_string());

        auto newLoc = method.addNewLocal(out.type, "%vectorized_replication");
        inst->replaceValue(out, newLoc, LocalUse::Type::WRITER);

        auto checkIt = replicateIt->nextInBlock();
        while(!checkIt.isEndOfBlock())
        {
            if(auto arg = checkIt->findRegisterArgument(REG_ACC5))
            {
                checkIt->replaceValue(*arg, newLoc, LocalUse::Type::READER);
                checkIt->decoration = remove_flag(checkIt->decoration, InstructionDecorations::IDENTICAL_ELEMENTS);
                removeSplatDecoration(checkIt.get(), checkIt);
            }
            if(auto arg = checkIt->findRegisterArgument(REG_REPLICATE_ALL))
            {
                checkIt->replaceValue(*arg, newLoc, LocalUse::Type::READER);
                checkIt->decoration = remove_flag(checkIt->decoration, InstructionDecorations::IDENTICAL_ELEMENTS);
                removeSplatDecoration(checkIt.get(), checkIt);
            }
            if(auto arg = checkIt->findRegisterArgument(REG_REPLICATE_QUAD))
            {
                checkIt->replaceValue(*arg, newLoc, LocalUse::Type::READER);
                checkIt->decoration = remove_flag(checkIt->decoration, InstructionDecorations::IDENTICAL_ELEMENTS);
                removeSplatDecoration(checkIt.get(), checkIt);
            }
            if(checkIt->writesRegister(REG_ACC5) || checkIt->writesRegister(REG_REPLICATE_ALL) ||
                checkIt->writesRegister(REG_REPLICATE_QUAD))
                // replication register is overwritten, abort
                break;
            checkIt.nextInBlock();
        }
        scheduleForVectorization(newLoc.local(), openInstructions, loop, false, vectorWidth, closedInstructions);
    }
}

static void vectorizeInstruction(intermediate::IntermediateInstruction* inst, Method& method,
    FastMap<const intermediate::IntermediateInstruction*, VectorizedAccess>& openInstructions,
    unsigned vectorizationFactor, ControlFlowLoop& loop, uint8_t minVectorWidth,
    const Optional<Value>& dynamicElementCount,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Vectorizing instruction: " << inst->to_string() << logging::endl);

    // 1. update types of values matching the types of their locals
    unsigned char vectorWidth = minVectorWidth;
    for(auto& arg : inst->getArguments())
    {
        if(auto loc = arg.checkLocal())
        {
            if(auto ptrType = loc->type.getPointerType())
            {
                const_cast<DataType&>(arg.type) = DataType(ptrType);
                vectorWidth = std::max(vectorWidth, ptrType->elementType.getVectorWidth());
            }
            else
            {
                const_cast<DataType&>(arg.type) = arg.type.toVectorType(loc->type.getVectorWidth());
                vectorWidth = std::max(vectorWidth, arg.type.getVectorWidth());
            }
        }
        else if(arg.checkRegister())
        {
            // TODO correct?? This is at least required for reading from TMU
            vectorWidth = static_cast<unsigned char>(vectorizationFactor);
        }
    }

    for(auto& arg : inst->getArguments())
    {
        if(auto loc = arg.checkLocal())
        {
            if(!loc->is<Parameter>() &&
                (check(loc->getSingleWriter()) & &IntermediateInstruction::getMoveSource) != UNIFORM_REGISTER)
                scheduleForVectorization(loc, openInstructions, loop, true, vectorWidth, closedInstructions);
        }
    }
    if(inst->hasConditionalExecution())
    {
        // if this instruction is conditional, need to also vectorize the flag setter
        // TODO true for all cases?
        auto instIt = loop.findInLoop(inst);
        if(auto lastFlagSetterIt =
                instIt ? instIt->getBasicBlock()->findLastSettingOfFlags(*instIt) : Optional<InstructionWalker>{})
        {
            if(closedInstructions.find(lastFlagSetterIt->get()) == closedInstructions.end())
                openInstructions.emplace(lastFlagSetterIt->get(), VectorizedAccess{vectorWidth, true, false});
        }
        else
            CPPLOG_LAZY(logging::Level::WARNING,
                log << "Failed to find flag setter for vectorized conditional instruction within loop: "
                    << inst->to_string() << logging::endl);
    }
    if(inst->doesSetFlag())
    {
        // if this instruction sets flags, need to also vectorize the dependent conditional instructions
        // TODO true for all cases?
        if(auto instIt = loop.findInLoop(inst))
        {
            auto it = instIt->nextInBlock();
            while(!it.isEndOfBlock())
            {
                if(!it.has())
                {
                    it.nextInBlock();
                    continue;
                }
                if(it->doesSetFlag())
                    break;
                if(it->hasConditionalExecution() && closedInstructions.find(it.get()) == closedInstructions.end())
                    openInstructions.emplace(it.get(), VectorizedAccess{vectorWidth, false, true});
                it.nextInBlock();
            }
        }
        else
            CPPLOG_LAZY(logging::Level::WARNING,
                log << "Failed to find flag setter within loop: " << inst->to_string() << logging::endl);
    }
    if(inst->readsRegister(REG_ACC5) || inst->readsRegister(REG_REPLICATE_ALL) ||
        inst->readsRegister(REG_REPLICATE_QUAD))
    {
        // if this instruction reads a replication, need to de-replicate the source
        // TODO true for all cases?
        auto instIt = loop.findInLoop(inst);
        if(auto replicationIt = instIt ?
                instIt->getBasicBlock()->findLastWritingOfRegister(*instIt, REG_REPLICATE_ALL) :
                Optional<InstructionWalker>{})
        {
            if(closedInstructions.find(replicationIt->get()) == closedInstructions.end())
                openInstructions.emplace(replicationIt->get(), VectorizedAccess{vectorWidth, true, false});
        }
        else
            CPPLOG_LAZY(logging::Level::WARNING,
                log << "Failed to find replication writer for vectorized instruction within loop: " << inst->to_string()
                    << logging::endl);
    }

    // 2. depending on operation performed, update type of output
    if(inst->getOutput() &&
        (dynamic_cast<intermediate::Operation*>(inst) || dynamic_cast<intermediate::MoveOperation*>(inst)))
    {
        // TODO vector-rotations need special handling?!
        Value& out = const_cast<Value&>(inst->getOutput().value());
        scheduleForOutputValue(out, inst, method, openInstructions, vectorWidth, loop, closedInstructions);
    }

    if(auto access = dynamic_cast<RAMAccessInstruction*>(inst))
    {
        if(auto cacheEntry = access->getTMUCacheEntry())
        {
            // if we load from a TMU address, we need to adapt the setting of valid TMU address elements (vs. zeroing
            // out) to match the new vector width
            if(dynamicElementCount)
                cacheEntry->numVectorElements = *dynamicElementCount;
            else if(auto lit = cacheEntry->numVectorElements.getLiteralValue())
                cacheEntry->numVectorElements.literal() = Literal(lit->unsignedInt() * vectorizationFactor);
            else
                // TODO need to set the static vector width dynamically
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Vectorizing this type of TMU vector width is not supported", inst->to_string());

            if(auto instIt = loop.findInLoop(inst))
            {
                /*
                 * Rewrite address calculation. This is required, since for loading vectors from TMU, the TMU lowering
                 * code inserts element-address = base-address + element-number * type stride. If the (vectorized) index
                 * accessed is not linear (e.g. there is a factor), the vectorized address calculation would be
                 * rewritten by the TMU lowering, so skip it.
                 *
                 * Example (taken from OpenCL-CTS/constant.cl):
                 *
                 * for(int i = 0; i < num; i++) {
                 *   float pos = i_pos[i * 3];
                 *   sum += pos;
                 * }
                 *
                 * vectorized (without rewrite) to something like:
                 *
                 * for(int i = 0; i < num; i+=16) {
                 *   float16 pos = (float16*)i_pos[i * 3];
                 *   sum += pos;
                 * }
                 *
                 * which wrongly accesses elements 0, 1, ..., 14, 15, 48, 49, ..., 62, 63, ...
                 */
                auto it = *instIt;
                assign(it, cacheEntry->addresses) = access->getMemoryAddress();
                cacheEntry->customAddressCalculation = true;
            }
            else
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Failed to rewrite address calculation for vectorized TMU read", inst->to_string());
        }
        if(auto cacheEntry = access->getVPMCacheEntry())
        {
            // if we read/write to DMA address via VPM, we need to adapt the element type to be accessed
            if(dynamicElementCount)
                cacheEntry->setDynamicElementCount(*dynamicElementCount);
            else
            {
                auto newVectorWidth = cacheEntry->getVectorType().getVectorWidth() * vectorizationFactor;
                cacheEntry->setStaticElementCount(static_cast<uint8_t>(newVectorWidth));
            }
        }

        // schedule all other accesses of the same cache entry for vectorization
        for(auto* accessInst : access->cache->accesses)
        {
            if(closedInstructions.find(accessInst) == closedInstructions.end())
                openInstructions.emplace(
                    accessInst, VectorizedAccess{vectorWidth, accessInst->isLoadAccess(), accessInst->isStoreAccess()});
        }
    }

    if(auto access = dynamic_cast<CacheAccessInstruction*>(inst))
    {
        if(access->op == MemoryOperation::READ)
        {
            auto& out = const_cast<Value&>(access->getData());
            scheduleForOutputValue(out, inst, method, openInstructions, vectorWidth, loop, closedInstructions);
        }

        // schedule all other accesses of the same cache entry for vectorization
        for(auto* accessInst : access->cache->accesses)
        {
            if(closedInstructions.find(accessInst) == closedInstructions.end())
                openInstructions.emplace(
                    accessInst, VectorizedAccess{vectorWidth, accessInst->isLoadAccess(), accessInst->isStoreAccess()});
        }
    }

    // TODO need to adapt types of some registers/output of load, etc.?
    // TODO cosmetic errors: depending on the order of vectorization, some locals are written as vectors, but read as
    // scalars, if the read-instruction was vectorized before the write-instruction

    // mark as already processed and remove from open-set
    closedInstructions.emplace(inst);
    openInstructions.erase(inst);
}

/*
 * Makes sure, the predecessor-node and the instruction-walker are found in correct order
 */
static Optional<InstructionWalker> findWalker(const CFGNode* node, const intermediate::IntermediateInstruction* inst)
{
    return node != nullptr ? node->key->findWalkerForInstruction(inst) : Optional<InstructionWalker>{};
}

/**
 * For some kernels, the induction step output (the incremented step) is used inside the loop (besides the usage in the
 * comparison for repeat/exit branches and being copied for the next iteration).
 *
 * In these cases the "fixed" induction step (stepping the whole vector) is wrong and thus we need to handle them
 * specially.
 *
 * This e.g. happens for code like this:
 *
 *   for(uint i = 0; i < size; i++)
 *     int value=_buf0[1+(i)];
 *     [...]
 *
 * See also: boost-compute/test_count.cl#serial_count_if_int_equal_offset
 */
static tools::SmallSortedPointerSet<IntermediateInstruction*> findOtherUsagesOfInductionStep(
    Method& method, ControlFlowLoop& loop, InductionVariable& inductionVariable, const intermediate::Operation& stepOp)
{
    auto loc = stepOp.checkOutputLocal();
    if(!loc || !inductionVariable.repeatCondition)
        return tools::SmallSortedPointerSet<IntermediateInstruction*>{};

    auto comparisonValue = inductionVariable.repeatCondition->comparisonValue;
    tools::SmallSortedPointerSet<IntermediateInstruction*> otherReaders;

    loc->forUsers(LocalUse::Type::READER, [&](const LocalUser* reader) {
        if(!reader || reader->hasDecoration(InstructionDecorations::PHI_NODE))
            // e.g. induction step copy for next iteration
            return;
        if((reader->doesSetFlag() || reader->hasConditionalExecution()) &&
            reader->findOtherArgument(loc->createReference()) == comparisonValue)
            // (most likely) part of the repeat/exit branch comparison
            return;
        auto optIt = loop.findInLoop(reader);
        if(optIt && reader->hasConditionalExecution() &&
            optIt->getBasicBlock()->findLastSettingOfFlags(*optIt) &
                [&](InstructionWalker setterIt) { return setterIt.has() && setterIt->readsLocal(loc); })
            // (most likely) part of the repeat/exit branch comparison
            return;
        if(optIt && reader->doesSetFlag())
        {
            auto it = optIt->nextInBlock();
            while(!it.isEndOfBlock() && (!it.has() || !it->doesSetFlag()))
            {
                if(!it.has())
                {
                    it.nextInBlock();
                    continue;
                }
                if(it->hasConditionalExecution() && it->writesLocal(inductionVariable.repeatCondition->conditionResult))
                    // (most likely) part of the repeat/exit branch comparison
                    return;
                it.nextInBlock();
            }
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Found reader of induction step output not part of the loop control: " << reader->to_string()
                << logging::endl);
        otherReaders.emplace(const_cast<IntermediateInstruction*>(reader));
    });
    return otherReaders;
}

static void fixInitialValueAndStep(Method& method, ControlFlowLoop& loop, InductionVariable& inductionVariable,
    Literal stepValue, unsigned vectorizationFactor,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    auto stepOp = const_cast<intermediate::Operation*>(inductionVariable.inductionStep);
    const_cast<DataType&>(inductionVariable.initialAssignment->getOutput()->type) =
        inductionVariable.initialAssignment->getOutput()->type.toVectorType(
            inductionVariable.local->type.getVectorWidth());
    intermediate::MoveOperation* move = const_cast<intermediate::MoveOperation*>(
        dynamic_cast<const intermediate::MoveOperation*>(inductionVariable.initialAssignment));
    Optional<InstructionWalker> initialValueWalker;
    bool isStepPlusOne = stepOp->op == OP_ADD && stepValue.unsignedInt() == 1u;
    Optional<Value> precalculatedInitialValue;
    if(move != nullptr && move->getSource().hasLiteral(0_lit) && isStepPlusOne)
    {
        // special/default case: initial value is zero and step is +1
        move->setSource(Value(ELEMENT_NUMBER_REGISTER));
        closedInstructions.emplace(move);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else if(move != nullptr && move->getSource().getLiteralValue() && isStepPlusOne &&
        (initialValueWalker = findWalker(loop.findPredecessor(), move)))
    {
        // more general case: initial value is a literal and step is +1
        initialValueWalker->reset(intermediate::createWithExtras<intermediate::Operation>(
            *move, OP_ADD, move->getOutput().value(), move->getSource(), ELEMENT_NUMBER_REGISTER));
        closedInstructions.emplace(initialValueWalker.value().get());
        inductionVariable.initialAssignment = initialValueWalker->get();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else if((precalculatedInitialValue = inductionVariable.initialAssignment->precalculate().first) && isStepPlusOne &&
        (initialValueWalker = findWalker(loop.findPredecessor(), inductionVariable.initialAssignment)))
    {
        // more general case: initial value is some constant and step is +1
        initialValueWalker->reset(intermediate::createWithExtras<intermediate::Operation>(
            *inductionVariable.initialAssignment, OP_ADD, inductionVariable.initialAssignment->getOutput().value(),
            *precalculatedInitialValue, ELEMENT_NUMBER_REGISTER));
        closedInstructions.emplace(initialValueWalker.value().get());
        inductionVariable.initialAssignment = initialValueWalker->get();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else if(isStepPlusOne && inductionVariable.initialAssignment->isSimpleMove() &&
        (initialValueWalker = findWalker(loop.findPredecessor(), inductionVariable.initialAssignment)))
    {
        // more general case: arbitrary initial value and step is +1
        auto source = inductionVariable.initialAssignment->getMoveSource().value();
        if(!inductionVariable.initialAssignment->hasDecoration(InstructionDecorations::IDENTICAL_ELEMENTS))
        {
            // need to replicate initial value across all elements
            auto tmp = method.addNewLocal(inductionVariable.initialAssignment->getOutput()->type, "%induction.start");
            ignoreReturnValue(intermediate::insertReplication(*initialValueWalker, source, tmp));
            (*initialValueWalker)->replaceValue(source, tmp, LocalUse::Type::READER);
            source = tmp;
        }
        initialValueWalker->reset(
            intermediate::createWithExtras<intermediate::Operation>(*inductionVariable.initialAssignment, OP_ADD,
                inductionVariable.initialAssignment->getOutput().value(), source, ELEMENT_NUMBER_REGISTER));
        closedInstructions.emplace(initialValueWalker.value().get());
        inductionVariable.initialAssignment = initialValueWalker->get();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else
        throw CompilationError(
            CompilationStep::OPTIMIZER, "Unhandled initial value", inductionVariable.initialAssignment->to_string());

    auto otherReaders = findOtherUsagesOfInductionStep(method, loop, inductionVariable, *stepOp);
    if(!otherReaders.empty())
    {
        // need to retain an unmodified copy of the induction step for the other usages
        auto stepIt = loop.findInLoop(inductionVariable.inductionStep).value();
        auto stepOutput = stepIt->getOutput().value();
        auto rawStepOutput = method.addNewLocal(
            stepOutput.type, stepOutput.checkLocal() ? stepOutput.checkLocal()->name : "%induction_step");
        stepIt.emplace(createWithExtras<Operation>(
            *stepOp, stepOp->op, rawStepOutput, stepOp->assertArgument(0), stepOp->assertArgument(1)));
        for(auto* reader : otherReaders)
            reader->replaceValue(stepOutput, rawStepOutput, LocalUse::Type::READER);
    }

    bool stepChanged = false;
    switch(stepOp->op.opAdd)
    {
    case OP_ADD.opAdd:
    case OP_SUB.opAdd:
        if(stepOp->getFirstArg().checkLocal())
        {
            const Value& offset = stepOp->assertArgument(1);
            if(offset.getLiteralValue())
                stepOp->setArgument(1,
                    Value(Literal(offset.getLiteralValue()->signedInt() * static_cast<int32_t>(vectorizationFactor)),
                        offset.type.toVectorType(
                            static_cast<unsigned char>(offset.type.getVectorWidth() * vectorizationFactor))));
            else
                throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step", stepOp->to_string());
        }
        else
        {
            const Value& offset = stepOp->getFirstArg();
            if(offset.getLiteralValue())
                stepOp->setArgument(0,
                    Value(Literal(offset.getLiteralValue()->signedInt() * static_cast<int32_t>(vectorizationFactor)),
                        offset.type.toVectorType(
                            static_cast<unsigned char>(offset.type.getVectorWidth() * vectorizationFactor))));
            else
                throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step", stepOp->to_string());
        }
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Changed iteration step: " << stepOp->to_string() << logging::endl);
        stepChanged = true;
    }

    if(!stepChanged)
        throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step operation", stepOp->to_string());
}

static unsigned fixRepetitionBranch(Method& method, ControlFlowLoop& loop, InductionVariable& inductionVariable,
    unsigned vectorizationFactor, const Optional<Value>& dynamicElementCount,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    // If the branch is on scalar, it usually is converted to flags via "register - = or %cond, element_number", which
    // will hide the zero/non-zero flags for all upper elements. This needs to be fixed.

    auto repeatConditionLocal =
        inductionVariable.repeatCondition ? inductionVariable.repeatCondition->conditionResult : nullptr;
    if(!repeatConditionLocal)
        throw CompilationError(CompilationStep::OPTIMIZER,
            "Cannot vectorize repetition branch without knowing repetition condition", inductionVariable.to_string());

    unsigned numRewritten = 0;
    repeatConditionLocal->forUsers(LocalUse::Type::READER, [&](const intermediate::IntermediateInstruction* reader) {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Vectorizing consumer of repetition condition: " << reader->to_string() << logging::endl);
        auto opIt = loop.findInLoop(reader);
        if(opIt && (reader->getMoveSource() & &Value::checkLocal) == repeatConditionLocal && reader->doesSetFlag() &&
            reader->checkOutputRegister() == REG_NOP)
        {
            // setting of scalar flags for phi-nodes, nothing needs to be done here
            closedInstructions.emplace(opIt->get());
            return;
        }
        auto readerOp = dynamic_cast<const intermediate::Operation*>(reader);
        if(!opIt || !readerOp || readerOp->op != OP_OR || !readerOp->readsRegister(REG_ELEMENT_NUMBER))
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Unhandled usage of repeat condition", reader->to_string());
        auto it = *opIt;
        auto mask = method.addNewLocal(TYPE_INT8.toVectorType(16), "%cond_mask");
        if(dynamicElementCount)
        {
            /*
             * Since we have a dynamic active element number per iteration, we do not statically know the highest active
             * element to cross the boundary first (see below), so we need to determine this dynamically.
             *
             * Depending on the compared value (induction variable or step variable), we need to either check the flags
             * for the highest used element or for the 0th element.
             */
            auto tmp = assign(it, mask.type, "%cond_mask") = load(0xFFFE, LoadType::PER_ELEMENT_UNSIGNED);
            closedInstructions.emplace(it.copy().previousInBlock().get());
            if(inductionVariable.conditionCheckedBeforeStep)
            {
                /*
                 * Comparison before the step, thus we need to check the current dynamic highest active element
                 */
                // we have the number of elements, but need the highest element offset
                auto tmpOffset = assign(it, dynamicElementCount->type, "%mask_offset") = (*dynamicElementCount - 1_val);
                it = insertVectorRotation(it, tmp, tmpOffset, mask);
                auto decoIt = it.copy().previousInBlock();
                // mark the vector rotation inserted just now as vectorized
                if(decoIt.has() && decoIt->getVectorRotation())
                {
                    closedInstructions.emplace(decoIt.get());
                }
            }
            else
                /*
                 * Comparison after the step. Our active element count of the current iteration is already invalid now,
                 * since the number of active elements might change from iteration to iteration. We want to go to the
                 * next iteration if we have at least one (the 0th) element matching the condition, which is the same
                 * behavior as before the vectorization.
                 */
                mask = ELEMENT_NUMBER_REGISTER;
        }
        else
        {
            // Since the uppermost (used) element will cross the limit first, we move the check to that element. This
            // removes the need to rewrite the actual repetition branch, since we retain the (zero/not-zero) flag
            // behavior.
            // TODO is this correct in all cases?
            assign(it, mask) = load(0xFFFF ^ (1u << (vectorizationFactor - 1)), LoadType::PER_ELEMENT_UNSIGNED);
            closedInstructions.emplace(it.copy().previousInBlock().get());
        }
        it->replaceValue(ELEMENT_NUMBER_REGISTER, mask, LocalUse::Type::READER);
        closedInstructions.emplace(it.get());
        ++numRewritten;
    });

    return numRewritten;
}

/**
 * Calculate the dynamic active element count for this iteration at the very top of the loop head to make sure it is
 * available throughout the loop.
 */
static unsigned calculateDynamicElementCount(Method& method, ControlFlowLoop& loop, const Local* iterationLocal,
    const Value& limitValue, unsigned vectorizationFactor, const Value& dynamicElementCount,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    auto head = loop.getHeader();
    if(!head)
        throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find head for loop", loop.to_string());

    auto it = head->key->walk().nextInBlock();
    // TODO need to invert for decrementing step??
    auto tmp = assign(it, dynamicElementCount.type, std::string{dynamicElementCount.local()->name}) =
        (limitValue - iterationLocal->createReference());
    tmp.local()->forUsers(
        LocalUse::Type::WRITER, [&closedInstructions](const LocalUser* user) { closedInstructions.emplace(user); });
    tmp = assign(it, dynamicElementCount.type, std::string{dynamicElementCount.local()->name}) =
        min(as_signed{Value(Literal(vectorizationFactor), TYPE_INT8)}, as_signed{tmp});
    tmp.local()->forUsers(
        LocalUse::Type::WRITER, [&closedInstructions](const LocalUser* user) { closedInstructions.emplace(user); });
    it = insertReplication(it, tmp, dynamicElementCount);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Inserted calculation of dynamic active element count into: " << dynamicElementCount.to_string()
            << logging::endl);

    return 2;
}

static bool mayReadLocal(const Value& input, const Local* output, FastSet<const Local*>& processedLocals)
{
    if(auto loc = input.checkLocal())
    {
        if(loc == output)
            return true;
        if(processedLocals.find(loc) != processedLocals.end())
            // This is to prevent stack overflows due to infinite recursion
            return false;
        auto writers = loc->getUsers(LocalUse::Type::WRITER);
        processedLocals.emplace(loc);
        return std::any_of(writers.begin(), writers.end(), [&](const LocalUser* writer) -> bool {
            if(writer->hasConditionalExecution())
                // XXX for now assume the condition could depend on the base local
                return true;
            return std::any_of(writer->getArguments().begin(), writer->getArguments().end(),
                [&](const Value& arg) -> bool { return mayReadLocal(arg, output, processedLocals); });
        });
    }
    if(input.getConstantValue() || input.hasRegister(REG_UNIFORM))
        // can't depend on any dynamic value
        return false;
    // any other value, e.g. replication register, for now assume we may read the base local
    return true;
}

/**
 * Checks that no control flow inside the loop (e.g. if-else or switch-case constructs) depends on the given iteration
 * variable.
 *
 * We cannot jump to separate locations for single vector elements, and we also cannot just run whole basic blocks with
 * masked vector access (e.g. they might set their own flags, some periphery ignores the flags, etc.). Thus, those
 * control flow constructs need special handling to make sure the side-effects of their contents match the behavior
 * as-if they would have bee entered/skipped by the single items.
 */
static bool checkIterationVariableDependentControlFlow(
    Method& method, ControlFlowLoop& loop, const Local* iterationVariable, unsigned vectorizationFactor)
{
    if(loop.size() == 1)
        // no diverging control flow possible for a single block
        return true;

    bool multipleSuccessorsInLoop = false;
    // The list of blocks which have conditional branches into multiple other blocks within the loop
    FastMap<BasicBlock*, tools::SmallSortedPointerSet<IntermediateInstruction*>> branchInstructions;
    for(auto* node : loop)
    {
        unsigned numSuccessorsInLoop = 0;
        node->forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
            if(loop.find(&successor) != loop.end())
            {
                branchInstructions[node->key].emplace(edge.data.getPredecessor(node->key).get());
                ++numSuccessorsInLoop;
            }
            return true;
        });
        if(numSuccessorsInLoop > 1)
            multipleSuccessorsInLoop = true;
        else
            // at most a single conditional branch to another block within the loop, ignore
            branchInstructions.erase(node->key);
    }
    if(!multipleSuccessorsInLoop)
        // no divergent control flow within loop
        return true;

    // check whether any found divergent control flow may depend on the iteration variable
    for(auto& block : branchInstructions)
    {
        for(auto& inst : block.second)
        {
            // TODO to be able to support switch-case, we would need (for dynamic branches) do this check for all
            // address writes
            auto comp = analysis::getComparison(inst, block.first->walk(), true);
            if(!comp)
                // can't determine the comparison, conservatively assume it may depend on the iteration variable
                return false;
            // be conservative here, only assume a comparison to not depend on the iteration variable if we can prove it
            // for all writes
            auto checkOperand = [iterationVariable](const Value& val) -> bool {
                FastSet<const Local*> processedLocals;
                return !mayReadLocal(val, iterationVariable, processedLocals);
            };
            if(!checkOperand(comp->leftOperand) || !checkOperand(comp->rightOperand))
                // at least one operand may use the iteration variable
                return false;
        }
    }

    return true;
}

struct AccumulationInfo
{
    const Local* local;
    OpCode op;
    tools::SmallSortedPointerSet<const Local*> outputLocals;
    tools::SmallSortedPointerSet<const LocalUser*> toBeFolded;
    tools::SmallSortedPointerSet<const Local*> toBeMasked;
    Optional<Value> initialValue;
    IntermediateInstruction* initialWriter;
};

/**
 * LLVM only writes the LCSSA local on loop exit (with inverted condition of the loop repetition
 * branch), since before the last iteration this local is not used and overwritten anyway. Since the
 * last iteration might have an active element mask with not all elements set, we do not write the
 * previous accumulated values of the previous iterations in the higher elements.
 *
 * Similarly, the default value of the LCSSA local is only set if the loop is not taken at all (for similar reason, if
 * the loop is taken, it is overwritten anyway). If now we run for a single iteration with an active element mask not
 * covering all elements, we have some undefined values.
 *
 * To fix this we need to rewrite the conditional assignments to not use the branch condition but always write all
 * active elements (or always initialize the default value).
 */
static unsigned fixLCSSAElementMask(Method& method, ControlFlowLoop& loop,
    const FastMap<const Local*, AccumulationInfo>& accumulationsToFold, const Value& dynamicElementCount,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    unsigned numModified = 0;
    for(auto& acc : accumulationsToFold)
    {
        for(auto& toBeMasked : acc.second.toBeMasked)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Rewriting LCSSA to heed active element mask: " << toBeMasked->to_string() << logging::endl);

            auto writers = toBeMasked->getUsers(LocalUse::Type::WRITER);
            if(writers.size() != 2)
                throw CompilationError(
                    CompilationStep::OPTIMIZER, "LCSAA local has not exactly 2 writers", toBeMasked->to_string());

            ExtendedInstruction* outOfLoopInst = nullptr;
            auto inLoopIt = loop.findInLoop(*writers.begin());
            if(inLoopIt)
                outOfLoopInst =
                    dynamic_cast<ExtendedInstruction*>(const_cast<IntermediateInstruction*>(*(++writers.begin())));
            else
            {
                inLoopIt = loop.findInLoop(*(++writers.begin()));
                outOfLoopInst =
                    dynamic_cast<ExtendedInstruction*>(const_cast<IntermediateInstruction*>(*writers.begin()));
            }

            if(!outOfLoopInst)
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Failed to find default value assignment for LCSSA local", toBeMasked->to_string());
            if(!inLoopIt)
                throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find instruction walker in loop for",
                    toBeMasked->to_string());
            auto lastSettingIt = inLoopIt->getBasicBlock()->findLastSettingOfFlags(*inLoopIt);
            if(!lastSettingIt)
                throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find last setting of flags in loop for",
                    toBeMasked->to_string());

            // TODO need to make sure the read variable is not written between the current and new position!

            // The instruction itself is conditional on the branch condition. Since we want to rewrite the condition of
            // the instruction, we need to move it before the current set-flags instruction
            lastSettingIt->emplace(inLoopIt->release());
            auto insertIt = *lastSettingIt;
            inLoopIt->erase();

            auto cond = assignNop(insertIt) = as_signed{ELEMENT_NUMBER_REGISTER} < as_signed{dynamicElementCount};
            closedInstructions.emplace(insertIt.copy().previousInBlock().get());
            insertIt.get<ExtendedInstruction>()->setCondition(cond);

            // The instruction itself is conditional, so it is only assigned if the loop is not entered. We need to
            // change that to always assign the default value
            // TODO is this true for a different initial value than the unit of the accumulation operation?
            // TODO if initial value is not splat, need to replicate it!
            outOfLoopInst->setCondition(COND_ALWAYS);
            closedInstructions.emplace(outOfLoopInst);

            numModified += 3;
        }
    }

    return numModified;
}

/**
 * Fold vectorized version into "scalar" version by applying the accumulation function
 * NOTE: The "scalar" version is not required to be actual scalar (vector-width of 1), could just be some other smaller
 * vector-width.
 */
static void foldVectorizedLocal(ControlFlowLoop& loop, Method& method, unsigned vectorizationFactor,
    FastMap<const intermediate::IntermediateInstruction*, VectorizedAccess>& openInstructions,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions, const IntermediateInstruction* inst,
    AccumulationInfo& info)
{
    const Local* matchingLocal = nullptr;
    for(auto* loc : info.outputLocals)
    {
        if(inst->readsLocal(loc))
        {
            if(matchingLocal)
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Folding instructions with multiple arguments depending on same folding local is not yet "
                    "supported",
                    inst->to_string());
            matchingLocal = loc;
        }
    }
    if(matchingLocal)
    {
        auto arg = matchingLocal->createReference();
        // since we are in the process of vectorization, the local type is already updated, but the argument
        // type is pending. For the next steps, we need to update the argument type.
        arg.type = arg.local()->type;
        auto origVectorType = arg.type;
        if(arg.type.getVectorWidth() >= vectorizationFactor)
            origVectorType =
                arg.type.toVectorType(static_cast<uint8_t>(arg.type.getVectorWidth() / vectorizationFactor));

        Optional<InstructionWalker> it;
        if(auto succ = loop.findSuccessor())
        {
            it = succ->key->findWalkerForInstruction(inst);
            while(!it && (succ = succ->getSingleSuccessor()))
                it = succ->key->findWalkerForInstruction(inst);
        }
        if(it)
        {
            // insert folding or argument, heed original vector width
            auto newArg = method.addNewLocal(origVectorType, "%vector_fold");
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Folding vectorized local " << arg.to_string() << " into " << newArg.to_string()
                    << " for: " << inst->to_string() << logging::endl);
            auto insertIt = insertFoldVector(*it, method, newArg, arg, info.op, closedInstructions);
            if(auto initial = info.initialValue)
            {
                // since the initial value setter will be set for all vector-elements and therefore applied too
                // often, disable it (but leave the the instruction to have an unconditional local setter).
                if(auto move = dynamic_cast<MoveOperation*>(info.initialWriter))
                    move->setSource(info.op.getLeftIdentity().value());
                else if(auto load = dynamic_cast<LoadImmediate*>(info.initialWriter))
                    load->setImmediate(info.op.getLeftIdentity().value().literal());
                else
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Unhandled initial value writer for folding accumulation as part of loop vectorization",
                        info.initialWriter->to_string());
                // ... and instead fold it back in at the very end
                auto tmp = method.addNewLocal(origVectorType, "%vector_fold");
                insertIt.emplace(std::make_unique<Operation>(info.op, tmp, newArg, *initial));
                closedInstructions.emplace(insertIt.get());
                insertIt.nextInBlock();
                newArg = tmp;
            }

            // replace argument with folded version
            const_cast<IntermediateInstruction*>(inst)->replaceValue(arg, newArg, LocalUse::Type::READER);
            openInstructions.erase(inst);
            closedInstructions.emplace(inst);
            // remove tracking of this folding for this parameter, so a second folding into the same instruction chooses
            // the other accumulation info (and not this one which will not work anymore, since the argument has already
            // been replaced by the folded version).
            info.toBeFolded.erase(inst);
            return;
        }
    }

    throw CompilationError(CompilationStep::OPTIMIZER,
        "Vector folding for this operation '" + std::string{info.op.name} + "' and instruction is not yet implemented",
        inst->to_string());
}

/**
 * Replicate "scalar" version into vectorized version by copying the "scalar" elements across the required vector
 * elements.
 * NOTE: The "scalar" version is not required to be actual scalar (vector-width of 1), could just be some
 * other smaller vector-width.
 */
static void replicateVectorizedLocal(ControlFlowLoop& loop, Method& method, unsigned vectorizationFactor,
    FastMap<const intermediate::IntermediateInstruction*, VectorizedAccess>& openInstructions,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions, const IntermediateInstruction* inst)
{
    Optional<InstructionWalker> optIt;
    if(auto pred = loop.findPredecessor())
    {
        optIt = pred->key->findWalkerForInstruction(inst);
        while(!optIt && (pred = pred->getSinglePredecessor()))
            optIt = pred->key->findWalkerForInstruction(inst);
    }
    if(optIt)
    {
        // can only be before the loop -> simply replicate the loaded elements
        auto it = *optIt;
        auto origOutput = inst->getOutput().value();
        auto newOutput = method.addNewLocal(origOutput.type, "%vector_replicate");
        it->setOutput(newOutput);
        it.nextInBlock();

        auto numElements = origOutput.type.isSimpleType() ? origOutput.type.getVectorWidth() : 1u;
        origOutput.type = (origOutput.type.getPointerType() ? TYPE_INT32 : origOutput.type)
                              .toVectorType(static_cast<uint8_t>(numElements * vectorizationFactor));
        it = insertVectorReplication(it, method, newOutput, origOutput);

        closedInstructions.emplace(inst);
        openInstructions.erase(inst);
        return;
    }

    throw CompilationError(
        CompilationStep::OPTIMIZER, "Vectorization for this instruction is not yet implemented", inst->to_string());
}

/*
 * Approach:
 * - set the iteration variable (local) to vector
 * - iterative (until no more values changed), modify all value (and local)-types so argument/result-types match again
 * - update TMU/VPM configuration and address calculation
 */
static std::size_t vectorize(ControlFlowLoop& loop, const Local* startLocal, Method& method,
    unsigned vectorizationFactor, FastMap<const Local*, AccumulationInfo>& accumulationsToFold,
    const Optional<Value>& dynamicElementCount, const InductionVariable& inductionVariable,
    FastSet<const intermediate::IntermediateInstruction*>& closedInstructions)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Vectorizing loop '" << loop.to_string() << "' with factor of " << vectorizationFactor
            << (dynamicElementCount ? " and dynamic element count" : "") << "..." << logging::endl);
    FastMap<const intermediate::IntermediateInstruction*, VectorizedAccess> openInstructions;

    const_cast<DataType&>(startLocal->type) = startLocal->type.toVectorType(
        static_cast<unsigned char>(startLocal->type.getVectorWidth() * vectorizationFactor));
    scheduleForVectorization(
        startLocal, openInstructions, loop, false, static_cast<uint8_t>(vectorizationFactor), closedInstructions);
    std::size_t numVectorized = 0;

    // iteratively change all instructions
    while(!openInstructions.empty())
    {
        auto instIt = openInstructions.begin();
        auto inst = instIt->first;
        if(auto it = loop.findInLoop(inst))
        {
            vectorizeInstruction(it->get(), method, openInstructions, vectorizationFactor, loop,
                instIt->second.minVectorWidth, dynamicElementCount, closedInstructions);
            ++numVectorized;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Vectorized local is accessed outside of loop: " << inst->to_string() << logging::endl);

            if(instIt->second.outputVectorized == instIt->second.inputVectorized)
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Cannot vectorize instruction outside of loop for both (or neither) input and output",
                    inst->to_string());

            if(instIt->second.inputVectorized)
            {
                // input is vectorized -> instruction is after loop -> fold
                auto foldIt = std::find_if(
                    accumulationsToFold.begin(), accumulationsToFold.end(), [&](const auto& entry) -> bool {
                        return entry.second.toBeFolded.find(inst) != entry.second.toBeFolded.end();
                    });
                if(foldIt != accumulationsToFold.end())
                {
                    foldVectorizedLocal(
                        loop, method, vectorizationFactor, openInstructions, closedInstructions, inst, foldIt->second);
                    ++numVectorized;
                }

                else if((inst->isSimpleMove() || dynamic_cast<const LoadImmediate*>(inst)) && inst->checkOutputLocal())
                {
                    // follow all simple moves to other locals (to find the instruction we really care about)
                    vectorizeInstruction(const_cast<intermediate::IntermediateInstruction*>(inst), method,
                        openInstructions, vectorizationFactor, loop, instIt->second.minVectorWidth, dynamicElementCount,
                        closedInstructions);
                    ++numVectorized;
                }
                else
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Non-folding access of vectorized locals outside of the loop is not yet implemented",
                        inst->to_string());
            }
            if(instIt->second.outputVectorized)
            {
                // output is vectorized -> instruction is before loop -> replicate
                if(inst == inductionVariable.initialAssignment)
                {
                    // will be treated separately via the #fixInitialValueAndStep function
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Skipping vectorization of initial value assignment for now, will be handled later: "
                            << inst->to_string() << logging::endl);
                    closedInstructions.emplace(inst);
                    openInstructions.erase(inst);
                }
                else
                {
                    replicateVectorizedLocal(
                        loop, method, vectorizationFactor, openInstructions, closedInstructions, inst);
                    ++numVectorized;
                }
            }
        }
    }

    if(dynamicElementCount)
        numVectorized +=
            fixLCSSAElementMask(method, loop, accumulationsToFold, *dynamicElementCount, closedInstructions);

    return numVectorized;
}

/*
 * Runs the above steps, with additionally:
 * - fix initial iteration value and step
 * - fix repetition loop condition
 * - calculate dynamic element count
 */
static void vectorize(ControlFlowLoop& loop, InductionVariable& inductionVariable, Method& method,
    unsigned vectorizationFactor, Literal stepValue, FastMap<const Local*, AccumulationInfo>& accumulationsToFold,
    const Optional<Value>& dynamicElementCount)
{
    FastSet<const intermediate::IntermediateInstruction*> closedInstructions;
    std::size_t numVectorized = vectorize(loop, inductionVariable.local, method, vectorizationFactor,
        accumulationsToFold, dynamicElementCount, inductionVariable, closedInstructions);

    fixInitialValueAndStep(method, loop, inductionVariable, stepValue, vectorizationFactor, closedInstructions);
    numVectorized += 2;

    numVectorized += fixRepetitionBranch(
        method, loop, inductionVariable, vectorizationFactor, dynamicElementCount, closedInstructions);

    if(dynamicElementCount)
        numVectorized += calculateDynamicElementCount(method, loop, inductionVariable.local,
            inductionVariable.repeatCondition.value().comparisonValue, vectorizationFactor, *dynamicElementCount,
            closedInstructions);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Vectorization done, changed " << numVectorized << " instructions!" << logging::endl);
}

static bool readsOutput(
    const Local* input, const Local* output, Optional<OpCode> op, FastSet<const Local*>& processedLocals)
{
    if(input == output)
        return true;
    if(processedLocals.find(input) != processedLocals.end())
        // This is to prevent stack overflows due to infinite recursion
        return false;
    auto writers = input->getUsers(LocalUse::Type::WRITER);
    processedLocals.emplace(input);
    if(std::any_of(writers.begin(), writers.end(), [&](const LocalUser* writer) -> bool {
           if(auto loc = writer->getMoveSource() & &Value::checkLocal)
               return readsOutput(loc, output, op, processedLocals);
           auto writeOp = dynamic_cast<const Operation*>(writer);
           if(writeOp && (!op || writeOp->op == op))
               // For now we only check whether the read is with the same operation. TODO is this required?
               return std::any_of(
                   writer->getArguments().begin(), writer->getArguments().end(), [&](const Value& arg) -> bool {
                       return arg.checkLocal() && readsOutput(arg.local(), output, op, processedLocals);
                   });
           return false;
       }))
        return true;
    return false;
}

/**
 * Allow for accumulations in the style:
 *
 * <outside of loop>
 * %loc = <initial value>
 * [...]
 * <inside of loop>
 * %tmp = <binary op> %loc, <loc-independent, loop-local value>
 * [...]
 * %loc = %tmp (phi)
 * [...]
 * <outside of loop>
 * <...> = %loc
 */
static Optional<AccumulationInfo> determineAccumulation(const Local* loc, const ControlFlowLoop& loop)
{
    // Local has (directly or indirectly via simple moves):
    // 1. initial write before loop
    // 2. is read in foldable binary operation in loop where the result is then again written to the local
    // 3. is read outside of loop (or the temporary used before the assignment back to the local)
    const LocalUser* initialWrite = nullptr;
    const LocalUser* loopOperationRead = nullptr;
    const LocalUser* loopWrite = nullptr;
    tools::SmallSortedPointerSet<const Local*> loopTemporaries;
    auto predecessor = loop.findPredecessor();
    for(auto& user : loc->getUsers())
    {
        auto loopIt = loop.findInLoop(user.first);
        if(user.second.writesLocal())
        {
            if(!loopIt && !user.first->hasConditionalExecution())
                initialWrite = user.first;
            else if(!loopIt && user.first->hasDecoration(InstructionDecorations::PHI_NODE) && predecessor &&
                predecessor->key->findWalkerForInstruction(user.first))
                // initial write where the conditionality of the phi-node could not be removed, e.g. for dynamical loop
                // iteration bounds
                initialWrite = user.first;
            else if(loopIt && user.first->hasDecoration(InstructionDecorations::PHI_NODE))
            {
                loopWrite = user.first;
                if(auto src = loopWrite->getMoveSource() & &Value::checkLocal)
                    // we might use the loop variable (which is written back to the currently handled local for the next
                    // iteration) later on for folding, since the assignment to the current local is not necessarily
                    // executed, since there might be no next iteration.
                    loopTemporaries.emplace(src);
            }
            else
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Unexpected write for accumulation candidate '" << loc->to_string()
                        << "', aborting: " << user.first->to_string() << logging::endl);
                return {};
            }
        }
        if(user.second.readsLocal())
        {
            if(loopIt && !loopOperationRead)
                loopOperationRead = user.first;
            else if(loopIt)
            {
                // multiple reads inside the loop
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Multiple reads of accumulation candidate '" << loc->to_string()
                        << "' inside loop, aborting: " << user.first->to_string() << logging::endl);
                return {};
            }
            else
                loopTemporaries.emplace(loc);
        }
    }

    if(!initialWrite || !loopOperationRead || !loopWrite || loopTemporaries.empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Could not find all required instructions for accumulation candidate: " << loc->to_string()
                << logging::endl);
        return {};
    }

    while(loopOperationRead->isSimpleMove() && loopOperationRead->checkOutputLocal())
    {
        auto readers = loopOperationRead->checkOutputLocal()->getUsers(LocalUse::Type::READER);
        if(readers.size() == 1 && loop.findInLoop(*readers.begin()))
        {
            // actualAccumulatedLocal = loopOperationRead->checkOutputLocal();
            loopOperationRead = *readers.begin();
            // loopTemporaries.emplace(loc);
            // return determineAccumulation(loopOperationRead->checkOutputLocal(), loop);
        }
        else
            break;
    }

    auto op = dynamic_cast<const intermediate::Operation*>(loopOperationRead);
    if(!op || op->op.numOperands != 2)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Loop operation is not a valid accumulation operation: " << loopOperationRead->to_string()
                << logging::endl);
        return {};
    }

    auto initialValue = initialWrite->precalculate().first;
    if(initialValue == op->op.getLeftIdentity())
        // no initial value to add
        initialValue = NO_VALUE;
    else if(!initialValue || !initialValue->type.isScalarType() || !op->op.isAssociative() || !op->op.isCommutative() ||
        (!dynamic_cast<const MoveOperation*>(initialWrite) && !dynamic_cast<const LoadImmediate*>(initialWrite)))
    {
        // since we apply the initial value after the folding, we can only do that for associative and commutative fold
        // operations
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Accumulation with non-identity initial value is not yet supported: " << initialWrite->to_string()
                << logging::endl);
        return {};
    }

    // make sure the input of the loop read is actually at some point the loop write output
    FastSet<const Local*> processedLocals;
    if(std::none_of(loopWrite->getArguments().begin(), loopWrite->getArguments().end(), [&](const Value& arg) -> bool {
           return arg.checkLocal() && readsOutput(arg.local(), loc, op->op, processedLocals);
       }))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "In-loop write '" << loopWrite->to_string()
                << "' does not read accumulation candidate: " << loc->to_string() << logging::endl);
        return {};
    }

    tools::SmallSortedPointerSet<const LocalUser*> toBeFolded;
    tools::SmallSortedPointerSet<const Local*> toBeMasked;
    auto outputLocals = loopTemporaries;
    for(auto local : loopTemporaries)
    {
        for(auto reader : local->getUsers(LocalUse::Type::READER))
        {
            if(!loop.findInLoop(reader))
                toBeFolded.emplace(reader);
            else if(reader->isSimpleMove() && reader->checkOutputLocal() &&
                reader->hasDecoration(InstructionDecorations::PHI_NODE))
            {
                /*
                 * Support for LLVM LCSSA (Loop Closed SSA), see
                 * https://github.com/llvm/llvm-project/blob/master/llvm/docs/LoopTerminology.rst#loop-closed-ssa-lcssa
                 *
                 * Basically, LLVM creates a copy of the accumulation local, which is initialized to the same value
                 * before the loop, set parallel to the accumulation local inside the loop and used after the loop exit
                 * instead of the accumulation local.
                 */
                auto candidate = reader->checkOutputLocal();
                auto writers = candidate->getUsers(LocalUse::Type::WRITER);
                auto readers = candidate->getUsers(LocalUse::Type::READER);
                if(writers.size() != 2)
                    // check only 2 writers (one for initial value, one for value update in loop)
                    continue;
                auto firstWriter = *writers.begin();
                auto secondWriter = *(++writers.begin());
                if((firstWriter == reader && secondWriter->getMoveSource() != initialWrite->getMoveSource()) ||
                    (secondWriter == reader && firstWriter->getMoveSource() != initialWrite->getMoveSource()))
                    // check same initial value set
                    continue;
                if(std::any_of(readers.begin(), readers.end(),
                       [&](const LocalUser* user) -> bool { return loop.findInLoop(user).has_value(); }))
                    // XXX would need better check to make sure all reads are after the loop
                    continue;
                if(!readers.empty() && firstWriter->hasConditionalExecution() &&
                    secondWriter->hasConditionalExecution())
                    // see #fixLCSSAElementMask for documentation
                    toBeMasked.emplace(candidate);

                for(auto user : readers)
                    toBeFolded.emplace(user);
                outputLocals.emplace(candidate);
            }
        }
    }

    if(toBeFolded.empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to find consumer for local '" << loc->to_string()
                << "' outside of loop to be folded, aborting" << logging::endl);
        return {};
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Local '" << loc->to_string() << "' is accumulated with operation '" << op->op.name << "' into {"
            << vc4c::to_string<const Local*>(outputLocals) << "}"
            << (toBeMasked.empty() ? "" : (" (masked in " + to_string<const Local*>(toBeMasked) + ")"))
            << " in: " << vc4c::to_string<const LocalUser*>(toBeFolded) << logging::endl);

    return AccumulationInfo{loc, op->op, std::move(outputLocals), std::move(toBeFolded), std::move(toBeMasked),
        initialValue, const_cast<IntermediateInstruction*>(initialWrite)};
}

std::size_t optimizations::vectorizeLoops(const Module& module, Method& method, const Configuration& config)
{
    if(method.empty())
        return 0u;

    // 1. find loops
    auto& cfg = method.getCFG();
    auto loops = cfg.findLoops(false);
    std::size_t numChanges = 0;

    // 2. determine data dependencies of loop bodies
    auto dependencyGraph = DataDependencyGraph::createDependencyGraph(method);

    for(auto& loop : loops)
    {
        // 3. determine operation on iteration variable and bounds
        auto inductionVariable = extractLoopControl(loop, *dependencyGraph);
        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_OPTIMIZATION, "Loops found", 1);
        if(inductionVariable.local == nullptr)
            // we could not find the iteration variable, skip this loop
            continue;

        if(!inductionVariable.initialAssignment || !inductionVariable.inductionStep ||
            !inductionVariable.repeatCondition || inductionVariable.repeatCondition->comparisonName.empty() ||
            inductionVariable.repeatCondition->comparisonValue.isUndefined())
        {
            // we need to know both bounds and the iteration step (for now)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to find all bounds and step for loop, aborting vectorization!" << logging::endl);
            continue;
        }

        auto stepConstant = inductionVariable.getStep();
        if(!stepConstant)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Iteration step increment is not a literal value, aborting vectorization!" << logging::endl);
            continue;
        }

        auto iterations = inductionVariable.getIterationCount();
        auto dynamicElementCount = NO_VALUE;
        if(iterations)
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Determined iteration count of " << *iterations << logging::endl);
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Could not determine integral iteration count, using dynamic active element mask"
                    << logging::endl);
            dynamicElementCount = method.addNewLocal(TYPE_INT8.toVectorType(NATIVE_VECTOR_SIZE), "%active_elements");
        }

        auto writeDependencies = loop.findLocalDependencies(*dependencyGraph);
        auto outputDependencies = loop.findOutputDependencies(*dependencyGraph);
        auto inputDependencies = loop.findInputDependencies(*dependencyGraph);
        FastMap<const Local*, AccumulationInfo> accumulationsToFold;
        if(!writeDependencies.empty())
        {
            /*
             * There are multiple locals which are written inside and before the loop. Thus, their value is dependent on
             * the previous loop iteration which requires rewriting to match the new increased loop step.
             *
             * One of these locals is the induction variable, which we already handle correctly.
             *
             * Other locals might be accumulations, which we detect in the below function call and then store the
             * information to rewrite them later using vector folding.
             *
             * On any other type of such local access within the loop, we abort, since we cannot rewrite them properly.
             */
            bool unknownDependency = false;
            for(auto candidate : writeDependencies)
            {
                if(candidate == inductionVariable.local)
                    continue;
                if(auto info = determineAccumulation(candidate, loop))
                {
                    outputDependencies.erase(candidate);
                    for(auto otherLoc : info->outputLocals)
                        outputDependencies.erase(otherLoc);
                    inputDependencies.erase(candidate);
                    accumulationsToFold.emplace(candidate, std::move(info).value());
                    continue;
                }
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Skipping loop with unknown type of write dependency inside and outside of loop: "
                        << candidate->to_string() << logging::endl);
                unknownDependency = true;
            }
            if(unknownDependency)
                continue;
        }
        inputDependencies.erase(inductionVariable.local);
        if(std::any_of(inputDependencies.begin(), inputDependencies.end(),
               [](const Local* loc) -> bool { return !loc->createReference().isAllSame(); }))
        {
            // These are locals written before the loop and used inside the loop. Since there are locals in this set
            // which are not explicitly handled by the vectorization nor splat values and therefore already vectorized,
            // we don't know how to handle them
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Skipping loop with unknown type of input dependencies: "
                    << vc4c::to_string<const Local*>(inputDependencies) << logging::endl);
            continue;
        }
        if(!outputDependencies.empty())
        {
            // These are locals written in the loop and read afterwards. Since the remaining locals in this set are
            // not accumulated, we don't know how to handle them
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Skipping loop with unknown type of output dependencies: "
                    << vc4c::to_string<const Local*>(outputDependencies) << logging::endl);
            continue;
        }

        // 4. determine vectorization factor
        auto vectorizationFactor = determineVectorizationFactor(loop, iterations);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Determined possible vectorization-factor of " << vectorizationFactor << logging::endl);
        if(vectorizationFactor <= 1)
        {
            // nothing to do
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Maximum vectorization factor is one, nothing to vectorize, aborting!" << logging::endl);
            continue;
        }

        // 5. cost-benefit calculation
        int rating = calculateCostsVsBenefits(loop, inductionVariable, vectorizationFactor,
            static_cast<unsigned>(accumulationsToFold.size()), dynamicElementCount.has_value());
        if(rating < 0 /* TODO some positive factor to be required before vectorizing loops? */)
        {
            // vectorization (probably) doesn't pay off
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Vectorization determined to be inefficient, aborting!" << logging::endl);

            continue;
        }

        // 6. check for divergent control flow depending on iteration variable
        if(!checkIterationVariableDependentControlFlow(method, loop, inductionVariable.local, vectorizationFactor))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Skipping loop with divergent control flow depending on induction variable '"
                    << inductionVariable.local->to_string() << "', aborting!" << logging::endl);
            continue;
        }

        // 7. run vectorization
        vectorize(loop, inductionVariable, method, vectorizationFactor, *stepConstant, accumulationsToFold,
            dynamicElementCount);
        // increasing the iteration step might create a value not fitting into small immediate
        normalization::handleImmediate(
            module, method, loop.findInLoop(inductionVariable.inductionStep).value(), config);
        ++numChanges;

        if(dynamicElementCount)
            PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_OPTIMIZATION, "Dynamic-sized vectorizations", 1);
        else
            PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_OPTIMIZATION, "Vectorization factors", vectorizationFactor);
    }

    return numChanges;
}

struct VectorFolding
{
    OpCode foldingOp;
    Value container;
    Value output;
    uint8_t numElements;
    uint8_t elementOffset;
    InstructionWalker lastFoldIt;
};

static Optional<VectorFolding> findElementwiseVectorFolding(InstructionWalker& it)
{
    using namespace pattern;
    /*
     * Both LLVM and SPIR-V front-ends generate vector folding like this:
     * - "extract" 0th element into temporary
     *   -> simple move of container
     * - extract Nth element into temporary
     *   -> rotate container down by the given index N (up by 16 - N)
     * - apply folding operation on previous result and value extracted from Nth element
     */

    Value container = UNDEFINED_VALUE;
    Value rotationOffset = UNDEFINED_VALUE;
    Value currentResult = UNDEFINED_VALUE;
    Value previousResult = UNDEFINED_VALUE;
    OpCode foldingOp = OP_NOP;
    Value foldingOutput = UNDEFINED_VALUE;
    Pattern firstElementFoldingPattern{{
        capture(currentResult) = (match(FAKEOP_ROTATE), capture(container), capture(rotationOffset)),
        capture(foldingOutput) =
            (capture(foldingOp), capture(previousResult), capture(currentResult), allowCommutation()),
    }};

    if(!(it = search(it, firstElementFoldingPattern, true)).isEndOfBlock())
    {
        std::bitset<NATIVE_VECTOR_SIZE> coveredIndices{};
        auto lastFoldIt = it;

        if(!foldingOp.isAssociative())
            return {};

        auto offset = check(rotationOffset.checkImmediate()) & &SmallImmediate::getRotationOffset;

        if(!offset)
            return {};

        // implicitly include base value (which for element offset of 0 is the container itself and therefore not
        // rotated and therefore not detected)
        auto baseOffset = static_cast<uint8_t>(NATIVE_VECTOR_SIZE - *offset - 1u);
        auto sourceContainer = intermediate::getSourceValue(container);

        if(baseOffset == 0 && intermediate::getSourceValue(previousResult) != sourceContainer)
            return {};
        else if(baseOffset != 0)
        {
            // make sure the other input of the first folding operation is actually the previous element
            auto currentRotation = dynamic_cast<const intermediate::VectorRotation*>(currentResult.getSingleWriter());
            auto previousRotation = dynamic_cast<const intermediate::VectorRotation*>(previousResult.getSingleWriter());

            if(!currentRotation || !previousRotation ||
                intermediate::getSourceValue(currentRotation->getSource()) != sourceContainer ||
                intermediate::getSourceValue(previousRotation->getSource()) != sourceContainer)
                return {};

            auto currentOffset = currentRotation->getVectorRotation()->offset.getRotationOffset();
            auto previousOffset = previousRotation->getVectorRotation()->offset.getRotationOffset();

            if(!currentOffset || !previousOffset)
                return {};

            /*
             * We have following valid options:
             *   %a = %container << (16 - BASE)
             *   %b = %container << (15 - BASE)
             *   %c = %a op %b
             * or (if the folding operation is commutative):
             *   %a = %container << (16 - BASE)
             *   %b = %container << (15 - BASE)
             *   %c = %b op %a
             */
            if(foldingOp.isCommutative() &&
                (NATIVE_VECTOR_SIZE - currentOffset.value()) == (NATIVE_VECTOR_SIZE - previousOffset.value() - 1u))
            {
                // we have our elements switched, so we need to unswitch them
                offset = previousOffset;
                baseOffset = static_cast<uint8_t>(NATIVE_VECTOR_SIZE - *offset - 1u);
            }
            else if((NATIVE_VECTOR_SIZE - currentOffset.value() - 1u) != (NATIVE_VECTOR_SIZE - previousOffset.value()))
                return {};
        }

        // set the first two elements accessed by the initial operation
        coveredIndices.set(baseOffset);
        coveredIndices.set(NATIVE_VECTOR_SIZE - *offset);

        while(!it.isEndOfBlock())
        {
            // we need to recreate the pattern every time, since the previous folding output is captured by copy, and we
            // need to set it to the output of the previous iteration, not the first folding output
            Pattern successiveElementFoldingPattern{{
                capture(pattern::V1) = (match(FAKEOP_ROTATE), match(container), capture(rotationOffset)),
                capture(foldingOutput) =
                    (match(foldingOp), capture(pattern::V1), match(foldingOutput), allowCommutation()),
            }};

            auto nextIt = it;
            if((nextIt = search(it, successiveElementFoldingPattern, true)).isEndOfBlock())
            {
                // make sure we have a continuous range of indices
                if((((1u << coveredIndices.count()) - 1u) << baseOffset) != coveredIndices.to_ulong())
                    // something weird is going on with the indices used, abort
                    break;

                if(!isPowerTwo(static_cast<uint32_t>(coveredIndices.count())) || coveredIndices.count() < 4)
                    // our improved folding is only applicable/faster for powers of 2 of at least 4
                    break;

                // end of this folding
                return VectorFolding{
                    foldingOp, container, foldingOutput, static_cast<uint8_t>(coveredIndices.count()), baseOffset, it};
            }
            // only forward global iterator if we found something, to not skip all other possible vector foldings in the
            // block
            it = nextIt;

            lastFoldIt = it;
            auto offset = check(rotationOffset.checkImmediate()) & &SmallImmediate::getRotationOffset;
            if(!offset || coveredIndices.test(NATIVE_VECTOR_SIZE - *offset))
                // something weird is going on with the indices used, abort
                break;
            coveredIndices.set(NATIVE_VECTOR_SIZE - *offset);
        }
    }
    return {};
}

std::size_t optimizations::compactVectorFolding(const Module& module, Method& method, const Configuration& config)
{
    std::size_t numChanges = 0;

    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(auto folding = findElementwiseVectorFolding(it))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Compacting vector folding of " << static_cast<unsigned>(folding->numElements)
                        << " elements with offset " << static_cast<unsigned>(folding->elementOffset) << " from "
                        << folding->container.to_string() << " into " << folding->output.to_string() << " over "
                        << folding->foldingOp.name << logging::endl);

                auto container = folding->container;
                if(folding->elementOffset != 0)
                {
                    container =
                        method.addNewLocal(folding->container.type.toVectorType(folding->numElements), "%vector_fold");
                    it = intermediate::insertVectorRotation(folding->lastFoldIt, folding->container,
                        Value(Literal(folding->elementOffset), TYPE_INT8), container, intermediate::Direction::DOWN);
                }
                else if(folding->numElements != container.type.getVectorWidth())
                    // need to truncate to container with the number of elements to actually fold
                    container = assign(it, folding->container.type.toVectorType(folding->numElements), "%vector_fold") =
                        container;

                it = insertFoldVector(
                    it, method, folding->output, container, folding->foldingOp, folding->lastFoldIt->decoration);
                // to not skip the next instruction
                it.previousInBlock();
                // remove original last write so our new code is used and the whole original folding cascade can be
                // removed as unused code
                folding->lastFoldIt.erase();
                ++numChanges;
            }
        }
    }

    return numChanges;
}

struct VectorElementCopy
{
    InstructionWalker staticFlagsSetter;
    std::bitset<NATIVE_VECTOR_SIZE> staticElementMask;
    InstructionWalker copyInstruction;
    ConditionCode copyCondition;
    Value source;
    const Local* destination;
    Unpack unpackMode;
    Pack packMode;
};

static Optional<std::bitset<NATIVE_VECTOR_SIZE>> toFlagsSetter(const VectorFlags& flags, ConditionCode cond)
{
    std::bitset<NATIVE_VECTOR_SIZE> mask{};
    for(std::size_t i = 0; i < flags.size(); ++i)
    {
        if(!flags[i].isFlagDefined(cond))
            return {};
        mask.set(i, flags[i].matchesCondition(cond));
    }
    return mask;
}

static SIMDVector toVectorMask(const std::bitset<NATIVE_VECTOR_SIZE>& mask, ConditionCode cond)
{
    SIMDVector result{Literal(0)};
    for(uint8_t i = 0; i < mask.size(); ++i)
    {
        if(cond == COND_ZERO_CLEAR)
            result[i] = mask.test(i) ? 1_lit : 0_lit;
        if(cond == COND_ZERO_SET)
            result[i] = mask.test(i) ? 0_lit : 1_lit;
        if(cond == COND_NEGATIVE_CLEAR)
            result[i] = mask.test(i) ? 0_lit : Literal(-1);
        if(cond == COND_NEGATIVE_SET)
            result[i] = mask.test(i) ? Literal(-1) : 0_lit;
    }
    return result;
}

std::size_t optimizations::combineVectorElementCopies(const Module& module, Method& method, const Configuration& config)
{
    std::size_t numChanges = 0;

    // run within all basic blocks
    for(auto& block : method)
    {
        Optional<VectorElementCopy> previousCopy;

        for(auto it = block.walk(); !it.isEndOfBlock(); it.nextInBlock())
        {
            if(it.has() && it->checkOutputLocal() && !it->hasSideEffects() && it->hasConditionalExecution())
            {
                auto flagsSetter = block.findLastSettingOfFlags(it);
                auto staticFlags = flagsSetter ?
                    StaticFlagsAnalysis::analyzeStaticFlags(flagsSetter->get(), *flagsSetter) :
                    Optional<VectorFlags>{};
                auto unpackMode = UNPACK_NOP;
                auto packMode = PACK_NOP;
                auto cond = COND_ALWAYS;
                if(auto extended = it.get<ExtendedInstruction>())
                {
                    cond = extended->getCondition();
                    packMode = extended->getPackMode();
                }
                if(auto unpacking = it.get<UnpackingInstruction>())
                    unpackMode = unpacking->getUnpackMode();

                Optional<std::bitset<NATIVE_VECTOR_SIZE>> staticMask;
                if(flagsSetter && (*flagsSetter)->writesRegister(REG_NOP) && staticFlags &&
                    (staticMask = toFlagsSetter(*staticFlags, cond)))
                {
                    if(previousCopy && it->getMoveSource() == previousCopy->source &&
                        it->checkOutputLocal() == previousCopy->destination && cond == previousCopy->copyCondition &&
                        previousCopy->unpackMode == unpackMode && previousCopy->packMode == packMode)
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Combining element-wise copies of elements "
                                << previousCopy->staticElementMask.to_string() << " and " << staticMask.to_string()
                                << " from and to the same values: " << it->to_string() << logging::endl);
                        it->decoration = remove_flag(it->decoration, InstructionDecorations::ELEMENT_INSERTION);
                        auto elementMask = toVectorMask(previousCopy->staticElementMask | *staticMask, cond);
                        if(auto lit = elementMask.getAllSame())
                            flagsSetter->reset(
                                createWithExtras<LoadImmediate>(*flagsSetter->get(), NOP_REGISTER, *lit));
                        else
                            flagsSetter->reset(createWithExtras<LoadImmediate>(*flagsSetter->get(), NOP_REGISTER,
                                LoadImmediate::fromLoadedValues(elementMask, LoadType::PER_ELEMENT_SIGNED),
                                LoadType::PER_ELEMENT_SIGNED));

                        previousCopy->staticFlagsSetter.erase();
                        previousCopy->staticFlagsSetter = *flagsSetter;
                        previousCopy->staticElementMask |= *staticMask;
                        previousCopy->copyInstruction.erase();
                        previousCopy->copyInstruction = it;
                        ++numChanges;
                    }
                    else if(auto src = it->getMoveSource())
                        previousCopy = VectorElementCopy{*flagsSetter, *staticMask, it, cond, src.value(),
                            it->checkOutputLocal(), unpackMode, packMode};

                    continue;
                }
            }
            if(it.has() && previousCopy &&
                (it->writesLocal(previousCopy->destination) || it->readsLocal(previousCopy->destination) ||
                    it->getOutput() == previousCopy->source))
                previousCopy = {};
        }
    }

    return numChanges;
}
