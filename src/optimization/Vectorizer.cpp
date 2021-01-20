/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Vectorizer.h"

#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/ControlFlowLoop.h"
#include "../analysis/DataDependencyGraph.h"
#include "../intermediate/Helper.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../normalization/LiteralValues.h"
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
    unsigned factor = NATIVE_VECTOR_SIZE / maxTypeWidth;
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
                if(it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_TMU0_ADDRESS) ||
                    it->writesRegister(REG_TMU1_ADDRESS))
                {
                    // for dynamic iteration counts, we need to insert a mask/dynamic element calculation per memory
                    // access
                    // XXX actually per loop iteration
                    costs += isDynamicIterationCount * (it->writesRegister(REG_VPM_DMA_LOAD_ADDR) ? 5 : 3);
                    for(const Value& arg : it->getArguments())
                    {
                        if(auto loc = arg.checkLocal())
                        {
                            readAddresses.emplace(loc);
                            if(auto data = loc->get<ReferenceData>())
                                readAddresses.emplace(data->base);
                            if(loc->residesInMemory())
                            {
                                // we directly read an absolute memory address (without any dynamic offset) inside the
                                // loop, we cannot vectorize this
                                CPPLOG_LAZY(logging::Level::DEBUG,
                                    log << "Cannot vectorize loops reading from absolute memory location: "
                                        << it->to_string() << logging::endl);
                                return std::numeric_limits<int>::min();
                            }
                        }
                    }
                }
                else if(it->writesRegister(REG_VPM_DMA_STORE_ADDR))
                {
                    // for dynamic iteration counts, we need to insert a mask/dynamic element calculation per memory
                    // access
                    // XXX actually per loop iteration
                    costs += isDynamicIterationCount * 4;
                    for(const Value& arg : it->getArguments())
                    {
                        if(auto loc = arg.checkLocal())
                        {
                            writtenAddresses.emplace(loc);
                            if(auto data = loc->get<ReferenceData>())
                                writtenAddresses.emplace(data->base);
                            if(loc->residesInMemory())
                            {
                                // we directly write an absolute memory address (without any dynamic offset) inside the
                                // loop, we cannot vectorize this
                                CPPLOG_LAZY(logging::Level::DEBUG,
                                    log << "Cannot vectorize loops writing to absolute memory location: "
                                        << it->to_string() << logging::endl);
                                return std::numeric_limits<int>::min();
                            }
                        }
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
    auto foldCosts = 3u * static_cast<unsigned>(std::ceil(std::log2(static_cast<double>(vectorizationFactor))));
    costs += foldCosts * numFoldings;
    // additional calculation of the dynamic element mask
    if(isDynamicIterationCount)
    {
        // replication for non-splat initial assignment
        costs += 2;
        // calculation of dynamic active element count
        costs += 4; // XXX actually per loop iteration
        // additional 1 instruction for masking the LCSSA variable with the element number per folding
        costs += numFoldings;
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

static void scheduleForVectorization(const Local* local,
    FastMap<const intermediate::IntermediateInstruction*, uint8_t>& openInstructions, ControlFlowLoop& loop,
    bool scheduleWriters, uint8_t minVectorWidth)
{
    local->forUsers(LocalUse::Type::READER, [&openInstructions, &loop, minVectorWidth](const LocalUser* user) -> void {
        if(!user->hasDecoration(intermediate::InstructionDecorations::AUTO_VECTORIZED))
            openInstructions.emplace(user, minVectorWidth);
        if(user->checkOutputRegister() &&
            (user->getOutput()->reg().isSpecialFunctionsUnit() || user->getOutput()->reg().isTextureMemoryUnit()))
        {
            // need to add the reading of SFU/TMU too
            if(auto optIt = loop.findInLoop(user))
            {
                InstructionWalker it = optIt.value().nextInBlock();
                while(!it.isEndOfBlock())
                {
                    if(it->readsRegister(REG_SFU_OUT) &&
                        !it->hasDecoration(intermediate::InstructionDecorations::AUTO_VECTORIZED))
                    {
                        openInstructions.emplace(it.get(), minVectorWidth);
                        break;
                    }

                    it.nextInBlock();
                }
            }
        }
    });
    if(scheduleWriters)
    {
        local->forUsers(LocalUse::Type::WRITER, [&openInstructions, minVectorWidth](const LocalUser* user) -> void {
            if(!user->hasDecoration(intermediate::InstructionDecorations::AUTO_VECTORIZED))
                openInstructions.emplace(user, minVectorWidth);
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

    if(inst->writesRegister(REG_TMU0_ADDRESS) || inst->writesRegister(REG_TMU1_ADDRESS))
    {
        if(!it)
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Cannot remove splat decoration from TMU read without knowing its position");
        auto checkIt = it->copy().nextInBlock();
        while(!checkIt.isEndOfBlock())
        {
            if(checkIt->readsRegister(REG_TMU_OUT))
            {
                removeSplatDecoration(checkIt.get(), checkIt);
                break;
            }
            checkIt.nextInBlock();
        }
        if(checkIt.isEndOfBlock())
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Failed to find TMU value read for no longer identical TMU address write", inst->to_string());
    }
    if(auto loc = inst->checkOutputLocal())
        loc->forUsers(LocalUse::Type::READER,
            [=](const LocalUser* reader) { removeSplatDecoration(const_cast<LocalUser*>(reader), it); });
}

static void vectorizeInstruction(intermediate::IntermediateInstruction* inst, Method& method,
    FastMap<const intermediate::IntermediateInstruction*, uint8_t>& openInstructions, unsigned vectorizationFactor,
    ControlFlowLoop& loop, uint8_t minVectorWidth, const Optional<Value>& dynamicElementCount)
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
                scheduleForVectorization(loc, openInstructions, loop, true, vectorWidth);
        }
    }

    // 2. depending on operation performed, update type of output
    if(inst->getOutput() &&
        (dynamic_cast<const intermediate::Operation*>(inst) || dynamic_cast<const intermediate::MoveOperation*>(inst)))
    {
        // TODO vector-rotations need special handling?!
        Value& out = const_cast<Value&>(inst->getOutput().value());
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
                    loc->type.getPointerType()->elementType.toVectorType(getVectorWidth(out.type)),
                    ptrType->addressSpace);
            else
                const_cast<DataType&>(loc->type) = loc->type.toVectorType(getVectorWidth(out.type));
            scheduleForVectorization(loc, openInstructions, loop, false, vectorWidth);
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
            scheduleForVectorization(newLoc.local(), openInstructions, loop, false, vectorWidth);
        }
    }

    if(inst->writesRegister(REG_TMU0_ADDRESS) || inst->writesRegister(REG_TMU1_ADDRESS))
    {
        // if we write to a TMU address register, we need to adapt the setting of valid TMU address elements (vs.
        // zeroing out) to match the new vector width
        auto addressIt = loop.findInLoop(inst);
        if(!addressIt)
            // TODO not actually a problem, we just have to find the instruction walker
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Cannot vectorize setting TMU address outside of loop", inst->to_string());

        if(dynamicElementCount)
        {
            /*
             * If we have a dynamic active element count, we need to mask off the elements not actually used, since
             * otherwise we might read memory which is not mapped at all.
             *
             * So we need to set all non-active elements to zero to tell the TMU to not load anything into there.
             *
             * NOTE: This code requires the dynamicElementCount value to be a splat value!
             */
            auto it = *addressIt;
            auto maskedAddress = assign(it, inst->assertArgument(0).type, "%masked_address") = INT_ZERO;
            auto cond = assignNop(it) = as_signed{ELEMENT_NUMBER_REGISTER} < as_signed{*dynamicElementCount};
            if(auto source = inst->getMoveSource())
                assign(it, maskedAddress) = (*source, cond);
            else if(dynamic_cast<Operation*>(inst) && dynamic_cast<Operation*>(inst)->op == OP_ADD)
                assign(it, maskedAddress) = (inst->assertArgument(0) + inst->assertArgument(1), cond);
            else
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Masking off TMU addresses dynamically for this kind of calculation is not yet implemented",
                    inst->to_string());
            it.reset((new MoveOperation(inst->getOutput().value(), maskedAddress))->copyExtrasFrom(inst));
            openInstructions.erase(inst);
            inst = it.get();
        }
    }

    // TODO need to adapt types of some registers/output of load, etc.?
    // TODO cosmetic errors: depending on the order of vectorization, some locals are written as vectors, but read as
    // scalars, if the read-instruction was vectorized before the write-instruction

    // mark as already processed and remove from open-set
    inst->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
    openInstructions.erase(inst);
}

static std::size_t fixVPMSetups(
    Method& method, ControlFlowLoop& loop, unsigned vectorizationFactor, const Optional<Value>& dynamicElementCount)
{
    std::size_t numVectorized = 0;

    for(auto& node : loop)
    {
        auto it = node->key->walk();
        while(!it.isEndOfBlock())
        {
            if(it->writesRegister(REG_VPM_OUT_SETUP))
            {
                periphery::VPWSetupWrapper vpwSetup(static_cast<intermediate::LoadImmediate*>(nullptr));
                if(auto load = it.get<intermediate::LoadImmediate>())
                    vpwSetup = periphery::VPWSetupWrapper(load);
                else if(auto move = it.get<intermediate::MoveOperation>())
                    vpwSetup = periphery::VPWSetupWrapper(move);
                else
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Unsupported instruction to write VPM for vectorized value", it->to_string());

                auto relatedInstructions = periphery::findRelatedVPMInstructions(it, false);
                bool isVPMWriteVectorized = relatedInstructions.vpmAccess &&
                    (*relatedInstructions.vpmAccess)->hasDecoration(InstructionDecorations::AUTO_VECTORIZED);
                bool isDMAAddressVectorized = relatedInstructions.addressWrite &&
                    (*relatedInstructions.addressWrite)->hasDecoration(InstructionDecorations::AUTO_VECTORIZED);
                if(vpwSetup.isDMASetup() && (isVPMWriteVectorized || isDMAAddressVectorized))
                {
                    // Since this is only true for values actually vectorized, the corresponding VPM-write is checked
                    if(dynamicElementCount)
                    {
                        // need to dynamically calculate the number of elements
                        auto oldDepth = vpwSetup.dmaSetup.getDepth();
                        vpwSetup.dmaSetup.setDepth(0);
                        auto tmpSetup = method.addNewLocal(TYPE_INT32, "%vpm_setup");
                        it->replaceValue(VPM_OUT_SETUP_REGISTER, tmpSetup, LocalUse::Type::WRITER);
                        it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
                        it.nextInBlock();
                        auto numElements = assign(it, TYPE_INT32, "%dynamic_depth") =
                            (*dynamicElementCount * Literal(oldDepth), InstructionDecorations::AUTO_VECTORIZED);
                        auto dynamicDepth = assign(it, TYPE_INT32, "%dynamic_depth") =
                            (numElements << 16_val, InstructionDecorations::AUTO_VECTORIZED);
                        it.emplace(new Operation(OP_ADD, VPM_OUT_SETUP_REGISTER, tmpSetup, dynamicDepth));
                        it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
                        numVectorized += 4;
                    }
                    else
                    {
                        vpwSetup.dmaSetup.setDepth(
                            static_cast<uint8_t>(vpwSetup.dmaSetup.getDepth() * vectorizationFactor));
                        ++numVectorized;
                        it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
                    }
                }
            }
            else if(it->writesRegister(REG_VPM_IN_SETUP))
            {
                periphery::VPRSetupWrapper vprSetup(static_cast<intermediate::LoadImmediate*>(nullptr));
                if(auto load = it.get<intermediate::LoadImmediate>())
                    vprSetup = periphery::VPRSetupWrapper(load);
                else if(auto move = it.get<intermediate::MoveOperation>())
                    vprSetup = periphery::VPRSetupWrapper(move);
                else
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Unsupported instruction to write VPM for vectorized value", it->to_string());

                auto relatedInstructions = periphery::findRelatedVPMInstructions(it, true);
                bool isVPMReadVectorized = relatedInstructions.vpmAccess &&
                    (*relatedInstructions.vpmAccess)->hasDecoration(InstructionDecorations::AUTO_VECTORIZED);
                bool isDMAAddressVectorized = relatedInstructions.addressWrite &&
                    (*relatedInstructions.addressWrite)->hasDecoration(InstructionDecorations::AUTO_VECTORIZED);
                if(vprSetup.isDMASetup() && (isVPMReadVectorized || isDMAAddressVectorized))
                {
                    // See VPM write
                    if(dynamicElementCount)
                    {
                        // need to dynamically calculate the number of elements
                        auto oldRowLength = vprSetup.dmaSetup.getRowLength();
                        vprSetup.dmaSetup.setRowLength(0);
                        auto tmpSetup = method.addNewLocal(TYPE_INT32, "%vpm_setup");
                        it->replaceValue(VPM_IN_SETUP_REGISTER, tmpSetup, LocalUse::Type::WRITER);
                        it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
                        it.nextInBlock();
                        auto numElements = assign(it, TYPE_INT32, "%dynamic_rowlength") =
                            (*dynamicElementCount * Literal(oldRowLength), InstructionDecorations::AUTO_VECTORIZED);
                        // 0 => 16, so we need to truncate to not override some other bit by accident
                        auto finalNumElements = assign(it, TYPE_INT32, "%dynamic_rowlength") =
                            (numElements & 15_val, InstructionDecorations::AUTO_VECTORIZED);
                        if(vprSetup.dmaSetup.getMode() > 1 && oldRowLength > 1)
                        {
                            // if we end up with more than 16 elements in a row, we truncate back (see above). To fix
                            // this, we need to switch to a higher element size
                            // TODO correct in general or only for byte-wise copy? Even applicable for anything except
                            // byte-wise copy? Does anywhere else have a row length of more than 1 while still not
                            // accessing vectors?
                            auto modeChange = assign(it, TYPE_INT32, "%dynamic_mode") =
                                (as_unsigned{numElements} >> 4_val, InstructionDecorations::AUTO_VECTORIZED);
                            modeChange = assign(it, TYPE_INT32, "%dynamic_mode") =
                                (0_val - modeChange, InstructionDecorations::AUTO_VECTORIZED);
                            modeChange = assign(it, TYPE_INT32, "%dynamic_mode") =
                                (modeChange << 8_val, InstructionDecorations::AUTO_VECTORIZED);
                            finalNumElements = assign(it, TYPE_INT32, "%dynamic_setup") =
                                (modeChange | finalNumElements, InstructionDecorations::AUTO_VECTORIZED);
                        }
                        auto dynamicRowLength = assign(it, TYPE_INT32, "%dynamic_rowlength") =
                            (finalNumElements << 20_val, InstructionDecorations::AUTO_VECTORIZED);
                        it.emplace(new Operation(OP_ADD, VPM_IN_SETUP_REGISTER, tmpSetup, dynamicRowLength));
                        it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
                        numVectorized += 5;
                    }
                    else
                    {
                        vprSetup.dmaSetup.setRowLength(
                            (vprSetup.dmaSetup.getRowLength() * vectorizationFactor) % 16 /* 0 => 16 */);
                        ++numVectorized;
                        it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
                    }
                }
            }

            it.nextInBlock();
        }
    }

    return numVectorized;
}

/*
 * Makes sure, the predecessor-node and the instruction-walker are found in correct order
 */
static Optional<InstructionWalker> findWalker(const CFGNode* node, const intermediate::IntermediateInstruction* inst)
{
    return node != nullptr ? node->key->findWalkerForInstruction(inst, node->key->walkEnd()) :
                             Optional<InstructionWalker>{};
}

static void fixInitialValueAndStep(Method& method, ControlFlowLoop& loop, InductionVariable& inductionVariable,
    Literal stepValue, unsigned vectorizationFactor)
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
        move->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else if(move != nullptr && move->getSource().getLiteralValue() && isStepPlusOne &&
        (initialValueWalker = findWalker(loop.findPredecessor(), move)))
    {
        // more general case: initial value is a literal and step is +1
        initialValueWalker->reset(
            (new intermediate::Operation(OP_ADD, move->getOutput().value(), move->getSource(), ELEMENT_NUMBER_REGISTER))
                ->copyExtrasFrom(move));
        initialValueWalker.value()->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
        inductionVariable.initialAssignment = initialValueWalker->get();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else if((precalculatedInitialValue = inductionVariable.initialAssignment->precalculate().first) && isStepPlusOne &&
        (initialValueWalker = findWalker(loop.findPredecessor(), inductionVariable.initialAssignment)))
    {
        // more general case: initial value is some constant and step is +1
        initialValueWalker->reset(
            (new intermediate::Operation(OP_ADD, inductionVariable.initialAssignment->getOutput().value(),
                 *precalculatedInitialValue, ELEMENT_NUMBER_REGISTER))
                ->copyExtrasFrom(inductionVariable.initialAssignment));
        initialValueWalker.value()->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
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
            (new intermediate::Operation(
                 OP_ADD, inductionVariable.initialAssignment->getOutput().value(), source, ELEMENT_NUMBER_REGISTER))
                ->copyExtrasFrom(inductionVariable.initialAssignment));
        (*initialValueWalker)->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
        inductionVariable.initialAssignment = initialValueWalker->get();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Changed initial value: " << inductionVariable.initialAssignment->to_string() << logging::endl);
    }
    else
        throw CompilationError(
            CompilationStep::OPTIMIZER, "Unhandled initial value", inductionVariable.initialAssignment->to_string());

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
    unsigned vectorizationFactor, const Optional<Value>& dynamicElementCount)
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
            (*opIt)->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
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
            auto tmp = method.addNewLocal(mask.type, "%cond_mask");
            it.emplace(new LoadImmediate(tmp, 0xFFFE, LoadType::PER_ELEMENT_UNSIGNED));
            it->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
            it.nextInBlock();
            if(inductionVariable.conditionCheckedBeforeStep)
            {
                /*
                 * Comparison before the step, thus we need to check the current dynamic highest active element
                 */
                // we have the number of elements, but need the highest element offset
                auto tmpOffset = assign(it, dynamicElementCount->type, "%mask_offset") = (*dynamicElementCount - 1_val);
                it = insertVectorRotation(it, tmp, tmpOffset, mask);
                auto decoIt = it.copy().previousInBlock();
                // add decoration to the vector rotation inserted just now
                if(decoIt.has() && decoIt->getVectorRotation())
                    decoIt->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
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
            it.emplace(
                new LoadImmediate(mask, 0xFFFF ^ (1u << (vectorizationFactor - 1)), LoadType::PER_ELEMENT_UNSIGNED));
            it->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
            it.nextInBlock();
        }
        it->replaceValue(ELEMENT_NUMBER_REGISTER, mask, LocalUse::Type::READER);
        it->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
        ++numRewritten;
    });

    return numRewritten;
}

/**
 * Calculate the dynamic active element count for this iteration at the very top of the loop head to make sure it is
 * available throughout the loop.
 */
static unsigned calculateDynamicElementCount(Method& method, ControlFlowLoop& loop,
    InductionVariable& inductionVariable, unsigned vectorizationFactor, const Value& dynamicElementCount)
{
    auto head = loop.getHeader();
    if(!head)
        throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find head for loop", loop.to_string());

    auto it = head->key->walk().nextInBlock();
    // TODO need to invert for decrementing step??
    auto tmp = assign(it, dynamicElementCount.type, std::string{dynamicElementCount.local()->name}) =
        (inductionVariable.repeatCondition.value().comparisonValue - inductionVariable.local->createReference(),
            InstructionDecorations::AUTO_VECTORIZED);
    tmp = assign(it, dynamicElementCount.type, std::string{dynamicElementCount.local()->name}) =
        (min(as_signed{Value(Literal(vectorizationFactor), TYPE_INT8)}, as_signed{tmp}),
            InstructionDecorations::AUTO_VECTORIZED);
    it = insertReplication(it, tmp, dynamicElementCount);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Inserted calculation of dynamic active element count into: " << dynamicElementCount.to_string()
            << logging::endl);

    return 2;
}

struct AccumulationInfo
{
    const Local* local;
    OpCode op;
    tools::SmallSortedPointerSet<const Local*> outputLocals;
    tools::SmallSortedPointerSet<const LocalUser*> toBeFolded;
    tools::SmallSortedPointerSet<const Local*> toBeMasked;
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
    const FastMap<const Local*, AccumulationInfo>& accumulationsToFold, const Value& dynamicElementCount)
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
            auto insertIt = lastSettingIt->emplace(inLoopIt->release());
            inLoopIt->erase();

            auto cond = assignNop(insertIt) = (as_signed{ELEMENT_NUMBER_REGISTER} < as_signed{dynamicElementCount},
                InstructionDecorations::AUTO_VECTORIZED);
            insertIt.get<ExtendedInstruction>()->setCondition(cond);

            // The instruction itself is conditional, so it is only assigned if the loop is not entered. We need to
            // change that to always assign the default value
            // TODO is this true for a different initial value than the unit of the accumulation operation?
            // TODO if initial value is not splat, need to replicate it!
            outOfLoopInst->setCondition(COND_ALWAYS);
            outOfLoopInst->addDecorations(InstructionDecorations::AUTO_VECTORIZED);

            numModified += 3;
        }
    }

    return numModified;
}

/*
 * Approach:
 * - set the iteration variable (local) to vector
 * - iterative (until no more values changed), modify all value (and local)-types so argument/result-types match again
 * - add new instruction-decoration (vectorized) to facilitate
 * - in final iteration, fix TMU/VPM configuration and address calculation and loop condition
 * - fix initial iteration value and step
 */
static void vectorize(ControlFlowLoop& loop, InductionVariable& inductionVariable, Method& method,
    unsigned vectorizationFactor, Literal stepValue, const FastMap<const Local*, AccumulationInfo>& accumulationsToFold,
    const Optional<Value>& dynamicElementCount)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Vectorizing loop '" << loop.to_string() << "' with factor of " << vectorizationFactor
            << (dynamicElementCount ? " and dynamic element count" : "") << "..." << logging::endl);
    FastMap<const intermediate::IntermediateInstruction*, uint8_t> openInstructions;

    const_cast<DataType&>(inductionVariable.local->type) = inductionVariable.local->type.toVectorType(
        static_cast<unsigned char>(inductionVariable.local->type.getVectorWidth() * vectorizationFactor));
    scheduleForVectorization(
        inductionVariable.local, openInstructions, loop, false, static_cast<uint8_t>(vectorizationFactor));
    std::size_t numVectorized = 0;

    // iteratively change all instructions
    while(!openInstructions.empty())
    {
        auto instIt = openInstructions.begin();
        auto inst = instIt->first;
        if(auto it = loop.findInLoop(inst))
        {
            vectorizeInstruction(
                it->get(), method, openInstructions, vectorizationFactor, loop, instIt->second, dynamicElementCount);
            ++numVectorized;
        }
        else if((inst->isSimpleMove() || dynamic_cast<const LoadImmediate*>(inst)) && inst->checkOutputLocal())
        {
            // follow all simple moves to other locals (to find the instruction we really care about)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local is accessed outside of loop: " << inst->to_string() << logging::endl);
            vectorizeInstruction(const_cast<intermediate::IntermediateInstruction*>(inst), method, openInstructions,
                vectorizationFactor, loop, instIt->second, dynamicElementCount);
            ++numVectorized;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local is accessed outside of loop: " << inst->to_string() << logging::endl);

            auto foldIt =
                std::find_if(accumulationsToFold.begin(), accumulationsToFold.end(), [&](const auto& entry) -> bool {
                    return entry.second.toBeFolded.find(inst) != entry.second.toBeFolded.end();
                });
            if(foldIt == accumulationsToFold.end())
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Non-folding access of vectorized locals outside of the loop or is not yet implemented",
                    inst->to_string());

            // fold vectorized version into "scalar" version by applying the accumulation function
            // NOTE: The "scalar" version is not required to be actual scalar (vector-width of 1), could just be
            // some other smaller vector-width
            if((inst->getArguments().size() == 1 || inst->assertArgument(1) == inst->assertArgument(0)) &&
                inst->assertArgument(0).checkLocal())
            {
                auto arg = inst->assertArgument(0);
                // since we are in the process of vectorization, the local type is already updated, but the argument
                // type is pending. For the next steps, we need to update the argument type.
                arg.type = arg.local()->type;
                auto origVectorType =
                    arg.type.toVectorType(static_cast<uint8_t>(arg.type.getVectorWidth() / vectorizationFactor));

                Optional<InstructionWalker> it;
                if(auto succ = loop.findSuccessor())
                    it = succ->key->findWalkerForInstruction(inst, succ->key->walkEnd());
                if(it)
                {
                    // insert folding or argument, heed original vector width
                    auto newArg = method.addNewLocal(origVectorType, "%vector_fold");
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Folding vectorized local " << arg.to_string() << " into " << newArg.to_string()
                            << " for: " << inst->to_string() << logging::endl);
                    ignoreReturnValue(insertFoldVector(
                        *it, method, newArg, arg, foldIt->second.op, InstructionDecorations::AUTO_VECTORIZED));
                    // replace argument with folded version
                    const_cast<IntermediateInstruction*>(inst)->replaceValue(arg, newArg, LocalUse::Type::READER);
                    openInstructions.erase(inst);
                    continue;
                }
            }

            throw CompilationError(CompilationStep::OPTIMIZER,
                "Vector folding for this instruction is not yet implemented", inst->to_string());
        }
    }

    numVectorized += fixVPMSetups(method, loop, vectorizationFactor, dynamicElementCount);

    fixInitialValueAndStep(method, loop, inductionVariable, stepValue, vectorizationFactor);
    numVectorized += 2;

    numVectorized += fixRepetitionBranch(method, loop, inductionVariable, vectorizationFactor, dynamicElementCount);

    if(dynamicElementCount)
    {
        numVectorized +=
            calculateDynamicElementCount(method, loop, inductionVariable, vectorizationFactor, *dynamicElementCount);
        numVectorized += fixLCSSAElementMask(method, loop, accumulationsToFold, *dynamicElementCount);
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Vectorization done, changed " << numVectorized << " instructions!" << logging::endl);
}

static bool readsOutput(const Local* input, const Local* output, OpCode op)
{
    if(input == output)
        return true;
    auto writers = input->getUsers(LocalUse::Type::WRITER);
    if(std::any_of(writers.begin(), writers.end(), [&](const LocalUser* writer) -> bool {
           if(auto loc = writer->getMoveSource() & &Value::checkLocal)
               return readsOutput(loc, output, op);
           auto writeOp = dynamic_cast<const Operation*>(writer);
           if(writeOp && writeOp->op == op)
               // For now we only check whether the read is with the same operation. TODO is this required? If we remove
               // this though, we need to make sure, we don't run into some stack overflow
               return std::any_of(writer->getArguments().begin(), writer->getArguments().end(),
                   [&](const Value& arg) -> bool { return arg.checkLocal() && readsOutput(arg.local(), output, op); });
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
Optional<AccumulationInfo> determineAccumulation(const Local* loc, const ControlFlowLoop& loop)
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
                predecessor->key->findWalkerForInstruction(user.first, predecessor->key->walkEnd()))
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

    auto op = dynamic_cast<const intermediate::Operation*>(loopOperationRead);
    if(!op || op->op.numOperands != 2)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Loop operation is not a valid accumulation operation: " << loopOperationRead->to_string()
                << logging::endl);
        return {};
    }

    if(initialWrite->precalculate().first != OpCode::getLeftIdentity(op->op))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Accumulation with non-identity initial value is not yet supported: " << initialWrite->to_string()
                << logging::endl);
        return {};
    }

    // make sure the input of the loop read is actually at some point the loop write output
    if(std::none_of(loopWrite->getArguments().begin(), loopWrite->getArguments().end(),
           [&](const Value& arg) -> bool { return arg.checkLocal() && readsOutput(arg.local(), loc, op->op); }))
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
        log << "Local '" << loc->to_string() << "' is accumulated with operation '" << op->op.name
            << "' in: " << vc4c::to_string<const LocalUser*>(toBeFolded) << logging::endl);

    return AccumulationInfo{loc, op->op, std::move(outputLocals), std::move(toBeFolded), std::move(toBeMasked)};
}

bool optimizations::vectorizeLoops(const Module& module, Method& method, const Configuration& config)
{
    if(method.empty())
        return false;
    // 1. find loops
    auto& cfg = method.getCFG();
    auto loops = cfg.findLoops(false);
    bool hasChanged = false;

    // 2. determine data dependencies of loop bodies
    auto dependencyGraph = DataDependencyGraph::createDependencyGraph(method);

    for(auto& loop : loops)
    {
        // 3. determine operation on iteration variable and bounds
        auto inductionVariable = extractLoopControl(loop, *dependencyGraph);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + 333, "Loops found", 1);
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

        // 6. run vectorization
        vectorize(loop, inductionVariable, method, vectorizationFactor, *stepConstant, accumulationsToFold,
            dynamicElementCount);
        // increasing the iteration step might create a value not fitting into small immediate
        normalization::handleImmediate(
            module, method, loop.findInLoop(inductionVariable.inductionStep).value(), config);
        hasChanged = true;

        if(dynamicElementCount)
            PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + 335, "Dynamic-sized vectorizations", 1);
        else
            PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + 334, "Vectorization factors", vectorizationFactor);
    }

    return hasChanged;
}
