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
static Optional<unsigned> determineVectorizationFactor(
    const ControlFlowLoop& loop, const InductionVariable& inductionVariable)
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

    // the number of iterations from the bounds depends on the iteration operation
    auto iterations = inductionVariable.getIterationCount();
    if(!iterations)
        return {};

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Determined iteration count of " << *iterations << logging::endl);

    // find the biggest factor fitting into 16 SIMD-elements
    unsigned factor = 16 / maxTypeWidth;
    while(factor > 0)
    {
        // TODO factors not in [1,2,3,4,8,16] possible?? Should be from hardware-specification side
        if((*iterations % factor) == 0)
            break;
        --factor;
    }
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Determined possible vectorization-factor of " << factor << logging::endl);
    return factor;
}

/*
 * On the cost-side, we have (as increments):
 * - instructions inserted to construct vectors from scalars
 * - additional delay for writing larger vectors through VPM
 * - memory address is read and written from within loop -> abort
 * - vector rotations -> for now abort
 *
 * On the benefit-side, we have (as factors):
 * - the iterations saved (times the number of instructions in an iteration)
 */
static int calculateCostsVsBenefits(const ControlFlowLoop& loop, const InductionVariable& inductionVariable,
    const DataDependencyGraph& dependencyGraph, unsigned vectorizationFactor)
{
    int costs = 0;

    SortedSet<const Local*> readAddresses;
    SortedSet<const Local*> writtenAddresses;

    for(const auto& node : loop)
    {
        for(const auto& it : *node->key)
        {
            if(it)
            {
                if(it->getOutput() & [](const Value& out) -> bool {
                       return out.hasRegister(REG_VPM_DMA_LOAD_ADDR) || out.hasRegister(REG_TMU0_ADDRESS) ||
                           out.hasRegister(REG_TMU1_ADDRESS);
                   })
                {
                    for(const Value& arg : it->getArguments())
                    {
                        if(auto loc = arg.checkLocal())
                        {
                            readAddresses.emplace(loc);
                            if(auto data = loc->get<ReferenceData>())
                                readAddresses.emplace(data->base);
                        }
                    }
                }
                else if(it->getOutput() && it->getOutput()->hasRegister(REG_VPM_DMA_STORE_ADDR))
                {
                    for(const Value& arg : it->getArguments())
                    {
                        if(auto loc = arg.checkLocal())
                        {
                            writtenAddresses.emplace(loc);
                            if(auto data = loc->get<ReferenceData>())
                                writtenAddresses.emplace(data->base);
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

    int numInstructions = 0;
    for(const CFGNode* node : loop)
    {
        // XXX to be exact, would need to include delays here too
        numInstructions += static_cast<int>(node->key->size());
    }
    // the number of instructions/cycles saved
    int benefits = numInstructions * static_cast<int>(vectorizationFactor);

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

static void vectorizeInstruction(intermediate::IntermediateInstruction* inst, Method& method,
    FastMap<const intermediate::IntermediateInstruction*, uint8_t>& openInstructions, unsigned vectorizationFactor,
    ControlFlowLoop& loop, uint8_t minVectorWidth)
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
            if(!loc->is<Parameter>())
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
    }

    if(inst->writesRegister(REG_TMU0_ADDRESS) || inst->writesRegister(REG_TMU1_ADDRESS))
    {
        // if we write to a TMU address register, we need to adapt the setting of valid TMU address elements (vs.
        // zeroing out) to match the new vector width
        auto optIt = loop.findInLoop(inst);
        if(!optIt)
            // TODO not actually a problem, we just have to find the instruction walker
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Cannot vectorize setting TMU address outside of loop", inst->to_string());

        optIt = optIt->getBasicBlock()->findLastSettingOfFlags(*optIt);
        if(!optIt)
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Failed to find setting of element-wise flags for setting of TMU address", inst->to_string());
        logging::debug() << "Vectorizing TMU load element mask: " << (*optIt)->to_string() << logging::endl;
        if((*optIt)->getMoveSource() & [](const Value val) -> bool { return val.hasRegister(REG_ELEMENT_NUMBER); })
        {
            // Rewrite single element selection via move elem_num to multi-element selection via per-element loads. This
            // way we do not need to change any flags
            optIt->reset((new LoadImmediate((*optIt)->getOutput().value(), 0xFFFF ^ ((1 << vectorWidth) - 1),
                              LoadType::PER_ELEMENT_UNSIGNED))
                             ->copyExtrasFrom(optIt->get()));
        }
        else if(auto op = optIt->get<intermediate::Operation>())
        {
            if(op->op != OP_SUB || !op->assertArgument(0).hasRegister(REG_ELEMENT_NUMBER) ||
                !op->assertArgument(1).getLiteralValue())
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Unhandled instruction setting flags for valid TMU address elements", (*optIt)->to_string());
            // we have instruction - = elem_num - vector_width (setf)
            op->setArgument(1, Value(Literal(vectorWidth), TYPE_INT8));
        }
        else
            // TODO make more robust!
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Unhandled instruction setting flags for valid TMU address elements", (*optIt)->to_string());
        (*optIt)->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
        // TODO if the previous size was 1 and the input is not directly an UNIFORM value, we need to replicate the base
        // address too!

        // when writing to TMU address register, we also need to change the element address offset, since it is added
        // twice (once for the modified original index and once for the TMU loading element address offset).
        // The TMU address is set conditionally depending on the above setting of flags to either zero or the base
        // address + element offset
        auto firstArg = inst->getArgument(0);
        if(dynamic_cast<MoveOperation*>(inst) == nullptr || !firstArg || firstArg->checkLocal() == nullptr ||
            firstArg->local()->countUsers(LocalUse::Type::WRITER) != 2)
            // TODO make more robust!
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Unhandled instruction setting TMU address elements", inst->to_string());
    }

    // TODO need to adapt types of some registers/output of load, etc.?
    // TODO cosmetic errors: depending on the order of vectorization, some locals are written as vectors, but read as
    // scalars, if the read-instruction was vectorized before the write-instruction

    // mark as already processed and remove from open-set
    inst->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
    openInstructions.erase(inst);
}

static std::size_t fixVPMSetups(ControlFlowLoop& loop, unsigned vectorizationFactor)
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

                auto vpmWrite = periphery::findRelatedVPMInstructions(it, false).vpmAccess;
                if(vpwSetup.isDMASetup() && vpmWrite &&
                    (*vpmWrite)->hasDecoration(intermediate::InstructionDecorations::AUTO_VECTORIZED))
                {
                    // Since this is only true for values actually vectorized, the corresponding VPM-write is checked
                    vpwSetup.dmaSetup.setDepth(
                        static_cast<uint8_t>(vpwSetup.dmaSetup.getDepth() * vectorizationFactor));
                    ++numVectorized;
                    it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
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

                auto vpmRead = periphery::findRelatedVPMInstructions(it, true).vpmAccess;
                if(vprSetup.isDMASetup() && vpmRead &&
                    (*vpmRead)->hasDecoration(intermediate::InstructionDecorations::AUTO_VECTORIZED))
                {
                    // See VPM write
                    vprSetup.dmaSetup.setRowLength(
                        (vprSetup.dmaSetup.getRowLength() * vectorizationFactor) % 16 /* 0 => 16 */);
                    ++numVectorized;
                    it->addDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
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

static void fixInitialValueAndStep(
    ControlFlowLoop& loop, InductionVariable& inductionVariable, Literal stepValue, unsigned vectorizationFactor)
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

static unsigned fixRepetitionBranch(
    Method& method, ControlFlowLoop& loop, InductionVariable& inductionVariable, unsigned vectorizationFactor)
{
    // If the branch is on scalar, it usually is converted to flags via "register - = or %cond, element_number", which
    // will mask the zero/non-zero flags for all upper elements. This needs to be fixes

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
        // Since the uppermost (used) element will cross the limit first, we move the check to that element. This
        // removes the need to rewrite the actual repetition branch, since we retain the (zero/not-zero) flag behavior.
        // TODO is this correct in all cases?
        it.emplace(new LoadImmediate(mask, 0xFFFF ^ (1u << (vectorizationFactor - 1)), LoadType::PER_ELEMENT_UNSIGNED));
        it->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
        it.nextInBlock();
        it->replaceValue(ELEMENT_NUMBER_REGISTER, mask, LocalUse::Type::READER);
        it->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
        ++numRewritten;
    });

    return numRewritten;
}

struct AccumulationInfo
{
    const Local* local;
    OpCode op;
    tools::SmallSortedPointerSet<const LocalUser*> toBeFolded;
};

/*
 * Approach:
 * - set the iteration variable (local) to vector
 * - iterative (until no more values changed), modify all value (and local)-types so argument/result-types match again
 * - add new instruction-decoration (vectorized) to facilitate
 * - in final iteration, fix TMU/VPM configuration and address calculation and loop condition
 * - fix initial iteration value and step
 */
static void vectorize(ControlFlowLoop& loop, InductionVariable& inductionVariable, Method& method,
    unsigned vectorizationFactor, Literal stepValue, const FastMap<const Local*, AccumulationInfo>& accumulationsToFold)
{
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
            vectorizeInstruction(it->get(), method, openInstructions, vectorizationFactor, loop, instIt->second);
            ++numVectorized;
        }
        else if(inst->isSimpleMove() && inst->checkOutputLocal() && !inst->hasSideEffects())
        {
            // follow all simple moves to other locals (to find the instruction we really care about)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local is accessed outside of loop: " << inst->to_string() << logging::endl);
            vectorizeInstruction(const_cast<intermediate::IntermediateInstruction*>(inst), method, openInstructions,
                vectorizationFactor, loop, instIt->second);
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

    numVectorized += fixVPMSetups(loop, vectorizationFactor);

    fixInitialValueAndStep(loop, inductionVariable, stepValue, vectorizationFactor);
    numVectorized += 2;

    numVectorized += fixRepetitionBranch(method, loop, inductionVariable, vectorizationFactor);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Vectorization done, changed " << numVectorized << " instructions!" << logging::endl);
}

/*
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
    for(auto& user : loc->getUsers())
    {
        auto loopIt = loop.findInLoop(user.first);
        if(user.second.writesLocal())
        {
            if(!loopIt && !user.first->hasConditionalExecution())
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

    tools::SmallSortedPointerSet<const LocalUser*> toBeFolded;
    for(auto local : loopTemporaries)
    {
        for(auto reader : local->getUsers(LocalUse::Type::READER))
        {
            if(!loop.findInLoop(reader))
                toBeFolded.emplace(reader);
        }
    }

    if(toBeFolded.empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to find consumer for local '" << loc->to_string()
                << "' outside of loop to be folded, aborting" << logging::endl);
        return {};
    }

    return AccumulationInfo{loc, op->op, std::move(toBeFolded)};
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

        if(!inductionVariable.getLowerBound() || !inductionVariable.getUpperBound())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Upper or lower bound is not a literal value, aborting vectorization!" << logging::endl);
            continue;
        }

        auto stepConstant = inductionVariable.getStep();
        if(!stepConstant)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Iteration step increment is not a literal value, aborting vectorization!" << logging::endl);
            continue;
        }

        auto writeDependencies = loop.findLocalDependencies(*dependencyGraph);
        FastMap<const Local*, AccumulationInfo> accumulationsToFold;
        if(writeDependencies.size() > 1)
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

        // 4. determine vectorization factor
        Optional<unsigned> vectorizationFactor = determineVectorizationFactor(loop, inductionVariable);
        if(!vectorizationFactor)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine a vectorization factor for the loop, aborting!" << logging::endl);
            continue;
        }
        if(vectorizationFactor.value() == 1)
        {
            // nothing to do
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Maximum vectorization factor is one, nothing to vectorize, aborting!" << logging::endl);
            continue;
        }

        // 5. cost-benefit calculation
        int rating = calculateCostsVsBenefits(loop, inductionVariable, *dependencyGraph, *vectorizationFactor);
        if(rating < 0 /* TODO some positive factor to be required before vectorizing loops? */)
        {
            // vectorization (probably) doesn't pay off
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Vectorization determined to be inefficient, aborting!" << logging::endl);

            continue;
        }

        // 6. run vectorization
        vectorize(loop, inductionVariable, method, *vectorizationFactor, *stepConstant, accumulationsToFold);
        // increasing the iteration step might create a value not fitting into small immediate
        normalization::handleImmediate(
            module, method, loop.findInLoop(inductionVariable.inductionStep).value(), config);
        hasChanged = true;

        PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + 334, "Vectorization factors", *vectorizationFactor);
    }

    return hasChanged;
}
