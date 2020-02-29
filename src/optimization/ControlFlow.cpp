/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlow.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/ControlFlowLoop.h"
#include "../analysis/DataDependencyGraph.h"
#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../normalization/LiteralValues.h"
#include "../periphery/VPM.h"
#include "./Combiner.h"
#include "log.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <queue>

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

static InductionVariable extractLoopControl(const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
{
    auto inductionVariables = loop.findInductionVariables(dependencyGraph, true);

    for(auto& var : inductionVariables)
        logging::debug() << "Induction variable: " << var.local->to_string() << " from "
                         << var.initialAssignment->to_string() << " step " << var.inductionStep->to_string()
                         << " while "
                         << (var.repeatCondition ?
                                    (var.repeatCondition->first + (" " + var.repeatCondition->second.to_string())) :
                                    "(?)")
                         << logging::endl;

    if(inductionVariables.empty())
        return InductionVariable{};
    else if(inductionVariables.size() == 1)
        return *inductionVariables.begin();

    throw CompilationError(
        CompilationStep::OPTIMIZER, "Selecting from multiple iteration variables is not supported yet!");
}

static Optional<unsigned> calculateDistance(
    const InductionVariable& inductionVariable, Literal lowerBound, Literal upperBound)
{
    auto comp = inductionVariable.repeatCondition.value().first;

    if((comp == intermediate::COMP_SIGNED_LE || comp == intermediate::COMP_SIGNED_LT ||
           comp == intermediate::COMP_UNSIGNED_LE || comp == intermediate::COMP_UNSIGNED_LT) &&
        lowerBound.signedInt() > upperBound.signedInt())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Iterating across type wrap is not supported for: " << inductionVariable.local->to_string()
                << " from " << lowerBound.to_string() << " to " << upperBound.to_string() << logging::endl);
        return {};
    }

    if((comp == intermediate::COMP_SIGNED_GE || comp == intermediate::COMP_SIGNED_GT ||
           comp == intermediate::COMP_UNSIGNED_GE || comp == intermediate::COMP_UNSIGNED_GT) &&
        lowerBound.signedInt() < upperBound.signedInt())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Iterating across type wrap is not supported for: " << inductionVariable.local->to_string()
                << " from " << lowerBound.to_string() << " to " << upperBound.to_string() << logging::endl);
        return {};
    }

    if(comp == intermediate::COMP_SIGNED_LT)
        return static_cast<unsigned>(upperBound.signedInt() - lowerBound.signedInt());
    if(comp == intermediate::COMP_SIGNED_LE)
        return static_cast<unsigned>(upperBound.signedInt() - lowerBound.signedInt() + 1);
    if(comp == intermediate::COMP_SIGNED_GT)
        return static_cast<unsigned>(lowerBound.signedInt() - upperBound.signedInt());
    if(comp == intermediate::COMP_SIGNED_GE)
        return static_cast<unsigned>(lowerBound.signedInt() - upperBound.signedInt() + 1);
    if(comp == intermediate::COMP_UNSIGNED_LT)
        return upperBound.unsignedInt() - lowerBound.unsignedInt();
    if(comp == intermediate::COMP_UNSIGNED_LE)
        return upperBound.unsignedInt() - lowerBound.unsignedInt() + 1u;
    if(comp == intermediate::COMP_UNSIGNED_GT)
        return lowerBound.unsignedInt() - upperBound.unsignedInt();
    if(comp == intermediate::COMP_UNSIGNED_GE)
        return lowerBound.unsignedInt() - upperBound.unsignedInt() + 1u;
    if(comp == intermediate::COMP_NEQ)
        // XXX could be wrong for unsigned induction variable and more than 2^31 iterations
        return static_cast<unsigned>(std::max(lowerBound.signedInt(), upperBound.signedInt()) -
            std::min(lowerBound.signedInt(), upperBound.signedInt()));

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Unsupported comparison for calculating distance for: " << inductionVariable.local->to_string()
            << " and comparison: " << comp << logging::endl);
    return {};
}

static Optional<unsigned> calculateIterationCount(
    const InductionVariable& inductionVariable, Literal lowerBound, Literal upperBound, Literal stepValue)
{
    auto distance = calculateDistance(inductionVariable, lowerBound, upperBound);
    if(!distance)
        return {};

    if(inductionVariable.inductionStep->op == OP_ADD)
        // iterations = (end - start) / step
        return *distance / static_cast<unsigned>(std::abs(stepValue.signedInt()));
    if(inductionVariable.inductionStep->op == OP_SUB)
        // iterations = (start - end) / step
        return *distance / static_cast<unsigned>(std::abs(stepValue.signedInt()));
    // XXX add support for more step operations
    // E.g. mul? Need to calculate:
    // limit = (start * step) ^ iterations -> iterations = log(start * step) / log(limit)

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Unsupported induction operation for: " << inductionVariable.local->to_string()
            << " and induction step: " << inductionVariable.inductionStep->to_string() << logging::endl);
    return {};
}

/*
 * For now uses a very simple algorithm:
 * - checks the maximum vector-width used inside the loop
 * - tries to find an optimal factor, which never exceeds 16 elements and divides the number of iterations equally
 */
static Optional<unsigned> determineVectorizationFactor(const ControlFlowLoop& loop,
    const InductionVariable& inductionVariable, Literal lowerBound, Literal upperBound, Literal stepValue)
{
    unsigned char maxTypeWidth = 1;
    InstructionWalker it = loop.front()->key->walk();
    while(!it.isEndOfMethod() && it != loop.back()->key->walkEnd())
    {
        if(it->getOutput())
        {
            // TODO is this check enough?
            maxTypeWidth = std::max(maxTypeWidth, it->getOutput()->type.getVectorWidth());
        }
        it.nextInMethod();
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Found maximum used vector-width of " << static_cast<unsigned>(maxTypeWidth) << " elements"
            << logging::endl);

    // the number of iterations from the bounds depends on the iteration operation
    auto iterations = calculateIterationCount(inductionVariable, lowerBound, upperBound, stepValue);
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

    InstructionWalker it = loop.front()->key->walk();
    while(!it.isEndOfMethod() && it != loop.back()->key->walkEnd())
    {
        if(it.has())
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
            else if(it.get<intermediate::VectorRotation>())
            {
                // abort
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Cannot vectorize loops containing vector rotations: " << it->to_string() << logging::endl);
                return std::numeric_limits<int>::min();
            }
            else if(it.get<intermediate::MemoryBarrier>())
            {
                // abort
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Cannot vectorize loops containing memory barriers: " << it->to_string() << logging::endl);
                return std::numeric_limits<int>::min();
            }
            else if(it.get<intermediate::SemaphoreAdjustment>())
            {
                // abort
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Cannot vectorize loops containing semaphore calls: " << it->to_string() << logging::endl);
                return std::numeric_limits<int>::min();
            }
        }

        // TODO check and increase costs
        it.nextInMethod();
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

static void scheduleForVectorization(
    const Local* local, FastSet<const intermediate::IntermediateInstruction*>& openInstructions, ControlFlowLoop& loop)
{
    local->forUsers(LocalUse::Type::READER, [&openInstructions, &loop](const LocalUser* user) -> void {
        if(!user->hasDecoration(intermediate::InstructionDecorations::AUTO_VECTORIZED))
            openInstructions.emplace(user);
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
                        openInstructions.emplace(it.get());
                        break;
                    }

                    it.nextInBlock();
                }
            }
        }
    });
}

static uint8_t getVectorWidth(DataType type)
{
    if(auto ptrType = type.getPointerType())
        return ptrType->elementType.getVectorWidth();
    return type.getVectorWidth();
}

static void vectorizeInstruction(intermediate::IntermediateInstruction* inst, Method& method,
    FastSet<const intermediate::IntermediateInstruction*>& openInstructions, unsigned vectorizationFactor,
    ControlFlowLoop& loop)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Vectorizing instruction: " << inst->to_string() << logging::endl);

    // 1. update types of values matching the types of their locals
    unsigned char vectorWidth = 1;
    for(auto& arg : inst->getArguments())
    {
        if(arg.checkLocal() && arg.type != arg.local()->type)
        {
            scheduleForVectorization(arg.local(), openInstructions, loop);
            if(auto ptrType = arg.local()->type.getPointerType())
            {
                const_cast<DataType&>(arg.type) = DataType(ptrType);
                vectorWidth = std::max(vectorWidth, ptrType->elementType.getVectorWidth());
            }
            else
            {
                const_cast<DataType&>(arg.type) = arg.type.toVectorType(arg.local()->type.getVectorWidth());
                vectorWidth = std::max(vectorWidth, arg.type.getVectorWidth());
            }
        }
        else if(arg.checkRegister())
        {
            // TODO correct?? This is at least required for reading from TMU
            vectorWidth = static_cast<unsigned char>(vectorizationFactor);
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
            scheduleForVectorization(loc, openInstructions, loop);
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
        auto op = optIt->get<intermediate::Operation>();
        if(!op || op->op != OP_SUB || !op->assertArgument(0).hasRegister(REG_ELEMENT_NUMBER) ||
            !op->assertArgument(1).getLiteralValue())
            // TODO make more robust!
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Unhandled instruction setting flags for valid TMU address elements", (*optIt)->to_string());
        // we have instruction - = elem_num - vector_width (setf)
        logging::debug() << "Vectorizing TMU load element mask: " << op->to_string() << logging::endl;
        op->setArgument(1, Value(Literal(vectorWidth), TYPE_INT8));
        op->addDecorations(InstructionDecorations::AUTO_VECTORIZED);
        // TODO if the previous size was 1 and the input is not directly an UNIFORM value, we need to replicate the base
        // address too!

        // when writing to TMU address register, we also need to change the element address offset, since it is added
        // twice (once for the modified original index and once for the TMU loading element address offset).
        // The TMU address is set conditionally depending on the above setting of flags to either zero or the base
        // address + element offset
        auto firstArg = inst->getArgument(0);
        if(dynamic_cast<MoveOperation*>(inst) == nullptr || !firstArg || firstArg->checkLocal() == nullptr ||
            firstArg->local()->getUsers(LocalUse::Type::WRITER).size() != 2)
            // TODO make more robust!
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Unhandled instruction setting TMU address elements", inst->to_string());
        firstArg->local()->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
            if(dynamic_cast<const MoveOperation*>(writer) && writer->assertArgument(0).getLiteralValue() == 0_lit)
                // setting of zero, skip this
                return;
            // we are interested in the other writer
            logging::debug() << "Fixing up TMU address element offset: " << writer->to_string() << logging::endl;
            auto wIt = loop.findInLoop(writer);
            if(!wIt || dynamic_cast<const Operation*>(writer) == nullptr)
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Failed to find instruction writing TMU address element offset", writer->to_string());
            // TODO is this true for all vector widths? Not just 1 -> x!?
            if(writer->assertArgument(0).type.getPointerType() && writer->assertArgument(1).getSingleWriter())
                // first argument is base address, second argument is element offset
                (*wIt)->replaceValue(writer->assertArgument(1), INT_ZERO, LocalUse::Type::READER);
            else if(writer->assertArgument(1).type.getPointerType() && writer->assertArgument(0).getSingleWriter())
                // second argument is base address, first argument is element offset
                (*wIt)->replaceValue(writer->assertArgument(0), INT_ZERO, LocalUse::Type::READER);
            else
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Unhandled instruction setting TMU address element offset", writer->to_string());
        });
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
    InstructionWalker it = loop.front()->key->walk();
    std::size_t numVectorized = 0;

    while(!it.isEndOfMethod() && it != loop.back()->key->walkEnd())
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
                vpwSetup.dmaSetup.setDepth(static_cast<uint8_t>(vpwSetup.dmaSetup.getDepth() * vectorizationFactor));
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

        it.nextInMethod();
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

/*
 * Approach:
 * - set the iteration variable (local) to vector
 * - iterative (until no more values changed), modify all value (and local)-types so argument/result-types match again
 * - add new instruction-decoration (vectorized) to facilitate
 * - in final iteration, fix TMU/VPM configuration and address calculation and loop condition
 * - fix initial iteration value and step
 */
static void vectorize(ControlFlowLoop& loop, InductionVariable& inductionVariable, Method& method,
    const DataDependencyGraph& dependencyGraph, unsigned vectorizationFactor, Literal stepValue)
{
    FastSet<const intermediate::IntermediateInstruction*> openInstructions;

    const_cast<DataType&>(inductionVariable.local->type) = inductionVariable.local->type.toVectorType(
        static_cast<unsigned char>(inductionVariable.local->type.getVectorWidth() * vectorizationFactor));
    scheduleForVectorization(inductionVariable.local, openInstructions, loop);
    std::size_t numVectorized = 0;

    // iteratively change all instructions
    while(!openInstructions.empty())
    {
        auto inst = *openInstructions.begin();
        if(auto it = loop.findInLoop(inst))
        {
            vectorizeInstruction(it->get(), method, openInstructions, vectorizationFactor, loop);
            ++numVectorized;
        }
        else if(dynamic_cast<const intermediate::MoveOperation*>(inst) && inst->checkOutputLocal() &&
            !inst->hasSideEffects())
        {
            // follow all simple moves to other locals (to find the instruction we really care about)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local is accessed outside of loop: " << inst->to_string() << logging::endl);
            vectorizeInstruction(const_cast<intermediate::IntermediateInstruction*>(inst), method, openInstructions,
                vectorizationFactor, loop);
            ++numVectorized;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Local is accessed outside of loop: " << inst->to_string() << logging::endl);

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

                Optional<OpCode> accumulationOp{};
                if(auto writer = arg.getSingleWriter())
                {
                    writer = intermediate::getSourceInstruction(writer);
                    if(auto op = dynamic_cast<const intermediate::Operation*>(writer))
                        accumulationOp = op->op;
                }

                if(accumulationOp)
                {
                    Optional<InstructionWalker> it;
                    if(auto succ = loop.findSuccessor())
                    {
                        it = succ->key->findWalkerForInstruction(inst, succ->key->walkEnd());
                    }
                    if(it)
                    {
                        // insert folding or argument, heed original vector width
                        auto newArg = method.addNewLocal(origVectorType, "%vector_fold");
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Folding vectorized local " << arg.to_string() << " into " << newArg.to_string()
                                << " for: " << inst->to_string() << logging::endl);
                        ignoreReturnValue(insertFoldVector(
                            *it, method, newArg, arg, *accumulationOp, InstructionDecorations::AUTO_VECTORIZED));
                        // replace argument with folded version
                        const_cast<IntermediateInstruction*>(inst)->replaceValue(arg, newArg, LocalUse::Type::READER);
                        openInstructions.erase(inst);
                        continue;
                    }
                    // TODO handle variable used in other blocks, how to find walker?
                }
                else
                {
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Failed to determine accumulation operation for vectorized local", arg.to_string());
                }
            }

            throw CompilationError(CompilationStep::OPTIMIZER,
                "Accessing vectorized locals outside of the loop is not yet implemented", inst->to_string());
        }
    }

    numVectorized += fixVPMSetups(loop, vectorizationFactor);

    fixInitialValueAndStep(loop, inductionVariable, stepValue, vectorizationFactor);
    numVectorized += 2;

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Vectorization done, changed " << numVectorized << " instructions!" << logging::endl);
}

bool optimizations::vectorizeLoops(const Module& module, Method& method, const Configuration& config)
{
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
            !inductionVariable.repeatCondition || !inductionVariable.repeatCondition->first ||
            inductionVariable.repeatCondition->second.isUndefined())
        {
            // we need to know both bounds and the iteration step (for now)
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to find all bounds and step for loop, aborting vectorization!" << logging::endl);
            continue;
        }

        auto lowerBound = (inductionVariable.initialAssignment->precalculate(4).first & &Value::getLiteralValue);
        auto upperBound = inductionVariable.repeatCondition->second.getLiteralValue();

        if(!lowerBound || !upperBound)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Upper or lower bound is not a literal value, aborting vectorization!" << logging::endl);
            continue;
        }

        Optional<Literal> stepConstant{};
        if(auto stepValue =
                inductionVariable.inductionStep->findOtherArgument(inductionVariable.local->createReference()))
        {
            stepConstant =
                ((stepValue->getSingleWriter() ? stepValue->getSingleWriter()->precalculate(4).first : stepValue) &
                    &Value::getLiteralValue);
        }

        if(!stepConstant)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Iteration step increment is not a literal value, aborting vectorization!" << logging::endl);
            continue;
        }

        // 4. determine vectorization factor
        Optional<unsigned> vectorizationFactor =
            determineVectorizationFactor(loop, inductionVariable, *lowerBound, *upperBound, *stepConstant);
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
        vectorize(loop, inductionVariable, method, *dependencyGraph, *vectorizationFactor, *stepConstant);
        // increasing the iteration step might create a value not fitting into small immediate
        normalization::handleImmediate(
            module, method, loop.findInLoop(inductionVariable.inductionStep).value(), config);
        hasChanged = true;

        PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION + 334, "Vectorization factors", *vectorizationFactor);
    }

    return hasChanged;
}

void optimizations::extendBranches(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    // we only need to set the same flag once
    std::pair<Value, intermediate::InstructionDecorations> lastSetFlags =
        std::make_pair(UNDEFINED_VALUE, intermediate::InstructionDecorations::NONE);
    while(!it.isEndOfMethod())
    {
        if(auto branch = it.get<intermediate::Branch>())
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Extending branch: " << branch->to_string() << logging::endl);
            if(branch->hasConditionalExecution() || !branch->getCondition().hasLiteral(Literal(true)))
            {
                /*
                 * branch can only depend on scalar value
                 * -> set any not used vector-element (all except element 0) to a value where it doesn't influence
                 * the condition
                 *
                 * Using ELEMENT_NUMBER sets the vector-elements 1 to 15 to a non-zero value and 0 to either 0 (if
                 * condition was false) or 1 (if condition was true)
                 */
                // TODO can be skipped, if it is checked/guaranteed, that the last instruction setting flags is the
                // boolean-selection for the given condition  but we need to check more than the last instructions,
                // since there could be moves inserted by phi

                // skip setting of flags, if the previous setting wrote the same flags
                if(lastSetFlags.first != branch->getCondition() ||
                    branch->hasDecoration(intermediate::InstructionDecorations::BRANCH_ON_ALL_ELEMENTS) !=
                        has_flag(lastSetFlags.second, intermediate::InstructionDecorations::BRANCH_ON_ALL_ELEMENTS))
                {
                    auto cond = branch->getCondition();
                    if(auto lit = cond.getLiteralValue())
                    {
                        if(auto imm = normalization::toImmediate(*lit))
                            cond = Value(*imm, cond.type);
                        else
                            throw CompilationError(CompilationStep::NORMALIZER,
                                "Unhandled literal value in branch condition", branch->to_string());
                    }
                    if(branch->hasDecoration(intermediate::InstructionDecorations::BRANCH_ON_ALL_ELEMENTS))
                        assign(it, NOP_REGISTER) = (branch->getCondition() | cond, SetFlag::SET_FLAGS);
                    else
                        assign(it, NOP_REGISTER) = (ELEMENT_NUMBER_REGISTER | cond, SetFlag::SET_FLAGS);
                }
                lastSetFlags.first = branch->getCondition();
                lastSetFlags.second = branch->decoration;
            }
            // go to next instruction
            it.nextInBlock();
            // insert 3 NOPs before
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
        }
        else if(it.get() != nullptr && it->setFlags == SetFlag::SET_FLAGS)
        {
            // any other instruction setting flags, need to re-set the branch-condition
            lastSetFlags = std::make_pair(UNDEFINED_VALUE, intermediate::InstructionDecorations::NONE);
        }
        it.nextInMethod();
    }
}

static NODISCARD InstructionWalker loadVectorParameter(Parameter& param, Method& method, InstructionWalker it)
{
    // we need to load a UNIFORM per vector element into the particular vector element
    for(uint8_t i = 0; i < param.type.getVectorWidth(); ++i)
    {
        // the first write to the parameter needs to unconditional, so the register allocator can find it
        if(i > 0)
        {
            assign(it, NOP_REGISTER) =
                (ELEMENT_NUMBER_REGISTER ^ Value(SmallImmediate(i), TYPE_INT8), SetFlag::SET_FLAGS);
        }
        if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
        {
            it = intermediate::insertSignExtension(it, method, Value(REG_UNIFORM, param.type),
                Value(&param, TYPE_INT32), false, i == 0 ? COND_ALWAYS : COND_ZERO_SET);
            it.copy().previousInBlock()->addDecorations(intermediate::InstructionDecorations::ELEMENT_INSERTION);
        }
        else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
        {
            it = intermediate::insertZeroExtension(it, method, Value(REG_UNIFORM, param.type),
                Value(&param, TYPE_INT32), false, i == 0 ? COND_ALWAYS : COND_ZERO_SET);
            it.copy().previousInBlock()->addDecorations(intermediate::InstructionDecorations::ELEMENT_INSERTION);
        }
        else
        {
            assign(it, param.createReference()) =
                (UNIFORM_REGISTER, i == 0 ? COND_ALWAYS : COND_ZERO_SET, InstructionDecorations::ELEMENT_INSERTION);
        }
        // TODO improve performance by first putting together the vector, then zero/sign extending all elements?
    }
    return it;
}

static void generateStopSegment(Method& method)
{
    // write interrupt for host
    // write QPU number finished (value must be NON-NULL, so we invert it -> the first 28 bits are always 1)
    method.appendToEnd(
        (new intermediate::Operation(OP_NOT, Value(REG_HOST_INTERRUPT, TYPE_INT8), Value(REG_QPU_NUMBER, TYPE_INT8)))
            ->addDecorations(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
    intermediate::IntermediateInstruction* nop = new intermediate::Nop(intermediate::DelayType::THREAD_END);
    // set signals to stop thread/program
    nop->setSignaling(SIGNAL_END_PROGRAM);
    method.appendToEnd(nop);
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::THREAD_END));
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::THREAD_END));
}

static const Local* isLocalUsed(Method& method, BuiltinLocal::Type type)
{
    auto loc = method.findBuiltin(type);
    if(loc != nullptr && !loc->getUsers(LocalUse::Type::READER).empty())
        return loc;
    return nullptr;
}

void optimizations::addStartStopSegment(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    if(it.isEndOfMethod() || !it.get<intermediate::BranchLabel>() ||
        BasicBlock::DEFAULT_BLOCK != it.get<intermediate::BranchLabel>()->getLabel()->name)
    {
        it = method.createAndInsertNewBlock(method.begin(), BasicBlock::DEFAULT_BLOCK).walk();
    }
    it.nextInBlock();

    // if the second TMU was used explicitly at some point, we disable TMU_SWAP
    {
        bool tmu1Used = false;
        auto checkIt = method.walkAllInstructions();
        while(!checkIt.isEndOfMethod())
        {
            if(checkIt->writesRegister(REG_TMU1_ADDRESS))
            {
                tmu1Used = true;
                break;
            }
            checkIt.nextInMethod();
        }
        if(tmu1Used)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Using both TMUs explicitly, disable automatic swapping!" << logging::endl);
            assign(it, Value(REG_TMU_NOSWAP, TYPE_BOOL)) = BOOL_TRUE;
        }
    }

    /*
     * The first UNIFORMs are reserved for relaying information about the work-item and work-group
     * - work_dim: number of dimensions
     * - local_sizes: local number of work-items in its work-group per dimension
     * - local_ids: local id of this work-item within its work-group
     * - num_groups (x,y,z): global number of work-groups per dimension
     * - group_id (x, y, z): id of this work-group
     * - global_offset (x, y, z): global initial offset per dimension
     * - address of global data / to load the global data from
     *
     */
    // initially set all implicit UNIFORMs to unused
    method.metaData.uniformsUsed.value = 0;
    auto workInfoDecorations =
        add_flag(InstructionDecorations::UNSIGNED_RESULT, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::WORK_DIMENSIONS))
    {
        method.metaData.uniformsUsed.setWorkDimensionsUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT8), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::LOCAL_SIZES))
    {
        method.metaData.uniformsUsed.setLocalSizesUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::LOCAL_IDS))
    {
        method.metaData.uniformsUsed.setLocalIDsUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32),
            remove_flag(workInfoDecorations, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::NUM_GROUPS_X))
    {
        method.metaData.uniformsUsed.setNumGroupsXUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::NUM_GROUPS_Y))
    {
        method.metaData.uniformsUsed.setNumGroupsYUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::NUM_GROUPS_Z))
    {
        method.metaData.uniformsUsed.setNumGroupsZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_ID_X))
    {
        method.metaData.uniformsUsed.setGroupIDXUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_ID_Y))
    {
        method.metaData.uniformsUsed.setGroupIDYUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_ID_Z))
    {
        method.metaData.uniformsUsed.setGroupIDZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_OFFSET_X))
    {
        method.metaData.uniformsUsed.setGlobalOffsetXUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_OFFSET_Y))
    {
        method.metaData.uniformsUsed.setGlobalOffsetYUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_OFFSET_Z))
    {
        method.metaData.uniformsUsed.setGlobalOffsetZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_DATA_ADDRESS))
    {
        method.metaData.uniformsUsed.setGlobalDataAddressUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }

    // load arguments to locals (via reading from uniform)
    for(Parameter& param : method.parameters)
    {
        // do the loading
        // we need special treatment for non-scalar parameter (e.g. vectors), since they can't be read with just 1
        // UNIFORM
        if(!param.type.getPointerType() && param.type.getVectorWidth() != 1)
        {
            it = loadVectorParameter(param, method, it);
        }
        else if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
        {
            it = intermediate::insertSignExtension(
                it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false);
        }
        else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
        {
            it = intermediate::insertZeroExtension(
                it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false);
        }
        else
        {
            /*
             * NOTE: Pointers with the byval decoration are treated as simple pointers, saving us from having to
             * re-write all instructions accessing them. In return, the VC4CL run-time needs to convert the direct
             * kernel argument (e.g. a struct) to a pointer-to-data argument by allocating a buffer (similar to
             * local arguments).
             *
             * Alternative ways of solving this:
             * - Read parameter from UNIFORMs and write to VPM, where it can be accessed like "normal" pointed-to
             * data
             * - Read directly from UNIFORM storage, needs pointer to UNIFORM and re-set UNIFORM pointer for
             * successive parameter
             * - Load the single parts separately via UNIFORMs like any other vector/scalar, replace index-chain and
             * access functions.
             */
            assign(it, param.createReference()) = (Value(REG_UNIFORM, param.type),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE,
                // all pointers are unsigned
                param.type.getPointerType() ? InstructionDecorations::UNSIGNED_RESULT : InstructionDecorations::NONE);
        }
    }

    generateStopSegment(method);
}

bool optimizations::removeConstantLoadInLoops(const Module& module, Method& method, const Configuration& config)
{
    const int moveDepth = config.additionalOptions.moveConstantsDepth;
    bool hasChanged = false;

    if(method.getCFG().getNodes().empty())
        return false;

    auto cfg = method.getCFG().clone();
#ifdef DEBUG_MODE
    cfg->dumpGraph("/tmp/before-removeConstantLoadInLoops.dot", true);
#endif

    // 1. Simplify the CFG to avoid combinatorial explosion. (e.g. testing/test_barrier.cl)
    // Remove unnecessary CFG nodes
    // - a. nodes which have more than two incoming or outgoing edges
    // - b. nodes which have constant loading instruction(s)
    // - c. predecessor nodes of the node which has back edge

    // FIXME: fix the simplifying count (to finding the fixed point?)
    for(int i = 0; i < 2; i++)
    {
        // set isBackEdge flag
        {
            std::set<CFGNode*> visited;

            std::queue<CFGNode*> que;
            que.push(&cfg->getStartOfControlFlow());
            while(!que.empty())
            {
                auto cur = que.front();
                que.pop();

                visited.insert(cur);

                cur->forAllOutgoingEdges([&que, &visited, &cur](CFGNode& next, CFGEdge& edge) -> bool {
                    if(visited.find(cur) != visited.end())
                    {
                        edge.data.isBackEdge = true;
                    }
                    else
                    {
                        que.push(&next);
                    }
                    return true;
                });
            }
        }

        std::vector<CFGNode*> unnecessaryNodes;

        for(auto& pair : cfg->getNodes())
        {
            // a.
            bool hasConstantInstruction = false;
            auto block = pair.first;
            for(auto it = block->walk(); it != block->walkEnd(); it = it.nextInBlock())
            {
                if(it.has() && it->isConstantInstruction())
                {
                    hasConstantInstruction = true;
                    break;
                }
            }
            if(hasConstantInstruction)
            {
                continue;
            }

            int incomingEdgesCount = 0;
            pair.second.forAllIncomingEdges([&incomingEdgesCount](const CFGNode& next, const CFGEdge& edge) -> bool {
                incomingEdgesCount++;
                return true;
            });
            int outgoingEdgesCount = 0;
            pair.second.forAllOutgoingEdges([&outgoingEdgesCount](const CFGNode& next, const CFGEdge& edge) -> bool {
                outgoingEdgesCount++;
                return true;
            });

            // b.
            if(incomingEdgesCount >= 2 || outgoingEdgesCount >= 2)
            {
                continue;
            }

            // c.
            bool hasBackEdge = false;
            pair.second.forAllOutgoingEdges([&hasBackEdge](const CFGNode& next, const CFGEdge& edge) -> bool {
                next.forAllIncomingEdges([&hasBackEdge](const CFGNode& nnext, const CFGEdge& nedge) -> bool {
                    if(nedge.data.isBackEdge)
                    {
                        hasBackEdge = true;
                        return false;
                    }
                    return true;
                });
                if(hasBackEdge)
                {
                    return false;
                }
                return true;
            });
            if(hasBackEdge)
            {
                continue;
            }

            unnecessaryNodes.push_back(&pair.second);
        }

        for(auto& node : unnecessaryNodes)
        {
            CFGNode *prev = nullptr, *next = nullptr;
            // An unnecessary node must have just one previous node and one next node.
            node->forAllIncomingEdges([&prev](CFGNode& node, CFGEdge&) -> bool {
                prev = &node;
                return false;
            });
            node->forAllOutgoingEdges([&next](CFGNode& node, CFGEdge&) -> bool {
                next = &node;
                return false;
            });

            cfg->eraseNode(node->key);
            if(prev && next && !prev->isAdjacent(next))
            {
                prev->addEdge(next, {});
            }
        }
    }

#ifdef DEBUG_MODE
    cfg->dumpGraph("/tmp/before-removeConstantLoadInLoops-simplified.dot", true);
#endif

    // 2. Find loops
    auto loops = cfg->findLoops(true);

    if(loops.size() > 100)
    {
        logging::warn() << "Skip this optimization due to many nodes in loops." << logging::endl;
        return false;
    }

    // 3. Generate inclusion relation of loops as trees
    auto inclusionTree = createLoopInclusingTree(loops);

    // 4. Move constant load operations from root of trees

    std::map<LoopInclusionTreeNode*, std::vector<InstructionWalker>> instMapper;

    // find instructions to be moved
    for(auto& loop : inclusionTree->getNodes())
    {
        auto& node = inclusionTree->getOrCreateNode(loop.first);
        for(auto& cfgNode : *node.key)
        {
            if(node.hasCFGNodeInChildren(cfgNode))
            {
                // treat this node as that it's in child nodes.
                continue;
            }

            auto block = cfgNode->key;
            for(auto it = block->walk(); it != block->walkEnd(); it.nextInBlock())
            {
                if(it->isConstantInstruction() && it->checkOutputLocal() &&
                    it->checkOutputLocal()->getSingleWriter() == it.get())
                {
                    // can only move constant writes of locals only written exactly here
                    instMapper[&node].push_back(it);
                    hasChanged = true;
                }
            }
        }
    }

    std::set<LoopInclusionTreeNode*> processedNodes;
    std::set<IntermediateInstruction*> processedInsts;
    BasicBlock* insertedBlock = nullptr;

    // move instructions
    for(auto& loop : inclusionTree->getNodes())
    {
        auto& node = inclusionTree->getOrCreateNode(loop.first);
        auto root = castToTreeNode(node.findRoot({}));

        if(processedNodes.find(root) != processedNodes.end())
            continue;
        processedNodes.insert(root);

        // process tree nodes with BFS
        std::queue<LoopInclusionTreeNode*> que;
        que.push(root);

        while(!que.empty())
        {
            auto currentNode = que.front();
            que.pop();

            auto targetTreeNode =
                castToTreeNode(currentNode->findRoot(moveDepth == -1 ? Optional<int>() : Optional<int>(moveDepth - 1)));
            auto targetLoop = targetTreeNode->key;

            currentNode->forAllOutgoingEdges(
                [&](const LoopInclusionTreeNode& child, const LoopInclusionTreeEdge&) -> bool {
                    que.push(const_cast<LoopInclusionTreeNode*>(&child));
                    return true;
                });

            auto insts = instMapper.find(currentNode);
            if(insts == instMapper.end())
            {
                continue;
            }

            const CFGNode* targetCFGNode = nullptr;
            // Find the predecessor block of targetLoop.
            {
                FastAccessList<const CFGNode*> predecessors = targetLoop->findPredecessors();
                if(predecessors.size() > 0)
                {
                    for(auto& block : method)
                    {
                        auto loopBB = std::find_if(targetLoop->begin(), targetLoop->end(),
                            [&block](const CFGNode* node) { return node->key == &block; });
                        if(loopBB != targetLoop->end())
                        {
                            // the predecessor block must exist before targetLoop.
                            break;
                        }

                        auto bb = std::find_if(predecessors.begin(), predecessors.end(),
                            [&block](const CFGNode* node) { return node->key == &block; });
                        if(bb != predecessors.end())
                        {
                            targetCFGNode = *bb;
                            break;
                        }
                    }
                }
            }

            auto targetBlock = targetCFGNode != nullptr ? targetCFGNode->key : insertedBlock;

            if(targetBlock != nullptr)
            {
                // insert before 'br' operation
                auto targetInst = targetBlock->walkEnd().previousInBlock();
                for(auto it : insts->second)
                {
                    auto inst = it.get();
                    if(!it.has() || processedInsts.find(inst) != processedInsts.end())
                        continue;
                    processedInsts.insert(inst);

                    CPPLOG_LAZY(logging::Level::DEBUG, log << "Moving constant load out of loop: " << it->to_string());
                    targetInst.emplace(it.release());
                    it.reset(nullptr);
                }
            }
            else
            {
                logging::debug() << "Create a new basic block before the target block" << logging::endl;

                auto headBlock = method.begin();

                insertedBlock = &method.createAndInsertNewBlock(method.begin(), "%createdByRemoveConstantLoadInLoops");
                for(auto it : insts->second)
                {
                    auto inst = it.get();
                    if(!it.has() || processedInsts.find(inst) != processedInsts.end())
                        continue;
                    processedInsts.insert(inst);

                    CPPLOG_LAZY(logging::Level::DEBUG, log << "Moving constant load out of loop: " << it->to_string());
                    insertedBlock->walkEnd().emplace(it.release());
                    it.reset(nullptr);
                }
                // Clear processed instructions
                insts->second.clear();

                auto headLabel = headBlock->getLabel()->getLabel();
                auto insertedLabel = insertedBlock->getLabel()->getLabel();
                if(headLabel && insertedLabel && headLabel->name == BasicBlock::DEFAULT_BLOCK)
                {
                    // swap labels because DEFAULT_BLOCK is treated as head block.
                    headLabel->name.swap(insertedLabel->name);
                }
            }
        }
    }

#ifdef DEBUG_MODE
    auto& cfg2 = method.getCFG();
    cfg2.dumpGraph("/tmp/after-removeConstantLoadInLoops.dot", true);
#endif

    if(hasChanged)
    {
        method.cleanEmptyInstructions();
        // combine the newly reordered (and at one place accumulated) loading instructions
        combineLoadingConstants(module, method, config);
    }

    return hasChanged;
}

static const Local* findSourceBlock(const Local* label, const FastMap<const Local*, const Local*>& blockMap)
{
    auto it = blockMap.find(label);
    if(it == blockMap.end())
        return label;
    return findSourceBlock(it->second, blockMap);
}

bool optimizations::mergeAdjacentBasicBlocks(const Module& module, Method& method, const Configuration& config)
{
    auto& graph = method.getCFG();

    std::vector<std::pair<const Local*, const Local*>> blocksToMerge;

    auto it = method.begin();
    ++it;
    while(it != method.end())
    {
        // XXX currently, this only merges adjacent (in list of blocks) blocks
        auto prevIt = it;
        --prevIt;

        const auto& prevNode = graph.assertNode(&(*prevIt));
        const auto& node = graph.assertNode(&(*it));
        if(node.getSinglePredecessor() == &prevNode && prevNode.getSingleSuccessor() == &node)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found basic block with single direct successor: " << prevIt->to_string() << " and "
                    << it->to_string() << logging::endl);
            blocksToMerge.emplace_back(prevIt->getLabel()->getLabel(), it->getLabel()->getLabel());
        }
        ++it;
    }

    // this is required to be able to merge more than 2 blocks together
    FastMap<const Local*, const Local*> blockMap;

    for(auto& pair : blocksToMerge)
    {
        BasicBlock* sourceBlock = method.findBasicBlock(findSourceBlock(pair.second, blockMap));
        BasicBlock* destBlock = method.findBasicBlock(findSourceBlock(pair.first, blockMap));

        // remove all instructions from source block and append to destination block (skipping the source label)
        auto sourceIt = sourceBlock->walk().nextInBlock();
        while(!sourceIt.isEndOfBlock())
        {
            destBlock->walkEnd().emplace(sourceIt.release());
            sourceIt.nextInBlock();
        }
        // remove explicit branch from destination block to source block, if any
        auto& sourceNode = method.getCFG().assertNode(sourceBlock);
        auto& destNode = method.getCFG().assertNode(destBlock);
        if(auto edge = sourceNode.getEdge(&destNode))
        {
            auto predecessorIt = edge->data.getPredecessor(destBlock);
            if(auto branch = predecessorIt.get<intermediate::Branch>())
            {
                if(branch->getTarget() == sourceBlock->getLabel()->getLabel())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing explicit branch to basic block about to be merged: "
                            << predecessorIt->to_string() << logging::endl);
                    predecessorIt.erase();
                }
            }
        }

        // then remove the source block
        if(method.removeBlock(*sourceBlock))
        {
            blockMap.emplace(pair.second, pair.first);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Merged block " << pair.second->to_string() << " into " << pair.first->to_string()
                    << logging::endl);
        }
        else
        {
            LCOV_EXCL_START
            CPPLOG_LAZY_BLOCK(logging::Level::ERROR, {
                logging::error() << "Failed to merge block " << pair.second->to_string() << " into "
                                 << pair.first->to_string() << logging::endl;
                if(!sourceBlock->empty())
                {
                    logging::error() << "Block was not empty: " << logging::endl;
                    sourceBlock->dumpInstructions();
                }
                sourceBlock->forPredecessors([](InstructionWalker it) {
                    if(it.get())
                        logging::error() << "Block has explicit predecessor: " << it->to_string() << logging::endl;
                });
            });
            LCOV_EXCL_STOP
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Failed to remove empty basic block: ", sourceBlock->to_string());
        }
    }

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Merged " << blocksToMerge.size() << " pair of blocks!" << logging::endl);
    return !blocksToMerge.empty();
}

bool optimizations::reorderBasicBlocks(const Module& module, Method& method, const Configuration& config)
{
    auto& cfg = method.getCFG();
    auto blockIt = method.begin();
    auto prevIt = method.begin();
    ++blockIt;
    while(blockIt != method.end())
    {
        auto& node = cfg.assertNode(&(*blockIt));
        const auto predecessor = node.getSinglePredecessor();
        // Never re-order end-of-block. Though it should work, there could be trouble anyway
        if(blockIt->getLabel()->getLabel()->name != BasicBlock::LAST_BLOCK && predecessor != nullptr &&
            predecessor->key != &(*prevIt) && !prevIt->fallsThroughToNextBlock())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Reordering block with single predecessor not being the previous block: " << blockIt->to_string()
                    << logging::endl);

            auto predecessorIt = method.begin();
            while(predecessorIt != method.end())
            {
                if(&(*predecessorIt) == predecessor->key)
                    break;
                ++predecessorIt;
            }

            if(predecessorIt == method.end())
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Failed to find predecessor basic block: ", predecessor->key->to_string());

            // we insert before the iteration, so we need to set the iterator after the predecessor
            ++predecessorIt;
            method.moveBlock(blockIt, predecessorIt);
            // prevIt stays the same, since we removed the block and the next blockIt now follows prevIt
            blockIt = prevIt;
            ++blockIt;

            // if the now moved block did fall-through, we need to insert an explicit branch to its previous successor,
            // since they might now not longer be adjacent.
            const CFGNode* fallThroughSuccessor = nullptr;
            node.forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
                if(edge.data.isImplicit(node.key))
                {
                    if(fallThroughSuccessor)
                        throw CompilationError(CompilationStep::GENERAL, "Multiple implicit branches from basic block",
                            node.key->to_string());
                    fallThroughSuccessor = &successor;
                }
                return true;
            });
            if(fallThroughSuccessor)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Inserting explicit branch to previous fall-through successor for moved block '"
                        << node.key->to_string() << "' to '" << fallThroughSuccessor->key->to_string() << '\''
                        << logging::endl);
                node.key->walkEnd().emplace(new intermediate::Branch(
                    fallThroughSuccessor->key->getLabel()->getLabel(), COND_ALWAYS, BOOL_TRUE));
            }
        }
        else
        {
            ++blockIt;
            ++prevIt;
        }
    }

#ifdef DEBUG_MODE
    cfg.dumpGraph("/tmp/vc4c-cfg-reordered.dot", false);
#endif
    return false;
}

struct IfElseBlock
{
    // The common predecessor block, the block whether the condition(s) are checked
    CFGNode* predecessor;
    // The blocks executed for the different cases (may be a single for if without else or several for switch-cases)
    FastSet<CFGNode*> conditionalBlocks;
    // The common successor block, i.e. the block after the if-else or switch-case block
    CFGNode* successor;
};

static FastAccessList<IfElseBlock> findIfElseBlocks(ControlFlowGraph& graph)
{
    FastAccessList<IfElseBlock> blocks;
    graph.forAllNodes([&](CFGNode& node) {
        IfElseBlock candidateBlock{&node, {}, nullptr};

        node.forAllOutgoingEdges([&](CFGNode& candidate, CFGEdge& edge) -> bool {
            /*
             * edge is a candidate, if it has a single successor (the same as all other candidates) and a single
             * predecessor (the base node being checked):
             *
             * If-Else:
             *     Node
             *     /  \
             * Then    Else  <- candidates
             *     \  /
             *   Successor
             *
             * Switch-Case-Default:
             *        Node
             *       /  |  \
             *      /   |   \
             * Case0  Case1  Default  <- candidates
             *      \   |   /
             *       \  |  /
             *      Successor
             */

            // TODO to guarantee that we not only save instructions, but also execution cycles, we should check the
            // maximum length of the resulting block not exceeding the instructions we save executing one of the
            // cases (e.g. 2 branches + some conditionals/phi).
            if(auto succ = candidate.getSingleSuccessor())
            {
                if((candidateBlock.successor == nullptr || succ == candidateBlock.successor) &&
                    candidate.getSinglePredecessor() == &node)
                {
                    candidateBlock.conditionalBlocks.emplace(&candidate);
                    candidateBlock.successor = succ;
                    return true;
                }
            }
            // first level successors have different/multiple second level successors (or multiple predecessors),
            // abort
            candidateBlock.successor = nullptr;
            return false;
        });

        if(candidateBlock.successor != nullptr && candidateBlock.conditionalBlocks.size() > 1)
            blocks.emplace_back(std::move(candidateBlock));
        else if(/* DISABLES CODE */ (false)) // TODO needs testing!
        {
            // TODO also needs extension in rewriting, special handling for successor which is conditional!
            // we failed with simple version above, recheck for more complex version:
            FastSet<CFGNode*> candidates;

            node.forAllOutgoingEdges([&](CFGNode& candidate, CFGEdge& edge) -> bool {
                candidates.emplace(&candidate);
                return true;
            });

            if(candidates.size() == 1 || candidates.find(&node) != candidates.end())
                // single successors will always match!  Also simple nodes might!
                return;

            FastSet<CFGNode*> conditionalBlocks;
            FastSet<CFGNode*> fallThroughBlocks;

            node.forAllOutgoingEdges([&](CFGNode& candidate, CFGEdge& edge) -> bool {
                /*
                 * edge is a candidate, if
                 * - it has a single successor (the same as almost all other candidates) and a single predecessor
                 * (the base node being checked) or
                 * - it is the single successor of all other candidates and has only candidates and the base node as
                 * successors
                 *
                 * If-without-Else:
                 * Node
                 *  | \
                 *  |  Then  <- candidate
                 *  | /
                 * Successor  <-candidate
                 *
                 * Switch-Case:
                 *       Node
                 *       / | \
                 *  Case0  |  Case1  <- candidates
                 *       \ | /
                 *     Successor  <- candidate
                 */
                if(auto succ = candidate.getSingleSuccessor())
                {
                    if(candidates.find(succ) != candidates.end() && candidate.getSinglePredecessor() == &node)
                    {
                        // this block is succeeded by another candidate, this is a "normal" conditional block
                        conditionalBlocks.emplace(&candidate);
                        return true;
                    }
                    bool allPredecessorsAreCandidates = true;
                    candidate.forAllIncomingEdges([&](CFGNode& pred, CFGEdge& e) -> bool {
                        if(candidates.find(&pred) == candidates.end() && &pred != &node)
                        {
                            allPredecessorsAreCandidates = false;
                            return false;
                        }
                        return true;
                    });
                    if(allPredecessorsAreCandidates && candidates.find(succ) == candidates.end())
                    {
                        // this block is succeeded by any other block and all predecessors are candidates for
                        // conditional, this is also a "fallthrough" block
                        conditionalBlocks.emplace(&candidate);
                        fallThroughBlocks.emplace(&candidate);
                        return true;
                    }
                }
                return false;
            });

            if(candidates.size() == conditionalBlocks.size() && fallThroughBlocks.size() == 1)
            {
                // all blocks could be assigned to either category and there is only 1 fall-through block
                blocks.emplace_back(IfElseBlock{&node, std::move(conditionalBlocks), *fallThroughBlocks.begin()});
                logging::error() << "ADVANCED VERSION" << logging::endl;
            }
        }
    });
    return blocks;
}

bool optimizations::simplifyConditionalBlocks(const Module& module, Method& method, const Configuration& config)
{
    // NOTE: boost-compute/test_binary_search.cl/calls to atomic_min are good test candidates!
    bool changedCode = false;
    for(const auto& block : findIfElseBlocks(method.getCFG()))
    {
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Found conditional block candidate: " << block.predecessor->key->to_string()
                             << logging::endl;
            for(auto succ : block.conditionalBlocks)
                logging::debug() << "\t" << succ->key->to_string() << logging::endl;
            logging::debug() << "Successor: " << block.successor->key->to_string() << logging::endl;
        });

        bool hasSideEffects = false;
        FastSet<const Local*> nonlocalLocals;
        for(auto succ : block.conditionalBlocks)
        {
            auto it = succ->key->walk().nextInBlock(); // skip label
            while(it != succ->key->walkEnd())
            {
                if(it.get<intermediate::Branch>() == nullptr && (it->hasSideEffects() || it->hasConditionalExecution()))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Side effect in " << succ->key->to_string() << " - " << it->to_string()
                            << logging::endl);
                    hasSideEffects = true;
                    break;
                }
                if(it->checkOutputLocal() && !succ->key->isLocallyLimited(it, it->getOutput()->local(), 8))
                    nonlocalLocals.emplace(it->getOutput()->local());
                it.nextInBlock();
            }
            if(hasSideEffects)
                break;
        }

        if(hasSideEffects)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Skipping this candidate, since conditional block has side effects" << logging::endl);
            continue;
        }

        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            for(auto loc : nonlocalLocals)
                logging::debug() << "Non-local: " << loc->to_string() << logging::endl;
        });

        // need to reorder successive blocks, so that default branch (without any condition) is inserted top-most
        // and not at last!
        InstructionWalker beforeBranchesIt = block.predecessor->key->walk().nextInBlock();
        while(!beforeBranchesIt.get<intermediate::Branch>())
            beforeBranchesIt.nextInBlock();
        // go to last before the first branch
        beforeBranchesIt.previousInBlock();

        for(auto succ : block.conditionalBlocks)
        {
            succ->forAllIncomingEdges([&](CFGNode& predecessor, CFGEdge& edge) -> bool {
                // the predecessor instruction is the branch to this block (if not fall-through)
                auto lastIt = edge.data.getPredecessor(predecessor.key);

                // copy the whole block content before the branch to the block, modify writing all external locals
                // to only be applied for the same condition the branch is applied and remove the branch (if not
                // fall-through).
                // at the moment of this optimization, the writing of the conditional the branch depends on is
                // already generated, so we can just re-use the conditional.
                Optional<Value> condVal{};
                ConditionCode cond = COND_ALWAYS;
                {
                    auto branch = lastIt.get<intermediate::Branch>();
                    if(branch && branch->getTarget() == succ->key->getLabel()->getLabel() &&
                        branch->hasConditionalExecution())
                    {
                        condVal = branch->getCondition();
                        cond = branch->conditional;
                    }
                    else
                    {
                        // the last branch maybe unconditional (e.g. the default for switch-cases), but we need to
                        // insert the unconditional local assignment as first instruction.
                        if(branch && branch->getTarget() == succ->key->getLabel()->getLabel())
                            // remove original unconditional branch. If this is a fall-through don't remove anything
                            lastIt.erase();
                        // make sure the instructions are inserted before all other
                        lastIt = beforeBranchesIt;
                    }
                }

                // 1.) insert flag depending on the conditional of the branch
                if(condVal && cond != COND_ALWAYS)
                    assign(lastIt, NOP_REGISTER) = (*condVal, SetFlag::SET_FLAGS);

                // 2.) insert all instructions
                for(auto& inst : *succ->key)
                {
                    if(dynamic_cast<const intermediate::BranchLabel*>(inst.get()))
                        // neither move nor delete the label
                        continue;

                    if(dynamic_cast<const intermediate::Branch*>(inst.get()))
                    {
                        // do not copy branches to successor label
                        inst.reset();
                        continue;
                    }

                    lastIt.emplace(inst.release());

                    // 3.) modify all instructions writing non-locals to only write under same condition as the
                    // branch
                    // XXX do we win anything in making all the instructions conditional? Technically this would be
                    // possible
                    for(auto loc : nonlocalLocals)
                    {
                        if(lastIt->writesLocal(loc))
                        {
                            lastIt->setCondition(cond);
                            break;
                        }
                    }

                    lastIt.nextInBlock();
                }

                // 4.) remove branch to original block
                if(condVal)
                    lastIt.erase();

                // 5.) remove original block
                if(!method.removeBlock(*succ->key))
                {
                    CPPLOG_LAZY_BLOCK(logging::Level::WARNING, {
                        logging::warn() << "Failed to remove move-from basic block: " << succ->key->to_string()
                                        << logging::endl;
                        succ->key->dumpInstructions();
                    });
                    // XXX throw exception here or continue??
                }

                // there is only one incoming edge
                return false;
            });
        }

        // insert branch to successor block to guarantee we switch into that, independent of the block order
        block.predecessor->key->walkEnd().emplace(
            new intermediate::Branch(block.successor->key->getLabel()->getLabel(), COND_ALWAYS, BOOL_TRUE));

        changedCode = true;
    }

    return changedCode;
}

// If we loop through all work-groups, the initial work-group id for all dimensions is always zero
// Also, to not override the work-group index at every iteration, extract the writing out of the loops
static bool moveGroupIdInitializers(Method& method, BasicBlock& defaultBlock, BasicBlock& newStartBlock)
{
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Moving group ID initializers out of work-group-loop..." << logging::endl);
    auto insertIt = newStartBlock.walkEnd();
    // whether the group id locals are not written and therefore not used inside the "real" kernel code
    bool groupIdsOnlyRead = true;
    for(auto type : {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z})
    {
        auto loc = method.findOrCreateBuiltin(type);
        if(auto writer = loc->getSingleWriter())
        {
            groupIdsOnlyRead = false;
            if(auto it = defaultBlock.findWalkerForInstruction(writer, defaultBlock.walkEnd()))
                it->erase();
        }
        assign(insertIt, loc->createReference()) = INT_ZERO;
    }

    return groupIdsOnlyRead;
}

// Reset all branches to the last-block (e.g. for return statements) to the reset block instead
static void resetReturnBranches(Method& method, BasicBlock& lastBlock, BasicBlock& resetBlock)
{
    lastBlock.forPredecessors([&](InstructionWalker walker) {
        if(auto branch = walker.get<intermediate::Branch>())
        {
            if(branch->getTarget() == lastBlock.getLabel()->getLabel())
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Resetting branch to last block to jump to first work-group repetition block instead: "
                        << walker->to_string() << logging::endl);
                // need to reset the instruction to correctly update the CFG
                walker.reset((new intermediate::Branch(
                                  resetBlock.getLabel()->getLabel(), branch->conditional, branch->getCondition()))
                                 ->copyExtrasFrom(branch));
            }
        }
        // fall-throughs are already handled by inserting the block
    });
}

// After the main kernel code executed, insert a block which
// - reads uniform address
// - reads maximum values for all group id dimensions
// - resets uniform pointer to previously read address
NODISCARD static InstructionWalker insertAddressResetBlock(
    Method& method, InstructionWalker it, const Value& maxGroupIdX, const Value& maxGroupIdY, const Value& maxGroupIdZ)
{
    auto& lastBlock = *it.getBasicBlock();
    it = method.emplaceLabel(
        it, new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL, "", "%work_group_repetition").local()));
    auto& resetBlock = *it.getBasicBlock();
    resetReturnBranches(method, lastBlock, resetBlock);

    // insert after label, not before
    it.nextInBlock();

    auto tmp = assign(it, TYPE_INT32) = UNIFORM_REGISTER;
    assign(it, maxGroupIdX) = UNIFORM_REGISTER;
    assign(it, maxGroupIdY) = UNIFORM_REGISTER;
    assign(it, maxGroupIdZ) = UNIFORM_REGISTER;
    assign(it, Value(REG_UNIFORM_ADDRESS, TYPE_INT32)) = tmp;
    return it;
}

// For every dimension insert a block which
// - Resets the previous id to zero
// - Increments the current id by one
// - Checks whether the current id is smaller than the maximum value
// - If so, jumps back to the default block
// - Otherwise, falls through to the next block
NODISCARD static InstructionWalker insertSingleDimensionRepetitionBlock(Method& method, const BasicBlock& defaultBlock,
    const Value& id, const Value& maxValue, InstructionWalker it, const Local* previousId)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Inserting repetition block for: " << id.to_string() << logging::endl);
    it = method.emplaceLabel(it,
        new intermediate::BranchLabel(
            *method.addNewLocal(TYPE_LABEL, "", "%repeat_" + id.local()->name.substr(1)).local()));
    it->addDecorations(InstructionDecorations::WORK_GROUP_LOOP);
    // insert after label, not before
    it.nextInBlock();

    // first increment current id then reset previous dimension to be able to skip inserting a nop, since we read the
    // current id immediately afterwards
    assign(it, id) = id + INT_ONE;
    if(previousId)
        assign(it, previousId->createReference()) = INT_ZERO;
    // NOTE: using signed comparison limits the number of work-groups per dimension to INT_MAX - 1 to avoid overflow.
    // This is also reflected host-side on the kernel_config::MAX_WORK_ITEM_DIMENSIONS limit and checked in
    // Kernel::enqueueNDRange.
    auto cond = assignNop(it) = as_signed{id} < as_signed{maxValue};
    // XXX can be optimized, when branch supports carry conditions
    auto condValue = method.addNewLocal(TYPE_BOOL);
    assign(it, condValue) = (BOOL_TRUE, cond);
    assign(it, condValue) = (BOOL_TRUE ^ BOOL_TRUE, cond.invert());
    it.emplace((new intermediate::Branch(defaultBlock.getLabel()->getLabel(), COND_ZERO_CLEAR, condValue))
                   ->addDecorations(InstructionDecorations::WORK_GROUP_LOOP));
    it.nextInMethod();
    return it;
}

static void insertRepetitionBlocks(Method& method, const BasicBlock& defaultBlock, BasicBlock& lastBlock)
{
    auto maxGroupIdX = method.findOrCreateBuiltin(BuiltinLocal::Type::MAX_GROUP_ID_X)->createReference();
    auto maxGroupIdY = method.findOrCreateBuiltin(BuiltinLocal::Type::MAX_GROUP_ID_Y)->createReference();
    auto maxGroupIdZ = method.findOrCreateBuiltin(BuiltinLocal::Type::MAX_GROUP_ID_Z)->createReference();

    auto it = lastBlock.walk();
    it = insertAddressResetBlock(method, it, maxGroupIdX, maxGroupIdY, maxGroupIdZ);

    auto groupIdX = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_ID_X)->createReference();
    it = insertSingleDimensionRepetitionBlock(method, defaultBlock, groupIdX, maxGroupIdX, it, nullptr);

    auto groupIdY = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_ID_Y)->createReference();
    it = insertSingleDimensionRepetitionBlock(method, defaultBlock, groupIdY, maxGroupIdY, it, maxGroupIdX.local());

    auto groupIdZ = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_ID_Z)->createReference();
    it = insertSingleDimensionRepetitionBlock(method, defaultBlock, groupIdZ, maxGroupIdZ, it, maxGroupIdY.local());
}

bool optimizations::addWorkGroupLoop(const Module& module, Method& method, const Configuration& config)
{
    if(method.walkAllInstructions().isEndOfMethod())
        return false;
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Wrapping kernel " << method.name << " in a work-group loop..." << logging::endl);

    // The old head block, the head block of the actual kernel execution
    auto& defaultBlock = *method.begin();
    if(defaultBlock.empty())
        return false;

    // The new head block, the block initializing the group ids
    auto startIt = method.emplaceLabel(method.walkAllInstructions(),
        new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL, "", "%group_id_initializer").local()));
    startIt->addDecorations(InstructionDecorations::WORK_GROUP_LOOP);
    auto& startBlock = *startIt.getBasicBlock();

    // The old and new tail block which indicates kernel execution finished
    auto lastBlock = method.findBasicBlock(BasicBlock::LAST_BLOCK);
    if(!lastBlock)
    {
        CPPLOG_LAZY(
            logging::Level::WARNING, log << "Failed to find the default last block, aborting!" << logging::endl);
        return false;
    }

    // Remove reads of UNIFORMs for group ids and move initializing to zero out of loop
    bool groupIdsNotUsed = moveGroupIdInitializers(method, defaultBlock, startBlock);

    // Insert all the code required to increment/reset the ids and repeat the kernel code
    insertRepetitionBlocks(method, defaultBlock, *lastBlock);

    // set correct information to metadata
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Adjusting kernel metadata..." << logging::endl);
    method.metaData.uniformsUsed.setGroupIDXUsed(false);
    method.metaData.uniformsUsed.setGroupIDYUsed(false);
    method.metaData.uniformsUsed.setGroupIDZUsed(false);
    method.metaData.uniformsUsed.setUniformAddressUsed(true);
    method.metaData.uniformsUsed.setMaxGroupIDXUsed(true);
    method.metaData.uniformsUsed.setMaxGroupIDYUsed(true);
    method.metaData.uniformsUsed.setMaxGroupIDZUsed(true);

    // TODO can lower register pressure by 2 over whole code at the cost of 2 instructions per cycle by compressing
    // all group_ids into single register. Also saves 2 instructions for initialization. Run at least if group_ids
    // are not used differently in code.
    // TODO cases whether group ids are not used are not very many (used in get_global_id(...))
    // TODO or instead group all group_id, group_offset, ... into vectors (per type, e.g. group_id[3],
    // group_offset[3])?? Requires 2 instructions per element to initialize, 2 instructions to extract. Could also do
    // calculation of e.g. get_global_id(?) in parallel for all (3) dimensions -> Extra optimization?! Enhance/Replace
    // local-compression with that? XXX Or run as adjustment step if register-pressure more than X??V
    if(groupIdsNotUsed)
        ; // XXX logging::warn() << "TEST: Group ids are not used in kernel: " << method.name << logging::endl;

    return true;
}
