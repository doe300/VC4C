/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Reordering.h"

#include "../Profiler.h"
#include "../intermediate/Helper.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;

/*
 * Finds the last instruction before the (list of) NOP(s) that is not a NOP -> the reason for the insertion of NOPs
 */
static NODISCARD InstructionWalker findPreviousInstruction(BasicBlock& basicBlock, const InstructionWalker pos)
{
    auto it = pos;
    while(!it.isStartOfBlock())
    {
        if(it.get() != nullptr && it->mapsToASMInstruction() && it->getOutput())
            break;
        it.previousInBlock();
    }
    return it;
}

/*
 * Finds an instruction within the basic block that does not access any of the given values
 */
static NODISCARD InstructionWalker findInstructionNotAccessing(BasicBlock& basicBlock, const InstructionWalker pos,
    FastSet<Value>& excludedValues, unsigned replaceNopThreshold, unsigned accumulatorThreshold)
{
    std::size_t instructionsLeft = replaceNopThreshold;
    auto it = pos;
    while(instructionsLeft > 0 && !it.isEndOfBlock())
    {
        if(!it.has())
        {
            // skip already replaced instructions
            it.nextInBlock();
            --instructionsLeft;
            continue;
        }
        bool validReplacement = true;
        PROFILE_START(checkExcludedValues);
        if(it->getOutput() && excludedValues.find(it->getOutput().value()) != excludedValues.end())
        {
            validReplacement = false;
        }
        if(validReplacement)
        {
            for(const Value& arg : it->getArguments())
            {
                if(excludedValues.find(arg) != excludedValues.end())
                {
                    validReplacement = false;
                    break;
                }
            }
        }
        PROFILE_END(checkExcludedValues);
        if(validReplacement && it->writesRegister(REG_MUTEX))
        {
            // never move MUTEX_RELEASE
            validReplacement = false;
            // Allow instructions to be moved over MUTEX_RELEASE to replace the VPM wait nops
        }
        if(validReplacement && it->readsRegister(REG_MUTEX))
        {
            // Re-ordering MUTEX_ACQUIRE would extend the critical section (maybe a lot!), so don't move it
            // Also, never move anything out of (or over) the critical section
            return basicBlock.walkEnd();
        }
        // for now, skip everything setting and using flags/signals
        if(validReplacement && (it->hasConditionalExecution() || it->hasSideEffects()))
        {
            validReplacement = false;
        }
        if(validReplacement && (it.get<Branch>() || it.get<BranchLabel>() || it.get<MemoryBarrier>()))
        {
            // NEVER RE-ORDER BRANCHES, LABELS OR BARRIERS!
            validReplacement = false;
        }
        if(validReplacement && it.get<Nop>())
        {
            // replacing NOP with NOP will violate the delay (e.g. for branches, SFU)
            validReplacement = false;
        }
        if(validReplacement && !it->mapsToASMInstruction())
        {
            // skip every instruction, which is not mapped to machine code, since otherwise the delay for the NOP will
            // be violated
            validReplacement = false;
        }
        if(validReplacement && it->checkOutputLocal() &&
            (replaceNopThreshold - instructionsLeft) > accumulatorThreshold &&
            it.getBasicBlock()->isLocallyLimited(it, it->checkOutputLocal(), accumulatorThreshold))
        {
            // skip any local which is currently locally limited enough to fit into an accumulator to not force it to a
            // physical register (unless the instruction would only be moved a little bit)
            validReplacement = false;
        }
        if(validReplacement)
        {
            logging::logLazy(logging::Level::DEBUG, [&](std::wostream& log) {
                log << "Found instruction not using any of the excluded values ("
                    << to_string<Value, FastSet<Value>>(excludedValues) << "): " << it->to_string() << logging::endl;
            });
            break;
        }

        // otherwise add all outputs by instructions in between (the NOP and the replacement), since they could be used
        // as input in the following instructions
        auto out = NO_VALUE;
        if((out = it->getOutput()) && !out->hasRegister(REG_NOP))
        {
            excludedValues.insert(out.value());
            // make sure, SFU/TMU calls are not moved over other SFU/TMU calls
            // this prevents nop-sfu-... from being replaced with sfu-sfu-...
            if(out->hasRegister(REG_SFU_EXP2) || out->hasRegister(REG_SFU_LOG2) || out->hasRegister(REG_SFU_RECIP) ||
                out->hasRegister(REG_SFU_RECIP_SQRT) || out->hasRegister(REG_TMU0_ADDRESS) ||
                out->hasRegister(REG_TMU1_ADDRESS))
            {
                excludedValues.emplace(Value(REG_SFU_EXP2, TYPE_FLOAT));
                excludedValues.emplace(Value(REG_SFU_LOG2, TYPE_FLOAT));
                excludedValues.emplace(Value(REG_SFU_OUT, TYPE_FLOAT));
                excludedValues.emplace(Value(REG_SFU_RECIP, TYPE_FLOAT));
                excludedValues.emplace(Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT));
                excludedValues.emplace(Value(REG_TMU0_ADDRESS, TYPE_VOID_POINTER));
                excludedValues.emplace(Value(REG_TMU1_ADDRESS, TYPE_VOID_POINTER));
            }
            if(out->hasRegister(REG_ACC5) || out->hasRegister(REG_REPLICATE_ALL) ||
                out->hasRegister(REG_REPLICATE_QUAD))
            {
                excludedValues.emplace(Value(REG_ACC5, TYPE_UNKNOWN));
                excludedValues.emplace(Value(REG_REPLICATE_ALL, TYPE_UNKNOWN));
                excludedValues.emplace(Value(REG_REPLICATE_QUAD, TYPE_UNKNOWN));
            }
        }
        if(it->readsRegister(REG_SFU_OUT))
        {
            excludedValues.emplace(Value(REG_SFU_EXP2, TYPE_FLOAT));
            excludedValues.emplace(Value(REG_SFU_LOG2, TYPE_FLOAT));
            excludedValues.emplace(Value(REG_SFU_OUT, TYPE_FLOAT));
            excludedValues.emplace(Value(REG_SFU_RECIP, TYPE_FLOAT));
            excludedValues.emplace(Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT));
            excludedValues.emplace(Value(REG_TMU0_ADDRESS, TYPE_VOID_POINTER));
            excludedValues.emplace(Value(REG_TMU1_ADDRESS, TYPE_VOID_POINTER));
        }
        if(it->readsRegister(REG_ACC5) || it->readsRegister(REG_REPLICATE_ALL) || it->readsRegister(REG_REPLICATE_QUAD))
        {
            excludedValues.emplace(Value(REG_ACC5, TYPE_UNKNOWN));
            excludedValues.emplace(Value(REG_REPLICATE_ALL, TYPE_UNKNOWN));
            excludedValues.emplace(Value(REG_REPLICATE_QUAD, TYPE_UNKNOWN));
        }
        --instructionsLeft;
        it.nextInBlock();
    }
    if(instructionsLeft == 0)
        it = basicBlock.walkEnd();
    return it;
}

/*
 * Finds a suitable instruction within this basic block to replace the NOP with, without violating the reason for the
 * NOP. Also, this instruction MUST not be dependent on any instruction in between the NOP and the
 * replacement-instruction
 */
static NODISCARD InstructionWalker findReplacementCandidate(
    BasicBlock& basicBlock, const InstructionWalker pos, const DelayType nopReason, const Configuration& config)
{
    FastSet<Value> excludedValues;
    InstructionWalker replacementIt = basicBlock.walkEnd();
    switch(nopReason)
    {
    case DelayType::BRANCH_DELAY:
        // This type of NOPs do not yet exist (they are created in CodeGenerator)
        return basicBlock.walkEnd();
    case DelayType::THREAD_END:
        // there are no more instructions after THREND
        return basicBlock.walkEnd();
    case DelayType::WAIT_VPM:
    case DelayType::WAIT_REGISTER:
    {
        // can insert any instruction which does not access the given register/local
        const InstructionWalker lastInstruction = findPreviousInstruction(basicBlock, pos);
        if(lastInstruction.isStartOfBlock())
        {
            // this can e.g. happen, if the vector rotation is the first instruction in a basic block
            // TODO for now, we can't handle this case, since there may be several writing instructions jumping to the
            // block
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Can't find reason for NOP in block: " << basicBlock.to_string() << logging::endl);
            return basicBlock.walkEnd();
        }
        excludedValues.insert(lastInstruction->getOutput().value());
        if(lastInstruction->writesRegister(REG_VPM_DMA_LOAD_ADDR))
        {
            excludedValues.emplace(Value(REG_VPM_DMA_LOAD_BUSY, TYPE_UNKNOWN));
            excludedValues.emplace(Value(REG_VPM_IO, TYPE_UNKNOWN));
        }
        if(lastInstruction->writesRegister(REG_VPM_DMA_STORE_ADDR))
        {
            excludedValues.emplace(Value(REG_VPM_DMA_STORE_BUSY, TYPE_UNKNOWN));
            excludedValues.emplace(Value(REG_VPM_IO, TYPE_UNKNOWN));
        }
        if(lastInstruction->writesRegister(REG_ACC5) || lastInstruction->writesRegister(REG_REPLICATE_ALL) ||
            lastInstruction->writesRegister(REG_REPLICATE_QUAD))
        {
            excludedValues.emplace(Value(REG_ACC5, TYPE_UNKNOWN));
            excludedValues.emplace(Value(REG_REPLICATE_ALL, TYPE_UNKNOWN));
            excludedValues.emplace(Value(REG_REPLICATE_QUAD, TYPE_UNKNOWN));
        }
        PROFILE_START(findInstructionNotAccessing);
        replacementIt = findInstructionNotAccessing(basicBlock, pos, excludedValues,
            config.additionalOptions.replaceNopThreshold, config.additionalOptions.accumulatorThreshold);
        PROFILE_END(findInstructionNotAccessing);
        break;
    }
    case DelayType::WAIT_SFU:
    case DelayType::WAIT_TMU:
    {
        // can insert any instruction which doesn't access SFU/TMU or accumulator r4
        excludedValues.emplace(Value(REG_SFU_EXP2, TYPE_FLOAT));
        excludedValues.emplace(Value(REG_SFU_LOG2, TYPE_FLOAT));
        excludedValues.emplace(Value(REG_SFU_OUT, TYPE_FLOAT));
        excludedValues.emplace(Value(REG_SFU_RECIP, TYPE_FLOAT));
        excludedValues.emplace(Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT));
        excludedValues.emplace(Value(REG_TMU0_ADDRESS, TYPE_VOID_POINTER));
        excludedValues.emplace(Value(REG_TMU1_ADDRESS, TYPE_VOID_POINTER));
        PROFILE_START(findInstructionNotAccessing);
        replacementIt = findInstructionNotAccessing(basicBlock, pos, excludedValues,
            config.additionalOptions.replaceNopThreshold, config.additionalOptions.accumulatorThreshold);
        PROFILE_END(findInstructionNotAccessing);
        break;
    }
    case DelayType::WAIT_UNIFORM:
        // TODO could reorder, as long as we do not access uniforms ??
        return basicBlock.walkEnd();
    }
    return replacementIt;
}

/*
 * only insert instruction,
 * - if local is used afterwards (and not just in the next few instructions)
 * - or the pack-mode of the previous instruction is set, since in that case, the register-file A
 * MUST be used, so it cannot be read in the next instruction
 * - or the unpack-mode of this instruction is set, since in that case, the register-file A MUST be
 * used, so it cannot be written to in the previous instruction
 * - also vector-rotations MUST be on accumulator, but the input MUST NOT be written in the previous
 * instruction, so they are also split up
 */
static bool needsDelay(
    InstructionWalker prevIt, InstructionWalker nextIt, const Local* local, std::size_t accumulatorThreshold)
{
    // we also need to insert an instruction, if the local is unpacked in any successive instruction,
    // in which case it cannot be on an accumulator. Since we have a direct read-after-write, the local
    // can also not be on register-file A -> we need to insert buffer
    bool isUnpacked = false;
    local->forUsers(LocalUse::Type::READER, [&isUnpacked](const LocalUser* user) {
        if(user->hasUnpackMode())
            isUnpacked = true;
    });

    return prevIt->hasPackMode() || nextIt->hasUnpackMode() || nextIt.get<VectorRotation>() ||
        !prevIt.getBasicBlock()->isLocallyLimited(prevIt, local, accumulatorThreshold) || isUnpacked;
}

static bool replaceNOPs(BasicBlock& basicBlock, Method& method, const Configuration& config)
{
    InstructionWalker it = basicBlock.walk();
    bool hasChanged = false;
    while(!it.isEndOfBlock())
    {
        const Nop* nop = it.get<Nop>();
        // only replace NOPs without side-effects (e.g. signal)
        if(nop != nullptr && !nop->hasSideEffects())
        {
            auto isMandatoryDelay = has_flag(nop->decoration, InstructionDecorations::MANDATORY_DELAY);
            PROFILE_START(findReplacementCandidate);
            InstructionWalker replacementIt = findReplacementCandidate(basicBlock, it, nop->type, config);
            PROFILE_END(findReplacementCandidate);
            if(!replacementIt.isEndOfBlock())
            {
                // replace NOP with instruction, reset instruction at position (do not yet erase, otherwise iterators
                // are wrong!)
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing NOP with: " << replacementIt->to_string() << logging::endl);
                it.reset(replacementIt.release());
                if(isMandatoryDelay)
                    it->addDecorations(InstructionDecorations::MANDATORY_DELAY);
                hasChanged = true;

                // make sure to not create a new conflict and insert NOP instead (which might be replaced again later
                // on)
                auto prevIt = replacementIt.copy().previousInBlock();
                auto nextIt = replacementIt.copy().nextInBlock();
                while(!prevIt.has())
                    prevIt.previousInBlock();
                while(!nextIt.isEndOfBlock() && !nextIt.has())
                    nextIt.nextInBlock();
                if(!prevIt.isStartOfBlock() && prevIt.has() && !nextIt.isEndOfBlock() && nextIt.has() &&
                    prevIt->checkOutputLocal() && nextIt->readsLocal(prevIt->getOutput()->local()) &&
                    needsDelay(
                        prevIt, nextIt, prevIt->getOutput()->local(), config.additionalOptions.accumulatorThreshold))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Inserting NOP to prevent further read-after-write errors" << logging::endl);
                    replacementIt.reset(new Nop(DelayType::WAIT_REGISTER));
                }
            }
            else if(nop->type == DelayType::WAIT_VPM && !isMandatoryDelay)
            {
                // nops inserted to wait for VPM to finish can be removed again,
                // since the wait-instruction will correctly wait the remaining number of instructions
                it.erase();
                // to not skip the next nop
                it.previousInBlock();
                hasChanged = true;
            }
        }
        it.nextInBlock();
    }
    return hasChanged;
}

bool optimizations::splitReadAfterWrites(const Module& module, Method& method, const Configuration& config)
{
    // try to split up consecutive instructions writing/reading to the same local (so less locals are forced to
    // accumulators) by inserting NOPs  the NOP then can be replaced with other instructions by the next optimization
    // (#reorderWithinBasicBlocks)
    bool hasChanged = false;
    auto it = method.walkAllInstructions();
    InstructionWalker lastInstruction = it;
    // at the beginning, the last parameter read is the last local written
    const Local* lastWrittenTo = method.parameters.empty() ? nullptr : &method.parameters.back();
    // skip the first instruction, since we start the check at the read (and need to look back at the write)
    if(!it.isEndOfMethod())
        it.nextInMethod();
    while(!it.isEndOfMethod())
    {
        // skip already replaced instructions
        if(it.get() != nullptr)
        {
            if(lastWrittenTo != nullptr)
            {
                if(it->readsLocal(lastWrittenTo))
                {
                    if(needsDelay(lastInstruction, it, lastWrittenTo, config.additionalOptions.accumulatorThreshold))
                    {
                        CPPLOG_LAZY(logging::Level::DEBUG,
                            log << "Inserting NOP to split up read-after-write before: " << it->to_string()
                                << logging::endl);
                        // emplacing after the last instruction instead of before this one fixes errors with
                        // wrote-label-read, which then becomes  write-nop-label-read instead of write-label-nop-read
                        // and the combiner can find a reason for the NOP
                        lastInstruction.copy().nextInBlock().emplace(new Nop(DelayType::WAIT_REGISTER));
                        hasChanged = true;
                    }
                }
            }
            if(it->mapsToASMInstruction())
            {
                // ignoring instructions not mapped to machine code, e.g. labels will also check for write-label-read
                lastWrittenTo = it->checkOutputLocal();
                lastInstruction = it;
            }

            if(it->readsRegister(REG_VPM_DMA_LOAD_WAIT) || it->readsRegister(REG_VPM_DMA_STORE_WAIT) ||
                it->readsRegister(REG_VPM_IO))
            {
                // TODO constant + x * nrows
                unsigned numDelays = 0;
                if(it->readsRegister(REG_VPM_DMA_LOAD_WAIT))
                    numDelays = 6; // XXX 8
                else if(it->readsRegister(REG_VPM_DMA_STORE_WAIT))
                    numDelays = 10; // XXX 12
                // TODO else insert delay only before first read!
                for(unsigned i = 0; i < numDelays; ++i)
                {
                    it.emplace(new Nop(DelayType::WAIT_VPM));
                    it.nextInBlock();
                    hasChanged = true;
                }
            }
        }
        it.nextInMethod();
    }
    return hasChanged;
}

bool optimizations::reorderWithinBasicBlocks(const Module& module, Method& method, const Configuration& config)
{
    /*
     * TODO re-order instructions to:
     * 2. combine instructions(try to pair instruction from ADD and MUL ALU together, or moves)
     * 3. split up VPM setup and wait VPM wait, so the delay can be used productively (only possible if we allow
     * reordering over mutex-release). How many instructions to try to insert? 3?
     */
    bool hasChanged = false;
    for(BasicBlock& block : method)
    {
        // remove NOPs by inserting instructions which do not violate the reason for the NOP
        PROFILE_START(replaceNOPs);
        if(replaceNOPs(block, method, config))
            hasChanged = true;
        PROFILE_END(replaceNOPs);
    }

    // after all re-orders are done, remove empty instructions
    method.cleanEmptyInstructions();
    return hasChanged;
}

InstructionWalker optimizations::moveRotationSourcesToAccumulators(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // makes sure, all sources for vector-rotations have a usage-range small enough to be on an accumulator
    /*
     * "The full horizontal vector rotate is only available when both of the mul ALU input arguments are taken from
     * accumulators r0-r3."
     * - Broadcom specification, page 20
     *
     */
    auto rot = it.get<VectorRotation>();
    if(rot && rot->isFullRotationAllowed())
    {
        // NOTE: can either run on if-full-rotation allowed, this is greedy, will rewrite some cases where not necessary
        // or on if-quad-rotation-not-allowed, this is generous, will only rewrite when necessary, but might cause
        // register allocation errors
        if(auto loc = rot->getSource().checkLocal())
        {
            InstructionWalker writer = it.copy().previousInBlock();
            while(!writer.isStartOfBlock())
            {
                if(writer.has() && writer->checkOutputLocal() && writer->getOutput()->hasLocal(loc))
                    break;
                writer.previousInBlock();
            }
            // if the local is either written in another block or the usage-range exceeds the accumulator threshold,
            // move to temporary
            if(writer.isStartOfBlock() ||
                !writer.getBasicBlock()->isLocallyLimited(writer, loc, config.additionalOptions.accumulatorThreshold))
            {
                InstructionWalker mapper = it.copy().previousInBlock();
                // insert mapper before first NOP
                while(!mapper.isStartOfBlock() && mapper.copy().previousInBlock().get<Nop>())
                    mapper.previousInBlock();
                if(mapper.isStartOfBlock())
                    mapper.nextInBlock();
                // TODO no need for the nop if there is another instruction before the rotation not writing the local
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Moving source of vector-rotation to temporary for: " << it->to_string() << logging::endl);
                const Value tmp = method.addNewLocal(loc->type, "%vector_rotation");
                mapper.emplace(new MoveOperation(tmp, loc->createReference()));
                it->replaceLocal(loc, tmp.local(), LocalUse::Type::READER);
                return writer;
            }
        }
        else if(rot->getSource().checkRegister() &&
            (!rot->getSource().reg().isAccumulator() || rot->getSource().reg().getAccumulatorNumber() > 3))
        {
            // e.g. inserting into vector from reading VPM
            // insert temporary local to be read into, rotate local and NOP, since it is required
            auto tmp = method.addNewLocal(rot->getSource().type, "%vector_rotation");
            it.emplace(new MoveOperation(tmp, rot->getSource()));
            it.nextInBlock();
            it.emplace(new Nop(DelayType::WAIT_REGISTER));
            it.nextInBlock();
            rot->setSource(std::move(tmp));
        }
    }
    return it;
}
