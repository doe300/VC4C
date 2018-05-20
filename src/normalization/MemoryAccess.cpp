/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryAccess.h"

#include "../InstructionWalker.h"
#include "../Module.h"
#include "../Profiler.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <functional>
#include <numeric>

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;
using namespace vc4c::periphery;

struct BaseAndOffset
{
    Optional<Value> base;
    Optional<int32_t> offset;
    Optional<int32_t> maxOffset;

    explicit BaseAndOffset() : base(NO_VALUE), offset{}, maxOffset{} {}

    BaseAndOffset(const Optional<Value>& base, Optional<int32_t> offset, Optional<int32_t> maxOffset = {}) :
        base(base), offset(offset), maxOffset(maxOffset)
    {
    }
};

static BaseAndOffset findOffset(const Value& val)
{
    if(!val.hasType(ValueType::LOCAL))
        return BaseAndOffset();
    const LocalUser* writer = val.getSingleWriter();
    if(writer != nullptr)
    {
        const Optional<Value> offset = writer->precalculate(8);
        if(offset.ifPresent(toFunction(&Value::isLiteralValue)))
        {
            return BaseAndOffset(
                NO_VALUE, offset->getLiteralValue()->signedInt(), offset->getLiteralValue()->signedInt());
        }
    }
    return BaseAndOffset();
}

static BaseAndOffset findBaseAndOffset(const Value& val)
{
    // TODO add support for offsets via getlocal/global_id, etc.
    // need to the set base to addr + offset and the offset to the offset of the offset (e.g. param[get_local_id(0) +
    // 7])  but how to determine?
    if(!val.hasType(ValueType::LOCAL))
        return BaseAndOffset();
    if(val.local->is<Parameter>() || val.local->is<Global>() || val.local->is<StackAllocation>())
        return BaseAndOffset(val, 0);

    // follow the references
    const Local* ref = val.local->getBase(false);
    if(ref != val.local)
        return findBaseAndOffset(ref->createReference());
    if(val.local->reference.first != nullptr && val.local->reference.second != ANY_ELEMENT)
        return BaseAndOffset(val.local->reference.first->createReference(), val.local->reference.second);

    const auto writers = val.local->getUsers(LocalUse::Type::WRITER);
    if(writers.size() != 1)
        return BaseAndOffset();

    // The reader can be one of several valid cases:
    // 1. a move from another local -> need to follow the move
    if(dynamic_cast<const MoveOperation*>(*writers.begin()) != nullptr)
        return findBaseAndOffset(dynamic_cast<const MoveOperation*>(*writers.begin())->getSource());
    const auto& args = (*writers.begin())->getArguments();
    // 2. an arithmetic operation with a local and a literal -> the local is the base, the literal the offset
    if(args.size() == 2 &&
        std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.hasType(ValueType::LOCAL); }) &&
        std::any_of(
            args.begin(), args.end(), [](const Value& arg) -> bool { return arg.getLiteralValue().has_value(); }))
    {
        return BaseAndOffset(std::find_if(args.begin(), args.end(),
                                 [](const Value& arg) -> bool { return arg.hasType(ValueType::LOCAL); })
                                 ->local->getBase(false)
                                 ->createReference(),
            static_cast<int32_t>((*std::find_if(args.begin(), args.end(),
                                      [](const Value& arg) -> bool { return arg.getLiteralValue().has_value(); }))
                                     .getLiteralValue()
                                     ->signedInt() /
                val.type.getElementType().getPhysicalWidth()));
    }

    // 3. an arithmetic operation with two locals -> one is the base, the other the calculation of the literal
    if(args.size() == 2 &&
        std::all_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.hasType(ValueType::LOCAL); }))
    {
        const auto offset0 = findOffset(args[0]);
        const auto offset1 = findOffset(args[1]);
        if(offset0.offset && args[1].hasType(ValueType::LOCAL))
            return BaseAndOffset(args[1].local->getBase(false)->createReference(),
                static_cast<int32_t>(offset0.offset.value() / val.type.getElementType().getPhysicalWidth()));
        if(offset1.offset && args[0].hasType(ValueType::LOCAL))
            return BaseAndOffset(args[0].local->getBase(false)->createReference(),
                static_cast<int32_t>(offset1.offset.value() / val.type.getElementType().getPhysicalWidth()));
    }

    return BaseAndOffset();
}

struct VPMAccessGroup
{
    bool isVPMWrite;
    DataType groupType = TYPE_UNKNOWN;
    RandomAccessList<InstructionWalker> dmaSetups;
    RandomAccessList<InstructionWalker> genericSetups;
    RandomAccessList<InstructionWalker> addressWrites;

    /*
     * E.g. for memory-fills or -copies, the setup-instructions are re-used,
     * so we need to clear the duplicates
     */
    void cleanDuplicateInstructions()
    {
        auto it = dmaSetups.begin();
        ++it;
        while(it != dmaSetups.end())
        {
            if((it - 1)->get() == it->get())
                it = dmaSetups.erase(it);
            else
                ++it;
        }

        it = genericSetups.begin();
        ++it;
        while(it != genericSetups.end())
        {
            if((it - 1)->get() == it->get())
                it = genericSetups.erase(it);
            else
                ++it;
        }

        it = addressWrites.begin();
        ++it;
        while(it != addressWrites.end())
        {
            if((it - 1)->get() == it->get())
                it = addressWrites.erase(it);
            else
                ++it;
        }
    }
};

static InstructionWalker findGroupOfVPMAccess(
    VPM& vpm, InstructionWalker start, const InstructionWalker end, VPMAccessGroup& group)
{
    Optional<Value> baseAddress = NO_VALUE;
    int32_t nextOffset = -1;
    group.groupType = TYPE_UNKNOWN;
    group.dmaSetups.clear();
    group.genericSetups.clear();
    group.addressWrites.clear();
    group.dmaSetups.reserve(64);
    group.dmaSetups.reserve(64);
    group.addressWrites.reserve(64);

    // FIXME to not build too large critical sections, only combine, if the resulting critical section:
    // 1) is not too large: either in total numbers of instructions or in ratio instructions / VPW writes, since we save
    // a few cycles per write (incl. delay for wait DMA)

    auto it = start;
    for(; !it.isEndOfBlock() && it != end; it.nextInBlock())
    {
        if(it.get() == nullptr)
            continue;

        if(it.has<MemoryBarrier>())
            // memory barriers end groups, also don't check this barrier again
            return it.nextInBlock();
        if(it.has<SemaphoreAdjustment>())
            // semaphore accesses end groups, also don't check this instruction again
            return it.nextInBlock();

        if(!(it->writesRegister(REG_VPM_IN_ADDR) || it->writesRegister(REG_VPM_OUT_ADDR)))
            // for simplicity, we only check for VPM addresses and find all other instructions relative to it
            continue;
        if(!it.has<MoveOperation>())
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Setting VPM address with non-move is not supported", it->to_string());
        const auto baseAndOffset = findBaseAndOffset(it.get<MoveOperation>()->getSource());
        const bool isVPMWrite = it->writesRegister(REG_VPM_OUT_ADDR);
        logging::debug() << "Found base address " << baseAndOffset.base.to_string() << " with offset "
                         << std::to_string(baseAndOffset.offset.value_or(-1L)) << " for "
                         << (isVPMWrite ? "writing into" : "reading from") << " memory" << logging::endl;

        if(!baseAndOffset.base)
            // this address-write could not be fixed to a base and an offset
            // skip this address write for the next check
            return it.nextInBlock();
        if(baseAndOffset.base && baseAndOffset.base->hasType(ValueType::LOCAL) &&
            baseAndOffset.base->local->is<Parameter>() &&
            has_flag(baseAndOffset.base->local->as<Parameter>()->decorations, ParameterDecorations::VOLATILE))
            // address points to a volatile parameter, which explicitly forbids combining reads/writes
            // skip this address write for the next check
            return it.nextInBlock();

        // check if this address is consecutive to the previous one (if any)
        if(baseAddress)
        {
            if(baseAndOffset.base && baseAddress.value() != baseAndOffset.base.value())
                // a group exists, but the base addresses don't match
                break;
            if(!baseAndOffset.offset || baseAndOffset.offset.value() != nextOffset)
                // a group exists, but the offsets do not match
                break;
        }

        // check if the access mode (read/write) is the same as for the group
        if(baseAddress && group.isVPMWrite != isVPMWrite)
            break;

        auto vpmSetups = periphery::findRelatedVPMInstructions(it, !isVPMWrite);
        auto genericSetup = vpmSetups.genericVPMSetup;
        auto dmaSetup = vpmSetups.dmaSetup;

        // check if the VPM and DMA configurations match with the previous one
        if(baseAddress)
        {
            if(!genericSetup || !dmaSetup)
                // either there are no setups for this VPM access, or they are not loaded from literals (e.g. dynamic
                // setup)
                break;
            if(!genericSetup->has<LoadImmediate>() ||
                (!group.genericSetups.empty() &&
                    genericSetup->get<LoadImmediate>()->getImmediate() !=
                        group.genericSetups.at(0).get<LoadImmediate>()->getImmediate()))
                // generic setups do not match
                break;
            if(!dmaSetup->has<LoadImmediate>() ||
                dmaSetup->get<LoadImmediate>()->getImmediate() !=
                    group.dmaSetups.at(0).get<LoadImmediate>()->getImmediate())
                // DMA setups do not match
                break;
        }

        // check for complex types
        DataType elementType = baseAndOffset.base->type.isPointerType() ?
            baseAndOffset.base->type.getPointerType().value()->elementType :
            baseAndOffset.base->type;
        elementType = elementType.getArrayType() ? elementType.getArrayType().value()->elementType : elementType;
        if(!elementType.isSimpleType())
            // XXX for now, skip combining any access to complex types (here: only struct, image)
            // don't check this read/write again
            return it.nextInBlock();

        // all matches so far, add to group (or create a new one)
        group.isVPMWrite = isVPMWrite;
        group.groupType = baseAndOffset.base->type;
        baseAddress = baseAndOffset.base.value();
        group.addressWrites.push_back(it);
        if(dmaSetup)
            // not always given, e.g. for caching in VPM without accessing RAM
            group.dmaSetups.push_back(dmaSetup.value());
        if(genericSetup)
            // not always given, e.g. for copying memory without reading/writing into/from QPU
            group.genericSetups.push_back(genericSetup.value());
        nextOffset = baseAndOffset.offset.value_or(-1) + 1;

        if(group.isVPMWrite && group.addressWrites.size() >= vpm.getMaxCacheVectors(elementType, true))
        {
            // since the current address write might be removed, skip to next instruction
            // TODO could the following instruction(s) be removed too?? (See beneath)
            return it.nextInBlock();
        }
        if(!group.isVPMWrite && group.addressWrites.size() >= vpm.getMaxCacheVectors(elementType, false))
        {
            // since the current address write might be removed, skip to next instruction
            // also need to skip the consecutive DMA wait and VPM setup
            do
            {
                it.nextInBlock();
            } while(!it.isEndOfBlock() && it.get() != nullptr && it->hasValueType(ValueType::REGISTER));
            return it;
        }
    }
    // end group, but do check this instruction again
    return it;
}

static void groupVPMWrites(VPM& vpm, VPMAccessGroup& group)
{
    if(group.genericSetups.size() != group.addressWrites.size() || group.genericSetups.size() != group.dmaSetups.size())
    {
        logging::debug() << "Number of instructions do not match for combining VPM writes!" << logging::endl;
        logging::debug() << group.genericSetups.size() << " generic VPM setups, " << group.addressWrites.size()
                         << " VPR address writes and " << group.dmaSetups.size() << " DMA setups" << logging::endl;
        return;
    }
    if(group.addressWrites.size() <= 1)
        return;
    logging::debug() << "Combining " << group.addressWrites.size()
                     << " writes to consecutive memory into one DMA write... " << logging::endl;

    // 1. Update DMA setup to the number of rows written
    {
        VPWSetupWrapper dmaSetupValue(group.dmaSetups.at(0).get<LoadImmediate>());
        dmaSetupValue.dmaSetup.setUnits(static_cast<uint8_t>(group.addressWrites.size()));
    }
    std::size_t numRemoved = 0;
    vpm.updateScratchSize(static_cast<unsigned char>(group.addressWrites.size()));

    // 2. Remove all but the first generic and DMA setups
    for(std::size_t i = 1; i < group.genericSetups.size(); ++i)
    {
        group.genericSetups[i].erase();
        const LoadImmediate* strideSetup = group.dmaSetups.at(i).copy().nextInBlock().get<LoadImmediate>();
        if(strideSetup == nullptr || !strideSetup->writesRegister(REG_VPM_OUT_SETUP) ||
            !VPWSetup::fromLiteral(strideSetup->getImmediate().unsignedInt()).isStrideSetup())
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPW DMA stride setup for DMA setup",
                group.dmaSetups.at(i)->to_string());
        group.dmaSetups.at(i).copy().nextInBlock().erase();
        group.dmaSetups.at(i).erase();
        numRemoved += 3;
    }

    // 3. remove all but the last address writes (and the following DMA waits), update the last write to write the first
    // address written to
    group.addressWrites.back().get<MoveOperation>()->setSource(
        group.addressWrites.at(0).get<MoveOperation>()->getSource());
    for(std::size_t i = 0; i < group.addressWrites.size() - 1; ++i)
    {
        if(!group.addressWrites[i].copy().nextInBlock()->readsRegister(
               group.isVPMWrite ? REG_VPM_OUT_WAIT : REG_VPM_IN_WAIT))
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPW wait for address write",
                group.addressWrites[i]->to_string());
        group.addressWrites[i].copy().nextInBlock().erase();
        group.addressWrites[i].erase();
        numRemoved += 2;
    }

    // 4. remove all Mutex acquires and releases between the first and the last write, so memory consistency is restored
    auto it = group.dmaSetups.front();
    while(!it.isEndOfBlock() && it != group.addressWrites.back())
    {
        if(it.get() && it->writesRegister(REG_MUTEX))
        {
            it = it.erase();
            ++numRemoved;
        }
        else if(it.get() && it->readsRegister(REG_MUTEX))
        {
            it = it.erase();
            ++numRemoved;
        }
        else
            it.nextInBlock();
    }

    logging::debug() << "Removed " << numRemoved << " instructions by combining VPW writes" << logging::endl;
}

static void groupVPMReads(VPM& vpm, VPMAccessGroup& group)
{
    if(group.genericSetups.size() != group.addressWrites.size() || group.genericSetups.size() != group.dmaSetups.size())
    {
        logging::debug() << "Number of instructions do not match for combining VPM reads!" << logging::endl;
        logging::debug() << group.genericSetups.size() << " generic VPM setups, " << group.addressWrites.size()
                         << " VPR address writes and " << group.dmaSetups.size() << " DMA setups" << logging::endl;
        return;
    }
    if(group.genericSetups.size() <= 1)
        return;
    logging::debug() << "Combining " << group.genericSetups.size()
                     << " reads of consecutive memory into one DMA read... " << logging::endl;

    // 1. Update DMA setup to the number of rows read
    {
        VPRSetupWrapper dmaSetupValue(group.dmaSetups.at(0).get<LoadImmediate>());
        dmaSetupValue.dmaSetup.setNumberRows(group.genericSetups.size() % 16);
        vpm.updateScratchSize(static_cast<unsigned char>(group.genericSetups.size()));
        // TODO can be space-optimized, half-words and bytes can be packed into single row (VPM writes too)
    }
    std::size_t numRemoved = 0;

    // 1.1 Update generic Setup to the number of rows read
    {
        VPRSetupWrapper genericSetup(group.genericSetups.at(0).get<LoadImmediate>());
        genericSetup.genericSetup.setNumber(group.genericSetups.size() % 16);
    }

    // 2. Remove all but the first generic and DMA setups
    for(std::size_t i = 1; i < group.genericSetups.size(); ++i)
    {
        group.genericSetups[i].erase();
        const LoadImmediate* strideSetup = group.dmaSetups.at(i).copy().nextInBlock().get<LoadImmediate>();
        if(strideSetup == nullptr || !strideSetup->writesRegister(REG_VPM_IN_SETUP) ||
            !VPRSetup::fromLiteral(strideSetup->getImmediate().unsignedInt()).isStrideSetup())
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPR DMA stride setup for DMA setup",
                group.dmaSetups.at(i)->to_string());
        group.dmaSetups.at(i).copy().nextInBlock().erase();
        group.dmaSetups.at(i).erase();
        numRemoved += 2;
    }

    // 3. remove all Mutex acquires and releases between the first and the last write, so memory consistency is restored
    auto it = group.addressWrites.front();
    while(!it.isEndOfBlock() && it != group.addressWrites.back())
    {
        if(it.get() && it->writesRegister(REG_MUTEX))
        {
            it = it.erase();
            ++numRemoved;
        }
        else if(it.get() && it->readsRegister(REG_MUTEX))
        {
            it = it.erase();
            ++numRemoved;
        }
        else
            it.nextInBlock();
    }

    // 4. remove all but the first address writes (and the following DMA writes)
    for(std::size_t i = 1; i < group.addressWrites.size(); ++i)
    {
        if(!group.addressWrites[i].copy().nextInBlock()->readsRegister(
               group.isVPMWrite ? REG_VPM_OUT_WAIT : REG_VPM_IN_WAIT))
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPR wait for address write",
                group.addressWrites[i]->to_string());
        group.addressWrites[i].copy().nextInBlock().erase();
        group.addressWrites[i].erase();
        numRemoved += 2;
    }

    logging::debug() << "Removed " << numRemoved << " instructions by combining VPR reads" << logging::endl;
}

/*
 * Combine consecutive configuration of VPW/VPR with the same settings
 *
 * In detail, this combines VPM read/writes of uniform type of access (read or write), uniform data-type and consecutive
 * memory-addresses
 *
 * NOTE: Combining VPM accesses merges their mutex-lock blocks which can cause other QPUs to stall for a long time.
 * Also, this optimization currently only supports access memory <-> QPU, data exchange between only memory and VPM are
 * not optimized
 */
static void combineVPMAccess(FastSet<BasicBlock*>& blocks, Method& method)
{
    // combine configurations of VPM (VPW/VPR) which have the same values

    // TODO for now, this cannot handle RAM->VPM, VPM->RAM only access as well as VPM->QPU or QPU->VPM

    // run within all basic blocks
    for(BasicBlock* block : blocks)
    {
        auto it = block->begin();
        while(!it.isEndOfBlock())
        {
            VPMAccessGroup group;
            it = findGroupOfVPMAccess(*method.vpm.get(), it, block->end(), group);
            if(group.addressWrites.size() > 1)
            {
                group.cleanDuplicateInstructions();
                if(group.isVPMWrite)
                    groupVPMWrites(*method.vpm.get(), group);
                else
                    groupVPMReads(*method.vpm.get(), group);
            }
        }
    }

    // clean up empty instructions
    method.cleanEmptyInstructions();
    PROFILE_COUNTER(
        vc4c::profiler::COUNTER_GENERAL + 80, "Scratch memory size (in rows)", method.vpm->getScratchArea().numRows);
}

InstructionWalker normalization::accessGlobalData(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    /*
     * Map pointer to global data to the start-of-global-data parameter
     *  plus the offset of the global data
     */
    for(std::size_t i = 0; i < it->getArguments().size(); ++i)
    {
        const auto& arg = it->assertArgument(i);
        if(arg.hasType(ValueType::LOCAL) && arg.local->is<Global>())
        {
            const Optional<unsigned int> globalOffset = module.getGlobalDataOffset(arg.local);
            if(globalOffset)
            {
                logging::debug() << "Replacing access to global data: " << it->to_string() << logging::endl;
                Value tmp = UNDEFINED_VALUE;
                if(globalOffset.value() == 0)
                {
                    tmp = method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference();
                }
                else
                {
                    // emplace calculation of global-data pointer and replace argument
                    tmp = method.addNewLocal(TYPE_INT32, "%global_data_offset");
                    it.emplace(new intermediate::Operation(OP_ADD, tmp,
                        method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference(),
                        Value(Literal(globalOffset.value()), TYPE_INT32)));
                    it.nextInBlock();
                }
                it->setArgument(i, tmp);
            }
        }
    }
    return it;
}

void normalization::spillLocals(const Module& module, Method& method, const Configuration& config)
{
    static const std::size_t MINIMUM_THRESHOLD = 128; /* TODO some better limit */
    // TODO need to know how much of the VPM is still free (need per-kernel VPM object)
    // also need to heed not to write/read into/from VPM from within a block of DMA reads/writes
    // XXX or revert: run this before #combineVPMAccess and reserve as much VPM as required (dynamically, how? or first
    // determine size, then spill)  and use the remainder for #combineVPMAccess  also need to regard writing into VPM
    // (without DMA) for #combineVPMAccess, so the VPM configurations do not conflict

    /*
     * 1. find all candidate locals for spilling:
     * - no labels (since they are never mapped to registers)
     * - only one write (for now, for easier handling)
     * - not used only locally within a minimum range, since those locals are more likely to be mapped to registers
     */
    // tracks the locals and their writing instructions
    FastMap<const Local*, InstructionWalker> spillingCandidates;
    for(const auto& pair : method.readLocals())
    {
        if(pair.second.type == TYPE_LABEL)
            continue;
        // XXX for now, only select locals which are written just once
        // or maybe never (not yet), e.g. for hidden parameter
        // or written several times but read only once
        // TODO also include explicit parameters
        auto numWrites = pair.second.getUsers(LocalUse::Type::WRITER).size();
        auto numReads = pair.second.getUsers(LocalUse::Type::READER).size();
        if((numWrites <= 1 && numReads > 0) || (numWrites >= 1 && numReads == 1))
        {
            spillingCandidates.emplace(&pair.second, InstructionWalker{});
        }
    }

    InstructionWalker it = method.walkAllInstructions();
    // skip all leading empty basic blocks
    while(!it.isEndOfMethod() && it.has<intermediate::BranchLabel>())
        it.nextInMethod();
    auto candIt = spillingCandidates.begin();
    while(!it.isEndOfMethod() && candIt != spillingCandidates.end())
    {
        // check if only read the first X instructions (from the start of the kernel), e.g. for (hidden) parameter,
        // where the next check fails,  since they are not yet written anywhere
        if(method.isLocallyLimited(it, candIt->first, MINIMUM_THRESHOLD))
            candIt = spillingCandidates.erase(candIt);
        else
            ++candIt;
    }
    while(!it.isEndOfMethod() && !spillingCandidates.empty())
    {
        // TODO if at some point all Basic block have references to their used locals, remove all locals which are used
        // just in one basic block instead of this logic??
        FastMap<const Local*, InstructionWalker>::iterator cIt;
        if(it->hasValueType(ValueType::LOCAL) &&
            (cIt = spillingCandidates.find(it->getOutput()->local)) != spillingCandidates.end())
        {
            if(method.isLocallyLimited(it, it->getOutput()->local, MINIMUM_THRESHOLD))
                spillingCandidates.erase(cIt);
            else
                cIt->second = it;
        }
        it.nextInMethod();
    }

    for(const auto& pair : spillingCandidates)
    {
        logging::debug() << "Spilling candidate: " << pair.first->to_string() << " ("
                         << pair.first->getUsers(LocalUse::Type::WRITER).size() << " writes, "
                         << pair.first->getUsers(LocalUse::Type::READER).size() << " reads)" << logging::endl;
    }

    // TODO do not preemptively spill, only on register conflicts. Which case??
}

void normalization::resolveStackAllocation(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // 1. calculate the offsets from the start of one QPU's "stack", heed alignment!
    // This is done in Normalizer

    const std::size_t stackBaseOffset = method.getStackBaseOffset();
    const std::size_t maximumStackSize = method.calculateStackSize();

    for(std::size_t i = 0; i < it->getArguments().size(); ++i)
    {
        const Value& arg = it->assertArgument(i);
        if(arg.hasType(ValueType::LOCAL) && arg.type.isPointerType() && arg.local->is<StackAllocation>())
        {
            // 2.remove the life-time instructions
            if(it.get<intermediate::LifetimeBoundary>() != nullptr)
            {
                logging::debug() << "Dropping life-time instruction for stack-allocation: " << arg.to_string()
                                 << logging::endl;
                it.erase();
                // to not skip the next instruction
                it.previousInBlock();
            }
            else
            {
                // 3. map the addresses to offsets from global-data pointer (see #accessGlobalData)
                /*
                 * Stack allocations are located in the binary data after the global data.
                 *
                 *
                 * To reduce the number of calculations, all stack allocations are grouped by their QPU, so the layout
                 * is as follows:
                 *
                 * | "Stack" of QPU0 | "Stack" of QPU1 | ...
                 *
                 * The offset of a single stack allocation can be calculated as:
                 * global-data address + global-data size + (QPU-ID * stack allocations maximum size) + offset of stack
                 * allocation = global-data address + (QPU-ID * stack allocations maximum size) + (global-data size +
                 * offset of stack allocation)
                 */
                // TODO to save instructions, could pre-calculate 'global-data address + global-data size + (QPU-ID *
                // stack allocations maximum size)' once, if any stack-allocation exists ??

                logging::debug() << "Replacing access to stack allocated data: " << it->to_string() << logging::endl;
                const Value qpuOffset = method.addNewLocal(TYPE_INT32, "%stack_offset");
                const Value addrTemp = method.addNewLocal(arg.type, "%stack_addr");
                Value finalAddr = method.addNewLocal(arg.type, "%stack_addr");
                finalAddr.local->reference = std::make_pair(arg.local, ANY_ELEMENT);

                it.emplace(new Operation(OP_MUL24, qpuOffset, Value(REG_QPU_NUMBER, TYPE_INT8),
                    Value(Literal(static_cast<uint32_t>(maximumStackSize)), TYPE_INT32)));
                it.nextInBlock();
                it.emplace(new Operation(OP_ADD, addrTemp, qpuOffset,
                    method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference()));
                it.nextInBlock();
                it.emplace(new Operation(OP_ADD, finalAddr, addrTemp,
                    Value(Literal(static_cast<uint32_t>(arg.local->as<StackAllocation>()->offset + stackBaseOffset)),
                        TYPE_INT32)));
                it.nextInBlock();
                it->setArgument(i, finalAddr);
            }
        }
    }
}

/*
 * Calculates the offset from the base-address in bytes
 */
static InstructionWalker calculateInAreaOffset(
    Method& method, InstructionWalker it, const Local* baseAddress, const Value& index, Value& inAreaOffset)
{
    if(index.hasLocal(baseAddress) || (index.hasType(ValueType::LOCAL) && index.local->getBase(false) == baseAddress))
        // no offset
        inAreaOffset = INT_ZERO;
    else if(index.hasType(ValueType::LOCAL) && index.local->reference.first == baseAddress &&
        index.local->reference.second >= 0)
        // fixed element offset
        // is offset in elements, not bytes -> so convert to byte-offset
        inAreaOffset = Value(Literal(static_cast<int32_t>(index.local->reference.second *
                                 baseAddress->type.getElementType().getPhysicalWidth())),
            TYPE_INT32);
    else if(index.getSingleWriter() != nullptr && index.getSingleWriter()->readsLocal(baseAddress))
    {
        // index is directly calculated from base-address
        const Operation* op = dynamic_cast<const Operation*>(index.getSingleWriter());
        const MoveOperation* move = dynamic_cast<const MoveOperation*>(index.getSingleWriter());
        if(op != nullptr && op->op == OP_ADD)
        {
            // index = base-address + something -> offset = something
            if(op->assertArgument(0).hasLocal(baseAddress))
                inAreaOffset = op->assertArgument(1);
            else
                inAreaOffset = op->assertArgument(0);
        }
        else if(move != nullptr && move->getSource().hasLocal(baseAddress))
        {
            // index = base-address
            inAreaOffset = INT_ZERO;
        }
        else
            throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled case of in-area offset calculation",
                index.getSingleWriter()->to_string());
    }
    else
    {
        // need to dynamically calculate offset
        if(!inAreaOffset.hasType(ValueType::LOCAL))
            inAreaOffset = method.addNewLocal(TYPE_INT32, "%relative_offset");
        // in-area offset = absolute index - start-address
        it.emplace(new Operation(OP_SUB, inAreaOffset, index, baseAddress->createReference()));
        it.nextInBlock();
    }
    return it;
}

static InstructionWalker mapToMemoryAccessInstructions(Method& method, InstructionWalker it,
    const VPMArea* sourceArea = nullptr, const VPMArea* destArea = nullptr, bool alwaysUseVPM = false)
{
    const MemoryInstruction* mem = it.get<MemoryInstruction>();
    switch(mem->op)
    {
    case MemoryOperation::COPY:
    {
        if(!mem->getNumEntries().isLiteralValue())
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Copying dynamically sized memory is not yet implemented", mem->to_string());
        uint64_t numBytes =
            mem->getNumEntries().getLiteralValue()->unsignedInt() * mem->getSourceElementType().getScalarBitCount() / 8;
        if(numBytes > std::numeric_limits<unsigned>::max())
            throw CompilationError(CompilationStep::OPTIMIZER, "Cannot copy more than 4GB of data", mem->to_string());
        if(sourceArea != nullptr || destArea != nullptr)
        {
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Copying from/to VPM cached data is not yet implemented", mem->to_string());
        }
        it = method.vpm->insertCopyRAM(
            method, it, mem->getDestination(), mem->getSource(), static_cast<unsigned>(numBytes));
        break;
    }
    case MemoryOperation::FILL:
    {
        if(!mem->getNumEntries().isLiteralValue())
            throw CompilationError(CompilationStep::OPTIMIZER,
                "Filling dynamically sized memory is not yet implemented", mem->to_string());
        uint64_t numCopies = mem->getNumEntries().getLiteralValue()->unsignedInt();
        if(numCopies > std::numeric_limits<unsigned>::max())
            throw CompilationError(CompilationStep::OPTIMIZER, "Cannot fill more than 4GB of data", mem->to_string());
        if(destArea != nullptr)
        {
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Filling VPM cached data is not yet implemented", mem->to_string());
        }
        it.emplace(new intermediate::MutexLock(intermediate::MutexAccess::LOCK));
        it.nextInBlock();
        // TODO could optimize (e.g. for zero-initializers) by writing several bytes at once
        method.vpm->insertWriteVPM(method, it, mem->getSource(), nullptr, false);
        it = method.vpm->insertFillRAM(method, it, mem->getDestination(), mem->getSourceElementType(),
            static_cast<unsigned>(numCopies), nullptr, false);
        it.emplace(new intermediate::MutexLock(intermediate::MutexAccess::RELEASE));
        it.nextInBlock();
        break;
    }
    case MemoryOperation::READ:
        if(sourceArea != nullptr)
        {
            Value subIndex = UNDEFINED_VALUE;
            it = calculateInAreaOffset(method, it, mem->getSource().local->getBase(true), mem->getSource(), subIndex);
            it = method.vpm->insertReadVPM(method, it, mem->getDestination(), sourceArea, true, subIndex);
        }
        else if(alwaysUseVPM)
        {
            it = periphery::insertReadDMA(method, it, mem->getDestination(), mem->getSource());
        }
        else
            it = periphery::insertReadVectorFromTMU(method, it, mem->getDestination(), mem->getSource());
        break;
    case MemoryOperation::WRITE:
        if(destArea != nullptr)
        {
            Value subIndex = UNDEFINED_VALUE;
            it = calculateInAreaOffset(
                method, it, mem->getDestination().local->getBase(true), mem->getDestination(), subIndex);
            it = method.vpm->insertWriteVPM(method, it, mem->getSource(), destArea, true, subIndex);
        }
        else
            it = periphery::insertWriteDMA(method, it, mem->getSource(), mem->getDestination());
        break;
    }
    // remove MemoryInstruction
    it.erase();
    return it;
}

/*
 * Memory that is written before it is read needs to be loaded via VPM,
 * since otherwise the caching results in TMU loading the old value.
 */
static bool checkMemoryReadIsAlsoWritten(const Value& address)
{
    // TODO this does not regard the same address value calculated by using different locals!
    if(!address.hasType(ValueType::LOCAL))
        return false;
    for(const auto& pair : address.local->getUsers())
    {
        if(pair.second.readsLocal() && pair.first->writesRegister(REG_VPM_IO))
            return true;
        if(pair.second.writesLocal() && dynamic_cast<const intermediate::MemoryInstruction*>(pair.first) != nullptr)
            return true;
    }

    return false;
}

static void generateStandardMemoryAccessInstructions(
    Method& method, FastSet<InstructionWalker>& memoryInstructions, bool checkOptimizable)
{
    // list of basic blocks where multiple VPM accesses could be combined
    FastSet<BasicBlock*> affectedBlocks;
    auto walkerIt = memoryInstructions.begin();
    while(walkerIt != memoryInstructions.end())
    {
        if(!checkOptimizable ||
            (!walkerIt->get<const MemoryInstruction>()->canMoveSourceIntoVPM() &&
                !walkerIt->get<const MemoryInstruction>()->canMoveDestinationIntoVPM()))
        {
            // cannot be optimized (or check skipped), simply map to TMU/VPM instructions
            InstructionWalker it = *walkerIt;
            if(!checkOptimizable)
                logging::debug() << "Found memory access for which none of the optimization-steps fit: "
                                 << it->to_string() << logging::endl;
            else
                logging::debug() << "Generating memory access which cannot be lowered into VPM: " << it->to_string()
                                 << logging::endl;
            walkerIt = memoryInstructions.erase(walkerIt);
            it = mapToMemoryAccessInstructions(method, it, nullptr, nullptr,
                checkMemoryReadIsAlsoWritten(it.get<const MemoryInstruction>()->getSource()));
            affectedBlocks.emplace(it.getBasicBlock());
        }
        else
            ++walkerIt;
    }

    // combine VPM access for all modified basic blocks
    if(checkOptimizable)
        combineVPMAccess(affectedBlocks, method);
}

static void lowerStackIntoVPM(Method& method, FastSet<InstructionWalker>& memoryInstructions,
    FastMap<const Local*, const VPMArea*>& vpmMappedLocals)
{
    const unsigned stackSize = method.metaData.isWorkGroupSizeSet() ?
        std::accumulate(method.metaData.workGroupSizes.begin(), method.metaData.workGroupSizes.end(), 1u,
            std::multiplies<uint32_t>()) :
        12;
    auto walkerIt = memoryInstructions.begin();
    while(walkerIt != memoryInstructions.end())
    {
        const MemoryInstruction* mem = walkerIt->get<const MemoryInstruction>();
        if(mem->accessesStackAllocation())
        {
            InstructionWalker it = *walkerIt;
            walkerIt = memoryInstructions.erase(walkerIt);

            const VPMArea* sourceArea = nullptr;
            const VPMArea* destArea = nullptr;

            // mapped to VPM by previous operation
            FastMap<const Local*, const VPMArea*>::iterator mappedIt;
            if(mem->getSource().hasType(ValueType::LOCAL) &&
                (mappedIt = vpmMappedLocals.find(mem->getSource().local->getBase(true))) != vpmMappedLocals.end())
            {
                sourceArea = mappedIt->second;
            }
            if(mem->getDestination().hasType(ValueType::LOCAL) &&
                (mappedIt = vpmMappedLocals.find(mem->getDestination().local->getBase(true))) != vpmMappedLocals.end())
            {
                destArea = mappedIt->second;
            }
            if(false) // TODO doesn't heed in-stack-offset
            {
                if(sourceArea == nullptr && mem->getSource().hasType(ValueType::LOCAL) &&
                    mem->getSource().local->getBase(true)->is<StackAllocation>())
                {
                    const Local* src = mem->getSource().local->getBase(true);
                    sourceArea = method.vpm->addArea(src, src->type.getElementType(), true, stackSize);
                    if(sourceArea != nullptr)
                        vpmMappedLocals.emplace(src, sourceArea);
                }
                if(destArea == nullptr && mem->getDestination().hasType(ValueType::LOCAL) &&
                    mem->getDestination().local->getBase(true)->is<StackAllocation>())
                {
                    const Local* dest = mem->getDestination().local->getBase(true);
                    destArea = method.vpm->addArea(dest, dest->type.getElementType(), true, stackSize);
                    if(sourceArea != nullptr)
                        vpmMappedLocals.emplace(dest, destArea);
                }
                if(sourceArea != nullptr)
                    logging::debug() << "Lowering stack-allocated data '" << sourceArea->originalAddress->to_string()
                                     << "' into VPM" << logging::endl;
                if(destArea != nullptr)
                    logging::debug() << "Lowering stack-allocated data '" << destArea->originalAddress->to_string()
                                     << "' into VPM" << logging::endl;
                if(sourceArea != nullptr || destArea != nullptr)
                    logging::debug() << "Optimizing access to stack allocated data by using the VPM as cache: "
                                     << mem->to_string() << logging::endl;
            }
            // automatically handles both cases (lowered to VPM, not lowered to VPM)
            // for copying stack <-> local, we assign an VPM area to the stack-object here, but delay the copying until
            // the locals are lowered to be able to lower into copy VPM <-> VPM
            if(mem->op != MemoryOperation::COPY || !mem->accessesLocalMemory())
            {
                mapToMemoryAccessInstructions(method, it, sourceArea, destArea);
                // TODO handle offset for selecting the correct stack-frame in VPM!
            }
        }
        else
            ++walkerIt;
    }
}

static void lowerLocalDataIntoVPM(Method& method, FastSet<InstructionWalker>& memoryInstructions,
    FastMap<const Local*, const VPMArea*>& vpmMappedLocals)
{
    // TODO errors when local memory is also accessed via DMA intrinsic??
    auto walkerIt = memoryInstructions.begin();
    while(walkerIt != memoryInstructions.end())
    {
        const MemoryInstruction* mem = walkerIt->get<const MemoryInstruction>();
        if(mem->accessesLocalMemory())
        {
            InstructionWalker it = *walkerIt;
            walkerIt = memoryInstructions.erase(walkerIt);

            const VPMArea* sourceArea = nullptr;
            const VPMArea* destArea = nullptr;
            // mapped to VPM by previous operation
            FastMap<const Local*, const VPMArea*>::iterator mappedIt;
            if(mem->getSource().hasType(ValueType::LOCAL) &&
                (mappedIt = vpmMappedLocals.find(mem->getSource().local->getBase(true))) != vpmMappedLocals.end())
            {
                sourceArea = mappedIt->second;
            }
            if(mem->getDestination().hasType(ValueType::LOCAL) &&
                (mappedIt = vpmMappedLocals.find(mem->getDestination().local->getBase(true))) != vpmMappedLocals.end())
            {
                destArea = mappedIt->second;
            }
            if(sourceArea == nullptr && mem->getSource().hasType(ValueType::LOCAL) &&
                mem->getSource().local->getBase(true)->is<Global>() &&
                mem->getSource().local->getBase(true)->type.getPointerType().value()->addressSpace ==
                    AddressSpace::LOCAL)
            {
                const Local* src = mem->getSource().local->getBase(true);
                sourceArea = method.vpm->addArea(src, src->type.getElementType(), false);
                if(sourceArea != nullptr)
                    vpmMappedLocals.emplace(src, sourceArea);
            }
            if(destArea == nullptr && mem->getDestination().hasType(ValueType::LOCAL) &&
                mem->getDestination().local->getBase(true)->is<Global>() &&
                mem->getDestination().local->getBase(true)->type.getPointerType().value()->addressSpace ==
                    AddressSpace::LOCAL)
            {
                const Local* dest = mem->getDestination().local->getBase(true);
                destArea = method.vpm->addArea(dest, dest->type.getElementType(), false);
                if(destArea != nullptr)
                    vpmMappedLocals.emplace(dest, destArea);
            }
            if(sourceArea != nullptr)
                logging::debug() << "Lowering local '" << sourceArea->originalAddress->to_string() << "' into VPM"
                                 << logging::endl;
            if(destArea != nullptr)
                logging::debug() << "Lowering local '" << destArea->originalAddress->to_string() << "' into VPM"
                                 << logging::endl;
            if(sourceArea != nullptr || destArea != nullptr)
                logging::debug() << "Optimizing access to local allocated data by using the VPM as cache: "
                                 << mem->to_string() << logging::endl;
            // automatically handles both cases (lowered to VPM, not lowered to VPM)
            mapToMemoryAccessInstructions(method, it, sourceArea, destArea);
        }
        else
            ++walkerIt;
    }
}

static Optional<Value> getConstantValue(const MemoryInstruction* mem)
{
    // can only read from constant global data, so the global is always the source
    const Global* global = mem->getSource().local->getBase(true)->as<Global>();
    if(mem->getSource().local->reference.second >= 0 && global->value.hasType(ValueType::CONTAINER))
        // fixed index
        return global->value.container.elements.at(mem->getSource().local->reference.second);
    else if(global->value.isLiteralValue())
        // scalar value
        return global->value;
    else if(global->value.isZeroInitializer())
        // all entries are the same
        return Value::createZeroInitializer(global->value.type.getElementType());
    else if(global->value.isUndefined())
        // all entries are undefined
        return Value(global->value.type.getElementType());
    return NO_VALUE;
}

static void lowerGlobalsToConstants(Method& method, FastSet<InstructionWalker>& memoryInstructions)
{
    /*
     * Tries to eliminate reading of constant global values from memory by replacing them with the load of the global's
     * content value.
     *
     * The following conditions need to be fulfilled for this optimization to apply:
     * - the source needs to be a Global with the constant-flag set
     * - if the global is a compound value (e.g. not read completely), the index needs a literal value or the global
     * needs to be uniform (e.g. all zeroes)
     *
     * Any other access to globals is simply mapped to the corresponding TMU/VPM operations
     */
    auto walkerIt = memoryInstructions.begin();
    while(walkerIt != memoryInstructions.end())
    {
        const MemoryInstruction* mem = walkerIt->get<const MemoryInstruction>();
        if(mem->accessesConstantGlobal())
        {
            logging::debug() << "Found reference to constant global: " << mem->to_string() << " ("
                             << mem->getSource().local->getBase(true)->to_string(true) << ")" << logging::endl;

            InstructionWalker it = *walkerIt;
            walkerIt = memoryInstructions.erase(walkerIt);

            // 1. find constant value read from the Global
            Optional<Value> content = getConstantValue(mem);
            if(content)
            {
                it.reset(new MoveOperation(mem->getOutput().value(), content.value()));
                logging::debug() << "Replaced loading of constant memory with constant literal: " << it->to_string()
                                 << logging::endl;
            }
            else
                // generate simply access to global data residing in memory
                mapToMemoryAccessInstructions(method, it);
        }
        else
            ++walkerIt;
    }
}

void normalization::mapMemoryAccess(const Module& module, Method& method, const Configuration& config)
{
    /*
     * Goals:
     * 1. map all memory-accesses correctly, so all QPUs accessing the same memory see the same/expected values
     * 2. lower/lift as many memory-accesses as possible into VPM to save unnecessary instructions accessing QPU/RAM
     *
     * Steps:
     * 1. map all "standard" memory accesses
     *  - map memory accesses which cannot be optimized into VPM to TMU/VPM instructions
     *  - combine successive accesses (use/replace #combineVPMAccess)
     *  -> determine VPM scratch size
     * 2. try to lower all stack-allocations into VPM
     *  - reserve enough space (for 12 stacks unless specified in compile-time work-group size!)
     *  - map accesses to stack to use the correct area in VPM
     *  - calculate offsets in VPM for every stack entry (see/replace #resolveStackAllocations)
     *  - rewrite indices to match offsets in VPM
     *  - generate TMU/VPM instructions for stack-allocations not fitting into VPM
     *  - update total stack-size (the buffer-size required to be reserved) for the method
     * 3. try to lower local memory into VPM
     *  - check sizes, reserve VPM area
     *  - map accesses using correct VPM area
     *  - rewrite indices to match offsets in VPM
     *  - generate TMU/VPM instructions for local memory areas not fitting into VPM
     *  - update globals, remove local memory areas located in VPM from buffer-size to be reserved
     * 4. try to lower constant globals into constant values
     *  - update globals, remove global memory for which all usages are rewritten from buffer-size to be reserved
     *  - generate TMU/VPM instructions for other globals
     *  - rewrite indices to remaining globals (see #accessGlobalData)
     * 5. map all remaining memory access to default TMU/VPM access
     */

    // contains all the positions of MemoryInstructions for easier/faster iteration in the next steps
    FastSet<InstructionWalker> memoryInstructions;
    {
        InstructionWalker it = method.walkAllInstructions();
        while(!it.isEndOfMethod())
        {
            if(it.has<MemoryInstruction>())
            {
                auto memInst = it.get<MemoryInstruction>();
                if(memInst->op == MemoryOperation::READ)
                {
                    auto nextIt = it.copy().nextInBlock();
                    auto nextMemInst = nextIt.get<MemoryInstruction>();
                    if(!nextIt.isEndOfBlock() && nextMemInst != nullptr && nextMemInst->op == MemoryOperation::WRITE &&
                        nextMemInst->getSource() == memInst->getDestination() &&
                        nextMemInst->getNumEntries() == memInst->getNumEntries())
                    {
                        logging::debug()
                            << "Found reading of memory where the sole usage writes the value back into memory: "
                            << memInst->to_string() << logging::endl;
                        logging::debug() << "Replacing manual copy of memory with memory copy instruction for write: "
                                         << nextMemInst->to_string() << logging::endl;

                        const Value src = memInst->getSource();
                        it.erase();
                        nextIt.reset(new MemoryInstruction(
                            MemoryOperation::COPY, nextMemInst->getDestination(), src, nextMemInst->getNumEntries()));
                        it = nextIt;
                    }
                }
                memoryInstructions.emplace(it);
            }
            it.nextInMethod();
        }
    }

    // stores already assigned VPM areas, e.g. for memory-copy operations
    FastMap<const Local*, const VPMArea*> vpmMappedLocals;

    // Step 1
    generateStandardMemoryAccessInstructions(method, memoryInstructions, true);
    // Step 2
    lowerStackIntoVPM(method, memoryInstructions, vpmMappedLocals);
    // Step 3
    lowerLocalDataIntoVPM(method, memoryInstructions, vpmMappedLocals);
    // Step 4
    lowerGlobalsToConstants(method, memoryInstructions);
    // Step 5
    // since we use at most 16 ints of scratch here (we don't combine anymore), the scratch-area is always large enough
    generateStandardMemoryAccessInstructions(method, memoryInstructions, false);

    // TODO move calculation of stack/global indices in here too?
}
