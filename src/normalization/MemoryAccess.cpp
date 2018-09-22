/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryAccess.h"

#include "../Expression.h"
#include "../InstructionWalker.h"
#include "../Module.h"
#include "../Profiler.h"
#include "../analysis/ValueRange.h"
#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <numeric>

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;
using namespace vc4c::periphery;

/**
 * TODO fix issues:
 * - invalid local type for memory: NVIDIA/MedianFilter.cl, HandBrake/yaif_filter.cl, rodinia/lud_kernel.cl, ... (e.g.
 * select between stack allocations)
 * - too complex phi-nodes with pointers: clNN/im2col.cl
 */

struct BaseAndOffset
{
    Optional<Value> base;
    Optional<int32_t> offset;

    explicit BaseAndOffset() : base(NO_VALUE), offset{} {}

    BaseAndOffset(const Optional<Value>& base, Optional<int32_t> offset) : base(base), offset(offset) {}
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
            return BaseAndOffset(NO_VALUE, offset->getLiteralValue()->signedInt());
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
    // 2. an addition with a local and a literal -> the local is the base, the literal the offset
    if(dynamic_cast<const Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
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

    // 3. an addition with two locals -> one is the base, the other the calculation of the literal
    if(dynamic_cast<const Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
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
    /*
        if(writers.size() == 1)
        {
            // couldn't find literal offset for any direct base, try with arbitrary values
            ref = val.local->getBase(true);
            Optional<Value> offset = NO_VALUE;
            for(const auto& arg : (*writers.begin())->getArguments())
            {
                if(ref != nullptr && arg.hasType(ValueType::LOCAL) && arg.local->getBase(false) == ref)
                    // skip finding the same base again
                    continue;
                auto tmp = findBaseAndOffset(arg);
                if(tmp.base && tmp.base->local->getBase(true) == ref && tmp.offset.is(0))
                    // this parameter is the base itself, is already handled
                    continue;
                // TODO how to combine the offsets?
                // TODO also need to handle non-addition of offsets (e.g. ptr = base + (offset + size * i))
                logging::debug() << "Found offset of " << tmp.base.to_string() << " + "
                                 << (tmp.offset ? tmp.offset.value() : -1) << logging::endl;
                logging::debug() << "Found offset of " << tmp.base.to_string() << " with expression: "
                                 << vc4c::Expression::createExpression(*(*writers.begin())).to_string() <<
       logging::endl;
            }
            // TODO why is this called twice? The whole function, from outside
            logging::debug() << "Found base and non-literal offset: " << ref->to_string() << " - " << offset.to_string()
                             << logging::endl;
            if(ref && (ref->residesInMemory() || (ref->is<Parameter>() && ref->type.isPointerType())))
                return BaseAndOffset(ref->createReference(), {});
        }
    */

    return BaseAndOffset();
}

struct VPMAccessGroup
{
    bool isVPMWrite;
    DataType groupType = TYPE_UNKNOWN;
    RandomAccessList<InstructionWalker> dmaSetups;
    RandomAccessList<InstructionWalker> genericSetups;
    RandomAccessList<InstructionWalker> addressWrites;
    // this is the distance/offset (start of row to start of row, 1 = consecutive) between two vectors in number of
    // vectors that would fit in between
    int stride = 1;

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
    // the number of elements between two entries in memory
    group.stride = 0;
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

        if(!(it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            // for simplicity, we only check for VPM addresses and find all other instructions relative to it
            continue;
        if(!it.has<MoveOperation>())
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Setting VPM address with non-move is not supported", it->to_string());
        const auto baseAndOffset = findBaseAndOffset(it.get<MoveOperation>()->getSource());
        const bool isVPMWrite = it->writesRegister(REG_VPM_DMA_STORE_ADDR);
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
            if(group.addressWrites.size() == 1 && baseAndOffset.offset && group.stride == 0)
            {
                // special case for first offset - use it to determine stride
                group.stride = baseAndOffset.offset.value() -
                    findBaseAndOffset(group.addressWrites[0].get<MoveOperation>()->getSource()).offset.value();
                logging::debug() << "Using a stride of " << group.stride
                                 << " elements between consecutive access to memory" << logging::endl;
            }
            if(!baseAndOffset.offset || baseAndOffset.offset.value() != (nextOffset * group.stride))
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
        nextOffset = (baseAndOffset.offset.value_or(-1) / (group.stride == 0 ? 1 : group.stride)) + 1;

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
    // 1.1 Update stride setup to the stride between rows
    {
        LoadImmediate* strideSetup = group.dmaSetups.at(0).copy().nextInBlock().get<LoadImmediate>();
        if(strideSetup == nullptr || !strideSetup->writesRegister(REG_VPM_OUT_SETUP) ||
            !VPWSetup::fromLiteral(strideSetup->getImmediate().unsignedInt()).isStrideSetup())
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPW DMA stride setup for DMA setup",
                group.dmaSetups.at(0)->to_string());
        VPWSetupWrapper strideSetupValue(strideSetup);
        // stride is the distance in bytes from end of v1 to start of v2
        strideSetupValue.strideSetup.setStride(
            static_cast<uint16_t>(static_cast<unsigned>(group.stride == 0 ? 0 : group.stride - 1) *
                group.groupType.getElementType().getPhysicalWidth()));
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
               group.isVPMWrite ? REG_VPM_DMA_STORE_WAIT : REG_VPM_DMA_LOAD_WAIT))
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

    // 1.2 Update stride setup for the stride used
    {
        LoadImmediate* strideSetup = group.dmaSetups.at(0).copy().nextInBlock().get<LoadImmediate>();
        if(strideSetup == nullptr || !strideSetup->writesRegister(REG_VPM_IN_SETUP) ||
            !VPRSetup::fromLiteral(strideSetup->getImmediate().unsignedInt()).isStrideSetup())
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPR DMA stride setup for DMA setup",
                group.dmaSetups.at(0)->to_string());
        VPRSetupWrapper strideSetupValue(strideSetup);
        // in contrast to writing memory, the pitch is the distance from start to start of successive rows
        strideSetupValue.strideSetup.setPitch(static_cast<uint16_t>(
            static_cast<unsigned>(group.stride) * group.groupType.getElementType().getPhysicalWidth()));
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
               group.isVPMWrite ? REG_VPM_DMA_STORE_WAIT : REG_VPM_DMA_LOAD_WAIT))
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
    /* TODO rewrite
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
    */

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
                it = it.erase();
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

enum class MemoryType
{
    // lower the value into a register and replace all loads with moves
    QPU_REGISTER_READONLY,
    // lower the value into a register and replace all loads/stores with moves
    QPU_REGISTER_READWRITE,
    // store in VPM in extra space per QPU!!
    VPM_PER_QPU,
    // store in VPM, QPUs share access to common data
    VPM_SHARED_ACCESS,
    // keep in RAM/global data segment, read via TMU
    RAM_LOAD_TMU,
    // keep in RAM/global data segment, access via VPM
    RAM_READ_WRITE_VPM
};

/*
 * Converts an address (e.g. an index chain) and the corresponding base pointer to the pointer difference
 *
 * NOTE: The result itself is still in "memory-address mode", meaning the offset is the number of bytes
 *
 * Returns (char*)address - (char*)baseAddress
 */
static InstructionWalker insertAddressToOffset(
    InstructionWalker it, Method& method, Value& out, const Local* baseAddress, const MemoryInstruction* mem)
{
    auto ptrVal = mem->op == MemoryOperation::READ ? mem->getSource() : mem->getDestination();
    auto indexOp = dynamic_cast<const Operation*>(ptrVal.getSingleWriter());
    if(!indexOp)
    {
        // for stores, the store itself is also a write instruction
        auto writers = ptrVal.local->getUsers(LocalUse::Type::WRITER);
        if(writers.size() == 2 && writers.find(mem) != writers.end())
        {
            writers.erase(mem);
            indexOp = dynamic_cast<const Operation*>(*writers.begin());
        }
    }
    if(ptrVal.hasLocal(baseAddress))
    {
        // trivial case, the offset is zero
        out = INT_ZERO;
    }
    else if(indexOp && indexOp->readsLocal(baseAddress) && indexOp->op == OP_ADD)
    {
        // for simple version where the index is base address + offset, simple use the offset directly
        out = indexOp->getFirstArg().hasLocal(baseAddress) ? indexOp->getSecondArg().value() : indexOp->getFirstArg();
    }
    else
    {
        // for more complex versions, calculate offset by subtracting base address from result
        // address
        out = method.addNewLocal(baseAddress->type, "%pointer_diff");
        it.emplace(new Operation(OP_SUB, out, ptrVal, baseAddress->createReference()));
        it.nextInBlock();
    }
    return it;
}

/*
 * Converts an address (e.g. an index-chain) and a base-address to the offset of the vector denoting the element
 * accessed by the index-chain. In addition to #insertAddressToOffset, this function also handles multiple stack-frames.
 *
 * NOTE: The result is still the offset in bytes, since VPM#insertReadVPM and VPM#insertWriteVPM take the offset in
 * bytes!
 *
 * Returns ((char*)address - (char*)baseAddress) + (typeSizeInBytes * stackIndex), where stackIndex is always zero (and
 * therefore the second part omitted) for shared memory
 */
static InstructionWalker insertAddressToStackOffset(InstructionWalker it, Method& method, Value& out,
    const Local* baseAddress, MemoryType type, const MemoryInstruction* mem)
{
    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToOffset(it, method, tmpIndex, baseAddress, mem);
    if(type == MemoryType::VPM_PER_QPU)
    {
        // size of one stack-frame in bytes
        auto stackByteSize = periphery::VPM::getVPMStorageType(baseAddress->type.getElementType()).getPhysicalWidth();
        // add offset of stack-frame
        Value stackOffset = method.addNewLocal(TYPE_VOID.toPointerType(), "%stack_offset");
        Value tmp = method.addNewLocal(baseAddress->type);
        it = insertOperation(OP_MUL24, it, method, stackOffset, Value(Literal(stackByteSize), TYPE_INT16),
            Value(REG_QPU_NUMBER, TYPE_INT8));
        it = insertOperation(OP_ADD, it, method, out, tmpIndex, stackOffset);
    }
    else
    {
        out = tmpIndex;
    }
    return it;
}

/*
 * Converts an address (e.g. index-chain) and the corresponding base-address to the element offset for an element of the
 * type used in the container
 *
 * Return ((char*)address - (char*)baseAddress) / sizeof(elementType)
 */
static InstructionWalker insertAddressToElementOffset(InstructionWalker it, Method& method, Value& out,
    const Local* baseAddress, const Value& container, const MemoryInstruction* mem)
{
    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToOffset(it, method, tmpIndex, baseAddress, mem);
    // the index (as per index calculation) is in bytes, but we need index in elements, so divide by element size
    auto offset = static_cast<int32_t>(std::log2(container.type.getElementType().getPhysicalWidth()));
    it = insertOperation(OP_SHR, it, method, out, tmpIndex, Value(Literal(offset), TYPE_INT8));
    return it;
}

/*
 * Maps a memory access instruction to an instruction accessing RAM through VPM.
 *
 * NOTE: At least one of the operands of the instruction to be mapped must be located in RAM
 * NOTE: this is the least optimal mapping possible and should avoided if possible.
 */
static InstructionWalker mapToVPMMemoryAccessInstructions(
    Method& method, InstructionWalker it, const VPMArea* sourceArea = nullptr, const VPMArea* destArea = nullptr)
{
    if(sourceArea != nullptr && destArea != nullptr)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Memory access with both operands located in VPM should have been handled already", it->to_string());
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
            // TODO heed stack offset for VPM mapped stack elements!
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Copying from/to VPM cached data is not yet implemented", mem->to_string());
        }
        it = method.vpm->insertCopyRAM(
            method, it, mem->getDestination(), mem->getSource(), static_cast<unsigned>(numBytes));

        auto* src = mem->getSource().hasType(ValueType::LOCAL) ? mem->getSource().local->getBase(true) : nullptr;
        if(src && src->is<Parameter>())
            const_cast<Parameter*>(src->as<const Parameter>())->decorations =
                add_flag(src->as<const Parameter>()->decorations, ParameterDecorations::INPUT);
        auto* dest =
            mem->getDestination().hasType(ValueType::LOCAL) ? mem->getDestination().local->getBase(true) : nullptr;
        if(dest && dest->is<Parameter>())
            const_cast<Parameter*>(dest->as<const Parameter>())->decorations =
                add_flag(dest->as<const Parameter>()->decorations, ParameterDecorations::OUTPUT);
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
            // TODO should this also be handled already?
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
        auto* dest =
            mem->getDestination().hasType(ValueType::LOCAL) ? mem->getDestination().local->getBase(true) : nullptr;
        if(dest && dest->is<Parameter>())
            const_cast<Parameter*>(dest->as<const Parameter>())->decorations =
                add_flag(dest->as<const Parameter>()->decorations, ParameterDecorations::OUTPUT);
        break;
    }
    case MemoryOperation::READ:
    {
        if(sourceArea != nullptr)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Reading from VPM mapped memory should already be handled", mem->to_string());
        it = periphery::insertReadDMA(method, it, mem->getDestination(), mem->getSource());
        auto* src = mem->getSource().hasType(ValueType::LOCAL) ? mem->getSource().local->getBase(true) : nullptr;
        if(src && src->is<Parameter>())
            const_cast<Parameter*>(src->as<const Parameter>())->decorations =
                add_flag(src->as<const Parameter>()->decorations, ParameterDecorations::INPUT);
        break;
    }
    case MemoryOperation::WRITE:
    {
        if(destArea != nullptr)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Writing into VPM mapped memory should already be handled", mem->to_string());
        it = periphery::insertWriteDMA(method, it, mem->getSource(), mem->getDestination());
        auto* dest =
            mem->getDestination().hasType(ValueType::LOCAL) ? mem->getDestination().local->getBase(true) : nullptr;
        if(dest && dest->is<Parameter>())
            const_cast<Parameter*>(dest->as<const Parameter>())->decorations =
                add_flag(dest->as<const Parameter>()->decorations, ParameterDecorations::OUTPUT);
        break;
    }
    }
    // remove MemoryInstruction
    // since a copy may have another iterator to it, do not remove the element, just clear it
    // the empty instruction is cleaned up in #combineVPMAccess
    return mem->op == MemoryOperation::COPY ? (it.reset(nullptr), it) : it.erase();
}

static bool isMemoryOnlyRead(const Local* local)
{
    auto base = local->getBase(true);
    if(base->is<Parameter>() && has_flag(base->as<const Parameter>()->decorations, ParameterDecorations::READ_ONLY))
        return true;
    if(base->is<Global>() && base->as<Global>()->isConstant)
        return true;
    if(base->type.getPointerType() && base->type.getPointerType().value()->addressSpace == AddressSpace::CONSTANT)
        return true;

    // TODO also check for no actual writes. Need to heed index-calculation from base!
    return false;
}

/*
 * Maps all memory access instructions for the given memory location by placing it in RAM and accessing it via VPM
 *
 * NOTE: This is the least optimal way of accessing memory and should be avoided if possible
 */
static bool mapReadWriteToMemoryViaVPM(Method& method, const Local* local,
    FastSet<InstructionWalker>& memoryInstructions, FastMap<const Local*, const VPMArea*>& vpmMappedLocals,
    FastSet<BasicBlock*>& affectedBlocks)
{
    for(auto it : memoryInstructions)
    {
        auto mem = it.get<const MemoryInstruction>();
        if(!mem)
            // already optimized (e.g. lowered into VPM)
            continue;
        logging::debug() << "Generating memory access which cannot be optimized: " << mem->to_string() << logging::endl;
        auto srcVpmArea = mem->getSource().hasType(ValueType::LOCAL) ?
            vpmMappedLocals[mem->getSource().local->getBase(true)] :
            nullptr;
        auto dstVpmArea = mem->getDestination().hasType(ValueType::LOCAL) ?
            vpmMappedLocals[mem->getDestination().local->getBase(true)] :
            nullptr;
        it = mapToVPMMemoryAccessInstructions(method, it, srcVpmArea, dstVpmArea);
        affectedBlocks.emplace(it.getBasicBlock());
    }

    // everything else throws errors
    return true;
}

/*
 * Returns the constant value which will be read from the given memory access instruction.
 *
 * The value is constant if:
 * - the source memory location is constant
 * - the index is constant or the value can be determined without knowing the exact index (e.g. all elements are the
 * same)
 */
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
    else if(global->value.hasType(ValueType::CONTAINER) && global->value.container.isElementNumber())
        return ELEMENT_NUMBER_REGISTER;
    else if(global->value.hasType(ValueType::CONTAINER) && global->value.container.isAllSame())
        // all entries are the same
        return global->value.container.elements.at(0);
    return NO_VALUE;
}

/*
 * Tries to convert the array-type pointed to by the given local to a vector-type to fit into a single register.
 *
 * For this conversion to succeed, the array-element type must be a scalar of bit-width <= 32-bit and the size of the
 * array known to be less or equals to 16.
 */
static Optional<DataType> convertSmallArrayToRegister(const Local* local)
{
    const Local* base = local->getBase(true);
    if(base->type.getPointerType())
    {
        const auto& baseType = base->type.getPointerType().value()->elementType;
        auto arrayType = baseType.getArrayType();
        if(arrayType && arrayType.value()->size <= NATIVE_VECTOR_SIZE && arrayType.value()->elementType.isScalarType())
            return arrayType.value()->elementType.toVectorType(static_cast<uint8_t>(arrayType.value()->size));
    }
    return {};
}

/*
 * Maps memory access to the given local into moves from/to the given register
 *
 * NOTE: This is the best optimization for memory access and should always be preferred.
 * NOTE: This optimization cannot be applied if changes made to the lowered register need to be reflected to other QPUs.
 */
static InstructionWalker lowerReadWriteOfMemoryToRegister(InstructionWalker it, Method& method, const Local* local,
    const Value& loweredRegister, const MemoryInstruction* mem)
{
    Value tmpIndex = UNDEFINED_VALUE;
    it = insertAddressToElementOffset(it, method, tmpIndex, local, loweredRegister, mem);
    if(mem->op == MemoryOperation::READ)
    {
        // TODO check whether index is guaranteed to be in range [0, 16[
        it = insertVectorExtraction(it, method, loweredRegister, tmpIndex, mem->getDestination());
        return it.erase();
    }
    if(mem->op == MemoryOperation::WRITE)
    {
        // TODO need special handling for inserting multiple elements to set all new elements
        it = insertVectorInsertion(it, method, loweredRegister, tmpIndex, mem->getSource());
        return it.erase();
    }
    throw CompilationError(
        CompilationStep::NORMALIZER, "Unhandled case of lowering memory access to register", mem->to_string());
}

/*
 * Lowers access to a memory location into a register.
 *
 * This can be done for constant or private (stack) memory locations.
 *
 * NOTE: This is the best optimization for memory access and should be preferred, where applicable.
 */
static bool lowerMemoryToRegister(
    Method& method, const Local* local, MemoryType type, FastSet<InstructionWalker>& memoryInstructions)
{
    /*
     * There are several cases of memory lowered into registers:
     * - constant memory with constant index (direct value determinable) -> map to direct value
     * - constant memory which fits into register but dynamic index -> map to register, index by vector rotation
     * - private memory which fits into register -> map to register
     * - private memory where the type can be converted to fit into register -> map to register + index by vector
     * rotation
     */
    auto toConvertedRegisterType = convertSmallArrayToRegister(local);
    if(type == MemoryType::QPU_REGISTER_READONLY)
    {
        // can handle extra read on its own, no check required for other accesses
        auto it = memoryInstructions.begin();
        while(it != memoryInstructions.end())
        {
            const MemoryInstruction* mem = it->get<const MemoryInstruction>();
            if(!mem)
            {
                // already converted (cannot happen, since this is the first round, but we don't care)
                ++it;
                continue;
            }
            if(mem->op != MemoryOperation::READ && mem->op != MemoryOperation::COPY)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Cannot perform a non-read operation on constant memory", mem->to_string());
            logging::debug() << "Trying to lower access to constant memory into register: " << mem->to_string()
                             << logging::endl;
            auto constantValue = getConstantValue(mem);
            auto tmpIt = *it;
            if(constantValue)
            {
                it = memoryInstructions.erase(it);
                if(mem->op == MemoryOperation::COPY)
                {
                    // since a copy always involves another memory object, this rewrite is picked up when the other
                    // object is processed
                    tmpIt.reset(
                        new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), constantValue.value()));
                    logging::debug() << "Replaced memory copy from constant memory to memory write of constant value: "
                                     << tmpIt->to_string() << logging::endl;
                }
                else
                {
                    tmpIt.reset(new MoveOperation(mem->getOutput().value(), constantValue.value()));
                    logging::debug() << "Replaced loading of constant memory with constant literal: "
                                     << tmpIt->to_string() << logging::endl;
                }
            }
            else if(mem->op == MemoryOperation::READ && local->is<Global>() && toConvertedRegisterType)
            {
                it = memoryInstructions.erase(it);
                auto tmp = method.addNewLocal(toConvertedRegisterType.value(), "%lowered_constant");

                tmpIt.emplace(new MoveOperation(tmp, local->as<const Global>()->value));
                tmpIt.nextInBlock();
                tmpIt = lowerReadWriteOfMemoryToRegister(tmpIt, method, local, tmp, mem);
                logging::debug() << "Replaced loading of constant memory with vector rotation of register: "
                                 << tmpIt.copy().previousInBlock()->to_string() << logging::endl;
            }
            else
            {
                // this can happen e.g. for memory copy
                logging::debug() << "Failed to lower access to constant memory into register: " << mem->to_string()
                                 << logging::endl;
                ++it;
            }
        }
        return memoryInstructions.empty();
    }
    else if(type == MemoryType::QPU_REGISTER_READWRITE && local->is<StackAllocation>())
    {
        // need to heed all access to memory area
        if(local->type.isSimpleType())
        {
            // fits into a single register on its own, without rewriting
            const Value loweredRegister = method.addNewLocal(local->type);
            for(auto it : memoryInstructions)
            {
                const MemoryInstruction* mem = it.get<const MemoryInstruction>();
                if(!mem)
                    // instruction cannot be already converted here (either all are already converted or none)
                    throw CompilationError(CompilationStep::NORMALIZER,
                        "Invalid instruction to be lowered into register", it->to_string());
                logging::debug() << "Trying to lower access to stack allocation into register: " << mem->to_string()
                                 << logging::endl;
                bool isRead =
                    mem->getSource().hasType(ValueType::LOCAL) && mem->getSource().local->getBase(true) == local;
                bool isWritten = mem->getDestination().hasType(ValueType::LOCAL) &&
                    mem->getDestination().local->getBase(true) == local;
                switch(mem->op)
                {
                case MemoryOperation::COPY:
                    if(isRead)
                        // since a copy always involves another memory object, this rewrite is picked up when the other
                        // object is processed
                        it.reset(new MemoryInstruction(MemoryOperation::WRITE, mem->getDestination(), loweredRegister));
                    else if(isWritten)
                        it.reset(new MemoryInstruction(MemoryOperation::READ, loweredRegister, mem->getSource()));
                    break;
                case MemoryOperation::FILL:
                    if(mem->getSource().type.isScalarType())
                    {
                        it = insertReplication(it, mem->getSource(), loweredRegister);
                        it.erase();
                    }
                    else
                        it.reset(new MoveOperation(loweredRegister, mem->getSource()));
                    break;
                case MemoryOperation::READ:
                    it.reset(new MoveOperation(mem->getDestination(), loweredRegister));
                    break;
                case MemoryOperation::WRITE:
                    it.reset(new MoveOperation(loweredRegister, mem->getSource()));
                    break;
                }
                logging::debug() << "Replaced access to stack allocation '" << local->to_string()
                                 << "' with: " << it->to_string() << logging::endl;
            }
            // the stack value always fits into a single register (is checked above) and therefore the lowering always
            // succeeds
            return true;
        }
        else if(toConvertedRegisterType)
        {
            if(std::any_of(memoryInstructions.begin(), memoryInstructions.end(), [](InstructionWalker it) -> bool {
                   const MemoryInstruction* mem = it.get<const MemoryInstruction>();
                   return mem->op == MemoryOperation::FILL || mem->op == MemoryOperation::COPY;
               }))
            {
                // not supported, keep all access to use VPM/RAM
                logging::debug()
                    << "Lowering of memory which is filled or copied into registers is not yet implemented: "
                    << local->to_string() << logging::endl;
                return false;
            }
            Value loweredBuffer = method.addNewLocal(toConvertedRegisterType.value(), "%lowered_stack");
            for(auto it : memoryInstructions)
            {
                const MemoryInstruction* mem = it.get<const MemoryInstruction>();
                if(!mem)
                    // instruction cannot be already converted here(either all are already converted or none)
                    throw CompilationError(CompilationStep::NORMALIZER,
                        "Invalid instruction to be lowered into register", it->to_string());
                logging::debug() << "Trying to lower access to stack allocation into register: " << mem->to_string()
                                 << logging::endl;
                it = lowerReadWriteOfMemoryToRegister(it, method, local, loweredBuffer, mem);
                logging::debug() << "Replaced access to stack allocation '" << local->to_string()
                                 << "' with: " << it.copy().previousInBlock()->to_string() << logging::endl;
            }
            // all reads and writes (with any index) can be lowered into register, if the type fits
            return true;
        }
        else
            throw CompilationError(CompilationStep::NORMALIZER,
                "Unhandled case of lowering stack allocation to register", local->to_string());
    }
    else
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of lowering to register", local->to_string());
    return false;
}

/*
 * Maps a single memory read to a TMU load
 *
 * NOTE: Memory locations loaded via TMU MUST NOT be written to by the same kernel (even on a different QPU)!
 */
static bool mapReadsToTMULoad(
    Method& method, const Local* local, FastSet<InstructionWalker>& memoryInstructions, bool tmuFlag)
{
    auto it = memoryInstructions.begin();
    while(it != memoryInstructions.end())
    {
        const MemoryInstruction* mem = it->get<const MemoryInstruction>();
        if(!mem)
            // already converted (e.g. when constant load lowered into register)
            continue;
        logging::debug() << "Trying to map load from read-only memory to TMU load: " << mem->to_string()
                         << logging::endl;
        if(mem->op != MemoryOperation::READ)
        {
            ++it;
            continue;
        }
        auto tmpIt = periphery::insertReadVectorFromTMU(
            method, *it, mem->getDestination(), mem->getSource(), tmuFlag ? periphery::TMU1 : periphery::TMU0);
        it = memoryInstructions.erase(it);
        tmpIt.erase();
        logging::debug() << "Replaced loading from read-only memory with TMU load: "
                         << tmpIt.copy().previousInBlock()->to_string() << logging::endl;
    }

    return memoryInstructions.empty();
}

/*
 * Tries to map the given memory location into VPM
 *
 * This is applicable for private (stack) or local memory.
 *
 * NOTE: A memory location can only be lowered into VPM if all access to it can be lowered to VPM
 * NOTE: This is to be preferred over keeping the memory location in RAM
 */
static bool lowerMemoryToVPM(Method& method, const Local* local, MemoryType type,
    FastSet<InstructionWalker>& memoryInstructions, FastMap<const Local*, const VPMArea*>& vpmAreas)
{
    // Need to make sure addressing is still correct!
    if(type == MemoryType::VPM_PER_QPU && !local->is<StackAllocation>())
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled case of per-QPU memory buffer", local->to_string());

    // since the stack allocation is read-write, need to lower all access or none
    auto vpmArea = method.vpm->addArea(
        local, local->type.getElementType(), type == MemoryType::VPM_PER_QPU, method.metaData.getWorkGroupSize());
    if(vpmArea == nullptr)
        // did not fit into VPM
        return false;
    vpmAreas.emplace(local, vpmArea);

    auto it = memoryInstructions.begin();
    while(it != memoryInstructions.end())
    {
        const MemoryInstruction* mem = it->get<const MemoryInstruction>();
        if(!mem)
            // instruction cannot be already converted here (either all are already converted or none)
            throw CompilationError(
                CompilationStep::NORMALIZER, "Invalid instruction to be lowered into register", (*it)->to_string());
        if(type == MemoryType::VPM_PER_QPU)
            logging::debug() << "Trying to lower access to stack allocation into VPM: " << mem->to_string()
                             << logging::endl;
        else
            logging::debug() << "Trying to lower access to shared local memory into VPM: " << mem->to_string()
                             << logging::endl;
        Value inAreaOffset = UNDEFINED_VALUE;
        auto tmpIt = insertAddressToStackOffset(*it, method, inAreaOffset, local, type, mem);
        switch(mem->op)
        {
        case MemoryOperation::COPY:
        {
            // if the other local is already mapped to VPM, insert copy instruction. Otherwise let other local handle
            // this
            auto memAreas = mem->getMemoryAreas();
            if(std::all_of(memAreas.begin(), memAreas.end(),
                   [&](const Local* l) -> bool { return vpmAreas.find(l) != vpmAreas.end(); }))
            {
                // TODO insert copy from/to VPM. Need to do via read/write
                break;
            }
            ++it;
            continue;
        }
        case MemoryOperation::FILL:
            throw CompilationError(
                CompilationStep::NORMALIZER, "Filling VPM area is not yet implemented", mem->to_string());
        case MemoryOperation::READ:
            it = memoryInstructions.erase(it);
            tmpIt = method.vpm->insertReadVPM(method, tmpIt, mem->getDestination(), vpmArea, true, inAreaOffset);
            tmpIt.erase();
            break;
        case MemoryOperation::WRITE:
            it = memoryInstructions.erase(it);
            tmpIt = method.vpm->insertWriteVPM(method, tmpIt, mem->getSource(), vpmArea, true, inAreaOffset);
            tmpIt.erase();
            break;
        }
        logging::debug() << "Replaced access to memory buffer with access to VPM" << logging::endl;
    }

    // even if we did not map all accesses, we fixed the local to the VPM area
    // for e.g. copy, the other local also has a reference to this MemoryInstruction and will handle it
    return memoryInstructions.empty();
}

struct MemoryAccess
{
    FastSet<InstructionWalker> accessInstructions;
    MemoryType preferred;
    MemoryType fallback;
};

/*
 * Basic algorithm to determine the preferred and fall-back (e.g. if access-types not supported by preferred)
 * way of
 * a) mapping the memory regions used by this method to the available "memory" (registers, VPM, RAM) and
 * b) mapping the memory access types (read, write, copy, fill) to the available memory access types (TMU, VPM, etc.)
 */
static FastMap<const Local*, MemoryAccess> determineMemoryAccess(Method& method)
{
    // TODO lower local/private struct-elements into VPM?! At least for single structs
    logging::debug() << "Determining memory access for kernel: " << method.name << logging::endl;
    FastMap<const Local*, MemoryAccess> mapping;
    for(const auto& param : method.parameters)
    {
        if(!param.type.isPointerType())
            continue;
        const auto* pointerType = param.type.getPointerType().value();
        if(pointerType->addressSpace == AddressSpace::CONSTANT)
        {
            logging::debug() << "Constant parameter '" << param.to_string() << "' will be read from RAM via TMU"
                             << logging::endl;
            mapping[&param].preferred = MemoryType::RAM_LOAD_TMU;
            // fall-back, e.g. for memory copy
            mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
        }
        else if(pointerType->addressSpace == AddressSpace::GLOBAL)
        {
            if(isMemoryOnlyRead(&param))
            {
                logging::debug() << "Global parameter '" << param.to_string()
                                 << "' without any write access will be read from RAM via TMU" << logging::endl;
                mapping[&param].preferred = MemoryType::RAM_LOAD_TMU;
                // fall-back, e.g. for memory copy
                mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
            }
            else
            {
                logging::debug() << "Global parameter '" << param.to_string()
                                 << "' which is written to will be stored in RAM and accessed via VPM" << logging::endl;
                mapping[&param].preferred = MemoryType::RAM_READ_WRITE_VPM;
                mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
            }
        }
        else if(pointerType->addressSpace == AddressSpace::LOCAL)
        {
            // TODO if last access index is known and fits into VPM, set for VPM-or-RAM
            logging::debug() << "Local parameter '" << param.to_string()
                             << "' will be stored in RAM and accessed via VPM" << logging::endl;
            mapping[&param].preferred = MemoryType::RAM_READ_WRITE_VPM;
            mapping[&param].fallback = MemoryType::RAM_READ_WRITE_VPM;
        }
        else
            throw CompilationError(
                CompilationStep::NORMALIZER, "Invalid address space for pointer parameter", param.to_string(true));
    }

    InstructionWalker it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(it.has<MemoryInstruction>())
        {
            for(const auto local : it.get<MemoryInstruction>()->getMemoryAreas())
            {
                if(mapping.find(local) != mapping.end())
                {
                    // local was already processed
                    mapping[local].accessInstructions.emplace(it);
                    continue;
                }
                mapping[local].accessInstructions.emplace(it);
                if(local->is<StackAllocation>())
                {
                    if(local->type.isSimpleType() || convertSmallArrayToRegister(local))
                    {
                        logging::debug() << "Small stack value '" << local->to_string()
                                         << "' will be stored in a register" << logging::endl;
                        mapping[local].preferred = MemoryType::QPU_REGISTER_READWRITE;
                        // we cannot pack an array into a VPM cache line, since always all 16 elements are read/written
                        // and we would overwrite the other elements
                        mapping[local].fallback =
                            local->type.isSimpleType() ? MemoryType::VPM_PER_QPU : MemoryType::RAM_READ_WRITE_VPM;
                    }
                    else if(!local->type.getElementType().getStructType())
                    {
                        logging::debug() << "Stack value '" << local->to_string()
                                         << "' will be stored in VPM per QPU (with fall-back to RAM via VPM)"
                                         << logging::endl;
                        mapping[local].preferred = MemoryType::VPM_PER_QPU;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                    else
                    {
                        logging::debug() << "Struct stack value '" << local->to_string()
                                         << "' will be stored in RAM per QPU (via VPM)" << logging::endl;
                        mapping[local].preferred = MemoryType::RAM_READ_WRITE_VPM;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                }
                else if(local->is<Global>())
                {
                    if(isMemoryOnlyRead(local))
                    {
                        // global buffer
                        const auto memInstr = it.get<const MemoryInstruction>();
                        if(getConstantValue(memInstr))
                        {
                            logging::debug() << "Constant element of constant buffer '" << local->to_string()
                                             << "' will be stored in a register " << logging::endl;
                            mapping[local].preferred = MemoryType::QPU_REGISTER_READONLY;
                            mapping[local].fallback = MemoryType::RAM_LOAD_TMU;
                        }
                        else if(convertSmallArrayToRegister(local))
                        {
                            logging::debug() << "Small constant buffer '" << local->to_string()
                                             << "' will be stored in a register" << logging::endl;
                            mapping[local].preferred = MemoryType::QPU_REGISTER_READONLY;
                            mapping[local].fallback = MemoryType::RAM_LOAD_TMU;
                        }
                        else
                        {
                            logging::debug() << "Constant buffer '" << local->to_string()
                                             << "' will be read from RAM via TMU" << logging::endl;
                            mapping[local].preferred = MemoryType::RAM_LOAD_TMU;
                            // fall-back, e.g. for memory copy
                            mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                        }
                    }
                    else if(!local->type.getElementType().getStructType())
                    {
                        // local buffer
                        logging::debug() << "Local buffer '" << local->to_string()
                                         << "' will be stored in VPM (with fall-back to RAM via VPM)" << logging::endl;
                        mapping[local].preferred = MemoryType::VPM_SHARED_ACCESS;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                    else
                    {
                        // local buffer
                        logging::debug() << "Local struct '" << local->to_string() << "' will be stored in RAM via VPM"
                                         << logging::endl;
                        mapping[local].preferred = MemoryType::RAM_READ_WRITE_VPM;
                        mapping[local].fallback = MemoryType::RAM_READ_WRITE_VPM;
                    }
                }
                else
                    // parameters MUST be handled before and there is no other type of memory objects
                    throw CompilationError(
                        CompilationStep::NORMALIZER, "Invalid local type for memory area", local->to_string(true));
            }
        }
        it.nextInMethod();
    }

    return mapping;
}

void normalization::mapMemoryAccess(const Module& module, Method& method, const Configuration& config)
{
    /*
     * Matrix of memory types and storage locations:
     *
     *           | global | local | private | constant
     * buffer    |   -    |VPM/GD | QPU/VPM | QPU/GD
     * parameter |  RAM   |RAM/(*)|    -    |   RAM
     *
     * buffer is both inside and outside of function scope (where allowed)
     * - : is not allowed by OpenCL
     * (*) could lower into VPM if the highest index accessed is known and fits?
     * GD: global data segment of kernel buffer
     * RAM: load via TMU if possible (not written to), otherwise use VPM
     *
     * Sources:
     * https://stackoverflow.com/questions/22471466/why-program-global-scope-variables-must-be-constant#22474119
     * https://stackoverflow.com/questions/17431941/how-to-use-arrays-in-program-global-scope-in-opencl
     */
    /*
     * 1. lower constant/private buffers into register
     *    lower global constant buffers into registers
     *    lower small enough private buffers to registers
     * 2. generate TMU loads for read-only memory
     *    keep all read-only parameters in RAM, load via TMU
     *    also load constants via TMU, which could not be lowered into register
     * 3. lower per-QPU (private) buffers into VPM
     * 4. lower shared buffers (local) into VPM
     * 5. generate remaining instructions for RAM access via VPM
     * TODO:
     * 3.1 for memory located in RAM, try to group/queue reads/writes
     * 3.2 also try to use VPM as cache (e.g. only write back into memory when VPM cache area full, prefetch into VPM)
     * 4. final pass which actually converts VPM cache
     */
    auto memoryMapping = determineMemoryAccess(method);
    // stores already assigned VPM areas, e.g. for memory-copy operations
    FastMap<const Local*, const VPMArea*> vpmMappedLocals;

    // 1. lower into registers
    auto mappingIt = memoryMapping.begin();
    while(mappingIt != memoryMapping.end())
    {
        if(mappingIt->second.preferred == MemoryType::QPU_REGISTER_READONLY ||
            mappingIt->second.preferred == MemoryType::QPU_REGISTER_READWRITE)
        {
            if(lowerMemoryToRegister(
                   method, mappingIt->first, mappingIt->second.preferred, mappingIt->second.accessInstructions))
                mappingIt = memoryMapping.erase(mappingIt);
            else if(mappingIt->second.fallback == MemoryType::QPU_REGISTER_READONLY ||
                mappingIt->second.fallback == MemoryType::QPU_REGISTER_READWRITE)
                throw CompilationError(
                    CompilationStep::NORMALIZER, "Failed to lower memory to register", mappingIt->first->to_string());
            else
            {
                // could not lower to register, fall back to fall-back and try again
                mappingIt->second.preferred = mappingIt->second.fallback;
                ++mappingIt;
            }
        }
        else
            ++mappingIt;
    }
    // 2. load read-only parameter via TMU
    mappingIt = memoryMapping.begin();
    // the flag as to which TMU to use
    // TODO for better performance, this should alternate in according to the order of usage (first read use TMU0,
    // second read use TMU1, ...)
    bool tmuFlag = false;
    // The insertion of the TMU_NOSWAP configuration is inserted in #addStartStopSegment to the start of the kernel
    while(mappingIt != memoryMapping.end())
    {
        if(mappingIt->second.preferred == MemoryType::RAM_LOAD_TMU)
        {
            if(mapReadsToTMULoad(method, mappingIt->first, mappingIt->second.accessInstructions, tmuFlag))
            {
                tmuFlag = !tmuFlag;
                if(mappingIt->first->is<Parameter>())
                {
                    const_cast<Parameter*>(mappingIt->first->as<Parameter>())->decorations =
                        add_flag(mappingIt->first->as<Parameter>()->decorations, ParameterDecorations::INPUT);
                }
                mappingIt = memoryMapping.erase(mappingIt);
            }
            else if(mappingIt->second.fallback == MemoryType::RAM_LOAD_TMU)
                throw CompilationError(
                    CompilationStep::NORMALIZER, "Failed to generate TMU load", mappingIt->first->to_string());
            else
            {
                // could not load via TMU (e.g. copy), retry with fall-back
                mappingIt->second.preferred = mappingIt->second.fallback;
                ++mappingIt;
            }
        }
        else
            ++mappingIt;
    }
    // 3. lower private memory into VPM
    mappingIt = memoryMapping.begin();
    while(mappingIt != memoryMapping.end())
    {
        if(mappingIt->second.preferred == MemoryType::VPM_PER_QPU)
        {
            // TODO could optimize by preferring the private buffer accessed more often to be in VPM
            if(lowerMemoryToVPM(method, mappingIt->first, MemoryType::VPM_PER_QPU, mappingIt->second.accessInstructions,
                   vpmMappedLocals))
                mappingIt = memoryMapping.erase(mappingIt);
            else
            {
                // could not lower to VPM, fall back to fall-back and try again
                mappingIt->second.preferred = mappingIt->second.fallback;
                ++mappingIt;
            }
        }
        else
            ++mappingIt;
    }
    // 4. lower local memory into VPM
    mappingIt = memoryMapping.begin();
    while(mappingIt != memoryMapping.end())
    {
        if(mappingIt->second.preferred == MemoryType::VPM_SHARED_ACCESS)
        {
            // TODO could optimize by preferring the local buffer accessed more often to be in VPM
            if(lowerMemoryToVPM(method, mappingIt->first, MemoryType::VPM_SHARED_ACCESS,
                   mappingIt->second.accessInstructions, vpmMappedLocals))
                mappingIt = memoryMapping.erase(mappingIt);
            else
            {
                // could not lower to VPM, fall back to fall-back and try again
                mappingIt->second.preferred = mappingIt->second.fallback;
                ++mappingIt;
            }
        }
        else
            ++mappingIt;
    }
    // 5. map remaining instructions to access RAM via VPM
    // list of basic blocks where multiple VPM accesses could be combined
    FastSet<BasicBlock*> affectedBlocks;
    mappingIt = memoryMapping.begin();
    while(mappingIt != memoryMapping.end())
    {
        if(mappingIt->second.preferred == MemoryType::RAM_READ_WRITE_VPM)
        {
            if(mapReadWriteToMemoryViaVPM(
                   method, mappingIt->first, mappingIt->second.accessInstructions, vpmMappedLocals, affectedBlocks))
                mappingIt = memoryMapping.erase(mappingIt);
            else
                ++mappingIt;
        }
        else
            ++mappingIt;
    }

    if(!memoryMapping.empty())
    {
        for(const auto& map : memoryMapping)
            logging::error() << "Unhandled memory access type: " << map.first->to_string() << logging::endl;
        throw CompilationError(CompilationStep::NORMALIZER, "Unhandled memory access types!");
    }

    combineVPMAccess(affectedBlocks, method);

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
                    /*
                     * replace read and write of value to memory copy to a) optimize for speed and b) also support types
                     * > 32 bit, if following conditions are given
                     * - the local is only written in the load instruction
                     * - the local is only read in the store instruction
                     * - the byte-size of the memory loaded and stored is the same
                     */
                    // FIXME this might not be correct when copying from/to stack!
                    if(false && !nextIt.isEndOfBlock() && nextMemInst != nullptr &&
                        nextMemInst->op == MemoryOperation::WRITE && !memInst->hasConditionalExecution() &&
                        !nextMemInst->hasConditionalExecution() &&
                        nextMemInst->getSource() == memInst->getDestination() &&
                        memInst->getDestination().getSingleWriter() == memInst &&
                        memInst->getDestination().local->getUsers(LocalUse::Type::READER).size() == 1 &&
                        nextMemInst->getDestinationElementType().getPhysicalWidth() ==
                            memInst->getSourceElementType().getPhysicalWidth())
                    {
                        // TODO add this to somewhere up front
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

    // TODO move calculation of stack/global indices in here too?

    // TODO clean up no longer used (all kernels!) globals and stack allocations
}
