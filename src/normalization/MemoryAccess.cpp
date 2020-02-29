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
#include "../intermediate/operators.h"
#include "../periphery/VPM.h"
#include "AddressCalculation.h"
#include "MemoryMappings.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::intermediate;
using namespace vc4c::periphery;
using namespace vc4c::operators;

// TODO make use of parameter's maxByteOffset? E.g. for caching?

struct BaseAndOffset
{
    Optional<Value> base;
    Optional<int32_t> offset;

    explicit BaseAndOffset() : base(NO_VALUE), offset{} {}

    BaseAndOffset(const Optional<Value>& base, Optional<int32_t> offset) : base(base), offset(offset) {}
};

static BaseAndOffset findOffset(const Value& val)
{
    if(!val.checkLocal())
        return BaseAndOffset();
    if(auto writer = val.getSingleWriter())
    {
        const Optional<Value> offset = writer->precalculate(8).first;
        if(offset && offset->isLiteralValue())
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
    if(!val.checkLocal())
        return BaseAndOffset();
    if(val.local()->is<Parameter>() || val.local()->is<Global>() || val.local()->is<StackAllocation>())
        return BaseAndOffset(val, 0);

    // follow the references
    const Local* ref = val.local()->getBase(false);
    if(ref != val.local())
        return findBaseAndOffset(ref->createReference());
    if(auto data = val.local()->get<ReferenceData>())
    {
        if(data->offset != ANY_ELEMENT)
            return BaseAndOffset(data->base->createReference(), data->offset);
    }

    const auto writers = val.local()->getUsers(LocalUse::Type::WRITER);
    if(writers.size() != 1)
        return BaseAndOffset();

    // The reader can be one of several valid cases:
    // 1. a move from another local -> need to follow the move
    if(auto move = dynamic_cast<const MoveOperation*>(*writers.begin()))
        return findBaseAndOffset(move->getSource());
    const auto& args = (*writers.begin())->getArguments();
    // 2. an addition with a local and a literal -> the local is the base, the literal the offset
    if(dynamic_cast<const Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
        std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.checkLocal(); }) &&
        std::any_of(
            args.begin(), args.end(), [](const Value& arg) -> bool { return arg.getLiteralValue().has_value(); }))
    {
        return BaseAndOffset(
            std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.checkLocal(); })
                ->local()
                ->getBase(false)
                ->createReference(),
            (*std::find_if(
                 args.begin(), args.end(), [](const Value& arg) -> bool { return arg.getLiteralValue().has_value(); }))
                    .getLiteralValue()
                    ->signedInt() /
                /* in-memory width can't be > 2^30 anyway */
                static_cast<int32_t>(val.type.getElementType().getInMemoryWidth()));
    }

    // 3. an addition with two locals -> one is the base, the other the calculation of the literal
    if(dynamic_cast<const Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
        std::all_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.checkLocal(); }))
    {
        const auto offset0 = findOffset(args[0]);
        const auto offset1 = findOffset(args[1]);
        if(offset0.offset && args[1].checkLocal())
            return BaseAndOffset(args[1].local()->getBase(false)->createReference(),
                offset0.offset.value() / static_cast<int32_t>(val.type.getElementType().getInMemoryWidth()));
        if(offset1.offset && args[0].checkLocal())
            return BaseAndOffset(args[0].local()->getBase(false)->createReference(),
                offset1.offset.value() / static_cast<int32_t>(val.type.getElementType().getInMemoryWidth()));
    }
    /*
        if(writers.size() == 1)
        {
            // couldn't find literal offset for any direct base, try with arbitrary values
            ref = val.local->getBase(true);
            Optional<Value> offset = NO_VALUE;
            for(const auto& arg : (*writers.begin())->getArguments())
            {
                if(ref != nullptr && arg.hasLocal() && arg.local->getBase(false) == ref)
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
    FastAccessList<InstructionWalker> dmaSetups;
    FastAccessList<InstructionWalker> genericSetups;
    FastAccessList<InstructionWalker> addressWrites;
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

        if(it.get<MemoryBarrier>())
            // memory barriers end groups, also don't check this barrier again
            return it.nextInBlock();
        if(it.get<SemaphoreAdjustment>())
            // semaphore accesses end groups, also don't check this instruction again
            return it.nextInBlock();

        if(!(it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            // for simplicity, we only check for VPM addresses and find all other instructions relative to it
            continue;
        if(!it.get<MoveOperation>())
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Setting VPM address with non-move is not supported", it->to_string());
        const auto baseAndOffset = findBaseAndOffset(it.get<MoveOperation>()->getSource());
        const bool isVPMWrite = it->writesRegister(REG_VPM_DMA_STORE_ADDR);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Found base address " << baseAndOffset.base.to_string() << " with offset "
                << std::to_string(baseAndOffset.offset.value_or(-1L)) << " for "
                << (isVPMWrite ? "writing into" : "reading from") << " memory" << logging::endl);

        if(!baseAndOffset.base)
            // this address-write could not be fixed to a base and an offset
            // skip this address write for the next check
            return it.nextInBlock();
        if(baseAndOffset.base && baseAndOffset.base->checkLocal() && baseAndOffset.base->local()->is<Parameter>() &&
            has_flag(baseAndOffset.base->local()->as<Parameter>()->decorations, ParameterDecorations::VOLATILE))
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
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Using a stride of " << group.stride << " elements between consecutive access to memory"
                        << logging::endl);
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
            if(!genericSetup->get<LoadImmediate>() ||
                (!group.genericSetups.empty() &&
                    genericSetup->get<LoadImmediate>()->getImmediate() !=
                        group.genericSetups.at(0).get<LoadImmediate>()->getImmediate()))
                // generic setups do not match
                break;
            if(!dmaSetup->get<LoadImmediate>() ||
                dmaSetup->get<LoadImmediate>()->getImmediate() !=
                    group.dmaSetups.at(0).get<LoadImmediate>()->getImmediate())
                // DMA setups do not match
                break;
        }

        // check for complex types
        DataType elementType = baseAndOffset.base->type.getPointerType() ?
            baseAndOffset.base->type.getPointerType()->elementType :
            baseAndOffset.base->type;
        elementType = elementType.getArrayType() ? elementType.getArrayType()->elementType : elementType;
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
            } while(!it.isEndOfBlock() && it.get() != nullptr && it->checkOutputRegister());
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
        LCOV_EXCL_START
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Number of instructions do not match for combining VPM writes!" << logging::endl;
            logging::debug() << group.genericSetups.size() << " generic VPM setups, " << group.addressWrites.size()
                             << " VPR address writes and " << group.dmaSetups.size() << " DMA setups" << logging::endl;
        });
        LCOV_EXCL_STOP
        return;
    }
    if(group.addressWrites.size() <= 1)
        return;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combining " << group.addressWrites.size() << " writes to consecutive memory into one DMA write... "
            << logging::endl);

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
                group.groupType.getElementType().getInMemoryWidth()));
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
        Value(group.addressWrites.at(0).get<MoveOperation>()->getSource()));
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
        LCOV_EXCL_START
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Number of instructions do not match for combining VPM reads!" << logging::endl;
            logging::debug() << group.genericSetups.size() << " generic VPM setups, " << group.addressWrites.size()
                             << " VPR address writes and " << group.dmaSetups.size() << " DMA setups" << logging::endl;
        });
        LCOV_EXCL_STOP
        return;
    }
    if(group.genericSetups.size() <= 1)
        return;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combining " << group.genericSetups.size() << " reads of consecutive memory into one DMA read... "
            << logging::endl);

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
            static_cast<unsigned>(group.stride) * group.groupType.getElementType().getInMemoryWidth()));
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

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Removed " << numRemoved << " instructions by combining VPR reads" << logging::endl);
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
        auto it = block->walk();
        while(!it.isEndOfBlock())
        {
            VPMAccessGroup group;
            it = findGroupOfVPMAccess(*method.vpm.get(), it, block->walkEnd(), group);
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
        if(arg.checkLocal() && arg.local()->is<Global>())
        {
            if(auto globalOffset = module.getGlobalDataOffset(arg.local()))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing access to global data: " << it->to_string() << logging::endl);
                Value tmp = UNDEFINED_VALUE;
                if(globalOffset.value() == 0)
                {
                    tmp = method.findOrCreateBuiltin(BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)->createReference();
                }
                else
                {
                    // emplace calculation of global-data pointer and replace argument
                    tmp = assign(it, TYPE_INT32, "%global_data_offset") =
                        method.findOrCreateBuiltin(BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)->createReference() +
                        Value(Literal(globalOffset.value()), TYPE_INT32);
                }
                it->setArgument(i, std::move(tmp));
            }
        }
    }
    return it;
}

void normalization::spillLocals(const Module& module, Method& method, const Configuration& config)
{
    static constexpr std::size_t MINIMUM_THRESHOLD = 128; /* TODO some better limit */

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
    while(!it.isEndOfMethod() && it.get<intermediate::BranchLabel>())
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
        if(it->checkOutputLocal() &&
            (cIt = spillingCandidates.find(it->getOutput()->local())) != spillingCandidates.end())
        {
            if(method.isLocallyLimited(it, it->getOutput()->local(), MINIMUM_THRESHOLD))
                spillingCandidates.erase(cIt);
            else
                cIt->second = it;
        }
        it.nextInMethod();
    }

    LCOV_EXCL_START
    CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
        for(const auto& pair : spillingCandidates)
        {
            logging::debug() << "Spilling candidate: " << pair.first->to_string() << " ("
                             << pair.first->getUsers(LocalUse::Type::WRITER).size() << " writes, "
                             << pair.first->getUsers(LocalUse::Type::READER).size() << " reads)" << logging::endl;
        }
    });
    LCOV_EXCL_STOP

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
        if(arg.checkLocal() && arg.type.getPointerType() && arg.local()->is<StackAllocation>())
        {
            // 2.remove the life-time instructions
            if(it.get<intermediate::LifetimeBoundary>() != nullptr)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Dropping life-time instruction for stack-allocation: " << arg.to_string() << logging::endl);
                it = it.erase();
                // to not skip the next instruction
                it.previousInBlock();
            }
            else if(stackBaseOffset == 0 && maximumStackSize == 0 && arg.local()->as<StackAllocation>()->isLowered)
            {
                /*
                 * Stack objects which are lowered into VPM (or registers) have a special address calculation.
                 *
                 * Instead of the per-QPU stack frame and within that the per-stack object offset (see below),
                 * lowered stack objects have a global per-object offset within the VPM and a per-QPU offset within
                 * that.
                 *
                 * E.g. the VPM layout of lowered stack objects is as follows:
                 *
                 * | object A . QPU0 . QPU1 . QPU2 . ... | object B . QPU0 . QPU1 . QPU2 . ... | ...
                 *
                 * To convert the "normal" per-QPU/per-object layout (below) to the per-object/per-QPU layout, the
                 * per-QPU/per-object is subtracted from the pointer generated by the "normal" method (see
                 * #insertAddressToOffset) and then the per-QPU offset is added again.
                 *
                 * If all stack objects are lowered into VPM, there is no offset from the stack base that we need to
                 * subtract (the offset is always zero), so we can just set it to zero.
                 */
                it->setArgument(i, Value(INT_ZERO));
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

                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Replacing access to stack allocated data: " << it->to_string() << logging::endl);

                auto qpuOffset = assign(it, TYPE_INT32, "%stack_offset") = mul24(Value(REG_QPU_NUMBER, TYPE_INT8),
                    Value(Literal(static_cast<uint32_t>(maximumStackSize)), TYPE_INT32));
                auto addrTemp = assign(it, arg.type, "%stack_addr") =
                    qpuOffset + method.findOrCreateBuiltin(BuiltinLocal::Type::GLOBAL_DATA_ADDRESS)->createReference();
                auto finalAddr = assign(it, arg.type, "%stack_addr") = addrTemp +
                    Value(Literal(static_cast<uint32_t>(arg.local()->as<StackAllocation>()->offset + stackBaseOffset)),
                        TYPE_INT32);
                finalAddr.local()->set(ReferenceData(*arg.local(), ANY_ELEMENT));
                it->setArgument(i, std::move(finalAddr));
            }
        }
    }
}

/* clang-format off */
/*
 * Matrix of memory types and storage locations:
 *
 *           | global | local | private | constant
 * buffer    |   -    |VPM/GD | QPU/VPM | QPU/GD
 * parameter |  RAM   |RAM/(*)|    -    |   RAM
 *
 * buffer is both inside and outside of function scope (where allowed)
 * - : is not allowed by OpenCL
 * (*) is lowered into VPM if the highest index accessed is known and fits
 * GD: global data segment of kernel buffer
 * RAM: load via TMU if possible (not written to), otherwise use VPM
 *
 * Sources:
 * https://stackoverflow.com/questions/22471466/why-program-global-scope-variables-must-be-constant#22474119
 * https://stackoverflow.com/questions/17431941/how-to-use-arrays-in-program-global-scope-in-opencl
 *
 * 
 * Matrix of memory types and access ways:
 * compile-time memory: __constant buffer with values known at compile-time
 * constant memory: __constant or read-only __global/__local buffer/parameter
 * private memory: __private buffer/stack allocations
 * read-write memory: any other __global/__local buffer/parameter
 *
 *                     |   optimization   |   location   |   read    |   write   |    copy from    |       copy to       | group | priority |
 * compile-time memory |     "normal"     |      GD      |    TMU    |     -     |    DMA/TMU(*)   |          -          |  (1)  |     2    |    
 *                     |   lowered load   |      QPU     | register  |     -     | VPM/register(*) |          -          |  (2)  |     1    |
 * constant memory     |     "normal"     |     GD/RAM   |    TMU    |     -     |    DMA/TMU(*)   |          -          |  (1)  |     2    |
 * private memory      |     "normal"     |      GD      |    DMA    |    DMA    |       DMA       |         DMA         |  (3)  |     3    |
 *                     | lowered register |      QPU     | register  | register  | VPM/register(*) | VPM/TMU/register(*) |  (2)  |     1    |
 *                     |   lowered VPM    |      VPM     |    VPM    |    VPM    |     VPM/DMA     |       VPM/DMA       |  (4)  |     2    |
 * read-write memory   |     "normal"     |     GD/RAM   |    DMA    |    DMA    |       DMA       |         DMA         |  (3)  |     3    |
 *                     |   lowered VPM    |      VPM     |    VPM    |    VPM    |     VPM/DMA     |       VPM/DMA       |  (4)  |     1    |
 *                     |    cached VPM    | VPM + GD/RAM | VPM + DMA | VPM + DMA |     VPM/DMA     |       VPM/DMA       |  (4)  |     2    |
 *
 * Special cases:
 *  (*) when copying from constant memory into register, TMU can be used instead. Copying from and to register is done inside the QPU
 *
 */
/* clang-format on */

void normalization::mapMemoryAccess(const Module& module, Method& method, const Configuration& config)
{
    /*
     * 1. lower constant/private buffers into register
     *    lower global constant buffers into registers
     *    lower small enough private buffers to registers
     * 2. generate TMU loads for read-only memory
     *    keep all read-only parameters in RAM, load via TMU
     *    also load constants via TMU, which could not be lowered into register
     * 3. lower per-QPU (private) buffers into VPM
     * 4. lower shared buffers (local) into VPM
     * 5. generate remaining instructions for RAM access via VPM scratch area
     * TODO:
     * 3.1 for memory located in RAM, try to group/queue reads/writes
     * 3.2 also try to use VPM as cache (e.g. only write back into memory when VPM cache area full, prefetch into VPM)
     * 4. final pass which actually converts VPM cache
     */

    // determine preferred and fall-back memory access type for each memory are
    auto memoryAccessInfo = determineMemoryAccess(method);

    FastMap<const Local*, MemoryInfo> infos;
    {
        // gather more information about the memory areas and modify the access types. E.g. if the preferred access type
        // cannot be used, use the fall-back
        infos.reserve(memoryAccessInfo.memoryAccesses.size());
        for(auto& mapping : memoryAccessInfo.memoryAccesses)
        {
            auto it = infos.emplace(mapping.first, checkMemoryMapping(method, mapping.first, mapping.second));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << (it.first->first->is<Parameter>() ?
                               "Parameter" :
                               (it.first->first->is<StackAllocation>() ? "Stack variable" : "Local"))
                    << " '" << it.first->first->to_string() << "' will be mapped to: " << it.first->second.to_string()
                    << logging::endl);
        }
    }

    // list of basic blocks where multiple VPM accesses could be combined
    FastSet<BasicBlock*> affectedBlocks;

    // TODO sort locals by where to put them and then call 1. check of mapping and 2. mapping on all
    for(auto& memIt : memoryAccessInfo.accessInstructions)
    {
        auto mem = memIt.get<const MemoryInstruction>();
        auto srcBaseLocal = mem->getSource().checkLocal() ? mem->getSource().local()->getBase(true) : nullptr;
        auto dstBaseLocal = mem->getDestination().checkLocal() ? mem->getDestination().local()->getBase(true) : nullptr;

        auto srcInfoIt = srcBaseLocal ? infos.find(srcBaseLocal) : infos.end();
        const MemoryInfo& srcInfo = srcInfoIt != infos.end() ?
            srcInfoIt->second :
            MemoryInfo{srcBaseLocal, MemoryAccessType::QPU_REGISTER_READWRITE};
        auto dstInfoIt = dstBaseLocal ? infos.find(dstBaseLocal) : infos.end();
        const MemoryInfo& dstInfo = dstInfoIt != infos.end() ?
            dstInfoIt->second :
            MemoryInfo{dstBaseLocal, MemoryAccessType::QPU_REGISTER_READWRITE};

        if(srcInfo.type == MemoryAccessType::RAM_READ_WRITE_VPM || dstInfo.type == MemoryAccessType::RAM_READ_WRITE_VPM)
            affectedBlocks.emplace(InstructionWalker{memIt}.getBasicBlock());

        // TODO Fail here preemptively, since the actual mapping  will only check the
        // direct base of the source/destination, which is the phi-node output local, which is not
        // mapped correctly! And here, we actually know the reason, so we can show better error messages.
        // TODO to solve this, we would need to be able to pass (and handle) all MemoryInfo for all phi-node accessed
        // memory areas for src/dest.
        if(srcInfo.local &&
            memoryAccessInfo.additionalAreaMappings.find(srcInfo.local) !=
                memoryAccessInfo.additionalAreaMappings.end())
            throw CompilationError(CompilationStep::NORMALIZER,
                "Reading memory through a phi-node is not implemented yet", memIt->to_string());
        if(dstInfo.local &&
            memoryAccessInfo.additionalAreaMappings.find(dstInfo.local) !=
                memoryAccessInfo.additionalAreaMappings.end())
            throw CompilationError(CompilationStep::NORMALIZER,
                "Writing memory through a phi-node is not implemented yet", memIt->to_string());

        mapMemoryAccess(method, memIt, const_cast<MemoryInstruction*>(mem), srcInfo, dstInfo);
        // TODO mark local for prefetch/write-back (if necessary)
    }

    method.vpm->dumpUsage();

    // TODO move this to optimization?
    combineVPMAccess(affectedBlocks, method);

    // TODO clean up no longer used (all kernels!) globals and stack allocations
}
