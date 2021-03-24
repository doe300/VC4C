/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Memory.h"

#include "../Expression.h"
#include "../GlobalValues.h"
#include "../Method.h"
#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/ControlFlowLoop.h"
#include "../analysis/DataDependencyGraph.h"
#include "../analysis/DominatorTree.h"
#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/operators.h"
#include "../performance.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <array>

using namespace vc4c;
using namespace vc4c::operators;

// TODO rewrite combination of VPM accesses to use MemoryAccessInstructions instead? Need to move before memory lowering
// and somehow be able to explicitly set stride/etc.
// TODO also rewrite to use expression to find base/offset?

bool optimizations::lowerMemoryAccess(const Module& module, Method& method, const Configuration& config)
{
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(auto memoryAccess = it.get<intermediate::MemoryAccessInstruction>())
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Lowering memory access: " << memoryAccess->to_string() << logging::endl);
                if(memoryAccess->getTMUCacheEntry())
                    it = periphery::lowerTMURead(method, it);
                else if(memoryAccess->getVPMCacheEntry())
                    it = periphery::lowerVPMAccess(method, it);
                else
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Unhandled memory access instruction", it->to_string());
            }
            else
                it.nextInBlock();
        }
    }
    // a kernel without a single memory access does not make sense at all
    return true;
}

struct BaseAndOffset
{
    Optional<Value> base;
    Optional<int32_t> offset;

    explicit BaseAndOffset() : base(NO_VALUE), offset{} {}

    BaseAndOffset(const Optional<Value>& base, Optional<int32_t> offset) : base(base), offset(std::move(offset)) {}
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
    if(auto source = (*writers.begin())->getMoveSource())
        return findBaseAndOffset(*source);
    const auto& args = (*writers.begin())->getArguments();
    // 2. an addition with a local and a literal -> the local is the base, the literal the offset
    if(dynamic_cast<const intermediate::Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const intermediate::Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
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
    if(dynamic_cast<const intermediate::Operation*>((*writers.begin())) != nullptr &&
        dynamic_cast<const intermediate::Operation*>((*writers.begin()))->op == OP_ADD && args.size() == 2 &&
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

    return BaseAndOffset();
}

struct VPMAccessGroup
{
    bool isVPMWrite;
    DataType groupType = TYPE_UNKNOWN;
    FastAccessList<InstructionWalker> dmaSetups;
    FastAccessList<InstructionWalker> strideSetups;
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

        it = strideSetups.begin();
        ++it;
        while(it != strideSetups.end())
        {
            if((it - 1)->get() == it->get())
                it = strideSetups.erase(it);
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

struct VPMInstructions
{
    Optional<InstructionWalker> genericVPMSetup;
    Optional<InstructionWalker> dmaSetup;
    Optional<InstructionWalker> strideSetup;
    Optional<InstructionWalker> vpmAccess;
    Optional<InstructionWalker> addressWrite;
    Optional<InstructionWalker> dmaWait;
};

/**
 * Returns the instruction related to the current VPM access of the instruction given.
 *
 * This function looks within the same mutex-lock block (if any) at the preceding and following instructions to
 * find the instructions required for the given VPM access.
 */
static VPMInstructions findRelatedVPMInstructions(InstructionWalker anyVPMInstruction, bool isVPMRead)
{
    using namespace periphery;

    const auto predAddressWrite = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(isVPMRead)
            return inst->writesRegister(REG_VPM_DMA_LOAD_ADDR);
        return inst->writesRegister(REG_VPM_DMA_STORE_ADDR);
    };
    const auto predDMASetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        auto val = inst->precalculate().first & &Value::getLiteralValue;
        if(isVPMRead)
            return val && inst->writesRegister(REG_VPM_IN_SETUP) &&
                VPRSetup::fromLiteral(val->unsignedInt()).isDMASetup();
        return val && inst->writesRegister(REG_VPM_OUT_SETUP) && VPWSetup::fromLiteral(val->unsignedInt()).isDMASetup();
    };
    const auto predDMAWait = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(isVPMRead)
            return inst->readsRegister(REG_VPM_DMA_LOAD_WAIT);
        return inst->readsRegister(REG_VPM_DMA_STORE_WAIT);
    };
    const auto predGenericSetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        auto val = inst->precalculate().first & &Value::getLiteralValue;
        if(isVPMRead)
            return val && inst->writesRegister(REG_VPM_IN_SETUP) &&
                VPRSetup::fromLiteral(val->unsignedInt()).isGenericSetup();
        return val && inst->writesRegister(REG_VPM_OUT_SETUP) &&
            VPWSetup::fromLiteral(val->unsignedInt()).isGenericSetup();
    };
    const auto predStrideSetup = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        auto val = inst->precalculate().first & &Value::getLiteralValue;
        if(isVPMRead)
            return val && inst->writesRegister(REG_VPM_IN_SETUP) &&
                VPRSetup::fromLiteral(val->unsignedInt()).isStrideSetup();
        return val && inst->writesRegister(REG_VPM_OUT_SETUP) &&
            VPWSetup::fromLiteral(val->unsignedInt()).isStrideSetup();
    };
    const auto predVPMAccess = [isVPMRead](const intermediate::IntermediateInstruction* inst) -> bool {
        if(isVPMRead)
            return inst->readsRegister(REG_VPM_IO);
        return inst->writesRegister(REG_VPM_IO);
    };

    VPMInstructions result;

    // TODO could this select the wrong instructions for multiple VPM accesses within a single mutex-lock block?
    // XXX are multiple VPM accesses within a mutex-lock even possible without combining the setups and addresses?
    auto it = anyVPMInstruction;
    while(!it.isStartOfBlock())
    {
        // only look up to the next mutex (un)lock
        if(it.get<intermediate::MutexLock>())
            break;
        if(it.has())
        {
            if(!result.addressWrite && predAddressWrite(it.get()))
                result.addressWrite = it;
            if(!result.dmaSetup && predDMASetup(it.get()))
                result.dmaSetup = it;
            if(!result.dmaWait && predDMAWait(it.get()))
                result.dmaWait = it;
            if(!result.genericVPMSetup && predGenericSetup(it.get()))
                result.genericVPMSetup = it;
            if(!result.strideSetup && predStrideSetup(it.get()))
                result.strideSetup = it;
            if(!result.vpmAccess && predVPMAccess(it.get()))
                result.vpmAccess = it;
        }
        it.previousInBlock();
    }

    it = anyVPMInstruction;
    while(!it.isEndOfBlock())
    {
        // only look up to the next mutex (un)lock
        if(it.get<intermediate::MutexLock>())
            break;
        if(it.has())
        {
            if(!result.addressWrite && predAddressWrite(it.get()))
                result.addressWrite = it;
            if(!result.dmaSetup && predDMASetup(it.get()))
                result.dmaSetup = it;
            if(!result.dmaWait && predDMAWait(it.get()))
                result.dmaWait = it;
            if(!result.genericVPMSetup && predGenericSetup(it.get()))
                result.genericVPMSetup = it;
            if(!result.strideSetup && predStrideSetup(it.get()))
                result.strideSetup = it;
            if(!result.vpmAccess && predVPMAccess(it.get()))
                result.vpmAccess = it;
        }
        it.nextInBlock();
    }

    return result;
}

NODISCARD static InstructionWalker findGroupOfVPMAccess(
    periphery::VPM& vpm, InstructionWalker start, const InstructionWalker end, VPMAccessGroup& group)
{
    Optional<Value> baseAddress = NO_VALUE;
    int32_t nextOffset = -1;
    // the number of elements between two entries in memory
    group.stride = 0;
    group.groupType = TYPE_UNKNOWN;
    group.dmaSetups.clear();
    group.strideSetups.clear();
    group.genericSetups.clear();
    group.addressWrites.clear();
    group.dmaSetups.reserve(64);
    group.strideSetups.reserve(64);
    group.genericSetups.reserve(64);
    group.addressWrites.reserve(64);

    // FIXME to not build too large critical sections, only combine, if the resulting critical section:
    // 1) is not too large: either in total numbers of instructions or in ratio instructions / VPW writes, since we save
    // a few cycles per write (incl. delay for wait DMA)

    auto it = start;
    for(; !it.isEndOfBlock() && it != end; it.nextInBlock())
    {
        if(it.get() == nullptr)
            continue;

        if(it.get<intermediate::MemoryBarrier>())
            // memory barriers end groups, also don't check this barrier again
            return it.nextInBlock();
        if(it.get<intermediate::SemaphoreAdjustment>())
            // semaphore accesses end groups, also don't check this instruction again
            return it.nextInBlock();

        if(!(it->writesRegister(REG_VPM_DMA_LOAD_ADDR) || it->writesRegister(REG_VPM_DMA_STORE_ADDR)))
            // for simplicity, we only check for VPM addresses and find all other instructions relative to it
            continue;
        auto source = it->getMoveSource();
        if(!source)
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Setting VPM address with non-move is not supported", it->to_string());
        const auto baseAndOffset = findBaseAndOffset(*source);
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
                    findBaseAndOffset(group.addressWrites[0]->getMoveSource().value()).offset.value();
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

        auto vpmSetups = findRelatedVPMInstructions(it, !isVPMWrite);
        auto genericSetup = vpmSetups.genericVPMSetup;
        auto dmaSetup = vpmSetups.dmaSetup;
        auto strideSetup = vpmSetups.strideSetup;

        // check if the VPM and DMA configurations match with the previous one
        if(baseAddress)
        {
            if(!genericSetup || !dmaSetup)
                // either there are no setups for this VPM access, or they are not loaded from literals (e.g. dynamic
                // setup)
                break;
            auto genericValue = (*genericSetup)->precalculate().first & &Value::getLiteralValue;
            auto groupGenericValue = !group.genericSetups.empty() ?
                (group.genericSetups.at(0)->precalculate().first & &Value::getLiteralValue) :
                Optional<Literal>{};
            if(!genericValue || (groupGenericValue && genericValue != groupGenericValue))
                // generic setups do not match
                break;
            auto dmaValue = (*dmaSetup)->precalculate().first & &Value::getLiteralValue;
            auto groupDmaValue = !group.dmaSetups.empty() ?
                (group.dmaSetups.at(0)->precalculate().first & &Value::getLiteralValue) :
                Optional<Literal>{};
            if(!dmaValue || dmaValue != groupDmaValue)
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
        if(strideSetup)
            // not always given, e.g. if small stride
            group.strideSetups.push_back(strideSetup.value());
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

template <typename T>
static T wrapVPMSetup(intermediate::IntermediateInstruction* inst)
{
    if(auto load = dynamic_cast<intermediate::LoadImmediate*>(inst))
        return T{load};
    if(auto move = dynamic_cast<intermediate::MoveOperation*>(inst))
        return T{move};
    throw CompilationError(
        CompilationStep::OPTIMIZER, "Invalid instruction type to wrap to VPM setup", inst->to_string());
}

NODISCARD static bool groupVPMWrites(periphery::VPM& vpm, VPMAccessGroup& group)
{
    if(group.genericSetups.size() != group.addressWrites.size() ||
        group.genericSetups.size() != group.dmaSetups.size() || group.genericSetups.size() != group.strideSetups.size())
    {
        LCOV_EXCL_START
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Number of instructions do not match for combining VPM writes!" << logging::endl;
            logging::debug() << group.genericSetups.size() << " generic VPM setups, " << group.addressWrites.size()
                             << " VPR address writes, " << group.dmaSetups.size() << " DMA setups and "
                             << group.strideSetups.size() << " DMA stride setups" << logging::endl;
        });
        LCOV_EXCL_STOP
        return false;
    }
    if(group.addressWrites.size() <= 1)
        return false;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combining " << group.addressWrites.size() << " writes to consecutive memory into one DMA write... "
            << logging::endl);

    // 1. Update DMA setup to the number of rows written
    {
        auto dmaSetupValue = wrapVPMSetup<periphery::VPWSetupWrapper>(group.dmaSetups.at(0).get());
        dmaSetupValue.dmaSetup.setUnits(static_cast<uint8_t>(group.addressWrites.size()));
    }
    // 1.1 Update stride setup to the stride between rows
    {
        auto dmaSetupValue = wrapVPMSetup<periphery::VPWSetupWrapper>(group.dmaSetups.at(0).get());
        auto strideSetup = wrapVPMSetup<periphery::VPWSetupWrapper>(group.strideSetups.at(0).get());
        // stride is the distance in bytes from end of v1 to start of v2
        // This line is to calculate the stride in actually written vectors, e.g. for vload/vstore in scalar pointers
        // FIXME this is wrong for mixed vload/vstore, e.g. I/O with different vector types
        auto numElements = dmaSetupValue.dmaSetup.getDepth() / group.groupType.getElementType().getVectorWidth();
        auto strideInElements = static_cast<unsigned>(group.stride == 0 ? 0 : group.stride - numElements);
        strideSetup.strideSetup.setStride(
            static_cast<uint16_t>(strideInElements * group.groupType.getElementType().getInMemoryWidth()));
    }
    std::size_t numRemoved = 0;
    vpm.updateScratchSize(static_cast<unsigned char>(group.addressWrites.size()));

    // 2. Remove all but the first generic and DMA setups
    for(std::size_t i = 1; i < group.genericSetups.size(); ++i)
    {
        group.genericSetups[i].erase();
        group.strideSetups.at(i).erase();
        group.dmaSetups.at(i).erase();
        numRemoved += 3;
    }

    // 3. remove all but the last address writes (and the following DMA waits), update the last write to write the first
    // address written to
    group.addressWrites.back().get<intermediate::MoveOperation>()->setSource(
        Value(group.addressWrites.at(0).get<intermediate::MoveOperation>()->getSource()));
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
    return true;
}

NODISCARD static bool groupVPMReads(periphery::VPM& vpm, VPMAccessGroup& group)
{
    if(group.genericSetups.size() != group.addressWrites.size() ||
        group.genericSetups.size() != group.dmaSetups.size() || group.genericSetups.size() != group.strideSetups.size())
    {
        LCOV_EXCL_START
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Number of instructions do not match for combining VPM reads!" << logging::endl;
            logging::debug() << group.genericSetups.size() << " generic VPM setups, " << group.addressWrites.size()
                             << " VPR address writes, " << group.dmaSetups.size() << " DMA setups and "
                             << group.strideSetups.size() << " DMA stride setups" << logging::endl;
        });
        LCOV_EXCL_STOP
        return false;
    }
    if(group.genericSetups.size() <= 1)
        return false;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combining " << group.genericSetups.size() << " reads of consecutive memory into one DMA read... "
            << logging::endl);

    // 1. Update DMA setup to the number of rows read
    {
        auto dmaSetupValue = wrapVPMSetup<periphery::VPRSetupWrapper>(group.dmaSetups.at(0).get());
        dmaSetupValue.dmaSetup.setNumberRows(group.genericSetups.size() % 16);
        vpm.updateScratchSize(static_cast<unsigned char>(group.genericSetups.size()));
        // TODO can be space-optimized, half-words and bytes can be packed into single row (VPM writes too)
    }
    std::size_t numRemoved = 0;

    // 1.1 Update generic Setup to the number of rows read
    {
        auto genericSetup = wrapVPMSetup<periphery::VPRSetupWrapper>(group.genericSetups.at(0).get());
        genericSetup.genericSetup.setNumber(group.genericSetups.size() % 16);
    }

    // 1.2 Update stride setup for the stride used
    {
        auto strideSetup = wrapVPMSetup<periphery::VPRSetupWrapper>(group.strideSetups.at(0).get());
        // in contrast to writing memory, the pitch is the distance from start to start of successive rows
        strideSetup.strideSetup.setPitch(static_cast<uint16_t>(
            static_cast<unsigned>(group.stride) * group.groupType.getElementType().getInMemoryWidth()));
    }

    // 2. Remove all but the first generic and DMA setups
    for(std::size_t i = 1; i < group.genericSetups.size(); ++i)
    {
        group.genericSetups[i].erase();
        group.strideSetups.at(i).erase();
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
    return true;
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
bool optimizations::groupMemoryAccess(const Module& module, Method& method, const Configuration& config)
{
    // TODO for now, this cannot handle RAM->VPM, VPM->RAM only access as well as VPM->QPU or QPU->VPM
    bool didChanges = false;

    // run within all basic blocks
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            VPMAccessGroup group;
            it = findGroupOfVPMAccess(*method.vpm, it, block.walkEnd(), group);
            if(group.addressWrites.size() > 1)
            {
                group.cleanDuplicateInstructions();
                auto func = group.isVPMWrite ? groupVPMWrites : groupVPMReads;
                if(func(*method.vpm, group))
                {
                    didChanges = true;
                    PROFILE_COUNTER(
                        vc4c::profiler::COUNTER_OPTIMIZATION + 6000, "DMA access groups", group.genericSetups.size());
                }
            }
        }
    }

    // clean up empty instructions
    if(didChanges)
        method.cleanEmptyInstructions();
    return didChanges;
}

struct TMULoadOffset
{
    const Local* baseLocal;
    analysis::InductionVariable inductionVariable;
    SubExpression offsetExpression;
};

static FastMap<InstructionWalker, TMULoadOffset> findTMULoadsInLoop(
    analysis::ControlFlowLoop& loop, Method& method, const analysis::DataDependencyGraph& dependencyGraph)
{
    std::array<FastMap<InstructionWalker, TMULoadOffset>, 2> relevantTMULoads{};
    std::array<unsigned, 2> numTMULoads{};
    auto inductionVariables = loop.findInductionVariables(dependencyGraph, false);
    const auto* globalDataAddress = method.findBuiltin(BuiltinLocal::Type::GLOBAL_DATA_ADDRESS);
    for(auto node : loop)
    {
        for(auto it = node->key->walk(); !it.isEndOfBlock(); it.nextInBlock())
        {
            auto ramAccess = it.get<intermediate::RAMAccessInstruction>();
            if(ramAccess && ramAccess->getTMUCacheEntry())
            {
                auto cacheEntry = ramAccess->getTMUCacheEntry();
                ++numTMULoads[cacheEntry->getTMUIndex()];

                auto addressWriter = ramAccess->getMemoryAddress().getSingleWriter();
                if(!addressWriter)
                    continue;

                auto expr = Expression::createRecursiveExpression(*addressWriter);
                if(!expr || expr->code != OP_ADD)
                    continue;

                const Local* baseLocal = nullptr;
                SubExpression addressOffset{};
                auto leftLocal = expr->arg0.checkLocal();
                if(leftLocal &&
                    (leftLocal->is<Parameter>() || leftLocal->residesInMemory() || leftLocal == globalDataAddress))
                {
                    baseLocal = leftLocal;
                    addressOffset = expr->arg1;
                }
                auto rightLocal = expr->arg1.checkLocal();
                if(rightLocal &&
                    (rightLocal->is<Parameter>() || rightLocal->residesInMemory() || rightLocal == globalDataAddress))
                {
                    baseLocal = rightLocal;
                    addressOffset = expr->arg0;
                }

                // TODO allow also for base address + offset + induction-variable depending offset
                // does this allow for any more hits??

                if(!baseLocal)
                    continue;

                const analysis::InductionVariable* matchingInductionVar = nullptr;

                if(auto offsetLoc = addressOffset.checkLocal())
                {
                    auto varIt = std::find_if(inductionVariables.begin(), inductionVariables.end(),
                        [&](const analysis::InductionVariable& var) -> bool { return var.local == offsetLoc; });
                    if(varIt != inductionVariables.end())
                        matchingInductionVar = &(*varIt);
                }
                else if(auto offsetExpr = addressOffset.checkExpression())
                {
                    auto varIt = std::find_if(inductionVariables.begin(), inductionVariables.end(),
                        [&](const analysis::InductionVariable& var) -> bool {
                            return (var.local == offsetExpr->arg0.checkLocal() &&
                                       offsetExpr->arg1.getConstantExpression()) ||
                                (var.local == offsetExpr->arg1.checkLocal() &&
                                    offsetExpr->arg0.getConstantExpression());
                        });
                    if(varIt != inductionVariables.end())
                        matchingInductionVar = &(*varIt);
                }

                if(!matchingInductionVar)
                    continue;

                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found TMU RAM address derived from induction variable with base '" << baseLocal->to_string()
                        << "' and offset: " << addressOffset.to_string() << " (induction variable: "
                        << matchingInductionVar->local->to_string() << ')' << logging::endl);

                relevantTMULoads[cacheEntry->getTMUIndex()].emplace(
                    it, TMULoadOffset{baseLocal, *matchingInductionVar, addressOffset});
            }
        }
    }

    // TODO for now only optimize for a single TMU load.
    // We could extend this up to 2/4/8? TMU loads (per TMU?), if we can guarantee the order
    // Tests on hardware running Pearson16 have shown that even allowing single load per TMU causes wrong values to be
    // loaded, same as on emulator
    if(numTMULoads[0] + numTMULoads[1] != 1)
        return {};

    FastMap<InstructionWalker, TMULoadOffset> result;
    result.insert(relevantTMULoads[0].begin(), relevantTMULoads[0].end());
    result.insert(relevantTMULoads[1].begin(), relevantTMULoads[1].end());

    return result;
}

static Value calculateAddress(InstructionWalker& it, const analysis::InductionVariable& inductionVariable,
    const SubExpression& offsetExpression, Method& method, DataType addressType, const Local* baseLocal)
{
    Value tmpOffset = inductionVariable.local->createReference();
    if(auto expr = offsetExpression.checkExpression())
    {
        tmpOffset = method.addNewLocal(inductionVariable.local->type, "%prefetch_tmu_offset");
        it.emplace(expr->toInstruction(tmpOffset));
        it.nextInBlock();
    }
    return assign(it, addressType, "%prefetch_tmu_address") = (baseLocal->createReference() + tmpOffset);
}

NODISCARD static bool prefetchTMULoadsInLoop(analysis::ControlFlowLoop& loop, Method& method,
    const analysis::DataDependencyGraph& dependencyGraph, const analysis::DominatorTree& dominators)
{
    auto preheader = loop.findPreheader(dominators);
    auto successor = loop.findSuccessor();
    auto header = loop.getHeader();
    auto tail = loop.getTail();
    auto repeatEdge = tail->getEdge(header);

    if(!preheader || !successor || !header || !tail || !repeatEdge)
        // fail fast, since we won't be able to insert the moved/copied instructions anywhere
        return false;

    auto matchingTMULoads = findTMULoadsInLoop(loop, method, dependencyGraph);

    for(auto& load : matchingTMULoads)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Prefetching TMU RAM load in loop '" << loop.to_string(false) << "': " << load.first->to_string()
                << logging::endl);
        auto initialPrefetchIt = preheader->key->findWalkerForInstruction(
            load.second.inductionVariable.initialAssignment, preheader->key->walkEnd());
        auto successivePrefetchIt = repeatEdge->data.getPredecessor(tail->key);
        if(!initialPrefetchIt)
            continue;

        auto originalAccess = load.first.get<intermediate::RAMAccessInstruction>();

        // move original TMU RAM load before the loop
        auto it = initialPrefetchIt->copy().nextInBlock();
        auto assignmentInst = initialPrefetchIt->get<intermediate::ExtendedInstruction>();
        if(assignmentInst && assignmentInst->hasConditionalExecution())
        {
            /*
             * In some cases where the loop might be skipped completely, the induction variable is only written
             * conditionally (if the loop will be taken).
             *
             * To make sure the address offset is written unconditionally (since we cannot conditionally read from
             * memory), insert a dummy offset in case the loop is not entered.
             */
            if(auto constant = assignmentInst->getMoveSource() & &Value::getLiteralValue)
                // if we write a constant value (have no data dependencies) just make the assignment unconditional
                assignmentInst->setCondition(COND_ALWAYS);
            else
                assign(it, load.second.inductionVariable.local->createReference()) =
                    (INT_ZERO, assignmentInst->getCondition().invert());
        }
        auto firstIterationAddress = calculateAddress(it, load.second.inductionVariable, load.second.offsetExpression,
            method, originalAccess->getMemoryAddress().type, load.second.baseLocal);
        it.emplace(const_cast<InstructionWalker&>(load.first).release());
        it.get<intermediate::RAMAccessInstruction>()->setMemoryAddress(firstIterationAddress);

        // add instruction prefetching the value for the next iteration into the TMU FIFO
        it = successivePrefetchIt.copy().previousInBlock();
        auto nextIterationAddress = calculateAddress(it, load.second.inductionVariable, load.second.offsetExpression,
            method, originalAccess->getMemoryAddress().type, load.second.baseLocal);
        if(auto branch = successivePrefetchIt.get<intermediate::Branch>())
        {
            /*
             * If the loop is not repeated anymore (this is our last iteration), we would prefetch a memory address
             * which is not intended to be addressed and therefore might not be allocated at all.
             *
             * To mitigate this, we re-load the first address instead, if we don't repeat the loop anymore. This memory
             * address should already be cached and also has already been accessed, so we know we can access it anyway.
             */
            auto branchCond = branch->branchCondition;
            if(branch->getSingleTargetLabel() == header->key->getLabel()->getLabel())
                branchCond = branchCond.invert();
            assign(it, nextIterationAddress) = (firstIterationAddress, branchCond.toConditionCode());
        }
        it.emplace(new intermediate::RAMAccessInstruction(
            intermediate::MemoryOperation::READ, nextIterationAddress, originalAccess->cache));

        // add instruction to drain the TMU FIFO after loop
        bool successorFollowsPreheader = false;
        successor->forAllIncomingEdges(
            [&](const analysis::CFGNode& predecessor, const analysis::CFGEdge& edge) -> bool {
                if(&predecessor == preheader)
                {
                    successorFollowsPreheader = true;
                    return false;
                }
                return true;
            });
        auto discardIt = successor->key->walk();
        if(!successorFollowsPreheader)
        {
            /*
             * If we have a proper preheader (i.e. a block from which control flow unconditionally jumps into the loop),
             * this preheader block is not executed if the loop is not taken at all. Since we do insert our prefetch TMU
             * RAM access into that block, no data is prefetched at all if the loop is not taken at all.
             *
             * To not hang indefinitely on the TMU FIFO drain instruction if the loop is not taken, we need to make sure
             * the drain is also only executed if the loop is actually taken.
             *
             * TODO improve on this by always inserting a proper preheader block and inserting the prefetch there? And
             * then also always insert a separate loop successor block which is only reached from the loop body?
             */
            auto newLabel = method.addNewLocal(TYPE_LABEL, "%loop_successor");
            discardIt = method.emplaceLabel(successor->key->walk(), new intermediate::BranchLabel(*newLabel.local()));

            auto exitEdge = loop.findExitEdge();
            if(!exitEdge)
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Failed to determine exit edge for TMU load prefetch", loop.to_string());
            if(exitEdge->isOutput(*successor))
            {
                // if the old successor block still has an edge from the loop itself (i.e. the edge from the loop to the
                // old successor block was not a fall-through), we need to redirect this edge to the new successor
                // block.
                // TODO make all this way cleaner (for all possible CFG constellations) and move to control flow loop?!
                auto& exitNode = exitEdge->getOtherNode(*successor);
                auto exitBranch = exitEdge->data.getPredecessor(exitNode.key).get<intermediate::Branch>();
                if(exitBranch && exitBranch->getSingleTargetLabel() == successor->key->getLabel()->getLabel())
                    exitBranch->setTarget(newLabel.local());
                else
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Unhandled case of redirecting loop successor branch for TMU prefetch drain", loop.to_string());
            }
        }

        it = discardIt.nextInBlock();
        it.emplace(new intermediate::CacheAccessInstruction(
            intermediate::MemoryOperation::READ, NOP_REGISTER, originalAccess->cache));
    }

    return !matchingTMULoads.empty();
}

static bool containsSynchronizationInstruction(const analysis::ControlFlowLoop& loop)
{
    for(auto block : loop)
    {
        for(auto it = block->key->walk(); it.isEndOfBlock(); it.nextInBlock())
        {
            if(it.get<intermediate::MemoryBarrier>() || it.get<intermediate::SemaphoreAdjustment>())
                return true;
        }
    }
    return false;
}

bool optimizations::prefetchTMULoads(const Module& module, Method& method, const Configuration& config)
{
    // 1. find (innermost) loops
    // 2. check number of TMU loads (per TMU)
    // 3. try to determine TMU addresses and whether they are derived from induction variable/can be statically
    // pre-computed
    // 4. move first load (RAM access) out of loop, further loads to previous iteration
    // 5. insert dropping of pre-loaded value after loop

    auto& cfg = method.getCFG();
    auto dominatorTree = analysis::DominatorTree::createDominatorTree(cfg);
    auto loops = cfg.findLoops(false, true, dominatorTree.get());
    auto dependencyGraph = analysis::DataDependencyGraph::createDependencyGraph(method);

    bool movedLoads = false;

    for(auto& loop : loops)
    {
        if(loop.size() > 1)
            /*
             * For now do not prefetch from loops with multiple blocks.
             *
             * TODO If we allow this, we need to have special handling for conditional loads (e.g. if-block in loop) and
             * either skip those explicitly (TMU RAM loads from within conditional blocks) or in the else-block (if
             * there is none, insert one) also discard the loaded value. This might lead to accessing out-of-bounds
             * memory?!
             *
             * Examples test-cases for this are vectorization17, vectorization19
             */
            continue;
        if(containsSynchronizationInstruction(loop))
            continue;
        if(prefetchTMULoadsInLoop(loop, method, *dependencyGraph, *dominatorTree))
            movedLoads = true;
    }

    if(movedLoads)
        method.cleanEmptyInstructions();

    return movedLoads;
}
