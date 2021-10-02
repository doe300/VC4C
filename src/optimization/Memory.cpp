/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Memory.h"

#include "../Expression.h"
#include "../GlobalValues.h"
#include "../Method.h"
#include "../Module.h"
#include "../Profiler.h"
#include "../SIMDVector.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/ControlFlowLoop.h"
#include "../analysis/DataDependencyGraph.h"
#include "../analysis/DominatorTree.h"
#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../intrinsics/Intrinsics.h"
#include "../performance.h"
#include "../periphery/RegisterLoweredMemory.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <array>
#include <cmath>

using namespace vc4c;
using namespace vc4c::operators;

// TODO rewrite combination of VPM accesses to use MemoryAccessInstructions instead? Need to move before memory lowering
// and somehow be able to explicitly set stride/etc. If rewritten, merge with groupTMUAccess optimization step

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
                else if(memoryAccess->getRegisterCacheEntry())
                    it = periphery::lowerRegisterAccess(method, it);
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
    const Local* baseAddress;
    SubExpression dynamicOffset;
    SubExpression workGroupConstantOffset;
};

static std::pair<SubExpression, SubExpression> findOffsets(const SubExpression& expr)
{
    if(auto constantOffset = expr.getConstantExpression())
        return std::make_pair(SubExpression{INT_ZERO}, SubExpression{*constantOffset});

    if(auto otherExpr = expr.checkExpression())
        return otherExpr->splitIntoDynamicAndConstantPart(
            true, add_flag(ExpressionOptions::ALLOW_FAKE_OPS, ExpressionOptions::STOP_AT_BUILTINS));
    return std::make_pair(expr, SubExpression{INT_ZERO});
}

static Optional<BaseAndOffset> findBaseAndOffset(const Value& address)
{
    const auto* loc = intermediate::getSourceValue(address).checkLocal();
    if(!loc)
        loc = address.checkLocal();
    if(!loc)
        return {};

    if(loc->residesInMemory())
        // direct access of memory location
        return BaseAndOffset{loc, INT_ZERO, INT_ZERO};

    auto addressWriter = loc->getSingleWriter();
    auto expr = addressWriter ? Expression::createRecursiveExpression(*addressWriter, 24) : nullptr;
    if(expr && expr->isMoveExpression() && expr->arg0.checkLocal(true))
        return BaseAndOffset{expr->arg0.checkLocal(true), INT_ZERO, INT_ZERO};
    if(!expr || expr->code != OP_ADD)
        return {};

    // direct base + offset
    if(auto loc = expr->arg0.checkLocal())
    {
        auto offsets = findOffsets(expr->arg1);
        return BaseAndOffset{loc, offsets.first, offsets.second};
    }
    if(auto loc = expr->arg1.checkLocal())
    {
        auto offsets = findOffsets(expr->arg0);
        return BaseAndOffset{loc, offsets.first, offsets.second};
    }

    // (base + offset) + some more offset
    auto offsets = findOffsets(expr);
    auto constantExpr = offsets.second.checkExpression();
    if(constantExpr && constantExpr->code == OP_ADD)
    {
        // the base local is considered work-group uniform, if it is a parameter or otherwise set in a work-group
        // uniform fashion
        // flatten the expression structure to be able to extract the base local from any (top-level associative)
        // position, e.g. from loc + (one + other) as well as one + (loc + other), etc.
        auto constantParts = constantExpr->getAssociativeParts();
        auto localIt = constantParts.end();
        for(auto it = constantParts.begin(); it != constantParts.end(); ++it)
        {
            if(it->checkLocal())
            {
                if(localIt != constantParts.end())
                    // multiple uniform locals, don't know which is base
                    // TODO could do some more advanced checking here, e.g. check for pointer type, reference base, etc.
                    return {};
                localIt = it;
            }
        }
        if(localIt == constantParts.end())
            // no uniform base local found
            return {};

        // no we need to assemble the constant parts again, just without the local
        SubExpression constantPart{INT_ZERO};
        for(auto it = constantParts.begin(); it != constantParts.end(); ++it)
        {
            if(it == localIt)
                // don't add the base local
                continue;
            if(constantPart.getLiteralValue() == 0_lit)
                // rewrite our default 0 + x to just x
                constantPart = *it;
            else
                constantPart = std::make_shared<Expression>(OP_ADD, constantPart, *it);
        }
        return BaseAndOffset{localIt->checkLocal(), offsets.first, constantPart};
    }
    else if(auto loc = offsets.second.checkLocal(true))
        return BaseAndOffset{loc, offsets.first, INT_ZERO};

    return {};
}

struct VPMAccessGroup
{
    bool isVPMWrite;
    DataType groupType = TYPE_UNKNOWN;
    FastAccessList<InstructionWalker> dmaSetups;
    FastAccessList<InstructionWalker> strideSetups;
    FastAccessList<InstructionWalker> genericSetups;
    FastAccessList<InstructionWalker> addressWrites;
    // this is the distance/offset (start of row to start of row, 1 = consecutive) between two vectors in bytes
    unsigned stride = 1;

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
    periphery::VPM& vpm, InstructionWalker start, VPMAccessGroup& group)
{
    const Local* baseAddress = nullptr;
    SubExpression dynamicOffset{};
    SubExpression initialConstantOffset{};
    SubExpression nextConstantOffset{};
    // the number of bytes between two entries in memory
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
    for(; !it.isEndOfBlock(); it.nextInBlock())
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

        if(!baseAndOffset || !baseAndOffset->baseAddress)
            // this address-write could not be fixed to a base and an offset
            // skip this address write for the next check
            return it.nextInBlock();

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Found base address " << baseAndOffset->baseAddress->to_string() << " with dynamic offset "
                << baseAndOffset->dynamicOffset.to_string() << " and work-group uniform offset "
                << baseAndOffset->workGroupConstantOffset.to_string() << " for "
                << (isVPMWrite ? "writing into" : "reading from") << " memory" << logging::endl);

        if(baseAndOffset->baseAddress->is<Parameter>() &&
            has_flag(baseAndOffset->baseAddress->as<Parameter>()->decorations, ParameterDecorations::VOLATILE))
            // address points to a volatile parameter, which explicitly forbids combining reads/writes
            // skip this address write for the next check
            return it.nextInBlock();

        // check if this address is consecutive to the previous one (if any)
        if(baseAddress)
        {
            if(baseAndOffset->baseAddress != baseAddress)
                // a group exists, but the base addresses don't match
                break;
            if(baseAndOffset->dynamicOffset != dynamicOffset)
                // a group exists, but the dynamic offsets don't match
                break;
            if(group.addressWrites.size() == 1 && group.stride == 0)
            {
                // special case for first offset - use it to determine stride
                auto strideVal =
                    std::make_shared<Expression>(OP_SUB, baseAndOffset->workGroupConstantOffset, initialConstantOffset)
                        ->combineWith({})
                        ->getConstantExpression() &
                    &Value::getLiteralValue;
                if(!strideVal || strideVal->signedInt() <= 0)
                    // a group exists, but can't calculate a constant stride literal
                    break;
                group.stride = strideVal->unsignedInt();
                nextConstantOffset = baseAndOffset->workGroupConstantOffset;
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Using a stride of " << group.stride << " bytes between consecutive access to memory"
                        << logging::endl);
            }
            if(baseAndOffset->workGroupConstantOffset != nextConstantOffset)
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
                (group.genericSetups.front()->precalculate().first & &Value::getLiteralValue) :
                Optional<Literal>{};
            if(!genericValue || (groupGenericValue && genericValue != groupGenericValue))
                // generic setups do not match
                break;
            auto dmaValue = (*dmaSetup)->precalculate().first & &Value::getLiteralValue;
            auto groupDmaValue = !group.dmaSetups.empty() ?
                (group.dmaSetups.front()->precalculate().first & &Value::getLiteralValue) :
                Optional<Literal>{};
            if(!dmaValue || dmaValue != groupDmaValue)
                // DMA setups do not match
                break;
            auto dmaSetup = periphery::VPWSetup{dmaValue->unsignedInt()}.dmaSetup;
            if(!dmaSetup.getHorizontal() && dmaSetup.getDepth() == 2)
                // writing 2 rows (with N columns) vertically means 64-bit write, which cannot be combined, reordering
                // of the actual VPM writes would be required
                break;
        }

        // check for complex types
        DataType elementType = baseAndOffset->baseAddress->type.getPointerType() ?
            baseAndOffset->baseAddress->type.getPointerType()->elementType :
            baseAndOffset->baseAddress->type;
        elementType = elementType.getArrayType() ? elementType.getArrayType()->elementType : elementType;
        if(!elementType.isSimpleType())
            // XXX for now, skip combining any access to complex types (here: only struct, image)
            // don't check this read/write again
            return it.nextInBlock();

        // all matches so far, add to group (or create a new one)
        group.isVPMWrite = isVPMWrite;
        group.groupType = source->type;
        baseAddress = baseAndOffset->baseAddress;
        dynamicOffset = baseAndOffset->dynamicOffset;
        initialConstantOffset = baseAndOffset->workGroupConstantOffset;
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
        nextConstantOffset =
            std::make_shared<Expression>(OP_ADD, nextConstantOffset, Value(Literal(group.stride), TYPE_INT32))
                ->combineWith({});
        if(auto constantOffset = nextConstantOffset.getConstantExpression())
            nextConstantOffset = *constantOffset;

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
        auto dmaSetupValue = wrapVPMSetup<periphery::VPWSetupWrapper>(group.dmaSetups.front().get());
        dmaSetupValue.dmaSetup.setUnits(static_cast<uint8_t>(group.addressWrites.size()));
    }
    // 1.1 Update stride setup to the stride between rows
    {
        auto dmaSetupValue = wrapVPMSetup<periphery::VPWSetupWrapper>(group.dmaSetups.front().get());
        auto strideSetup = wrapVPMSetup<periphery::VPWSetupWrapper>(group.strideSetups.front().get());
        // stride is the distance in bytes from end of v1 to start of v2
        // This line is to calculate the stride in actually written vectors, e.g. for vload/vstore in scalar pointers
        // FIXME this is wrong for mixed vload/vstore, e.g. I/O with different vector types
        auto numElements = dmaSetupValue.dmaSetup.getDepth() / group.groupType.getElementType().getVectorWidth();
        auto numBytes = static_cast<unsigned>(numElements) * group.groupType.getElementType().getInMemoryWidth();
        strideSetup.strideSetup.setStride(static_cast<uint16_t>(group.stride == 0 ? 0 : (group.stride - numBytes)));
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
        Value(group.addressWrites.front().get<intermediate::MoveOperation>()->getSource()));
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
        auto dmaSetupValue = wrapVPMSetup<periphery::VPRSetupWrapper>(group.dmaSetups.front().get());
        dmaSetupValue.dmaSetup.setNumberRows(group.genericSetups.size() % 16);
        vpm.updateScratchSize(static_cast<unsigned char>(group.genericSetups.size()));
        // TODO can be space-optimized, half-words and bytes can be packed into single row (VPM writes too)
    }
    std::size_t numRemoved = 0;

    // 1.1 Update generic Setup to the number of rows read
    {
        auto genericSetup = wrapVPMSetup<periphery::VPRSetupWrapper>(group.genericSetups.front().get());
        genericSetup.genericSetup.setNumber(group.genericSetups.size() % 16);
    }

    // 1.2 Update stride setup for the stride used
    {
        auto strideSetup = wrapVPMSetup<periphery::VPRSetupWrapper>(group.strideSetups.front().get());
        // in contrast to writing memory, the pitch is the distance from start to start of successive rows
        strideSetup.strideSetup.setPitch(static_cast<uint16_t>(group.stride));
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

bool optimizations::groupVPMAccess(const Module& module, Method& method, const Configuration& config)
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
            it = findGroupOfVPMAccess(*method.vpm, it, group);
            if(group.addressWrites.size() > 1)
            {
                group.cleanDuplicateInstructions();
                auto func = group.isVPMWrite ? groupVPMWrites : groupVPMReads;
                if(func(*method.vpm, group))
                {
                    didChanges = true;
                    PROFILE_COUNTER(
                        vc4c::profiler::COUNTER_OPTIMIZATION, "DMA access groups", group.genericSetups.size());
                }
            }
        }
    }

    // clean up empty instructions
    if(didChanges)
        method.cleanEmptyInstructions();
    return didChanges;
}

struct TMUAccessGroup
{
    std::shared_ptr<periphery::TMUCacheEntry> cacheEntry;
    FastAccessList<InstructionWalker> ramReads;
    FastAccessList<InstructionWalker> cacheLoads;
    FastAccessList<SubExpression> uniformAddressParts;
    uint8_t usedVectorSize;
};

NODISCARD static InstructionWalker findGroupOfTMUAccess(InstructionWalker it, TMUAccessGroup& group)
{
    BaseAndOffset groupBaseAndOffset;
    for(; !it.isEndOfBlock(); it.nextInBlock())
    {
        if(auto load = it.get<intermediate::RAMAccessInstruction>())
        {
            auto tmuCacheEntry = load->getTMUCacheEntry();
            if(load->op == intermediate::MemoryOperation::READ && tmuCacheEntry)
            {
                auto cacheReadInst = tmuCacheEntry->getCacheReader();
                auto cacheReadIt = cacheReadInst ?
                    it.getBasicBlock()->findWalkerForInstruction(cacheReadInst, it, it.getBasicBlock()->walkEnd()) :
                    Optional<InstructionWalker>{};
                if(!cacheReadIt)
                    // this load does not have exactly 1 cache read (or we could not find it), so skip
                    return it.nextInBlock();

                auto numVectorElements = tmuCacheEntry->numVectorElements.getLiteralValue();
                if(!numVectorElements)
                    // this load reads a non-constant number of vector elements
                    // skip this address write for the next check
                    return it.nextInBlock();

                // check if the TMU accessed is the same as for the group (if any)
                if(group.cacheEntry && group.cacheEntry->getTMUIndex() != tmuCacheEntry->getTMUIndex())
                    break;

                const auto baseAndOffset = findBaseAndOffset(load->getMemoryAddress());

                if(!baseAndOffset || !baseAndOffset->baseAddress)
                    // this address-write could not be fixed to a base and an offset
                    // skip this address write for the next check
                    return it.nextInBlock();

                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found base address " << baseAndOffset->baseAddress->to_string() << " with dynamic offset "
                        << baseAndOffset->dynamicOffset.to_string() << ", work-group uniform offset "
                        << baseAndOffset->workGroupConstantOffset.to_string() << " and element stride "
                        << tmuCacheEntry->elementStrideInBytes << " for "
                        << "reading from memory via TMU" << logging::endl);

                if(baseAndOffset->baseAddress->is<Parameter>() &&
                    has_flag(baseAndOffset->baseAddress->as<Parameter>()->decorations, ParameterDecorations::VOLATILE))
                    // address points to a volatile parameter, which explicitly forbids combining reads/writes
                    // skip this address write for the next check
                    return it.nextInBlock();

                if(group.ramReads.empty())
                {
                    // create new group
                    groupBaseAndOffset = *baseAndOffset;
                    group.cacheEntry = tmuCacheEntry;
                    group.usedVectorSize = 0;
                }

                // check whether this read belongs to the same group
                if(groupBaseAndOffset.baseAddress != baseAndOffset->baseAddress)
                    // a group exists, but the base addresses don't match
                    break;
                if(groupBaseAndOffset.dynamicOffset != baseAndOffset->dynamicOffset)
                    // a group exists, but the dynamic offsets don't match
                    break;

                // check whether we load the same type width for all loads
                // XXX we could combine those, but then we would need to handle the value extraction on a
                // per-replaced-cache-read basis instead of the current initial value extraction.
                if(group.cacheEntry->elementStrideInBytes != tmuCacheEntry->elementStrideInBytes)
                    // a group exists, but the type-width do not match
                    break;

                if(static_cast<uint8_t>(group.usedVectorSize + numVectorElements->unsignedInt()) > NATIVE_VECTOR_SIZE)
                    // group is too big when adding this new load
                    break;

                // add to group
                group.usedVectorSize += static_cast<uint8_t>(numVectorElements->unsignedInt());
                group.ramReads.emplace_back(it);
                group.cacheLoads.emplace_back(*cacheReadIt);
                group.uniformAddressParts.emplace_back(baseAndOffset->workGroupConstantOffset);
            }
        }
    }
    return it;
}

NODISCARD static InstructionWalker insertCustomAddressesCalculation(InstructionWalker it, Method& method,
    const Value& baseAddress, const Value& addressOffsets, uint8_t numVectorElements, const Value& outputAddress)
{
    Value replicatedAddress = baseAddress;
    if(!baseAddress.isAllSame())
    {
        replicatedAddress = method.addNewLocal(
            method.createPointerType(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE)), "%tmu_group_replicated_address");
        it = intermediate::insertReplication(it, baseAddress, replicatedAddress);
    }

    assign(it, outputAddress) =
        (replicatedAddress + addressOffsets, intermediate::InstructionDecorations::UNSIGNED_RESULT);
    /*
     * Since the address offsets might be large (or have undefined values for the not-accessed upper SIMD vector
     * elements), loading these addresses might read unallocated memory. To mitigate this, only set the addresses for
     * the elements actually loaded.
     *
     * See TMU.cpp#insertCalculateAddressOffsets(...) for details.
     */
    if(numVectorElements != NATIVE_VECTOR_SIZE)
    {
        auto cond = assignNop(it) =
            as_signed{ELEMENT_NUMBER_REGISTER} >= as_signed{Value(Literal(numVectorElements), TYPE_INT8)};
        assign(it, outputAddress) = (INT_ZERO, cond);
    }

    return it;
}

static std::vector<SubExpression> getAddParts(const SubExpression& expr)
{
    if(auto exp = expr.checkExpression())
    {
        if(exp->code == OP_ADD)
            return exp->getAssociativeParts();
    }
    return {expr};
}

static std::vector<SubExpression> addConstantOffset(const std::vector<SubExpression>& parts, uint32_t offset)
{
    std::vector<SubExpression> result = parts;
    for(auto& res : result)
    {
        auto val = res.checkValue();
        if(auto lit = val & &Value::getLiteralValue)
        {
            // update existing constant value
            res = Value(Literal(lit->unsignedInt() + offset), val->type);
            return result;
        }
    }
    // add new constant entry
    result.emplace_back(Value(Literal(offset), TYPE_INT32));
    return result;
}

NODISCARD static bool checkSecondAddressCalculation(InstructionWalker start, InstructionWalker end,
    const Local* openLocal, FastAccessList<InstructionWalker>& calculatingInstructions)
{
    if(!openLocal)
        return false;

    while(start != end)
    {
        if(start.has() && start->writesLocal(openLocal))
        {
            if(start->hasConditionalExecution() || start->readsRegister())
                // just to keep it simple, abort on conditional writes and e.g. replications
                return false;
            calculatingInstructions.emplace_back(start);
            for(auto& arg : start->getArguments())
            {
                if(auto loc = arg.checkLocal())
                {
                    if(!checkSecondAddressCalculation(
                           start.copy().previousInBlock(), end, loc, calculatingInstructions))
                        return false;
                }
            }
            return true;
        }
        start.previousInBlock();
    }
    // local was not written at all in between, so it has to (assuming the code before was correct) be written before
    // the end iterator and thus is already available
    return true;
}

static std::pair<std::vector<SubExpression>, std::vector<std::vector<SubExpression>>> getElementOffsetsAndStrides(
    TMUAccessGroup& group, const std::vector<SubExpression>& baseUniformParts)
{
    auto previousUniformOffset = group.uniformAddressParts.front();
    std::vector<SubExpression> elementStrides;
    std::vector<std::vector<SubExpression>> elementOffsetsParts;
    elementStrides.reserve(group.usedVectorSize);
    elementOffsetsParts.reserve(group.usedVectorSize);
    for(std::size_t i = 0; i < group.ramReads.size(); ++i)
    {
        auto cacheEntry = group.ramReads[i].get<intermediate::RAMAccessInstruction>()->getTMUCacheEntry();
        auto numVectorElements = cacheEntry->numVectorElements.getLiteralValue().value().unsignedInt();
        auto loadStrideExpr = std::make_shared<Expression>(OP_SUB, group.uniformAddressParts[i], previousUniformOffset)
                                  ->combineWith({},
                                      add_flag(ExpressionOptions::ALLOW_FAKE_OPS, ExpressionOptions::STOP_AT_BUILTINS,
                                          ExpressionOptions::RECURSIVE));

        std::vector<SubExpression> uniformOffsetParts;
        if(i == 0)
            uniformOffsetParts = baseUniformParts;
        else
        {
            uniformOffsetParts = getAddParts(group.uniformAddressParts.at(i));
            uniformOffsetParts.erase(std::remove_if(uniformOffsetParts.begin(), uniformOffsetParts.end(),
                                         [&](const SubExpression& part) -> bool {
                                             return !part.getLiteralValue() &&
                                                 std::find(baseUniformParts.begin(), baseUniformParts.end(), part) !=
                                                 baseUniformParts.end();
                                         }),
                uniformOffsetParts.end());
        }
        if(uniformOffsetParts.empty())
            uniformOffsetParts.emplace_back(INT_ZERO);

        /*
         * For the elements loaded by a single RAM load, the following strides (to the previous element in the resulting
         * RAM load) are calculated:
         * - the first element has a stride of: work-group uniform offset - previous stride
         * - all other elements have a stride of the fixed stride value in the TMU cache entry
         */
        SubExpression loadStride = loadStrideExpr;
        if(auto constantStride = loadStrideExpr->getConstantExpression())
            loadStride = *constantStride;

        elementStrides.emplace_back(loadStride);
        elementOffsetsParts.emplace_back(uniformOffsetParts);

        for(uint8_t i = 1; i < numVectorElements; ++i)
        {
            elementStrides.emplace_back(Value(Literal(cacheEntry->elementStrideInBytes), TYPE_INT32));
            elementOffsetsParts.emplace_back(
                addConstantOffset(uniformOffsetParts, i * cacheEntry->elementStrideInBytes));
        }

        std::shared_ptr<Expression> nextOffsetExpr;
        if(numVectorElements == 1)
            nextOffsetExpr = group.uniformAddressParts[i].checkExpression() ?
                group.uniformAddressParts[i].checkExpression() :
                std::make_shared<Expression>(OP_V8MIN, group.uniformAddressParts[i], group.uniformAddressParts[i]);
        else
            nextOffsetExpr = std::make_shared<Expression>(OP_ADD, group.uniformAddressParts[i],
                Value(Literal((numVectorElements - 1) * cacheEntry->elementStrideInBytes), TYPE_INT32))
                                 ->combineWith({},
                                     add_flag(ExpressionOptions::ALLOW_FAKE_OPS, ExpressionOptions::STOP_AT_BUILTINS,
                                         ExpressionOptions::RECURSIVE));
        previousUniformOffset = nextOffsetExpr;
        if(auto constantOffset = nextOffsetExpr->getConstantExpression())
            previousUniformOffset = *constantOffset;
    }
    return std::make_pair(std::move(elementStrides), std::move(elementOffsetsParts));
}

struct HierarchicalStrides
{
    uint32_t powerOfTwo;
    std::shared_ptr<Expression> primaryStride;
    Literal secondaryStride;
};

static Optional<HierarchicalStrides> checkTwoDistinctStrides(const std::vector<SubExpression>& elementStrides)
{
    if(elementStrides.size() < 3)
        return {};

    // ignore stride of 0th element, since there is no previous element and therefore no stride to it
    auto secondaryStride = elementStrides.at(1).getConstantExpression() & &Value::getLiteralValue;

    // only allow literal secondary stride, so we do not have to worry about moving all calculating instructions before
    // the first load. Also, this is the most common case, since the secondary stride is usually between elements of a
    // single vector load (stride is the literal type size).
    if(!secondaryStride || secondaryStride->signedInt() < 0)
        return {};

    // only allow if "primary" stride has element stride with a power of 2 (e.g. 0th, 4th, 8th and 12th element) for
    // easier and faster code generation.
    for(auto powerTwo : {8u, 4u, 2u})
    {
        if(elementStrides.size() < 2 * powerTwo)
            continue;

        auto primaryStride = elementStrides.at(powerTwo);
        bool allElementsMatch = true;
        for(std::size_t i = 1; i < elementStrides.size(); ++i)
        {
            if((i % powerTwo == 0) && elementStrides.at(i) != primaryStride)
            {
                allElementsMatch = false;
                break;
            }
            if((i % powerTwo != 0) &&
                (elementStrides.at(i).getConstantExpression() & &Value::getLiteralValue) != secondaryStride)
            {
                allElementsMatch = false;
                break;
            }
        }
        if(allElementsMatch)
        {
            // convert primary stride from stride to previous secondary to stride to previous primary
            // e.g. primary offsets are X * N and the applicable power is 4, then primary stride is X - 3 * sizeof(type)
            // we convert to X
            auto primaryPrimaryStride = std::make_shared<Expression>(
                OP_ADD, primaryStride, Value(Literal((powerTwo - 1) * secondaryStride->unsignedInt()), TYPE_INT32));
            primaryPrimaryStride = primaryPrimaryStride->combineWith(
                {}, add_flag(ExpressionOptions::RECURSIVE, ExpressionOptions::STOP_AT_BUILTINS));
            return HierarchicalStrides{powerTwo, primaryPrimaryStride, *secondaryStride};
        }
    }
    return {};
}

NODISCARD static bool groupTMUReads(Method& method, TMUAccessGroup& group)
{
    if(group.ramReads.size() != group.cacheLoads.size())
    {
        LCOV_EXCL_START
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Number of instructions do not match for combining TMU reads!" << logging::endl;
            logging::debug() << group.ramReads.size() << " RAM loads and " << group.cacheLoads.size()
                             << " TMU cache reads" << logging::endl;
        });
        LCOV_EXCL_STOP
        return false;
    }
    if(group.ramReads.size() <= 1)
        return false;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combining " << group.ramReads.size() << " reads of memory into one TMU load... " << logging::endl);

    // 1. Calculate all element offsets and strides
    std::vector<SubExpression> elementStrides;
    std::vector<std::vector<SubExpression>> elementOffsetsParts;
    auto baseUniformParts = getAddParts(group.uniformAddressParts.front());
    std::tie(elementStrides, elementOffsetsParts) = getElementOffsetsAndStrides(group, baseUniformParts);

    auto currentOffset = group.cacheEntry->numVectorElements.getLiteralValue().value().unsignedInt();

    // The 1st offset does not have to match the others, since it is added separately anyway and the stride is only
    // valid from the 2nd element on!
    auto commonStride = elementStrides.at(1);
    auto commonConstantStride = commonStride.getConstantExpression() & &Value::getLiteralValue;
    auto hasConstantCommonStride = [&](const Value& val) -> bool {
        return val.hasLiteral(commonConstantStride.value());
    };
    FastAccessList<InstructionWalker> secondAddressCalculations;

    // 2. Update TMU cache entry to the number and stride/offsets of elements read
    auto it = group.ramReads.front();
    if(commonConstantStride && commonConstantStride->signedInt() >= 0 &&
        /* TODO currently this needs to be a positive power of 2, since TMU lowering does not handle anything else! */
        isPowerTwo(commonConstantStride->unsignedInt()) &&
        std::all_of(elementStrides.begin() + 1, elementStrides.end(), [&](const SubExpression& stride) -> bool {
            return stride.getConstantExpression() & hasConstantCommonStride;
        }))
    {
        /*
         * If all elements have the same constant literal stride (within and between single group elements/loads to be
         * merged), we can just adapt the element stride information of the the TMU RAM load to be lowered.
         */
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using literal TMU access group stride of: " << commonConstantStride->unsignedInt()
                << logging::endl);
        group.cacheEntry->elementStrideInBytes = commonConstantStride->unsignedInt();
    }
    else if(commonConstantStride &&
        std::all_of(elementStrides.begin() + 1, elementStrides.end(),
            [&](const SubExpression& stride) -> bool { return stride == commonStride; }))
    {
        /*
         * If all elements have the same literal stride (within and between single group elements/loads to be
         * merged), we can manually calculate the addresses by applying this stride to all vector elements and make sure
         * they are not overwritten by the lowering of the TMU load.
         *
         * NOTE: We here also only handle literal strides, since otherwise we would also use an address calculation
         * instruction (of the second load) which is located after the first load (see general case).
         */
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using common constant TMU access group stride of: " << commonStride.to_string() << logging::endl);

        // need to manually insert and lower the multiplication instruction
        auto customAddressOffsets =
            method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%tmu_group_offsets");
        it.emplace(std::make_unique<intermediate::IntrinsicOperation>("mul", Value(customAddressOffsets),
            Value(ELEMENT_NUMBER_REGISTER), Value(*commonConstantStride, TYPE_INT32)));
        it->addDecorations(intermediate::InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        auto copyIt = it.copy().nextInBlock();
        intrinsics::intrinsify(method.module, method, it, {});

        auto initialRAMLoad = group.ramReads.front().get<intermediate::RAMAccessInstruction>();
        it = insertCustomAddressesCalculation(copyIt, method, initialRAMLoad->getMemoryAddress(), customAddressOffsets,
            group.usedVectorSize, group.cacheEntry->addresses);
        group.cacheEntry->customAddressCalculation = true;
        initialRAMLoad->setMemoryAddress(group.cacheEntry->addresses);
    }
    else if(std::all_of(elementOffsetsParts.begin(), elementOffsetsParts.end(),
                [](const std::vector<SubExpression>& parts) -> bool {
                    return parts.size() == 1 && parts.front().getLiteralValue();
                }))
    {
        /*
         * If all elements have a literal work-group uniform address offsets, use this to calculate the upper element
         * addresses.
         */
        std::vector<Value> addressOffsets;
        addressOffsets.reserve(group.usedVectorSize);

        auto baseOffset = baseUniformParts.front().getLiteralValue().value();
        for(std::size_t i = 0; i < group.usedVectorSize; ++i)
        {
            Literal offset(elementOffsetsParts.at(i).front().getLiteralValue()->signedInt() - baseOffset.signedInt());
            addressOffsets.emplace_back(offset, TYPE_INT32);
        }

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using constant address offsets of: " << to_string<Value>(addressOffsets) << logging::endl);

        auto customAddressOffsets =
            method.addNewLocal(TYPE_INT32.toVectorType(group.usedVectorSize), "%tmu_group_offsets");

        it = intermediate::insertAssembleVector(it, method, customAddressOffsets, addressOffsets);

        auto initialRAMLoad = group.ramReads.front().get<intermediate::RAMAccessInstruction>();
        it = insertCustomAddressesCalculation(it, method, initialRAMLoad->getMemoryAddress(), customAddressOffsets,
            group.usedVectorSize, group.cacheEntry->addresses);
        group.cacheEntry->customAddressCalculation = true;
        initialRAMLoad->setMemoryAddress(group.cacheEntry->addresses);
    }
    else if(currentOffset == 1 /* needed to calculate the stride via sub of second to first address */ &&
        std::all_of(elementStrides.begin() + 1, elementStrides.end(),
            [=](const SubExpression& stride) -> bool { return stride == commonStride; }) &&
        checkSecondAddressCalculation(group.ramReads.at(1), group.ramReads.front(),
            group.ramReads.at(1).get<intermediate::RAMAccessInstruction>()->getMemoryAddress().checkLocal(),
            secondAddressCalculations))
    {
        /*
         * If all elements have the same (non-constant literal) stride (within and between single group elements/loads
         * to be merged), we can manually calculate the addresses by applying this stride to all vector elements and
         * make sure they are not overwritten by the lowering of the TMU load.
         *
         * We calculate the stride as the difference between the second and the firsth element address. Since in this
         * general case (in contrast to the constant-literal case handled above), the instructions calculating the
         * stride might not exist at the point of the first RAM load, we need to move all dependent instructions too
         * before the new group load instruction.
         *
         * TODO generalize by allowing e.g. the offsets: 0, 0+constA, 0+2*constA, 0+3*constA, x, x+constB, x+2*constB,
         * x+3*constB, ...
         * TODO Need to make sure we do not insert too many instructions
         */
        auto initialRAMLoad = group.ramReads.front().get<intermediate::RAMAccessInstruction>();

        auto elementStride = method.addNewLocal(TYPE_INT32.toVectorType(group.usedVectorSize), "%tmu_group_stride");
        auto strideExpression = commonStride.checkExpression();
        if(auto strideInst = (strideExpression ? strideExpression->toInstruction(elementStride) : nullptr))
        {
            it.emplace(std::move(strideInst));
            it.nextInBlock();
        }
        else
            assign(it, elementStride) =
                (group.ramReads.at(1).get<intermediate::RAMAccessInstruction>()->getMemoryAddress() -
                    initialRAMLoad->getMemoryAddress());
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using common non-constant TMU access group stride of: " << elementStride.to_string()
                << logging::endl);

        // move all necessary address-calculating instructions before the new group load (in inverse order)
        // we also do it if we could convert the stride expression above, in case they are still required for that
        auto insertIt = it.copy().previousInBlock();
        for(auto& calc : secondAddressCalculations)
        {
            insertIt.emplace(calc.release());
            calc.safeErase();
        }

        // need to manually insert and lower the multiplication instruction
        auto customAddressOffsets =
            method.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%tmu_group_offsets");
        it.emplace(std::make_unique<intermediate::IntrinsicOperation>(
            "mul", Value(customAddressOffsets), Value(ELEMENT_NUMBER_REGISTER), std::move(elementStride)));
        auto copyIt = it.copy().nextInBlock();
        intrinsics::intrinsify(method.module, method, it, {});

        it = insertCustomAddressesCalculation(copyIt, method, initialRAMLoad->getMemoryAddress(), customAddressOffsets,
            group.usedVectorSize, group.cacheEntry->addresses);
        group.cacheEntry->customAddressCalculation = true;
        initialRAMLoad->setMemoryAddress(group.cacheEntry->addresses);
    }
    else if(auto strides = checkTwoDistinctStrides(elementStrides))
    {
        // In contrast to most of the above cases, this also applies for vloadN (N = 2, 4 or 8) cases, where the stride
        // between loads is not identical to the stride between vector elements.
        // Since we do check for the "primary" stride to be a power of 2, we can use the element numbers to speed up
        // offset calculation.
        auto initialRAMLoad = group.ramReads.front().get<intermediate::RAMAccessInstruction>();
        auto secondRAMLoad = group.ramReads.at(1).get<intermediate::RAMAccessInstruction>();
        if(initialRAMLoad->getTMUCacheEntry()->numVectorElements.getLiteralValue().value_or(Literal(0)).unsignedInt() !=
            strides->powerOfTwo)
            // we use the difference of the second to the first original TMU RAM read addresses to calculate the primary
            // stride. For this to work, the distance in elements between primary strides has to actually be the number
            // of elements in the first RAM load. Or in other words: the first primary stride needs to actually come
            // from the second RAM load instruction.
            return false;
        if(!checkSecondAddressCalculation(group.ramReads.at(1), group.ramReads.front(),
               secondRAMLoad->getMemoryAddress().local(), secondAddressCalculations))
            // Same reasoning as above, we use the address of the second load to calculate the primary stride, so we
            // need to be able to move its calculating instructions before the first load.
            return false;

        auto primaryStride =
            method.addNewLocal(TYPE_INT32.toVectorType(group.usedVectorSize), "%tmu_group_primary_stride");

        if(auto strideInst = strides->primaryStride->toInstruction(primaryStride))
        {
            it.emplace(std::move(strideInst));
            it.nextInBlock();
        }
        else
            assign(it, primaryStride) = (secondRAMLoad->getMemoryAddress() - initialRAMLoad->getMemoryAddress());

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using two distinct hierarchical strides with offset " << strides->powerOfTwo << ": "
                << strides->primaryStride->to_string() << " and " << strides->secondaryStride.to_string()
                << logging::endl);

        // move all necessary address-calculating instructions before the new group load (in inverse order)
        // we also do it if we could convert the stride expression above, in case they are still required for that
        auto insertIt = it.copy().previousInBlock();
        for(auto& calc : secondAddressCalculations)
        {
            insertIt.emplace(calc.release());
            calc.safeErase();
        }

        auto replicatedStride =
            method.addNewLocal(TYPE_INT32.toVectorType(group.usedVectorSize), "%tmu_group_primary_stride");
        it = intermediate::insertReplication(it, primaryStride, replicatedStride);

        // primary offset = primary stride * (element number / power of two)
        auto tmp = assign(it, replicatedStride.type) = ELEMENT_NUMBER_REGISTER / Literal(strides->powerOfTwo);
        // TODO is mul24 here enough?? Should be for positive strides only?!
        auto primaryOffset = assign(it, replicatedStride.type, "%tmu_group_primary_offset") = mul24(primaryStride, tmp);

        // full offset = primary offset + (element number % power of two) * secondary stride
        tmp = assign(it, replicatedStride.type) = ELEMENT_NUMBER_REGISTER % Literal(strides->powerOfTwo);
        auto secondaryOffset = assign(it, replicatedStride.type, "%tmu_group_secondary_offset") =
            isPowerTwo(strides->secondaryStride.unsignedInt()) ?
            (tmp * strides->secondaryStride) :
            mul24(tmp, Value(strides->secondaryStride, TYPE_INT32));
        auto customAddressOffsets = assign(it, replicatedStride.type, "%tmu_group_offsets") =
            primaryOffset + secondaryOffset;

        // full address = base (first RAM load address) + full offset
        it = insertCustomAddressesCalculation(it, method, initialRAMLoad->getMemoryAddress(), customAddressOffsets,
            group.usedVectorSize, group.cacheEntry->addresses);
        group.cacheEntry->customAddressCalculation = true;
        initialRAMLoad->setMemoryAddress(group.cacheEntry->addresses);
    }
    else
    {
        /*
         * We have different strides of the single loaded vector elements, so we can't use the TMU load lowering
         * calculation based on a fixed stride.
         *
         * Thus, we need to calculate the addresses of all SIMD elements for each (used) vector element and set
         * these as manual addresses.
         *
         * TODO To be able to optimize the remaining cases, we need to be very careful, since the addresses of the
         * successive elements are most likely not yet calculated when the first value is loaded from RAM. So we
         * would need to recalculate the absolute addresses/offsets and make sure all used values are already
         * accessible before using them. Also, at some point (e.g. if calculating and inserting the addresses for
         * every element) the instructions inserted might outweigh the stall cycles saved due to combining the
         * memory accesses...
         */
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "General combination of TMU memory reads is not implemented yet!" << logging::endl);
        return false;
    }

    // only after we found a way to rewrite the stride/element offsets, actually apply some changes to be able to
    // cleanly abort above
    group.cacheEntry->numVectorElements = Value(Literal(group.usedVectorSize), TYPE_INT8);

    // 3. Extract all data with first cache read
    auto firstCacheReadIt = group.cacheLoads.front();
    auto firstCacheRead = firstCacheReadIt.get<intermediate::CacheAccessInstruction>();
    auto tmpData =
        method.addNewLocal(firstCacheRead->getData().type.toVectorType(group.usedVectorSize), "%grouped_tmu_loads");
    if(auto oldCacheReadOutput = firstCacheRead->getOutput())
    {
        // "extract" 0th element by simply copying the value
        it = firstCacheReadIt.copy().nextInBlock();
        it = intermediate::insertVectorExtraction(it, method, tmpData, INT_ZERO, *oldCacheReadOutput);
    }
    firstCacheRead->setOutput(tmpData);

    // 4. Replace all other cache reads with vector extraction
    for(std::size_t i = 1; i < group.cacheLoads.size(); ++i)
    {
        auto cacheRead = group.cacheLoads[i].get<intermediate::CacheAccessInstruction>();
        auto tmpIt = intermediate::insertVectorExtraction(
            group.cacheLoads[i], method, tmpData, Value(Literal(currentOffset), TYPE_INT8), cacheRead->getData());
        currentOffset += cacheRead->getTMUCacheEntry()->numVectorElements.getLiteralValue().value().unsignedInt();
        tmpIt.erase();
    }

    // 5. remove all but the first RAM load
    for(std::size_t i = 1; i < group.ramReads.size(); ++i)
        group.ramReads[i].erase();

    return true;
}

bool optimizations::groupTMUAccess(const Module& module, Method& method, const Configuration& config)
{
    bool didChanges = false;

    // run within all basic blocks
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            TMUAccessGroup group;
            it = findGroupOfTMUAccess(it, group);
            if(group.ramReads.size() > 1)
            {
                if(groupTMUReads(method, group))
                {
                    didChanges = true;
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_OPTIMIZATION, "TMU access groups", group.ramReads.size());
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
    const analysis::ControlFlowLoop& loop, Method& method, const analysis::DataDependencyGraph& dependencyGraph)
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
                auto leftLocal = expr->arg0.checkLocal(true);
                if(leftLocal && (leftLocal->residesInMemory() || leftLocal == globalDataAddress))
                {
                    baseLocal = leftLocal;
                    addressOffset = expr->arg1;
                }
                auto rightLocal = expr->arg1.checkLocal(true);
                if(rightLocal && (rightLocal->residesInMemory() || rightLocal == globalDataAddress))
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
                            return (var.local == offsetExpr->arg0.checkLocal(true) &&
                                       offsetExpr->arg1.getConstantExpression()) ||
                                (var.local == offsetExpr->arg1.checkLocal(true) &&
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
        if(auto offsetCalculation = expr->toInstruction(tmpOffset))
        {
            it.emplace(std::move(offsetCalculation));
            it.nextInBlock();
        }
        else if(expr->code == Expression::FAKEOP_UMUL && expr->arg0.checkValue() && expr->arg1.checkValue())
        {
            // need to manually insert and lower the multiplication instruction
            it.emplace(std::make_unique<intermediate::IntrinsicOperation>(
                "mul", Value(tmpOffset), *expr->arg0.checkValue(), *expr->arg1.checkValue()));
            it->addDecorations(expr->deco);
            auto copyIt = it.copy().nextInBlock();
            intrinsics::intrinsify(method.module, method, it, {});
            it = copyIt;
        }
        else
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to create expression for TMU offset calculation",
                expr->to_string());
    }
    return assign(it, addressType, "%prefetch_tmu_address") = (baseLocal->createReference() + tmpOffset);
}

NODISCARD static bool prefetchTMULoadsInLoop(const analysis::ControlFlowLoop& loop, Method& method,
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
        auto initialPrefetchIt =
            preheader->key->findWalkerForInstruction(load.second.inductionVariable.initialAssignment);
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
        it.emplace(std::make_unique<intermediate::RAMAccessInstruction>(
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
            discardIt = method.emplaceLabel(
                successor->key->walk(), std::make_unique<intermediate::BranchLabel>(*newLabel.local()));

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
        it.emplace(std::make_unique<intermediate::CacheAccessInstruction>(
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
    auto dominatorTree = cfg.getDominatorTree();
    auto loops = cfg.findLoops(false, true);
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

struct LoweredRegisterAccessGroup
{
    Value loweredRegister = UNDEFINED_VALUE;
    Value baseByteOffset = UNDEFINED_VALUE;
    DataType groupedAccessType = TYPE_UNKNOWN;
    std::vector<InstructionWalker> accessInstructions;
};

static bool isNextByteOffset(const LoweredRegisterAccessGroup& group, const Value& byteOffset)
{
    if(group.baseByteOffset.isUndefined() || group.groupedAccessType.isUnknown())
        // first offset
        return !byteOffset.isUndefined();

    auto constantBaseOffset = group.baseByteOffset.getConstantValue() & &Value::getLiteralValue;
    auto constantByteOffset = byteOffset.getConstantValue() & &Value::getLiteralValue;
    if(constantBaseOffset && constantByteOffset)
        return constantBaseOffset->unsignedInt() + group.groupedAccessType.getLogicalWidth() ==
            constantByteOffset->unsignedInt();

    std::shared_ptr<Expression> baseOffsetExpression{};
    if(auto writer = group.baseByteOffset.getSingleWriter())
        baseOffsetExpression = Expression::createRecursiveExpression(*writer);
    std::shared_ptr<Expression> byteOffsetExpression{};
    if(auto writer = byteOffset.getSingleWriter())
        byteOffsetExpression = Expression::createExpression(*writer);
    if(!baseOffsetExpression || !byteOffsetExpression)
        return false;

    auto baseParts = baseOffsetExpression->splitIntoDynamicAndConstantPart(false);
    auto byteParts = byteOffsetExpression->splitIntoDynamicAndConstantPart(false);
    if(baseParts.first != byteParts.first)
        // different dynamic offset parts
        return false;

    constantBaseOffset = baseParts.second.getLiteralValue();
    constantByteOffset = byteParts.second.getLiteralValue();
    return constantBaseOffset && constantByteOffset &&
        constantBaseOffset->unsignedInt() + group.groupedAccessType.getLogicalWidth() ==
        constantByteOffset->unsignedInt();
}

NODISCARD static InstructionWalker findGroupOfLoweredRegisterAccesses(
    InstructionWalker it, LoweredRegisterAccessGroup& group, intermediate::MemoryOperation op)
{
    for(; !it.isEndOfBlock(); it.nextInBlock())
    {
        if(auto access = it.get<intermediate::CacheAccessInstruction>())
        {
            auto loweredCacheEntry = access->getRegisterCacheEntry();
            if(!loweredCacheEntry)
                // no access of register-lowered memory
                // skip this address write for the next check
                continue;
            if(!group.loweredRegister.isUndefined() && loweredCacheEntry->loweredRegister != group.loweredRegister)
                // access of different register-lowered memory, skip
                continue;
            if(access->op != op)
                // different to same register-lowered memory, finish group
                // skip this address write for the next check
                return it.nextInBlock();
            if(!group.groupedAccessType.isUnknown() &&
                loweredCacheEntry->valueType.getElementType() != group.groupedAccessType.getElementType())
                // access to same register-lowered memory with different element type, finish group
                break;
            if(!group.groupedAccessType.isUnknown() &&
                (group.groupedAccessType.getVectorWidth() + loweredCacheEntry->valueType.getVectorWidth()) >
                    NATIVE_VECTOR_SIZE)
                // too many accesses to combine, finish group
                break;

            auto byteOffset = loweredCacheEntry->precalculateOffset();
            if(!isNextByteOffset(group, byteOffset))
                // access to same register-lowered memory with a different offset, finish group
                break;

            if(op == intermediate::MemoryOperation::WRITE)
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found write to register-lowered memory " << loweredCacheEntry->loweredRegister.to_string()
                        << " with byte-offset " << byteOffset.to_string() << " storing "
                        << loweredCacheEntry->valueType.to_string() << " from " << access->getData().to_string()
                        << logging::endl);
            else
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found read from register-lowered memory " << loweredCacheEntry->loweredRegister.to_string()
                        << " with byte-offset " << byteOffset.to_string() << " loading "
                        << loweredCacheEntry->valueType.to_string() << " into " << access->getData().to_string()
                        << logging::endl);

            if(group.baseByteOffset.isUndefined())
                group.baseByteOffset = byteOffset;
            if(group.loweredRegister.isUndefined())
                group.loweredRegister = loweredCacheEntry->loweredRegister;
            if(group.groupedAccessType.isUnknown())
                group.groupedAccessType = loweredCacheEntry->valueType;
            else
                group.groupedAccessType = group.groupedAccessType.toVectorType(static_cast<uint8_t>(
                    group.groupedAccessType.getVectorWidth() + loweredCacheEntry->valueType.getVectorWidth()));
            group.accessInstructions.emplace_back(it);
        }
    }
    return it;
}

NODISCARD static bool groupLoweredRegisterWrites(Method& method, LoweredRegisterAccessGroup& group)
{
    if(group.accessInstructions.size() <= 1)
        return false;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combining " << group.accessInstructions.size()
            << " writes of register-lowered memory into one store... " << logging::endl);

    uint8_t nextVectorElement = 0;
    SIMDVector constantElements{};
    if(std::all_of(group.accessInstructions.begin(), group.accessInstructions.end(), [&](InstructionWalker it) {
           auto cacheInst = it.get<intermediate::CacheAccessInstruction>();
           if(auto constantElement = cacheInst->getData().getConstantValue())
           {
               SIMDVector writtenElements{};
               if(auto elementVector = constantElement->checkVector())
                   writtenElements = *elementVector;
               else if(auto elementLiteral = constantElement->getLiteralValue())
               {
                   for(uint8_t i = 0; i < cacheInst->getData().type.getVectorWidth(); ++i)
                       writtenElements[i] = *elementLiteral;
               }
               else
                   return false;
               for(uint8_t i = 0; i < cacheInst->getData().type.getVectorWidth(); ++i)
                   constantElements[nextVectorElement++] = writtenElements[i];
               return true;
           }
           return false;
       }))
    {
        // we can directly write the assembled vector
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Writing constant vector " << constantElements.to_string(true) << logging::endl);

        auto constantSource = method.module.storeVector(std::move(constantElements), group.groupedAccessType);
        ignoreReturnValue(periphery::insertWriteLoweredRegister(
            method, group.accessInstructions.back(), constantSource, group.baseByteOffset, group.loweredRegister));
    }
    else
    {
        auto tmpData = method.addNewLocal(group.groupedAccessType, "%lowered_register_write_group");
        auto it = group.accessInstructions.back();

        nextVectorElement = 0;
        for(const auto& writeIt : group.accessInstructions)
        {
            auto writer = writeIt.get<intermediate::CacheAccessInstruction>();
            auto tmpElement = assign(it, writer->getRegisterCacheEntry()->valueType) = writer->getData();
            it = intermediate::insertVectorInsertion(
                it, method, tmpData, Value(Literal(nextVectorElement), TYPE_INT8), tmpElement);
            nextVectorElement = static_cast<uint8_t>(nextVectorElement + tmpElement.type.getVectorWidth());
        }

        CPPLOG_LAZY(logging::Level::DEBUG, log << "Writing assembled vector " << tmpData.to_string() << logging::endl);
        it = periphery::insertWriteLoweredRegister(method, it, tmpData, group.baseByteOffset, group.loweredRegister);
    }

    for(auto writeIt : group.accessInstructions)
        writeIt.erase();

    return true;
}

bool optimizations::groupLoweredRegisterAccess(const Module& module, Method& method, const Configuration& config)
{
    bool didChanges = false;

    // run within all basic blocks
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            LoweredRegisterAccessGroup group;
            it = findGroupOfLoweredRegisterAccesses(it, group, intermediate::MemoryOperation::WRITE);
            if(group.accessInstructions.size() > 1)
            {
                if(groupLoweredRegisterWrites(method, group))
                {
                    didChanges = true;
                    PROFILE_COUNTER(
                        vc4c::profiler::COUNTER_OPTIMIZATION, "Register write groups", group.accessInstructions.size());
                }
            }
        }
    }

    // clean up empty instructions
    if(didChanges)
        method.cleanEmptyInstructions();

    return didChanges;
}
