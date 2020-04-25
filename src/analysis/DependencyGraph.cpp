/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DependencyGraph.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "DebugGraph.h"

#include "log.h"

#include <algorithm>
#include <limits>

using namespace vc4c;
using namespace vc4c::analysis;

unsigned Dependency::rateDelay(unsigned currentDistance) const
{
    if(numDelayCycles == 0 || currentDistance >= numDelayCycles)
        return 0;
    if(isMandatoryDelay && currentDistance < numDelayCycles)
        return std::numeric_limits<unsigned>::max();
    return numDelayCycles - currentDistance;
}

bool Dependency::canBeInserted(const intermediate::IntermediateInstruction* instr) const
{
    if((has_flag(type, DependencyType::SIGNAL_READ_AFTER_WRITE) ||
           has_flag(type, DependencyType::SIGNAL_WRITE_AFTER_WRITE)) &&
        instr->getSignal().hasSideEffects())
    {
        // valid as long as the other instruction does not trigger a signal
        return false;
    }
    if(has_flag(type, DependencyType::SIGNAL_WRITE_AFTER_READ) && instr->readsRegister(REG_SFU_OUT))
    {
        // valid as long as the other instruction does not consume the signal
        return false;
    }
    if((has_flag(type, DependencyType::FLAGS_READ_AFTER_WRITE) ||
           has_flag(type, DependencyType::FLAGS_WRITE_AFTER_WRITE)) &&
        instr->doesSetFlag())
    {
        // valid as long as the other instruction does not set flags
        return false;
    }
    return true;
}

bool DependencyNodeBase::hasIncomingDependencies() const
{
    const auto* self = reinterpret_cast<const DependencyNode*>(this);
    bool incomingDependencies = false;
    self->forAllIncomingEdges([&incomingDependencies](const DependencyNode&, const DependencyEdge&) -> bool {
        incomingDependencies = true;
        return false;
    });
    return incomingDependencies;
}

bool DependencyNodeBase::hasOutGoingDependencies() const
{
    const auto* self = reinterpret_cast<const DependencyNode*>(this);
    bool outgoingDependencies = false;
    self->forAllOutgoingEdges([&outgoingDependencies](const DependencyNode&, const DependencyEdge&) -> bool {
        outgoingDependencies = true;
        return false;
    });
    return outgoingDependencies;
}

const DependencyNode* DependencyNodeBase::getFlagsSetter() const
{
    const auto* self = reinterpret_cast<const DependencyNode*>(this);
    const DependencyNode* setter = nullptr;
    self->forAllIncomingEdges([&setter](const DependencyNode& neighbor, const DependencyEdge& edge) -> bool {
        if(has_flag(edge.data.type, DependencyType::FLAGS_READ_AFTER_WRITE))
        {
            setter = &neighbor;
            return false;
        }
        return true;
    });
    return setter;
}

const DependencyNode* DependencyNodeBase::getSignalTrigger() const
{
    const auto* self = reinterpret_cast<const DependencyNode*>(this);
    const DependencyNode* setter = nullptr;
    self->forAllIncomingEdges([&setter](const DependencyNode& neighbor, const DependencyEdge& edge) -> bool {
        if(has_flag(edge.data.type, DependencyType::SIGNAL_READ_AFTER_WRITE))
        {
            setter = &neighbor;
            return false;
        }
        return true;
    });
    return setter;
}

std::size_t DependencyNodeBase::calculatePrecedingCriticalPathLength(
    bool onlyMandatoryDelay, FastMap<const intermediate::IntermediateInstruction*, std::size_t>* cache) const
{
    const auto* self = reinterpret_cast<const DependencyNode*>(this);

    if(cache)
    {
        auto cacheIt = cache->find(self->key);
        if(cacheIt != cache->end())
        {
            return cacheIt->second;
        }
    }

    std::size_t maxLength = 0;
    self->forAllIncomingEdges([&](const DependencyNode& predecessor, const DependencyEdge& edge) -> bool {
        std::size_t transitiveDelay = predecessor.calculatePrecedingCriticalPathLength(onlyMandatoryDelay, cache);
        auto pathLength = 1 /* the predecessor itself */ +
            (edge.data.isMandatoryDelay || !onlyMandatoryDelay ? edge.data.numDelayCycles : 0) + transitiveDelay;
        maxLength = std::max(maxLength, pathLength);
        return true;
    });
    if(cache)
        cache->emplace(self->key, maxLength);
    return maxLength;
}

std::size_t DependencyNodeBase::calculateSucceedingCriticalPathLength(
    bool onlyMandatoryDelay, FastMap<const intermediate::IntermediateInstruction*, std::size_t>* cache) const
{
    const auto* self = reinterpret_cast<const DependencyNode*>(this);

    if(cache)
    {
        auto cacheIt = cache->find(self->key);
        if(cacheIt != cache->end())
        {
            return cacheIt->second;
        }
    }

    std::size_t maxLength = 0;
    self->forAllOutgoingEdges([&](const DependencyNode& successor, const DependencyEdge& edge) -> bool {
        std::size_t transitiveDelay = successor.calculateSucceedingCriticalPathLength(onlyMandatoryDelay, cache);
        auto pathLength = 1 /* the successor itself */ +
            (edge.data.isMandatoryDelay || !onlyMandatoryDelay ? edge.data.numDelayCycles : 0) + transitiveDelay;
        maxLength = std::max(maxLength, pathLength);
        return true;
    });
    if(cache)
        cache->emplace(self->key, maxLength);
    return maxLength;
}

static void addDependency(Dependency& dependency, DependencyType type, unsigned numCycles = 0, bool fixedDelay = false)
{
    dependency.type = add_flag(dependency.type, type);
    dependency.numDelayCycles = std::max(dependency.numDelayCycles, numCycles);
    dependency.isMandatoryDelay = dependency.isMandatoryDelay || fixedDelay;
}

static bool isLocallyLimited(const Local* local, const intermediate::IntermediateInstruction* currentInstr,
    const intermediate::IntermediateInstruction* lastWriter, const intermediate::IntermediateInstruction* lastReader)
{
    auto tmp = local->getUsers();
    tmp.erase(currentInstr);
    tmp.erase(lastWriter);
    tmp.erase(lastReader);
    return tmp.empty();
}

static void createLocalDependencies(DependencyGraph& graph, DependencyNode& node,
    const FastMap<const Local*, const intermediate::IntermediateInstruction*>& lastLocalWrites,
    const FastMap<const Local*, const intermediate::IntermediateInstruction*>& lastLocalReads)
{
    bool isVectorRotation = dynamic_cast<const intermediate::VectorRotation*>(node.key) != nullptr;
    // can only unpack from register-file A which requires a read-after-write delay of at least 1 instruction
    bool hasUnpackMode = node.key->hasUnpackMode();
    node.key->forUsedLocals(
        [&](const Local* loc, LocalUse::Type type, const intermediate::IntermediateInstruction& inst) -> void {
            const intermediate::IntermediateInstruction* lastWrite = nullptr;
            const intermediate::IntermediateInstruction* lastRead = nullptr;
            {
                auto writeIt = lastLocalWrites.find(loc);
                if(writeIt != lastLocalWrites.end())
                    lastWrite = writeIt->second;
                auto readIt = lastLocalReads.find(loc);
                if(readIt != lastLocalReads.end())
                    lastRead = readIt->second;
            }
            unsigned distance = (!isVectorRotation && !hasUnpackMode &&
                                    (isLocallyLimited(loc, node.key, lastWrite, lastRead) ||
                                        isLocallyLimited(loc, &inst, lastWrite, lastRead))) ?
                0 :
                1;
            if(has_flag(type, LocalUse::Type::READER))
            {
                // any reading of a local depends on the previous write
                if(lastWrite != nullptr)
                {
                    distance = std::max(distance, lastWrite->hasPackMode() ? 1u : 0u);
                    auto& otherNode = graph.assertNode(lastWrite);
                    addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_WRITE,
                        distance, isVectorRotation || hasUnpackMode || lastWrite->hasPackMode());
                }
            }
            if(has_flag(type, LocalUse::Type::WRITER))
            {
                if(lastWrite != nullptr)
                {
                    // writing a local needs to preserve the order the local is written before
                    distance = std::max(distance, lastWrite->hasPackMode() ? 1u : 0u);
                    auto& otherNode = graph.assertNode(lastWrite);
                    addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_WRITE,
                        distance, lastWrite->hasPackMode());
                }
                if(lastRead != nullptr)
                {
                    // writing a local must be ordered after previous reads
                    auto& otherNode = graph.assertNode(lastRead);
                    addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_READ);
                }
            }
        });
}

static void createFlagDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastSettingOfFlags,
    const intermediate::IntermediateInstruction* lastConditional)
{
    if(node.key->hasConditionalExecution() &&
        (lastSettingOfFlags != nullptr || dynamic_cast<const intermediate::Branch*>(node.key) == nullptr))
    {
        // any conditional execution depends on flags being set previously
        auto& otherNode = graph.assertNode(lastSettingOfFlags);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::FLAGS_READ_AFTER_WRITE);
    }
    if(node.key->doesSetFlag() && lastSettingOfFlags != nullptr)
    {
        // any setting of flags must be ordered after the previous setting of flags
        auto& otherNode = graph.assertNode(lastSettingOfFlags);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::FLAGS_WRITE_AFTER_WRITE);
    }
    if(node.key->doesSetFlag() && lastConditional != nullptr)
    {
        // any setting of flags must be ordered after any previous use of these flags
        auto& otherNode = graph.assertNode(lastConditional);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::FLAGS_WRITE_AFTER_READ);
    }
}

static void createR4Dependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastTriggerOfR4,
    const intermediate::IntermediateInstruction* lastReadOfR4)
{
    if(node.key->readsRegister(REG_SFU_OUT))
    {
        // any read of r4 needs to be ordered after the trigger of r4
        auto& otherNode = graph.assertNode(lastTriggerOfR4);
        unsigned delayCycles = 0;
        bool fixedDelay = false;
        if(lastTriggerOfR4->checkOutputRegister() & &Register::triggersReadOfR4)
        {
            // delay slots for SFU calculations
            delayCycles = 2;
            fixedDelay = true;
        }
        // For reading from TMU, the actual reading of memory is done between writing the TMU address and the signals.
        // According to experiments (https://gist.github.com/nomaddo/31aaf839f2463816beb9563173b48728), inserting
        // instructions between triggering the signal and reading r4 immediately introduces delays, so we don't insert
        // any
        addDependency(
            otherNode.getOrCreateEdge(&node).data, DependencyType::SIGNAL_READ_AFTER_WRITE, delayCycles, fixedDelay);
    }
    if(node.key->getSignal().triggersReadOfR4() || (node.key->checkOutputRegister() & &Register::triggersReadOfR4))
    {
        // XXX Although it might not be necessary to order signals of reading from TMU and the actual reads of r4 (e.g.
        // load_tmu0; read r4; load_tmu1; read r4), we still do so, since it has not been tested whether the QPU hsa
        // some kind of queue. Also, at this position, it doesn't really make a different whether we enforce order or
        // not, since there is no delay we would need to fill
        if(lastTriggerOfR4 != nullptr)
        {
            // any trigger of r4 needs to be ordered after the previous trigger of r4
            auto& otherNode = graph.assertNode(lastTriggerOfR4);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SIGNAL_WRITE_AFTER_WRITE);
        }
        if(lastReadOfR4 != nullptr)
        {
            // any trigger of r4 must be ordered after the previous read of r4
            auto& otherNode = graph.assertNode(lastReadOfR4);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SIGNAL_WRITE_AFTER_READ);
        }
    }
}

static void createMutexDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastMutexLock,
    const intermediate::IntermediateInstruction* lastMutexUnlock,
    const intermediate::IntermediateInstruction* lastInstruction,
    const intermediate::IntermediateInstruction* lastSemaphoreAccess,
    const intermediate::IntermediateInstruction* lastMemFence)
{
    if(lastMutexLock &&
        ((node.key->checkOutputRegister() & &Register::isVertexPipelineMemory) ||
            std::any_of(node.key->getArguments().begin(), node.key->getArguments().end(),
                [](const Value& arg) -> bool {
                    return check(arg.checkRegister()) & &Register::isVertexPipelineMemory;
                }) ||
            node.key->writesRegister(REG_MUTEX)))
    {
        // any VPM operation or mutex unlock must be ordered after the previous mutex lock, if it exists
        auto& otherNode = graph.assertNode(lastMutexLock);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::MUTEX_LOCK);
    }
    if(node.key->readsRegister(REG_MUTEX) && lastMutexUnlock != nullptr)
    {
        // any mutex lock must be ordered after the previous unlock, if any
        auto& otherNode = graph.assertNode(lastMutexUnlock);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(node.key->readsRegister(REG_MUTEX) && lastSemaphoreAccess != nullptr)
    {
        // any mutex lock must be ordered after the previous semaphore access, if any
        auto& otherNode = graph.assertNode(lastSemaphoreAccess);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(node.key->readsRegister(REG_MUTEX) && lastMemFence != nullptr)
    {
        // any mutex lock must be ordered after the previous memory fence, if any
        auto& otherNode = graph.assertNode(lastMemFence);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createSemaphoreDepencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastSemaphoreAccess,
    const intermediate::IntermediateInstruction* lastMemFence,
    const intermediate::IntermediateInstruction* lastMutexUnlock,
    const intermediate::IntermediateInstruction* lastReadOfR4)
{
    const auto semaphoreAccess = dynamic_cast<const intermediate::SemaphoreAdjustment*>(node.key);
    const auto memoryFence = dynamic_cast<const intermediate::MemoryBarrier*>(node.key);
    if(semaphoreAccess != nullptr && lastSemaphoreAccess != nullptr)
    {
        // the order of semaphore-accesses cannot be modified!
        auto& otherNode = graph.assertNode(lastSemaphoreAccess);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(semaphoreAccess != nullptr && lastMemFence != nullptr)
    {
        // the order of semaphore-accesses and memory fences cannot be modified!
        auto& otherNode = graph.assertNode(lastMemFence);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(semaphoreAccess != nullptr && lastMutexUnlock != nullptr)
    {
        // the order of semaphore-accesses and mutex unlocks cannot be modified!
        auto& otherNode = graph.assertNode(lastMutexUnlock);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(semaphoreAccess != nullptr && lastReadOfR4 != nullptr)
    {
        // the order of semaphore-accesses and reading from TMU cannot be modified!
        auto& otherNode = graph.assertNode(lastReadOfR4);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(memoryFence != nullptr && lastSemaphoreAccess != nullptr)
    {
        // the order of memory fences cannot be modified!
        auto& otherNode = graph.assertNode(lastSemaphoreAccess);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(memoryFence != nullptr && lastMemFence != nullptr)
    {
        // the order of semaphore-accesses and memory fences cannot be modified!
        auto& otherNode = graph.assertNode(lastMemFence);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(memoryFence != nullptr && lastMutexUnlock != nullptr)
    {
        // the order of memory fences and mutex unlocks cannot be modified!
        auto& otherNode = graph.assertNode(lastMutexUnlock);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
    if(memoryFence != nullptr && lastReadOfR4 != nullptr)
    {
        // the order of memory fences and reading from TMU cannot be modified!
        auto& otherNode = graph.assertNode(lastReadOfR4);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::SEMAPHORE_ORDER);
    }
}

static void createUniformDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastWriteOfUniformAddress,
    const intermediate::IntermediateInstruction* lastReadOfUniform)
{
    if(node.key->readsRegister(REG_UNIFORM))
    {
        if(lastWriteOfUniformAddress != nullptr)
        {
            // any reading of UNIFORM value must be ordered after the previous writing of their address
            auto& otherNode = graph.assertNode(lastWriteOfUniformAddress);
            /*
             * " [...] there must be at least two nonuniform-accessing instructions following a pointer change before
             * uniforms can be accessed once more."
             * - Broadcom specification, page 22
             */
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_WRITE, 2, true);
        }
        if(lastReadOfUniform != nullptr)
        {
            // the order UNIFORMS are read in must not change!
            auto& otherNode = graph.assertNode(lastReadOfUniform);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_READ);
        }
    }
    if(node.key->writesRegister(REG_UNIFORM_ADDRESS))
    {
        if(lastWriteOfUniformAddress != nullptr)
        {
            // any writing of UNIFORM address must be ordered after the previous write
            auto& otherNode = graph.assertNode(lastWriteOfUniformAddress);
            // just to be sure, add the mandatory distance here too
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_WRITE, 2, true);
        }
        if(lastReadOfUniform != nullptr)
        {
            // any writing of UNIFORM address must be ordered after any previous reading of UNIFORMs
            auto& otherNode = graph.assertNode(lastReadOfUniform);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_READ);
        }
    }
}

static void createReplicationDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastReplicationWrite,
    const intermediate::IntermediateInstruction* lastReplicationRead)
{
    if(node.key->writesRegister(REG_REPLICATE_ALL) || node.key->writesRegister(REG_REPLICATE_QUAD))
    {
        if(lastReplicationRead != nullptr)
        {
            // any writing to the replication registers need to be executed after the previous read, if any
            auto& otherNode = graph.assertNode(lastReplicationRead);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_READ);
        }
        if(lastReplicationWrite != nullptr)
        {
            // any writing to the replication registers need to be executed after the previous write, if any
            auto& otherNode = graph.assertNode(lastReplicationWrite);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_WRITE);
        }
    }
    if(node.key->readsRegister(REG_ACC5))
    {
        if(lastReplicationRead != nullptr)
        {
            // any reading to the replication registers need to be executed after the previous read, if any
            auto& otherNode = graph.assertNode(lastReplicationRead);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_READ);
        }
        if(lastReplicationWrite != nullptr)
        {
            // any reading to the replication registers need to be executed after the previous write, if any
            auto& otherNode = graph.assertNode(lastReplicationWrite);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_WRITE);
        }
    }
}

static void createVPMSetupDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWriteSetup,
    const intermediate::IntermediateInstruction* lastVPMReadSetup)
{
    if(lastVPMWriteSetup != nullptr &&
        (node.key->writesRegister(REG_VPM_IO) || node.key->writesRegister(REG_VPM_OUT_SETUP) ||
            node.key->writesRegister(REG_VPM_DMA_STORE_ADDR)))
    {
        // any other VPM write setup, VPM write or VPM write address setup must be ordered after the VPM write setup
        auto& otherNode = graph.assertNode(lastVPMWriteSetup);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastVPMReadSetup != nullptr &&
        (node.key->readsRegister(REG_VPM_IO) || node.key->writesRegister(REG_VPM_IN_SETUP) ||
            node.key->writesRegister(REG_VPM_DMA_LOAD_ADDR)))
    {
        // any other VPM read setup, VPM read or VPM read address setup must be ordered after the VPM read setup
        auto& otherNode = graph.assertNode(lastVPMReadSetup);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createVPMIODependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWrite, const intermediate::IntermediateInstruction* lastVPMRead)
{
    if(lastVPMWrite != nullptr &&
        (node.key->writesRegister(REG_VPM_IO) || node.key->writesRegister(REG_VPM_DMA_STORE_ADDR) ||
            node.key->writesRegister(REG_MUTEX)))
    {
        // any other VPM write, VPM write address setup or unlocking mutex must be executed after the VPM write
        auto& otherNode = graph.assertNode(lastVPMWrite);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastVPMRead != nullptr && (node.key->readsRegister(REG_VPM_IO) || node.key->writesRegister(REG_MUTEX)))
    {
        // any other VPM read or unlocking mutex must be executed after the VPM read
        auto& otherNode = graph.assertNode(lastVPMRead);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createVPMAddressDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWriteAddress,
    const intermediate::IntermediateInstruction* lastVPMReadAddress)
{
    if(node.key->readsRegister(REG_VPM_DMA_STORE_WAIT))
    {
        // the VPM write wait instruction needs to be executed after the setting of the VPM write address
        auto& otherNode = graph.assertNode(lastVPMWriteAddress);
        // XXX correct delay
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER, 10);
    }
    if(node.key->readsRegister(REG_VPM_DMA_LOAD_WAIT))
    {
        // the VPM read wait instruction needs to be executed after the setting of the VPM read address
        auto& otherNode = graph.assertNode(lastVPMReadAddress);
        // XXX correct delay
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER, 6);
    }
}

static void createVPMWaitDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWriteWait,
    const intermediate::IntermediateInstruction* lastVPMReadWait)
{
    if(lastVPMWriteWait != nullptr &&
        (node.key->writesRegister(REG_MUTEX) || (node.key->checkOutputRegister() & &Register::isVertexPipelineMemory)))
    {
        // unlocking of the mutex as well as any following VPM accesses (within that mutex) need to be ordered after the
        // VPM write wait instruction
        auto& otherNode = graph.assertNode(lastVPMWriteWait);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastVPMReadWait != nullptr &&
        (node.key->readsRegister(REG_VPM_IO) ||
            /* this check is for writing following reads, e.g. for memory copies */
            (node.key->checkOutputRegister() & &Register::isVertexPipelineMemory)))
    {
        // all reading of VPM need to be ordered after the VPM read wait instruction (if any)
        auto& otherNode = graph.assertNode(lastVPMReadWait);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createTMUCoordinateDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastTMU0CoordsWrite,
    const intermediate::IntermediateInstruction* lastTMU1CoordsWrite,
    const intermediate::IntermediateInstruction* lastTMUNoswapWrite,
    const intermediate::IntermediateInstruction* lastSemaphoreAccess,
    const intermediate::IntermediateInstruction* lastMemFence)
{
    if(lastTMUNoswapWrite != nullptr && (node.key->checkOutputRegister() & &Register::isTextureMemoryUnit))
    {
        // do not re-order/keep minimum distance between TMU swapping configuration and writing TMU addresses
        auto& otherNode = graph.assertNode(lastTMUNoswapWrite);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER, 3, true);
    }
    if(lastTMU0CoordsWrite != nullptr &&
        (node.key->writesRegister(REG_TMU0_COORD_B_LOD_BIAS) ||
            node.key->writesRegister(REG_TMU0_COORD_R_BORDER_COLOR) || node.key->writesRegister(REG_TMU0_COORD_S_U_X) ||
            node.key->writesRegister(REG_TMU0_COORD_T_V_Y)))
    {
        // do not re-order writes to same TMU (especially not writes to S coordinates, which triggers the read)
        auto& otherNode = graph.assertNode(lastTMU0CoordsWrite);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastTMU1CoordsWrite != nullptr &&
        (node.key->writesRegister(REG_TMU1_COORD_B_LOD_BIAS) ||
            node.key->writesRegister(REG_TMU1_COORD_R_BORDER_COLOR) || node.key->writesRegister(REG_TMU1_COORD_S_U_X) ||
            node.key->writesRegister(REG_TMU1_COORD_T_V_Y)))
    {
        // do not re-order writes to same TMU (especially not writes to S coordinates, which triggers the read)
        auto& otherNode = graph.assertNode(lastTMU1CoordsWrite);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastSemaphoreAccess && (node.key->checkOutputRegister() & &Register::isTextureMemoryUnit))
    {
        // do not re-order setting of TMU registers (addresses) before semaphore access
        auto& otherNode = graph.assertNode(lastSemaphoreAccess);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastMemFence && (node.key->checkOutputRegister() & &Register::isTextureMemoryUnit))
    {
        // do not re-order setting of TMU registers (addresses) before memory locks
        auto& otherNode = graph.assertNode(lastMemFence);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
    }
    // TMU seems to need 9 cycles to load from cache and (additional?) 20 to load from memory
    // Tests show a delay of 8 instructions in between (total of 9 cycles) to be the most efficient:
    // 8 instructions inserted increase execution time almost not at all (a bit due to instruction fetching), 9+ do
    // noticeably
    const unsigned tmuLoadDelay = 8;
    if(node.key->getSignal() == SIGNAL_LOAD_TMU0)
    {
        // triggering of read from the FIFO depends on the memory address being set previously which fills the FIFO from
        // memory (thus taking longer)
        auto& otherNode = graph.assertNode(lastTMU0CoordsWrite);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER, tmuLoadDelay);
    }
    if(node.key->getSignal() == SIGNAL_LOAD_TMU1)
    {
        // triggering of read from the FIFO depends on the memory address being set previously which fills the FIFO from
        // memory (thus taking longer)
        auto& otherNode = graph.assertNode(lastTMU1CoordsWrite);
        addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER, tmuLoadDelay);
    }
    if(node.key->writesRegister(REG_TMU_NOSWAP))
    {
        if(lastTMU0CoordsWrite != nullptr)
        {
            auto& otherNode = graph.assertNode(lastTMU0CoordsWrite);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
        }
        if(lastTMU1CoordsWrite != nullptr)
        {
            auto& otherNode = graph.assertNode(lastTMU1CoordsWrite);
            addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::PERIPHERY_ORDER);
        }
    }
}

static void createR5Dependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastWriteOfR5,
    const intermediate::IntermediateInstruction* lastReadOfR5)
{
    // TODO write more general for any (non-periphery) register? E.g. ms_mask, rev_flag
    if(lastWriteOfR5 && dynamic_cast<const intermediate::VectorRotation*>(node.key) &&
        (node.key->readsRegister(REG_ACC5)))
    {
        // enforce distance of 1 from writing r5 to reading r5 for vector rotations
        addDependency(graph.assertNode(lastWriteOfR5).getOrCreateEdge(&node).data,
            DependencyType::VALUE_READ_AFTER_WRITE, 1, true);
    }
    if(lastWriteOfR5 &&
        (node.key->writesRegister(REG_ACC5) || node.key->writesRegister(REG_REPLICATE_ALL) ||
            node.key->writesRegister(REG_REPLICATE_QUAD)))
    {
        // enforce order of writing r5
        addDependency(
            graph.assertNode(lastWriteOfR5).getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_WRITE);
    }
    if(lastWriteOfR5 &&
        (node.key->readsRegister(REG_ACC5) || node.key->readsRegister(REG_REPLICATE_ALL) ||
            node.key->readsRegister(REG_REPLICATE_QUAD)))
    {
        // enforce read-after-write for r5
        addDependency(
            graph.assertNode(lastWriteOfR5).getOrCreateEdge(&node).data, DependencyType::VALUE_READ_AFTER_WRITE);
    }
    if(lastReadOfR5 &&
        (node.key->writesRegister(REG_ACC5) || node.key->writesRegister(REG_REPLICATE_ALL) ||
            node.key->writesRegister(REG_REPLICATE_QUAD)))
    {
        // enforce write-after-read for r5
        addDependency(
            graph.assertNode(lastReadOfR5).getOrCreateEdge(&node).data, DependencyType::VALUE_WRITE_AFTER_READ);
    }
}

static void createThreadEndDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastHostInterrupt,
    const intermediate::IntermediateInstruction* lastProgramEnd)
{
    if(node.key->writesRegister(REG_HOST_INTERRUPT))
    {
        // host interrupt depends on any preceding instruction
        graph.forAllNodes([&](DependencyNode& otherNode) {
            if(&node != &otherNode && otherNode.key)
            {
                addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::THREAD_END_ORDER);
            }
        });
    }

    // program end depends on host interrupt
    if(node.key->getSignal() == SIGNAL_END_PROGRAM)
    {
        addDependency(
            graph.assertNode(lastHostInterrupt).getOrCreateEdge(&node).data, DependencyType::THREAD_END_ORDER);
    }

    // program end nops depend on program end signal
    if(node.key->getSignal() != SIGNAL_END_PROGRAM && dynamic_cast<const intermediate::Nop*>(node.key) &&
        dynamic_cast<const intermediate::Nop*>(node.key)->type == intermediate::DelayType::THREAD_END)
    {
        addDependency(graph.assertNode(lastProgramEnd).getOrCreateEdge(&node).data, DependencyType::THREAD_END_ORDER);
    }
}

std::unique_ptr<DependencyGraph> DependencyGraph::createGraph(const BasicBlock& block)
{
    PROFILE_START(createDependencyGraph);
    std::unique_ptr<DependencyGraph> graph(new DependencyGraph(block.size()));

    const intermediate::IntermediateInstruction* lastSettingOfFlags = nullptr;
    const intermediate::IntermediateInstruction* lastConditional = nullptr;
    const intermediate::IntermediateInstruction* lastTriggerOfR4 = nullptr;
    const intermediate::IntermediateInstruction* lastReadOfR4 = nullptr;
    const intermediate::IntermediateInstruction* lastMutexLock = nullptr;
    const intermediate::IntermediateInstruction* lastMutexUnlock = nullptr;
    const intermediate::IntermediateInstruction* lastSemaphoreAccess = nullptr;
    const intermediate::IntermediateInstruction* lastReadOfUniform = nullptr;
    const intermediate::IntermediateInstruction* lastWriteOfUniformAddress = nullptr;
    const intermediate::IntermediateInstruction* lastReplicationWrite = nullptr;
    const intermediate::IntermediateInstruction* lastReplicationRead = nullptr;
    const intermediate::IntermediateInstruction* lastVPMWriteSetup = nullptr;
    const intermediate::IntermediateInstruction* lastVPMReadSetup = nullptr;
    const intermediate::IntermediateInstruction* lastVPMWrite = nullptr;
    const intermediate::IntermediateInstruction* lastVPMRead = nullptr;
    const intermediate::IntermediateInstruction* lastVPMWriteAddress = nullptr;
    const intermediate::IntermediateInstruction* lastVPMReadAddress = nullptr;
    const intermediate::IntermediateInstruction* lastVPMWriteWait = nullptr;
    const intermediate::IntermediateInstruction* lastVPMReadWait = nullptr;
    const intermediate::IntermediateInstruction* lastTMU0CoordsWrite = nullptr;
    const intermediate::IntermediateInstruction* lastTMU1CoordsWrite = nullptr;
    const intermediate::IntermediateInstruction* lastTMUNoswapWrite = nullptr;
    const intermediate::IntermediateInstruction* lastInstruction = nullptr;
    const intermediate::IntermediateInstruction* lastWriteOfR5 = nullptr;
    const intermediate::IntermediateInstruction* lastReadOfR5 = nullptr;
    const intermediate::IntermediateInstruction* lastHostInterrupt = nullptr;
    const intermediate::IntermediateInstruction* lastProgramEnd = nullptr;
    const intermediate::IntermediateInstruction* lastMemFence = nullptr;
    FastMap<const Local*, const intermediate::IntermediateInstruction*> lastLocalWrites;
    FastMap<const Local*, const intermediate::IntermediateInstruction*> lastLocalReads;
    // TODO "normal" register dependencies?
    // TODO check also limitations/barriers from Reordering and nomaddo's PR

    for(const auto& inst : block)
    {
        if(!inst ||
            (dynamic_cast<const intermediate::Nop*>(inst.get()) && !inst->hasSideEffects() &&
                dynamic_cast<const intermediate::Nop*>(inst.get())->type != intermediate::DelayType::THREAD_END))
            continue;
        auto& node = graph->getOrCreateNode(inst.get());

        createLocalDependencies(*graph, node, lastLocalWrites, lastLocalReads);
        createFlagDependencies(*graph, node, lastSettingOfFlags, lastConditional);
        createR4Dependencies(*graph, node, lastTriggerOfR4, lastReadOfR4);
        createR5Dependencies(*graph, node, lastWriteOfR5, lastReadOfR5);
        createMutexDependencies(
            *graph, node, lastMutexLock, lastMutexUnlock, lastInstruction, lastSemaphoreAccess, lastMemFence);
        createSemaphoreDepencies(*graph, node, lastSemaphoreAccess, lastMemFence, lastMutexUnlock, lastReadOfR4);
        createUniformDependencies(*graph, node, lastWriteOfUniformAddress, lastReadOfUniform);
        createReplicationDependencies(*graph, node, lastReplicationWrite, lastReplicationRead);
        createVPMSetupDependencies(*graph, node, lastVPMWriteSetup, lastVPMReadSetup);
        createVPMIODependencies(*graph, node, lastVPMWrite, lastVPMRead);
        createVPMAddressDependencies(*graph, node, lastVPMWriteAddress, lastVPMReadAddress);
        createVPMWaitDependencies(*graph, node, lastVPMWriteWait, lastVPMReadWait);
        createTMUCoordinateDependencies(*graph, node, lastTMU0CoordsWrite, lastTMU1CoordsWrite, lastTMUNoswapWrite,
            lastSemaphoreAccess, lastMemFence);
        createThreadEndDependencies(*graph, node, lastHostInterrupt, lastProgramEnd);
        auto branch = dynamic_cast<const intermediate::Branch*>(inst.get());
        if(branch)
        {
            // branches depend on everything before to make sure they come after everything
            // XXX this disables reordering of conditional branches for now
            graph->forAllNodes([&](DependencyNode& otherNode) {
                if(&node != &otherNode && otherNode.key)
                {
                    addDependency(otherNode.getOrCreateEdge(&node).data, DependencyType::BRANCH_ORDER);
                }
            });
        }

        // update the cached values
        if(inst->doesSetFlag() || (branch && !branch->isUnconditional()))
            // conditional branches may introduce setting of flags
            lastSettingOfFlags = inst.get();
        if(inst->hasConditionalExecution() || (branch && !branch->isUnconditional()))
            lastConditional = inst.get();
        if(inst->getSignal().triggersReadOfR4() || (inst->checkOutputRegister() & &Register::triggersReadOfR4))
            lastTriggerOfR4 = inst.get();
        if(inst->readsRegister(REG_SFU_OUT))
            lastReadOfR4 = inst.get();
        auto mutex = dynamic_cast<const intermediate::MutexLock*>(inst.get());
        if(mutex && mutex->locksMutex())
        {
            lastMutexLock = inst.get();
            lastMutexUnlock = nullptr;
        }
        if(mutex && mutex->releasesMutex())
        {
            lastMutexLock = nullptr;
            lastMutexUnlock = inst.get();
            // to not wrongly depend on VPM access of previous mutex block
            lastVPMRead = lastVPMReadAddress = lastVPMReadSetup = lastVPMReadWait = nullptr;
            lastVPMWrite = lastVPMWriteAddress = lastVPMWriteSetup = lastVPMWriteWait = nullptr;
        }
        if(dynamic_cast<const intermediate::SemaphoreAdjustment*>(inst.get()))
            lastSemaphoreAccess = inst.get();
        if(inst->readsRegister(REG_UNIFORM))
            lastReadOfUniform = inst.get();
        if(inst->writesRegister(REG_UNIFORM_ADDRESS))
            lastWriteOfUniformAddress = inst.get();
        if(inst->writesRegister(REG_REPLICATE_QUAD) || inst->writesRegister(REG_REPLICATE_ALL))
            lastReplicationWrite = inst.get();
        if(inst->readsRegister(REG_ACC5))
            lastReplicationRead = inst.get();
        if(inst->writesRegister(REG_VPM_OUT_SETUP))
            lastVPMWriteSetup = inst.get();
        if(inst->writesRegister(REG_VPM_IN_SETUP))
            lastVPMReadSetup = inst.get();
        if(inst->writesRegister(REG_VPM_IO))
            lastVPMWrite = inst.get();
        if(inst->readsRegister(REG_VPM_IO))
            lastVPMRead = inst.get();
        if(inst->writesRegister(REG_VPM_DMA_STORE_ADDR))
            lastVPMWriteAddress = inst.get();
        if(inst->writesRegister(REG_VPM_DMA_LOAD_ADDR))
            lastVPMReadAddress = inst.get();
        if(inst->readsRegister(REG_VPM_DMA_STORE_WAIT))
            lastVPMWriteWait = inst.get();
        if(inst->readsRegister(REG_VPM_DMA_LOAD_WAIT))
            lastVPMReadWait = inst.get();
        if(inst->writesRegister(REG_TMU0_COORD_T_V_Y) || inst->writesRegister(REG_TMU0_COORD_R_BORDER_COLOR) ||
            inst->writesRegister(REG_TMU0_COORD_B_LOD_BIAS) || inst->writesRegister(REG_TMU0_COORD_S_U_X))
            lastTMU0CoordsWrite = inst.get();
        if(inst->writesRegister(REG_TMU1_COORD_T_V_Y) || inst->writesRegister(REG_TMU1_COORD_R_BORDER_COLOR) ||
            inst->writesRegister(REG_TMU1_COORD_B_LOD_BIAS) || inst->writesRegister(REG_TMU1_COORD_S_U_X))
            lastTMU1CoordsWrite = inst.get();
        if(inst->writesRegister(REG_TMU_NOSWAP))
            lastTMUNoswapWrite = inst.get();
        if(auto loc = inst->checkOutputLocal())
            lastLocalWrites[loc] = inst.get();
        for(const Value& arg : inst->getArguments())
        {
            if(auto loc = arg.checkLocal())
                lastLocalReads[loc] = inst.get();
        }
        if(inst->writesRegister(REG_ACC5) || inst->writesRegister(REG_REPLICATE_ALL) ||
            inst->writesRegister(REG_REPLICATE_QUAD))
            lastWriteOfR5 = inst.get();
        if(inst->readsRegister(REG_ACC5) || inst->readsRegister(REG_REPLICATE_ALL) ||
            inst->readsRegister(REG_REPLICATE_QUAD))
            lastReadOfR5 = inst.get();
        if(inst->writesRegister(REG_HOST_INTERRUPT))
            lastHostInterrupt = inst.get();
        if(inst->getSignal() == SIGNAL_END_PROGRAM)
            lastProgramEnd = inst.get();
        if(dynamic_cast<const intermediate::MemoryBarrier*>(inst.get()))
            lastMemFence = inst.get();
        lastInstruction = inst.get();
    }

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        if(block.size() > 12)
        {
            auto nameFunc = [](const intermediate::IntermediateInstruction* i) -> std::string {
                return i->to_string();
            };
            auto weakEdgeFunc = [](const Dependency& dep) -> bool { return !dep.isMandatoryDelay; };
            auto edgeLabelFunc = [](const Dependency& dep) -> std::string {
                return std::to_string(dep.numDelayCycles);
            };
            DebugGraph<const intermediate::IntermediateInstruction*, Dependency,
                Directionality::DIRECTED>::dumpGraph<DependencyGraph>(*graph, "/tmp/vc4c-deps.dot", nameFunc,
                weakEdgeFunc, edgeLabelFunc);
        }
    });
    LCOV_EXCL_STOP
#endif

    PROFILE_END(createDependencyGraph);
    return graph;
}
