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
    if((has_flag(type, DependencyType::CONSUME_SIGNAL) || has_flag(type, DependencyType::SIGNAL_ORDER)) &&
        instr->signal.hasSideEffects())
    {
        // valid as long as the other instruction does not trigger a signal
        return false;
    }
    if(has_flag(type, DependencyType::OVERWRITE_SIGNAL) && instr->readsRegister(REG_SFU_OUT))
    {
        // valid as long as the other instruction does not consume the signal
        return false;
    }
    if((has_flag(type, DependencyType::CONDITIONAL) || has_flag(type, DependencyType::FLAG_ORDER)) &&
        instr->setFlags == SetFlag::SET_FLAGS)
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
        if(has_flag(edge.data.type, DependencyType::CONDITIONAL))
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
        if(has_flag(edge.data.type, DependencyType::CONSUME_SIGNAL))
        {
            setter = &neighbor;
            return false;
        }
        return true;
    });
    return setter;
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
    // can only pack from register-file A which requires a read-after-write delay of at least 1 instruction
    bool hasPackMode = node.key->hasPackMode();
    node.key->forUsedLocals([&](const Local* loc, LocalUse::Type type) -> void {
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
        unsigned distance =
            (!isVectorRotation && !hasPackMode && isLocallyLimited(loc, node.key, lastWrite, lastRead)) ? 0 : 1;
        if(has_flag(type, LocalUse::Type::READER))
        {
            // any reading of a local depends on the previous write
            if(lastWrite != nullptr)
            {
                auto& otherNode = graph.assertNode(lastWrite);
                addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::FLOW, distance,
                    isVectorRotation || hasPackMode || lastWrite->hasUnpackMode());
            }
        }
        if(has_flag(type, LocalUse::Type::WRITER))
        {
            if(lastWrite != nullptr)
            {
                // writing a local needs to preserve the order the local is written before
                auto& otherNode = graph.assertNode(lastWrite);
                addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::OUTPUT, distance,
                    lastWrite->hasUnpackMode());
            }
            if(lastRead != nullptr)
            {
                // writing a local must be ordered after previous reads
                auto& otherNode = graph.assertNode(lastRead);
                addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::ANTI);
            }
        }
    });
}

static void createFlagDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastSettingOfFlags,
    const intermediate::IntermediateInstruction* lastConditional)
{
    if(node.key->hasConditionalExecution())
    {
        // any conditional execution depends on flags being set previously
        auto& otherNode = graph.assertNode(lastSettingOfFlags);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::CONDITIONAL);
    }
    if(node.key->setFlags == SetFlag::SET_FLAGS && lastSettingOfFlags != nullptr)
    {
        // any setting of flags must be ordered after the previous setting of flags
        auto& otherNode = graph.assertNode(lastSettingOfFlags);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::FLAG_ORDER);
    }
    if(node.key->setFlags == SetFlag::SET_FLAGS && lastConditional != nullptr)
    {
        // any setting of flags must be ordered after any previous use of these flags
        auto& otherNode = graph.assertNode(lastSettingOfFlags);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::OVERWRITE_FLAGS);
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
        if(lastTriggerOfR4->hasValueType(ValueType::REGISTER) && lastTriggerOfR4->getOutput()->reg.triggersReadOfR4())
        {
            // delay slots for SFU calculations
            delayCycles = 2;
            fixedDelay = true;
        }
        else if(lastTriggerOfR4->signal == SIGNAL_LOAD_TMU0 || lastTriggerOfR4->signal == SIGNAL_LOAD_TMU1)
        {
            // TODO what is the recommended delay [9, 20]
            delayCycles = 9;
        }
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::CONSUME_SIGNAL, delayCycles, fixedDelay);
    }
    if(node.key->signal.triggersReadOfR4() ||
        (node.key->hasValueType(ValueType::REGISTER) && node.key->getOutput()->reg.triggersReadOfR4()))
    {
        if(lastTriggerOfR4 != nullptr)
        {
            // any trigger of r4 needs to be ordered after the previous trigger of r4
            auto& otherNode = graph.assertNode(lastTriggerOfR4);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::SIGNAL_ORDER);
        }
        if(lastReadOfR4 != nullptr)
        {
            // any trigger of r4 must be ordered after the previous read of r4
            auto& otherNode = graph.assertNode(lastReadOfR4);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::OVERWRITE_SIGNAL);
        }
    }
}

static void createMutexDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastMutexLock,
    const intermediate::IntermediateInstruction* lastMutexUnlock,
    const intermediate::IntermediateInstruction* lastInstruction)
{
    if((node.key->hasValueType(ValueType::REGISTER) && node.key->getOutput()->reg.isVertexPipelineMemory()) ||
        std::any_of(node.key->getArguments().begin(), node.key->getArguments().end(),
            [](const Value& arg) -> bool {
                return arg.hasType(ValueType::REGISTER) && arg.reg.isVertexPipelineMemory();
            }) ||
        node.key->writesRegister(REG_MUTEX))
    {
        // any VPM operation or mutex unlock must be ordered after the corresponding mutex lock
        auto& otherNode = graph.assertNode(lastMutexLock);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::MUTEX_LOCK);
    }
    if(node.key->readsRegister(REG_MUTEX) && lastMutexUnlock != nullptr)
    {
        // any mutex lock must be ordered after the previous unlock, if any
        auto& otherNode = graph.assertNode(lastMutexUnlock);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createSemaphoreDepencies(
    DependencyGraph& graph, DependencyNode& node, const intermediate::IntermediateInstruction* lastSemaphoreAccess)
{
    const auto semaphoreAccess = dynamic_cast<const intermediate::SemaphoreAdjustment*>(node.key);
    if(semaphoreAccess != nullptr && lastSemaphoreAccess != nullptr)
    {
        // the order of semaphore-accesses cannot be modified!
        auto& otherNode = graph.assertNode(lastSemaphoreAccess);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::SEMAPHORE_ORDER);
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
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::FLOW, 2, true);
        }
        if(lastReadOfUniform != nullptr)
        {
            // the order UNIFORMS are read in must not change!
            auto& otherNode = graph.assertNode(lastReadOfUniform);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::READ_ORDER);
        }
    }
    if(node.key->writesRegister(REG_UNIFORM_ADDRESS))
    {
        if(lastWriteOfUniformAddress != nullptr)
        {
            // any writing of UNIFORM address must be ordered after the previous write
            auto& otherNode = graph.assertNode(lastWriteOfUniformAddress);
            // just to be sure, add the mandatory distance here too
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::OUTPUT, 2, true);
        }
        if(lastReadOfUniform != nullptr)
        {
            // any writing of UNIFORM address must be ordered after any previous reading of UNIFORMs
            auto& otherNode = graph.assertNode(lastReadOfUniform);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::ANTI);
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
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::ANTI);
        }
        if(lastReplicationWrite != nullptr)
        {
            // any writing to the replication registers need to be executed after the previous write, if any
            auto& otherNode = graph.assertNode(lastReplicationWrite);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::OUTPUT);
        }
    }
    if(node.key->readsRegister(REG_ACC5))
    {
        if(lastReplicationRead != nullptr)
        {
            // any reading to the replication registers need to be executed after the previous read, if any
            auto& otherNode = graph.assertNode(lastReplicationRead);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::READ_ORDER);
        }
        if(lastReplicationWrite != nullptr)
        {
            // any reading to the replication registers need to be executed after the previous write, if any
            auto& otherNode = graph.assertNode(lastReplicationWrite);
            addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::FLOW);
        }
    }
}

static void createVPMSetupDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWriteSetup,
    const intermediate::IntermediateInstruction* lastVPMReadSetup)
{
    if(lastVPMWriteSetup != nullptr &&
        (node.key->writesRegister(REG_VPM_IO) || node.key->writesRegister(REG_VPM_OUT_SETUP) ||
            node.key->writesRegister(REG_VPM_OUT_ADDR)))
    {
        // any other VPM write setup, VPM write or VPM write address setup must be ordered after the VPM write setup
        auto& otherNode = graph.assertNode(lastVPMWriteSetup);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastVPMReadSetup != nullptr &&
        (node.key->readsRegister(REG_VPM_IO) || node.key->writesRegister(REG_VPM_IN_SETUP) ||
            node.key->writesRegister(REG_VPM_IN_ADDR)))
    {
        // any other VPM read setup, VPM read or VPM read address setup must be ordered after the VPM read setup
        auto& otherNode = graph.assertNode(lastVPMReadSetup);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createVPMIODependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWrite, const intermediate::IntermediateInstruction* lastVPMRead)
{
    if(lastVPMWrite != nullptr &&
        (node.key->writesRegister(REG_VPM_IO) || node.key->writesRegister(REG_VPM_OUT_ADDR) ||
            node.key->writesRegister(REG_MUTEX)))
    {
        // any other VPM write, VPM write address setup or unlocking mutex must be executed aftre the VPM write
        auto& otherNode = graph.assertNode(lastVPMWrite);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastVPMRead != nullptr && (node.key->readsRegister(REG_VPM_IO) || node.key->writesRegister(REG_MUTEX)))
    {
        // any other VPM read or unlocking mutex must be executed aftre the VPM read
        auto& otherNode = graph.assertNode(lastVPMRead);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createVPMAddressDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWriteAddress,
    const intermediate::IntermediateInstruction* lastVPMReadAddress)
{
    if(node.key->readsRegister(REG_VPM_OUT_WAIT))
    {
        // the VPM write wait instruction needs to be executed after the setting of the VPM write address
        auto& otherNode = graph.assertNode(lastVPMWriteAddress);
        // XXX correct delay
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER, 10);
    }
    if(node.key->readsRegister(REG_VPM_IN_WAIT))
    {
        // the VPM read wait instruction needs to be executed after the setting of the VPM read address
        auto& otherNode = graph.assertNode(lastVPMReadAddress);
        // XXX correct delay
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER, 6);
    }
}

static void createVPMWaitDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastVPMWriteWait,
    const intermediate::IntermediateInstruction* lastVPMReadWait)
{
    if(lastVPMWriteWait != nullptr &&
        (node.key->writesRegister(REG_MUTEX) ||
            (node.key->hasValueType(ValueType::REGISTER) && node.key->getOutput()->reg.isVertexPipelineMemory())))
    {
        // unlocking of the mutex as well as any following VPM accesses (within that mutex) need to be ordered after the
        // VPM write wait instruction
        auto& otherNode = graph.assertNode(lastVPMWriteWait);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
    if(node.key->readsRegister(REG_VPM_IO))
    {
        // all reading of VPM need to be ordered after the VPM read wait instruction
        auto& otherNode = graph.assertNode(lastVPMReadWait);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
}

static void createTMUCoordinateDependencies(DependencyGraph& graph, DependencyNode& node,
    const intermediate::IntermediateInstruction* lastTMU0CoordsWrite,
    const intermediate::IntermediateInstruction* lastTMU1CoordsWrite)
{
    if(lastTMU0CoordsWrite != nullptr && node.key->hasValueType(ValueType::REGISTER) &&
        node.key->getOutput()->reg.isTextureMemoryUnit())
    {
        // do not re-order writes to same TMU (especially not writes to S coordinates, which triggers the read)
        // TODO only check for same TMU!
        auto& otherNode = graph.assertNode(lastTMU0CoordsWrite);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
    if(lastTMU1CoordsWrite != nullptr && node.key->hasValueType(ValueType::REGISTER) &&
        node.key->getOutput()->reg.isTextureMemoryUnit())
    {
        // do not re-order writes to same TMU (especially not writes to S coordinates, which triggers the read)
        // TODO only check for same TMU!
        auto& otherNode = graph.assertNode(lastTMU1CoordsWrite);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
    if(node.key->signal == SIGNAL_LOAD_TMU0)
    {
        // triggering of read depends on the memory address being set previously
        auto& otherNode = graph.assertNode(lastTMU0CoordsWrite);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
    if(node.key->signal == SIGNAL_LOAD_TMU1)
    {
        // triggering of read depends on the memory address being set previously
        auto& otherNode = graph.assertNode(lastTMU1CoordsWrite);
        addDependency(node.getOrCreateEdge(&otherNode).data, DependencyType::PERIPHERY_ORDER);
    }
}

std::unique_ptr<DependencyGraph> DependencyGraph::createGraph(const BasicBlock& block)
{
    PROFILE_START(createDependencyGraph);
    std::unique_ptr<DependencyGraph> graph(new DependencyGraph());

    /*
     * TODO do we really need these types of dependencies??
     *
     * conditional <- set-flags (setting of flags depends on previous conditional, if any)
     * mutex unlock <- mutex lock
     *
     * These prevent the whole mutex-blocks to be re-ordered
     */
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
    const intermediate::IntermediateInstruction* lastInstruction = nullptr;
    FastMap<const Local*, const intermediate::IntermediateInstruction*> lastLocalWrites;
    FastMap<const Local*, const intermediate::IntermediateInstruction*> lastLocalReads;
    // TODO "normal" register dependencies?

    auto it = block.begin();
    while(!it.isEndOfBlock())
    {
        if(it.has<intermediate::Nop>())
        {
            it.nextInBlock();
            continue;
        }
        auto& node = graph->getOrCreateNode(it.get());

        createLocalDependencies(*graph.get(), node, lastLocalWrites, lastLocalReads);
        createFlagDependencies(*graph.get(), node, lastSettingOfFlags, lastConditional);
        createR4Dependencies(*graph.get(), node, lastTriggerOfR4, lastReadOfR4);
        createMutexDependencies(*graph.get(), node, lastMutexLock, lastMutexUnlock, lastInstruction);
        createSemaphoreDepencies(*graph.get(), node, lastSemaphoreAccess);
        createUniformDependencies(*graph.get(), node, lastWriteOfUniformAddress, lastReadOfUniform);
        createReplicationDependencies(*graph.get(), node, lastReplicationWrite, lastReplicationRead);
        createVPMSetupDependencies(*graph.get(), node, lastVPMWriteSetup, lastVPMReadSetup);
        createVPMIODependencies(*graph.get(), node, lastVPMWrite, lastVPMRead);
        createVPMAddressDependencies(*graph.get(), node, lastVPMWriteAddress, lastVPMReadAddress);
        createVPMWaitDependencies(*graph.get(), node, lastVPMWriteWait, lastVPMReadWait);
        createTMUCoordinateDependencies(*graph.get(), node, lastTMU0CoordsWrite, lastTMU1CoordsWrite);

        // update the cached values
        if(it->setFlags == SetFlag::SET_FLAGS)
            lastSettingOfFlags = it.get();
        if(it->hasConditionalExecution())
            lastConditional = it.get();
        if(it->signal.triggersReadOfR4() ||
            (it->hasValueType(ValueType::REGISTER) && it->getOutput()->reg.triggersReadOfR4()))
            lastTriggerOfR4 = it.get();
        if(it->readsRegister(REG_SFU_OUT))
            lastReadOfR4 = it.get();
        if(it.has<intermediate::MutexLock>() && it.get<const intermediate::MutexLock>()->locksMutex())
        {
            lastMutexLock = it.get();
            lastMutexUnlock = nullptr;
        }
        if(it.has<intermediate::MutexLock>() && it.get<const intermediate::MutexLock>()->releasesMutex())
        {
            lastMutexLock = nullptr;
            lastMutexUnlock = it.get();
            // to not wrongly depend on VPM access of previous mutex block
            lastVPMRead = lastVPMReadAddress = lastVPMReadSetup = lastVPMReadWait = nullptr;
            lastVPMWrite = lastVPMWriteAddress = lastVPMWriteSetup = lastVPMWriteWait = nullptr;
        }
        if(it.has<intermediate::SemaphoreAdjustment>())
            lastSemaphoreAccess = it.get();
        if(it->readsRegister(REG_UNIFORM))
            lastReadOfUniform = it.get();
        if(it->writesRegister(REG_UNIFORM_ADDRESS))
            lastWriteOfUniformAddress = it.get();
        if(it->writesRegister(REG_REPLICATE_QUAD) || it->writesRegister(REG_REPLICATE_ALL))
            lastReplicationWrite = it.get();
        if(it->readsRegister(REG_ACC5))
            lastReplicationRead = it.get();
        if(it->writesRegister(REG_VPM_OUT_SETUP))
            lastVPMWriteSetup = it.get();
        if(it->writesRegister(REG_VPM_IN_SETUP))
            lastVPMReadSetup = it.get();
        if(it->writesRegister(REG_VPM_IO))
            lastVPMWrite = it.get();
        if(it->readsRegister(REG_VPM_IO))
            lastVPMRead = it.get();
        if(it->writesRegister(REG_VPM_OUT_ADDR))
            lastVPMWriteAddress = it.get();
        if(it->writesRegister(REG_VPM_IN_ADDR))
            lastVPMReadAddress = it.get();
        if(it->readsRegister(REG_VPM_OUT_WAIT))
            lastVPMWriteWait = it.get();
        if(it->readsRegister(REG_VPM_IN_WAIT))
            lastVPMReadWait = it.get();
        if(it->writesRegister(REG_TMU0_COORD_T_V_Y) || it->writesRegister(REG_TMU0_COORD_R_BORDER_COLOR) ||
            it->writesRegister(REG_TMU0_COORD_B_LOD_BIAS))
            lastTMU0CoordsWrite = it.get();
        if(it->writesRegister(REG_TMU1_COORD_T_V_Y) || it->writesRegister(REG_TMU1_COORD_R_BORDER_COLOR) ||
            it->writesRegister(REG_TMU1_COORD_B_LOD_BIAS))
            lastTMU1CoordsWrite = it.get();
        if(it->hasValueType(ValueType::LOCAL))
            lastLocalWrites[it->getOutput()->local] = it.get();
        for(const Value& arg : it->getArguments())
        {
            if(arg.hasType(ValueType::LOCAL))
                lastLocalReads[arg.local] = it.get();
        }
        lastInstruction = it.get();
        it.nextInBlock();
    }

#ifdef DEBUG_MODE
    auto nameFunc = [](const intermediate::IntermediateInstruction* i) -> std::string { return i->to_string(); };
    auto weakEdgeFunc = [](const Dependency& dep) -> bool { return dep.isMandatoryDelay; };
    auto edgeLabelFunc = [](const Dependency& dep) -> std::string { return std::to_string(dep.numDelayCycles); };
    DebugGraph<const intermediate::IntermediateInstruction*, Dependency, true>::dumpGraph<DependencyGraph>(
        *graph.get(), "/tmp/vc4c-deps.dot", nameFunc, weakEdgeFunc, edgeLabelFunc);
#endif

    PROFILE_END(createDependencyGraph);
    return graph;
}