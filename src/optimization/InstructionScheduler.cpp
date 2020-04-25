/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "InstructionScheduler.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../analysis/DependencyGraph.h"
#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

struct NodeSorter : public std::less<intermediate::IntermediateInstruction*>
{
    static int ratePriority(intermediate::IntermediateInstruction* inst)
    {
        int priority = 0;
        // prioritizing conditional instructions keeps setting flags and their uses together
        if(inst->hasConditionalExecution())
        {
            priority += 100;
        }
        // prioritize setting of TMU_NO_SWAP to make the best of the delays required before loading
        if(inst->writesRegister(REG_TMU_NOSWAP))
            priority += 100;
        // prioritize unlocking mutex to keep critical section as small as possible
        if(inst->writesRegister(REG_MUTEX))
            priority += 80;
        // XXX give reading of work-item info (as well as parameters) a high priority (minimizes their local's
        // life-time)
        if(std::any_of(inst->getArguments().begin(), inst->getArguments().end(),
               [](const Value& val) -> bool { return val.checkLocal() && val.local()->is<Parameter>(); }))
            priority += 50;

        // writing TMU address gets higher priority, to leave enough space for utilizing the delay to actually read the
        // value
        if(inst->writesRegister(REG_TMU0_ADDRESS) || inst->writesRegister(REG_TMU1_ADDRESS))
            priority += 40;

        // prioritize triggering read of TMU and reading from TMU to be directly scheduled after the delay is up to free
        // queue space
        if(inst->getSignal().triggersReadOfR4() || inst->readsRegister(REG_TMU_OUT))
            priority += 30;

        // Increasing semaphores gets higher priority, decreasing it lower to reduce stall time
        auto semaphore = dynamic_cast<const intermediate::SemaphoreAdjustment*>(inst);
        if(semaphore)
        {
            if(semaphore->increase)
                priority += 20;
            else
                priority -= 20;
        }

        // Triggering of TMU reads gets lower priority to leave enough space between setting address and reading value
        // to utilize delay
        if(inst->getSignal().triggersReadOfR4())
            priority -= 30;

        // give vector rotations lower priority to make the usage-range of the accumulator used smaller
        if(dynamic_cast<const intermediate::VectorRotation*>(inst) &&
            dynamic_cast<const intermediate::VectorRotation*>(inst)->isFullRotationAllowed())
            priority -= 50;

        // for conditional instructions setting flags themselves not preceding the other instructions depending on same
        // flags
        if(inst->doesSetFlag())
            priority -= 60;

        // loading/calculation of literals get smaller priority, since they are used mostly locally
        if(std::all_of(inst->getArguments().begin(), inst->getArguments().end(), [](const Value& val) -> bool {
               return val.getLiteralValue() || val.hasRegister(REG_ELEMENT_NUMBER) || val.hasRegister(REG_QPU_NUMBER);
           }))
            priority -= 80;

        // given branches a lower priority moves them to the end of the block where they belong
        if(dynamic_cast<const intermediate::Branch*>(inst))
            priority -= 90;

        // mutex_acquire gets the lowest priority to not extend the critical section
        if(inst->readsRegister(REG_MUTEX))
            priority -= 100;

        return -priority;
    }

    bool operator()(intermediate::IntermediateInstruction* x, intermediate::IntermediateInstruction* y)
    {
        int prioX;
        auto it = priorities.find(x);
        if(it != priorities.end())
            prioX = it->second;
        else
            prioX = priorities[x] = ratePriority(x);
        int prioY;
        it = priorities.find(y);
        if(it != priorities.end())
            prioY = it->second;
        else
            prioY = priorities[y] = ratePriority(y);
        if(prioX == prioY)
            return x < y;
        return prioX < prioY;
    }

    // caches priorities per instruction, so we do not have to re-calculate them
    FastMap<intermediate::IntermediateInstruction*, int> priorities;

    explicit NodeSorter(std::size_t numEntries)
    {
        priorities.reserve(numEntries);
    }
};

// TODO OpenSet leaks all pending instructions if exception is thrown
using OpenSet = SortedSet<intermediate::IntermediateInstruction*, NodeSorter>;
using DelaysMap = FastMap<const intermediate::IntermediateInstruction*, std::size_t>;

static constexpr int DEFAULT_PRIORITY = 1000;
// needs to be larger then the default priority to disregard in any case
static constexpr int MIN_PRIORITY = 2000;

static int calculateSchedulingPriority(analysis::DependencyEdge& dependency, BasicBlock& block)
{
    PROFILE_START(calculateSchedulingPriority);
    int latencyLeft = static_cast<int>(dependency.data.numDelayCycles);
    auto it = --block.end();
    while(it != block.begin() && latencyLeft > 0)
    {
        if(it->get() == dependency.getInput().key)
            // we found the dependent instruction
            break;
        if(*it && (*it)->mapsToASMInstruction())
            --latencyLeft;
        --it;
    }
    if(latencyLeft <= 0 && !dependency.data.isMandatoryDelay)
    {
        auto instr = dependency.getOutput().key;
        // also value instructions freeing resources (e.g. reading periphery/releasing mutex/last read of locals)
        // and devalue instructions locking resources and using new registers, as well as instructions only depending on
        // literals
        if(instr->writesRegister(REG_MUTEX) || instr->writesRegister(REG_VPM_DMA_LOAD_ADDR) ||
            instr->writesRegister(REG_VPM_DMA_STORE_ADDR))
            latencyLeft -= 3;
        if(instr->readsRegister(REG_TMU_OUT) || instr->readsRegister(REG_VPM_IO) ||
            instr->writesRegister(REG_TMU0_ADDRESS) || instr->writesRegister(REG_TMU1_ADDRESS))
            latencyLeft -= 4;
        if(instr->readsRegister(REG_VPM_DMA_LOAD_WAIT) || instr->readsRegister(REG_VPM_DMA_STORE_WAIT) ||
            instr->readsRegister(REG_MUTEX))
            latencyLeft += 2;
        if(std::any_of(instr->getArguments().begin(), instr->getArguments().end(), [&](const Value& arg) -> bool {
               return arg.checkLocal() && arg.local()->getUsers(LocalUse::Type::READER).size() == 1;
           }))
            --latencyLeft;
        if(instr->checkOutputLocal() && instr->getOutput()->getSingleWriter() == instr)
            latencyLeft += 2;
        if(std::all_of(instr->getArguments().begin(), instr->getArguments().end(), [&](const Value& arg) -> bool {
               return arg.getLiteralValue() || arg.hasRegister(REG_QPU_NUMBER) || arg.hasRegister(REG_ELEMENT_NUMBER);
           }))
            ++latencyLeft;
    }
    // if variable latency, return remaining latency (to achieving best case)
    // otherwise, return MIN_PRIORITY
    PROFILE_END(calculateSchedulingPriority);
    return (latencyLeft > 0 && dependency.data.isMandatoryDelay) ? MIN_PRIORITY : latencyLeft;
    // TODO also look into the future and keep some instructions (e.g. vector rotations/arithmetics) close to their
    // use?? (would need to calculate priority of uses and deduct distance)
    // or look into the future and calculate the maximum mandatory (+preferred) delay between this instruction and the
    // end of block (e.g. maximum path for all dependent delays) -> prefer maximum mandatory delay > maximum preferred
    // delay > any other. Or directly calculate remaining delay into priority mandatory/preferred. Delay "behind"
    // instruction can be calculated once per instruction?!
}

static int checkDependenciesMet(analysis::DependencyNode& entry, BasicBlock& block, OpenSet& openNodes)
{
    if(!entry.hasIncomingDependencies())
        return true;
    int schedulingPriority = 0;
    entry.forAllIncomingEdges([&](analysis::DependencyNode& neighbor, analysis::DependencyEdge& edge) -> bool {
        if(openNodes.find(const_cast<intermediate::IntermediateInstruction*>(neighbor.key)) != openNodes.end())
        {
            // the dependent instruction was not yet scheduled, cannot schedule
            schedulingPriority = MIN_PRIORITY;
            return false;
        }
        // at this point, the dependent instruction is already scheduled
        else if(edge.data.numDelayCycles == 0)
        {
            // simple dependency to maintain order, is already given -> check other dependencies
            return true;
        }
        else
        {
            int priority = calculateSchedulingPriority(edge, block);
            if(priority == 0)
            {
                // dependency is completely met -> check other dependencies
                return true;
            }
            schedulingPriority += priority;
        }
        return true;
    });
    return schedulingPriority;
}

static OpenSet::const_iterator selectInstruction(OpenSet& openNodes, analysis::DependencyGraph& graph,
    BasicBlock& block, const DelaysMap& successiveMandatoryDelays, const DelaysMap& successiveDelays)
{
    // iterate open-set until entry with no more dependencies
    auto it = openNodes.begin();
    std::pair<OpenSet::const_iterator, int> selected = std::make_pair(openNodes.end(), DEFAULT_PRIORITY);
    auto lastInstruction = block.walkEnd().previousInBlock();
    PROFILE_START(SelectInstruction);
    while(it != openNodes.end())
    {
        // select first entry (with highest priority) for which all dependencies are fulfilled (with latency)
        int priority = checkDependenciesMet(graph.assertNode(*it), block, openNodes);
        if(priority < MIN_PRIORITY)
            priority -= static_cast<int>(successiveMandatoryDelays.at(*it));
        // TODO use preferred delays?
        // TODO remove adding/removing priorities in calculateSchedulingPriority?
        // TODO remove extra cases here?
        // TODO need to combine more instructions!
        // TODO success rate (i.e. emulation tests) is good, need to optimize performance
        // TODO is putting too much pressure on mutex lock (15% more than before)
        if(priority < std::get<1>(selected) ||
            // keep instructions writing the same local together to be combined more easily
            // TODO make better/check result
            (priority == std::get<1>(selected) && lastInstruction->checkOutputLocal() &&
                (*it)->writesLocal(lastInstruction->getOutput()->local())) ||
            // keep vector rotations close to their use by devaluing them after all other equal-priority instructions
            (priority == std::get<1>(selected) && lastInstruction.get<intermediate::VectorRotation>() &&
                dynamic_cast<const intermediate::VectorRotation*>(*it) == nullptr) ||
            // prefer reading of r4 to free up space in TMU queue/allow other triggers to write to r4
            (priority == std::get<1>(selected) && (*it)->readsRegister(REG_TMU_OUT)) ||
            // prefer anything over locking mutex
            (priority == std::get<1>(selected) && std::get<0>(selected) != openNodes.end() &&
                (*std::get<0>(selected))->readsRegister(REG_MUTEX)))
        {
            std::get<0>(selected) = it;
            std::get<1>(selected) = priority;
        }
        // TODO some more efficient version? E.g. caching priorities, check from lowest, update, take new lowest
        ++it;
    }
    PROFILE_END(SelectInstruction);

    if(std::get<0>(selected) != openNodes.end())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Selected '" << (*std::get<0>(selected))->to_string()
                << "' as next instruction with remaining latency of " << std::get<1>(selected) << logging::endl);
        return std::get<0>(selected);
    }

    return openNodes.end();
}

// FIXME largely extends usage-ranges and increases pressure on registers
// FIXME increases mutex-ranges!!
// TODO make sure no more than 4(8?) TMU requests/responds are queued at any time (per TMU?)
// - Specification documents 8 entries of single row (for general memory query), but does not specify whether one queue
// or per TMU
// - Tests show that up to 8 requests per TMU run (whether result is correct not tested!!), 9+ hang QPU

/*
 * Select an instruction which does not depend on any instruction (not yet scheduled) anymore and insert it into the
 * basic block
 */
static void selectInstructions(analysis::DependencyGraph& graph, BasicBlock& block,
    const DelaysMap& successiveMandatoryDelays, const DelaysMap& successiveDelays)
{
    // 1. "empty" basic block without deleting the instructions, skipping the label
    auto it = block.walk().nextInBlock();
    OpenSet openNodes(NodeSorter(block.size()));
    while(!it.isEndOfBlock())
    {
        if(it.has() &&
            !(it.get<intermediate::Nop>() && !it->hasSideEffects() &&
                it.get<const intermediate::Nop>()->type != intermediate::DelayType::THREAD_END))
            // remove all non side-effect NOPs
            openNodes.emplace(it.release());
        it.erase();
    }

    // 2. fill again with reordered instructions
    while(!openNodes.empty())
    {
        auto inst = selectInstruction(openNodes, graph, block, successiveMandatoryDelays, successiveDelays);
        if(inst == openNodes.end())
        {
            // no instruction could be scheduled not violating the fixed latency, insert NOPs
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to schedule an instruction, falling back to inserting NOP" << logging::endl);
            block.walkEnd().emplace(new intermediate::Nop(intermediate::DelayType::WAIT_REGISTER));
        }
        else
        {
            block.walkEnd().emplace(*inst);
            openNodes.erase(inst);
        }
    }
}

bool optimizations::reorderInstructions(const Module& module, Method& kernel, const Configuration& config)
{
    for(BasicBlock& bb : kernel)
    {
        auto dependencies = analysis::DependencyGraph::createGraph(bb);
        // calculate required and recommended successive delays for all instructions
        DelaysMap successiveMandatoryDelays;
        DelaysMap successiveDelays;
        PROFILE_START(CalculateCriticalPath);
        for(const auto& node : dependencies->getNodes())
        {
            // since we cache all delays (also for all intermediate results), it is only calculated once per node
            node.second.calculateSucceedingCriticalPathLength(true, &successiveMandatoryDelays);
            node.second.calculateSucceedingCriticalPathLength(false, &successiveDelays);
        }
        PROFILE_END(CalculateCriticalPath);
        selectInstructions(*dependencies, bb, successiveMandatoryDelays, successiveDelays);
    }
    return false;
}
