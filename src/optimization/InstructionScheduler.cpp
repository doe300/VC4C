/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "InstructionScheduler.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/DebugGraph.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../periphery/VPM.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;
#if 0 // TODO remove if unused
/*
 * For dependencies of "load_tmu -> read r4" and "writing sfu-register -> read r4"
 */
static bool checkSFUTMUAccess(const intermediate::IntermediateInstruction* inst, bool isTMULoad)
{
    if((inst->signal == SIGNAL_LOAD_TMU0 || inst->signal == SIGNAL_LOAD_TMU1) && !isTMULoad)
        // cannot insert a trigger of reading from TMU into r4 unless this dependency is also for a load of TMU into r4
        // -> appends to TMU queue
        return false;
    if(inst->signal.triggersReadOfR4())
        // cannot insert an instruction triggering some load into r4
        return false;
    for(const Value& arg : inst->getArguments())
        if(arg.hasRegister(REG_SFU_OUT))
            // cannot insert any instruction reading from r4
            return false;
    if(inst->getOutput())
    {
        const Value output = inst->getOutput().value();
        if(output.hasType(ValueType::REGISTER) || output.reg.triggersReadOfR4())
            // cannot insert any instruction triggering a write to r4
            return false;
    }
    return true;
}

/*
 * For read-after-write dependencies
 */
static bool checkReadAfterWriteAccess(const intermediate::IntermediateInstruction* inst, const Value& criticalValue)
{
    for(const Value& arg : inst->getArguments())
        if(arg == criticalValue)
            // cannot insert any instruction reading the value
            return false;
    if(inst->getOutput().is(criticalValue))
    {
        // cannot insert any instruction writing to the value
        return false;
    }
    return true;
}

static Dependency createSFUDependency(const Value& sfuParam, const Value& result)
{
    Dependency dep;
    dep.latency = 2;
    dep.fixedLatency = true;
    dep.checkFunc = [sfuParam, result](const intermediate::IntermediateInstruction* inst) -> bool {
        if((sfuParam.hasType(ValueType::LOCAL) && inst->writesLocal(sfuParam.local)) ||
            (result.hasType(ValueType::LOCAL) && inst->readsLocal(result.local)))
            return false;
        return checkSFUTMUAccess(inst, false);
    };

    return dep;
}

static Dependency createTMUDependency(const Value& output, const Signaling whichTMU)
{
    Dependency dep;
    // XXX values 9? 20?
    dep.latency = 9;
    dep.fixedLatency = false;
    dep.checkFunc = [output](const intermediate::IntermediateInstruction* inst) -> bool {
        if(output.hasType(ValueType::LOCAL) && (inst->readsLocal(output.local) || inst->writesLocal(output.local)))
            return false;
        return checkSFUTMUAccess(inst, true);
    };

    return dep;
}

static Dependency createDataHazardDependency(const Value& criticalValue, bool isReadAfterWrite, bool isLocallyLimited)
{
    Dependency dep;
    dep.latency = 1;
    // if the instruction is locally limited, try to create the space (enables to move to physical register), but it is
    // also okay, if no instruction was found  otherwise, need to have at least 1 instruction in between read and write
    // for e.g. write-after-read, no delay is necessary
    dep.fixedLatency = isReadAfterWrite && !isLocallyLimited;
    dep.checkFunc = [criticalValue](const intermediate::IntermediateInstruction* inst) -> bool {
        return checkReadAfterWriteAccess(inst, criticalValue);
    };
    return dep;
}

static Dependency createFlagsDependency()
{
    Dependency dep;
    dep.latency = 0;
    dep.fixedLatency = false;
    dep.checkFunc = [](const intermediate::IntermediateInstruction* inst) -> bool {
        return inst->setFlags == SetFlag::DONT_SET;
    };
    return dep;
}

static Dependency createVPMWaitDependency(bool isVPMWrite)
{
    Dependency dep;
    // XXX values
    dep.latency = isVPMWrite ? 10 : 6;
    dep.fixedLatency = false;
    dep.checkFunc = [](const intermediate::IntermediateInstruction* inst) -> bool {
        const auto& args = inst->getArguments();
        if(std::any_of(args.begin(), args.end(), [](const Value& val) -> bool {
               return val.hasType(ValueType::REGISTER) && val.reg.isVertexPipelineMemory();
           }))
            return false;
        return !(inst->hasValueType(ValueType::REGISTER) && inst->getOutput()->reg.isVertexPipelineMemory());
    };
    return dep;
}

/*
 * Only forbids re-ordering
 */
static Dependency createSimpleDependency()
{
    Dependency dep;
    dep.latency = 0;
    dep.fixedLatency = false;
    dep.checkFunc = [](const intermediate::IntermediateInstruction* inst) -> bool { return true; };
    return dep;
}

static DependencyNode* findLastInstruction(
    InstructionWalker it, DependencyGraph& graph, const std::function<bool(InstructionWalker)>& predicate)
{
    while(!it.isStartOfBlock())
    {
        it.previousInBlock();

        if(predicate(it))
            return &graph.assertNode(it.get());
    }

    return nullptr;
}

static void forAllInstructions(
    InstructionWalker it, DependencyGraph& graph, const std::function<bool(InstructionWalker)>& predicate)
{
    while(!it.isStartOfBlock() && predicate(it.previousInBlock()))
    {
        // empty on purpose
    }
}

static DependencyGraph createDependencyGraph(BasicBlock& block)
{
    DependencyGraph graph;

    /*
     * TODO unresolved:
     * - make sure not to separate setting and reading of flags, but whole set-and-use-flags blocks can be re-ordered
     * - usage of r4 could be allowed to be re-ordered, if the whole blocks (triggering read and reading r4) are
     * re-ordered
     */

    auto it = block.begin().nextInBlock();
    while(!it.isEndOfBlock())
    {
        // we need a new node for every instruction
        auto& node = graph.getOrCreateNode(it.get());

        bool dependsOnSomething = false;
        for(const Value& arg : it->getArguments())
        {
            if(arg.hasType(ValueType::LOCAL))
            {
                arg.local->forUsers(LocalUse::Type::BOTH,
                    [it, &block, &arg, &graph, &node, &dependsOnSomething](const LocalUser* user) -> void {
                        if(user == it.get())
                            return;
                        // skip read-after-read
                        if(!user->writesLocal(arg.local) && !it->writesLocal(arg.local))
                            return;
                        if(graph.findNode(const_cast<LocalUser*>(user)))
                        {
                            logging::debug() << "Creating simple local-dependency for '" << it->to_string() << "' on '"
                                             << user->to_string() << "'" << logging::endl;
                            auto& otherNode = graph.assertNode(const_cast<LocalUser*>(user));
                            // user lies within the same basic block (more accurate, before this instruction)
                            node.addEdge(&otherNode,
                                createDataHazardDependency(
                                    arg, user->writesLocal(arg.local), block.isLocallyLimited(it, arg.local)));
                            dependsOnSomething = true;
                        }
                    });
            }
            else if(arg.hasRegister(REG_MUTEX))
            {
                // mutex acquire depends on previous mutex release
                auto n = findLastInstruction(
                    it, graph, [](InstructionWalker check) -> bool { return check->writesRegister(REG_MUTEX); });
                if(n != nullptr)
                {
                    logging::debug() << "Creating dependency from acquiring mutex '" << it->to_string()
                                     << "' on releasing mutex '" << n->key->to_string() << "'" << logging::endl;
                    node.addEdge(n, createSimpleDependency());
                    dependsOnSomething = true;
                }
            }
            else if(arg.hasRegister(REG_SFU_OUT))
            {
                // depends on trigger of read as well as the previous read of r4 (if any)
                bool isSFUDependency = false;
                bool isTMUDependency = false;
                auto n = findLastInstruction(
                    it, graph, [arg, &isSFUDependency, &isTMUDependency](InstructionWalker check) -> bool {
                        if(check->hasValueType(ValueType::REGISTER) && check->getOutput()->reg.isSpecialFunctionsUnit())
                        {
                            isSFUDependency = true;
                            return true;
                        }
                        if(check->signal.triggersReadOfR4())
                        {
                            isTMUDependency = check->signal == SIGNAL_LOAD_TMU0 || check->signal == SIGNAL_LOAD_TMU1;
                            return true;
                        }
                        return false;
                    });
                if(n == nullptr)
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Found reading of r4 without triggering write to r4", it->to_string());
                logging::debug() << "Creating dependency from reading of r4 '" << it->to_string()
                                 << "' on triggering write to r4 '" << n->key->to_string() << "'" << logging::endl;
                if(isSFUDependency)
                    node.addEdge(n, createSFUDependency(n->key->assertArgument(0), it->getOutput().value()));
                else if(isTMUDependency)
                    node.addEdge(n, createTMUDependency(it->getOutput().value(), n->key->signal));
                else
                    throw CompilationError(CompilationStep::OPTIMIZER, "Unknown dependency for trigger reading of r4",
                        n->key->to_string());
                // do not re-order multiple readings of r4
                n = findLastInstruction(
                    it, graph, [](InstructionWalker check) -> bool { return check->readsRegister(REG_SFU_OUT); });
                if(n != nullptr)
                {
                    logging::debug() << "Creating dependency from reading of r4 '" << it->to_string()
                                     << "' on previous reading of r4 '" << n->key->to_string() << "'" << logging::endl;
                    node.addEdge(n, createSimpleDependency());
                }
                dependsOnSomething = true;
            }
            else if(arg.hasRegister(REG_UNIFORM))
            {
                // need space to writing uniform_address
                auto n = findLastInstruction(it, graph,
                    [](InstructionWalker check) -> bool { return check->writesRegister(REG_UNIFORM_ADDRESS); });
                if(n != nullptr)
                {
                    logging::debug() << "Creating dependency from reading UNIFORM '" << it->to_string()
                                     << "' on setting UNIFORM-address '" << n->key->to_string() << "'" << logging::endl;
                    // TODO need some latency?
                    node.addEdge(n, createSimpleDependency());
                    dependsOnSomething = true;
                }
                // cannot re-order the order of UNIFORMS being read
                else
                {
                    n = findLastInstruction(
                        it, graph, [](InstructionWalker check) -> bool { return check->readsRegister(REG_UNIFORM); });
                    if(n != nullptr)
                    {
                        logging::debug() << "Creating dependency from reading UNIFORM '" << it->to_string()
                                         << "' on previous reading of UNIFORM '" << n->key->to_string() << "'"
                                         << logging::endl;
                        node.addEdge(n, createSimpleDependency());
                        dependsOnSomething = true;
                    }
                }
            }
            else if(arg.hasRegister(REG_REPLICATE_ALL) || arg.hasRegister(REG_REPLICATE_QUAD) ||
                arg.hasRegister(REG_ACC5))
            {
                // handles explicit registers which are written/read just like locals
                auto n = findLastInstruction(
                    it, graph, [arg](InstructionWalker check) -> bool { return check->writesRegister(arg.reg); });
                if(n == nullptr)
                    throw CompilationError(CompilationStep::OPTIMIZER,
                        "Found reading of explicit register without previous write to it", it->to_string());
                logging::debug() << "Creating dependency from reading explicit register '" << it->to_string()
                                 << "' on previous write '" << n->key->to_string() << "'" << logging::endl;
                node.addEdge(n, createDataHazardDependency(arg, true, !arg.hasRegister(REG_ACC5)));
                dependsOnSomething = true;
            }
            else if(arg.hasRegister(REG_VPM_IO))
            {
                // depends on all previous VPM setups up to the mutex being set
                forAllInstructions(
                    it, graph, [it, &dependsOnSomething, &node, &graph](InstructionWalker check) -> bool {
                        // break at acquiring mutex
                        if(check->readsRegister(REG_MUTEX))
                            return false;
                        else if(check->writesRegister(REG_VPM_IN_SETUP))
                        {
                            logging::debug()
                                << "Creating dependency from reading VPM '" << it->to_string()
                                << "' on setting VPM setup '" << check->to_string() << "'" << logging::endl;
                            node.addEdge(&graph.assertNode(check.get()), createSimpleDependency());
                            dependsOnSomething = true;
                        }
                        // also depends on any earlier reads within this mutex-lock
                        else if(check->readsRegister(REG_VPM_IO))
                        {
                            logging::debug()
                                << "Creating dependency from reading VPM '" << it->to_string()
                                << "' on previous reading of VPM '" << check->to_string() << "'" << logging::endl;
                            node.addEdge(&graph.assertNode(check.get()), createSimpleDependency());
                            dependsOnSomething = true;
                        }
                        return true;
                    });
            }
            else if(arg.hasRegister(REG_VPM_IN_WAIT) || arg.hasRegister(REG_VPM_OUT_WAIT))
            {
                // depends on writing DMA address
                auto n = findLastInstruction(it, graph, [arg](InstructionWalker check) -> bool {
                    return check->writesRegister(arg.hasRegister(REG_VPM_IN_WAIT) ? REG_VPM_IN_ADDR : REG_VPM_OUT_ADDR);
                });
                if(n == nullptr)
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Found VPM wait without setting of VPM address", it->to_string());
                logging::debug() << "Creating dependency from VPM wait '" << it->to_string()
                                 << "' on setting of VPM address '" << n->key->to_string() << "'" << logging::endl;
                node.addEdge(n, createVPMWaitDependency(arg.hasRegister(REG_VPM_OUT_WAIT)));
                dependsOnSomething = true;
            }
        }
        if(it->getOutput())
        {
            // TODO write-after-read etc.
            // TODO: writing of TMU address has minimum space to setting TMU-noswap, VPM depends on last mutex-release,
            // mutex-release on last mutex-aqcure, ...

            if(it->writesRegister(REG_VPM_IN_SETUP) || it->writesRegister(REG_VPM_OUT_SETUP))
            {
                // depends on mutex being set
                // TODO actually depends on all previous VPM (DMA) setups
                // for writing into VPM, also depends on all VPM writes in the same mutex-block
                auto n = findLastInstruction(
                    it, graph, [](InstructionWalker check) -> bool { return check->readsRegister(REG_MUTEX); });
                if(n == nullptr)
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Found VPM setup without setting of mutex", it->to_string());
                logging::debug() << "Creating dependency from VPM setup '" << it->to_string()
                                 << "' on setting of mutex '" << n->key->to_string() << "'" << logging::endl;
                node.addEdge(n, createSimpleDependency());
                dependsOnSomething = true;
            }
            else if(it->writesRegister(REG_VPM_IO))
            {
                // depends on QPU <-> VPM setup (and mutex)
                bool alreadyDependsOnVPMWrite = false;
                forAllInstructions(it, graph,
                    [it, &graph, &node, &dependsOnSomething, &alreadyDependsOnVPMWrite](
                        InstructionWalker check) -> bool {
                        // break at acquiring mutex
                        if(check->readsRegister(REG_MUTEX))
                            return false;
                        auto setup = check->precalculate(1);
                        if(check->writesRegister(REG_VPM_OUT_SETUP) && setup &&
                            periphery::VPWSetup::fromLiteral(setup->getLiteralValue()->unsignedInt()).isGenericSetup())
                        {
                            logging::debug()
                                << "Creating dependency from VPM write '" << it->to_string()
                                << "' on VPM generic setup '" << check->to_string() << "'" << logging::endl;
                            node.addEdge(&graph.assertNode(check.get()), createSimpleDependency());
                            dependsOnSomething = true;
                        }
                        // also depends on the previous VPM write in the same block
                        if(!alreadyDependsOnVPMWrite && check->writesRegister(REG_VPM_IO))
                        {
                            logging::debug()
                                << "Creating dependency from VPM write '" << it->to_string()
                                << "' on previous VPM write '" << check->to_string() << "'" << logging::endl;
                            node.addEdge(&graph.assertNode(check.get()), createSimpleDependency());
                            dependsOnSomething = true;
                            alreadyDependsOnVPMWrite = true;
                        }
                        return true;
                    });
            }
            else if(it->writesRegister(REG_VPM_IN_ADDR) || it->writesRegister(REG_VPM_OUT_ADDR))
            {
                // depends on previous VPM setups (and mutex)
                auto n = findLastInstruction(it, graph, [it](InstructionWalker check) -> bool {
                    auto setup = check->precalculate(1);
                    if(it->writesRegister(REG_VPM_OUT_ADDR) && check->writesRegister(REG_VPM_OUT_SETUP) && setup &&
                        periphery::VPWSetup::fromLiteral(setup->getLiteralValue()->unsignedInt()).isDMASetup())
                        return true;
                    if(it->writesRegister(REG_VPM_IN_ADDR) && check->writesRegister(REG_VPM_IN_SETUP) && setup &&
                        periphery::VPRSetup::fromLiteral(setup->getLiteralValue()->unsignedInt()).isDMASetup())
                        return true;
                    return false;
                });
                if(n == nullptr)
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Found VPM address write without VPM DMA setup", it->to_string());
                logging::debug() << "Creating dependency from VPM address write '" << it->to_string()
                                 << "' on VPM DMA setup '" << n->key->to_string() << "'" << logging::endl;
                node.addEdge(n, createSimpleDependency());
                dependsOnSomething = true;
                // also check stride setup
                n = findLastInstruction(it, graph, [](InstructionWalker check) -> bool {
                    auto setup = check->precalculate(1);
                    return check->writesRegister(REG_VPM_OUT_SETUP) && setup &&
                        (periphery::VPWSetup::fromLiteral(setup->getLiteralValue()->unsignedInt()).isStrideSetup() ||
                            periphery::VPRSetup::fromLiteral(setup->getLiteralValue()->unsignedInt()).isStrideSetup());
                });
                if(n != nullptr)
                {
                    logging::debug() << "Creating dependency from VPM address write '" << it->to_string()
                                     << "' on VPM stride setup '" << n->key->to_string() << "'" << logging::endl;
                    node.addEdge(n, createSimpleDependency());
                }
                if(it->writesRegister(REG_VPM_OUT_ADDR))
                {
                    // also depends on any VPM writes in the same mutex-lock
                    n = findLastInstruction(it, graph, [](InstructionWalker check) -> bool {
                        return check->hasValueType(ValueType::REGISTER) && check->writesRegister(REG_VPM_IO);
                    });
                    if(n != nullptr)
                    {
                        logging::debug() << "Creating dependency from VPM address write '" << it->to_string()
                                         << "' on VPM value write '" << n->key->to_string() << "'" << logging::endl;
                        node.addEdge(n, createSimpleDependency());
                    }
                }
            }
            else if(it->writesRegister(REG_MUTEX))
            {
                // TODO mutex release depends on last instruction before using mutex
                auto n = findLastInstruction(it, graph, [](InstructionWalker check) -> bool {
                    auto args = check->getArguments();
                    if(std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool {
                           return arg.hasType(ValueType::REGISTER) && arg.reg.isVertexPipelineMemory();
                       }))
                        return true;
                    if(check->hasValueType(ValueType::REGISTER) && check->getOutput()->reg.isVertexPipelineMemory())
                        return true;
                    // TODO other cases?
                    return check->hasValueType(ValueType::REGISTER) && check->getOutput()->reg == REG_MUTEX;
                });
                if(n == nullptr)
                    throw CompilationError(
                        CompilationStep::OPTIMIZER, "Found mutex release without setting of mutex", it->to_string());
                logging::debug() << "Creating dependency from mutex release '" << it->to_string()
                                 << "' on instruction using mutex '" << n->key->to_string() << "'" << logging::endl;
                node.addEdge(n, createSimpleDependency());
                dependsOnSomething = true;
            }
            else if(it->writesRegister(REG_TMU0_COORD_S_U_X) || it->writesRegister(REG_TMU1_COORD_S_U_X))
            {
                // depends on previous write of same TMU-address
                const Register tmuAddress = it->getOutput()->reg;
                auto n = findLastInstruction(it, graph, [&tmuAddress](InstructionWalker check) -> bool {
                    return check->hasValueType(ValueType::REGISTER) && check->getOutput()->reg == tmuAddress;
                });
                if(n != nullptr)
                {
                    logging::debug() << "Creating dependency from writing TMU address '" << it->to_string()
                                     << "' on previous write of same TMU address '" << n->key->to_string() << "'"
                                     << logging::endl;
                    node.addEdge(n, createSimpleDependency());
                    dependsOnSomething = true;
                }
                // TODO depend on (if exists) writing Y-coordinate (only search up until the previous TMU load)
            }
            else if(it->writesRegister(REG_TMU0_COORD_T_V_Y) || it->writesRegister(REG_TMU1_COORD_T_V_Y))
            {
                // TODO depends on earlier writes of this register, actually more general for almost all registers!
            }
        }
        if(it->signal == SIGNAL_LOAD_TMU0 || it->signal == SIGNAL_LOAD_TMU1)
        {
            // depends on writing of TMUx address
            auto n = findLastInstruction(it, graph, [it](InstructionWalker check) -> bool {
                return (check->writesRegister(REG_TMU0_ADDRESS) && it->signal == SIGNAL_LOAD_TMU0) ||
                    (check->writesRegister(REG_TMU1_ADDRESS) && it->signal == SIGNAL_LOAD_TMU1);
            });
            if(n == nullptr)
                throw CompilationError(
                    CompilationStep::OPTIMIZER, "Found load from TMU without setting of TMU address", it->to_string());
            logging::debug() << "Creating dependency from loading TMU '" << it->to_string()
                             << "' on setting of TMU address '" << n->key->to_string() << "'" << logging::endl;
            node.addEdge(n, createSimpleDependency());
            dependsOnSomething = true;
        }
        if(it->conditional != COND_ALWAYS && it.get<intermediate::Branch>() == nullptr)
        {
            // depends on whatever set the flags
            auto n = findLastInstruction(it, graph, [it](InstructionWalker check) -> bool {
                return check->setFlags == SetFlag::SET_FLAGS && check->conditional == COND_ALWAYS;
            });
            if(n == nullptr)
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Found conditional execution without previous setting of flags", it->to_string());
            logging::debug() << "Creating dependency from conditional execution '" << it->to_string()
                             << "' on setting of flags '" << n->key->to_string() << "'" << logging::endl;
            node.addEdge(n, createFlagsDependency());
            dependsOnSomething = true;
        }
        if(it->setFlags == SetFlag::SET_FLAGS)
        {
            // depends on previous instruction using flags
            auto n = findLastInstruction(
                it, graph, [](InstructionWalker check) -> bool { return check->conditional != COND_ALWAYS; });
            if(n != nullptr)
            {
                logging::debug() << "Creating dependency from setting of flags '" << it->to_string()
                                 << "' on previous conditional execution '" << n->key->to_string() << "'"
                                 << logging::endl;
                node.addEdge(n, createSimpleDependency());
                dependsOnSomething = true;
            }
        }
        // if instruction doesn't depend on anything else (within this basic block), depend on label (excluding NOPs
        // without side-effects)
        //		if(!dependsOnSomething && (!it.has<intermediate::Nop>() || it->hasSideEffects()))
        //			addDependency(node, labelNode, createSimpleDependency(), dependenciesCounter);
        it.nextInBlock();
    }

    logging::debug() << "Created dependency graph with " << graph.getNodes().size() << " entries" << logging::endl;

#ifdef DEBUG_MODE
    if(graph.getNodes().size() > 1)
    {
        const auto nameFunc = [](intermediate::IntermediateInstruction* inst) -> std::string {
            return inst->to_string();
        };
        const auto weakEdgeFunc = [](const Dependency& dep) -> bool { return !dep.fixedLatency; };
        const auto edgeLabelFunc = [](const Dependency& dep) -> std::string { return std::to_string(dep.latency); };
        DebugGraph<intermediate::IntermediateInstruction*, Dependency, Directionality::DIRECTED>::dumpGraph(
            graph, "/tmp/vc4c-dependency-graph.dot", nameFunc, weakEdgeFunc, edgeLabelFunc);
    }
#endif

    return graph;
}
#endif

struct NodeSorter : public std::less<intermediate::IntermediateInstruction*>
{
    static int ratePriority(intermediate::IntermediateInstruction* inst)
    {
        int priority = 0;
        // prioritizing conditional instructions keeps setting flags and their uses together
        if(inst->conditional != COND_ALWAYS)
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
               [](const Value& val) -> bool { return val.hasType(ValueType::LOCAL) && val.local->is<Parameter>(); }))
            priority += 50;

        // writing TMU address gets higher priority, to leave enough space for utilizing the delay to actually read the
        // value
        if(inst->writesRegister(REG_TMU0_ADDRESS) || inst->writesRegister(REG_TMU1_ADDRESS))
            priority += 40;

        // prioritize triggering read of TMU and reading from TMU to be directly scheduled after the delay is up to free
        // queue space
        if(inst->signal.triggersReadOfR4() || inst->readsRegister(REG_TMU_OUT))
            priority += 30;

        // Increasing semaphores gets higher priority, decreasing it lower to reduce stall time
        if(dynamic_cast<const intermediate::SemaphoreAdjustment*>(inst))
        {
            auto semaphore = dynamic_cast<const intermediate::SemaphoreAdjustment*>(inst);
            if(semaphore->increase)
                priority += 20;
            else
                priority -= 20;
        }

        // Triggering of TMU reads gets lower priority to leave enough space between setting address and reading value
        // to utilize delay
        if(inst->signal.triggersReadOfR4())
            priority -= 30;

        // give vector rotations lower priority to make the usage-range of the accumulator used smaller
        if(dynamic_cast<const intermediate::VectorRotation*>(inst))
            priority -= 50;

        // for conditional instructions setting flags themselves not preceding the other instructions depending on same
        // flags
        if(inst->setFlags == SetFlag::SET_FLAGS)
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

    bool operator()(intermediate::IntermediateInstruction* x, intermediate::IntermediateInstruction* y) const
    {
        int prioX = ratePriority(x);
        int prioY = ratePriority(y);
        if(prioX == prioY)
            return x < y;
        return prioX < prioY;
    }
};

// TODO OpenSet leaks all pending instructions if exception is thrown
using OpenSet = OrderedSet<intermediate::IntermediateInstruction*, NodeSorter>;
using DelaysMap = FastMap<const intermediate::IntermediateInstruction*, std::size_t>;

static const int DEFAULT_PRIORITY = 1000;
// needs to be larger then the default priority to disregard in any case
static const int MIN_PRIORITY = 2000;

static int calculateSchedulingPriority(DependencyEdge& dependency, BasicBlock& block)
{
    PROFILE_START(calculateSchedulingPriority);
    int latencyLeft = static_cast<int>(dependency.data.numDelayCycles);
    auto it = block.end().previousInBlock();
    while(!it.isStartOfBlock() && latencyLeft > 0)
    {
        if(it.get() == dependency.getInput().key)
            // we found the dependent instruction
            break;
        it.previousInBlock();
        --latencyLeft;
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
               return arg.hasType(ValueType::LOCAL) && arg.local->getUsers(LocalUse::Type::READER).size() == 1;
           }))
            --latencyLeft;
        if(instr->hasValueType(ValueType::LOCAL) && instr->getOutput()->getSingleWriter() == instr)
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

static int checkDependenciesMet(DependencyNode& entry, BasicBlock& block, OpenSet& openNodes)
{
    if(!entry.hasIncomingDependencies())
        return true;
    int schedulingPriority = 0;
    entry.forAllIncomingEdges([&](DependencyNode& neighbor, DependencyEdge& edge) -> bool {
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

static OpenSet::iterator selectInstruction(OpenSet& openNodes, DependencyGraph& graph, BasicBlock& block,
    const DelaysMap& successiveMandatoryDelays, const DelaysMap& successiveDelays)
{
    // iterate open-set until entry with no more dependencies
    auto it = openNodes.begin();
    std::pair<OpenSet::iterator, int> selected = std::make_pair(openNodes.end(), DEFAULT_PRIORITY);
    auto lastInstruction = block.end().previousInBlock();
    PROFILE_START(SelectInstruction);
    while(it != openNodes.end())
    {
        // select first entry (with highest priority) for which all dependencies are fulfilled (with latency)
        int priority = checkDependenciesMet(graph.assertNode(*it), block, openNodes);
        if(priority < MIN_PRIORITY)
            priority -= successiveMandatoryDelays.at(*it);
        // TODO use preferred delays?
        // TODO remove adding/removing priorities in calculateSchedulingPriority?
        // TODO remove extra cases here?
        if(priority < std::get<1>(selected) ||
            // keep instructions writing the same local together to be combined more easily
            // TODO make better/check result
            (priority == std::get<1>(selected) && lastInstruction->hasValueType(ValueType::LOCAL) &&
                (*it)->writesLocal(lastInstruction->getOutput()->local)) ||
            // keep vector rotations close to their use by devaluing them after all other equal-priority instructions
            (priority == std::get<1>(selected) && lastInstruction.has<intermediate::VectorRotation>() &&
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
        logging::debug() << "Selected '" << (*std::get<0>(selected))->to_string()
                         << "' as next instruction with remaining latency of " << std::get<1>(selected)
                         << logging::endl;
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
static void selectInstructions(DependencyGraph& graph, BasicBlock& block, const DelaysMap& successiveMandatoryDelays,
    const DelaysMap& successiveDelays)
{
    // 1. "empty" basic block without deleting the instructions, skipping the label
    auto it = block.begin().nextInBlock();
    OpenSet openNodes;
    while(!it.isEndOfBlock())
    {
        if(!(it.has<intermediate::Nop>() && !it->hasSideEffects() &&
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
            logging::debug() << "Failed to schedule an instruction, falling back to inserting NOP" << logging::endl;
            block.end().emplace(new intermediate::Nop(intermediate::DelayType::WAIT_REGISTER));
        }
        else
        {
            block.end().emplace(*inst);
            openNodes.erase(inst);
        }
    }
}

bool optimizations::reorderInstructions(const Module& module, Method& kernel, const Configuration& config)
{
    for(BasicBlock& bb : kernel)
    {
        auto dependencies = DependencyGraph::createGraph(bb);
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
