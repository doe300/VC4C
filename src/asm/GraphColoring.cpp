/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "GraphColoring.h"

#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/DebugGraph.h"
#include "RegisterAllocation.h"
#include "log.h"

#include <algorithm>

using namespace vc4c;
using namespace vc4c::qpu_asm;

// TODO rewrite, start with node with the most edges? See http://hjemmesider.diku.dk/~torbenm/Basics/basics_lulu2.pdf
// page 199ff

LocalUsage::LocalUsage(InstructionWalker first, InstructionWalker last) :
    firstOccurrence(first), lastOccurrence(last), possibleFiles(RegisterFile::ANY), blockedFiles(RegisterFile::NONE)
{
    associatedInstructions.insert(first);
    associatedInstructions.insert(last);
}

ColoredNodeBase::ColoredNodeBase(const RegisterFile possibleFiles) :
    initialFile(possibleFiles), possibleFiles(possibleFiles)
{
}

void ColoredNodeBase::blockRegister(RegisterFile file, const std::size_t index)
{
    if(file == RegisterFile::ACCUMULATOR)
        availableAcc.reset(index);
    else if(file == RegisterFile::PHYSICAL_A)
        availableA.reset(index);
    else if(file == RegisterFile::PHYSICAL_B)
        availableB.reset(index);
    if(availableAcc.none())
        possibleFiles = remove_flag(possibleFiles, RegisterFile::ACCUMULATOR);
    if(availableA.none())
        possibleFiles = remove_flag(possibleFiles, RegisterFile::PHYSICAL_A);
    if(availableB.none())
        possibleFiles = remove_flag(possibleFiles, RegisterFile::PHYSICAL_B);
}

bool ColoredNodeBase::hasFreeRegisters(const RegisterFile file) const
{
    if(has_flag(file, RegisterFile::ACCUMULATOR) && availableAcc.any())
        return true;
    if(has_flag(file, RegisterFile::PHYSICAL_A) && availableA.any())
        return true;
    if(has_flag(file, RegisterFile::PHYSICAL_B) && availableB.any())
        return true;
    return false;
}

std::size_t ColoredNodeBase::countFreeRegisters(const RegisterFile file) const
{
    if(has_flag(file, RegisterFile::ACCUMULATOR) && availableAcc.any())
        return availableAcc.count();
    if(has_flag(file, RegisterFile::PHYSICAL_A) && availableA.any())
        return availableA.count();
    if(has_flag(file, RegisterFile::PHYSICAL_B) && availableB.any())
        return availableB.count();
    return 0;
}

void ColoredNodeBase::takeValues(const ColoredNodeBase& other)
{
    this->availableAcc = other.availableAcc;
    this->availableA = other.availableA;
    this->availableB = other.availableB;
    auto* self = reinterpret_cast<ColoredNode*>(this);
    const auto* otherNode = reinterpret_cast<const ColoredNode*>(&other);
    otherNode->forAllEdges([self](const ColoredNode& neighbor, const ColoredEdge& edge) -> bool {
        self->getOrCreateEdge(const_cast<ColoredNode*>(&neighbor), LocalRelation{edge.data});
        return true;
    });
}

Register ColoredNodeBase::getRegisterFixed() const
{
    if(possibleFiles == RegisterFile::NONE)
    {
        if(initialFile != RegisterFile::NONE)
            throw CompilationError(
                CompilationStep::LABEL_REGISTER_MAPPING, "Failed to assign local to valid register", to_string());
        return REG_NOP;
    }
    if(!isFixed(possibleFiles))
        throw CompilationError(
            CompilationStep::LABEL_REGISTER_MAPPING, "Cannot get register of non-fixed node", to_string());
    if(possibleFiles == RegisterFile::PHYSICAL_A)
    {
        if(availableA.count() != 1)
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Cannot get register of node with multiple available registers on file A", to_string());
        for(unsigned char i = 0; i < availableA.size(); ++i)
            if(availableA.test(i))
                return Register{RegisterFile::PHYSICAL_A, i};
    }
    if(possibleFiles == RegisterFile::PHYSICAL_B)
    {
        if(availableB.count() != 1)
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Cannot get register of node with multiple available registers on file B", to_string());
        for(unsigned char i = 0; i < availableB.size(); ++i)
            if(availableB.test(i))
                return Register{RegisterFile::PHYSICAL_B, i};
    }
    if(possibleFiles == RegisterFile::ACCUMULATOR)
    {
        if(availableAcc.count() != 1)
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Cannot get register of node with multiple available registers", to_string());
        for(std::size_t i = 0; i < availableAcc.size(); ++i)
            if(availableAcc.test(i))
                return ACCUMULATORS.at(i);
    }
    throw CompilationError(
        CompilationStep::LABEL_REGISTER_MAPPING, "Unhandled case in fixing node to register", to_string());
}

template <std::size_t size>
static std::size_t fixToRegisterFile(std::bitset<size>& set)
{
    for(std::size_t i = 0; i < set.size(); ++i)
    {
        if(set.test(i))
        {
            set.reset();
            set.set(i);
            return i;
        }
    }
    throw CompilationError(CompilationStep::GENERAL, "No more free registers for previously checked file");
}

std::size_t ColoredNodeBase::fixToRegister()
{
    if(possibleFiles == RegisterFile::NONE)
        return SIZE_MAX;
    // do NOT prefer accumulators here.
    // since if any of the other register-file are possible, the accumulator can be used by other locals, which might
    // require them
    // Also, to make it easier for other locals to find a free register on either file,
    // don't prefer a fixed file, but use the file which has more free registers left
    if(has_flag(possibleFiles, RegisterFile::PHYSICAL_A) &&
        availableA.count() > (has_flag(possibleFiles, RegisterFile::PHYSICAL_B) ? availableB.count() : 0))
    {
        possibleFiles = RegisterFile::PHYSICAL_A;
        return fixToRegisterFile(availableA);
    }
    if(has_flag(possibleFiles, RegisterFile::PHYSICAL_B) && availableB.any())
    {
        possibleFiles = RegisterFile::PHYSICAL_B;
        return fixToRegisterFile(availableB);
    }
    if(has_flag(possibleFiles, RegisterFile::ACCUMULATOR) && availableAcc.any())
    {
        possibleFiles = RegisterFile::ACCUMULATOR;
        return fixToRegisterFile(availableAcc);
    }
    throw CompilationError(
        CompilationStep::LABEL_REGISTER_MAPPING, "Cannot fix local to file with no registers left", to_string());
}

std::string ColoredNodeBase::to_string(bool longDescription) const
{
    const auto* self = reinterpret_cast<const ColoredNode*>(this);
    std::string res = (self->key->name + " init: ")
                          .append(toString(initialFile))
                          .append(", avail: ")
                          .append(toString(possibleFiles))
                          .append(" (")
                          .append(availableAcc.to_string())
                          .append(", ")
                          .append(availableA.to_string())
                          .append(", ")
                          .append(availableB.to_string())
                          .append(")");
    if(longDescription)
    {
        res.append(", neighbors: ");
        self->forAllEdges([&res](const ColoredNode& neighbor, const ColoredEdge&) -> bool {
            res.append(neighbor.key->name).append(", ");
            return true;
        });
    }
    return res;
}

static void fixToRegisterFile(const RegisterFile file, const Local* local, FastMap<const Local*, LocalUsage>& localUses)
{
    localUses.at(local).possibleFiles = intersect_flags(localUses.at(local).possibleFiles, file);
}

static void blockRegisterFile(const RegisterFile file, const Local* local, FastMap<const Local*, LocalUsage>& localUses)
{
    localUses.at(local).possibleFiles = remove_flag(localUses.at(local).possibleFiles, file);
    localUses.at(local).blockedFiles = add_flag(localUses.at(local).blockedFiles, file);
}

static void updateFixedLocals(const intermediate::IntermediateInstruction& instr,
    const Optional<const Local*>& writtenInPreviousInstruction0,
    const Optional<const Local*>& writtenInPreviousInstruction1, const RegisterFile blockedFiles,
    FastMap<const Local*, LocalUsage>& localUses)
{
    const Optional<Value> firstArg = instr.getArgument(0);
    const Optional<Value> secondArg = instr.getArgument(1);
    if(!firstArg)
    {
        return;
    }

    // an input can only be blocked if there is another one
    if(secondArg)
    {
        // only accumulators can be rotated
        if(dynamic_cast<const intermediate::VectorRotation*>(&instr) != nullptr && firstArg->hasType(ValueType::LOCAL))
        {
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be an accumulator, because it
            // is used in a vector-rotation in " << instr.to_string() << logging::endl;
            blockRegisterFile(RegisterFile::PHYSICAL_ANY, firstArg->local, localUses);
        } // the else here skip all the other checks, since they are useless for vector-rotations (already limits to
          // accumulator-only)
        else if(firstArg->hasType(ValueType::LOCAL) && secondArg->hasType(ValueType::SMALL_IMMEDIATE))
        {
            // B is reserved ->other input must be on A or accumulator
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " can't be on register-file B,
            // because of " << instr.to_string() << logging::endl;
            blockRegisterFile(RegisterFile::PHYSICAL_B, firstArg->local, localUses);
        }
        else if(firstArg->hasType(ValueType::SMALL_IMMEDIATE) && secondArg->hasType(ValueType::LOCAL))
        {
            // B is reserved ->other input must be on A or accumulator
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " can't be on register-file B,
            // because of " << instr.to_string() << logging::endl;
            blockRegisterFile(RegisterFile::PHYSICAL_B, secondArg->local, localUses);
        }
        else if(firstArg->hasType(ValueType::LOCAL) && secondArg->hasType(ValueType::REGISTER) &&
            (secondArg->reg.file == RegisterFile::PHYSICAL_A || secondArg->reg.file == RegisterFile::PHYSICAL_B))
        {
            const RegisterFile file = secondArg->reg.file;
            // one of the inputs is fixed to a file, exclude from other
            // logging::debug() << "Local " << firstArg.get().local->to_string() << " can't be on register-file " <<
            // (file == RegisterFile::PHYSICAL_A ? 'A' : 'B') << ", because of " << instr.to_string() << logging::endl;
            blockRegisterFile(file, firstArg->local, localUses);
        }
        else if(firstArg->hasType(ValueType::REGISTER) && secondArg->hasType(ValueType::LOCAL) &&
            (firstArg->reg.file == RegisterFile::PHYSICAL_A || firstArg->reg.file == RegisterFile::PHYSICAL_B))
        {
            const RegisterFile file = firstArg->reg.file;
            // one of the inputs is fixed to a file, exclude from other
            // logging::debug() << "Local " << secondArg.get().local->to_string() << " can't be on register-file " <<
            // (file == RegisterFile::PHYSICAL_A ? 'A' : 'B') << ", because of " << instr.to_string() << logging::endl;
            blockRegisterFile(file, secondArg->local, localUses);
        }
    }

    //"Normally, the Pack and Unpack fields program the A register file pack/unpack blocks.
    // The A-regfile unpack block will convert packed 8 or 16 bit data to 32 bit values ready for use by the ALUs."
    // Broadcom Specification, page 30
    if(instr.hasUnpackMode())
    {
        if(firstArg->hasType(ValueType::LOCAL) && (!secondArg || !secondArg->hasType(ValueType::LOCAL)))
        {
            // there is only one input local, fix to file A
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be on register-file A, because
            // of unpack-mode in " << instr.to_string() << logging::endl;
            blockRegisterFile(remove_flag(RegisterFile::ANY, RegisterFile::PHYSICAL_A), firstArg->local, localUses);
        }
        else if(!firstArg->hasType(ValueType::LOCAL) && secondArg && secondArg->hasType(ValueType::LOCAL))
        {
            // there is only one input local, fix to file A
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " must be on register-file A,
            // because of unpack-mode in " << instr.to_string() << logging::endl;
            blockRegisterFile(remove_flag(RegisterFile::ANY, RegisterFile::PHYSICAL_A), secondArg->local, localUses);
        }
        else if(firstArg->hasType(ValueType::LOCAL) && secondArg && secondArg->hasType(ValueType::LOCAL) &&
            firstArg.value() != secondArg.value())
        {
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Can't unpack two inputs in one instruction", instr.to_string());
        }
    }
    if(instr.hasPackMode() && instr.hasValueType(ValueType::LOCAL))
    {
        //"[...] the a-regfile pack block allows the 32-bit ALU result to be packed back into the a-regfile as 8 or 16
        // bit data."  logging::debug() << "Fixed local " << instr.output.get().local.to_string() << " to register-file
        // A, because of pack-mode in " << instr.to_string() << logging::endl;
        blockRegisterFile(
            remove_flag(RegisterFile::ANY, RegisterFile::PHYSICAL_A), instr.getOutput()->local, localUses);
    }

    if(writtenInPreviousInstruction0)
    {
        // if the first argument was written in the previous instruction, it MUST be on accumulator
        if(firstArg->hasLocal(writtenInPreviousInstruction0.value()))
        {
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, firstArg->local, localUses);
        }
        else if(secondArg && secondArg->hasLocal(writtenInPreviousInstruction0.value()))
        {
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, secondArg->local, localUses);
        }
    }

    if(writtenInPreviousInstruction1)
    {
        // if the first argument was written in the previous instruction, it MUST be on accumulator
        if(firstArg->hasLocal(writtenInPreviousInstruction1.value()))
        {
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, firstArg->local, localUses);
        }
        else if(secondArg && secondArg->hasLocal(writtenInPreviousInstruction1.value()))
        {
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, secondArg->local, localUses);
        }
    }

    // remove all blocked files from all locals
    if(blockedFiles != RegisterFile::NONE)
    {
        if(firstArg && firstArg->hasType(ValueType::LOCAL))
            blockRegisterFile(blockedFiles, firstArg->local, localUses);

        if(secondArg && secondArg->hasType(ValueType::LOCAL))
            blockRegisterFile(blockedFiles, secondArg->local, localUses);
    }
}

static void fixLocals(const InstructionWalker it, FastMap<const Local*, LocalUsage>& localUses,
    Optional<const Local*>& lastWrittenLocal0, Optional<const Local*>& lastWrittenLocal1)
{
    static const Optional<const Local*> EMPTY;

    const intermediate::CombinedOperation* comp = it.get<const intermediate::CombinedOperation>();
    if(comp != nullptr)
    {
        // this duplicate check is required to map the influence combined operations have on each other (blocking each
        // others physical files)
        RegisterFile blockedFiles = RegisterFile::NONE;
        it.forAllInstructions([&blockedFiles](const intermediate::IntermediateInstruction* instr) {
            for(const Value& arg : instr->getArguments())
            {
                if(arg.getLiteralValue())
                    blockedFiles = add_flag(blockedFiles, RegisterFile::PHYSICAL_B);
            }
        });
        if(comp->op1)
        {
            PROFILE_START(updateFixedLocals);
            updateFixedLocals(*comp->op1.get(), lastWrittenLocal0, lastWrittenLocal1, blockedFiles, localUses);
            PROFILE_END(updateFixedLocals);
        }
        if(comp->op2)
        {
            PROFILE_START(updateFixedLocals);
            updateFixedLocals(*comp->op2.get(), lastWrittenLocal0, lastWrittenLocal1, blockedFiles, localUses);
            PROFILE_END(updateFixedLocals);
        }
        // if both instructions for a combined instruction write to the same output, the output MUST be on an
        // accumulator
        if(comp->op1 && comp->op1->hasValueType(ValueType::LOCAL) && comp->op2 &&
            comp->op2->hasValueType(ValueType::LOCAL) && comp->op1->getOutput()->local == comp->op2->getOutput()->local)
        {
            fixToRegisterFile(RegisterFile::ACCUMULATOR, comp->op1->getOutput()->local, localUses);
        }
        // FIXME handling of forcing local to register-file A because of unpack-mode (unpack + combined even possible??)
        lastWrittenLocal0 = comp->op1 && comp->op1->hasValueType(ValueType::LOCAL) ?
            Optional<const Local*>(comp->op1->getOutput()->local) :
            EMPTY;
        lastWrittenLocal1 = comp->op2 && comp->op2->hasValueType(ValueType::LOCAL) ?
            Optional<const Local*>(comp->op2->getOutput()->local) :
            EMPTY;
    }
    else
    {
        PROFILE(updateFixedLocals, *it.get(), lastWrittenLocal0, lastWrittenLocal1, RegisterFile::NONE, localUses);
        lastWrittenLocal0 = it->hasValueType(ValueType::LOCAL) ? Optional<const Local*>(it->getOutput()->local) : EMPTY;
        lastWrittenLocal1 = EMPTY;
    }
}

GraphColoring::GraphColoring(Method& method, InstructionWalker it) :
    method(method), closedSet(), openSet(), interferenceGraph(), localUses()
{
    closedSet.reserve(method.readLocals().size());
    openSet.reserve(method.readLocals().size());
    localUses.reserve(method.readLocals().size());

    Optional<const Local*> lastWrittenLocal0;
    Optional<const Local*> lastWrittenLocal1;
    while(!it.isEndOfMethod())
    {
        if(it.get() != nullptr && !it.has<intermediate::Branch>() && !it.has<intermediate::BranchLabel>() &&
            !it.has<intermediate::MemoryBarrier>())
        {
            // 1) create entry per local
            it->forUsedLocals([this, it](const Local* l, const LocalUse::Type type) -> void {
                if(localUses.find(l) == localUses.end())
                {
                    if(l->type == TYPE_LABEL)
                        throw CompilationError(
                            CompilationStep::LABEL_REGISTER_MAPPING, "Created use for label", it->to_string());
                    localUses.emplace(l, LocalUsage(it, it));
                }
            });
            // 2) update fixed locals
            PROFILE(fixLocals, it, localUses, lastWrittenLocal0, lastWrittenLocal1);
            // 3) update local usage-ranges as well as assign all locals to closed-set or open-set
            it->forUsedLocals([this, it](const Local* l, const LocalUse::Type type) -> void {
                auto& range = localUses.at(l);
                range.associatedInstructions.insert(it);
                range.lastOccurrence = it;
                if(isFixed(range.possibleFiles))
                {
                    // local is fixed to a certain register-file, move to closed set
                    closedSet.insert(l);
                    openSet.erase(l);
                }
                else if(range.lastOccurrence.get() == range.firstOccurrence.get())
                {
                    // first use of local (initialization), add to open-set
                    openSet.insert(l);
                }
            });
        }
        it.nextInMethod();
    }

    // parameters are used from the beginning
    for(const Parameter& arg : method.parameters)
    {
        auto it = localUses.find(&arg);
        if(it != localUses.end())
        {
            it->second.firstOccurrence = method.walkAllInstructions();
            if(!isFixed(it->second.possibleFiles))
                // make sure, parameters are not mapped to accumulators
                it->second.possibleFiles = remove_flag(it->second.possibleFiles, RegisterFile::ACCUMULATOR);
            break;
        }
    }
}

void GraphColoring::createGraph()
{
    interferenceGraph = analysis::InterferenceGraph::createGraph(method);
    // 1. iteration: set files and locals used together and map to start/end of range
    PROFILE_START(createColoredNodes);
    for(const auto& pair : localUses)
    {
        auto& node = graph.getOrCreateNode(pair.first);
        node.possibleFiles = pair.second.possibleFiles;
        node.initialFile = pair.second.possibleFiles;
        if(pair.second.firstOccurrence.get() == pair.second.lastOccurrence.get())
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Local " << pair.first->name << " is never read!" << logging::endl);
            node.possibleFiles = RegisterFile::NONE;
            node.initialFile = RegisterFile::NONE;
            // any local which is never used is added to the colored graph
            // as a node without neighbors (since it has no influence to any neighbors)
            // so we also remove all reference from the closed or open locals
            closedSet.erase(node.key);
            openSet.erase(node.key);
            continue;
        }
        if(pair.first->type == TYPE_LABEL)
        {
            node.possibleFiles = RegisterFile::NONE;
            node.initialFile = RegisterFile::NONE;
            // labels are not mapped to registers
            closedSet.erase(node.key);
            openSet.erase(node.key);
            continue;
        }
        if(node.initialFile == RegisterFile::NONE)
        {
            logging::warn() << "Node " << node.key->name << " has no initially possible registers:" << logging::endl;
            logging::warn() << "\tBlocked files: " << toString(pair.second.blockedFiles) << logging::endl;
            logging::warn() << "\tInstructions: " << logging::endl;
            for(const auto& it : pair.second.associatedInstructions)
                logging::warn() << "\t\t" << it->to_string() << logging::endl;
        }
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Created node: " << node.to_string(false) << logging::endl);
    }
    PROFILE_END(createColoredNodes);

    // TODO if this method works, could here spill all locals with more than XX (64) neighbors!?!
    for(const auto& node : graph.getNodes())
    {
        if(node.second.getEdgesSize() >= 32)
            logging::debug() << "Spill candidate: " << node.first->to_string() << logging::endl;
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 5, "SpillCandidates", node.second.getEdgesSize() >= 32);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 6, "SpillCandidates (uses)",
            (node.second.getEdgesSize() >= 32) * node.first->getUsers().size());
    }
    // 2. iteration: associate locals used together
    PROFILE_START(InterferenceToColoredGraph);
    for(const auto& interferenceNode : interferenceGraph->getNodes())
    {
        ColoredNode& node = graph.assertNode(interferenceNode.first);
        interferenceNode.second.forAllEdges(
            [&](const analysis::InterferenceNode& neighbor, const analysis::Interference& edge) -> bool {
                node.getOrCreateEdge(&graph.assertNode(neighbor.key)).data =
                    edge.data == analysis::InterferenceType::USED_TOGETHER ? LocalRelation::USED_TOGETHER :
                                                                             LocalRelation::USED_SIMULTANEOUSLY;
                return true;
            });
    }
    PROFILE_END(InterferenceToColoredGraph);

    logging::debug() << "Colored graph with " << graph.getNodes().size() << " nodes created!" << logging::endl;
#ifdef DEBUG_MODE
    DebugGraph<const Local*, LocalRelation, ColoredEdge::Directed> debugGraph("/tmp/vc4c-register-graph.dot");
    const std::function<std::string(const Local* const&)> nameFunc = [](const Local* const& l) -> std::string {
        return l->name;
    };
    const std::function<bool(const LocalRelation&)> weakEdgeFunc = [](const LocalRelation& r) -> bool {
        return r != LocalRelation::USED_TOGETHER;
    };
    for(const auto& node : graph.getNodes())
    {
        debugGraph.addNodeWithNeighbors<ColoredNode>(node.second, nameFunc, weakEdgeFunc);
    }
#endif
}

static void processClosedSet(ColoredGraph& graph, FastSet<const Local*>& closedSet, FastSet<const Local*>& openSet,
    FastSet<const Local*>& errorSet)
{
    PROFILE_START(processClosedSet);
    while(!closedSet.empty())
    {
        // for every entry in closed-set, remove fixed register from all used-together neighbors
        // and decrement register-file for all other neighbors
        auto node = graph.findNode(*closedSet.begin());
        if(node == nullptr)
        {
            throw CompilationError(
                CompilationStep::LABEL_REGISTER_MAPPING, "Error getting local from graph", (*closedSet.begin())->name);
        }
        if(node->possibleFiles == RegisterFile::NONE)
        {
            if(node->initialFile != RegisterFile::NONE)
                // error
                errorSet.insert(node->key);
            // otherwise, this is on purpose, so this node is finished being processed
            else // actually, this case should never occur, since locals mapped to nop-register should never be in the
                 // closed-set
                throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                    "Unhandled case, unused local in closed-set", node->key->name);
        }
        else
        {
            const std::size_t fixedRegister = node->fixToRegister();
            node->forAllEdges([&](ColoredNode& neighbor, ColoredEdge& edge) -> bool {
                if(edge.data == LocalRelation::USED_TOGETHER &&
                    (node->possibleFiles == RegisterFile::PHYSICAL_A ||
                        node->possibleFiles == RegisterFile::PHYSICAL_B))
                {
                    neighbor.possibleFiles = remove_flag(neighbor.possibleFiles, node->possibleFiles);
                }
                else
                    neighbor.blockRegister(node->possibleFiles, fixedRegister);
                auto it = openSet.find(neighbor.key);
                if(isFixed(neighbor.possibleFiles) && it != openSet.end())
                {
                    openSet.erase(it);
                    closedSet.insert(neighbor.key);
                }

                return true;
            });
        }
        closedSet.erase(node->key);
    }
    PROFILE_END(processClosedSet);
}

bool GraphColoring::colorGraph()
{
    if(!graph.getNodes().empty())
    {
        PROFILE(resetGraph);
    }
    PROFILE(createGraph);

    // process all nodes fixed initially to a register-file
    processClosedSet(graph, closedSet, openSet, errorSet);

    while(!openSet.empty())
    {
        // for every node in the open-set, assign to accumulator if possible, assign to the first available
        // register-file otherwise  and update all neighbors
        auto node = graph.findNode(*openSet.begin());
        if(node == nullptr)
        {
            throw CompilationError(
                CompilationStep::LABEL_REGISTER_MAPPING, "Error getting local from graph", (*openSet.begin())->name);
        }
        RegisterFile currentFile = RegisterFile::NONE;
        if(has_flag(node->possibleFiles, RegisterFile::ACCUMULATOR))
            currentFile = RegisterFile::ACCUMULATOR;
        else if(has_flag(node->possibleFiles, RegisterFile::PHYSICAL_A))
            currentFile = RegisterFile::PHYSICAL_A;
        else if(has_flag(node->possibleFiles, RegisterFile::PHYSICAL_B))
            currentFile = RegisterFile::PHYSICAL_B;
        else
        {
            errorSet.insert(node->key);
            openSet.erase(node->key);
            continue;
        }
        node->possibleFiles = currentFile;
        closedSet.insert(node->key);
        openSet.erase(node->key);
        processClosedSet(graph, closedSet, openSet, errorSet);
    }

    return errorSet.empty();
}

static RegisterFile getBlockedInputs(
    const InstructionWalker it, const ColoredGraph& graph, const Local* toSkip = nullptr)
{
    RegisterFile blockedFiles = RegisterFile::NONE;
    it.forAllInstructions([&graph, toSkip, &blockedFiles](const intermediate::IntermediateInstruction* instr) {
        for(const auto& arg : instr->getArguments())
        {
            if(toSkip != nullptr && arg.hasLocal(toSkip))
                continue;
            if(arg.hasType(ValueType::LOCAL) && isFixed(graph.assertNode(arg.local).possibleFiles) &&
                graph.assertNode(arg.local).possibleFiles != RegisterFile::ACCUMULATOR)
                blockedFiles = add_flag(blockedFiles, graph.assertNode(arg.local).possibleFiles);
            else if(arg.getLiteralValue())
                blockedFiles = add_flag(blockedFiles, RegisterFile::PHYSICAL_B);
            else if(arg.hasType(ValueType::REGISTER))
                blockedFiles = add_flag(blockedFiles, arg.reg.file);
        }
    });
    return blockedFiles;
}

static LocalUse checkUser(const OrderedMap<const LocalUser*, LocalUse>& users, const InstructionWalker it)
{
    LocalUse use;
    it.forAllInstructions([&users, &use](const intermediate::IntermediateInstruction* instr) {
        auto it = users.find(instr);
        if(it != users.end())
        {
            use.numReads += it->second.numReads;
            use.numWrites += it->second.numWrites;
        }
    });
    return use;
}

static LocalUse assertUser(const OrderedMap<const LocalUser*, LocalUse>& users, const InstructionWalker it)
{
    auto use = checkUser(users, it);
    if(!use.readsLocal() && !use.writesLocal())
    {
        throw CompilationError(
            CompilationStep::LABEL_REGISTER_MAPPING, "User is not listed in the list of users", it->to_string());
    }
    return use;
}

static bool reassignNodeToRegister(ColoredGraph& graph, ColoredNode& node)
{
    node.forAllEdges([&](ColoredNode& neighbor, ColoredEdge& edge) -> bool {
        if(edge.data == LocalRelation::USED_TOGETHER &&
            (neighbor.possibleFiles == RegisterFile::PHYSICAL_A || neighbor.possibleFiles == RegisterFile::PHYSICAL_B))
        {
            node.possibleFiles = remove_flag(node.possibleFiles, neighbor.possibleFiles);
        }
        else if(isFixed(neighbor.possibleFiles) &&
            neighbor.hasFreeRegisters(neighbor.possibleFiles)) // if the neighbor is a temporary introduced by this
                                                               // fix, it may not yet be fixed to a register-file
            node.blockRegister(neighbor.possibleFiles, neighbor.fixToRegister());

        return true;
    });

    bool fixed =
        isFixed(node.possibleFiles) && node.hasFreeRegisters(node.possibleFiles) && node.fixToRegister() != SIZE_MAX;
    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 40, "reassignNodeToRegister", fixed);
    return fixed;
}

static bool moveLocalToRegisterFile(Method& method, ColoredGraph& graph, ColoredNode& node,
    FastMap<const Local*, LocalUsage>& localUses, LocalUsage& localUse, const RegisterFile file)
{
    bool needNextRound = false;
    const auto& users = node.key->getUsers();

    // this might strain the accumulators, which can be fixed by the next iteration in CASE 1)

    // we need to copy the associated instructions, since we modify the collection
    const FastSet<InstructionWalker> copy(localUse.associatedInstructions);
    for(InstructionWalker it : copy)
    {
        // 1) check if instruction reads this local
        if(!assertUser(users, it).readsLocal())
        {
            continue;
        }
        // 2) check if the instructions blocks all available files
        const RegisterFile blockedInputs = getBlockedInputs(it, graph, node.key);
        if(file == RegisterFile::PHYSICAL_A && !has_flag(blockedInputs, RegisterFile::PHYSICAL_A))
        {
            // this instruction does not block A as input, which could be used by the local -> skip
            continue;
        }
        if(file == RegisterFile::PHYSICAL_B && !has_flag(blockedInputs, RegisterFile::PHYSICAL_B))
        {
            // this instruction does not block B as input, which could be used by the local -> skip
            continue;
        }
        // 3) insert move to temporary and use temporary as input to instruction
        const Value tmp = method.addNewLocal(node.key->type, "%register_fix");
        logging::debug() << "Fixing register-conflict by using temporary as input for: " << it->to_string()
                         << logging::endl;
        it.emplace(new intermediate::MoveOperation(tmp, node.key->createReference()));
        auto tmpUse = localUses.emplace(tmp.local, LocalUsage(it, it)).first->second;
        it.nextInBlock();
        it->replaceLocal(node.key, tmp.local, LocalUse::Type::READER);
        // 4) add temporary to graph (and local usage) with same blocked registers as local, but accumulator as file
        // (since it is read in the next instruction)
        tmpUse.possibleFiles = RegisterFile::ACCUMULATOR;
        // XXX or RegisterFile::NONE, since we do not know, if the block come from literals/fixed registers ??
        tmpUse.blockedFiles = blockedInputs;
        tmpUse.lastOccurrence = it;
        tmpUse.associatedInstructions.insert(it);
        if(localUse.lastOccurrence.get() == it.get())
            // if this was the last use of the local, move the pointer to the copying into the temporary
            localUse.lastOccurrence = tmpUse.firstOccurrence;
        if(localUse.firstOccurrence.get() == it.get())
            localUse.firstOccurrence = tmpUse.firstOccurrence;
        localUse.associatedInstructions.erase(it);
        localUse.associatedInstructions.insert(tmpUse.firstOccurrence);
        // TODO or always force a re-creation of the graph ?? Could remove all setting/updating of graph-nodes
        auto& tmpNode = graph.getOrCreateNode(tmp.local, ColoredNode(graph, tmp.local, RegisterFile::ACCUMULATOR));
        // XXX setting the neighbors of the temporary to the neighbors of the local actually is far too broad, but we
        // cannot determine the actual neighbors
        tmpNode.takeValues(node);
        // TODO need to update the local used in the current instruction as input with the new temporary
        node.forAllEdges([&](ColoredNode& neighbor, ColoredEdge&) -> bool {
            neighbor.getOrCreateEdge(&tmpNode, LocalRelation::USED_SIMULTANEOUSLY);
            return true;
        });
        if(!reassignNodeToRegister(graph, graph.assertNode(tmp.local)))
            needNextRound = true;
    }
    // 5) update available files of local
    localUse.blockedFiles = remove_flag(localUse.blockedFiles, file);
    localUse.possibleFiles = remove_flag(RegisterFile::ANY, localUse.blockedFiles);
    node.possibleFiles = add_flag(node.possibleFiles, intersect_flags(node.initialFile, RegisterFile::PHYSICAL_ANY));
    return !needNextRound && reassignNodeToRegister(graph, node);
}

static bool blocksLocal(const ColoredNode* neighbor, LocalRelation relation)
{
    return relation == LocalRelation::USED_TOGETHER && isFixed(neighbor->possibleFiles) &&
        !has_flag(neighbor->possibleFiles, RegisterFile::ACCUMULATOR);
}

static bool fixSingleError(Method& method, ColoredGraph& graph, ColoredNode& node,
    FastMap<const Local*, LocalUsage>& localUses, LocalUsage& localUse)
{
    /*
     * The following cases can occur:
     * 1) local is fixed to accumulator, but there is no more free accumulator
     *  -> can be fixed by inserting NOP between writing and reading of local to allow it to be moved to the physical
     * files. This won't even change the graph, since the local associations don't change NOTES:
     *   - this is not correct if the local is used as vector rotation input
     *   - need to make sure, none of the register files are blocked by other locals, this local is used together (as
     * inputs) or literal values/fixed registers
     *
     * 2) local could be on one of the physical files (initial files as well as free registers), but the file is blocked
     * by another local used together with the erroneous one
     *  -> can be fixed by copying the local before every conflicting use and using the copy (on accumulator) as input
     * to the conflicting instruction NOTES:
     *   - this only works if the local is never unpacked as argument, which is not valid for accumulator inputs
     *
     *  -> another way to fix this would be to create a copy and move it to the other physical file
     *  NOTES:
     *   - this will only work, if the other file is not blocked (either completely by other input locals or all
     * registers being already in use). Otherwise, we would need two copies, one per file, which are used in
     * instructions, the other file is blocked
     *   - need to make sure, all copies are written, when the main local is written!
     *
     * 3) local could be on one of the physical files (possible files), but there are no more free registers on that
     * file (and of course the accumulators)
     *  -> could be fixed by copying the local before any use to a temporary (which will land on accumulators), so it
     * can be assigned to the other physical file NOTES:
     *   - this will only work, if there are free registers on the other file (if not, the only way out would be
     * spilling!)
     *   - need to make sure, the uses of the local do not block both register files.
     *     Otherwise, one copy per file would need to be created (and we would need to hope, the next iteration can
     * assign both)
     *   - need to make sure, copy is written to when the main local is written!
     */

    // TODO if we only use fixes which do not change the graph (don't insert new locals)
    // we could skip re-creating the whole graph and simply try to assign a register to the node
    // by re-checking which registers are not used by any neighbor

    const auto& users = node.key->getUsers();

    // CASE 1)
    if(node.initialFile == RegisterFile::ACCUMULATOR && !node.hasFreeRegisters(RegisterFile::ACCUMULATOR))
    {
        // fix read-after-writes, so local can be on non-accumulator:
        logging::debug() << "Fixing register error case 1 for: " << node.key->to_string() << logging::endl;
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 10, "Register error case 1", 1);

        // the register-files which can be used after the fix by this local
        RegisterFile freeFiles = remove_flag(RegisterFile::ANY, localUses.at(node.key).blockedFiles);

        for(InstructionWalker it : localUse.associatedInstructions)
        {
            // 1) check if usage is a write
            if(assertUser(users, it).writesLocal())
            {
                // 2) check if next instruction reads this local
                it.nextInMethod();
                bool localRead = checkUser(users, it).readsLocal();
                if(localRead && it.has<intermediate::VectorRotation>())
                {
                    // TODO for locals used in vector rotations, this fix is wrong
                    throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                        "Fixing errors in locals used as input to vector rotations is not implemented",
                        it->to_string());
                }
                // 3) if so, insert nop
                if(localRead)
                {
                    logging::debug() << "Fixing register-conflict by inserting NOP before: " << it->to_string()
                                     << logging::endl;
                    it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_REGISTER));
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 11, "NOP insertions", 1);
                }
            }
            else if(assertUser(users, it).readsLocal())
            {
                freeFiles = remove_flag(freeFiles, getBlockedInputs(it, graph, node.key));
            }
        }

        // now the register can be on the physical files
        // if free-files in NONE, maybe the next iteration can assign a valid register
        node.possibleFiles = freeFiles;
        node.initialFile = remove_flag(RegisterFile::ANY, localUses.at(node.key).blockedFiles);
        localUse.possibleFiles = remove_flag(RegisterFile::ANY, localUses.at(node.key).blockedFiles);
    }
    // CASE 2)
    else if((has_flag(node.initialFile, RegisterFile::PHYSICAL_A) && node.hasFreeRegisters(RegisterFile::PHYSICAL_A)) ||
        (has_flag(node.initialFile, RegisterFile::PHYSICAL_B) && node.hasFreeRegisters(RegisterFile::PHYSICAL_B)))
    {
        // physical file A(B) was available at the start, and there are still free registers on it, but none could be
        // assigned
        //-> both register-files are blocked by other locals this local is used together with
        //-> insert a new temporary to be used instead of this local as parameter for all instructions,
        // this local is used together with another local fixed to a physical file
        //-> or, if blocking local is in other combined instruction, split up instructions
        logging::debug() << "Fixing register error case 2 for: " << node.key->to_string() << logging::endl;
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 20, "Register error case 2", 1);

        bool fileACouldBeUsed =
            has_flag(node.initialFile, RegisterFile::PHYSICAL_A) && node.hasFreeRegisters(RegisterFile::PHYSICAL_A);
        bool fileBCouldBeUsed =
            has_flag(node.initialFile, RegisterFile::PHYSICAL_B) && node.hasFreeRegisters(RegisterFile::PHYSICAL_B);

        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 21, "A blocked", !fileACouldBeUsed);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 22, "B blocked", !fileBCouldBeUsed);

        if(fileACouldBeUsed && fileBCouldBeUsed)
        {
            // if both files could be used, move to file with more available registers
            if(node.countFreeRegisters(RegisterFile::PHYSICAL_A) > node.countFreeRegisters(RegisterFile::PHYSICAL_B))
                fileBCouldBeUsed = false;
            else
                fileACouldBeUsed = false;
            // TODO alternatively, we could move to the file, where we would need less copies
        }

        // TODO need to update blocked files for split combinations

        logging::debug() << "Trying to fix local to register-file "
                         << toString(add_flag(fileACouldBeUsed ? RegisterFile::PHYSICAL_A : RegisterFile::NONE,
                                fileBCouldBeUsed ? RegisterFile::PHYSICAL_B : RegisterFile::NONE))
                         << logging::endl;

        if(!has_flag(localUses.at(node.key).blockedFiles, RegisterFile::ACCUMULATOR))
        {
            // the "easier" solution is to copy the local into an accumulator before each use, where it conflicts with
            // other inputs
            return moveLocalToRegisterFile(method, graph, node, localUses, localUse,
                fileACouldBeUsed ? RegisterFile::PHYSICAL_A : RegisterFile::PHYSICAL_B);
        }
        else
        {
            // if only one file is blocked, create a copy, write it (after the local is written to) and use it as inputs
            // for all uses of the original local  if both files are blocked, create two copies, copy the local into
            // them and use them where their respective file is not blocked

            // TODO need to make sure, temporary are written when original is written

            // TODO on updating the locals, copy the neighbors to the temporary
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "This version of conflict is not yet implemented", node.to_string());
        }
    }
    // CASE 3)
    else if((!has_flag(node.initialFile, RegisterFile::PHYSICAL_A) ||
                !node.hasFreeRegisters(RegisterFile::PHYSICAL_A)) ||
        (!has_flag(node.initialFile, RegisterFile::PHYSICAL_B) || !node.hasFreeRegisters(RegisterFile::PHYSICAL_B)))
    {
        // for any of the possible files, there are no more free registers to assign
        // so we need to copy the local to a temporary before every use, so it can be mapped to the other file
        logging::debug() << "Fixing register error case 3 for: " << node.key->to_string() << logging::endl;
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 30, "Register error case 3", 1);

        bool moveToFileA = node.hasFreeRegisters(RegisterFile::PHYSICAL_A);
        bool moveToFileB = node.hasFreeRegisters(RegisterFile::PHYSICAL_B);

        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 31, "move to A", moveToFileA);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 32, "move to B", moveToFileB);

        if(moveToFileA && moveToFileB)
        {
            // this could only happen, if neither physical file was in the initial files, in which case CASE 1) should
            // have been entered
            throw CompilationError(
                CompilationStep::LABEL_REGISTER_MAPPING, "Invalid case of free blocked registers", node.to_string());
        }
        else if(!moveToFileA && !moveToFileB)
        {
            // there are no more free register AT ALL
            // since we do not spill, we can only about
            logging::error() << "Local " << node.key->to_string() << " cannot be assigned to ANY register, aborting!"
                             << logging::endl;
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING, "Failed to assign local to ANY register",
                node.key->to_string());
        }

        logging::debug() << "Trying to fix local to register-file "
                         << toString(add_flag(moveToFileA ? RegisterFile::PHYSICAL_A : RegisterFile::NONE,
                                moveToFileB ? RegisterFile::PHYSICAL_B : RegisterFile::NONE))
                         << logging::endl;

        if(has_flag(localUses.at(node.key).blockedFiles, RegisterFile::ACCUMULATOR))
        {
            // some of the instruction this local is used as input, do not accept accumulators (e.g. unpack)
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Cannot fix register-conflict with local used as packed input", node.key->to_string());
        }

        return moveLocalToRegisterFile(method, graph, node, localUses, localUse,
            moveToFileA ? RegisterFile::PHYSICAL_A : RegisterFile::PHYSICAL_B);
    }
    else
        throw CompilationError(
            CompilationStep::LABEL_REGISTER_MAPPING, "Unhandled conflict in register-mapping node", node.to_string());

    return reassignNodeToRegister(graph, node);
}

bool GraphColoring::fixErrors()
{
    PROFILE_START(fixRegisterErrors);
    for(const auto& node : graph.getNodes())
    {
        logging::debug() << node.second.to_string() << logging::endl;
    }

    bool allFixed = true;
    for(const Local* local : errorSet)
    {
        ColoredNode& node = graph.assertNode(local);
        logging::debug() << "Error in register-allocation for node: " << node.to_string() << logging::endl;
        auto& s = logging::debug() << "Local is blocked by: ";
        node.forAllEdges([&](const ColoredNode& neighbor, const ColoredEdge& edge) -> bool {
            if(blocksLocal(&neighbor, edge.data))
                s << neighbor.to_string() << ", ";
            return true;
        });
        s << logging::endl;
        if(!fixSingleError(method, graph, node, localUses, localUses.at(local)))
            allFixed = false;
    }
    PROFILE_END(fixRegisterErrors);
    return allFixed;
}

FastMap<const Local*, Register> GraphColoring::toRegisterMap() const
{
    if(!errorSet.empty())
    {
        for(const Local* loc : errorSet)
            logging::error() << "Error assigning local to register: " << loc->name << logging::endl;
        throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING, "There are erroneous register-associations!");
    }

    UnorderedMap<const Local*, Register> result;

    for(const auto& pair : graph.getNodes())
    {
        result.emplace(pair.first, pair.second.getRegisterFixed());
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Assigned local " << pair.first->name << " to register "
                << result.at(pair.first).to_string(true, false) << logging::endl);
    }

    return result;
}

void GraphColoring::resetGraph()
{
    // reset the graph and the closed- and open sets
    openSet.clear();
    closedSet.clear();
    errorSet.clear();
    graph.clear();
    for(const auto& pair : localUses)
    {
        if(isFixed(pair.second.possibleFiles))
        {
            // local is fixed to a certain register-file, move to closed set
            closedSet.insert(pair.first);
        }
        else if(pair.second.possibleFiles != RegisterFile::NONE)
        {
            // first use of local (initialization), add to open-set
            openSet.insert(pair.first);
        }
    }
}
