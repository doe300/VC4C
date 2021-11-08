/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "GraphColoring.h"

#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/DebugGraph.h"
#include "../analysis/LivenessAnalysis.h"
#include "../intermediate/IntermediateInstruction.h"
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
        for(std::size_t i = 0; i < availableA.size(); ++i)
            if(availableA.test(i))
                return Register{RegisterFile::PHYSICAL_A, static_cast<uint8_t>(i)};
    }
    if(possibleFiles == RegisterFile::PHYSICAL_B)
    {
        if(availableB.count() != 1)
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Cannot get register of node with multiple available registers on file B", to_string());
        for(std::size_t i = 0; i < availableB.size(); ++i)
            if(availableB.test(i))
                return Register{RegisterFile::PHYSICAL_B, static_cast<uint8_t>(i)};
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
static NODISCARD std::size_t fixToRegisterFile(std::bitset<size>& set)
{
    if(size == 6 /* accumulators */ && set.test(5))
    {
        // if possible, prefer the use of r5, to keep the other registers free for locals which cannot use r5
        set.reset();
        set.set(5);
        return 5;
    }
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

LCOV_EXCL_START
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
LCOV_EXCL_STOP

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
    const Local* writtenInPreviousInstruction0, const Local* writtenInPreviousInstruction1,
    const RegisterFile blockedFiles, FastMap<const Local*, LocalUsage>& localUses)
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
        auto rot = instr.getVectorRotation();
        if(rot && firstArg->checkLocal())
        {
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be an accumulator, because it
            // is used in a vector-rotation in " << instr.to_string() << logging::endl;
            auto toBlock = RegisterFile::PHYSICAL_B;
            if(!rot->isFullRotationAllowed())
                // per-quad vector rotation MUST NOT take accumulator as input!
                toBlock = add_flag(toBlock, RegisterFile::ACCUMULATOR);
            else
                localUses.at(firstArg->local()).isFullVectorRotated = true;
            if(!rot->isPerQuadRotationAllowed())
                // only accumulators can be rotated across all vector elements
                toBlock = add_flag(toBlock, RegisterFile::PHYSICAL_A);
            blockRegisterFile(toBlock, firstArg->local(), localUses);
        } // the else here skip all the other checks, since they are useless for vector-rotations (already limits to
          // accumulator-only)
        else if(firstArg->checkLocal() && secondArg->checkImmediate())
        {
            // B is reserved ->other input must be on A or accumulator
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " can't be on register-file B,
            // because of " << instr.to_string() << logging::endl;
            blockRegisterFile(RegisterFile::PHYSICAL_B, firstArg->local(), localUses);
        }
        else if(firstArg->checkImmediate() && secondArg->checkLocal())
        {
            // B is reserved ->other input must be on A or accumulator
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " can't be on register-file B,
            // because of " << instr.to_string() << logging::endl;
            blockRegisterFile(RegisterFile::PHYSICAL_B, secondArg->local(), localUses);
        }
        else if(firstArg->checkLocal() && secondArg->checkRegister() &&
            (secondArg->reg().file == RegisterFile::PHYSICAL_A || secondArg->reg().file == RegisterFile::PHYSICAL_B))
        {
            const RegisterFile file = secondArg->reg().file;
            // one of the inputs is fixed to a file, exclude from other
            // logging::debug() << "Local " << firstArg.get().local->to_string() << " can't be on register-file " <<
            // (file == RegisterFile::PHYSICAL_A ? 'A' : 'B') << ", because of " << instr.to_string() << logging::endl;
            blockRegisterFile(file, firstArg->local(), localUses);
        }
        else if(firstArg->checkRegister() && secondArg->checkLocal() &&
            (firstArg->reg().file == RegisterFile::PHYSICAL_A || firstArg->reg().file == RegisterFile::PHYSICAL_B))
        {
            const RegisterFile file = firstArg->reg().file;
            // one of the inputs is fixed to a file, exclude from other
            // logging::debug() << "Local " << secondArg.get().local->to_string() << " can't be on register-file " <<
            // (file == RegisterFile::PHYSICAL_A ? 'A' : 'B') << ", because of " << instr.to_string() << logging::endl;
            blockRegisterFile(file, secondArg->local(), localUses);
        }
    }

    //"Normally, the Pack and Unpack fields program the A register file pack/unpack blocks.
    // The A-regfile unpack block will convert packed 8 or 16 bit data to 32 bit values ready for use by the ALUs."
    // Broadcom Specification, page 30
    if(instr.hasUnpackMode())
    {
        if(firstArg->checkLocal() && (!secondArg || !secondArg->checkLocal() || secondArg.value() == firstArg))
        {
            // there is only one input local, fix to file A
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be on register-file A, because
            // of unpack-mode in " << instr.to_string() << logging::endl;
            blockRegisterFile(remove_flag(RegisterFile::ANY, RegisterFile::PHYSICAL_A), firstArg->local(), localUses);
        }
        else if(!firstArg->checkLocal() && secondArg && secondArg->checkLocal())
        {
            // there is only one input local, fix to file A
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " must be on register-file A,
            // because of unpack-mode in " << instr.to_string() << logging::endl;
            blockRegisterFile(remove_flag(RegisterFile::ANY, RegisterFile::PHYSICAL_A), secondArg->local(), localUses);
        }
        else if(firstArg->checkLocal() && secondArg && secondArg->checkLocal() && firstArg.value() != secondArg.value())
        {
            throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                "Can't unpack two inputs in one instruction", instr.to_string());
        }
    }
    if(instr.hasPackMode() && instr.checkOutputLocal())
    {
        //"[...] the a-regfile pack block allows the 32-bit ALU result to be packed back into the a-regfile as 8 or 16
        // bit data."  logging::debug() << "Fixed local " << instr.output.get().local.to_string() << " to register-file
        // A, because of pack-mode in " << instr.to_string() << logging::endl;
        blockRegisterFile(
            remove_flag(RegisterFile::ANY, RegisterFile::PHYSICAL_A), instr.getOutput()->local(), localUses);
    }

    if(writtenInPreviousInstruction0)
    {
        // if the first argument was written in the previous instruction, it MUST be on accumulator
        if(firstArg->hasLocal(writtenInPreviousInstruction0))
        {
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, firstArg->local(), localUses);
        }
        else if(secondArg && secondArg->hasLocal(writtenInPreviousInstruction0))
        {
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, secondArg->local(), localUses);
        }
    }

    if(writtenInPreviousInstruction1)
    {
        // if the first argument was written in the previous instruction, it MUST be on accumulator
        if(firstArg->hasLocal(writtenInPreviousInstruction1))
        {
            // logging::debug() << "Local " << firstArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, firstArg->local(), localUses);
        }
        else if(secondArg && secondArg->hasLocal(writtenInPreviousInstruction1))
        {
            // logging::debug() << "Local " << secondArg.get().local.to_string() << " must be an accumulator, because it
            // is written in the previous instruction before " << instr.to_string() << logging::endl;
            fixToRegisterFile(RegisterFile::ACCUMULATOR, secondArg->local(), localUses);
        }
    }

    if(dynamic_cast<const intermediate::Branch*>(&instr) && firstArg->checkLocal() && firstArg->type != TYPE_LABEL)
        // code addresses used in branches can only be read from physical register-file A
        fixToRegisterFile(RegisterFile::PHYSICAL_A, firstArg->local(), localUses);

    // remove all blocked files from all locals
    if(blockedFiles != RegisterFile::NONE)
    {
        if(auto loc = (firstArg & &Value::checkLocal))
            blockRegisterFile(blockedFiles, loc, localUses);

        if(auto loc = (secondArg & &Value::checkLocal))
            blockRegisterFile(blockedFiles, loc, localUses);
    }
}

static void fixLocals(const InstructionWalker it, FastMap<const Local*, LocalUsage>& localUses,
    const Local*& lastWrittenLocal0, const Local*& lastWrittenLocal1)
{
    if(auto comp = it.get<const intermediate::CombinedOperation>())
    {
        // this duplicate check is required to map the influence combined operations have on each other (blocking each
        // others physical files)
        RegisterFile blockedFiles = RegisterFile::NONE;
        it.forAllInstructions([&blockedFiles](const intermediate::IntermediateInstruction& instr) {
            for(const Value& arg : instr.getArguments())
            {
                if(arg.getLiteralValue())
                    blockedFiles = add_flag(blockedFiles, RegisterFile::PHYSICAL_B);
            }
        });
        if(auto op1 = comp->getFirstOp())
        {
            PROFILE_SCOPE(updateFixedLocals);
            updateFixedLocals(*op1, lastWrittenLocal0, lastWrittenLocal1, blockedFiles, localUses);
        }
        if(auto op2 = comp->getSecondOp())
        {
            PROFILE_SCOPE(updateFixedLocals);
            updateFixedLocals(*op2, lastWrittenLocal0, lastWrittenLocal1, blockedFiles, localUses);
        }
        // if both instructions for a combined instruction write to the same output, the output MUST be on an
        // accumulator
        if(comp->getFirstOp() && comp->getFirstOp()->checkOutputLocal() && comp->getSecondOp() &&
            comp->getSecondOp()->checkOutputLocal() &&
            comp->getFirstOp()->getOutput()->local() == comp->getSecondOp()->getOutput()->local())
        {
            fixToRegisterFile(RegisterFile::ACCUMULATOR, comp->getFirstOp()->getOutput()->local(), localUses);
        }
        // FIXME handling of forcing local to register-file A because of unpack-mode (unpack + combined even possible??)
        lastWrittenLocal0 = comp->getFirstOp() && comp->getFirstOp()->checkOutputLocal() ?
            comp->getFirstOp()->getOutput()->local() :
            nullptr;
        lastWrittenLocal1 = comp->getSecondOp() && comp->getSecondOp()->checkOutputLocal() ?
            comp->getSecondOp()->getOutput()->local() :
            nullptr;
    }
    else
    {
        PROFILE(updateFixedLocals, *it.get(), lastWrittenLocal0, lastWrittenLocal1, RegisterFile::NONE, localUses);
        lastWrittenLocal0 = it->checkOutputLocal() ? it->getOutput()->local() : nullptr;
        lastWrittenLocal1 = nullptr;
    }
}

GraphColoring::GraphColoring(Method& method, InstructionWalker it) :
    method(method), closedSet(), openSet(), livenessAnalysis(true), localUses()
{
    closedSet.reserve(method.getNumLocals());
    openSet.reserve(method.getNumLocals());
    localUses.reserve(method.getNumLocals());

    const Local* lastWrittenLocal0 = nullptr;
    const Local* lastWrittenLocal1 = nullptr;
    while(!it.isEndOfMethod())
    {
        if(it.has() && !it.get<intermediate::BranchLabel>() && !it.get<intermediate::MemoryBarrier>() &&
            it->mapsToASMInstruction())
        {
            // 1) create entry per local
            it->forUsedLocals([this, it](const Local* l, const LocalUse::Type type,
                                  const intermediate::IntermediateInstruction& inst) -> void {
                if(l->type != TYPE_LABEL && localUses.find(l) == localUses.end())
                    localUses.emplace(l, LocalUsage(it, it));
            });
            // 2) update fixed locals
            PROFILE(fixLocals, it, localUses, lastWrittenLocal0, lastWrittenLocal1);
            // 3) update local usage-ranges as well as assign all locals to closed-set or open-set
            it->forUsedLocals([this, it](const Local* l, const LocalUse::Type type,
                                  const intermediate::IntermediateInstruction& inst) -> void {
                if(l->type == TYPE_LABEL)
                    return;

                auto& range = localUses.at(l);
                range.associatedInstructions.insert(it);
                range.lastOccurrence = it;
                if(isFixed(range.possibleFiles))
                {
                    // XXX this block is called every time after local is fixed instead of just once per local
                    // but from a performance point of view, it makes no difference whether to check existence in
                    // open/closed set or not
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
}

/**
 * Insert node for r5 register precolored to only allow r5.
 *
 * This is used to only block the r5 accumulator for the ranges where it is actually used, to be able to map more load
 * output locals to r5.
 */
static void insertR5Node(ColoredGraph& graph)
{
    auto& r5Node = graph.getOrCreateNode(analysis::FAKE_REPLICATE_REGISTER,
        ColoredNode(graph, analysis::FAKE_REPLICATE_REGISTER, RegisterFile::ACCUMULATOR));
    r5Node.blockRegister(RegisterFile::ACCUMULATOR, 0);
    r5Node.blockRegister(RegisterFile::ACCUMULATOR, 1);
    r5Node.blockRegister(RegisterFile::ACCUMULATOR, 2);
    r5Node.blockRegister(RegisterFile::ACCUMULATOR, 3);
    r5Node.blockRegister(RegisterFile::ACCUMULATOR, 4);
    if(r5Node.getRegisterFixed() != REG_ACC5)
        throw CompilationError(
            CompilationStep::LABEL_REGISTER_MAPPING, "Replication register node is not fixed to replication register");
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Created dummy node: " << r5Node.to_string(false) << logging::endl);
}

void GraphColoring::createGraph()
{
    // We need to update these every time, since fix in the previous iteration may have changed the livenesses and will
    // have changed the local interferences.
    livenessAnalysis = analysis::GlobalLivenessAnalysis(true);
    livenessAnalysis(method);
    interferenceGraph = analysis::InterferenceGraph::createGraph(method, &livenessAnalysis);

    graph.reserveNodeSize(localUses.size());
    insertR5Node(graph);
    // 1. iteration: set files and locals used together and map to start/end of range
    PROFILE_START(createColoredNodes);
    for(const auto& pair : localUses)
    {
        auto& node = graph.getOrCreateNode(pair.first);
        node.possibleFiles = pair.second.possibleFiles;
        node.initialFile = pair.second.possibleFiles;
        {
            auto writer = pair.first->getSingleWriter();
            auto load = dynamic_cast<const intermediate::LoadImmediate*>(writer);
            auto movesSplatValue =
                check(writer) & &intermediate::IntermediateInstruction::getMoveSource & &Value::isAllSame;
            // TODO can we use the splat value decoration here? First test seem to fail for some reason...
            if(!(load && load->type == intermediate::LoadType::REPLICATE_INT32) && !movesSplatValue)
                // Since writing to r5 automatically replicates, we only use it for values we know to be the same across
                // all SIMD elements
                node.blockR5();
            if(writer && writer->hasConditionalExecution())
                // Since writing to replication register also heeds the element flags, using the replicate-all register
                // as output makes the output of all elements dependent on the flags of the 0th element instead of the
                // flags of the single elements.
                node.blockR5();
        }
        if(pair.second.isFullVectorRotated)
        {
            // "The full horizontal vector rotate is only available when both of the mul ALU input arguments are taken
            // from accumulators r0-r3."
            node.blockRegister(RegisterFile::ACCUMULATOR, 4);
            node.blockRegister(RegisterFile::ACCUMULATOR, 5);
        }
        if(pair.second.firstOccurrence.get() == pair.second.lastOccurrence.get() ||
            !pair.first->hasUsers(LocalUse::Type::READER))
        {
            // If a local is only accessed by a single instruction, it does not do anything useful (either it is only
            // written or only read...). Similarly, if a local is written by multiple instructions but never read, the
            // written value has no use and can be omitted too.
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Local " << pair.first->name << " is never read!" << logging::endl);
            // sanity check
            if(pair.first->hasUsers(LocalUse::Type::READER))
            {
                // This can happen due to errors in the compiler, but also due to undefined behavior in the original
                // source code, in which case we should not fail to compile completely!
                CPPLOG_LAZY(logging::Level::WARNING, {
                    logging::warn() << "Local is being read, but first and last occurrence are the same: "
                                    << pair.first->to_string() << logging::endl;
                    for(const auto& user : pair.first->getUsers())
                        logging::warn() << user.first->to_string() << logging::endl;
                });
            }
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
            LCOV_EXCL_START
            logging::logLazy(logging::Level::WARNING, [&]() {
                logging::warn() << "Node " << node.key->name
                                << " has no initially possible registers:" << logging::endl;
                logging::warn() << "\tBlocked files: " << toString(pair.second.blockedFiles) << logging::endl;
                logging::warn() << "\tInstructions: " << logging::endl;
                for(const auto& it : pair.second.associatedInstructions)
                    logging::warn() << "\t\t" << it->to_string() << logging::endl;
            });
            LCOV_EXCL_STOP
        }
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Created node: " << node.to_string(false) << logging::endl);
    }
    PROFILE_END(createColoredNodes);

    // 2. iteration: associate locals used together
    PROFILE_START(InterferenceToColoredGraph);
    for(const auto& interferenceNode : interferenceGraph->getNodes())
    {
        ColoredNode& node = graph.assertNode(interferenceNode.first);
        if(node.getEdgesSize() == 0)
            // To not force rehashing, only reserve for nodes without edges
            node.reserveEdgesSize(interferenceNode.second.getEdgesSize());
        interferenceNode.second.forAllEdges(
            [&](const analysis::InterferenceNode& neighbor, const analysis::Interference& edge) -> bool {
                // TODO can we here somehow only run for one half of the edges? since now we want to add every edge
                // twice (for each adjacent node)
                node.getOrCreateEdge(&graph.assertNode(neighbor.key)).data = edge.data;
                return true;
            });
    }
    PROFILE_END(InterferenceToColoredGraph);

    // 3. iteration: pre-color neighbors with fixed/blocked register-files
    // This prevents some locals from being assigned to a register-file where other locals used together cannot be
    // assigned anymore to anything, since their only valid register-file is now blocked.
    PROFILE_START(ForwardBlockedRegisterFiles);
    FastSet<ColoredNode*> openNodes;
    for(auto& node : graph.getNodes())
    {
        if(node.second.possibleFiles == RegisterFile::PHYSICAL_A ||
            node.second.possibleFiles == RegisterFile::PHYSICAL_B)
        {
            // this node is fixed to register-file A(B), all others used together cannot be on file A(B)
            auto blockedFile = node.second.possibleFiles;
            auto otherFile =
                blockedFile == RegisterFile::PHYSICAL_A ? RegisterFile::PHYSICAL_B : RegisterFile::PHYSICAL_A;
            node.second.forAllEdges([&](ColoredNode& neighbor, ColoredEdge& edge) -> bool {
                if(edge.data == analysis::InterferenceType::USED_TOGETHER)
                {
                    bool fileRemoved = has_flag(neighbor.possibleFiles, blockedFile);
                    neighbor.possibleFiles = remove_flag(neighbor.possibleFiles, blockedFile);
                    if(fileRemoved && neighbor.possibleFiles == otherFile)
                        // neighbor is now fixed to the other physical file, process all of its neighbors afterwards
                        openNodes.emplace(&neighbor);
                }
                return true;
            });
        }
    }
    // block all locals which interfere with r5 usage from using r5
    graph.assertNode(analysis::FAKE_REPLICATE_REGISTER)
        .forAllEdges([](ColoredNode& neighbor, ColoredEdge& edge) -> bool {
            neighbor.blockR5();
            return true;
        });
    while(!openNodes.empty())
    {
        auto& node = *openNodes.begin();
        auto blockedFile = node->possibleFiles;
        auto otherFile = blockedFile == RegisterFile::PHYSICAL_A ? RegisterFile::PHYSICAL_B : RegisterFile::PHYSICAL_A;
        node->forAllEdges([&](ColoredNode& neighbor, ColoredEdge& edge) -> bool {
            if(edge.data == analysis::InterferenceType::USED_TOGETHER)
            {
                bool fileRemoved = has_flag(neighbor.possibleFiles, blockedFile);
                neighbor.possibleFiles = remove_flag(neighbor.possibleFiles, blockedFile);
                if(fileRemoved && neighbor.possibleFiles == otherFile)
                    // neighbor is now fixed to the other physical file, process all of its neighbors afterwards
                    openNodes.emplace(&neighbor);
            }
            return true;
        });
        openNodes.erase(openNodes.begin());
    }
    PROFILE_END(ForwardBlockedRegisterFiles);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Colored graph with " << graph.getNodes().size() << " nodes created!" << logging::endl);
#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        DebugGraph<const Local*, LocalRelation, ColoredEdge::Directed> debugGraph(
            "/tmp/vc4c-register-graph.dot", graph.getNodes().size());
        auto nameFunc = [](const Local* const& l) -> std::string { return l->name; };
        auto weakEdgeFunc = [](const LocalRelation& r) -> bool { return r != LocalRelation::USED_TOGETHER; };
        for(const auto& node : graph.getNodes())
        {
            debugGraph.addNodeWithNeighbors<ColoredNode>(node.second, nameFunc, weakEdgeFunc);
        }
    });
    LCOV_EXCL_STOP
#endif
}

static void processClosedSet(ColoredGraph& graph, FastSet<const Local*>& closedSet, FastSet<const Local*>& openSet,
    FastSet<const Local*>& errorSet)
{
    PROFILE_SCOPE(processClosedSet);
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

            if(node->possibleFiles == RegisterFile::ACCUMULATOR)
                PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Accumulator assigned", 1);
            else if(node->possibleFiles == RegisterFile::PHYSICAL_A)
                PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Register-file A assigned", 1);
            else if(node->possibleFiles == RegisterFile::PHYSICAL_B)
                PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Register-file B assigned", 1);
        }
        closedSet.erase(node->key);
    }
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
        // for every node in the open-set, assign to register-file B if possible (since this is where most locals can
        // not be assigned due to read-after-write and use-with-literal), then fall back to register-file A and finally
        // accumulator and update all neighbors
        auto& node = graph.assertNode(*openSet.begin());
        RegisterFile currentFile = RegisterFile::NONE;
        if(has_flag(node.possibleFiles, RegisterFile::PHYSICAL_B))
            currentFile = RegisterFile::PHYSICAL_B;
        else if(has_flag(node.possibleFiles, RegisterFile::PHYSICAL_A))
            currentFile = RegisterFile::PHYSICAL_A;
        else if(has_flag(node.possibleFiles, RegisterFile::ACCUMULATOR))
            currentFile = RegisterFile::ACCUMULATOR;
        else
        {
            errorSet.insert(node.key);
            openSet.erase(node.key);
            continue;
        }
        node.possibleFiles = currentFile;
        closedSet.insert(node.key);
        openSet.erase(node.key);
        processClosedSet(graph, closedSet, openSet, errorSet);
    }

    return errorSet.empty();
}

static RegisterFile getBlockedInputs(
    const InstructionWalker it, const ColoredGraph& graph, const Local* toSkip = nullptr)
{
    RegisterFile blockedFiles = RegisterFile::NONE;
    it.forAllInstructions([&graph, toSkip, &blockedFiles](const intermediate::IntermediateInstruction& instr) {
        for(const auto& arg : instr.getArguments())
        {
            if(toSkip != nullptr && arg.hasLocal(toSkip))
                continue;
            if(arg.checkLocal() && isFixed(graph.assertNode(arg.local()).possibleFiles) &&
                graph.assertNode(arg.local()).possibleFiles != RegisterFile::ACCUMULATOR)
                blockedFiles = add_flag(blockedFiles, graph.assertNode(arg.local()).possibleFiles);
            else if(arg.getLiteralValue())
                blockedFiles = add_flag(blockedFiles, RegisterFile::PHYSICAL_B);
            else if(auto reg = arg.checkRegister())
                blockedFiles = add_flag(blockedFiles, reg->file);
        }
    });
    return blockedFiles;
}

static NODISCARD LocalUse checkUser(
    const tools::SmallSortedPointerMap<const LocalUser*, LocalUse>& users, const InstructionWalker it)
{
    LocalUse use;
    it.forAllInstructions([&users, &use](const intermediate::IntermediateInstruction& instr) {
        auto it = users.find(&instr);
        if(it != users.end())
        {
            use.numReads += it->second.numReads;
            use.numWrites += it->second.numWrites;
        }
    });
    return use;
}

static NODISCARD LocalUse assertUser(
    const tools::SmallSortedPointerMap<const LocalUser*, LocalUse>& users, const InstructionWalker it)
{
    auto use = checkUser(users, it);
    if(!use.readsLocal() && !use.writesLocal())
    {
        throw CompilationError(
            CompilationStep::LABEL_REGISTER_MAPPING, "User is not listed in the list of users", it->to_string());
    }
    return use;
}

static NODISCARD bool reassignNodeToRegister(ColoredGraph& graph, ColoredNode& node)
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
    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND, "reassignNodeToRegister", fixed);
    return fixed;
}

static NODISCARD bool moveLocalToRegisterFile(Method& method, ColoredGraph& graph, ColoredNode& node,
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
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Fixing register-conflict by using temporary as input for: " << it->to_string() << logging::endl);
        it.emplace(std::make_unique<intermediate::MoveOperation>(tmp, node.key->createReference()));
        auto& tmpUse = localUses.emplace(tmp.local(), LocalUsage(it, it)).first->second;
        it.nextInBlock();
        it->replaceLocal(node.key, tmp.local(), LocalUse::Type::READER);
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
        auto& tmpNode = graph.getOrCreateNode(tmp.local(), ColoredNode(graph, tmp.local(), RegisterFile::ACCUMULATOR));
        // XXX setting the neighbors of the temporary to the neighbors of the local actually is far too broad, but we
        // cannot determine the actual neighbors
        tmpNode.takeValues(node);
        // TODO need to update the local used in the current instruction as input with the new temporary
        tmpNode.reserveEdgesSize(node.getEdgesSize());
        node.forAllEdges([&](ColoredNode& neighbor, ColoredEdge&) -> bool {
            neighbor.getOrCreateEdge(&tmpNode, LocalRelation::USED_SIMULTANEOUSLY);
            return true;
        });
        if(!reassignNodeToRegister(graph, graph.assertNode(tmp.local())))
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

static NODISCARD bool fixSingleError(Method& method, ColoredGraph& graph, ColoredNode& node,
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
    auto& use = localUses.at(node.key);

    // CASE 1)
    if(node.initialFile == RegisterFile::ACCUMULATOR && !node.hasFreeRegisters(RegisterFile::ACCUMULATOR))
    {
        // fix read-after-writes, so local can be on non-accumulator:
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Fixing register error case 1 for: " << node.key->to_string() << logging::endl);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND, "Register error case 1", 1);

        // the register-files which can be used after the fix by this local
        RegisterFile freeFiles = remove_flag(RegisterFile::ANY, use.blockedFiles);

        // we need to copy the list, since we might edit the original list inside the loop
        auto oldInstructions = localUse.associatedInstructions;
        for(InstructionWalker it : oldInstructions)
        {
            if(auto comb = it.get<intermediate::CombinedOperation>())
            {
                if(comb->getFirstOp()->writesLocal(node.key) && comb->getSecondOp()->writesLocal(node.key))
                {
                    // If a combined instruction writes the same local in both options, the local needs to be on an
                    // accumulator, independent whether the next instruction reads it. To fix this, new need to split up
                    // the combined instructions into two separate ones.
                    auto parts = comb->splitUp();
                    auto firstIt = it.copy();
                    firstIt.emplace(std::move(parts.first));
                    localUse.associatedInstructions.emplace(firstIt);
                    it.reset(std::move(parts.second));
                    // the "original" InstructionWalker is already in the associatedInstructions set, since we only
                    // change its content
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Split up combined instruction into '" << firstIt->to_string() << "' and '"
                            << it->to_string() << '\'' << logging::endl);
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND, "Combined split up", 1);
                }
                // fall-through on purpose in any case (even if rewritten), since there still could be a
                // read-after-write to be handled below
            }
            // 1) check if usage is a write
            if(assertUser(users, it).writesLocal())
            {
                // 2) check if next instruction reads this local
                do
                {
                    it.nextInMethod();
                } while(!it.isEndOfMethod() && it.has() && !it->mapsToASMInstruction());
                bool localRead = checkUser(users, it).readsLocal();
                if(localRead && it->getVectorRotation())
                {
                    // TODO for locals used in vector rotations, this fix is wrong
                    throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING,
                        "Fixing errors in locals used as input to vector rotations is not implemented",
                        it->to_string());
                }
                // 3) if so, insert nop
                if(localRead)
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Fixing register-conflict by inserting NOP before: " << it->to_string()
                            << logging::endl);
                    it.emplace(std::make_unique<intermediate::Nop>(intermediate::DelayType::WAIT_REGISTER));
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND, "NOP insertions", 1);
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
        node.initialFile = remove_flag(RegisterFile::ANY, use.blockedFiles);
        localUse.possibleFiles = remove_flag(RegisterFile::ANY, use.blockedFiles);
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
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Fixing register error case 2 for: " << node.key->to_string() << logging::endl);
        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Register error case 2", 1);

        bool fileACouldBeUsed =
            has_flag(node.initialFile, RegisterFile::PHYSICAL_A) && node.hasFreeRegisters(RegisterFile::PHYSICAL_A);
        bool fileBCouldBeUsed =
            has_flag(node.initialFile, RegisterFile::PHYSICAL_B) && node.hasFreeRegisters(RegisterFile::PHYSICAL_B);

        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "A blocked", !fileACouldBeUsed);
        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "B blocked", !fileBCouldBeUsed);

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

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Trying to fix local to register-file "
                << toString(add_flag(fileACouldBeUsed ? RegisterFile::PHYSICAL_A : RegisterFile::NONE,
                       fileBCouldBeUsed ? RegisterFile::PHYSICAL_B : RegisterFile::NONE))
                << logging::endl);

        if(!has_flag(use.blockedFiles, RegisterFile::ACCUMULATOR))
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
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Fixing register error case 3 for: " << node.key->to_string() << logging::endl);
        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "Register error case 3", 1);

        bool moveToFileA = node.hasFreeRegisters(RegisterFile::PHYSICAL_A);
        bool moveToFileB = node.hasFreeRegisters(RegisterFile::PHYSICAL_B);

        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "move to A", moveToFileA);
        PROFILE_COUNTER_SCOPE(vc4c::profiler::COUNTER_BACKEND, "move to B", moveToFileB);

        if(moveToFileA && moveToFileB)
        {
            // this could only happen, if neither physical file was in the initial files, in which case CASE 1) should
            // have been entered
            throw CompilationError(
                CompilationStep::LABEL_REGISTER_MAPPING, "Invalid case of free blocked registers", node.to_string());
        }
        else if(!moveToFileA && !moveToFileB)
        {
            // there are no more free register AT ALL, so we cannot do anything useful here
            return false;
        }

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Trying to fix local to register-file "
                << toString(add_flag(moveToFileA ? RegisterFile::PHYSICAL_A : RegisterFile::NONE,
                       moveToFileB ? RegisterFile::PHYSICAL_B : RegisterFile::NONE))
                << logging::endl);

        if(has_flag(use.blockedFiles, RegisterFile::ACCUMULATOR))
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
    PROFILE_SCOPE(fixRegisterErrors);
    logging::logLazy(logging::Level::DEBUG, [&]() {
        for(const auto& node : graph.getNodes())
            logging::debug() << node.second.to_string() << logging::endl;
    });

    auto locIt = errorSet.begin();
    while(locIt != errorSet.end())
    {
        ColoredNode& node = graph.assertNode(*locIt);
        LCOV_EXCL_START
        logging::logLazy(logging::Level::DEBUG, [&]() {
            logging::debug() << "Error in register-allocation for node: " << node.to_string() << logging::endl;
            auto& s = logging::debug() << "Local is blocked by: ";
            node.forAllEdges([&](const ColoredNode& neighbor, const ColoredEdge& edge) -> bool {
                if(blocksLocal(&neighbor, edge.data))
                    s << neighbor.to_string() << ", ";
                return true;
            });
            s << logging::endl;
        });
        LCOV_EXCL_STOP
        if(fixSingleError(method, graph, node, localUses, localUses.at(*locIt)))
            locIt = errorSet.erase(locIt);
        else
            ++locIt;
    }
    return errorSet.empty();
}

FastMap<const Local*, Register> GraphColoring::toRegisterMap() const
{
    if(!errorSet.empty())
    {
        for(const Local* loc : errorSet)
            logging::error() << "Error assigning local to register: " << loc->name << logging::endl;
        throw CompilationError(CompilationStep::LABEL_REGISTER_MAPPING, "There are erroneous register-associations!");
    }

    FastMap<const Local*, Register> result;
    result.reserve(graph.getNodes().size());

    for(const auto& pair : graph.getNodes())
    {
        auto it = result.emplace(pair.first, pair.second.getRegisterFixed()).first;
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Assigned local " << pair.first->name << " to register " << it->second.to_string(true, false)
                << logging::endl);
    }

    return result;
}

const analysis::GlobalLivenessAnalysis& GraphColoring::getLivenessAnalysis() const
{
    return livenessAnalysis;
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
