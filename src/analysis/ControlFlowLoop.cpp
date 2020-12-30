/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "ControlFlowLoop.h"

#include "../analysis/FlagsAnalysis.h"
#include "../intermediate/Helper.h"
#include "ControlFlowGraph.h"
#include "DataDependencyGraph.h"
#include "DebugGraph.h"
#include "log.h"

#include <limits>
#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

std::string InductionVariable::to_string() const
{
    std::string condString = "(?)";
    if(repeatCondition)
    {
        condString = conditionCheckedBeforeStep ? local->name : inductionStep->checkOutputLocal()->name;
        condString += std::string(" ") + repeatCondition->comparisonName + " " +
            repeatCondition->comparisonValue.to_string() + " into " + repeatCondition->conditionResult->to_string();
    }
    return local->to_string() + " from " + initialAssignment->to_string() + ", step " + inductionStep->to_string() +
        ", while " + condString;
}

Optional<Literal> InductionVariable::getLowerBound() const
{
    return initialAssignment->precalculate(4).first & &Value::getLiteralValue;
}

Optional<Literal> InductionVariable::getUpperBound() const
{
    return repeatCondition ? repeatCondition->comparisonValue.getLiteralValue() : Optional<Literal>{};
}

Optional<Literal> InductionVariable::getStep() const
{
    if(auto stepValue = inductionStep->findOtherArgument(local->createReference()))
    {
        if(auto stepWriter = stepValue->getSingleWriter())
            return stepWriter->precalculate(4).first & &Value::getLiteralValue;
        return stepValue->getConstantValue() & &Value::getLiteralValue;
    }
    return {};
}

Optional<unsigned> InductionVariable::getRange() const
{
    if(!repeatCondition)
        return {};
    auto comp = repeatCondition.value().comparisonName;
    auto lowerBound = getLowerBound();
    auto upperBound = getUpperBound();
    if(!lowerBound || !upperBound || comp.empty())
        return {};

    if((comp == intermediate::COMP_SIGNED_LE || comp == intermediate::COMP_SIGNED_LT ||
           comp == intermediate::COMP_UNSIGNED_LE || comp == intermediate::COMP_UNSIGNED_LT) &&
        lowerBound->signedInt() > upperBound->signedInt())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Iterating across type wrap is not supported for: " << to_string() << logging::endl);
        return {};
    }

    if((comp == intermediate::COMP_SIGNED_GE || comp == intermediate::COMP_SIGNED_GT ||
           comp == intermediate::COMP_UNSIGNED_GE || comp == intermediate::COMP_UNSIGNED_GT) &&
        lowerBound->signedInt() < upperBound->signedInt())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Iterating across type wrap is not supported for: " << to_string() << logging::endl);
        return {};
    }

    if(comp == intermediate::COMP_SIGNED_LT)
        return static_cast<unsigned>(upperBound->signedInt() - lowerBound->signedInt());
    if(comp == intermediate::COMP_SIGNED_LE)
        return static_cast<unsigned>(upperBound->signedInt() - lowerBound->signedInt() + 1);
    if(comp == intermediate::COMP_SIGNED_GT)
        return static_cast<unsigned>(lowerBound->signedInt() - upperBound->signedInt());
    if(comp == intermediate::COMP_SIGNED_GE)
        return static_cast<unsigned>(lowerBound->signedInt() - upperBound->signedInt() + 1);
    if(comp == intermediate::COMP_UNSIGNED_LT)
        return upperBound->unsignedInt() - lowerBound->unsignedInt();
    if(comp == intermediate::COMP_UNSIGNED_LE)
        return upperBound->unsignedInt() - lowerBound->unsignedInt() + 1u;
    if(comp == intermediate::COMP_UNSIGNED_GT)
        return lowerBound->unsignedInt() - upperBound->unsignedInt();
    if(comp == intermediate::COMP_UNSIGNED_GE)
        return lowerBound->unsignedInt() - upperBound->unsignedInt() + 1u;
    if(comp == intermediate::COMP_NEQ)
        // XXX could be wrong for unsigned induction variable and more than 2^31 iterations
        return static_cast<unsigned>(std::max(lowerBound->signedInt(), upperBound->signedInt()) -
            std::min(lowerBound->signedInt(), upperBound->signedInt()));

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Unsupported comparison for calculating distance for: " << to_string() << logging::endl);
    return {};
}

Optional<unsigned> InductionVariable::getIterationCount() const
{
    auto distance = getRange();
    auto stepValue = getStep();
    if(!distance || !stepValue)
        return {};

    auto step = static_cast<unsigned>(std::abs(stepValue->signedInt()));

    // if the condition is done before the step, we need to iteration one step extra
    auto actualDistance = *distance + (conditionCheckedBeforeStep ? step : 0);

    if(inductionStep->op == OP_ADD)
        // iterations = (end - start) / step
        return actualDistance / step;
    if(inductionStep->op == OP_SUB)
        // iterations = (start - end) / step
        return actualDistance / step;
    // XXX add support for more step operations
    // E.g. mul? Need to calculate:
    // limit = (start * step) ^ iterations -> iterations = log(start * step) / log(limit)

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Unsupported induction operation for: " << to_string() << logging::endl);
    return {};
}

const CFGNode* ControlFlowLoop::findPredecessor() const
{
    const CFGNode* predecessor = nullptr;
    for(const CFGNode* node : *this)
    {
        node->forAllIncomingEdges([this, &predecessor](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(find(&neighbor) == end())
            {
                // the relation is backwards and node is not within this loop -> predecessor
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found predecessor for CFG loop: " << neighbor.key->to_string() << logging::endl);

                if(predecessor != nullptr && predecessor != &neighbor)
                {
                    // multiple predecessors, abort
                    predecessor = nullptr;
                    return false;
                }
                predecessor = &neighbor;
            }
            return true;
        });
    }
    return predecessor;
}

tools::SmallSortedPointerSet<const CFGNode*> ControlFlowLoop::findPredecessors() const
{
    tools::SmallSortedPointerSet<const CFGNode*> predecessors;
    for(const CFGNode* node : *this)
    {
        node->forAllIncomingEdges([this, &predecessors](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(find(&neighbor) == end())
            {
                predecessors.emplace(&neighbor);
            }
            return true;
        });
    }
    return predecessors;
}

const CFGNode* ControlFlowLoop::findSuccessor() const
{
    const CFGNode* successor = nullptr;
    for(const CFGNode* node : *this)
    {
        node->forAllOutgoingEdges([this, &successor](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(find(&neighbor) == end())
            {
                // the relation is forward and node is not within this loop -> successor
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found successor for CFG loop: " << neighbor.key->to_string() << logging::endl);

                if(successor != nullptr && successor != &neighbor)
                {
                    // multiple successors, abort
                    successor = nullptr;
                    return false;
                }
                successor = &neighbor;
            }
            return true;
        });
    }
    return successor;
}

tools::SmallSortedPointerSet<const CFGNode*> ControlFlowLoop::findSuccessors() const
{
    tools::SmallSortedPointerSet<const CFGNode*> successors;
    for(const CFGNode* node : *this)
    {
        node->forAllOutgoingEdges([this, &successors](const CFGNode& neighbor, const CFGEdge& edge) -> bool {
            if(find(&neighbor) == end())
                successors.emplace(&neighbor);
            return true;
        });
    }
    return successors;
}

Optional<InstructionWalker> ControlFlowLoop::findInLoop(const intermediate::IntermediateInstruction* inst) const
{
    for(const CFGNode* node : *this)
    {
        if(auto it = node->key->findWalkerForInstruction(inst, node->key->walkEnd()))
            return it;
    }
    return {};
}

bool ControlFlowLoop::includes(const ControlFlowLoop& other) const
{
    if(*this == other || size() <= other.size())
        return false;

    for(auto otherItr : other)
    {
        if(find(otherItr) == end())
            return false;
    }

    return true;
}

tools::SmallSortedPointerSet<const Local*> ControlFlowLoop::findLocalDependencies(
    const DataDependencyGraph& dependencyGraph) const
{
    SortedSet<const Local*> innerDependencies;
    SortedSet<const Local*> outerDependencies;
    for(auto& node : *this)
    {
        // not all basic blocks have an entry in the dependency graph (e.g. if they have no dependency)
        if(auto dependencyNode = dependencyGraph.findNode(node->key))
        {
            dependencyNode->forAllIncomingEdges(
                [&](const DataDependencyNode& neighbor, const DataDependencyEdge& edge) -> bool {
                    // check if this basic block has a local dependent on at least two phi-nodes
                    auto it = edge.data.find(neighbor.key);
                    if(it != edge.data.end())
                    {
                        for(auto& dependency : it->second)
                        {
                            if(has_flag(dependency.second, add_flag(DataDependencyType::PHI, DataDependencyType::FLOW)))
                            {
                                // TODO couldn't this be simplified to checking neighbor against the main dependency
                                // node's basic block?? Since this is an edge from/to that node?
                                if(std::find_if(begin(), end(), [&neighbor](const CFGNode* node) -> bool {
                                       return node->key == neighbor.key;
                                   }) != end())
                                    //... one of which lies within the loop
                                    innerDependencies.emplace(dependency.first);
                                else
                                    //... and the other outside of it
                                    outerDependencies.emplace(dependency.first);
                            }
                        }
                    }

                    return true;
                });
        }
    }

    tools::SmallSortedPointerSet<const Local*> intersection;
    // NOTE: Cannot pass unordered_set into set_intersection, since it requires its inputs (and output) to be sorted!
    std::set_intersection(innerDependencies.begin(), innerDependencies.end(), outerDependencies.begin(),
        outerDependencies.end(), std::inserter(intersection, intersection.begin()));

    return intersection;
}

FastAccessList<InductionVariable> ControlFlowLoop::findInductionVariables(
    const DataDependencyGraph& dependencyGraph, bool includeIterationInformation) const
{
    /*
     * Algorithm taken from https://www.cs.princeton.edu/courses/archive/spring03/cs320/notes/loops.pdf slides 29-31:
     *
     * 1. Find all basic induction variables:
     *    - Find all locals which are written once from inside the loop and once from outside of it
     *    - Filter the list to only include locals which are modified inside the loop by a constant expression
     *    - Determine initial value by taking value assigned to local outside of function body
     * TODO 2. Find all derived induction variables:
     *    (see slides)
     */
    FastAccessList<InductionVariable> variables;

    // The loop tail, the block taking the repetition branch
    auto tail = getTail();
    // The repetition branch
    InstructionWalker tailBranch = tail ? backEdge->data.getPredecessor(tail->key) : InstructionWalker{};
    if(!tail || tailBranch.isEndOfBlock())
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Failed to determine loop tail for: " << to_string() << logging::endl);
        return {};
    }

    for(auto local : findLocalDependencies(dependencyGraph))
    {
        if(local == nullptr)
            continue;
        if(auto inductionVar = extractInductionVariable(local, tailBranch, tail, includeIterationInformation))
            variables.emplace_back(std::move(inductionVar).value());
    }

    return variables;
}

const CFGNode* ControlFlowLoop::getHeader() const
{
    // According to https://www.cs.princeton.edu/courses/archive/spring03/cs320/notes/loops.pdf, slide 11,
    // the header is the only node not in the loop that has predecessors which are not inside the loop.

    const CFGNode* header = nullptr;

    if(size() == 1)
        // special case for single-block loops
        return *begin();

    // we also know that the header is one of the nodes adjacent to the back edge, so only check these two
    for(auto& node : backEdge->getNodes())
    {
        node->forAllIncomingEdges([&](const CFGNode& pred, const CFGEdge& edge) {
            if(find(&pred) == end())
            {
                // node inside the loop with predecessor outside of it
                header = node;
                return false;
            }
            return true;
        });
    }

    return header;
}

const CFGNode* ControlFlowLoop::getTail() const
{
    // The tail is defined as the node (inside the loop) which has the back-edge to the header

    if(size() == 1)
        // special case for single block loops
        return *begin();

    if(auto header = getHeader())
        return &backEdge->getOtherNode(*header);
    return nullptr;
}

FastSet<InstructionWalker> ControlFlowLoop::findLoopInvariants()
{
    /*
     * For algorithm, see
     * https://www.cs.princeton.edu/courses/archive/spring03/cs320/notes/loops.pdf, slide 23
     */
    FastMap<const intermediate::IntermediateInstruction*, InstructionWalker> invariantInstructions;

    // given a certain loop, any instruction which is known to be invariant, calculates a constant expression (without
    // depending on some flags) or is not part of the loop is considered invariant
    auto writerIsInvariant = [&](const LocalUser* writer) -> bool {
        return invariantInstructions.find(writer) != invariantInstructions.end() ||
            (!writer->hasConditionalExecution() && writer->precalculate().first) || !findInLoop(writer);
    };

    // given a certain loop, any instruction which only consumes constants or locals written by invariant instructions
    // (see above) is considered invariant
    auto checkArgInvariant = [&](const Value& arg) -> bool {
        if(arg.getLiteralValue())
            return true;
        if(arg.hasRegister(REG_ELEMENT_NUMBER) || arg.hasRegister(REG_QPU_NUMBER))
            return true;
        if(auto local = arg.checkLocal())
        {
            return local->allUsers(LocalUse::Type::WRITER, writerIsInvariant);
        }
        return false;
    };

    for(auto node : *this)
    {
        auto it = node->key->walk();
        while(!it.isEndOfBlock())
        {
            if(it.has() && !it.get<intermediate::BranchLabel>() && !it->getSignal().hasSideEffects() &&
                std::all_of(it->getArguments().begin(), it->getArguments().end(), checkArgInvariant))
            {
                bool invariantCondition = false;
                if(!it->hasConditionalExecution())
                    invariantCondition = true;
                else if(auto setflag = it.getBasicBlock()->findLastSettingOfFlags(it))
                    invariantCondition = invariantInstructions.find((*setflag).get()) != invariantInstructions.end();

                if(invariantCondition)
                    invariantInstructions.emplace(it.get(), it);
            }
            it.nextInBlock();
        }
    }

    FastSet<InstructionWalker> result;
    result.reserve(invariantInstructions.size());
    for(const auto& entry : invariantInstructions)
        result.emplace(entry.second);

    return result;
}

bool ControlFlowLoop::isWorkGroupLoop() const
{
    // check all edges of adjacent nodes within this loop
    for(const auto& node : *this)
    {
        bool hasWorkGroupLoop = false;
        node->forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
            if(edge.data.isWorkGroupLoop && find(&successor) != end())
            {
                hasWorkGroupLoop = true;
                return false;
            }
            return true;
        });
        if(hasWorkGroupLoop)
            return true;
    }
    return false;
}

bool ControlFlowLoop::operator==(const ControlFlowLoop& other) const noexcept
{
    if(size() != other.size())
        return false;
    if(backEdge != other.backEdge)
        return false;
    if(empty())
        return true;
    return std::equal_to<FastSet<const CFGNode*>>{}(*this, other);
}

bool ControlFlowLoop::operator!=(const ControlFlowLoop& other) const noexcept
{
    if(size() != other.size())
        return true;
    if(backEdge != other.backEdge)
        return true;
    return !(*this == other);
}

LCOV_EXCL_START
std::string ControlFlowLoop::to_string() const
{
    std::stringstream ss;
    if(auto head = getHeader())
        ss << head->key->to_string();
    else
        ss << "(unknown header)";
    ss << " -> ";
    if(auto tail = getTail())
        ss << tail->key->to_string();
    else
        ss << "(unknown tail)";

    ss << " (" << size() << ") {";
    for(const auto& node : *this)
        ss << node->key->to_string() << ", ";
    ss << '}';
    return ss.str();
}
LCOV_EXCL_STOP

static void addForwardCandidates(SortedSet<const intermediate::IntermediateInstruction*>& candidates,
    const intermediate::IntermediateInstruction& inst, const ControlFlowLoop& loop, bool initialRun)
{
    // if the instruction is a phi-node, abort
    if(inst.hasConditionalExecution())
        return;
    if(!initialRun && inst.hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
        return;
    // if the instruction is a move, recursively check all readers of the instruction's result that are inside of the
    // loop
    if(auto move = dynamic_cast<const intermediate::MoveOperation*>(&inst))
    {
        if(!move->isSimpleMove() || move->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
            // allow phi-node only if it is directly the step operation candidate
            return;
        if(auto loc = move->getOutput()->checkLocal())
        {
            loc->forUsers(LocalUse::Type::READER, [&](const LocalUser* reader) {
                if(loop.findInLoop(reader))
                    addForwardCandidates(candidates, *reader, loop, false);
            });
        }
    }
    // if the instruction calculates something, it is a candidate
    if(auto op = dynamic_cast<const intermediate::Operation*>(&inst))
        candidates.emplace(op);
}

static void addBackwardCandidates(SortedSet<const intermediate::IntermediateInstruction*>& candidates,
    const intermediate::IntermediateInstruction& inst, const ControlFlowLoop& loop)
{
    // if the instruction is a move with a single writer, recursively check the writer of the source
    if(auto move = dynamic_cast<const intermediate::MoveOperation*>(&inst))
    {
        if(!move->isSimpleMove())
            return;
        if(auto loc = move->getSource().checkLocal())
        {
            loc->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
                if(loop.findInLoop(writer))
                    addBackwardCandidates(candidates, *writer, loop);
            });
        }
    }
    // if the instruction calculates something, it is a candidate
    if(auto op = dynamic_cast<const intermediate::Operation*>(&inst))
        candidates.emplace(op);
}

static void addForwardFlagCandidates(FastSet<const intermediate::IntermediateInstruction*>& candidates,
    const intermediate::IntermediateInstruction& inst, const ControlFlowLoop& loop)
{
    // if the instruction is a phi-node, abort
    if(inst.hasDecoration(intermediate::InstructionDecorations::PHI_NODE) || inst.hasConditionalExecution())
        return;
    // if the instruction sets the flags, it is a candidate
    if(inst.doesSetFlag())
        candidates.emplace(&inst);
    // if the instruction is a move, recursively check all readers of the instruction's result that are inside of the
    // loop
    if(auto move = dynamic_cast<const intermediate::MoveOperation*>(&inst))
    {
        if(dynamic_cast<const intermediate::IntermediateInstruction*>(move))
            return;
        if(auto loc = move->getOutput()->checkLocal())
        {
            loc->forUsers(LocalUse::Type::READER, [&](const LocalUser* reader) {
                if(loop.findInLoop(reader))
                    addForwardFlagCandidates(candidates, *reader, loop);
            });
        }
    }
}

static std::pair<const intermediate::IntermediateInstruction*,
    tools::SmallSortedPointerSet<const intermediate::IntermediateInstruction*>>
findStepCandidates(const Local* local, const ControlFlowLoop& loop)
{
    const intermediate::IntermediateInstruction* initialAssignment = nullptr;
    // since there might be a few moves between the step expression and the assignment from/to the local, we walk
    // the moves from both ends into the "middle" and see if there is one instruction which is both reachable via
    // moves from the first read and the last write of the local inside the loop.
    SortedSet<const intermediate::IntermediateInstruction*> forwardStepCandidates;
    SortedSet<const intermediate::IntermediateInstruction*> backwardStepCandidates;

    for(const auto& pair : local->getUsers())
    {
        auto it = loop.findInLoop(pair.first);

        if(pair.second.writesLocal() && pair.first->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
        {
            // check for the write inside or outside of the loop
            if(it)
                // inside of loop - add to list of step function candidates
                addBackwardCandidates(backwardStepCandidates, *pair.first, loop);
            else
                // outside of loop - writes initial value
                initialAssignment = pair.first;
        }
        if(pair.second.readsLocal() && it)
            // read inside of loop - walk all readers to assemble the list of step candiates
            addForwardCandidates(forwardStepCandidates, *pair.first, loop, true);
    }

    tools::SmallSortedPointerSet<const intermediate::IntermediateInstruction*> stepCandidates{};
    // NOTE: Cannot pass unordered_set into set_intersection, since it requires its inputs (and output) to be
    // sorted!
    std::set_intersection(forwardStepCandidates.begin(), forwardStepCandidates.end(), backwardStepCandidates.begin(),
        backwardStepCandidates.end(), std::inserter(stepCandidates, stepCandidates.begin()));

    return std::make_pair(initialAssignment, std::move(stepCandidates));
}

static std::pair<const intermediate::IntermediateInstruction*, const intermediate::Operation*>
findInitialAssignmentAndSingleStep(const Local* local, const ControlFlowLoop& loop)
{
    auto stepInfo = findStepCandidates(local, loop);
    if(!stepInfo.first || stepInfo.second.empty())
        return {};
    auto& stepCandidates = stepInfo.second;

    if(stepCandidates.size() > 1)
    {
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Induction variable has multiple step candidate instructions: " << local->to_string()
                             << logging::endl;
            for(auto& candidate : stepCandidates)
                logging::debug() << '\t' << candidate->to_string() << logging::endl;
        });
        return {};
    }

    auto stepOperation = dynamic_cast<const intermediate::Operation*>(*stepCandidates.begin());
    if(!stepOperation)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Step instruction is not an operation: " << (*stepCandidates.begin())->to_string() << logging::endl);
        return {};
    }

    auto step = Expression::createExpression(*stepOperation);
    if(!step || !step->hasConstantOperand())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine induction step expression for: " << local->to_string() << logging::endl);
        return {};
    }

    return std::make_pair(stepInfo.first, stepOperation);
}

static const char* switchComparisonOperands(const std::string& comparison)
{
    if(comparison == intermediate::COMP_EQ)
        return intermediate::COMP_EQ;
    if(comparison == intermediate::COMP_NEQ)
        return intermediate::COMP_NEQ;
    if(comparison == intermediate::COMP_UNSIGNED_GE)
        return intermediate::COMP_UNSIGNED_LE;
    if(comparison == intermediate::COMP_UNSIGNED_GT)
        return intermediate::COMP_UNSIGNED_LT;
    if(comparison == intermediate::COMP_UNSIGNED_LE)
        return intermediate::COMP_UNSIGNED_GE;
    if(comparison == intermediate::COMP_UNSIGNED_LT)
        return intermediate::COMP_UNSIGNED_GT;
    if(comparison == intermediate::COMP_SIGNED_GE)
        return intermediate::COMP_SIGNED_LE;
    if(comparison == intermediate::COMP_SIGNED_GT)
        return intermediate::COMP_SIGNED_LT;
    if(comparison == intermediate::COMP_SIGNED_LE)
        return intermediate::COMP_SIGNED_GE;
    if(comparison == intermediate::COMP_SIGNED_LT)
        return intermediate::COMP_SIGNED_GT;

    throw CompilationError(CompilationStep::GENERAL, "Unhandled comparison operation to switch", comparison);
}

static void addIterationInformation(
    InductionVariable& inductionVar, const ControlFlowLoop& loop, InstructionWalker tailBranch, const CFGNode* tail)
{
    auto local = inductionVar.local;
    auto stepOperation = inductionVar.inductionStep;
    const Local* repeatConditionLocal = nullptr;
    ConditionCode repeatCondition = COND_NEVER;

    if(auto branch = tailBranch.get<intermediate::Branch>())
    {
        if(auto branchCondition = tailBranch.getBasicBlock()->findLastSettingOfFlags(tailBranch))
        {
            auto pair = intermediate::getBranchCondition(branchCondition->get<intermediate::ExtendedInstruction>());
            if(!pair.first || pair.second != 0x1)
            {
                // for now we don't support any non-standard conditions. Otherwise we also would need to support all
                // any/all flags combinations
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Skipping non-default branch condition: " << (*branchCondition)->to_string()
                        << logging::endl);
                return;
            }
            repeatConditionLocal = pair.first->local();
        }
        repeatCondition = branch->branchCondition.toConditionCode();
    }

    if(!repeatConditionLocal)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine repeat condition for: " << local->to_string() << logging::endl);
        return;
    }

    auto repeatComparison = getComparison(repeatConditionLocal, tail->key->walk());
    if(!repeatComparison)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine repeat comparison for boolean variable: " << repeatConditionLocal->to_string()
                << logging::endl);
        return;
    }

    auto stepLocal = stepOperation->getOutput().value().local();
    auto leftOperand = intermediate::getSourceValue(repeatComparison->leftOperand);
    leftOperand = leftOperand.getConstantValue().value_or(leftOperand);
    auto rightOperand = intermediate::getSourceValue(repeatComparison->rightOperand);
    rightOperand = rightOperand.getConstantValue().value_or(rightOperand);

    bool isLeftOperand = true;
    if(leftOperand.checkLocal() == stepLocal || rightOperand.checkLocal() == stepLocal)
    {
        isLeftOperand = leftOperand.checkLocal() == stepLocal;
        inductionVar.conditionCheckedBeforeStep = false;
    }
    else if(leftOperand.checkLocal() == local || rightOperand.checkLocal() == local)
    {
        isLeftOperand = leftOperand.checkLocal() == local;
        inductionVar.conditionCheckedBeforeStep = true;
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine whether the induction or the step local is used in the comparison: "
                << repeatComparison->to_string() << logging::endl);
        return;
    }

    auto compName = repeatComparison->name;
    if(!isLeftOperand)
        // we expect the induction variable (or its derivative) to be on the left side of the comparison, so we need
        // to switch the operands and the comparison function
        compName = switchComparisonOperands(compName);
    if(repeatCondition != COND_ZERO_CLEAR)
    {
        // TODO need to heed the repeatCondition? E.g. if repeat branch is not on "true", need to invert comparison!
        CPPLOG_LAZY(logging::Level::WARNING,
            log << "Repetition branch on this flag is not yet supported: " << repeatCondition.to_string()
                << logging::endl);
        return;
    }
    inductionVar.repeatCondition =
        RepeatCondition{compName, isLeftOperand ? rightOperand : leftOperand, repeatConditionLocal};
}

Optional<InductionVariable> ControlFlowLoop::extractInductionVariable(
    const Local* local, InstructionWalker tailBranch, const CFGNode* tail, bool includeIterationInformation) const
{
    auto stepInfo = findInitialAssignmentAndSingleStep(local, *this);
    if(!stepInfo.first || !stepInfo.second)
        return {};
    auto initialAssignment = stepInfo.first;
    auto stepOperation = stepInfo.second;

    // we have an initial value as well as a single step expression
    // since the remaining values and instructions are not required for an induction variable, we try to find it out
    // after adding the variable to the result

    InductionVariable inductionVar{const_cast<Local*>(local), initialAssignment, stepOperation};

    if(includeIterationInformation)
        // try to find the additional information indicating repetition count, etc.
        addIterationInformation(inductionVar, *this, tailBranch, tail);

    return inductionVar;
}

LoopInclusionTreeNodeBase::~LoopInclusionTreeNodeBase() noexcept = default;

LoopInclusionTreeNode* analysis::castToTreeNode(LoopInclusionTreeNodeBase* base)
{
    auto* node = dynamic_cast<LoopInclusionTreeNode*>(base);
    if(node == nullptr)
    {
        throw CompilationError(CompilationStep::OPTIMIZER, "Cannot downcast to LoopInclusionTreeNode.");
    }
    return node;
}

const LoopInclusionTreeNode* analysis::castToTreeNode(const LoopInclusionTreeNodeBase* base)
{
    auto* node = dynamic_cast<const LoopInclusionTreeNode*>(base);
    if(node == nullptr)
    {
        throw CompilationError(CompilationStep::OPTIMIZER, "Cannot downcast to LoopInclusionTreeNode.");
    }
    return node;
}

LoopInclusionTreeNodeBase* LoopInclusionTreeNodeBase::findRoot(Optional<int> depth)
{
    auto realDepth = depth ? *depth : std::numeric_limits<int>::max();
    auto root = castToTreeNode(this);
    while(realDepth > 0)
    {
        auto tmp = root->getSinglePredecessor();
        if(!tmp)
            break;
        root = tmp;
        --realDepth;
    }

    return root;
}

unsigned int LoopInclusionTreeNodeBase::getLongestPathToRoot() const
{
    auto* self = castToTreeNode(this);

    unsigned int longestLength = 0;
    self->forAllIncomingEdges([&](const LoopInclusionTreeNodeBase& parent, const LoopInclusionTreeEdge&) -> bool {
        auto length = parent.getLongestPathToRoot() + 1u;
        longestLength = std::max(longestLength, length);
        return true;
    });

    return longestLength;
}

bool LoopInclusionTreeNodeBase::hasCFGNodeInChildren(const CFGNode* node) const
{
    auto* self = castToTreeNode(this);

    bool found = false;
    self->forAllOutgoingEdges([&](const LoopInclusionTreeNodeBase& childBase, const LoopInclusionTreeEdge&) -> bool {
        auto* child = castToTreeNode(&childBase);
        auto nodes = child->key;

        auto targetNode = nodes->find(node);
        if(targetNode != nodes->end())
        {
            found = true;
            return false;
        }
        auto foundInChildren = child->hasCFGNodeInChildren(node);
        if(foundInChildren)
        {
            found = true;
            return false;
        }
        return true;
    });

    return found;
}

LCOV_EXCL_START
std::string LoopInclusionTreeNodeBase::to_string() const
{
    auto* self = castToTreeNode(this);
    std::string name;
    if(auto header = self->key->getHeader())
        name = header->key->to_string();
    else
        name = "(unknown header)";
    name.append(" -> ");
    if(auto tail = self->key->getTail())
        name.append(tail->key->to_string());
    else
        name.append("(unknown tail)");
    return name;
}
LCOV_EXCL_STOP

std::unique_ptr<LoopInclusionTree> analysis::createLoopInclusingTree(const FastAccessList<ControlFlowLoop>& loops)
{
    std::unique_ptr<LoopInclusionTree> inclusionTree(new LoopInclusionTree());
    for(auto& loop1 : loops)
    {
        for(auto& loop2 : loops)
        {
            if(loop1.includes(loop2))
            {
                auto& node1 = inclusionTree->getOrCreateNode(&loop1);
                auto& node2 = inclusionTree->getOrCreateNode(&loop2);
                if(!node1.isAdjacent(&node2))
                {
                    node1.addEdge(&node2, {});
                }
            }
        }
    }
    // Remove extra relations.
    for(auto& loop : loops)
    {
        auto& currentNode = inclusionTree->getOrCreateNode(&loop);

        // Remove parent nodes except node which has longest path to the root node.
        LoopInclusionTreeNodeBase* longestNode = nullptr;
        int longestLength = -1;
        currentNode.forAllIncomingEdges([&](LoopInclusionTreeNode& parent, LoopInclusionTreeEdge&) -> bool {
            auto parentNode = &parent;
            int length = static_cast<int>(parentNode->getLongestPathToRoot());

            if(length > longestLength)
            {
                longestNode = parentNode;
                longestLength = length;
            }

            return true;
        });

        if(longestNode)
        {
            std::vector<LoopInclusionTreeNode*> nonConnectedEdges;

            currentNode.forAllIncomingEdges([&](LoopInclusionTreeNode& other, LoopInclusionTreeEdge&) -> bool {
                auto otherNode = &other;
                if(longestNode != otherNode)
                {
                    // To avoid finishing loop
                    nonConnectedEdges.push_back(otherNode);
                }
                return true;
            });

            for(auto& otherNode : nonConnectedEdges)
            {
                otherNode->removeAsNeighbor(&currentNode);
            }
        }
    }

#ifdef DEBUG_MODE
    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        auto nameFunc = [](const ControlFlowLoop* loop) -> std::string {
            std::string result{};
            if(auto header = loop->getHeader())
                result = header->key->to_string();
            else
                result = "(unknown header)";
            result += " to ";
            if(auto tail = loop->getTail())
                result += tail->key->to_string();
            else
                result += "(unknown tail)";
            return result;
        };
        DebugGraph<const ControlFlowLoop*, LoopInclusion, Directionality::DIRECTED>::dumpGraph<LoopInclusionTree>(
            *inclusionTree, "/tmp/vc4c-loop-inclusion.dot", nameFunc);
    });
    LCOV_EXCL_STOP
#endif
    return inclusionTree;
}
