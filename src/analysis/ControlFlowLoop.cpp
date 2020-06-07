/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "ControlFlowLoop.h"

#include "../intermediate/Helper.h"
#include "ControlFlowGraph.h"
#include "DataDependencyGraph.h"
#include "DebugGraph.h"
#include "PatternMatching.h"
#include "logger.h"

#include <limits>
#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

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

static SortedSet<Local*> findInductionCandidates(
    const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
{
    SortedSet<Local*> innerDependencies;
    SortedSet<Local*> outerDependencies;
    for(auto& node : loop)
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
                                if(std::find_if(loop.begin(), loop.end(), [&neighbor](const CFGNode* node) -> bool {
                                       return node->key == neighbor.key;
                                   }) != loop.end())
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

    SortedSet<Local*> intersection;
    // NOTE: Cannot pass unordered_set into set_intersection, since it requires its inputs (and output) to be sorted!
    std::set_intersection(innerDependencies.begin(), innerDependencies.end(), outerDependencies.begin(),
        outerDependencies.end(), std::inserter(intersection, intersection.begin()));

    return intersection;
}

static void addForwardCandidates(SortedSet<const intermediate::IntermediateInstruction*>& candidates,
    const intermediate::IntermediateInstruction& inst, const ControlFlowLoop& loop)
{
    // if the instruction is a phi-node, abort
    if(inst.hasDecoration(intermediate::InstructionDecorations::PHI_NODE) || inst.hasConditionalExecution())
        return;
    // if the instruction is a move, recursively check all readers of the instruction's result that are inside of the
    // loop
    if(auto move = dynamic_cast<const intermediate::MoveOperation*>(&inst))
    {
        if(!move->isSimpleMove())
            return;
        if(auto loc = move->getOutput()->checkLocal())
        {
            for(auto reader : loc->getUsers(LocalUse::Type::READER))
            {
                if(loop.findInLoop(reader))
                    addForwardCandidates(candidates, *reader, loop);
            }
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
            for(auto writer : loc->getUsers(LocalUse::Type::WRITER))
            {
                if(loop.findInLoop(writer))
                    addBackwardCandidates(candidates, *writer, loop);
            }
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
            for(auto reader : loc->getUsers(LocalUse::Type::READER))
            {
                if(loop.findInLoop(reader))
                    addForwardFlagCandidates(candidates, *reader, loop);
            }
        }
    }
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
    const CFGNode* tail = nullptr;
    // The repetition branch
    InstructionWalker tailBranch{};

    for(auto local : findInductionCandidates(*this, dependencyGraph))
    {
        if(local == nullptr)
            continue;

        const intermediate::IntermediateInstruction* initialAssignment = nullptr;
        // since there might be a few moves between the step expression and the assignment from/to the local, we walk
        // the moves from both ends into the "middle" and see if there is one instruction which is both reachable via
        // moves from the first read and the last write of the local inside the loop.
        SortedSet<const intermediate::IntermediateInstruction*> forwardStepCandidates;
        SortedSet<const intermediate::IntermediateInstruction*> backwardStepCandidates;

        for(const auto& pair : local->getUsers())
        {
            auto it = findInLoop(pair.first);

            if(pair.second.writesLocal() && pair.first->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
            {
                // check for the write inside or outside of the loop
                if(it)
                    // inside of loop - add to list of step function candidates
                    addBackwardCandidates(backwardStepCandidates, *pair.first, *this);
                else
                    // outside of loop - writes initial value
                    initialAssignment = pair.first;
            }
            if(pair.second.readsLocal() && it)
                // read inside of loop - walk all readers to assemble the list of step candiates
                addForwardCandidates(forwardStepCandidates, *pair.first, *this);
        }

        SortedSet<const intermediate::IntermediateInstruction*> stepCandidates{};
        // NOTE: Cannot pass unordered_set into set_intersection, since it requires its inputs (and output) to be
        // sorted!
        std::set_intersection(forwardStepCandidates.begin(), forwardStepCandidates.end(),
            backwardStepCandidates.begin(), backwardStepCandidates.end(),
            std::inserter(stepCandidates, stepCandidates.begin()));

        if(!initialAssignment || stepCandidates.empty())
            break;

        if(stepCandidates.size() > 1)
        {
            CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
                logging::debug() << "Induction variable has multiple step candidate instructions: "
                                 << local->to_string() << logging::endl;
                for(auto& candidate : stepCandidates)
                    logging::debug() << '\t' << candidate->to_string() << logging::endl;
            });
            break;
        }

        auto stepOperation = dynamic_cast<const intermediate::Operation*>(*stepCandidates.begin());
        if(!stepOperation)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Step instruction is not an operation: " << (*stepCandidates.begin())->to_string()
                    << logging::endl);
            break;
        }

        auto step = Expression::createExpression(*stepOperation);
        if(!step || !step->hasConstantOperand())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine induction step expression for: " << local->to_string() << logging::endl);
            break;
        }

        // we have an initial value as well as a single step expression
        // since the remaining values and instructions are not required for an induction variable, we try to find it out
        // after adding the variable to the result
        variables.emplace_back(InductionVariable{local, initialAssignment, stepOperation});

        if(!includeIterationInformation)
            // if we don't care for iteration variable information, we are done here
            break;

        auto successors = findSuccessors();
        if(!tail && (tail = getTail()))
            tailBranch = backEdge->data.getPredecessor(tail->key);
        if(!tail || tailBranch.isEndOfBlock())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine loop tail for: " << local->to_string() << logging::endl);
            break;
        }

        Local* repeatConditionLocal = nullptr;
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
                    break;
                }
                repeatConditionLocal = pair.first->local();
            }
            repeatCondition = branch->branchCondition.toConditionCode();
        }

        if(!repeatConditionLocal)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine repeat condition for: " << local->to_string() << logging::endl);
            break;
        }

        // NOTE: There may be two setting of repetition flags!
        // - One: the flags from the condition, resulting in a boolean value
        // - Second: the flags from consuming the boolean value to actually select the branch to take

        // simple case, there exists an instruction, where the output of the step is directly used as branch condition
        const intermediate::IntermediateInstruction* comparisonInstruction =
            stepOperation->writesLocal(repeatConditionLocal) ? stepOperation : nullptr;
        auto stepLocal = stepOperation->getOutput().value().local();
        // TODO support induction variable (not result of induction step) is used in comparison?? Possible at all, e.g.
        // for i++? Need to distinguish between the two cases
        if(!comparisonInstruction)
        {
            //"default" case, the iteration-variable is compared to something and the result of this comparison is
            // used to branch e.g. "- = xor <iteration-variable>, <upper-bound> (setf)"

            // find all setting of flags using the output of the induction step as input
            FastSet<const intermediate::IntermediateInstruction*> inductionFlags;
            stepLocal->forUsers(LocalUse::Type::READER,
                [&](const LocalUser* reader) { addForwardFlagCandidates(inductionFlags, *reader, *this); });

            if(inductionFlags.empty())
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Failed to determine repeat condition flag setters for: " << local->to_string()
                        << logging::endl);
                break;
            }

            // search the setting of flags which decides which boolean value is set for the branch condition
            // NOTE: There may be multiple setters for the repeat condition (e.g. one for true, one for false), but they
            // depend on the same flags, so we only care about one
            auto repeatCondSetter = tail->key->findWalkerForInstruction(
                *repeatConditionLocal->getUsers(LocalUse::Type::WRITER).begin(), tailBranch);
            if(repeatCondSetter)
            {
                if(auto repeatFlagSetter = tail->key->findLastSettingOfFlags(*repeatCondSetter))
                {
                    // if the setting of flags for the branch condition is in the list of setting of flags derived from
                    // the induction variable, we have a match
                    auto it = inductionFlags.find(repeatFlagSetter->get());
                    if(it != inductionFlags.end())
                    {
                        comparisonInstruction = *it;

                        // need to adapt the repeat condition here, since the first flags could be inverted to the
                        // second flags. We have:
                        // comparison (setf) -> boolean values (ifxx) -> branch condition (setf) -> branch(es) (ifxx)
                        //                                               ^ we can deduce from  ->   ^ we know from above
                        // We need to calculate the flags of the comparison that decide repeat/cancel!
                        if(repeatCondition != COND_ZERO_SET && repeatCondition != COND_ZERO_CLEAR)
                        {
                            // for now, we only handle the zero/not zero condition which is default for switching on
                            // boolean flags
                            CPPLOG_LAZY(logging::Level::DEBUG,
                                log << "Non-zero flags are not supported for repeat condition for: "
                                    << local->to_string() << logging::endl);
                            break;
                        }
                        // the flags (of the comparison) that causes the boolean value zero to be set
                        ConditionCode zeroSetCondition = COND_NEVER;
                        repeatConditionLocal->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
                            if(writer->precalculate().first == INT_ZERO)
                                zeroSetCondition =
                                    (check(dynamic_cast<const intermediate::ExtendedInstruction*>(writer)) &
                                        &intermediate::ExtendedInstruction::getCondition)
                                        .value_or(COND_ALWAYS);
                        });

                        /*
                         * Example:
                         * zeroSetCondition = ifnc, repeatCondition = ifzc
                         * -> !n -> 0               -> 0 -> cancel
                         * -> !n -> cancel -> n -> repeat
                         *
                         * zeroSetCondition = ifzs, repeatCondition = ifzs
                         * -> 0 -> 0                -> 0 -> repeat
                         * -> 0 -> repeat
                         */
                        repeatCondition =
                            repeatCondition == COND_ZERO_SET ? zeroSetCondition : zeroSetCondition.invert();
                    }
                }
            }
        }

        if(!comparisonInstruction || repeatCondition == COND_ALWAYS || repeatCondition == COND_NEVER)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Failed to determine comparison instruction or repeat condition for: " << local->to_string()
                    << logging::endl);
            break;
        }

        auto& inductionVar = variables.back();

        // unary "comparison" compares implicitly with zero
        if(comparisonInstruction->getArguments().size() == 1)
        {
            // e.g. for comparison with zero, it could just set the flags for the value and check for zero
            if(repeatCondition == COND_ZERO_CLEAR)
                inductionVar.repeatCondition = std::make_pair(intermediate::COMP_NEQ, INT_ZERO);
            else if(repeatCondition == COND_ZERO_SET)
                inductionVar.repeatCondition = std::make_pair(intermediate::COMP_EQ, INT_ZERO);
            if(repeatCondition == COND_NEGATIVE_CLEAR)
                inductionVar.repeatCondition = std::make_pair(intermediate::COMP_SIGNED_GE, INT_ZERO);
            if(repeatCondition == COND_NEGATIVE_SET)
                inductionVar.repeatCondition = std::make_pair(intermediate::COMP_SIGNED_LT, INT_ZERO);
            // we are done here if this is a simplified comparison to zero
            break;
        }

        // check for binary comparison with direct constant
        Optional<Value> comparisonValue{};
        int comparisonArgumentIndex = -1;
        if(comparisonInstruction->getArguments().size() > 1)
        {
            if(auto arg0 = comparisonInstruction->getArgument(0))
            {
                if(arg0->getLiteralValue())
                    comparisonValue = *arg0;
                else if(auto writer = arg0->getSingleWriter())
                {
                    if(auto precalc = writer->precalculate().first)
                        comparisonValue = precalc;
                    else if(writer->getArguments().size() == 1 && writer->readsRegister(REG_UNIFORM))
                        comparisonValue = *arg0;
                }
                if(comparisonValue)
                    comparisonArgumentIndex = 0;
            }
            if(!comparisonValue)
            {
                if(auto arg1 = comparisonInstruction->getArgument(1))
                {
                    if(arg1->getLiteralValue())
                        comparisonValue = *arg1;
                    else if(auto writer = arg1->getSingleWriter())
                    {
                        if(auto precalc = writer->precalculate().first)
                            comparisonValue = precalc;
                        else if(writer->getArguments().size() == 1 && writer->readsRegister(REG_UNIFORM))
                            comparisonValue = *arg1;
                    }
                    if(comparisonValue)
                        comparisonArgumentIndex = 1;
                }
            }
        }
        if(comparisonValue && comparisonArgumentIndex >= 0)
        {
            if(auto op = dynamic_cast<const intermediate::Operation*>(comparisonInstruction))
            {
                if(op->op == OP_XOR && (repeatCondition == COND_ZERO_CLEAR || repeatCondition == COND_ZERO_SET))
                    // xor induction, constant -> equality comparison
                    inductionVar.repeatCondition = std::make_pair(
                        repeatCondition == COND_ZERO_SET ? intermediate::COMP_EQ : intermediate::COMP_NEQ,
                        *comparisonValue);
                else if(has_flag(op->op.flagBehavior, FlagBehavior::CARRY_FIRST_GREATER_SECOND) &&
                    (repeatCondition == COND_CARRY_CLEAR || repeatCondition == COND_CARRY_SET))
                {
                    bool isFloat = op->op.acceptsFloat;
                    // max induction, constant -> less/greater-than/equals comparison
                    // (f)min/max all set the carry flag if the first argument is greater than the second!
                    const char* comparison = nullptr;
                    if(comparisonArgumentIndex == 0)
                    {
                        if(repeatCondition == COND_CARRY_SET)
                            // repeat on "constant is greater than induction variable" -> induction < constant
                            comparison = isFloat ? intermediate::COMP_ORDERED_LT : intermediate::COMP_SIGNED_LT;
                        else if(repeatCondition == COND_CARRY_CLEAR)
                            // repeat on "constant is not greater than induction variable" -> induction >= constant
                            comparison = isFloat ? intermediate::COMP_ORDERED_GE : intermediate::COMP_SIGNED_GE;
                    }
                    else if(comparisonArgumentIndex == 1)
                    {
                        if(repeatCondition == COND_CARRY_SET)
                            // repeat on "induction variable is greater than constant" -> induction > constant
                            comparison = isFloat ? intermediate::COMP_ORDERED_GT : intermediate::COMP_SIGNED_GT;
                        else if(repeatCondition == COND_CARRY_CLEAR)
                            // repeat on "induction variable is not greater than constant" -> induction <= constant
                            comparison = isFloat ? intermediate::COMP_ORDERED_LE : intermediate::COMP_SIGNED_LE;
                    }
                    if(comparison)
                        inductionVar.repeatCondition = std::make_pair(comparison, *comparisonValue);
                }
                // TODO handle some more options/handle generally? Also combine with more complex version below!
            }
            // we are done here if this is a simplified comparison to a constant/parameter
            break;
        }

        // The comparison is usually more than 1 instruction, so handle this below
        const auto stepValue = stepLocal->createReference();
        if(auto otherArg = comparisonInstruction->findOtherArgument(stepValue))
        {
            // one of the inputs of the comparison instruction is the step local (the induction variable with the next
            // step applied)

            using namespace pattern;

            Literal constant = UNDEFINED_LITERAL;
            Value constantValue = UNDEFINED_VALUE;

            // Check for unsigned comparison < 2^x
            Pattern andConstantPattern = {{

                /* i32 %imm = loadi i32 1023 (group_uniform) */
                capture(constantValue) = (match(FAKEOP_LDI), capture(constant)),
                /* bool %icomp.38 = and i32 %inc, i32 %imm */
                match(*otherArg) = (match(OP_AND), match(stepValue), capture(constantValue)),
                /* - = xor bool %icomp.38, i32 %inc (setf) */
                anyValue() = (match(OP_XOR), match(*otherArg), match(stepValue), match(SetFlag::SET_FLAGS))}};

            if((repeatCondition == COND_ZERO_SET || repeatCondition == COND_ZERO_CLEAR) &&
                !pattern::search(tail->key->walk(), andConstantPattern).isEndOfBlock() && !constant.isUndefined() &&
                isPowerTwo(constant.unsignedInt() + 1u))
            {
                // XXX could be improved:
                // does not match e.g. %constant = mov 15 (15) or inverted arguments for and/xor
                // also does not match if the constant is a direct constant in the and ... instruction

                // otherOp is an AND with a constant 2^x - 1
                // (%step & (2^x - 1)) ^ %step -> 0 for %step < 2^x, !0 for %step >= 2^x
                // %step < 2^x <=> %step <= (2^x - 1), %step >= 2^x <=> %step > (2^x -1)
                auto comparison =
                    repeatCondition == COND_ZERO_SET ? intermediate::COMP_UNSIGNED_LE : intermediate::COMP_UNSIGNED_GT;
                inductionVar.repeatCondition = std::make_pair(comparison, Value(constant, constantValue.type));
                // we are done for this case
                break;
            }

            ConditionCode minCond = COND_NEVER;

            // Check for general unsigned comparison
            Pattern minMaxConstantPattern = {{

                /* register - = xor i32 %inc, i32 %imm (setf ) */
                anyValue() = (match(OP_XOR), match(stepValue), capture(constantValue), match(SetFlag::SET_FLAGS)),
                /* i32 %icomp.53 = min i32 %inc, i32 %imm (ifn ) */
                capture(pattern::V1) = (match(OP_MIN), match(stepValue), capture(constantValue), capture(minCond)),
                /* i32 %icomp.53 = max i32 %inc, i32 %imm (ifnc ) */
                capture(pattern::V1) =
                    (match(OP_MAX), match(stepValue), capture(constantValue), captureInverse(minCond)),
                /* - = xor i32 %icomp.53, i32 %inc (setf ) */
                anyValue() = (match(OP_XOR), capture(pattern::V1), match(stepValue), match(SetFlag::SET_FLAGS))}};

            if((repeatCondition == COND_ZERO_SET || repeatCondition == COND_ZERO_CLEAR) &&
                !search(tail->key->walk(), minMaxConstantPattern).isEndOfBlock() && minCond != COND_NEVER &&
                constantValue.getSingleWriter())
            {
                // XXX could improve:
                // does not work for step being first argument on min/max, e.g. for gt comparison
                if(auto realConstant = constantValue.getSingleWriter()->precalculate(1).first)
                {
                    const char* comparison = nullptr;
                    // %step ^ min(%x, %step) -> 0 for %step <= %x <- special either MSB set case
                    // %step ^ max(%x, %step) -> 0 for %step >= %x <- default case
                    // %step ^ max(%x, %step) -> !0 for %step < %x
                    if(minCond == COND_NEGATIVE_SET)
                        comparison = repeatCondition == COND_ZERO_CLEAR ? intermediate::COMP_UNSIGNED_LT :
                                                                          intermediate::COMP_UNSIGNED_GE;

                    if(comparison)
                    {
                        inductionVar.repeatCondition = std::make_pair(comparison, *realConstant);
                        // we are done for this case
                        break;
                    }
                }
            }
        }

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to determine repeat condition for: " << local->to_string() << logging::endl);
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
    // (see above) is considered invaraint
    auto checkArgInvariant = [&](const Value& arg) -> bool {
        if(arg.getLiteralValue())
            return true;
        if(arg.hasRegister(REG_ELEMENT_NUMBER) || arg.hasRegister(REG_QPU_NUMBER))
            return true;
        if(auto local = arg.checkLocal())
        {
            auto writers = local->getUsers(LocalUse::Type::WRITER);
            return std::all_of(writers.begin(), writers.end(), writerIsInvariant);
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
