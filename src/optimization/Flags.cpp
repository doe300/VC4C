/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Flags.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../analysis/FlagsAnalysis.h"
#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

static bool isFlagDefined(ConditionCode cond, ElementFlags flags)
{
    if(cond == COND_ALWAYS || cond == COND_NEVER)
        return true;
    if(cond == COND_ZERO_CLEAR || cond == COND_ZERO_SET)
        return flags.zero != FlagStatus::UNDEFINED;
    if(cond == COND_NEGATIVE_CLEAR || cond == COND_NEGATIVE_SET)
        return flags.negative != FlagStatus::UNDEFINED;
    if(cond == COND_CARRY_CLEAR || cond == COND_CARRY_SET)
        return flags.carry != FlagStatus::UNDEFINED;
    throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled condition code", cond.to_string());
}

static bool rewriteSettingOfFlags(
    InstructionWalker setFlags, FastAccessList<InstructionWalker>&& conditionalInstructions)
{
    if(conditionalInstructions.empty() && setFlags.get<intermediate::ExtendedInstruction>())
    {
        // flags are set but never used
        if(setFlags->writesRegister(REG_NOP) && !setFlags->getSignal().hasSideEffects())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Removing unused setting of flags: " << setFlags->to_string() << logging::endl);
            setFlags.erase();
            return true;
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Removing unused SetFlags bit from instruction: " << setFlags->to_string() << logging::endl);
        setFlags.get<intermediate::ExtendedInstruction>()->setSetFlags(SetFlag::DONT_SET);
        return true;
    }
    auto allFlags = analysis::StaticFlagsAnalysis::analyzeStaticFlags(setFlags.get(), setFlags);
    if(allFlags && std::all_of(allFlags->begin(), allFlags->end(), [&](ElementFlags flags) -> bool {
           return flags == (*allFlags)[0];
       }))
    {
        // flags are compile-time static
        // TODO improve to also handle container constant flags
        ElementFlags flags = (*allFlags)[0];
        auto condIt = conditionalInstructions.begin();
        bool changedInstructions = false;
        while(condIt != conditionalInstructions.end())
        {
            auto extendedInst = condIt->get<intermediate::ExtendedInstruction>();
            auto branch = condIt->get<intermediate::Branch>();
            ConditionCode cond = COND_ALWAYS;
            if(extendedInst)
                cond = extendedInst->getCondition();
            else if(branch)
                cond = branch->branchCondition.toConditionCode();
            if(isFlagDefined(cond, flags))
            {
                if(branch && allFlags->matchesCondition(branch->branchCondition))
                {
                    // this branch becomes unconditional
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Making branch with constant condition unconditional: " << (*condIt)->to_string()
                            << logging::endl);
                    branch->branchCondition = BRANCH_ALWAYS;
                    changedInstructions = true;
                    ++condIt;
                    // TODO now we could also remove the phi-node writes
                }
                else if(flags.matchesCondition(cond))
                {
                    // condition is statically matched, remove conditional
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Making instruction with constant condition unconditional: " << (*condIt)->to_string()
                            << logging::endl);
                    if(extendedInst)
                        extendedInst->setCondition(COND_ALWAYS);
                    changedInstructions = true;
                    ++condIt;
                }
                else if((*condIt)->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                {
                    // We can't remove phi-node writes, since otherwise we remove the end of the liveness range (for
                    // that block), making it possibly "infinite". But we can make this instruction not depend on
                    // anything else to be able to remove its sources
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Simplifying conditional phi-node which will never be executed: "
                            << (*condIt)->to_string() << logging::endl);
                    condIt->reset((new intermediate::MoveOperation((*condIt)->getOutput().value(), UNDEFINED_VALUE))
                                      ->copyExtrasFrom((*condIt).get()));
                    ++condIt;
                }
                else
                {
                    // condition is statically not matched, remove instruction
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing conditional instruction which will never be executed: "
                            << (*condIt)->to_string() << logging::endl);
                    condIt->erase();
                    condIt = conditionalInstructions.erase(condIt);
                    changedInstructions = true;
                }
            }
            else
                ++condIt;
        }
        if(conditionalInstructions.empty())
            // to maybe remove the flag
            rewriteSettingOfFlags(setFlags, FastAccessList<InstructionWalker>{});
        return changedInstructions;
    }
    return false;
}

bool optimizations::removeUselessFlags(const Module& module, Method& method, const Configuration& config)
{
    bool changedSomething = false;
    for(auto& block : method)
    {
        Optional<InstructionWalker> lastSettingOfFlags;
        FastAccessList<InstructionWalker> conditionalInstructions;

        auto it = block.walk();
        while(it != block.walkEnd())
        {
            if(!it.has())
            {
                it.nextInBlock();
                continue;
            }
            if(it->hasConditionalExecution())
                conditionalInstructions.push_back(it);
            if(it.get<intermediate::Branch>() && !it.get<intermediate::Branch>()->isUnconditional())
                conditionalInstructions.push_back(it);
            if(it->doesSetFlag())
            {
                if(lastSettingOfFlags)
                {
                    // process previous setting of flags
                    FastAccessList<InstructionWalker> tmp;
                    std::swap(tmp, conditionalInstructions);
                    if(rewriteSettingOfFlags(*lastSettingOfFlags, std::move(tmp)))
                        changedSomething = true;
                }
                lastSettingOfFlags = it;
            }
            it.nextInBlock();
        }

        if(lastSettingOfFlags)
        {
            // process previous setting of flags
            if(rewriteSettingOfFlags(*lastSettingOfFlags, std::move(conditionalInstructions)))
                changedSomething = true;
        }
    }
    return changedSomething;
}

InstructionWalker optimizations::combineSameFlags(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(it.get() == nullptr || !it->doesSetFlag())
        return it;
    // only combine writing into NOP-register for now
    if(it->getOutput() && it->getOutput().value() != NOP_REGISTER)
        return it;
    // only remove this setting of flags, if we have no other side effects and don't execute conditionally
    if(it->getSignal().hasSideEffects() || it->hasConditionalExecution())
        return it;
    // only combine setting flags from moves (e.g. for PHI-nodes) for now
    if(it.get<intermediate::MoveOperation>() == nullptr)
        return it;
    const Value src = it.get<intermediate::MoveOperation>()->getSource();

    InstructionWalker checkIt = it.copy().previousInBlock();
    while(!checkIt.isStartOfBlock())
    {
        if(checkIt.get() && checkIt->doesSetFlag())
        {
            if(checkIt.get<intermediate::MoveOperation>() != nullptr &&
                checkIt.get<intermediate::MoveOperation>()->getSource() == src && !checkIt->hasConditionalExecution())
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Removing duplicate setting of same flags: " << it->to_string() << logging::endl);
                it.erase();
                // don't skip next instruction
                it.previousInBlock();
            }
            // otherwise some other flags are set -> cancel
            return it;
        }
        checkIt.previousInBlock();
    }

    return it;
}

InstructionWalker optimizations::combineFlagWithOutput(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // XXX this currently never moves the value-moves to the flag-moves, only the flag-moves to the value-moves
    // -> does not increase local liveness, but on the other side, we could save instructions...
    if(!it.has() || !it->doesSetFlag())
        // does not set flags
        return it;
    if(!it->writesRegister(REG_NOP))
        // already has output
        return it;
    if(!it.get<intermediate::MoveOperation>())
        // only combine setting flags from moves (e.g. for PHI-nodes) for now
        return it;
    const auto& in = it->assertArgument(0);
    if(it->getSignal().hasSideEffects() || it->hasConditionalExecution() ||
        (in.checkRegister() && in.reg().hasSideEffectsOnRead()))
        // only remove this setting of flags, if we have no other side effects and don't execute conditionally
        return it;
    const auto movesFromSameInput = [&](InstructionWalker checkIt) -> bool {
        return checkIt.has() &&
            (check(checkIt.get<intermediate::MoveOperation>()) & &intermediate::MoveOperation::isSimpleMove) &&
            (checkIt->assertArgument(0) == in ||
                (in.getLiteralValue() && checkIt->assertArgument(0).hasLiteral(*in.getLiteralValue())));
    };

    // look into the future until we hit the instruction requiring the flag
    InstructionWalker resultIt = it.getBasicBlock()->walkEnd();
    auto checkIt = it.copy().nextInBlock();
    while(!checkIt.isEndOfBlock())
    {
        // this is usually only executed few times, since setting of flags and usage thereof are close to each other
        if(checkIt.has() &&
            (checkIt->hasConditionalExecution() || checkIt->doesSetFlag() || checkIt->getOutput() == in))
            /*
             * Abort, since we cannot move a setting of flags over:
             * - a conditional instruction (depending on this flags)
             * - the next setting of flags
             * - an instruction writing the flag source
             */
            break;
        if(movesFromSameInput(checkIt))
        {
            resultIt = checkIt;
            break;
        }
        checkIt.nextInBlock();
    }

    // look back only a few instructions
    if(resultIt.isEndOfBlock())
    {
        checkIt = it.copy().previousInBlock();
        while(!checkIt.isStartOfBlock())
        {
            // TODO limit number of steps?

            if(checkIt.has() &&
                (checkIt->hasConditionalExecution() || checkIt->doesSetFlag() || checkIt->getOutput() == in))
                /*
                 * Abort, since we cannot move a setting of flags over:
                 * - a conditional instruction (depending on the previous flags)
                 * - the previous setting of flags
                 * - an instruction writing the flag source
                 */
                break;
            if(movesFromSameInput(checkIt))
            {
                resultIt = checkIt;
                break;
            }
            checkIt.previousInBlock();
        }
    }
    if(!resultIt.isEndOfBlock())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Combining move to set flags '" << it->to_string()
                << "' with move to output: " << resultIt->to_string() << logging::endl);
        resultIt.get<intermediate::ExtendedInstruction>()->setSetFlags(SetFlag::SET_FLAGS);
        // TODO decorations? If input is the same, most decorations are also the same?! Also, setflags usually don't
        // have that many!?
        it.erase();
        // don't skip next instruction
        it.previousInBlock();
    }
    return it;
}

static bool checkAllResultElementsAreSame(
    const intermediate::IntermediateInstruction& inst, Optional<InstructionWalker>);

static bool checkAllInputElementsAreSame(const Value& input, InstructionWalker it)
{
    bool allElementsSame = input.isAllSame();
    if(!allElementsSame && input.checkLocal())
    {
        allElementsSame = input.local()->allUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) -> bool {
            return checkAllResultElementsAreSame(*writer, it.getBasicBlock()->findWalkerForInstruction(writer, it));
        });
    }
    return allElementsSame;
}

static bool checkAllResultElementsAreSame(
    const intermediate::IntermediateInstruction& inst, Optional<InstructionWalker> it)
{
    if(inst.hasDecoration(intermediate::InstructionDecorations::IDENTICAL_ELEMENTS))
        return true;
    if(inst.hasConditionalExecution())
    {
        auto flagIt = it ? it->getBasicBlock()->findLastSettingOfFlags(*it) : Optional<InstructionWalker>{};
        if(flagIt && !flagIt->isEndOfBlock() && checkAllResultElementsAreSame(*(*flagIt).get(), *flagIt))
            // If we can argue that the (required) flags are set the same for all elements, then the conditional
            // instruction sets the same value for all elements.
            return true;
        // Otherwise, we need to exclude this, since conditional writes would be per-element
        return false;
    }
    if(inst.precalculate().first & &Value::isAllSame)
        // constant value which is the same in all SIMD elements
        return true;
    if(inst.checkOutputLocal() && inst.readsLocal(inst.checkOutputLocal()))
    {
        if(dynamic_cast<const intermediate::Operation*>(&inst) &&
            inst.findOtherArgument(*inst.getOutput()) & &Value::isAllSame)
            // Assuming the written (and read) variable is all the same across the SIMD vector (which is decided by
            // the other writers that have to exist), and we only do (any kind of) operation taking another value
            // which is the same across all SIMD elements, then we know the result to also have this property, since
            // all (unconditional) operations are independent of the SIMD element. This is e.g. true for iteration
            // variables.
            return true;

        // for any other instruction reading its output abort, since we might get to an infinite stack recursion below
        // when we recursively recheck for the operation inputs
        return false;
    }

    if(it && std::all_of(inst.getArguments().begin(), inst.getArguments().end(), [&](const Value& arg) -> bool {
           return checkAllInputElementsAreSame(arg, *it);
       }))
        // TODO do we need to exclude some operation types? Or heed some extra option/flags?
        return true;
    return false;
}

static bool checkConditionalsAcceptFlagRewrite(InstructionWalker it, ElementFlags conditionFlags)
{
    it.nextInBlock();
    while(!it.isEndOfBlock() && !it->doesSetFlag())
    {
        if(it->hasConditionalExecution())
            // normal conditionals are per-element, so we cannot rewrite their flag setters
            return false;
        if(auto br = it.get<intermediate::Branch>())
        {
            if(br->branchCondition == BRANCH_ALL_C_CLEAR || br->branchCondition == BRANCH_ANY_C_SET)
            {
                if(conditionFlags.carry != FlagStatus::SET)
                    return false;
            }
            else if(br->branchCondition == BRANCH_ALL_N_CLEAR || br->branchCondition == BRANCH_ANY_N_SET)
            {
                if(conditionFlags.negative != FlagStatus::SET)
                    return false;
            }
            else if(br->branchCondition == BRANCH_ALL_Z_CLEAR || br->branchCondition == BRANCH_ANY_Z_SET)
            {
                if(conditionFlags.zero != FlagStatus::SET)
                    return false;
            }
            else
                // any other branch condition will be changed by simplifying the flag setter
                return br->branchCondition == BRANCH_ALWAYS;
        }
        it.nextInBlock();
    }
    return true;
}

InstructionWalker optimizations::simplifyFlag(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(!it.get() || it->hasConditionalExecution() || !it->doesSetFlag())
        return it;

    if(it.get<intermediate::MoveOperation>())
        // is already the simplest case
        return it;

    if(!it->writesRegister(REG_NOP))
        // we can only simplify if the output value is not actually used, since we change it
        return it;

    // check whether we have a supported operation
    Optional<Value> input = UNDEFINED_VALUE;
    // these are the allowed flags to depend on which will still be identical after the simplification
    ElementFlags condFlags{};
    if(auto op = it.get<intermediate::Operation>())
    {
        // this is currently our only supported case
        if(op->op == OP_OR && op->readsRegister(REG_ELEMENT_NUMBER))
        {
            input = op->findOtherArgument(ELEMENT_NUMBER_REGISTER);
            // for this case, the can simplify if all dependent checks on (not)zero or (not)negative
            condFlags.zero = FlagStatus::SET;
            condFlags.negative = FlagStatus::SET;
        }
    }
    if(!input)
        // unsupported case of flag setter
        return it;

    // check whether all our inputs are identical over the whole SIMD vector
    if(!checkAllInputElementsAreSame(*input, it))
        return it;

    // check all depending conditional executions, whether they allow us to change the flags
    if(!checkConditionalsAcceptFlagRewrite(it, condFlags))
        return it;

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Simplifying flag setter '" << it->to_string()
            << "', all conditional instructions will behave the same afterwards" << logging::endl);
    it.reset((new intermediate::MoveOperation(it->getOutput().value(), *input, COND_ALWAYS, SetFlag::SET_FLAGS))
                 ->copyExtrasFrom(it.get()));
    return it;
}

// Returns (if matching) the condition for the boolean true value
static Optional<ConditionCode> getConditionalBooleanWrites(const Value& val, InstructionWalker it)
{
    // check source local
    auto loc = val.checkLocal();
    if(!loc)
        return {};
    // check local written exactly twice
    auto writers = loc->getUsers(LocalUse::Type::WRITER);
    if(writers.size() != 2)
        return {};
    // check local writes are within the same block
    std::vector<InstructionWalker> writerWalkers;
    for(auto writer : writers)
    {
        if(auto writerIt = it.getBasicBlock()->findWalkerForInstruction(writer, it))
            writerWalkers.emplace_back(*writerIt);
        else
            return {};
    }
    // check writes are conditional with inverted conditions
    if(!writerWalkers.front()->hasConditionalExecution() || !writerWalkers.back()->hasConditionalExecution())
        return {};
    if(!writerWalkers.front().get<intermediate::ExtendedInstruction>()->getCondition().isInversionOf(
           writerWalkers.back().get<intermediate::ExtendedInstruction>()->getCondition()))
        return {};
    // check writes are conditional on the same setting of flags
    auto firstFlagIt = it.getBasicBlock()->findLastSettingOfFlags(writerWalkers.front());
    auto secondFlagIt = it.getBasicBlock()->findLastSettingOfFlags(writerWalkers.back());
    if(!firstFlagIt || firstFlagIt != secondFlagIt)
        return {};
    // check the flags setter for the conditional value is also the last flag setter in the block as seen from the
    // current flag setter
    auto selfFlagIt = it.getBasicBlock()->findLastSettingOfFlags(it);
    if(!selfFlagIt || *selfFlagIt != *firstFlagIt || *selfFlagIt != *secondFlagIt)
        return {};
    // check written values are boolean true or false
    auto firstValue = writerWalkers.front()->precalculate().first;
    auto secondValue = writerWalkers.back()->precalculate().first;
    if(!firstValue || !secondValue)
        return {};
    if(firstValue == BOOL_TRUE && secondValue == BOOL_FALSE)
        return writerWalkers.front().get<intermediate::ExtendedInstruction>()->getCondition();
    if(firstValue == BOOL_FALSE && secondValue == BOOL_TRUE)
        return writerWalkers.back().get<intermediate::ExtendedInstruction>()->getCondition();
    return {};
}

static bool checkAllConditionalsMatchingCondition(
    InstructionWalker pos, ConditionCode code, FastAccessList<InstructionWalker>& conditionalInstructions)
{
    pos.nextInBlock();
    while(!pos.isEndOfBlock())
    {
        if(pos.get())
        {
            if(pos->doesSetFlag())
                // next setting of flags, end
                return !pos->hasConditionalExecution();

            auto condCode = code;
            if(pos->hasConditionalExecution())
            {
                condCode = pos.get<intermediate::ExtendedInstruction>()->getCondition();
                conditionalInstructions.emplace_back(pos);
            }
            else if(auto br = pos.get<intermediate::Branch>())
            {
                condCode = br->branchCondition.toConditionCode();
                conditionalInstructions.emplace_back(pos);
            }
            if(condCode != code && !condCode.isInversionOf(code))
                // conditional instruction depending on different flags
                return false;
        }
        pos.nextInBlock();
    }
    return true;
}

bool optimizations::removeConditionalFlags(const Module& module, Method& method, const Configuration& config)
{
    bool rewroteFlags = false;
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            auto move = it.get<intermediate::MoveOperation>();
            FastAccessList<InstructionWalker> conditionalInstructions;
            if(move && move->doesSetFlag() && move->writesRegister(REG_NOP) &&
                checkAllConditionalsMatchingCondition(it, COND_ZERO_SET, conditionalInstructions))
            {
                // very simple case, move of some variable to to NOP to set zero/non-zero flags
                if(auto cond = getConditionalBooleanWrites(move->getSource(), it))
                {
                    // move sets non-zero, iff cond is set, otherwise it sets zero
                    // we can replace all checks for zero/non-zero with the cond (and its inversion) and remove this
                    // flag setter
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing superfluous conditional setting of flags by making all dependent conditional "
                               "instruction dependent on the original flags instead: "
                            << move->to_string() << logging::endl);
                    for(auto condIt : conditionalInstructions)
                    {
                        if(auto inst = condIt.get<intermediate::ExtendedInstruction>())
                            inst->setCondition(inst->getCondition() == COND_ZERO_CLEAR ? *cond : cond->invert());
                        else if(auto br = condIt.get<intermediate::Branch>())
                        {
                            // at least after running some of the optimizations above, this can happen
                            if(br->branchCondition == BRANCH_ALL_Z_CLEAR)
                                // TODO simplify this, so ALL_SET and ALL_CLEAR are both created from SET/CLEAR and true
                                // flag?
                                br->branchCondition = cond->toBranchCondition(true);
                            else if(br->branchCondition == BRANCH_ALL_Z_SET)
                                br->branchCondition = cond->invert().toBranchCondition(true);
                            else if(br->branchCondition == BRANCH_ANY_Z_CLEAR)
                                br->branchCondition = cond->toBranchCondition(false);
                            else if(br->branchCondition == BRANCH_ANY_Z_SET)
                                br->branchCondition = cond->invert().toBranchCondition(false);
                        }
                        else
                            throw CompilationError(CompilationStep::OPTIMIZER,
                                "Unhandled type of conditional instruction", condIt->to_string());
                    }
                    rewroteFlags = true;
                    it.erase();
                    // to not skip the next instruction
                    it.previousInBlock();
                }
            }
            it.nextInBlock();
        }
    }

    return rewroteFlags;
}
