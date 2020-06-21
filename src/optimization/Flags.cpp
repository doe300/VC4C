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
                if(flags.matchesCondition(cond))
                {
                    // condition is statically matched, remove conditional
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Making instruction with constant condition unconditional: " << (*condIt)->to_string()
                            << logging::endl);
                    if(extendedInst)
                        extendedInst->setCondition(COND_ALWAYS);
                    else if(branch)
                        branch->branchCondition = BRANCH_ALWAYS;
                    changedInstructions = true;
                    ++condIt;
                }
                else if(branch)
                    // XXX for now don't rewrite any flags where branches depend on them
                    ++condIt;
                else if((*condIt)->hasDecoration(intermediate::InstructionDecorations::PHI_NODE))
                {
                    // We can't remove phi-node writes, since otherwise we remove the end of the liveness range (for
                    // that block), making it possibly "infinite". But we can make this instruction not depend on
                    // anything else to be able to remove its sources
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Simplifying conditional phi-node which will never be executed: "
                            << (*condIt)->to_string() << logging::endl);
                    condIt->reset((new intermediate::MoveOperation((*condIt)->getOutput().value(), INT_ZERO))
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
        return checkIt.has() && !checkIt->doesSetFlag() && !checkIt->hasSideEffects() &&
            checkIt.get<intermediate::MoveOperation>() &&
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
