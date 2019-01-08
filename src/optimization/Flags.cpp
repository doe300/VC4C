/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Flags.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

static bool isFlagDefined(ConditionCode cond, const ElementFlags& flags)
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
    if(conditionalInstructions.empty())
    {
        // flags are set but never used
        if(setFlags->writesRegister(REG_NOP) && !setFlags->signal.hasSideEffects())
        {
            logging::debug() << "Removing unused setting of flags: " << setFlags->to_string() << logging::endl;
            setFlags.erase();
            return true;
        }
        logging::debug() << "Removing unused SetFlags bit from instruction: " << setFlags->to_string() << logging::endl;
        setFlags->setSetFlags(SetFlag::DONT_SET);
        return true;
    }
    Optional<Value> precalc;
    VectorFlags allFlags;
    std::tie(precalc, allFlags) = setFlags->precalculate();
    if(precalc && precalc->getLiteralValue() &&
        std::all_of(
            allFlags.begin(), allFlags.end(), [&](const ElementFlags& flags) -> bool { return flags == allFlags[0]; }))
    {
        // flags are compile-time static
        // TODO improve to also handle container constant flags
        ElementFlags flags = allFlags[0];
        auto condIt = conditionalInstructions.begin();
        bool changedInstructions = false;
        while(condIt != conditionalInstructions.end())
        {
            if(isFlagDefined((*condIt)->conditional, flags))
            {
                if(flags.matchesCondition((*condIt)->conditional))
                {
                    // condition is statically matched, remove conditional
                    logging::debug() << "Making instruction with constant condition unconditional: "
                                     << (*condIt)->to_string() << logging::endl;
                    (*condIt)->conditional = COND_ALWAYS;
                    changedInstructions = true;
                    ++condIt;
                }
                else
                {
                    // condition is statically not matched, remove instruction
                    logging::debug() << "Removing conditional instruction which will never be executed: "
                                     << (*condIt)->to_string() << logging::endl;
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
    if(it.get() == nullptr || it->setFlags != SetFlag::SET_FLAGS)
        return it;
    // only combine writing into NOP-register for now
    if(it->getOutput() && it->getOutput().value() != NOP_REGISTER)
        return it;
    // only remove this setting of flags, if we have no other side effects and don't execute conditionally
    if(it->signal.hasSideEffects() || it->conditional != COND_ALWAYS)
        return it;
    // only combine setting flags from moves (e.g. for PHI-nodes) for now
    if(it.get<intermediate::MoveOperation>() == nullptr)
        return it;
    const Value src = it.get<intermediate::MoveOperation>()->getSource();

    InstructionWalker checkIt = it.copy().previousInBlock();
    while(!checkIt.isStartOfBlock())
    {
        if(checkIt.get() && checkIt->setFlags == SetFlag::SET_FLAGS)
        {
            if(checkIt.get<intermediate::MoveOperation>() != nullptr &&
                checkIt.get<intermediate::MoveOperation>()->getSource() == src && checkIt->conditional == COND_ALWAYS)
            {
                logging::debug() << "Removing duplicate setting of same flags: " << it->to_string() << logging::endl;
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
    if(!it.has() || !it->doesSetFlag())
        // does not set flags
        return it;
    if(!it->writesRegister(REG_NOP))
        // already has output
        return it;
    if(!it.has<intermediate::MoveOperation>())
        // only combine setting flags from moves (e.g. for PHI-nodes) for now
        return it;
    const auto& in = it->assertArgument(0);
    if(it->signal.hasSideEffects() || it->hasConditionalExecution() ||
        (in.hasRegister() && in.reg().hasSideEffectsOnRead()))
        // only remove this setting of flags, if we have no other side effects and don't execute conditionally
        return it;
    const auto checkFunc = [&](InstructionWalker checkIt) -> bool {
        return checkIt.has() && !checkIt->doesSetFlag() && !checkIt->hasSideEffects() &&
            checkIt.has<intermediate::MoveOperation>() &&
            (checkIt->assertArgument(0) == in ||
                (in.getLiteralValue() && checkIt->assertArgument(0).hasLiteral(*in.getLiteralValue())));
    };

    // look into the future until we hit the instruction requiring the flag
    InstructionWalker resultIt = it.getBasicBlock()->walkEnd();
    auto checkIt = it.copy().nextInBlock();
    while(!checkIt.isEndOfBlock())
    {
        // this is usually only executed few times, since setting of flags and usage thereof are close to each other
        if(checkIt.has() && (checkIt->hasConditionalExecution() || checkIt->doesSetFlag()))
            break;
        if(checkFunc(checkIt))
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
                break;
            if(checkFunc(checkIt))
            {
                resultIt = checkIt;
                break;
            }
            checkIt.previousInBlock();
        }
    }
    if(!resultIt.isEndOfBlock())
    {
        logging::debug() << "Combining move to set flags '" << it->to_string()
                         << "' with move to output: " << resultIt->to_string() << logging::endl;
        resultIt->setSetFlags(SetFlag::SET_FLAGS);
        // TODO decorations? If input is the same, most decorations are also the same?! Also, setflags usually don't
        // have that many!?
        it.erase();
        // don't skip next instruction
        it.previousInBlock();
    }
    return it;
}
