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
    auto op = setFlags.get<const intermediate::Operation>();
    if(op)
        std::tie(precalc, allFlags) = op->op(op->getFirstArg(), op->getSecondArg());
    else
    {
        precalc = setFlags->precalculate();
        if(precalc && precalc->getLiteralValue())
        {
            auto resultLit = precalc->getLiteralValue().value();
            ElementFlags flags;
            flags.zero = resultLit.unsignedInt() == 0 ? FlagStatus::SET : FlagStatus::CLEAR;
            flags.negative = resultLit.signedInt() < 0 ? FlagStatus::SET : FlagStatus::CLEAR;
            allFlags = flags;
        }
    }
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
