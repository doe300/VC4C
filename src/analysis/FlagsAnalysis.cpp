/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "FlagsAnalysis.h"

#include "../intermediate/Helper.h"

#include <climits>
#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

StaticFlagsAnalysis::StaticFlagsAnalysis() :
    LocalAnalysis(StaticFlagsAnalysis::analyzeStaticFlagsWrapper, StaticFlagsAnalysis::to_string)
{
}

static Optional<VectorFlags> getConditionalFlags(const Local* loc, InstructionWalker it)
{
    // multiple writers, check for boolean (0 or 1, depending on other flag) writing
    auto writers = loc->getUsers(LocalUse::Type::WRITER);
    if(writers.size() != 2 || it.isEndOfBlock())
        // only handle the very simple (and most common) case for now, where the boolean value is written by
        // either "true" or "false" depending on a single other flag setter
        return {};
    FastSet<Value> sources(2);
    FastAccessList<InstructionWalker> sourceFlagsSetter;
    FastAccessList<ConditionCode> sourceConditions;
    for(auto writer : writers)
    {
        if(auto writerIt = it.getBasicBlock()->findWalkerForInstruction(writer, it))
        {
            if(writer->hasConditionalExecution())
                sourceConditions.emplace_back(
                    dynamic_cast<const intermediate::ExtendedInstruction*>(writer)->getCondition());
            if(auto writerVal = writer->precalculate().first)
                sources.emplace(*writerVal);
            if(auto flagsSetIt = it.getBasicBlock()->findLastSettingOfFlags(*writerIt))
                sourceFlagsSetter.emplace_back((*flagsSetIt));
        }
    }
    if(sources == FastSet<Value>{BOOL_FALSE, BOOL_TRUE} && sourceConditions.size() == 2 &&
        sourceConditions.front().isInversionOf(sourceConditions.back()) && sourceFlagsSetter.size() == 2 &&
        sourceFlagsSetter.front() == sourceFlagsSetter.back())
    {
        // we found our boolean writing depending on some other flag
        // => we know the original flags to only set zeroes and ones
        // TODO We could also follow other flags more to see if scalar/vector, etc. Then we would need to check
        // the boolean value set conditions (e.g. on zero set/clear, negative set/clear, etc.)

        ElementFlags allElements{};
        // is determined dynamically by boolean value
        allElements.zero = FlagStatus::UNDEFINED;
        // can never be set for boolean values
        allElements.negative = allElements.carry = allElements.overflow = FlagStatus::CLEAR;
        return VectorFlags{allElements};
    }

    return {};
}

static InstructionWalker getInstructionWalker(const intermediate::IntermediateInstruction* inst, InstructionWalker it)
{
    if(it.getBasicBlock())
        return it.getBasicBlock()->findWalkerForInstruction(inst, it).value_or(InstructionWalker{});
    return it;
}

static std::pair<Optional<VectorFlags>, Optional<VectorFlags>> getInnerFlags(
    const intermediate::IntermediateInstruction& inst, InstructionWalker it)
{
    Optional<VectorFlags> firstFlags{};
    if(auto loc = inst.getArgument(0) & &Value::checkLocal)
    {
        if(auto writer = loc->getSingleWriter())
            firstFlags = StaticFlagsAnalysis::analyzeStaticFlags(writer, getInstructionWalker(writer, it), true);
        else
            firstFlags = getConditionalFlags(loc, it);
    }
    else if(auto arg = inst.getArgument(0))
        firstFlags = VectorFlags::fromValue(*arg);
    Optional<VectorFlags> secondFlags{};
    if(auto loc = inst.getArgument(1) & &Value::checkLocal)
    {
        if(auto writer = loc->getSingleWriter())
            secondFlags = StaticFlagsAnalysis::analyzeStaticFlags(writer, getInstructionWalker(writer, it), true);
        else
            secondFlags = getConditionalFlags(loc, it);
    }
    else if(auto arg = inst.getArgument(1))
        secondFlags = VectorFlags::fromValue(*arg);
    return std::make_pair(firstFlags, secondFlags);
}

Optional<VectorFlags> StaticFlagsAnalysis::analyzeStaticFlags(
    const intermediate::IntermediateInstruction* inst, InstructionWalker it, bool allowNoFlagsSetter)
{
    if(!inst)
        return {};
    if(!allowNoFlagsSetter && !inst->doesSetFlag())
        return {};

    // default handling of constant calculation
    VectorFlags flags{};
    Optional<Value> precalc{};
    std::tie(precalc, flags) = inst->precalculate(3);
    if(precalc)
        return flags;

    // handling of special cases
    if(auto op = dynamic_cast<const intermediate::Operation*>(inst))
    {
        if(op->op == OP_OR && op->readsRegister(REG_ELEMENT_NUMBER))
        {
            // as e.g. very often used by conditional branches
            ElementFlags upperElement{};
            // this is 1..15, so can never be zero
            upperElement.zero = FlagStatus::CLEAR;
            // depends on the contents of the OR'ed value
            upperElement.carry = upperElement.negative = FlagStatus::UNDEFINED;
            // 32-bit overflow is never set by OR
            upperElement.overflow = FlagStatus::CLEAR;

            flags = VectorFlags{upperElement};

            if(auto sourceLoc = inst->findOtherArgument(ELEMENT_NUMBER_REGISTER) & &Value::checkLocal)
            {
                if(auto innerFlags = getConditionalFlags(sourceLoc, it))
                {
                    // If we know the inner flags, we can limit the flags set here. E.g. if we know the conditional
                    // input is always zero or one (for default boolean handling), we know that carry and negative flags
                    // cannot be set. Or in general, the currently handled flag setter cannot set flags not set by the
                    // inner flag setter, since OR does neither take away bit (e.g. for negative) nor overflows.
                    flags = *innerFlags;
                    for(auto& elem : flags)
                        // all upper elements can't be zero, the 0th element is overwritten below
                        elem.zero = FlagStatus::CLEAR;
                }
            }

            // 0th element can also be zero (dynamically)
            flags[0].zero = FlagStatus::UNDEFINED;

            return flags;
        }
        if(op->op == OP_OR)
        {
            // as e.g. for OR'ing boolean conditions
            Optional<VectorFlags> firstFlags{};
            Optional<VectorFlags> secondFlags{};
            std::tie(firstFlags, secondFlags) = getInnerFlags(*op, it);
            if(firstFlags && secondFlags)
            {
                // OR'ing values OR'es their possible flags, with exception of carry and 32-bit overflow, which can
                // never be set
                ElementFlags allElements{};
                // is determined dynamically by boolean values
                allElements.zero = allElements.negative = FlagStatus::UNDEFINED;
                // can never be set for OR
                allElements.carry = allElements.overflow = FlagStatus::CLEAR;
                VectorFlags flags{allElements};
                for(uint8_t i = 0; i < flags.size(); ++i)
                {
                    auto& elem = flags[i];
                    auto& first = (*firstFlags)[i];
                    auto& second = (*secondFlags)[i];

                    if(first.zero == second.zero)
                        // if both would statically set the same zero flag, we also set that
                        elem.zero = first.zero;
                    else if(first.zero == FlagStatus::CLEAR || second.zero == FlagStatus::CLEAR)
                        // if either statically sets a non-zero value, we also are non-zero
                        elem.zero = FlagStatus::CLEAR;
                    if(first.negative == second.negative)
                        // if both would statically set the same negative flag, we also set that
                        elem.negative = first.negative;
                    else if(first.negative == FlagStatus::SET || second.negative == FlagStatus::SET)
                        // if either statically sets a negative value, we are also negative
                        elem.negative = FlagStatus::SET;
                }
                return flags;
            }
        }
        if(op->op == OP_AND)
        {
            // as e.g. for AND'ing boolean conditions
            Optional<VectorFlags> firstFlags{};
            Optional<VectorFlags> secondFlags{};
            std::tie(firstFlags, secondFlags) = getInnerFlags(*op, it);
            if(firstFlags && secondFlags)
            {
                // AND'ing values AND'es their possible flags, with exception of carry and 32-bit overflow, which can
                // never be set
                ElementFlags allElements{};
                // is determined dynamically by boolean values
                allElements.zero = allElements.negative = FlagStatus::UNDEFINED;
                // can never be set for AND
                allElements.carry = allElements.overflow = FlagStatus::CLEAR;
                VectorFlags flags{allElements};
                for(uint8_t i = 0; i < flags.size(); ++i)
                {
                    auto& elem = flags[i];
                    auto& first = (*firstFlags)[i];
                    auto& second = (*secondFlags)[i];

                    if(first.zero == second.zero)
                        // if both would statically set the same zero flag, we also set that
                        elem.zero = first.zero;
                    else if(first.zero == FlagStatus::SET || second.zero == FlagStatus::SET)
                        // if either statically sets a zero value, we also are zero
                        elem.zero = FlagStatus::SET;
                    if(first.negative == second.negative)
                        // if both would statically set the same negative flag, we also set that
                        elem.negative = first.negative;
                    else if(first.negative == FlagStatus::CLEAR || second.negative == FlagStatus::CLEAR)
                        // if either statically sets a non-negative value, we are also non-negative
                        elem.negative = FlagStatus::CLEAR;
                }
                return flags;
            }
        }
        if(op->op == OP_MAX || op->op == OP_FMAX || op->op == OP_MIN || op->op == OP_FMIN || op->op == OP_FMAXABS ||
            op->op == OP_FMINABS)
        {
            // for (f)max, if one of the operands is known to be positive (>0), the negative and zero flags are never
            // set
            // similarly for (f)min, of one of the operands is known to be negative (<0), the negative flag is
            // guaranteed to be set and the zero flag not
            // also, fminabs and fmaxabs additionally can never set the negative flag
            auto firstOp = op->getFirstArg().getConstantValue();
            auto secondOp = op->assertArgument(1).getConstantValue();
            auto firstFlags = firstOp ? VectorFlags::fromValue(*firstOp) : VectorFlags{};
            auto secondFlags = secondOp ? VectorFlags::fromValue(*secondOp) : VectorFlags{};

            ElementFlags allElements{};
            // any of the min/max variants never set 32-bit overflow
            allElements.overflow = FlagStatus::CLEAR;
            VectorFlags flags{allElements};
            for(uint8_t i = 0; i < flags.size(); ++i)
            {
                auto& elem = flags[i];
                auto& first = firstFlags[i];
                auto& second = secondFlags[i];
                if(op->op == OP_MAX || op->op == OP_FMAX)
                {
                    if(first.negative == FlagStatus::CLEAR || second.negative == FlagStatus::CLEAR)
                        elem.negative = FlagStatus::CLEAR;
                    if(first.zero == FlagStatus::CLEAR && second.zero == FlagStatus::CLEAR)
                        elem.zero = FlagStatus::CLEAR;
                    if((first.negative == FlagStatus::CLEAR && first.zero == FlagStatus::CLEAR) ||
                        (second.negative == FlagStatus::CLEAR && second.zero == FlagStatus::CLEAR))
                        elem.zero = FlagStatus::CLEAR;
                }
                else if(op->op == OP_MIN || op->op == OP_FMIN)
                {
                    if(first.negative == FlagStatus::SET || second.negative == FlagStatus::SET)
                    {
                        elem.zero = FlagStatus::CLEAR;
                        elem.negative = FlagStatus::SET;
                    }
                    if(first.zero == FlagStatus::CLEAR && second.zero == FlagStatus::CLEAR)
                        elem.zero = FlagStatus::CLEAR;
                }
                else if(op->op == OP_FMAXABS || op->op == OP_FMINABS)
                {
                    if(op->op == OP_FMAXABS &&
                        (first.negative == FlagStatus::SET || second.negative == FlagStatus::SET))
                        // any negative value is larger than zero when given to the abs() function
                        elem.zero = FlagStatus::CLEAR;
                    if(op->op == OP_FMINABS && (first.zero == FlagStatus::SET || second.zero == FlagStatus::SET))
                        elem.zero = FlagStatus::SET;
                    elem.negative = FlagStatus::CLEAR;
                }
            }
            return flags;
        }
    }
    else if(auto move = dynamic_cast<const intermediate::MoveOperation*>(inst))
    {
        if(auto writer = intermediate::getSourceInstruction(move->getSource().getSingleWriter()))
            // single writer, check whether we can determine the flags from that
            return analyzeStaticFlags(writer, getInstructionWalker(writer, it), true);
        if(auto loc = move->getSource().checkLocal())
            // the source of the move is possibly written conditionally to a boolean depending on other flags
            return getConditionalFlags(loc, it);
    }

    // by default we don't know anything about the flags and just assume all to be dynamic
    return {};
}

Optional<VectorFlags> StaticFlagsAnalysis::analyzeStaticFlagsWrapper(const intermediate::IntermediateInstruction* inst,
    const Optional<VectorFlags>& previousFlags, InstructionWalker& it, BasicBlock& block)
{
    // update our "cache" instruction walker to point to the current instruction
    if(dynamic_cast<const intermediate::BranchLabel*>(inst))
        // on the first instruction (the label), initialize the instruction walker
        it = block.walk();
    while(!it.isEndOfBlock())
    {
        if(it.get() == inst)
            break;
        it.nextInBlock();
    }
    return analyzeStaticFlags(inst, it);
}

LCOV_EXCL_START
std::string StaticFlagsAnalysis::to_string(const Optional<VectorFlags>& flags)
{
    return flags.to_string();
}
LCOV_EXCL_STOP
