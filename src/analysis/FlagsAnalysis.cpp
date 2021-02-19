/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "FlagsAnalysis.h"

#include "../intermediate/Helper.h"
#include "PatternMatching.h"

#include <climits>
#include <sstream>
#include <vector>

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
    else if(auto moveSource = inst->getMoveSource())
    {
        if(auto writer = intermediate::getSourceInstruction(moveSource->getSingleWriter()))
            // single writer, check whether we can determine the flags from that
            return analyzeStaticFlags(writer, getInstructionWalker(writer, it), true);
        if(auto loc = moveSource->checkLocal())
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

std::string ComparisonInfo::to_string() const
{
    auto outputPart = result ? (result->to_string() + " = ") : "";
    return outputPart + leftOperand.to_string() + " " + std::string(name) + " " + rightOperand.to_string() + " (" +
        condition.to_string() + ", " + elementMask.to_string() + ")";
}
LCOV_EXCL_STOP

struct ComparisonData
{
    std::string comp;
    InstructionWalker endOfComparison;
    ConditionCode trueCondition;
    std::bitset<NATIVE_VECTOR_SIZE> elementMask{0xFFFF};

    LCOV_EXCL_START
    inline std::string to_string(const Value& left, const Value& right) const
    {
        ComparisonInfo tmp{comp, left, right, nullptr, trueCondition, elementMask};
        return tmp.to_string();
    }
    LCOV_EXCL_STOP
};

static Optional<ComparisonData> checkEquals(InstructionWalker searchIt, Value& leftOperand, Value& rightOperand)
{
    using namespace vc4c::pattern;

    // Check for left == right
    Pattern equalsPattern = {{

        /* register - = xor %left, %right (setf) */
        anyValue() = (match(OP_XOR), capture(leftOperand), capture(rightOperand), match(SetFlag::SET_FLAGS))

    }};

    auto result = pattern::search(searchIt, equalsPattern, true);
    if(!result.isEndOfBlock())
        return ComparisonData{intermediate::COMP_EQ, result, COND_ZERO_SET};

    // Check for left == 0
    Pattern zeroPattern = {{

        /* register - = mov %left (setf) */
        anyValue() = (match(FAKEOP_MOV), capture(leftOperand), match(SetFlag::SET_FLAGS))

    }};

    result = pattern::search(searchIt, zeroPattern, true);
    if(!result.isEndOfBlock())
    {
        rightOperand = INT_ZERO;
        return ComparisonData{intermediate::COMP_EQ, result, COND_ZERO_SET};
    }

    // Check for scalar left == 0
    Pattern scalarZeroPattern = {{

        /* register - = or element_number, %left (setf) */
        anyValue() = (match(OP_OR), match(ELEMENT_NUMBER_REGISTER), capture(leftOperand), match(SetFlag::SET_FLAGS))

    }};

    result = pattern::search(searchIt, scalarZeroPattern, true);
    if(!result.isEndOfBlock() && (leftOperand.type.isScalarType() || leftOperand.type.getPointerType()))
    {
        // only allow this if the type is actually a scalar type
        rightOperand = INT_ZERO;
        return ComparisonData{intermediate::COMP_EQ, result, COND_ZERO_SET, 0x1 /* single element */};
    }

    return {};
}

static Optional<ComparisonData> checkUnsignedLessThan(
    InstructionWalker searchIt, Value& leftOperand, Value& rightOperand)
{
    using namespace vc4c::pattern;

    // Check for unsigned comparison left < right (with right = 2^x - 1)
    Value rightInverted = UNDEFINED_VALUE;
    Value trueValue = UNDEFINED_VALUE;
    Pattern rightMaskPattern = {{

        /* bool %tmp0 = and i32 %left, i32 ~%right */
        capture(pattern::V1) = (match(OP_AND), capture(leftOperand), capture(rightInverted)),
        /* register - = xor i32 %left,  i32 %right (setf) */
        anyValue() = (match(OP_XOR), capture(leftOperand), capture(rightOperand), match(SetFlag::SET_FLAGS)),
        /* bool %tmp1 = bool true (ifz) */
        capture(pattern::V2) = (match(FAKEOP_MOV), capture(trueValue), match(COND_ZERO_SET)),
        /* bool %tmp1 = bool false (ifzc) */
        capture(pattern::V2) = (match(OP_XOR), capture(trueValue), capture(trueValue), match(COND_ZERO_CLEAR)),
        /* register - = or bool %tmp0, bool %tmp1 (setf) */
        anyValue() = (match(OP_OR), capture(pattern::V1), capture(pattern::V2), match(SetFlag::SET_FLAGS))

    }};

    auto result = pattern::search(searchIt, rightMaskPattern, true);
    if(!result.isEndOfBlock() && trueValue.getLiteralValue() & &Literal::isTrue)
    {
        // check that the right inverted value is the inversion of the right value and that the right value is actually
        // a constant 2^x-1
        auto rightInvertedConstant = rightInverted.getConstantValue() & &Value::getLiteralValue;
        auto rightConstant = rightOperand.getConstantValue() & &Value::getLiteralValue;
        if(rightConstant && rightInvertedConstant &&
            rightInvertedConstant->unsignedInt() == ~rightConstant->unsignedInt() &&
            isPowerTwo(rightConstant->unsignedInt() + 1))
            return ComparisonData{intermediate::COMP_UNSIGNED_LT, result, COND_ZERO_SET};
    }

    // Check for unsigned comparison left <= right (with left = 2^x - 1 or right = 2^x - 1)
    Pattern andConstantPattern = {{

        /* register - = and i32 %left, i32 %right (setf) */
        anyValue() = (match(OP_AND), capture(leftOperand), capture(rightOperand), match(SetFlag::SET_FLAGS))

    }};

    result = pattern::search(searchIt, andConstantPattern, true);
    if(!result.isEndOfBlock())
    {
        // check that the left value is actually a constant (~2^x)-1
        // ~(2^x-1) & %val == 0 <=> 2^x-1 >= %val <=> 2^x > %val
        auto lit = leftOperand.getConstantValue() & &Value::getLiteralValue;
        if(lit & [](Literal lit) { return isPowerTwo(~lit.unsignedInt() + 1); })
        {
            leftOperand = Value(Literal(~lit->unsignedInt() + 1), leftOperand.type);
            return ComparisonData{intermediate::COMP_UNSIGNED_GT, result, COND_ZERO_SET};
        }

        // check that the right value is actually a constant (~2^x)-1
        // %val & ~(2^x-1) == 0 <=> %val <= 2^x-1 <=> %val < 2^x
        lit = rightOperand.getConstantValue() & &Value::getLiteralValue;
        if(lit & [](Literal lit) { return isPowerTwo(~lit.unsignedInt() + 1); })
        {
            rightOperand = Value(Literal(~lit->unsignedInt() + 1), rightOperand.type);
            return ComparisonData{intermediate::COMP_UNSIGNED_LT, result, COND_ZERO_SET};
        }
    }

    // Check for general 32-bit unsigned comparison left < right
    Pattern minMaxPattern = {{

        /* register - = xor i32 %left, i32 %right (setf ) */
        anyValue() = (match(OP_XOR), capture(leftOperand), capture(rightOperand), match(SetFlag::SET_FLAGS)),
        /* i32 %tmp = min i32 %left, i32 %right (ifn ) */
        capture(pattern::V1) = (match(OP_MIN), capture(leftOperand), capture(rightOperand), match(COND_NEGATIVE_SET)),
        /* i32 %tmp = max i32 %left, i32 %right (ifnc ) */
        capture(pattern::V1) = (match(OP_MAX), capture(leftOperand), capture(rightOperand), match(COND_NEGATIVE_CLEAR)),
        /* register - = xor i32 %tmp, i32 %left (setf ) */
        anyValue() = (match(OP_XOR), capture(pattern::V1), capture(leftOperand), match(SetFlag::SET_FLAGS))

    }};

    result = pattern::search(searchIt, minMaxPattern, true);
    if(!result.isEndOfBlock())
        return ComparisonData{intermediate::COMP_UNSIGNED_LT, result, COND_ZERO_CLEAR};

    // NOTE: The general case for unsigned left < right for |left| and |right| < 32-bit is identical to the signed less
    // < right and thus handled there.

    return {};
}

static Optional<ComparisonData> checkSignedGreaterThan(
    InstructionWalker searchIt, Value& leftOperand, Value& rightOperand)
{
    using namespace vc4c::pattern;

    // Check for left > right
    Pattern pattern = {{

        /* register - = max %left, %right (setf) */
        anyValue() = (match(OP_MAX), capture(leftOperand), capture(rightOperand), match(SetFlag::SET_FLAGS))

    }};
    auto result = pattern::search(searchIt, pattern, true);
    if(!result.isEndOfBlock())
        return ComparisonData{intermediate::COMP_SIGNED_GT, result, COND_CARRY_SET};
    return {};
}

static std::pair<const intermediate::IntermediateInstruction*, ConditionCode> findTrueSetter(const Local* loc)
{
    auto writers = loc->getUsers(LocalUse::Type::WRITER);
    if(writers.size() != 2)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Cannot determine comparison for local without exactly 2 conditional writes: " << loc->to_string()
                << logging::endl);
        return std::make_pair(nullptr, COND_NEVER);
    }

    auto trueWriterIt = std::find_if(writers.begin(), writers.end(), [](const LocalUser* writer) -> bool {
        auto source = writer->getMoveSource();
        return source && source->getConstantValue() & &Value::getLiteralValue & &Literal::isTrue &&
            dynamic_cast<const intermediate::ExtendedInstruction*>(writer);
    });
    if(trueWriterIt == writers.end())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to find writer of 'true' value for: " << loc->to_string() << logging::endl);
        return std::make_pair(nullptr, COND_NEVER);
    }
    auto trueCond = dynamic_cast<const intermediate::ExtendedInstruction*>(*trueWriterIt)->getCondition();
    if(trueCond == COND_ALWAYS || trueCond == COND_NEVER)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Writer of 'true' value is not conditional: " << (*trueWriterIt)->to_string() << logging::endl);
        return std::make_pair(nullptr, COND_NEVER);
    }
    return std::make_pair(*trueWriterIt, trueCond);
}

/*
 * Tries to merge multiple dependent comparisons into a single data set.
 *
 * Starting from our current resulting comparison info, recursively check whether this is a boolean (true/false)
 * comparison and if so, try to find the comparison setting the used boolean values. If such a comparison is found,
 * merge their information, so the final result is dependent on the original comparison.
 */
static ComparisonInfo mergeComparisons(ComparisonInfo&& info,
    const FastMap<InstructionWalker, std::tuple<ComparisonData, Value, Value>>& otherComparisons, BasicBlock& block)
{
    if(otherComparisons.empty())
        return std::move(info);

    if(info.name != intermediate::COMP_EQ && info.name != intermediate::COMP_NEQ)
        // for only support merging of boolean comparisons and their source comparisons
        return std::move(info);

    // 1. find the flag set instruction walker for the input boolean value
    Optional<InstructionWalker> previousFlagSetter{};
    ConditionCode trueCond = COND_NEVER;
    if(info.leftOperand.checkLocal() && info.leftOperand.type.getElementType() == TYPE_BOOL)
    {
        auto pair = findTrueSetter(info.leftOperand.local());
        if(pair.first && pair.second != COND_NEVER)
        {
            if(auto tmpIt = block.findWalkerForInstruction(pair.first, block.walkEnd()))
                previousFlagSetter = block.findLastSettingOfFlags(*tmpIt);
            if(previousFlagSetter)
                trueCond = pair.second;
        }
    }
    if(!previousFlagSetter && info.rightOperand.checkLocal() && info.rightOperand.type.getElementType() == TYPE_BOOL)
    {
        auto pair = findTrueSetter(info.rightOperand.local());
        if(pair.first && pair.second != COND_NEVER)
        {
            if(auto tmpIt = block.findWalkerForInstruction(pair.first, block.walkEnd()))
                previousFlagSetter = block.findLastSettingOfFlags(*tmpIt);
            if(previousFlagSetter)
                trueCond = pair.second;
        }
    }

    if(!previousFlagSetter || trueCond == COND_NEVER)
        return std::move(info);

    // 2. find the comparison matching the flag set instruction walker
    auto compIt = otherComparisons.find(*previousFlagSetter);
    if(compIt == otherComparisons.end())
        return std::move(info);

    // 3. merge comparison info and heed any inversion of flags
    // original comparison has condition for boolean "true" result
    auto originalCond = std::get<0>(compIt->second).trueCondition;
    // we have our intermediate boolean condition, on which flags they are set
    auto intermediateCond = trueCond;
    // and our final boolean true condition of the output comparison
    auto outputCond = info.condition;

    if(originalCond != intermediateCond && !originalCond.isInversionOf(intermediateCond))
        // they are unrelated, which should never happen
        return std::move(info);

    // intermediate boolean is true on original comparison match -> retain output condition
    auto resultCond = originalCond == intermediateCond ? outputCond : outputCond.invert();

    if(info.name == intermediate::COMP_NEQ)
        // intermediate boolean is not "some true value" -> invert condition
        resultCond = resultCond.invert();

    if(info.leftOperand.getConstantValue() == INT_ZERO || info.rightOperand.getConstantValue() == INT_ZERO)
        // intermediate boolean is zero (false value) -> invert condition
        resultCond = resultCond.invert();

    ComparisonInfo resultInfo{std::get<0>(compIt->second).comp, std::get<1>(compIt->second),
        std::get<2>(compIt->second), info.result, resultCond,
        std::get<0>(compIt->second).elementMask & info.elementMask};

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Combined comparison '" << info.to_string() << "' and '"
            << std::get<0>(compIt->second).to_string(std::get<1>(compIt->second), std::get<2>(compIt->second))
            << "' into: " << resultInfo.to_string() << logging::endl);

    // recursively check further
    return mergeComparisons(std::move(resultInfo), otherComparisons, block);
}

Optional<analysis::ComparisonInfo> analysis::getComparison(
    const intermediate::IntermediateInstruction* conditionalInstruction, InstructionWalker searchStart,
    bool checkTransitive)
{
    // TODO add floating point and long comparisons??

    Optional<InstructionWalker> matchingFlagSetterIt;
    if(auto it = searchStart.getBasicBlock()->findWalkerForInstruction(
           conditionalInstruction, searchStart.getBasicBlock()->walkEnd()))
        matchingFlagSetterIt = searchStart.getBasicBlock()->findLastSettingOfFlags(*it);

    if(!matchingFlagSetterIt)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Failed to find flag setter instruction for conditional instruction: "
                << conditionalInstruction->to_string() << logging::endl);
        return {};
    }

    Value leftOperand = UNDEFINED_VALUE;
    Value rightOperand = UNDEFINED_VALUE;
    // Since there might be multiple comparisons between the search start and the writing of the output value, we need
    // to iteratively check until we have a match or do not find any comparison anymore. The order of the checks is on
    // purpose, since e.g. some the unsigned less then check include an equality-check, so we need to check them first.

    // Keep track of already found comparisons, in case we need to merge them in later
    FastMap<InstructionWalker, std::tuple<ComparisonData, Value, Value>> otherComparisons;
    for(const auto& check : {checkUnsignedLessThan, checkSignedGreaterThan, checkEquals})
    {
        auto checkIt = searchStart;
        while(auto tmp = check(checkIt, leftOperand, rightOperand))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found comparison: " << tmp->to_string(leftOperand, rightOperand) << logging::endl);
            if(tmp->endOfComparison.get() == matchingFlagSetterIt->get())
                // The comparison ends with the flag setter responsible for setting the boolean values, this is the
                // comparison we want!
                return mergeComparisons(
                    ComparisonInfo{tmp->comp, leftOperand, rightOperand, nullptr, tmp->trueCondition, tmp->elementMask},
                    otherComparisons, *searchStart.getBasicBlock());

            checkIt = tmp->endOfComparison;
            // The end of the comparison is inside the comparison, which for single instruction comparisons is
            // also its start)
            checkIt.nextInBlock();

            if(checkTransitive)
                // We do not override any previous comparison on purpose for the reason stated above, so that e.g.
                // equals comparisons do not override other comparisons including them
                otherComparisons.emplace(
                    tmp->endOfComparison, std::make_tuple(std::move(tmp.value()), leftOperand, rightOperand));
        }
    }

    return {};
}

static const char* invertComparisonName(const std::string& comparison)
{
    if(comparison == intermediate::COMP_EQ)
        return intermediate::COMP_NEQ;
    if(comparison == intermediate::COMP_NEQ)
        return intermediate::COMP_EQ;
    if(comparison == intermediate::COMP_UNSIGNED_GE)
        return intermediate::COMP_UNSIGNED_LT;
    if(comparison == intermediate::COMP_UNSIGNED_GT)
        return intermediate::COMP_UNSIGNED_LE;
    if(comparison == intermediate::COMP_UNSIGNED_LE)
        return intermediate::COMP_UNSIGNED_GT;
    if(comparison == intermediate::COMP_UNSIGNED_LT)
        return intermediate::COMP_UNSIGNED_GE;
    if(comparison == intermediate::COMP_SIGNED_GE)
        return intermediate::COMP_SIGNED_LT;
    if(comparison == intermediate::COMP_SIGNED_GT)
        return intermediate::COMP_SIGNED_LE;
    if(comparison == intermediate::COMP_SIGNED_LE)
        return intermediate::COMP_SIGNED_GT;
    if(comparison == intermediate::COMP_SIGNED_LT)
        return intermediate::COMP_SIGNED_GE;

    throw CompilationError(CompilationStep::GENERAL, "Unhandled comparison operation to invert", comparison);
}

Optional<analysis::ComparisonInfo> analysis::getComparison(
    const Local* comparisonResult, InstructionWalker searchStart, bool checkTransitive)
{
    auto pair = findTrueSetter(comparisonResult);
    if(!pair.first || pair.second == COND_NEVER)
        return {};

    if(auto result = getComparison(pair.first, searchStart, checkTransitive))
    {
        result->result = comparisonResult;
        if(pair.second.isInversionOf(result->condition))
        {
            // Invert comparison if necessary depending on actual flags setting the bool true. E.g. for "not equals", we
            // insert an "equals" comparison and invert the flags, so we need to invert the comparison and the flags
            // back again here.
            result->name = invertComparisonName(result->name);
            result->condition = pair.second;
        }
        if(pair.second != result->condition)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found matching comparison, but flags mismatch for '" << comparisonResult->to_string()
                    << "' and: " << result->to_string() << logging::endl);
            return {};
        }
        return result;
    }
    return {};
}
