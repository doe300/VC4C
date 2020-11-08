/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Comparisons.h"

#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/operators.h"
#include "log.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

static NODISCARD InstructionWalker replaceWithSetBoolean(
    InstructionWalker it, const Value& dest, ConditionCode trueCode, const Value& value = BOOL_TRUE)
{
    /*
     * This code allows us to combine all "mov.ifz x, true" and "mov.ifnz x, false", since only 1 literal value is used
     * anymore
     *
     * NOTE: re-ordering might move them away. Also, they might get merged with other instructions
     * -> They are not guaranteed to be merged together -> Can't set flags (since the flags of first will influence the
     * second). This is no problem though, since everywhere the boolean value is used (e.g. branches, the flags are
     * re-set)
     */
    if(dest.hasRegister(REG_NOP))
    {
        // comparisons into nothing are only useful for their flags
        // -> no need to set the boolean result
        it.erase();
        // so next instruction is not skipped
        it.previousInBlock();
    }
    else
    {
        assign(it, dest) = (value, trueCode);
        it.reset(new Operation(OP_XOR, dest, value, value, trueCode.invert()));
    }
    return it;
}

static void intrinsifyIntegerRelation(Method& method, InstructionWalker it, const Comparison* comp, bool invertResult)
{
    // http://llvm.org/docs/LangRef.html#icmp-instruction
    auto boolType = TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth());
    if(COMP_EQ == comp->opCode)
    {
        auto cond = assignNop(it) = (as_unsigned{comp->getFirstArg()} == as_unsigned{comp->assertArgument(1)});
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? cond.invert() : cond);
    }
    else if(COMP_UNSIGNED_LT == comp->opCode)
    {
        // a < b [<=> umin(a, b) != b] <=> umax(a, b) != a
        /*
         * 32-bit:
         * a < b <=> ((a ^ b) < 0 ? min(a, b) : max(a, b)) != a
         *
         * 16-bit:
         * a < b [<=> max(a & 0xFFFF, b & 0xFFFF) != a] <=> max(a, b) != a (since MSB is never set -> always positive)
         *
         * 8-bit:
         * a < b [<=> max(a & 0xFF, b & 0xFF) != a] <=> max(a, b) != a (since MSB is never set -> always positive)
         */
        ConditionCode cond = COND_NEVER;
        if(comp->getFirstArg().type.getScalarBitCount() == 32)
        {
            // a < b [b = 2^x] <=> (a & (b-1)) == a <=> (a & ~(b-1)) == 0
            if(comp->assertArgument(1).checkLiteral() && isPowerTwo(comp->assertArgument(1).literal().unsignedInt()))
            {
                // this is actually pre-calculated, so no insertion is performed
                Value mask = assign(it, comp->assertArgument(1).type) = comp->assertArgument(1) - 1_val;
                Value tmp1 = assign(it, boolType, "%icomp") = comp->getFirstArg() & mask;

                assign(it, NOP_REGISTER) = (tmp1 ^ comp->getFirstArg(), SetFlag::SET_FLAGS);
                // we set to true, if flag is zero, false otherwise
                invertResult = !invertResult;
            }
            // a < b [b = 2^x -1] <=> (a & b) == a && a != b <=> (a & ~b) == 0 && a != b (this version is used)!
            // <=> (a >> log2(b + 1)) == 0 && a != b
            else if(comp->assertArgument(1).checkLiteral() &&
                isPowerTwo(comp->assertArgument(1).literal().unsignedInt() + 1))
            {
                // this is actually pre-calculated, so no insertion is performed
                Value mask = assign(it, comp->assertArgument(1).type) = comp->assertArgument(1) ^ 0xFFFFFFFF_val;
                Value tmp1 = assign(it, boolType, "%icomp") = comp->getFirstArg() & mask;
                const Value tmp2 = method.addNewLocal(boolType, "%icomp");

                assign(it, NOP_REGISTER) = (comp->getFirstArg() ^ comp->assertArgument(1), SetFlag::SET_FLAGS);
                // insert dummy comparison to be replaced
                it.emplace(new Nop(DelayType::WAIT_VPM));
                it = replaceWithSetBoolean(it, tmp2, COND_ZERO_SET);
                it.nextInBlock();
                assign(it, NOP_REGISTER) = (tmp1 | tmp2, SetFlag::SET_FLAGS);
                // we set to true, if flag is zero, false otherwise
                invertResult = !invertResult;
            }
            // TODO a < b [a = 2^x -1] <=> (~a & b) != 0
            else
            {
                /*
                 * a < b (unsigned)
                 *
                 * "positive": MSB = 0
                 * "negative": MSB = 1
                 * a < b  |      b pos     |      b neg     | b zero |
                 * a pos  | max(a, b) != a |        1       |    0   |
                 * a neg  |        0       | max(a, b) != a |    0   |
                 * a zero |        1       |        1       |    0   |
                 * simplified:
                 * a < b  |      b pos     |      b neg     | b zero = b pos |
                 * a pos  | max(a, b) != a | min(a, b) != a | max(a, b) != a |
                 * a neg  | min(a, b) != a | max(a, b) != a | min(a, b) != a |
                 * a zero | max(a, b) != a | min(a, b) != a | max(a, b) != a |
                 */
                auto firstArg = comp->getFirstArg();
                auto secondArg = comp->assertArgument(1);
                // insert a single instruction loading literal values to not need to insert one per usage
                if(firstArg.getLiteralValue())
                {
                    firstArg = method.addNewLocal(firstArg.type);
                    assign(it, firstArg) = comp->getFirstArg();
                }
                if(secondArg.getLiteralValue())
                {
                    secondArg = method.addNewLocal(secondArg.type);
                    assign(it, secondArg) = comp->assertArgument(1);
                }
                assign(it, NOP_REGISTER) = (firstArg ^ secondArg, SetFlag::SET_FLAGS);
                Value unsignedMax = method.addNewLocal(firstArg.type, "%icomp");
                assign(it, unsignedMax) = (min(firstArg, secondArg), COND_NEGATIVE_SET);
                assign(it, unsignedMax) = (max(firstArg, secondArg), COND_NEGATIVE_CLEAR);
                assign(it, NOP_REGISTER) = (unsignedMax ^ firstArg, SetFlag::SET_FLAGS);
            }
            cond = COND_ZERO_CLEAR;
        }
        else
        {
            // for non 32-bit types, high-bit is not set and therefore signed max == unsigned max
            cond = assignNop(it) = (as_signed{comp->getFirstArg()} < as_signed{comp->assertArgument(1)});
        }
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? cond.invert() : cond);
    }
    else if(COMP_SIGNED_LT == comp->opCode)
    {
        Value firstArg = comp->getFirstArg();
        Value secondArg = comp->assertArgument(1);
        if(firstArg.type.getScalarBitCount() < 32)
        {
            // TODO is this required?
            firstArg = method.addNewLocal(TYPE_INT32.toVectorType(firstArg.type.getVectorWidth()), "%icomp");
            secondArg = method.addNewLocal(TYPE_INT32.toVectorType(secondArg.type.getVectorWidth()), "%icomp");
            it = insertSignExtension(it, method, comp->getFirstArg(), firstArg, true);
            it = insertSignExtension(it, method, comp->assertArgument(1), secondArg, true);
        }
        auto cond = assignNop(it) = (as_signed{firstArg} < as_signed{secondArg});
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? cond.invert() : cond);
    }
    else
        throw CompilationError(CompilationStep::NORMALIZER, "Unrecognized integer comparison", comp->opCode);
}

static NODISCARD InstructionWalker insertCheckForNaN(
    InstructionWalker it, const Value& result, const Comparison* comp, bool invertResult)
{
    assign(it, result) = invertResult ? BOOL_TRUE : BOOL_FALSE;
    if(has_flag(comp->decoration, InstructionDecorations::NO_NAN) ||
        has_flag(comp->decoration, InstructionDecorations::FAST_MATH))
        // we are allowed to assume the operands to be non-nan, so we don't need to check this
        return it;
    auto nanCond = assignNop(it) = isnan(as_float{comp->getFirstArg()});
    assign(it, result) = (invertResult ? BOOL_FALSE : BOOL_TRUE, nanCond);
    if(auto arg1 = comp->getSecondArg())
    {
        nanCond = assignNop(it) = isnan(as_float{*arg1});
        assign(it, result) = (invertResult ? BOOL_FALSE : BOOL_TRUE, nanCond);
    }
    return it;
}

static NODISCARD InstructionWalker insertCheckFloatZero(
    InstructionWalker it, const Value& result, const Comparison* comp)
{
    assign(it, result) = BOOL_FALSE;
    auto zeroCond = assignNop(it) = iszero(as_float{comp->getFirstArg()});
    assign(it, result) = (BOOL_TRUE, zeroCond);
    if(auto arg1 = comp->getSecondArg())
    {
        // if we have 2 inputs, we need to check whether BOTH are zero
        zeroCond = assignNop(it) = iszero(as_float{*arg1});
        // if the second one is NOT zero, we need to clear the flag, so for ifzc, set to 0
        assign(it, result) = (BOOL_FALSE, zeroCond.invert());
    }
    return it;
}

static void intrinsifyFloatingRelation(Method& method, InstructionWalker it, const Comparison* comp)
{
    // since we do not support NaNs/Inf and denormals, all ordered comparisons are treated as unordered
    // -> "don't care if value could be Nan/Inf"

    /*
     * Expected treatment of NaN:
     * == -> false, if any is NaN
     * != -> true, if any is NaN
     * >, <, >=, <= -> false if any is NaN
     */

    // http://llvm.org/docs/LangRef.html#fcmp-instruction
    if(COMP_TRUE == comp->opCode)
    {
        // true
        it.reset(new MoveOperation(comp->getOutput().value(), BOOL_TRUE, COND_ALWAYS, SetFlag::SET_FLAGS));
    }
    else if(COMP_FALSE == comp->opCode)
    {
        // false
        it.reset(new MoveOperation(comp->getOutput().value(), BOOL_FALSE, COND_ALWAYS, SetFlag::SET_FLAGS));
    }
    else if(COMP_ORDERED_EQ == comp->opCode)
    {
        // IEEE 754 considers -0 and 0 to be equal according to the "usual numeric comparisons", see
        // https://en.wikipedia.org/wiki/Signed_zero#Comparisons
        // !isnan(a) && !isnan(b) && (a == b || (iszero(a) && iszero(b))) <=>
        // !(isnan(a) || isnan(b) || a != b) || (iszero(a) && iszero(b))
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, true);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} != as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_FALSE, cond);
        auto tmpZero = method.addNewLocal(TYPE_BOOL.toVectorType(comp->getOutput()->type.getVectorWidth()), "%iszero");
        it = insertCheckFloatZero(it, tmpZero, comp);
        assign(it, res) = res || tmpZero;
        it.erase();
    }
    else if(COMP_ORDERED_NEQ == comp->opCode)
    {
        // IEEE 754 considers -0 and 0 to be equal according to the "usual numeric comparisons", see
        // https://en.wikipedia.org/wiki/Signed_zero#Comparisons
        // !isnan(a) && !isnan(b) && a != b && !(iszero(a) && iszero(b)) <=>
        // !(isnan(a) || isnan(b) || a == b || (iszero(a) && iszero(b))
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, true);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} == as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_FALSE, cond);
        auto tmpZero = method.addNewLocal(TYPE_BOOL.toVectorType(comp->getOutput()->type.getVectorWidth()), "%iszero");
        it = insertCheckFloatZero(it, tmpZero, comp);
        cond = assignNop(it) = as_unsigned{tmpZero} != as_unsigned{INT_ZERO};
        assign(it, res) = (BOOL_FALSE, cond);
        it.erase();
    }
    else if(COMP_ORDERED_LT == comp->opCode)
    {
        // !isnan(a) && !isnan(b) && a < b <=> !(isnan(a) || isnan(b) || a >= b)
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, true);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} >= as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_FALSE, cond);
        it.erase();
    }
    else if(COMP_ORDERED_LE == comp->opCode)
    {
        // !isnan(a) && !isnan(b) && a <= b <=> !(isnan(a) || isnan(b) || a > b)
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, true);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} > as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_FALSE, cond);
        it.erase();
    }
    else if(COMP_ORDERED == comp->opCode)
    {
        // !isnan(a) && !isnan(b) <=> !(isnan(a) || isnan(b))
        it = insertCheckForNaN(it, comp->getOutput().value(), comp, true);
        it.erase();
    }
    else if(COMP_UNORDERED_EQ == comp->opCode)
    {
        // isnan(a) || isnan(b) || a == b
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, false);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} == as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_TRUE, cond);
        it.erase();
    }
    else if(COMP_UNORDERED_NEQ == comp->opCode)
    {
        // IEEE 754 considers -0 and 0 to be equal according to the "usual numeric comparisons", see
        // https://en.wikipedia.org/wiki/Signed_zero#Comparisons
        // isnan(a) || isnan(b) || (a != b && !(iszero(a) && iszero(b)))
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, false);
        auto tmpZero = method.addNewLocal(TYPE_BOOL.toVectorType(comp->getOutput()->type.getVectorWidth()), "%iszero");
        it = insertCheckFloatZero(it, tmpZero, comp);
        assign(it, tmpZero) = tmpZero ^ BOOL_TRUE;
        // if the values are equal, we need to clear the flag
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} == as_float{comp->assertArgument(1)});
        assign(it, tmpZero) = (BOOL_FALSE, cond);
        assign(it, res) = res || tmpZero;
        it.erase();
    }
    else if(COMP_UNORDERED_LT == comp->opCode)
    {
        // isnan(a) || isnan(b) || a < b
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, false);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} < as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_TRUE, cond);
        it.erase();
    }
    else if(COMP_UNORDERED_LE == comp->opCode)
    {
        // isnan(a) || isnan(b) || a <= b
        const auto& res = comp->getOutput().value();
        it = insertCheckForNaN(it, res, comp, false);
        auto cond = assignNop(it) = (as_float{comp->getFirstArg()} <= as_float{comp->assertArgument(1)});
        assign(it, res) = (BOOL_TRUE, cond);
        it.erase();
    }
    else if(COMP_UNORDERED == comp->opCode)
    {
        // isnan(a) || isnan(b)
        it = insertCheckForNaN(it, comp->getOutput().value(), comp, false);
        it.erase();
    }
    else
        throw CompilationError(CompilationStep::NORMALIZER, "Unrecognized floating-point comparison", comp->opCode);
}

static void swapComparisons(const std::string& opCode, Comparison* comp)
{
    Value tmp = comp->getFirstArg();
    comp->setArgument(0, comp->assertArgument(1));
    comp->setArgument(1, std::move(tmp));
    comp->opCode = opCode;
}

bool intrinsics::intrinsifyComparison(Method& method, InstructionWalker it)
{
    Comparison* comp = it.get<Comparison>();
    if(comp == nullptr)
    {
        return false;
    }
    if(comp->hasConditionalExecution())
        throw CompilationError(
            CompilationStep::NORMALIZER, "Comparisons cannot have conditional execution", comp->to_string());
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Intrinsifying comparison '" << comp->opCode << "' to arithmetic operations" << logging::endl);
    bool isFloating = comp->getFirstArg().type.isFloatingType();
    bool negateResult = false;
    if(!isFloating)
    {
        // simplification, map all unequal comparisons to "equals" and "less-then"
        if(COMP_NEQ == comp->opCode)
        {
            // a != b == !(a == b)
            comp->opCode = COMP_EQ;
            negateResult = true;
        }
        else if(COMP_UNSIGNED_GE == comp->opCode)
        {
            // a >= b -> !(a < b)
            comp->opCode = COMP_UNSIGNED_LT;
            negateResult = true;
        }
        else if(COMP_UNSIGNED_GT == comp->opCode)
        {
            // a > b -> b < a
            swapComparisons(COMP_UNSIGNED_LT, comp);
            negateResult = false;
        }
        else if(COMP_UNSIGNED_LE == comp->opCode)
        {
            // a <= b -> !(b < a)
            swapComparisons(COMP_UNSIGNED_LT, comp);
            negateResult = true;
        }
        else if(COMP_SIGNED_GE == comp->opCode)
        {
            // a >= b -> !(a < b)
            comp->opCode = COMP_SIGNED_LT;
            negateResult = true;
        }
        else if(COMP_SIGNED_GT == comp->opCode)
        {
            // a > b -> b < a
            swapComparisons(COMP_SIGNED_LT, comp);
            negateResult = false;
        }
        else if(COMP_SIGNED_LE == comp->opCode)
        {
            // a <= b -> !(b < a)
            swapComparisons(COMP_SIGNED_LT, comp);
            negateResult = true;
        }
        intrinsifyIntegerRelation(method, it, comp, negateResult);
    }
    else
    {
        // simplification, make a R b -> b R' a
        if(COMP_ORDERED_GE == comp->opCode)
        {
            swapComparisons(COMP_ORDERED_LE, comp);
        }
        else if(COMP_ORDERED_GT == comp->opCode)
        {
            swapComparisons(COMP_ORDERED_LT, comp);
        }
        else if(COMP_UNORDERED_GE == comp->opCode)
        {
            swapComparisons(COMP_UNORDERED_LE, comp);
        }
        else if(COMP_UNORDERED_GT == comp->opCode)
        {
            swapComparisons(COMP_UNORDERED_LT, comp);
        }
        intrinsifyFloatingRelation(method, it, comp);
    }
    return true;
}
