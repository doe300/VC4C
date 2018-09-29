/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Comparisons.h"

#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "log.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;

static InstructionWalker replaceWithSetBoolean(
    InstructionWalker it, const Value& dest, const ConditionCode& trueCode, const Value& value = BOOL_TRUE)
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
        it.emplace(new MoveOperation(dest, value, trueCode));
        it.nextInBlock();
        it.reset(new Operation(OP_XOR, dest, value, value, trueCode.invert()));
    }
    return it;
}

static bool isPowerTwo(uint32_t val)
{
    // https://en.wikipedia.org/wiki/Power_of_two#Fast_algorithm_to_check_if_a_positive_number_is_a_power_of_two
    return val > 0 && (val & (val - 1)) == 0;
}

InstructionWalker intrinsifyIntegerRelation(
    Method& method, InstructionWalker it, const Comparison* comp, bool invertResult)
{
    // http://llvm.org/docs/LangRef.html#icmp-instruction
    const Value tmp = method.addNewLocal(comp->getFirstArg().type, "%icomp");
    if(COMP_EQ == comp->opCode)
    {
        // a == b <=> a xor b == 0 [<=> a - b == 0]
        // a != b <=> a xor b != 0
        if(comp->getFirstArg().hasLocal() && comp->assertArgument(1).hasLiteral(Literal(0u)))
            // special case for a == 0
            // does not save instructions, but does not force value a to be on register-file A (since B is reserved for
            // literal 0)
            it.emplace(new MoveOperation(NOP_REGISTER, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        else
            it.emplace(new Operation(OP_XOR, NOP_REGISTER, comp->getFirstArg(), comp->assertArgument(1),
                comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? COND_ZERO_CLEAR : COND_ZERO_SET);
    }
    else if(COMP_UNSIGNED_LT == comp->opCode)
    {
        // a < b [<=> umin(a, b) != b] <=> umax(a, b) != a
        /*
         * 32-bit:
         * a < b <=> (a >> 16 < b >> 16) || ((a >> 16 == b >> 16) && a & 0xFFFF < b & 0xFFFF)
         *
         * 16-bit:
         * a < b [<=> max(a & 0xFFFF, b & 0xFFFF) != a] <=> max(a, b) != a (since MSB is never set -> always positive)
         *
         * 8-bit:
         * a < b [<=> max(a & 0xFF, b & 0xFF) != a] <=> max(a, b) != a (since MSB is never set -> always positive)
         */
        if(comp->getFirstArg().type.getScalarBitCount() == 32)
        {
            // a < b [b = 2^x] <=> (a & (b-1)) == a <=> (a & ~(b-1)) == 0
            if(comp->assertArgument(1).hasLiteral() && isPowerTwo(comp->assertArgument(1).literal().unsignedInt()))
            {
                const Value mask(
                    Literal(comp->assertArgument(1).literal().unsignedInt() - 1), comp->assertArgument(1).type);
                const Value tmp1 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");

                it.emplace(new Operation(OP_AND, tmp1, comp->getFirstArg(), mask));
                it.nextInBlock();
                it.emplace(
                    new Operation(OP_XOR, NOP_REGISTER, tmp1, comp->getFirstArg(), COND_ALWAYS, SetFlag::SET_FLAGS));
                it.nextInBlock();
                // we set to true, if flag is zero, false otherwise
                invertResult = !invertResult;
            }
            // a < b [b = 2^x -1] <=> (a & b) == a && a != b <=> (a & ~b) == 0 && a != b (this version is used)!
            // <=> (a >> log2(b + 1)) == 0 && a != b
            else if(comp->assertArgument(1).hasLiteral() &&
                isPowerTwo(comp->assertArgument(1).literal().unsignedInt() + 1))
            {
                const Value mask(
                    Literal(comp->assertArgument(1).literal().unsignedInt() ^ 0xFFFFFFFFu), comp->getFirstArg().type);
                const Value tmp1 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");
                const Value tmp2 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");

                it.emplace(new Operation(OP_AND, tmp1, comp->getFirstArg(), mask));
                it.nextInBlock();
                it.emplace(new Operation(OP_XOR, NOP_REGISTER, comp->getFirstArg(), comp->assertArgument(1),
                    COND_ALWAYS, SetFlag::SET_FLAGS));
                it.nextInBlock();
                // insert dummy comparison to be replaced
                it.emplace(new Nop(DelayType::WAIT_VPM));
                it = replaceWithSetBoolean(it, tmp2, COND_ZERO_SET);
                it.nextInBlock();
                it.emplace(new Operation(OP_OR, NOP_REGISTER, tmp1, tmp2, COND_ALWAYS, SetFlag::SET_FLAGS));
                it.nextInBlock();
                // we set to true, if flag is zero, false otherwise
                invertResult = !invertResult;
            }
            // TODO a < b [a = 2^x -1] <=> (~a & b) != 0
            else
            {
                // XXX optimize? combine both comparisons of upper half? can short-circuit on ||?
                // TODO the general case seems to be wrong
                /*
                 * Another way of calculating ult:
                 * a < b <=> clz(a) > clz(b) || (clz(a) == clz(b) && max(a, b) != a)
                 * For equal MSB, ult == slt, special case for different MSB
                 */
                const Value tmp1 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");
                const Value tmp2 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");
                const Value tmp3 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");
                const Value tmp4 =
                    method.addNewLocal(TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%icomp");
                const Value leftUpper = method.addNewLocal(
                    TYPE_INT16.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%comp.left");
                const Value leftLower = method.addNewLocal(
                    TYPE_INT16.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%comp.left");
                const Value rightUpper = method.addNewLocal(
                    TYPE_INT16.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%comp.right");
                const Value rightLower = method.addNewLocal(
                    TYPE_INT16.toVectorType(comp->getFirstArg().type.getVectorWidth()), "%comp.right");
                it.emplace(new Operation(OP_SHR, leftUpper, comp->getFirstArg(), Value(Literal(16u), TYPE_INT8)));
                it.nextInBlock();
                it.emplace(new Operation(OP_AND, leftLower, comp->getFirstArg(),
                    Value(Literal(TYPE_INT16.getScalarWidthMask()), TYPE_INT32)));
                it.nextInBlock();
                it.emplace(new Operation(OP_SHR, rightUpper, comp->assertArgument(1), Value(Literal(16u), TYPE_INT8)));
                it.nextInBlock();
                it.emplace(new Operation(OP_AND, rightLower, comp->assertArgument(1),
                    Value(Literal(TYPE_INT16.getScalarWidthMask()), TYPE_INT32)));
                it.nextInBlock();
                //(a >> 16) < (b >> 16)
                // insert dummy comparison to be intrinsified
                it.emplace(new Comparison(COMP_UNSIGNED_LT, tmp1, leftUpper, rightUpper));
                it = intrinsifyIntegerRelation(method, it, it.get<Comparison>(), false);
                it.nextInBlock();
                //(a >> 16) == (b >> 16)
                // insert dummy comparison to be intrinsified
                it.emplace(new Comparison(COMP_EQ, tmp2, leftUpper, rightUpper));
                it = intrinsifyIntegerRelation(method, it, it.get<Comparison>(), false);
                it.nextInBlock();
                //(a & 0xFFFF) < (b & 0xFFFF)
                // insert dummy comparison to be intrinsified
                it.emplace(new Comparison(COMP_UNSIGNED_LT, tmp3, leftLower, rightLower));
                it = intrinsifyIntegerRelation(method, it, it.get<Comparison>(), false);
                it.nextInBlock();
                // upper && lower
                it.emplace(new Operation(OP_AND, tmp4, tmp2, tmp3));
                it.nextInBlock();
                it.emplace(new Operation(OP_OR, NOP_REGISTER, tmp1, tmp4, comp->conditional, SetFlag::SET_FLAGS));
                it.nextInBlock();
            }
        }
        else
        {
            it.emplace(new Operation(OP_MAX, tmp, comp->getFirstArg(), comp->assertArgument(1), comp->conditional));
            it.nextInBlock();
            it.emplace(
                new Operation(OP_XOR, NOP_REGISTER, tmp, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
            it.nextInBlock();
        }
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? COND_ZERO_SET : COND_ZERO_CLEAR);
    }
    else if(COMP_SIGNED_LT == comp->opCode)
    {
        // a < b [<=> min(a, b) != b] <=> max(a, b) != a
        Value firstArg = comp->getFirstArg();
        Value secondArg = comp->assertArgument(1);
        if(firstArg.type.getScalarBitCount() < 32)
        {
            firstArg = method.addNewLocal(TYPE_INT32.toVectorType(firstArg.type.getVectorWidth()), "%icomp");
            secondArg = method.addNewLocal(TYPE_INT32.toVectorType(secondArg.type.getVectorWidth()), "%icomp");
            it = insertSignExtension(it, method, comp->getFirstArg(), firstArg, true);
            it = insertSignExtension(it, method, comp->assertArgument(1), secondArg, true);
        }
        it.emplace(new Operation(OP_MAX, tmp, firstArg, secondArg, comp->conditional));
        it.nextInBlock();
        it.emplace(new Operation(OP_XOR, NOP_REGISTER, tmp, firstArg, comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? COND_ZERO_SET : COND_ZERO_CLEAR);
    }
    else
        throw CompilationError(CompilationStep::OPTIMIZER, "Unrecognized integer comparison", comp->opCode);

    return it;
}

static std::pair<InstructionWalker, Value> insertCheckForNaN(
    Method& method, InstructionWalker it, const Comparison* comp)
{
    const Value firstArgNaN = method.addNewLocal(TYPE_BOOL, "%nan_check");
    it.emplace(new Operation(OP_XOR, firstArgNaN, comp->getFirstArg(), FLOAT_NAN, comp->conditional));
    it.nextInBlock();
    Value eitherNaN = firstArgNaN;
    if(comp->getArguments().size() > 1)
    {
        const Value secondArgNaN = method.addNewLocal(TYPE_BOOL, "%nan_check");
        it.emplace(new Operation(OP_XOR, secondArgNaN, comp->assertArgument(1), FLOAT_NAN, comp->conditional));
        it.nextInBlock();
        eitherNaN = method.addNewLocal(TYPE_BOOL, "%nan_check");
        it.emplace(new Operation(OP_OR, eitherNaN, firstArgNaN, secondArgNaN, comp->conditional));
        it.nextInBlock();
    }
    return std::make_pair(it, eitherNaN);
}

InstructionWalker intrinsifyFloatingRelation(Method& method, InstructionWalker it, const Comparison* comp)
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
    const Value tmp = method.addNewLocal(comp->getFirstArg().type, comp->getOutput()->local()->name);
    if(COMP_TRUE == comp->opCode)
    {
        // true
        it.reset(new MoveOperation(comp->getOutput().value(), BOOL_TRUE, comp->conditional, SetFlag::SET_FLAGS));
    }
    else if(COMP_FALSE == comp->opCode)
    {
        // false
        it.reset(new MoveOperation(comp->getOutput().value(), BOOL_FALSE, comp->conditional, SetFlag::SET_FLAGS));
    }
    else if(COMP_ORDERED_EQ == comp->opCode || COMP_UNORDERED_EQ == comp->opCode)
    {
        // a == b <=> a xor b == 0 [<=> a - b == 0]
        it.emplace(new Operation(
            OP_XOR, NOP_REGISTER, comp->getFirstArg(), comp->assertArgument(1), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_SET);
    }
    else if(COMP_ORDERED_NEQ == comp->opCode || COMP_UNORDERED_NEQ == comp->opCode)
    {
        // a != b <=> a xor b != 0
        it.emplace(new Operation(
            OP_XOR, NOP_REGISTER, comp->getFirstArg(), comp->assertArgument(1), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_CLEAR);
    }
    else if(COMP_ORDERED_LT == comp->opCode || COMP_UNORDERED_LT == comp->opCode)
    {
        // a < b [<=> min(a, b) != b] [<=> max(a, b) != a] <=> a - b < 0
        it.emplace(new Operation(OP_FSUB, NOP_REGISTER, comp->getFirstArg(), comp->assertArgument(1), comp->conditional,
            SetFlag::SET_FLAGS));
        it.nextInBlock();
        // true if NEGATIVE is set, otherwise false
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_NEGATIVE_SET);
    }
    else if(COMP_ORDERED_LE == comp->opCode || COMP_UNORDERED_LE == comp->opCode)
    {
        // a <= b <=> min(a, b) == a [<=> max(a, b) == b]
        it.emplace(new Operation(
            OP_FMIN, tmp, comp->getFirstArg(), comp->assertArgument(1), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it.emplace(
            new Operation(OP_XOR, NOP_REGISTER, tmp, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_SET);
    }
    else if(COMP_ORDERED == comp->opCode)
    {
        // ord(a, b) <=> a != NaN && b != NaN
        const Value tmp0 = method.addNewLocal(TYPE_BOOL, "%ordered");
        const Value tmp1 = method.addNewLocal(TYPE_BOOL, "%ordered");
        // tmp0 = a != NaN
        it.emplace(new Operation(OP_XOR, tmp0, comp->getFirstArg(), FLOAT_NAN, comp->conditional));
        it.nextInBlock();
        // tmp1 = b != NaN
        it.emplace(new Operation(OP_XOR, tmp1, comp->assertArgument(1), FLOAT_NAN, comp->conditional));
        it.nextInBlock();
        // tmp = tmp0 && tmp1
        it.emplace(new Operation(OP_AND, tmp, tmp0, tmp1, comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        // res = tmp != 0 <=> (tmp0 && tmp1) == 0 <=> (a != NaN) && (b != NaN)
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_CLEAR);
    }
    else if(COMP_UNORDERED == comp->opCode)
    {
        // uno(a, b) <=> a == NaN || b == NaN
        const Value tmp0 = method.addNewLocal(TYPE_BOOL, "%ordered");
        const Value tmp1 = method.addNewLocal(TYPE_BOOL, "%ordered");
        // tmp0 = a != NaN
        it.emplace(new Operation(OP_XOR, tmp0, comp->getFirstArg(), FLOAT_NAN, comp->conditional));
        it.nextInBlock();
        // tmp1 = b != NaN
        it.emplace(new Operation(OP_XOR, tmp1, comp->assertArgument(1), FLOAT_NAN, comp->conditional));
        it.nextInBlock();
        // tmp = tmp0 && tmp1
        it.emplace(new Operation(OP_AND, tmp, tmp0, tmp1, comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        // res = tmp == 0 <=> (tmp0 || tmp1) == 0 <=> (a != NaN) || (b != NaN) == 0 <=> (a == NaN) || (b == NaN)
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_SET);
    }
    else
        throw CompilationError(CompilationStep::OPTIMIZER, "Unrecognized floating-point comparison", comp->opCode);

    return it;
}

static void swapComparisons(const std::string& opCode, Comparison* comp)
{
    Value tmp = comp->getFirstArg();
    comp->setArgument(0, comp->assertArgument(1));
    comp->setArgument(1, tmp);
    comp->opCode = opCode;
}

InstructionWalker intermediate::intrinsifyComparison(Method& method, InstructionWalker it)
{
    Comparison* comp = it.get<Comparison>();
    if(comp == nullptr)
    {
        return it;
    }
    logging::debug() << "Intrinsifying comparison '" << comp->opCode << "' to arithmetic operations" << logging::endl;
    bool isFloating = comp->getFirstArg().type.isFloatingType();
    bool negateResult = false;
    if(!isFloating)
    {
        // simplification, map all unequal comparisons to "equals" and "less-then"
        if(COMP_NEQ == comp->opCode)
        {
            // a != b == !(a == b)
            negateResult = true;
            const_cast<std::string&>(comp->opCode) = COMP_EQ;
        }
        else if(COMP_UNSIGNED_GE == comp->opCode)
        {
            // a >= b -> !(a < b)
            const_cast<std::string&>(comp->opCode) = COMP_UNSIGNED_LT;
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
            const_cast<std::string&>(comp->opCode) = COMP_SIGNED_LT;
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

        it = intrinsifyIntegerRelation(method, it, comp, negateResult);
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

        it = intrinsifyFloatingRelation(method, it, comp);
    }

    return it;
}

InstructionWalker intermediate::insertIsNegative(InstructionWalker it, const Value& src, Value& dest)
{
    if(src.getLiteralValue())
    {
        dest = src.getLiteralValue()->signedInt() < 0 ? INT_MINUS_ONE : INT_ZERO;
    }
    else if(src.hasContainer())
    {
        dest = Value(ContainerValue(src.container().elements.size()), TYPE_BOOL);
        for(const auto& elem : src.container().elements)
        {
            if(elem.getLiteralValue())
                dest.container().elements.push_back(elem.getLiteralValue()->signedInt() < 0 ? INT_MINUS_ONE : INT_ZERO);
            else
                throw CompilationError(CompilationStep::OPTIMIZER, "Can't handle container with non-literal values",
                    src.to_string(false, true));
        }
    }
    else if(src.getSingleWriter() != nullptr &&
        src.getSingleWriter()->hasDecoration(InstructionDecorations::UNSIGNED_RESULT))
    {
        // the value is set to be unsigned, so it cannot be negative
        dest = INT_ZERO;
    }
    else
    {
        if(dest.isUndefined() || !dest.isWriteable())
            throw CompilationError(CompilationStep::GENERAL, "Cannot write into this value", dest.to_string(true));
        it.emplace(new Operation(
            OP_ASR, dest, src, Value(Literal(static_cast<uint32_t>(TYPE_INT32.getScalarBitCount() - 1)), TYPE_INT32)));
        it.nextInBlock();
    }
    return it;
}
