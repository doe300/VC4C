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
        assign(it, dest) = (value, trueCode);
        it.reset(new Operation(OP_XOR, dest, value, value, trueCode.invert()));
    }
    return it;
}

InstructionWalker intrinsifyIntegerRelation(
    Method& method, InstructionWalker it, const Comparison* comp, bool invertResult)
{
    // http://llvm.org/docs/LangRef.html#icmp-instruction
    const Value tmp = method.addNewLocal(comp->getFirstArg().type, "%icomp");
    auto boolType = TYPE_BOOL.toVectorType(comp->getFirstArg().type.getVectorWidth());
    if(COMP_EQ == comp->opCode)
    {
        // a == b <=> a xor b == 0 [<=> a - b == 0]
        // a != b <=> a xor b != 0
        if(comp->getFirstArg().hasLocal() && comp->assertArgument(1).hasLiteral(Literal(0u)))
            // special case for a == 0
            // does not save instructions, but does not force value a to be on register-file A (since B is reserved for
            // literal 0)
            assign(it, NOP_REGISTER) = (comp->getFirstArg(), SetFlag::SET_FLAGS);
        else
            assign(it, NOP_REGISTER) = (comp->getFirstArg() ^ comp->assertArgument(1), SetFlag::SET_FLAGS);
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
                // this is actually pre-calculated, so no insertion is performed
                Value mask = assign(it, comp->assertArgument(1).type) = comp->assertArgument(1) - 1_val;
                Value tmp1 = assign(it, boolType, "%icomp") = comp->getFirstArg() & mask;

                assign(it, NOP_REGISTER) = (tmp1 ^ comp->getFirstArg(), SetFlag::SET_FLAGS);
                // we set to true, if flag is zero, false otherwise
                invertResult = !invertResult;
            }
            // a < b [b = 2^x -1] <=> (a & b) == a && a != b <=> (a & ~b) == 0 && a != b (this version is used)!
            // <=> (a >> log2(b + 1)) == 0 && a != b
            else if(comp->assertArgument(1).hasLiteral() &&
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
                // XXX optimize? combine both comparisons of upper half? can short-circuit on ||?
                // TODO the general case seems to be wrong
                /*
                 * Another way of calculating ult:
                 * a < b <=> clz(a) > clz(b) || (clz(a) == clz(b) && max(a, b) != a)
                 * For equal MSB, ult == slt, special case for different MSB
                 */
                auto partType = TYPE_INT16.toVectorType(comp->getFirstArg().type.getVectorWidth());
                const Value tmp1 = method.addNewLocal(boolType, "%icomp");
                const Value tmp2 = method.addNewLocal(boolType, "%icomp");
                const Value tmp3 = method.addNewLocal(boolType, "%icomp");
                const Value tmp4 = method.addNewLocal(boolType, "%icomp");
                Value leftUpper = assign(it, partType, "%comp.left") = comp->getFirstArg() >> 16_val;
                Value leftLower = assign(it, partType, "%comp.left") = comp->getFirstArg() & 0xFFFF_val;
                Value rightUpper = assign(it, partType, "%comp.right") = comp->assertArgument(1) >> 16_val;
                Value rightLower = assign(it, partType, "%comp.right") = comp->assertArgument(1) & 0xFFFF_val;
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
                assign(it, tmp4) = tmp2 & tmp3;
                assign(it, NOP_REGISTER) = (tmp1 | tmp4, SetFlag::SET_FLAGS);
            }
        }
        else
        {
            assign(it, tmp) = (max(comp->getFirstArg(), comp->assertArgument(1)));
            assign(it, NOP_REGISTER) = (tmp ^ comp->getFirstArg(), SetFlag::SET_FLAGS);
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
        assign(it, tmp) = (max(firstArg, secondArg));
        assign(it, NOP_REGISTER) = (tmp ^ firstArg, SetFlag::SET_FLAGS);
        it = replaceWithSetBoolean(it, comp->getOutput().value(), invertResult ? COND_ZERO_SET : COND_ZERO_CLEAR);
    }
    else
        throw CompilationError(CompilationStep::OPTIMIZER, "Unrecognized integer comparison", comp->opCode);

    return it;
}

static NODISCARD std::pair<InstructionWalker, Value> insertCheckForNaN(
    Method& method, InstructionWalker it, const Comparison* comp)
{
    const Value firstArgNaN = method.addNewLocal(TYPE_BOOL, "%nan_check");
    it.emplace(new Operation(OP_XOR, firstArgNaN, comp->getFirstArg(), FLOAT_NAN));
    it.nextInBlock();
    Value eitherNaN = firstArgNaN;
    if(comp->getArguments().size() > 1)
    {
        Value secondArgNaN = assign(it, TYPE_BOOL, "%nan_check") = (comp->assertArgument(1) ^ FLOAT_NAN);
        eitherNaN = assign(it, TYPE_BOOL, "%nan_check") = (firstArgNaN || secondArgNaN);
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
        it.reset(new MoveOperation(comp->getOutput().value(), BOOL_TRUE, COND_ALWAYS, SetFlag::SET_FLAGS));
    }
    else if(COMP_FALSE == comp->opCode)
    {
        // false
        it.reset(new MoveOperation(comp->getOutput().value(), BOOL_FALSE, COND_ALWAYS, SetFlag::SET_FLAGS));
    }
    else if(COMP_ORDERED_EQ == comp->opCode || COMP_UNORDERED_EQ == comp->opCode)
    {
        // a == b <=> a xor b == 0 [<=> a - b == 0]
        assign(it, NOP_REGISTER) = (comp->getFirstArg() ^ comp->assertArgument(1), COND_ALWAYS, SetFlag::SET_FLAGS);
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_SET);
    }
    else if(COMP_ORDERED_NEQ == comp->opCode || COMP_UNORDERED_NEQ == comp->opCode)
    {
        // a != b <=> a xor b != 0
        assign(it, NOP_REGISTER) = (comp->getFirstArg() ^ comp->assertArgument(1), COND_ALWAYS, SetFlag::SET_FLAGS);
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_CLEAR);
    }
    else if(COMP_ORDERED_LT == comp->opCode || COMP_UNORDERED_LT == comp->opCode)
    {
        // a < b [<=> min(a, b) != b] [<=> max(a, b) != a] <=> a - b < 0
        assign(it, NOP_REGISTER) = (comp->getFirstArg() - comp->assertArgument(1), COND_ALWAYS, SetFlag::SET_FLAGS);
        // true if NEGATIVE is set, otherwise false
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_NEGATIVE_SET);
    }
    else if(COMP_ORDERED_LE == comp->opCode || COMP_UNORDERED_LE == comp->opCode)
    {
        // a <= b <=> min(a, b) == a [<=> max(a, b) == b]
        assign(it, tmp) = (min(comp->getFirstArg(), comp->assertArgument(1)), COND_ALWAYS, SetFlag::SET_FLAGS);
        assign(it, NOP_REGISTER) = (tmp ^ comp->getFirstArg(), COND_ALWAYS, SetFlag::SET_FLAGS);
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_SET);
    }
    else if(COMP_ORDERED == comp->opCode)
    {
        // ord(a, b) <=> a != NaN && b != NaN
        // tmp0 = a != NaN
        Value tmp0 = assign(it, TYPE_BOOL, "%ordered") = (comp->getFirstArg() ^ FLOAT_NAN);
        // tmp1 = b != NaN
        Value tmp1 = assign(it, TYPE_BOOL, "%ordered") = (comp->assertArgument(1) ^ FLOAT_NAN);
        // tmp = tmp0 && tmp1
        assign(it, tmp) = (tmp0 && tmp1, SetFlag::SET_FLAGS);
        // res = tmp != 0 <=> (tmp0 && tmp1) == 0 <=> (a != NaN) && (b != NaN)
        it = replaceWithSetBoolean(it, comp->getOutput().value(), COND_ZERO_CLEAR);
    }
    else if(COMP_UNORDERED == comp->opCode)
    {
        // uno(a, b) <=> a == NaN || b == NaN
        // tmp0 = a != NaN
        Value tmp0 = assign(it, TYPE_BOOL, "%ordered") = (comp->getFirstArg() ^ FLOAT_NAN);
        // tmp1 = b != NaN
        Value tmp1 = assign(it, TYPE_BOOL, "%ordered") = (comp->assertArgument(1) ^ FLOAT_NAN);
        // tmp = tmp0 && tmp1
        assign(it, tmp) = (tmp0 && tmp1, SetFlag::SET_FLAGS);
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
    if(comp->hasConditionalExecution())
        throw CompilationError(
            CompilationStep::OPTIMIZER, "Comparisons cannot have conditional execution", comp->to_string());
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
