/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Operators.h"

#include "../Module.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "../periphery/SFU.h"
#include "Comparisons.h"
#include "log.h"

#include <bitset>
#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

// TODO optimize "exact" divisions. How? no need to calculate remainder?!
// first need to find a case where the flag is set!

// see VC4CLStdLib (_intrinsics.h)
static constexpr unsigned char VC4CL_UNSIGNED{1};

InstructionWalker intrinsics::intrinsifySignedIntegerMultiplication(
    Method& method, InstructionWalker it, IntrinsicOperation& op)
{
    /*
     * For 2's-complement, the unsigned and signed multiplication is identical
     *
     * See LLVM language reference for mul operation:
     * "Because LLVM integers use a two’s complement representation, and the result is the same width as the operands,
     * this instruction returns the correct result for both signed and unsigned integers." -
     * https://llvm.org/docs/LangRef.html#mul-instruction
     *
     * Similarly, for the Mesa VC4 driver, smul is directly mapped to umul, which is implemented identically to the
     * unsigned integer multiplication below, see
     * https://gitlab.freedesktop.org/mesa/mesa/blob/master/src/gallium/drivers/vc4/vc4_program.c (function
     * ntq_emit_alu)
     */
    it = intrinsifyUnsignedIntegerMultiplication(method, it, op);
    it->decoration = remove_flag(it->decoration, InstructionDecorations::UNSIGNED_RESULT);
    return it;
}

InstructionWalker intrinsics::intrinsifyUnsignedIntegerMultiplication(
    Method& method, InstructionWalker it, IntrinsicOperation& op)
{
    const Value& arg0 = op.getFirstArg();
    const Value& arg1 = op.getSecondArg().value_or(UNDEFINED_VALUE);

    // mul24 can multiply 24-bits * 24-bits into 32-bits
    // default case, full multiplication
    // NOTE: the instructions are ordered in a way, that the insertion of NOPs to split read-after-write is minimal
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying unsigned multiplication of integers" << logging::endl);

    /*
     *                             | a[0] .        a[1]        |
     *  *                          | b[0] .        b[1]        |
     * ---------------------------------------------------------
     * |xxxxxx.xxxxxx.xxxxxx.xxxxxx|      .      .      .      |
     *
     *                             |        a[1] * b[1]        |
     *   +    |        a[1] * b[0]        |
     *   +    |        a[0] * b[1]        |
     *
     */
    // the following code is shamelessly taken from mesa3D
    // original: https://gitlab.freedesktop.org/mesa/mesa/blob/master/src/gallium/drivers/vc4/vc4_program.c (function
    // ntq_umul), created by Eric Anholt

    // split arguments into parts
    auto outputType = op.getOutput()->type;
    Value arg0Hi = assign(it, outputType, "%mul.arg0Hi") = as_unsigned{arg0} >> 24_val;
    Value arg1Hi = assign(it, outputType, "%mul.arg1Hi") = as_unsigned{arg1} >> 24_val;

    Value resHiLo = assign(it, outputType, "%mul.resHiLo") = mul24(arg0Hi, arg1);
    Value resLoHi = assign(it, outputType, "%mul.resLoHi") = mul24(arg0, arg1Hi);
    Value resLo = assign(it, outputType, "%mul.resLo") = mul24(arg0, arg1);
    Value resTmp = assign(it, outputType) = resHiLo + resLoHi;
    Value resHi = assign(it, outputType, "%mul.resHi") = resTmp << 24_val;

    it.reset(new Operation(OP_ADD, op.getOutput().value(), resLo, resHi));
    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);

    return it;
}

InstructionWalker intrinsics::intrinsifyIntegerToLongMultiplication(
    Method& method, InstructionWalker it, const MethodCall* call, Optional<Value> lowResult)
{
    auto arg0 = call->assertArgument(0);
    auto arg1 = call->assertArgument(1);
    bool isUnsigned = call->getArgument(2) && call->assertArgument(2).getLiteralValue() &&
        call->assertArgument(2).getLiteralValue()->unsignedInt() == VC4CL_UNSIGNED;

    // Similar to the Mesa implementation in
    // https://gitlab.freedesktop.org/mesa/mesa/blob/master/src/compiler/nir/nir_lower_alu.c (function lower_alu_instr,
    // case nir_op_imul_high), we need to do the absolute value and reapplication of the sign for signed mul_hi

    if(!isUnsigned)
    {
        auto minusArg0 = assign(it, arg0.type, "%mul_hi.arg0Abs") = -as_signed{arg0};
        arg0 = assign(it, arg0.type, "%mul_hi.arg0Abs") = max(as_signed{arg0}, as_signed{minusArg0});
        auto minusArg1 = assign(it, arg1.type, "%mul_hi.arg1Abs") = -as_signed{arg1};
        arg1 = assign(it, arg1.type, "%mul_hi.arg1Abs") = max(as_signed{arg1}, as_signed{minusArg1});
    }

    /*
     *                               |    a[0]     .    a[1]     |
     *    *                          |    b[0]     .    b[1]     |
     *   ---------------------------------------------------------
     *   |      .      .      .      |xxxxxx.xxxxxx.xxxxxx.xxxxxx|
     *
     *                               |        a[1] * b[1]        |
     * +               |        a[1] * b[0]        |
     * +               |        a[0] * b[1]        |
     * + |        a[0] * b[0]        |
     *
     */
    // see #intrinsifyUnsignedIntegerMultiplication for comments on the implementation and signed/unsigned
    // (non-)distinction
    // NOTE: in contrast to the normal 32-bit multiplication, we need to take 16-bit portions, since for 24-bit
    // multiplication, we lose the upper 16 bits (2 * 24 = 48 > 32)!
    Value lowTmp = UNDEFINED_VALUE;

    auto outputType = call->getOutput()->type;
    auto arg0Hi = assign(it, outputType, "%mul_hi.arg0Hi") = as_unsigned{arg0} >> 16_val;
    auto arg1Hi = assign(it, outputType, "%mul_hi.arg1Hi") = as_unsigned{arg1} >> 16_val;
    auto arg0Lo = assign(it, outputType, "%mul_hi.arg0Lo") = arg0 & 0xFFFF_val;
    auto arg1Lo = assign(it, outputType, "%mul_hi.arg1Lo") = arg1 & 0xFFFF_val;

    auto resHiLo = assign(it, outputType, "%mul_hi.resHiLo") = mul24(arg0Hi, arg1Lo);
    auto resLoHi = assign(it, outputType, "%mul_hi.resLoHi") = mul24(arg0Lo, arg1Hi);
    auto resLo = assign(it, outputType, "%mul_hi.resLo") = mul24(arg0Lo, arg1Lo);
    if(lowResult)
        lowTmp = assign(it, outputType) = resLo;
    auto resOverflow = assign(it, outputType, "%mul_hi.of") = 0_val;
    resLo = assign(it, outputType, "%mul_hi.resLo") = as_unsigned{resLo} >> 16_val;
    auto resMid = assign(it, outputType, "%mul_hi.resMid") = (resHiLo + resLoHi, SetFlag::SET_FLAGS);
    if(lowResult)
    {
        auto tmp = assign(it, outputType) = resMid << 16_val;
        assign(it, *lowResult) = lowTmp + tmp;
    }
    assign(it, resOverflow) = (resOverflow + 1_val, COND_CARRY_SET);
    resMid = assign(it, outputType, "%mul_hi.resMid") = (resMid + resLo, SetFlag::SET_FLAGS);
    assign(it, resOverflow) = (resOverflow + 1_val, COND_CARRY_SET);
    resMid = assign(it, outputType, "%mul_hi.resMid") = as_unsigned{resMid} >> 16_val;
    resOverflow = assign(it, outputType, "%mul_hi.of") = resOverflow << 16_val;
    resMid = assign(it, outputType, "%mul_hi.resMid") = resMid + resOverflow;

    auto resHi = assign(it, outputType, "%mul_hi.resHi") = mul24(arg0Hi, arg1Hi);

    if(!isUnsigned)
    {
        auto out = assign(it, outputType, "%mul_hi.out") = resMid + resHi;
        auto arg0Neg = assign(it, arg0.type, "%mul_hi.arg0Sign") = as_signed{call->assertArgument(0)} >> 31_val;
        auto arg1Neg = assign(it, arg1.type, "%mul_hi.arg1Sign") = as_signed{call->assertArgument(1)} >> 31_val;
        auto sign = assign(it, outputType, "%mul_hi.sign") = arg0Neg ^ arg1Neg;
        /*
         * This is a short-circuit of (sign & (-out - 1)) | (~sign & out) with
         * - out = the unsigned result and
         * - sign = ((x >> 31) ^ (y >> 31)) (either sign set)
         *
         * (sign & (-out - 1)) | (~sign & out)
         * <=> (sign & ~out) | (~sign & out)
         * <=> sign ^ out
         */
        out = assign(it, outputType, "%mul_hi.out") = sign ^ out;
        it.reset((new MoveOperation(call->getOutput().value(), std::move(out)))->copyExtrasFrom(call));
    }
    else
    {
        it.reset(new Operation(OP_ADD, call->getOutput().value(), resMid, resHi));
        it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    }

    return it;
}

/*
 * "The number of elementary operations q is the number of 1’s in the binary expansion, minus 1"
 * note: q is a shift and an addition
 *
 * - https://www.clear.rice.edu/comp512/Lectures/Papers/Lefevre-Multiplication.pdf, page 5
 *
 * Since we use a minimum of 8 operations for unsigned multiplication (without any optimization applied),
 * we set this as the threshold for applying this optimization.
 */
static constexpr int BINARY_METHOD_OPERATIONS_THRESHOLD = 8;

bool intrinsics::canOptimizeMultiplicationWithBinaryMethod(const IntrinsicOperation& op)
{
    return std::any_of(op.getArguments().begin(), op.getArguments().end(), [](const Value& arg) -> bool {
        if(arg.getLiteralValue() && arg.getLiteralValue()->signedInt() > 0)
        {
            std::bitset<32> tmp(arg.getLiteralValue()->unsignedInt());
            return tmp.count() <= BINARY_METHOD_OPERATIONS_THRESHOLD;
        }
        return false;
    });
}

/*
 * Optimization of integer multiplication with binary method
 *
 * See: https://www.clear.rice.edu/comp512/Lectures/Papers/Lefevre-Multiplication.pdf, chapter 3
 *
 * NOTE: The constant multiplication factor needs to be unsigned!
 */
InstructionWalker intrinsics::intrinsifyIntegerMultiplicationViaBinaryMethod(
    Method& method, InstructionWalker it, IntrinsicOperation& op)
{
    auto factor = (op.getFirstArg().getLiteralValue() | op.getSecondArg()->getLiteralValue())->signedInt();
    const auto& src = op.getFirstArg().getLiteralValue() ? op.getSecondArg().value() : op.getFirstArg();

    if(factor <= 0)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Invalid factor for this multiplication optimization", op.to_string());

    // tracks the deconstruction of the factor into its parts
    std::bitset<32> deconstruct(static_cast<unsigned>(factor));
    auto highBit = [](auto u) -> unsigned {
        for(unsigned i = 0; i < 32; ++i)
        {
            if((u >> i) == 0)
                return i - 1;
        }
        return 32;
    };
    Value intermediateResult = src;
    unsigned lastHigh = highBit(deconstruct.to_ulong());
    deconstruct.reset(lastHigh);
    /*
     * 113 = 1110001 (2)
     * x * 113:
     * 3x = x << 1 + x
     * 7x = 3x << 1 + 1
     * 113x = 7x << 4 + 1
     */
    while(deconstruct.any())
    {
        unsigned nextHigh = highBit(deconstruct.to_ulong());

        // shift by the difference between the high bits
        auto tmp = assign(it, src.type, "%mul_shift") = intermediateResult
            << Value(Literal(lastHigh - nextHigh), TYPE_INT8);
        // add the value for this high bit
        auto tmp2 = assign(it, src.type, "%mul_add") = tmp + src;

        intermediateResult = tmp2;
        deconstruct.reset(nextHigh);
        lastHigh = nextHigh;
    }

    if(lastHigh != 0)
    {
        // insert last shift
        it.reset(
            (new Operation(OP_SHL, op.getOutput().value(), intermediateResult, Value(Literal(lastHigh), TYPE_INT8)))
                ->copyExtrasFrom(&op));
    }
    else
    {
        it.reset((new MoveOperation(op.getOutput().value(), intermediateResult))->copyExtrasFrom(&op));
    }

    return it;
}

/*
 * Sources/Info:
 * - http://ipa.ece.illinois.edu/mif/pubs/web-only/Frank-RawMemo12-1999.html
 * - http://flounder.com/multiplicative_inverse.htm
 */

InstructionWalker intrinsics::intrinsifySignedIntegerDivision(
    Method& method, InstructionWalker it, IntrinsicOperation& op, const bool useRemainder)
{
    Value opDest = op.getOutput().value();
    // check any operand is negative
    Value op1Sign = UNDEFINED_VALUE;
    Value op2Sign = UNDEFINED_VALUE;

    // convert operands to positive
    Value op1Pos = method.addNewLocal(op.assertArgument(0).type, "%unsigned");
    Value op2Pos = method.addNewLocal(op.assertArgument(0).type, "%unsigned");

    it = insertMakePositive(it, method, op.assertArgument(0), op1Pos, op1Sign);
    it = insertMakePositive(it, method, op.assertArgument(1), op2Pos, op2Sign);

    op.setArgument(0, std::move(op1Pos));
    op.setArgument(1, std::move(op2Pos));

    // use new temporary result, so we can store the final result in the correct value
    const Value tmpDest = method.addNewLocal(opDest.type, "%result");
    op.setOutput(tmpDest);

    // calculate unsigned division
    it = intrinsifyUnsignedIntegerDivision(method, it, op, useRemainder);
    it.nextInBlock();

    if(op1Sign.hasLiteral(0_lit) && op2Sign.hasLiteral(0_lit))
    {
        // if both operands are marked with (unsigned), we don't need to invert the result
        it.emplace(new MoveOperation(opDest, tmpDest));
        it.nextInBlock();
    }
    else if(useRemainder)
    {
        // For signed remainder (srem), the results sign only depends on the sign of the dividend!
        return insertRestoreSign(it, method, tmpDest, opDest, op1Sign);
    }
    else
    {
        // if exactly one operand was negative, invert sign of result
        Value eitherSign = assign(it, op1Sign.type) = op1Sign ^ op2Sign;
        return insertRestoreSign(it, method, tmpDest, opDest, eitherSign);
    }
    return it;
}

InstructionWalker intrinsics::intrinsifyUnsignedIntegerDivision(
    Method& method, InstructionWalker it, IntrinsicOperation& op, const bool useRemainder)
{
    // https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_.28unsigned.29_with_remainder
    // see also: https://www.microsoft.com/en-us/research/wp-content/uploads/2008/08/tr-2008-141.pdf
    // TODO for |type| < 24, use floating-point division??
    // NOTE: the instructions are ordered in a way, that the insertion of NOPs to split read-after-write is minimal
    const Value& numerator = op.getFirstArg();
    const Value& divisor = op.getSecondArg().value_or(UNDEFINED_VALUE);

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying division of unsigned integers" << logging::endl);

    // TODO divisor = 0 handling!

    // Q := 0                 -- initialize quotient and remainder to zero
    // R := 0
    // set explicitly to zero
    Value quotient = 0_val;
    Value remainder = 0_val;

    // for i := n ? 1 ... 0 do     -- where n is number of bits in N
    // "number of bits in N" as in in the numerator, not in the type!!
    // but since the leading bits in the numerator are all zero, including them does not modify the result
    for(int i = numerator.type.getScalarBitCount() - 1; i >= 0; --i)
    {
        // R := R << 1          -- left-shift R by 1 bit
        remainder = assign(it, op.getOutput()->type, "%udiv.remainder") = remainder << 1_val;
        // R(0) := N(i)         -- set the least-significant bit of R equal to bit i of the numerator
        // R = R | ((N >> i) & 1) <=> R = R | (N & (1 << i) == 1 ? 1 : 0) <=> R = R | 1, if N & (1 << i) != 0
        {
            Value tmp = assign(it, numerator.type, "%udiv.tmp") =
                as_unsigned{numerator} >> Value(Literal(i), TYPE_INT32);
            Value tmp2 = assign(it, tmp.type, "%udiv.tmp") = tmp & 1_val;
            // XXX actually OP_OR, but it gets somehow removed/optimized away
            remainder = assign(it, op.getOutput()->type, "%udiv.remainder") = remainder + tmp2;
        }
        // if R >= D then
        // R = R >= D ? R - D : R <=> R = R - D >= 0 ? R - D : R <=> R = R - D < 0 ? R : R - D
        ConditionCode newRemainderCond = COND_ALWAYS;
        {
            Value tmp = UNDEFINED_VALUE;
            // TODO need to optimize!!
            // XXX can this be skipped if numerator and divisor are known to not have the high-bit set??
            if(numerator.type.getScalarBitCount() == 32)
            {
                // need some special treatment here, since unsigned (R - D) <=> 0 is not correct, since - sets flags for
                // signed arithmetic. => Use more complex unsigned comparison functions, see Comparisons.cpp, block
                // which handles COMP_UNSIGNED_LT

                // (R >= D) == !(R < D)
                assign(it, NOP_REGISTER) = (remainder ^ divisor, SetFlag::SET_FLAGS);
                Value unsignedMax = method.addNewLocal(remainder.type, "%icomp");
                assign(it, unsignedMax) = (min(remainder, divisor), COND_NEGATIVE_SET);
                assign(it, unsignedMax) = (max(remainder, divisor), COND_NEGATIVE_CLEAR);
                assign(it, NOP_REGISTER) = (unsignedMax ^ remainder, SetFlag::SET_FLAGS);

                tmp = assign(it, op.getOutput()->type, "%udiv.tmp") = (remainder - divisor);
                newRemainderCond = COND_ZERO_SET;
            }
            else
            {
                tmp = assign(it, op.getOutput()->type, "%udiv.tmp") = (remainder - divisor, SetFlag::SET_FLAGS);
                newRemainderCond = COND_NEGATIVE_CLEAR;
            }
            Value newRemainder = method.addNewLocal(op.getOutput()->type, "%udiv.remainder");
            assign(it, newRemainder) = (tmp, newRemainderCond);
            assign(it, newRemainder) = (remainder, newRemainderCond.invert());
            remainder = newRemainder;
        }
        // Q(i) := 1
        {
            Value newQuotient = method.addNewLocal(op.getOutput()->type, "%udiv.quotient");
            assign(it, newQuotient) =
                (quotient | Value(Literal(static_cast<int32_t>(1) << i), TYPE_INT32), newRemainderCond);
            // else Q(new) := Q(old)
            assign(it, newQuotient) = (quotient, newRemainderCond.invert());
            quotient = newQuotient;
        }
    }

    // make move from original instruction

    if(useRemainder)
        it.reset(new MoveOperation(op.getOutput().value(), remainder));
    else
        it.reset(new MoveOperation(op.getOutput().value(), quotient));
    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);

    return it;
}

InstructionWalker intrinsics::intrinsifySignedIntegerDivisionByConstant(
    Method& method, InstructionWalker it, IntrinsicOperation& op, bool useRemainder)
{
    /*
     * Conversion between signedness is taken from:
     * https://llvm.org/doxygen/IntegerDivision_8cpp_source.html
     */

    Value opDest = op.getOutput().value();
    // check any operand is negative
    Value op1Sign = UNDEFINED_VALUE;
    Value op2Sign = UNDEFINED_VALUE;

    // convert operands to positive
    Value op1Pos = method.addNewLocal(op.assertArgument(0).type, "%unsigned");
    Value op2Pos = method.addNewLocal(op.assertArgument(0).type, "%unsigned");

    it = insertMakePositive(it, method, op.assertArgument(0), op1Pos, op1Sign);
    it = insertMakePositive(it, method, op.assertArgument(1), op2Pos, op2Sign);

    op.setArgument(0, std::move(op1Pos));
    op.setArgument(1, std::move(op2Pos));

    // use new temporary result, so we can store the final result in the correct value
    const Value tmpDest = method.addNewLocal(opDest.type, "%result");
    op.setOutput(tmpDest);

    // calculate unsigned division
    it = intrinsifyUnsignedIntegerDivisionByConstant(method, it, op, useRemainder);
    it.nextInBlock();

    if(op1Sign.hasLiteral(0_lit) && op2Sign.hasLiteral(0_lit))
    {
        // if both operands are marked with (unsigned), we don't need to invert the result
        assign(it, opDest) = tmpDest;
    }
    else if(useRemainder)
    {
        // For signed remainder (srem), the results sign only depends on the sign of the dividend!
        return insertRestoreSign(it, method, tmpDest, opDest, op1Sign);
    }
    else
    {
        // if exactly one operand was negative, invert sign of result
        Value eitherSign = assign(it, op1Sign.type) = op1Sign ^ op2Sign;
        return insertRestoreSign(it, method, tmpDest, opDest, eitherSign);
    }
    return it;
}

static std::pair<Literal, Literal> calculateConstant(Literal divisor, unsigned accuracy)
{
    // See OpenCL 1.2 specification, section 6.2:
    // "A divide by zero with integer types does not cause an exception but will result in an unspecified value."
    // -> just return a dummy value. This also prevents UBSAN errors
    if(divisor.isUndefined() || divisor.unsignedInt() == 0)
        // a multiplication by 1 and a shift by 0 should both be able to be eliminated
        return std::make_pair(Literal(1), Literal(0));

    uint32_t shift = static_cast<uint32_t>(std::log2(divisor.unsignedInt() * accuracy)) + 2;
    uint32_t factor =
        static_cast<uint32_t>(std::round(std::pow(2.0f, shift) / static_cast<double>(divisor.unsignedInt())));
    if(shift > 31)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Unsigned division by constant generated invalid shift offset", std::to_string(shift));
    if(factor >= std::numeric_limits<uint16_t>::max())
        throw CompilationError(CompilationStep::NORMALIZER,
            "Unsigned division by constant generated invalid multiplication factor", std::to_string(factor));
    return std::make_pair(Literal(factor), Literal(shift));
}

static std::pair<Value, Value> calculateConstant(Module& module, const Value& divisor, unsigned accuracy)
{
    if(auto vector = divisor.checkVector())
    {
        SIMDVector factors;
        SIMDVector shifts;
        for(unsigned i = 0; i < vector->size(); ++i)
        {
            std::tie(factors[i], shifts[i]) = calculateConstant((*vector)[i], accuracy);
        }
        return std::make_pair(
            module.storeVector(std::move(factors), divisor.type), module.storeVector(std::move(shifts), divisor.type));
    }

    const Literal div = divisor.getLiteralValue().value();
    auto tmp = calculateConstant(div, accuracy);
    return std::make_pair(Value(tmp.first, divisor.type), Value(tmp.second, divisor.type));
}

InstructionWalker intrinsics::intrinsifyUnsignedIntegerDivisionByConstant(
    Method& method, InstructionWalker it, IntrinsicOperation& op, bool useRemainder)
{
    /*
     * Taken from here:
     * http://forums.parallax.com/discussion/114807/fast-faster-fastest-code-integer-division
     *
     * If we accept unsigned char and short values, the maximum values for the numerator/denominator are USHORT_MAX
     * (65536). Thus, for the multiplication not to overflow for any numerator, the maximum value for the factor can be
     * USHORT_MAX - 1.
     */

    if(op.getFirstArg().type.getScalarBitCount() > 16)
        throw CompilationError(CompilationStep::NORMALIZER, "Division by constant may overflow for argument type",
            op.getFirstArg().type.to_string());
    if(!(op.getSecondArg() & &Value::isLiteralValue) && !(op.getSecondArg() & &Value::checkVector))
        throw CompilationError(CompilationStep::NORMALIZER, "Can only optimize division by constant", op.to_string());

    /*
     * Relative accuracy, the value is determined by experiment:
     * - values <= 16000 trigger value mismatch to "exact" division
     * - values >= 16500 trigger overflow in multiplication with factor or shifts of >= 32 positions
     */
    static constexpr unsigned accuracy = 16100;
    auto constants = calculateConstant(method.module, op.assertArgument(1), accuracy);
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Intrinsifying unsigned division by " << op.assertArgument(1).to_string(false, true)
            << " by multiplication with " << constants.first.to_string(false, true) << " and right-shift by "
            << constants.second.to_string(false, true) << logging::endl);

    Value tmp = assign(it, op.getFirstArg().type, "%udiv") = mul24(op.getFirstArg(), constants.first);
    const Value divOut = method.addNewLocal(op.getFirstArg().type, "%udiv");
    it.emplace(new Operation(OP_SHR, divOut, tmp, constants.second));
    it->copyExtrasFrom(&op);
    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    it.nextInBlock();
    // the original version has an error, which returns a too small value for exact multiples of the denominator, the
    // next lines fix this error
    Value tmpFix0 = assign(it, op.getFirstArg().type, "%udiv.fix") = mul24(divOut, op.assertArgument(1));
    Value tmpFix1 = assign(it, op.getFirstArg().type, "%udiv.fix") = op.getFirstArg() - tmpFix0;
    assign(it, NOP_REGISTER) = (op.assertArgument(1) - tmpFix1, SetFlag::SET_FLAGS);
    const Value finalResult =
        useRemainder ? method.addNewLocal(op.getFirstArg().type, "%udiv.result") : op.getOutput().value();
    assign(it, finalResult) = (divOut, InstructionDecorations::UNSIGNED_RESULT);
    assign(it, finalResult) = (divOut + 1_val, COND_NEGATIVE_SET, InstructionDecorations::UNSIGNED_RESULT);
    assign(it, finalResult) = (divOut + 1_val, COND_ZERO_SET, InstructionDecorations::UNSIGNED_RESULT);

    if(useRemainder)
    {
        // x mod y = x - (x/y) * y;
        Value tmpMul = assign(it, op.getFirstArg().type, "%udiv.remainder") = mul24(finalResult, op.assertArgument(1));
        // replace original division
        it.reset(new Operation(OP_SUB, op.getOutput().value(), op.getFirstArg(), tmpMul));
        it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    }
    else
    {
        // erase original division
        it.erase();
        // so next instruction is not skipped
        it.previousInBlock();
    }
    return it;
}

InstructionWalker intrinsics::intrinsifyFloatingDivision(Method& method, InstructionWalker it, IntrinsicOperation& op)
{
    /*
     * https://dspace.mit.edu/bitstream/handle/1721.1/80133/43609668-MIT.pdf
     * https://en.wikipedia.org/wiki/Division_algorithm#Newton.E2.80.93Raphson_division
     * http://www.rfwireless-world.com/Tutorials/floating-point-tutorial.html
     */
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying floating-point division" << logging::endl);

    const Value nominator = op.getFirstArg();
    const Value& divisor = op.assertArgument(1);
    auto outputType = op.getOutput()->type;

    ////
    // Newton-Raphson
    ////
    // TODO: "The Newton-Raphson  algorithm [...] is commonly used if the result does not require proper rounding"
    //-> use Goldschmidt??

    // 1. initialization step: P0 = SFU_RECIP(D)
    /*
     * The GLSL shader uses the SFU_RECIP with a Newton-Raphson step "to improve our approximation",
     * see http://anholt.livejournal.com/49474.html
     */
    it = periphery::insertSFUCall(REG_SFU_RECIP, it, divisor);
    Value P0 = assign(it, outputType, "%fdiv_recip") = Value(REG_SFU_OUT, TYPE_FLOAT);

    // 2. iteration step: Pi+1 = Pi(2 - D * Pi)
    // run 5 iterations
    Value P1 = assign(it, outputType, "%fdiv_p1") = divisor * P0;
    Value P1_1 = assign(it, outputType, "%fdiv_p1") = 2.0_val - P1;
    Value P1_2 = assign(it, outputType, "%fdiv_p1") = P0 * P1_1;

    Value P2 = assign(it, outputType, "%fdiv_p2") = divisor * P1_2;
    Value P2_1 = assign(it, outputType, "%fdiv_p2") = 2.0_val - P2;
    Value P2_2 = assign(it, outputType, "%fdiv_p2") = P1_2 * P2_1;

    Value P3 = assign(it, outputType, "%fdiv_p3") = divisor * P2_2;
    Value P3_1 = assign(it, outputType, "%fdiv_p3") = 2.0_val - P3;
    Value P3_2 = assign(it, outputType, "%fdiv_p3") = P2_2 * P3_1;

    Value P4 = assign(it, outputType, "%fdiv_p4") = divisor * P3_2;
    Value P4_1 = assign(it, outputType, "%fdiv_p4") = 2.0_val - P4;
    Value P4_2 = assign(it, outputType, "%fdiv_p4") = P3_2 * P4_1;

    Value P5 = assign(it, outputType, "%fdiv_p5") = divisor * P4_2;
    Value P5_1 = assign(it, outputType, "%fdiv_p5") = 2.0_val - P5;
    Value P5_2 = assign(it, outputType, "%fdiv_p5") = P4_2 * P5_1;

    // TODO add a 6th step? Sometimes the float-division is too inaccurate

    // 3. final step: Q = Pn * N
    it.reset(new Operation(OP_FMUL, op.getOutput().value(), nominator, P5_2));

    return it;
}

static constexpr unsigned MSB = 31;

Literal intrinsics::asr(Literal left, Literal right)
{
    if(right.signedInt() < 0)
        throw CompilationError(CompilationStep::GENERAL, "ASR with negative numbers is not implemented");
    // Tests have shown that on VC4 all shifts (asr, shr, shl) only take the last 5 bits of the offset (modulo 32)
    auto offset = right.unsignedInt() & 0x1F;
    if((-1 >> 31u) == -1)
        // if signed right shift is arithmetic shift, then use that instead of the manual shifting
        return Literal(left.signedInt() >> offset);

    std::bitset<sizeof(int32_t) * 8> tmp(static_cast<uint64_t>(left.signedInt()));
    for(unsigned i = 0; i < offset; ++i)
    {
        bool MSBSet = tmp.test(MSB);
        tmp >>= 1;
        tmp.set(MSB, MSBSet);
    }
    return Literal(static_cast<uint32_t>(tmp.to_ulong()));
}

Literal intrinsics::clz(Literal val)
{
#ifdef __GNUC__
    // __builtin_clz(0) is undefined, so check before
    if(val.unsignedInt() != 0)
        return Literal(__builtin_clz(val.unsignedInt()));
#else
    for(int i = MSB; i >= 0; --i)
    {
        if(((val.unsignedInt() >> i) & 0x1) == 0x1)
            return Literal(static_cast<int32_t>(MSB) - i);
    }
#endif
    // Tests show that VC4 returns 32 for clz(0)
    return Literal(32);
}

Literal intrinsics::smod(DataType type, const Literal& numerator, const Literal& denominator)
{
    throw CompilationError(CompilationStep::GENERAL, "SMOD is currently not implemented!");
}

Literal intrinsics::srem(DataType type, const Literal& numerator, const Literal& denominator)
{
    throw CompilationError(CompilationStep::GENERAL, "SREM is currently not implemented!");
}

Literal intrinsics::fmod(DataType type, const Literal& numerator, const Literal& denominator)
{
    throw CompilationError(CompilationStep::GENERAL, "FMOD is currently not implemented!");
}

Literal intrinsics::frem(DataType type, const Literal& numerator, const Literal& denominator)
{
    throw CompilationError(CompilationStep::GENERAL, "FREM is currently not implemented!");
}
