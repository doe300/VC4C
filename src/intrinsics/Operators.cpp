/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Operators.h"

#include "../Module.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "../normalization/LongOperations.h"
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
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt)
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
    auto it = intrinsifyUnsignedIntegerMultiplication(method, inIt);
    it->decoration = remove_flag(it->decoration, InstructionDecorations::UNSIGNED_RESULT);
    return it;
}

InstructionWalker intrinsics::intrinsifyUnsignedIntegerMultiplication(
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt)
{
    auto& op = *inIt.get();
    InstructionWalker it = inIt;
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

    it.reset(std::make_unique<Operation>(OP_ADD, op.getOutput().value(), resLo, resHi));
    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);

    return it;
}

InstructionWalker intrinsics::intrinsifyLongMultiplication(
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt)
{
    const auto& op = *inIt.get();
    Value firstLower = UNDEFINED_VALUE;
    Value firstUpper = UNDEFINED_VALUE;
    Value secondLower = UNDEFINED_VALUE;
    Value secondUpper = UNDEFINED_VALUE;
    std::tie(firstLower, firstUpper) = normalization::getLowerAndUpperWords(op.getFirstArg());
    std::tie(secondLower, secondUpper) = normalization::getLowerAndUpperWords(op.assertArgument(1));

    /*
     * Similar to the algorithm we use to split a 32-bit multiplications (above or below) into 24-bit/16-bit
     * multiplications, we split the 64-bit multiplication into 32-bit multiplications:
     *
     *                             |     aUp     |     aLo     |
     *  *                          |     bUp     |     bLo     |
     * ---------------------------------------------------------
     * |xxxxxx.xxxxxx.xxxxxx.xxxxxx|             |             |
     *
     *                             |         aLo * bLo         |
     *   +                         |  aLo * bUp  |
     *   +                         |  aUp * bLo  |
     */

    auto partType = TYPE_INT32.toVectorType(op.getOutput()->type.getVectorWidth());
    auto result = method.addNewLocal(op.getOutput()->type, "%mul.result");
    auto resultData = Local::getLocalData<MultiRegisterData>(result.checkLocal());
    if(!resultData)
        throw CompilationError(
            CompilationStep::NORMALIZER, "Cannot lower a 64-bit multiplication to a non 64-bit output", op.to_string());

    InstructionWalker it = inIt;
    auto resLowLow = method.addNewLocal(partType, "%mul.resLowLow");
    auto newCall = &it.emplace(
        std::make_unique<MethodCall>(Value(resLowLow), "mul_full", std::vector<Value>{firstLower, secondLower}));
    it = intrinsifyIntegerToLongMultiplication(
        method, typeSafe(it, *newCall), resultData->lower->createReference(), true /* unsigned */);
    it.nextInBlock();

    auto resLowHigh = method.addNewLocal(partType, "%mul.resLowHigh");
    auto newOp = &it.emplace(
        std::make_unique<IntrinsicOperation>("mul_full", Value(resLowHigh), Value(firstLower), Value(secondUpper)));
    it = intrinsifyUnsignedIntegerMultiplication(method, typeSafe(it, *newOp));
    it.nextInBlock();

    auto resHighLow = method.addNewLocal(partType, "%mul.resHighLow");
    newOp = &it.emplace(
        std::make_unique<IntrinsicOperation>("mul_full", Value(resHighLow), Value(firstUpper), Value(secondLower)));
    it = intrinsifyUnsignedIntegerMultiplication(method, typeSafe(it, *newOp));
    it.nextInBlock();

    // Add the parts back together
    auto highPart = assign(it, partType) = resLowLow + resLowHigh;
    assign(it, resultData->upper->createReference()) = highPart + resHighLow;

    // Let the #lowerLongOperation normalization step handle the actual move to the result, e.g. to be able to also
    // handle flags
    it.reset(createWithExtras<MoveOperation>(op, *op.getOutput(), result));
    return it;
}

InstructionWalker intrinsics::intrinsifyIntegerToLongMultiplication(Method& method,
    TypedInstructionWalker<intermediate::MethodCall> inIt, Optional<Value> lowResult, bool forceUnsigned)
{
    const auto& call = *inIt.get();
    InstructionWalker it = inIt;

    auto arg0 = call.assertArgument(0);
    auto arg1 = call.assertArgument(1);
    bool isUnsigned = forceUnsigned ||
        (call.getArgument(2) && call.assertArgument(2).getLiteralValue() &&
            call.assertArgument(2).getLiteralValue()->unsignedInt() == VC4CL_UNSIGNED);

    // Similar to the Mesa implementation in
    // https://gitlab.freedesktop.org/mesa/mesa/blob/master/src/compiler/nir/nir_lower_alu.c (function lower_alu_instr,
    // case nir_op_imul_high), we need to do the absolute value and reapplication of the sign for signed mul_hi

    if(!isUnsigned)
    {
        // TODO what is max(INT_MIN, -INT_MIN)? Or first what is -INT_MIN?
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

    auto outputType = call.getOutput()->type;
    auto arg0Hi = assign(it, outputType, "%mul_hi.arg0Hi") = as_unsigned{arg0} >> 16_val;
    auto arg1Hi = assign(it, outputType, "%mul_hi.arg1Hi") = as_unsigned{arg1} >> 16_val;
    auto arg0Lo = assign(it, outputType, "%mul_hi.arg0Lo") = arg0 & 0xFFFF_val;
    auto arg1Lo = assign(it, outputType, "%mul_hi.arg1Lo") = arg1 & 0xFFFF_val;

    auto resHiLo = assign(it, outputType, "%mul_hi.resHiLo") = mul24(arg0Hi, arg1Lo);
    auto resLoHi = assign(it, outputType, "%mul_hi.resLoHi") = mul24(arg0Lo, arg1Hi);
    auto resLo = assign(it, outputType, "%mul_hi.resLo") = mul24(arg0Lo, arg1Lo);
    auto lowTmp = assign(it, outputType) = resLo;
    auto resOverflow = assign(it, outputType, "%mul_hi.of") = 0_val;
    resLo = assign(it, outputType, "%mul_hi.resLo") = as_unsigned{resLo} >> 16_val;
    auto resMid = assign(it, outputType, "%mul_hi.resMid") = (resHiLo + resLoHi, SetFlag::SET_FLAGS);
    auto tmp = assign(it, outputType) = resMid << 16_val;
    auto resLower = assign(it, outputType, "%mul_hi.resLo") = lowTmp + tmp;
    assign(it, resOverflow) = (resOverflow + 1_val, COND_CARRY_SET);
    resMid = assign(it, outputType, "%mul_hi.resMid") = (resMid + resLo, SetFlag::SET_FLAGS);
    assign(it, resOverflow) = (resOverflow + 1_val, COND_CARRY_SET);
    resMid = assign(it, outputType, "%mul_hi.resMid") = as_unsigned{resMid} >> 16_val;
    resOverflow = assign(it, outputType, "%mul_hi.of") = resOverflow << 16_val;
    resMid = assign(it, outputType, "%mul_hi.resMid") = resMid + resOverflow;

    auto resHi = assign(it, outputType, "%mul_hi.resHi") = mul24(arg0Hi, arg1Hi);

    if(!isUnsigned)
    {
        // Code to apply 64-bit two's complement (and select that) taken from
        // https://gitlab.freedesktop.org/mesa/mesa/blob/master/src/compiler/nir/nir_lower_alu.c (function
        // lower_alu_instr, case nir_op_imul_high)
        auto lowerComplement = assign(it, outputType, "%mul_hi.lowComp") = ~resLower;
        lowerComplement = assign(it, outputType, "%mul_hi.lowComp") = (lowerComplement + INT_ONE, SetFlag::SET_FLAGS);
        auto carry = assign(it, outputType, "%mul_hi.lowCarry") = INT_ZERO;
        assign(it, carry) = (INT_ONE, COND_CARRY_SET);

        auto out = assign(it, outputType, "%mul_hi.out") = resMid + resHi;
        auto highComplement = assign(it, outputType, "%mul_hi.highComp") = ~out;
        highComplement = assign(it, outputType, "%mul_hi.highComp") = highComplement + carry;

        auto finalResult = assign(it, outputType, "%mul_hi.out") = out;
        auto arg0Neg = assign(it, arg0.type, "%mul_hi.arg0Sign") = as_signed{call.assertArgument(0)} >> 31_val;
        auto arg1Neg = assign(it, arg1.type, "%mul_hi.arg1Sign") = as_signed{call.assertArgument(1)} >> 31_val;
        auto cond = assignNop(it) = as_unsigned{arg0Neg} != as_unsigned{arg1Neg};
        assign(it, finalResult) = (highComplement, cond);
        if(lowResult)
        {
            // TODO is this correct?
            assign(it, *lowResult) = resLower;
            assign(it, *lowResult) = (lowerComplement, cond);
        }

        it.reset(createWithExtras<MoveOperation>(call, call.getOutput().value(), std::move(finalResult)));
    }
    else
    {
        if(lowResult)
            assign(it, *lowResult) = resLower;
        it.reset(std::make_unique<Operation>(OP_ADD, call.getOutput().value(), resMid, resHi));
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
        if(arg.getLiteralValue() && arg.getLiteralValue()->signedInt(arg.type) > 0)
        {
            std::bitset<32> tmp(arg.getLiteralValue()->unsignedInt(arg.type));
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
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt)
{
    const auto& op = *inIt.get();
    InstructionWalker it = inIt;
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
        it.reset(createWithExtras<Operation>(
            op, OP_SHL, op.getOutput().value(), intermediateResult, Value(Literal(lastHigh), TYPE_INT8)));
    }
    else
    {
        it.reset(createWithExtras<MoveOperation>(op, op.getOutput().value(), intermediateResult));
    }

    return it;
}

/*
 * Sources/Info:
 * - http://ipa.ece.illinois.edu/mif/pubs/web-only/Frank-RawMemo12-1999.html
 * - http://flounder.com/multiplicative_inverse.htm
 */

InstructionWalker intrinsics::intrinsifySignedIntegerDivision(
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt, const bool useRemainder)
{
    auto& op = *inIt.get();
    Value opDest = op.getOutput().value();
    // check any operand is negative
    Value op1Sign = UNDEFINED_VALUE;
    Value op2Sign = UNDEFINED_VALUE;

    // convert operands to positive
    Value op1Pos = method.addNewLocal(op.assertArgument(0).type, "%unsigned");
    Value op2Pos = method.addNewLocal(op.assertArgument(0).type, "%unsigned");

    InstructionWalker it = inIt;
    it = insertMakePositive(it, method, op.assertArgument(0), op1Pos, op1Sign);
    it = insertMakePositive(it, method, op.assertArgument(1), op2Pos, op2Sign);

    op.setArgument(0, std::move(op1Pos));
    op.setArgument(1, std::move(op2Pos));

    // use new temporary result, so we can store the final result in the correct value
    const Value tmpDest = method.addNewLocal(opDest.type, "%result");
    op.setOutput(tmpDest);

    // calculate unsigned division
    it = intrinsifyUnsignedIntegerDivision(method, typeSafe(it, op), useRemainder);
    it.nextInBlock();

    if(op1Sign.hasLiteral(0_lit) && op2Sign.hasLiteral(0_lit))
        // if both operands are marked with (unsigned), we don't need to invert the result
        assign(it, opDest) = tmpDest;
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
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt, const bool useRemainder)
{
    const auto& op = *inIt.get();
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
    InstructionWalker it = inIt;

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
        it.reset(std::make_unique<MoveOperation>(op.getOutput().value(), remainder));
    else
        it.reset(std::make_unique<MoveOperation>(op.getOutput().value(), quotient));
    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);

    return it;
}

InstructionWalker intrinsics::intrinsifySignedIntegerDivisionByConstant(
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt, bool useRemainder)
{
    auto& op = *inIt.get();
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

    InstructionWalker it = inIt;
    it = insertMakePositive(it, method, op.assertArgument(0), op1Pos, op1Sign);
    it = insertMakePositive(it, method, op.assertArgument(1), op2Pos, op2Sign);

    op.setArgument(0, std::move(op1Pos));
    op.setArgument(1, std::move(op2Pos));

    // use new temporary result, so we can store the final result in the correct value
    const Value tmpDest = method.addNewLocal(opDest.type, "%result");
    op.setOutput(tmpDest);

    // calculate unsigned division
    it = intrinsifyUnsignedIntegerDivisionByConstant(method, typeSafe(it, op), useRemainder);
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

struct DivisionConstants
{
    uint32_t factor;
    uint32_t shiftOffset;
};

static DivisionConstants calculateLongConstant(Literal divisor)
{
    if(divisor.isUndefined() || divisor.unsignedInt() == 0)
        // a multiplication by 1 and a shift by 0 should both be able to be eliminated
        return DivisionConstants{};

    auto shift = static_cast<uint32_t>(std::ceil(std::log2(divisor.unsignedInt())));
    auto factor = static_cast<uint64_t>(std::ceil(std::pow(2, 32 + shift) / divisor.unsignedInt()));

    // The factor has a 33rd bit , which needs to be applied separately to not overflow 64-bit multiplication for
    // large nominators
    auto hasHighBit = (factor & uint64_t{0x100000000}) != 0;
    factor = factor - uint64_t{0x100000000};

    if(shift > 31)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Unsigned division by constant generated invalid shift offset", std::to_string(shift));
    if(factor > std::numeric_limits<uint32_t>::max() || !hasHighBit)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Unsigned division by constant generated invalid multiplication factor", std::to_string(factor));
    return DivisionConstants{static_cast<uint32_t>(factor), shift};
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
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt, bool useRemainder)
{
    const auto& op = *inIt.get();
    InstructionWalker it = inIt;
    if(op.getSecondArg().value().getLiteralValue() && op.getFirstArg().type.getScalarBitCount() > 16)
    {
        // use int to long multiplication and take upper part
        /*
         * Taken from: https://www.ridiculousfish.com/blog/posts/labor-of-division-episode-i.html
         *
         * r = n / d = (m * n) / 2^(32 + p)
         * p = ceil(log2(d))
         * m = ceil(2^(32 + p) / d) [33-bit integer due to ceilings]
         * m' = m - 2^32
         */
        // TODO implement for vector divisor
        auto constants = calculateLongConstant(*op.assertArgument(1).getLiteralValue());
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying unsigned division by " << op.assertArgument(1).to_string(false, true)
                << " by long multiplication with " << constants.factor << " and right-shift by 32+"
                << constants.shiftOffset << logging::endl);

        // mn = m * n = (m - 2^32) * n + 2^32 * n = m' * n + 2^32 * n
        // q = (m' * n) >> 32 = (mn - 2^32 * n) >> 32 = mn >> 32 - n
        auto mulResult = method.addNewLocal(op.getFirstArg().type, "%udiv_multiply");
        auto& mulHi = it.emplace(std::make_unique<MethodCall>(Value(mulResult), "mul_hi",
            std::vector<Value>{Value(Literal(constants.factor), TYPE_INT32), op.getFirstArg()}));
        it = intrinsifyIntegerToLongMultiplication(method, typeSafe(it, mulHi), NO_VALUE, true /* unsigned */);
        it.nextInBlock();
        // t = (n - q) >> 1 + q = (n - q) / 2 + q = (n + q) / 2 = (n + q) >> 1 = (n + mn >> 32 - n) >> 1 = mn >> 33
        auto tmp = assign(it, op.getFirstArg().type) = op.getFirstArg() - mulResult;
        tmp = assign(it, op.getFirstArg().type) = as_unsigned{tmp} >> 1_val;
        tmp = assign(it, op.getFirstArg().type) = tmp + mulResult;
        // t = t >> (p - 1) = (mn >> 33) >> (p - 1) = mn >> (32 + p) = n / d = r
        auto offset = assign(it, TYPE_INT8) = Value(Literal(constants.shiftOffset), TYPE_INT8) - 1_val;
        auto finalResult = assign(it, op.getOutput()->type, "%udiv_result") =
            (as_unsigned{tmp} >> offset, InstructionDecorations::UNSIGNED_RESULT);
        if(useRemainder)
        {
            // x mod y = x - (x/y) * y [since r = n / d <= n, z = r * d fits into a 32-bit multiplication]
            auto tmpMul = method.addNewLocal(op.getFirstArg().type);
            it = insertMultiplication(
                it, method, op.assertArgument(1), finalResult, tmpMul, InstructionDecorations::UNSIGNED_RESULT);
            // replace original division
            it.reset(createWithExtras<Operation>(op, OP_SUB, op.getOutput().value(), op.getFirstArg(), tmpMul));
        }
        else
            it.reset(createWithExtras<MoveOperation>(op, op.getOutput().value(), finalResult));

        it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        return it;
    }

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
    it.emplace(createWithExtras<Operation>(op, OP_SHR, divOut, tmp, constants.second));
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
        it.reset(std::make_unique<Operation>(OP_SUB, op.getOutput().value(), op.getFirstArg(), tmpMul));
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

NODISCARD InstructionWalker intrinsics::intrinsifyIntegerDivisionByFloatingDivision(
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt, bool useRemainder)
{
    auto& op = *inIt.get();
    InstructionWalker it = inIt;

    auto floatType = TYPE_FLOAT.toVectorType(op.getOutput()->type.getVectorWidth());
    auto floatNominator = assign(it, floatType, "%fdiv") = itof(op.assertArgument(0));
    auto floatDivisor = assign(it, floatType, "%fdiv") = itof(op.assertArgument(1));
    op.setArgument(0, floatNominator);
    op.setArgument(1, floatDivisor);
    auto realResult = op.getOutput();
    auto floatResult = method.addNewLocal(floatType, "%fdiv");
    op.setOutput(floatResult);
    // The only "edge case" we have for integer division is division by zero, which we are allowed to return an
    // undefined value anyway
    it = intrinsifyFloatingDivision(method, typeSafe(it, op), false /* no need to handle edge cases */);
    it.nextInBlock();
    auto result = assign(it, realResult->type, "%div") = ftoi(floatResult);

    /*
     * Due to the float division not being infinite precise, we get some sparse errors on exact division.
     *
     * E.g. for 2a / a, the float division may return 1.999... instead of 2.0 which is the truncated down to 1, instead
     * of the expected 2.
     *
     * We know that we are off by 0 or 1 (or: at most 1), so we can check:
     * real result = result + 1 if result * divisor + divisor <= nominator
     *
     * Even with this fix required, this calculation is still far faster than doing the bit-wise integer division.
     */
    floatResult = assign(it, floatType, "%div_fix") = itof(result);
    // diff = abs(nominator - result * divisor)
    auto tmpResult = assign(it, floatType, "%div_fix") = as_float{floatResult} * as_float{floatDivisor};
    auto resultDiff = assign(it, floatType, "%div_fix") = as_float{floatNominator} - as_float{tmpResult};
    tmpResult = method.addNewLocal(floatType, "%div_fix");
    it.emplace(std::make_unique<Operation>(OP_FMAXABS, tmpResult, resultDiff, resultDiff));
    it.nextInBlock();
    // offset = (nominator < 0) ^ (divisor < 0) ? -1 : 1
    auto sign = assign(it, realResult->type, "%div_sign") = floatNominator ^ floatDivisor;
    sign = assign(it, realResult->type, "%div_sign") = as_signed{sign} >> 31_val;
    auto increment = assign(it, realResult->type, "%div_fix") = sign | 1_val;
    // result = result + offset, iff abs(diff) >= abs(divisor) (<=> abs(divisor) ! > abs(diff))
    it.emplace(std::make_unique<Operation>(
        OP_FMAXABS, NOP_REGISTER, floatDivisor, tmpResult, COND_ALWAYS, SetFlag::SET_FLAGS));
    it.nextInBlock();
    assign(it, result) = (result + increment, COND_CARRY_CLEAR);

    if(useRemainder)
    {
        floatResult = assign(it, floatType, "%mod") = itof(result);
        tmpResult = assign(it, floatType, "%mod") = as_float{floatResult} * as_float{floatDivisor};
        tmpResult = assign(it, floatType, "%mod") = as_float{floatNominator} - as_float{tmpResult};
        it.emplace(std::make_unique<Operation>(OP_FTOI, realResult.value(), tmpResult));
    }
    else
        it.emplace(std::make_unique<MoveOperation>(realResult.value(), result));

    return it;
}

InstructionWalker intrinsics::intrinsifyFloatingDivision(
    Method& method, TypedInstructionWalker<intermediate::IntrinsicOperation> inIt, bool fullRangeDivision)
{
    /*
     * Current implementation is a Goldschmidt algorithm taken from
     * http://www.informatik.uni-trier.de/Reports/TR-08-2004/rnc6_12_markstein.pdf (formula 9)
     *
     * Tests on the VideoCore IV GPU have shown that a 2 steps are enough to give an accurate result (within the allowed
     * 3 ULP). This implementation has the same accuracy as the previous Newton-Raphson implementation with 5 steps.
     * Adding more Goldschmidt's steps does not make the result more accurate, in the contrary, it raises the relative
     * error to 4 ULP for a lot of values. Thus, we instead use Newton-Raphson for the second step to reduce the
     * accumulated error.
     *
     * Other resources:
     * https://dspace.mit.edu/bitstream/handle/1721.1/80133/43609668-MIT.pdf
     * https://en.wikipedia.org/wiki/Division_algorithm#Newton.E2.80.93Raphson_division
     * http://www.rfwireless-world.com/Tutorials/floating-point-tutorial.html
     *
     * The GLSL shader (in mesa) uses the SFU_RECIP with a Newton-Raphson step "to improve our approximation",
     * see http://anholt.livejournal.com/49474.html
     *
     */
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying floating-point division" << logging::endl);

    const auto& op = *inIt.get();
    const Value nominator = op.getFirstArg();
    const Value& divisor = op.assertArgument(1);
    auto outputType = op.getOutput()->type;

    auto reducedNominator = nominator;
    auto reducedDivisor = divisor;

    InstructionWalker it = inIt;
    if(fullRangeDivision)
    {
        /*
         * The Goldschmidt's algorithm by its own becomes too inaccurate for huge divisors (|x| > 2^32) and as stated
         * above, adding more steps only introduces more error across all values.
         *
         * To mitigate this, we do argument reduction to reduce the number for which to calculate the reciprocal.
         *
         * (M1 * 2^E1) / (M2 * 2^E2) = (M1 / M2) * 2^(E1 - E2) = M1 * (1 / M2) * 2^(E1 - E2)
         * with:
         * E1 = 127 + P1
         * E2 = 127 + P2
         *
         * => M = M1 * (1 / M2)
         * => E = (E1 - 127) - (E2 - 127) + 127 = E1 - E2 + 127
         */
        // truncate the exponent to 127 for both nominator and divisor
        reducedNominator = assign(it, outputType, "%fdiv_reduction") = nominator & 0x807FFFFF_val;
        reducedNominator = assign(it, outputType, "%div_reduction") = reducedNominator | 0x3F800000_val;
        reducedDivisor = assign(it, outputType, "%fdiv_reduction") = divisor & 0x807FFFFF_val;
        reducedDivisor = assign(it, outputType, "%div_reduction") = reducedDivisor | 0x3F800000_val;
    }

    ////
    // Combined Goldschmidt's and Newton-Raphson Algorithm
    ////

    // 1. initialization step: Y0 = SFU_RECIP(D)
    it = periphery::insertSFUCall(REG_SFU_RECIP, it, reducedDivisor);
    auto Y0 = assign(it, outputType, "%fdiv_recip") = Value(REG_SFU_OUT, outputType);

    // 2. calculate error: e = 1 - D * Y0
    auto tmp = assign(it, outputType, "%fdiv_error") = as_float{reducedDivisor} * as_float{Y0};
    auto e = assign(it, outputType, "%fdiv_error") = 1.0_val - tmp;

    // 3. iteration step: Yi+1 = Yi * (1 + e^i)
    // For running more Goldschmidt's steps, use Yn = Y0 * (1 + e + e^2 + e^3 + e^4 + ...)
    auto step = assign(it, outputType, "%fdiv_step") = 1.0_val + e;
    auto Y1 = assign(it, outputType, "%fdiv_step") = Y0 * step;
    /*
     * Since Goldschmidt's algorithm introduces an accumulated rounding error, we use the Newton-Raphson algorithm for
     * the second step:
     * - ei = 1 - b0 * Y1
     * - Yi+1 = Yi + Yi * ei
     *
     * In practice, using the Newton-Raphson algorithm for the second step reduces the relative error by enough for this
     * division algorithm to be precise (within the allowed error).
     */
    tmp = assign(it, outputType, "%fdiv_error") = as_float{reducedDivisor} * as_float{Y1};
    e = assign(it, outputType, "%fdiv_error") = 1.0_val - tmp;
    auto stepTmp2 = assign(it, outputType, "%fdiv_step") = as_float{Y1} * as_float{e};
    auto recip = assign(it, outputType, "%fdiv_recip") = Y1 + stepTmp2;

    // 4. final step: Q = Yn * N
    auto result = assign(it, outputType, "%fdiv") = as_float{reducedNominator} * as_float{recip};

    if(fullRangeDivision)
    {
        /**
         * Revert the range reduction by calculating and applying the exponent difference
         *
         * We don't need to actually calculate E, since we can just add the difference E1 - E2 to the exponent of the
         * resulting range reduces value.
         */
        auto intType = TYPE_INT8.toVectorType(outputType.getVectorWidth());
        auto nominatorExponent = assign(it, intType, "%fdiv_exponent") = as_unsigned{nominator} >> 23_val;
        nominatorExponent = assign(it, intType, "%fdiv_exponent") = nominatorExponent & 0x000000FF_val;
        auto divisorExponent = assign(it, intType, "%fdiv_exponent") = as_unsigned{divisor} >> 23_val;
        divisorExponent = assign(it, intType, "%fdiv_exponent") = divisorExponent & 0x000000FF_val;
        // Ediff = Enom - Ediv
        auto exponentDiff = assign(it, intType, "%fdiv_exponent") = nominatorExponent - divisorExponent;
        auto resultExponent = assign(it, intType, "%fdiv_exponent") = as_unsigned{result} >> 23_val;
        resultExponent = assign(it, intType, "%fdiv_exponent") = resultExponent & 0x000000FF_val;
        // Eres = Eres + Ediff
        resultExponent = assign(it, intType, "%fdiv_exponent") = resultExponent + exponentDiff;
        // clamp to range [0, 254] to make sure we do not modify any other bits and do not generate wrong Infs
        resultExponent = assign(it, intType, "%fdiv_exponent") = max(as_signed{resultExponent}, as_signed{0_val});
        resultExponent = assign(it, intType, "%fdiv_exponent") = min(as_signed{resultExponent}, as_signed{254_val});
        resultExponent = assign(it, intType, "%fdiv_exponent") = resultExponent << 23_val;
        // reinsert the exponent into the result
        auto resultRest = assign(it, outputType, "%fdiv_reduction") = result & 0x807FFFFF_val;
        assign(it, result) = resultRest | resultExponent;

        /*
         * 5. all the special value handling
         * Priorities (we need to calculate in inverse order):
         * 1. x / NaN = NaN
         * 2. NaN / x = NaN
         * 3. +-Inf / +-Inf = NaN
         * 4. 0 / 0 = NaN
         */
        // we can now (correctly?) calculate subnormal values, which we do not support, so flush to zero
        auto cond = assignNop(it) = iszero(as_unsigned{resultExponent});
        assign(it, result) = (FLOAT_ZERO, cond);
        cond = assignNop(it) = iszero(as_unsigned{nominatorExponent});
        assign(it, result) = (FLOAT_ZERO, cond);
        cond = assignNop(it) = isinf(as_float{divisor});
        assign(it, result) = (FLOAT_ZERO, cond);
        auto tmp = assign(it, outputType, "%fdiv_edge") = nominator | divisor;
        // both of them are zero
        cond = assignNop(it) = iszero(as_float{tmp});
        assign(it, result) = (FLOAT_NAN, cond);
        tmp = assign(it, outputType, "%fdiv_edge") = nominator & divisor;
        // both of them are Inf
        cond = assignNop(it) = isinf(as_float{tmp});
        assign(it, result) = (FLOAT_NAN, cond);
        cond = assignNop(it) = isnan(as_float{nominator});
        assign(it, result) = (FLOAT_NAN, cond);
        cond = assignNop(it) = isnan(as_float{divisor});
        assign(it, result) = (FLOAT_NAN, cond);
    }

    it.reset(createWithExtras<MoveOperation>(op, op.getOutput().value(), result));
    return it;
}

InstructionWalker intrinsics::insertMultiplication(InstructionWalker it, Method& method, const Value& arg0,
    const Value& arg1, Value& dest, intermediate::InstructionDecorations decorations)
{
    if(arg0.type.isFloatingType() != arg1.type.isFloatingType())
        throw CompilationError(CompilationStep::GENERAL, "Cannot multiply integral and floating-point arguments");

    if(arg0.type.isFloatingType() && arg1.type.isFloatingType())
    {
        assign(it, dest) = (as_float{arg0} * as_float{arg1}, decorations);
        return it;
    }

    auto op =
        &it.emplace(std::make_unique<intermediate::IntrinsicOperation>("mul", Value(dest), Value(arg0), Value(arg1)));
    op->addDecorations(decorations);

    if(Local::getLocalData<MultiRegisterData>(arg0.checkLocal()) ||
        Local::getLocalData<MultiRegisterData>(arg1.checkLocal()))
    {
        // 64-bit multiplication
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Calculating result for 64-bit integer multiplication: " << op->to_string() << logging::endl);
        it = intrinsifyLongMultiplication(method, typeSafe(it, *op));
    }
    else if(arg0.getLiteralValue() && arg1.getLiteralValue())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Calculating result for multiplication with constants: " << op->to_string() << logging::endl);
        it.reset(createWithExtras<MoveOperation>(*it.get(), Value(op->getOutput()->local(), arg0.type),
            Value(Literal(arg0.getLiteralValue()->signedInt(arg0.type) * arg1.getLiteralValue()->signedInt(arg1.type)),
                arg0.type)));
    }
    else if(arg0.getLiteralValue() && arg0.getLiteralValue()->signedInt(arg0.type) > 0 &&
        isPowerTwo(arg0.getLiteralValue()->unsignedInt(arg0.type)))
    {
        // a * 2^n = a << n
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying multiplication with left-shift: " << op->to_string() << logging::endl);
        it.reset(createWithExtras<Operation>(*it.get(), OP_SHL, op->getOutput().value(), arg1,
            Value(Literal(static_cast<int32_t>(std::log2(arg0.getLiteralValue()->signedInt(arg0.type)))), arg0.type)));
    }
    else if(arg1.getLiteralValue() && arg1.getLiteralValue()->signedInt(arg1.type) > 0 &&
        isPowerTwo(arg1.getLiteralValue()->unsignedInt(arg1.type)))
    {
        // a * 2^n = a << n
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying multiplication with left-shift: " << op->to_string() << logging::endl);
        it.reset(createWithExtras<Operation>(*it.get(), OP_SHL, op->getOutput().value(), op->getFirstArg(),
            Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->signedInt(arg1.type)))), arg1.type)));
    }
    else if(std::max(arg0.type.getScalarBitCount(), arg1.type.getScalarBitCount()) <= 24)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying multiplication of small integers to mul24: " << op->to_string() << logging::endl);
        it.reset(createWithExtras<Operation>(
            *it.get(), OP_MUL24, op->getOutput().value(), op->getFirstArg(), op->assertArgument(1)));
    }
    else if(arg0.getLiteralValue() && arg0.getLiteralValue()->signedInt(arg0.type) > 0 &&
        isPowerTwo(arg0.getLiteralValue()->unsignedInt(arg0.type) + 1))
    {
        // x * (2^k - 1) = x * 2^k - x = x << k - x
        // This is a special case of the "binary method", but since the "binary method" only applies shifts and
        // adds, we handle shift and minus separately.
        // TODO could make more general, similar to "binary method" implementation/integrate into that
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying multiplication with left-shift and minus: " << op->to_string() << logging::endl);
        auto tmp = assign(it, arg1.type, "%mul_shift") =
            (arg1 << Value(Literal(static_cast<int32_t>(std::log2(arg0.getLiteralValue()->signedInt(arg0.type) + 1))),
                 arg0.type));
        it.reset(createWithExtras<Operation>(*it.get(), OP_SUB, op->getOutput().value(), tmp, arg1));
    }
    else if(arg1.getLiteralValue() && arg1.getLiteralValue()->signedInt(arg1.type) > 0 &&
        isPowerTwo(arg1.getLiteralValue()->unsignedInt(arg1.type) + 1))
    {
        // x * (2^k - 1) = x * 2^k - x = x << k - x
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying multiplication with left-shift and minus: " << op->to_string() << logging::endl);
        auto tmp = assign(it, arg0.type, "%mul_shift") =
            (arg0 << Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->signedInt(arg1.type) + 1))),
                 arg0.type));
        it.reset(createWithExtras<Operation>(*it.get(), OP_SUB, op->getOutput().value(), tmp, arg0));
    }
    else if(canOptimizeMultiplicationWithBinaryMethod(*op))
    {
        // e.g. x * 3 = x << 1 + x
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying multiplication via binary method: " << op->to_string() << logging::endl);
        it = intrinsifyIntegerMultiplicationViaBinaryMethod(method, typeSafe(it, *op));
    }
    else
        it = intrinsifySignedIntegerMultiplication(method, typeSafe(it, *op));

    return it.nextInBlock();
}

static constexpr unsigned MSB = 31;

Literal intrinsics::asr(Literal left, Literal right)
{
    // Tests have shown that on VC4 all shifts (asr, shr, shl) only take the last 5 bits of the offset (modulo 32)
    auto offset = right.unsignedInt() & 0x1F;
    if((-1 >> 31u) == -1)
        // if signed right shift is arithmetic shift on the underlying architecture, then use that instead of the manual
        // shifting
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
