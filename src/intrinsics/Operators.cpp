/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Operators.h"

#include "../intermediate/Helper.h"
#include "../periphery/SFU.h"
#include "Comparisons.h"
#include "helper.h"
#include "log.h"

#include <bitset>
#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;

//TODO reorder instructions, so no/less NOPs are inserted (and again reordered away)

InstructionWalker intermediate::intrinsifySignedIntegerMultiplication(Method& method, InstructionWalker it, Operation& op)
{
	Value opDest = op.getOutput();

	const Value arg0 = op.getArgument(0);
	const Value arg1 = op.getArgument(1);

	//convert operands to positive
	Value op1Pos = method.addNewLocal(arg0.type, "%unsigned");
	Value op2Pos = method.addNewLocal(arg1.type, "%unsigned");

	it = insertMakePositive(it, method, arg0, op1Pos);
	it = insertMakePositive(it, method, arg1, op2Pos);

	op.setArgument(0, op1Pos);
	op.setArgument(1, op2Pos);

	//use new temporary result, so we can store the final result in the correct value
	const Value tmpDest = method.addNewLocal(opDest.type, "%result");
	op.setOutput(tmpDest);

	//do unsigned multiplication
	it = intrinsifyUnsignedIntegerMultiplication(method, it, op);
	//skip the original instruction
	it.nextInBlock();

	//check any operand is negative
	Value op1Sign = method.addNewLocal(TYPE_BOOL, "%sign");
	Value op2Sign = method.addNewLocal(TYPE_BOOL, "%sign");
	it = insertIsNegative(it, arg0, op1Sign);
	it = insertIsNegative(it, arg1, op2Sign);
	if(op1Sign.hasType(ValueType::LITERAL) && op2Sign.hasType(ValueType::LITERAL))
	{
		throw CompilationError(CompilationStep::OPTIMIZER, "This case of multiplication of literal integers should have been replaced with constant", op.to_string());
	}

	//if exactly one operand was negative, invert sign of result
	it.emplace(new Operation(OP_XOR, NOP_REGISTER, op1Sign, op2Sign, COND_ALWAYS, SetFlag::SET_FLAGS));
	it.nextInBlock();
	return insertInvertSign(it, method, tmpDest, opDest, COND_ZERO_CLEAR);
}

InstructionWalker intermediate::intrinsifyUnsignedIntegerMultiplication(Method& method, InstructionWalker it, Operation& op)
{
    const Value& arg0 = op.getFirstArg();
    const Value& arg1 = op.getSecondArg().orElse(UNDEFINED_VALUE);
    
    //mul24 can multiply 24-bits * 24-bits into 32-bits
    //default case, full multiplication
    //NOTE: the instructions are ordered in a way, that the insertion of NOPs to split read-after-write is minimal
    logging::debug() << "Intrinsifying unsigned multiplication of integers" << logging::endl;

    const Value a0 = method.addNewLocal(op.getOutput().get().type, "%mul.a0");
    const Value a1 = method.addNewLocal(op.getOutput().get().type, "%mul.a1");
    const Value b0 = method.addNewLocal(op.getOutput().get().type, "%mul.b0");
    const Value b1 = method.addNewLocal(op.getOutput().get().type, "%mul.b1");
    const Value out0 = method.addNewLocal(op.getOutput().get().type, "%mul.out0");
    const Value out1 = method.addNewLocal(op.getOutput().get().type, "%mul.out1");
    const Value out2 = method.addNewLocal(op.getOutput().get().type, "%mul.out2");
    /*
     *                             |     a[0]    .    a[1]     |
     *  *                          |     b[0]    .    b[1]     |
     * ---------------------------------------------------------
     * |xxxxxx.xxxxxx.xxxxxx.xxxxxx|      .      .      .      |
     * 
     *                             |        a[1] * b[1]        |
     *   +           |        a[1] * b[0]        |
     *   +           |        a[0] * b[1]        |
     * 
     */
    //split arguments into parts
    bool hasA0Part, hasA1Part, hasB0Part, hasB1Part;
    if(arg0.hasType(ValueType::LITERAL))
    {
        hasA1Part = (arg0.literal.integer & 0xFFFF) != 0;
        it.emplace(new MoveOperation(a1, Value(Literal(static_cast<int64_t>(arg0.literal.integer & 0xFFFF)), TYPE_INT16)));
        it.nextInBlock();
        hasA0Part = (arg0.literal.integer >> 16) != 0;
		it.emplace(new MoveOperation(a0, Value(Literal(static_cast<int64_t>(arg0.literal.integer >> 16)), TYPE_INT16)));
		it.nextInBlock();
    }
    else
    {
        hasA0Part = true;   //not known
        hasA1Part = true;
        it.emplace(new Operation(OP_AND, a1, arg0, Value(Literal(static_cast<uint64_t>(0xFFFF)), TYPE_INT16)));
        it.nextInBlock();
        it.emplace(new Operation(OP_SHR, a0, arg0, Value(Literal(static_cast<int64_t>(16)), TYPE_INT16)));
        it.nextInBlock();
    }
    if(arg1.hasType(ValueType::LITERAL))
    {
    	hasB1Part = (arg1.literal.integer & 0xFFFF) != 0;
		it.emplace(new MoveOperation(b1, Value(Literal(static_cast<int64_t>(arg1.literal.integer & 0xFFFF)), TYPE_INT8)));
		it.nextInBlock();
        hasB0Part = (arg1.literal.integer >> 16) != 0;
        it.emplace(new MoveOperation(b0, Value(Literal(static_cast<int64_t>(arg1.literal.integer >> 16)), TYPE_INT8)));
        it.nextInBlock();
    }
    else
    {
        //not known
        hasB0Part = true;
        hasB1Part = true;
        it.emplace( new Operation(OP_AND, b1, arg1, Value(Literal(static_cast<uint64_t>(0xFFFF)), TYPE_INT8)));
        it.nextInBlock();
        it.emplace( new Operation(OP_SHR, b0, arg1, Value(Literal(static_cast<int64_t>(16)), TYPE_INT8)));
        it.nextInBlock();
    }
    if(hasA1Part && hasB1Part)
    {
        it.emplace( new Operation(OP_MUL24, out0, a1, b1));
        it.nextInBlock();
    }
    else
    {
        it.emplace( new MoveOperation(out0, INT_ZERO));
        it.nextInBlock();
    }
    if(hasA1Part && hasB0Part)
    {
    	const Value tmp = method.addNewLocal(op.getOutput().get().type);
        it.emplace( new Operation(OP_MUL24, tmp, a1, b0));
        it.nextInBlock();
        it.emplace( new Operation(OP_SHL, tmp, tmp, Value(Literal(static_cast<int64_t>(16)), TYPE_INT8)));
        it.nextInBlock();
        it.emplace( new Operation(OP_ADD, out1, out0, tmp));
        it.nextInBlock();
    }
    else
    {
        it.emplace( new MoveOperation(out1, out0));
        it.nextInBlock();
    }
    if(hasA0Part && hasB1Part)
    {
    	const Value tmp = method.addNewLocal(op.getOutput().get().type);
        it.emplace( new Operation(OP_MUL24, tmp, a0, b1));
        it.nextInBlock();
        it.emplace( new Operation(OP_SHL, out2, tmp, Value(Literal(static_cast<int64_t>(16)), TYPE_INT8)));
        it.nextInBlock();
    }
    else
    {
        it.emplace( new MoveOperation(out2, INT_ZERO));
        it.nextInBlock();
    }
    op.setOpCode(OP_ADD);
    op.setArgument(0, out1);
    op.setArgument(1, out2);
    op.decoration = add_flag(op.decoration, InstructionDecorations::UNSIGNED_RESULT);
    
    return it;
}

/*
 * Sources/Info:
 * - http://ipa.ece.illinois.edu/mif/pubs/web-only/Frank-RawMemo12-1999.html
 * - http://flounder.com/multiplicative_inverse.htm
 */

InstructionWalker intermediate::intrinsifySignedIntegerDivision(Method& method, InstructionWalker it, Operation& op, const bool useRemainder)
{
	Value opDest = op.getOutput();
	//check any operand is negative
	Value op1Sign = method.addNewLocal(TYPE_BOOL, "%sign");
	Value op2Sign = method.addNewLocal(TYPE_BOOL, "%sign");
	it = insertIsNegative(it, op.getArgument(0), op1Sign);
	it = insertIsNegative(it, op.getArgument(1), op2Sign);
	if(op1Sign.hasType(ValueType::LITERAL) && op2Sign.hasType(ValueType::LITERAL))
	{
		throw CompilationError(CompilationStep::OPTIMIZER, "This case of multiplication of literal integers should have been replaced with constant", op.to_string());
	}

	//convert operands to positive
	Value op1Pos = method.addNewLocal(op.getArgument(0).get().type, "%unsigned");
	Value op2Pos = method.addNewLocal(op.getArgument(0).get().type, "%unsigned");

	it = insertMakePositive(it, method, op.getArgument(0), op1Pos);
	it = insertMakePositive(it, method, op.getArgument(1), op2Pos);

	op.setArgument(0, op1Pos);
	op.setArgument(1, op2Pos);

	//use new temporary result, so we can store the final result in the correct value
	const Value tmpDest = method.addNewLocal(opDest.type, "%result");
	op.setOutput(tmpDest);
    
    //calculate unsigned division
    it = intrinsifyUnsignedIntegerDivision(method, it, op, useRemainder);
    it.nextInBlock();
    
    //if exactly one operand was negative, invert sign of result
	it.emplace(new Operation(OP_XOR, NOP_REGISTER, op1Sign, op2Sign, COND_ALWAYS, SetFlag::SET_FLAGS));
	it.nextInBlock();
	it = insertInvertSign(it, method, tmpDest, opDest, COND_ZERO_CLEAR);
	return it;
}

InstructionWalker intermediate::intrinsifyUnsignedIntegerDivision(Method& method, InstructionWalker it, Operation& op, const bool useRemainder)
{
    //https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_.28unsigned.29_with_remainder
	//see also: https://www.microsoft.com/en-us/research/wp-content/uploads/2008/08/tr-2008-141.pdf
	//TODO for |type| < 24, use floating-point division??
	//NOTE: the instructions are ordered in a way, that the insertion of NOPs to split read-after-write is minimal
    const Value& numerator = op.getFirstArg();
    const Value& divisor = op.getSecondArg().orElse(UNDEFINED_VALUE);
    
    logging::debug() << "Intrinsifying division of unsigned integers" << logging::endl;
    
    //TODO divisor = 0 handling!
    
    //Q := 0                 -- initialize quotient and remainder to zero
    //R := 0      
    Value quotient = method.addNewLocal(op.getOutput().get().type, "%udiv.quotient");
    Value remainder = method.addNewLocal(op.getOutput().get().type, "%udiv.remainder");
    //set explicitly to zero
	it.emplace( new MoveOperation(remainder, INT_ZERO));
	it.nextInBlock();
	it.emplace( new MoveOperation(quotient, INT_ZERO));
	it.nextInBlock();

    //for i := n ? 1 ... 0 do     -- where n is number of bits in N
    for(int i = numerator.type.getScalarBitCount() - 1; i >= 0; --i)
    {
        //R := R << 1          -- left-shift R by 1 bit
    	Value newRemainder = method.addNewLocal(op.getOutput().get().type, "%udiv.remainder");
        it.emplace(new Operation(OP_SHL, newRemainder, remainder, INT_ONE));
        it.nextInBlock();
        remainder = newRemainder;
        //R(0) := N(i)         -- set the least-significant bit of R equal to bit i of the numerator
        //R = R | ((N >> i) & 1) <=> R = R | (N & (1 << i) == 1 ? 1 : 0) <=> R = R | 1, if N & (1 << i) != 0
        newRemainder = method.addNewLocal(op.getOutput().get().type, "%udiv.remainder");
        it.emplace(new Operation(OP_AND, NOP_REGISTER, numerator, Value(Literal(static_cast<int64_t>(1) << i), TYPE_INT32), COND_ALWAYS, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it.emplace( new Operation(OP_OR, newRemainder, remainder, INT_ONE, COND_ZERO_CLEAR));
        it.nextInBlock();
        //else R(new) := R(old)
        it.emplace(new MoveOperation(newRemainder, remainder, COND_ZERO_SET));
		it.nextInBlock();
        remainder = newRemainder;
        //if R >= D then
        const Value tmp = method.addNewLocal(op.getOutput().get().type, "%udiv.tmp");
        it.emplace(new Operation(OP_MAX, tmp, remainder, divisor));
        it.nextInBlock();
        it.emplace(new Operation(OP_XOR, NOP_REGISTER, tmp, remainder, COND_ALWAYS, SetFlag::SET_FLAGS));
        it.nextInBlock();
        //R := R - D
        newRemainder = method.addNewLocal(op.getOutput().get().type, "%udiv.remainder");
        it.emplace( new Operation(OP_SUB, newRemainder, remainder, divisor, COND_ZERO_SET));
        it.nextInBlock();
        //else R(new) := R(old)
        it.emplace(new MoveOperation(newRemainder, remainder, COND_ZERO_CLEAR));
        it.nextInBlock();
        remainder = newRemainder;
        //Q(i) := 1
        Value newQuotient = method.addNewLocal(op.getOutput().get().type, "%udiv.quotient");
        it.emplace( new Operation(OP_OR, newQuotient, quotient, Value(Literal(static_cast<int64_t>(1) << i), TYPE_INT32), COND_ZERO_SET));
        it.nextInBlock();
        //else Q(new) := Q(old)
        it.emplace(new MoveOperation(newQuotient, quotient, COND_ZERO_CLEAR));
        it.nextInBlock();
        quotient = newQuotient;
    }
    
    //make move from original instruction
    op.setOpCode(OP_OR);
    op.decoration = add_flag(op.decoration, InstructionDecorations::UNSIGNED_RESULT);
    if(useRemainder)
    {
        op.setArgument(0, remainder);
        op.setArgument(1, remainder);
    }
    else
    {
        op.setArgument(0, quotient);
        op.setArgument(1, quotient);
    }
    
    return it;
}

InstructionWalker intermediate::intrinsifyFloatingDivision(Method& method, InstructionWalker it, Operation& op)
{
    //TODO correct??

    /*
     * https://dspace.mit.edu/bitstream/handle/1721.1/80133/43609668-MIT.pdf
     * https://en.wikipedia.org/wiki/Division_algorithm#Newton.E2.80.93Raphson_division
     * http://www.rfwireless-world.com/Tutorials/floating-point-tutorial.html
     */
    logging::debug() << "Intrinsifying floating-point division" << logging::endl;
    
    const Value nominator = op.getFirstArg();
    const Value divisor = op.getSecondArg();
    
    ////
    // Newton-Raphson
    ////
    //TODO: "The Newton-Raphson  algorithm [...] is commonly used if the result does not require proper rounding"
    //-> use Goldschmidt??
    
    //1. initialization step: P0 = SFU_RECIP(D)
    /*
     * The GLSL shader uses the SFU_RECIP with a Newton-Raphson step "to improve our approximation",
     * see http://anholt.livejournal.com/49474.html
     */
    const Value P0 = method.addNewLocal(op.getOutput().get().type, "%fdiv_recip");
    periphery::insertSFUCall(REG_SFU_RECIP, it, divisor);
    it.emplace(new MoveOperation(P0, Value(REG_SFU_OUT, TYPE_FLOAT)));
    it.nextInBlock();
    const Value const2(Literal(2.0), TYPE_FLOAT);
    
    //2. iteration step: Pi+1 = Pi(2 - D * Pi)
    //run 5 iterations
    const Value P1 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p1");
    const Value P1_1 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p1");
    const Value P1_2 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p1");
    it.emplace( new Operation(OP_FMUL, P1, divisor, P0));
    it.nextInBlock();
    it.emplace( new Operation(OP_FSUB, P1_1, const2, P1));
    it.nextInBlock();
    it.emplace( new Operation(OP_FMUL, P1_2, P0, P1_1));
    it.nextInBlock();
    
    const Value P2 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p2");
    const Value P2_1 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p2");
    const Value P2_2 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p2");
    it.emplace( new Operation(OP_FMUL, P2, divisor, P1_2));
    it.nextInBlock();
    it.emplace( new Operation(OP_FSUB, P2_1, const2, P2));
    it.nextInBlock();
    it.emplace( new Operation(OP_FMUL, P2_2, P1_2, P2_1));
    it.nextInBlock();
    
    const Value P3 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p3");
    const Value P3_1 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p3");
    const Value P3_2 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p3");
    it.emplace( new Operation(OP_FMUL, P3, divisor, P2_2));
    it.nextInBlock();
    it.emplace( new Operation(OP_FSUB, P3_1, const2, P3));
    it.nextInBlock();
    it.emplace( new Operation(OP_FMUL, P3_2, P2_2, P3_1));
    it.nextInBlock();
    
    const Value P4 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p4");
    const Value P4_1 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p4");
    const Value P4_2 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p4");
    it.emplace( new Operation(OP_FMUL, P4, divisor, P3_2));
    it.nextInBlock();
    it.emplace( new Operation(OP_FSUB, P4_1, const2, P4));
    it.nextInBlock();
    it.emplace( new Operation(OP_FMUL, P4_2, P3_2, P4_1));
    it.nextInBlock();
    
    const Value P5 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p5");
    const Value P5_1 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p5");
    const Value P5_2 = method.addNewLocal(op.getOutput().get().type, "%fdiv_p5");
	it.emplace( new Operation(OP_FMUL, P5, divisor, P4_2));
	it.nextInBlock();
	it.emplace( new Operation(OP_FSUB, P5_1, const2, P5));
	it.nextInBlock();
	it.emplace( new Operation(OP_FMUL, P5_2, P4_2, P5_1));
	it.nextInBlock();

    //3. final step: Q = Pn * N
    op.setArgument(0, nominator);
    op.setArgument(1, P5_2);
    op.setOpCode(OP_FMUL);
    
    return it;
}

Literal intermediate::asr(const DataType& type, const Literal& left, const Literal& right)
{
	std::bitset<sizeof(int64_t) * 8> tmp(left.integer);
	if(right.integer < 0)
		throw CompilationError(CompilationStep::GENERAL, "ASR with negative numbers is not implemented");
	for(auto i = 0; i < right.integer; ++i)
	{

		bool MSBSet = tmp.test(type.getScalarBitCount() - 1);
		tmp >>= 1;
		tmp.set(type.getScalarBitCount() - 1, MSBSet);
	}
	if(sizeof(unsigned long) == sizeof(int64_t))
		return Literal(static_cast<uint64_t>(tmp.to_ulong()));
	else
		return Literal(static_cast<uint64_t>(tmp.to_ullong()));
}

Literal intermediate::clz(const DataType& type, const Literal& val)
{
	for(auto i = type.getScalarBitCount() - 1; i >= 0; --i)
	{
		if(((val.integer >> i) & 0x1) == 0x1)
			return Literal(static_cast<int64_t>(type.getScalarBitCount() - 1 - i));
	}
	return Literal(static_cast<int64_t>(type.getScalarBitCount()));
}

Literal intermediate::smod(const DataType& type, const Literal& numerator, const Literal& denominator)
{
	throw CompilationError(CompilationStep::GENERAL, "SMOD is currently not implemented!");
}

Literal intermediate::srem(const DataType& type, const Literal& numerator, const Literal& denominator)
{
	throw CompilationError(CompilationStep::GENERAL, "SREM is currently not implemented!");
}

Literal intermediate::fmod(const DataType& type, const Literal& numerator, const Literal& denominator)
{
	throw CompilationError(CompilationStep::GENERAL, "FMOD is currently not implemented!");
}

Literal intermediate::frem(const DataType& type, const Literal& numerator, const Literal& denominator)
{
	throw CompilationError(CompilationStep::GENERAL, "FREM is currently not implemented!");
}
