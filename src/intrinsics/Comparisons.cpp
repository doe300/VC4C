/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <math.h>

#include "Comparisons.h"
#include "helper.h"
#include "../intermediate/Helper.h"

using namespace vc4c;
using namespace vc4c::intermediate;

static InstructionWalker replaceWithSetBoolean(InstructionWalker it, const Value& dest, const ConditionCode& trueCode, const Value& value = BOOL_TRUE)
{
	/*
	 * This code allows us to combine all "mov.ifz x, true" and "mov.ifnz x, false", since only 1 literal value is used anymore
	 *
	 * NOTE: re-ordering might move them away. Also, they might get merged with other instructions
	 * -> They are not guaranteed to be merged together -> Can't set flags (since the flags of first will influence the second).
	 * This is no problem though, since everywhere the boolean value is used (e.g. branches, the flags are re-set)
	 */
	if(dest.hasRegister(REG_NOP))
	{
		//comparisons into nothing are only useful for their flags
		// -> no need to set the boolean result
		it.erase();
		//so next instruction is not skipped
		it.previousInBlock();
	}
	else
	{
		it.emplace(new MoveOperation(dest, value, trueCode));
		it.nextInBlock();
		it.reset(new Operation("xor", dest, value, value, trueCode.invert()));
	}
	return it;
}

InstructionWalker intermediate::intrinsifyIntegerRelation(Method& method, InstructionWalker it, const Comparison* comp)
{
    //http://llvm.org/docs/LangRef.html#icmp-instruction
	const Value tmp = method.addNewLocal(comp->getFirstArg().type, "%icomp");
    if(COMP_EQ.compare(comp->opCode) == 0)
    {
        // a == b <=> a xor b == 0 [<=> a - b == 0]
        if(comp->getFirstArg().hasType(ValueType::LOCAL) && comp->getSecondArg().get().hasLiteral(Literal(0L)))
            //special case for a == 0
            //does not save instructions, but does not force value a to be on register-file A (since B is reserved for literal 0)
            it.emplace(new MoveOperation(NOP_REGISTER, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        else
            it.emplace(new Operation("xor", NOP_REGISTER, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_SET);
    }
    else if(COMP_NEQ.compare(comp->opCode) == 0)
    {
        // a != b <=> a xor b != 0 [<=> a - b != 0] [<=> max(a,b) - min(a,b) != 0]
        if(comp->getFirstArg().hasType(ValueType::LOCAL) && comp->getSecondArg().get().hasLiteral(Literal(0L)))
            //special case for a != 0
            //does not save instructions, but does not force value a to be on register-file A (since B is reserved for literal 0)
            it.emplace( new MoveOperation(NOP_REGISTER, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        else
            it.emplace(new Operation("xor", NOP_REGISTER, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        //true if ZERO is not set, otherwise false
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_CLEAR);
    }
    else if(COMP_UNSIGNED_LT.compare(comp->opCode) == 0)
    {
        //a < b [<=> min(a, b) != b] <=> max(a, b) != a
        it.emplace(new Operation("max", tmp, comp->getFirstArg(), comp->getSecondArg(), comp->conditional));
        it.nextInBlock();
        it.emplace(new Operation("xor", NOP_REGISTER, tmp, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        //true if ZERO is not set, otherwise false
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_CLEAR);
    }
    else if(COMP_UNSIGNED_LE.compare(comp->opCode) == 0)
    {
        //a <= b [<=> min(a, b) == a] <=> max(a, b) == b
        it.emplace(new Operation("max", tmp, comp->getFirstArg(), comp->getSecondArg(), comp->conditional));
        it.nextInBlock();
        it.emplace(new Operation("xor", NOP_REGISTER, tmp, comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_SET);
    }
    else if(COMP_SIGNED_LT.compare(comp->opCode) == 0)
    {
        //a < b [<=> min(a, b) != b] [<=> max(a, b) != a] <=> a - b < 0

    	//TODO fails even more for boost-compute/test_insertion_sort
//		if(comp->getFirstArg().type.getScalarBitCount() < 32)
//		{
//			//for non 32-bit types, we need to make sure the correct flag (MSB instead of always the 31th bit) is checked
//			it.emplace(new Operation("sub", tmp, comp->getFirstArg(), comp->getSecondArg(), comp->conditional));
//			it.nextInBlock();
//			it.emplace(new Nop(DelayType::WAIT_REGISTER));
//			it.nextInBlock();
//			it.emplace(new MoveOperation(NOP_REGISTER, tmp, comp->conditional, SetFlag::SET_FLAGS));
//			it->setUnpackMode(Unpack::unpackTo32Bit(tmp.type));
//		}
//		else
		{
			it.emplace(new Operation("sub", NOP_REGISTER, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
		}
		it.nextInBlock();
		//true if NEGATIVE is set, otherwise false
		it = replaceWithSetBoolean(it, comp->getOutput(), COND_NEGATIVE_SET);
    }
    else if(COMP_SIGNED_LE.compare(comp->opCode) == 0)
    {
        //a <= b <=> min(a, b) == a [<=> max(a, b) == b]
        it.emplace(new Operation("min", tmp, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it.emplace(new Operation("xor", NOP_REGISTER, tmp, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_SET);
    }
    else
    	throw CompilationError(CompilationStep::OPTIMIZER, "Unrecognized integer comparison", comp->opCode);
    
    return it;
}

InstructionWalker intermediate::intrinsifyFloatingRelation(Method& method, InstructionWalker it, const Comparison* comp)
{
    //since we do not supprt NaNs/Inf and denormals, all ordered comparisons are treated as unordered
	// -> "don't care if value could be Nan/Inf"

    //http://llvm.org/docs/LangRef.html#fcmp-instruction
	const Value tmp = method.addNewLocal(comp->getFirstArg().type, comp->getOutput().get().local->name);
    if(COMP_TRUE.compare(comp->opCode) == 0)
    {
        // true
        it.reset(new MoveOperation(comp->getOutput(), BOOL_TRUE, comp->conditional, SetFlag::SET_FLAGS));
    }
    else if(COMP_FALSE.compare(comp->opCode) == 0)
    {
        // false
        it.reset(new MoveOperation(comp->getOutput(), BOOL_FALSE, comp->conditional, SetFlag::SET_FLAGS));
    }
    else if(COMP_ORDERED_EQ.compare(comp->opCode) == 0 || COMP_UNORDERED_EQ.compare(comp->opCode) == 0)
    {
        // a == b <=> a xor b == 0 [<=> a - b == 0]
        it.emplace(new Operation("xor", NOP_REGISTER, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_SET);
    }
    else if(COMP_ORDERED_NEQ.compare(comp->opCode) == 0 || COMP_UNORDERED_NEQ.compare(comp->opCode) == 0)
    {
        //a != b <=> a xor b != 0
        it.emplace(new Operation("xor", NOP_REGISTER, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_CLEAR);
    }
    else if(COMP_ORDERED_LT.compare(comp->opCode) == 0 || COMP_UNORDERED_LT.compare(comp->opCode) == 0)
    {
        //a < b [<=> min(a, b) != b] [<=> max(a, b) != a] <=> a - b < 0
        it.emplace(new Operation("fsub", NOP_REGISTER, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        //true if NEGATIVE is set, otherwise false
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_NEGATIVE_SET);
    }
    else if(COMP_ORDERED_LE.compare(comp->opCode) == 0 || COMP_UNORDERED_LE.compare(comp->opCode) == 0)
    {
        //a <= b <=> min(a, b) == a [<=> max(a, b) == b]
        it.emplace(new Operation("fmin", tmp, comp->getFirstArg(), comp->getSecondArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it.emplace(new Operation("xor", NOP_REGISTER, tmp, comp->getFirstArg(), comp->conditional, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_SET);
    }
    else if(COMP_ORDERED.compare(comp->opCode) == 0)
    {
        //ord(a, b) <=> a != NaN && b != NaN
    	const Value tmp0 = method.addNewLocal(TYPE_BOOL, "%ordered");
    	const Value tmp1 = method.addNewLocal(TYPE_BOOL, "%ordered");
    	//tmp0 = a != NaN
    	it.emplace(new Operation("xor", tmp0, comp->getFirstArg(), FLOAT_NAN, comp->conditional));
    	it.nextInBlock();
    	//tmp1 = b != NaN
    	it.emplace(new Operation("xor", tmp1, comp->getSecondArg(), FLOAT_NAN, comp->conditional));
		it.nextInBlock();
		//tmp = tmp0 && tmp1
		it.emplace(new Operation("and", tmp, tmp0, tmp1, comp->conditional, SetFlag::SET_FLAGS));
		it.nextInBlock();
		//res = tmp != 0 <=> (tmp0 && tmp1) == 0 <=> (a != NaN) && (b != NaN)
		it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_CLEAR);
    }
    else if(COMP_UNORDERED.compare(comp->opCode) == 0)
    {
    	//uno(a, b) <=> a == NaN || b == NaN
		const Value tmp0 = method.addNewLocal(TYPE_BOOL, "%ordered");
		const Value tmp1 = method.addNewLocal(TYPE_BOOL, "%ordered");
		//tmp0 = a != NaN
		it.emplace(new Operation("xor", tmp0, comp->getFirstArg(), FLOAT_NAN, comp->conditional));
		it.nextInBlock();
		//tmp1 = b != NaN
		it.emplace(new Operation("xor", tmp1, comp->getSecondArg(), FLOAT_NAN, comp->conditional));
		it.nextInBlock();
		//tmp = tmp0 && tmp1
		it.emplace(new Operation("and", tmp, tmp0, tmp1, comp->conditional, SetFlag::SET_FLAGS));
		it.nextInBlock();
		//res = tmp == 0 <=> (tmp0 || tmp1) == 0 <=> (a != NaN) || (b != NaN) == 0 <=> (a == NaN) || (b == NaN)
		it = replaceWithSetBoolean(it, comp->getOutput(), COND_ZERO_SET);
    }
    else
    	throw CompilationError(CompilationStep::OPTIMIZER, "Unrecognized floating-point comparison", comp->opCode);
    
    return it;
}

InstructionWalker intermediate::insertIsNegative(InstructionWalker it, const Value& src, Value& dest)
{
	if(src.hasType(ValueType::LITERAL))
	{
		dest = src.literal.integer < 0 ? BOOL_TRUE : BOOL_FALSE;
	}
	else
	{
		it.emplace(new Operation("shr", NOP_REGISTER, src, Value(Literal(static_cast<unsigned long>(src.type.getScalarBitCount() - 1)), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
		it.nextInBlock();
		//some dummy instruction to be replaced
		it.emplace(new Nop(DelayType::WAIT_REGISTER));
		replaceWithSetBoolean(it, dest, COND_ZERO_CLEAR);
		it.nextInBlock();
	}
	return it;
}
