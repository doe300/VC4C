/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Helper.h"

#include "CompilationError.h"
#include "config.h"

#include <algorithm>

using namespace vc4c;
using namespace vc4c::intermediate;

InstructionWalker intermediate::insertVectorRotation(InstructionWalker it, const Value& src, const Value& offset, const Value& dest, const Direction direction)
{
    /*
     * The vector rotation is done by
     * 1. rotating the inputs to the MUL ALU by the value specified in the small-immediate
     * - the inputs MUST be accumulators!
     * (2. calculating the result of the MUL ALU)
     * (3. writing the result to the MUL output)
     * 
     * Since we use the rotation as isolated instruction, we can use following simplifications:
     * - use just 1 input
     * - use move on the MUL ALU as instruction
     */

	//0. if the container is a literal, no need to rotate, simply move, since all elements have the same value
	if(src.isLiteralValue())
	{
		it.emplace(  new MoveOperation(dest, src));
		it.nextInBlock();
		return it;
	}
    
    //1. set amount of rotation
    Value appliedOffset(UNDEFINED_VALUE);
    if(offset.hasType(ValueType::LITERAL))
    {
        //if the offset is a literal, set it as small immediate
        appliedOffset = Value(SmallImmediate(offset.literal.integer), offset.type);
        if(direction == Direction::DOWN)
        {
            appliedOffset.immediate.value = (16 - offset.literal.integer) % 16;
        }
        else
        {
        	appliedOffset.immediate.value = offset.literal.integer % 16;
        }
        if(appliedOffset.immediate.value == 0)
        	//convert into simple move operation
        	appliedOffset= INT_ZERO;
        else
        	appliedOffset.immediate = SmallImmediate::fromRotationOffset(appliedOffset.immediate);
    }
    else if(offset.hasType(ValueType::SMALL_IMMEDIATE))
    {
    	appliedOffset = offset;
    	//vector is rotated by offset-constant not by rotation constant -> convert to rotation constant
    	if(offset.immediate.getIntegerValue())
    	{
    		if(direction == Direction::DOWN)
			{
				appliedOffset.immediate.value = (16 - offset.immediate.value) % 16;
			}
    		else
    		{
    			appliedOffset.immediate.value = offset.immediate.value % 16;
    		}
    		if(appliedOffset.immediate.value == 0)
				appliedOffset= INT_ZERO;
			else
				appliedOffset.immediate = SmallImmediate::fromRotationOffset(appliedOffset.immediate);
    	}
    }
    else
    {
        //if the offset is not known, write it into r5
        appliedOffset = ROTATION_REGISTER;
        if(direction == Direction::UP)
            //r5 = offset
            it.emplace( new MoveOperation(ROTATION_REGISTER, offset));
        else
        {
            //to exclude the case case 16-0 = 16
            it.emplace( new MoveOperation(NOP_REGISTER, offset, COND_ALWAYS, SetFlag::SET_FLAGS));
            it.nextInBlock();
            //r5 = 16 - offset
            it.emplace( new Operation(OP_SUB, ROTATION_REGISTER, Value(Literal(static_cast<int64_t>(16)), TYPE_INT8), offset, COND_ZERO_CLEAR));
            it.nextInBlock();
            it.emplace( new MoveOperation(ROTATION_REGISTER, INT_ZERO, COND_ZERO_SET));
        }
        it.nextInBlock();
    }
    
    //2. create rotation instruction
    if(appliedOffset.hasLiteral(INT_ZERO.literal))
        //a rotation by 0 is a simple move
        it.emplace( new MoveOperation(dest, src));
    else
    {
    	//we insert a delay before every vector rotation, since the rotated value can't be written in the previous
    	//instruction and a NOP guarantees it. Also, it should be removed by reordering in most cases
    	it.emplace(new Nop(DelayType::WAIT_REGISTER));
    	it.nextInBlock();
        it.emplace( new VectorRotation(dest, src, appliedOffset));
    }
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertReplication(InstructionWalker it, const Value& src, const Value& dest, const bool useDestionation)
{
    //distribute value 0 to all positions in the vector
    it.emplace( new intermediate::MoveOperation(Value(REG_REPLICATE_ALL, src.type), src));
    it.nextInBlock();
    if(useDestionation)
    {
        //"Reading r5 returns the per-quad 32-bit value replicated across the four elements of that quad" (p. 18)
        it.emplace( new intermediate::MoveOperation(dest, Value(REG_REPLICATE_ALL, src.type)));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker intermediate::insertVectorExtraction(InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest)
{
	if(container.isLiteralValue())
	{
		//vector extraction from literal is a simple move of the first element, since all elements of a literal are the same
		it.emplace(  new MoveOperation(dest, container));
		it.nextInBlock();
		return it;
	}
	return insertVectorRotation(it, container, index, dest, Direction::DOWN);
}

InstructionWalker intermediate::insertVectorInsertion(InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& value)
{
	const Value tmp = method.addNewLocal(container.type.getElementType(), "%vector_insert");
	//1) rotate scalar value to the correct vector-position
	it = intermediate::insertVectorRotation(it, value, index, tmp, intermediate::Direction::UP);
    //2) create condition only met in given index
    it.emplace( new intermediate::Operation(OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, index, COND_ALWAYS, SetFlag::SET_FLAGS));
    it.nextInBlock();
    //3) move when condition is met
    it.emplace( new intermediate::MoveOperation(container, tmp, COND_ZERO_SET));
    it->setDecorations(InstructionDecorations::ELEMENT_INSERTION);
    it.nextInBlock();
    return it;
}

/*
 * Since we pretend for UNDEFINED indices, that the sequence continues, there may be a sequence where the overlapping
 * indices are actually undefined and therefore don't need to be copied from the second vector (e.g. by moving 3-element vector into 4-element vector).
 */
static bool checkIndicesNotUndefined(const ContainerValue& container, const unsigned int startIndex)
{
	for(auto i = startIndex; i < container.elements.size(); ++i)
		if(container.elements.at(i).isUndefined())
			return false;
	return true;
}

InstructionWalker intermediate::insertVectorShuffle(InstructionWalker it, Method& method, const Value& destination, const Value& source0, const Value& source1, const Value& mask)
{
    if(mask.isUndefined())
    {
        //order does not matter
        //TODO is anything required to be done at all??
        //Make sure, as of this point the destination is valid and has a register associated with it
        throw CompilationError(CompilationStep::GENERAL, "Cannot shuffle a vector with an undefined mask", mask.to_string());
    }
    else if(mask.isZeroInitializer())
    {
        //initialize all values with the first index
		return intermediate::insertReplication(it, source0, destination);
    }
    else if(!mask.hasType(ValueType::CONTAINER))
    	//TODO could at least support this for one vector (e.g. second one is undefined or the same as the first) by selecting (at run-time) the vector element and rotating
        throw CompilationError(CompilationStep::GENERAL, "Shuffling vectors with non-constant mask-layout is not supported yet", mask.to_string());
    
    
    //if all indices are ascending (correspond to the elements of source 0), we can simply copy it
    //if all indices point to the same, replicate this index over the vector
    bool indicesCorrespond = mask.container.isElementNumber();
    bool allIndicesSame = mask.container.isAllSame();
    if(indicesCorrespond)
    {
        //the vector is copied in-order
        if(mask.container.elements.size() > source0.type.getVectorWidth() && checkIndicesNotUndefined(mask.container, source0.type.getVectorWidth()))
        {
        	//The second vector participates in the shuffling
        	//move the first vector in-order
        	it.emplace(new intermediate::MoveOperation(destination, source0));
        	it.nextInBlock();
        	//rotate the second vector with the size of the first as offset
        	const Value secondRotated = method.addNewLocal(destination.type, "%vector_shuffle");
        	const Value numElementsFirst(Literal(static_cast<uint64_t>(source0.type.num)), TYPE_INT8);
        	it = insertVectorRotation(it, source1, numElementsFirst, secondRotated, Direction::UP);
        	//insert the elements of the second vector with an element-number of higher or equals the size of the first vector into the result
        	it.emplace(new intermediate::Operation(OP_SUB, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, numElementsFirst, COND_ALWAYS, SetFlag::SET_FLAGS));
        	it.nextInBlock();
        	it.emplace(new intermediate::MoveOperation(destination, secondRotated, COND_NEGATIVE_CLEAR));
        }
        else
        {
        	//only one vector participates in the shuffling and the elements are inserted in-order -> simply move
        	it.emplace( new MoveOperation(destination, source0));
        }
        it.nextInBlock();
		return it;
    }
    if(allIndicesSame)
    {
        const int64_t indexValue = mask.container.elements[0].literal.integer < static_cast<int64_t>(source0.type.getVectorWidth()) ? mask.container.elements[0].literal.integer : mask.container.elements[0].literal.integer - static_cast<long>(source0.type.num);
        const Value source = mask.container.elements[0].literal.integer < static_cast<int64_t>(source0.type.getVectorWidth()) ? source0 : source1;
        //if all indices same, replicate
        Value tmp(UNDEFINED_VALUE);
        if(indexValue == 0)
            tmp = source;
        else
        {
            //if the index to be used is not 0, rotate to position 0
            tmp = method.addNewLocal(source.type, "%vector_shuffle");
            it = insertVectorRotation(it, source, Value(Literal(indexValue), TYPE_INT8), tmp, Direction::DOWN);
        }
        return insertReplication(it, tmp, destination);
    }
    
    //zero out destination first, also required so register allocator finds unconditional write to destination
    if(destination.hasType(ValueType::LOCAL) && destination.local->getUsers(LocalUser::Type::WRITER).empty())
    {
		it.emplace(new intermediate::MoveOperation(destination, INT_ZERO));
		it.nextInBlock();
    }

    //mask is container of literals, indices have arbitrary order
    for(std::size_t i = 0; i < mask.container.elements.size(); ++i)
    {
        Value index = mask.container.elements.at(i);
        if(index.isUndefined())
        	//don't write anything at this position
        	continue;
        if(!index.hasType(ValueType::LITERAL))
        	throw CompilationError(CompilationStep::GENERAL, "Invalid mask value", mask.to_string(false, true));
        const Value& container = index.literal.integer < static_cast<int64_t>(source0.type.getVectorWidth()) ? source0 : source1;
        if(index.literal.integer >= static_cast<int64_t>(source0.type.getVectorWidth()))
        	index.literal.integer = index.literal.integer - source0.type.getVectorWidth();
        index.type = TYPE_INT8;
        const Value tmp = method.addNewLocal(container.type.getElementType(), "%vector_shuffle");
        it = insertVectorExtraction(it, method, container, index, tmp);
        it = insertVectorInsertion(it, method, destination, Value(Literal(static_cast<int64_t>(i)), TYPE_INT8), tmp);
    }
    return it;
}

InstructionWalker intermediate::insertMakePositive(InstructionWalker it, Method& method, const Value& src, Value& dest)
{
	if(src.hasType(ValueType::LITERAL))
	{
		bool isNegative = src.literal.integer < 0;
		dest = isNegative ? Value(Literal(-src.literal.integer), src.type) : src;
	}
	else if(src.hasType(ValueType::CONTAINER))
	{
		dest = Value(ContainerValue(), src.type);
		dest.container.elements.reserve(src.container.elements.size());
		for(const auto& elem : src.container.elements)
		{
			if(!elem.hasType(ValueType::LITERAL))
				throw CompilationError(CompilationStep::OPTIMIZER, "Can't handle container with non-literal values", src.to_string(false, true));
			bool isNegative = elem.literal.integer < 0;
			dest.container.elements.push_back(isNegative ? Value(Literal(-elem.literal.integer), elem.type) : elem);
		}
	}
	else if(src.hasType(ValueType::LOCAL) && src.local->getSingleWriter() != nullptr && has_flag(dynamic_cast<const IntermediateInstruction*>(src.local->getSingleWriter())->decoration, InstructionDecorations::UNSIGNED_RESULT))
	{
		//the value is already unsigned
		dest = src;
	}
	else
	{
		//do we have a negative number?
		it.emplace(new Operation(OP_SHR, NOP_REGISTER, src, Value(Literal(static_cast<uint64_t>(src.type.getScalarBitCount() - 1)), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
		it.nextInBlock();
		//flip all bits
		const Value tmp = method.addNewLocal(src.type, "%twos_complement");
		it.emplace(new Operation(OP_NOT, tmp, src, COND_ZERO_CLEAR));
		it.nextInBlock();
		//add 1
		it.emplace(new Operation(OP_ADD, dest, tmp, INT_ONE, COND_ZERO_CLEAR));
		it.nextInBlock();
		//simply copy for already positive numbers
		it.emplace(new MoveOperation(dest, src, COND_ZERO_SET));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker intermediate::insertInvertSign(InstructionWalker it, Method& method, const Value& src, Value& dest, const ConditionCode cond)
{
	if(src.hasType(ValueType::LITERAL))
	{
		it.emplace(new MoveOperation(dest, Value(Literal(-src.literal.integer), src.type), cond));
		it.nextInBlock();
		it.emplace(new MoveOperation(dest, src, cond.invert()));
		it.nextInBlock();
	}
	else
	{
		//flip all bits
		const Value tmp = method.addNewLocal(src.type, "%twos_complement");
		it.emplace(new Operation(OP_NOT, tmp, src, cond));
		it.nextInBlock();
		//add 1
		it.emplace(new Operation(OP_ADD, dest, tmp, INT_ONE, cond));
		it.nextInBlock();
		//otherwise, simply copy
		it.emplace(new MoveOperation(dest, src, cond.invert()));
		it.nextInBlock();
	}
	return it;
}

InstructionWalker intermediate::insertCalculateIndices(InstructionWalker it, Method& method, const Value& container, const Value& dest, const std::vector<Value>& indices, const bool firstIndexIsElement)
{
	//handle multi-level indices
	Value offset = INT_ZERO;
	DataType subContainerType = container.type;
	for(const Value& index : indices)
	{
		Value subOffset(UNDEFINED_VALUE);
		if(subContainerType.isPointerType() || subContainerType.getArrayType())
		{
			//index is index in pointer/array
			//-> add offset of element at given index to global offset
			if(index.getLiteralValue())
			{
				subOffset = Value(Literal(index.getLiteralValue()->integer * subContainerType.getElementType().getPhysicalWidth()), TYPE_INT32);
			}
			else
			{
				subOffset = method.addNewLocal(TYPE_INT32, "%index_offset");
				//FIXME is wrong for array types? E.g. test_global_data.cl loading global data (container is pointer to array, element 0, but for second element the physical-size of the array is used as offset-factor)
				it.emplace(new intermediate::Operation("mul", subOffset, index, Value(Literal(static_cast<uint64_t>(subContainerType.getElementType().getPhysicalWidth())), TYPE_INT32)));
				it.nextInBlock();
			}

			//according to SPIR-V 1.2 specification, the type doesn't change if the first index is the "element":
			//"The type of Base after being dereferenced with Element is still the same as the original type of Base."
			if(!firstIndexIsElement || &index != &indices.front())
				subContainerType = subContainerType.getElementType().getElementType(index.getLiteralValue().value_or(Literal(static_cast<int64_t>(ANY_ELEMENT))).integer).toPointerType();
		}
		else if(subContainerType.getStructType())
		{
			//index is element in struct -> MUST be literal
			if(!index.getLiteralValue())
				throw CompilationError(CompilationStep::LLVM_2_IR, "Can't access struct-element with non-literal index", index.to_string());

			subOffset = Value(Literal(static_cast<uint64_t>(subContainerType.getStructType().value()->getStructSize(static_cast<int>(index.getLiteralValue()->integer)))), TYPE_INT32);
			subContainerType = subContainerType.getElementType(static_cast<int>(index.getLiteralValue()->integer));
		}
		else
			throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid container-type to retrieve element via index", subContainerType.to_string());

		if(offset.getLiteralValue() && subOffset.getLiteralValue())
		{
			offset = Value(Literal(offset.getLiteralValue()->integer + subOffset.getLiteralValue()->integer), TYPE_INT32);
		}
		else if(offset.isZeroInitializer())
		{
			//previous offset is zero -> zero + x = x
			offset = subOffset;
		}
		else if(subOffset.isZeroInitializer())
		{
			//sub-offset is zero -> x + zero = x
			//offset = offset -> do nothing
		}
		else
		{
			Value tmp = method.addNewLocal(TYPE_INT32, "%index_offset");
			it.emplace(new intermediate::Operation(OP_ADD, tmp, offset, subOffset));
			it.nextInBlock();
			offset = tmp;
		}
	}
	//add last offset to container
	it.emplace(new intermediate::Operation(OP_ADD, dest, container, offset));
	it.nextInBlock();

	/*
	 * associates the index with the local/parameter it refers to.
	 * This is required, so the input/output-parameters are correctly recognized
	 *
	 * NOTE: The associated index can only be set, if there is a single literal index.
	 * (Or the element is element 0, than the reference-index can be retrieved from the second index)
	 */
	Value index = UNDEFINED_VALUE;
	if(indices.size() == 1)
		index = indices.at(0);
	if(firstIndexIsElement && indices.at(0).isZeroInitializer())
		index = indices.size() > 1 ? indices.at(1) : UNDEFINED_VALUE;
	const int refIndex = index.getLiteralValue().value_or(Literal(static_cast<int64_t>(ANY_ELEMENT))).integer;
	const_cast<std::pair<Local*, int>&>(dest.local->reference) = std::make_pair(container.local, refIndex);

	if(dest.type != subContainerType)
		throw CompilationError(CompilationStep::LLVM_2_IR, "Types of retrieving indices do not match!", subContainerType.to_string());

	return it;
}

InstructionWalker intermediate::insertByteSwap(InstructionWalker it, Method& method, const Value& src, const Value& dest)
{
	/*
	 * llvm.bswap:
	 * "The llvm.bswap.i16 intrinsic returns an i16 value that has the high and low byte of the input i16 swapped.
	 * Similarly, the llvm.bswap.i32 intrinsic returns an i32 value that has the four bytes of the input i32 swapped,
	 * so that if the input bytes are numbered 0, 1, 2, 3 then the returned i32 will have its bytes in 3, 2, 1, 0 order. "
	 */
	auto numBytes = src.type.getScalarBitCount() / 8;

	//TODO loses signedness!

	if(numBytes == 2)
	{
		// ? ? A B -> 0 ? ? A
		const Value tmpA0 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_SHR, tmpA0, src, Value(SmallImmediate::fromInteger(8).value(), TYPE_INT8)));
		it.nextInBlock();
		// ? ? A B -> ? A B 0
		const Value tmpB0 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_SHL, tmpB0, src, Value(SmallImmediate::fromInteger(8).value(), TYPE_INT8)));
		it.nextInBlock();
		// 0 ? ? A -> 0 0 0 A
		const Value tmpA1 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_AND, tmpA1, tmpA0, Value(Literal(TYPE_INT8.getScalarWidthMask()), src.type)));
		it.nextInBlock();
		// ? A B 0 -> 0 0 B 0
		const Value tmpB1 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_AND, tmpB1, tmpB0, Value(Literal(TYPE_INT8.getScalarWidthMask() << 8), src.type)));
		it.nextInBlock();
		// 0 0 0 A | 0 0 B 0 -> 0 0 A B
		it.emplace(new Operation(OP_OR, dest, tmpA1, tmpB1));
		it.nextInBlock();
	}
	else if(numBytes == 4)
	{
		// A B C D -> B C D A
		const Value tmpAC0 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_ROR, tmpAC0, src, Value(Literal(static_cast<int64_t>(24)), TYPE_INT8)));
		it.nextInBlock();
		// A B C D -> D A B C
		const Value tmpBD0 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_ROR, tmpBD0, src, Value(Literal(static_cast<int64_t>(16)), TYPE_INT8)));
		it.nextInBlock();
		// B C D A -> 0 0 0 A
		const Value tmpA1 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_AND, tmpA1, tmpAC0, Value(Literal(TYPE_INT8.getScalarWidthMask()), src.type)));
		it.nextInBlock();
		// D A B C -> 0 0 B 0
		const Value tmpB1 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_AND, tmpB1, tmpBD0, Value(Literal(TYPE_INT8.getScalarWidthMask() << 8), src.type)));
		it.nextInBlock();
		// B C D A -> 0 C 0 0
		const Value tmpC1 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_AND, tmpC1, tmpAC0, Value(Literal(TYPE_INT8.getScalarWidthMask() << 16), src.type)));
		it.nextInBlock();
		// D A B C -> D 0 0 0
		const Value tmpD1 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_AND, tmpD1, tmpBD0, Value(Literal(TYPE_INT8.getScalarWidthMask() << 24), src.type)));
		it.nextInBlock();
		// 0 0 0 A | 0 0 B 0 -> 0 0 B A
		const Value tmpAB2 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_OR, tmpAB2, tmpA1, tmpB1));
		it.nextInBlock();
		// 0 C 0 0 | D 0 0 0 -> D C 0 0
		const Value tmpCD2 = method.addNewLocal(src.type, "byte_swap");
		it.emplace(new Operation(OP_OR, tmpCD2, tmpC1, tmpD1));
		it.nextInBlock();
		// 0 0 B A | D C 0 0 -> D C B A
		it.emplace(new Operation(OP_OR, dest, tmpAB2, tmpCD2));
		it.nextInBlock();
	}
	else
		throw CompilationError(CompilationStep::GENERAL, "Invalid number of bytes for byte-swap", std::to_string(numBytes));

	return it;
}
