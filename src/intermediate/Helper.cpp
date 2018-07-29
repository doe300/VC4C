/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Helper.h"

#include "CompilationError.h"
#include "TypeConversions.h"
#include "config.h"
#include "log.h"

#include <algorithm>

using namespace vc4c;
using namespace vc4c::intermediate;

InstructionWalker intermediate::insertVectorRotation(
    InstructionWalker it, const Value& src, const Value& offset, const Value& dest, const Direction direction)
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

    // 0. if the container is a literal, no need to rotate, simply move, since all elements have the same value
    // the same counts for any input register where all elements have the same value
    if(src.isLiteralValue() || src.hasRegister(REG_UNIFORM) || src.hasRegister(REG_QPU_NUMBER))
    {
        it.emplace(new MoveOperation(dest, src));
        it.nextInBlock();
        return it;
    }

    // 1. set amount of rotation
    Value appliedOffset(UNDEFINED_VALUE);
    if(offset.hasType(ValueType::LITERAL))
    {
        // if the offset is a literal, set it as small immediate
        /*
         * Possible inputs and their outputs:
         * negative offset, rotate upwards   -> rotate downwards with absolute value
         * positive offset, rotate upwards   -> rotate upwards with absolute value
         * negative offset, rotate downwards -> rotate upwards with absolute value
         * positive offset, rotate downwards -> rotate downwards with absolute value
         */
        int32_t offsetValue = offset.literal.signedInt();
        const Direction actualDirection = offset.literal.signedInt() >= 0 ?
            direction :
            (direction == Direction::DOWN ? Direction::UP : Direction::DOWN);
        offsetValue = actualDirection != direction ? -offsetValue : offsetValue;

        if(actualDirection == Direction::DOWN)
        {
            offsetValue = (16 - offsetValue) % 16;
        }
        else
        {
            offsetValue = offsetValue % 16;
        }
        if(offsetValue == 0)
            // convert into simple move operation
            appliedOffset = INT_ZERO;
        else
            appliedOffset = Value(SmallImmediate::fromRotationOffset(static_cast<uint8_t>(offsetValue)), offset.type);
    }
    else if(offset.hasType(ValueType::SMALL_IMMEDIATE))
    {
        appliedOffset = offset;
        // vector is rotated by offset-constant not by rotation constant -> convert to rotation constant
        if(offset.immediate.getIntegerValue())
        {
            if(direction == Direction::DOWN)
            {
                appliedOffset.immediate.value = static_cast<unsigned char>((16 - offset.immediate.value) % 16);
            }
            else
            {
                appliedOffset.immediate.value = offset.immediate.value % 16;
            }
            if(appliedOffset.immediate.value == 0)
                appliedOffset = INT_ZERO;
            else
                appliedOffset.immediate = SmallImmediate::fromRotationOffset(appliedOffset.immediate);
        }
    }
    else
    {
        // if the offset is not known, write it into r5
        appliedOffset = ROTATION_REGISTER;
        if(direction == Direction::UP)
            // r5 = offset
            it.emplace(new MoveOperation(ROTATION_REGISTER, offset));
        else
        {
            // to exclude the case case 16-0 = 16
            it.emplace(new MoveOperation(NOP_REGISTER, offset, COND_ALWAYS, SetFlag::SET_FLAGS));
            it.nextInBlock();
            // r5 = 16 - offset
            it.emplace(
                new Operation(OP_SUB, ROTATION_REGISTER, Value(Literal(16u), TYPE_INT8), offset, COND_ZERO_CLEAR));
            it.nextInBlock();
            it.emplace(new MoveOperation(ROTATION_REGISTER, INT_ZERO, COND_ZERO_SET));
        }
        it.nextInBlock();
    }

    // 2. create rotation instruction
    if(appliedOffset.hasLiteral(INT_ZERO.literal))
        // a rotation by 0 is a simple move
        it.emplace(new MoveOperation(dest, src));
    else
    {
        // we insert a delay before every vector rotation, since the rotated value can't be written in the previous
        // instruction and a NOP guarantees it. Also, it should be removed by reordering in most cases
        it.emplace(new Nop(DelayType::WAIT_REGISTER));
        it.nextInBlock();
        it.emplace(new VectorRotation(dest, src, appliedOffset));
    }
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertReplication(
    InstructionWalker it, const Value& src, const Value& dest, const bool useDestionation)
{
    // distribute value 0 to all positions in the vector
    it.emplace(new intermediate::MoveOperation(Value(REG_REPLICATE_ALL, src.type), src));
    it.nextInBlock();
    if(useDestionation)
    {
        //"Reading r5 returns the per-quad 32-bit value replicated across the four elements of that quad" (p. 18)
        it.emplace(new intermediate::MoveOperation(dest, Value(REG_REPLICATE_ALL, src.type)));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker intermediate::insertVectorExtraction(
    InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest)
{
    if(container.isLiteralValue() || container.hasRegister(REG_UNIFORM) || container.hasRegister(REG_QPU_NUMBER))
    {
        // vector extraction from literal is a simple move of the first element, since all elements of a literal are the
        // same
        it.emplace(new MoveOperation(dest, container));
        it.nextInBlock();
        return it;
    }
    return insertVectorRotation(it, container, index, dest, Direction::DOWN);
}

InstructionWalker intermediate::insertVectorInsertion(
    InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& value)
{
    Value tmp = UNDEFINED_VALUE;
    if(value.isLiteralValue() || value.hasRegister(REG_UNIFORM) || value.hasRegister(REG_QPU_NUMBER))
    {
        // simplified version, just insert into container at index (no rotation necessary)
        tmp = value;
    }
    else
    {
        tmp = method.addNewLocal(container.type.getElementType(), "%vector_insert");
        // 1) rotate scalar value to the correct vector-position
        it = intermediate::insertVectorRotation(it, value, index, tmp, intermediate::Direction::UP);
    }
    // 2) create condition only met in given index
    it.emplace(new intermediate::Operation(
        OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, index, COND_ALWAYS, SetFlag::SET_FLAGS));
    it.nextInBlock();
    // 3) move when condition is met
    it.emplace(new intermediate::MoveOperation(container, tmp, COND_ZERO_SET));
    it->addDecorations(InstructionDecorations::ELEMENT_INSERTION);
    it.nextInBlock();
    return it;
}

/*
 * Since we pretend for UNDEFINED indices, that the sequence continues, there may be a sequence where the overlapping
 * indices are actually undefined and therefore don't need to be copied from the second vector (e.g. by moving 3-element
 * vector into 4-element vector).
 */
static bool checkIndicesNotUndefined(const ContainerValue& container, const unsigned int startIndex)
{
    for(auto i = startIndex; i < container.elements.size(); ++i)
        if(container.elements[i].isUndefined())
            return false;
    return true;
}

static InstructionWalker insertDynamicVectorShuffle(
    InstructionWalker it, Method& method, const Value& destination, const Value& source, const Value& mask)
{
    // for each element, write rotation offset to element 0 of r5, rotate and insert into result vector
    for(unsigned char i = 0; i < mask.type.getVectorWidth(); ++i)
    {
        Value offsetTmp0 = method.addNewLocal(TYPE_INT8, "%shuffle_offset");
        Value offsetTmp1 = method.addNewLocal(TYPE_INT8, "%shuffle_offset");
        Value resultTmp = method.addNewLocal(source.type, "%shuffle_tmp");
        // Rotate into temporary, because of "An instruction that does a vector rotate by r5 must not immediately follow
        // an instruction that writes to r5." - Broadcom Specification, page 37
        it = insertVectorRotation(it, mask, Value(Literal(i), TYPE_INT8), offsetTmp0, Direction::DOWN);
        // pos 3 -> 1 => rotate up by -2 (14), pos 1 -> 3 => rotate up by 2
        it.emplace(new Operation(OP_SUB, offsetTmp1, Value(Literal(i), TYPE_INT8), offsetTmp0));
        it.nextInBlock();
        it = insertVectorRotation(it, source, offsetTmp1, resultTmp, Direction::UP);

        if(i == 0)
        {
            // the first write to the element needs to unconditional, so the register allocator can find it
            // also, the setting flags does not work for the first element
            it.emplace(new MoveOperation(destination, resultTmp));
        }
        else
        {
            it.emplace(new Operation(OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(Literal(i), TYPE_INT8),
                COND_ALWAYS, SetFlag::SET_FLAGS));
            it.nextInBlock();
            it.emplace(new MoveOperation(destination, resultTmp, COND_ZERO_SET));
            it->addDecorations(InstructionDecorations::ELEMENT_INSERTION);
        }
        it.nextInBlock();
    }
    return it;
}

InstructionWalker intermediate::insertVectorShuffle(InstructionWalker it, Method& method, const Value& destination,
    const Value& source0, const Value& source1, const Value& mask)
{
    if(mask.isUndefined())
    {
        // order does not matter
        // TODO is anything required to be done at all??
        // Make sure, as of this point the destination is valid and has a register associated with it
        throw CompilationError(
            CompilationStep::GENERAL, "Cannot shuffle a vector with an undefined mask", mask.to_string());
    }
    else if(mask.isZeroInitializer())
    {
        // initialize all values with the first index
        return intermediate::insertReplication(it, source0, destination);
    }
    else if(!mask.hasType(ValueType::CONTAINER))
    {
        if(source1.isUndefined())
            return insertDynamicVectorShuffle(it, method, destination, source0, mask);
        // if we have both vectors, build one large vector by appending them
        Value tmpInput = method.addNewLocal(source0.type.toVectorType(mask.type.getVectorWidth()), "%shuffle_input");
        Value tmpSource1 = method.addNewLocal(source1.type.toVectorType(16), "%shuffle_result");
        it.emplace(new MoveOperation(tmpInput, source0));
        it.nextInBlock();
        it = insertVectorRotation(
            it, source1, Value(Literal(source0.type.getVectorWidth()), TYPE_INT8), tmpSource1, Direction::UP);
        it.emplace(new Operation(
            OP_SUB, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(Literal(source0.type.getVectorWidth()), TYPE_INT8)));
        it->setSetFlags(SetFlag::SET_FLAGS);
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpInput, tmpSource1, COND_NEGATIVE_CLEAR));
        it->addDecorations(InstructionDecorations::ELEMENT_INSERTION);
        it.nextInBlock();
        return insertDynamicVectorShuffle(it, method, destination, tmpInput, mask);
    }

    // if all indices are ascending (correspond to the elements of source 0), we can simply copy it
    // if all indices point to the same, replicate this index over the vector
    bool indicesCorrespond = mask.container.isElementNumber();
    bool allIndicesSame = mask.container.isAllSame();
    if(indicesCorrespond)
    {
        // the vector is copied in-order
        if(mask.container.elements.size() > source0.type.getVectorWidth() &&
            checkIndicesNotUndefined(mask.container, source0.type.getVectorWidth()))
        {
            // The second vector participates in the shuffling
            // move the first vector in-order
            it.emplace(new intermediate::MoveOperation(destination, source0));
            it.nextInBlock();
            // rotate the second vector with the size of the first as offset
            const Value secondRotated = method.addNewLocal(destination.type, "%vector_shuffle");
            const Value numElementsFirst(Literal(static_cast<uint32_t>(source0.type.getVectorWidth())), TYPE_INT8);
            it = insertVectorRotation(it, source1, numElementsFirst, secondRotated, Direction::UP);
            // insert the elements of the second vector with an element-number of higher or equals the size of the first
            // vector into the result
            it.emplace(new intermediate::Operation(
                OP_SUB, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, numElementsFirst, COND_ALWAYS, SetFlag::SET_FLAGS));
            it.nextInBlock();
            it.emplace(new intermediate::MoveOperation(destination, secondRotated, COND_NEGATIVE_CLEAR));
        }
        else
        {
            // only one vector participates in the shuffling and the elements are inserted in-order -> simply move
            it.emplace(new MoveOperation(destination, source0));
        }
        it.nextInBlock();
        return it;
    }
    if(allIndicesSame)
    {
        const int32_t indexValue =
            mask.container.elements[0].literal.signedInt() < static_cast<int32_t>(source0.type.getVectorWidth()) ?
            mask.container.elements[0].literal.signedInt() :
            mask.container.elements[0].literal.signedInt() - static_cast<int32_t>(source0.type.getVectorWidth());
        const Value source =
            mask.container.elements[0].literal.signedInt() < static_cast<int32_t>(source0.type.getVectorWidth()) ?
            source0 :
            source1;
        // if all indices same, replicate
        Value tmp(UNDEFINED_VALUE);
        if(indexValue == 0)
            tmp = source;
        else
        {
            // if the index to be used is not 0, rotate to position 0
            tmp = method.addNewLocal(source.type, "%vector_shuffle");
            it = insertVectorRotation(it, source, Value(Literal(indexValue), TYPE_INT8), tmp, Direction::DOWN);
        }
        return insertReplication(it, tmp, destination);
    }

    // zero out destination first, also required so register allocator finds unconditional write to destination
    if(destination.hasType(ValueType::LOCAL) && destination.local->getUsers(LocalUse::Type::WRITER).empty())
    {
        it.emplace(new intermediate::MoveOperation(destination, INT_ZERO));
        it.nextInBlock();
    }

    // mask is container of literals, indices have arbitrary order
    for(std::size_t i = 0; i < mask.container.elements.size(); ++i)
    {
        Value index = mask.container.elements[i];
        if(index.isUndefined())
            // don't write anything at this position
            continue;
        if(!index.hasType(ValueType::LITERAL))
            throw CompilationError(CompilationStep::GENERAL, "Invalid mask value", mask.to_string(false, true));
        const Value& container =
            index.literal.signedInt() < static_cast<int32_t>(source0.type.getVectorWidth()) ? source0 : source1;
        if(index.literal.signedInt() >= static_cast<int32_t>(source0.type.getVectorWidth()))
            index.literal = Literal(index.literal.signedInt() - source0.type.getVectorWidth());
        index.type = TYPE_INT8;
        const Value tmp = method.addNewLocal(container.type.getElementType(), "%vector_shuffle");
        it = insertVectorExtraction(it, method, container, index, tmp);
        it = insertVectorInsertion(it, method, destination, Value(Literal(static_cast<int32_t>(i)), TYPE_INT8), tmp);
    }
    return it;
}

InstructionWalker intermediate::insertMakePositive(
    InstructionWalker it, Method& method, const Value& src, Value& dest, Value& writeIsNegative)
{
    if(src.getLiteralValue())
    {
        bool isNegative = src.getLiteralValue()->signedInt() < 0;
        dest = isNegative ? Value(Literal(-src.getLiteralValue()->signedInt()), src.type) : src;
        writeIsNegative = isNegative ? INT_MINUS_ONE : INT_ZERO;
    }
    else if(src.hasType(ValueType::CONTAINER))
    {
        dest = Value(ContainerValue(src.container.elements.size()), src.type);
        writeIsNegative = Value(ContainerValue(src.container.elements.size()), src.type);
        for(const auto& elem : src.container.elements)
        {
            if(!elem.getLiteralValue())
                throw CompilationError(CompilationStep::OPTIMIZER, "Can't handle container with non-literal values",
                    src.to_string(false, true));
            bool isNegative = elem.getLiteralValue()->signedInt() < 0;
            dest.container.elements.push_back(
                isNegative ? Value(Literal(-elem.getLiteralValue()->signedInt()), elem.type) : elem);
            writeIsNegative.container.elements.push_back(isNegative ? INT_MINUS_ONE : INT_ZERO);
        }
    }
    else if(src.getSingleWriter() != nullptr &&
        src.getSingleWriter()->hasDecoration(InstructionDecorations::UNSIGNED_RESULT))
    {
        // the value is already unsigned
        dest = src;
        writeIsNegative = INT_ZERO;
    }
    else
    {
        /*
         * Calculation of positive value:
         * %sign = asr %src, 31 -> -1 for negative, 0 for positive numbers
         * %tmp = xor %src, %sign
         * %unsigned = sub %tmp, %sign
         *
         * For positive:
         * %sign = 0
         * %tmp = %src
         * %unsigned = sub %src, 0 -> %src
         *
         * For negative:
         * %sign = -1
         * %tmp = ~%src
         * %unsigned = ~%src, -1 -> ~%src + 1 -> two's complement
         *
         * Source:
         * https://llvm.org/doxygen/IntegerDivision_8cpp_source.html
         */

        //%sign = asr %src, 31 -> -1 for negative, 0 for positive numbers
        Value srcInt = src;
        if(src.type.getScalarBitCount() < 32)
        {
            // to make sure, the leading bits are set
            srcInt = method.addNewLocal(TYPE_INT32.toVectorType(src.type.getVectorWidth()), "%sext");
            it = insertSignExtension(it, method, src, srcInt, true);
        }
        if(!writeIsNegative.hasType(ValueType::LOCAL))
            writeIsNegative = method.addNewLocal(TYPE_INT32.toVectorType(src.type.getVectorWidth()), "%sign");
        it.emplace(new Operation(OP_ASR, writeIsNegative, srcInt, Value(Literal(31u), TYPE_INT8)));
        it.nextInBlock();
        //%tmp = xor %src, %sign
        const Value tmp = method.addNewLocal(src.type, "%twos_complement");
        it.emplace(new Operation(OP_XOR, tmp, srcInt, writeIsNegative));
        it.nextInBlock();
        //%unsigned = sub %tmp, %sign
        if(!dest.isWriteable())
            dest = method.addNewLocal(src.type, "%unsigned");
        it.emplace(new Operation(OP_SUB, dest, tmp, writeIsNegative));
        it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        it.nextInBlock();
    }
    return it;
}

InstructionWalker intermediate::insertRestoreSign(
    InstructionWalker it, Method& method, const Value& src, Value& dest, const Value& sign)
{
    if(src.getLiteralValue() && sign.getLiteralValue())
    {
        dest = sign.isZeroInitializer() ? src : Value(Literal(-src.literal.signedInt()), src.type);
    }
    else
    {
        /*
         * Calculation of signed value:
         *
         * %tmp = xor %src, %sign
         * %dest = sub %tmp, %sign
         *
         * To restore positive value (%sign = 0):
         * %tmp = %src
         * %dest = sub %src, 0 -> %src
         *
         * To restore negative value (%sign = -1):
         * %tmp = ~%src
         * %dest = sub ~%src, -1 -> ~%src + 1 -> tow's complement
         *
         * Source:
         * https://llvm.org/doxygen/IntegerDivision_8cpp_source.html
         */

        //%tmp = xor %src, %sign
        const Value tmp = method.addNewLocal(src.type, "%twos_complement");
        it.emplace(new Operation(OP_XOR, tmp, src, sign));
        it.nextInBlock();
        //%dest = sub %tmp, %sign
        if(!dest.isWriteable())
            dest = method.addNewLocal(src.type, "%twos_complement");
        it.emplace(new Operation(OP_SUB, dest, tmp, sign));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker intermediate::insertCalculateIndices(InstructionWalker it, Method& method, const Value& container,
    const Value& dest, const std::vector<Value>& indices, const bool firstIndexIsElement)
{
    // handle multi-level indices
    Value offset = INT_ZERO;
    DataType subContainerType = container.type;
    for(const Value& index : indices)
    {
        Value subOffset(UNDEFINED_VALUE);
        if(subContainerType.isPointerType() || subContainerType.getArrayType())
        {
            // index is index in pointer/array
            //-> add offset of element at given index to global offset
            if(index.getLiteralValue())
            {
                subOffset = Value(Literal(index.getLiteralValue()->signedInt() *
                                      subContainerType.getElementType().getPhysicalWidth()),
                    TYPE_INT32);
            }
            else
            {
                subOffset = method.addNewLocal(TYPE_INT32, "%index_offset");
                it.emplace(new intermediate::IntrinsicOperation("mul", subOffset, index,
                    Value(Literal(subContainerType.getElementType().getPhysicalWidth()), TYPE_INT32)));
                it.nextInBlock();
            }

            // according to SPIR-V 1.2 specification, the type doesn't change if the first index is the "element":
            //"The type of Base after being dereferenced with Element is still the same as the original type of Base."
            if(!firstIndexIsElement || &index != &indices.front())
                subContainerType = subContainerType.getElementType();
        }
        else if(subContainerType.getStructType())
        {
            // index is element in struct -> MUST be literal
            if(!index.getLiteralValue())
                throw CompilationError(CompilationStep::LLVM_2_IR, "Can't access struct-element with non-literal index",
                    index.to_string());

            subOffset = Value(
                Literal(subContainerType.getStructType().value()->getStructSize(index.getLiteralValue()->signedInt())),
                TYPE_INT32);
            subContainerType = subContainerType.getElementType(index.getLiteralValue()->signedInt());
        }
        else if(subContainerType.isVectorType())
        {
            // takes the address of an element of the vector
            if(index.getLiteralValue())
            {
                subOffset = Value(Literal(index.getLiteralValue()->signedInt() *
                                      subContainerType.getElementType().getPhysicalWidth()),
                    TYPE_INT32);
            }
            else
            {
                // FIXME this does not handle negative numbers correctly, since they are cut off after 24 bit
                insertOperation(OP_MUL24, it, method, subOffset, index,
                    Value(Literal(subContainerType.getElementType().getPhysicalWidth()), TYPE_INT8));
            }
            subContainerType = subContainerType.getElementType();
        }
        else
            throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid container-type to retrieve element via index",
                subContainerType.to_string());

        if(offset.getLiteralValue() && subOffset.getLiteralValue())
        {
            offset = Value(
                Literal(offset.getLiteralValue()->signedInt() + subOffset.getLiteralValue()->signedInt()), TYPE_INT32);
        }
        else if(offset.isZeroInitializer())
        {
            // previous offset is zero -> zero + x = x
            offset = subOffset;
        }
        else if(subOffset.isZeroInitializer())
        {
            // sub-offset is zero -> x + zero = x
            // offset = offset -> do nothing
        }
        else
        {
            Value tmp = method.addNewLocal(TYPE_INT32, "%index_offset");
            it.emplace(new intermediate::Operation(OP_ADD, tmp, offset, subOffset));
            it.nextInBlock();
            offset = tmp;
        }
    }
    // add last offset to container
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
        index = indices[0];
    if(firstIndexIsElement && indices.at(0).isZeroInitializer())
        index = indices.size() > 1 ? indices.at(1) : UNDEFINED_VALUE;
    const int refIndex = index.getLiteralValue().value_or(Literal(ANY_ELEMENT)).signedInt();
    const_cast<std::pair<Local*, int>&>(dest.local->reference) = std::make_pair(container.local, refIndex);

    DataType finalType = subContainerType;
    if(subContainerType.getArrayType())
        // convert x[num] to x*
        // TODO shouldn't x[num] be converted to x[num]* ?? (e.g. for HandBrake/vscale_all_dither_opencl.cl)
        // or distinguish between first and following indices?
        finalType = subContainerType.getElementType().toPointerType(container.type.getPointerType() ?
                container.type.getPointerType().value()->addressSpace :
                AddressSpace::PRIVATE);
    else if(!(firstIndexIsElement && indices.size() == 1))
        finalType = subContainerType.toPointerType(container.type.getPointerType().value()->addressSpace);

    if(dest.type != finalType)
    {
        logging::error() << "Final index does not match expected type for source " << container.to_string()
                         << ", destination " << dest.to_string() << ", final index type" << finalType.to_string()
                         << " and indices: " << to_string<Value>(indices)
                         << (firstIndexIsElement ? " (first index is element)" : "") << logging::endl;
        throw CompilationError(
            CompilationStep::LLVM_2_IR, "Types of retrieving indices do not match!", finalType.to_string());
    }

    return it;
}

InstructionWalker intermediate::insertByteSwap(
    InstructionWalker it, Method& method, const Value& src, const Value& dest)
{
    /*
     * llvm.bswap:
     * "The llvm.bswap.i16 intrinsic returns an i16 value that has the high and low byte of the input i16 swapped.
     * Similarly, the llvm.bswap.i32 intrinsic returns an i32 value that has the four bytes of the input i32 swapped,
     * so that if the input bytes are numbered 0, 1, 2, 3 then the returned i32 will have its bytes in 3, 2, 1, 0 order.
     * "
     */
    auto numBytes = src.type.getScalarBitCount() / 8;

    if(numBytes == 2)
    {
        // TODO shorts lose signedness!

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
        it.emplace(new Operation(OP_ROR, tmpAC0, src, Value(Literal(24u), TYPE_INT8)));
        it.nextInBlock();
        // A B C D -> D A B C
        const Value tmpBD0 = method.addNewLocal(src.type, "byte_swap");
        it.emplace(new Operation(OP_ROR, tmpBD0, src, Value(Literal(16u), TYPE_INT8)));
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
        it.emplace(
            new Operation(OP_AND, tmpC1, tmpAC0, Value(Literal(TYPE_INT8.getScalarWidthMask() << 16), src.type)));
        it.nextInBlock();
        // D A B C -> D 0 0 0
        const Value tmpD1 = method.addNewLocal(src.type, "byte_swap");
        it.emplace(
            new Operation(OP_AND, tmpD1, tmpBD0, Value(Literal(TYPE_INT8.getScalarWidthMask() << 24), src.type)));
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
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid number of bytes for byte-swap", std::to_string(numBytes));

    return it;
}

InstructionWalker intermediate::insertOperation(const OpCode& opCode, InstructionWalker it, Method& method,
    Value& output, const Value& firstOperand, const Optional<Value>& secondOperand, bool allowConstantCalculation)
{
    if(allowConstantCalculation && output.isUndefined())
    {
        Optional<Value> constantResult = opCode.calculate(firstOperand, secondOperand);
        if(constantResult)
        {
            output = constantResult.value();
            return it;
        }
    }
    auto vectorSize = std::max(firstOperand.type.getVectorWidth(),
        secondOperand ? secondOperand->type.getVectorWidth() : static_cast<unsigned char>(1));
    if(output.isUndefined())
        output = method.addNewLocal(opCode.returnsFloat ?
                TYPE_FLOAT.toVectorType(vectorSize) :
                (firstOperand.type.isFloatingType() ? TYPE_INT32.toVectorType(vectorSize) : firstOperand.type));
    if(opCode.numOperands == 1)
        it.emplace(new Operation(opCode, output, firstOperand));
    else
        it.emplace(new Operation(opCode, output, firstOperand, secondOperand.value()));
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertMove(
    InstructionWalker it, Method& method, Value& output, const Value& source, bool allowConstantCalculation)
{
    if(allowConstantCalculation && output.isUndefined())
    {
        if(source.getLiteralValue())
            output = Value(source.getLiteralValue().value(), source.type);
        else if(source.hasType(ValueType::CONTAINER))
            output = Value(source.container, source.type);
        if(!output.isUndefined())
            return it;
    }
    if(output.isUndefined())
        output = method.addNewLocal(source.type);
    it.emplace(new MoveOperation(output, source));
    it.nextInBlock();
    return it;
}
