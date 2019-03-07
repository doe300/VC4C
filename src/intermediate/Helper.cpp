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
#include "operators.h"

#include <algorithm>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

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
        assign(it, dest) = src;
        return it;
    }

    // 1. set amount of rotation
    Value appliedOffset(UNDEFINED_VALUE);
    if(auto lit = offset.checkLiteral())
    {
        // if the offset is a literal, set it as small immediate
        /*
         * Possible inputs and their outputs:
         * negative offset, rotate upwards   -> rotate downwards with absolute value
         * positive offset, rotate upwards   -> rotate upwards with absolute value
         * negative offset, rotate downwards -> rotate upwards with absolute value
         * positive offset, rotate downwards -> rotate downwards with absolute value
         */
        int32_t offsetValue = lit->signedInt();
        const Direction actualDirection =
            lit->signedInt() >= 0 ? direction : (direction == Direction::DOWN ? Direction::UP : Direction::DOWN);
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
    else if(auto imm = offset.checkImmediate())
    {
        appliedOffset = offset;
        // vector is rotated by offset-constant not by rotation constant -> convert to rotation constant
        if(imm->getIntegerValue())
        {
            if(direction == Direction::DOWN)
            {
                appliedOffset.immediate().value = static_cast<unsigned char>((16 - imm->value) % 16);
            }
            else
            {
                appliedOffset.immediate().value = imm->value % 16;
            }
            if(appliedOffset.immediate().value == 0)
                appliedOffset = INT_ZERO;
            else
                appliedOffset.immediate() = SmallImmediate::fromRotationOffset(appliedOffset.immediate());
        }
    }
    else
    {
        // if the offset is not known, write it into r5
        appliedOffset = ROTATION_REGISTER;
        if(direction == Direction::UP)
            // r5 = offset
            assign(it, ROTATION_REGISTER) = offset;
        else
            // QPU uses bits [3:0] which implicitly converts 16 to 0 (see specs, table 5)
            // r5 = 16 - offset
            assign(it, ROTATION_REGISTER) = 16_val - offset;
    }

    // 2. create rotation instruction
    if(appliedOffset.hasLiteral(INT_ZERO.literal()))
        // a rotation by 0 is a simple move
        assign(it, dest) = src;
    else
    {
        // we insert a delay before every vector rotation, since the rotated value can't be written in the previous
        // instruction and a NOP guarantees it. Also, it should be removed by reordering in most cases
        nop(it, DelayType::WAIT_REGISTER);
        it.emplace(new VectorRotation(dest, src, appliedOffset));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker intermediate::insertReplication(
    InstructionWalker it, const Value& src, const Value& dest, const bool useDestionation)
{
    // distribute value 0 to all positions in the vector
    assign(it, Value(REG_REPLICATE_ALL, src.type)) = src;
    if(useDestionation)
        //"Reading r5 returns the per-quad 32-bit value replicated across the four elements of that quad" (p. 18)
        assign(it, dest) = Value(REG_REPLICATE_ALL, src.type);
    return it;
}

InstructionWalker intermediate::insertVectorExtraction(
    InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest)
{
    if(container.isLiteralValue() || container.hasRegister(REG_UNIFORM) || container.hasRegister(REG_QPU_NUMBER))
    {
        // vector extraction from literal is a simple move of the first element, since all elements of a literal are the
        // same
        assign(it, dest) = container;
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
        tmp = method.addNewLocal(value.type, "%vector_insert");
        // 1) rotate value to the correct vector-position
        it = intermediate::insertVectorRotation(it, value, index, tmp, intermediate::Direction::UP);
    }
    // 2) insert element(s) into container
    if(value.type.isScalarType())
    {
        // single element -> create condition only met in given index
        auto cond = assignNop(it) = (as_unsigned{ELEMENT_NUMBER_REGISTER} == as_unsigned{index});
        // 3) move when condition is met
        assign(it, container) = (tmp, cond, InstructionDecorations::ELEMENT_INSERTION);
    }
    else
    {
        // multiple elements -> insert range of indices
        // preconditions: index >= 0 and we want to insert elements [insert, insert + vector-width[ while leaving the
        // other unchanged
        // we use the mask version of loads to set the elements we want to insert to and then use flags to insert only
        // those
        unsigned maskLit = (1 << value.type.getVectorWidth()) - 1;
        auto mask = method.addNewLocal(TYPE_INT32, "%vector_mask");
        auto shiftedMask = method.addNewLocal(TYPE_INT32, "%vector_mask");
        it.emplace(new LoadImmediate(mask, maskLit, LoadType::PER_ELEMENT_UNSIGNED));
        it.nextInBlock();
        it = insertVectorRotation(it, mask, index, shiftedMask);
        assign(it, NOP_REGISTER) = (shiftedMask, SetFlag::SET_FLAGS);
        assign(it, container) = (value, COND_ZERO_CLEAR);
    }
    return it;
}

/*
 * Since we pretend for UNDEFINED indices, that the sequence continues, there may be a sequence where the overlapping
 * indices are actually undefined and therefore don't need to be copied from the second vector (e.g. by moving 3-element
 * vector into 4-element vector).
 */
static bool checkIndicesNotUndefined(const SIMDVector& container, const unsigned int startIndex)
{
    for(auto i = startIndex; i < container.size(); ++i)
        if(container[i].isUndefined())
            return false;
    return true;
}

static NODISCARD InstructionWalker insertDynamicVectorShuffle(
    InstructionWalker it, Method& method, const Value& destination, const Value& source, const Value& mask)
{
    // for each element, write rotation offset to element 0 of r5, rotate and insert into result vector
    for(unsigned char i = 0; i < mask.type.getVectorWidth(); ++i)
    {
        Value offsetTmp0 = method.addNewLocal(TYPE_INT8, "%shuffle_offset");
        Value resultTmp = method.addNewLocal(source.type, "%shuffle_tmp");
        // Rotate into temporary, because of "An instruction that does a vector rotate by r5 must not immediately follow
        // an instruction that writes to r5." - Broadcom Specification, page 37
        it = insertVectorRotation(it, mask, Value(Literal(i), TYPE_INT8), offsetTmp0, Direction::DOWN);
        // pos 3 -> 1 => rotate up by -2 (14), pos 1 -> 3 => rotate up by 2
        Value offsetTmp1 = assign(it, TYPE_INT8, "%shuffle_offset") = Value(Literal(i), TYPE_INT8) - offsetTmp0;
        it = insertVectorRotation(it, source, offsetTmp1, resultTmp, Direction::UP);

        if(i == 0)
        {
            // the first write to the element needs to unconditional, so the register allocator can find it
            // also, the setting flags does not work for the first element
            assign(it, destination) = resultTmp;
        }
        else
        {
            auto cond = assignNop(it) =
                (as_unsigned{ELEMENT_NUMBER_REGISTER} == as_unsigned{Value(Literal(i), TYPE_INT8)});
            assign(it, destination) = (resultTmp, cond, InstructionDecorations::ELEMENT_INSERTION);
        }
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
    else if(!mask.checkVector())
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
        assign(it, NOP_REGISTER) =
            (ELEMENT_NUMBER_REGISTER - Value(Literal(source0.type.getVectorWidth()), TYPE_INT8), SetFlag::SET_FLAGS);
        assign(it, tmpInput) = (tmpSource1, COND_NEGATIVE_CLEAR, InstructionDecorations::ELEMENT_INSERTION);
        return insertDynamicVectorShuffle(it, method, destination, tmpInput, mask);
    }

    // if all indices are ascending (correspond to the elements of source 0), we can simply copy it
    // if all indices point to the same, replicate this index over the vector
    const auto& maskContainer = mask.vector();
    bool indicesCorrespond = maskContainer.isElementNumber();
    bool allIndicesSame = maskContainer.isAllSame();
    if(indicesCorrespond)
    {
        // the vector is copied in-order
        if(mask.type.getVectorWidth() > source0.type.getVectorWidth() &&
            checkIndicesNotUndefined(maskContainer, source0.type.getVectorWidth()))
        {
            // The second vector participates in the shuffling
            // move the first vector in-order
            assign(it, destination) = source0;
            // rotate the second vector with the size of the first as offset
            const Value secondRotated = method.addNewLocal(destination.type, "%vector_shuffle");
            const Value numElementsFirst(Literal(static_cast<uint32_t>(source0.type.getVectorWidth())), TYPE_INT8);
            it = insertVectorRotation(it, source1, numElementsFirst, secondRotated, Direction::UP);
            // insert the elements of the second vector with an element-number of higher or equals the size of the first
            // vector into the result
            assign(it, NOP_REGISTER) = (ELEMENT_NUMBER_REGISTER - numElementsFirst, SetFlag::SET_FLAGS);
            assign(it, destination) = (secondRotated, COND_NEGATIVE_CLEAR);
        }
        else
        {
            // only one vector participates in the shuffling and the elements are inserted in-order -> simply move
            assign(it, destination) = source0;
        }
        return it;
    }
    if(allIndicesSame)
    {
        const int32_t indexValue = maskContainer[0].signedInt() < static_cast<int32_t>(source0.type.getVectorWidth()) ?
            maskContainer[0].signedInt() :
            maskContainer[0].signedInt() - static_cast<int32_t>(source0.type.getVectorWidth());
        const Value source =
            maskContainer[0].signedInt() < static_cast<int32_t>(source0.type.getVectorWidth()) ? source0 : source1;
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
    if(destination.checkLocal() && destination.local()->getUsers(LocalUse::Type::WRITER).empty())
    {
        assign(it, destination) = 0_val;
    }

    // mask is container of literals, indices have arbitrary order
    for(std::size_t i = 0; i < maskContainer.size(); ++i)
    {
        auto index = maskContainer[i];
        if(index.isUndefined())
            // don't write anything at this position
            continue;
        const Value& container =
            index.signedInt() < static_cast<int32_t>(source0.type.getVectorWidth()) ? source0 : source1;
        if(index.signedInt() >= static_cast<int32_t>(source0.type.getVectorWidth()))
            index = Literal(index.signedInt() - source0.type.getVectorWidth());
        const Value tmp = method.addNewLocal(container.type.getElementType(), "%vector_shuffle");
        it = insertVectorExtraction(it, method, container, Value(index, TYPE_INT8), tmp);
        it = insertVectorInsertion(it, method, destination, Value(Literal(static_cast<int32_t>(i)), TYPE_INT8), tmp);
    }
    return it;
}

InstructionWalker intermediate::insertMakePositive(
    InstructionWalker it, Method& method, const Value& src, Value& dest, Value& writeIsNegative)
{
    if(auto lit = src.getLiteralValue())
    {
        bool isNegative = lit->signedInt() < 0;
        dest = isNegative ? Value(Literal(-lit->signedInt()), src.type) : src;
        writeIsNegative = isNegative ? INT_MINUS_ONE : INT_ZERO;
    }
    else if(auto vector = src.checkVector())
    {
        SIMDVector tmpDest;
        SIMDVector tmpNegative;
        for(unsigned i = 0; i < vector->size(); ++i)
        {
            auto elem = (*vector)[i];
            bool isNegative = elem.signedInt() < 0;
            tmpDest[i] = isNegative ? Literal(-elem.signedInt()) : elem;
            tmpNegative[i] = isNegative ? Literal(-1) : Literal(0u);
        }
        dest = Value(std::move(tmpDest), src.type);
        writeIsNegative = Value(std::move(tmpNegative), src.type);
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
        if(!writeIsNegative.checkLocal())
            writeIsNegative = method.addNewLocal(TYPE_INT32.toVectorType(src.type.getVectorWidth()), "%sign");
        it.emplace(new Operation(OP_ASR, writeIsNegative, srcInt, Value(Literal(31u), TYPE_INT8)));
        it.nextInBlock();
        //%tmp = xor %src, %sign
        Value tmp = assign(it, src.type, "%twos_complement") = srcInt ^ writeIsNegative;
        //%unsigned = sub %tmp, %sign
        if(!dest.isWriteable())
            dest = method.addNewLocal(src.type, "%unsigned");
        assign(it, dest) = (tmp - writeIsNegative, InstructionDecorations::UNSIGNED_RESULT);
    }
    return it;
}

InstructionWalker intermediate::insertRestoreSign(
    InstructionWalker it, Method& method, const Value& src, Value& dest, const Value& sign)
{
    if(src.getLiteralValue() && sign.getLiteralValue())
    {
        dest = sign.isZeroInitializer() ? src : Value(Literal(-src.literal().signedInt()), src.type);
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
        Value tmp = assign(it, src.type, "%twos_complement") = src ^ sign;
        //%dest = sub %tmp, %sign
        if(!dest.isWriteable())
            dest = method.addNewLocal(src.type, "%twos_complement");
        assign(it, dest) = tmp - sign;
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
        if(subContainerType.getPointerType() || subContainerType.getArrayType())
        {
            // index is index in pointer/array
            //-> add offset of element at given index to global offset
            if(auto lit = index.getLiteralValue())
            {
                subOffset =
                    Value(Literal(lit->signedInt() * subContainerType.getElementType().getPhysicalWidth()), TYPE_INT32);
            }
            else
            {
                subOffset = method.addNewLocal(TYPE_INT32, "%index_offset");
                it.emplace(new intermediate::IntrinsicOperation("mul", Value(subOffset), Value(index),
                    Value(Literal(subContainerType.getElementType().getPhysicalWidth()), TYPE_INT32)));
                it.nextInBlock();
            }

            // according to SPIR-V 1.2 specification, the type doesn't change if the first index is the "element":
            //"The type of Base after being dereferenced with Element is still the same as the original type of Base."
            if(!firstIndexIsElement || &index != &indices.front())
                subContainerType = subContainerType.getElementType();
        }
        else if(auto structType = subContainerType.getStructType())
        {
            // index is element in struct -> MUST be literal
            if(!index.getLiteralValue())
                throw CompilationError(CompilationStep::LLVM_2_IR, "Can't access struct-element with non-literal index",
                    index.to_string());

            subOffset = Value(Literal(structType->getStructSize(index.getLiteralValue()->signedInt())), TYPE_INT32);
            subContainerType = subContainerType.getElementType(index.getLiteralValue()->signedInt());
        }
        else if(subContainerType.isVectorType())
        {
            // takes the address of an element of the vector
            if(auto lit = index.getLiteralValue())
                subOffset =
                    Value(Literal(lit->signedInt() * subContainerType.getElementType().getPhysicalWidth()), TYPE_INT32);
            else
                subOffset = assign(it, TYPE_INT32, "%vector_element_offset") =
                    index * Literal(subContainerType.getElementType().getPhysicalWidth());
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
            Value tmp = assign(it, TYPE_INT32, "%index_offset") = offset + subOffset;
            offset = tmp;
        }
    }
    // add last offset to container
    assign(it, dest) = container + offset;

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
    const_cast<std::pair<Local*, int>&>(dest.local()->reference) = std::make_pair(container.local(), refIndex);

    DataType finalType = subContainerType;
    if(auto arrayType = subContainerType.getArrayType())
        // convert x[num] to x*
        // TODO shouldn't x[num] be converted to x[num]* ?? (e.g. for HandBrake/vscale_all_dither_opencl.cl)
        // or distinguish between first and following indices?
        finalType = method.createPointerType(arrayType->elementType,
            container.type.getPointerType() ? container.type.getPointerType()->addressSpace : AddressSpace::PRIVATE);
    else if(!(firstIndexIsElement && indices.size() == 1))
        finalType = method.createPointerType(subContainerType, container.type.getPointerType()->addressSpace);

    if(dest.type != finalType)
    {
        logging::error() << "Final index does not match expected type for source " << container.to_string()
                         << ", destination " << dest.to_string() << ", final index type " << finalType.to_string()
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
        Value tmpA0 = assign(it, src.type, "byte_swap") = src >> 8_val;
        // ? ? A B -> ? A B 0
        Value tmpB0 = assign(it, src.type, "byte_swap") = src << 8_val;
        // 0 ? ? A -> 0 0 0 A
        Value tmpA1 = assign(it, src.type, "byte_swap") = tmpA0 & 0x000000FF_val;
        // ? A B 0 -> 0 0 B 0
        Value tmpB1 = assign(it, src.type, "byte_swap") = tmpB0 & 0x0000FF00_val;
        // 0 0 0 A | 0 0 B 0 -> 0 0 A B
        assign(it, dest) = tmpA1 | tmpB1;
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
        Value tmpA1 = assign(it, src.type, "byte_swap") = tmpAC0 & 0x000000FF_val;
        // D A B C -> 0 0 B 0
        Value tmpB1 = assign(it, src.type, "byte_swap") = tmpBD0 & 0x0000FF00_val;
        // B C D A -> 0 C 0 0
        Value tmpC1 = assign(it, src.type, "byte_swap") = tmpAC0 & 0x00FF0000_val;
        // D A B C -> D 0 0 0
        Value tmpD1 = assign(it, src.type, "byte_swap") = tmpBD0 & 0xFF000000_val;
        // 0 0 0 A | 0 0 B 0 -> 0 0 B A
        Value tmpAB2 = assign(it, src.type, "byte_swap") = tmpA1 | tmpB1;
        // 0 C 0 0 | D 0 0 0 -> D C 0 0
        Value tmpCD2 = assign(it, src.type, "byte_swap") = tmpC1 | tmpD1;
        // 0 0 B A | D C 0 0 -> D C B A
        assign(it, dest) = tmpAB2 | tmpCD2;
    }
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid number of bytes for byte-swap", std::to_string(numBytes));

    return it;
}
