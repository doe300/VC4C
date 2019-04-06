/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

// TODO retest shuffle tests! OpenCL-CTS relationals/test_relationals/shuffle*, boost/test_random_shuffle

#include "VectorHelper.h"

#include "../Profiler.h"
#include "log.h"
#include "operators.h"

#include <sstream>

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
        if(auto writer = mask.getSingleWriter())
        {
            if(auto precompiledMask = writer->precalculate(3).first)
            {
                return insertVectorShuffle(it, method, destination, source0, source1, *precompiledMask);
            }
        }

        if(source1.isUndefined())
            return insertDynamicVectorShuffle(it, method, destination, source0, mask);
        // if we have both vectors, build one large vector by appending them
        // TODO does not work if both vectors are 16-element!!
        // for smaller vector, combine to one and shuffle the one, for larger need to select from both
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
    // TODO find combinations of elements to rotate together (same offset). Also use for literal container assembly
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

bool ElementSource::operator<(const ElementSource& other) const noexcept
{
    // "Smaller" source types have precedence, since they use less instructions
    if(sourceType > other.sourceType)
        return false;
    bool smaller = sourceType < other.sourceType;
    if(!smaller)
    {
        // NOTE: * and -> also works for tombstone values!
        if(sourceValue->unsignedInt() > other.sourceValue->unsignedInt())
            return false;
        smaller = sourceValue->unsignedInt() < other.sourceValue->unsignedInt();
    }
    if(!smaller)
    {
        if(modificationType > other.modificationType)
            return false;
        smaller = modificationType < other.modificationType;
    }
    if(!smaller)
    {
        if(modificationValue->unsignedInt() > other.modificationValue->unsignedInt())
            return false;
        smaller = modificationValue->unsignedInt() < other.modificationValue->unsignedInt();
    }
    return smaller;
}

bool ElementSource::operator==(const ElementSource& other) const noexcept
{
    return sourceType == other.sourceType && sourceValue == other.sourceValue &&
        modificationType == other.modificationType && modificationValue == other.modificationValue;
}

bool ElementSource::isCompatible(const ElementSource& other) const noexcept
{
    if(sourceType == SourceType::ANY || other.sourceType == SourceType::ANY)
        return true;
    if(sourceType != other.sourceType)
        return false;
    if(modificationType != other.modificationType || modificationValue != other.modificationValue)
        return false;

    if(sourceType == SourceType::CONSTANT)
        return sourceValue == other.sourceValue;
    return true;
}

std::string ElementSource::to_string() const
{
    std::stringstream ss;

    switch(sourceType)
    {
    case SourceType::ANY:
        ss << "any";
        break;
    case SourceType::ELEMENT_NUMBER:
        ss << "elem_num";
        break;
    case SourceType::CONSTANT:
        ss << "ldi";
        break;
    case SourceType::LOAD_SIGNED_MASK:
        ss << "ldsi";
        break;
    case SourceType::LOAD_UNSIGNED_MASK:
        ss << "ldui";
        break;
    case SourceType::REPLICATE_QUAD_NUMBER:
        ss << "rep_quad";
        break;
    }
    if(sourceValue)
        ss << ' ' << sourceValue->to_string();

    switch(modificationType)
    {
    case ModificationType::NONE:
        break;
    case ModificationType::MULTIPLY:
        ss << " mul24";
        break;
    case ModificationType::ADD:
        ss << " add";
        break;
    case ModificationType::ROTATE:
        ss << " <<";
        break;
    }

    if(modificationValue)
        ss << ' ' << modificationValue->to_string();

    return ss.str();
}

// TODO why not trivially moveable??
static_assert(std::is_trivially_destructible<ElementSource>::value, "");

static bool checkAdditionOverflow(int32_t val, int32_t offset)
{
    // only limit is 32-bit under-/overflow
    return static_cast<int64_t>(val - offset) > static_cast<int64_t>(std::numeric_limits<int32_t>::min()) &&
        static_cast<int64_t>(val - offset) < static_cast<int64_t>(std::numeric_limits<int32_t>::max());
}

static bool checkMultiplication(uint32_t val, uint32_t factor)
{
    // for multiplication use upper limit to not get into problems with mul24
    return factor != 0 && val < std::numeric_limits<uint16_t>::max() && (val % factor) == 0;
}

static void addMatchingElementNumber(Literal val, uint8_t index, std::set<ElementSource>& candidates)
{
    if(val.unsignedInt() == index)
        candidates.emplace(ElementSource{SourceType::ELEMENT_NUMBER, {}});
    else
    {
        // no need to add elem_num add 0, elem_num mul24 1 or elem_num << 0 since they do not do anything!
        if(checkAdditionOverflow(val.signedInt(), index))
            candidates.emplace(
                ElementSource{SourceType::ELEMENT_NUMBER, {}, ModificationType::ADD, Literal(val.signedInt() - index)});

        if(checkMultiplication(val.unsignedInt(), index))
            candidates.emplace(ElementSource{
                SourceType::ELEMENT_NUMBER, {}, ModificationType::MULTIPLY, Literal(val.unsignedInt() / index)});

        if(false && val.signedInt() >= 0 && val.signedInt() < static_cast<int32_t>(NATIVE_VECTOR_SIZE))
            // FIXME needs to be rotation sub/sub with modulo!!
            candidates.emplace(ElementSource{
                SourceType::ELEMENT_NUMBER, {}, ModificationType::ROTATE, Literal(val.unsignedInt() - index)});
    }
}

static void addMatchingLoadUnsignedMask(Literal val, uint8_t index, std::set<ElementSource>& candidates)
{
    if(val.signedInt() >= 0 && val.signedInt() <= 3)
        candidates.emplace(ElementSource{SourceType::LOAD_UNSIGNED_MASK, val});

    if(val.signedInt() != 0)
        candidates.emplace(
            ElementSource{SourceType::LOAD_UNSIGNED_MASK, Literal(0), ModificationType::ADD, Literal(val.signedInt())});

    if(val.signedInt() != 1 && checkAdditionOverflow(val.signedInt(), 1))
        candidates.emplace(ElementSource{
            SourceType::LOAD_UNSIGNED_MASK, Literal(1), ModificationType::ADD, Literal(val.signedInt() - 1)});

    if(val.signedInt() != 2 && checkAdditionOverflow(val.signedInt(), 2))
        candidates.emplace(ElementSource{
            SourceType::LOAD_UNSIGNED_MASK, Literal(2), ModificationType::ADD, Literal(val.signedInt() - 2)});

    if(val.signedInt() != 3 && checkAdditionOverflow(val.signedInt(), 3))
        candidates.emplace(ElementSource{
            SourceType::LOAD_UNSIGNED_MASK, Literal(3), ModificationType::ADD, Literal(val.signedInt() - 3)});

    if(val.signedInt() != 1)
        candidates.emplace(ElementSource{
            SourceType::LOAD_UNSIGNED_MASK, Literal(1), ModificationType::MULTIPLY, Literal(val.signedInt())});

    if(val.signedInt() != 2 && checkMultiplication(val.unsignedInt(), 2))
        candidates.emplace(ElementSource{
            SourceType::LOAD_UNSIGNED_MASK, Literal(2), ModificationType::MULTIPLY, Literal(val.unsignedInt() / 2)});

    if(val.signedInt() != 3 && checkMultiplication(val.unsignedInt(), 3))
        candidates.emplace(ElementSource{
            SourceType::LOAD_UNSIGNED_MASK, Literal(3), ModificationType::MULTIPLY, Literal(val.unsignedInt() / 3)});

    // Rotation is irrelevant here, since the possible loaded values do not depend on the index
}

static void addMatchingLoadSignedMask(Literal val, uint8_t index, std::set<ElementSource>& candidates)
{
    if(val.signedInt() >= -2 && val.signedInt() <= 1)
        candidates.emplace(ElementSource{SourceType::LOAD_SIGNED_MASK, val});

    if(val.signedInt() != -2 && checkAdditionOverflow(val.signedInt(), -2))
        candidates.emplace(ElementSource{
            SourceType::LOAD_SIGNED_MASK, Literal(-2), ModificationType::ADD, Literal(val.signedInt() - -2)});

    if(val.signedInt() != -1 && checkAdditionOverflow(val.signedInt(), -1))
        candidates.emplace(ElementSource{
            SourceType::LOAD_SIGNED_MASK, Literal(-1), ModificationType::ADD, Literal(val.signedInt() - -1)});

    if(val.signedInt() != 0)
        candidates.emplace(
            ElementSource{SourceType::LOAD_SIGNED_MASK, Literal(0), ModificationType::ADD, Literal(val.signedInt())});

    if(val.signedInt() != 1 && checkAdditionOverflow(val.signedInt(), 1))
        candidates.emplace(ElementSource{
            SourceType::LOAD_SIGNED_MASK, Literal(1), ModificationType::ADD, Literal(val.signedInt() - 1)});

    if(val.signedInt() != 1)
        candidates.emplace(ElementSource{
            SourceType::LOAD_SIGNED_MASK, Literal(1), ModificationType::MULTIPLY, Literal(val.signedInt())});

    // Rotation is irrelevant here, since the possible loaded values do not depend on the index
}

static void addMatchingReplicateQuadNumber(Literal val, uint8_t index, std::set<ElementSource>& candidates)
{
    if(val.unsignedInt() == (index / 4))
        candidates.emplace(ElementSource{SourceType::REPLICATE_QUAD_NUMBER, {}});
    else
    {
        // no need to add rep_quad add 0 or rep_quad mul24 1 since they don't do anything
        if(checkAdditionOverflow(val.signedInt(), index / 4))
            candidates.emplace(ElementSource{
                SourceType::REPLICATE_QUAD_NUMBER, {}, ModificationType::ADD, Literal(val.signedInt() - (index / 4))});

        if(checkMultiplication(val.unsignedInt(), index / 4))
            candidates.emplace(ElementSource{SourceType::REPLICATE_QUAD_NUMBER, {}, ModificationType::MULTIPLY,
                Literal(val.unsignedInt() / (index / 4))});
    }
}

static std::vector<std::set<ElementSource>> getElementSources(DataType vectorType, const SIMDVector& vector)
{
    std::vector<std::set<ElementSource>> sources(vectorType.getVectorWidth());

    for(unsigned char i = 0; i < vectorType.getVectorWidth(); ++i)
    {
        /*
         * x is element in vector:
         */
        if(vector[i].isUndefined())
        {
            /*
             * f(x) = undef -> all accepted
             */
            sources[i].emplace(ElementSource{SourceType::ANY, {}});
        }
        else
        {
            /*
             * f(x) = const -> y = ldi const
             */
            sources[i].emplace(ElementSource{SourceType::CONSTANT, vector[i]});
            /*
             * f(x) = x -> y = mov elem_num
             * f(x) = x + const -> y = add const, elem_num
             * f(x) = x * const -> y = mul24 const, elem_num
             */
            addMatchingElementNumber(vector[i], i, sources[i]);
            /*
             * f(x) = [0,3] -> y = ldui [0,3]
             * f(x) = [0,3] + const -> a = ldui [0,3], y = add const, a
             * f(x) = [0,3] * const -> a = ldsu [0,3], y = mul24 const, a
             */
            addMatchingLoadUnsignedMask(vector[i], i, sources[i]);
            /*
             * f(x) = [-2,1] -> y = ldsi [-2,1]
             * f(x) = [-2,1] + const -> a = ldsi [-2,1], y = add const, a
             * f(x) = [-2,1] * const -> a = ldsi [-2,1], y = mul24 const, a
             */
            addMatchingLoadSignedMask(vector[i], i, sources[i]);
            /*
             * f(x) = x / 4 -> rep_quad = shr elem_num, 2, y = mov rep_quad
             * f(x) = (x / 4) + const -> rep_quad = shr elem_num, 2, y = add rep_quad, const
             * f(x) = (x / 4) * const -> rep_quad = shr elem_num, 2, y = mul24 rep_quad, const
             */
            addMatchingReplicateQuadNumber(vector[i], i, sources[i]);
            // TODO: could also combine any above, e.g. f(x) = (x + const) * [0,3]
        }
    }
    return sources;
}

Optional<std::vector<ElementSource>> intermediate::checkVectorCanBeAssembled(DataType type, SIMDVector vector)
{
    PROFILE_START(getElementSources);
    auto sources = getElementSources(type, vector);
    PROFILE_END(getElementSources);

    std::vector<ElementSource> results;
    results.reserve(sources.size());

    PROFILE_START(checkVectorCanBeAssembled);
    if(!sources.empty())
    {
        // make sure the SIMD element we compare all against is not undefined!
        unsigned mainElement = 0;
        for(; mainElement < sources.size(); ++mainElement)
        {
            if(!sources[mainElement].empty() && sources[mainElement].begin()->sourceType != SourceType::ANY)
                break;
        }
        for(const auto& source : sources[mainElement])
        {
            // TODO need more efficient solution
            results.emplace_back(source);
            for(unsigned i = 0; i < sources.size(); ++i)
            {
                if(i == mainElement)
                    // no need to compare with self
                    continue;
                auto it = std::find_if(sources[i].begin(), sources[i].end(),
                    [&](const ElementSource& src) -> bool { return src.isCompatible(source); });
                if(it != sources[i].end())
                {
                    results.emplace_back(*it);
                }
            }
            if(results.size() == sources.size())
                // we found one matching per element, yeah!
                break;
            else
                // otherwise try again with next candidate
                results.clear();
        }
        
        /*
         * TODO extend by trying to find one source matching
         * 1. all (currently done)
         * 2. per quad (and insert for each quad)
         * 3. according to simple selection (see below)
         *
         * NOTE: Make sure code generated is shorter than single insertion per element!
         *
         * Selections:
         * subtraction - elements larger/smaller x
         * xor - specific element
         * and - elements multiple of power of two (e.g. x & 0x3 != 0 <=> x | 4)
         * ldsi/ldui - any element combination
         */
    }
    PROFILE_END(checkVectorCanBeAssembled);

    if(results.empty())
        return {};
    logging::debug() << "Matching vector sources: " << to_string<ElementSource>(results) << logging::endl;
    return std::move(results);
}

InstructionWalker intermediate::insertAssembleVector(
    InstructionWalker it, Method& method, const Value& dest, std::vector<ElementSource>&& sources)
{
    if(sources.empty())
        throw CompilationError(CompilationStep::GENERAL, "Cannot assemble vector from empty sources", dest.to_string());

    SourceType sourceType = SourceType::ANY;
    ModificationType modType = sources[0].modificationType;
    auto modVal = sources[0].modificationValue;
    for(const auto& src : sources)
    {
        if(src.sourceType == SourceType::ANY)
            continue;
        if(sourceType != SourceType::ANY && sourceType != src.sourceType)
            throw CompilationError(CompilationStep::GENERAL, "Cannot assemble vector with different source types",
                to_string<ElementSource>(sources));
        sourceType = src.sourceType;
        if(modType != src.modificationType)
            throw CompilationError(CompilationStep::GENERAL, "Cannot assemble vector with different modification types",
                to_string<ElementSource>(sources));
        if(modVal != src.modificationValue)
            throw CompilationError(CompilationStep::GENERAL,
                "Cannot assemble vector with different modification values", to_string<ElementSource>(sources));
    }

    SIMDVector loadElements{};
    for(unsigned i = 0; i < std::min(sources.size(), static_cast<std::size_t>(loadElements.size())); ++i)
        loadElements[i] = sources[i].sourceValue ? *sources[i].sourceValue : UNDEFINED_LITERAL;

    // load source
    Value tmp = UNDEFINED_VALUE;
    switch(sourceType)
    {
    case SourceType::ANY:
        // all are undefined
        tmp = assign(it, dest.type) = INT_ZERO;
        break;
    case SourceType::CONSTANT:
        if(std::any_of(sources.begin(), sources.end(),
               [&](const ElementSource& src) -> bool { return src.sourceValue != sources[0].sourceValue; }))
            throw CompilationError(CompilationStep::GENERAL, "Cannot assemble vector with different constant sources",
                to_string<ElementSource>(sources));
        tmp = assign(it, dest.type) = Value(*sources[0].sourceValue, dest.type);
        break;
    case SourceType::ELEMENT_NUMBER:
        tmp = assign(it, dest.type) = ELEMENT_NUMBER_REGISTER;
        break;
    case SourceType::LOAD_UNSIGNED_MASK:
        tmp = method.addNewLocal(dest.type);
        it.emplace(new LoadImmediate(tmp, LoadImmediate::fromLoadedValues(loadElements, LoadType::PER_ELEMENT_UNSIGNED),
            LoadType::PER_ELEMENT_UNSIGNED));
        break;
    case SourceType::LOAD_SIGNED_MASK:
        tmp = method.addNewLocal(dest.type);
        it.emplace(new LoadImmediate(tmp, LoadImmediate::fromLoadedValues(loadElements, LoadType::PER_ELEMENT_SIGNED),
            LoadType::PER_ELEMENT_SIGNED));
        break;
    case SourceType::REPLICATE_QUAD_NUMBER:
        assign(it, Value(REG_REPLICATE_QUAD, dest.type)) = ELEMENT_NUMBER_REGISTER / 4_lit;
        tmp = assign(it, dest.type) = Value(REG_REPLICATE_QUAD, dest.type);
        break;
    }

    // apply modification
    switch(modType)
    {
    case ModificationType::NONE:
        assign(it, dest) = tmp;
        break;
    case ModificationType::ADD:
        assign(it, dest) = tmp + Value(*modVal, dest.type);
        break;
    case ModificationType::MULTIPLY:
        assign(it, dest) = mul24(tmp, Value(*modVal, dest.type));
        break;
    case ModificationType::ROTATE:
        // TODO is direction correct??
        return insertVectorRotation(it, tmp, dest, Value(*modVal, dest.type), Direction::UP);
    }
    return it;
}
