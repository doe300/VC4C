/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "ValueRange.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../Profiler.h"
#include "../asm/OpCodes.h"

#include "log.h"

#include <algorithm>
#include <map>

using namespace vc4c;
using namespace vc4c::analysis;
using namespace vc4c::intermediate;

static bool isUnsignedType(const DataType& type)
{
    /* bool and pointer types are unsigned, everything else could be signed */
    return type.getElementType() == TYPE_BOOL || type.getPointerType();
}

static FloatRange& floatRange(Variant<FloatRange, IntegerRange>& range)
{
    return VariantNamespace::get<FloatRange>(range);
}

static const FloatRange& floatRange(const Variant<FloatRange, IntegerRange>& range)
{
    return VariantNamespace::get<FloatRange>(range);
}

static IntegerRange& intRange(Variant<FloatRange, IntegerRange>& range)
{
    return VariantNamespace::get<IntegerRange>(range);
}

static const IntegerRange& intRange(const Variant<FloatRange, IntegerRange>& range)
{
    return VariantNamespace::get<IntegerRange>(range);
}

ValueRange::ValueRange(bool isFloat, bool isSigned) : hasDefaultBoundaries(true)
{
    // make sure the correct defaults are set
    if(isFloat)
        range = FloatRange();
    else
    {
        range = IntegerRange();
        if(isSigned)
            intRange(range).maxValue = std::numeric_limits<int32_t>::max();
        else
            intRange(range).minValue = 0;
    }
}

static std::hash<DataType> h;
static std::map<std::size_t, std::pair<double, double>> floatTypeLimits = {
    {h(TYPE_HALF), std::make_pair(6.103515625e-05, 65504.0)},
    {h(TYPE_FLOAT),
        std::make_pair<double, double>(std::numeric_limits<float>::min(), std::numeric_limits<float>::max())},
    {h(TYPE_DOUBLE), std::make_pair(std::numeric_limits<double>::min(), std::numeric_limits<double>::max())}};

ValueRange::ValueRange(const DataType& type) : ValueRange(type.isFloatingType(), !isUnsignedType(type))
{
    const DataType elemType = type.getPointerType() ? type : type.getElementType();
    if(type.isFloatingType())
    {
        auto it = floatTypeLimits.find(h(elemType));
        if(it == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        floatRange(range).minValue = it->second.first;
        floatRange(range).maxValue = it->second.second;
    }
    else if(isUnsignedType(type))
    {
        intRange(range).minValue = 0;
        intRange(range).maxValue = elemType.getScalarBitCount() == 8 ?
            std::numeric_limits<uint8_t>::max() :
            (elemType.getScalarBitCount() == 16 ? std::numeric_limits<uint16_t>::max() :
                                                  std::numeric_limits<uint32_t>::max());
    }
    else
    {
        intRange(range).minValue = elemType.getScalarBitCount() == 8 ?
            std::numeric_limits<int8_t>::min() :
            (elemType.getScalarBitCount() == 16 ? std::numeric_limits<int16_t>::min() :
                                                  std::numeric_limits<int32_t>::min());
        intRange(range).maxValue = elemType.getScalarBitCount() == 8 ?
            std::numeric_limits<uint8_t>::max() :
            (elemType.getScalarBitCount() == 16 ? std::numeric_limits<uint16_t>::max() :
                                                  std::numeric_limits<uint32_t>::max());
    }
}

Optional<FloatRange> ValueRange::getFloatRange() const
{
    FloatRange result;
    if(VariantNamespace::holds_alternative<FloatRange>(range))
        return floatRange(range);
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
    {
        if(static_cast<double>(intRange(range).maxValue) <= static_cast<double>(std::numeric_limits<float>::max()) &&
            static_cast<double>(intRange(range).minValue) >= static_cast<double>(std::numeric_limits<float>::min()))
        {
            result.minValue = static_cast<float>(intRange(range).minValue);
            result.maxValue = static_cast<float>(intRange(range).maxValue);
            return result;
        }
        return {};
    }
    return {};
}

Optional<IntegerRange> ValueRange::getIntRange() const
{
    IntegerRange result;
    if(VariantNamespace::holds_alternative<FloatRange>(range))
    {
        if(static_cast<int64_t>(floatRange(range).maxValue) <=
                static_cast<int64_t>(std::numeric_limits<int32_t>::max()) &&
            static_cast<int64_t>(floatRange(range).minValue) >=
                static_cast<int64_t>(std::numeric_limits<int32_t>::min()))
        {
            result.maxValue = static_cast<int32_t>(floatRange(range).maxValue);
            result.minValue = static_cast<int32_t>(floatRange(range).minValue);
            return result;
        }
        return {};
    }
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
        return intRange(range);
    return {};
}

bool ValueRange::isUnsigned() const
{
    if(VariantNamespace::holds_alternative<FloatRange>(range))
        return floatRange(range).minValue >= 0.0 && floatRange(range).maxValue >= 0.0;
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
        return intRange(range).minValue >= 0 && intRange(range).maxValue >= 0;
    return false;
}

bool isInRange(double valMin, double valMax, double min, double max)
{
    return valMin >= min && valMin <= max && valMax >= min && valMax <= max;
}

bool isInRange(int64_t valMin, int64_t valMax, uint64_t numBits, bool isSigned)
{
    // signed: -2^(bits - 1)
    // unsigned: 0
    int64_t min = isSigned ? -bit_cast<uint64_t, int64_t>(1 << (numBits - 1)) : 0;
    // signed: 2^(bits - 1) - 1
    // unsigned: 2^bits - 1
    int64_t max = bit_cast<uint64_t, int64_t>(isSigned ? (1 << (numBits - 1)) : 1 << numBits) - 1;

    return valMin >= min && valMin <= max && valMax >= min && valMax <= max;
}

bool ValueRange::fitsIntoType(const DataType& type, bool isSigned) const
{
    const DataType elemType = type.getElementType();
    if(VariantNamespace::holds_alternative<FloatRange>(range))
    {
        if(!type.isFloatingType())
            return false;
        if(floatTypeLimits.find(h(elemType)) == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        return isInRange(floatRange(range).minValue, floatRange(range).maxValue, floatTypeLimits.at(h(elemType)).first,
            floatTypeLimits.at(h(elemType)).second);
    }
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
    {
        if(!type.isIntegralType())
            return false;
        return isInRange(intRange(range).minValue, intRange(range).maxValue, elemType.getScalarBitCount(), isSigned);
    }
    return false;
}

bool ValueRange::hasExplicitBoundaries() const
{
    return !hasDefaultBoundaries;
}

std::string ValueRange::to_string() const
{
    if(VariantNamespace::holds_alternative<FloatRange>(range))
        return std::string("[") + (std::to_string(floatRange(range).minValue) + ", ") +
            std::to_string(floatRange(range).maxValue) + "]";
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
        return std::string("[") + (std::to_string(intRange(range).minValue) + ", ") +
            std::to_string(intRange(range).maxValue) + "]";
    throw CompilationError(CompilationStep::GENERAL, "Invalid range type");
}

void ValueRange::update(const Optional<Value>& constant, const FastMap<const Local*, ValueRange>& ranges,
    const intermediate::IntermediateInstruction* it, Method* method)
{
    const Operation* op = dynamic_cast<const Operation*>(it);

    // values set by built-ins
    if(it &&
        (it->hasDecoration(InstructionDecorations::BUILTIN_GLOBAL_ID) ||
            it->hasDecoration(InstructionDecorations::BUILTIN_GLOBAL_OFFSET) ||
            it->hasDecoration(InstructionDecorations::BUILTIN_GLOBAL_SIZE) ||
            it->hasDecoration(InstructionDecorations::BUILTIN_GROUP_ID) ||
            it->hasDecoration(InstructionDecorations::BUILTIN_NUM_GROUPS)))
    {
        // is always positive
        extendBoundaries(static_cast<int64_t>(0), std::numeric_limits<uint32_t>::max());
    }
    else if(it && it->hasDecoration(InstructionDecorations::BUILTIN_LOCAL_ID))
    {
        int64_t maxID = 0;
        if(method && method->metaData.isWorkGroupSizeSet())
        {
            maxID =
                *std::max_element(method->metaData.workGroupSizes.begin(), method->metaData.workGroupSizes.end()) - 1;
        }
        else
            maxID = 11;
        extendBoundaries(0l, maxID);
    }
    else if(it && it->hasDecoration(InstructionDecorations::BUILTIN_LOCAL_SIZE))
    {
        int64_t maxSize = 0;
        if(method && method->metaData.isWorkGroupSizeSet())
        {
            maxSize = *std::max_element(method->metaData.workGroupSizes.begin(), method->metaData.workGroupSizes.end());
        }
        else
            maxSize = NUM_QPUS;
        extendBoundaries(0l, maxSize);
    }
    else if(it && it->hasDecoration(InstructionDecorations::BUILTIN_WORK_DIMENSIONS))
    {
        extendBoundaries(static_cast<int64_t>(1), static_cast<int64_t>(3));
    }
    // loading of immediates/literals
    else if(constant && constant->isLiteralValue())
    {
        if(constant->type.isFloatingType())
            extendBoundaries(constant->getLiteralValue()->real(), constant->getLiteralValue()->real());
        else
            extendBoundaries(std::min(static_cast<int64_t>(constant->getLiteralValue()->signedInt()),
                                 static_cast<int64_t>(constant->getLiteralValue()->unsignedInt())),
                std::max(static_cast<int64_t>(constant->getLiteralValue()->signedInt()),
                    static_cast<int64_t>(constant->getLiteralValue()->unsignedInt())));
    }
    else if(constant && constant->hasContainer() && constant->container().hasOnlyScalarElements())
    {
        if(constant->type.isFloatingType())
        {
            double min = std::numeric_limits<double>::max();
            double max = std::numeric_limits<double>::min();
            for(const Value& element : constant->container().elements)
            {
                min = std::min(min, static_cast<double>(element.getLiteralValue()->real()));
                max = std::max(max, static_cast<double>(element.getLiteralValue()->real()));
            }
            extendBoundaries(min, max);
        }
        else
        {
            int64_t min = std::numeric_limits<int64_t>::max();
            int64_t max = std::numeric_limits<int64_t>::min();
            for(const Value& element : constant->container().elements)
            {
                min = std::min(min,
                    std::min(static_cast<int64_t>(element.getLiteralValue()->signedInt()),
                        static_cast<int64_t>(element.getLiteralValue()->unsignedInt())));
                max = std::max(max,
                    std::max(static_cast<int64_t>(element.getLiteralValue()->signedInt()),
                        static_cast<int64_t>(element.getLiteralValue()->unsignedInt())));
            }
            extendBoundaries(min, max);
        }
    }
    else if(constant && constant->hasRegister(REG_QPU_NUMBER))
    {
        extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(11));
    }
    else if(constant && constant->hasRegister(REG_ELEMENT_NUMBER))
    {
        extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(NATIVE_VECTOR_SIZE) - 1);
    }
    else if(dynamic_cast<const MoveOperation*>(it) && it->assertArgument(0).hasLocal() &&
        ranges.find(it->assertArgument(0).local()) != ranges.end())
    {
        // move -> copy range from source local (TODO: would need to link the ranges e.g. if source changes
        // afterwards!)
        const ValueRange& sourceRange = ranges.at(it->assertArgument(0).local());
        extendBoundaries(sourceRange);
    }
    else if(op && op->op == OP_AND && it->readsLiteral())
    {
        /*
         * y = x & constant
         *
         * y is in range [0, constant] (unsigned)
         */
        auto argIt = std::find_if(it->getArguments().begin(), it->getArguments().end(),
            [](const Value& val) -> bool { return val.isLiteralValue(); });
        if(argIt == it->getArguments().end())
            throw CompilationError(CompilationStep::GENERAL,
                "Failed to get literal argument for operation which reads a literal value", it->to_string());
        extendBoundaries(0, static_cast<int64_t>(argIt->getLiteralValue()->unsignedInt()));
    }
    else if(op && op->op == OP_CLZ)
    {
        /*
         * y = clz x
         *
         * y is in range [0, 32] (unsigned)
         */
        extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(32));
    }
    else if(op && (op->op == OP_FMAXABS || op->op == OP_FMINABS))
    {
        /*
         * y = fmaxabs/fminabs x, z
         *
         * y is in range [0.0, float_max]
         */
        extendBoundaries(0.0, std::numeric_limits<float>::max());
    }
    else if(op && op->op == OP_SHR && it->readsLiteral())
    {
        /*
         * y = x >> constant
         *
         * y is in range [x.min >> constant, x.max >> constant] (unsigned)
         */
        if(it->assertArgument(0).hasLocal() && ranges.find(it->assertArgument(0).local()) != ranges.end() &&
            it->assertArgument(1).isLiteralValue())
        {
            const ValueRange& sourceRange = ranges.at(it->assertArgument(0).local());
            int64_t offset = static_cast<int64_t>(it->assertArgument(1).getLiteralValue()->signedInt());
            // TODO not correct if min/max is negative
            extendBoundaries(
                sourceRange.getIntRange()->minValue >> offset, sourceRange.getIntRange()->maxValue >> offset);
        }

        /*
         * y = constant >> x
         *
         * y is in range [0, constant] (unsigned)
         */
        if(it->assertArgument(0).isLiteralValue())
        {
            extendBoundaries(0, static_cast<int64_t>(it->assertArgument(0).getLiteralValue()->unsignedInt()));
        }
    }
    // general case for operations, only works if the used locals are only written once (otherwise, their range
    // could change afterwards!)
    else if(op && !it->getArguments().empty() &&
        (op->op == OP_ADD || op->op == OP_AND || op->op == OP_FADD || op->op == OP_FMAX || op->op == OP_FMAXABS ||
            op->op == OP_FMIN || op->op == OP_FMINABS || op->op == OP_FMUL || op->op == OP_FSUB || op->op == OP_ITOF ||
            op->op == OP_MAX || op->op == OP_MIN || op->op == OP_MUL24 || op->op == OP_SHR || op->op == OP_SUB) &&
        std::all_of(it->getArguments().begin(), it->getArguments().end(),
            [](const Value& arg) -> bool { return arg.isLiteralValue() || (arg.getSingleWriter() != nullptr); }))
    {
        /*
         * We have an operation (with a valid op-code) where all operands are either constants or locals which
         * are written only once before (and therefore have a fixed range, that is already known)
         */
        const Value& arg0 = op->getFirstArg();
        ValueRange firstRange(arg0.type);
        if(arg0.isLiteralValue())
        {
            if(arg0.type.isFloatingType())
                firstRange.extendBoundaries(arg0.getLiteralValue()->real(), arg0.getLiteralValue()->real());
            else
                firstRange.extendBoundaries(std::min(static_cast<int64_t>(arg0.getLiteralValue()->signedInt()),
                                                static_cast<int64_t>(arg0.getLiteralValue()->unsignedInt())),
                    std::min(static_cast<int64_t>(arg0.getLiteralValue()->signedInt()),
                        static_cast<int64_t>(arg0.getLiteralValue()->unsignedInt())));
        }
        else if(arg0.hasLocal() && ranges.find(arg0.local()) != ranges.end())
            firstRange.extendBoundaries(ranges.at(arg0.local()));

        Value firstMin(TYPE_UNKNOWN);
        Value firstMax(TYPE_UNKNOWN);

        if(arg0.type.isFloatingType())
        {
            firstMin = Value(Literal(saturate(firstRange.getFloatRange()->minValue)), arg0.type);
            firstMax = Value(Literal(saturate(firstRange.getFloatRange()->maxValue)), arg0.type);
        }
        else
        {
            firstMin = Value(Literal(saturate<int32_t>(firstRange.getIntRange()->minValue)), arg0.type);
            firstMax = Value(Literal(saturate<uint32_t>(firstRange.getIntRange()->maxValue)), arg0.type);
        }

        Optional<Value> minVal = NO_VALUE;
        Optional<Value> maxVal = NO_VALUE;

        if(it->getArguments().size() > 1)
        {
            const Value arg1 = op->assertArgument(1);
            ValueRange secondRange(arg1.type);
            if(arg1.isLiteralValue())
            {
                if(arg1.type.isFloatingType())
                    secondRange.extendBoundaries(arg1.getLiteralValue()->real(), arg1.getLiteralValue()->real());
                else
                    secondRange.extendBoundaries(std::min(static_cast<int64_t>(arg1.getLiteralValue()->signedInt()),
                                                     static_cast<int64_t>(arg1.getLiteralValue()->unsignedInt())),
                        std::min(static_cast<int64_t>(arg1.getLiteralValue()->signedInt()),
                            static_cast<int64_t>(arg1.getLiteralValue()->unsignedInt())));
            }
            else if(arg1.hasLocal() && ranges.find(arg1.local()) != ranges.end())
                secondRange.extendBoundaries(ranges.at(arg1.local()));

            Value secondMin(TYPE_UNKNOWN);
            Value secondMax(TYPE_UNKNOWN);

            if(arg1.type.isFloatingType())
            {
                secondMin = Value(Literal(saturate(secondRange.getFloatRange()->minValue)), arg1.type);
                secondMax = Value(Literal(saturate(secondRange.getFloatRange()->maxValue)), arg1.type);
            }
            else
            {
                secondMin = Value(Literal(saturate<int32_t>(secondRange.getIntRange()->minValue)), arg1.type);
                secondMax = Value(Literal(saturate<uint32_t>(secondRange.getIntRange()->maxValue)), arg1.type);
            }

            minVal = op->op(firstMin, secondMin).first;
            maxVal = op->op(firstMax, secondMax).first;
        }
        else
        {
            minVal = op->op(firstMin, NO_VALUE).first;
            maxVal = op->op(firstMax, NO_VALUE).first;
        }

        if(it->hasDecoration(InstructionDecorations::UNSIGNED_RESULT) && !it->getOutput()->type.isFloatingType())
        {
            if(minVal && minVal->isLiteralValue())
                minVal = Value(Literal(std::max(minVal->getLiteralValue()->signedInt(), 0)), minVal->type);
            if(maxVal && maxVal->isLiteralValue())
                maxVal = Value(Literal(std::max(maxVal->getLiteralValue()->signedInt(), 0)), minVal->type);
        }

        if(minVal && minVal->isLiteralValue() && maxVal && maxVal->isLiteralValue())
        {
            if(it->getOutput()->type.isFloatingType())
                extendBoundaries(minVal->getLiteralValue()->real(), maxVal->getLiteralValue()->real());
            else
                extendBoundaries(std::min(static_cast<int64_t>(minVal->getLiteralValue()->signedInt()),
                                     static_cast<int64_t>(minVal->getLiteralValue()->unsignedInt())),
                    std::max(static_cast<int64_t>(maxVal->getLiteralValue()->signedInt()),
                        static_cast<int64_t>(maxVal->getLiteralValue()->unsignedInt())));
        }
        else
            // failed to pre-calculate the bounds
            extendBoundariesToUnknown(
                isUnsignedType(it->getOutput()->type) || it->hasDecoration(InstructionDecorations::UNSIGNED_RESULT));
    }
    else
    {
        // some operations cannot go into negative if both inputs are positive
        bool hasCandidateOperation = false;
        auto op = dynamic_cast<const Operation*>(it);
        if(op != nullptr)
        {
            hasCandidateOperation = op->op == OP_ADD || op->op == OP_AND || op->op == OP_ASR || op->op == OP_FADD ||
                op->op == OP_FMAX || op->op == OP_FMAXABS || op->op == OP_FMIN || op->op == OP_FMINABS ||
                op->op == OP_FMUL || op->op == OP_FTOI || op->op == OP_ITOF || op->op == OP_MAX || op->op == OP_MIN ||
                op->op == OP_MUL24 || op->op == OP_OR || op->op == OP_SHR || op->op == OP_XOR;
        }
        // any other operation, set to min/max
        extendBoundariesToUnknown(it &&
            (isUnsignedType(it->getOutput()->type) || it->hasDecoration(InstructionDecorations::UNSIGNED_RESULT) ||
                (hasCandidateOperation &&
                    std::all_of(it->getArguments().begin(), it->getArguments().end(), [&](const Value& val) -> bool {
                        return val.hasLocal() && ranges.find(val.local()) != ranges.end() &&
                            ranges.at(val.local()).isUnsigned();
                    }))));
    }
}

ValueRange ValueRange::getValueRange(const Value& val, Method* method)
{
    ValueRange range(val.type.isFloatingType(), true);
    auto singleWriter = val.hasLocal() ? val.local()->getSingleWriter() : nullptr;
    FastMap<const Local*, ValueRange> ranges;
    if(singleWriter && dynamic_cast<const MoveOperation*>(singleWriter))
    {
        const Value& src = dynamic_cast<const MoveOperation*>(singleWriter)->getSource();
        if(src.hasLocal())
        {
            auto& tmp = ranges.emplace(src.local(), src.local()->type).first->second;
            tmp.update(NO_VALUE, ranges, src.local()->getSingleWriter(), method);
        }
    }
    range.update(val.isLiteralValue() ? Optional<Value>(val) :
                                        (singleWriter ? singleWriter->precalculate(3).first : Optional<Value>{}),
        ranges, singleWriter, method);
    return range;
}

FastMap<const Local*, ValueRange> ValueRange::determineValueRanges(Method& method)
{
    PROFILE_START(DetermineValueRanges);
    FastMap<const Local*, ValueRange> ranges;
    ranges.reserve(method.getNumLocals());

    for(const Parameter& param : method.parameters)
    {
        ranges.emplace(&param, param.type);
    }

    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(it.has() && !it.has<BranchLabel>() && it->hasValueType(ValueType::LOCAL))
        {
            ValueRange& range = ranges.emplace(it->getOutput()->local(), it->getOutput()->local()->type).first->second;
            range.update(it->precalculate(3).first, ranges, it.get(), &method);
        }

        it.nextInMethod();
    }

    logging::logLazy(logging::Level::DEBUG, [&]() {
        std::for_each(ranges.begin(), ranges.end(), [](const std::pair<const Local*, ValueRange>& pair) -> void {
            logging::debug() << "Local " << pair.first->to_string() << " with range " << pair.second.to_string()
                             << logging::endl;
        });
    });
    PROFILE_END(DetermineValueRanges);
    return ranges;
}

void ValueRange::extendBoundaries(double newMin, double newMax)
{
    if(newMax < newMin)
        std::swap(newMax, newMin);
    if(VariantNamespace::holds_alternative<FloatRange>(range))
    {
        if(hasDefaultBoundaries)
        {
            floatRange(range).maxValue = newMax;
            floatRange(range).minValue = newMin;
        }
        else
        {
            floatRange(range).maxValue = std::max(floatRange(range).maxValue, newMax);
            floatRange(range).minValue = std::min(floatRange(range).minValue, newMin);
        }
    }
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
    {
        if(hasDefaultBoundaries)
        {
            intRange(range).maxValue = static_cast<int64_t>(newMax);
            intRange(range).minValue = static_cast<int64_t>(newMin);
        }
        else
        {
            intRange(range).maxValue = std::max(intRange(range).maxValue, static_cast<int64_t>(newMax));
            intRange(range).minValue = std::min(intRange(range).minValue, static_cast<int64_t>(newMin));
        }
    }

    hasDefaultBoundaries = false;
}

void ValueRange::extendBoundaries(int64_t newMin, int64_t newMax)
{
    if(newMax < newMin)
        std::swap(newMax, newMin);
    if(VariantNamespace::holds_alternative<FloatRange>(range))
    {
        if(hasDefaultBoundaries)
        {
            floatRange(range).maxValue = static_cast<double>(newMax);
            floatRange(range).minValue = static_cast<double>(newMin);
        }
        else
        {
            floatRange(range).maxValue = std::max(floatRange(range).maxValue, static_cast<double>(newMax));
            floatRange(range).minValue = std::min(floatRange(range).minValue, static_cast<double>(newMin));
        }
    }
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
    {
        if(hasDefaultBoundaries)
        {
            intRange(range).maxValue = newMax;
            intRange(range).minValue = newMin;
        }
        else
        {
            intRange(range).maxValue = std::max(intRange(range).maxValue, newMax);
            intRange(range).minValue = std::min(intRange(range).minValue, newMin);
        }
    }

    hasDefaultBoundaries = false;
}

void ValueRange::extendBoundaries(const ValueRange& other)
{
    if(VariantNamespace::holds_alternative<FloatRange>(other.range))
        extendBoundaries(other.getFloatRange()->minValue, other.getFloatRange()->maxValue);
    if(VariantNamespace::holds_alternative<FloatRange>(other.range))
        extendBoundaries(other.getIntRange()->minValue, other.getIntRange()->maxValue);
}

void ValueRange::extendBoundariesToUnknown(bool isKnownToBeUnsigned)
{
    if(hasDefaultBoundaries)
    {
        if(VariantNamespace::holds_alternative<IntegerRange>(range) && isKnownToBeUnsigned)
        {
            range = IntegerRange();
            intRange(range).minValue = 0;
            hasDefaultBoundaries = false;
        }
        // the default boundaries are already the widest for the underlying type
        return;
    }
    if(VariantNamespace::holds_alternative<FloatRange>(range))
        range = FloatRange();
    if(VariantNamespace::holds_alternative<IntegerRange>(range))
    {
        range = IntegerRange();
        if(isKnownToBeUnsigned)
            intRange(range).minValue = 0;
    }
    hasDefaultBoundaries = false;
}
