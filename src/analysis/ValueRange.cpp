/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "ValueRange.h"

#include "../Expression.h"
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

static bool isUnsignedType(DataType type)
{
    /* bool and pointer types are unsigned, everything else could be signed */
    return type.getElementType() == TYPE_BOOL || type.getPointerType();
}

static constexpr FloatRange& floatRange(Variant<FloatRange, IntegerRange>& range)
{
    return VariantNamespace::get<FloatRange>(range);
}

static constexpr IntegerRange& intRange(Variant<FloatRange, IntegerRange>& range)
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

static std::map<DataType, std::pair<double, double>> floatTypeLimits = {
    {TYPE_HALF, std::make_pair(6.103515625e-05, 65504.0)},
    {TYPE_FLOAT,
        std::make_pair<double, double>(static_cast<double>(std::numeric_limits<float>::min()),
            static_cast<double>(std::numeric_limits<float>::max()))},
    {TYPE_DOUBLE, std::make_pair(std::numeric_limits<double>::min(), std::numeric_limits<double>::max())}};

ValueRange::ValueRange(DataType type) : ValueRange(type.isFloatingType(), !isUnsignedType(type))
{
    const DataType elemType = type.getPointerType() ? type : type.getElementType();
    if(type.isFloatingType())
    {
        auto it = floatTypeLimits.find(elemType);
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
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
        return *floatRange;
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
    {
        if(static_cast<double>(intRange->maxValue) <= static_cast<double>(std::numeric_limits<float>::max()) &&
            static_cast<double>(intRange->minValue) >= static_cast<double>(std::numeric_limits<float>::min()))
        {
            result.minValue = static_cast<double>(intRange->minValue);
            result.maxValue = static_cast<double>(intRange->maxValue);
            return result;
        }
        return {};
    }
    return {};
}

Optional<IntegerRange> ValueRange::getIntRange() const
{
    IntegerRange result;
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
    {
        if(static_cast<int64_t>(floatRange->maxValue) <= static_cast<int64_t>(std::numeric_limits<int32_t>::max()) &&
            static_cast<int64_t>(floatRange->minValue) >= static_cast<int64_t>(std::numeric_limits<int32_t>::min()))
        {
            result.maxValue = static_cast<int32_t>(floatRange->maxValue);
            result.minValue = static_cast<int32_t>(floatRange->minValue);
            return result;
        }
        return {};
    }
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
        return *intRange;
    return {};
}

bool ValueRange::isUnsigned() const
{
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
        return floatRange->minValue >= 0.0 && floatRange->maxValue >= 0.0;
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
        return intRange->minValue >= 0 && intRange->maxValue >= 0;
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

bool ValueRange::fitsIntoType(DataType type, bool isSigned) const
{
    const DataType elemType = type.getElementType();
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
    {
        if(!type.isFloatingType())
            return false;
        if(floatTypeLimits.find(elemType) == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        return isInRange(floatRange->minValue, floatRange->maxValue, floatTypeLimits.at(elemType).first,
            floatTypeLimits.at(elemType).second);
    }
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
    {
        if(!type.isIntegralType())
            return false;
        return isInRange(intRange->minValue, intRange->maxValue, elemType.getScalarBitCount(), isSigned);
    }
    return false;
}

bool ValueRange::hasExplicitBoundaries() const
{
    return !hasDefaultBoundaries;
}

Optional<Value> ValueRange::getLowerLimit() const
{
    if(hasDefaultBoundaries)
        return NO_VALUE;
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
    {
        if(floatRange->minValue < static_cast<double>(std::numeric_limits<float>::lowest()))
            return NO_VALUE;
        if(floatRange->minValue > static_cast<double>(std::numeric_limits<float>::max()))
            return NO_VALUE;
        return Value(Literal(static_cast<float>(floatRange->minValue)), TYPE_FLOAT);
    }
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
    {
        if(intRange->minValue < std::numeric_limits<int32_t>::min())
            return NO_VALUE;
        if(intRange->minValue > std::numeric_limits<uint32_t>::max())
            return NO_VALUE;
        if(intRange->minValue > std::numeric_limits<int32_t>::max())
            return Value(Literal(static_cast<uint32_t>(intRange->minValue)), TYPE_FLOAT);
        return Value(Literal(static_cast<int32_t>(intRange->minValue)), TYPE_FLOAT);
    }
    return NO_VALUE;
}

Optional<Value> ValueRange::getUpperLimit() const
{
    if(hasDefaultBoundaries)
        return NO_VALUE;
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
    {
        if(floatRange->maxValue < static_cast<double>(std::numeric_limits<float>::lowest()))
            return NO_VALUE;
        if(floatRange->maxValue > static_cast<double>(std::numeric_limits<float>::max()))
            return NO_VALUE;
        return Value(Literal(static_cast<float>(floatRange->maxValue)), TYPE_FLOAT);
    }
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
    {
        if(intRange->maxValue < std::numeric_limits<int32_t>::min())
            return NO_VALUE;
        if(intRange->maxValue > std::numeric_limits<uint32_t>::max())
            return NO_VALUE;
        if(intRange->maxValue > std::numeric_limits<int32_t>::max())
            return Value(Literal(static_cast<uint32_t>(intRange->maxValue)), TYPE_FLOAT);
        return Value(Literal(static_cast<int32_t>(intRange->maxValue)), TYPE_FLOAT);
    }
    return NO_VALUE;
}

LCOV_EXCL_START
std::string ValueRange::to_string() const
{
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
        return std::string("[") + (std::to_string(floatRange->minValue) + ", ") + std::to_string(floatRange->maxValue) +
            "]";
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
        return std::string("[") + (std::to_string(intRange->minValue) + ", ") + std::to_string(intRange->maxValue) +
            "]";
    throw CompilationError(CompilationStep::GENERAL, "Invalid range type");
}
LCOV_EXCL_STOP

void ValueRange::update(const Optional<Value>& constant, const FastMap<const Local*, ValueRange>& ranges,
    const intermediate::IntermediateInstruction* it, Method* method)
{
    const Operation* op = dynamic_cast<const Operation*>(it);

    // values set by built-ins
    if(it && isGroupBuiltin(it->decoration, false))
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
    else if(auto lit = (constant & &Value::getLiteralValue))
    {
        extendBoundaries(*lit, constant->type.isFloatingType());
    }
    else if(constant && constant->checkVector())
    {
        if(constant->type.isFloatingType())
        {
            double min = std::numeric_limits<double>::max();
            double max = std::numeric_limits<double>::min();
            for(auto element : constant->vector())
            {
                min = std::min(min, static_cast<double>(element.real()));
                max = std::max(max, static_cast<double>(element.real()));
            }
            extendBoundaries(min, max);
        }
        else
        {
            int64_t min = std::numeric_limits<int64_t>::max();
            int64_t max = std::numeric_limits<int64_t>::min();
            for(auto element : constant->vector())
            {
                min = std::min(min,
                    std::min(static_cast<int64_t>(element.signedInt()), static_cast<int64_t>(element.unsignedInt())));
                max = std::max(max,
                    std::max(static_cast<int64_t>(element.signedInt()), static_cast<int64_t>(element.unsignedInt())));
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
    else if(constant && constant->hasRegister(REG_MS_MASK))
    {
        extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(0xF));
    }
    else if(constant && constant->hasRegister(REG_REV_FLAG))
    {
        extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(1));
    }
    else if(dynamic_cast<const MoveOperation*>(it) && it->assertArgument(0).checkLocal() &&
        ranges.find(it->assertArgument(0).local()) != ranges.end())
    {
        // move -> copy range from source local (TODO: would need to link the ranges e.g. if source changes
        // afterwards!)
        const ValueRange& sourceRange = ranges.at(it->assertArgument(0).local());
        extendBoundaries(sourceRange);
    }
    else if(op && op->op == OP_AND && op->readsLiteral())
    {
        /*
         * y = x & constant
         *
         * y is in range [0, constant] (unsigned)
         */
        if(auto litArg = op->findLiteralArgument())
            extendBoundaries(0, static_cast<int64_t>(litArg->literal().unsignedInt()));
        else
            throw CompilationError(CompilationStep::GENERAL,
                "Failed to get literal argument for operation which reads a literal value", op->to_string());
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
        extendBoundaries(0.0, static_cast<double>(std::numeric_limits<float>::max()));
    }
    else if(op && op->op == OP_SHR && op->readsLiteral())
    {
        /*
         * y = x >> constant
         *
         * y is in range [x.min >> constant, x.max >> constant] (unsigned)
         */
        if(op->assertArgument(0).checkLocal() && ranges.find(op->assertArgument(0).local()) != ranges.end() &&
            op->assertArgument(1).isLiteralValue())
        {
            const ValueRange& sourceRange = ranges.at(op->assertArgument(0).local());
            int64_t offset = static_cast<int64_t>(op->assertArgument(1).getLiteralValue()->signedInt());
            // TODO not correct if min/max is negative
            extendBoundaries(
                sourceRange.getIntRange()->minValue >> offset, sourceRange.getIntRange()->maxValue >> offset);
        }

        /*
         * y = constant >> x
         *
         * y is in range [0, constant] (unsigned)
         */
        if(op->assertArgument(0).isLiteralValue())
        {
            extendBoundaries(0, static_cast<int64_t>(op->assertArgument(0).getLiteralValue()->unsignedInt()));
        }
    }
    // general case for operations, only works if the used locals are only written once (otherwise, their range
    // could change afterwards!)
    else if(op && !op->getArguments().empty() &&
        (op->op == OP_ADD || op->op == OP_AND || op->op == OP_FADD || op->op == OP_FMAX || op->op == OP_FMAXABS ||
            op->op == OP_FMIN || op->op == OP_FMINABS || op->op == OP_FMUL || op->op == OP_FSUB || op->op == OP_ITOF ||
            op->op == OP_MAX || op->op == OP_MIN || op->op == OP_MUL24 || op->op == OP_SHR || op->op == OP_SUB) &&
        std::all_of(op->getArguments().begin(), op->getArguments().end(),
            [](const Value& arg) -> bool { return arg.isLiteralValue() || (arg.getSingleWriter() != nullptr); }))
    {
        /*
         * We have an operation (with a valid op-code) where all operands are either constants or locals which
         * are written only once before (and therefore have a fixed range, that is already known)
         */
        const Value& arg0 = op->getFirstArg();
        ValueRange firstRange(arg0.type);
        if(auto lit = arg0.getLiteralValue())
            firstRange.extendBoundaries(*lit, arg0.type.isFloatingType());
        else if(arg0.checkLocal() && ranges.find(arg0.local()) != ranges.end())
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

        if(op->getArguments().size() > 1)
        {
            const Value arg1 = op->assertArgument(1);
            ValueRange secondRange(arg1.type);
            if(auto lit = arg1.getLiteralValue())
                secondRange.extendBoundaries(*lit, arg1.type.isFloatingType());
            else if(arg1.checkLocal() && ranges.find(arg1.local()) != ranges.end())
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

        if(op->hasDecoration(InstructionDecorations::UNSIGNED_RESULT) && !op->getOutput()->type.isFloatingType())
        {
            if(minVal && minVal->isLiteralValue())
                minVal = Value(Literal(std::max(minVal->getLiteralValue()->signedInt(), 0)), minVal->type);
            if(maxVal && maxVal->isLiteralValue())
                maxVal = Value(Literal(std::max(maxVal->getLiteralValue()->signedInt(), 0)), minVal->type);
        }

        if(minVal && minVal->getLiteralValue() && maxVal && maxVal->getLiteralValue())
        {
            if(op->getOutput()->type.isFloatingType())
                extendBoundaries(static_cast<double>(minVal->getLiteralValue()->real()),
                    static_cast<double>(maxVal->getLiteralValue()->real()));
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
        if(auto op = dynamic_cast<const Operation*>(it))
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
                        return val.checkLocal() && ranges.find(val.local()) != ranges.end() &&
                            ranges.at(val.local()).isUnsigned();
                    }))));
    }
}

void ValueRange::updateRecursively(const Local* currentLocal, Method* method, FastMap<const Local*, ValueRange>& ranges,
    FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
    FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet)
{
    if(ranges.find(currentLocal) != ranges.end())
        return;
    ValueRange localRange(currentLocal->type);
    // writes to this local which are not yet fully resolved (where the range is not yet fully known)
    FastSet<const intermediate::IntermediateInstruction*> openWrites;
    // 1. try to determine non-recursive (but transitive) ranges for single writers
    currentLocal->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
        auto closedIt = closedSet.find(writer);
        if(closedIt != closedSet.end())
        {
            // already processed
            localRange.extendBoundaries(closedIt->second);
            return;
        }
        if(openSet.find(writer) != openSet.end())
        {
            // we already processed/are already processing this instruction, abort to not run in stack overflow below
            openWrites.emplace(writer);
            return;
        }
        // insert dummy to mark as already processed without setting the (incomplete) partial result
        openSet.emplace(writer, Optional<ValueRange>{});
        bool allInputsProcessed = true;
        if(!isGroupBuiltin(writer->decoration, true))
        {
            // we can skip recursively processing the arguments, if we know that the below #update will already result
            // in an explicit range, e.g. for accessing work-item info
            writer->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                if(has_flag(type, LocalUse::Type::READER))
                {
                    updateRecursively(loc, method, ranges, closedSet, openSet);
                    if(ranges.find(loc) == ranges.end())
                        allInputsProcessed = false;
                }
            });
        }
        ValueRange tmpRange(currentLocal->type);
        tmpRange.update(writer->precalculate().first, ranges, writer, method);
        if(allInputsProcessed && tmpRange.hasExplicitBoundaries())
        {
            // in the first step, only close if bounds are explicitly set to something. The default bounds are handled
            // afterwards
            openSet.erase(writer);
            closedSet.emplace(writer, tmpRange);
            localRange.extendBoundaries(tmpRange);
        }
        else
            openWrites.emplace(writer);
    });

    // 2. store partial ranges for open writes to be processed later
    for(auto writer : openWrites)
        // set partial range to all open writes (overwrite if needed)
        openSet.emplace(writer, localRange).first->second = localRange;

    // 3. handle special case with single open writer
    if(openWrites.size() == 1)
    {
        auto write = *openWrites.begin();
        // if we only have 1 open write, can determine an expression for it and have a non-default partial range (from
        // all other writes) for this local, we can check whether the expression converges to a value and then extend
        // the bounds to it.
        // NOTE: The convergence is pessimistic, since we could create a smaller range if we knew the exact number the
        // (most likely phi-instruction) is executed, e.g. once for if-else and a fixed number for some loops.
        if(auto expr = Expression::createRecursiveExpression(*write, 6, true))
        {
            FastSet<Value> limits;
            if((expr->arg0.checkLocal() && expr->arg0.checkLocal() != currentLocal) ||
                (expr->arg1.checkLocal() && expr->arg1.checkLocal() != currentLocal))
            {
                // the expression takes another local as input, use its bounds as starting values for the convergence
                // limit calculation
                auto otherLocal = expr->arg0.checkLocal() ? expr->arg0.checkLocal() : expr->arg1.checkLocal();
                ValueRange otherRange(otherLocal->type);
                auto rangeIt = ranges.find(otherLocal);
                bool isPartialRange = false;
                if(rangeIt != ranges.end())
                    // we know the fixed range of the single input local, use as base for convergence
                    otherRange = rangeIt->second;
                else
                {
                    for(const auto& open : openSet)
                    {
                        if(open.second && open.first->writesLocal(otherLocal) && open.first->readsLocal(currentLocal) &&
                            dynamic_cast<const MoveOperation*>(open.first))
                        {
                            // This is only true if the only open writes of the other local depend on (more precisely,
                            // are moves of) the current local (e.g. phi-node), otherwise the range of the other local
                            // might still extend. If the other local moves from this local, the extension is then
                            // handled when processing the open entry for the other local.
                            otherRange = *open.second;
                            isPartialRange = true;
                            break;
                        }
                    }
                }

                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Using input '" << otherLocal->to_string() << "' with"
                        << (isPartialRange ? " partial " : " ") << "range '" << otherRange.to_string()
                        << "' for calculating convergence limit of: " << expr->to_string() << logging::endl);

                limits.emplace(expr->getConvergenceLimit(otherRange.getLowerLimit() & &Value::getLiteralValue)
                                   .value_or(UNDEFINED_VALUE));
                limits.emplace(expr->getConvergenceLimit(otherRange.getUpperLimit() & &Value::getLiteralValue)
                                   .value_or(UNDEFINED_VALUE));

                // If (partial) range of input is set, and output range is not (e.g. this is the only write), set output
                // range to this. Otherwise, we always get full-range... This should be okay, since if f(lower) -> x and
                // f(upper) -> x, then the result is in range [min(lower, x), max(upper, x)]
                if(localRange.hasDefaultBoundaries && otherRange.hasExplicitBoundaries())
                    localRange.extendBoundaries(otherRange);
            }
            else
                // some operations always converge to a fixed value, independent of the actual start values
                limits.emplace(expr->getConvergenceLimit().value_or(UNDEFINED_VALUE));

            if(limits.size() == 1 && !limits.begin()->isUndefined())
            {
                auto limit = *limits.begin();
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Expression '" << expr->to_string() << "' for local '" << currentLocal->to_string()
                        << "' converges to: " << limit.to_string() << logging::endl);

                // if at this point the localRange is still the default range (e.g. this is the only write and we could
                // not determine the input local's range, see above), set to explicitly use all values for safety.
                // Otherwise, the resulting range would only be the converged value!
                localRange.hasDefaultBoundaries = false;

                if(auto lit = limit.getLiteralValue())
                {
                    localRange.extendBoundaries(*lit, limit.type.isFloatingType());
                    openSet.erase(write);
                    closedSet.emplace(write, localRange);
                    openWrites.erase(write);
                }
            }
        }
    }

    // 4. store final range
    if(openWrites.empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Finished value range for '" << currentLocal->to_string() << "': " << localRange.to_string()
                << logging::endl);
        // convert entries from closed set to result ranges
        ranges.emplace(currentLocal, std::move(localRange));
    }
}

void ValueRange::processedOpenSet(Method* method, FastMap<const Local*, ValueRange>& ranges,
    FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
    FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet)
{
    auto it = openSet.begin();
    while(it != openSet.end())
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Rerunning value range for: " << it->first->to_string() << logging::endl);
        FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>> tmpOpenSet{openSet};
        updateRecursively(it->first->checkOutputLocal(), method, ranges, closedSet, tmpOpenSet);
        if(tmpOpenSet.size() != openSet.size())
        {
            // we actually did something, update open set. Since we don't known which entries were removed (and whether
            // the iterator is still valid), replace completely
            it = tmpOpenSet.begin();
            openSet = std::move(tmpOpenSet);
        }
        else
            ++it;
    }

    // TODO does not handle all dependencies, but can also not run in loop until open set empty, since (at least for
    // now) this results in an infinite loop, e.g. for ./testing/test_vectorization.cl

    LCOV_EXCL_START
    CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
        for(const auto& inst : openSet)
        {
            logging::debug() << "Value range still unspecified: " << inst.first->to_string()
                             << (inst.second ? " with partial range: " + inst.second.to_string() : "") << logging::endl;
        }
    });
    LCOV_EXCL_STOP
}

ValueRange ValueRange::getValueRange(const Value& val, Method* method)
{
    ValueRange range(val.type.isFloatingType(), true);
    auto singleWriter = val.getSingleWriter();
    FastMap<const Local*, ValueRange> ranges;
    if(singleWriter && dynamic_cast<const MoveOperation*>(singleWriter))
    {
        const Value& src = dynamic_cast<const MoveOperation*>(singleWriter)->getSource();
        if(auto loc = src.checkLocal())
        {
            auto& tmp = ranges.emplace(loc, loc->type).first->second;
            tmp.update(NO_VALUE, ranges, loc->getSingleWriter(), method);
        }
    }
    range.update(val.getConstantValue() | (singleWriter ? singleWriter->precalculate(3).first : Optional<Value>{}),
        ranges, singleWriter, method);
    return range;
}

ValueRange ValueRange::getValueRangeRecursive(const Value& val, Method* method)
{
    FastMap<const Local*, ValueRange> ranges;
    if(auto loc = val.checkLocal())
    {
        PROFILE_START(RecursiveValueRange);
        FastMap<const intermediate::IntermediateInstruction*, ValueRange> closedSet;
        FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>> openSet;
        updateRecursively(loc, method, ranges, closedSet, openSet);
        processedOpenSet(method, ranges, closedSet, openSet);
        PROFILE_END(RecursiveValueRange);

        auto rangeIt = ranges.find(loc);
        if(rangeIt != ranges.end())
            return rangeIt->second;
    }
    ValueRange range(val.type.isFloatingType(), true);
    range.update(val.getConstantValue(), ranges, val.getSingleWriter(), method);
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
        if(it.has() && !it.get<BranchLabel>() && it->checkOutputLocal())
        {
            ValueRange& range = ranges.emplace(it->getOutput()->local(), it->getOutput()->local()->type).first->second;
            range.update(it->precalculate(3).first, ranges, it.get(), &method);
        }

        it.nextInMethod();
    }

    LCOV_EXCL_START
    logging::logLazy(logging::Level::DEBUG, [&]() {
        std::for_each(ranges.begin(), ranges.end(), [](const std::pair<const Local*, ValueRange>& pair) -> void {
            logging::debug() << "Local " << pair.first->to_string() << " with range " << pair.second.to_string()
                             << logging::endl;
        });
    });
    LCOV_EXCL_STOP
    PROFILE_END(DetermineValueRanges);
    return ranges;
}

void ValueRange::extendBoundaries(double newMin, double newMax)
{
    if(newMax < newMin)
        std::swap(newMax, newMin);
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
    {
        if(hasDefaultBoundaries)
        {
            floatRange->maxValue = newMax;
            floatRange->minValue = newMin;
        }
        else
        {
            floatRange->maxValue = std::max(floatRange->maxValue, newMax);
            floatRange->minValue = std::min(floatRange->minValue, newMin);
        }
    }
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
    {
        if(hasDefaultBoundaries)
        {
            intRange->maxValue = static_cast<int64_t>(newMax);
            intRange->minValue = static_cast<int64_t>(newMin);
        }
        else
        {
            intRange->maxValue = std::max(intRange->maxValue, static_cast<int64_t>(newMax));
            intRange->minValue = std::min(intRange->minValue, static_cast<int64_t>(newMin));
        }
    }

    hasDefaultBoundaries = false;
}

void ValueRange::extendBoundaries(int64_t newMin, int64_t newMax)
{
    if(newMax < newMin)
        std::swap(newMax, newMin);
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&range))
    {
        if(hasDefaultBoundaries)
        {
            floatRange->maxValue = static_cast<double>(newMax);
            floatRange->minValue = static_cast<double>(newMin);
        }
        else
        {
            floatRange->maxValue = std::max(floatRange->maxValue, static_cast<double>(newMax));
            floatRange->minValue = std::min(floatRange->minValue, static_cast<double>(newMin));
        }
    }
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&range))
    {
        if(hasDefaultBoundaries)
        {
            intRange->maxValue = newMax;
            intRange->minValue = newMin;
        }
        else
        {
            intRange->maxValue = std::max(intRange->maxValue, newMax);
            intRange->minValue = std::min(intRange->minValue, newMin);
        }
    }

    hasDefaultBoundaries = false;
}

void ValueRange::extendBoundaries(const ValueRange& other)
{
    if(auto floatRange = VariantNamespace::get_if<FloatRange>(&other.range))
        extendBoundaries(floatRange->minValue, floatRange->maxValue);
    if(auto intRange = VariantNamespace::get_if<IntegerRange>(&other.range))
        extendBoundaries(intRange->minValue, intRange->maxValue);
}

void ValueRange::extendBoundaries(Literal literal, bool isFloat)
{
    if(isFloat)
        extendBoundaries(static_cast<double>(literal.real()), static_cast<double>(literal.real()));
    else
        extendBoundaries(
            std::min(static_cast<int64_t>(literal.signedInt()), static_cast<int64_t>(literal.unsignedInt())),
            std::max(static_cast<int64_t>(literal.signedInt()), static_cast<int64_t>(literal.unsignedInt())));
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
