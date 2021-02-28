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
#include "../SIMDVector.h"
#include "../asm/OpCodes.h"

#include "log.h"

#include <algorithm>
#include <map>
#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;
using namespace vc4c::intermediate;

ValueRange analysis::RANGE_HALF{static_cast<double>(std::numeric_limits<half_t>::lowest()),
    static_cast<double>(std::numeric_limits<half_t>::max())};

// Sanity check, that we do not lose precision for all representable values
static_assert(static_cast<int32_t>(static_cast<double>(std::numeric_limits<int32_t>::min())) ==
        std::numeric_limits<int32_t>::min(),
    "");
static_assert(static_cast<int32_t>(static_cast<double>(std::numeric_limits<int32_t>::max())) ==
        std::numeric_limits<int32_t>::max(),
    "");
static_assert(static_cast<uint32_t>(static_cast<double>(std::numeric_limits<uint32_t>::max())) ==
        std::numeric_limits<uint32_t>::max(),
    "");
static_assert(static_cast<float>(static_cast<double>(std::numeric_limits<float>::lowest())) ==
        std::numeric_limits<float>::lowest(),
    "");
static_assert(
    static_cast<float>(static_cast<double>(std::numeric_limits<float>::min())) == std::numeric_limits<float>::min(),
    "");
static_assert(
    static_cast<float>(static_cast<double>(std::numeric_limits<float>::max())) == std::numeric_limits<float>::max(),
    "");

static bool isUnsignedType(DataType type)
{
    /* bool and pointer types are unsigned, everything else could be signed */
    return type.getElementType() == TYPE_BOOL || type.getPointerType();
}

static std::map<DataType, ValueRange> floatTypeLimits = {{TYPE_HALF, RANGE_HALF}, {TYPE_FLOAT, RANGE_FLOAT},
    {TYPE_DOUBLE, ValueRange(std::numeric_limits<double>::min(), std::numeric_limits<double>::max())}};

static constexpr ValueRange toIntTypeLimit(uint32_t numBits, bool isSigned) noexcept
{
    int64_t minValue = std::numeric_limits<int64_t>::min();
    int64_t maxValue = std::numeric_limits<int64_t>::max();
    if(isSigned)
    {
        minValue = numBits == 8 ?
            std::numeric_limits<int8_t>::min() :
            (numBits == 16 ? std::numeric_limits<int16_t>::min() : std::numeric_limits<int32_t>::min());
        maxValue = numBits == 8 ?
            std::numeric_limits<uint8_t>::max() :
            (numBits == 16 ? std::numeric_limits<uint16_t>::max() : std::numeric_limits<uint32_t>::max());
    }
    else
    {
        minValue = 0;
        maxValue = numBits == 8 ?
            std::numeric_limits<uint8_t>::max() :
            (numBits == 16 ? std::numeric_limits<uint16_t>::max() : std::numeric_limits<uint32_t>::max());
    }
    return ValueRange(static_cast<double>(minValue), static_cast<double>(maxValue));
}

ValueRange::ValueRange(Literal lit, DataType type)
{
    minValue = type.isFloatingType() ?
        static_cast<double>(lit.real()) :
        isUnsignedType(type) ? static_cast<double>(lit.unsignedInt()) : static_cast<double>(lit.signedInt());
    maxValue = type.isFloatingType() ? static_cast<double>(lit.real()) : static_cast<double>(lit.unsignedInt());
}

ValueRange::ValueRange(DataType type)
{
    const DataType elemType = type.getPointerType() ? type : type.getElementType();
    if(type.isFloatingType())
    {
        auto it = floatTypeLimits.find(elemType);
        if(it == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        minValue = it->second.minValue;
        maxValue = it->second.maxValue;
    }
    else
    {
        auto range = toIntTypeLimit(elemType.getScalarBitCount(), !isUnsignedType(elemType));
        minValue = range.minValue;
        maxValue = range.maxValue;
    }
}

bool ValueRange::isUnsigned() const
{
    return minValue >= 0.0 && maxValue >= 0.0;
}

bool isInRange(double valMin, double valMax, double min, double max)
{
    return valMin >= min && valMin <= max && valMax >= min && valMax <= max;
}

bool ValueRange::fitsIntoType(DataType type, bool isSigned) const
{
    const DataType elemType = type.getElementType();
    if(type.isFloatingType())
    {
        auto it = floatTypeLimits.find(elemType);
        if(it == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        return isInRange(minValue, maxValue, it->second.minValue, it->second.maxValue);
    }
    if(type.isIntegralType())
    {
        auto range = toIntTypeLimit(elemType.getScalarBitCount(), isSigned);
        return isInRange(minValue, maxValue, range.minValue, range.maxValue);
    }
    return false;
}

bool ValueRange::hasExplicitBoundaries() const
{
    return !std::isinf(minValue) && !std::isnan(minValue) && !std::isinf(maxValue) && !std::isnan(maxValue);
}

Optional<Value> ValueRange::getLowerLimit(DataType type) const
{
    if(!hasExplicitBoundaries())
        return NO_VALUE;
    ValueRange typeLimits;
    std::function<Literal(double)> conv;
    if(type.isFloatingType())
    {
        auto it = floatTypeLimits.find(type);
        if(it == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        typeLimits = it->second;
        conv = [](double d) -> Literal { return Literal(static_cast<float>(d)); };
    }
    else if(type.isIntegralType())
    {
        auto isSigned = !isUnsignedType(type);
        typeLimits = toIntTypeLimit(type.getScalarBitCount(), isSigned);
        if(isSigned)
            conv = [](double d) -> Literal { return Literal(static_cast<int32_t>(std::floor(d))); };
        else
            conv = [](double d) -> Literal { return Literal(static_cast<uint32_t>(std::floor(d))); };
    }
    else
        return NO_VALUE;

    if(minValue < typeLimits.minValue)
        return NO_VALUE;
    if(minValue > typeLimits.maxValue)
        return NO_VALUE;
    return Value(conv(minValue), type);
}

Optional<Value> ValueRange::getUpperLimit(DataType type) const
{
    if(!hasExplicitBoundaries())
        return NO_VALUE;
    ValueRange typeLimits;
    std::function<Optional<Literal>(double)> conv;
    if(type.isFloatingType())
    {
        auto it = floatTypeLimits.find(type);
        if(it == floatTypeLimits.end())
            throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
        typeLimits = it->second;
        conv = [](double d) -> Optional<Literal> { return Literal(static_cast<float>(d)); };
    }
    else if(type.isIntegralType())
    {
        auto isSigned = !isUnsignedType(type);
        typeLimits = toIntTypeLimit(type.getScalarBitCount(), isSigned);
        if(isSigned)
            conv = [](double d) -> Optional<Literal> {
                auto val = std::ceil(d);
                auto max = static_cast<double>(std::numeric_limits<int32_t>::max());
                auto min = static_cast<double>(std::numeric_limits<int32_t>::min());
                return val <= max && val >= min ? Literal(static_cast<int32_t>(std::ceil(d))) : Optional<Literal>{};
            };
        else
            conv = [](double d) -> Optional<Literal> {
                auto val = std::ceil(d);
                auto max = static_cast<double>(std::numeric_limits<uint32_t>::max());
                auto min = static_cast<double>(std::numeric_limits<uint32_t>::min());
                return val <= max && val >= min ? Literal(static_cast<uint32_t>(std::ceil(d))) : Optional<Literal>{};
            };
    }
    else
        return NO_VALUE;

    if(maxValue < typeLimits.minValue)
        return NO_VALUE;
    if(maxValue > typeLimits.maxValue)
        return NO_VALUE;
    if(auto lit = conv(maxValue))
        return Value(*lit, type);
    return NO_VALUE;
}

ValueRange ValueRange::toAbsoluteRange() const noexcept
{
    if(!hasExplicitBoundaries())
        return *this;
    // min <= 0 && max <= 0 -> [abs(max), abs(min)]
    if(minValue <= 0.0 && maxValue <= 0.0)
        return ValueRange{std::abs(maxValue), std::abs(minValue)};
    // min >= 0 && max >= 0 -> [min, max]
    if(minValue >= 0.0 && maxValue >= 0.0)
        return *this;
    // min <= 0 && max >= 0 -> [0, max(abs(min), abs(max))]
    return ValueRange{0.0, std::max(std::abs(minValue), std::abs(maxValue))};
}

LCOV_EXCL_START
std::string ValueRange::to_string() const
{
    if(static_cast<double>(static_cast<int64_t>(minValue)) == minValue &&
        static_cast<double>(static_cast<int64_t>(maxValue)) == maxValue)
        // if we have integer bounds, show as integer range
        return std::string("[") + (std::to_string(static_cast<int64_t>(minValue)) + ", ") +
            std::to_string(static_cast<int64_t>(maxValue)) + "]";
    return std::string("[") + (std::to_string(minValue) + ", ") + std::to_string(maxValue) + "]";
}
LCOV_EXCL_STOP

Optional<ValueRange> ValueRange::getValueRange(intermediate::InstructionDecorations deco, const Method* method)
{
    if(isGroupBuiltin(deco, false))
        // is always positive
        return RANGE_UINT;
    else if(has_flag(deco, InstructionDecorations::BUILTIN_LOCAL_ID))
    {
        int64_t maxID = 0;
        if(method && method->metaData.getFixedWorkGroupSize())
            maxID =
                *std::max_element(method->metaData.workGroupSizes.begin(), method->metaData.workGroupSizes.end()) - 1;
        else if(method)
            maxID = method->metaData.getMaximumWorkGroupSize() - 1;
        else
            maxID = NUM_QPUS - 1;
        return ValueRange{0.0, static_cast<double>(maxID)};
    }
    else if(has_flag(deco, InstructionDecorations::BUILTIN_LOCAL_SIZE))
    {
        int64_t maxSize = 0;
        if(method && method->metaData.getFixedWorkGroupSize())
            maxSize = *std::max_element(method->metaData.workGroupSizes.begin(), method->metaData.workGroupSizes.end());
        else if(method)
            maxSize = method->metaData.getMaximumWorkGroupSize();
        else
            maxSize = NUM_QPUS;
        return ValueRange{0.0, static_cast<double>(maxSize)};
    }
    else if(has_flag(deco, InstructionDecorations::BUILTIN_WORK_DIMENSIONS))
        return ValueRange{1.0, 3.0};
    return {};
}

// TODO also return partial range
ValueRange ValueRange::getValueRange(
    const intermediate::IntermediateInstruction& inst, const Method* method, const ValueRanges& knownRanges)
{
    // TODO consider (un)pack modes

    if(auto val = inst.precalculate(3).first)
        return getValueRange(*val, method);

    if(auto range = getValueRange(inst.decoration, method))
        return *range;

    const Local* inputLocal = nullptr;
    auto inputRangeIt = knownRanges.fullRanges.end();
    auto move = dynamic_cast<const MoveOperation*>(&inst);
    auto op = dynamic_cast<const Operation*>(&inst);

    if(move && (inputLocal = inst.assertArgument(0).checkLocal()) &&
        (inputRangeIt = knownRanges.fullRanges.find(inputLocal)) != knownRanges.fullRanges.end())
    {
        // move -> copy range from source local
        // if the move is a vector rotation, this does not change the range of the full vector
        auto range = inputRangeIt->second;
        if(inst.hasUnpackMode())
            range = dynamic_cast<const UnpackingInstruction&>(inst).getUnpackMode()(range, op && op->op.acceptsFloat);
        if(inst.hasPackMode())
            range = dynamic_cast<const ExtendedInstruction&>(inst).getPackMode()(range, op && op->op.returnsFloat);
        return range;
    }
    else if(op && op->op == OP_AND && op->readsLiteral())
    {
        /*
         * y = x & constant
         *
         * y is in range [0, constant] (unsigned)
         */
        // TODO can be improved if range of other input known
        if(auto litArg = op->findLiteralArgument())
            op->getPackMode()(ValueRange(0.0, static_cast<double>(litArg->getLiteralValue()->unsignedInt())), false);
    }
    else if(op && op->op == OP_CLZ)
    {
        /*
         * y = clz x
         *
         * y is in range [0, 32] (unsigned)
         */
        // TODO can be improved if range of other input known
        return op->getPackMode()(ValueRange(0.0, 32.0), false);
    }
    else if(op && (op->op == OP_FMAXABS || op->op == OP_FMINABS))
    {
        /*
         * y = fmaxabs/fminabs x, z
         *
         * y is in range [0.0, float_max]
         */
        // TODO use argument values -> range [0.0, maxabs/minabs(arg0, arg1)]
        return op->getPackMode()(ValueRange(0.0, static_cast<double>(std::numeric_limits<float>::max())), true);
    }
    else if(op && op->op == OP_SHR && op->readsLiteral())
    {
        /*
         * y = x >> constant
         *
         * y is in range [x.min >> constant, x.max >> constant] (unsigned)
         */
        if((inputLocal = op->assertArgument(0).checkLocal()) &&
            (inputRangeIt = knownRanges.fullRanges.find(inputLocal)) != knownRanges.fullRanges.end() &&
            op->assertArgument(1).isLiteralValue())
        {
            const auto& sourceRange = inputRangeIt->second;
            int64_t offset = static_cast<int64_t>(op->assertArgument(1).getLiteralValue()->signedInt());
            auto div = static_cast<double>(1 << offset);
            // TODO not correct if min/max is negative
            return op->getPackMode()(
                ValueRange(std::trunc(sourceRange.minValue / div), std::trunc(sourceRange.maxValue / div)), false);
        }

        /*
         * y = constant >> x
         *
         * y is in range [0, constant] (unsigned)
         */
        // TODO can be improved if range of other input known
        if(op->assertArgument(0).isLiteralValue())
            return op->getPackMode()(
                ValueRange(0.0, static_cast<double>(op->assertArgument(0).getLiteralValue()->unsignedInt())), false);
    }
    // general case for operations, only works if the ranges of the input locals are already known
    else if(op && !op->getArguments().empty() &&
        (op->op == OP_ADD || op->op == OP_AND || op->op == OP_FADD || op->op == OP_FMAX || op->op == OP_FMAXABS ||
            op->op == OP_FMIN || op->op == OP_FMINABS || op->op == OP_FMUL || op->op == OP_FSUB || op->op == OP_ITOF ||
            op->op == OP_MAX || op->op == OP_MIN || op->op == OP_MUL24 || op->op == OP_SHR || op->op == OP_SUB) &&
        std::all_of(op->getArguments().begin(), op->getArguments().end(), [&](const Value& arg) -> bool {
            return arg.isLiteralValue() ||
                (arg.checkLocal() && knownRanges.fullRanges.find(arg.local()) != knownRanges.fullRanges.end());
        }))
    {
        /*
         * We have an operation (with a valid op-code) where all operands are either constants or locals where the full
         * range is already known
         */
        const Value& arg0 = op->getFirstArg();
        ValueRange firstRange(arg0.type);
        if(auto lit = arg0.getLiteralValue())
            firstRange = ValueRange(*lit, arg0.type);
        else if(arg0.checkLocal() && knownRanges.fullRanges.find(arg0.local()) != knownRanges.fullRanges.end())
            firstRange = knownRanges.fullRanges.at(arg0.local());

        if(op->getArguments().size() > 1)
        {
            const Value arg1 = op->assertArgument(1);
            ValueRange secondRange(arg1.type);
            if(auto lit = arg1.getLiteralValue())
                secondRange = ValueRange(*lit, arg1.type);
            else if(arg1.checkLocal() && knownRanges.fullRanges.find(arg1.local()) != knownRanges.fullRanges.end())
                secondRange = knownRanges.fullRanges.at(arg1.local());

            return op->getPackMode()(ValueRange(op->op(firstRange, secondRange)), op->op.returnsFloat);
        }
        else
            return op->getPackMode()(ValueRange(op->op(firstRange, ValueRange{})), op->op.returnsFloat);
    }
    else if(op)
    {
        // some operations cannot go into negative if both inputs are positive
        bool hasCandidateOperation = op->op == OP_ADD || op->op == OP_AND || op->op == OP_ASR || op->op == OP_FADD ||
            op->op == OP_FMAX || op->op == OP_FMAXABS || op->op == OP_FMIN || op->op == OP_FMINABS ||
            op->op == OP_FMUL || op->op == OP_FTOI || op->op == OP_ITOF || op->op == OP_MAX || op->op == OP_MIN ||
            op->op == OP_MUL24 || op->op == OP_OR || op->op == OP_SHR || op->op == OP_XOR;
        if(isUnsignedType(inst.getOutput()->type) || inst.hasDecoration(InstructionDecorations::UNSIGNED_RESULT) ||
            (hasCandidateOperation &&
                std::all_of(inst.getArguments().begin(), inst.getArguments().end(), [&](const Value& val) -> bool {
                    auto rangeIt =
                        val.checkLocal() ? knownRanges.fullRanges.find(val.local()) : knownRanges.fullRanges.end();
                    return rangeIt != knownRanges.fullRanges.end() && rangeIt->second.isUnsigned();
                })))
            return RANGE_UINT;
        // any other operation, set to min/max
    }
    return ValueRange{};
}

void ValueRange::update(const Optional<Value>& constant, const FastMap<const Local*, ValueRange>& ranges,
    const intermediate::IntermediateInstruction* it, const Method* method)
{
    // values set by built-ins
    if(auto range = getValueRange(it ? it->decoration : InstructionDecorations::NONE, method))
    {
        extendBoundaries(*range);
    }
    // loading of immediates/literals
    else if(auto lit = (constant & &Value::getLiteralValue))
    {
        extendBoundaries(*lit, constant->type.isFloatingType());
    }
    else if(auto vec = constant & &Value::checkVector)
    {
        if(constant->type.isFloatingType())
        {
            double min = std::numeric_limits<double>::max();
            double max = std::numeric_limits<double>::lowest();
            for(auto element : *vec)
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
            for(auto element : *vec)
            {
                min = std::min(min,
                    std::min(static_cast<int64_t>(element.signedInt()), static_cast<int64_t>(element.unsignedInt())));
                max = std::max(max,
                    std::max(static_cast<int64_t>(element.signedInt()), static_cast<int64_t>(element.unsignedInt())));
            }
            extendBoundaries(static_cast<double>(min), static_cast<double>(max));
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
    else if(it)
        extendBoundaries(getValueRange(*it, method, ValueRanges{ranges, {}}));
}

void ValueRange::updateRecursively(const Local* currentLocal, const Method* method,
    FastMap<const Local*, ValueRange>& ranges,
    FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
    FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet)
{
    if(ranges.find(currentLocal) != ranges.end())
        return;
    ValueRange localRange;
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
            writer->forUsedLocals(
                [&](const Local* loc, LocalUse::Type type, const intermediate::IntermediateInstruction& inst) {
                    if(has_flag(type, LocalUse::Type::READER))
                    {
                        updateRecursively(loc, method, ranges, closedSet, openSet);
                        if(ranges.find(loc) == ranges.end())
                            allInputsProcessed = false;
                    }
                });
        }
        ValueRange tmpRange;
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
        if(auto expr = Expression::createRecursiveExpression(*write))
        {
            FastSet<Value> limits;
            if((expr->arg0.checkLocal() && expr->arg0.checkLocal() != currentLocal) ||
                (expr->arg1.checkLocal() && expr->arg1.checkLocal() != currentLocal))
            {
                // TODO does this still work as expected since rewriting expressions??
                // TODO need to fix this, esp. for fake operations!!

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

                limits.emplace(expr->getConvergenceLimit(
                                       otherRange.getLowerLimit(expr->code.acceptsFloat ? TYPE_FLOAT : TYPE_INT32) &
                                       &Value::getLiteralValue)
                                   .value_or(UNDEFINED_VALUE));
                limits.emplace(expr->getConvergenceLimit(
                                       otherRange.getUpperLimit(expr->code.acceptsFloat ? TYPE_FLOAT : TYPE_INT32) &
                                       &Value::getLiteralValue)
                                   .value_or(UNDEFINED_VALUE));

                // If (partial) range of input is set, and output range is not (e.g. this is the only write), set output
                // range to this. Otherwise, we always get full-range... This should be okay, since if f(lower) -> x and
                // f(upper) -> x, then the result is in range [min(lower, x), max(upper, x)]
                if(!localRange.hasExplicitBoundaries() && otherRange.hasExplicitBoundaries())
                    localRange.extendBoundaries(otherRange);
            }
            else
                // some operations always converge to a fixed value, independent of the actual start values
                limits.emplace(expr->getConvergenceLimit().value_or(UNDEFINED_VALUE));

            if(limits.size() == 1 && !limits.begin()->isUndefined())
            {
                auto limit = *limits.begin();
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Expression '" << expr->to_string() << "' for local '"
                        << currentLocal->to_string()
                        // TODO the limit is confusing, since UINT_MAX is displayed as -1, but treated as both -1 and
                        // UINT_MAX
                        << "' converges to: " << limit.to_string() << logging::endl);

                // if at this point the localRange is still the default range (e.g. this is the only write and we could
                // not determine the input local's range, see above), set to explicitly use all values for safety.
                // Otherwise, the resulting range would only be the converged value!
                localRange.extendBoundariesToUnknown();

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

void ValueRange::processedOpenSet(const Method* method, FastMap<const Local*, ValueRange>& ranges,
    FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
    FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet)
{
    auto it = openSet.begin();
    while(it != openSet.end())
    {
        // TODO if there are multiple locals open (e.g. a local and its dependencies) and the local comes first, it
        // has the wrong result, since the range of its dependencies are not yet known
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Rerunning value range for: " << it->first->to_string() << logging::endl);
        FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>> tmpOpenSet{openSet};
        tmpOpenSet.erase(it->first);
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

ValueRange ValueRange::getValueRange(const Value& val, const Method* method)
{
    if(val.isUndefined())
        return ValueRange{};
    ValueRange range;
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

ValueRange ValueRange::getValueRangeRecursive(const Value& val, const Method* method)
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
    ValueRange range;
    range.update(val.getConstantValue(), ranges, val.getSingleWriter(), method);
    return range;
}

static Optional<analysis::ValueRange> getRange(const SubExpression& sub, const Method* method)
{
    if(auto val = sub.checkValue())
        return analysis::ValueRange::getValueRange(*val, method);

    if(auto expr = sub.checkExpression())
        return ValueRange::getValueRange(*expr, method);
    return {};
}

ValueRange ValueRange::getValueRange(const Expression& expr, const Method* method)
{
    if(auto range = getValueRange(expr.deco, method))
        return *range;

    auto leftRange = getRange(expr.arg0, method).value_or(ValueRange{});
    auto rightRange = (expr.code.numOperands > 1 ? getRange(expr.arg1, method) : ValueRange{}).value_or(ValueRange{});
    if(expr.unpackMode.hasEffect())
    {
        leftRange = expr.unpackMode(leftRange, expr.code.acceptsFloat);
        if(expr.code.numOperands > 1)
            rightRange = expr.unpackMode(rightRange, expr.code.acceptsFloat);
    }

    if(expr.isMoveExpression())
        return leftRange;

    if(expr.code == Expression::FAKEOP_UMUL)
    {
        if(!leftRange || !rightRange)
            return RANGE_UINT;
        if(leftRange.minValue < 0.0 || rightRange.minValue < 0.0)
            return RANGE_UINT;
        return ValueRange{leftRange.minValue * rightRange.minValue, leftRange.maxValue * rightRange.maxValue};
    }

    auto result = expr.code(leftRange, rightRange);
    if(has_flag(expr.deco, intermediate::InstructionDecorations::UNSIGNED_RESULT))
        // TODO somehow generalize this!
        result.shrinkToIntersection(RANGE_UINT);
    return expr.packMode.hasEffect() ? expr.packMode(result, expr.code.returnsFloat) : result;
}

ValueRange ValueRange::operator|(const ValueRange& other) const noexcept
{
    ValueRange copy = *this;
    copy.extendBoundaries(other);
    return copy;
}

ValueRange& ValueRange::operator|=(const ValueRange& other) noexcept
{
    extendBoundaries(other);
    return *this;
}

bool ValueRange::operator==(const ValueRange& other) const
{
    return (!hasExplicitBoundaries() && !other.hasExplicitBoundaries()) ||
        (minValue == other.minValue && maxValue == other.maxValue);
}

void ValueRange::extendBoundaries(double newMin, double newMax)
{
    if(newMax < newMin)
        std::swap(newMax, newMin);
    if(hasExplicitBoundaries())
    {
        maxValue = std::max(maxValue, newMax);
        minValue = std::min(minValue, newMin);
    }
    else
    {
        maxValue = newMax;
        minValue = newMin;
    }
}

void ValueRange::extendBoundaries(const ValueRange& other)
{
    if(other)
        extendBoundaries(other.minValue, other.maxValue);
}

void ValueRange::extendBoundaries(Literal literal, bool isFloat)
{
    if(isFloat)
        extendBoundaries(static_cast<double>(literal.real()), static_cast<double>(literal.real()));
    else
        extendBoundaries(std::min(static_cast<double>(literal.signedInt()), static_cast<double>(literal.unsignedInt())),
            std::max(static_cast<double>(literal.signedInt()), static_cast<double>(literal.unsignedInt())));
}

void ValueRange::extendBoundariesToUnknown(bool isKnownToBeUnsigned)
{
    if(isKnownToBeUnsigned)
        extendBoundaries(RANGE_UINT);
    else
    {
        minValue = std::numeric_limits<double>::lowest();
        maxValue = std::numeric_limits<double>::max();
    }
}

void ValueRange::shrinkToIntersection(const ValueRange& other)
{
    if(!hasExplicitBoundaries())
        extendBoundaries(other);
    else if(!other.hasExplicitBoundaries())
        // if the other range is unspecified, the intersection is this range
        return;
    else
    {
        minValue = std::max(minValue, other.minValue);
        maxValue = std::min(maxValue, other.maxValue);

        if(minValue > maxValue)
            std::swap(minValue, maxValue);
    }
}

ValueRangeAnalysis::ValueRangeAnalysis(ValueRanges&& initialRanges) :
    LocalAnalysis(&ValueRangeAnalysis::analyzeRanges, &ValueRangeAnalysis::to_string, std::move(initialRanges))
{
}

ValueRanges ValueRangeAnalysis::analyzeRanges(
    const intermediate::IntermediateInstruction* inst, const ValueRanges& previousRanges, const void* dummy)
{
    ValueRanges newRanges = previousRanges;

    if(!inst || dynamic_cast<const BranchLabel*>(inst))
        return newRanges;

    if(auto loc = inst->checkOutputLocal())
    {
        // TODO how to pass in the method??
        if(auto newRange = ValueRange::getValueRange(*inst, nullptr, previousRanges))
        {
            if(inst->hasConditionalExecution())
            {
                // extend previous range for the local with the new result
                auto fullRangeIt = newRanges.fullRanges.emplace(loc, ValueRange{loc->type}).first;
                fullRangeIt->second.extendBoundaries(newRange);
            }
            else
                // set new range for the local
                newRanges.fullRanges.emplace(loc, newRange).first->second = newRange;
        }
        else
        {
            // the value range is not fully determined, we need to track the partial ranges
            tools::SmallSortedPointerSet<const Local*> locals;
            for(const auto& arg : inst->getArguments())
            {
                if(auto loc = arg.checkLocal())
                    locals.emplace(loc);
            }
            // TODO wouldn't we also need the (partial) ranges of the input locals at this instruction??
            newRanges.partialRanges[loc].generatingExpressions.emplace_back(inst, std::move(locals));
        }
    }

    return newRanges;
}

LCOV_EXCL_START
std::string ValueRangeAnalysis::to_string(const ValueRanges& knownRanges)
{
    std::stringstream ss;

    if(!knownRanges.fullRanges.empty())
    {
        ss << "Known ranges: " << std::endl;
        for(const auto& range : knownRanges.fullRanges)
            ss << range.first->to_string() << " " << range.second.to_string() << std::endl;
    }
    if(!knownRanges.partialRanges.empty())
    {
        ss << "Partial ranges: " << std::endl;
        for(const auto& range : knownRanges.partialRanges)
        {
            ss << range.first->to_string();
            for(const auto& part : range.second.generatingExpressions)
                ss << " - " << part.first->to_string();
            ss << std::endl;
        }
    }
    return ss.str();
}
LCOV_EXCL_STOP
