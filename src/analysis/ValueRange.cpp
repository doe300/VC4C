/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "ValueRange.h"

#include "../InstructionWalker.h"
#include "../Module.h"
#include "../asm/OpCodes.h"

#include "log.h"

#include <algorithm>
#include <map>

using namespace vc4c;
using namespace vc4c::intermediate;


static bool isUnsignedType(const DataType& type)
{
	/* bool and pointer types are unsigned, everything else could be signed */
	return type.getElementType() == TYPE_BOOL || type.getPointerType();
}

ValueRange::ValueRange(bool isFloat, bool isSigned) : type(isFloat ? RangeType::FLOAT : RangeType::INTEGER), hasDefaultBoundaries(true)
{
	//make sure the correct defaults are set
	switch(type)
	{
		case RangeType::FLOAT:
			floatRange = FloatRange();
			break;
		case RangeType::INTEGER:
			intRange = IntegerRange();
			if(isSigned)
				intRange.maxValue = std::numeric_limits<int32_t>::max();
			else
				intRange.minValue = 0;
			break;
	}
}

static std::map<std::string, std::pair<double, double>> floatTypeLimits =
{
		{TYPE_HALF.typeName, std::make_pair(6.103515625e-05, 65504.0)},
		{TYPE_FLOAT.typeName, std::make_pair<double, double>(std::numeric_limits<float>::min(), std::numeric_limits<float>::max())},
		{TYPE_DOUBLE.typeName, std::make_pair(std::numeric_limits<double>::min(), std::numeric_limits<double>::max())}
};

ValueRange::ValueRange(const DataType& type) : ValueRange(type.isFloatingType(), !isUnsignedType(type))
{
	const DataType elemType = type.getElementType();
	if(type.isFloatingType())
	{
		if(floatTypeLimits.find(elemType.typeName) == floatTypeLimits.end())
			throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
		floatRange.minValue = floatTypeLimits.at(elemType.typeName).first;
		floatRange.maxValue = floatTypeLimits.at(elemType.typeName).second;
	}
	else if(isUnsignedType(type))
	{
		intRange.minValue = 0;
		intRange.maxValue = elemType.getScalarBitCount() == 8 ? std::numeric_limits<uint8_t>::max() : (elemType.getScalarBitCount() == 16 ? std::numeric_limits<uint16_t>::max() : std::numeric_limits<uint32_t>::max());
	}
	else
	{
		intRange.minValue = elemType.getScalarBitCount() == 8 ? std::numeric_limits<int8_t>::min() : (elemType.getScalarBitCount() == 16 ? std::numeric_limits<int16_t>::min() : std::numeric_limits<int32_t>::min());
		intRange.maxValue = elemType.getScalarBitCount() == 8 ? std::numeric_limits<uint8_t>::max() : (elemType.getScalarBitCount() == 16 ? std::numeric_limits<uint16_t>::max() : std::numeric_limits<uint32_t>::max());
	}
}

Optional<FloatRange> ValueRange::getFloatRange() const
{
	FloatRange result;
	switch(type)
	{
		case RangeType::FLOAT:
			return floatRange;
		case RangeType::INTEGER:
			if(static_cast<double>(intRange.maxValue) <= static_cast<double>(std::numeric_limits<float>::max()) && static_cast<double>(intRange.minValue) >= static_cast<double>(std::numeric_limits<float>::min()))
			{
				result.minValue = static_cast<float>(intRange.minValue);
				result.maxValue = static_cast<float>(intRange.maxValue);
				return result;
			}
			return {};
	}
	return {};
}

Optional<IntegerRange> ValueRange::getIntRange() const
{
	IntegerRange result;
	switch(type)
	{
		case RangeType::FLOAT:
			if(static_cast<int64_t>(floatRange.maxValue) <= static_cast<int64_t>(std::numeric_limits<int32_t>::max()) && static_cast<int64_t>(floatRange.minValue) >= static_cast<int64_t>(std::numeric_limits<int32_t>::min()))
			{
				result.maxValue = static_cast<int32_t>(floatRange.maxValue);
				result.minValue = static_cast<int32_t>(floatRange.minValue);
				return result;
			}
			return {};
		case RangeType::INTEGER:
			return intRange;
	}
	return {};
}

bool ValueRange::isUnsigned() const
{
	switch(type)
	{
		case RangeType::FLOAT:
			return floatRange.minValue >= 0.0 && floatRange.maxValue >= 0.0;
		case RangeType::INTEGER:
			return intRange.minValue >= 0 && intRange.maxValue >= 0;
	}
	return false;
}

bool isInRange(double valMin, double valMax, double min, double max)
{
	return valMin >= min && valMin <= max && valMax >= min && valMax <= max;
}

bool isInRange(int64_t valMin, int64_t valMax, uint64_t numBits, bool isSigned)
{
	//signed: -2^(bits - 1)
	//unsigned: 0
	int64_t min = isSigned ? -bit_cast<uint64_t, int64_t>(1 << (numBits - 1)) : 0;
	//signed: 2^(bits - 1) - 1
	//unsigned: 2^bits - 1
	int64_t max = bit_cast<uint64_t, int64_t>(isSigned ? (1 << (numBits - 1)) : 1 << numBits) - 1;

	return valMin >= min && valMin <= max && valMax >= min && valMax <= max;
}

bool ValueRange::fitsIntoType(const DataType& type, bool isSigned) const
{
	const DataType elemType = type.getElementType();
	switch(this->type)
	{
		case RangeType::FLOAT:
			if(!type.isFloatingType())
				return false;
			if(floatTypeLimits.find(elemType.typeName) == floatTypeLimits.end())
				throw CompilationError(CompilationStep::GENERAL, "Unhandled floating-point type", type.to_string());
			return isInRange(floatRange.minValue, floatRange.maxValue, floatTypeLimits.at(elemType.typeName).first, floatTypeLimits.at(elemType.typeName).second);
		case RangeType::INTEGER:
			if(!type.isIntegralType())
				return false;
			return isInRange(intRange.minValue, intRange.maxValue, elemType.getScalarBitCount(), isSigned);
		}
	return false;
}

std::string ValueRange::to_string() const
{
	switch(type)
	{
		case RangeType::FLOAT:
			return std::string("[") + (std::to_string(floatRange.minValue) + ", ") + std::to_string(floatRange.maxValue) + "]";
		case RangeType::INTEGER:
			return std::string("[") + (std::to_string(intRange.minValue) + ", ") + std::to_string(intRange.maxValue) + "]";
	}
	throw CompilationError(CompilationStep::GENERAL, "Invalid range type", std::to_string(static_cast<unsigned>(type)));
}

FastMap<const Local*, ValueRange> ValueRange::determineValueRanges(Method& method)
{
	FastMap<const Local*, ValueRange> ranges;

	for(const Parameter& param : method.parameters)
	{
		ranges.emplace(&param, param.type);
	}

	auto it = method.walkAllInstructions();
	while(!it.isEndOfMethod())
	{
		if(it.has() && !it.has<BranchLabel>() && it->hasValueType(ValueType::LOCAL))
		{
			ValueRange& range = ranges.emplace(it->getOutput()->local, it->getOutput()->local->type).first->second;
			auto constant = it->precalculate(3);
			const Operation* op = it.get<Operation>();

			//values set by built-ins
			if(it->hasDecoration(InstructionDecorations::BUILTIN_GLOBAL_ID) || it->hasDecoration(InstructionDecorations::BUILTIN_GLOBAL_OFFSET)
					|| it->hasDecoration(InstructionDecorations::BUILTIN_GLOBAL_SIZE) || it->hasDecoration(InstructionDecorations::BUILTIN_GROUP_ID)
					|| it->hasDecoration(InstructionDecorations::BUILTIN_NUM_GROUPS))
			{
				//is always positive
				range.extendBoundaries(static_cast<int64_t>(0), std::numeric_limits<uint32_t>::max());
			}
			else if(it->hasDecoration(InstructionDecorations::BUILTIN_LOCAL_ID))
			{
				int64_t maxID = 0;
				if(method.metaData.isWorkGroupSizeSet())
				{
					maxID = *std::max_element(method.metaData.workGroupSizes.begin(), method.metaData.workGroupSizes.end()) - 1;
				}
				else
					maxID = 11;
				range.extendBoundaries(0l, maxID);
			}
			else if(it->hasDecoration(InstructionDecorations::BUILTIN_LOCAL_SIZE))
			{
				int64_t maxSize = 0;
				if(method.metaData.isWorkGroupSizeSet())
				{
					maxSize = *std::max_element(method.metaData.workGroupSizes.begin(), method.metaData.workGroupSizes.end());
				}
				else
					maxSize = 12;
				range.extendBoundaries(0l, maxSize);
			}
			else if(it->hasDecoration(InstructionDecorations::BUILTIN_WORK_DIMENSIONS))
			{
				range.extendBoundaries(static_cast<int64_t>(1), static_cast<int64_t>(3));
			}
			//loading of immediates/literals
			else if(constant.ifPresent(toFunction(&Value::isLiteralValue)))
			{
				if(constant->type.isFloatingType())
					range.extendBoundaries(constant->getLiteralValue()->real(), constant->getLiteralValue()->real());
				else
					range.extendBoundaries(std::min(static_cast<int64_t>(constant->getLiteralValue()->signedInt()), static_cast<int64_t>(constant->getLiteralValue()->unsignedInt())), std::max(static_cast<int64_t>(constant->getLiteralValue()->signedInt()), static_cast<int64_t>(constant->getLiteralValue()->unsignedInt())));
			}
			else if(constant && constant->hasType(ValueType::CONTAINER))
			{
				if(constant->type.isFloatingType())
				{
					double min = std::numeric_limits<double>::max();
					double max = std::numeric_limits<double>::min();
					for(const Value& element : constant->container.elements)
					{
						min = std::min(min, static_cast<double>(element.getLiteralValue()->real()));
						max = std::max(max, static_cast<double>(element.getLiteralValue()->real()));
					}
					range.extendBoundaries(min, max);
				}
				else
				{
					int64_t min = std::numeric_limits<int64_t>::max();
					int64_t max = std::numeric_limits<int64_t>::min();
					for(const Value& element : constant->container.elements)
					{
						min = std::min(min, std::min(static_cast<int64_t>(element.getLiteralValue()->signedInt()), static_cast<int64_t>(element.getLiteralValue()->unsignedInt())));
						max = std::max(max, std::max(static_cast<int64_t>(element.getLiteralValue()->signedInt()), static_cast<int64_t>(element.getLiteralValue()->unsignedInt())));
					}
					range.extendBoundaries(min, max);
				}
			}
			else if(constant && constant->hasRegister(REG_QPU_NUMBER))
			{
				range.extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(11));
			}
			else if(constant && constant->hasRegister(REG_ELEMENT_NUMBER))
			{
				range.extendBoundaries(static_cast<int64_t>(0), static_cast<int64_t>(NATIVE_VECTOR_SIZE) - 1);
			}
			else if(it.has<MoveOperation>() && it->getArgument(0)->hasType(ValueType::LOCAL) && ranges.find(it->getArgument(0)->local) != ranges.end())
			{
				//move -> copy range from source local (TODO: would need to link the ranges e.g. if source changes afterwards!)
				const ValueRange& sourceRange = ranges.at(it->getArgument(0)->local);
				range.extendBoundaries(sourceRange);
			}
			else if(op && op->op == OP_AND && it->readsLiteral())
			{
				/*
				 * y = x & constant
				 *
				 * y is in range [0, constant] (unsigned)
				 */
				auto argIt = std::find_if(it->getArguments().begin(), it->getArguments().end(), [](const Value& val) -> bool { return val.isLiteralValue();});
				if(argIt == it->getArguments().end())
					throw CompilationError(CompilationStep::GENERAL, "Failed to get literal argument for operation which reads a literal value", it->to_string());
				range.extendBoundaries(0, static_cast<int64_t>(argIt->getLiteralValue()->unsignedInt()));
			}
			else if(op && op->op == OP_CLZ)
			{
				/*
				 * y = clz x
				 *
				 * y is in range [0, 32] (unsigned)
				 */
				range.extendBoundaries(static_cast<int64_t>(0),  static_cast<int64_t>(32));
			}
			else if(op && (op->op == OP_FMAXABS || op->op == OP_FMINABS))
			{
				/*
				 * y = fmaxabs/fminabs x, z
				 *
				 * y is in range [0.0, float_max]
				 */
				range.extendBoundaries(0.0, std::numeric_limits<float>::max());
			}
			else if(op && op->op == OP_SHR && it->readsLiteral())
			{
				/*
				 * y = x >> constant
				 *
				 * y is in range [x.min >> constant, x.max >> constant] (unsigned)
				 */
				if(it->getArgument(0)->hasType(ValueType::LOCAL) && ranges.find(it->getArgument(0)->local) != ranges.end() && it->getArgument(1)->isLiteralValue())
				{
					const ValueRange& sourceRange = ranges.at(it->getArgument(0)->local);
					int64_t offset = static_cast<int64_t>(it->getArgument(1)->getLiteralValue()->signedInt());
					//TODO not correct if min/max is negative
					range.extendBoundaries(sourceRange.getIntRange()->minValue >> offset, sourceRange.getIntRange()->maxValue >> offset);
				}

				/*
				 * y = constant >> x
				 *
				 * y is in range [0, constant] (unsigned)
				 */
				if(it->getArgument(0)->isLiteralValue())
				{
					range.extendBoundaries(0, static_cast<int64_t>(it->getArgument(0)->getLiteralValue()->unsignedInt()));
				}
			}
			//general case for operations, only works if the used locals are only written once (otherwise, their range could change afterwards!)
			else if(op && !it->getArguments().empty() && (op->op == OP_ADD || op->op == OP_AND || op->op == OP_FADD || op->op == OP_FMAX || op->op == OP_FMAXABS || op->op == OP_FMIN || op->op == OP_FMINABS || op->op == OP_FMUL || op->op == OP_FSUB ||
					op->op == OP_ITOF || op->op == OP_MAX || op->op == OP_MIN || op->op == OP_MUL24 || op->op == OP_SHR || op->op == OP_SUB) &&
					std::all_of(it->getArguments().begin(), it->getArguments().end(), [](const Value& arg) -> bool { return arg.isLiteralValue() || (arg.getSingleWriter() != nullptr);}))
			{
				/*
				 * We have an operation (with a valid op-code) where all operands are either constants or locals which are written only once before
				 * (and therefore have a fixed range, that is already known)
				 */
				const Value arg0 = op->getFirstArg();
				ValueRange firstRange(arg0.type);
				if(arg0.isLiteralValue())
				{
					if(arg0.type.isFloatingType())
						firstRange.extendBoundaries(arg0.getLiteralValue()->real(), arg0.getLiteralValue()->real());
					else
						firstRange.extendBoundaries(std::min(static_cast<int64_t>(arg0.getLiteralValue()->signedInt()), static_cast<int64_t>(arg0.getLiteralValue()->unsignedInt())), std::min(static_cast<int64_t>(arg0.getLiteralValue()->signedInt()), static_cast<int64_t>(arg0.getLiteralValue()->unsignedInt())));
				}
				else if(arg0.hasType(ValueType::LOCAL) && ranges.find(arg0.local) != ranges.end())
					firstRange.extendBoundaries(ranges.at(arg0.local));

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
					const Value arg1 = op->getSecondArg().value();
					ValueRange secondRange(arg1.type);
					if(arg1.isLiteralValue())
					{
						if(arg1.type.isFloatingType())
							secondRange.extendBoundaries(arg1.getLiteralValue()->real(), arg1.getLiteralValue()->real());
						else
							secondRange.extendBoundaries(std::min(static_cast<int64_t>(arg1.getLiteralValue()->signedInt()), static_cast<int64_t>(arg1.getLiteralValue()->unsignedInt())), std::min(static_cast<int64_t>(arg1.getLiteralValue()->signedInt()), static_cast<int64_t>(arg1.getLiteralValue()->unsignedInt())));
					}
					else if(arg1.hasType(ValueType::LOCAL) && ranges.find(arg1.local) != ranges.end())
						secondRange.extendBoundaries(ranges.at(arg1.local));


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

					minVal = op->op.calculate(firstMin, secondMin);
					maxVal = op->op.calculate(firstMax, secondMax);
				}
				else
				{
					minVal = op->op.calculate(firstMin, NO_VALUE);
					maxVal = op->op.calculate(firstMax, NO_VALUE);
				}

				if(minVal.ifPresent(toFunction(&Value::isLiteralValue)) && maxVal.ifPresent(toFunction(&Value::isLiteralValue)))
				{
					if(it->getOutput()->type.isFloatingType())
						range.extendBoundaries(minVal->getLiteralValue()->real(), maxVal->getLiteralValue()->real());
					else
						range.extendBoundaries(std::min(static_cast<int64_t>(minVal->getLiteralValue()->signedInt()), static_cast<int64_t>(minVal->getLiteralValue()->unsignedInt())), std::max(static_cast<int64_t>(maxVal->getLiteralValue()->signedInt()), static_cast<int64_t>(maxVal->getLiteralValue()->unsignedInt())));
				}
				else
					//failed to pre-calculate the bounds
					range.extendBoundariesToUnknown(isUnsignedType(it->getOutput()->type) || it->hasDecoration(InstructionDecorations::UNSIGNED_RESULT));
			}
			else
			{
				//any other operation, set to min/max?
				range.extendBoundariesToUnknown(isUnsignedType(it->getOutput()->type) || it->hasDecoration(InstructionDecorations::UNSIGNED_RESULT));
			}
		}

		it.nextInMethod();
	}

#ifdef DEBUG_MODE
	std::for_each(ranges.begin(), ranges.end(), [](const std::pair<const Local*, ValueRange>& pair) -> void {
		logging::debug() << "Local " << pair.first-> to_string() << " with range " << pair.second.to_string() << logging::endl;
	});
#endif

	return ranges;
}

void ValueRange::extendBoundaries(double newMin, double newMax)
{
	if(newMax < newMin)
		std::swap(newMax, newMin);
	switch(type)
	{
		case RangeType::FLOAT:
			if(hasDefaultBoundaries)
			{
				floatRange.maxValue = newMax;
				floatRange.minValue = newMin;
			}
			else
			{
				floatRange.maxValue = std::max(floatRange.maxValue, newMax);
				floatRange.minValue = std::min(floatRange.minValue, newMin);
			}
			break;
		case RangeType::INTEGER:
			if(hasDefaultBoundaries)
			{
				intRange.maxValue = static_cast<int64_t>(newMax);
				intRange.minValue = static_cast<int64_t>(newMin);
			}
			else
			{
				intRange.maxValue = std::max(intRange.maxValue, static_cast<int64_t>(newMax));
				intRange.minValue = std::min(intRange.minValue, static_cast<int64_t>(newMin));
			}
			break;
	}

	hasDefaultBoundaries = false;
}

void ValueRange::extendBoundaries(int64_t newMin, int64_t newMax)
{
	if(newMax < newMin)
		std::swap(newMax, newMin);
	switch(type)
	{
		case RangeType::FLOAT:
			if(hasDefaultBoundaries)
			{
				floatRange.maxValue = static_cast<double>(newMax);
				floatRange.minValue = static_cast<double>(newMin);
			}
			else
			{
				floatRange.maxValue = std::max(floatRange.maxValue, static_cast<double>(newMax));
				floatRange.minValue = std::min(floatRange.minValue, static_cast<double>(newMin));
			}
			break;
		case RangeType::INTEGER:
			if(hasDefaultBoundaries)
			{
				intRange.maxValue = newMax;
				intRange.minValue = newMin;
			}
			else
			{
				intRange.maxValue = std::max(intRange.maxValue, newMax);
				intRange.minValue = std::min(intRange.minValue, newMin);
			}
			break;
	}

	hasDefaultBoundaries = false;
}

void ValueRange::extendBoundaries(const ValueRange& other)
{
	switch(other.type)
	{
		case RangeType::FLOAT:
			extendBoundaries(other.getFloatRange()->minValue, other.getFloatRange()->maxValue);
			break;
		case RangeType::INTEGER:
			extendBoundaries(other.getIntRange()->minValue, other.getIntRange()->maxValue);
			break;
	}
}

void ValueRange::extendBoundariesToUnknown(bool isKnownToBeUnsigned)
{
	if(hasDefaultBoundaries)
		//the default boundaries are already the widest for the underlying type
		return;
	switch(type)
	{
		case RangeType::FLOAT:
			floatRange = FloatRange();
			break;
		case RangeType::INTEGER:
			intRange = IntegerRange();
			if(isKnownToBeUnsigned)
				intRange.minValue = 0;
			break;
	}
	hasDefaultBoundaries = false;
}
