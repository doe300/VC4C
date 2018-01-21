/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VALUE_RANGE_H
#define VC4C_VALUE_RANGE_H

#include "helper.h"
#include "../performance.h"

#include <inttypes.h>
#include <limits>

namespace vc4c
{
	struct DataType;
	class Local;
	class Method;

	namespace intermediate
	{
		struct FloatRange
		{
			double minValue = std::numeric_limits<float>::lowest();
			double maxValue = std::numeric_limits<float>::max();
		};

		struct IntegerRange
		{
			int64_t minValue = std::numeric_limits<int32_t>::lowest();
			int64_t maxValue = std::numeric_limits<uint32_t>::max();
		};

		/*
		 * Contains a the value range for a certain local
		 */
		class ValueRange
		{
		public:

			ValueRange(bool isFloat, bool isSigned = true);
			ValueRange(const DataType& type);

			Optional<FloatRange> getFloatRange() const;
			Optional<IntegerRange> getIntRange() const;

			/*
			 * Returns whether all possible values are positive
			 */
			bool isUnsigned() const;
			bool fitsIntoType(const DataType& type, bool isSigned = true) const;

			std::string to_string() const;

			static FastMap<const Local*, ValueRange> determineValueRanges(Method& method);

		private:

			enum class RangeType
			{
				FLOAT, INTEGER
			};

			union
			{
				FloatRange floatRange;
				IntegerRange intRange;
			};

			const RangeType type;
			bool hasDefaultBoundaries;

			void extendBoundaries(double newMin, double newMax);
			void extendBoundaries(int64_t newMin, int64_t newMax);
			void extendBoundaries(const ValueRange& other);
			void extendBoundariesToUnknown(bool isKnownToBeUnsigned = false);
		};

	} /* namespace intermediate */
} /* namespace vc4c */

#endif /* VC4C_VALUE_RANGE_H */
