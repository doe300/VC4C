/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VALUE_RANGE_H
#define VC4C_VALUE_RANGE_H

#include "../Variant.h"
#include "../performance.h"
#include "Optional.h"

#include <inttypes.h>
#include <limits>

namespace vc4c
{
    class DataType;
    class Local;
    class Literal;
    class Method;
    struct Value;

    namespace intermediate
    {
        class IntermediateInstruction;
    }

    namespace analysis
    {
        struct FloatRange
        {
            double minValue = static_cast<double>(std::numeric_limits<float>::lowest());
            double maxValue = static_cast<double>(std::numeric_limits<float>::max());
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
            ValueRange(DataType type);

            Optional<FloatRange> getFloatRange() const;
            Optional<IntegerRange> getIntRange() const;

            /*
             * Returns whether all possible values are positive
             */
            bool isUnsigned() const;
            bool fitsIntoType(DataType type, bool isSigned = true) const;
            bool hasExplicitBoundaries() const;

            Optional<Value> getLowerLimit() const;
            Optional<Value> getUpperLimit() const;

            std::string to_string() const;

            static ValueRange getValueRange(const Value& val, Method* method = nullptr);
            static ValueRange getValueRangeRecursive(const Value& val, Method* method = nullptr);
            static FastMap<const Local*, ValueRange> determineValueRanges(Method& method);

        private:
            Variant<FloatRange, IntegerRange> range;
            bool hasDefaultBoundaries;

            void extendBoundaries(double newMin, double newMax);
            void extendBoundaries(int64_t newMin, int64_t newMax);
            void extendBoundaries(const ValueRange& other);
            void extendBoundaries(Literal literal, bool isFloat);
            void extendBoundariesToUnknown(bool isKnownToBeUnsigned = false);
            void update(const Optional<Value>& constant, const FastMap<const Local*, ValueRange>& ranges,
                const intermediate::IntermediateInstruction* it = nullptr, Method* method = nullptr);

            static void updateRecursively(const Local* currentLocal, Method* method,
                FastMap<const Local*, ValueRange>& ranges,
                FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
                FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet);

            static void processedOpenSet(Method* method, FastMap<const Local*, ValueRange>& ranges,
                FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
                FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet);
        };

    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_VALUE_RANGE_H */
