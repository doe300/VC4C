/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VALUE_RANGE_H
#define VC4C_VALUE_RANGE_H

#include "../HalfType.h"
#include "../performance.h"
#include "Optional.h"

#include <algorithm>
#include <inttypes.h>
#include <limits>

namespace vc4c
{
    class DataType;
    class Local;
    class Literal;
    class Method;
    struct Value;
    struct Expression;

    namespace intermediate
    {
        class IntermediateInstruction;
        enum class InstructionDecorations : uint32_t;
    } // namespace intermediate

    namespace analysis
    {
        /*
         * Contains a the value range for a certain value/expression
         *
         * The value range only considers the values an object can take on at a given point in time (or in general) and
         * does not consider the different representations (e.g. float, integer, short, half_t, ...)!
         *
         * The underlying type is chosen as double, since it can exactly represent all 32-bit integer and float values.
         *
         * The ValueRange type represents a closed interval, containing both bounds in the range!
         */
        class ValueRange
        {
        public:
            explicit ValueRange() = default;
            explicit ValueRange(Literal lit);
            explicit ValueRange(DataType type);
            constexpr ValueRange(double min, double max) : minValue(min), maxValue(max) {}

            /*
             * Returns whether all possible values are positive
             */
            bool isUnsigned() const;

            /**
             * Returns whether all values of this range fit into the given type
             */
            bool fitsIntoType(DataType type, bool isSigned = true) const;

            /**
             * Returns whether this range fits fully into the other given range
             */
            constexpr bool fitsIntoRange(const ValueRange& other) const noexcept
            {
                return minValue >= other.minValue && maxValue <= other.maxValue;
            }

            bool hasExplicitBoundaries() const;

            inline explicit operator bool() const noexcept
            {
                return hasExplicitBoundaries();
            }

            Optional<Value> getLowerLimit(DataType type) const;
            Optional<Value> getUpperLimit(DataType type) const;
            constexpr Optional<double> getSingletonValue() const noexcept
            {
                if(minValue == maxValue)
                    return minValue;
                return {};
            }

            /**
             * Returns the range of the absolute values from this range
             */
            ValueRange toAbsoluteRange() const noexcept;

            std::string to_string() const;

            static Optional<ValueRange> getValueRange(
                intermediate::InstructionDecorations deco, const Method* method = nullptr);
            static ValueRange getValueRange(const Value& val, const Method* method = nullptr);
            static ValueRange getValueRangeRecursive(const Value& val, const Method* method = nullptr);
            static ValueRange getValueRange(const Expression& expr, const Method* method = nullptr);
            static FastMap<const Local*, ValueRange> determineValueRanges(Method& method);

            constexpr ValueRange operator*(double val) const noexcept
            {
                return ValueRange{minValue * val, maxValue * val};
            }

            constexpr ValueRange operator/(double val) const noexcept
            {
                return ValueRange{minValue / val, maxValue / val};
            }

            constexpr ValueRange operator+(double val) const noexcept
            {
                return ValueRange{minValue + val, maxValue + val};
            }

            constexpr ValueRange operator-(double val) const noexcept
            {
                return ValueRange{minValue - val, maxValue - val};
            }

            bool operator==(const ValueRange& other) const;
            bool operator!=(const ValueRange& other) const
            {
                return !(*this == other);
            }

            double minValue = -std::numeric_limits<double>::infinity();
            double maxValue = std::numeric_limits<double>::infinity();

        private:
            void extendBoundaries(double newMin, double newMax);
            void extendBoundaries(const ValueRange& other);
            void extendBoundaries(Literal literal, bool isFloat);
            void extendBoundariesToUnknown(bool isKnownToBeUnsigned = false);
            void shrinkToIntersection(const ValueRange& other);
            void update(const Optional<Value>& constant, const FastMap<const Local*, ValueRange>& ranges,
                const intermediate::IntermediateInstruction* it = nullptr, const Method* method = nullptr);

            static void updateRecursively(const Local* currentLocal, const Method* method,
                FastMap<const Local*, ValueRange>& ranges,
                FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
                FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet);

            static void processedOpenSet(const Method* method, FastMap<const Local*, ValueRange>& ranges,
                FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
                FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet);
        };

        extern ValueRange RANGE_HALF;
        constexpr ValueRange RANGE_FLOAT{static_cast<double>(std::numeric_limits<float>::lowest()),
            static_cast<double>(std::numeric_limits<float>::max())};
        constexpr ValueRange RANGE_UCHAR{static_cast<double>(std::numeric_limits<uint8_t>::min()),
            static_cast<double>(std::numeric_limits<uint8_t>::max())};
        constexpr ValueRange RANGE_CHAR{static_cast<double>(std::numeric_limits<int8_t>::min()),
            static_cast<double>(std::numeric_limits<int8_t>::max())};
        constexpr ValueRange RANGE_USHORT{static_cast<double>(std::numeric_limits<uint16_t>::min()),
            static_cast<double>(std::numeric_limits<uint16_t>::max())};
        constexpr ValueRange RANGE_SHORT{static_cast<double>(std::numeric_limits<int16_t>::min()),
            static_cast<double>(std::numeric_limits<int16_t>::max())};
        constexpr ValueRange RANGE_UINT{static_cast<double>(std::numeric_limits<uint32_t>::min()),
            static_cast<double>(std::numeric_limits<uint32_t>::max())};
        constexpr ValueRange RANGE_INT{static_cast<double>(std::numeric_limits<int32_t>::min()),
            static_cast<double>(std::numeric_limits<int32_t>::max())};

    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_VALUE_RANGE_H */
