/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VALUE_RANGE_H
#define VC4C_VALUE_RANGE_H

#include "../performance.h"
#include "../tools/SmallSet.h"
#include "Analysis.h"
#include "Optional.h"

#include <algorithm>
#include <functional>
#include <limits>
#include <memory>
#include <string>

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
        struct ValueRanges;

        enum class RangeType : uint8_t
        {
            // The range cannot be determined, it is "infinite"
            INDETERMINATE,
            // The actual dynamic value range cannot exceed this range (e.g. the range allowed by a data type)
            MAXIMUM,
            // The actual dynamic value represents this exact range
            FIXED
        };

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
            explicit ValueRange();
            explicit ValueRange(Literal lit, DataType type);
            explicit ValueRange(DataType type);
            explicit constexpr ValueRange(double val) : minValue(val), maxValue(val), type(RangeType::FIXED) {}
            constexpr ValueRange(double min, double max) :
                minValue(std::min(min, max)), maxValue(std::max(min, max)), type(RangeType::MAXIMUM)
            {
            }

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

            constexpr bool hasExplicitBoundaries() const
            {
                return type != RangeType::INDETERMINATE;
            }

            inline explicit operator bool() const noexcept
            {
                return hasExplicitBoundaries();
            }

            Optional<Value> getLowerLimit(DataType type) const;
            Optional<Value> getUpperLimit(DataType type) const;
            constexpr Optional<double> getSingletonValue() const noexcept
            {
                if(hasExplicitBoundaries() && minValue == maxValue)
                    return minValue;
                return {};
            }

            /**
             * Returns the range of the absolute values from this range
             */
            ValueRange toAbsoluteRange() const noexcept;

            constexpr double getRange() const noexcept
            {
                if(type == RangeType::INDETERMINATE)
                    return std::numeric_limits<double>::infinity();
                return maxValue - minValue;
            }

            std::string to_string() const;

            static Optional<ValueRange> getValueRange(
                intermediate::InstructionDecorations deco, const Method* method = nullptr);
            static ValueRange getValueRangeFlat(const intermediate::IntermediateInstruction& inst,
                const FastMap<const Local*, ValueRange>& knownRanges, const Method* method = nullptr);
            static ValueRange getValueRangeFlat(
                const Value& val, bool indeterminateOnFulRange, const Method* method = nullptr);
            static ValueRange getValueRangeRecursive(
                const Value& val, Method* method, FastMap<const Local*, ValueRange>& knownRanges);
            static ValueRange getValueRange(const Expression& expr, const Method* method = nullptr);

            ValueRange operator*(double val) const noexcept
            {
                return ValueRange{*this} *= val;
            }
            ValueRange& operator*=(double val) noexcept;

            ValueRange operator/(double val) const noexcept
            {
                return ValueRange{*this} /= val;
            }
            ValueRange& operator/=(double val) noexcept;

            ValueRange operator+(double val) const noexcept
            {
                return ValueRange{*this} += val;
            }
            ValueRange& operator+=(double val) noexcept;
            ValueRange operator+(const ValueRange& other) const noexcept
            {
                return ValueRange{*this} += other;
            }
            ValueRange& operator+=(const ValueRange& other) noexcept;

            ValueRange operator-(double val) const noexcept
            {
                return ValueRange{*this} -= val;
            }
            ValueRange& operator-=(double val) noexcept;
            ValueRange operator-(const ValueRange& other) const noexcept
            {
                return ValueRange{*this} -= other;
            }
            ValueRange& operator-=(const ValueRange& other) noexcept;

            ValueRange operator|(const ValueRange& other) const noexcept
            {
                return ValueRange{*this} |= other;
            }
            ValueRange& operator|=(const ValueRange& other) noexcept;
            ValueRange operator&(const ValueRange& other) const noexcept
            {
                return ValueRange{*this} &= other;
            }
            ValueRange& operator&=(const ValueRange& other) noexcept;

            bool operator==(const ValueRange& other) const;
            bool operator!=(const ValueRange& other) const
            {
                return !(*this == other);
            }

            ValueRange transform(const std::function<double(double)>& func) const;

            double minValue;
            double maxValue;

        private:
            RangeType type;

            void extendBoundaries(const ValueRange& other);
            void extendBoundaries(Literal literal, bool isFloat, bool isKnownSigned, bool isKnownUnsigned);
            void update(const Optional<Value>& constant, const FastMap<const Local*, ValueRange>& ranges,
                const intermediate::IntermediateInstruction* it = nullptr, const Method* method = nullptr,
                const BuiltinLocal* builtin = nullptr);

            static void updateRecursively(const Local* currentLocal, Method* method,
                FastMap<const Local*, ValueRange>& ranges,
                FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
                FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet, bool secondRun);

            static void processedOpenSet(Method* method, FastMap<const Local*, ValueRange>& ranges,
                FastMap<const intermediate::IntermediateInstruction*, ValueRange>& closedSet,
                FastMap<const intermediate::IntermediateInstruction*, Optional<ValueRange>>& openSet);

            friend class ValueRangeAnalysis;
        };

        ValueRange min(const ValueRange& one, const ValueRange& other) noexcept;
        ValueRange max(const ValueRange& one, const ValueRange& other) noexcept;

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

        struct PartialRange
        {
            FastAccessList<
                std::pair<const intermediate::IntermediateInstruction*, tools::SmallSortedPointerSet<const Local*>>>
                generatingExpressions;
        };

        struct ValueRanges
        {
            FastMap<const Local*, ValueRange> fullRanges;
            FastMap<const Local*, PartialRange> partialRanges;
        };

        /**
         * Analysis the value ranges of every local written in a single basic block
         */
        class ValueRangeAnalysis : public LocalAnalysis<AnalysisDirection::FORWARD, ValueRanges>
        {
        public:
            explicit ValueRangeAnalysis(ValueRanges&& initialRanges = {});

            static std::string to_string(const ValueRanges& knownRanges);

            /**
             * Updates the given previously known value ranges with the operation executed in the given instruction.
             *
             * NOTE: If only the value ranges at a certain point are of interest (and the value ranges at any point in
             * the basic block), this function should be directly called instead of the analysis run over the whole
             * block and the previous result should be passed in as new input to reduce the memory overhead!
             */
            static ValueRanges analyzeRanges(
                const intermediate::IntermediateInstruction* inst, const ValueRanges& previousRanges, void*&);
        };

    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_VALUE_RANGE_H */
