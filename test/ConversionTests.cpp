/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include <algorithm>
#include <cfenv>
#include <climits>
#include <cmath>
#include <cstring>
#include <functional>
#include <limits>
#include <stdexcept>

// TODO skip testing for all the out-of-bounds behavior, at least where it is UB anyway?

static const std::string CONVERSION_FUNCTION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define CONCAT(A, B) CONCAT_(A, B)
#define CONCAT_(A, B) A##B

__kernel void test(__global CONCAT(OUT,16)* out, const __global CONCAT(IN,16)* in) {
  size_t gid = get_global_id(0);
  out[gid] = CONCAT(CONCAT(convert_,OUT),16)(in[gid]);
}

__kernel void test_operator(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = (OUT)(in[gid]);
}

#ifndef NO_SATURATION
__kernel void test_saturate(__global CONCAT(OUT,16)* out, const __global CONCAT(IN,16)* in) {
  size_t gid = get_global_id(0);
  out[gid] = CONCAT(CONCAT(CONCAT(convert_,OUT),16), _sat)(in[gid]);
}
#endif

__kernel void test_reinterpret_address(__global CONCAT(OUT,OUTNUM)* out, const __global CONCAT(IN,INNUM)* in) {
  size_t gid = get_global_id(0);
  // This directly reads the input as a value of the output type
  out[gid] = CONCAT(as_,CONCAT(OUT,OUTNUM))(in[gid]);
}

__kernel void test_reinterpret_value(__global CONCAT(OUT,OUTNUM)* out, const __global CONCAT(IN,INNUM)* in1, const __global CONCAT(IN,INNUM)* in2) {

  size_t gid = get_global_id(0);
  CONCAT(IN,INNUM) a = in1[gid];
  CONCAT(IN,INNUM) b = in2[gid];
  // we do some modifications before to disable reading as output type directly
  out[gid] = CONCAT(as_,CONCAT(OUT,OUTNUM))(a + a) + CONCAT(as_,CONCAT(OUT,OUTNUM))(b + b);
}
)";

template <typename R, typename T>
static R castToType(T val)
{
    if(std::is_floating_point<R>::value)
    {
#pragma STDC FENV_ACCESS on
        // need special handling, need to use trunc-to-zero conversion, since this is also used on the VideoCore IV GPU
        auto oldMode = std::fegetround();
        std::fesetround(FE_TOWARDZERO);
        auto result = static_cast<R>(val);
        std::fesetround(oldMode);
        return result;
    }
    return static_cast<R>(val);
}

template <typename R>
struct FloatToIntConverter
{
    // TODO does not work for 64-bit integers
    R operator()(float val) const noexcept
    {
        // Out of range for float -> x: implementation defined (OpenCL C 3.0, 6.4.3.3)
        // -> VideoCore IV GPU returns 0 for itof for most out-of-bounds values (this is on 32-bit signed integer
        // bounds!)
        auto roundedValue = std::trunc(val);
        if(std::isinf(val) || std::isnan(val))
            return 0;
        if(val == std::numeric_limits<float>::max() || val == std::numeric_limits<float>::lowest())
            // special case for exact FLT_MIN and FLT_MAX
            return static_cast<R>(std::numeric_limits<int32_t>::min());
        if(roundedValue > static_cast<float>(std::numeric_limits<int32_t>::max()))
            return 0;
        if(roundedValue < static_cast<float>(std::numeric_limits<int32_t>::min()))
            return 0;
        return static_cast<R>(static_cast<int32_t>(roundedValue));
    }
};

template <>
struct FloatToIntConverter<uint32_t>
{
    uint32_t operator()(float val) const noexcept
    {
        // Out of range for float -> x: implementation defined (OpenCL C 3.0, 6.4.3.3)
        // -> We have a special intrinsic for float -> 32-bit unsigned integer
        auto roundedValue = std::trunc(val);
        if(std::isinf(val) || std::isnan(val))
            return 0;
        if(val == std::numeric_limits<float>::max() || val == std::numeric_limits<float>::lowest())
            // special case for exact FLT_MIN and FLT_MAX
            return static_cast<uint32_t>(std::numeric_limits<int32_t>::min());
        if(roundedValue > static_cast<float>(std::numeric_limits<uint32_t>::max()))
            return std::numeric_limits<uint32_t>::max();
        return static_cast<uint32_t>(roundedValue);
    }
};

template <typename R, typename T>
struct GeneralConverter
{
    R operator()(T val) const noexcept
    {
        // Out of range for x -> signed integer: implementation defined (C99, 6.3.1.3.3)
        // Out of range for x -> unsigned integer: truncate upper bits, bitcast (C99, confusingly described
        // in 6.3.1.3.2)
        // -> we do truncate + bitcast in both cases, which is also done by the host compiler
        return static_cast<R>(val);
    }
};

template <typename R, typename T>
static R saturate(T s)
{
    if(std::is_unsigned<R>::value == std::is_unsigned<T>::value && sizeof(R) > sizeof(T))
        // both are signed or unsigned and result is bigger than input -> no saturation required
        return static_cast<R>(s);
    if(std::is_signed<T>::value && std::is_unsigned<R>::value)
        // truncate to zero in the signed type to avoid bit-casting negative values to huge positive values
        s = std::max(T{0}, s);
    if(std::is_unsigned<T>::value && std::is_signed<R>::value && sizeof(T) >= sizeof(T))
        // truncate to RESULT_MAX in unsigned value, since it is guaranteed to be representable there
        s = std::min(static_cast<T>(std::numeric_limits<R>::max()), s);
    if(std::is_same<R, uint64_t>::value)
        return std::max(std::numeric_limits<uint64_t>::min(),
            std::min(std::numeric_limits<uint64_t>::max(), static_cast<uint64_t>(s)));
    if(std::is_same<T, uint64_t>::value)
        return static_cast<R>(std::min(static_cast<uint64_t>(std::numeric_limits<R>::max()), static_cast<uint64_t>(s)));
    return static_cast<R>(std::max(static_cast<int64_t>(std::numeric_limits<R>::min()),
        std::min(static_cast<int64_t>(std::numeric_limits<R>::max()), static_cast<int64_t>(s))));
}

template <typename R>
static R saturate(float val)
{
    if(std::isnan(val))
        // OpenCL C 3.0, section 6.4.3.3: "NaN should be converted to 0"
        return 0;
    if(std::isinf(val))
        return std::signbit(val) ? std::numeric_limits<R>::min() : std::numeric_limits<R>::max();
    if(std::is_unsigned<R>::value)
        val = std::max(val, 0.0f);
    return static_cast<R>(std::max(static_cast<double>(std::numeric_limits<R>::min()),
        std::min(static_cast<double>(std::numeric_limits<R>::max()), static_cast<double>(val))));
}

template <typename R, typename T, std::size_t Factor>
struct CombineElements
{
    std::vector<R> operator()(const std::vector<T>& in) const noexcept
    {
        std::vector<R> result;
        result.reserve(in.size() / Factor);
        for(std::size_t i = 0; i < in.size(); i += Factor)
        {
            std::array<T, Factor> inTmp{};
            std::copy(in.begin() + i, in.begin() + i + Factor, inTmp.begin());
            R outTmp{};
            std::memcpy(&outTmp, inTmp.data(), std::min(sizeof(R), sizeof(T) * Factor));
            result.emplace_back(outTmp);
        }
        return result;
    }

    std::vector<R> operator()(const std::vector<T>& in0, const std::vector<T>& in1) const noexcept
    {
        std::vector<R> result;
        result.reserve(in0.size() / Factor);
        for(std::size_t i = 0; i < in0.size(); i += Factor)
        {
            std::array<T, Factor> inTmp0{};
            std::array<T, Factor> inTmp1{};
            std::transform(in0.begin() + i, in0.begin() + i + Factor, in0.begin() + i, inTmp0.begin(), std::plus<T>{});
            std::transform(in1.begin() + i, in1.begin() + i + Factor, in1.begin() + i, inTmp1.begin(), std::plus<T>{});
            R outTmp0{};
            R outTmp1{};
            std::memcpy(&outTmp0, inTmp0.data(), std::min(sizeof(R), sizeof(T) * Factor));
            std::memcpy(&outTmp1, inTmp1.data(), std::min(sizeof(R), sizeof(T) * Factor));
            if(std::is_floating_point<R>::value && std::fpclassify(outTmp0 + outTmp1) == FP_SUBNORMAL)
                // need to flush subnormal results to zero
                result.emplace_back(0);
            else
                result.emplace_back(outTmp0 + outTmp1);
        }
        return result;
    }
};

template <typename R, typename T, std::size_t Factor>
struct SplitElements
{
    std::vector<R> operator()(const std::vector<T>& in) const noexcept
    {
        std::vector<R> result;
        result.reserve(in.size() * Factor);
        for(std::size_t i = 0; i < in.size(); i++)
        {
            std::array<R, Factor> outTmp{};
            std::memcpy(outTmp.data(), &in[i], std::min(sizeof(T), sizeof(R) * Factor));
            result.insert(result.end(), outTmp.begin(), outTmp.end());
        }
        return result;
    }

    std::vector<R> operator()(const std::vector<T>& in0, const std::vector<T>& in1) const noexcept
    {
        std::vector<R> result;
        result.reserve(in0.size() * Factor);
        for(std::size_t i = 0; i < in0.size(); i++)
        {
            T inTmp0 = static_cast<T>(in0[i] + in0[i]);
            T inTmp1 = static_cast<T>(in1[i] + in1[i]);
            std::array<R, Factor> outTmp0{};
            std::array<R, Factor> outTmp1{};
            std::memcpy(outTmp0.data(), &inTmp0, std::min(sizeof(T), sizeof(R) * Factor));
            std::memcpy(outTmp1.data(), &inTmp1, std::min(sizeof(T), sizeof(R) * Factor));
            std::transform(outTmp0.begin(), outTmp0.end(), outTmp1.begin(), std::back_inserter(result), std::plus<R>{});
        }
        return result;
    }
};

template <typename R, typename T>
using ReinterpretCaster = std::conditional_t<sizeof(R) < sizeof(T), SplitElements<R, T, sizeof(T) / sizeof(R)>,
    CombineElements<R, T, sizeof(R) / sizeof(T)>>;

/**
 * This function generates the actual tests for conversion from and to a specific type pair
 */
template <std::size_t InIndex, std::size_t OutIndex, typename... Types>
static void generateConversionFunctions(const std::array<std::string, sizeof...(Types)>& typeNames)
{
    using namespace test_data;

    using InType = std::tuple_element_t<InIndex, std::tuple<Types...>>;
    using OutType = std::tuple_element_t<OutIndex, std::tuple<Types...>>;
    using Converter = std::conditional_t<std::is_floating_point<InType>::value, FloatToIntConverter<OutType>,
        GeneralConverter<OutType, InType>>;
    // 1 ULP for round-to-nearest-even vs. trunc-to-zero
    using Comparator =
        std::conditional_t<std::is_floating_point<OutType>::value, CompareULP<1>, CompareEquals<OutType>>;

    std::vector<InType> input = {0, 17, 42,                                          // 3
        static_cast<InType>(-1), static_cast<InType>(-17), static_cast<InType>(-42), // 6
        std::numeric_limits<InType>::min(), std::numeric_limits<InType>::max(),
        std::numeric_limits<InType>::lowest(), // 9
        castToType<InType>(std::numeric_limits<OutType>::min()),
        castToType<InType>(std::numeric_limits<OutType>::max()),
        castToType<InType>(std::numeric_limits<OutType>::lowest()), // 12
        static_cast<InType>(-0), std::numeric_limits<InType>::infinity(), -std::numeric_limits<InType>::infinity(),
        std::numeric_limits<InType>::quiet_NaN()};
    if(input.size() != 16)
        throw std::runtime_error{
            "Tests are all set up for 16 input elements, but we got " + std::to_string(input.size())};

    auto flags = DataFilter::TYPE_CONVERSIONS;
    if(sizeof(InType) > sizeof(uint32_t) || sizeof(OutType) > sizeof(uint32_t))
    {
        // long conversions are not yet correctly handled in SPIR-V front-end, at least not in CI
        flags = flags | DataFilter::USES_LONG | DataFilter::SPIRV_DISABLED;
    }
    if(std::is_floating_point<InType>::value)
        // most (if not all) conversions from float have some errors on edge cases
        flags = flags | DataFilter::DISABLED;

    bool reducesElementCount = sizeof(InType) < sizeof(OutType);
    auto factor = reducesElementCount ? (sizeof(OutType) / sizeof(InType)) : (sizeof(InType) / sizeof(OutType));
    auto inElements = reducesElementCount ? 16 : (16 / factor);
    auto outElements = reducesElementCount ? (16 / factor) : 16;

    auto options = "-DIN=" + typeNames[InIndex] + " -DOUT=" + typeNames[OutIndex] +
        " -DINNUM=" + std::to_string(inElements) + " -DOUTNUM=" + std::to_string(outElements);
    if(std::is_floating_point<OutType>::value)
        // There is no saturation to floating point types
        options += " -DNO_SATURATION";

    {
        TestDataBuilder<Buffer<OutType>, Buffer<InType>> builder(
            "convert_" + typeNames[InIndex] + "16_to_" + typeNames[OutIndex] + "16", CONVERSION_FUNCTION, "test",
            options);
        builder.setFlags(flags);
        builder.calculateDimensions(input.size(), 16);
        builder.template allocateParameter<0>(input.size(), 0x42);
        builder.template setParameter<1>(Buffer<InType>(input));
        builder.template checkParameter<0, Comparator>(transform<OutType, InType>(input, Converter{}));
    }

    {
        TestDataBuilder<Buffer<OutType>, Buffer<InType>> builder(
            "cast_" + typeNames[InIndex] + "_to_" + typeNames[OutIndex], CONVERSION_FUNCTION, "test_operator", options);
        builder.setFlags(flags);
        builder.calculateDimensions(input.size(), 1);
        builder.template allocateParameter<0>(input.size(), 0x42);
        builder.template setParameter<1>(Buffer<InType>(input));
        builder.template checkParameter<0, Comparator>(transform<OutType, InType>(input, Converter{}));
    }

    if(!std::is_same<float, OutType>::value)
    {
        // There is no saturation to floating point types
        auto additionalFlags = (std::is_same<InType, int64_t>::value && std::is_same<OutType, uint32_t>::value) ||
                (std::is_same<InType, uint64_t>::value && std::is_same<OutType, int64_t>::value) ?
            DataFilter::DISABLED :
            DataFilter::NONE;
        TestDataBuilder<Buffer<OutType>, Buffer<InType>> builder(
            "saturate_" + typeNames[InIndex] + "16_to_" + typeNames[OutIndex] + "16", CONVERSION_FUNCTION,
            "test_saturate", options);
        builder.setFlags(flags | additionalFlags);
        builder.calculateDimensions(input.size(), 16);
        builder.template allocateParameter<0>(input.size(), 0x42);
        builder.template setParameter<1>(std::vector<InType>(input));
        builder.template checkParameterEquals<0>(
            transform<OutType, InType>(input, [](InType in) -> OutType { return saturate<OutType>(in); }));
    }

    auto someValues = toRandom<InType>(16, true);
    auto outputSize = (someValues.size() * sizeof(InType)) / sizeof(OutType);

    // TODO also has some conversion errors in CI to float
    auto additonalFlags = std::is_floating_point<OutType>::value ? DataFilter::DISABLED : DataFilter::NONE;

    {
        TestDataBuilder<Buffer<OutType>, Buffer<InType>> builder("reinterpret_address_" + typeNames[InIndex] +
                std::to_string(inElements) + "_to_" + typeNames[OutIndex] + std::to_string(outElements),
            CONVERSION_FUNCTION, "test_reinterpret_address", options);
        builder.setFlags(flags | additonalFlags);
        builder.calculateDimensions(someValues.size(), inElements);
        builder.template allocateParameter<0>(outputSize, 0x42);
        builder.template setParameter<1>(std::vector<InType>(someValues));
        builder.template checkParameterEquals<0>(ReinterpretCaster<OutType, InType>{}(someValues));
    }

    {
        // FIXME has errors on emulator from float to any integer type which the hardware execution does not have!
        TestDataBuilder<Buffer<OutType>, Buffer<InType>, Buffer<InType>> builder("reinterpret_value_" +
                typeNames[InIndex] + std::to_string(inElements) + "_to_" + typeNames[OutIndex] +
                std::to_string(outElements),
            CONVERSION_FUNCTION, "test_reinterpret_value", options);
        builder.setFlags(flags | DataFilter::VECTOR_OPERATIONS | additonalFlags);
        builder.calculateDimensions(someValues.size(), inElements);
        builder.template allocateParameter<0>(outputSize, 0x42);
        builder.template setParameter<1>(std::vector<InType>(someValues));
        builder.template setParameter<2>(std::vector<InType>(someValues));
        builder.template checkParameterEquals<0>(ReinterpretCaster<OutType, InType>{}(someValues, someValues));
    }
}

/**
 * Helper type to create a cross product of all listed types
 */
template <std::size_t InIndex, std::size_t OutIndex, typename... Types>
struct OutputTypeIterator
{
    void operator()(const std::array<std::string, sizeof...(Types)>& typeNames) const
    {
        generateConversionFunctions<InIndex, OutIndex, Types...>(typeNames);
        OutputTypeIterator<InIndex, OutIndex - 1, Types...>{}(typeNames);
    }
};

/**
 * Helper type to create a cross product of all listed types
 *
 * Specialization for end of induction
 */
template <std::size_t InIndex, typename... Types>
struct OutputTypeIterator<InIndex, 0, Types...>
{
    void operator()(const std::array<std::string, sizeof...(Types)>& typeNames) const
    {
        generateConversionFunctions<InIndex, 0, Types...>(typeNames);
    }
};

/**
 * Helper type to create a cross product of all listed types
 */
template <std::size_t InIndex, std::size_t OutIndex, typename... Types>
struct InputTypeIterator
{
    void operator()(const std::array<std::string, sizeof...(Types)>& typeNames) const
    {
        OutputTypeIterator<InIndex, OutIndex, Types...>{}(typeNames);
        InputTypeIterator<InIndex - 1, OutIndex, Types...>{}(typeNames);
    }
};

/**
 * Helper type to create a cross product of all listed types
 *
 * Specialization for end of induction
 */
template <std::size_t OutIndex, typename... Types>
struct InputTypeIterator<0, OutIndex, Types...>
{
    void operator()(const std::array<std::string, sizeof...(Types)>& typeNames) const
    {
        OutputTypeIterator<0, OutIndex, Types...>{}(typeNames);
    }
};

template <std::size_t NumTypes, typename... Types>
static void generateAllConversionFunctions(const std::array<std::string, NumTypes>& typeNames)
{
    static_assert(NumTypes == sizeof...(Types), "");
    InputTypeIterator<NumTypes - 1, NumTypes - 1, Types...>{}(typeNames);
}

void test_data::registerTypeConversionTests()
{
    std::array<std::string, 9> typeNames = {
        "char", "uchar", "short", "ushort", "int", "uint", "long", "ulong", "float"};
    generateAllConversionFunctions<9, int8_t, uint8_t, int16_t, uint16_t, int32_t, uint32_t, int64_t, uint64_t, float>(
        typeNames);

    /*
     * Hardware test:
     *  In \ Out  "char", "uchar", "short", "ushort", "int", "uint", "long", "ulong", "float"
     * "char",       x       x        x         x       x      x        x       x        x
     * "uchar",      x       x        x         x       x      x        x       x        x
     * "short",      x       x        x         x       x      x        x       x        x
     * "ushort",     x       x        x         x       x      x        x       x        x
     * "int",        x       x        x         x       x      x        x       x        x
     * "uint",       x       x        x         x       x      x        x       x        x
     * "long",       x       x        x         x       x     sat       x       x        x (is saturated to int first)
     * "ulong",      x       x        x         x       x      x        x      sat       x
     * "float"       *       *        *         *       **    ***     error   error    error
     *
     * (* only for "convert" elements 7, 13, ** only for "convert" elements 7, 13, 14, *** only for "cast"/"convert")
     */
}
