/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include <algorithm>
#include <bitset>
#include <limits>

// FIXME some tests fail, some more fail for SPIR-V (probably even more on CI, since older SPIRV-LLVM-Translator
// version)

static const std::string UNARY_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string BINARY_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN0* in0, __global IN1* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

static const std::string TERNARY_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN0* in0, __global IN1* in1, __global IN2* in2) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid], in2[gid]);
}
)";

template <typename R, typename T>
static R bit_cast(T val) noexcept
{
    static_assert(sizeof(R) == sizeof(T), "");
    union
    {
        T in;
        R out;
    } converter;
    converter.in = val;
    return converter.out;
}

template <typename T, bool>
struct Abs
{
};

template <typename T>
struct Abs<T, false>
{
    std::make_unsigned_t<T> operator()(T val) const noexcept
    {
        return static_cast<std::make_unsigned_t<T>>(std::abs(val));
    }

    std::make_unsigned_t<T> operator()(T val0, T val1) const noexcept
    {
        auto nooverflow = operator()(static_cast<T>(val0 - val1));
        auto overflow = static_cast<std::make_unsigned_t<T>>(operator()(val0) + operator()(val1));
        return std::signbit(val0) == std::signbit(val1) ? nooverflow : overflow;
    }
};

template <typename T>
struct Abs<T, true>
{
    T operator()(T val) const noexcept
    {
        return val;
    }

    T operator()(T val0, T val1) const noexcept
    {
        return static_cast<T>(std::max(val0, val1) - std::min(val0, val1));
    }
};

template <typename T>
static T addSat(T in1, T in2)
{
    // TODO does not work for 64-bit types
    return static_cast<T>(std::max(std::min(static_cast<int64_t>(in1) + static_cast<int64_t>(in2),
                                       static_cast<int64_t>(std::numeric_limits<T>::max())),
        static_cast<int64_t>(std::numeric_limits<T>::min())));
}

template <typename T>
static T hAdd(T in1, T in2)
{
    return static_cast<T>((in1 >> 1) + (in2 >> 1) + (in1 & in2 & 1));
}

template <typename T>
static T rHAdd(T in1, T in2)
{
    return static_cast<T>((in1 >> 1) + (in2 >> 1) + ((in1 | in2) & 1));
}

template <typename T>
static T clamp(T val, T min, T max)
{
    return std::min(std::max(val, min), max);
}

template <typename T>
static T clz(T in)
{
    for(int i = sizeof(T) * 8 - 1u; i >= 0; --i)
    {
        if(((static_cast<uint64_t>(in) >> i) & 0x1) == 0x1)
            return static_cast<T>(sizeof(T) * 8u - 1u - static_cast<unsigned>(i));
    }
    return static_cast<T>(sizeof(T) * 8u);
}

template <typename T>
static T mulHi(T in1, T in2)
{
    // TODO does not work for 64-bit types
    auto op1 = bit_cast<uint64_t>(static_cast<int64_t>(in1));
    auto op2 = bit_cast<uint64_t>(static_cast<int64_t>(in2));
    // signed integer overflow is UB, unsigned overflow is not
    return static_cast<T>((op1 * op2) >> (sizeof(T) * 8));
}

static int64_t mulHi(int64_t in1, int64_t in2)
{
    // dummy to not fail compilation for shifting out of type size above
    return 0;
}

static uint64_t mulHi(uint64_t in1, uint64_t in2)
{
    // dummy to not fail compilation for shifting out of type size above
    return 0;
}

template <typename T>
static T madHi(T in1, T in2, T in3)
{
    // TODO does not work for 64-bit types
    auto op1 = bit_cast<uint64_t>(static_cast<int64_t>(mulHi(in1, in2)));
    auto tmp = bit_cast<uint64_t>(static_cast<int64_t>(in3));
    // add with unsigned types to not cause UB on overflow
    return static_cast<T>(bit_cast<int64_t>(op1 + tmp));
}

template <typename T>
static T madSat(T in1, T in2, T in3)
{
    // TODO does not work for 64-bit types
    return static_cast<T>(
        std::min(std::max(static_cast<int64_t>(in1) * static_cast<int64_t>(in2) + static_cast<int64_t>(in3),
                     static_cast<int64_t>(std::numeric_limits<T>::min())),
            static_cast<int64_t>(std::numeric_limits<T>::max())));
}

// taken from: https://stackoverflow.com/questions/25799215/bitwise-rotation-circular-shift
template <typename T>
static T rotate(T v, T shift)
{
    // auto s = shift >= 0 ? shift % (sizeof(T) * 8) : -((-shift) % (sizeof(T) * 8));
    auto s = static_cast<uint64_t>(static_cast<int64_t>(shift) + (int64_t{1} << 32)) % (sizeof(T) * 8);
    auto tmp = bit_cast<std::make_unsigned_t<T>>(v);
    if(s != 0 && s != (sizeof(T) * 8))
        tmp = static_cast<decltype(tmp)>((tmp << s) | (tmp >> ((sizeof(T) * 8) - s)));
    return static_cast<T>(bit_cast<T>(tmp));
}

template <typename T>
static T subSat(T in1, T in2)
{
    // TODO does not work for 64-bit types
    return static_cast<T>(std::max(std::min(static_cast<int64_t>(in1) - static_cast<int64_t>(in2),
                                       static_cast<int64_t>(std::numeric_limits<T>::max())),
        static_cast<int64_t>(std::numeric_limits<T>::min())));
}

template <typename R, typename T, bool>
struct Upsample
{
};

template <typename R, typename T>
struct Upsample<R, T, false>
{
    R operator()(T in1, T in2)
    {
        auto op1 = bit_cast<std::make_unsigned_t<R>>(static_cast<R>((in1)));
        // left shift of negative values is UB, so bit-cast to unsigned type
        return bit_cast<R>(
            static_cast<std::make_unsigned_t<R>>((op1 << (sizeof(T) * 8)) | bit_cast<std::make_unsigned_t<T>>(in2)));
    }
};

template <typename R, typename T>
struct Upsample<R, T, true>
{
    R operator()(T in1, T in2)
    {
        // dummy to not fail compilation for shifting out of type size above
        return 0;
    }
};

template <typename T>
static T popcount(T val)
{
    std::bitset<sizeof(T) * 8> set(static_cast<uint64_t>(val));
    return static_cast<T>(set.count());
}

template <typename T>
static T mul24(T in1, T in2)
{
    auto tmp1 = static_cast<int64_t>(in1) & 0xFFFFFF;
    auto tmp2 = static_cast<int64_t>(in2) & 0xFFFFFF;
    return static_cast<T>(tmp1 * tmp2);
}

template <typename T>
static T mad24(T in1, T in2, T in3)
{
    auto op1 = bit_cast<std::make_unsigned_t<T>>(mul24(in1, in2));
    auto tmp = bit_cast<std::make_unsigned_t<T>>(in3);
    // add with unsigned types to not cause UB on overflow
    return bit_cast<T>(static_cast<std::make_unsigned_t<T>>(op1 + tmp));
}

template <typename T, typename UpsampleType>
static void registerTypeTests(const std::string& typeName, const std::string& upsampleTypeName)
{
    using namespace test_data;
    auto flags = DataFilter::INT_ARITHMETIC;
    auto LONG_UNSUPPORTED = DataFilter::NONE;
    if(sizeof(T) > sizeof(uint32_t))
    {
        flags = flags | DataFilter::USES_LONG;
        LONG_UNSUPPORTED = DataFilter::DISABLED;
    }

    auto unsignedName = typeName[0] == 'u' ? typeName : ("u" + typeName);

    using limits = std::numeric_limits<T>;
    auto values = toRandom<T>(16, true);
    values[0] = 0;
    values[1] = 1;
    if(std::is_signed<T>::value)
        values[2] = -1;
    values[3] = limits::min();
    values[4] = limits::max();

    // put together input vectors by creating a cartesian product of the input set
    std::vector<T> productLeft(values.size() * values.size());
    std::vector<T> productRight(values.size() * values.size());
    for(std::size_t i = 0; i < values.size(); ++i)
    {
        std::fill(&productLeft[i * values.size()], &productLeft[(i + 1) * values.size()], values[i]);
        for(std::size_t k = 0; k < values.size(); k++)
        {
            productRight[i + k * values.size()] = values[i];
        }
    }

    registerTest(TestData{"abs_" + typeName, flags, &UNARY_FUNCTION,
        "-DOUT=" + unsignedName + "16 -DIN=" + typeName + "16 -DFUNC=abs", "test",
        {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(
            0, transform<std::make_unsigned_t<T>, T>(values, Abs<T, std::is_unsigned<T>::value>{}))}});

    // TODO abs_diff_ulong fails on hardware and Emulator
    registerTest(TestData{"abs_diff_" + typeName, flags, &BINARY_FUNCTION,
        "-DOUT=" + unsignedName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=abs_diff", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0,
            transform<std::make_unsigned_t<T>, T, T>(
                productLeft, productRight, Abs<T, std::is_unsigned<T>::value>{}))}});

    registerTest(TestData{"add_sat_" + typeName, flags | LONG_UNSUPPORTED, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=add_sat", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return addSat(in0, in1); }))}});

    registerTest(TestData{"hadd_" + typeName, flags, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=hadd", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return hAdd(in0, in1); }))}});

    registerTest(TestData{"rhadd_" + typeName, flags, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=rhadd", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return rHAdd(in0, in1); }))}});

    // OpenCL 1.2 specification: "Results are undefined if minval > maxval."
    auto minValues = transform<T>(productLeft, productRight, [](T a, T b) -> T { return std::min(a, b); });
    auto maxValues = transform<T>(productLeft, productRight, [](T a, T b) -> T { return std::max(a, b); });
    registerTest(TestData{"clamp_" + typeName, flags, &TERNARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DIN2=" + typeName +
            "16 -DFUNC=clamp",
        "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(minValues)), toBufferParameter(std::vector<T>(maxValues))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<T>(productLeft, minValues, maxValues, [](T in0, T in1, T in2) -> T {
            return clamp(in0, in1, in2);
        }))}});

    registerTest(TestData{"clz_" + typeName, flags | LONG_UNSUPPORTED, &UNARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN=" + typeName + "16 -DFUNC=clz", "test",
        {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<T>(values, [](T val) -> T { return clz(val); }))}});

    if(sizeof(T) <= sizeof(uint32_t))
    {
        // TODO add 64-bit support
        // TODO mad_hi_int fails on emulator and hardware (but OpenCL CTS test passes!)
        registerTest(TestData{"mad_hi_" + typeName, flags, &TERNARY_FUNCTION,
            "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DIN2=" + typeName +
                "16 -DFUNC=mad_hi",
            "test",
            {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)),
                toBufferParameter(std::vector<T>(productLeft)), toBufferParameter(std::vector<T>(productRight)),
                toBufferParameter(std::vector<T>(productRight))},
            calculateDimensions(productLeft.size()),
            {checkParameterEquals(0,
                transform<T>(productLeft, productRight, productRight,
                    [](T in0, T in1, T in2) -> T { return madHi(in0, in1, in2); }))}});
    }

    // TODO mad_sat_(u)int fails on emulator and hardware (but OpenCL CTS test passes!)
    registerTest(TestData{"mad_sat_" + typeName, flags | LONG_UNSUPPORTED, &TERNARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DIN2=" + typeName +
            "16 -DFUNC=mad_sat",
        "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight)), toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<T>(productLeft, productRight, productRight, [](T in0, T in1, T in2) -> T {
            return madSat(in0, in1, in2);
        }))}});

    registerTest(TestData{"max_" + typeName, flags, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=max", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return std::max(in0, in1); }))}});

    registerTest(TestData{"min_" + typeName, flags, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=min", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return std::min(in0, in1); }))}});

    if(sizeof(T) <= sizeof(uint32_t))
    {
        // TODO add 64-bit support
        //TODO mul_hi_int fails on emulator and hardware (but OpenCL CTS test passes!)
        registerTest(TestData{"mul_hi_" + typeName, flags, &BINARY_FUNCTION,
            "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=mul_hi", "test",
            {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)),
                toBufferParameter(std::vector<T>(productLeft)), toBufferParameter(std::vector<T>(productRight))},
            calculateDimensions(productLeft.size()),
            {checkParameterEquals(
                0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return mulHi(in0, in1); }))}});
    }

    registerTest(TestData{"rotate_" + typeName, flags | LONG_UNSUPPORTED, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=rotate", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return rotate(in0, in1); }))}});

    registerTest(TestData{"sub_sat_" + typeName, flags | LONG_UNSUPPORTED, &BINARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=sub_sat", "test",
        {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)), toBufferParameter(std::vector<T>(productLeft)),
            toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return subSat(in0, in1); }))}});

    registerTest(TestData{"upsample_" + typeName, flags | LONG_UNSUPPORTED, &BINARY_FUNCTION,
        "-DOUT=" + upsampleTypeName + "16 -DIN0=" + typeName + "16 -DIN1=" + unsignedName + "16 -DFUNC=upsample",
        "test",
        {toBufferParameter(std::vector<UpsampleType>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<T>(productLeft)), toBufferParameter(std::vector<T>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(
            0, transform<UpsampleType, T>(productLeft, productRight, [](T in0, T in1) -> UpsampleType {
                return Upsample<UpsampleType, T, sizeof(T) >= sizeof(int64_t)>{}(in0, in1);
            }))}});

    registerTest(TestData{"popcount_" + typeName, flags, &UNARY_FUNCTION,
        "-DOUT=" + typeName + "16 -DIN=" + typeName + "16 -DFUNC=popcount", "test",
        {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<T>(values, [](T val) -> T { return popcount(val); }))}});

    if(sizeof(T) == sizeof(int32_t))
    {
        registerTest(TestData{"mul24_" + typeName, flags, &BINARY_FUNCTION,
            "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DFUNC=mul24", "test",
            {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)),
                toBufferParameter(std::vector<T>(productLeft)), toBufferParameter(std::vector<T>(productRight))},
            calculateDimensions(productLeft.size()),
            {checkParameterEquals(
                0, transform<T>(productLeft, productRight, [](T in0, T in1) -> T { return mul24(in0, in1); }))}});

        registerTest(TestData{"mad24_" + typeName, flags, &TERNARY_FUNCTION,
            "-DOUT=" + typeName + "16 -DIN0=" + typeName + "16 -DIN1=" + typeName + "16 -DIN2=" + typeName +
                "16 -DFUNC=mad24",
            "test",
            {toBufferParameter(std::vector<T>(productLeft.size(), 0x42)),
                toBufferParameter(std::vector<T>(productLeft)), toBufferParameter(std::vector<T>(productRight)),
                toBufferParameter(std::vector<T>(productRight))},
            calculateDimensions(productLeft.size()),
            {checkParameterEquals(0,
                transform<T>(productLeft, productRight, productRight,
                    [](T in0, T in1, T in2) -> T { return mad24(in0, in1, in2); }))}});
    }
}

void test_data::registerOpenCLIntegerFunctionTests()
{
    registerTypeTests<int64_t, int64_t>("long", "");
    registerTypeTests<uint64_t, uint64_t>("ulong", "");
    registerTypeTests<int32_t, int64_t>("int", "long");
    registerTypeTests<uint32_t, uint64_t>("uint", "ulong");
    registerTypeTests<int16_t, int32_t>("short", "int");
    registerTypeTests<uint16_t, uint32_t>("ushort", "uint");
    registerTypeTests<int8_t, int16_t>("char", "short");
    registerTypeTests<uint8_t, uint16_t>("uchar", "ushort");
}
