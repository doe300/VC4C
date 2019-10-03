/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestConversionFunctions.h"

#include "emulation_helper.h"

#include <algorithm>
#include <climits>
#include <iomanip>
#include <sstream>

static const std::string CONVERSION_OPERATION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define CONCAT(A, B) CONCAT_(A, B)
#define CONCAT_(A, B) A##B

#ifndef SATURATION
#define SATURATION
#endif

__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = CONCAT(CONCAT(convert_,OUT), SATURATION)(in[gid]);
}
)";

static const std::string REINTERPRET_ADDRESS_OPERATION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define CONCAT(A, B) CONCAT_(A, B)
#define CONCAT_(A, B) A##B

__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = CONCAT(as_,OUT)(in[gid]);
}
)";

static const std::string REINTERPRET_VALUE_OPERATION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define CONCAT(A, B) CONCAT_(A, B)
#define CONCAT_(A, B) A##B

__kernel void test(__global OUT* out, const __global IN* in1, const __global IN* in2) {

  size_t gid = get_global_id(0);
  IN a = in1[gid];
  IN b = in2[gid];
  OUT c = CONCAT(as_,OUT)(a + a) + CONCAT(as_,OUT)(b + b);
  out[gid] = c;
}
)";

TestConversionFuntions::TestConversionFuntions(const vc4c::Configuration& config) : TestEmulator(false, config)
{
    TEST_ADD(TestConversionFuntions::testSignedTruncation);
    TEST_ADD(TestConversionFuntions::testUnsignedTruncation);
    TEST_ADD(TestConversionFuntions::testSignExtension);
    TEST_ADD(TestConversionFuntions::testZeroExtension);
    TEST_ADD(TestConversionFuntions::testSignedIntToFloat);
    TEST_ADD(TestConversionFuntions::testSignedShortToFloat);
    TEST_ADD(TestConversionFuntions::testSignedCharToFloat);
    TEST_ADD(TestConversionFuntions::testUnsignedIntToFloat);
    TEST_ADD(TestConversionFuntions::testUnsignedShortToFloat);
    TEST_ADD(TestConversionFuntions::testUnsignedCharToFloat);
    TEST_ADD(TestConversionFuntions::testFloatToSignedInt);
    TEST_ADD(TestConversionFuntions::testFloatToSignedShort);
    TEST_ADD(TestConversionFuntions::testFloatToSignedChar);
    TEST_ADD(TestConversionFuntions::testFloatToUnsignedInt);
    TEST_ADD(TestConversionFuntions::testFloatToUnsignedShort);
    TEST_ADD(TestConversionFuntions::testFloatToUnsignedChar);

    TEST_ADD(TestConversionFuntions::testSaturateSignedIntToUnsignedInt);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedIntToSignedInt);
    TEST_ADD(TestConversionFuntions::testSaturateFloatToSignedInt);
    TEST_ADD(TestConversionFuntions::testSaturateFloatToUnsignedInt);
    TEST_ADD(TestConversionFuntions::testSaturateSignedIntToSignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateSignedIntToUnsignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedIntToSignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedIntToUnsignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateFloatToSignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateFloatToUnsignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateSignedIntToSignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateSignedIntToUnsignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedIntToSignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedIntToUnsignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateSignedShortToUnsignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedShortToSignedShort);
    TEST_ADD(TestConversionFuntions::testSaturateSignedShortToSignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateSignedShortToUnsignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedShortToSignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedShortToUnsignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateFloatToSignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateFloatToUnsignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateSignedCharToUnsignedChar);
    TEST_ADD(TestConversionFuntions::testSaturateUnsignedCharToSignedChar);

    TEST_ADD(TestConversionFuntions::testVectorBitcastTruncation4To1);
    TEST_ADD(TestConversionFuntions::testVectorBitcastTruncation2To1);
    TEST_ADD(TestConversionFuntions::testVectorBitcastExtension1To4);
    TEST_ADD(TestConversionFuntions::testVectorBitcastExtension1To2);
}

TestConversionFuntions::~TestConversionFuntions() = default;

void TestConversionFuntions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result)
}

template <typename I, typename O>
static void testConversionOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<O(I)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, CONVERSION_OPERATION, options);

    // values out of the range of O (the integer type) are implementation-defined
    // but for saturation tests, it is allowed
    std::array<I, 16 * 12> in;
    if(options.find("SATURATION") != std::string::npos)
        in = generateInput<I, 16 * 12>(true);
    else
        in = generateInput<I, 16 * 12, O>(true);

    auto out = runEmulation<I, O, 16, 12>(code, {in});
    checkUnaryResults<O, I, 16 * 12>(
        in, out, op, std::string("convert") + (options.find("SATURATION") != std::string::npos ? "_sat" : ""), onError);
}

template <typename I, typename O, std::size_t NumIn, std::size_t NumOut, std::size_t Num = std::max(NumIn, NumOut)>
static void testAddressReinterpretation(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::array<I, Num>&, const std::array<O, Num>&,
        const std::function<void(const std::string&, const std::string&)>&)>& check,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, REINTERPRET_ADDRESS_OPERATION, options);

    auto in = generateInput<I, Num>(true);
    auto out = runEmulation<I, O, Num, 1>(code, {in});
    check(in, out, onError);
}

template <typename I, typename O, std::size_t NumIn, std::size_t NumOut, std::size_t Num = std::max(NumIn, NumOut)>
static void testValueReinterpretation(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::array<I, Num>&, const std::array<I, Num>&, const std::array<O, Num>&,
        const std::function<void(const std::string&, const std::string&)>&)>& check,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, REINTERPRET_VALUE_OPERATION, options);

    auto in1 = generateInput<I, Num>(true);
    auto in2 = generateInput<I, Num>(true);
    auto out = runEmulation<I, O, Num, 1>(code, {in1, in2});
    check(in1, in2, out, onError);
}

template <typename T, typename S>
static T convert(S s)
{
    return static_cast<T>(s);
}

template <typename S, typename T>
static T saturate(S s)
{
    return static_cast<T>(std::max(static_cast<int64_t>(std::numeric_limits<T>::min()),
        std::min(static_cast<int64_t>(std::numeric_limits<T>::max()), static_cast<int64_t>(s))));
}

template <typename T>
static T saturate(float s)
{
    // temporary value so we do not overflow long and get into implementation defined behavior
    // the nextafter are there since e.g. (float)LONG_MAX = 2^63 not 2^63-1
    auto tmp = std::max(std::nextafter(static_cast<float>(std::numeric_limits<int64_t>::min()), 0.0f),
        std::min(std::nextafter(static_cast<float>(std::numeric_limits<int64_t>::max()), 0.0f), s));
    return static_cast<T>(std::max(static_cast<int64_t>(std::numeric_limits<T>::min()),
        std::min(static_cast<int64_t>(std::numeric_limits<T>::max()), static_cast<int64_t>(tmp))));
}

void TestConversionFuntions::testSignedTruncation()
{
    testConversionOperation<int, short>(config, "-DIN=int16 -DOUT=short16", convert<short, int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<int, char>(config, "-DIN=int16 -DOUT=char16", convert<char, int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<short, char>(config, "-DIN=short16 -DOUT=char16", convert<char, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testUnsignedTruncation()
{
    testConversionOperation<unsigned int, unsigned short>(config, "-DIN=uint16 -DOUT=ushort16",
        convert<unsigned short, unsigned int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned int, unsigned char>(config, "-DIN=uint16 -DOUT=uchar16",
        convert<unsigned char, unsigned int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned short, unsigned char>(config, "-DIN=ushort16 -DOUT=uchar16",
        convert<unsigned char, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSignExtension()
{
    testConversionOperation<short, int>(config, "-DIN=short16 -DOUT=int16", convert<int, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<char, int>(config, "-DIN=char16 -DOUT=int16", convert<int, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<char, short>(config, "-DIN=char16 -DOUT=short16", convert<short, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testZeroExtension()
{
    testConversionOperation<unsigned short, unsigned int>(config, "-DIN=ushort16 -DOUT=uint16",
        convert<unsigned int, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned char, unsigned int>(config, "-DIN=uchar16 -DOUT=uint16",
        convert<unsigned int, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned char, unsigned short>(config, "-DIN=uchar16 -DOUT=ushort16",
        convert<unsigned short, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSignedIntToFloat()
{
    testConversionOperation<int, float>(config, "-DIN=int16 -DOUT=float16", convert<int, float>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSignedShortToFloat()
{
    testConversionOperation<short, float>(config, "-DIN=short16 -DOUT=float16", convert<short, float>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSignedCharToFloat()
{
    testConversionOperation<char, float>(config, "-DIN=char16 -DOUT=float16", convert<char, float>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testUnsignedIntToFloat()
{
    testConversionOperation<unsigned int, float>(config, "-DIN=uint16 -DOUT=float16", convert<unsigned int, float>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testUnsignedShortToFloat()
{
    testConversionOperation<unsigned short, float>(config, "-DIN=ushort16 -DOUT=float16",
        convert<unsigned short, float>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testUnsignedCharToFloat()
{
    testConversionOperation<unsigned char, float>(config, "-DIN=uchar16 -DOUT=float16", convert<unsigned char, float>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToSignedInt()
{
    testConversionOperation<float, int>(config, "-DIN=float16 -DOUT=int16", convert<float, int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToSignedShort()
{
    testConversionOperation<float, short>(config, "-DIN=float16 -DOUT=short16", convert<float, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToSignedChar()
{
    testConversionOperation<float, char>(config, "-DIN=float16 -DOUT=char16", convert<float, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToUnsignedInt()
{
    testConversionOperation<float, unsigned int>(config, "-DIN=float16 -DOUT=uint16", convert<float, unsigned int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToUnsignedShort()
{
    testConversionOperation<float, unsigned short>(config, "-DIN=float16 -DOUT=ushort16",
        convert<float, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToUnsignedChar()
{
    testConversionOperation<float, unsigned char>(config, "-DIN=float16 -DOUT=uchar16", convert<float, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedIntToUnsignedInt()
{
    testConversionOperation<int, unsigned int>(config, "-DIN=int16 -DOUT=uint16 -DSATURATION=_sat",
        saturate<int, unsigned int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedIntToSignedInt()
{
    testConversionOperation<unsigned int, int>(config, "-DIN=uint16 -DOUT=int16 -DSATURATION=_sat",
        saturate<unsigned int, int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToSignedInt()
{
    testConversionOperation<float, int>(config, "-DIN=float16 -DOUT=int16 -DSATURATION=_sat", saturate<int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToUnsignedInt()
{
    testConversionOperation<float, unsigned int>(config, "-DIN=float16 -DOUT=uint16 -DSATURATION=_sat",
        saturate<unsigned int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedIntToSignedShort()
{
    testConversionOperation<int, short>(config, "-DIN=int16 -DOUT=short16 -DSATURATION=_sat", saturate<int, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedIntToUnsignedShort()
{
    testConversionOperation<int, unsigned short>(config, "-DIN=int16 -DOUT=ushort16 -DSATURATION=_sat",
        saturate<int, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedIntToSignedShort()
{
    testConversionOperation<unsigned int, short>(config, "-DIN=uint16 -DOUT=short16 -DSATURATION=_sat",
        saturate<unsigned int, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedIntToUnsignedShort()
{
    testConversionOperation<unsigned int, unsigned short>(config, "-DIN=uint16 -DOUT=ushort16 -DSATURATION=_sat",
        saturate<unsigned int, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestConversionFuntions::testSaturateFloatToSignedShort()
{
    testConversionOperation<float, short>(config, "-DIN=float16 -DOUT=short16 -DSATURATION=_sat", saturate<short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToUnsignedShort()
{
    testConversionOperation<float, unsigned short>(config, "-DIN=float16 -DOUT=ushort16 -DSATURATION=_sat",
        saturate<unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedIntToSignedChar()
{
    testConversionOperation<int, char>(config, "-DIN=int16 -DOUT=char16 -DSATURATION=_sat", saturate<int, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedIntToUnsignedChar()
{
    testConversionOperation<int, unsigned char>(config, "-DIN=int16 -DOUT=uchar16 -DSATURATION=_sat",
        saturate<int, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedIntToSignedChar()
{
    testConversionOperation<unsigned int, char>(config, "-DIN=uint16 -DOUT=char16 -DSATURATION=_sat",
        saturate<unsigned int, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedIntToUnsignedChar()
{
    testConversionOperation<unsigned int, unsigned char>(config, "-DIN=uint16 -DOUT=uchar16 -DSATURATION=_sat",
        saturate<unsigned int, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedShortToUnsignedShort()
{
    testConversionOperation<short, unsigned short>(config, "-DIN=short16 -DOUT=ushort16 -DSATURATION=_sat",
        saturate<short, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedShortToSignedShort()
{
    testConversionOperation<short, char>(config, "-DIN=short16 -DOUT=char16 -DSATURATION=_sat", saturate<short, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedShortToSignedChar()
{
    testConversionOperation<unsigned short, short>(config, "-DIN=ushort16 -DOUT=short16 -DSATURATION=_sat",
        saturate<unsigned short, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedShortToUnsignedChar()
{
    testConversionOperation<short, unsigned char>(config, "-DIN=short16 -DOUT=uchar16 -DSATURATION=_sat",
        saturate<short, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedShortToSignedChar()
{
    testConversionOperation<unsigned short, char>(config, "-DIN=ushort16 -DOUT=char16 -DSATURATION=_sat",
        saturate<unsigned short, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedShortToUnsignedChar()
{
    testConversionOperation<unsigned short, unsigned char>(config, "-DIN=ushort16 -DOUT=uchar16 -DSATURATION=_sat",
        saturate<unsigned short, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToSignedChar()
{
    testConversionOperation<float, char>(config, "-DIN=float16 -DOUT=char16 -DSATURATION=_sat", saturate<char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToUnsignedChar()
{
    testConversionOperation<float, unsigned char>(config, "-DIN=float16 -DOUT=uchar16 -DSATURATION=_sat",
        saturate<unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateSignedCharToUnsignedChar()
{
    testConversionOperation<char, unsigned char>(config, "-DIN=char16 -DOUT=uchar16 -DSATURATION=_sat",
        saturate<char, unsigned char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateUnsignedCharToSignedChar()
{
    testConversionOperation<unsigned char, char>(config, "-DIN=uchar16 -DOUT=char16 -DSATURATION=_sat",
        saturate<unsigned char, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

// NOTE: OpenCL 1.2, 6.2.4.2 Reinterpreting Types Using as_type() and as_typen():
// Actual behavior (e.g. endianess) is implementation defined, we use little endian

template <std::size_t Factor, typename T, std::size_t Num>
struct Reinterpretation
{
};

template <typename T>
static constexpr uint32_t TYPE_SIZE = sizeof(T) * CHAR_BIT;
template <typename T>
static constexpr uint32_t TYPE_MASK = static_cast<uint32_t>((uint64_t{1} << TYPE_SIZE<T>) -1u);

static_assert(TYPE_MASK<uint8_t> == 0xFFu, "");
static_assert(TYPE_MASK<uint16_t> == 0xFFFFu, "");
static_assert(TYPE_MASK<uint32_t> == 0xFFFFFFFFu, "");

template <typename A, typename B>
struct LargerType
{
    using type = typename std::conditional<sizeof(A) >= sizeof(B), A, B>::type;
};

static_assert(std::is_same<uint32_t, typename LargerType<uint16_t, uint32_t>::type>::value, "");
static_assert(std::is_same<uint16_t, typename LargerType<uint16_t, uint16_t>::type>::value, "");
static_assert(std::is_same<uint16_t, typename LargerType<uint16_t, uint8_t>::type>::value, "");

template <typename T, std::size_t Num>
struct Reinterpretation<4, T, Num>
{
    uint32_t operator()(const std::array<T, Num>& in, unsigned i) const noexcept
    {
        return (static_cast<uint32_t>(in[i * 4]) << (sizeof(T) * 0)) |
            (static_cast<uint32_t>(in[i * 4 + 1]) << (sizeof(T) * 8)) |
            (static_cast<uint32_t>(in[i * 4 + 2]) << (sizeof(T) * 16)) |
            (static_cast<uint32_t>(in[i * 4 + 3]) << (sizeof(T) * 24));
    }

    uint32_t operator()(const std::array<T, Num>& in, unsigned i, unsigned j) const noexcept
    {
        auto tmp0 = (static_cast<uint32_t>(in[i * 4]) + static_cast<uint32_t>(in[j * 4])) & TYPE_MASK<T>;
        auto tmp1 = (static_cast<uint32_t>(in[i * 4 + 1]) + static_cast<uint32_t>(in[j * 4 + 1])) & TYPE_MASK<T>;
        auto tmp2 = (static_cast<uint32_t>(in[i * 4 + 2]) + static_cast<uint32_t>(in[j * 4 + 2])) & TYPE_MASK<T>;
        auto tmp3 = (static_cast<uint32_t>(in[i * 4 + 3]) + static_cast<uint32_t>(in[j * 4 + 3])) & TYPE_MASK<T>;

        return (tmp0 << (sizeof(T) * 0)) | (tmp1 << (sizeof(T) * 8)) | (tmp2 << (sizeof(T) * 16)) |
            (tmp3 << (sizeof(T) * 24));
    }

    uint32_t add(uint32_t a, uint32_t b) const noexcept
    {
        auto SIZE = TYPE_SIZE<T>;
        auto tmp0 = (a + b) & TYPE_MASK<T>;
        auto tmp1 = ((a >> SIZE) + (b >> SIZE)) & TYPE_MASK<T>;
        auto tmp2 = ((a >> 2 * SIZE) + (b >> 2 * SIZE)) & TYPE_MASK<T>;
        auto tmp3 = ((a >> 3 * SIZE) + (b >> 3 * SIZE)) & TYPE_MASK<T>;

        return (tmp0 << (SIZE * 0)) | (tmp1 << (SIZE * 1)) | (tmp2 << (SIZE * 2)) | (tmp3 << (SIZE * 3));
    }

    std::string print(const std::array<T, Num>& in, unsigned i) const
    {
        std::stringstream ss;
        ss << std::hex << '{' << static_cast<uint32_t>(in[i * 4]) << ", " << static_cast<uint32_t>(in[i * 4 + 1])
           << ", " << static_cast<uint32_t>(in[i * 4 + 2]) << ", " << static_cast<uint32_t>(in[i * 4 + 3])
           << "} = " << operator()(in, i);
        return ss.str();
    }

    std::string print(const std::array<T, Num>& in, unsigned i, unsigned j) const
    {
        std::stringstream ss;
        ss << std::hex << '{' << static_cast<uint32_t>(in[i * 4]) << ", " << static_cast<uint32_t>(in[i * 4 + 1])
           << ", " << static_cast<uint32_t>(in[i * 4 + 2]) << ", " << static_cast<uint32_t>(in[i * 4 + 3]) << "} + {"
           << static_cast<uint32_t>(in[j * 4]) << ", " << static_cast<uint32_t>(in[j * 4 + 1]) << ", "
           << static_cast<uint32_t>(in[j * 4 + 2]) << ", " << static_cast<uint32_t>(in[j * 4 + 3])
           << "} = " << operator()(in, i, j);
        return ss.str();
    }
};

template <typename T, std::size_t Num>
struct Reinterpretation<2, T, Num>
{
    uint32_t operator()(const std::array<T, Num>& in, unsigned i) const noexcept
    {
        return (static_cast<uint32_t>(in[i * 2]) << (sizeof(T) * 0)) |
            (static_cast<uint32_t>(in[i * 2 + 1]) << (sizeof(T) * 8));
    }

    uint32_t operator()(const std::array<T, Num>& in, unsigned i, unsigned j) const noexcept
    {
        auto tmp0 = (static_cast<uint32_t>(in[i * 2]) + static_cast<uint32_t>(in[j * 2])) & TYPE_MASK<T>;
        auto tmp1 = (static_cast<uint32_t>(in[i * 2 + 1]) + static_cast<uint32_t>(in[j * 2 + 1])) & TYPE_MASK<T>;
        return (tmp0 << (sizeof(T) * 0)) | (tmp1 << (sizeof(T) * 8));
    }

    uint32_t add(uint32_t a, uint32_t b) const noexcept
    {
        auto SIZE = TYPE_SIZE<T>;
        auto tmp0 = (a + b) & TYPE_MASK<T>;
        auto tmp1 = ((a >> SIZE) + (b >> SIZE)) & TYPE_MASK<T>;

        return (tmp0 << (SIZE * 0)) | (tmp1 << (SIZE * 1));
    }

    std::string print(const std::array<T, Num>& in, unsigned i) const
    {
        std::stringstream ss;
        ss << std::hex << '{' << static_cast<uint32_t>(in[i * 2]) << ", " << static_cast<uint32_t>(in[i * 2 + 1])
           << "} = " << operator()(in, i);
        return ss.str();
    }

    std::string print(const std::array<T, Num>& in, unsigned i, unsigned j) const
    {
        std::stringstream ss;
        ss << std::hex << '{' << static_cast<uint32_t>(in[i * 2]) << ", " << static_cast<uint32_t>(in[i * 2 + 1])
           << "} + {" << static_cast<uint32_t>(in[j * 2]) << ", " << static_cast<uint32_t>(in[j * 2 + 1])
           << "} = " << operator()(in, i, j);
        return ss.str();
    }
};

template <typename T, std::size_t Num>
struct Reinterpretation<1, T, Num>
{
    uint32_t operator()(const std::array<T, Num>& in, unsigned i) const noexcept
    {
        return in[i];
    }

    uint32_t operator()(const std::array<T, Num>& in, unsigned i, unsigned j) const noexcept
    {
        return (in[i] + in[j]) & TYPE_MASK<T>;
    }

    uint32_t add(uint32_t a, uint32_t b) const noexcept
    {
        return (a + b) & TYPE_MASK<T>;
    }

    std::string print(const std::array<T, Num>& in, unsigned i) const
    {
        std::stringstream ss;
        ss << std::hex << static_cast<uint32_t>(in[i]);
        return ss.str();
    }

    std::string print(const std::array<T, Num>& in, unsigned i, unsigned j) const
    {
        std::stringstream ss;
        ss << std::hex << static_cast<uint32_t>(in[i]) << " + " << static_cast<uint32_t>(in[j]);
        return ss.str();
    }
};

template <typename T, std::size_t Num>
struct Reinterpretation<0, T, Num> : public Reinterpretation<1, T, Num>
{
};

template <typename I, typename O, std::size_t Num,
    std::size_t Factor = std::max(sizeof(I) / sizeof(O), sizeof(O) / sizeof(I))>
static void checkReinterpretation(const std::array<I, Num>& in, const std::array<O, Num>& out,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Reinterpretation<sizeof(O) / sizeof(I), I, Num> inputConverter{};
    Reinterpretation<sizeof(I) / sizeof(O), O, Num> outputConverter{};
    for(unsigned i = 0; i < Num / Factor; ++i)
    {
        auto inSample = inputConverter(in, i);
        auto outSample = outputConverter(out, i);
        if(inSample != outSample)
            onError(inputConverter.print(in, i) + " for sample " + std::to_string(i), outputConverter.print(out, i));
    }
}

template <typename I, typename O, std::size_t Num,
    std::size_t Factor = std::max(sizeof(I) / sizeof(O), sizeof(O) / sizeof(I))>
static void checkReinterpretation2(const std::array<I, Num>& in1, const std::array<I, Num>& in2,
    const std::array<O, Num>& out, const std::function<void(const std::string&, const std::string&)>& onError)
{
    Reinterpretation<sizeof(O) / sizeof(I), I, Num> inputConverter{};
    Reinterpretation<sizeof(I) / sizeof(O), O, Num> outputConverter{};
    for(unsigned i = 0; i < Num / Factor; ++i)
    {
        auto inSample1 = inputConverter(in1, i, i);
        auto inSample2 = inputConverter(in2, i, i);
        auto outSample = outputConverter(out, i);
        if(outputConverter.add(inSample1, inSample2) != outSample)
            onError(inputConverter.print(in1, i, i) + " + " + inputConverter.print(in2, i, i) + " for sample " +
                    std::to_string(i),
                outputConverter.print(out, i));
    }
}

void TestConversionFuntions::testVectorBitcastTruncation4To1()
{
    testAddressReinterpretation<uint8_t, uint32_t, 16, 4, 16>(config, "-DIN=uchar16 -DOUT=uint4",
        checkReinterpretation<uint8_t, uint32_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint8_t, uint32_t, 8, 2, 8>(config, "-DIN=uchar8 -DOUT=uint2",
        checkReinterpretation<uint8_t, uint32_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint8_t, uint32_t, 4, 1, 4>(config, "-DIN=uchar4 -DOUT=uint",
        checkReinterpretation<uint8_t, uint32_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testValueReinterpretation<uint8_t, uint32_t, 16, 4, 16>(config, "-DIN=uchar16 -DOUT=uint4",
        checkReinterpretation2<uint8_t, uint32_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint8_t, uint32_t, 8, 2, 8>(config, "-DIN=uchar8 -DOUT=uint2",
        checkReinterpretation2<uint8_t, uint32_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint8_t, uint32_t, 4, 1, 4>(config, "-DIN=uchar4 -DOUT=uint",
        checkReinterpretation2<uint8_t, uint32_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testVectorBitcastTruncation2To1()
{
    testAddressReinterpretation<uint16_t, uint32_t, 16, 8, 16>(config, "-DIN=ushort16 -DOUT=uint8",
        checkReinterpretation<uint16_t, uint32_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint16_t, uint32_t, 8, 4, 8>(config, "-DIN=ushort8 -DOUT=uint4",
        checkReinterpretation<uint16_t, uint32_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint16_t, uint32_t, 4, 2, 4>(config, "-DIN=ushort4 -DOUT=uint2",
        checkReinterpretation<uint16_t, uint32_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint16_t, uint32_t, 2, 1, 2>(config, "-DIN=ushort2 -DOUT=uint",
        checkReinterpretation<uint16_t, uint32_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testAddressReinterpretation<uint8_t, uint16_t, 16, 8, 16>(config, "-DIN=uchar16 -DOUT=ushort8",
        checkReinterpretation<uint8_t, uint16_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint8_t, uint16_t, 8, 4, 8>(config, "-DIN=uchar8 -DOUT=ushort4",
        checkReinterpretation<uint8_t, uint16_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint8_t, uint16_t, 4, 2, 4>(config, "-DIN=uchar4 -DOUT=ushort2",
        checkReinterpretation<uint8_t, uint16_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint8_t, uint16_t, 2, 1, 2>(config, "-DIN=uchar2 -DOUT=ushort",
        checkReinterpretation<uint8_t, uint16_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testValueReinterpretation<uint16_t, uint32_t, 16, 8, 16>(config, "-DIN=ushort16 -DOUT=uint8",
        checkReinterpretation2<uint16_t, uint32_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint16_t, uint32_t, 8, 4, 8>(config, "-DIN=ushort8 -DOUT=uint4",
        checkReinterpretation2<uint16_t, uint32_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint16_t, uint32_t, 4, 2, 4>(config, "-DIN=ushort4 -DOUT=uint2",
        checkReinterpretation2<uint16_t, uint32_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint16_t, uint32_t, 2, 1, 2>(config, "-DIN=ushort2 -DOUT=uint",
        checkReinterpretation2<uint16_t, uint32_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testValueReinterpretation<uint8_t, uint16_t, 16, 8, 16>(config, "-DIN=uchar16 -DOUT=ushort8",
        checkReinterpretation2<uint8_t, uint16_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint8_t, uint16_t, 8, 4, 8>(config, "-DIN=uchar8 -DOUT=ushort4",
        checkReinterpretation2<uint8_t, uint16_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint8_t, uint16_t, 4, 2, 4>(config, "-DIN=uchar4 -DOUT=ushort2",
        checkReinterpretation2<uint8_t, uint16_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint8_t, uint16_t, 2, 1, 2>(config, "-DIN=uchar2 -DOUT=ushort",
        checkReinterpretation2<uint8_t, uint16_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testVectorBitcastExtension1To4()
{
    testAddressReinterpretation<uint32_t, uint8_t, 4, 16, 16>(config, "-DIN=uint4 -DOUT=uchar16",
        checkReinterpretation<uint32_t, uint8_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint32_t, uint8_t, 2, 8, 8>(config, "-DIN=uint2 -DOUT=uchar8",
        checkReinterpretation<uint32_t, uint8_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint32_t, uint8_t, 1, 4, 4>(config, "-DIN=uint -DOUT=uchar4",
        checkReinterpretation<uint32_t, uint8_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testValueReinterpretation<uint32_t, uint8_t, 4, 16, 16>(config, "-DIN=uint4 -DOUT=uchar16",
        checkReinterpretation2<uint32_t, uint8_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint32_t, uint8_t, 2, 8, 8>(config, "-DIN=uint2 -DOUT=uchar8",
        checkReinterpretation2<uint32_t, uint8_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint32_t, uint8_t, 1, 4, 4>(config, "-DIN=uint -DOUT=uchar4",
        checkReinterpretation2<uint32_t, uint8_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testVectorBitcastExtension1To2()
{
    testAddressReinterpretation<uint32_t, uint16_t, 8, 16, 16>(config, "-DIN=uint8 -DOUT=ushort16",
        checkReinterpretation<uint32_t, uint16_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint32_t, uint16_t, 4, 8, 8>(config, "-DIN=uint4 -DOUT=ushort8",
        checkReinterpretation<uint32_t, uint16_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint32_t, uint16_t, 2, 4, 4>(config, "-DIN=uint2 -DOUT=ushort4",
        checkReinterpretation<uint32_t, uint16_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint32_t, uint16_t, 1, 2, 2>(config, "-DIN=uint -DOUT=ushort2",
        checkReinterpretation<uint32_t, uint16_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testAddressReinterpretation<uint16_t, uint8_t, 8, 16, 16>(config, "-DIN=ushort8 -DOUT=uchar16",
        checkReinterpretation<uint16_t, uint8_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint16_t, uint8_t, 4, 8, 8>(config, "-DIN=ushort4 -DOUT=uchar8",
        checkReinterpretation<uint16_t, uint8_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint16_t, uint8_t, 2, 4, 4>(config, "-DIN=ushort2 -DOUT=uchar4",
        checkReinterpretation<uint16_t, uint8_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testAddressReinterpretation<uint16_t, uint8_t, 1, 2, 2>(config, "-DIN=ushort -DOUT=uchar2",
        checkReinterpretation<uint16_t, uint8_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testValueReinterpretation<uint32_t, uint16_t, 8, 16, 16>(config, "-DIN=uint8 -DOUT=ushort16",
        checkReinterpretation2<uint32_t, uint16_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint32_t, uint16_t, 4, 8, 8>(config, "-DIN=uint4 -DOUT=ushort8",
        checkReinterpretation2<uint32_t, uint16_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint32_t, uint16_t, 2, 4, 4>(config, "-DIN=uint2 -DOUT=ushort4",
        checkReinterpretation2<uint32_t, uint16_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint32_t, uint16_t, 1, 2, 2>(config, "-DIN=uint -DOUT=ushort2",
        checkReinterpretation2<uint32_t, uint16_t, 2>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    testValueReinterpretation<uint16_t, uint8_t, 8, 16, 16>(config, "-DIN=ushort8 -DOUT=uchar16",
        checkReinterpretation2<uint16_t, uint8_t, 16>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint16_t, uint8_t, 4, 8, 8>(config, "-DIN=ushort4 -DOUT=uchar8",
        checkReinterpretation2<uint16_t, uint8_t, 8>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testValueReinterpretation<uint16_t, uint8_t, 2, 4, 4>(config, "-DIN=ushort2 -DOUT=uchar4",
        checkReinterpretation2<uint16_t, uint8_t, 4>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    // FIXME, compilation error, but only for this one!
    // testValueReinterpretation<uint16_t, uint8_t, 1, 2, 2>(config, "-DIN=ushort -DOUT=uchar2",
    //     checkReinterpretation2<uint16_t, uint8_t, 2>,
    //     std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
