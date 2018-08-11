/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestConversionFunctions.h"

#include "emulation_helper.h"


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

static const std::string CAST_OPERATION = R"(
__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = as_##OUT(in[gid]);
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
}

void TestConversionFuntions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <typename I, typename O>
static void testConversionOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<O(I)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, CONVERSION_OPERATION, options);

    auto in = generateInput<I, 16 * 12>(true);

    auto out = runEmulation<I, O, 16, 12>(code, {in});
    checkUnaryResults<O, I, 16 * 12>(
        in, out, op, std::string("convert") + (options.find("SATURATION") != std::string::npos ? "_sat" : ""), onError);
}

template <typename O>
static void testConversionOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<O(float)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, CONVERSION_OPERATION, options);

    // values out of the range of O (the integer type) are implementation-defined
    // but for saturation tests, it is allowed
    std::array<float, 16 * 12> in;
    if(options.find("SATURATION") != std::string::npos)
        in = generateInput<16 * 12>(true);
    else
        in = generateInput<16 * 12, O>(true);

    auto out = runEmulation<float, O, 16, 12>(code, {in});
    checkUnaryResults<O, float, 16 * 12>(
        in, out, op, std::string("convert") + (options.find("SATURATION") != std::string::npos ? "_sat" : ""), onError);
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
    testConversionOperation<int>(config, "-DIN=float16 -DOUT=int16", convert<float, int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToSignedShort()
{
    testConversionOperation<short>(config, "-DIN=float16 -DOUT=short16", convert<float, short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToSignedChar()
{
    testConversionOperation<char>(config, "-DIN=float16 -DOUT=char16", convert<float, char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToUnsignedInt()
{
    testConversionOperation<unsigned int>(config, "-DIN=float16 -DOUT=uint16", convert<float, unsigned int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToUnsignedShort()
{
    testConversionOperation<unsigned short>(config, "-DIN=float16 -DOUT=ushort16", convert<float, unsigned short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testFloatToUnsignedChar()
{
    testConversionOperation<unsigned char>(config, "-DIN=float16 -DOUT=uchar16", convert<float, unsigned char>,
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
    testConversionOperation<int>(config, "-DIN=float16 -DOUT=int16 -DSATURATION=_sat", saturate<int>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToUnsignedInt()
{
    testConversionOperation<unsigned int>(config, "-DIN=float16 -DOUT=uint16 -DSATURATION=_sat", saturate<unsigned int>,
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
    testConversionOperation<short>(config, "-DIN=float16 -DOUT=short16 -DSATURATION=_sat", saturate<short>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToUnsignedShort()
{
    testConversionOperation<unsigned short>(config, "-DIN=float16 -DOUT=ushort16 -DSATURATION=_sat",
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
    testConversionOperation<char>(config, "-DIN=float16 -DOUT=char16 -DSATURATION=_sat", saturate<char>,
        std::bind(&TestConversionFuntions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestConversionFuntions::testSaturateFloatToUnsignedChar()
{
    testConversionOperation<unsigned char>(config, "-DIN=float16 -DOUT=uchar16 -DSATURATION=_sat",
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