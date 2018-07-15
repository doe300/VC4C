/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestGeometricFunctions.h"
#include "emulation_helper.h"

static const std::string UNARY_GROUPED_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string BINARY_GROUPED_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN0* in0, __global IN1* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

TestGeometricFunctions::TestGeometricFunctions(const vc4c::Configuration& config) : config(config)
{
    TEST_ADD(TestGeometricFunctions::testCross3);
    TEST_ADD(TestGeometricFunctions::testCross4);
    TEST_ADD(TestGeometricFunctions::testDotScalar);
    TEST_ADD(TestGeometricFunctions::testDot2);
    TEST_ADD(TestGeometricFunctions::testDot3);
    TEST_ADD(TestGeometricFunctions::testDot4);
    TEST_ADD(TestGeometricFunctions::testDistanceScalar);
    TEST_ADD(TestGeometricFunctions::testDistance2);
    TEST_ADD(TestGeometricFunctions::testDistance3);
    TEST_ADD(TestGeometricFunctions::testDistance4);
    TEST_ADD(TestGeometricFunctions::testLengthScalar);
    TEST_ADD(TestGeometricFunctions::testLength2);
    TEST_ADD(TestGeometricFunctions::testLength3);
    TEST_ADD(TestGeometricFunctions::testLength4);
    TEST_ADD(TestGeometricFunctions::testNormalizeScalar);
    TEST_ADD(TestGeometricFunctions::testNormalize2);
    TEST_ADD(TestGeometricFunctions::testNormalize3);
    TEST_ADD(TestGeometricFunctions::testNormalize4);
}

void TestGeometricFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <std::size_t GroupSize>
static void testUnaryGroupFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(const std::array<float, GroupSize>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_GROUPED_FUNCTION, options);

    auto in = generateInput<16 * 12>(true);

    auto out = runEmulation<float, float, 16, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryGroupedResults<float, float, 16 * 12, GroupSize>(
        in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <std::size_t GroupSize>
static void testBinaryGroupFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(const std::array<float, GroupSize>&, const std::array<float, GroupSize>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_GROUPED_FUNCTION, options);

    auto in0 = generateInput<16 * 12>(true);
    auto in1 = generateInput<16 * 12>(true);

    auto out = runEmulation<float, float, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryGroupedResults<float, float, 16 * 12, GroupSize>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

// TODO need test for vector to scalar + check whole vectors

template <std::size_t N>
static float checkDot(const std::array<float, N>& in1, const std::array<float, N>& in2)
{
    float sum = 0.0f;
    for(std::size_t i = 0; i < N; ++i)
        sum += in1[i] * in2[i];
    return sum;
}

template <std::size_t N>
static float checkLength(const std::array<float, N>& in)
{
    return std::sqrt(checkDot(in, in));
}

template <std::size_t N>
static float checkDistance(const std::array<float, N>& in1, const std::array<float, N>& in2)
{
    std::array<float, N> diff;
    for(std::size_t i = 0; i < N; ++i)
        diff[i] = in1[i] - in2[i];
    return checkLength(diff);
}

void TestGeometricFunctions::testCross3() {}

void TestGeometricFunctions::testCross4() {}

void TestGeometricFunctions::testDotScalar()
{
    testBinaryGroupFunction<1>(config, "-DOUT=float -DIN0=float -DIN1=float -DFUNC=dot", checkDot<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDot2()
{
    testBinaryGroupFunction<2>(config, "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=dot", checkDot<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDot3()
{
    testBinaryGroupFunction<3>(config, "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=dot", checkDot<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDot4()
{
    testBinaryGroupFunction<4>(config, "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=dot", checkDot<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistanceScalar()
{
    testBinaryGroupFunction<1>(config, "-DOUT=float -DIN0=float -DIN1=float -DFUNC=distance", checkDistance<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistance2()
{
    testBinaryGroupFunction<2>(config, "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=distance", checkDistance<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistance3()
{
    testBinaryGroupFunction<3>(config, "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=distance", checkDistance<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistance4()
{
    testBinaryGroupFunction<4>(config, "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=distance", checkDistance<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLengthScalar()
{
    testUnaryGroupFunction<1>(config, "-DOUT=float -DIN=float -DFUNC=length", checkLength<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLength2()
{
    testUnaryGroupFunction<2>(config, "-DOUT=float -DIN=float2 -DFUNC=length", checkLength<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLength3()
{
    testUnaryGroupFunction<3>(config, "-DOUT=float -DIN=float3 -DFUNC=length", checkLength<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLength4()
{
    testUnaryGroupFunction<4>(config, "-DOUT=float -DIN=float4 -DFUNC=length", checkLength<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testNormalizeScalar() {
}

void TestGeometricFunctions::testNormalize2() {}

void TestGeometricFunctions::testNormalize3() {}

void TestGeometricFunctions::testNormalize4() {}