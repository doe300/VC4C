/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestGeometricFunctions.h"
#include "emulation_helper.h"

//TODO use vload/vstore for memory access to fix alignment/padding

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
    /*XXX comparisons for all 3-element vectors are wrong, since they are stored/loaded as 4-element vectors per standard
    TEST_ADD(TestGeometricFunctions::testCross3);
    */
    TEST_ADD(TestGeometricFunctions::testCross4);
    TEST_ADD(TestGeometricFunctions::testDotScalar);
    TEST_ADD(TestGeometricFunctions::testDot2);
    //TEST_ADD(TestGeometricFunctions::testDot3);
    TEST_ADD(TestGeometricFunctions::testDot4);
    TEST_ADD(TestGeometricFunctions::testDistanceScalar);
    TEST_ADD(TestGeometricFunctions::testDistance2);
    //TEST_ADD(TestGeometricFunctions::testDistance3);
    TEST_ADD(TestGeometricFunctions::testDistance4);
    TEST_ADD(TestGeometricFunctions::testLengthScalar);
    TEST_ADD(TestGeometricFunctions::testLength2);
    //TEST_ADD(TestGeometricFunctions::testLength3);
    TEST_ADD(TestGeometricFunctions::testLength4);
    TEST_ADD(TestGeometricFunctions::testNormalizeScalar);
    TEST_ADD(TestGeometricFunctions::testNormalize2);
    //TEST_ADD(TestGeometricFunctions::testNormalize3);
    TEST_ADD(TestGeometricFunctions::testNormalize4);
}

void TestGeometricFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <std::size_t N>
static std::array<float, N> generateReasonableInput(bool withZero)
{
    // any error occurring with number out of this range are due to fdiv/fmul errors, not the implementation of the
    // functions in test anyway
    return generateInput<N, char>(withZero);
}

template <std::size_t GroupSize>
static void testUnaryReducedFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(const std::array<float, GroupSize>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_GROUPED_FUNCTION, options);

    auto in = generateReasonableInput<GroupSize * 12>(true);

    auto out = runEmulation<float, float, GroupSize, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryReducedResults<float, float, GroupSize * 12, GroupSize>(
        in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <std::size_t GroupSize>
static void testBinaryReducedFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(const std::array<float, GroupSize>&, const std::array<float, GroupSize>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_GROUPED_FUNCTION, options);

    auto in0 = generateReasonableInput<GroupSize * 12>(true);
    auto in1 = generateReasonableInput<GroupSize * 12>(true);

    auto out = runEmulation<float, float, GroupSize, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryReducedResults<float, float, GroupSize * 12, GroupSize>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <std::size_t GroupSize>
static void testUnaryGroupFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<std::array<float, GroupSize>(const std::array<float, GroupSize>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_GROUPED_FUNCTION, options);

    auto in = generateReasonableInput<GroupSize * 12>(true);

    auto out = runEmulation<float, float, GroupSize, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryGroupedResults<float, float, GroupSize * 12, GroupSize>(
        in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <std::size_t GroupSize>
static void testBinaryGroupFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<std::array<float, GroupSize>(
        const std::array<float, GroupSize>&, const std::array<float, GroupSize>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_GROUPED_FUNCTION, options);

    auto in0 = generateReasonableInput<GroupSize * 12>(true);
    auto in1 = generateReasonableInput<GroupSize * 12>(true);

    auto out = runEmulation<float, float, GroupSize, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryGroupedResults<float, float, GroupSize * 12, GroupSize>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

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

template <std::size_t N>
static std::array<float, N> checkNormalize(const std::array<float, N>& in)
{
    std::array<float, N> res;
    auto l = checkLength(in);
    for(std::size_t i = 0; i < N; ++i)
        res[i] = in[i] / l;
    return res;
}

static std::array<float, 3> checkCross3(const std::array<float, 3>& in0, const std::array<float, 3>& in1)
{
    return std::array<float, 3>{
        in0[1] * in1[2] - in0[2] * in1[1], in0[2] * in1[0] - in0[0] * in1[2], in0[0] * in1[1] - in0[1] * in1[0]};
}

static std::array<float, 4> checkCross4(const std::array<float, 4>& in0, const std::array<float, 4>& in1)
{
    return std::array<float, 4>{
        in0[1] * in1[2] - in0[2] * in1[1], in0[2] * in1[0] - in0[0] * in1[2], in0[0] * in1[1] - in0[1] * in1[0], 0.0f};
}

void TestGeometricFunctions::testCross3()
{
    testBinaryGroupFunction<3>(config, "-DOUT=float3 -DIN0=float3 -DIN1=float3 -DFUNC=cross", checkCross3,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testCross4()
{
    testBinaryGroupFunction<4>(config, "-DOUT=float4 -DIN0=float4 -DIN1=float4 -DFUNC=cross", checkCross4,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDotScalar()
{
    testBinaryReducedFunction<1>(config, "-DOUT=float -DIN0=float -DIN1=float -DFUNC=dot", checkDot<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDot2()
{
    testBinaryReducedFunction<2>(config, "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=dot", checkDot<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDot3()
{
    testBinaryReducedFunction<3>(config, "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=dot", checkDot<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDot4()
{
    testBinaryReducedFunction<4>(config, "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=dot", checkDot<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistanceScalar()
{
    testBinaryReducedFunction<1>(config, "-DOUT=float -DIN0=float -DIN1=float -DFUNC=distance", checkDistance<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistance2()
{
    testBinaryReducedFunction<2>(config, "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=distance", checkDistance<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistance3()
{
    testBinaryReducedFunction<3>(config, "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=distance", checkDistance<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testDistance4()
{
    testBinaryReducedFunction<4>(config, "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=distance", checkDistance<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLengthScalar()
{
    testUnaryReducedFunction<1>(config, "-DOUT=float -DIN=float -DFUNC=length", checkLength<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLength2()
{
    testUnaryReducedFunction<2>(config, "-DOUT=float -DIN=float2 -DFUNC=length", checkLength<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLength3()
{
    testUnaryReducedFunction<3>(config, "-DOUT=float -DIN=float3 -DFUNC=length", checkLength<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testLength4()
{
    testUnaryReducedFunction<4>(config, "-DOUT=float -DIN=float4 -DFUNC=length", checkLength<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testNormalizeScalar()
{
    testUnaryGroupFunction<1>(config, "-DOUT=float -DIN=float -DFUNC=normalize", checkNormalize<1>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testNormalize2()
{
    testUnaryGroupFunction<2>(config, "-DOUT=float2 -DIN=float2 -DFUNC=normalize", checkNormalize<2>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testNormalize3()
{
    testUnaryGroupFunction<3>(config, "-DOUT=float3 -DIN=float3 -DFUNC=normalize", checkNormalize<3>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestGeometricFunctions::testNormalize4()
{
    testUnaryGroupFunction<4>(config, "-DOUT=float4 -DIN=float4 -DFUNC=normalize", checkNormalize<4>,
        std::bind(&TestGeometricFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}