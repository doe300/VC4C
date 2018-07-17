/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestCommonFunctions.h"
#include "emulation_helper.h"

static const std::string UNARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string BINARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global float16* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

static const std::string TERNARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global float16* in1, __global float16* in2) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid], in2[gid]);
}
)";

TestCommonFunctions::TestCommonFunctions(const vc4c::Configuration& config) : config(config)
{
    TEST_ADD(TestCommonFunctions::testClamp);
    TEST_ADD(TestCommonFunctions::testDegrees);
    TEST_ADD(TestCommonFunctions::testMax);
    TEST_ADD(TestCommonFunctions::testMin);
    TEST_ADD(TestCommonFunctions::testMix);
    TEST_ADD(TestCommonFunctions::testRadians);
    TEST_ADD(TestCommonFunctions::testStep);
    /*XXX
    TEST_ADD(TestCommonFunctions::testSmoothStep);
    */
    TEST_ADD(TestCommonFunctions::testSign);
}

void TestCommonFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

static void testUnaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);

    auto in = generateInput<16 * 12>(true);

    auto out = runEmulation<float, float, 16, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryResults<float, float>(in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

static void testBinaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);

    auto in0 = generateInput<16 * 12>(true);
    auto in1 = generateInput<16 * 12>(true);

    auto out = runEmulation<float, float, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryResults<float, float>(in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

static void testTernaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float, float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, TERNARY_FUNCTION, options);

    auto in0 = generateInput<16 * 12>(true);
    auto in1 = generateInput<16 * 12>(true);
    auto in2 = generateInput<16 * 12>(true);

    auto out = runEmulation<float, float, 16, 12>(code, {in0, in1, in2});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkTernaryResults<float, float>(
        in0, in1, in2, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

void TestCommonFunctions::testClamp()
{
    testTernaryFunction(config, "-DFUNC=clamp",
        [](float a, float b, float c) -> float { return std::min(std::max(a, b), c); },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testDegrees()
{
    testUnaryFunction(config, "-DFUNC=degrees", [](float a) -> float { return a * (180.f / static_cast<float>(M_PI)); },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testMax()
{
    testBinaryFunction(config, "-DFUNC=max", [](float a, float b) -> float { return std::max(a, b); },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testMin()
{
    testBinaryFunction(config, "-DFUNC=min", [](float a, float b) -> float { return std::min(a, b); },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testMix()
{
    testTernaryFunction(config, "-DFUNC=mix", [](float a, float b, float c) -> float { return a + (b - a) * c; },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testRadians()
{
    testUnaryFunction(config, "-DFUNC=radians", [](float a) -> float { return a * (static_cast<float>(M_PI) / 180.f); },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testStep()
{
    testBinaryFunction(config, "-DFUNC=step", [](float a, float b) -> float { return b < a ? 0.0f : 1.0f; },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestCommonFunctions::testSign()
{
    testUnaryFunction(config, "-DFUNC=sign",
        [](float a) -> float { return (std::signbit(a) ? -1.0f : 1.0f) * (std::abs(a) > 0 ? 1.0f : 0.0f); },
        std::bind(&TestCommonFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}