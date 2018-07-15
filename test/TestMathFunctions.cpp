/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestMathFunctions.h"
#include "emulation_helper.h"

#include "helper.h"

#include <cmath>

using namespace vc4c::tools;

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

TestMathFunctions::TestMathFunctions(const vc4c::Configuration& config) : TestEmulator(false, config)
{
    /*XXX
    TEST_ADD(TestMathFunctions::testAcos);
    TEST_ADD(TestMathFunctions::testAcosh);
    TEST_ADD(TestMathFunctions::testAcosPi);
    TEST_ADD(TestMathFunctions::testAsin);
    TEST_ADD(TestMathFunctions::testAsinh);
    TEST_ADD(TestMathFunctions::testAsinPi);
    TEST_ADD(TestMathFunctions::testAtan);
    TEST_ADD(TestMathFunctions::testAtan2);
    TEST_ADD(TestMathFunctions::testAtanh);
    TEST_ADD(TestMathFunctions::testAtanPi);
    TEST_ADD(TestMathFunctions::testAtan2Pi);
    TEST_ADD(TestMathFunctions::testCbrt);
    */
    TEST_ADD(TestMathFunctions::testCeil);
    TEST_ADD(TestMathFunctions::testCopysign);
    /*XXX
    TEST_ADD(TestMathFunctions::testCos);
    TEST_ADD(TestMathFunctions::testCosh);
    TEST_ADD(TestMathFunctions::testCosPi);
    TEST_ADD(TestMathFunctions::testErfc);
    TEST_ADD(TestMathFunctions::testErf);
    TEST_ADD(TestMathFunctions::testExp);
    TEST_ADD(TestMathFunctions::testExp2);
    TEST_ADD(TestMathFunctions::testExp10);
    TEST_ADD(TestMathFunctions::testExpm1);
    */
    TEST_ADD(TestMathFunctions::testFabs);
    TEST_ADD(TestMathFunctions::testFdim);
    TEST_ADD(TestMathFunctions::testFloor);
    TEST_ADD(TestMathFunctions::testFma);
    TEST_ADD(TestMathFunctions::testFmax);
    TEST_ADD(TestMathFunctions::testFmin);
    TEST_ADD(TestMathFunctions::testFmod);
    TEST_ADD(TestMathFunctions::testFract);
    TEST_ADD(TestMathFunctions::testFrexp);
    /*XXX
    TEST_ADD(TestMathFunctions::testHypot);
    */
    TEST_ADD(TestMathFunctions::testIlogb);
    TEST_ADD(TestMathFunctions::testLdexp);
    /*XXX
    TEST_ADD(TestMathFunctions::testLgamma);
    TEST_ADD(TestMathFunctions::testLgammaR);
    TEST_ADD(TestMathFunctions::testLog);
    TEST_ADD(TestMathFunctions::testLog2);
    TEST_ADD(TestMathFunctions::testLog10);
    TEST_ADD(TestMathFunctions::testLog1p);
    TEST_ADD(TestMathFunctions::testLogb);
    */
    TEST_ADD(TestMathFunctions::testMad);
    TEST_ADD(TestMathFunctions::testMaxMag);
    TEST_ADD(TestMathFunctions::testMinMag);
    TEST_ADD(TestMathFunctions::testModf);
    TEST_ADD(TestMathFunctions::testNan);
    TEST_ADD(TestMathFunctions::testNextafter);
    /*XXX
    TEST_ADD(TestMathFunctions::testPow);
    TEST_ADD(TestMathFunctions::testPown);
    TEST_ADD(TestMathFunctions::testPowr);
    */
    TEST_ADD(TestMathFunctions::testRemainder);
    TEST_ADD(TestMathFunctions::testRemquo);
    TEST_ADD(TestMathFunctions::testRint);
    /*XXX
    TEST_ADD(TestMathFunctions::testRootn);
    */
    TEST_ADD(TestMathFunctions::testRound);
    TEST_ADD(TestMathFunctions::testRsqrt);
    /*XXX
    TEST_ADD(TestMathFunctions::testSin);
    TEST_ADD(TestMathFunctions::testSinCos);
    TEST_ADD(TestMathFunctions::testSinh);
    TEST_ADD(TestMathFunctions::testSinPi);
    */
    TEST_ADD(TestMathFunctions::testSqrt);
    /*XXX
    TEST_ADD(TestMathFunctions::testTan);
    TEST_ADD(TestMathFunctions::testTanh);
    TEST_ADD(TestMathFunctions::testTanPi);
    TEST_ADD(TestMathFunctions::testTgamma);
    */
    TEST_ADD(TestMathFunctions::testTrunc);
}

void TestMathFunctions::onMismatch(const std::string& expected, const std::string& result)
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

void TestMathFunctions::testCeil()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=ceil", ceilf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testCopysign()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=copysign", copysignf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFabs()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=fabs", fabsf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFdim()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fdim", fdimf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFloor()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=floor", floorf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFma() {}

void TestMathFunctions::testFmax()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fmax", std::max<float>,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFmin()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fmin", std::min<float>,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFmod()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fmod", fmodf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFract() {}

void TestMathFunctions::testFrexp() {}

void TestMathFunctions::testHypot()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=hypot", hypotf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testIlogb() {}

void TestMathFunctions::testLdexp() {}

void TestMathFunctions::testMad() {}

void TestMathFunctions::testMaxMag() {}

void TestMathFunctions::testMinMag() {}

void TestMathFunctions::testModf() {}

void TestMathFunctions::testNan() {}

void TestMathFunctions::testNextafter()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=nextafter", nextafterf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRemainder()
{
    testBinaryFunction(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=remainder", remainderf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRemquo() {}

void TestMathFunctions::testRint()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=rint", rintf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRound()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=round", roundf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRsqrt()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=rsqrt",
        [](float a) -> float { return 1.0f / sqrtf(a); },
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testSqrt()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=sqrt", sqrtf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testTrunc()
{
    testUnaryFunction(config, "-DOUT=float16 -DIN=float16 -DFUNC=trunc", truncf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
/*
void TestMathFunctions::testAsyncCopy()
{
    //TODO move somewhere else
    //TODO this does something strange (e.g. mixing copy via VPM with copy via TMU?)
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_async_copy.cl");

    EmulationData data;
    data.kernelName = "test_aync_copy";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.workGroup.localSizes[0] = 12;
    data.module = std::make_pair("", &buffer);
    // 12 * 16 integer input
    data.parameter.emplace_back(0u, std::vector<uint32_t>{});
    data.parameter[0].second->resize(16 * 12, 0xDEADBEEF);
    // 12 * 16 integer output 1
    data.parameter.emplace_back(0u, std::vector<uint32_t>(16 * 12));
    // 12 * 16 integer output 2
    data.parameter.emplace_back(0u, std::vector<uint32_t>(16 * 12));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(3u, result.results.size());

    const auto &in = *result.results.at(0).second;
    const auto &out0 = *result.results.at(1).second;
    const auto &out1 = *result.results.at(2).second;

    for(std::size_t i = 0; i < in.size(); ++i)
    {
        TEST_ASSERT_EQUALS(in[i], out0[i]);
        TEST_ASSERT_EQUALS(in[i], out1[i]);
    }
}
*/