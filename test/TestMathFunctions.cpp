/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestMathFunctions.h"
#include "emulation_helper.h"

#include "helper.h"
#include "intrinsics/Operators.h"

#include <algorithm>
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

TestMathFunctions::TestMathFunctions(const vc4c::Configuration& config) : TestCompilationHelper(config)
{
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
    TEST_ADD(TestMathFunctions::testCeil);
    TEST_ADD(TestMathFunctions::testCopysign);
    TEST_ADD(TestMathFunctions::testCos);
    TEST_ADD(TestMathFunctions::testCosh);
    TEST_ADD(TestMathFunctions::testCosPi);
    TEST_ADD(TestMathFunctions::testErfc);
    TEST_ADD(TestMathFunctions::testErf);
    TEST_ADD(TestMathFunctions::testExp);
    TEST_ADD(TestMathFunctions::testExp2);
    TEST_ADD(TestMathFunctions::testExp10);
    TEST_ADD(TestMathFunctions::testExpm1);
    TEST_ADD(TestMathFunctions::testFabs);
    TEST_ADD(TestMathFunctions::testFdim);
    TEST_ADD(TestMathFunctions::testFloor);
    TEST_ADD(TestMathFunctions::testFma);
    TEST_ADD(TestMathFunctions::testFmax);
    TEST_ADD(TestMathFunctions::testFmin);
    TEST_ADD(TestMathFunctions::testFmod);
    /*XXX
    TEST_ADD(TestMathFunctions::testFract);
    TEST_ADD(TestMathFunctions::testFrexp);
    */
    TEST_ADD(TestMathFunctions::testHypot);
    TEST_ADD(TestMathFunctions::testIlogb);
    TEST_ADD(TestMathFunctions::testLdexp);
    TEST_ADD(TestMathFunctions::testLgamma);
    /*XXX
    TEST_ADD(TestMathFunctions::testLgammaR);
    */
    TEST_ADD(TestMathFunctions::testLog);
    TEST_ADD(TestMathFunctions::testLog2);
    TEST_ADD(TestMathFunctions::testLog10);
    TEST_ADD(TestMathFunctions::testLog1p);
    TEST_ADD(TestMathFunctions::testLogb);
    TEST_ADD(TestMathFunctions::testMad);
    TEST_ADD(TestMathFunctions::testMaxMag);
    TEST_ADD(TestMathFunctions::testMinMag);
    TEST_ADD(TestMathFunctions::testModf);
    TEST_ADD(TestMathFunctions::testNan);
    TEST_ADD(TestMathFunctions::testNextafter);
    TEST_ADD(TestMathFunctions::testPow);
    TEST_ADD(TestMathFunctions::testPown);
    /*XXX
    TEST_ADD(TestMathFunctions::testPowr);
    */
    TEST_ADD(TestMathFunctions::testRemainder);
    /*XXX
    TEST_ADD(TestMathFunctions::testRemquo);
    */
    TEST_ADD(TestMathFunctions::testRint);
    TEST_ADD(TestMathFunctions::testRootn);
    TEST_ADD(TestMathFunctions::testRound);
    TEST_ADD(TestMathFunctions::testRsqrt);
    TEST_ADD(TestMathFunctions::testSin);
    /*XXX
    TEST_ADD(TestMathFunctions::testSinCos);
    */
    TEST_ADD(TestMathFunctions::testSinh);
    TEST_ADD(TestMathFunctions::testSinPi);
    TEST_ADD(TestMathFunctions::testSqrt);
    TEST_ADD(TestMathFunctions::testTan);
    TEST_ADD(TestMathFunctions::testTanh);
    TEST_ADD(TestMathFunctions::testTanPi);
    TEST_ADD(TestMathFunctions::testTgamma);
    TEST_ADD(TestMathFunctions::testTrunc);
}

void TestMathFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <std::size_t ULP, typename Distribution = UniformDistribution<double>, typename Out = float,
    typename Comparison = CompareULP<ULP>>
static void testUnaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float)>& op, const std::function<void(const std::string&, const std::string&)>& onError,
    float min = std::numeric_limits<float>::lowest(), float max = std::numeric_limits<float>::max())
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);

    auto in = generateInput<float, 16 * 12, float, Distribution>(true, min, max);

    auto out = runEmulation<float, Out, 16, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryResults<Out, float, 16 * 12, Comparison>(
        in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <std::size_t ULP, typename Distribution = UniformDistribution<double>, typename SecondType = float,
    typename SecondDistribution = Distribution>
static void testBinaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError,
    float min = std::numeric_limits<float>::lowest(), float max = std::numeric_limits<float>::max())
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);

    auto in0 = generateInput<float, 16 * 12, float, Distribution>(true, min, max);
    auto in1 = generateInput<SecondType, 16 * 12, float, SecondDistribution>(true, min, max);
    // work-around to allow integer second arguments
    std::array<float, 16 * 12> tmpIn1;
    std::memcpy(tmpIn1.data(), in1.data(), 16 * 12 * sizeof(float));

    auto out = runEmulation<float, float, 16, 12>(code, {in0, tmpIn1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryResults<float, float, 16 * 12, CompareULP<ULP>, SecondType>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <std::size_t ULP, typename Distribution = UniformDistribution<double>, typename SecondType = float,
    typename SecondDistribution = Distribution, typename ThirdType = float, typename ThirdDistribution = Distribution>
static void testTernaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float, float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError,
    float min = std::numeric_limits<float>::lowest(), float max = std::numeric_limits<float>::max())
{
    std::stringstream code;
    compileBuffer(config, code, TERNARY_FUNCTION, options);

    auto in0 = generateInput<float, 16 * 12, float, Distribution>(true, min, max);
    auto in1 = generateInput<SecondType, 16 * 12, float, SecondDistribution>(true, min, max);
    auto in2 = generateInput<ThirdType, 16 * 12, float, ThirdDistribution>(true, min, max);
    // work-around to allow integer second/third arguments
    std::array<float, 16 * 12> tmpIn1;
    std::memcpy(tmpIn1.data(), in1.data(), 16 * 12 * sizeof(float));
    std::array<float, 16 * 12> tmpIn2;
    std::memcpy(tmpIn2.data(), in2.data(), 16 * 12 * sizeof(float));

    auto out = runEmulation<float, float, 16, 12>(code, {in0, tmpIn1, tmpIn2});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkTernaryResults<float, float, 16 * 12, CompareULP<ULP>, SecondType, ThirdType>(
        in0, in1, in2, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

struct FlushAndRoundUnary : private vc4c::FlushDenormsAndRoundToZero<float, float, std::function<float(float, float)>>
{
    using Base = vc4c::FlushDenormsAndRoundToZero<float, float, std::function<float(float, float)>>;
    explicit FlushAndRoundUnary(float (*func)(float)) : Base{[=](float a, float b) -> float { return func(a); }} {}

    float operator()(float arg) const
    {
        return Base::operator()(arg, arg);
    }
};

using FlushAndRoundBinary = vc4c::FlushDenormsAndRoundToZero<float, float, std::function<float(float, float)>>;

void TestMathFunctions::testAcos()
{
    testUnaryFunction<4, NormalDistribution<0, 1>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=acos",
        FlushAndRoundUnary{acosf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -1, 1);
}

void TestMathFunctions::testAcosh()
{
    testUnaryFunction<4, NormalDistribution<1>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=acosh", acoshf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), 1);
}

void TestMathFunctions::testAcosPi()
{
    testUnaryFunction<5, NormalDistribution<0, 2>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=acospi",
        FlushAndRoundUnary{[](float f) -> float { return std::acos(f) / static_cast<float>(M_PI); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -1, 1);
}

void TestMathFunctions::testAsin()
{
    testUnaryFunction<4, NormalDistribution<0, 1>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=asin",
        FlushAndRoundUnary{asinf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -1, 1);
}

void TestMathFunctions::testAsinh()
{
    testUnaryFunction<4, NormalDistribution<0>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=asinh",
        FlushAndRoundUnary{asinhf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testAsinPi()
{
    testUnaryFunction<5, NormalDistribution<0, 2>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=asinpi",
        FlushAndRoundUnary{[](float f) -> float { return std::asin(f) / static_cast<float>(M_PI); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -1, 1);
}

void TestMathFunctions::testAtan()
{
    testUnaryFunction<5, NormalDistribution<0>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=atan", atanf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testAtan2()
{
    testBinaryFunction<6, NormalDistribution<0>>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=atan2",
        FlushAndRoundBinary{atan2f},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testAtanh()
{
    testUnaryFunction<5, NormalDistribution<0, 1>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=atanh",
        FlushAndRoundUnary{atanhf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -1, 1);
}

void TestMathFunctions::testAtanPi()
{
    testUnaryFunction<5, NormalDistribution<0>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=atanpi",
        FlushAndRoundUnary{[](float f) -> float { return std::atan(f) / static_cast<float>(M_PI); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testAtan2Pi()
{
    testBinaryFunction<6, NormalDistribution<0>>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=atan2pi",
        FlushAndRoundBinary{[](float f1, float f2) -> float { return std::atan2(f1, f2) / static_cast<float>(M_PI); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testCbrt()
{
    testUnaryFunction<4>(config, "-DOUT=float16 -DIN=float16 -DFUNC=cbrt", FlushAndRoundUnary{cbrtf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testCeil()
{
    testUnaryFunction<0>(config, "-DOUT=float16 -DIN=float16 -DFUNC=ceil", FlushAndRoundUnary{ceilf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testCopysign()
{
    testBinaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=copysign", copysignf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testCos()
{
    testUnaryFunction<4, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=cos",
        FlushAndRoundUnary{cosf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testCosh()
{
    testUnaryFunction<4, NormalDistribution<0, 25>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=cosh",
        FlushAndRoundUnary{coshf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -85, 85);
}

void TestMathFunctions::testCosPi()
{
    testUnaryFunction<4, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=cospi",
        FlushAndRoundUnary{[](float f) -> float { return std::cos(f) * static_cast<float>(M_PI); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testErfc()
{
    testUnaryFunction<16, NormalDistribution<0, 4>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=erfc",
        FlushAndRoundUnary{erfcf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testErf()
{
    testUnaryFunction<16, NormalDistribution<0, 4>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=erf",
        FlushAndRoundUnary{erff},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testExp()
{
    testUnaryFunction<4, NormalDistribution<0, 20>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=exp",
        FlushAndRoundUnary{expf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -80, 80);
}

void TestMathFunctions::testExp2()
{
    testUnaryFunction<4, NormalDistribution<0, 32>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=exp2",
        FlushAndRoundUnary{exp2f},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -126, 126);
}

void TestMathFunctions::testExp10()
{
    testUnaryFunction<4, NormalDistribution<0, 10>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=exp10",
        FlushAndRoundUnary{exp10f},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -36, 36);
}

void TestMathFunctions::testExpm1()
{
    testUnaryFunction<4, NormalDistribution<0, 20>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=expm1",
        FlushAndRoundUnary{expm1f},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -80, 80);
}

void TestMathFunctions::testFabs()
{
    testUnaryFunction<0>(config, "-DOUT=float16 -DIN=float16 -DFUNC=fabs", fabsf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFdim()
{
    testBinaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fdim", FlushAndRoundBinary{fdimf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFloor()
{
    testUnaryFunction<0>(config, "-DOUT=float16 -DIN=float16 -DFUNC=floor", floorf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFma()
{
    testTernaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DIN2=float16 -DFUNC=fma", fmaf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFmax()
{
    testBinaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fmax", fmax,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFmin()
{
    testBinaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fmin", fmin,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFmod()
{
    testBinaryFunction<0, NormalDistribution<0>>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=fmod",
        FlushAndRoundBinary{fmodf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testFract() {}

void TestMathFunctions::testFrexp() {}

void TestMathFunctions::testHypot()
{
    testBinaryFunction<4, NormalDistribution<0>>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=hypot",
        FlushAndRoundBinary{hypotf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testIlogb()
{
    testUnaryFunction<0, UniformDistribution<double>, int, CompareEqual<int>>(config,
        "-DOUT=int16 -DIN=float16 -DFUNC=ilogb", ilogbf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testLdexp()
{
    testBinaryFunction<16, UniformDistribution<double>, int, UniformDistribution<long>>(config,
        "-DOUT=float16 -DIN0=float16 -DIN1=int16 -DFUNC=ldexp", ldexpf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testLgamma()
{
    testUnaryFunction<1024 /* maximum error is undefined */, NormalDistribution<0, 15>>(config,
        "-DOUT=float16 -DIN=float16 -DFUNC=lgamma", FlushAndRoundUnary{lgammaf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -30, 30);
}

void TestMathFunctions::testLog()
{
    testUnaryFunction<4, NormalDistribution<0, 100>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=log",
        FlushAndRoundUnary{logf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), 0);
}

void TestMathFunctions::testLog2()
{
    testUnaryFunction<4, NormalDistribution<0, 100>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=log2",
        FlushAndRoundUnary{log2f},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), 0);
}

void TestMathFunctions::testLog10()
{
    testUnaryFunction<4, NormalDistribution<0, 100>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=log10",
        FlushAndRoundUnary{log10f},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), 0);
}

void TestMathFunctions::testLog1p()
{
    testUnaryFunction<4, NormalDistribution<0, 100>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=log1p",
        FlushAndRoundUnary{log1pf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2),
        -1 /* log(1+x)*/);
}

void TestMathFunctions::testLogb()
{
    testUnaryFunction<0, NormalDistribution<0>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=logb",
        FlushAndRoundUnary{logbf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testMad()
{
    testTernaryFunction<4096 /* infinite ULP */>(
        config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DIN2=float16 -DFUNC=mad",
        [](float a, float b, float c) -> float { return a * b + c; },
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testMaxMag()
{
    testBinaryFunction<0>(
        config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=maxmag",
        [](float a, float b) -> float {
            return std::abs(a) > std::abs(b) ? a : std::abs(b) > std::abs(a) ? b : std::max(a, b);
        },
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testMinMag()
{
    testBinaryFunction<0>(
        config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=minmag",
        [](float a, float b) -> float {
            return std::abs(a) < std::abs(b) ? a : std::abs(b) < std::abs(a) ? b : std::min(a, b);
        },
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testModf() {}

void TestMathFunctions::testNan()
{
    testUnaryFunction<0>(
        config, "-DOUT=float16 -DIN=uint16 -DFUNC=nan",
        [](unsigned code) -> float { return std::numeric_limits<float>::quiet_NaN(); },
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testNextafter()
{
    testBinaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=nextafter", nextafterf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testPow()
{
    testBinaryFunction<16, NormalDistribution<0>>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=pow",
        FlushAndRoundBinary{powf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testPown()
{
    testBinaryFunction<16, UniformDistribution<double>, int, UniformDistribution<long>>(config,
        "-DOUT=float16 -DIN0=float16 -DIN1=int16 -DFUNC=pown", FlushAndRoundBinary{powf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testPowr() {}

void TestMathFunctions::testRemainder()
{
    testBinaryFunction<0>(config, "-DOUT=float16 -DIN0=float16 -DIN1=float16 -DFUNC=remainder",
        FlushAndRoundBinary{remainderf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRemquo() {}

void TestMathFunctions::testRint()
{
    testUnaryFunction<0>(config, "-DOUT=float16 -DIN=float16 -DFUNC=rint", rintf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRootn()
{
    testBinaryFunction<16, NormalDistribution<0>>(config, "-DOUT=float16 -DIN0=float16 -DIN1=int16 -DFUNC=rootn",
        FlushAndRoundBinary{[](float a, int b) -> float { return std::pow(a, 1.0f / static_cast<float>(b)); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRound()
{
    testUnaryFunction<0>(config, "-DOUT=float16 -DIN=float16 -DFUNC=round", roundf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testRsqrt()
{
    testUnaryFunction<4, NormalDistribution<0>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=rsqrt",
        FlushAndRoundUnary{[](float a) -> float { return 1.0f / sqrtf(a); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), 0);
}

void TestMathFunctions::testSin()
{
    testUnaryFunction<4, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=sin",
        FlushAndRoundUnary{sinf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testSinh()
{
    testUnaryFunction<4, NormalDistribution<0, 25>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=sinh",
        FlushAndRoundUnary{sinhf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -85, 85);
}

void TestMathFunctions::testSinPi()
{
    testUnaryFunction<4, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=sinpi",
        FlushAndRoundUnary{[](float a) -> float { return std::sin(static_cast<float>(M_PI) * a); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testSqrt()
{
    testUnaryFunction<4, NormalDistribution<0>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=sqrt",
        FlushAndRoundUnary{sqrtf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), 0);
}

void TestMathFunctions::testTan()
{
    testUnaryFunction<5, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=tan",
        FlushAndRoundUnary{tanf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testTanh()
{
    testUnaryFunction<5, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=tanh",
        FlushAndRoundUnary{tanhf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testTanPi()
{
    testUnaryFunction<6, NormalDistribution<0, 1000>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=tanpi",
        FlushAndRoundUnary{[](float f) -> float { return std::tan(f) * static_cast<float>(M_PI); }},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMathFunctions::testTgamma()
{
    testUnaryFunction<16, NormalDistribution<0, 10>>(config, "-DOUT=float16 -DIN=float16 -DFUNC=tgamma",
        FlushAndRoundUnary{tgammaf},
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2), -30, 30);
}

void TestMathFunctions::testTrunc()
{
    testUnaryFunction<0>(config, "-DOUT=float16 -DIN=float16 -DFUNC=trunc", truncf,
        std::bind(&TestMathFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
