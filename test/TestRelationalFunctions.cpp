/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestRelationalFunctions.h"
#include "../src/helper.h"
#include "emulation_helper.h"

#include <algorithm>

//TODO all and any may both not work, but the probability for all MSBs set is very low

static const std::string UNARY_FUNCTION = R"(
__kernel void test(__global int16* out, __global float16* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string BINARY_FUNCTION = R"(
__kernel void test(__global int16* out, __global float16* in0, __global float16* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

static const std::string TERNARY_FUNCTION = R"(
__kernel void test(__global TYPE* out, __global TYPE* in0, __global TYPE* in1, __global TYPE* in2) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid], in2[gid]);
}
)";

static const std::string UNARY_GROUPED_FUNCTION = R"(
__kernel void test(__global int* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

TestRelationalFunctions::TestRelationalFunctions(const vc4c::Configuration& config) : config(config)
{
    TEST_ADD(TestRelationalFunctions::testIsEqual);
    TEST_ADD(TestRelationalFunctions::testIsNotEqual);
    TEST_ADD(TestRelationalFunctions::testIsGreater);
    TEST_ADD(TestRelationalFunctions::testIsGreaterEqual);
    TEST_ADD(TestRelationalFunctions::testIsLess);
    TEST_ADD(TestRelationalFunctions::testIsLessEqual);
    TEST_ADD(TestRelationalFunctions::testIsLessGreater);
    TEST_ADD(TestRelationalFunctions::testIsFinite);
    TEST_ADD(TestRelationalFunctions::testIsInf);
    TEST_ADD(TestRelationalFunctions::testIsNaN);
    TEST_ADD(TestRelationalFunctions::testIsNormal);
    TEST_ADD(TestRelationalFunctions::testIsOrdered);
    TEST_ADD(TestRelationalFunctions::testIsUnordered);
    TEST_ADD(TestRelationalFunctions::testSignbit);
    TEST_ADD(TestRelationalFunctions::testIntAny);
    TEST_ADD(TestRelationalFunctions::testShortAny);
    TEST_ADD(TestRelationalFunctions::testCharAny);
    TEST_ADD(TestRelationalFunctions::testIntAll);
    TEST_ADD(TestRelationalFunctions::testShortAll);
    TEST_ADD(TestRelationalFunctions::testCharAll);
    TEST_ADD(TestRelationalFunctions::testBitselectInt);
    TEST_ADD(TestRelationalFunctions::testBitselectShort);
    TEST_ADD(TestRelationalFunctions::testBitselectChar);
    TEST_ADD(TestRelationalFunctions::testSelectInt);
    TEST_ADD(TestRelationalFunctions::testSelectShort);
    TEST_ADD(TestRelationalFunctions::testSelectChar);
}

void TestRelationalFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

static void testUnaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<int(float)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);

    auto in = generateInput<float, 16 * 12>(true);

    auto out = runEmulation<float, int, 16, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryResults<int, float>(in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

static void testBinaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<int(float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);

    auto in0 = generateInput<float, 16 * 12>(true);
    auto in1 = generateInput<float, 16 * 12>(true);

    auto out = runEmulation<float, int, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryResults<int, float>(in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename T>
static void testTernaryFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<T(T, T, T)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, TERNARY_FUNCTION, options);

    auto in0 = generateInput<T, 16 * 12>(true);
    auto in1 = generateInput<T, 16 * 12>(true);
    auto in2 = generateInput<T, 16 * 12>(true);

    auto out = runEmulation<T, T, 16, 12>(code, {in0, in1, in2});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkTernaryResults<T, T>(in0, in1, in2, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename T, typename R = T>
static void testUnaryGroupFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<R(const std::array<T, 16>&)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_GROUPED_FUNCTION, options);

    auto in = generateInput<T, 16 * 12>(true);

    auto out = runEmulation<T, R, 16, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryReducedResults<R, T>(in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename C, typename T = typename C::first_argument_type>
static int checkRelation(T arg1, T arg2)
{
    C c{};
    return c(arg1, arg2) ? -1 : 0;
}

template <typename T>
static T checkBitSelect(T in1, T in2, T mask)
{
    return (in2 & mask) | (in1 & ~mask);
}

template <typename T>
static T checkSelect(T in1, T in2, T mask)
{
    return vc4c::bit_cast<T, typename std::make_signed<T>::type>(mask) < 0 ? in2 : in1;
}

template <typename T>
static int checkAll(const std::array<T, 16>& val)
{
    return std::all_of(val.begin(), val.end(),
               [](T t) -> bool { return vc4c::bit_cast<T, typename std::make_signed<T>::type>(t) < 0; }) ?
        1 :
        0;
}

template <typename T>
static int checkAny(const std::array<T, 16>& val)
{
    return std::any_of(val.begin(), val.end(),
               [](T t) -> bool { return vc4c::bit_cast<T, typename std::make_signed<T>::type>(t) < 0; }) ?
        1 :
        0;
}

void TestRelationalFunctions::testIsEqual()
{
    testBinaryFunction(config, "-DFUNC=isequal", checkRelation<std::equal_to<float>>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsNotEqual()
{
    testBinaryFunction(config, "-DFUNC=isnotequal", checkRelation<std::not_equal_to<float>>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsGreater()
{
    testBinaryFunction(config, "-DFUNC=isgreater", checkRelation<std::greater<float>>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsGreaterEqual()
{
    testBinaryFunction(config, "-DFUNC=isgreater", checkRelation<std::greater_equal<float>>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsLess()
{
    testBinaryFunction(config, "-DFUNC=isless", checkRelation<std::less<float>>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsLessEqual()
{
    testBinaryFunction(config, "-DFUNC=islessequal", checkRelation<std::less_equal<float>>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsLessGreater()
{
    testBinaryFunction(config, "-DFUNC=islessgreater",
        [](float a, float b) -> int { return ((a > b) || (a < b)) ? -1 : 0; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsFinite()
{
    testUnaryFunction(config, "-DFUNC=isfinite", [](float a) -> int { return std::isinf(a) ? 0 : -1; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsInf()
{
    testUnaryFunction(config, "-DFUNC=isinf", [](float a) -> int { return std::isinf(a) ? -1 : 0; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsNaN()
{
    testUnaryFunction(config, "-DFUNC=isnan", isnanf,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsNormal()
{
    testUnaryFunction(config, "-DFUNC=isnormal", [](float a) -> int { return std::isnormal(a) ? -1 : 0; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsOrdered()
{
    testBinaryFunction(config, "-DFUNC=isordered", [](float a, float b) -> int { return (a == a && b == b) ? -1 : 0; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIsUnordered()
{
    testBinaryFunction(config, "-DFUNC=isunordered",
            [](float a, float b) -> int { return (std::isnan(a) || std::isnan(b)) ? -1 : 0; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testSignbit()
{
    testUnaryFunction(config, "-DFUNC=signbit", [](float a) -> int { return std::signbit(a) ? -1 : 0; },
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIntAny()
{
    testUnaryGroupFunction<int, int>(config, "-DTYPE=int16 -DFUNC=any", checkAny<int>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testShortAny()
{
    testUnaryGroupFunction<short, int>(config, "-DTYPE=short16 -DFUNC=any", checkAny<short>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testCharAny()
{
    testUnaryGroupFunction<char, int>(config, "-DTYPE=char16 -DFUNC=any", checkAny<char>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testIntAll()
{
    testUnaryGroupFunction<int, int>(config, "-DTYPE=int16 -DFUNC=all", checkAll<int>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testShortAll()
{
    testUnaryGroupFunction<short, int>(config, "-DTYPE=short16 -DFUNC=all", checkAll<short>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testCharAll()
{
    testUnaryGroupFunction<char, int>(config, "-DTYPE=char16 -DFUNC=all", checkAll<char>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testBitselectInt()
{
    testTernaryFunction<int>(config, "-DTYPE=int16 -DFUNC=bitselect", checkBitSelect<int>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testBitselectShort()
{
    testTernaryFunction<short>(config, "-DTYPE=short16 -DFUNC=bitselect", checkBitSelect<short>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testBitselectChar()
{
    testTernaryFunction<char>(config, "-DTYPE=char16 -DFUNC=bitselect", checkBitSelect<char>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testSelectInt()
{
    testTernaryFunction<int>(config, "-DTYPE=int16 -DFUNC=select", checkSelect<int>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testSelectShort()
{
    testTernaryFunction<short>(config, "-DTYPE=short16 -DFUNC=select", checkSelect<short>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestRelationalFunctions::testSelectChar()
{
    testTernaryFunction<char>(config, "-DTYPE=char16 -DFUNC=select", checkSelect<char>,
        std::bind(&TestRelationalFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
