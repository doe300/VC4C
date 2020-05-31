/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestArithmetic.h"
#include "emulation_helper.h"

#include "intrinsics/Operators.h"

#include <climits>

// TODO for floating point checks, add Inf/NaN values

static const std::string UNARY_OPERATION = R"(
#ifdef TYPE
#define IN TYPE
#define OUT TYPE
#endif
__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  // need to cache locally, e.g. for increment
  IN tmp = in[gid];
  // need to have braces for sizeof operator
  out[gid] = OP (tmp);
}
)";

static const std::string BINARY_OPERATION = R"(
#ifdef TYPE
#define IN TYPE
#define OUT TYPE
#endif
__kernel void test(__global OUT* out, const __global IN* in0, const __global IN* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] OP in1[gid];
}
)";

static const std::string SELECTION_OPERATION = R"(
__kernel void test(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = select(in0[gid], in1[gid], in0[gid]);
}
)";

static const std::string RELATIONAL_OPERATION = R"(
__kernel void test(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] OP in1[gid]);
}
)";

static const std::string TERNARY_OPERATOR = R"(
#ifdef TYPE
#define IN TYPE
#define OUT TYPE
#endif
__kernel void test(__global OUT* out, const __global IN* in0, const __global IN* in1) {
  size_t gid = get_global_id(0);
  IN a = in0[gid];
  IN b = in1[gid];
  out[gid] = a OP b ? a : b;
}
)";

TestArithmetic::TestArithmetic(const vc4c::Configuration& config) : config(config)
{
    TEST_ADD(TestArithmetic::testSignedIntUnaryPlus);
    TEST_ADD(TestArithmetic::testUnsignedIntUnaryPlus);
    TEST_ADD(TestArithmetic::testSignedShortUnaryPlus);
    TEST_ADD(TestArithmetic::testUnsignedShortUnaryPlus);
    TEST_ADD(TestArithmetic::testSignedCharUnaryPlus);
    TEST_ADD(TestArithmetic::testUnsignedCharUnaryPlus);
    TEST_ADD(TestArithmetic::testFloatUnaryPlus);
    TEST_ADD(TestArithmetic::testSignedLongUnaryPlus);
    TEST_ADD(TestArithmetic::testUnsignedLongUnaryPlus);

    TEST_ADD(TestArithmetic::testSignedIntUnaryMinus);
    TEST_ADD(TestArithmetic::testUnsignedIntUnaryMinus);
    TEST_ADD(TestArithmetic::testSignedShortUnaryMinus);
    TEST_ADD(TestArithmetic::testUnsignedShortUnaryMinus);
    TEST_ADD(TestArithmetic::testSignedCharUnaryMinus);
    TEST_ADD(TestArithmetic::testUnsignedCharUnaryMinus);
    TEST_ADD(TestArithmetic::testFloatUnaryMinus);
    TEST_ADD(TestArithmetic::testSignedLongUnaryMinus);
    TEST_ADD(TestArithmetic::testUnsignedLongUnaryMinus);

    TEST_ADD(TestArithmetic::testSignedIntAddition);
    TEST_ADD(TestArithmetic::testUnsignedIntAddition);
    TEST_ADD(TestArithmetic::testSignedShortAddition);
    TEST_ADD(TestArithmetic::testUnsignedShortAddition);
    TEST_ADD(TestArithmetic::testSignedCharAddition);
    TEST_ADD(TestArithmetic::testUnsignedCharAddition);
    TEST_ADD(TestArithmetic::testFloatAddition);
    TEST_ADD(TestArithmetic::testSignedLongAddition);
    TEST_ADD(TestArithmetic::testUnsignedLongAddition);

    TEST_ADD(TestArithmetic::testSignedIntSubtraction);
    TEST_ADD(TestArithmetic::testUnsignedIntSubtraction);
    TEST_ADD(TestArithmetic::testSignedShortSubtraction);
    TEST_ADD(TestArithmetic::testUnsignedShortSubtraction);
    TEST_ADD(TestArithmetic::testSignedCharSubtraction);
    TEST_ADD(TestArithmetic::testUnsignedCharSubtraction);
    TEST_ADD(TestArithmetic::testFloatSubtraction);
    TEST_ADD(TestArithmetic::testSignedLongSubtraction);
    TEST_ADD(TestArithmetic::testUnsignedLongSubtraction);

    TEST_ADD(TestArithmetic::testSignedIntMultiplication);
    TEST_ADD(TestArithmetic::testSignedShortMultiplication);
    TEST_ADD(TestArithmetic::testSignedCharMultiplication);
    TEST_ADD(TestArithmetic::testUnsignedIntMultiplication);
    TEST_ADD(TestArithmetic::testUnsignedShortMultiplication);
    TEST_ADD(TestArithmetic::testUnsignedCharMultiplication);
    TEST_ADD(TestArithmetic::testFloatingPointMultiplication);

    TEST_ADD(TestArithmetic::testSignedIntDivision);
    TEST_ADD(TestArithmetic::testSignedShortDivision);
    TEST_ADD(TestArithmetic::testSignedCharDivision);
    TEST_ADD(TestArithmetic::testUnsignedIntDivision);
    TEST_ADD(TestArithmetic::testUnsignedShortDivision);
    TEST_ADD(TestArithmetic::testUnsignedCharDivision);
    TEST_ADD(TestArithmetic::testFloatingPointDivision);

    TEST_ADD(TestArithmetic::testSignedIntModulo);
    TEST_ADD(TestArithmetic::testSignedShortModulo);
    TEST_ADD(TestArithmetic::testSignedCharModulo);
    TEST_ADD(TestArithmetic::testUnsignedIntModulo);
    TEST_ADD(TestArithmetic::testUnsignedShortModulo);
    TEST_ADD(TestArithmetic::testUnsignedCharModulo);

    TEST_ADD(TestArithmetic::testSignedIntIncrement);
    TEST_ADD(TestArithmetic::testUnsignedIntIncrement);
    TEST_ADD(TestArithmetic::testSignedShortIncrement);
    TEST_ADD(TestArithmetic::testUnsignedShortIncrement);
    TEST_ADD(TestArithmetic::testSignedCharIncrement);
    TEST_ADD(TestArithmetic::testUnsignedCharIncrement);
    TEST_ADD(TestArithmetic::testSignedLongIncrement);
    TEST_ADD(TestArithmetic::testUnsignedLongIncrement);

    TEST_ADD(TestArithmetic::testSignedIntDecrement);
    TEST_ADD(TestArithmetic::testUnsignedIntDecrement);
    TEST_ADD(TestArithmetic::testSignedShortDecrement);
    TEST_ADD(TestArithmetic::testUnsignedShortDecrement);
    TEST_ADD(TestArithmetic::testSignedCharDecrement);
    TEST_ADD(TestArithmetic::testUnsignedCharDecrement);
    TEST_ADD(TestArithmetic::testSignedLongDecrement);
    TEST_ADD(TestArithmetic::testUnsignedLongDecrement);

    TEST_ADD(TestArithmetic::testIntegerEquality);
    TEST_ADD(TestArithmetic::testShortEquality);
    TEST_ADD(TestArithmetic::testCharEquality);
    TEST_ADD(TestArithmetic::testFloatEquality);
    // TEST_ADD(TestArithmetic::testLongEquality);

    TEST_ADD(TestArithmetic::testIntegerInequality);
    TEST_ADD(TestArithmetic::testShortInequality);
    TEST_ADD(TestArithmetic::testCharInequality);
    TEST_ADD(TestArithmetic::testFloatInequality);
    //    TEST_ADD(TestArithmetic::testLongInequality);

    TEST_ADD(TestArithmetic::testSignedIntGreater);
    TEST_ADD(TestArithmetic::testSignedShortGreater);
    TEST_ADD(TestArithmetic::testSignedCharGreater);
    TEST_ADD(TestArithmetic::testUnsignedIntGreater);
    TEST_ADD(TestArithmetic::testUnsignedShortGreater);
    TEST_ADD(TestArithmetic::testUnsignedCharGreater);
    TEST_ADD(TestArithmetic::testFloatGreater);
    // TEST_ADD(TestArithmetic::testSignedLongGreater);
    // TEST_ADD(TestArithmetic::testUnsignedLongGreater);

    TEST_ADD(TestArithmetic::testSignedIntLess);
    TEST_ADD(TestArithmetic::testSignedShortLess);
    TEST_ADD(TestArithmetic::testSignedCharLess);
    TEST_ADD(TestArithmetic::testUnsignedIntLess);
    TEST_ADD(TestArithmetic::testUnsignedShortLess);
    TEST_ADD(TestArithmetic::testUnsignedCharLess);
    TEST_ADD(TestArithmetic::testFloatLess);
    // TEST_ADD(TestArithmetic::testSignedLongLess);
    // TEST_ADD(TestArithmetic::testUnsignedLongLess);

    TEST_ADD(TestArithmetic::testSignedIntGreaterEquals);
    TEST_ADD(TestArithmetic::testSignedShortGreaterEquals);
    TEST_ADD(TestArithmetic::testSignedCharGreaterEquals);
    TEST_ADD(TestArithmetic::testUnsignedIntGreaterEquals);
    TEST_ADD(TestArithmetic::testUnsignedShortGreaterEquals);
    TEST_ADD(TestArithmetic::testUnsignedCharGreaterEquals);
    TEST_ADD(TestArithmetic::testFloatGreaterEquals);
    // TEST_ADD(TestArithmetic::testSignedLongGreaterEquals);
    // TEST_ADD(TestArithmetic::testUnsignedLongGreaterEquals);

    TEST_ADD(TestArithmetic::testSignedIntLessEquals);
    TEST_ADD(TestArithmetic::testSignedShortLessEquals);
    TEST_ADD(TestArithmetic::testSignedCharLessEquals);
    TEST_ADD(TestArithmetic::testUnsignedIntLessEquals);
    TEST_ADD(TestArithmetic::testUnsignedShortLessEquals);
    TEST_ADD(TestArithmetic::testUnsignedCharLessEquals);
    TEST_ADD(TestArithmetic::testFloatLessEquals);
    // TEST_ADD(TestArithmetic::testSignedLongLessEquals);
    // TEST_ADD(TestArithmetic::testUnsignedLongLessEquals);

    TEST_ADD(TestArithmetic::testSignedIntSelectScalar);
    TEST_ADD(TestArithmetic::testSignedIntSelectVector);
    TEST_ADD(TestArithmetic::testSignedShortSelectScalar);
    TEST_ADD(TestArithmetic::testSignedShortSelectVector);
    TEST_ADD(TestArithmetic::testSignedCharSelectScalar);
    TEST_ADD(TestArithmetic::testSignedCharSelectVector);
    TEST_ADD(TestArithmetic::testUnsignedIntSelectScalar);
    TEST_ADD(TestArithmetic::testUnsignedIntSelectVector);
    TEST_ADD(TestArithmetic::testUnsignedShortSelectScalar);
    TEST_ADD(TestArithmetic::testUnsignedShortSelectVector);
    TEST_ADD(TestArithmetic::testUnsignedCharSelectScalar);
    TEST_ADD(TestArithmetic::testUnsignedCharSelectVector);
    // TEST_ADD(TestArithmetic::testSignedLongSelectScalar);
    // TEST_ADD(TestArithmetic::testSignedLongSelectVector);
    // TEST_ADD(TestArithmetic::testUnsignedLongSelectScalar);
    // TEST_ADD(TestArithmetic::testUnsignedLongSelectVector);

    TEST_ADD(TestArithmetic::testSignedIntAnd);
    TEST_ADD(TestArithmetic::testSignedShortAnd);
    TEST_ADD(TestArithmetic::testSignedCharAnd);
    TEST_ADD(TestArithmetic::testUnsignedIntAnd);
    TEST_ADD(TestArithmetic::testUnsignedShortAnd);
    TEST_ADD(TestArithmetic::testUnsignedCharAnd);
    TEST_ADD(TestArithmetic::testFloatAnd);
    // TEST_ADD(TestArithmetic::testSignedLongAnd);
    // TEST_ADD(TestArithmetic::testUnsignedLongAnd);

    TEST_ADD(TestArithmetic::testSignedIntOr);
    TEST_ADD(TestArithmetic::testSignedShortOr);
    TEST_ADD(TestArithmetic::testSignedCharOr);
    TEST_ADD(TestArithmetic::testUnsignedIntOr);
    TEST_ADD(TestArithmetic::testUnsignedShortOr);
    TEST_ADD(TestArithmetic::testUnsignedCharOr);
    TEST_ADD(TestArithmetic::testFloatOr);
    // TEST_ADD(TestArithmetic::testSignedLongOr);
    // TEST_ADD(TestArithmetic::testUnsignedLongOr);

    TEST_ADD(TestArithmetic::testSignedIntNot);
    TEST_ADD(TestArithmetic::testSignedShortNot);
    TEST_ADD(TestArithmetic::testSignedCharNot);
    TEST_ADD(TestArithmetic::testUnsignedIntNot);
    TEST_ADD(TestArithmetic::testUnsignedShortNot);
    TEST_ADD(TestArithmetic::testUnsignedCharNot);
    TEST_ADD(TestArithmetic::testFloatNot);
    TEST_ADD(TestArithmetic::testSignedLongNot);
    TEST_ADD(TestArithmetic::testUnsignedLongNot);

    TEST_ADD(TestArithmetic::testSignedIntBitNot);
    TEST_ADD(TestArithmetic::testUnsignedIntBitNot);
    TEST_ADD(TestArithmetic::testSignedShortBitNot);
    TEST_ADD(TestArithmetic::testUnsignedShortBitNot);
    TEST_ADD(TestArithmetic::testSignedCharBitNot);
    TEST_ADD(TestArithmetic::testUnsignedCharBitNot);
    TEST_ADD(TestArithmetic::testSignedLongBitNot);
    TEST_ADD(TestArithmetic::testUnsignedLongBitNot);

    TEST_ADD(TestArithmetic::testSignedIntBitAnd);
    TEST_ADD(TestArithmetic::testUnsignedIntBitAnd);
    TEST_ADD(TestArithmetic::testSignedShortBitAnd);
    TEST_ADD(TestArithmetic::testUnsignedShortBitAnd);
    TEST_ADD(TestArithmetic::testSignedCharBitAnd);
    TEST_ADD(TestArithmetic::testUnsignedCharBitAnd);
    TEST_ADD(TestArithmetic::testSignedLongBitAnd);
    TEST_ADD(TestArithmetic::testUnsignedLongBitAnd);

    TEST_ADD(TestArithmetic::testSignedIntBitOr);
    TEST_ADD(TestArithmetic::testUnsignedIntBitOr);
    TEST_ADD(TestArithmetic::testSignedShortBitOr);
    TEST_ADD(TestArithmetic::testUnsignedShortBitOr);
    TEST_ADD(TestArithmetic::testSignedCharBitOr);
    TEST_ADD(TestArithmetic::testUnsignedCharBitOr);
    TEST_ADD(TestArithmetic::testSignedLongBitOr);
    TEST_ADD(TestArithmetic::testUnsignedLongBitOr);

    TEST_ADD(TestArithmetic::testSignedIntBitXor);
    TEST_ADD(TestArithmetic::testUnsignedIntBitXor);
    TEST_ADD(TestArithmetic::testSignedShortBitXor);
    TEST_ADD(TestArithmetic::testUnsignedShortBitXor);
    TEST_ADD(TestArithmetic::testSignedCharBitXor);
    TEST_ADD(TestArithmetic::testUnsignedCharBitXor);
    TEST_ADD(TestArithmetic::testSignedLongBitXor);
    TEST_ADD(TestArithmetic::testUnsignedLongBitXor);

    TEST_ADD(TestArithmetic::testSignedIntBitShiftLeft);
    TEST_ADD(TestArithmetic::testUnsignedIntBitShiftLeft);
    TEST_ADD(TestArithmetic::testSignedShortBitShiftLeft);
    TEST_ADD(TestArithmetic::testUnsignedShortBitShiftLeft);
    TEST_ADD(TestArithmetic::testSignedCharBitShiftLeft);
    TEST_ADD(TestArithmetic::testUnsignedCharBitShiftLeft);
    TEST_ADD(TestArithmetic::testSignedLongBitShiftLeft);
    TEST_ADD(TestArithmetic::testUnsignedLongBitShiftLeft);

    TEST_ADD(TestArithmetic::testSignedIntBitShiftRight);
    TEST_ADD(TestArithmetic::testUnsignedIntBitShiftRight);
    TEST_ADD(TestArithmetic::testSignedShortBitShiftRight);
    TEST_ADD(TestArithmetic::testUnsignedShortBitShiftRight);
    TEST_ADD(TestArithmetic::testSignedCharBitShiftRight);
    TEST_ADD(TestArithmetic::testUnsignedCharBitShiftRight);
    TEST_ADD(TestArithmetic::testSignedLongBitShiftRight);
    TEST_ADD(TestArithmetic::testUnsignedLongBitShiftRight);

    TEST_ADD(TestArithmetic::testSignedIntTrinaryScalar);
    TEST_ADD(TestArithmetic::testSignedIntTrinaryVector);
    TEST_ADD(TestArithmetic::testSignedShortTrinaryScalar);
    TEST_ADD(TestArithmetic::testSignedShortTrinaryVector);
    TEST_ADD(TestArithmetic::testSignedCharTrinaryScalar);
    TEST_ADD(TestArithmetic::testSignedCharTrinaryVector);
    // TEST_ADD(TestArithmetic::testSignedLongTrinaryScalar);
    // TEST_ADD(TestArithmetic::testSignedLongTrinaryVector);

    TEST_ADD(TestArithmetic::testUnsignedIntTrinaryScalar);
    TEST_ADD(TestArithmetic::testUnsignedIntTrinaryVector);
    TEST_ADD(TestArithmetic::testUnsignedShortTrinaryScalar);
    TEST_ADD(TestArithmetic::testUnsignedShortTrinaryVector);
    TEST_ADD(TestArithmetic::testUnsignedCharTrinaryScalar);
    TEST_ADD(TestArithmetic::testUnsignedCharTrinaryVector);
    // TEST_ADD(TestArithmetic::testUnsignedLongTrinaryScalar);
    // TEST_ADD(TestArithmetic::testUnsignedLongTrinaryVector);
}

TestArithmetic::~TestArithmetic() = default;

void TestArithmetic::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result)
}

template <typename T, typename Comparison = CompareEqual<T>>
static void testUnaryOperation(vc4c::Configuration& config, const std::string& options, const std::function<T(T)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError, bool allowZero = true)
{
    std::stringstream code;
    compileBuffer(config, code, UNARY_OPERATION, options);

    auto in = generateInput<T, 16 * 12>(true);

    auto out = runEmulation<T, T, 16, 12>(code, {in});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkUnaryResults<T, T>(in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename T, typename Comparison = CompareEqual<T>>
static void testBinaryOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<T(T, T)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_OPERATION, options);

    auto in0 = generateInput<T, 16 * 12>(false);
    auto in1 = generateInput<T, 16 * 12>(false);

    auto out = runEmulation<T, T, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<T, T, 16 * 12, Comparison>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename T, std::size_t N, typename Comparison = CompareEqual<T>>
static void testTernaryOperator(vc4c::Configuration& config, const std::string& options,
    const std::function<T(T, T)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, TERNARY_OPERATOR, options);

    auto in0 = generateInput<T, N * 12>(true);
    auto in1 = generateInput<T, N * 12>(true);

    auto out = runEmulation<T, T, N, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<T, T, N * 12, Comparison>(
        in0, in1, out, op, "(a" + options.substr(pos, options.find(' ', pos) - pos) + "b)?a:b", onError);
}

template <typename T>
static void testRelationalOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<int(T, T)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, RELATIONAL_OPERATION, options);

    auto in0 = generateInput<T, 16 * 12>(true);
    auto in1 = generateInput<T, 16 * 12>(true);

    auto out = runEmulation<T, int, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<int, T, 16 * 12>(in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename T, std::size_t N>
static void testSelectOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<T(T, T)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, SELECTION_OPERATION, options);

    auto in0 = generateInput<T, N * 12>(true);
    auto in1 = generateInput<T, N * 12>(true);

    auto out = runEmulation<T, T, N, 12>(code, {in0, in1});
    checkBinaryResults<T, T, N * 12, CompareEqual<T>, T>(in0, in1, out, op, "select", onError);
}

template <typename C, typename T = typename C::first_argument_type,
    typename R = typename std::common_type<T, int>::type>
static R checkRelation(T arg1, T arg2)
{
    C c{};
    return c(arg1, arg2) ? -1 : 0;
}

template <typename T, typename R = typename std::common_type<T, int>::type>
static R checkAnd(T arg1, T arg2)
{
    return (arg1 != 0 && arg2 != 0) ? -1 : 0;
}

template <typename T, typename R = typename std::common_type<T, int>::type>
static R checkOr(T arg1, T arg2)
{
    return (arg1 != 0 || arg2 != 0) ? -1 : 0;
}

template <typename T, typename R = typename std::common_type<T, int>::type>
static R checkNot(T arg)
{
    return !arg ? -1 : 0;
}

/*
 * For the shift operands, for some arbitrary reason, the OpenCL 1.2 (and 2.0) standard defines some weird behavior:
 * The shift mask is ANDed with the number of bits - 1 of the shifted type before applying the shift.
 *
 * E.g. (ushort)a << (ushort)b -> (ushort)a << ((ushort)b & 15)
 *
 * This is applied by LLVM (introduced in
 * https://github.com/llvm-mirror/clang/commit/7a83421776416d6a9044fb03b5b02208b47646c1) and also checked by OpenCL CTS
 * (https://github.com/KhronosGroup/OpenCL-CTS/blob/cl12_trunk/test_conformance/integer_ops/verification_and_generation_functions.c)
 * see also https://stackoverflow.com/questions/51919757/left-shift-by-negative-value-in-opencl
 */

template <typename T, typename R = typename std::common_type<T, int>::type>
static R checkShiftLeft(T arg1, T arg2)
{
    return static_cast<R>(arg1 << (vc4c::bit_cast<T, std::make_unsigned_t<T>>(arg2) % (sizeof(T) * CHAR_BIT)));
}

template <typename T, typename R = typename std::common_type<T, int>::type>
static R checkShiftRight(T arg1, T arg2)
{
    return static_cast<R>(arg1 >> (vc4c::bit_cast<T, std::make_unsigned_t<T>>(arg2) % (sizeof(T) * CHAR_BIT)));
}

template <typename T>
static T checkSelectScalar(T a, T b)
{
    return a != 0 ? b : a;
};

template <typename T>
static T checkSelectVector(T a, T b)
{
    auto signedVal = vc4c::bit_cast<T, std::make_signed_t<T>>(a);
    return signedVal < 0 ? b : a;
};

template <typename T>
static T checkTrinaryScalar(T a, T b)
{
    return (a < b) != 0 ? a : b;
};

template <typename T>
static T checkTrinaryVector(T a, T b)
{
    // OpenCL 1.2, 6.3.i: "exp1 ? exp2 : exp3 [...] If the result is a vector value, then this is equivalent to calling
    // select(exp3, exp2, exp1)"
    std::make_signed_t<T> signedVal =
        vc4c::bit_cast<T, std::make_signed_t<T>>(a) - vc4c::bit_cast<T, std::make_signed_t<T>>(b);
    // a - b ? a : b -> select(b, a, a - b) -> MSB(a - b) ? a : b
    return signedVal < 0 ? a : b;
};

void TestArithmetic::testSignedIntUnaryPlus()
{
    testUnaryOperation<int>(
        config, "-DTYPE=int16 -DOP=+", [](int i) -> int { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntUnaryPlus()
{
    testUnaryOperation<unsigned>(
        config, "-DTYPE=uint16 -DOP=+", [](unsigned i) -> unsigned { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortUnaryPlus()
{
    testUnaryOperation<short>(
        config, "-DTYPE=short16 -DOP=+", [](short i) -> short { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortUnaryPlus()
{
    testUnaryOperation<unsigned short>(
        config, "-DTYPE=ushort16 -DOP=+", [](unsigned short i) -> unsigned short { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharUnaryPlus()
{
    testUnaryOperation<char>(
        config, "-DTYPE=char16 -DOP=+", [](char i) -> char { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharUnaryPlus()
{
    testUnaryOperation<unsigned char>(
        config, "-DTYPE=uchar16 -DOP=+", [](unsigned char i) -> unsigned char { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testFloatUnaryPlus()
{
    testUnaryOperation<float>(
        config, "-DTYPE=float16 -DOP=+", [](float i) -> float { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongUnaryPlus()
{
    testUnaryOperation<int64_t>(
        config, "-DTYPE=long16 -DOP=+", [](int64_t i) -> int64_t { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongUnaryPlus()
{
    testUnaryOperation<uint64_t>(
        config, "-DTYPE=ulong16 -DOP=+", [](uint64_t i) -> uint64_t { return +i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntUnaryMinus()
{
    testUnaryOperation<int>(
        config, "-DTYPE=int16 -DOP=-", [](int i) -> int { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntUnaryMinus()
{
    testUnaryOperation<unsigned>(
        config, "-DTYPE=uint16 -DOP=-", [](unsigned i) -> unsigned { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortUnaryMinus()
{
    testUnaryOperation<short>(
        config, "-DTYPE=short16 -DOP=-", [](short i) -> short { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortUnaryMinus()
{
    testUnaryOperation<unsigned short>(
        config, "-DTYPE=ushort16 -DOP=-", [](unsigned short i) -> unsigned short { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharUnaryMinus()
{
    testUnaryOperation<char>(
        config, "-DTYPE=char16 -DOP=-", [](char i) -> char { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharUnaryMinus()
{
    testUnaryOperation<unsigned char>(
        config, "-DTYPE=uchar16 -DOP=-", [](unsigned char i) -> unsigned char { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testFloatUnaryMinus()
{
    testUnaryOperation<float>(
        config, "-DTYPE=float16 -DOP=-", [](float i) -> float { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongUnaryMinus()
{
    testUnaryOperation<int64_t>(
        config, "-DTYPE=long16 -DOP=-", [](int64_t i) -> int64_t { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongUnaryMinus()
{
    testUnaryOperation<uint64_t>(
        config, "-DTYPE=long16 -DOP=-", [](uint64_t i) -> uint64_t { return -i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntAddition()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=+", std::plus<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntAddition()
{
    testBinaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=+", std::plus<unsigned>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortAddition()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=+", std::plus<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortAddition()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=+", std::plus<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharAddition()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=+", std::plus<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharAddition()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=+", std::plus<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testFloatAddition()
{
    testBinaryOperation<float>(config, "-DTYPE=float16 -DOP=+",
        vc4c::FlushDenormsAndRoundToZero<float, float, std::plus<float>>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongAddition()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=+", std::plus<int64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongAddition()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=+", std::plus<uint64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntSubtraction()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=-", std::minus<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntSubtraction()
{
    testBinaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=-", std::minus<unsigned>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortSubtraction()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=-", std::minus<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortSubtraction()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=-", std::minus<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharSubtraction()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=-", std::minus<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharSubtraction()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=-", std::minus<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testFloatSubtraction()
{
    testBinaryOperation<float>(config, "-DTYPE=float16 -DOP=-",
        vc4c::FlushDenormsAndRoundToZero<float, float, std::minus<float>>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongSubtraction()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=-", std::minus<int64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongSubtraction()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=-", std::minus<uint64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntMultiplication()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=*", std::multiplies<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortMultiplication()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=*", std::multiplies<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharMultiplication()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=*", std::multiplies<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntMultiplication()
{
    testBinaryOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=*", std::multiplies<unsigned int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortMultiplication()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=*", std::multiplies<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharMultiplication()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=*", std::multiplies<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatingPointMultiplication()
{
    testBinaryOperation<float>(config, "-DTYPE=float16 -DOP=*", std::multiplies<float>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntDivision()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=/", std::divides<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortDivision()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=/", std::divides<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharDivision()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=/", std::divides<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntDivision()
{
    testBinaryOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=/", std::divides<unsigned int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortDivision()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=/", std::divides<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharDivision()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=/", std::divides<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatingPointDivision()
{
    testBinaryOperation<float, CompareULP<3>>(config, "-DTYPE=float16 -DOP=/",
        vc4c::FlushDenormsAndRoundToZero<float, float, std::divides<float>>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntModulo()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=%", std::modulus<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortModulo()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=%", std::modulus<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharModulo()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=%", std::modulus<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntModulo()
{
    testBinaryOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=%", std::modulus<unsigned int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortModulo()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=%", std::modulus<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharModulo()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=%", std::modulus<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntIncrement()
{
    testUnaryOperation<int>(
        config, "-DTYPE=int16 -DOP=++", [](int i) -> int { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntIncrement()
{
    testUnaryOperation<unsigned>(
        config, "-DTYPE=uint16 -DOP=++", [](unsigned i) -> unsigned { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortIncrement()
{
    testUnaryOperation<short>(
        config, "-DTYPE=short16 -DOP=++", [](short i) -> short { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortIncrement()
{
    testUnaryOperation<unsigned short>(
        config, "-DTYPE=ushort16 -DOP=++", [](unsigned short i) -> unsigned short { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharIncrement()
{
    testUnaryOperation<char>(
        config, "-DTYPE=char16 -DOP=++", [](char i) -> char { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharIncrement()
{
    testUnaryOperation<unsigned char>(
        config, "-DTYPE=uchar16 -DOP=++", [](unsigned char i) -> unsigned char { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongIncrement()
{
    testUnaryOperation<int64_t>(
        config, "-DTYPE=long16 -DOP=++", [](int64_t i) -> int64_t { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongIncrement()
{
    testUnaryOperation<uint64_t>(
        config, "-DTYPE=ulong16 -DOP=++", [](uint64_t i) -> uint64_t { return ++i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntDecrement()
{
    testUnaryOperation<int>(
        config, "-DTYPE=int16 -DOP=--", [](int i) -> int { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntDecrement()
{
    testUnaryOperation<unsigned>(
        config, "-DTYPE=uint16 -DOP=--", [](unsigned i) -> unsigned { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortDecrement()
{
    testUnaryOperation<short>(
        config, "-DTYPE=short16 -DOP=--", [](short i) -> short { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortDecrement()
{
    testUnaryOperation<unsigned short>(
        config, "-DTYPE=ushort16 -DOP=--", [](unsigned short i) -> unsigned short { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharDecrement()
{
    testUnaryOperation<char>(
        config, "-DTYPE=char16 -DOP=--", [](char i) -> char { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharDecrement()
{
    testUnaryOperation<unsigned char>(
        config, "-DTYPE=uchar16 -DOP=--", [](unsigned char i) -> unsigned char { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongDecrement()
{
    testUnaryOperation<int64_t>(
        config, "-DTYPE=long16 -DOP=--", [](int64_t i) -> int64_t { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongDecrement()
{
    testUnaryOperation<uint64_t>(
        config, "-DTYPE=ulong16 -DOP=--", [](uint64_t i) -> uint64_t { return --i; },
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testIntegerEquality()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP===", checkRelation<std::equal_to<int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testShortEquality()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP===", checkRelation<std::equal_to<short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testCharEquality()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP===", checkRelation<std::equal_to<char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatEquality()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP===", checkRelation<std::equal_to<float>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testLongEquality()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP===", checkRelation<std::equal_to<int64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testIntegerInequality()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=!=", checkRelation<std::not_equal_to<int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testShortInequality()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=!=", checkRelation<std::not_equal_to<short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testCharInequality()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=!=", checkRelation<std::not_equal_to<char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatInequality()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=!=", checkRelation<std::not_equal_to<float>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testLongInequality()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=!=", checkRelation<std::not_equal_to<int64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntGreater()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=>", checkRelation<std::greater<int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortGreater()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=>", checkRelation<std::greater<short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharGreater()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=>", checkRelation<std::greater<char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntGreater()
{
    testRelationalOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=>", checkRelation<std::greater<unsigned int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortGreater()
{
    testRelationalOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=>",
        checkRelation<std::greater<unsigned short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharGreater()
{
    testRelationalOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=>", checkRelation<std::greater<unsigned char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatGreater()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=>", checkRelation<std::greater<float>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongGreater()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=>", checkRelation<std::greater<int64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongGreater()
{
    testRelationalOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=>", checkRelation<std::greater<uint64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntLess()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=<", checkRelation<std::less<int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortLess()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=<", checkRelation<std::less<short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharLess()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=<", checkRelation<std::less<char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntLess()
{
    testRelationalOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=<", checkRelation<std::less<unsigned int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortLess()
{
    testRelationalOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=<", checkRelation<std::less<unsigned short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharLess()
{
    testRelationalOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=<", checkRelation<std::less<unsigned char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatLess()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=<", checkRelation<std::less<float>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongLess()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=<", checkRelation<std::less<int64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongLess()
{
    testRelationalOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=<", checkRelation<std::less<uint64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntGreaterEquals()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=>=", checkRelation<std::greater_equal<int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortGreaterEquals()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=>=", checkRelation<std::greater_equal<short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharGreaterEquals()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=>=", checkRelation<std::greater_equal<char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntGreaterEquals()
{
    testRelationalOperation<unsigned int>(config,
        "-DTYPE=uint16 -DOP=>=", checkRelation<std::greater_equal<unsigned int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortGreaterEquals()
{
    testRelationalOperation<unsigned short>(config,
        "-DTYPE=ushort16 -DOP=>=", checkRelation<std::greater_equal<unsigned short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharGreaterEquals()
{
    testRelationalOperation<unsigned char>(config,
        "-DTYPE=uchar16 -DOP=>=", checkRelation<std::greater_equal<unsigned char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatGreaterEquals()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=>=", checkRelation<std::greater_equal<float>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongGreaterEquals()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=>=", checkRelation<std::greater_equal<int64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongGreaterEquals()
{
    testRelationalOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=>=", checkRelation<std::greater_equal<uint64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntLessEquals()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=<=", checkRelation<std::less_equal<int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortLessEquals()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=<=", checkRelation<std::less_equal<short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharLessEquals()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=<=", checkRelation<std::less_equal<char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntLessEquals()
{
    testRelationalOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=<=", checkRelation<std::less_equal<unsigned int>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortLessEquals()
{
    testRelationalOperation<unsigned short>(config,
        "-DTYPE=ushort16 -DOP=<=", checkRelation<std::less_equal<unsigned short>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharLessEquals()
{
    testRelationalOperation<unsigned char>(config,
        "-DTYPE=uchar16 -DOP=<=", checkRelation<std::less_equal<unsigned char>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatLessEquals()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=<=", checkRelation<std::less_equal<float>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongLessEquals()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=<=", checkRelation<std::less_equal<int64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongLessEquals()
{
    testRelationalOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=<=", checkRelation<std::less_equal<uint64_t>>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntSelectScalar()
{
    testSelectOperation<int, 1>(config, "-DTYPE=int", checkSelectScalar<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntSelectVector()
{
    testSelectOperation<int, 16>(config, "-DTYPE=int16", checkSelectVector<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortSelectScalar()
{
    testSelectOperation<short, 1>(config, "-DTYPE=short", checkSelectScalar<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortSelectVector()
{
    testSelectOperation<short, 16>(config, "-DTYPE=short16", checkSelectVector<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharSelectScalar()
{
    testSelectOperation<char, 1>(config, "-DTYPE=char", checkSelectScalar<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharSelectVector()
{
    testSelectOperation<char, 16>(config, "-DTYPE=char16", checkSelectVector<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntSelectScalar()
{
    testSelectOperation<unsigned, 1>(config, "-DTYPE=uint", checkSelectScalar<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntSelectVector()
{
    testSelectOperation<unsigned int, 16>(config, "-DTYPE=uint16", checkSelectVector<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortSelectScalar()
{
    testSelectOperation<unsigned short, 1>(config, "-DTYPE=ushort", checkSelectScalar<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortSelectVector()
{
    testSelectOperation<unsigned short, 16>(config, "-DTYPE=ushort16", checkSelectVector<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharSelectScalar()
{
    testSelectOperation<unsigned char, 1>(config, "-DTYPE=uchar", checkSelectScalar<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharSelectVector()
{
    testSelectOperation<unsigned char, 16>(config, "-DTYPE=uchar16", checkSelectVector<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongSelectScalar()
{
    testSelectOperation<int64_t, 1>(config, "-DTYPE=long", checkSelectScalar<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongSelectVector()
{
    testSelectOperation<int64_t, 16>(config, "-DTYPE=long16", checkSelectVector<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongSelectScalar()
{
    testSelectOperation<uint64_t, 1>(config, "-DTYPE=ulong", checkSelectScalar<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongSelectVector()
{
    testSelectOperation<uint64_t, 16>(config, "-DTYPE=ulong16", checkSelectVector<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntAnd()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=&&", checkAnd<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortAnd()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=&&", checkAnd<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharAnd()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=&&", checkAnd<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntAnd()
{
    testRelationalOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=&&", checkAnd<unsigned int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortAnd()
{
    testRelationalOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=&&", checkAnd<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharAnd()
{
    testRelationalOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=&&", checkAnd<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatAnd()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=&&", checkAnd<float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongAnd()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=&&", checkAnd<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongAnd()
{
    testRelationalOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=&&", checkAnd<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntOr()
{
    testRelationalOperation<int>(config, "-DTYPE=int16 -DOP=||", checkOr<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortOr()
{
    testRelationalOperation<short>(config, "-DTYPE=short16 -DOP=||", checkOr<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharOr()
{
    testRelationalOperation<char>(config, "-DTYPE=char16 -DOP=||", checkOr<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntOr()
{
    testRelationalOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=||", checkOr<unsigned int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortOr()
{
    testRelationalOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=||", checkOr<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharOr()
{
    testRelationalOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=||", checkOr<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatOr()
{
    testRelationalOperation<float>(config, "-DTYPE=float16 -DOP=||", checkOr<float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongOr()
{
    testRelationalOperation<int64_t>(config, "-DTYPE=long16 -DOP=||", checkOr<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongOr()
{
    testRelationalOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=||", checkOr<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntNot()
{
    testUnaryOperation<int, int>(config, "-DIN=int16 -DOUT=int16 -DOP=!", checkNot<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntNot()
{
    testUnaryOperation<unsigned, int>(config, "-DIN=uint16 -DOUT=int16 -DOP=!", checkNot<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortNot()
{
    testUnaryOperation<short, int>(config, "-DIN=short16 -DOUT=short16 -DOP=!", checkNot<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortNot()
{
    testUnaryOperation<unsigned short, int>(config, "-DIN=ushort16 -DOUT=short16 -DOP=!", checkNot<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharNot()
{
    testUnaryOperation<char, int>(config, "-DIN=char16 -DOUT=char16 -DOP=!", checkNot<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharNot()
{
    testUnaryOperation<unsigned char, int>(config, "-DIN=uchar16 -DOUT=char16 -DOP=!", checkNot<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testFloatNot()
{
    testUnaryOperation<float, int>(config, "-DIN=float16 -DOUT=int16 -DOP=!", checkNot<float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedLongNot()
{
    testUnaryOperation<int64_t, int>(config, "-DIN=long16 -DOUT=long16 -DOP=!", checkNot<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedLongNot()
{
    testUnaryOperation<uint64_t, int>(config, "-DIN=ulong16 -DOUT=long16 -DOP=!", checkNot<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntBitNot()
{
    testUnaryOperation<int>(config, "-DTYPE=int16 -DOP=~", std::bit_not<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntBitNot()
{
    testUnaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=~", std::bit_not<unsigned>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortBitNot()
{
    testUnaryOperation<short>(config, "-DTYPE=short16 -DOP=~", std::bit_not<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortBitNot()
{
    testUnaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=~", std::bit_not<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharBitNot()
{
    testUnaryOperation<char>(config, "-DTYPE=char16 -DOP=~", std::bit_not<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharBitNot()
{
    testUnaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=~", std::bit_not<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongBitNot()
{
    testUnaryOperation<int64_t, int>(config, "-DTYPE=long16 -DOP=~", std::bit_not<int64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongBitNot()
{
    testUnaryOperation<uint64_t, int>(config, "-DTYPE=ulong16 -DOP=~", std::bit_not<uint64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntBitAnd()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=&", std::bit_and<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntBitAnd()
{
    testBinaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=&", std::bit_and<unsigned>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortBitAnd()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=&", std::bit_and<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortBitAnd()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=&", std::bit_and<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharBitAnd()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=&", std::bit_and<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharBitAnd()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=&", std::bit_and<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongBitAnd()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=&", std::bit_and<int64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongBitAnd()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=&", std::bit_and<uint64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntBitOr()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=|", std::bit_or<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntBitOr()
{
    testBinaryOperation<unsigned int>(config, "-DTYPE=uint16 -DOP=|", std::bit_or<unsigned int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortBitOr()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=|", std::bit_or<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortBitOr()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=|", std::bit_or<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharBitOr()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=|", std::bit_or<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharBitOr()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=|", std::bit_or<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongBitOr()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=|", std::bit_or<int64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongBitOr()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=|", std::bit_or<uint64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntBitXor()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=^", std::bit_xor<int>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntBitXor()
{
    testBinaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=^", std::bit_xor<unsigned>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortBitXor()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=^", std::bit_xor<short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortBitXor()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=^", std::bit_xor<unsigned short>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharBitXor()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=^", std::bit_xor<char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharBitXor()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=^", std::bit_xor<unsigned char>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongBitXor()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=^", std::bit_xor<int64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongBitXor()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=^", std::bit_xor<uint64_t>{},
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntBitShiftLeft()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=<<", checkShiftLeft<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntBitShiftLeft()
{
    testBinaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=<<", checkShiftLeft<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortBitShiftLeft()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=<<", checkShiftLeft<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortBitShiftLeft()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=<<", checkShiftLeft<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharBitShiftLeft()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=<<", checkShiftLeft<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharBitShiftLeft()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=<<", checkShiftLeft<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongBitShiftLeft()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=<<", checkShiftLeft<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongBitShiftLeft()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=<<", checkShiftLeft<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntBitShiftRight()
{
    testBinaryOperation<int>(config, "-DTYPE=int16 -DOP=>>", checkShiftRight<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntBitShiftRight()
{
    testBinaryOperation<unsigned>(config, "-DTYPE=uint16 -DOP=>>", checkShiftRight<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortBitShiftRight()
{
    testBinaryOperation<short>(config, "-DTYPE=short16 -DOP=>>", checkShiftRight<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortBitShiftRight()
{
    testBinaryOperation<unsigned short>(config, "-DTYPE=ushort16 -DOP=>>", checkShiftRight<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharBitShiftRight()
{
    testBinaryOperation<char>(config, "-DTYPE=char16 -DOP=>>", checkShiftRight<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharBitShiftRight()
{
    testBinaryOperation<unsigned char>(config, "-DTYPE=uchar16 -DOP=>>", checkShiftRight<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongBitShiftRight()
{
    testBinaryOperation<int64_t>(config, "-DTYPE=long16 -DOP=>>", checkShiftRight<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongBitShiftRight()
{
    testBinaryOperation<uint64_t>(config, "-DTYPE=ulong16 -DOP=>>", checkShiftRight<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntTrinaryScalar()
{
    testTernaryOperator<int, 1>(config, "-DTYPE=int -DOP=<", checkTrinaryScalar<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedIntTrinaryVector()
{
    testTernaryOperator<int, 16>(config, "-DTYPE=int16 -DOP=-", checkTrinaryVector<int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortTrinaryScalar()
{
    testTernaryOperator<short, 1>(config, "-DTYPE=short -DOP=<", checkTrinaryScalar<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedShortTrinaryVector()
{
    testTernaryOperator<short, 16>(config, "-DTYPE=short16 -DOP=-", checkTrinaryVector<short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharTrinaryScalar()
{
    testTernaryOperator<char, 1>(config, "-DTYPE=char -DOP=<", checkTrinaryScalar<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedCharTrinaryVector()
{
    testTernaryOperator<char, 16>(config, "-DTYPE=char16 -DOP=-", checkTrinaryVector<char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongTrinaryScalar()
{
    testTernaryOperator<int64_t, 1>(config, "-DTYPE=long -DOP=<", checkTrinaryScalar<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testSignedLongTrinaryVector()
{
    testTernaryOperator<int64_t, 16>(config, "-DTYPE=long16 -DOP=-", checkTrinaryVector<int64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntTrinaryScalar()
{
    testTernaryOperator<unsigned, 1>(config, "-DTYPE=uint -DOP=<", checkTrinaryScalar<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedIntTrinaryVector()
{
    testTernaryOperator<unsigned, 16>(config, "-DTYPE=uint16 -DOP=-", checkTrinaryVector<unsigned>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortTrinaryScalar()
{
    testTernaryOperator<unsigned short, 1>(config, "-DTYPE=ushort -DOP=<", checkTrinaryScalar<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedShortTrinaryVector()
{
    testTernaryOperator<unsigned short, 16>(config, "-DTYPE=ushort16 -DOP=-", checkTrinaryVector<unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharTrinaryScalar()
{
    testTernaryOperator<unsigned char, 1>(config, "-DTYPE=uchar -DOP=<", checkTrinaryScalar<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedCharTrinaryVector()
{
    testTernaryOperator<unsigned char, 16>(config, "-DTYPE=uchar16 -DOP=-", checkTrinaryVector<unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongTrinaryScalar()
{
    testTernaryOperator<uint64_t, 1>(config, "-DTYPE=ulong -DOP=<", checkTrinaryScalar<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestArithmetic::testUnsignedLongTrinaryVector()
{
    testTernaryOperator<uint64_t, 16>(config, "-DTYPE=ulong16 -DOP=-", checkTrinaryVector<uint64_t>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
