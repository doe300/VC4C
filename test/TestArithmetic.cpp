/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestArithmetic.h"

#include "Compiler.h"
#include "VC4C.h"
#include "tools.h"

#include <cstring>
#include <limits>
#include <random>
#include <sstream>

template <typename T, std::size_t N, T min = std::numeric_limits<T>::min(), T max = std::numeric_limits<T>::max()>
static std::array<T, N> generateInput(bool allowNull)
{
    std::array<T, N> arr;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<T> dis(min, max);

    for(std::size_t i = 0; i < N; ++i)
    {
        T tmp;
        do
        {
            // to prevent division by zero
            tmp = dis(gen);
        } while(!allowNull && tmp == 0);
        arr[i] = tmp;
    }

    return arr;
}

template <std::size_t N, typename T = float>
static std::array<float, N> generateInput(bool allowNull)
{
    std::array<float, N> arr;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<float> dis(
        static_cast<float>(std::numeric_limits<T>::min()), static_cast<float>(std::numeric_limits<T>::max()));

    for(std::size_t i = 0; i < N; ++i)
    {
        float tmp;
        do
        {
            // to prevent division by zero
            tmp = dis(gen);
        } while(!allowNull && tmp == 0.0f);
        arr[i] = tmp;
    }
    return arr;
}

static const std::string BINARY_OPERATION = R"(
__kernel void test(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
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

static const std::string UNARY_OPERATION = R"(
__kernel void test(__global int16* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(OP in[gid]);
}
)";

static const std::string CONVERSION_OPERATION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define CONCAT(A, B) CONCAT_(A, B)
#define CONCAT_(A, B) A##B

__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = CONCAT(convert_,OUT)(in[gid]);
}
)";

static const std::string CAST_OPERATION = R"(
__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = as_##OUT(in[gid]);
}
)";

template <typename T, typename I, std::size_t N>
static void checkBinaryResults(const std::array<I, N>& input0, const std::array<I, N>& input1,
    const std::array<T, N>& output, const std::function<T(I, I)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    for(std::size_t i = 0; i < N; ++i)
    {
        if(output[i] != op(input0[i], input1[i]))
        {
            auto result = std::to_string(output[i]);
            auto expected = std::to_string(input0[i]) + " " + opName + " " + std::to_string(input1[i]) + " = " +
                std::to_string(op(input0[i], input1[i]));
            onError(expected, result);
        }
    }
}

template <typename T, typename I, std::size_t N>
static void checkUnaryResults(const std::array<I, N>& input, const std::array<T, N>& output,
    const std::function<T(I)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    for(std::size_t i = 0; i < N; ++i)
    {
        if(output[i] != op(input[i]))
        {
            auto result = std::to_string(output[i]);
            auto expected = opName + " " + std::to_string(input[i]) + " = " + std::to_string(op(input[i]));
            onError(expected, result);
        }
    }
}

TestArithmetic::TestArithmetic(const vc4c::Configuration& config) : config(config)
{
    TEST_ADD(TestArithmetic::testSignedIntMultiplication);
    TEST_ADD(TestArithmetic::testSignedShortMultiplication);
    TEST_ADD(TestArithmetic::testSignedIntMultiplication);
    TEST_ADD(TestArithmetic::testSignedShortMultiplication);
    TEST_ADD(TestArithmetic::testSignedCharMultiplication);
    TEST_ADD(TestArithmetic::testUnsignedIntMultiplication);
    TEST_ADD(TestArithmetic::testUnsignedShortMultiplication);
    TEST_ADD(TestArithmetic::testUnsignedCharMultiplication);
    TEST_ADD(TestArithmetic::testSignedIntDivision);
    TEST_ADD(TestArithmetic::testSignedShortDivision);
    TEST_ADD(TestArithmetic::testSignedCharDivision);
    TEST_ADD(TestArithmetic::testUnsignedIntDivision);
    TEST_ADD(TestArithmetic::testUnsignedShortDivision);
    TEST_ADD(TestArithmetic::testUnsignedCharDivision);
    TEST_ADD(TestArithmetic::testSignedIntModulo);
    TEST_ADD(TestArithmetic::testSignedShortModulo);
    TEST_ADD(TestArithmetic::testSignedCharModulo);
    TEST_ADD(TestArithmetic::testUnsignedIntModulo);
    TEST_ADD(TestArithmetic::testUnsignedShortModulo);
    TEST_ADD(TestArithmetic::testUnsignedCharModulo);
    TEST_ADD(TestArithmetic::testFloatingPointDivision);

    TEST_ADD(TestArithmetic::testIntegerEquality);
    TEST_ADD(TestArithmetic::testShortEquality);
    TEST_ADD(TestArithmetic::testCharEquality);
    TEST_ADD(TestArithmetic::testFloatEquality);
    TEST_ADD(TestArithmetic::testIntegerInequality);
    TEST_ADD(TestArithmetic::testShortInequality);
    TEST_ADD(TestArithmetic::testCharInequality);
    TEST_ADD(TestArithmetic::testFloatInequality);
    TEST_ADD(TestArithmetic::testSignedIntGreater);
    TEST_ADD(TestArithmetic::testSignedShortGreater);
    TEST_ADD(TestArithmetic::testSignedCharGreater);
    TEST_ADD(TestArithmetic::testUnsignedIntGreater);
    TEST_ADD(TestArithmetic::testUnsignedShortGreater);
    TEST_ADD(TestArithmetic::testUnsignedCharGreater);
    TEST_ADD(TestArithmetic::testFloatGreater);
    TEST_ADD(TestArithmetic::testSignedIntLess);
    TEST_ADD(TestArithmetic::testSignedShortLess);
    TEST_ADD(TestArithmetic::testSignedCharLess);
    TEST_ADD(TestArithmetic::testUnsignedIntLess);
    TEST_ADD(TestArithmetic::testUnsignedShortLess);
    TEST_ADD(TestArithmetic::testUnsignedCharLess);
    TEST_ADD(TestArithmetic::testFloatLess);
    TEST_ADD(TestArithmetic::testSignedIntGreaterEquals);
    TEST_ADD(TestArithmetic::testSignedShortGreaterEquals);
    TEST_ADD(TestArithmetic::testSignedCharGreaterEquals);
    TEST_ADD(TestArithmetic::testUnsignedIntGreaterEquals);
    TEST_ADD(TestArithmetic::testUnsignedShortGreaterEquals);
    TEST_ADD(TestArithmetic::testUnsignedCharGreaterEquals);
    TEST_ADD(TestArithmetic::testFloatGreaterEquals);
    TEST_ADD(TestArithmetic::testSignedIntLessEquals);
    TEST_ADD(TestArithmetic::testSignedShortLessEquals);
    TEST_ADD(TestArithmetic::testSignedCharLessEquals);
    TEST_ADD(TestArithmetic::testUnsignedIntLessEquals);
    TEST_ADD(TestArithmetic::testUnsignedShortLessEquals);
    TEST_ADD(TestArithmetic::testUnsignedCharLessEquals);
    TEST_ADD(TestArithmetic::testFloatLessEquals);

    TEST_ADD(TestArithmetic::testSignedIntSelection);
    TEST_ADD(TestArithmetic::testSignedShortSelection);
    TEST_ADD(TestArithmetic::testSignedCharSelection);
    TEST_ADD(TestArithmetic::testUnsignedIntSelection);
    TEST_ADD(TestArithmetic::testUnsignedShortSelection);
    TEST_ADD(TestArithmetic::testUnsignedCharSelection);
    TEST_ADD(TestArithmetic::testSignedIntAnd);
    TEST_ADD(TestArithmetic::testSignedShortAnd);
    TEST_ADD(TestArithmetic::testSignedCharAnd);
    TEST_ADD(TestArithmetic::testUnsignedIntAnd);
    TEST_ADD(TestArithmetic::testUnsignedShortAnd);
    TEST_ADD(TestArithmetic::testUnsignedCharAnd);
    TEST_ADD(TestArithmetic::testFloatAnd);
    TEST_ADD(TestArithmetic::testSignedIntOr);
    TEST_ADD(TestArithmetic::testSignedShortOr);
    TEST_ADD(TestArithmetic::testSignedCharOr);
    TEST_ADD(TestArithmetic::testUnsignedIntOr);
    TEST_ADD(TestArithmetic::testUnsignedShortOr);
    TEST_ADD(TestArithmetic::testUnsignedCharOr);
    TEST_ADD(TestArithmetic::testFloatOr);

    TEST_ADD(TestArithmetic::testSignedTruncation);
    TEST_ADD(TestArithmetic::testUnsignedTruncation);
    TEST_ADD(TestArithmetic::testSignExtension);
    TEST_ADD(TestArithmetic::testZeroExtension);
    TEST_ADD(TestArithmetic::testSignedIntToFloat);
    TEST_ADD(TestArithmetic::testSignedShortToFloat);
    TEST_ADD(TestArithmetic::testSignedCharToFloat);
    TEST_ADD(TestArithmetic::testUnsignedIntToFloat);
    TEST_ADD(TestArithmetic::testUnsignedShortToFloat);
    TEST_ADD(TestArithmetic::testUnsignedCharToFloat);
    TEST_ADD(TestArithmetic::testFloatToSignedInt);
    TEST_ADD(TestArithmetic::testFloatToSignedShort);
    TEST_ADD(TestArithmetic::testFloatToSignedChar);
    TEST_ADD(TestArithmetic::testFloatToUnsignedInt);
    TEST_ADD(TestArithmetic::testFloatToUnsignedShort);
    TEST_ADD(TestArithmetic::testFloatToUnsignedChar);
}

void TestArithmetic::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

static void compileBuffer(
    vc4c::Configuration& config, std::stringstream& buffer, const std::string& source, const std::string& options)
{
    config.outputMode = vc4c::OutputMode::BINARY;
    config.writeKernelInfo = true;
    std::istringstream input(source);
    vc4c::Compiler::compile(input, buffer, config, options);
}

template <std::size_t N, typename I, typename O>
static void copyConvert(const I& in, O& out)
{
    if(out.size() < N)
        throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL, "Invalid container size for copy");
    auto base = reinterpret_cast<const typename O::value_type*>(in.data());
    std::copy(base, base + N, out.data());
}

template <typename T, typename R, std::size_t V, std::size_t L, std::size_t G = 1>
static std::array<R, V * L * G> runEmulation(
    std::stringstream& codeBuffer, const std::vector<std::array<T, V * L * G>>& inputs)
{
    using namespace vc4c::tools;

    std::vector<std::pair<uint32_t, vc4c::Optional<std::vector<uint32_t>>>> parameter;
    parameter.emplace_back(std::make_pair(0, std::vector<uint32_t>(V * L * G * sizeof(R) / sizeof(uint32_t))));
    for(const auto& input : inputs)
    {
        parameter.emplace_back(std::make_pair(0, std::vector<uint32_t>(V * L * G * sizeof(T) / sizeof(uint32_t))));
        copyConvert<V * L * G * sizeof(T) / sizeof(uint32_t)>(input, parameter.back().second.value());
    }

    WorkGroupConfig workGroups;
    workGroups.dimensions = 1;
    workGroups.localSizes[0] = L;
    workGroups.numGroups[0] = G;

    EmulationData data(codeBuffer, "test", parameter, workGroups);

    auto result = emulate(data);

    if(!result.executionSuccessful)
        throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL, "Kernel execution failed");

    std::array<R, V * L * G> output;
    copyConvert<V * L * G>(result.results[0].second.value(), output);
    return output;
}

template <typename T>
static void testBinaryOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<T(T, T)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_OPERATION, options);

    auto in0 = generateInput<T, 16 * 12>(false);
    auto in1 = generateInput<T, 16 * 12>(false);

    auto out = runEmulation<T, T, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<T>(in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <>
void testBinaryOperation<float>(vc4c::Configuration& config, const std::string& options,
    const std::function<float(float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, BINARY_OPERATION, options);

    auto in0 = generateInput<16 * 12>(false);
    auto in1 = generateInput<16 * 12>(false);

    auto out = runEmulation<float, float, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<float>(in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
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

template <>
void testRelationalOperation<float>(vc4c::Configuration& config, const std::string& options,
    const std::function<int(float, float)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, RELATIONAL_OPERATION, options);

    auto in0 = generateInput<16 * 12>(true);
    auto in1 = generateInput<16 * 12>(true);

    auto out = runEmulation<float, int, 16, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<int, float, 16 * 12>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename T>
static void testSelectionOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, SELECTION_OPERATION, options);

    auto in0 = generateInput<T, 16 * 12>(true);
    auto in1 = generateInput<T, 16 * 12>(true);

    auto out = runEmulation<T, T, 16, 12>(code, {in0, in1});
    auto op = [](T a, T b) -> T {
        typename std::make_signed<T>::type signedVal;
        std::memcpy(&signedVal, &a, sizeof(T));
        return signedVal < 0 ? b : a;
    };
    checkBinaryResults<T, T>(in0, in1, out, op, "select", onError);
}

template <typename I, typename O>
static void testConversionOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<O(I)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, CONVERSION_OPERATION, options);

    auto in = generateInput<I, 16 * 12>(true);

    auto out = runEmulation<I, O, 16, 12>(code, {in});
    checkUnaryResults<O, I, 16 * 12>(in, out, op, "convert", onError);
}

template <typename O>
static void testConversionOperation(vc4c::Configuration& config, const std::string& options,
    const std::function<O(float)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, CONVERSION_OPERATION, options);

    // values out of the range of O (the integer type) are implementation-defined
    auto in = generateInput<16 * 12, O>(true);

    auto out = runEmulation<float, O, 16, 12>(code, {in});
    checkUnaryResults<O, float, 16 * 12>(in, out, op, "convert", onError);
}

template <typename C, typename T = typename C::first_argument_type>
static int checkRelation(T arg1, T arg2)
{
    C c{};
    return c(arg1, arg2) ? -1 : 0;
}

template <typename T>
static int checkAnd(T arg1, T arg2)
{
    return (arg1 != 0 && arg2 != 0) ? -1 : 0;
}

template <typename T>
static int checkOr(T arg1, T arg2)
{
    return (arg1 != 0 || arg2 != 0) ? -1 : 0;
}

template <typename T, typename S>
static T convert(S s)
{
    return static_cast<T>(s);
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

void TestArithmetic::testFloatingPointDivision()
{
    testBinaryOperation<float>(config, "-DTYPE=float16 -DOP=/", std::divides<float>{},
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

void TestArithmetic::testSignedIntSelection()
{
    testSelectionOperation<int>(config, "-DTYPE=int16",
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortSelection()
{
    testSelectionOperation<short>(config, "-DTYPE=short16",
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharSelection()
{
    testSelectionOperation<char>(config, "-DTYPE=char16",
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntSelection()
{
    testSelectionOperation<unsigned int>(config, "-DTYPE=uint16",
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortSelection()
{
    testSelectionOperation<unsigned short>(config, "-DTYPE=ushort16",
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharSelection()
{
    testSelectionOperation<unsigned char>(config, "-DTYPE=uchar16",
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

void TestArithmetic::testSignedTruncation()
{
    testConversionOperation<int, short>(config, "-DIN=int16 -DOUT=short16", convert<short, int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<int, char>(config, "-DIN=int16 -DOUT=char16", convert<char, int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<short, char>(config, "-DIN=short16 -DOUT=char16", convert<char, short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedTruncation()
{
    testConversionOperation<unsigned int, unsigned short>(config, "-DIN=uint16 -DOUT=ushort16",
        convert<unsigned short, unsigned int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned int, unsigned char>(config, "-DIN=uint16 -DOUT=uchar16",
        convert<unsigned char, unsigned int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned short, unsigned char>(config, "-DIN=ushort16 -DOUT=uchar16",
        convert<unsigned char, unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignExtension()
{
    testConversionOperation<short, int>(config, "-DIN=short16 -DOUT=int16", convert<int, short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<char, int>(config, "-DIN=char16 -DOUT=int16", convert<int, char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<char, short>(config, "-DIN=char16 -DOUT=short16", convert<short, char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testZeroExtension()
{
    testConversionOperation<unsigned short, unsigned int>(config, "-DIN=ushort16 -DOUT=uint16",
        convert<unsigned int, unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned char, unsigned int>(config, "-DIN=uchar16 -DOUT=uint16",
        convert<unsigned int, unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
    testConversionOperation<unsigned char, unsigned short>(config, "-DIN=uchar16 -DOUT=ushort16",
        convert<unsigned short, unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedIntToFloat()
{
    testConversionOperation<int, float>(config, "-DIN=int16 -DOUT=float16", convert<int, float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedShortToFloat()
{
    testConversionOperation<short, float>(config, "-DIN=short16 -DOUT=float16", convert<short, float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testSignedCharToFloat()
{
    testConversionOperation<char, float>(config, "-DIN=char16 -DOUT=float16", convert<char, float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedIntToFloat()
{
    testConversionOperation<unsigned int, float>(config, "-DIN=uint16 -DOUT=float16", convert<unsigned int, float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedShortToFloat()
{
    testConversionOperation<unsigned short, float>(config, "-DIN=ushort16 -DOUT=float16",
        convert<unsigned short, float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testUnsignedCharToFloat()
{
    testConversionOperation<unsigned char, float>(config, "-DIN=uchar16 -DOUT=float16", convert<unsigned char, float>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatToSignedInt()
{
    testConversionOperation<int>(config, "-DIN=float16 -DOUT=int16", convert<float, int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatToSignedShort()
{
    testConversionOperation<short>(config, "-DIN=float16 -DOUT=short16", convert<float, short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatToSignedChar()
{
    testConversionOperation<char>(config, "-DIN=float16 -DOUT=char16", convert<float, char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatToUnsignedInt()
{
    testConversionOperation<unsigned int>(config, "-DIN=float16 -DOUT=uint16", convert<float, unsigned int>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatToUnsignedShort()
{
    testConversionOperation<unsigned short>(config, "-DIN=float16 -DOUT=ushort16", convert<float, unsigned short>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestArithmetic::testFloatToUnsignedChar()
{
    testConversionOperation<unsigned char>(config, "-DIN=float16 -DOUT=uchar16", convert<float, unsigned char>,
        std::bind(&TestArithmetic::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}