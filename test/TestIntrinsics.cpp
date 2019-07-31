/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestIntrinsics.h"

#include "asm/OpCodes.h"
#include "emulation_helper.h"
#include "helper.h"
#include "intrinsics/Operators.h"

static const std::string UNARY_FUNCTION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define VAL(A) VAL_(A)
#define VAL_(A) A

#define FUNCTION(ret,name,arg) ret name(arg) __attribute__((overloadable))

#ifdef DEFINE_PROTOTYPE
FUNCTION(OUT,VAL(FUNC),IN);
#endif

__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string UNARY_FUNCTION_CONSTANT = R"(
// trick to allow concatenating macro (content!!) to symbol
#define VAL(A) VAL_(A)
#define VAL_(A) A

#define FUNCTION(ret,name,arg) ret name(arg) __attribute__((overloadable))

#ifdef DEFINE_PROTOTYPE
FUNCTION(OUT,VAL(FUNC),IN);
#endif

__kernel void test(__global OUT* out) {
  *out = FUNC(SOURCES);
}
)";

static const std::string BINARY_OPERATION = R"(
__kernel void test(__global OUT* out, const __global IN* in0, const __global IN* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] OP in1[gid];
}
)";

static const std::string BINARY_OPERATION_SECOND_CONTANT = R"(
__kernel void test(__global OUT* out, const __global IN* in) {
  size_t gid = get_global_id(0);
  out[gid] = in[gid] OP SOURCE;
}
)";

static const std::string BINARY_FUNCTION = R"(
// trick to allow concatenating macro (content!!) to symbol
#define VAL(A) VAL_(A)
#define VAL_(A) A

#define FUNCTION(ret,name,arg) ret name(arg,arg) __attribute__((overloadable))

#ifdef DEFINE_PROTOTYPE
FUNCTION(OUT,VAL(FUNC),IN);
#endif

__kernel void test(__global OUT* out, const __global IN* in0, const __global IN* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

static const std::string BINARY_FUNCTION_CONSTANT = R"(
// trick to allow concatenating macro (content!!) to symbol
#define VAL(A) VAL_(A)
#define VAL_(A) A

#define FUNCTION(ret,name,arg) ret name(arg,arg) __attribute__((overloadable))

#ifdef DEFINE_PROTOTYPE
FUNCTION(OUT,VAL(FUNC),IN);
#endif

__kernel void test(__global OUT* out) {
  *out = FUNC(SOURCES0, SOURCES1);
}
)";

TestIntrinsicFunctions::TestIntrinsicFunctions(const vc4c::Configuration& config) : TestEmulator(false, config)
{
    TEST_ADD(TestIntrinsicFunctions::testIntMultiplicationWithConstant);
    TEST_ADD(TestIntrinsicFunctions::testShortMultiplicationWithConstant);
    TEST_ADD(TestIntrinsicFunctions::testCharMultiplicationWithConstant);

    TEST_ADD(TestIntrinsicFunctions::testUnsignedIntMultiplicationWithConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedShortMultiplicationWithConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedCharMultiplicationWithConstant);

    TEST_ADD(TestIntrinsicFunctions::testIntDivisionByConstant);
    TEST_ADD(TestIntrinsicFunctions::testShortDivisionByConstant);
    TEST_ADD(TestIntrinsicFunctions::testCharDivisionByConstant);
    TEST_ADD(TestIntrinsicFunctions::testIntModuloByConstant);
    TEST_ADD(TestIntrinsicFunctions::testShortModuloByConstant);
    TEST_ADD(TestIntrinsicFunctions::testCharModuloByConstant);

    TEST_ADD(TestIntrinsicFunctions::testUnsignedIntDivisionByConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedShortDivisionByConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedCharDivisionByConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedIntModuloByConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedShortModuloByConstant);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedCharModuloByConstant);

    TEST_ADD(TestIntrinsicFunctions::testFloatMultiplicationWithConstant);
    TEST_ADD(TestIntrinsicFunctions::testFloatDivisionByConstant);

    TEST_ADD(TestIntrinsicFunctions::testIntToFloat);
    TEST_ADD(TestIntrinsicFunctions::testShortToFloat);
    TEST_ADD(TestIntrinsicFunctions::testCharToFloat);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedIntToFloat);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedShortToFloat);
    TEST_ADD(TestIntrinsicFunctions::testUnsignedCharToFloat);

    TEST_ADD(TestIntrinsicFunctions::testFloatToInt);
    TEST_ADD(TestIntrinsicFunctions::testFloatToShort);
    TEST_ADD(TestIntrinsicFunctions::testFloatToChar);
    TEST_ADD(TestIntrinsicFunctions::testFloatToUnsignedInt);
    TEST_ADD(TestIntrinsicFunctions::testFloatToUnsignedShort);
    TEST_ADD(TestIntrinsicFunctions::testFloatToUnsignedChar);

    TEST_ADD(TestIntrinsicFunctions::testFtoi);
    TEST_ADD(TestIntrinsicFunctions::testItof);
    TEST_ADD(TestIntrinsicFunctions::testClz);
    TEST_ADD(TestIntrinsicFunctions::testSfuRsqrt);
    TEST_ADD(TestIntrinsicFunctions::testSfuExp2);
    TEST_ADD(TestIntrinsicFunctions::testSfuLog2);
    TEST_ADD(TestIntrinsicFunctions::testSfuRecip);
    TEST_ADD(TestIntrinsicFunctions::testIsNaN);
    TEST_ADD(TestIntrinsicFunctions::testIsInfNaN);

    TEST_ADD(TestIntrinsicFunctions::testFmax);
    TEST_ADD(TestIntrinsicFunctions::testFmin);
    TEST_ADD(TestIntrinsicFunctions::testFmaxabs);
    TEST_ADD(TestIntrinsicFunctions::testFminabs);
    /* TODO
    TEST_ADD(TestIntrinsicFunctions::testAsr);
    TEST_ADD(TestIntrinsicFunctions::testRor);
    */
    TEST_ADD(TestIntrinsicFunctions::testMax);
    TEST_ADD(TestIntrinsicFunctions::testMin);
    TEST_ADD(TestIntrinsicFunctions::testAnd);
    TEST_ADD(TestIntrinsicFunctions::testMul24);
    TEST_ADD(TestIntrinsicFunctions::testV8Adds);
    TEST_ADD(TestIntrinsicFunctions::testV8Subs);
    TEST_ADD(TestIntrinsicFunctions::testV8Min);
    TEST_ADD(TestIntrinsicFunctions::testV8Max);

    TEST_ADD(TestIntrinsicFunctions::testBitcastUnsignedChar);
    TEST_ADD(TestIntrinsicFunctions::testBitcastChar);
    TEST_ADD(TestIntrinsicFunctions::testBitcastUnsignedShort);
    TEST_ADD(TestIntrinsicFunctions::testBitcastShort);
    TEST_ADD(TestIntrinsicFunctions::testBitcastUnsignedInt);
    TEST_ADD(TestIntrinsicFunctions::testBitcastInt);
    TEST_ADD(TestIntrinsicFunctions::testBitcastFloat);
}

void TestIntrinsicFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <typename In, typename Out, std::size_t N, typename Limits = In, typename Comparison = std::equal_to<Out>>
static void testUnaryFunction(std::stringstream& code, const std::string& options, const std::function<Out(In)>& op,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto in = generateInput<In, N * 12, Limits>(false);

    auto out = runEmulation<In, Out, N, 12>(code, {in});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkUnaryResults<Out, In, N * 12, Comparison>(
        in, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename In, typename Out, typename Comparison = std::equal_to<Out>>
static void testUnaryFunctionWithConstant(std::stringstream& code, const std::string& options, In in,
    const std::function<Out(In)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto out = runEmulation<In, Out, 1, 1>(code, {});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    std::array<In, 1> tmpIn{in};
    checkUnaryResults<Out, In, 1, Comparison>(
        tmpIn, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename In, typename Out, std::size_t N, typename Comparison = std::equal_to<Out>>
static void testBinaryOperation(std::stringstream& code, const std::string& options,
    const std::function<Out(In, In)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto in0 = generateInput<In, N * 12>(true);
    auto in1 = generateInput<In, N * 12>(false);

    auto out = runEmulation<In, Out, N, 12>(code, {in0, in1});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();
    checkBinaryResults<Out, In, N * 12, Comparison>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename In, typename Out, std::size_t N, typename Comparison = std::equal_to<Out>>
static void testBinaryOperationWithSecondConstants(std::stringstream& code, const std::string& options, In constant,
    const std::function<Out(In, In)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto in0 = generateInput<In, N * 12>(true);
    std::array<In, N * 12> tmpIn1{};
    tmpIn1.fill(constant);

    auto out = runEmulation<In, Out, N, 12>(code, {in0});
    auto pos = options.find("-DOP=") + std::string("-DOP=").size();

    checkBinaryResults<Out, In, N * 12, Comparison>(
        in0, tmpIn1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename In, typename Out, std::size_t N, typename Comparison = std::equal_to<Out>>
static void testBinaryFunction(std::stringstream& code, const std::string& options,
    const std::function<Out(In, In)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto in0 = generateInput<In, N * 12>(true);
    auto in1 = generateInput<In, N * 12>(true);

    auto out = runEmulation<In, Out, N, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryResults<Out, In, N * 12, Comparison>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

template <typename In, typename Out, typename Comparison = std::equal_to<Out>>
static void testBinaryFunctionWithConstants(std::stringstream& code, const std::string& options, In in0, In in1,
    const std::function<Out(In, In)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto out = runEmulation<In, Out, 1, 1>(code, {});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    std::array<In, 1> tmpIn0{in0};
    std::array<In, 1> tmpIn1{in1};
    checkBinaryResults<Out, In, 1, Comparison>(
        tmpIn0, tmpIn1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

void TestIntrinsicFunctions::testIntMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=int16 -DOUT=int16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 32, std::multiplies<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 31, std::multiplies<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 17, std::multiplies<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 12, std::multiplies<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testShortMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=short16 -DOUT=short16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(short)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 32, std::multiplies<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(short)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 31, std::multiplies<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(short)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 17, std::multiplies<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(short)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 12, std::multiplies<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testCharMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=char16 -DOUT=char16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(char)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 32, std::multiplies<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(char)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 31, std::multiplies<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(char)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 17, std::multiplies<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(char)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 12, std::multiplies<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testUnsignedIntMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=uint16 -DOUT=uint16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 32, std::multiplies<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 31, std::multiplies<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 17, std::multiplies<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 12, std::multiplies<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedShortMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=ushort16 -DOUT=ushort16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(ushort)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 32,
        std::multiplies<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 31,
        std::multiplies<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 17,
        std::multiplies<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 12,
        std::multiplies<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedCharMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=uchar16 -DOUT=uchar16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(uchar)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 32,
        std::multiplies<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 31,
        std::multiplies<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 17,
        std::multiplies<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 12,
        std::multiplies<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testIntDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=int16 -DOUT=int16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 32, std::divides<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 31, std::divides<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 17, std::divides<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 12, std::divides<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testShortDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=short16 -DOUT=short16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(short)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 32, std::divides<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(short)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 31, std::divides<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(short)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 17, std::divides<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(short)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 12, std::divides<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testCharDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=char16 -DOUT=char16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(char)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 32, std::divides<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(char)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 31, std::divides<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(char)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 17, std::divides<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(char)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 12, std::divides<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testIntModuloByConstant()
{
    // constant 2^x
    std::string options = "-DOP=% -DIN=int16 -DOUT=int16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 32, std::modulus<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 31, std::modulus<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 17, std::modulus<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<int, int, 16>(code, newOptions, 12, std::modulus<int>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testShortModuloByConstant()
{
    // constant 2^x
    std::string options = "-DOP=% -DIN=short16 -DOUT=short16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(short)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 32, std::modulus<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(short)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 31, std::modulus<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(short)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 17, std::modulus<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(short)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<short, short, 16>(code, newOptions, 12, std::modulus<short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testCharModuloByConstant()
{
    // constant 2^x
    std::string options = "-DOP=% -DIN=char16 -DOUT=char16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(char)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 32, std::modulus<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(char)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 31, std::modulus<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(char)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 17, std::modulus<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(char)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<char, char, 16>(code, newOptions, 12, std::modulus<char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testUnsignedIntDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=uint16 -DOUT=uint16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 32, std::divides<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 31, std::divides<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 17, std::divides<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 12, std::divides<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedShortDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=ushort16 -DOUT=ushort16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(ushort)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 32,
        std::divides<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 31,
        std::divides<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 17,
        std::divides<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 12,
        std::divides<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedCharDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=uchar16 -DOUT=uchar16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(uchar)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 32,
        std::divides<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 31,
        std::divides<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 17,
        std::divides<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 12,
        std::divides<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedIntModuloByConstant()
{
    // constant 2^x
    std::string options = "-DOP=% -DIN=uint16 -DOUT=uint16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 32, std::modulus<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 31, std::modulus<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 17, std::modulus<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned, unsigned, 16>(code, newOptions, 12, std::modulus<unsigned>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedShortModuloByConstant()
{
    // constant 2^x
    std::string options = "-DOP=% -DIN=ushort16 -DOUT=ushort16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(ushort)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 32,
        std::modulus<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 31,
        std::modulus<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 17,
        std::modulus<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(ushort)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned short, unsigned short, 16>(code, newOptions, 12,
        std::modulus<unsigned short>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedCharModuloByConstant()
{
    // constant 2^x
    std::string options = "-DOP=% -DIN=uchar16 -DOUT=uchar16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=(uchar)32";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 32,
        std::modulus<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)31";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 31,
        std::modulus<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)17";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 17,
        std::modulus<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=(uchar)12";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<unsigned char, unsigned char, 16>(code, newOptions, 12,
        std::modulus<unsigned char>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testFloatMultiplicationWithConstant()
{
    // constant 2^x
    std::string options = "-DOP=* -DIN=float16 -DOUT=float16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 32.0f, std::multiplies<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 31.0f, std::multiplies<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 17.0f, std::multiplies<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 12.0f, std::multiplies<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testFloatDivisionByConstant()
{
    // constant 2^x
    std::string options = "-DOP=/ -DIN=float16 -DOUT=float16";
    std::stringstream code;
    auto newOptions = options + " -DSOURCE=32.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 32.0f, std::divides<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant 2^x - 1
    code.str("");
    newOptions = options + " -DSOURCE=31.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 31.0f, std::divides<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant prime
    code.str("");
    newOptions = options + " -DSOURCE=17.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 17.0f, std::divides<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    // constant non-prime
    code.str("");
    newOptions = options + " -DSOURCE=12.0f";
    compileBuffer(config, code, BINARY_OPERATION_SECOND_CONTANT, newOptions);
    testBinaryOperationWithSecondConstants<float, float, 16>(code, newOptions, 12.0f, std::divides<float>{},
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testIntToFloat()
{
    std::string options = "-DFUNC=(float) -DIN=int -DOUT=float";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, float, 1>(
        code, options, [](int i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testShortToFloat()
{
    std::string options = "-DFUNC=(float) -DIN=short -DOUT=float";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<short, float, 1>(
        code, options, [](short i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testCharToFloat()
{
    std::string options = "-DFUNC=(float) -DIN=char -DOUT=float";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<char, float, 1>(
        code, options, [](char i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedIntToFloat()
{
    std::string options = "-DFUNC=(float) -DIN=uint -DOUT=float";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<unsigned, float, 1>(
        code, options, [](unsigned i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedShortToFloat()
{
    std::string options = "-DFUNC=(float) -DIN=ushort -DOUT=float";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<unsigned short, float, 1>(
        code, options, [](unsigned short i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testUnsignedCharToFloat()
{
    std::string options = "-DFUNC=(float) -DIN=uchar -DOUT=float";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<unsigned char, float, 1>(
        code, options, [](unsigned char i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testFloatToInt()
{
    std::string options = "-DFUNC=(int) -DIN=float -DOUT=int";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    // conversion from float to integer with out-of-range values is implementation defined, see OpenCL 1.2,
    // section 6.2.3.3
    testUnaryFunction<float, int, 1, int>(
        code, options, [](float f) -> int { return static_cast<int>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFloatToShort()
{
    std::string options = "-DFUNC=(short) -DIN=float -DOUT=short";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    // conversion from float to integer with out-of-range values is implementation defined, see OpenCL 1.2,
    // section 6.2.3.3
    testUnaryFunction<float, short, 1, short>(
        code, options, [](float f) -> short { return static_cast<short>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFloatToChar()
{
    std::string options = "-DFUNC=(char) -DIN=float -DOUT=char";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    // conversion from float to integer with out-of-range values is implementation defined, see OpenCL 1.2,
    // section 6.2.3.3
    testUnaryFunction<float, char, 1, char>(
        code, options, [](float f) -> char { return static_cast<char>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFloatToUnsignedInt()
{
    std::string options = "-DFUNC=(uint) -DIN=float -DOUT=uint";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    // conversion from float to integer with out-of-range values is implementation defined, see OpenCL 1.2,
    // section 6.2.3.3
    testUnaryFunction<float, unsigned, 1, unsigned>(
        code, options, [](float f) -> unsigned { return static_cast<unsigned>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFloatToUnsignedShort()
{
    std::string options = "-DFUNC=(ushort) -DIN=float -DOUT=ushort";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    // conversion from float to integer with out-of-range values is implementation defined, see OpenCL 1.2,
    // section 6.2.3.3
    testUnaryFunction<float, unsigned short, 1, unsigned short>(
        code, options, [](float f) -> unsigned short { return static_cast<unsigned short>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFloatToUnsignedChar()
{
    std::string options = "-DFUNC=(uchar) -DIN=float -DOUT=uchar";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    // conversion from float to integer with out-of-range values is implementation defined, see OpenCL 1.2,
    // section 6.2.3.3
    testUnaryFunction<float, unsigned char, 1, unsigned char>(
        code, options, [](float f) -> unsigned char { return static_cast<unsigned char>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testFtoi()
{
    std::string options = "-DFUNC=vc4cl_ftoi -DIN=float -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, int, 1, int>(
        code, options, [](float f) -> int { return static_cast<int>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<float, 1, int>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in)).append("f");
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<float, int>(
        code, options, in, [](float f) -> int { return static_cast<int>(f); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testItof()
{
    std::string options = "-DFUNC=vc4cl_itof -DIN=int -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, float, 1>(
        code, options, [](int i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, float>(
        code, options, in, [](int i) -> float { return static_cast<float>(i); },
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testClz()
{
    auto func = [](int i) -> int { return vc4c::intermediate::clz(vc4c::Literal(i)).signedInt(); };

    std::string options = "-DFUNC=vc4cl_clz -DIN=int -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, int>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testSfuRsqrt()
{
    auto func = [](float f) -> float { return 1.0f / std::sqrt(f); };

    std::string options = "-DFUNC=vc4cl_sfu_rsqrt -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, float, 1, unsigned, CompareULP<8192>>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<float, 1, unsigned>(false)[0];
    options.append(" -DSOURCES=").append(std::to_string(in)).append("f");
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<float, float>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testSfuExp2()
{
    std::string options = "-DFUNC=vc4cl_sfu_exp2 -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, float, 1, unsigned, CompareULP<8192>>(code, options, std::exp2f,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<float, 1, unsigned>(false)[0];
    options.append(" -DSOURCES=").append(std::to_string(in)).append("f");
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<float, float>(code, options, in, std::exp2f,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testSfuLog2()
{
    std::string options = "-DFUNC=vc4cl_sfu_log2 -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, float, 1, unsigned, CompareULP<8192>>(code, options, std::log2f,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<float, 1, unsigned>(false)[0];
    options.append(" -DSOURCES=").append(std::to_string(in)).append("f");
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<float, float>(code, options, in, std::log2f,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testSfuRecip()
{
    auto func = [](float f) -> float { return 1.0f / f; };

    std::string options = "-DFUNC=vc4cl_sfu_recip -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, float, 1, unsigned, CompareULP<8192>>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<float, 1, unsigned>(false)[0];
    options.append(" -DSOURCES=").append(std::to_string(in)).append("f");
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<float, float>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testIsNaN()
{
    auto func = [](float f) -> int { return std::isnan(f); };

    std::string options = "-DFUNC=vc4cl_is_nan -DIN=float -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = std::numeric_limits<float>::quiet_NaN();
    auto newOptions = options + " -DSOURCES=NAN";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    in = std::numeric_limits<float>::infinity();
    newOptions = options + " -DSOURCES=INFINITY";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    in = std::numeric_limits<float>::max();
    newOptions = options + " -DSOURCES=" + std::to_string(in) + "f";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    in = std::numeric_limits<float>::min();
    newOptions = options + " -DSOURCES=" + std::to_string(in) + "f";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testIsInfNaN()
{
    auto func = [](float f) -> int { return std::isnan(f) || std::isinf(f); };

    std::string options = "-DFUNC=vc4cl_is_inf_nan -DIN=float -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<float, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = std::numeric_limits<float>::quiet_NaN();
    auto newOptions = options + " -DSOURCES=NAN";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    in = std::numeric_limits<float>::infinity();
    newOptions = options + " -DSOURCES=INFINITY";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    in = std::numeric_limits<float>::max();
    newOptions = options + " -DSOURCES=" + std::to_string(in) + "f";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    in = std::numeric_limits<float>::min();
    newOptions = options + " -DSOURCES=" + std::to_string(in) + "f";
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, newOptions);
    testUnaryFunctionWithConstant<float, int>(code, newOptions, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testFmax()
{
    auto func = [](float a, float b) -> float { return std::max(a, b); };

    std::string options = "-DFUNC=vc4cl_fmax -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<float, float, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<float, 1>(true)[0];
    auto in1 = generateInput<float, 1>(true)[0];
    options.append(" -DSOURCES0=")
        .append(std::to_string(in0))
        .append("f -DSOURCES1=")
        .append(std::to_string(in1))
        .append("f");
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<float, float>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFmin()
{
    auto func = [](float a, float b) -> float { return std::min(a, b); };

    std::string options = "-DFUNC=vc4cl_fmin -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<float, float, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<float, 1>(true)[0];
    auto in1 = generateInput<float, 1>(true)[0];
    options.append(" -DSOURCES0=")
        .append(std::to_string(in0))
        .append("f -DSOURCES1=")
        .append(std::to_string(in1))
        .append("f");
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<float, float>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testFmaxabs()
{
    auto func = [](float a, float b) -> float { return std::max(std::abs(a), std::abs(b)); };

    std::string options = "-DFUNC=vc4cl_fmaxabs -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<float, float, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<float, 1>(true)[0];
    auto in1 = generateInput<float, 1>(true)[0];
    options.append(" -DSOURCES0=")
        .append(std::to_string(in0))
        .append("f -DSOURCES1=")
        .append(std::to_string(in1))
        .append("f");
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<float, float>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testFminabs()
{
    auto func = [](float a, float b) -> float { return std::min(std::abs(a), std::abs(b)); };

    std::string options = "-DFUNC=vc4cl_fminabs -DIN=float -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<float, float, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<float, 1>(true)[0];
    auto in1 = generateInput<float, 1>(true)[0];
    options.append(" -DSOURCES0=")
        .append(std::to_string(in0))
        .append("f -DSOURCES1=")
        .append(std::to_string(in1))
        .append("f");
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<float, float>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testAsr() {}
void TestIntrinsicFunctions::testRor() {}
void TestIntrinsicFunctions::testMax()
{
    // TODO for unsigned version, need to add unsigned flag to function
    auto func = [](int a, int b) -> int { return std::max(a, b); };

    std::string options = "-DFUNC=vc4cl_max -DIN=int -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<int, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<int, 1>(true)[0];
    auto in1 = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<int, int>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testMin()
{
    auto func = [](int a, int b) -> int { return std::min(a, b); };

    std::string options = "-DFUNC=vc4cl_min -DIN=int -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<int, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<int, 1>(true)[0];
    auto in1 = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<int, int>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testAnd()
{
    // TODO is the and (supposed to be) sign-extending or not??
    auto func = [](short a, short b) -> int { return static_cast<int>(a) & static_cast<int>(b); };

    std::string options = "-DFUNC=vc4cl_and -DIN=short -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<short, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<short, 1>(true)[0];
    auto in1 = generateInput<short, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<short, int>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testMul24()
{
    auto func = [](int a, int b) -> int { return (a & 0x00FFFFFF) * (b & 0x00FFFFFF); };

    std::string options = "-DFUNC=vc4cl_mul24 -DIN=int -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<int, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<int, 1>(true)[0];
    auto in1 = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<int, int>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testV8Adds()
{
    using namespace vc4c;
    auto func = [](unsigned a, unsigned b) -> unsigned {
        return OP_V8ADDS(Value(Literal(a), TYPE_INT32), Value(Literal(b), TYPE_INT32)).first->literal().unsignedInt();
    };

    std::string options = "-DFUNC=vc4cl_v8adds -DIN=uint -DOUT=uint -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<unsigned, unsigned, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<unsigned, 1>(true)[0];
    auto in1 = generateInput<unsigned, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<unsigned, unsigned>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testV8Subs()
{
    using namespace vc4c;
    auto func = [](unsigned a, unsigned b) -> unsigned {
        return OP_V8SUBS(Value(Literal(a), TYPE_INT32), Value(Literal(b), TYPE_INT32)).first->literal().unsignedInt();
    };

    std::string options = "-DFUNC=vc4cl_v8subs -DIN=uint -DOUT=uint -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<unsigned, unsigned, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<unsigned, 1>(true)[0];
    auto in1 = generateInput<unsigned, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<unsigned, unsigned>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testV8Min()
{
    using namespace vc4c;
    auto func = [](unsigned a, unsigned b) -> unsigned {
        return OP_V8MIN(Value(Literal(a), TYPE_INT32), Value(Literal(b), TYPE_INT32)).first->literal().unsignedInt();
    };

    std::string options = "-DFUNC=vc4cl_v8min -DIN=uint -DOUT=uint -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<unsigned, unsigned, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<unsigned, 1>(true)[0];
    auto in1 = generateInput<unsigned, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<unsigned, unsigned>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testV8Max()
{
    using namespace vc4c;
    auto func = [](unsigned a, unsigned b) -> unsigned {
        return OP_V8MAX(Value(Literal(a), TYPE_INT32), Value(Literal(b), TYPE_INT32)).first->literal().unsignedInt();
    };

    std::string options = "-DFUNC=vc4cl_v8max -DIN=uint -DOUT=uint -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, BINARY_FUNCTION, options);
    testBinaryFunction<unsigned, unsigned, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in0 = generateInput<unsigned, 1>(true)[0];
    auto in1 = generateInput<unsigned, 1>(true)[0];
    options.append(" -DSOURCES0=").append(std::to_string(in0)).append(" -DSOURCES1=").append(std::to_string(in1));
    compileBuffer(config, code, BINARY_FUNCTION_CONSTANT, options);
    testBinaryFunctionWithConstants<unsigned, unsigned>(code, options, in0, in1, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestIntrinsicFunctions::testBitcastUnsignedChar()
{
    auto func = [](int i) -> unsigned char {
        return static_cast<unsigned char>(vc4c::bit_cast<int, unsigned>(i) & 0xFF);
    };

    std::string options = "-DFUNC=vc4cl_bitcast_uchar -DIN=int -DOUT=uchar -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, unsigned char, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, unsigned char>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testBitcastChar()
{
    auto func = [](int i) -> char { return static_cast<char>(i & 0xFF); };

    std::string options = "-DFUNC=vc4cl_bitcast_char -DIN=int -DOUT=char -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, char, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, char>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testBitcastUnsignedShort()
{
    auto func = [](int i) -> unsigned short {
        return static_cast<unsigned short>(vc4c::bit_cast<int, unsigned>(i) & 0xFFFF);
    };

    std::string options = "-DFUNC=vc4cl_bitcast_ushort -DIN=int -DOUT=ushort -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, unsigned short, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, unsigned short>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testBitcastShort()
{
    auto func = [](int i) -> short { return static_cast<short>(i & 0xFFFF); };

    std::string options = "-DFUNC=vc4cl_bitcast_short -DIN=int -DOUT=short -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, short, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, short>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testBitcastUnsignedInt()
{
    auto func = [](int i) -> unsigned { return vc4c::bit_cast<int, unsigned>(i); };

    std::string options = "-DFUNC=vc4cl_bitcast_uint -DIN=int -DOUT=uint -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, unsigned, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, unsigned>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestIntrinsicFunctions::testBitcastInt()
{
    auto func = [](unsigned i) -> int { return vc4c::bit_cast<unsigned, int>(i); };

    std::string options = "-DFUNC=vc4cl_bitcast_int -DIN=uint -DOUT=int -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<unsigned, int, 1>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<unsigned, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<unsigned, int>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

struct EqualNaN
{
    // The "normal" float equality operator always returns false for NaN
    bool operator()(float a, float b) const noexcept
    {
        return (std::isnan(a) && std::isnan(b) && std::signbit(a) == std::signbit(b)) || a == b;
    }
};

void TestIntrinsicFunctions::testBitcastFloat()
{
    auto func = [](int i) -> float { return vc4c::bit_cast<int, float>(i); };

    std::string options = "-DFUNC=vc4cl_bitcast_float -DIN=int -DOUT=float -DDEFINE_PROTOTYPE";
    std::stringstream code;
    compileBuffer(config, code, UNARY_FUNCTION, options);
    testUnaryFunction<int, float, 1, int, EqualNaN>(code, options, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));

    code.str("");
    auto in = generateInput<int, 1>(true)[0];
    options.append(" -DSOURCES=").append(std::to_string(in));
    compileBuffer(config, code, UNARY_FUNCTION_CONSTANT, options);
    testUnaryFunctionWithConstant<int, float>(code, options, in, func,
        std::bind(&TestIntrinsicFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}