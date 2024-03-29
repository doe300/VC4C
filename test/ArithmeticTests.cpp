/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include <algorithm>
#include <climits>
#include <functional>
#include <limits>

static const std::string UNARY_OPERATIONS = R"(
__kernel void test_plus(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = +in[gid];
}

__kernel void test_minus(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = -in[gid];
}

#ifndef FLOAT_TYPE // There is no float in-/decrement or bitwise not
__kernel void test_increment(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  // need to cache locally for increment
  TYPE tmp = in[gid];
  out[gid] = ++tmp;
}

__kernel void test_decrement(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  // need to cache locally for decrement
  TYPE tmp = in[gid];
  out[gid] = --tmp;
}

__kernel void test_bitnot(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = ~in[gid];
}
#endif

__kernel void test_not(__global OUT* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = !in[gid];
}
)";

static const std::string BINARY_OPERATIONS = R"(
__kernel void test_add(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] + in1[gid];
}

__kernel void test_sub(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] - in1[gid];
}

__kernel void test_mul(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] * in1[gid];
}

__kernel void test_div(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] / in1[gid];
}

#ifndef FLOAT_TYPE // There is no modulo, bitwise and/or/xor or shifts for float
__kernel void test_mod(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] % in1[gid];
}

__kernel void test_bitand(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] & in1[gid];
}

__kernel void test_bitor(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] | in1[gid];
}

__kernel void test_bitxor(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] ^ in1[gid];
}

__kernel void test_shl(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] << in1[gid];
}

__kernel void test_shr(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = in0[gid] >> in1[gid];
}
#endif
)";

static const std::string RELATIONAL_OPERATIONS = R"(
__kernel void test_equal(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] == in1[gid]);
}

__kernel void test_notequal(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] != in1[gid]);
}

__kernel void test_greater(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] > in1[gid]);
}

__kernel void test_less(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] < in1[gid]);
}

__kernel void test_greaterequal(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] >= in1[gid]);
}

__kernel void test_lessequal(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] <= in1[gid]);
}

__kernel void test_and(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] && in1[gid]);
}

__kernel void test_or(__global int16* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  out[gid] = convert_int16(in0[gid] || in1[gid]);
}
)";

static const std::string TERNARY_OPERATOR = R"(
__kernel void test_scalar(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1) {
  size_t gid = get_global_id(0);
  TYPE a = in0[gid];
  TYPE b = in1[gid];
  out[gid] = a < b ? a : b;
}
)";

/*
 * For the shift operands, the OpenCL 1.2 (and 2.0) standard defines some weird behavior:
 * The shift mask is ANDed with the number of bits - 1 of the shifted type before applying the shift.
 *
 * E.g. (ushort)a << (ushort)b -> (ushort)a << ((ushort)b & 15)
 *
 * This is applied by LLVM (introduced in
 * https://github.com/llvm-mirror/clang/commit/7a83421776416d6a9044fb03b5b02208b47646c1) and also checked by OpenCL CTS
 * (https://github.com/KhronosGroup/OpenCL-CTS/blob/cl12_trunk/test_conformance/integer_ops/verification_and_generation_functions.c)
 * see also https://stackoverflow.com/questions/51919757/left-shift-by-negative-value-in-opencl
 */

template <typename T>
static T checkShiftLeft(T arg1, T arg2)
{
    auto op1 = static_cast<std::make_unsigned_t<T>>(arg1);
    auto op2 = static_cast<std::make_unsigned_t<T>>(arg2);
    return static_cast<T>(op1 << (op2 % (sizeof(T) * CHAR_BIT)));
}

template <typename T>
static T checkShiftRight(T arg1, T arg2)
{
    return static_cast<T>(arg1 >> (arg2 % (sizeof(T) * CHAR_BIT)));
}

template <typename T, typename Func>
static typename std::enable_if<std::is_integral<T>::value, Func>::type wrap(const Func& func)
{
    return func;
}

template <typename T>
static typename std::enable_if<std::is_floating_point<T>::value, std::function<float(float)>>::type wrap(
    const std::function<float(float)>& func)
{
    return test_data::roundToZero(func);
}

template <typename T>
static typename std::enable_if<std::is_floating_point<T>::value, std::function<float(float, float)>>::type wrap(
    const std::function<float(float, float)>& func)
{
    return test_data::roundToZero(func);
}

template <typename T>
static void registerNonFloatTests(const std::string& typeName, test_data::DataFilter flags, const std::string& options,
    const std::vector<T>& unaryInputs, const std::vector<T>& binaryInputsLeft, const std::vector<T>& binaryInputsRight)
{
    using namespace test_data;

    ////
    // Unary operators
    ////

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "unary_increment_" + typeName, UNARY_OPERATIONS, "test_increment", options);
        builder.setFlags(flags);
        builder.calculateDimensions(unaryInputs.size());
        builder.template allocateParameter<0>(unaryInputs.size());
        builder.template setParameter<1>(std::vector<T>(unaryInputs));
        builder.template checkParameterEquals<0>(transform<T>(unaryInputs, [](T val) -> T { return ++val; }));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "unary_decrement_" + typeName, UNARY_OPERATIONS, "test_decrement", options);
        builder.setFlags(flags);
        builder.calculateDimensions(unaryInputs.size());
        builder.template allocateParameter<0>(unaryInputs.size());
        builder.template setParameter<1>(std::vector<T>(unaryInputs));
        builder.template checkParameterEquals<0>(transform<T>(unaryInputs, [](T val) -> T { return --val; }));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "unary_bitwise_not_" + typeName, UNARY_OPERATIONS, "test_bitnot", options);
        builder.setFlags(flags);
        builder.calculateDimensions(unaryInputs.size());
        builder.template allocateParameter<0>(unaryInputs.size());
        builder.template setParameter<1>(std::vector<T>(unaryInputs));
        builder.template checkParameterEquals<0>(
            transform<T>(unaryInputs, [](T val) -> T { return static_cast<T>(~val); }));
    }

    ////
    // Binary operators
    ////

    // TODO (u)long division/modulo is not correct yet
    auto divisionFlags = sizeof(T) > sizeof(uint32_t) ? DataFilter::DISABLED : DataFilter::NONE;

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_mod_" + typeName, BINARY_OPERATIONS, "test_mod", options);
        builder.setFlags(flags | divisionFlags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(
            transform<T>(binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> T {
                if(val2 == static_cast<T>(-1))
                    // somehow my host does not like divisions by -1 (at least not for some types)
                    return std::is_unsigned<T>::value ? (val1 == static_cast<T>(-1) ? 0 : val1) : 0;
                return static_cast<T>(val2 != 0 ? val1 % val2 : val1);
            }));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_bitwise_and_" + typeName, BINARY_OPERATIONS, "test_bitand", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(
            transform<T>(binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> T { return val1 & val2; }));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_bitwise_or_" + typeName, BINARY_OPERATIONS, "test_bitor", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(
            transform<T>(binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> T { return val1 | val2; }));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_bitwise_xor_" + typeName, BINARY_OPERATIONS, "test_bitxor", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(
            transform<T>(binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> T { return val1 ^ val2; }));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_shl_" + typeName, BINARY_OPERATIONS, "test_shl", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<T>(binaryInputsLeft, binaryInputsRight, checkShiftLeft<T>));
    }

    {
        // TODO binary_shr_long fails on hardware
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_shr_" + typeName, BINARY_OPERATIONS, "test_shr", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<T>(binaryInputsLeft, binaryInputsRight, checkShiftRight<T>));
    }

    {
        // OpenCL 1.2, 6.3.i: "exp1 ? exp2 : exp3 [...] If the result is a vector value, then this is equivalent to
        // calling select(exp3, exp2, exp1)" -> is already tested
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "ternary_selection_" + typeName, TERNARY_OPERATOR, "test_scalar", "-DTYPE=" + typeName);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size(), 1);
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<T>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> T { return std::min(val1, val2); }));
    }
}

static void registerNonFloatTests(const std::string& typeName, test_data::DataFilter flags, const std::string& options,
    const std::vector<float>& unaryInputs, const std::vector<float>& binaryInputsLeft,
    const std::vector<float>& binaryInputsRight)
{
    // specialization for float to not register such tests
}

template <typename T, typename IntType>
static void registerTypeTests(const std::string& typeName)
{
    using namespace test_data;

    // Test exact match, but allow for NaN equality for float
    using Comparator = std::conditional_t<std::is_floating_point<T>::value, CompareULP<0>, CompareEquals<T>>;
    using MulComparator = std::conditional_t<std::is_floating_point<T>::value, CompareULP<3>, CompareEquals<T>>;

    std::vector<T> unaryInputs{
        0, 1, 17, 42,                                                                                   // 4
        static_cast<T>(-1), static_cast<T>(-17), static_cast<T>(-42),                                   // 7
        std::numeric_limits<T>::min(), std::numeric_limits<T>::max(), std::numeric_limits<T>::lowest(), // 10
        std::numeric_limits<T>::infinity(), -std::numeric_limits<T>::infinity(),
        std::numeric_limits<T>::quiet_NaN(),                                                                       // 13
        std::numeric_limits<T>::max() - 1, std::numeric_limits<T>::lowest() + 1, std::numeric_limits<T>::max() / 2 // 16
    };

    // put together input vectors by creating a cartesian product of the input set
    std::vector<T> binaryInputsLeft(unaryInputs.size() * unaryInputs.size());
    std::vector<T> binaryInputsRight(unaryInputs.size() * unaryInputs.size());
    for(std::size_t i = 0; i < unaryInputs.size(); ++i)
    {
        std::fill(
            &binaryInputsLeft[i * unaryInputs.size()], &binaryInputsLeft[(i + 1) * unaryInputs.size()], unaryInputs[i]);
        for(std::size_t k = 0; k < unaryInputs.size(); k++)
        {
            binaryInputsRight[i + k * unaryInputs.size()] = unaryInputs[i];
        }
    }

    auto flags = std::is_floating_point<T>::value ? DataFilter::FLOAT_ARITHMETIC : DataFilter::INT_ARITHMETIC;
    if(sizeof(T) > sizeof(uint32_t))
        flags = flags | DataFilter::USES_LONG;
    if(std::is_floating_point<T>::value)
        // TODO most float operations return Inf where they should return a NaN
        flags = flags | DataFilter::DISABLED;
    // TODO (u)long division/modulo is not correct yet
    auto divisionFlags = sizeof(T) > sizeof(uint32_t) ? DataFilter::DISABLED : DataFilter::NONE;

    auto options = "-DTYPE=" + typeName + "16";
    if(std::is_floating_point<T>::value)
        options += " -DFLOAT_TYPE -DOUT=int16";
    else if(typeName[0] == 'u')
        options += " -DOUT=" + typeName.substr(1) + "16";
    else
        options += " -DOUT=" + typeName + "16";

    ////
    // Unary operators
    ////

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder("unary_plus_" + typeName, UNARY_OPERATIONS, "test_plus", options);
        builder.setFlags(flags);
        builder.calculateDimensions(unaryInputs.size());
        builder.template allocateParameter<0>(unaryInputs.size());
        builder.template setParameter<1>(std::vector<T>(unaryInputs));
        builder.template checkParameter<0, Comparator>(std::vector<T>(unaryInputs));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "unary_minus_" + typeName, UNARY_OPERATIONS, "test_minus", options);
        builder.setFlags(flags);
        builder.calculateDimensions(unaryInputs.size());
        builder.template allocateParameter<0>(unaryInputs.size());
        builder.template setParameter<1>(std::vector<T>(unaryInputs));
        builder.template checkParameter<0, Comparator>(
            transform<T>(unaryInputs, wrap<T>([](T val) -> T { return static_cast<T>(-val); })));
    }

    {
        // TODO wrong results for (u)long and SPIR-V front-end
        auto additionalFlags = sizeof(T) > sizeof(uint32_t) ? DataFilter::SPIRV_DISABLED : DataFilter::NONE;
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "unary_logical_not_" + typeName, UNARY_OPERATIONS, "test_not", options);
        builder.setFlags(flags | additionalFlags);
        builder.calculateDimensions(unaryInputs.size());
        builder.template allocateParameter<0>(unaryInputs.size());
        builder.template setParameter<1>(std::vector<T>(unaryInputs));
        builder.template checkParameterEquals<0>(
            transform<T>(unaryInputs, [](T val) -> IntType { return !val ? -1 : 0; }));
    }

    ////
    // Binary operators
    ////

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_add_" + typeName, BINARY_OPERATIONS, "test_add", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameter<0, Comparator>(
            transform<T>(binaryInputsLeft, binaryInputsRight, wrap<T>(std::plus<T>{})));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_sub_" + typeName, BINARY_OPERATIONS, "test_sub", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameter<0, Comparator>(
            transform<T>(binaryInputsLeft, binaryInputsRight, wrap<T>(std::minus<T>{})));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_mul_" + typeName, BINARY_OPERATIONS, "test_mul", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameter<0, MulComparator>(
            transform<T>(binaryInputsLeft, binaryInputsRight, wrap<T>(std::multiplies<T>{})));
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
            "binary_div_" + typeName, BINARY_OPERATIONS, "test_div", options);
        builder.setFlags(flags | divisionFlags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameter<0, MulComparator>(
            transform<T>(binaryInputsLeft, binaryInputsRight, wrap<T>([](T val1, T val2) -> T {
                if(std::is_integral<T>::value && val2 == static_cast<T>(-1))
                    // somehow my host does not like divisions by -1 (at least not for some types)
                    return static_cast<T>(std::is_unsigned<T>::value ? (val1 == static_cast<T>(-1) ? 1 : 0) : -val1);
                // "A divide by zero with integer types does not cause an exception but will result in an unspecified
                // value. Division by zero for floating-point types will result in +-infinity or NaN as prescribed by
                // the IEEE-754 standard"
                if(std::is_integral<T>::value && val2 == 0)
                    // (u)short and (u)char use float division, while (u)int and (u)long have their own implementation
                    return sizeof(T) <= 3 ? (std::signbit(val1) != std::signbit(val2) ? -1 : 1) :
                                            static_cast<T>(std::signbit(val1) ? 1 : -1);
                return static_cast<T>(val1 / val2);
            })));
    }

    ////
    // Relational operators
    ////
    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_equal_" + typeName, RELATIONAL_OPERATIONS, "test_equal", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 == val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_notequal_" + typeName, RELATIONAL_OPERATIONS, "test_notequal", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 != val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_greater_" + typeName, RELATIONAL_OPERATIONS, "test_greater", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 > val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_less_" + typeName, RELATIONAL_OPERATIONS, "test_less", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 < val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_greaterequal_" + typeName, RELATIONAL_OPERATIONS, "test_greaterequal", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 >= val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_lessequal_" + typeName, RELATIONAL_OPERATIONS, "test_lessequal", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 <= val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_logical_and_" + typeName, RELATIONAL_OPERATIONS, "test_and", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 && val2 ? -1 : 0; }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<T>, Buffer<T>> builder(
            "binary_logical_or_" + typeName, RELATIONAL_OPERATIONS, "test_or", options);
        builder.setFlags(flags);
        builder.calculateDimensions(binaryInputsLeft.size());
        builder.template allocateParameter<0>(binaryInputsLeft.size());
        builder.template setParameter<1>(std::vector<T>(binaryInputsLeft));
        builder.template setParameter<2>(std::vector<T>(binaryInputsRight));
        builder.template checkParameterEquals<0>(transform<int32_t>(
            binaryInputsLeft, binaryInputsRight, [](T val1, T val2) -> int32_t { return val1 || val2 ? -1 : 0; }));
    }

    registerNonFloatTests(typeName, flags, options, unaryInputs, binaryInputsLeft, binaryInputsRight);
}

void test_data::registerArithmeticTests()
{
    registerTypeTests<uint8_t, int8_t>("uchar");
    registerTypeTests<int8_t, int8_t>("char");
    registerTypeTests<uint16_t, int16_t>("ushort");
    registerTypeTests<int16_t, int16_t>("short");
    registerTypeTests<uint32_t, int32_t>("uint");
    registerTypeTests<int32_t, int32_t>("int");
    registerTypeTests<uint64_t, int64_t>("ulong");
    registerTypeTests<int64_t, int64_t>("long");
    registerTypeTests<float, int32_t>("float");
}
