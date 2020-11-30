/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include "test_files.h"

#include <algorithm>
#include <limits>

using namespace test_files;

static const std::string UNARY_FUNCTION = R"(
__kernel void test(__global int16* out, __global float16* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string UNARY_FUNCTION_SCALAR = R"(
__kernel void test(__global int* out, __global float* in) {
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

static const std::string BINARY_FUNCTION_SCALAR = R"(
__kernel void test(__global int* out, __global float* in0, __global float* in1) {
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

template <typename T, std::size_t VectorWidth>
static std::vector<T> generateAnyAllInputs()
{
    // we need 3 combinations of 16 values each
    // 1. no high-bits set (implicitly done by initializing the vector with zeroes
    std::vector<T> result(VectorWidth * 3, 0);
    // 2. all high-bits set
    std::fill_n(result.begin() + VectorWidth, VectorWidth, -16);
    // 3. some high-bits set
    std::fill_n(result.begin() + 2 * VectorWidth, VectorWidth / 2, -42);
    return result;
}

template <typename T>
static std::vector<T> replicate(uint8_t val)
{
    T result = 0;
    for(unsigned i = 0; i < sizeof(T); ++i)
        result |= static_cast<T>(static_cast<T>(val) << 8u * i);
    return {result};
}

void test_data::registerOpenCLRelationalFunctionTests()
{
    auto generalFlags = DataFilter::CORNER_CASES;
    auto relationalFlags = generalFlags | DataFilter::FLOAT_ARITHMETIC | DataFilter::COMPARISONS;
    auto groupingFlags = generalFlags | DataFilter::VECTOR_OPERATIONS;

    using limits = std::numeric_limits<float>;
    const std::vector<float> values = {0.0f, -0.0f, 1.0f, -1.0f, 0.5f, -0.5f, 6666.6f, -6666.6f, limits::min(),
        -limits::min(), limits::max(), limits::lowest(), limits::infinity(), -limits::infinity(), limits::quiet_NaN(),
        limits::signaling_NaN()};

    // put together input vectors by creating a cartesian product of the input set
    std::vector<float> productLeft(values.size() * values.size());
    std::vector<float> productRight(values.size() * values.size());
    for(std::size_t i = 0; i < values.size(); ++i)
    {
        std::fill(&productLeft[i * values.size()], &productLeft[(i + 1) * values.size()], values[i]);
        for(std::size_t k = 0; k < values.size(); k++)
        {
            productRight[i + k * values.size()] = values[i];
        }
    }

    ////
    // Binary comparisons
    ////

    registerTest(TestData{"isequal_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isequal [...] shall return a [...] 0 if the specified relation is false and a -1
            // (i.e. all bits set) if the specified relation is true for vector argument types."
            // "The relational functions isequal [...] always return 0 if either argument is not a number (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a == b) ? -1 : 0;
        }))}});

    registerTest(TestData{"isequal_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isequal [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            // "The relational functions isequal [...] always return 0 if either argument is not a number (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a == b) ? 1 : 0;
        }))}});

    registerTest(TestData{"isnotequal_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isnotequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isnotequal [...] shall return a [...] 0 if the specified relation is false and a -1
            // (i.e. all bits set) if the specified relation is true for vector argument types."
            // "isnotequal returns returns -1 if one or both arguments are not a number (NaN) and the argument type is a
            // vector."
            return (std::isnan(a) || std::isnan(b)) ? -1 : (a != b) ? -1 : 0;
        }))}});

    registerTest(TestData{"isnotequal_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isnotequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isnotequal [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            // "isnotequal returns 1 if one or both arguments are not a number (NaN) and the argument type is a scalar."
            return (std::isnan(a) || std::isnan(b)) ? 1 : (a != b) ? 1 : 0;
        }))}});

    registerTest(TestData{"isgreater_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isgreater", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isgreater [...] shall return a [...] 0 if the specified relation is false and a -1
            // (i.e. all bits set) if the specified relation is true for vector argument types."
            // "The relational functions [...] isgreater [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a > b) ? -1 : 0;
        }))}});

    registerTest(TestData{"isgreater_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isgreater", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isgreater [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            // "The relational functions [...] isgreater [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a > b) ? 1 : 0;
        }))}});

    registerTest(TestData{"isgreaterequal_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isgreaterequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isgreaterequal [...] shall return a [...] 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            // "The relational functions [...] isgreaterequal [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a >= b) ? -1 : 0;
        }))}});

    registerTest(TestData{"isgreaterequal_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isgreaterequal",
        "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isgreaterequal [...] shall return a 0 if the specified relation is false and a 1 if
            // the specified relation is true for scalar argument types."
            // "The relational functions [...] isgreaterequal [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a >= b) ? 1 : 0;
        }))}});

    registerTest(TestData{"isless_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isless", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isless [...] shall return a [...] 0 if the specified relation is false and a -1
            // (i.e. all bits set) if the specified relation is true for vector argument types."
            // "The relational functions [...] isless [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b) ? -1 : 0;
        }))}});

    registerTest(TestData{"isless_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isless", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isless [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            // "The relational functions [...] isless [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b) ? 1 : 0;
        }))}});

    registerTest(TestData{"islessequal_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=islessequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] islessequal [...] shall return a [...] 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            // "The relational functions [...] islessequal [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a <= b) ? -1 : 0;
        }))}});

    registerTest(TestData{"islessequal_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=islessequal", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] islessequal [...] shall return a 0 if the specified relation is false and a 1 if
            // the specified relation is true for scalar argument types."
            // "The relational functions [...] islessequal [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a <= b) ? 1 : 0;
        }))}});

    registerTest(TestData{"islessgreater_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=islessgreater", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] islessgreater [...] shall return a [...] 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            // "The relational functions [...] islessgreater [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b || a > b) ? -1 : 0;
        }))}});

    registerTest(TestData{"islessgreater_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=islessgreater",
        "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] islessgreater [...] shall return a 0 if the specified relation is false and a 1 if
            // the specified relation is true for scalar argument types."
            // "The relational functions [...] islessgreater [...] always return 0 if either argument is not a number
            // (NaN)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b || a > b) ? 1 : 0;
        }))}});

    registerTest(TestData{"isordered_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isordered", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isordered [...] shall return a [...] 0 if the specified relation is false and a -1
            // (i.e. all bits set) if the specified relation is true for vector argument types."
            // "Test if arguments are ordered. isordered() takes arguments x and y, and returns the result isequal(x, x)
            // && isequal(y, y)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a == a && b == b) ? -1 : 0;
        }))}});

    registerTest(TestData{"isordered_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isordered", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isordered [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            // "Test if arguments are ordered. isordered() takes arguments x and y, and returns the result isequal(x, x)
            // && isequal(y, y)."
            return (std::isnan(a) || std::isnan(b)) ? 0 : (a == a && b == b) ? 1 : 0;
        }))}});

    registerTest(TestData{"isunordered_vector", relationalFlags, &BINARY_FUNCTION, "-DFUNC=isunordered", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isunordered [...] shall return a [...] 0 if the specified relation is false and a -1
            // (i.e. all bits set) if the specified relation is true for vector argument types."
            // "Test if arguments are unordered. isunordered() takes arguments x and y, returning non-zero if x or y is
            // NaN, and zero otherwise."
            return (std::isnan(a) || std::isnan(b)) ? -1 : 0;
        }))}});

    registerTest(TestData{"isunordered_scalar", relationalFlags, &BINARY_FUNCTION_SCALAR, "-DFUNC=isunordered", "test",
        {toBufferParameter(std::vector<int32_t>(productLeft.size(), 0x42)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        calculateDimensions(productLeft.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
            // "The function[...] isunordered [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            // "Test if arguments are unordered. isunordered() takes arguments x and y, returning non-zero if x or y is
            // NaN, and zero otherwise."
            return (std::isnan(a) || std::isnan(b)) ? 1 : 0;
        }))}});

    ////
    // Unary predicates
    ////

    registerTest(TestData{"isfinite_vector", relationalFlags, &UNARY_FUNCTION, "-DFUNC=isfinite", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isfinite [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isinf(val) || std::isnan(val) ? 0 : -1;
        }))}});

    registerTest(TestData{"isfinite_scalar", relationalFlags, &UNARY_FUNCTION_SCALAR, "-DFUNC=isfinite", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isfinite [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isinf(val) || std::isnan(val) ? 0 : 1;
        }))}});

    registerTest(TestData{"isinf_vector", relationalFlags, &UNARY_FUNCTION, "-DFUNC=isinf", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isinf [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isinf(val) ? -1 : 0;
        }))}});

    registerTest(TestData{"isinf_scalar", relationalFlags, &UNARY_FUNCTION_SCALAR, "-DFUNC=isinf", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isinf [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isinf(val) ? 1 : 0;
        }))}});

    registerTest(TestData{"isnan_vector", relationalFlags, &UNARY_FUNCTION, "-DFUNC=isnan", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnan [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isnan(val) ? -1 : 0;
        }))}});

    registerTest(TestData{"isnan_scalar", relationalFlags, &UNARY_FUNCTION_SCALAR, "-DFUNC=isnan", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnan [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isnan(val) ? 1 : 0;
        }))}});

    registerTest(TestData{"isnormal_vector", relationalFlags, &UNARY_FUNCTION, "-DFUNC=isnormal", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnormal [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isnormal(val) ? -1 : 0;
        }))}});

    registerTest(TestData{"isnormal_scalar", relationalFlags, &UNARY_FUNCTION_SCALAR, "-DFUNC=isnormal", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnormal [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isnormal(val) ? 1 : 0;
        }))}});

    ////
    // Other unary functions
    ////

    registerTest(TestData{"signbit_vector", generalFlags, &UNARY_FUNCTION, "-DFUNC=signbit", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size()),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] signbit [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::signbit(val) ? -1 : 0;
        }))}});

    registerTest(TestData{"signbit_scalar", generalFlags, &UNARY_FUNCTION_SCALAR, "-DFUNC=signbit", "test",
        {toBufferParameter(std::vector<int32_t>(values.size(), 0x42)), toBufferParameter(std::vector<float>(values))},
        calculateDimensions(values.size(), 1),
        {checkParameterEquals(0, transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] signbit [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::signbit(val) ? 1 : 0;
        }))}});

    ////
    // Grouping predicates
    ////

    registerTest(TestData{"any_long", groupingFlags | DataFilter::USES_LONG, &UNARY_GROUPED_FUNCTION,
        "-DTYPE=long16 -DFUNC=any", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int64_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 1})}});

    registerTest(TestData{"any_int", groupingFlags, &UNARY_GROUPED_FUNCTION, "-DTYPE=int16 -DFUNC=any", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int32_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 1})}});

    registerTest(TestData{"any_short", groupingFlags, &UNARY_GROUPED_FUNCTION, "-DTYPE=short16 -DFUNC=any", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int16_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 1})}});

    registerTest(TestData{"any_char", groupingFlags, &UNARY_GROUPED_FUNCTION, "-DTYPE=char16 -DFUNC=any", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int8_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 1})}});

    registerTest(TestData{"all_long", groupingFlags | DataFilter::USES_LONG, &UNARY_GROUPED_FUNCTION,
        "-DTYPE=long16 -DFUNC=all", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int64_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 0})}});

    registerTest(TestData{"all_int", groupingFlags, &UNARY_GROUPED_FUNCTION, "-DTYPE=int16 -DFUNC=all", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int32_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 0})}});

    registerTest(TestData{"all_short", groupingFlags, &UNARY_GROUPED_FUNCTION, "-DTYPE=short16 -DFUNC=all", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int16_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 0})}});

    registerTest(TestData{"all_char", groupingFlags, &UNARY_GROUPED_FUNCTION, "-DTYPE=char16 -DFUNC=all", "test",
        {toBufferParameter(std::vector<int32_t>(3, 0x42)), toBufferParameter(generateAnyAllInputs<int8_t, 16>())},
        toDimensions(3), {checkParameterEquals(0, std::vector<int32_t>{0, 1, 0})}});

    ////
    // Ternary selections
    ////

    // we have a total of 2 (compare bit set/clear) * 2 (actually used bit set/clear) * 2 (other bit set/clear)
    // possibilities how a result bit can receive its value, which fits into a single value for all types
    uint8_t bitselectMask = 0b11110000;
    uint8_t bitselectFirst = 0b00110011;
    uint8_t bitselectSecond = 0b01010101;
    auto bitselectResult = static_cast<uint8_t>((~bitselectMask & bitselectFirst) | (bitselectMask & bitselectSecond));

    registerTest(
        TestData{"bitselect_long", DataFilter::USES_LONG, &TERNARY_FUNCTION, "-DTYPE=long -DFUNC=bitselect", "test",
            {toBufferParameter(std::vector<int64_t>(1)), toBufferParameter(replicate<int64_t>(bitselectFirst)),
                toBufferParameter(replicate<int64_t>(bitselectSecond)),
                toBufferParameter(replicate<int64_t>(bitselectMask))},
            toDimensions(1), {checkParameterEquals(0, replicate<int64_t>(bitselectResult))}});

    registerTest(
        TestData{"bitselect_ulong", DataFilter::USES_LONG, &TERNARY_FUNCTION, "-DTYPE=ulong -DFUNC=bitselect", "test",
            {toBufferParameter(std::vector<uint64_t>(1)), toBufferParameter(replicate<uint64_t>(bitselectFirst)),
                toBufferParameter(replicate<uint64_t>(bitselectSecond)),
                toBufferParameter(replicate<uint64_t>(bitselectMask))},
            toDimensions(1), {checkParameterEquals(0, replicate<uint64_t>(bitselectResult))}});

    registerTest(TestData{"bitselect_int", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=int -DFUNC=bitselect", "test",
        {toBufferParameter(std::vector<int32_t>(1)), toBufferParameter(replicate<int32_t>(bitselectFirst)),
            toBufferParameter(replicate<int32_t>(bitselectSecond)),
            toBufferParameter(replicate<int32_t>(bitselectMask))},
        toDimensions(1), {checkParameterEquals(0, replicate<int32_t>(bitselectResult))}});

    registerTest(TestData{"bitselect_uint", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=uint -DFUNC=bitselect", "test",
        {toBufferParameter(std::vector<uint32_t>(1)), toBufferParameter(replicate<uint32_t>(bitselectFirst)),
            toBufferParameter(replicate<uint32_t>(bitselectSecond)),
            toBufferParameter(replicate<uint32_t>(bitselectMask))},
        toDimensions(1), {checkParameterEquals(0, replicate<uint32_t>(bitselectResult))}});

    registerTest(
        TestData{"bitselect_float", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=float -DFUNC=bitselect", "test",
            {toBufferParameter(std::vector<uint32_t>(1)), toBufferParameter(replicate<uint32_t>(bitselectFirst)),
                toBufferParameter(replicate<uint32_t>(bitselectSecond)),
                toBufferParameter(replicate<uint32_t>(bitselectMask))},
            toDimensions(1), {checkParameterEquals(0, replicate<uint32_t>(bitselectResult))}});

    registerTest(
        TestData{"bitselect_short", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=short -DFUNC=bitselect", "test",
            {toBufferParameter(std::vector<int16_t>(1)), toBufferParameter(replicate<int16_t>(bitselectFirst)),
                toBufferParameter(replicate<int16_t>(bitselectSecond)),
                toBufferParameter(replicate<int16_t>(bitselectMask))},
            toDimensions(1), {checkParameterEquals(0, replicate<int16_t>(bitselectResult))}});

    registerTest(
        TestData{"bitselect_ushort", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=ushort -DFUNC=bitselect", "test",
            {toBufferParameter(std::vector<uint16_t>(1)), toBufferParameter(replicate<uint16_t>(bitselectFirst)),
                toBufferParameter(replicate<uint16_t>(bitselectSecond)),
                toBufferParameter(replicate<uint16_t>(bitselectMask))},
            toDimensions(1), {checkParameterEquals(0, replicate<uint16_t>(bitselectResult))}});

    registerTest(TestData{"bitselect_char", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=char -DFUNC=bitselect", "test",
        {toBufferParameter(std::vector<int8_t>(1)), toBufferParameter(replicate<int8_t>(bitselectFirst)),
            toBufferParameter(replicate<int8_t>(bitselectSecond)), toBufferParameter(replicate<int8_t>(bitselectMask))},
        toDimensions(1), {checkParameterEquals(0, replicate<int8_t>(bitselectResult))}});

    registerTest(
        TestData{"bitselect_uchar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=uchar -DFUNC=bitselect", "test",
            {toBufferParameter(std::vector<uint8_t>(1)), toBufferParameter(replicate<uint8_t>(bitselectFirst)),
                toBufferParameter(replicate<uint8_t>(bitselectSecond)),
                toBufferParameter(replicate<uint8_t>(bitselectMask))},
            toDimensions(1), {checkParameterEquals(0, replicate<uint8_t>(bitselectResult))}});

    // there are only 2 possibilities:
    // 1. condition is true (MSB in vector element set or scalar != 0) -> second value is selected
    // 2. condition is false (MSB in vector element clear or scalar == 0) -> first value is selected

    registerTest(
        TestData{"select_long_vector", DataFilter::USES_LONG, &TERNARY_FUNCTION, "-DTYPE=long2 -DFUNC=select", "test",
            {toBufferParameter(std::vector<int64_t>(2)),
                toBufferParameter(std::vector<int64_t>{0x4041424344454647, 0x4041424344454647}),
                toBufferParameter(std::vector<int64_t>{0x1011121314151617, 0x1011121314151617}),
                toBufferParameter(std::vector<int64_t>{-1, 1})},
            toDimensions(1), {checkParameterEquals(0, std::vector<int64_t>{0x1011121314151617, 0x4041424344454647})}});

    registerTest(
        TestData{"select_long_scalar", DataFilter::USES_LONG, &TERNARY_FUNCTION, "-DTYPE=long -DFUNC=select", "test",
            {toBufferParameter(std::vector<int64_t>(2)),
                toBufferParameter(std::vector<int64_t>{0x4041424344454647, 0x4041424344454647}),
                toBufferParameter(std::vector<int64_t>{0x1011121314151617, 0x1011121314151617}),
                toBufferParameter(std::vector<int64_t>{1, 0})},
            toDimensions(2), {checkParameterEquals(0, std::vector<int64_t>{0x1011121314151617, 0x4041424344454647})}});

    registerTest(
        TestData{"select_ulong_vector", DataFilter::USES_LONG, &TERNARY_FUNCTION, "-DTYPE=ulong2 -DFUNC=select", "test",
            {toBufferParameter(std::vector<uint64_t>(2)),
                toBufferParameter(std::vector<uint64_t>{0x4041424344454647, 0x4041424344454647}),
                toBufferParameter(std::vector<uint64_t>{0x1011121314151617, 0x1011121314151617}),
                toBufferParameter(std::vector<uint64_t>{0x80123456789ABCDE, 1})},
            toDimensions(1), {checkParameterEquals(0, std::vector<uint64_t>{0x1011121314151617, 0x4041424344454647})}});

    registerTest(
        TestData{"select_ulong_scalar", DataFilter::USES_LONG, &TERNARY_FUNCTION, "-DTYPE=ulong -DFUNC=select", "test",
            {toBufferParameter(std::vector<uint64_t>(2)),
                toBufferParameter(std::vector<uint64_t>{0x4041424344454647, 0x4041424344454647}),
                toBufferParameter(std::vector<uint64_t>{0x1011121314151617, 0x1011121314151617}),
                toBufferParameter(std::vector<uint64_t>{1, 0})},
            toDimensions(2), {checkParameterEquals(0, std::vector<uint64_t>{0x1011121314151617, 0x4041424344454647})}});

    registerTest(TestData{"select_int_vector", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=int2 -DFUNC=select", "test",
        {toBufferParameter(std::vector<int32_t>(2)), toBufferParameter(std::vector<int32_t>{0x40414243, 0x40414243}),
            toBufferParameter(std::vector<int32_t>{0x10111213, 0x10111213}),
            toBufferParameter(std::vector<int32_t>{-1, 1})},
        toDimensions(1), {checkParameterEquals(0, std::vector<int32_t>{0x10111213, 0x40414243})}});

    registerTest(TestData{"select_int_scalar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=int -DFUNC=select", "test",
        {toBufferParameter(std::vector<int32_t>(2)), toBufferParameter(std::vector<int32_t>{0x40414243, 0x40414243}),
            toBufferParameter(std::vector<int32_t>{0x10111213, 0x10111213}),
            toBufferParameter(std::vector<int32_t>{1, 0})},
        toDimensions(2), {checkParameterEquals(0, std::vector<int32_t>{0x10111213, 0x40414243})}});

    registerTest(TestData{"select_uint_vector", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=uint2 -DFUNC=select",
        "test",
        {toBufferParameter(std::vector<uint32_t>(2)), toBufferParameter(std::vector<uint32_t>{0x40414243, 0x40414243}),
            toBufferParameter(std::vector<uint32_t>{0x10111213, 0x10111213}),
            toBufferParameter(std::vector<uint32_t>{0x80123456, 1})},
        toDimensions(1), {checkParameterEquals(0, std::vector<uint32_t>{0x10111213, 0x40414243})}});

    registerTest(TestData{"select_uint_scalar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=uint -DFUNC=select",
        "test",
        {toBufferParameter(std::vector<uint32_t>(2)), toBufferParameter(std::vector<uint32_t>{0x40414243, 0x40414243}),
            toBufferParameter(std::vector<uint32_t>{0x10111213, 0x10111213}),
            toBufferParameter(std::vector<uint32_t>{1, 0})},
        toDimensions(2), {checkParameterEquals(0, std::vector<uint32_t>{0x10111213, 0x40414243})}});

    registerTest(TestData{"select_short_vector", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=short2 -DFUNC=select",
        "test",
        {toBufferParameter(std::vector<int16_t>(2)), toBufferParameter(std::vector<int16_t>{0x4041, 0x4041}),
            toBufferParameter(std::vector<int16_t>{0x1011, 0x1011}), toBufferParameter(std::vector<int16_t>{-1, 1})},
        toDimensions(1), {checkParameterEquals(0, std::vector<int16_t>{0x1011, 0x4041})}});

    registerTest(
        TestData{"select_short_scalar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=short -DFUNC=select", "test",
            {toBufferParameter(std::vector<int16_t>(2)), toBufferParameter(std::vector<int16_t>{0x4041, 0x4041}),
                toBufferParameter(std::vector<int16_t>{0x1011, 0x1011}), toBufferParameter(std::vector<int16_t>{1, 0})},
            toDimensions(2), {checkParameterEquals(0, std::vector<int16_t>{0x1011, 0x4041})}});

    registerTest(
        TestData{"select_ushort_vector", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=ushort2 -DFUNC=select", "test",
            {toBufferParameter(std::vector<uint16_t>(2)), toBufferParameter(std::vector<uint16_t>{0x4041, 0x4041}),
                toBufferParameter(std::vector<uint16_t>{0x1011, 0x1011}),
                toBufferParameter(std::vector<uint16_t>{0x8012, 1})},
            toDimensions(1), {checkParameterEquals(0, std::vector<uint16_t>{0x1011, 0x4041})}});

    registerTest(TestData{"select_ushort_scalar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=ushort -DFUNC=select",
        "test",
        {toBufferParameter(std::vector<uint16_t>(2)), toBufferParameter(std::vector<uint16_t>{0x4041, 0x4041}),
            toBufferParameter(std::vector<uint16_t>{0x1011, 0x1011}), toBufferParameter(std::vector<uint16_t>{1, 0})},
        toDimensions(2), {checkParameterEquals(0, std::vector<uint16_t>{0x1011, 0x4041})}});

    registerTest(
        TestData{"select_char_vector", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=char2 -DFUNC=select", "test",
            {toBufferParameter(std::vector<int8_t>(2)), toBufferParameter(std::vector<int8_t>{0x40, 0x40}),
                toBufferParameter(std::vector<int8_t>{0x10, 0x10}), toBufferParameter(std::vector<int8_t>{-1, 1})},
            toDimensions(1), {checkParameterEquals(0, std::vector<int8_t>{0x10, 0x40})}});

    registerTest(
        TestData{"select_char_scalar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=char -DFUNC=select", "test",
            {toBufferParameter(std::vector<int8_t>(2)), toBufferParameter(std::vector<int8_t>{0x40, 0x40}),
                toBufferParameter(std::vector<int8_t>{0x10, 0x10}), toBufferParameter(std::vector<int8_t>{1, 0})},
            toDimensions(2), {checkParameterEquals(0, std::vector<int8_t>{0x10, 0x40})}});

    registerTest(
        TestData{"select_uchar_vector", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=uchar2 -DFUNC=select", "test",
            {toBufferParameter(std::vector<uint8_t>(2)), toBufferParameter(std::vector<uint8_t>{0x40, 0x40}),
                toBufferParameter(std::vector<uint8_t>{0x10, 0x10}), toBufferParameter(std::vector<uint8_t>{0x80, 1})},
            toDimensions(1), {checkParameterEquals(0, std::vector<uint8_t>{0x10, 0x40})}});

    registerTest(
        TestData{"select_uchar_scalar", DataFilter::NONE, &TERNARY_FUNCTION, "-DTYPE=uchar -DFUNC=select", "test",
            {toBufferParameter(std::vector<uint8_t>(2)), toBufferParameter(std::vector<uint8_t>{0x40, 0x40}),
                toBufferParameter(std::vector<uint8_t>{0x10, 0x10}), toBufferParameter(std::vector<uint8_t>{1, 0})},
            toDimensions(2), {checkParameterEquals(0, std::vector<uint8_t>{0x10, 0x40})}});
}
