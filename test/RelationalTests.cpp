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
        -limits::quiet_NaN()};

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

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isequal_vector", BINARY_FUNCTION, "test", "-DFUNC=isequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isequal [...] shall return a [...] 0 if the specified relation is false and a -1
                // (i.e. all bits set) if the specified relation is true for vector argument types."
                // "The relational functions isequal [...] always return 0 if either argument is not a number (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a == b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isequal_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isequal [...] shall return a 0 if the specified relation is false and a 1 if the
                // specified relation is true for scalar argument types."
                // "The relational functions isequal [...] always return 0 if either argument is not a number (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a == b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isnotequal_vector", BINARY_FUNCTION, "test", "-DFUNC=isnotequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isnotequal [...] shall return a [...] 0 if the specified relation is false and a
                // -1 (i.e. all bits set) if the specified relation is true for vector argument types." "isnotequal
                // returns returns -1 if one or both arguments are not a number (NaN) and the argument type is a
                // vector."
                return (std::isnan(a) || std::isnan(b)) ? -1 : (a != b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isnotequal_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isnotequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isnotequal [...] shall return a 0 if the specified relation is false and a 1 if
                // the specified relation is true for scalar argument types." "isnotequal returns 1 if one or both
                // arguments are not a number (NaN) and the argument type is a scalar."
                return (std::isnan(a) || std::isnan(b)) ? 1 : (a != b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isgreater_vector", BINARY_FUNCTION, "test", "-DFUNC=isgreater");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isgreater [...] shall return a [...] 0 if the specified relation is false and a -1
                // (i.e. all bits set) if the specified relation is true for vector argument types."
                // "The relational functions [...] isgreater [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a > b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isgreater_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isgreater");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isgreater [...] shall return a 0 if the specified relation is false and a 1 if the
                // specified relation is true for scalar argument types."
                // "The relational functions [...] isgreater [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a > b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isgreaterequal_vector", BINARY_FUNCTION, "test", "-DFUNC=isgreaterequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isgreaterequal [...] shall return a [...] 0 if the specified relation is false and
                // a -1 (i.e. all bits set) if the specified relation is true for vector argument types." "The
                // relational functions [...] isgreaterequal [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a >= b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isgreaterequal_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isgreaterequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isgreaterequal [...] shall return a 0 if the specified relation is false and a 1
                // if the specified relation is true for scalar argument types." "The relational functions [...]
                // isgreaterequal [...] always return 0 if either argument is not a number (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a >= b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isless_vector", BINARY_FUNCTION, "test", "-DFUNC=isless");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isless [...] shall return a [...] 0 if the specified relation is false and a -1
                // (i.e. all bits set) if the specified relation is true for vector argument types."
                // "The relational functions [...] isless [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isless_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isless");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isless [...] shall return a 0 if the specified relation is false and a 1 if the
                // specified relation is true for scalar argument types."
                // "The relational functions [...] isless [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "islessequal_vector", BINARY_FUNCTION, "test", "-DFUNC=islessequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] islessequal [...] shall return a [...] 0 if the specified relation is false and a
                // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
                // "The relational functions [...] islessequal [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a <= b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "islessequal_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=islessequal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] islessequal [...] shall return a 0 if the specified relation is false and a 1 if
                // the specified relation is true for scalar argument types."
                // "The relational functions [...] islessequal [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a <= b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "islessgreater_vector", BINARY_FUNCTION, "test", "-DFUNC=islessgreater");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] islessgreater [...] shall return a [...] 0 if the specified relation is false and
                // a -1 (i.e. all bits set) if the specified relation is true for vector argument types." "The
                // relational functions [...] islessgreater [...] always return 0 if either argument is not a number
                // (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b || a > b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "islessgreater_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=islessgreater");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] islessgreater [...] shall return a 0 if the specified relation is false and a 1 if
                // the specified relation is true for scalar argument types."
                // "The relational functions [...] islessgreater [...] always return 0 if either argument is not a
                // number (NaN)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a < b || a > b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isordered_vector", BINARY_FUNCTION, "test", "-DFUNC=isordered");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isordered [...] shall return a [...] 0 if the specified relation is false and a -1
                // (i.e. all bits set) if the specified relation is true for vector argument types."
                // "Test if arguments are ordered. isordered() takes arguments x and y, and returns the result
                // isequal(x, x)
                // && isequal(y, y)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a == a && b == b) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isordered_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isordered");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isordered [...] shall return a 0 if the specified relation is false and a 1 if the
                // specified relation is true for scalar argument types."
                // "Test if arguments are ordered. isordered() takes arguments x and y, and returns the result
                // isequal(x, x)
                // && isequal(y, y)."
                return (std::isnan(a) || std::isnan(b)) ? 0 : (a == a && b == b) ? 1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isunordered_vector", BINARY_FUNCTION, "test", "-DFUNC=isunordered");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isunordered [...] shall return a [...] 0 if the specified relation is false and a
                // -1 (i.e. all bits set) if the specified relation is true for vector argument types." "Test if
                // arguments are unordered. isunordered() takes arguments x and y, returning non-zero if x or y is NaN,
                // and zero otherwise."
                return (std::isnan(a) || std::isnan(b)) ? -1 : 0;
            }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>, Buffer<float>> builder(
            "isunordered_scalar", BINARY_FUNCTION_SCALAR, "test", "-DFUNC=isunordered");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(productLeft.size(), 1);
        builder.allocateParameter<0>(productLeft.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<int32_t, float>(productLeft, productRight, [](float a, float b) -> int32_t {
                // "The function[...] isunordered [...] shall return a 0 if the specified relation is false and a 1 if
                // the specified relation is true for scalar argument types." "Test if arguments are unordered.
                // isunordered() takes arguments x and y, returning non-zero if x or y is NaN, and zero otherwise."
                return (std::isnan(a) || std::isnan(b)) ? 1 : 0;
            }));
    }

    ////
    // Unary predicates
    ////

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "isfinite_vector", UNARY_FUNCTION, "test", "-DFUNC=isfinite");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isfinite [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isinf(val) || std::isnan(val) ? 0 : -1;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "isfinite_scalar", UNARY_FUNCTION_SCALAR, "test", "-DFUNC=isfinite");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size(), 1);
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isfinite [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isinf(val) || std::isnan(val) ? 0 : 1;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder("isinf_vector", UNARY_FUNCTION, "test", "-DFUNC=isinf");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isinf [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isinf(val) ? -1 : 0;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "isinf_scalar", UNARY_FUNCTION_SCALAR, "test", "-DFUNC=isinf");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size(), 1);
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isinf [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isinf(val) ? 1 : 0;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder("isnan_vector", UNARY_FUNCTION, "test", "-DFUNC=isnan");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnan [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isnan(val) ? -1 : 0;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "isnan_scalar", UNARY_FUNCTION_SCALAR, "test", "-DFUNC=isnan");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size(), 1);
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnan [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isnan(val) ? 1 : 0;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "isnormal_vector", UNARY_FUNCTION, "test", "-DFUNC=isnormal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnormal [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::isnormal(val) ? -1 : 0;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "isnormal_scalar", UNARY_FUNCTION_SCALAR, "test", "-DFUNC=isnormal");
        builder.setFlags(relationalFlags);
        builder.calculateDimensions(values.size(), 1);
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] isnormal [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::isnormal(val) ? 1 : 0;
        }));
    }

    ////
    // Other unary functions
    ////

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "signbit_vector", UNARY_FUNCTION, "test", "-DFUNC=signbit");
        builder.setFlags(generalFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] signbit [...] shall return a 0 if the specified relation is false and a
            // -1 (i.e. all bits set) if the specified relation is true for vector argument types."
            return std::signbit(val) ? -1 : 0;
        }));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "signbit_scalar", UNARY_FUNCTION_SCALAR, "test", "-DFUNC=signbit");
        builder.setFlags(generalFlags);
        builder.calculateDimensions(values.size(), 1);
        builder.allocateParameter<0>(values.size(), 0x42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t, float>(values, [](float val) -> int32_t {
            // "The function[...] signbit [...] shall return a 0 if the specified relation is false and a 1 if the
            // specified relation is true for scalar argument types."
            return std::signbit(val) ? 1 : 0;
        }));
    }

    ////
    // Grouping predicates
    ////

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int64_t>> builder(
            "any_long", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=long16 -DFUNC=any");
        builder.setFlags(groupingFlags | DataFilter::USES_LONG);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int64_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 1});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "any_int", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=int16 -DFUNC=any");
        builder.setFlags(groupingFlags);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int32_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 1});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int16_t>> builder(
            "any_short", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=short16 -DFUNC=any");
        builder.setFlags(groupingFlags);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int16_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 1});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int8_t>> builder(
            "any_char", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=char16 -DFUNC=any");
        builder.setFlags(groupingFlags);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int8_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 1});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int64_t>> builder(
            "all_long", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=long16 -DFUNC=all");
        builder.setFlags(groupingFlags | DataFilter::USES_LONG);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int64_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 0});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "all_int", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=int16 -DFUNC=all");
        builder.setFlags(groupingFlags);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int32_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 0});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int16_t>> builder(
            "all_short", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=short16 -DFUNC=all");
        builder.setFlags(groupingFlags);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int16_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 0});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int8_t>> builder(
            "all_char", UNARY_GROUPED_FUNCTION, "test", "-DTYPE=char16 -DFUNC=all");
        builder.setFlags(groupingFlags);
        builder.setDimensions(3);
        builder.allocateParameter<0>(3, 0x42);
        builder.setParameter<1>(generateAnyAllInputs<int8_t, 16>());
        builder.checkParameterEquals<0>({0, 1, 0});
    }

    ////
    // Ternary selections
    ////

    // we have a total of 2 (compare bit set/clear) * 2 (actually used bit set/clear) * 2 (other bit set/clear)
    // possibilities how a result bit can receive its value, which fits into a single value for all types
    uint8_t bitselectMask = 0b11110000;
    uint8_t bitselectFirst = 0b00110011;
    uint8_t bitselectSecond = 0b01010101;
    auto bitselectResult = static_cast<uint8_t>((~bitselectMask & bitselectFirst) | (bitselectMask & bitselectSecond));

    {
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>> builder(
            "bitselect_long", TERNARY_FUNCTION, "test", "-DTYPE=long -DFUNC=bitselect");
        builder.setFlags(DataFilter::USES_LONG);
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<int64_t>(bitselectFirst));
        builder.setParameter<2>(replicate<int64_t>(bitselectSecond));
        builder.setParameter<3>(replicate<int64_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<int64_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<uint64_t>, Buffer<uint64_t>, Buffer<uint64_t>, Buffer<uint64_t>> builder(
            "bitselect_ulong", TERNARY_FUNCTION, "test", "-DTYPE=ulong -DFUNC=bitselect");
        builder.setFlags(DataFilter::USES_LONG);
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<uint64_t>(bitselectFirst));
        builder.setParameter<2>(replicate<uint64_t>(bitselectSecond));
        builder.setParameter<3>(replicate<uint64_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<uint64_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "bitselect_int", TERNARY_FUNCTION, "test", "-DTYPE=int -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<int32_t>(bitselectFirst));
        builder.setParameter<2>(replicate<int32_t>(bitselectSecond));
        builder.setParameter<3>(replicate<int32_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<int32_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "bitselect_uint", TERNARY_FUNCTION, "test", "-DTYPE=uint -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<uint32_t>(bitselectFirst));
        builder.setParameter<2>(replicate<uint32_t>(bitselectSecond));
        builder.setParameter<3>(replicate<uint32_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<uint32_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "bitselect_float", TERNARY_FUNCTION, "test", "-DTYPE=float -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<uint32_t>(bitselectFirst));
        builder.setParameter<2>(replicate<uint32_t>(bitselectSecond));
        builder.setParameter<3>(replicate<uint32_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<uint32_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>> builder(
            "bitselect_short", TERNARY_FUNCTION, "test", "-DTYPE=short -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<int16_t>(bitselectFirst));
        builder.setParameter<2>(replicate<int16_t>(bitselectSecond));
        builder.setParameter<3>(replicate<int16_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<int16_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>> builder(
            "bitselect_ushort", TERNARY_FUNCTION, "test", "-DTYPE=ushort -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<uint16_t>(bitselectFirst));
        builder.setParameter<2>(replicate<uint16_t>(bitselectSecond));
        builder.setParameter<3>(replicate<uint16_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<uint16_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>> builder(
            "bitselect_char", TERNARY_FUNCTION, "test", "-DTYPE=char -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<int8_t>(bitselectFirst));
        builder.setParameter<2>(replicate<int8_t>(bitselectSecond));
        builder.setParameter<3>(replicate<int8_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<int8_t>(bitselectResult));
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>> builder(
            "bitselect_uchar", TERNARY_FUNCTION, "test", "-DTYPE=uchar -DFUNC=bitselect");
        builder.allocateParameter<0>(1);
        builder.setParameter<1>(replicate<uint8_t>(bitselectFirst));
        builder.setParameter<2>(replicate<uint8_t>(bitselectSecond));
        builder.setParameter<3>(replicate<uint8_t>(bitselectMask));
        builder.checkParameterEquals<0>(replicate<uint8_t>(bitselectResult));
    }

    // there are only 2 possibilities:
    // 1. condition is true (MSB in vector element set or scalar != 0) -> second value is selected
    // 2. condition is false (MSB in vector element clear or scalar == 0) -> first value is selected

    {
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>> builder(
            "select_long_vector", TERNARY_FUNCTION, "test", "-DTYPE=long2 -DFUNC=select");
        builder.setFlags(DataFilter::USES_LONG);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041424344454647, 0x4041424344454647});
        builder.setParameter<2>({0x1011121314151617, 0x1011121314151617});
        builder.setParameter<3>({-1, 1});
        builder.checkParameterEquals<0>({0x1011121314151617, 0x4041424344454647});
    }

    {
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>> builder(
            "select_long_scalar", TERNARY_FUNCTION, "test", "-DTYPE=long -DFUNC=select");
        builder.setFlags(DataFilter::USES_LONG);
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041424344454647, 0x4041424344454647});
        builder.setParameter<2>({0x1011121314151617, 0x1011121314151617});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x1011121314151617, 0x4041424344454647});
    }

    {
        TestDataBuilder<Buffer<uint64_t>, Buffer<uint64_t>, Buffer<uint64_t>, Buffer<uint64_t>> builder(
            "select_ulong_vector", TERNARY_FUNCTION, "test", "-DTYPE=ulong2 -DFUNC=select");
        builder.setFlags(DataFilter::USES_LONG);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041424344454647, 0x4041424344454647});
        builder.setParameter<2>({0x1011121314151617, 0x1011121314151617});
        builder.setParameter<3>({0x80123456789ABCDE, 1});
        builder.checkParameterEquals<0>({0x1011121314151617, 0x4041424344454647});
    }

    {
        TestDataBuilder<Buffer<uint64_t>, Buffer<uint64_t>, Buffer<uint64_t>, Buffer<uint64_t>> builder(
            "select_ulong_scalar", TERNARY_FUNCTION, "test", "-DTYPE=ulong -DFUNC=select");
        builder.setFlags(DataFilter::USES_LONG);
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041424344454647, 0x4041424344454647});
        builder.setParameter<2>({0x1011121314151617, 0x1011121314151617});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x1011121314151617, 0x4041424344454647});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_int_vector", TERNARY_FUNCTION, "test", "-DTYPE=int2 -DFUNC=select");
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40414243, 0x40414243});
        builder.setParameter<2>({0x10111213, 0x10111213});
        builder.setParameter<3>({-1, 1});
        builder.checkParameterEquals<0>({0x10111213, 0x40414243});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_int_scalar", TERNARY_FUNCTION, "test", "-DTYPE=int -DFUNC=select");
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40414243, 0x40414243});
        builder.setParameter<2>({0x10111213, 0x10111213});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x10111213, 0x40414243});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "select_uint_vector", TERNARY_FUNCTION, "test", "-DTYPE=uint2 -DFUNC=select");
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40414243, 0x40414243});
        builder.setParameter<2>({0x10111213, 0x10111213});
        builder.setParameter<3>({0x80123456, 1});
        builder.checkParameterEquals<0>({0x10111213, 0x40414243});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "select_uint_scalar", TERNARY_FUNCTION, "test", "-DTYPE=uint -DFUNC=select");
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40414243, 0x40414243});
        builder.setParameter<2>({0x10111213, 0x10111213});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x10111213, 0x40414243});
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>> builder(
            "select_short_vector", TERNARY_FUNCTION, "test", "-DTYPE=short2 -DFUNC=select");
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041, 0x4041});
        builder.setParameter<2>({0x1011, 0x1011});
        builder.setParameter<3>({-1, 1});
        builder.checkParameterEquals<0>({0x1011, 0x4041});
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>> builder(
            "select_short_scalar", TERNARY_FUNCTION, "test", "-DTYPE=short -DFUNC=select");
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041, 0x4041});
        builder.setParameter<2>({0x1011, 0x1011});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x1011, 0x4041});
    }

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>> builder(
            "select_ushort_vector", TERNARY_FUNCTION, "test", "-DTYPE=ushort2 -DFUNC=select");
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041, 0x4041});
        builder.setParameter<2>({0x1011, 0x1011});
        builder.setParameter<3>({0x8012, 1});
        builder.checkParameterEquals<0>({0x1011, 0x4041});
    }

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>> builder(
            "select_ushort_scalar", TERNARY_FUNCTION, "test", "-DTYPE=ushort -DFUNC=select");
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x4041, 0x4041});
        builder.setParameter<2>({0x1011, 0x1011});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x1011, 0x4041});
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>> builder(
            "select_char_vector", TERNARY_FUNCTION, "test", "-DTYPE=char2 -DFUNC=select");
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40, 0x40});
        builder.setParameter<2>({0x10, 0x10});
        builder.setParameter<3>({-1, 1});
        builder.checkParameterEquals<0>({0x10, 0x40});
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>> builder(
            "select_char_scalar", TERNARY_FUNCTION, "test", "-DTYPE=char -DFUNC=select");
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40, 0x40});
        builder.setParameter<2>({0x10, 0x10});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x10, 0x40});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>> builder(
            "select_uchar_vector", TERNARY_FUNCTION, "test", "-DTYPE=uchar2 -DFUNC=select");
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40, 0x40});
        builder.setParameter<2>({0x10, 0x10});
        builder.setParameter<3>({0x80, 1});
        builder.checkParameterEquals<0>({0x10, 0x40});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>> builder(
            "select_uchar_scalar", TERNARY_FUNCTION, "test", "-DTYPE=uchar -DFUNC=select");
        builder.setDimensions(2);
        builder.allocateParameter<0>(2);
        builder.setParameter<1>({0x40, 0x40});
        builder.setParameter<2>({0x10, 0x10});
        builder.setParameter<3>({1, 0});
        builder.checkParameterEquals<0>({0x10, 0x40});
    }
}
