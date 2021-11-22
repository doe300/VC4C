/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include <algorithm>
#include <cmath>
#include <limits>

static const std::string UNARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in[gid]);
}
)";

static const std::string UNARY_INT_FUNCTION = R"(
__kernel void test(__global int16* out, __global float16* in) {
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

static const std::string BINARY_INT_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global int16* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

static const std::string BINARY_POINTER_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global POINTER_TYPE* out1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], &out1[gid]);
}
)";

static const std::string TERNARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global float16* in1, __global float16* in2) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid], in2[gid]);
}
)";

static const std::string TERNARY_POINTER_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global float16* in1, __global POINTER_TYPE* out1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid], &out1[gid]);
}
)";

template <typename Func>
struct MathFunction
{
    std::string name;
    Func func;
    std::size_t ulp;
    test_data::DataFilter flags = test_data::DataFilter::NONE;
};

using UnaryFunction = MathFunction<std::function<float(float)>>;
using BinaryFunction = MathFunction<std::function<float(float, float)>>;

static float acospi(float f)
{
    return acosf(f) / static_cast<float>(M_PI);
}

static float asinpi(float f)
{
    return asinf(f) / static_cast<float>(M_PI);
}

static float atanpi(float f)
{
    return atanf(f) / static_cast<float>(M_PI);
}

static float atan2pi(float x, float y)
{
    return atan2f(x, y) / static_cast<float>(M_PI);
}

static float cospi(float f)
{
    return cosf(static_cast<float>(M_PI) * f);
}

static float maxmag(float x, float y)
{
    return std::fabs(x) > std::fabs(y) ? x : std::fabs(y) > std::fabs(x) ? y : std::fmax(x, y);
}

static float minmag(float x, float y)
{
    return std::fabs(x) < std::fabs(y) ? x : std::fabs(y) < std::fabs(x) ? y : std::fmin(x, y);
}

static float rsqrt(float f)
{
    return 1.0f / sqrtf(f);
}

static float sinpi(float f)
{
    return sinf(static_cast<float>(M_PI) * f);
}

static float tanpi(float f)
{
    return tanf(static_cast<float>(M_PI) * f);
}

/*
 * As of OpenCL C 1.2 specification, 7.5.2. Changes to C99 TC2 Behavior:
 *
 * modf behaves as though implemented by:
 */
static float openclc_modf(float value, float* iptr)
{
    *iptr = truncf(value);
    return copysignf(isinff(value) ? 0.0f : value - *iptr, value);
}

void test_data::registerMathTests()
{
    auto defaultFlags = DataFilter::FLOAT_ARITHMETIC;

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

    // TODO some of these functions do not support the full float range, how to handle?

    const std::vector<UnaryFunction> unaryFunctions = {
        {"acos", acosf, 4},
        {"acosh", acoshf, 4, DataFilter::DISABLED},
        {"acospi", acospi, 5},
        {"asin", asinf, 4},
        {"asinh", asinhf, 4, DataFilter::DISABLED},
        {"asinpi", asinpi, 5},
        {"atan", atanf, 5},
        {"atanh", atanhf, 5, DataFilter::DISABLED},
        {"atanpi", atanpi, 5},
        {"cbrt", cbrtf, 4, DataFilter::DISABLED},
        {"ceil", ceilf, 0},
        {"cos", cosf, 4, DataFilter::DISABLED},
        {"cosh", coshf, 4, DataFilter::DISABLED},
        {"cospi", cospi, 4, DataFilter::DISABLED},
        {"erfc", erfcf, 16, DataFilter::DISABLED},
        {"erf", erff, 16, DataFilter::DISABLED},
        {"exp", expf, 4, DataFilter::DISABLED},
        {"exp2", exp2f, 4, DataFilter::DISABLED},
        {"exp10", exp10f, 4, DataFilter::DISABLED},
        {"expm1", expm1f, 4, DataFilter::DISABLED},
        {"fabs", fabsf, 0},
        {"floor", floorf, 0},
        {"lgamma", lgammaf, 1024 /* maximum error is undefined */, DataFilter::DISABLED},
        {"log", logf, 4},
        {"log2", log2f, 4},
        {"log10", log10f, 4},
        {"log1p", log1pf, 4, DataFilter::DISABLED},
        {"logb", logbf, 0},
        // TODO host truncates to zero where it should not
        {"rint", rintf, 0, DataFilter::DISABLED},
        {"round", roundf, 0},
        {"rsqrt", rsqrt, 4},
        {"sin", sinf, 4, DataFilter::DISABLED},
        {"sinh", sinhf, 4, DataFilter::DISABLED},
        {"sinpi", sinpi, 4, DataFilter::DISABLED},
        {"sqrt", sqrtf, 4},
        {"tan", tanf, 5, DataFilter::DISABLED},
        {"tanh", tanhf, 5, DataFilter::DISABLED},
        {"tanpi", tanpi, 6, DataFilter::DISABLED},
        {"tgamma", tgammaf, 16, DataFilter::DISABLED},
        {"trunc", truncf, 0},
    };

    for(const auto& func : unaryFunctions)
    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            std::string(func.name), UNARY_FUNCTION, "test", "-DFUNC=" + func.name);
        builder.setFlags(defaultFlags | func.flags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameter<0, CompareDynamicULP>(
            transform<float>(values, test_data::roundToZero(func.func)), CompareDynamicULP{func.ulp});
    }

    const std::vector<BinaryFunction> binaryFunctions = {
        {"atan2", atan2f, 6, DataFilter::DISABLED},
        {"atan2pi", atan2pi, 6, DataFilter::DISABLED},
        {"copysign", copysignf, 0},
        {"fdim", fdimf, 0},
        // SPIR-V maps them directly to the fmin/fmax opcode which does not handle NaN correctly
        {"fmax", fmaxf, 0, DataFilter::SPIRV_DISABLED},
        {"fmin", fminf, 0, DataFilter::SPIRV_DISABLED},
        {"fmod", fmodf, 0, DataFilter::DISABLED},
        {"hypot", hypotf, 4, DataFilter::DISABLED},
        {"maxmag", maxmag, 0},
        {"minmag", minmag, 0},
        {"nextafter", nextafterf, 0},
        {"pow", powf, 16, DataFilter::DISABLED},
        {"powr", powf, 16, DataFilter::DISABLED},
        {"remainder", remainderf, 0, DataFilter::DISABLED},
    };

    for(const auto& func : binaryFunctions)
    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            std::string(func.name), BINARY_FUNCTION, "test", "-DFUNC=" + func.name);
        builder.setFlags(defaultFlags | func.flags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameter<0, CompareDynamicULP>(
            transform<float>(productLeft, productRight, test_data::roundToZero(func.func)),
            CompareDynamicULP{func.ulp});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "fract", BINARY_POINTER_FUNCTION, "test", "-DFUNC=fract -DPOINTER_TYPE=float16");
        builder.setFlags(defaultFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.allocateParameter<2>(values.size(), 42);
        builder.checkParameter<0, CompareULP<0>>(
            transform<float>(values, test_data::roundToZero<float>([](float x) -> float {
                return isnanf(x) ? x : fminf(x - floorf(x), std::nextafter(1.0f, 0.0f));
            })));
        builder.checkParameter<2, CompareULP<0>>(transform<float>(values, floorf));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<int32_t>> builder(
            "frexp", BINARY_POINTER_FUNCTION, "test", "-DFUNC=frexp -DPOINTER_TYPE=int16");
        builder.setFlags(defaultFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.allocateParameter<2>(values.size(), 42);
        builder.checkParameter<0, CompareULP<0>>(
            transform<float>(values, test_data::roundToZero<float>([](float x) -> float {
                int32_t dummyExp = 0;
                return frexpf(x, &dummyExp);
            })));
        builder.checkParameterEquals<2>(transform<int32_t>(values, [](float x) -> int32_t {
            int32_t exp = 0;
            frexpf(x, &exp);
            return exp;
        }));
    }

    {
        // TODO fails on hardware for +-0.0, returns INT_MIN instead of -INT_MAX returned by the host ilogb(0)
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder("ilogb", UNARY_INT_FUNCTION, "test", "-DFUNC=ilogb");
        builder.setFlags(defaultFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<int32_t>(values, [](float x) -> int32_t {
            // Clang defines FP_ILOGBNAN to INT_MAX, which might differ from host compiler definition
            return std::isnan(x) ? std::numeric_limits<int32_t>::max() : ilogbf(x);
        }));
    }

    std::vector<int32_t> exponents(productLeft.size());
    auto it = exponents.begin();
    for(auto exp :
        {0, 1, -1, 16, -16, 32, -32, 64, -64, 127, -127, 128, -128, 256, -256, std::numeric_limits<int32_t>::max()})
    {
        it = std::fill_n(it, 16, exp);
    }
    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<int32_t>> builder(
            "ldexp", BINARY_INT_FUNCTION, "test", "-DFUNC=ldexp");
        builder.setFlags(defaultFlags);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<int32_t>(exponents));
        builder.checkParameter<0, CompareULP<0>>(transform<float>(productLeft, exponents, ldexpf));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<int32_t>> builder(
            "lgamma_r", BINARY_POINTER_FUNCTION, "test", "-DFUNC=lgamma_r -DPOINTER_TYPE=int16");
        builder.setFlags(defaultFlags | DataFilter::DISABLED);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.allocateParameter<2>(values.size(), 42);
        builder.checkParameter<0, CompareULP<1024 /* maximum error is undefined */>>(
            transform<float>(values, test_data::roundToZero<float>(lgammaf)));
        builder.checkParameterEquals<2>(transform<int32_t>(values, [](float x) -> int32_t {
            int32_t sign = 0;
            lgammaf_r(x, &sign);
            return sign;
        }));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "modf", BINARY_POINTER_FUNCTION, "test", "-DFUNC=modf -DPOINTER_TYPE=float16");
        builder.setFlags(defaultFlags);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.allocateParameter<2>(values.size(), 42.0f);
        builder.checkParameter<0, CompareULP<0>>(
            transform<float>(values, test_data::roundToZero<float>([](float x) -> float {
                float dummy = 0;
                return openclc_modf(x, &dummy);
            })));
        builder.checkParameter<2, CompareULP<0>>(transform<float>(values, [](float x) -> float {
            float integral = 0;
            openclc_modf(x, &integral);
            return integral;
        }));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<int32_t>> builder(
            "pown", BINARY_INT_FUNCTION, "test", "-DFUNC=pown");
        builder.setFlags(defaultFlags | DataFilter::DISABLED);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<int32_t>(exponents));
        builder.checkParameter<0, CompareULP<16>>(transform<float>(productLeft, exponents,
            test_data::roundToZero<float>([](float x, int32_t y) -> float { return powf(x, static_cast<float>(y)); })));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<int32_t>> builder(
            "remquo", TERNARY_POINTER_FUNCTION, "test", "-DFUNC=remquo -DPOINTER_TYPE=int16");
        builder.setFlags(defaultFlags | DataFilter::DISABLED);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.allocateParameter<3>(productLeft.size(), 42);
        builder.checkParameter<0, CompareULP<0>>(
            transform<float>(productLeft, productRight, test_data::roundToZero<float>([](float x, float y) -> float {
                int32_t dummy = 0;
                return remquof(x, y, &dummy);
            })));
        builder.checkParameterEquals<3>(transform<int32_t>(productLeft, productRight, [](float x, float y) -> int32_t {
            int32_t quotient = 0;
            remquof(x, y, &quotient);
            return quotient;
        }));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "sincos", BINARY_POINTER_FUNCTION, "test", "-DFUNC=sincos -DPOINTER_TYPE=float16");
        builder.setFlags(defaultFlags | DataFilter::DISABLED);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.allocateParameter<2>(values.size(), 42.0f);
        builder.checkParameter<0, CompareULP<4>>(transform<float>(values, test_data::roundToZero<float>(sinf)));
        builder.checkParameter<2, CompareULP<4>>(transform<float>(values, test_data::roundToZero<float>(cosf)));
    }
}
