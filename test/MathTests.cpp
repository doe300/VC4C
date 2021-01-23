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

static const std::string BINARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global float16* in1) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid]);
}
)";

static const std::string TERNARY_FUNCTION = R"(
__kernel void test(__global float16* out, __global float16* in0, __global float16* in1, __global float16* in2) {
  size_t gid = get_global_id(0);
  out[gid] = FUNC(in0[gid], in1[gid], in2[gid]);
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
    // TODO missing all functions with more complex signature (int types or pointers)

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
        {"log", logf, 4, DataFilter::DISABLED},
        {"log2", log2f, 4, DataFilter::DISABLED},
        {"log10", log10f, 4, DataFilter::DISABLED},
        {"log1p", log1pf, 4, DataFilter::DISABLED},
        {"logb", logbf, 0, DataFilter::DISABLED},
        // TODO host truncates to zero where it should not
        {"rint", rintf, 0, DataFilter::DISABLED},
        {"round", roundf, 0},
        {"rsqrt", rsqrt, 4, DataFilter::DISABLED},
        {"sin", sinf, 4, DataFilter::DISABLED},
        {"sinh", sinhf, 4, DataFilter::DISABLED},
        {"sinpi", sinpi, 4, DataFilter::DISABLED},
        {"sqrt", sqrtf, 4, DataFilter::DISABLED},
        {"tan", tanf, 5, DataFilter::DISABLED},
        {"tanh", tanhf, 5, DataFilter::DISABLED},
        {"tanpi", tanpi, 6, DataFilter::DISABLED},
        {"tgamma", tgammaf, 16, DataFilter::DISABLED},
        {"trunc", truncf, 0},
    };

    for(const auto& func : unaryFunctions)
    {
        registerTest(TestData{func.name, defaultFlags | func.flags, &UNARY_FUNCTION, "-DFUNC=" + func.name, "test",
            {toBufferParameter(std::vector<float>(values.size(), 42.0f)),
                toBufferParameter(std::vector<float>(values))},
            calculateDimensions(values.size()),
            {checkParameter<CompareDynamicULP>(
                0, transform<float>(values, test_data::roundToZero(func.func)), CompareDynamicULP{func.ulp})}});
    }

    const std::vector<BinaryFunction> binaryFunctions = {
        {"atan2", atan2f, 6, DataFilter::DISABLED},
        {"atan2pi", atan2pi, 6, DataFilter::DISABLED},
        {"copysign", copysignf, 0},
        {"fdim", fdimf, 0},
        {"fmax", fmaxf, 0},
        {"fmin", fminf, 0},
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
        registerTest(TestData{func.name, defaultFlags | func.flags, &BINARY_FUNCTION, "-DFUNC=" + func.name, "test",
            {toBufferParameter(std::vector<float>(productLeft.size(), 42.0f)),
                toBufferParameter(std::vector<float>(productLeft)),
                toBufferParameter(std::vector<float>(productRight))},
            calculateDimensions(productLeft.size()),
            {checkParameter<CompareDynamicULP>(0,
                transform<float>(productLeft, productRight, test_data::roundToZero(func.func)),
                CompareDynamicULP{func.ulp})}});
    }
}
