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

static const std::string UNARY_GROUPED_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN* in) {
  size_t gid = get_global_id(0);
#if defined(TRIPLE) && TRIPLE == 3
  vstore3(FUNC(vload3(gid, (__global TYPE*)in)), gid, (__global TYPE*)out);
#elif defined(TRIPLE)
  out[gid] = FUNC(vload3(gid, (__global TYPE*)in));
#else
  out[gid] = FUNC(in[gid]);
#endif
}
)";

static const std::string BINARY_GROUPED_FUNCTION = R"(
__kernel void test(__global OUT* out, __global IN0* in0, __global IN1* in1) {
  size_t gid = get_global_id(0);
#if defined(TRIPLE) && TRIPLE == 3
  vstore3(FUNC(vload3(gid, (__global TYPE*)in0), vload3(gid, (__global TYPE*)in1)), gid, (__global TYPE*)out);
#elif defined(TRIPLE)
  out[gid] = FUNC(vload3(gid, (__global TYPE*)in0), vload3(gid, (__global TYPE*)in1));
#else
  out[gid] = FUNC(in0[gid], in1[gid]);
#endif
}
)";

static float smoothstep(float edge0, float edge1, float val) noexcept
{
    auto checkClamp = [](float x, float min, float max) { return std::fmin(std::fmax(x, min), max); };
    if(edge0 >= edge1 || std::isnan(edge0) || std::isnan(edge1) || std::isnan(val))
        // TODO this is not true, the result is undefined, not NaN and out calculation does not guarantee an NaN either
        return std::numeric_limits<float>::quiet_NaN();
    float t = checkClamp((val - edge0) / (edge1 - edge0), 0, 1);
    return t * t * (3 - 2 * t);
}

static std::array<float, 3> cross3(const float* in0, const float* in1, std::size_t groupSize)
{
    return std::array<float, 3>{
        in0[1] * in1[2] - in0[2] * in1[1], in0[2] * in1[0] - in0[0] * in1[2], in0[0] * in1[1] - in0[1] * in1[0]};
}

static std::array<float, 4> cross4(const float* in0, const float* in1, std::size_t groupSize)
{
    return std::array<float, 4>{
        in0[1] * in1[2] - in0[2] * in1[1], in0[2] * in1[0] - in0[0] * in1[2], in0[0] * in1[1] - in0[1] * in1[0], 0.0f};
}

static float dot(const float* in1, const float* in2, std::size_t N)
{
    float sum = 0.0f;
    for(std::size_t i = 0; i < N; ++i)
        sum += in1[i] * in2[i];
    return sum;
}

static float length(const float* in, std::size_t N)
{
    return std::sqrt(dot(in, in, N));
}

static float distance(const float* in1, const float* in2, std::size_t N)
{
    std::vector<float> diff(N);
    for(std::size_t i = 0; i < N; ++i)
        diff[i] = in1[i] - in2[i];
    return length(diff.data(), N);
}

template <std::size_t N>
static std::array<float, N> normalize(const float* in, std::size_t /* dummy */)
{
    std::array<float, N> res;
    auto l = length(in, N);
    for(std::size_t i = 0; i < N; ++i)
        res[i] = in[i] / l;
    return res;
}

/*
 * "absolute error tolerance of 'max * max * (3 * FLT_EPSILON)' per vector component, where max is the maximum input
 * operand magnitude"
 *
 * The code below is taken from the OpenCL-CTS:
 * https://github.com/KhronosGroup/OpenCL-CTS/blob/master/test_conformance/geometrics/test_geometrics.cpp
 */
template <std::size_t VectorSize>
static std::vector<float> calculateCrossAllowedErrors(const std::vector<float>& in0, const std::vector<float>& in1)
{
    auto numElements = std::min(in0.size(), in1.size());
    std::vector<float> result(numElements, 0.0f);

    for(std::size_t i = 0; i <= (numElements - 3); i += VectorSize)
    {
        auto error0 =
            std::max({std::abs(in0[i + 1]), std::abs(in1[i + 1]), std::abs(in0[i + 2]), std::abs(in1[i + 2])});
        auto error1 =
            std::max({std::abs(in0[i + 0]), std::abs(in1[i + 0]), std::abs(in0[i + 2]), std::abs(in1[i + 2])});
        auto error2 =
            std::max({std::abs(in0[i + 0]), std::abs(in1[i + 0]), std::abs(in0[i + 1]), std::abs(in1[i + 1])});

        result[i + 0] = error0 * error0 * 3.0f * std::numeric_limits<float>::epsilon();
        result[i + 1] = error1 * error1 * 3.0f * std::numeric_limits<float>::epsilon();
        result[i + 2] = error2 * error2 * 3.0f * std::numeric_limits<float>::epsilon();
    }

    return result;
}

/*
 * "absolute error tolerance of 'max * max * (2n - 1) * FLT_EPSILON', for vector width n and maximum input operand
 * magnitude max across all vector components"
 *
 * The code below is taken from the OpenCL-CTS:
 * https://github.com/KhronosGroup/OpenCL-CTS/blob/master/test_conformance/geometrics/test_geometrics.cpp
 */
template <std::size_t VectorSize>
static std::vector<float> calculateDotAllowedErrors(const std::vector<float>& in0, const std::vector<float>& in1)
{
    auto numElements = std::min(in0.size(), in1.size());
    std::vector<float> result(numElements / VectorSize, 0.0f);

    for(std::size_t i = 0; i <= (numElements - VectorSize); i += VectorSize)
    {
        float maxValue = 0.0f;
        for(std::size_t k = 0; k < VectorSize; ++k)
        {
            maxValue = std::max({maxValue, std::abs(in0[i + k]), std::abs(in1[i + k])});
        }
        auto errorTolerance =
            maxValue * maxValue * (2 * static_cast<float>(VectorSize) - 1) * std::numeric_limits<float>::epsilon();
        result[i / VectorSize] = errorTolerance;
    }

    return result;
}

void test_data::registerOpenCLCommonFunctionTests()
{
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

    auto smallRandomData0 = toRandom<float>(64, false, -128.0f, 128.0f);
    auto smallRandomData1 = toRandom<float>(64, false, -128.0f, 128.0f);
    auto smallRandomData2 = toRandom<float>(64, true, -128.0f, 128.0f);

    {
        // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "clamp", TERNARY_FUNCTION, "test", "-DFUNC=clamp");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.setParameter<2>(toRange<float>(-16.0f, 0.0f));
        builder.setParameter<3>(toRange<float>(0.0f, 16.0f));
        builder.checkParameterEquals<0>({0.0f, 0.0f, 1.0f, -1.0f, 0.5f, -0.5f, 6.0f, -9.0f, limits::min(),
            -limits::min(), 10.0f, -5.0f, 12.0f, -3.0f, -2.0f, -1.0f});
    }

    {
        // maximum error of 2 ULP not in OpenCL 1.2 standard, but in latest
        TestDataBuilder<Buffer<float>, Buffer<float>> builder("degrees", UNARY_FUNCTION, "test", "-DFUNC=degrees");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES | DataFilter::DISABLED);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameter<0, CompareULP<2>>(transform<float>(values, [](float val) -> float {
            // FIXME is not, C99 refers to IEEE 754 which states in section 6.2: "For an operation with quiet NaN
            // inputs, other than maximum and minimum operations,if a floating-point result is to be delivered the
            // result shall be a quiet NaN which should be one of the inputNaNs."
            return val * 57.295780181884765625f;
        }));
    }

    {
        // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
        // NOTE: we do not check NaN behavior for max/min, since it is undefined
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "maxf", BINARY_FUNCTION, "test", "-DFUNC=max");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size());
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameterEquals<0>(transform<float>(
            smallRandomData0, smallRandomData1, [](float a, float b) -> float { return std::max(a, b); }));
    }

    {
        // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
        // NOTE: we do not check NaN behavior for max/min, since it is undefined
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "minf", BINARY_FUNCTION, "test", "-DFUNC=min");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size());
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameterEquals<0>(transform<float>(
            smallRandomData0, smallRandomData1, [](float a, float b) -> float { return std::min(a, b); }));
    }

    {
        // maximum error not in OpenCL 1.2 standard, but in latest
        // for "full profile" it states: absolute error tolerance of 1e-3, for "embedded profile", the allowed error is
        // implementation defined
        // the third parameter must be in the range [0.0, 1.0], otherwise the result is UB
        auto normalizedValues = transform<float>(smallRandomData2, [](float f) { return std::abs(f) / 128.0f; });
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "mix", TERNARY_FUNCTION, "test", "-DFUNC=mix");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size());
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.setParameter<3>(std::vector<float>(normalizedValues));
        builder.checkParameter<0, CompareAbsoluteError>(
            transform<float>(smallRandomData0, smallRandomData1, normalizedValues,
                [](float a, float b, float c) -> float { return a + (b - a) * c; }),
            CompareAbsoluteError{1e-3f});
    }

    {
        // maximum error of 2 ULP not in OpenCL 1.2 standard, but in latest
        TestDataBuilder<Buffer<float>, Buffer<float>> builder("radians", UNARY_FUNCTION, "test", "-DFUNC=radians");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES | DataFilter::DISABLED);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameter<0, CompareULP<2>>(
            transform<float>(values, [](float val) -> float { return val * (static_cast<float>(M_PI) / 180.f); }));
    }

    {
        // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "step", BINARY_FUNCTION, "test", "-DFUNC=step");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES);
        builder.calculateDimensions(productLeft.size());
        builder.allocateParameter<0>(productLeft.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(productLeft));
        builder.setParameter<2>(std::vector<float>(productRight));
        builder.checkParameterEquals<0>(
            transform<float>(productLeft, productRight, [](float a, float b) -> float { return b < a ? 0.0f : 1.0f; }));
    }

    {
        // TODO result mismatch (ULP)
        // OpenCL 1.2 specification: "Results are undefined if edge0 >= edge1 or if x, edge0 or edge1 is a NaN."
        auto minValues = transform<float>(
            smallRandomData0, smallRandomData1, [](float a, float b) -> float { return std::min(a, b); });
        auto maxValues = transform<float>(
            smallRandomData0, smallRandomData1, [](float a, float b) -> float { return std::max(a, b); });
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "smoothstep", TERNARY_FUNCTION, "test", "-DFUNC=smoothstep");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC | DataFilter::DISABLED);
        builder.calculateDimensions(smallRandomData0.size());
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(minValues));
        builder.setParameter<2>(std::vector<float>(maxValues));
        builder.setParameter<3>(std::vector<float>(smallRandomData2));
        builder.checkParameter<0, CompareAbsoluteError>(
            transform<float>(minValues, maxValues, values, smoothstep), CompareAbsoluteError{1e-5f});
    }

    {
        // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
        TestDataBuilder<Buffer<float>, Buffer<float>> builder("sign", UNARY_FUNCTION, "test", "-DFUNC=sign");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES);
        builder.calculateDimensions(values.size());
        builder.allocateParameter<0>(values.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(values));
        builder.checkParameterEquals<0>(transform<float>(values,
            [](float a) -> float { return (std::signbit(a) ? -1.0f : 1.0f) * (std::abs(a) > 0 ? 1.0f : 0.0f); }));
    }
}

void test_data::registerOpenCLGeometricFunctionTests()
{
    // TODO add corner case/large value tests
    auto smallRandomData0 = toRandom<float>(64, true, -128.0f, 128.0f);
    auto smallRandomData1 = toRandom<float>(64, true, -128.0f, 128.0f);
    auto vector3Size = smallRandomData0.size() / 3u * 3u;
    std::vector<float> smallRandomData0AlignedTo3(
        smallRandomData0.begin(), smallRandomData0.begin() + static_cast<std::ptrdiff_t>(vector3Size));

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder("cross3", BINARY_GROUPED_FUNCTION, "test",
            "-DOUT=float3 -DIN0=float3 -DIN1=float3 -DFUNC=cross -DTRIPLE=3 -DTYPE=float");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(vector3Size, 3);
        builder.allocateParameter<0>(vector3Size, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareDynamicError>(
            transform<float, 3>(smallRandomData0AlignedTo3, smallRandomData1, cross3),
            CompareDynamicError{calculateCrossAllowedErrors<3>(smallRandomData0AlignedTo3, smallRandomData1)});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "cross4", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float4 -DIN0=float4 -DIN1=float4 -DFUNC=cross");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 4);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareDynamicError>(transform<float, 4>(smallRandomData0, smallRandomData1, cross4),
            CompareDynamicError{calculateCrossAllowedErrors<4>(smallRandomData0, smallRandomData1)});
    }

    {
        // for allowed error, see latest OpenCL C specification: max * max * (2 * |vector| - 1) * FLT_EPSILON
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "dot1", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN0=float -DIN1=float -DFUNC=dot");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 1);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareDynamicError>(reduce<float, 1>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<1>(smallRandomData0, smallRandomData1)});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "dot2", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=dot");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 2);
        builder.allocateParameter<0>(smallRandomData0.size() / 2, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareDynamicError>(reduce<float, 2>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<2>(smallRandomData0, smallRandomData1)});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder("dot3", BINARY_GROUPED_FUNCTION, "test",
            "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=dot -DTRIPLE=1 -DTYPE=float");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(vector3Size, 3);
        builder.allocateParameter<0>(smallRandomData0.size() / 3, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareDynamicError>(reduce<float, 3>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<3>(smallRandomData0, smallRandomData1)});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "dot4", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=dot");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 4);
        builder.allocateParameter<0>(smallRandomData0.size() / 4, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareDynamicError>(reduce<float, 4>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<4>(smallRandomData0, smallRandomData1)});
    }

    {
        // for ULP, see latest OpenCL specification: 4 ("sqrt") + (1.5 * |vector|) + (0.5 * (|vector| - 1))
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "distance1", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN0=float -DIN1=float -DFUNC=distance");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 1);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareULP<5>>(reduce<float, 1>(smallRandomData0, smallRandomData1, distance));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "distance2", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=distance");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 2);
        builder.allocateParameter<0>(smallRandomData0.size() / 2, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareULP<7>>(reduce<float, 2>(smallRandomData0, smallRandomData1, distance));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder("distance3", BINARY_GROUPED_FUNCTION,
            "test", "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=distance -DTRIPLE=1 -DTYPE=float");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(vector3Size, 3);
        builder.allocateParameter<0>(smallRandomData0.size() / 3, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareULP<9>>(reduce<float, 3>(smallRandomData0, smallRandomData1, distance));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "distance4", BINARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=distance");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 4);
        builder.allocateParameter<0>(smallRandomData0.size() / 4, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.setParameter<2>(std::vector<float>(smallRandomData1));
        builder.checkParameter<0, CompareULP<11>>(reduce<float, 4>(smallRandomData0, smallRandomData1, distance));
    }

    {
        // for ULP, see latest OpenCL specification: 4 ("sqrt") + 0.5 * ((0.5 * |vector|) + (0.5 * (|vector| - 1)))
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "length1", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN=float -DFUNC=length");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 1);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<4>>(reduce<float, 1>(smallRandomData0, length));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "length2", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN=float2 -DFUNC=length");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 2);
        builder.allocateParameter<0>(smallRandomData0.size() / 2, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<5>>(reduce<float, 2>(smallRandomData0, length));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "length3", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN=float3 -DFUNC=length -DTRIPLE=1 -DTYPE=float");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(vector3Size, 3);
        builder.allocateParameter<0>(smallRandomData0.size() / 3, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<5>>(reduce<float, 3>(smallRandomData0, length));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "length4", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN=float4 -DFUNC=length");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 4);
        builder.allocateParameter<0>(smallRandomData0.size() / 4, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<6>>(reduce<float, 4>(smallRandomData0, length));
    }

    {
        // ULP: sqrt + fdiv
        // OpenCL specification, full profile: "2 + n ulp, for gentype with vector width n", embedded profile specifies
        // "implementation defined"
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "normalize1", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float -DIN=float -DFUNC=normalize");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 1);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<4>>(transform<float, 1>(smallRandomData0, normalize<1>));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "normalize2", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float2 -DIN=float2 -DFUNC=normalize");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 2);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<5>>(transform<float, 2>(smallRandomData0, normalize<2>));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder("normalize3", UNARY_GROUPED_FUNCTION, "test",
            "-DOUT=float3 -DIN=float3 -DFUNC=normalize -DTRIPLE=3 -DTYPE=float");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(vector3Size, 3);
        builder.allocateParameter<0>(vector3Size, 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<5>>(transform<float, 3>(smallRandomData0AlignedTo3, normalize<3>));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "normalize4", UNARY_GROUPED_FUNCTION, "test", "-DOUT=float4 -DIN=float4 -DFUNC=normalize");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.calculateDimensions(smallRandomData0.size(), 4);
        builder.allocateParameter<0>(smallRandomData0.size(), 42.0f);
        builder.setParameter<1>(std::vector<float>(smallRandomData0));
        builder.checkParameter<0, CompareULP<6>>(transform<float, 4>(smallRandomData0, normalize<4>));
    }
}
