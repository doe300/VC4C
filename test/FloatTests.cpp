
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

static test_data::WorkDimensions toWorkDimensions(std::size_t numElements, std::size_t vectorWidth = 16)
{
    auto numWorkItems = numElements / vectorWidth;
    if((numElements % vectorWidth) != 0)
        ++numWorkItems;
    for(uint32_t groupSize = 12; groupSize > 0; --groupSize)
    {
        if(numWorkItems % groupSize == 0)
        {
            auto numGroups = static_cast<uint32_t>(numWorkItems / groupSize);
            return test_data::toDimensions(groupSize, 1, 1, numGroups, 1, 1);
        }
    }
    throw std::invalid_argument{"Work cannot be distributed across work-groups: " + std::to_string(numElements)};
}

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
            maxValue = std::max({maxValue, in0[i + k], in1[i + k]});
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

    auto smallRandomData0 = toRandom<float>(64, false, -128.0f, 128.0f);
    auto smallRandomData1 = toRandom<float>(64, false, -128.0f, 128.0f);
    auto smallRandomData2 = toRandom<float>(64, true, -128.0f, 128.0f);

    // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"clamp", DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES, &TERNARY_FUNCTION,
        "-DFUNC=clamp", "test",
        {toBufferParameter(std::vector<float>(values.size(), 42.0f)), toBufferParameter(std::vector<float>(values)),
            toBufferParameter(toRange<float>(-16.0f, 0.0f)), toBufferParameter(toRange<float>(0.0f, 16.0f))},
        toWorkDimensions(values.size()),
        {checkParameterEquals(0,
            std::vector<float>{0.0f, 0.0f, 1.0f, -1.0f, 0.5f, -0.5f, 6.0f, -9.0f, limits::min(), -limits::min(), 10.0f,
                -5.0f, 12.0f, -3.0f, -2.0f, -1.0f})}});

    // maximum error of 2 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"degrees", DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES, &UNARY_FUNCTION,
        "-DFUNC=degrees", "test",
        {toBufferParameter(std::vector<float>(values.size(), 42.0f)), toBufferParameter(std::vector<float>(values))},
        toWorkDimensions(values.size()),
        {checkParameter<CompareULP<2>>(0, transform<float>(values, [](float val) -> float {
            // TODO is this okay with the OpenCL C standard? At least this is what the GPU does
            return std::isnan(val) ? std::numeric_limits<float>::infinity() : val * 57.295780181884765625f;
        }))}});

    // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"maxf", DataFilter::FLOAT_ARITHMETIC, &BINARY_FUNCTION, "-DFUNC=max", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size()),
        {checkParameterEquals(0, transform<float>(smallRandomData0, smallRandomData1, [](float a, float b) -> float {
            return std::max(a, b);
        }))}});

    // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"minf", DataFilter::FLOAT_ARITHMETIC, &BINARY_FUNCTION, "-DFUNC=min", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size()),
        {checkParameterEquals(0, transform<float>(smallRandomData0, smallRandomData1, [](float a, float b) -> float {
            return std::min(a, b);
        }))}});

    // maximum error not in OpenCL 1.2 standard, but in latest
    // for "full profile" it states: absolute error tolerance of 1e-3, for "embedded profile", the allowed error is
    // implementation defined
    // the third parameter must be in the range [0.0, 1.0], otherwise the result is UB
    auto normalizedValues = transform<float>(smallRandomData2, [](float f) { return std::abs(f) / 128.0f; });
    registerTest(TestData{"mix", DataFilter::FLOAT_ARITHMETIC, &TERNARY_FUNCTION, "-DFUNC=mix", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1)),
            toBufferParameter(std::vector<float>(normalizedValues))},
        toWorkDimensions(smallRandomData0.size()),
        {checkParameter<CompareAbsoluteError>(0,
            transform<float>(smallRandomData0, smallRandomData1, normalizedValues,
                [](float a, float b, float c) -> float { return a + (b - a) * c; }),
            CompareAbsoluteError{1e-3f})}});

    // maximum error of 2 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"radians", DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES, &UNARY_FUNCTION,
        "-DFUNC=radians", "test",
        {toBufferParameter(std::vector<float>(values.size(), 42.0f)), toBufferParameter(std::vector<float>(values))},
        toWorkDimensions(values.size()),
        {checkParameter<CompareULP<2>>(0, transform<float>(values, [](float val) -> float {
            // TODO is this okay with the OpenCL C standard? At least this is what the GPU does
            return std::isnan(val) ? std::numeric_limits<float>::infinity() : val * (static_cast<float>(M_PI) / 180.f);
        }))}});

    // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"step", DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES, &BINARY_FUNCTION,
        "-DFUNC=step", "test",
        {toBufferParameter(std::vector<float>(productLeft.size(), 42.0f)),
            toBufferParameter(std::vector<float>(productLeft)), toBufferParameter(std::vector<float>(productRight))},
        toWorkDimensions(productLeft.size()),
        {checkParameterEquals(0, transform<float>(productLeft, productRight, [](float a, float b) -> float {
            return b < a ? 0.0f : 1.0f;
        }))}});

    // TODO result mismatch (ULP)
    // OpenCL 1.2 specification: "Results are undefined if edge0 >= edge1 or if x, edge0 or edge1 is a NaN."
    auto minValues =
        transform<float>(smallRandomData0, smallRandomData1, [](float a, float b) -> float { return std::min(a, b); });
    auto maxValues =
        transform<float>(smallRandomData0, smallRandomData1, [](float a, float b) -> float { return std::max(a, b); });
    registerTest(TestData{"smoothstep", DataFilter::FLOAT_ARITHMETIC, &TERNARY_FUNCTION, "-DFUNC=smoothstep", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(minValues)), toBufferParameter(std::vector<float>(maxValues)),
            toBufferParameter(std::vector<float>(smallRandomData2))},
        toWorkDimensions(smallRandomData0.size()),
        {checkParameter<CompareAbsoluteError>(
            0, transform<float>(minValues, maxValues, values, smoothstep), CompareAbsoluteError{1e-5f})}});

    // maximum error of 0 ULP not in OpenCL 1.2 standard, but in latest
    registerTest(TestData{"sign", DataFilter::FLOAT_ARITHMETIC | DataFilter::CORNER_CASES, &UNARY_FUNCTION,
        "-DFUNC=sign", "test",
        {toBufferParameter(std::vector<float>(values.size(), 42.0f)), toBufferParameter(std::vector<float>(values))},
        toWorkDimensions(values.size()), {checkParameterEquals(0, transform<float>(values, [](float a) -> float {
            return (std::signbit(a) ? -1.0f : 1.0f) * (std::abs(a) > 0 ? 1.0f : 0.0f);
        }))}});
}

void test_data::registerOpenCLGeometricFunctionTests()
{
    auto smallRandomData0 = toRandom<float>(64, true, -128.0f, 128.0f);
    auto smallRandomData1 = toRandom<float>(64, true, -128.0f, 128.0f);

    registerTest(TestData{"cross3", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float3 -DIN0=float3 -DIN1=float3 -DFUNC=cross -DTRIPLE=3 -DTYPE=float", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 3),
        {checkParameter<CompareDynamicError>(0, transform<float, 3>(smallRandomData0, smallRandomData1, cross3),
            CompareDynamicError{calculateCrossAllowedErrors<3>(smallRandomData0, smallRandomData1)})}});

    registerTest(TestData{"cross4", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float4 -DIN0=float4 -DIN1=float4 -DFUNC=cross", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 4),
        {checkParameter<CompareDynamicError>(0, transform<float, 4>(smallRandomData0, smallRandomData1, cross4),
            CompareDynamicError{calculateCrossAllowedErrors<4>(smallRandomData0, smallRandomData1)})}});

    // TODO result validation (ULP) errors on hardware
    // for allowed error, see latest OpenCL C specification: max * max * (2 * |vector| - 1) * FLT_EPSILON
    registerTest(TestData{"dot1", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float -DIN1=float -DFUNC=dot", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 1),
        {checkParameter<CompareDynamicError>(0, reduce<float, 1>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<1>(smallRandomData0, smallRandomData1)})}});

    // TODO result validation (ULP) errors on hardware
    registerTest(TestData{"dot2", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=dot", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 2, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 2),
        {checkParameter<CompareDynamicError>(0, reduce<float, 2>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<2>(smallRandomData0, smallRandomData1)})}});

    // TODO result validation (ULP) errors on hardware
    registerTest(TestData{"dot3", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=dot -DTRIPLE=1 -DTYPE=float", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 3, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 3),
        {checkParameter<CompareDynamicError>(0, reduce<float, 3>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<3>(smallRandomData0, smallRandomData1)})}});

    // TODO result validation (ULP) errors on hardware
    registerTest(TestData{"dot4", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=dot", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 4, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 4),
        {checkParameter<CompareDynamicError>(0, reduce<float, 4>(smallRandomData0, smallRandomData1, dot),
            CompareDynamicError{calculateDotAllowedErrors<4>(smallRandomData0, smallRandomData1)})}});

    // for ULP, see latest OpenCL specification: 4 ("sqrt") + (1.5 * |vector|) + (0.5 * (|vector| - 1))
    registerTest(TestData{"distance1", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float -DIN1=float -DFUNC=distance", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 1),
        {checkParameter<CompareULP<5>>(0, reduce<float, 1>(smallRandomData0, smallRandomData1, distance))}});

    registerTest(TestData{"distance2", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float2 -DIN1=float2 -DFUNC=distance", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 2, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 2),
        {checkParameter<CompareULP<7>>(0, reduce<float, 2>(smallRandomData0, smallRandomData1, distance))}});

    registerTest(TestData{"distance3", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float3 -DIN1=float3 -DFUNC=distance -DTRIPLE=1 -DTYPE=float", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 3, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 3),
        {checkParameter<CompareULP<9>>(0, reduce<float, 3>(smallRandomData0, smallRandomData1, distance))}});

    registerTest(TestData{"distance4", DataFilter::FLOAT_ARITHMETIC, &BINARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN0=float4 -DIN1=float4 -DFUNC=distance", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 4, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0)),
            toBufferParameter(std::vector<float>(smallRandomData1))},
        toWorkDimensions(smallRandomData0.size(), 4),
        {checkParameter<CompareULP<11>>(0, reduce<float, 4>(smallRandomData0, smallRandomData1, distance))}});

    // for ULP, see latest OpenCL specification: 4 ("sqrt") + 0.5 * ((0.5 * |vector|) + (0.5 * (|vector| - 1)))
    registerTest(TestData{"length1", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN=float -DFUNC=length", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 1),
        {checkParameter<CompareULP<4>>(0, reduce<float, 1>(smallRandomData0, length))}});

    registerTest(TestData{"length2", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN=float2 -DFUNC=length", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 2, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 2),
        {checkParameter<CompareULP<5>>(0, reduce<float, 2>(smallRandomData0, length))}});

    registerTest(TestData{"length3", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN=float3 -DFUNC=length -DTRIPLE=1 -DTYPE=float", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 3, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 3),
        {checkParameter<CompareULP<5>>(0, reduce<float, 3>(smallRandomData0, length))}});

    registerTest(TestData{"length4", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN=float4 -DFUNC=length", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size() / 4, 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 4),
        {checkParameter<CompareULP<6>>(0, reduce<float, 4>(smallRandomData0, length))}});

    // ULP: sqrt + fdiv
    registerTest(TestData{"normalize1", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float -DIN=float -DFUNC=normalize", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 1),
        {checkParameter<CompareULP<4>>(0, transform<float, 1>(smallRandomData0, normalize<1>))}});

    registerTest(TestData{"normalize2", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float2 -DIN=float2 -DFUNC=normalize", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 2),
        {checkParameter<CompareULP<5>>(0, transform<float, 2>(smallRandomData0, normalize<2>))}});

    registerTest(TestData{"normalize3", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float3 -DIN=float3 -DFUNC=normalize -DTRIPLE=3 -DTYPE=float", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 3),
        {checkParameter<CompareULP<5>>(0, transform<float, 3>(smallRandomData0, normalize<3>))}});

    registerTest(TestData{"normalize4", DataFilter::FLOAT_ARITHMETIC, &UNARY_GROUPED_FUNCTION,
        "-DOUT=float4 -DIN=float4 -DFUNC=normalize", "test",
        {toBufferParameter(std::vector<float>(smallRandomData0.size(), 42.0f)),
            toBufferParameter(std::vector<float>(smallRandomData0))},
        toWorkDimensions(smallRandomData0.size(), 4),
        {checkParameter<CompareULP<6>>(0, transform<float, 4>(smallRandomData0, normalize<4>))}});
}
