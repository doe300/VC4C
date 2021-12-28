/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include <limits>
#include <random>
#include <set>
#include <unordered_map>
#include <vector>

static const std::string VECTOR_LOAD_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2(__global CAT(TYPE,2)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload2(gid, in);
}

__kernel void test3(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  vstore3(vload3(gid, in), gid, out);
}

__kernel void test4(__global CAT(TYPE,4)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload4(gid, in);
}

__kernel void test8(__global CAT(TYPE,8)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload8(gid, in);
}

__kernel void test16(__global CAT(TYPE,16)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload16(gid, in);
}

__kernel void test3_uneven(__global CAT(TYPE,3)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload3(gid, in);
}
)";

static const std::string VECTOR_LOAD_HALF_FUNCTION = R"(
__kernel void test(__global float* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload_half(gid, in);
}

__kernel void test_aligned(__global float* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vloada_half(gid, in);
}

__kernel void test2(__global float2* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload_half2(gid, in);
}

__kernel void test_aligned2(__global float2* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vloada_half2(gid, in);
}

__kernel void test3(__global float* out, const __global half* in) {
  size_t gid = get_global_id(0);
  vstore3(vload_half3(gid, in), gid, out);
}

__kernel void test_aligned3(__global float4* out, const __global half* in) {
  size_t gid = get_global_id(0);
  // "vloada_half3 reads a half3 from address (p + (offset * 4)) and returns a float3.
  //  The address computed as (p + (offset * 4)) must be aligned to sizeof (half) * 4 bytes."
  // => read 4th value and treat output as float4
  float4 tmp;
  tmp.xyz = vloada_half3(gid, in);
  tmp.w = vloada_half(4 * gid + 3, in);
  out[gid] = tmp;
}

__kernel void test4(__global float4* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload_half4(gid, in);
}

__kernel void test_aligned4(__global float4* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vloada_half4(gid, in);
}

__kernel void test8(__global float8* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload_half8(gid, in);
}

__kernel void test_aligned8(__global float8* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vloada_half8(gid, in);
}

__kernel void test16(__global float16* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload_half16(gid, in);
}

__kernel void test_aligned16(__global float16* out, const __global half* in) {
  size_t gid = get_global_id(0);
  out[gid] = vloada_half16(gid, in);
}

// Adapted from	OpenCL-CTS vload_half tests
__kernel void test_vload_private(const __global half* in, __global float* out, __global float2* out2, __global float* out3, __global float4* out4, __global float8* out8, __global float16* out16)
{
    // should be lowered to register
    __private int data[16];
    __private half* hdata_in = (__private half*) data;
    __global int* i_in = (__global int*) in;
    size_t i = get_global_id(0);
    for(int k = 0; k < 8; k++)
        data[k] = i_in[i + k];

    // test out	the different vload versions
    for(int k = 0; k < 16; ++k)
        out[k] = vload_half(k, hdata_in);
    for(int k = 0; k < 8; ++k)
        out2[k] = vload_half2(k, hdata_in);
    for(int k = 0; k < 5; ++k)
        vstore3(vload_half3(k, hdata_in), k, out3);
    for(int k = 0; k < 4; ++k)
        out4[k] = vload_half4(k, hdata_in);
    for(int k = 0; k < 2; ++k)
        out8[k] = vload_half8(k, hdata_in);
    *out16 = vload_half16(0, hdata_in);
}

// Adapted from	OpenCL-CTS vload_half tests
__kernel void test_vloada_private(const __global half* in, __global float* out, __global float2* out2, __global float* out3, __global float4* out4, __global float8* out8, __global float16* out16)
{
    // should be lowered to register
    __private int data[16];
    __private half* hdata_in = (__private half*) data;
    __global int* i_in = (__global int*) in;
    size_t i = get_global_id(0);
    for(int k = 0; k < 8; k++)
        data[k] = i_in[i + k];

    // test out	the different vloada versions
    for(int k = 0; k < 16; ++k)
        out[k] = vloada_half(k, hdata_in);
    for(int k = 0; k < 8; ++k)
        out2[k] = vloada_half2(k, hdata_in);
    for(int k = 0; k < 4; ++k)
        vstore3(vloada_half3(k, hdata_in), k, out3);
    for(int k = 0; k < 4; ++k)
        out4[k] = vloada_half4(k, hdata_in);
    for(int k = 0; k < 2; ++k)
        out8[k] = vloada_half8(k, hdata_in);
    *out16 = vload_half16(0, hdata_in);
}
)";

static const std::string VECTOR_STORE_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2(__global TYPE* out, const __global CAT(TYPE,2)* in) {
  size_t gid = get_global_id(0);
  vstore2(in[gid], gid, out);
}

__kernel void test3(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  vstore3(vload3(gid, in), gid, out);
}

__kernel void test4(__global TYPE* out, const __global CAT(TYPE,4)* in) {
  size_t gid = get_global_id(0);
  vstore4(in[gid], gid, out);
}

__kernel void test8(__global TYPE* out, const __global CAT(TYPE,8)* in) {
  size_t gid = get_global_id(0);
  vstore8(in[gid], gid, out);
}

__kernel void test16(__global TYPE* out, const __global CAT(TYPE,16)* in) {
  size_t gid = get_global_id(0);
  vstore16(in[gid], gid, out);
}

__kernel void test3_uneven(__global TYPE* out, const __global CAT(TYPE,3)* in) {
  size_t gid = get_global_id(0);
  vstore3(in[gid], gid, out);
}
)";

static const std::string VECTOR_STORE_HALF_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test(__global half* out, const __global float* in) {
  size_t gid = get_global_id(0);
  CAT(vstore_half,ROUNDING)(in[gid], gid, out);
}

__kernel void test_aligned(__global half* out, const __global float* in) {
  size_t gid = get_global_id(0);
  CAT(vstorea_half,ROUNDING)(in[gid], gid, out);
}

__kernel void test2(__global half* out, const __global float2* in) {
  size_t gid = get_global_id(0);
  CAT(vstore_half2,ROUNDING)(in[gid], gid, out);
}

__kernel void test_aligned2(__global half* out, const __global float2* in) {
  size_t gid = get_global_id(0);
  CAT(vstorea_half2,ROUNDING)(in[gid], gid, out);
}

__kernel void test3(__global half* out, const __global float* in) {
  size_t gid = get_global_id(0);
  CAT(vstore_half3,ROUNDING)(vload3(gid, in), gid, out);
}

__kernel void test_aligned3(__global half* out, const __global float* in) {
  size_t gid = get_global_id(0);
  CAT(vstorea_half3,ROUNDING)(vload3(gid, in), gid, out);
}

__kernel void test4(__global half* out, const __global float4* in) {
  size_t gid = get_global_id(0);
  CAT(vstore_half4,ROUNDING)(in[gid], gid, out);
}

__kernel void test_aligned4(__global half* out, const __global float4* in) {
  size_t gid = get_global_id(0);
  CAT(vstorea_half4,ROUNDING)(in[gid], gid, out);
}

__kernel void test8(__global half* out, const __global float8* in) {
  size_t gid = get_global_id(0);
  CAT(vstore_half8,ROUNDING)(in[gid], gid, out);
}

__kernel void test_aligned8(__global half* out, const __global float8* in) {
  size_t gid = get_global_id(0);
  CAT(vstorea_half8,ROUNDING)(in[gid], gid, out);
}

__kernel void test16(__global half* out, const __global float16* in) {
  size_t gid = get_global_id(0);
  CAT(vstore_half16,ROUNDING)(in[gid], gid, out);
}

__kernel void test_aligned16(__global half* out, const __global float16* in) {
  size_t gid = get_global_id(0);
  CAT(vstorea_half16,ROUNDING)(in[gid], gid, out);
}
)";

static const std::string VECTOR_SHUFFLE_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2(__global CAT(TYPE,2)* out, const __global CAT(TYPE,2)* in, const __global CAT(TYPE,2)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle(in[gid], mask[gid]);
}
__kernel void test4(__global CAT(TYPE,4)* out, const __global CAT(TYPE,4)* in, const __global CAT(TYPE,4)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle(in[gid], mask[gid]);
}
__kernel void test8(__global CAT(TYPE,8)* out, const __global CAT(TYPE,8)* in, const __global CAT(TYPE,8)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle(in[gid], mask[gid]);
}
__kernel void test16(__global CAT(TYPE,16)* out, const __global CAT(TYPE,16)* in, const __global CAT(TYPE,16)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle(in[gid], mask[gid]);
}
)";

static const std::string VECTOR_SHUFFLE2_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2(__global CAT(TYPE,2)* out, const __global CAT(TYPE,2)* in0, const __global CAT(TYPE,2)* in1, const __global CAT(TYPE,2)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle2(in0[gid], in1[gid], mask[gid]);
}
__kernel void test4(__global CAT(TYPE,4)* out, const __global CAT(TYPE,4)* in0, const __global CAT(TYPE,4)* in1, const __global CAT(TYPE,4)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle2(in0[gid], in1[gid],mask[gid]);
}
__kernel void test8(__global CAT(TYPE,8)* out, const __global CAT(TYPE,8)* in0, const __global CAT(TYPE,8)* in1, const __global CAT(TYPE,8)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle2(in0[gid], in1[gid],mask[gid]);
}
__kernel void test16(__global CAT(TYPE,16)* out, const __global CAT(TYPE,16)* in0, const __global CAT(TYPE,16)* in1, const __global CAT(TYPE,16)* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle2(in0[gid], in1[gid],mask[gid]);
}
)";

static const std::string VECTOR_REORDER_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2(__global CAT(TYPE, 2)* out, const __global CAT(TYPE, 2)* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 2) tmp = in[gid];
  out[gid] = (CAT(TYPE, 2))(tmp.s10);
}

__kernel void test4(__global CAT(TYPE, 4)* out, const __global CAT(TYPE, 4)* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 4) tmp = in[gid];
  out[gid] = (CAT(TYPE, 4))(tmp.s1032);
}

__kernel void test8(__global CAT(TYPE, 8)* out, const __global CAT(TYPE, 8)* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 8) tmp = in[gid];
  out[gid] = (CAT(TYPE, 8))(tmp.s10321342);
}

__kernel void test16(__global CAT(TYPE, 16)* out, const __global CAT(TYPE, 16)* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 16) tmp = in[gid];
  out[gid] = (CAT(TYPE, 16))(tmp.s10321342a39baf5c);
}
)";

static const std::string VECTOR_ASSEMBLY_FUNCTION = R"(
__kernel void test(__global TYPE* out) {
  size_t gid = get_global_id(0);
  out[gid] = (TYPE)(SOURCES);
}
)";

static const std::string VECTOR_SHUFFLE_ASSIGN_1_TO_X_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test1to2(__global CAT(TYPE, 2)* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 2) tmp = (CAT(TYPE, 2)) PLACEHOLDER;
  tmp.MASK2 = in[gid];
  out[gid] = tmp;
}

__kernel void test1to3(__global TYPE* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 3) tmp = (CAT(TYPE, 3)) PLACEHOLDER;
  tmp.MASK3 = in[gid];
  vstore3(tmp, gid, out);
}

__kernel void test1to4(__global CAT(TYPE, 4)* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 4) tmp = (CAT(TYPE, 4)) PLACEHOLDER;
  tmp.MASK4 = in[gid];
  out[gid] = tmp;
}

__kernel void test1to8(__global CAT(TYPE, 8)* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 8) tmp = (CAT(TYPE, 8)) PLACEHOLDER;
  tmp.MASK8 = in[gid];
  out[gid] = tmp;
}

__kernel void test1to16(__global CAT(TYPE, 16)* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  CAT(TYPE, 16) tmp = (CAT(TYPE, 16)) PLACEHOLDER;
  tmp.MASK16 = in[gid];
  out[gid] = tmp;
}
)";

static const std::string VECTOR_SHUFFLE_ASSIGN_X_TO_1_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2to1(__global TYPE* out, __global CAT(TYPE, 2)* in) {
  size_t gid = get_global_id(0);
  out[gid] = in[gid].MASK2;
}

__kernel void test3to1(__global TYPE* out, __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload3(gid, in).MASK3;
}

__kernel void test4to1(__global TYPE* out, __global CAT(TYPE, 4)* in) {
  size_t gid = get_global_id(0);
  out[gid] = in[gid].MASK4;
}

__kernel void test8to1(__global TYPE* out, __global CAT(TYPE, 8)* in) {
  size_t gid = get_global_id(0);
  out[gid] = in[gid].MASK8;
}

__kernel void test16to1(__global TYPE* out, __global CAT(TYPE, 16)* in) {
  size_t gid = get_global_id(0);
  out[gid] = in[gid].MASK16;
}
)";

static const std::string VECTOR_SHUFFLE_ASSIGN_X_TO_X_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test(
#if OUT == 3
  __global TYPE* out,
#else
  __global CAT(TYPE, OUT)* out,
#endif
#if IN == 3
  __global TYPE* in
#else
  __global CAT(TYPE, IN)* in
#endif
) {
  size_t gid = get_global_id(0);
  CAT(TYPE, OUT) tmp = (CAT(TYPE, OUT)) PLACEHOLDER;
#if IN == 3
  tmp.OUTMASK = vload3(gid, in).INMASK;
#else
  tmp.OUTMASK = in[gid].INMASK;
#endif
#if OUT == 3
  vstore3(tmp, gid, out);
#else
  out[gid] = tmp;
#endif
}
)";

static const std::unordered_map<std::string, std::array<uint32_t, 16>> assemblySources = {
    {"random", {1, 15, 2, 3, 14, 4, 15, 7, 2, 8, 14, 15, 11, 14, 14, 3}},
    {"elem_num", {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}},
    {"constant", {42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42}},
    {"per_quad", {0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}},
    {"ldui", {2, 3, 1, 3, 0, 2, 1, 2, 3, 0, 1, 1, 2, 1, 1, 3}},
    {"elem_num+offset", {40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55}},
    {"per_quad+offset", {7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10}},
    {"ldui+offset", {6, 7, 8, 7, 6, 8, 6, 7, 9, 7, 6, 7, 8, 6, 9, 7}},
    {"elem_num*factor", {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30}},
    {"per_quad*factor", {0, 0, 0, 0, 7, 7, 7, 7, 14, 14, 14, 14, 21, 21, 21, 21}},
    {"ldui*factor", {0, 7, 14, 21, 7, 14, 0, 21, 14, 7, 21, 0, 21, 21, 7, 7}},
    {"elem_num<<rotated", {11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}}};

template <typename T>
std::vector<T> shuffle(std::size_t vectorSize, const std::vector<T>& in, const std::vector<T>& mask)
{
    std::vector<T> result(in.size());

    for(std::size_t i = 0; i < result.size(); ++i)
    {
        auto baseOffset = (i / vectorSize) * vectorSize;
        result[i] = in[baseOffset + static_cast<std::size_t>(mask[i])];
    }

    return result;
}

template <typename T>
std::vector<T> shuffle2(std::size_t vectorSize, const std::vector<T>& in, const std::vector<T>& mask)
{
    std::vector<T> result(in.size());

    for(std::size_t i = 0; i < result.size(); ++i)
    {
        auto baseOffset = (i / vectorSize) * vectorSize;
        auto offset = static_cast<std::size_t>(mask[i]);
        // since we reuse the same vector for both inputs, we can just wrap around its size
        offset = offset % vectorSize;
        result[i] = in[baseOffset + offset];
    }

    return result;
}

template <typename T>
std::vector<T> reorder(std::size_t vectorSize, const std::vector<T>& in, const std::vector<std::size_t>& mask)
{
    std::vector<T> result(in.size());

    for(std::size_t i = 0; i < result.size(); ++i)
    {
        auto baseOffset = (i / vectorSize) * vectorSize;
        result[i] = in[baseOffset + mask[i % vectorSize]];
    }

    return result;
}

static std::vector<uint8_t> generateMask(uint8_t numElements, uint8_t maxValue, bool allowDuplicates)
{
    static std::default_random_engine generator;
    static std::uniform_real_distribution<float> distribution;

    std::set<uint8_t> usedValues;
    std::vector<uint8_t> mask(std::min(numElements, maxValue), maxValue);
    for(auto& entry : mask)
    {
        do
        {
            entry = static_cast<uint8_t>(std::round(distribution(generator) * static_cast<float>(maxValue)));
        } while(!allowDuplicates && usedValues.find(entry) != usedValues.end());
        usedValues.emplace(entry);
    }
    return mask;
}

static constexpr std::array<char, 16> MASK_NAMES = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

template <typename T>
static void registerTypeTests(const std::string& typeName)
{
    using namespace test_data;
    auto flags = DataFilter::MEMORY_ACCESS;
    if(sizeof(T) > sizeof(uint32_t))
        flags = flags | DataFilter::USES_LONG;

    std::vector<T> values{0, 1, 17, 42, std::numeric_limits<T>::min(), std::numeric_limits<T>::max(),
        static_cast<T>(-1), static_cast<T>(-17), static_cast<T>(-42)};
    // fill buffer up to 16 elements so we do not access unallocated memory for 16-element tests
    values.resize(16, 0x17);

    auto values3 = values;
    // TestDataBuilder#calculateDimensions rounds up, so we need more values to be divisible by 3
    values3.emplace_back(43);
    values3.emplace_back(71);

    for(auto num : {2, 3, 4, 8, 16})
    {
        auto applicableValues = num == 3 ? values3 : values;
        {
            TestDataBuilder<Buffer<T>, Buffer<T>> builder("vload" + std::to_string(num) + "_" + typeName,
                VECTOR_LOAD_FUNCTION, "test" + std::to_string(num), "-DTYPE=" + typeName);
            builder.setFlags(flags);
            builder.calculateDimensions(applicableValues.size(), num);
            builder.template allocateParameter<0>(applicableValues.size(), 0x42);
            builder.template setParameter<1>(std::vector<T>(applicableValues));
            builder.template checkParameterEquals<0>(std::vector<T>(applicableValues));
        }

        {
            TestDataBuilder<Buffer<T>, Buffer<T>> builder("vstore" + std::to_string(num) + "_" + typeName,
                VECTOR_STORE_FUNCTION, "test" + std::to_string(num), "-DTYPE=" + typeName);
            builder.setFlags(flags);
            builder.calculateDimensions(applicableValues.size(), num);
            builder.template allocateParameter<0>(applicableValues.size(), 0x42);
            builder.template setParameter<1>(Buffer<T>(applicableValues));
            builder.template checkParameterEquals<0>(std::vector<T>(applicableValues));
        }

        if(num != 3)
        {
            // shuffle mask can only be unsigned and also the code executed is identical
            // also there is no 3-element mask shuffle
            auto mask = toRandom<T>(values.size(), true, 0, static_cast<T>(num - 1));

            {
                TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>> builder(
                    "shuffle_vector" + std::to_string(num) + "_" + typeName, VECTOR_SHUFFLE_FUNCTION,
                    "test" + std::to_string(num), "-DTYPE=" + typeName);
                builder.setFlags(flags | DataFilter::VECTOR_OPERATIONS);
                builder.calculateDimensions(values.size(), num);
                builder.template allocateParameter<0>(values.size(), 0x42);
                builder.template setParameter<1>(std::vector<T>(values));
                builder.template setParameter<2>(std::vector<T>(mask));
                builder.template checkParameterEquals<0>(shuffle(num, values, mask));
            }

            mask = toRandom<T>(values.size(), true, 0, static_cast<T>(num * 2 - 1));

            {
                TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>> builder(
                    "shuffle2_vector" + std::to_string(num) + "_" + typeName, VECTOR_SHUFFLE2_FUNCTION,
                    "test" + std::to_string(num), "-DTYPE=" + typeName);
                builder.setFlags(flags | DataFilter::VECTOR_OPERATIONS);
                builder.calculateDimensions(values.size(), num);
                builder.template allocateParameter<0>(values.size(), 0x42);
                builder.template setParameter<1>(std::vector<T>(values));
                builder.template setParameter<2>(std::vector<T>(values));
                builder.template setParameter<3>(std::vector<T>(mask));
                builder.template checkParameterEquals<0>(shuffle2(num, values, mask));
            }

            {
                TestDataBuilder<Buffer<T>, Buffer<T>> builder("reorder_vector" + std::to_string(num) + "_" + typeName,
                    VECTOR_REORDER_FUNCTION, "test" + std::to_string(num), "-DTYPE=" + typeName);
                builder.setFlags(flags | DataFilter::VECTOR_OPERATIONS);
                builder.calculateDimensions(values.size(), num);
                builder.template allocateParameter<0>(values.size(), 0x42);
                builder.template setParameter<1>(std::vector<T>(values));
                builder.template checkParameterEquals<0>(
                    reorder(num, values, {1, 0, 3, 2, 1, 3, 4, 2, 10, 3, 9, 11, 10, 15, 5, 12}));
            }
        }
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "vload3_uneven_" + typeName, VECTOR_LOAD_FUNCTION, "test3_uneven", "-DTYPE=" + typeName);
        builder.setFlags(flags);
        builder.calculateDimensions(values.size(), 3);
        builder.template allocateParameter<0>(values.size() * 4 / 3, 0x42);
        builder.template setParameter<1>(std::vector<T>(values));
        builder.template checkParameter<0>(values.size() * 4 / 3, [values](const std::vector<T>& result) -> Result {
            for(std::size_t i = 0; i < values.size() / 3; i += 3)
            {
                auto k = (i / 3) * 4 + (i % 3);
                if(result[k] != values[i] || result[k + 1] != values[i + 1] || result[k + 2] != values[i + 2])
                    return Result{false,
                        "Value error: Got {" + std::to_string(result[k]) + ", " + std::to_string(result[k + 1]) + ", " +
                            std::to_string(result[k + 2]) + "}, expected {" + std::to_string(values[i]) + ", " +
                            std::to_string(values[i + 1]) + ", " + std::to_string(values[i + 2]) + "}, for indices {" +
                            std::to_string(i) + ", " + std::to_string(i + 1) + ", " + std::to_string(i + 2) + "}"};
            }
            return RESULT_OK;
        });
    }

    {
        TestDataBuilder<Buffer<T>, Buffer<T>> builder(
            "vstore3_uneven_" + typeName, VECTOR_STORE_FUNCTION, "test3_uneven", "-DTYPE=" + typeName);
        builder.setFlags(flags);
        builder.calculateDimensions(values.size(), 3);
        builder.template allocateParameter<0>(values.size() * 4 / 3, 0x42);
        builder.template setParameter<1>(std::vector<T>(values));
        builder.template checkParameter<0>(values.size() * 4 / 3, [values](const std::vector<T>& result) -> Result {
            for(std::size_t i = 0; i < values.size() / 4; i += 4)
            {
                auto k = (i / 4) * 3 + (i % 4);
                if(result[k] != values[i] || result[k + 1] != values[i + 1] || result[k + 2] != values[i + 2])
                    return Result{false,
                        "Value error: Got {" + std::to_string(result[k]) + ", " + std::to_string(result[k + 1]) + ", " +
                            std::to_string(result[k + 2]) + "}, expected {" + std::to_string(values[i]) + ", " +
                            std::to_string(values[i + 1]) + ", " + std::to_string(values[i + 2]) + "}, for indices {" +
                            std::to_string(i) + ", " + std::to_string(i + 1) + ", " + std::to_string(i + 2) + "}"};
            }
            return RESULT_OK;
        });
    }

    constexpr T placeholder = 17;
    constexpr T baseOffset = std::min(std::numeric_limits<T>::max() - 12 * 16, (std::numeric_limits<T>::max() / 2 - 7));
    values.resize(12 * 16);
    std::iota(values.begin(), values.end(), baseOffset);

    std::unordered_map<uint8_t, uint8_t> scalarMasks;
    std::string scalarDefines;
    for(auto num : std::vector<uint8_t>{2u, 3u, 4u, 8u, 16u})
    {
        auto it = scalarMasks.emplace(num, generateMask(1, num - 1, true).front()).first;
        scalarDefines += " -DMASK" + std::to_string(static_cast<unsigned>(num)) + "=s" + MASK_NAMES.at(it->second);
    }

    for(auto num : std::vector<uint8_t>{2u, 3u, 4u, 8u, 16u})
    {
        TestDataBuilder<Buffer<T>, Buffer<T>> scalarToVectorBuilder(
            "shuffle_assign_1_to_" + std::to_string(num) + "_" + typeName, VECTOR_SHUFFLE_ASSIGN_1_TO_X_FUNCTION,
            "test1to" + std::to_string(num),
            "-DTYPE=" + typeName + scalarDefines +
                " -DPLACEHOLDER=" + std::to_string(static_cast<uint64_t>(placeholder)));
        scalarToVectorBuilder.setFlags(flags | DataFilter::VECTOR_OPERATIONS);
        scalarToVectorBuilder.setDimensions(12);
        scalarToVectorBuilder.template allocateParameter<0>(num * 12);
        scalarToVectorBuilder.template setParameter<1>(std::vector<T>(values));
        scalarToVectorBuilder.template checkParameter<0>(
            num * 12, [scalarMasks, num](const std::vector<T>& result) -> Result {
                auto mask = scalarMasks.at(num);
                for(std::size_t i = 0; i < result.size(); ++i)
                {
                    auto vectorIndex = static_cast<T>(i / num);
                    auto vectorElement = i % num;
                    if(vectorElement == mask && result[i] != baseOffset + vectorIndex)
                        return Result{false,
                            "Value error: Got " + std::to_string(static_cast<uint64_t>(result[i])) + ", expected " +
                                std::to_string(static_cast<uint64_t>(baseOffset + vectorIndex)) + ", for index " +
                                std::to_string(i) + " (element " + std::to_string(vectorElement) + " of vector " +
                                std::to_string(static_cast<uint64_t>(vectorIndex)) + " with element mask " +
                                std::to_string(static_cast<unsigned>(mask)) + ")"};
                    if(vectorElement != mask && result[i] != placeholder)
                        return Result{false,
                            "Value error: Got " + std::to_string(static_cast<uint64_t>(result[i])) + ", expected " +
                                std::to_string(static_cast<uint64_t>(placeholder)) + ", for index " +
                                std::to_string(i) + " (element " + std::to_string(vectorElement) + " of vector " +
                                std::to_string(static_cast<uint64_t>(vectorIndex)) + " with element mask " +
                                std::to_string(static_cast<unsigned>(mask)) + ")"};
                }
                return RESULT_OK;
            });

        TestDataBuilder<Buffer<T>, Buffer<T>> vectorToScalarBuilder(
            "shuffle_assign_" + std::to_string(num) + "_to_1_" + typeName, VECTOR_SHUFFLE_ASSIGN_X_TO_1_FUNCTION,
            "test" + std::to_string(num) + "to1", "-DTYPE=" + typeName + scalarDefines);
        vectorToScalarBuilder.setFlags(flags | DataFilter::VECTOR_OPERATIONS);
        vectorToScalarBuilder.setDimensions(12);
        vectorToScalarBuilder.template allocateParameter<0>(12);
        vectorToScalarBuilder.template setParameter<1>(std::vector<T>(values));
        vectorToScalarBuilder.template checkParameter<0>(
            12, [scalarMasks, num](const std::vector<T>& result) -> Result {
                auto mask = scalarMasks.at(num);
                for(std::size_t i = 0; i < result.size(); ++i)
                {
                    if(result[i] != baseOffset + num * i + mask)
                        return Result{false,
                            "Value error: Got " + std::to_string(static_cast<uint64_t>(result[i])) + ", expected " +
                                std::to_string(static_cast<uint64_t>(baseOffset + num * i + mask)) + ", for index " +
                                std::to_string(i) + " (with element mask " +
                                std::to_string(static_cast<unsigned>(mask)) + ")"};
                }
                return RESULT_OK;
            });
    }

    for(auto inNum : std::vector<uint8_t>{2u, 3u, 4u, 8u, 16u})
    {
        for(auto outNum : std::vector<uint8_t>{2u, 3u, 4u, 8u, 16u})
        {
            auto inMask = generateMask(std::min(inNum, outNum) / 2, inNum - 1, true);
            auto outMask = generateMask(std::min(inNum, outNum) / 2, outNum - 1, false);
            std::string defines = " -DIN=" + std::to_string(static_cast<unsigned>(inNum)) +
                " -DOUT=" + std::to_string(static_cast<unsigned>(outNum));
            defines += " -DINMASK=s";
            for(auto val : inMask)
                defines += MASK_NAMES.at(val);
            defines += " -DOUTMASK=s";
            for(auto val : outMask)
                defines += MASK_NAMES.at(val);
            std::vector<uint8_t> mask(outNum, 0xFFu);
            for(std::size_t i = 0; i < outMask.size(); ++i)
                mask.at(outMask.at(i)) = inMask.at(i);

            TestDataBuilder<Buffer<T>, Buffer<T>> builder(
                "shuffle_assign_" + std::to_string(inNum) + "_to_" + std::to_string(outNum) + "_" + typeName,
                VECTOR_SHUFFLE_ASSIGN_X_TO_X_FUNCTION, "test",
                "-DTYPE=" + typeName + defines +
                    " -DPLACEHOLDER=" + std::to_string(static_cast<uint64_t>(placeholder)));
            builder.setFlags(flags | DataFilter::VECTOR_OPERATIONS);
            builder.setDimensions(12);
            builder.template allocateParameter<0>(outNum * 12);
            builder.template setParameter<1>(std::vector<T>(values));
            builder.template checkParameter<0>(
                outNum * 12, [mask{std::move(mask)}, inNum, outNum, defines](const std::vector<T>& result) -> Result {
                    for(std::size_t i = 0; i < result.size(); ++i)
                    {
                        auto vectorIndex = static_cast<T>(i / outNum);
                        auto vectorElement = i % outNum;
                        if(mask.at(vectorElement) != 0xFFu &&
                            result[i] != (baseOffset + inNum * vectorIndex + mask.at(vectorElement)))
                            return Result{false,
                                "Value error: Got " + std::to_string(static_cast<uint64_t>(result[i])) + ", expected " +
                                    std::to_string(static_cast<uint64_t>(
                                        baseOffset + inNum * vectorIndex + mask.at(vectorElement))) +
                                    ", for index " + std::to_string(i) + " (element " + std::to_string(vectorElement) +
                                    " of vector " + std::to_string(static_cast<uint64_t>(vectorIndex)) +
                                    " with element masks " + defines + ")"};
                        if(mask.at(vectorElement) == 0xFFu && result[i] != placeholder)
                            return Result{false,
                                "Value error: Got " + std::to_string(static_cast<uint64_t>(result[i])) + ", expected " +
                                    std::to_string(static_cast<uint64_t>(placeholder)) + ", for index " +
                                    std::to_string(i) + " (element " + std::to_string(vectorElement) + " of vector " +
                                    std::to_string(static_cast<uint64_t>(vectorIndex)) + " with element masks " +
                                    defines + ")"};
                    }
                    return RESULT_OK;
                });
        }
    }
}

template <typename Mode>
static constexpr uint16_t roundFloatToHalf(uint32_t floatBits)
{
    constexpr auto EXPONENT = 0x7F800000u;
    constexpr auto MANTISSA = 0x007FFFFFu;
    bool isNegative = floatBits >> 31u;
    if((floatBits & 0x7FFFFFFFu) == 0)
        // +/- 0.0
        return static_cast<uint16_t>(floatBits >> 16u);
    if((floatBits & EXPONENT) == 0)
        // denormal value -> zero or smallest denormal value
        return static_cast<uint16_t>((isNegative ? 0x8000 : 0) | Mode::round(isNegative, 0, floatBits & MANTISSA, 23u));
    if((floatBits & EXPONENT) == EXPONENT)
    {
        if((floatBits & MANTISSA) == 0)
            return isNegative ? 0xFC00u : 0x7C00u;
        // all NaNs are created equal
        return isNegative ? 0xFE00u : 0x7E00u;
    }
    auto exponent = static_cast<int32_t>((floatBits & EXPONENT) >> 23u) - 127;
    auto mantissa = floatBits & MANTISSA;
    if(exponent > 15)
        return isNegative ? Mode::UNDERFLOW_VALUE : Mode::OVERFLOW_VALUE;
    if(exponent > -14)
    {
        // "normal" value
        auto halfExponent = static_cast<uint32_t>(exponent + 15) << 10u;
        auto halfMantissa = static_cast<uint16_t>(mantissa >> 13u);
        halfMantissa = Mode::round(isNegative, halfMantissa, floatBits & 0x1FFFu, 13u);
        return static_cast<uint16_t>((isNegative ? 0x8000u : 0u) | halfExponent | halfMantissa);
    }
    // denormal value
    // e.g. exponent of -17 -> shift of 13 + 3
    auto exponentOffset = static_cast<uint32_t>(-exponent - 14);
    auto mantissaOffset = std::min(31u, 13u /* normal difference between float and half mantissa */ + exponentOffset);
    auto halfMantissa = mantissa >> mantissaOffset;
    // add implicit float high-bit to explicit half high-bit
    halfMantissa |= 1u << (10u - exponentOffset);
    halfMantissa = Mode::round(
        isNegative, static_cast<uint16_t>(halfMantissa), mantissa & ((1u << mantissaOffset) - 1u), mantissaOffset);
    return static_cast<uint16_t>((isNegative ? 0x8000u : 0u) | halfMantissa);
}

struct RoundToZero
{
    static constexpr uint16_t OVERFLOW_VALUE = 0x7BFFu;
    static constexpr uint16_t UNDERFLOW_VALUE = 0xFBFFu;
    static constexpr uint16_t round(
        bool isNegative, uint16_t halfMantissa, uint32_t mantissaRemainder, uint32_t remainderBits)
    {
        return halfMantissa;
    }
};

struct RoundToEven
{
    static constexpr uint16_t OVERFLOW_VALUE = 0x7C00u;
    static constexpr uint16_t UNDERFLOW_VALUE = 0xFC00u;
    static constexpr uint16_t round(
        bool isNegative, uint16_t halfMantissa, uint32_t mantissaRemainder, uint32_t remainderBits)
    {
        if((mantissaRemainder & (1u << (remainderBits - 1u))) != 0)
        {
            bool isOdd = halfMantissa & 1u;
            // if the two highest bits are set, the next upper value is closer too
            // TODO is this check correct/exact enough?? E.g. what about 0b101111... ??
            bool nextIsCloser = mantissaRemainder & (1u << (remainderBits - 2u));
            return (isOdd || nextIsCloser) ? (halfMantissa + 1u) : halfMantissa;
        }
        return halfMantissa;
    }
};

struct RoundToPositiveInfinity
{
    static constexpr uint16_t OVERFLOW_VALUE = 0x7C00u;
    static constexpr uint16_t UNDERFLOW_VALUE = 0xFBFFu;
    static constexpr uint16_t round(
        bool isNegative, uint16_t halfMantissa, uint32_t mantissaRemainder, uint32_t remainderBits)
    {
        return (!isNegative && mantissaRemainder != 0) ? halfMantissa + 1u : halfMantissa;
    }
};

struct RoundToNegativeInfinity
{
    static constexpr uint16_t OVERFLOW_VALUE = 0x7BFFu;
    static constexpr uint16_t UNDERFLOW_VALUE = 0xFC00u;
    static constexpr uint16_t round(
        bool isNegative, uint16_t halfMantissa, uint32_t mantissaRemainder, uint32_t remainderBits)
    {
        return (isNegative && mantissaRemainder != 0) ? halfMantissa + 1u : halfMantissa;
    }
};

static constexpr std::initializer_list<uint32_t> floatOutputValues = {
    0x00000000u, // 0.0
    0x80000000u, // -0.0
    0x3F800000u, // 1.0
    0xBF800000u, // -1.0
    0x7F800000u, // Inf
    0xFF800000u, // -Inf
    0x7FC00000u, // NaN
    0xFFC00000u, // -NaN
    0x477FE000u, // HALF_MAX
    0xC77FE000u, // HALF_MIN
    0x38800000u, // min normal half
    0xB8800000u, // -min normal half
    0x33800000u, // min denormal half
    0xB3800000u, // -min denormal half
    0x387FC000u, // max denormal half
    0xB87FC000u, // -max denormal half
};

static constexpr std::initializer_list<uint16_t> halfInputValues = {
    0x0000, // 0.0
    0x8000, // -0.0
    0x3C00, // 1.0
    0xBC00, // -1.0
    0x7C00, // Inf
    0xFC00, // -Inf
    0x7E00, // NaN
    0xFE00, // -NaN
    0x7BFF, // HALF_MAX
    0xFBFF, // HALF_MIN
    0x0400, // min normal half
    0x8400, // -min normal half
    0x0001, // min denormal half
    0x8001, // -min denormal half
    0x03FF, // max denormal half
    0x83FF, // -max denormal half
};

template <uint16_t (*func)(uint32_t)>
static constexpr bool checkLosslessConversion()
{
    static_assert(func(floatOutputValues.begin()[0]) == halfInputValues.begin()[0], "");
    static_assert(func(floatOutputValues.begin()[1]) == halfInputValues.begin()[1], "");
    static_assert(func(floatOutputValues.begin()[2]) == halfInputValues.begin()[2], "");
    static_assert(func(floatOutputValues.begin()[3]) == halfInputValues.begin()[3], "");
    static_assert(func(floatOutputValues.begin()[4]) == halfInputValues.begin()[4], "");
    static_assert(func(floatOutputValues.begin()[5]) == halfInputValues.begin()[5], "");
    static_assert(func(floatOutputValues.begin()[6]) == halfInputValues.begin()[6], "");
    static_assert(func(floatOutputValues.begin()[7]) == halfInputValues.begin()[7], "");
    static_assert(func(floatOutputValues.begin()[8]) == halfInputValues.begin()[8], "");
    static_assert(func(floatOutputValues.begin()[9]) == halfInputValues.begin()[9], "");
    static_assert(func(floatOutputValues.begin()[10]) == halfInputValues.begin()[10], "");
    static_assert(func(floatOutputValues.begin()[11]) == halfInputValues.begin()[11], "");
    static_assert(func(floatOutputValues.begin()[12]) == halfInputValues.begin()[12], "");
    static_assert(func(floatOutputValues.begin()[13]) == halfInputValues.begin()[13], "");
    static_assert(func(floatOutputValues.begin()[14]) == halfInputValues.begin()[14], "");
    static_assert(func(floatOutputValues.begin()[15]) == halfInputValues.begin()[15], "");

    return true;
}

static_assert(checkLosslessConversion<roundFloatToHalf<RoundToZero>>(), "");
static_assert(roundFloatToHalf<RoundToZero>(0x40061000) == 0x4030u, "");
static_assert(roundFloatToHalf<RoundToZero>(0xC0021000) == 0xC010u, "");
static_assert(roundFloatToHalf<RoundToZero>(0xC0060001) == 0xC030u, "");
static_assert(roundFloatToHalf<RoundToZero>(0x3E99999A) == 0x34CCu, "");
static_assert(roundFloatToHalf<RoundToZero>(0x387FFF80) == 0x03FFu, "");
static_assert(roundFloatToHalf<RoundToZero>(0xB87FFF80) == 0x83FFu, "");
static_assert(roundFloatToHalf<RoundToZero>(0x4D7FE000) == 0x7BFFu, "");
static_assert(roundFloatToHalf<RoundToZero>(0xCD7FE000) == 0xFBFFu, "");
static_assert(roundFloatToHalf<RoundToZero>(0x00400000) == 0x0000u, "");
static_assert(roundFloatToHalf<RoundToZero>(0x80400000) == 0x8000u, "");
static_assert(roundFloatToHalf<RoundToZero>(0x37AAAAAA) == 0x0155u, "");
static_assert(roundFloatToHalf<RoundToZero>(0xB7AAAAAA) == 0x8155u, "");

static_assert(checkLosslessConversion<roundFloatToHalf<RoundToEven>>(), "");
static_assert(roundFloatToHalf<RoundToEven>(0x40061000) == 0x4030u, "");
static_assert(roundFloatToHalf<RoundToEven>(0xC0021000) == 0xC010u, "");
static_assert(roundFloatToHalf<RoundToEven>(0xC0060001) == 0xC030u, "");
static_assert(roundFloatToHalf<RoundToEven>(0x3E99999A) == 0x34CDu, "");
static_assert(roundFloatToHalf<RoundToEven>(0x387FFF80) == 0x0400u, "");
static_assert(roundFloatToHalf<RoundToEven>(0xB87FFF80) == 0x8400u, "");
static_assert(roundFloatToHalf<RoundToEven>(0x4D7FE000) == 0x7C00u, "");
static_assert(roundFloatToHalf<RoundToEven>(0xCD7FE000) == 0xFC00u, "");
static_assert(roundFloatToHalf<RoundToEven>(0x00400000) == 0x0000u, "");
static_assert(roundFloatToHalf<RoundToEven>(0x80400000) == 0x8000u, "");
static_assert(roundFloatToHalf<RoundToEven>(0x37AAAAAA) == 0x0155u, "");
static_assert(roundFloatToHalf<RoundToEven>(0xB7AAAAAA) == 0x8155u, "");

static_assert(checkLosslessConversion<roundFloatToHalf<RoundToPositiveInfinity>>(), "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x40061000) == 0x4031u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0xC0021000) == 0xC010u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0xC0060001) == 0xC030u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x3E99999A) == 0x34CDu, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x387FFF80) == 0x0400u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0xB87FFF80) == 0x83FFu, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x4D7FE000) == 0x7C00u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0xCD7FE000) == 0xFBFFu, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x00400000) == 0x0001u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x80400000) == 0x8000u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0x37AAAAAA) == 0x0156u, "");
static_assert(roundFloatToHalf<RoundToPositiveInfinity>(0xB7AAAAAA) == 0x8155u, "");

static_assert(checkLosslessConversion<roundFloatToHalf<RoundToNegativeInfinity>>(), "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x40061000) == 0x4030u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0xC0021000) == 0xC011u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0xC0060001) == 0xC031u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x3E99999A) == 0x34CCu, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x387FFF80) == 0x03FFu, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0xB87FFF80) == 0x8400u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x4D7FE000) == 0x7BFFu, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0xCD7FE000) == 0xFC00u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x00400000) == 0x0000u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x80400000) == 0x8001u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0x37AAAAAA) == 0x0155u, "");
static_assert(roundFloatToHalf<RoundToNegativeInfinity>(0xB7AAAAAA) == 0x8156u, "");

static void registerHalfTests()
{
    using namespace test_data;

    // NOTE: Correct mapping including rounding can be calculated via <CL/cl_half> and cl_half_from_float(...) function
    const std::vector<uint32_t> additonalFloatValues = {
        0x40061000, // half 0x4030/0x4031 (some in-range values which require rounding)
        0xC0061000, // half 0xC030/0xC031
        0x40021000, // half 0x4010/0x4011
        0xC0021000, // half 0xC010/0xC011
        0x3E99999A, // half 0x34CC/0x34CD
        0xBE99999A, // half 0xB4CC/0xB4CD
        0x37AAAAAA, // half 0x0155/0x0156 (in denormal range)
        0xB7AAAAAA, // half 0x8155/0x8156
        0x387FFF80, // half 0x0400/0x03FF (border between normal and denormal)
        0xB87FFF80, // half 0x8400/0x83FF
        0x47FFE000, // half 0x7C00/0x7BFF (border between normal and Inf)
        0xC7FFE000, // half 0xFC00/0xFBFF
        0x4D7FE000, // half 0x7C00/0x7BFF (way outside of half range)
        0xCD7FE000, // half 0xFC00/0xFBFF
        0x00400000, // half 0x0000 (float denormal)
        0x80400000, // half 0x8000
    };
    std::vector<uint32_t> floatInputValues;
    floatInputValues.reserve(floatOutputValues.size() + additonalFloatValues.size());
    floatInputValues.insert(floatInputValues.end(), floatOutputValues.begin(), floatOutputValues.end());
    floatInputValues.insert(floatInputValues.end(), additonalFloatValues.begin(), additonalFloatValues.end());

    auto flags = DataFilter::TYPE_HANDLING | DataFilter::TYPE_CONVERSIONS;
    for(auto vectorSize : {1u, 2u, 3u, 4u, 8u, 16u})
    {
        auto n = vectorSize == 1u ? "" : std::to_string(vectorSize);
        auto numHalfElements = vectorSize == 3u ? (halfInputValues.size() / 3u) * 3u : halfInputValues.size();
        auto numFloatElements = vectorSize == 3u ? (floatInputValues.size() / 3u) * 3u : floatInputValues.size();

        {
            TestDataBuilder<Buffer<uint32_t>, Buffer<uint16_t>> builder(
                "vload_half" + n, VECTOR_LOAD_HALF_FUNCTION, "test" + n);
            builder.setFlags(flags);
            builder.calculateDimensions(numHalfElements, vectorSize);
            builder.allocateParameter<0>(numHalfElements, 0x42);
            builder.setParameter<1>(std::vector<uint16_t>(halfInputValues));
            builder.checkParameterEquals<0, HexPrinter<uint32_t>>(
                std::vector<uint32_t>(floatOutputValues.begin(), floatOutputValues.begin() + numHalfElements));
        }

        {
            TestDataBuilder<Buffer<uint32_t>, Buffer<uint16_t>> builder(
                "vloada_half" + n, VECTOR_LOAD_HALF_FUNCTION, "test_aligned" + n);
            builder.setFlags(flags);
            builder.calculateDimensions(halfInputValues.size(), vectorSize == 3 ? 4 : vectorSize);
            builder.allocateParameter<0>(halfInputValues.size(), 0x42);
            builder.setParameter<1>(std::vector<uint16_t>(halfInputValues));
            builder.checkParameterEquals<0, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        }

        for(auto roundingMode : std::vector<std::pair<std::string, uint16_t (*)(uint32_t)>>{
                {"" /* default rounding mode which is CL_FP_ROUND_TO_ZERO */, roundFloatToHalf<RoundToZero>},
                {"_rte", roundFloatToHalf<RoundToEven>},
                {"_rtp", roundFloatToHalf<RoundToPositiveInfinity>},
                {"_rtz", roundFloatToHalf<RoundToZero>},
                {"_rtn", roundFloatToHalf<RoundToNegativeInfinity>},
            })
        {
            TestDataBuilder<Buffer<uint16_t>, Buffer<uint32_t>> builder("vstore_half" + n + roundingMode.first,
                VECTOR_STORE_HALF_FUNCTION, "test" + n, "-DROUNDING=" + roundingMode.first);
            builder.setFlags(flags);
            builder.calculateDimensions(numFloatElements, vectorSize);
            builder.allocateParameter<0>(numFloatElements, 0x42);
            builder.setParameter<1>(std::vector<uint32_t>(floatInputValues));
            builder.checkParameterEquals<0, HexPrinter<uint16_t>>(transform<uint16_t>(
                std::vector<uint32_t>(floatInputValues.begin(), floatInputValues.begin() + numFloatElements),
                roundingMode.second));
        }
    }

    std::vector<uint32_t> vector3Result = floatOutputValues;
    // for vload_half3 we align to 3 elements, so we may not read all values
    while(vector3Result.size() % 3)
        vector3Result.pop_back();

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>,
            Buffer<uint32_t>, Buffer<uint32_t>>
            builder("vload_half_private", VECTOR_LOAD_HALF_FUNCTION, "test_vload_private");
        builder.setFlags(flags);
        builder.calculateDimensions(halfInputValues.size(), 16);
        builder.setParameter<0>(std::vector<uint16_t>(halfInputValues));
        builder.allocateParameter<1>(16, 0x42);
        builder.allocateParameter<2>(16, 0x42);
        builder.allocateParameter<3>(15, 0x42);
        builder.allocateParameter<4>(16, 0x42);
        builder.allocateParameter<5>(16, 0x42);
        builder.allocateParameter<6>(16, 0x42);
        builder.checkParameterEquals<1, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<2, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<3, HexPrinter<uint32_t>>(std::move(vector3Result));
        builder.checkParameterEquals<4, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<5, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<6, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
    }

    // vloada_half aligns to 4-elements, but only reads/writes 3 elements, so we do not convert every 4th value, but
    // write compacted
    vector3Result = floatOutputValues;
    for(std::size_t i = vector3Result.size(); i > 0; --i)
    {
        if(i % 4 == 3)
            vector3Result.erase(vector3Result.begin() + static_cast<ssize_t>(i));
    }

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>,
            Buffer<uint32_t>, Buffer<uint32_t>>
            builder("vloada_half_private", VECTOR_LOAD_HALF_FUNCTION, "test_vloada_private");
        builder.setFlags(flags);
        builder.calculateDimensions(halfInputValues.size(), 16);
        builder.setParameter<0>(std::move(halfInputValues));
        builder.allocateParameter<1>(16, 0x42);
        builder.allocateParameter<2>(16, 0x42);
        builder.allocateParameter<3>(12, 0x42);
        builder.allocateParameter<4>(16, 0x42);
        builder.allocateParameter<5>(16, 0x42);
        builder.allocateParameter<6>(16, 0x42);
        builder.checkParameterEquals<1, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<2, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<3, HexPrinter<uint32_t>>(std::move(vector3Result));
        builder.checkParameterEquals<4, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<5, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
        builder.checkParameterEquals<6, HexPrinter<uint32_t>>(std::vector<uint32_t>(floatOutputValues));
    }
}

template <typename T, std::size_t N>
std::string to_string(const std::array<T, N>& vec, unsigned numElements)
{
    std::string res;
    if(vec.empty())
        return res;
    for(unsigned i = 0; i < numElements; ++i)
    {
        // NO SPACE is on purpose!
        res.append(",").append(std::to_string(vec[i]));
    }
    return res.substr(1);
}

void test_data::registerVectorTests()
{
    // we only test unsigned types since
    // a) the behavior is sign-agnostic and identical for same-sized types
    // b) the shuffle functions cannot take signed mask vectors anyway
    registerTypeTests<uint64_t>("ulong");
    registerTypeTests<uint32_t>("uint");
    registerTypeTests<uint16_t>("ushort");
    registerTypeTests<uint8_t>("uchar");
    registerHalfTests();

    for(const auto& pair : assemblySources)
    {
        for(auto num : {2u, 4u, 8u, 16u})
        {
            TestDataBuilder<Buffer<uint32_t>> builder("vector_assembly_" + pair.first + std::to_string(num),
                VECTOR_ASSEMBLY_FUNCTION, "test",
                "-DTYPE=uint" + std::to_string(num) + " -DSOURCES=" + to_string(pair.second, num));
            builder.setFlags(DataFilter::VECTOR_OPERATIONS);
            builder.allocateParameter<0>(num, 0x42);
            builder.checkParameterEquals<0>(std::vector<uint32_t>(pair.second.begin(), pair.second.begin() + num));
        }
    }
}
