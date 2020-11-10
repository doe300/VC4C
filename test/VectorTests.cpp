/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include <limits>
#include <unordered_map>

static const std::string VECTOR_LOAD_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test2(__global CAT(TYPE,2)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = vload2(gid, in);
}

#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
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

static std::unordered_map<std::string, std::array<uint32_t, 16>> assemblySources = {
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

    for(auto num : {2, 3, 4, 8, 16})
    {
        registerTest(TestData{"vload" + std::to_string(num) + "_" + typeName, flags, &VECTOR_LOAD_FUNCTION,
            "-DTYPE=" + typeName, "test" + std::to_string(num),
            {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values))},
            calculateDimensions(values.size(), num), {checkParameterEquals(0, std::vector<T>(values))}});

        registerTest(TestData{"vstore" + std::to_string(num) + "_" + typeName, flags, &VECTOR_STORE_FUNCTION,
            "-DTYPE=" + typeName, "test" + std::to_string(num),
            {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values))},
            calculateDimensions(values.size(), num), {checkParameterEquals(0, std::vector<T>(values))}});

        if(num != 3)
        {
            // shuffle mask can only be unsigned and also the code executed is identical
            // also there is no 3-element mask shuffle
            auto mask = toRandom<T>(values.size(), true, 0, static_cast<T>(num - 1));

            registerTest(
                TestData{"shuffle_vector" + std::to_string(num) + "_" + typeName, flags | DataFilter::VECTOR_OPERATIONS,
                    &VECTOR_SHUFFLE_FUNCTION, "-DTYPE=" + typeName, "test" + std::to_string(num),
                    {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values)),
                        toBufferParameter(std::vector<T>(mask))},
                    calculateDimensions(values.size(), num), {checkParameterEquals(0, shuffle(num, values, mask))}});

            mask = toRandom<T>(values.size(), true, 0, static_cast<T>(num * 2 - 1));

            registerTest(TestData{"shuffle2_vector" + std::to_string(num) + "_" + typeName,
                flags | DataFilter::VECTOR_OPERATIONS, &VECTOR_SHUFFLE2_FUNCTION, "-DTYPE=" + typeName,
                "test" + std::to_string(num),
                {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values)),
                    toBufferParameter(std::vector<T>(values)), toBufferParameter(std::vector<T>(mask))},
                calculateDimensions(values.size(), num), {checkParameterEquals(0, shuffle2(num, values, mask))}});

            registerTest(
                TestData{"reorder_vector" + std::to_string(num) + "_" + typeName, flags | DataFilter::VECTOR_OPERATIONS,
                    &VECTOR_REORDER_FUNCTION, "-DTYPE=" + typeName, "test" + std::to_string(num),
                    {toBufferParameter(std::vector<T>(values.size(), 0x42)), toBufferParameter(std::vector<T>(values))},
                    calculateDimensions(values.size(), num),
                    {checkParameterEquals(
                        0, reorder(num, values, {1, 0, 3, 2, 1, 3, 4, 2, 10, 3, 9, 11, 10, 15, 5, 12}))}});
        }
    }

    registerTest(TestData{"vload3_uneven_" + typeName, flags, &VECTOR_LOAD_FUNCTION, "-DTYPE=" + typeName,
        "test3_uneven",
        {toBufferParameter(std::vector<T>(values.size() * 4 / 3, 0x42)), toBufferParameter(std::vector<T>(values))},
        calculateDimensions(values.size(), 3),
        {checkParameter<T>(0, values.size() * 4 / 3, [values](const std::vector<T>& result) -> Result {
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
        })}});

    registerTest(TestData{"vstore3_uneven_" + typeName, flags, &VECTOR_STORE_FUNCTION, "-DTYPE=" + typeName,
        "test3_uneven",
        {toBufferParameter(std::vector<T>(values.size() * 3 / 4, 0x42)), toBufferParameter(std::vector<T>(values))},
        calculateDimensions(values.size(), 4),
        {checkParameter<T>(0, values.size() * 3 / 4, [values](const std::vector<T>& result) -> Result {
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
        })}});
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

    for(const auto& pair : assemblySources)
    {
        for(auto num : {2u, 4u, 8u, 16u})
        {
            registerTest(TestData{"vector_assembly_" + pair.first + std::to_string(num), DataFilter::VECTOR_OPERATIONS,
                &VECTOR_ASSEMBLY_FUNCTION,
                "-DTYPE=uint" + std::to_string(num) + " -DSOURCES=" + to_string(pair.second, num), "test",
                {toBufferParameter(std::vector<uint32_t>(num, 0x42))}, toDimensions(1),
                {checkParameterEquals(0, std::vector<uint32_t>(pair.second.begin(), pair.second.begin() + num))}});
        }
    }
}
