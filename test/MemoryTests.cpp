/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include "test_files.h"

#include <algorithm>

using namespace test_files;

static const std::string COPY64BIT = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test_copy(__global Type* out, __global Type* in) {
  size_t gid = get_global_id(0);
  out[gid] = in[gid];
}

__kernel void test_load_store(__global Type* out, __global Type* in) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  // this prevents e.g. copy optimization
  in[gid] = (Type){.a = 0, .b = 0};
  out[gid] = val;
  in[gid] = val;
})";

static const std::string READ64BITSINGLEWORD = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test_lower(__global uint* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  ulong b = other[gid];
  uint2 c = (uint2)(b & 0xFFFFFFFF, b >> 32);
  out[gid] = val.a + c.x;
}

__kernel void test_upper(__global uint* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  ulong b = other[gid];
  uint2 c = (uint2)(b & 0xFFFFFFFF, b >> 32);
  out[gid] = val.b + c.y;
})";

static const std::string CASTPOINTER = R"(
__kernel void test(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);

  ulong inTmp = (ulong) in;
  long outTmp = (long) out;
  inTmp += gid * sizeof(TYPE);
  outTmp += gid * sizeof(TYPE);

  __global TYPE* inAddr = (__global TYPE*) inTmp;
  __global TYPE* outAddr = (__global TYPE*) outTmp;
  *outAddr = *inAddr;
}
)";

static const std::string LOAD_PRIVATE_REGISTER_ALIAS = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)

__kernel void test(const __global STORAGE* in, __global TYPE* out, __global CAT(TYPE,2)* out2, __global TYPE* out3, __global CAT(TYPE,4)* out4, __global CAT(TYPE,8)* out8, __global CAT(TYPE,16)* out16)
{
    // should be lowered to register
    __private STORAGE data[16];
    __private TYPE* data_in = (__private TYPE*) data;
    size_t i = get_global_id(0);
    for(int k = 0; k < 16; k++)
        data[k] = in[i + k] + 1;

    // test out the different vload versions
    float factor = ((float)sizeof(STORAGE)) / sizeof(TYPE);
    for(int k = 0; k < (int)(factor * 16); ++k)
        out[k] = data_in[k];
    for(int k = 0; k < (int)(factor * 8); ++k)
        out2[k] = vload2(k, data_in);
    for(int k = 0; k < (int)(factor * 5); ++k)
        vstore3(vload3(k, data_in), k, out3);
    for(int k = 0; k < (int)(factor * 4); ++k)
        out4[k] = vload4(k, data_in);
    for(int k = 0; k < (int)(factor * 2); ++k)
        out8[k] = vload8(k, data_in);
    for(int k = 0; k < (int)factor; ++k)
        out16[k] = vload16(k, data_in);
})";

static const std::string STORE_PRIVATE_REGISTER_ALIAS = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)

__kernel void test(const __global TYPE* in, __global STORAGE* out, __global STORAGE* out2, __global STORAGE* out3, __global STORAGE* out4, __global STORAGE* out8, __global STORAGE* out16)
{
    // should be lowered to register
    __private STORAGE data[16];
    __private TYPE* data_out = (__private TYPE*) data;
    size_t i = get_global_id(0);

    // test out the different vstore versions
    float factor = ((float)sizeof(STORAGE)) / sizeof(TYPE);

    for(int k = 0; k < (int)(factor * 16); ++k)
        data_out[k] = in[k];
    for(int k = 0; k < 16; ++k)
        out[i + k] = data[k] + 1;

    for(int k = 0; k < (int)(factor * 8); ++k)
        vstore2(((__global CAT(TYPE,2)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out2[i + k] = data[k] + 1;

    for(int k = 0; k < (int)(factor * 5); ++k)
        vstore3(vload3(k, in), k, data_out);
    for(int k = 0; k < 16; ++k)
        out3[i + k] = data[k] + 1;

    for(int k = 0; k < (int)(factor * 4); ++k)
        vstore4(((__global CAT(TYPE,4)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out4[i + k] = data[k] + 1;

    for(int k = 0; k < (int)(factor * 2); ++k)
        vstore8(((__global CAT(TYPE,8)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out8[i + k] = data[k] + 1;

    for(int k = 0; k < (int)factor; ++k)
        vstore16(((__global CAT(TYPE,16)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out16[i + k] = data[k] + 1;
})";

static const std::string STORE_PRIVATE_REGISTER_ALIAS_STRIDED = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)

__kernel void test(const __global TYPE* in, __global STORAGE* out, __global STORAGE* out2, __global STORAGE* out3, __global STORAGE* out4, __global STORAGE* out8, __global STORAGE* out16)
{
    // should be lowered to register
    __private STORAGE data[16];
    __private TYPE* data_out = (__private TYPE*) data;
    size_t i = get_global_id(0);

    // test out the different vstore versions
    // also leave some space to test that we do not write surrounding data
    float factor = ((float)sizeof(STORAGE)) / sizeof(TYPE);

    for(int k = 0; k < 16; ++k)
        data[k] = 0x42;
    for(int k = 0; k < (int)(factor * 16); k += 2)
        data_out[k] = in[k];
    for(int k = 0; k < 16; ++k)
        out[i + k] = data[k] + 1;

    for(int k = 0; k < 16; ++k)
        data[k] = 0x42;
    for(int k = 0; k < (int)(factor * 8); k += 2)
        vstore2(((__global CAT(TYPE,2)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out2[i + k] = data[k] + 1;

    for(int k = 0; k < (int)(factor * 5); k += 2)
        vstore3(vload3(k, in), k, data_out);
    for(int k = 0; k < 16; ++k)
        out3[i + k] = data[k] + 1;

    for(int k = 0; k < 16; ++k)
        data[k] = 0x42;
    for(int k = 0; k < (int)(factor * 4); k += 2)
        vstore4(((__global CAT(TYPE,4)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out4[i + k] = data[k] + 1;

    for(int k = 0; k < 16; ++k)
        data[k] = 0x42;
    for(int k = 0; k < (int)(factor * 2); k += 2)
        vstore8(((__global CAT(TYPE,8)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out8[i + k] = data[k] + 1;

    for(int k = 0; k < 16; ++k)
        data[k] = 0x42;
    for(int k = 0; k < (int)factor; k+= 2)
        vstore16(((__global CAT(TYPE,16)*)in)[k], k, data_out);
    for(int k = 0; k < 16; ++k)
        out16[i + k] = data[k] + 1;
})";

template <typename T, T divisor>
static bool checkIsMultipleOf(T val, std::size_t index)
{
    return val % divisor == 0;
}

template <typename T>
static std::vector<T> toStorageTestResult(std::vector<T>&& input)
{
    std::vector<T> result = std::move(input);
    for(auto& val : result)
    {
        val = static_cast<T>(val + (val + 3) + (val + 7) + (val + 11) + (val + 13) + (val + 17));
    }
    return result;
}

template <typename T, std::size_t N>
static std::vector<T> maskOffOddEntries(const std::vector<T>& input, std::size_t accessTypeSize, T maskValue)
{
    auto result = input;
    if(sizeof(T) <= accessTypeSize)
    {
        // e.g. for storing short3 into short[], set 3 shorts to value and 3 shorts to default
        // e.g. for storing int2 into short[], set 4 shorts to value and 4 shorts to default
        auto M = N * accessTypeSize / sizeof(T);
        for(std::size_t i = 0; i < result.size(); ++i)
        {
            result[i] = ((i % (2 * M)) >= M ? maskValue : result[i]) + 1;
        }
    }
    else
    {
        // need to set part of the result to the part of the default value
        auto typeOffset = accessTypeSize * 8;
        auto typeMask = (1u << typeOffset) - 1u;
        for(std::size_t i = 0; i < result.size(); ++i)
        {
            auto val = result[i];
            for(std::size_t k = 0; k < (sizeof(T) / accessTypeSize); ++k)
            {
                auto offset = typeOffset * k;
                auto bitMask = typeMask << offset;
                auto subMask = maskValue & bitMask;
                if(((i * (sizeof(T) / accessTypeSize) + k) % (2 * N)) >= N)
                    val = static_cast<T>((val & ~bitMask) | subMask);
            }
            result[i] = val + 1;
        }
    }
    return result;
}

template <typename T>
static void registerPrivateAliasingTests(const std::string& typeName)
{
    using namespace test_data;

    std::vector<T> data(16);
    std::iota(data.begin(), data.end(), -5);

    std::vector<T> result(16);
    std::transform(data.begin(), data.end(), result.begin(), [](T val) { return val + 1; });

    // the actual types do not matter, behavior is only dependent on the type's bit-width
    std::vector<std::pair<std::string, uint8_t>> accessTypes = {{"char", 1}, {"short", 2}, {"int", 4}};
    for(const auto& accessType : accessTypes)
    {
        auto vector3Result = result;
        // we have 16 * N bytes of data and load/store 3 * I * 5 * M bytes of data
        // e.g. for char16 to int3, we do not transfer 4 byte
        // e.g. for int16 to 20 * char3, we do not transfer 4 byte
        // e.g. for char16 to 2 * short3, we do not transfer 4 byte
        // e.g. for short16 to 2 * int3, we do not transfer 8 byte
        // e.g. for short16 to long3, we do not transfer 8 byte
        // e.g. for long16 to 21 * short3, we do not transfer 2 byte
        auto bytesToRemove = (sizeof(T) * 16) % (accessType.second * 3);
        for(std::size_t i = 0; i < bytesToRemove; i += sizeof(T))
            vector3Result.pop_back();

        std::vector<ResultVerification> loadChecks{
            checkParameterEquals(1, std::vector<T>{result}),
            checkParameterEquals(2, std::vector<T>{result}),
        };
        std::vector<ResultVerification> storeStridedChecks{
            checkParameterEquals(1, maskOffOddEntries<T, 1>(data, accessType.second, T{0x42})),
            checkParameterEquals(2, maskOffOddEntries<T, 2>(data, accessType.second, T{0x42})),
        };
        if(accessType.second <= sizeof(T) * 4)
        {
            loadChecks.push_back(checkParameterEquals(3, std::move(vector3Result)));
            storeStridedChecks.push_back(checkParameterEquals(3,
                maskOffOddEntries<T, 3>(
                    std::vector<T>(data.begin(), data.begin() + vector3Result.size()), accessType.second, T{0x42})));
            loadChecks.push_back(checkParameterEquals(4, std::vector<T>{result}));
            storeStridedChecks.push_back(
                checkParameterEquals(4, maskOffOddEntries<T, 4>(data, accessType.second, T{0x42})));
        }
        if(accessType.second <= sizeof(T) * 2)
        {
            loadChecks.push_back(checkParameterEquals(5, std::vector<T>{result}));
            storeStridedChecks.push_back(
                checkParameterEquals(5, maskOffOddEntries<T, 8>(data, accessType.second, T{0x42})));
        }
        if(accessType.second <= sizeof(T))
        {
            // we can't e.g. run the vload16 with 64-bit data type on 16 * 32-bit of data
            loadChecks.push_back(checkParameterEquals(6, std::vector<T>{result}));
            storeStridedChecks.push_back(
                checkParameterEquals(6, maskOffOddEntries<T, 16>(data, accessType.second, T{0x42})));
        }

        auto storeChecks = loadChecks;

        registerTest(
            TestData{"vload_alias_private_register_" + typeName + "_to_" + accessType.first, DataFilter::MEMORY_ACCESS,
                &LOAD_PRIVATE_REGISTER_ALIAS, "-DSTORAGE=" + typeName + " -DTYPE=" + accessType.first, "test",
                {toBufferParameter(std::vector<T>{data}), toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42))},
                toDimensions(1), std::move(loadChecks)});

        registerTest(
            TestData{"vstore_alias_private_register_" + accessType.first + "_to_" + typeName, DataFilter::MEMORY_ACCESS,
                &STORE_PRIVATE_REGISTER_ALIAS, "-DSTORAGE=" + typeName + " -DTYPE=" + accessType.first, "test",
                {toBufferParameter(std::vector<T>{data}), toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42)),
                    toBufferParameter(std::vector<T>(data.size(), 0x42))},
                toDimensions(1), std::move(storeChecks)});

        registerTest(TestData{"vstore_alias_private_register_strided_" + accessType.first + "_to_" + typeName,
            DataFilter::MEMORY_ACCESS, &STORE_PRIVATE_REGISTER_ALIAS_STRIDED,
            "-DSTORAGE=" + typeName + " -DTYPE=" + accessType.first, "test",
            {toBufferParameter(std::vector<T>{data}), toBufferParameter(std::vector<T>(data.size(), 0x42)),
                toBufferParameter(std::vector<T>(data.size(), 0x42)),
                toBufferParameter(std::vector<T>(data.size(), 0x42)),
                toBufferParameter(std::vector<T>(data.size(), 0x42)),
                toBufferParameter(std::vector<T>(data.size(), 0x42)),
                toBufferParameter(std::vector<T>(data.size(), 0x42))},
            toDimensions(1), std::move(storeStridedChecks)});
    }
}

void test_data::registerMemoryTests()
{
    registerTest(TestData{"pointer_cast", DataFilter::MEMORY_ACCESS, &CASTPOINTER, "-DTYPE=int16", "test",
        {toBufferParameter(std::vector<uint32_t>(16 * 12, 0x42)), toBufferParameter(toRange<uint32_t>(0, 16 * 12))},
        toDimensions(12), {checkParameterEquals(0, toRange<uint32_t>(0, 16 * 12))}});

    registerTest(TestData{"memory_copy_long", DataFilter::MEMORY_ACCESS, &COPY64BIT, "", "test_copy",
        {toBufferParameter(std::vector<uint32_t>(16, 0x42)), toBufferParameter(toRange<uint32_t>(0, 16))},
        toDimensions(8), {checkParameterEquals(0, toRange<uint32_t>(0, 16))}});

    registerTest(TestData{"memory_read_write_long", DataFilter::MEMORY_ACCESS, &COPY64BIT, "", "test_load_store",
        {toBufferParameter(std::vector<uint32_t>(16, 0x42)), toBufferParameter(toRange<uint32_t>(0, 16))},
        toDimensions(8),
        {checkParameterEquals(0, toRange<uint32_t>(0, 16)), checkParameterEquals(1, toRange<uint32_t>(0, 16))}});

    // covers 64-bit TMU read
    registerTest(TestData{"memory_read_long_lower", DataFilter::MEMORY_ACCESS, &READ64BITSINGLEWORD, "", "test_lower",
        {toBufferParameter(std::vector<uint32_t>(8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 16)),
            toBufferParameter(toRange<uint64_t>(0x0FF0000000000000, 0x0FF0000000000008))},
        toDimensions(8), {checkParameterEquals(0, std::vector<uint32_t>{0, 3, 6, 9, 12, 15, 18, 21})}});

    // covers 64-bit TMU read as well as lshr %src.upper >> 32 into %dest.lower
    registerTest(TestData{"memory_read_long_upper", DataFilter::MEMORY_ACCESS, &READ64BITSINGLEWORD, "", "test_upper",
        {toBufferParameter(std::vector<uint32_t>(8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 16)),
            toBufferParameter(toRange<uint64_t>(0x0FF0000000000000, 0x0FF0000000000008))},
        toDimensions(8),
        {checkParameterEquals(0,
            std::vector<uint32_t>{
                0x0FF00001, 0x0FF00003, 0x0FF00005, 0x0FF00007, 0x0FF00009, 0x0FF0000B, 0x0FF0000D, 0x0FF0000F})}});

    registerTest(TestData{"select_write_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_select_write_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(std::vector<int32_t>(30, 0x42)),
            toBufferParameter(std::vector<int32_t>(30, 0x42))},
        toDimensions(10, 1, 1, 3, 1, 1),
        {checkParameterEquals(1,
             std::vector<uint32_t>{17, 0x42, 19, 0x42, 21, 0x42, 23, 0x42, 25, 0x42, 27, 0x42, 29, 0x42, 31, 0x42, 33,
                 0x42, 35, 0x42, 37, 0x42, 39, 0x42, 41, 0x42, 43, 0x42, 45, 0x42}),
            checkParameterEquals(2,
                std::vector<uint32_t>{0x42, 18, 0x42, 20, 0x42, 22, 0x42, 24, 0x42, 26, 0x42, 28, 0x42, 30, 0x42, 32,
                    0x42, 34, 0x42, 36, 0x42, 38, 0x42, 40, 0x42, 42, 0x42, 44, 0x42, 46})}});

    registerTest(TestData{"select_read_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_select_read_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(toRange<int32_t>(100, 130)),
            toBufferParameter(std::vector<int32_t>(30, 0x42))},
        toDimensions(10, 1, 1, 3, 1, 1),
        {checkParameterEquals(2,
            std::vector<uint32_t>{17, 118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29, 130, 31, 132, 33, 134, 35,
                136, 37, 138, 39, 140, 41, 142, 43, 144, 45, 146})}});

    registerTest(TestData{"select_read_write_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string,
        "", "test_select_read_write_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(toRange<int32_t>(100, 130))},
        toDimensions(10, 1, 1, 3, 1, 1),
        {checkParameterEquals(0,
             std::vector<uint32_t>{117, 1, 119, 3, 121, 5, 123, 7, 125, 9, 127, 11, 129, 13, 131, 15, 133, 17, 135, 19,
                 137, 21, 139, 23, 141, 25, 143, 27, 145, 29}),
            checkParameterEquals(1,
                std::vector<uint32_t>{100, 18, 102, 20, 104, 22, 106, 24, 108, 26, 110, 28, 112, 30, 114, 32, 116, 34,
                    118, 36, 120, 38, 122, 40, 124, 42, 126, 44, 128, 46})}});

    registerTest(TestData{"select_copy_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_select_copy_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(toRange<int32_t>(100, 130))},
        toDimensions(10, 1, 1, 3, 1, 1),
        {checkParameterEquals(0,
             std::vector<uint32_t>{100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17, 118, 19,
                 120, 21, 122, 23, 124, 25, 126, 27, 128, 29}),
            checkParameterEquals(1,
                std::vector<uint32_t>{100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17, 118,
                    19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29})}});

    registerTest(TestData{"phi_write_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_phi_write_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(std::vector<int32_t>(30, 0x42)),
            toBufferParameter(std::vector<int32_t>(30, 0x42)), toBufferParameter(std::vector<uint32_t>{3})},
        toDimensions(5, 1, 1, 2, 1, 1),
        {checkParameterEquals(1,
             std::vector<uint32_t>{17, 0x42, 19, 0x42, 21, 0x42, 23, 0x42, 25, 0x42, 17, 0x42, 19, 0x42, 21, 0x42, 23,
                 0x42, 25, 0x42, 17, 0x42, 19, 0x42, 21, 0x42, 23, 0x42, 25, 0x42}),
            checkParameterEquals(2,
                std::vector<uint32_t>{0x42, 18, 0x42, 20, 0x42, 22, 0x42, 24, 0x42, 26, 0x42, 18, 0x42, 20, 0x42, 22,
                    0x42, 24, 0x42, 26, 0x42, 18, 0x42, 20, 0x42, 22, 0x42, 24, 0x42, 26})}});

    registerTest(TestData{"phi_read_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_phi_read_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(toRange<int32_t>(100, 130)),
            toBufferParameter(std::vector<int32_t>(30, 0x42)), toBufferParameter(std::vector<uint32_t>{3})},
        toDimensions(5, 1, 1, 2, 1, 1),
        {checkParameterEquals(2,
            std::vector<uint32_t>{17, 118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29, 130, 31, 132, 33, 134, 35,
                136, 37, 138, 39, 140, 41, 142, 43, 144, 45, 146})}});

    registerTest(TestData{"phi_read_write_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_phi_read_write_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(toRange<int32_t>(100, 130)),
            toBufferParameter(std::vector<uint32_t>{3})},
        toDimensions(5, 1, 1, 2, 1, 1),
        {checkParameterEquals(0,
             std::vector<uint32_t>{117, 1, 119, 3, 121, 5, 123, 7, 125, 9, 127, 11, 129, 13, 131, 15, 133, 17, 135, 19,
                 137, 21, 139, 23, 141, 25, 143, 27, 145, 29}),
            checkParameterEquals(1,
                std::vector<uint32_t>{100, 18, 102, 20, 104, 22, 106, 24, 108, 26, 110, 28, 112, 30, 114, 32, 116, 34,
                    118, 36, 120, 38, 122, 40, 124, 42, 126, 44, 128, 46})}});

    registerTest(TestData{"phi_copy_address", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string, "",
        "test_phi_copy_address_simple",
        {toBufferParameter(toRange<int32_t>(0, 30)), toBufferParameter(toRange<int32_t>(100, 130)),
            toBufferParameter(std::vector<uint32_t>{3})},
        toDimensions(5, 1, 1, 2, 1, 1),
        {checkParameterEquals(0,
             std::vector<uint32_t>{100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17, 118, 19,
                 120, 21, 122, 23, 124, 25, 126, 27, 128, 29}),
            checkParameterEquals(1,
                std::vector<uint32_t>{100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17, 118,
                    19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29})}});

    registerTest(TestData{"select_read_address_local", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string,
        "", "test_select_read_address_local",
        {toBufferParameter(std::vector<int32_t>(30, 0x42)), toBufferParameter(std::vector<int32_t>(30, 0x42))},
        toDimensions(10, 1, 1, 3, 1, 1), {checkParameterEquals(1, toRange<uint32_t>(0 + 17, 30 + 17))}});

    registerTest(TestData{"select_read_address_private", DataFilter::MEMORY_ACCESS, &test_conditional_address_cl_string,
        "", "test_select_read_address_private",
        {toBufferParameter(toRange<int32_t>(0, 16)), toBufferParameter(std::vector<int32_t>(16, 0x42))},
        toDimensions(8, 1, 1, 2, 1, 1),
        {checkParameterEquals(
            1, std::vector<uint32_t>{17, 18, 19, 20, 21, 22, 23, 24, 59, 34, 59, 34, 59, 34, 59, 34})}});

    registerTest(
        TestData{"constant_load", DataFilter::MEMORY_ACCESS, &test_constant_load_cl_string, "", "test_constant_load",
            {toBufferParameter(std::vector<uint32_t>(2)), toBufferParameter(std::vector<uint16_t>(2)),
                toBufferParameter(std::vector<uint8_t>(2)), toScalarParameter(0)},
            toDimensions(1),
            {
                checkParameterEquals(0, std::vector<uint32_t>{0x42, 0x17 + 42}),
                checkParameterEquals(1, std::vector<uint16_t>{0x42, 0x17 + 42}),
                checkParameterEquals(2, std::vector<uint8_t>{0x42, 0x17 + 42}),
            }});

    registerTest(TestData{"global_data", DataFilter::MEMORY_ACCESS | DataFilter::ASYNC_BARRIER, &test_other_cl_string,
        "", "test_global_data", {toScalarParameter(1), toBufferParameter(std::vector<int32_t>(2))}, toDimensions(1),
        {checkParameterEquals(1, std::vector<uint32_t>{2, 21})}});

    registerTest(TestData{"constant_storage_small", DataFilter::MEMORY_ACCESS, &local_private_storage_cl_string, "",
        "test_constant_storage", {toBufferParameter(std::vector<uint8_t>(12, 0x42))}, toDimensions(12),
        {checkParameterEquals(0, std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '\0'})}});

    registerTest(TestData{"constant_storage_large", DataFilter::MEMORY_ACCESS, &local_private_storage_cl_string, "",
        "test_constant_storage2", {toBufferParameter(std::vector<uint8_t>(12, 0x42))}, toDimensions(12),
        {checkParameterEquals(0, std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'})}});

    registerTest(
        TestData{"local_storage", DataFilter::MEMORY_ACCESS, &local_private_storage_cl_string, "", "test_local_storage",
            {toBufferParameter(std::vector<uint32_t>(24, 7)), toBufferParameter(std::vector<uint32_t>(24, 0x42))},
            toDimensions(12, 1, 1, 2, 1, 1),
            {checkParameterMatches<uint32_t>(1, 24, checkIsMultipleOf<uint32_t, 7u>, "7 divides value")}});

    registerTest(TestData{"private_storage", DataFilter::MEMORY_ACCESS, &local_private_storage_cl_string, "",
        "test_private_storage",
        {toBufferParameter(std::vector<uint32_t>(24, 7)), toBufferParameter(std::vector<uint32_t>(24, 0x42))},
        toDimensions(12, 1, 1, 2, 1, 1),
        {checkParameterEquals(1,
            std::vector<uint32_t>{
                14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28})}});

    registerTest(TestData{"register_storage", DataFilter::MEMORY_ACCESS, &local_private_storage_cl_string, "",
        "test_register_storage", {toBufferParameter(std::vector<uint8_t>(12, 0x42))}, toDimensions(12),
        {checkParameterEquals(0, std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '\0'})}});

    registerTest(TestData{"storage_private_char", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=char -DSTORAGE=__private", "test",
        {toBufferParameter(std::vector<int8_t>(16)), toBufferParameter(toRange<int8_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int8_t>(-6, 10)))}});

    registerTest(TestData{"storage_local_char", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=char -DSTORAGE=__local", "test",
        {toBufferParameter(std::vector<int8_t>(16)), toBufferParameter(toRange<int8_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int8_t>(-6, 10)))}});

    registerTest(
        TestData{"storage_global_char", DataFilter::MEMORY_ACCESS, &test_storage_cl_string, "-DTYPE=char", "test",
            {toBufferParameter(std::vector<int8_t>(16)), toBufferParameter(toRange<int8_t>(-6, 10)),
                toBufferParameter(std::vector<int8_t>(16)), toBufferParameter(std::vector<int8_t>(16)),
                toBufferParameter(std::vector<int8_t>(16)), toBufferParameter(std::vector<int8_t>(16)),
                toBufferParameter(std::vector<int8_t>(16)), toBufferParameter(std::vector<int8_t>(16))},
            toDimensions(1), {checkParameterEquals(0, toStorageTestResult(toRange<int8_t>(-6, 10)))}});

    registerTest(TestData{"storage_private_short", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=short -DSTORAGE=__private", "test",
        {toBufferParameter(std::vector<int16_t>(16)), toBufferParameter(toRange<int16_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int16_t>(-6, 10)))}});

    registerTest(TestData{"storage_local_short", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=short -DSTORAGE=__local", "test",
        {toBufferParameter(std::vector<int16_t>(16)), toBufferParameter(toRange<int16_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int16_t>(-6, 10)))}});

    registerTest(
        TestData{"storage_global_short", DataFilter::MEMORY_ACCESS, &test_storage_cl_string, "-DTYPE=short", "test",
            {toBufferParameter(std::vector<int16_t>(16)), toBufferParameter(toRange<int16_t>(-6, 10)),
                toBufferParameter(std::vector<int16_t>(16)), toBufferParameter(std::vector<int16_t>(16)),
                toBufferParameter(std::vector<int16_t>(16)), toBufferParameter(std::vector<int16_t>(16)),
                toBufferParameter(std::vector<int16_t>(16)), toBufferParameter(std::vector<int16_t>(16))},
            toDimensions(1), {checkParameterEquals(0, toStorageTestResult(toRange<int16_t>(-6, 10)))}});

    registerTest(TestData{"storage_private_int", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=int -DSTORAGE=__private", "test",
        {toBufferParameter(std::vector<int32_t>(16)), toBufferParameter(toRange<int32_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int32_t>(-6, 10)))}});

    registerTest(TestData{"storage_local_int", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=int -DSTORAGE=__local", "test",
        {toBufferParameter(std::vector<int32_t>(16)), toBufferParameter(toRange<int32_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int32_t>(-6, 10)))}});

    registerTest(
        TestData{"storage_global_int", DataFilter::MEMORY_ACCESS, &test_storage_cl_string, "-DTYPE=int", "test",
            {toBufferParameter(std::vector<int32_t>(16)), toBufferParameter(toRange<int32_t>(-6, 10)),
                toBufferParameter(std::vector<int32_t>(16)), toBufferParameter(std::vector<int32_t>(16)),
                toBufferParameter(std::vector<int32_t>(16)), toBufferParameter(std::vector<int32_t>(16)),
                toBufferParameter(std::vector<int32_t>(16)), toBufferParameter(std::vector<int32_t>(16))},
            toDimensions(1), {checkParameterEquals(0, toStorageTestResult(toRange<int32_t>(-6, 10)))}});

    // TODO result mismatch on actual hardware
    // Got 0x0000'000E'0000'000F instead of 0x0F for 0st element, rest is correct (independent of initial values of
    // result buffer)
    // TODO code generated on RPi and host is mostly identical -> buffer/parameter or code + emulator error
    registerTest(TestData{"storage_private_long", DataFilter::MEMORY_ACCESS, &test_storage_cl_string,
        "-DTYPE=long -DSTORAGE=__private", "test",
        {toBufferParameter(std::vector<int64_t>(16, 0x42)), toBufferParameter(toRange<int64_t>(-6, 10))},
        toDimensions(1), {checkParameterEquals(0, toStorageTestResult(toRange<int64_t>(-6, 10)))}});

    // TODO hangs/takes long in Normalization step! Is long/a lot in LongOperations.cpp:431
    registerTest(TestData{"storage_local_long", DataFilter::DISABLED | DataFilter::MEMORY_ACCESS,
        &test_storage_cl_string, "-DTYPE=long -DSTORAGE=__local", "test",
        {toBufferParameter(std::vector<int64_t>(16)), toBufferParameter(toRange<int64_t>(-6, 10))}, toDimensions(1),
        {checkParameterEquals(0, toStorageTestResult(toRange<int64_t>(-6, 10)))}});

    // TODO result mismatch on actual hardware
    // Got 0x0000'0011'0000'000F instead of 0xF for first element, also is off by +17 for remainder elements
    // (independent of initial values of buffers)
    // TODO code generated on RPi and host is mostly identical -> buffer/parameter or code + emulator error
    registerTest(
        TestData{"storage_global_long", DataFilter::MEMORY_ACCESS, &test_storage_cl_string, "-DTYPE=long", "test",
            {toBufferParameter(std::vector<int64_t>(16, 0x40)), toBufferParameter(toRange<int64_t>(-6, 10)),
                toBufferParameter(std::vector<int64_t>(16, 0x41)), toBufferParameter(std::vector<int64_t>(16, 0x42)),
                toBufferParameter(std::vector<int64_t>(16, 0x43)), toBufferParameter(std::vector<int64_t>(16, 0x44)),
                toBufferParameter(std::vector<int64_t>(16, 0x45)), toBufferParameter(std::vector<int64_t>(16, 0x46))},
            toDimensions(1), {checkParameterEquals(0, toStorageTestResult(toRange<int64_t>(-6, 10)))}});

    registerTest(TestData{"global_parameter_unaligned", DataFilter::MEMORY_ACCESS, &unaligned_memory_access_cl_string,
        "", "test_global",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 32)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 11}), toBufferParameter(std::vector<uint32_t>(2 * 16))},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19, 0x1a,
                0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
                0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f})}});

    registerTest(TestData{"local_parameter_unaligned", DataFilter::MEMORY_ACCESS, &unaligned_memory_access_cl_string,
        "", "test_local_parameter",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 32)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 11}), toBufferParameter(std::vector<uint32_t>(2 * 16))},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19, 0x1a,
                0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
                0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f})}});

    registerTest(TestData{"local_vpm_full_row_unaligned", DataFilter::DISABLED | DataFilter::MEMORY_ACCESS,
        &unaligned_memory_access_cl_string, "", "test_local_vpm_full_row",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 32)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 11})},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19, 0x1a,
                0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
                0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f})}});

    registerTest(TestData{"local_vpm_partial_row_unaligned", DataFilter::DISABLED | DataFilter::MEMORY_ACCESS,
        &unaligned_memory_access_cl_string, "", "test_local_vpm_partial_row",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 32)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 11})},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19, 0x1a,
                0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
                0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f})}});

    registerTest(TestData{"private_register_unaligned", DataFilter::MEMORY_ACCESS, &unaligned_memory_access_cl_string,
        "", "test_private_register",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 16)),
            toBufferParameter(toRange<uint32_t>(0, 5))},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x0, 0x1, 0xc, 0xd, 0xe, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa, 0xc, 0xd, 0xe, 0xf, 0x0, 0x8,
                0x9, 0xa, 0xc, 0xd, 0xe, 0xf, 0x0, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa, 0xc, 0xd, 0xd, 0xe, 0xf, 0x6, 0x7, 0x8,
                0x9, 0xa})}});

    registerTest(TestData{"private_vpm_full_row_unaligned", DataFilter::MEMORY_ACCESS,
        &unaligned_memory_access_cl_string, "", "test_private_vpm_full_row",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 32)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 11})},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19, 0x1a,
                0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
                0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f})}});

    registerTest(TestData{"private_vpm_partial_row_unaligned", DataFilter::MEMORY_ACCESS,
        &unaligned_memory_access_cl_string, "", "test_private_vpm_partial_row",
        {toBufferParameter(std::vector<uint32_t>(5 * 8, 0x42)), toBufferParameter(toRange<uint32_t>(0, 32)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 11})},
        toDimensions(1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19, 0x1a,
                0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
                0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f})}});

    registerTest(TestData{"vpm_read", DataFilter::MEMORY_ACCESS, &test_vpm_read_cl_string, "", "test_vpm_read",
        {toBufferParameter(toRange<int32_t>(0, 4 * 10)), toBufferParameter(toRange<int32_t>(100, 100 + 4 * 10 * 3))},
        toDimensions(1),
        {checkParameterEquals(0,
             std::vector<uint32_t>{100, 101, 102, 103, 112, 113, 114, 115, 124, 125, 126, 127, 136, 137, 138, 139, 148,
                 149, 150, 151, 160, 161, 162, 163, 172, 173, 174, 175, 184, 185, 186, 187, 196, 197, 198, 199}),
            checkParameterEquals(1,
                std::vector<uint32_t>{100, 101, 102, 103, 112, 113, 114, 115, 124, 125, 126, 127, 136, 137, 138, 139,
                    148, 149, 150, 151, 160, 161, 162, 163, 172, 173, 174, 175, 184, 185, 186, 187, 196, 197, 198,
                    199})}});

    registerTest(TestData{"vpm_write", DataFilter::MEMORY_ACCESS, &test_vpm_write_cl_string, "", "test_vpm_write",
        {toBufferParameter(toRange(0, 16)), toBufferParameter(std::vector<uint32_t>(10 * 16)),
            toBufferParameter(std::vector<uint16_t>(10 * 16)), toBufferParameter(std::vector<uint8_t>(10 * 16)),
            toBufferParameter(std::vector<uint32_t>(10 * 16 * 4, 0x42))},
        toDimensions(1),
        {checkParameterEquals(1,
             std::vector<uint32_t>{
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 1. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 2. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 3. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 4. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 5. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 6. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 7. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 8. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 9. iteration
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15  // 10. iteration
             }),
            checkParameterEquals(2,
                std::vector<uint16_t>{
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 1. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 2. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 3. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 4. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 5. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 6. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 7. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 8. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 9. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15  // 10. iteration
                }),
            checkParameterEquals(3,
                std::vector<uint8_t>{
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 1. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 2. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 3. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 4. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 5. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 6. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 7. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 8. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 9. iteration
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15  // 10. iteration
                }),
            checkParameterEquals(4,
                std::vector<uint32_t>{
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 1. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 2. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 3. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 4. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 5. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 6. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 7. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 8. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 9. iteration
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 // 10. iteration
                })}});

    registerTest(
        TestData{"cross_group_memory_access_simple", DataFilter::MEMORY_ACCESS, &test_cross_group_access_cl_string, "",
            "test_cross_write_simple", {toBufferParameter(std::vector<uint32_t>(40, 0x42))}, toDimensions(10, 1, 1, 4),
            {checkParameterEquals(0,
                std::vector<uint32_t>{45, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, // sum([0, 9])
                    145, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                  // sum([10, 19])
                    245, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                  //
                    345, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42})}});

    registerTest(TestData{"cross_group_memory_access_advanced", DataFilter::MEMORY_ACCESS,
        &test_cross_group_access_cl_string, "", "test_cross_write_advanced",
        {toBufferParameter(std::vector<uint32_t>(40, 0x42)), toBufferParameter(std::vector<uint32_t>(16, 0x42))},
        toDimensions(10, 1, 1, 4),
        {checkParameterEquals(0,
            std::vector<uint32_t>{28, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, // sum([0, 7])
                108, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                  // sum([10, 17])
                188, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                  //
                268, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42})}});

    registerPrivateAliasingTests<int8_t>("char");
    registerPrivateAliasingTests<int16_t>("short");
    registerPrivateAliasingTests<int32_t>("int");
}
