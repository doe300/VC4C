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

    for(int k = 0; k < 16; ++k)
        data[k] = 0x42;
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
            result[i] = static_cast<T>(((i % (2 * M)) >= M ? maskValue : result[i]) + 1);
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
            result[i] = static_cast<T>(val + 1);
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
        auto vector3Size = vector3Result.size();

        // XXX has compilation errors in llvm-spirv (on CI, newer local version compiles them correctly)
        auto flags = DataFilter::MEMORY_ACCESS | DataFilter::SPIRV_DISABLED;

        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>> loadBuilder(
            "vload_alias_private_register_" + typeName + "_to_" + accessType.first, LOAD_PRIVATE_REGISTER_ALIAS, "test",
            "-DSTORAGE=" + typeName + " -DTYPE=" + accessType.first);
        loadBuilder.setFlags(flags);
        loadBuilder.template setParameter<0>(std::vector<T>(data));
        loadBuilder.template allocateParameter<1>(data.size(), 0x42);
        loadBuilder.template allocateParameter<2>(data.size(), 0x42);
        loadBuilder.template allocateParameter<3>(vector3Size, 0x42);
        loadBuilder.template allocateParameter<4>(data.size(), 0x42);
        loadBuilder.template allocateParameter<5>(data.size(), 0x42);
        loadBuilder.template allocateParameter<6>(data.size(), 0x42);

        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>> storeBuilder(
            "vstore_alias_private_register_" + accessType.first + "_to_" + typeName, STORE_PRIVATE_REGISTER_ALIAS,
            "test", "-DSTORAGE=" + typeName + " -DTYPE=" + accessType.first);
        storeBuilder.setFlags(flags);
        storeBuilder.template setParameter<0>(std::vector<T>(data));
        storeBuilder.template allocateParameter<1>(data.size(), 0x42);
        storeBuilder.template allocateParameter<2>(data.size(), 0x42);
        storeBuilder.template allocateParameter<3>(vector3Size, 0x42);
        storeBuilder.template allocateParameter<4>(data.size(), 0x42);
        storeBuilder.template allocateParameter<5>(data.size(), 0x42);
        storeBuilder.template allocateParameter<6>(data.size(), 0x42);

        TestDataBuilder<Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>, Buffer<T>>
            storeStridedBuilder("vstore_alias_private_register_strided_" + accessType.first + "_to_" + typeName,
                STORE_PRIVATE_REGISTER_ALIAS_STRIDED, "test", "-DSTORAGE=" + typeName + " -DTYPE=" + accessType.first);
        storeStridedBuilder.setFlags(flags);
        storeStridedBuilder.template setParameter<0>(std::vector<T>(data));
        storeStridedBuilder.template allocateParameter<1>(data.size(), 0x42);
        storeStridedBuilder.template allocateParameter<2>(data.size(), 0x42);
        storeStridedBuilder.template allocateParameter<3>(vector3Size, 0x42);
        storeStridedBuilder.template allocateParameter<4>(data.size(), 0x42);
        storeStridedBuilder.template allocateParameter<5>(data.size(), 0x42);
        storeStridedBuilder.template allocateParameter<6>(data.size(), 0x42);

        loadBuilder.template checkParameterEquals<1>(std::vector<T>{result});
        loadBuilder.template checkParameterEquals<2>(std::vector<T>{result});
        storeBuilder.template checkParameterEquals<1>(std::vector<T>{result});
        storeBuilder.template checkParameterEquals<2>(std::vector<T>{result});
        storeStridedBuilder.template checkParameterEquals<1>(maskOffOddEntries<T, 1>(data, accessType.second, T{0x42}));
        storeStridedBuilder.template checkParameterEquals<2>(maskOffOddEntries<T, 2>(data, accessType.second, T{0x42}));

        if(accessType.second <= sizeof(T) * 4)
        {
            loadBuilder.template checkParameterEquals<3>(std::vector<T>(vector3Result));
            storeBuilder.template checkParameterEquals<3>(std::move(vector3Result));
            storeStridedBuilder.template checkParameterEquals<3>(maskOffOddEntries<T, 3>(
                std::vector<T>(data.begin(), data.begin() + vector3Size), accessType.second, T{0x42}));
            loadBuilder.template checkParameterEquals<4>(std::vector<T>{result});
            storeBuilder.template checkParameterEquals<4>(std::vector<T>{result});
            storeStridedBuilder.template checkParameterEquals<4>(
                maskOffOddEntries<T, 4>(data, accessType.second, T{0x42}));
        }
        if(accessType.second <= sizeof(T) * 2)
        {
            loadBuilder.template checkParameterEquals<5>(std::vector<T>{result});
            storeBuilder.template checkParameterEquals<5>(std::vector<T>{result});
            storeStridedBuilder.template checkParameterEquals<5>(
                maskOffOddEntries<T, 8>(data, accessType.second, T{0x42}));
        }
        if(accessType.second <= sizeof(T))
        {
            // we can't e.g. run the vload16 with 64-bit data type on 16 * 32-bit of data
            loadBuilder.template checkParameterEquals<6>(std::vector<T>{result});
            storeBuilder.template checkParameterEquals<6>(std::vector<T>{result});
            storeStridedBuilder.template checkParameterEquals<6>(
                maskOffOddEntries<T, 16>(data, accessType.second, T{0x42}));
        }
    }
}

void test_data::registerMemoryTests()
{
    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "pointer_cast", CASTPOINTER, "test", "-DTYPE=int16");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(12);
        builder.allocateParameter<0>(16 * 12, 0x42);
        builder.allocateParameterRange<1>(0, 16 * 12);
        builder.checkParameterEquals<0>(toRange<uint32_t>(0, 16 * 12));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder("memory_copy_long", COPY64BIT, "test_copy");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(8);
        builder.allocateParameter<0>(16, 0x42);
        builder.setParameter<1>(toRange(0u, 16u));
        builder.checkParameterEquals<0>(toRange(0u, 16u));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "memory_read_write_long", COPY64BIT, "test_load_store");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(8);
        builder.allocateParameter<0>(16, 0x42);
        builder.setParameter<1>(toRange(0u, 16u));
        builder.checkParameterEquals<0>(toRange(0u, 16u));
        builder.checkParameterEquals<1>(toRange(0u, 16u));
    }

    {
        // covers 64-bit TMU read
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint64_t>> builder(
            "memory_read_long_lower", READ64BITSINGLEWORD, "test_lower");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(8);
        builder.allocateParameter<0>(8, 0x42);
        builder.setParameter<1>(toRange(0u, 16u));
        builder.setParameter<2>(toRange<uint64_t>(0x0FF0000000000000, 0x0FF0000000000008));
        builder.checkParameterEquals<0>({0, 3, 6, 9, 12, 15, 18, 21});
    }

    {
        // covers 64-bit TMU read as well as lshr %src.upper >> 32 into %dest.lower
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint64_t>> builder(
            "memory_read_long_upper", READ64BITSINGLEWORD, "test_upper");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(8);
        builder.allocateParameter<0>(8, 0x42);
        builder.setParameter<1>(toRange(0u, 16u));
        builder.setParameter<2>(toRange<uint64_t>(0x0FF0000000000000, 0x0FF0000000000008));
        builder.checkParameterEquals<0>(
            {0x0FF00001, 0x0FF00003, 0x0FF00005, 0x0FF00007, 0x0FF00009, 0x0FF0000B, 0x0FF0000D, 0x0FF0000F});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_write_address", test_conditional_address_cl_string, "test_select_write_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 3, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.allocateParameter<1>(30, 0x42);
        builder.allocateParameter<2>(30, 0x42);
        builder.checkParameterEquals<1>({17, 0x42, 19, 0x42, 21, 0x42, 23, 0x42, 25, 0x42, 27, 0x42, 29, 0x42, 31, 0x42,
            33, 0x42, 35, 0x42, 37, 0x42, 39, 0x42, 41, 0x42, 43, 0x42, 45, 0x42});
        builder.checkParameterEquals<2>({0x42, 18, 0x42, 20, 0x42, 22, 0x42, 24, 0x42, 26, 0x42, 28, 0x42, 30, 0x42, 32,
            0x42, 34, 0x42, 36, 0x42, 38, 0x42, 40, 0x42, 42, 0x42, 44, 0x42, 46});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_read_address", test_conditional_address_cl_string, "test_select_read_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 3, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.setParameter<1>(toRange(100, 130));
        builder.allocateParameter<2>(30, 0x42);
        builder.checkParameterEquals<2>({17, 118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29, 130, 31, 132, 33,
            134, 35, 136, 37, 138, 39, 140, 41, 142, 43, 144, 45, 146});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_read_write_address", test_conditional_address_cl_string, "test_select_read_write_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 3, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.setParameter<1>(toRange(100, 130));
        builder.checkParameterEquals<0>({117, 1, 119, 3, 121, 5, 123, 7, 125, 9, 127, 11, 129, 13, 131, 15, 133, 17,
            135, 19, 137, 21, 139, 23, 141, 25, 143, 27, 145, 29});
        builder.checkParameterEquals<1>({100, 18, 102, 20, 104, 22, 106, 24, 108, 26, 110, 28, 112, 30, 114, 32, 116,
            34, 118, 36, 120, 38, 122, 40, 124, 42, 126, 44, 128, 46});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_copy_address", test_conditional_address_cl_string, "test_select_copy_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 3, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.setParameter<1>(toRange(100, 130));
        builder.checkParameterEquals<0>({100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17,
            118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29});
        builder.checkParameterEquals<1>({100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17,
            118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "phi_write_address", test_conditional_address_cl_string, "test_phi_write_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(5, 1, 1, 2, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.allocateParameter<1>(30, 0x42);
        builder.allocateParameter<2>(30, 0x42);
        builder.setParameter<3>({3});
        builder.checkParameterEquals<1>({17, 0x42, 19, 0x42, 21, 0x42, 23, 0x42, 25, 0x42, 17, 0x42, 19, 0x42, 21, 0x42,
            23, 0x42, 25, 0x42, 17, 0x42, 19, 0x42, 21, 0x42, 23, 0x42, 25, 0x42});
        builder.checkParameterEquals<2>({0x42, 18, 0x42, 20, 0x42, 22, 0x42, 24, 0x42, 26, 0x42, 18, 0x42, 20, 0x42, 22,
            0x42, 24, 0x42, 26, 0x42, 18, 0x42, 20, 0x42, 22, 0x42, 24, 0x42, 26});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "phi_read_address", test_conditional_address_cl_string, "test_phi_read_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(5, 1, 1, 2, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.setParameter<1>(toRange(100, 130));
        builder.allocateParameter<2>(30, 0x42);
        builder.setParameter<3>({3});
        builder.checkParameterEquals<2>({17, 118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29, 130, 31, 132, 33,
            134, 35, 136, 37, 138, 39, 140, 41, 142, 43, 144, 45, 146});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "phi_read_write_address", test_conditional_address_cl_string, "test_phi_read_write_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(5, 1, 1, 2, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.setParameter<1>(toRange(100, 130));
        builder.setParameter<2>({3});
        builder.checkParameterEquals<0>({117, 1, 119, 3, 121, 5, 123, 7, 125, 9, 127, 11, 129, 13, 131, 15, 133, 17,
            135, 19, 137, 21, 139, 23, 141, 25, 143, 27, 145, 29});
        builder.checkParameterEquals<1>({100, 18, 102, 20, 104, 22, 106, 24, 108, 26, 110, 28, 112, 30, 114, 32, 116,
            34, 118, 36, 120, 38, 122, 40, 124, 42, 126, 44, 128, 46});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "phi_copy_address", test_conditional_address_cl_string, "test_phi_copy_address_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(5, 1, 1, 2, 1, 1);
        builder.setParameter<0>(toRange(0, 30));
        builder.setParameter<1>(toRange(100, 130));
        builder.setParameter<2>({3});
        builder.checkParameterEquals<0>({100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17,
            118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29});
        builder.checkParameterEquals<1>({100, 1, 102, 3, 104, 5, 106, 7, 108, 9, 110, 11, 112, 13, 114, 15, 116, 17,
            118, 19, 120, 21, 122, 23, 124, 25, 126, 27, 128, 29});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_read_address_local", test_conditional_address_cl_string, "test_select_read_address_local");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 3, 1, 1);
        builder.allocateParameter<0>(30, 0x42);
        builder.allocateParameter<1>(30, 0x42);
        builder.checkParameterEquals<1>(toRange(0 + 17, 30 + 17));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "select_read_address_private", test_conditional_address_cl_string, "test_select_read_address_private");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(8, 1, 1, 2, 1, 1);
        builder.setParameter<0>(toRange(0, 16));
        builder.allocateParameter<1>(16, 0x42);
        builder.checkParameterEquals<1>({17, 18, 19, 20, 21, 22, 23, 24, 59, 34, 59, 34, 59, 34, 59, 34});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint16_t>, Buffer<uint8_t>, uint32_t> builder(
            "constant_load", test_constant_load_cl_string, "test_constant_load");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(2);
        builder.allocateParameter<1>(2);
        builder.allocateParameter<2>(2);
        builder.setParameter<3>(0);
        builder.checkParameterEquals<0>({0x42, 0x17 + 42});
        builder.checkParameterEquals<1>({0x42, 0x17 + 42});
        builder.checkParameterEquals<2>({0x42, 0x17 + 42});
    }

    {
        TestDataBuilder<int32_t, Buffer<int32_t>> builder("global_data", test_other_cl_string, "test_global_data");
        builder.setFlags(DataFilter::MEMORY_ACCESS | DataFilter::ASYNC_BARRIER);
        builder.setParameter<0>(1);
        builder.allocateParameter<1>(2);
        builder.checkParameterEquals<1>({2, 21});
    }

    {
        TestDataBuilder<Buffer<uint8_t>> builder(
            "constant_storage_small", local_private_storage_cl_string, "test_constant_storage");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(12);
        builder.allocateParameter<0>(12, 0x42);
        builder.checkParameterEquals<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '\0'});
    }

    {
        TestDataBuilder<Buffer<uint8_t>> builder(
            "constant_storage_large", local_private_storage_cl_string, "test_constant_storage2");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(12);
        builder.allocateParameter<0>(12, 0x42);
        builder.checkParameterEquals<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "local_storage", local_private_storage_cl_string, "test_local_storage");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(12, 1, 1, 2, 1, 1);
        builder.allocateParameter<0>(24, 7);
        builder.allocateParameter<1>(24, 0x42);
        builder.checkParameterMatches<1>(24, checkIsMultipleOf<uint32_t, 7u>, "7 divides value");
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "private_storage", local_private_storage_cl_string, "test_private_storage");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(12, 1, 1, 2, 1, 1);
        builder.allocateParameter<0>(24, 7);
        builder.allocateParameter<1>(24, 0x42);
        builder.checkParameterEquals<1>(
            {14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28});
    }

    {
        TestDataBuilder<Buffer<uint8_t>> builder(
            "register_storage", local_private_storage_cl_string, "test_register_storage");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(12);
        builder.allocateParameter<0>(12, 0x42);
        builder.checkParameterEquals<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '\0'});
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>> builder(
            "storage_private_char", test_storage_cl_string, "test", "-DTYPE=char -DSTORAGE=__private");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int8_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int8_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>> builder(
            "storage_local_char", test_storage_cl_string, "test", "-DTYPE=char -DSTORAGE=__local");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int8_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int8_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>,
            Buffer<int8_t>, Buffer<int8_t>>
            builder("storage_global_char", test_storage_cl_string, "test", "-DTYPE=char");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int8_t>(-6, 10));
        builder.allocateParameter<2>(16);
        builder.allocateParameter<3>(16);
        builder.allocateParameter<4>(16);
        builder.allocateParameter<5>(16);
        builder.allocateParameter<6>(16);
        builder.allocateParameter<7>(16);
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int8_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>> builder(
            "storage_private_short", test_storage_cl_string, "test", "-DTYPE=short -DSTORAGE=__private");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int16_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int16_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>> builder(
            "storage_local_short", test_storage_cl_string, "test", "-DTYPE=short -DSTORAGE=__local");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int16_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int16_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>,
            Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>>
            builder("storage_global_short", test_storage_cl_string, "test", "-DTYPE=short");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int16_t>(-6, 10));
        builder.allocateParameter<2>(16);
        builder.allocateParameter<3>(16);
        builder.allocateParameter<4>(16);
        builder.allocateParameter<5>(16);
        builder.allocateParameter<6>(16);
        builder.allocateParameter<7>(16);
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int16_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "storage_private_int", test_storage_cl_string, "test", "-DTYPE=int -DSTORAGE=__private");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int32_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int32_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "storage_local_int", test_storage_cl_string, "test", "-DTYPE=int -DSTORAGE=__local");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int32_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int32_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>,
            Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>>
            builder("storage_global_int", test_storage_cl_string, "test", "-DTYPE=int");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int32_t>(-6, 10));
        builder.allocateParameter<2>(16);
        builder.allocateParameter<3>(16);
        builder.allocateParameter<4>(16);
        builder.allocateParameter<5>(16);
        builder.allocateParameter<6>(16);
        builder.allocateParameter<7>(16);
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int32_t>(-6, 10)));
    }

    {
        // TODO result mismatch on actual hardware
        // Got 0x0000'000E'0000'000F instead of 0x0F for 0st element, rest is correct (independent of initial values of
        // result buffer)
        // TODO code generated on RPi and host is mostly identical -> buffer/parameter or code + emulator error
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>> builder(
            "storage_private_long", test_storage_cl_string, "test", "-DTYPE=long -DSTORAGE=__private");
        builder.setFlags(DataFilter::MEMORY_ACCESS | DataFilter::USES_LONG | DataFilter::DISABLED);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int64_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int64_t>(-6, 10)));
    }

    {
        // TODO hangs/takes long in Normalization step! Is long/a lot in LongOperations.cpp:431
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>> builder(
            "storage_local_long", test_storage_cl_string, "test", "-DTYPE=long -DSTORAGE=__local");
        builder.setFlags(DataFilter::MEMORY_ACCESS | DataFilter::USES_LONG | DataFilter::DISABLED);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(toRange<int64_t>(-6, 10));
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int64_t>(-6, 10)));
    }

    {
        // TODO result mismatch on actual hardware, all other values are wrong too
        // Got 0x0000'0011'0000'000F instead of 0xF for first element, also is off by +17 for remainder elements
        // (independent of initial values of buffers)
        // TODO code generated on RPi and host is mostly identical -> buffer/parameter or code + emulator error
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>,
            Buffer<int64_t>, Buffer<int64_t>, Buffer<int64_t>>
            builder("storage_global_long", test_storage_cl_string, "test", "-DTYPE=long");
        builder.setFlags(DataFilter::MEMORY_ACCESS | DataFilter::USES_LONG);
        builder.allocateParameter<0>(16, 0x40);
        builder.setParameter<1>(toRange<int64_t>(-6, 10));
        builder.allocateParameter<2>(16, 0x41);
        builder.allocateParameter<3>(16, 0x42);
        builder.allocateParameter<4>(16, 0x43);
        builder.allocateParameter<5>(16, 0x44);
        builder.allocateParameter<6>(16, 0x45);
        builder.allocateParameter<7>(16, 0x46);
        builder.checkParameterEquals<0>(toStorageTestResult(toRange<int64_t>(-6, 10)));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "global_parameter_unaligned", unaligned_memory_access_cl_string, "test_global");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 32u));
        builder.setParameter<2>({0, 1, 2, 3, 11});
        builder.allocateParameter<3>(2 * 16);
        builder.checkParameterEquals<0>({0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
            0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "local_parameter_unaligned", unaligned_memory_access_cl_string, "test_local_parameter");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 32u));
        builder.setParameter<2>({0, 1, 2, 3, 11});
        builder.allocateParameter<3>(2 * 16);
        builder.checkParameterEquals<0>({0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
            0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "local_vpm_full_row_unaligned", unaligned_memory_access_cl_string, "test_local_vpm_full_row");
        builder.setFlags(DataFilter::MEMORY_ACCESS | DataFilter::DISABLED);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 32u));
        builder.setParameter<2>({0, 1, 2, 3, 11});
        builder.allocateParameter<3>(2 * 16);
        builder.checkParameterEquals<0>({0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
            0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "local_vpm_partial_row_unaligned", unaligned_memory_access_cl_string, "test_local_vpm_partial_row");
        builder.setFlags(DataFilter::MEMORY_ACCESS | DataFilter::DISABLED);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 32u));
        builder.setParameter<2>({0, 1, 2, 3, 11});
        builder.checkParameterEquals<0>({0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
            0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "private_register_unaligned", unaligned_memory_access_cl_string, "test_private_register");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 16u));
        builder.setParameter<2>(toRange(0u, 5u));
        builder.checkParameterEquals<0>(
            {0x0, 0x1, 0xc, 0xd, 0xe, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa, 0xc, 0xd, 0xe, 0xf, 0x0, 0x8, 0x9, 0xa, 0xc, 0xd,
                0xe, 0xf, 0x0, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa, 0xc, 0xd, 0xd, 0xe, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "private_vpm_full_row_unaligned", unaligned_memory_access_cl_string, "test_private_vpm_full_row");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 32u));
        builder.setParameter<2>({0, 1, 2, 3, 11});
        builder.checkParameterEquals<0>({0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
            0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "private_vpm_partial_row_unaligned", unaligned_memory_access_cl_string, "test_private_vpm_partial_row");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.allocateParameter<0>(5 * 8, 0x42);
        builder.setParameter<1>(toRange(0u, 32u));
        builder.setParameter<2>({0, 1, 2, 3, 11});
        builder.checkParameterEquals<0>({0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
            0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
            0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder("vpm_read", test_vpm_read_cl_string, "test_vpm_read");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setParameter<0>(toRange(0, 4 * 10));
        builder.setParameter<1>(toRange(100, 100 + 4 * 10 * 3));
        builder.checkParameterEquals<0>(
            {100, 101, 102, 103, 112, 113, 114, 115, 124, 125, 126, 127, 136, 137, 138, 139, 148, 149, 150, 151, 160,
                161, 162, 163, 172, 173, 174, 175, 184, 185, 186, 187, 196, 197, 198, 199, 208, 209, 210, 211});
        builder.checkParameterPartialEquals<1>(
            {100, 101, 102, 103, 112, 113, 114, 115, 124, 125, 126, 127, 136, 137, 138, 139, 148, 149, 150, 151, 160,
                161, 162, 163, 172, 173, 174, 175, 184, 185, 186, 187, 196, 197, 198, 199});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int16_t>, Buffer<int8_t>, Buffer<int32_t>> builder(
            "vpm_write", test_vpm_write_cl_string, "test_vpm_write");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setParameter<0>(toRange(0, 16));
        builder.allocateParameter<1>(10 * 16);
        builder.allocateParameter<2>(10 * 16);
        builder.allocateParameter<3>(10 * 16);
        builder.allocateParameter<4>(10 * 16 * 3, 0x42);
        builder.checkParameterEquals<1>({
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
        });
        builder.checkParameterEquals<2>({
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
        });
        builder.checkParameterEquals<3>({
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
        });
        builder.checkParameterEquals<4>({
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
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, // 10. iteration
            0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
            0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, //
        });
    }

    {
        TestDataBuilder<Buffer<uint32_t>> builder(
            "cross_group_memory_access_simple", test_cross_group_access_cl_string, "test_cross_write_simple");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 4, 1, 1);
        builder.allocateParameter<0>(40, 0x42);
        builder.checkParameterEquals<0>({45, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, // sum([0, 9])
            145, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                             // sum([10, 19])
            245, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                             //
            345, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "cross_group_memory_access_advanced", test_cross_group_access_cl_string, "test_cross_write_advanced");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(10, 1, 1, 4, 1, 1);
        builder.allocateParameter<0>(40, 0x42);
        builder.allocateParameter<1>(16, 0x42);
        builder.checkParameterEquals<0>({28, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, // sum([0, 7])
            108, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                             // sum([10, 17])
            188, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,                             //
            268, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42});
    }

    registerPrivateAliasingTests<int8_t>("char");
    registerPrivateAliasingTests<int16_t>("short");
    registerPrivateAliasingTests<int32_t>("int");
}
