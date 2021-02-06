/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestEntries.h"

#include "test_files.h"

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
}
