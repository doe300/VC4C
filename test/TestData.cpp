/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestData.h"

#include "TestEntries.h"
#include "test_files.h"

#include <regex>
#include <stdexcept>

using namespace test_data;
using namespace test_files;

Result test_data::RESULT_OK{true, ""};

TestRunner::~TestRunner() noexcept = default;

static Result checkPiSum(const std::vector<float>& result)
{
    auto tmp = std::accumulate(result.begin(), result.end(), 0.0f) / 1024.0f;
    return checkScalarEqualsUlp(static_cast<float>(M_PI), tmp, 8, " as total sum");
}

static std::map<std::string, TestData> ALL_TESTS{};

void test_data::registerTest(TestData&& data)
{
    auto key = data.uniqueName;
    auto wasInserted = ALL_TESTS.emplace(key, std::move(data)).second;
    if(!wasInserted)
        throw std::runtime_error{"TestData key is not unique: " + key};
}

void test_data::registerGeneralTests()
{
    ////
    // Examples
    ////

    registerTest(TestData{"fibonacci", DataFilter::NONE, &fibonacci_cl_string, "", "fibonacci",
        {toScalarParameter(1u), toScalarParameter(1u), toBufferParameter(std::vector<uint32_t>(10))}, toDimensions(1),
        {checkParameterEquals(2, std::vector<uint32_t>{2, 3, 5, 8, 13, 21, 34, 55, 89, 144})}});

    registerTest(TestData{"hello_world", DataFilter::NONE, &hello_world_cl_string, "", "hello_world",
        {toBufferParameter(std::vector<uint8_t>(8 * 16))}, toDimensions(8),
        {checkParameterEquals(0,
            std::vector<uint8_t>{
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 1st work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 2nd work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 3rd work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 4th work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 5th work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 6th work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 7th work-item
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 8th work-item
            })}});

    registerTest(TestData{"hello_world_constant", DataFilter::NONE, &hello_world_constant_cl_string, "", "hello_world",
        {toBufferParameter(std::vector<uint8_t>(16, 0x42))}, toDimensions(12),
        {checkParameterEquals(0,
            std::vector<uint8_t>{
                'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', 0x42, 0x42, 0x42, 0x42})}});

    registerTest(TestData{"hello_world_vector", DataFilter::NONE, &hello_world_vector_cl_string, "", "hello_world",
        {toBufferParameter(
             std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0'}),
            toBufferParameter(std::vector<uint8_t>(16))},
        toDimensions(1),
        {checkParameterEquals(0,
             std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0'}),
            checkParameterEquals(1,
                std::vector<uint8_t>{
                    'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0'})}});

    registerTest(TestData{"llvm_ir", DataFilter::NONE, &test_cl_string, "", "test_llvm_ir",
        {toBufferParameter(std::vector<uint32_t>(10))}, toDimensions(1),
        {checkParameterEquals(0, std::vector<uint32_t>{142})}});

    registerTest(TestData{"instructions", DataFilter::INT_ARITHMETIC | DataFilter::FLOAT_ARITHMETIC,
        &test_instructions_cl_string, "", "test_instructions",
        {toScalarParameter(2u), toScalarParameter(4u), toScalarParameter(2.0f), toScalarParameter(4.0f),
            toBufferParameter(std::vector<uint32_t>(32)), toBufferParameter(std::vector<uint32_t>(32))},
        toDimensions(1),
        {checkParameterEquals(4,
             std::vector<int32_t>{
                 6, -2, 8, 0, 2, 4, 2, 2, 32, 0, 0, 6, 6, -3, 30, 3, 3, 1, 1, 0, 0, 0, 1, 1, 1, 0, 4, 8}),
            checkParameter<CompareULP<1>>(5, std::vector<float>{6.0f, -2.0f, 8.0f, 0.5f, 4.0f, 2.0f, 2.0f})}});

    registerTest(TestData{"SHA1", DataFilter::DISABLED | DataFilter::COMPLEX_KERNEL, &md5_cl_string, "",
        "sha1_crypt_kernel",
        {// parameter 0 is the control data
            toBufferParameter(std::vector<uint32_t>{0 /* padding*/, 1 /* number of keys */}),
            // parameter 1 is the salt, set to zero
            toBufferParameter(std::vector<uint32_t>(24, 0x0)),
            // parameter 2 is the "plain_key", the input TODO might need to invert the bytes in a word!
            toBufferParameter(std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'}),
            // parameter 3 is the digest
            toBufferParameter(std::vector<uint32_t>(8))},
        toDimensions(1),
        {checkParameterEquals(3, std::vector<uint32_t>{0x2ef7bde6, 0x08ce5404, 0xe97d5f04, 0x2f95f89f, 0x1c232871})}});

    registerTest(TestData{"SHA256", DataFilter::DISABLED | DataFilter::COMPLEX_KERNEL, &SHA_256_cl_string, "",
        "execute_sha256_cpu",
        {// parameter 0 is the input with a block-size of 16 words
            toBufferParameter(std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '1', '1',
                '1', '1', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}),
            // parameter 1 is the digest
            toBufferParameter(std::vector<uint32_t>(128)),
            // parameter 2 is the stride
            toScalarParameter(0)},
        toDimensions(1),
        {checkParameterEquals(1,
            std::vector<uint32_t>{
                0xf90a1ef4, 0x422350ca, 0x8c448530, 0xa7d5d0b2, 0x35054803, 0xf7b2a73d, 0x86f4b639, 0x4b1329a5})}});

    registerTest(TestData{"prime_found", DataFilter::NONE, &test_prime_cl_string, "", "test_prime",
        {toScalarParameter(17u), toBufferParameter(std::vector<uint32_t>(1))}, toDimensions(1),
        {checkParameterEquals(1, std::vector<uint32_t>{true})}});
    registerTest(TestData{"prime_no_prime", DataFilter::NONE, &test_prime_cl_string, "", "test_prime",
        {toScalarParameter(18u), toBufferParameter(std::vector<uint32_t>(1))}, toDimensions(1),
        {checkParameterEquals(1, std::vector<uint32_t>{false})}});

    ////
    // General Tests
    ////

    registerTest(TestData{"async_copy", DataFilter::ASYNC_BARRIER, &test_async_copy_cl_string, "", "test_async_copy",
        {toBufferParameter(toRange<unsigned>(0, 12 * 16)), toBufferParameter(std::vector<unsigned>(12 * 16)),
            toBufferParameter(std::vector<unsigned>(12 * 16))},
        toDimensions(12),
        {/* the __local arg might be lowered to VPM */ checkParameterEquals(2, toRange<unsigned>(0, 12 * 16))}});

    registerTest(TestData{"async_copy_general", DataFilter::ASYNC_BARRIER, &test_async_copy_cl_string, "",
        "test_async_copy_general",
        {toBufferParameter(toRange<unsigned>(0, 7 * 16)), toBufferParameter(std::vector<unsigned>(7 * 16)),
            toBufferParameter(std::vector<unsigned>(7 * 16))},
        toDimensions(7),
        {/* the __local arg might be lowered to VPM */ checkParameterEquals(2, toRange<unsigned>(0, 7 * 16))}});

    registerTest(TestData{"barrier_dynamic_work_size", DataFilter::ASYNC_BARRIER, &test_barrier_cl_string, "",
        "test_barrier", {toBufferParameter(std::vector<uint32_t>(12 * 8 * 2, 0x42))}, toDimensions(8, 1, 1, 2),
        {checkParameterEquals(0,
            std::vector<uint32_t>{
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 1st work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 2nd work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 3rd work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 4th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 5th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 6th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 7th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 8th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 9th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 10th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 11th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 12th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 13th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 14th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 15th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 16th work-item
            })}});

    registerTest(TestData{"barrier_fix_work_size", DataFilter::ASYNC_BARRIER, &test_barrier_cl_string, "-DFIXED_SIZE=1",
        "test_barrier", {toBufferParameter(std::vector<uint32_t>(12 * 8 * 2, 0x42))}, toDimensions(8, 1, 1, 2),
        {checkParameterEquals(0,
            std::vector<uint32_t>{
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 1st work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 2nd work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 3rd work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 4th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 5th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 6th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 7th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 8th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 9th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 10th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 11th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 12th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 13th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 14th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 15th work-item
                0, 1, 2, 0x42, 4, 5, 6, 7, 8, 9, 10, 0x42, // 16th work-item
            })}});

    registerTest(TestData{"branches", DataFilter::CONTROL_FLOW, &test_branches_cl_string, "", "test_branches",
        {toBufferParameter(std::vector<uint32_t>{512}), toBufferParameter(std::vector<uint32_t>(16, 0x42))},
        toDimensions(1), {checkParameterEquals(1, std::vector<uint32_t>{109, 1849, 512, 100, 100, 512, 0x42, 109})}});

    registerTest(TestData{"CRC16", DataFilter::COMPLEX_KERNEL, &test_hashes_cl_string, "", "crc16",
        {// output half-word
            toBufferParameter(std::vector<uint16_t>(1)),
            // data
            toBufferParameter(std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'}),
            // data size
            toScalarParameter(12)},
        // algorithm is CRC-16/ARC
        toDimensions(1), {checkParameterEquals(0, std::vector<uint16_t>{0x57BE})}});

    registerTest(TestData{"Pearson16", DataFilter::COMPLEX_KERNEL, &test_hashes_cl_string, "", "Pearson16",
        {// data
            toBufferParameter(std::vector<uint8_t>{'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'}),
            // data size
            toScalarParameter(12),
            // 8 byte result
            toBufferParameter(std::vector<uint8_t>(8))},
        toDimensions(1), {checkParameterEquals(2, std::vector<uint8_t>{92, 148, 236, 199, 49, 126, 212, 250})}});

    registerTest(TestData{"atomics", DataFilter::ATOMIC_FUNCTIONS, &test_other_cl_string, "", "test_atomics",
        {toBufferParameter(toRange<int32_t>(1, 12)), toBufferParameter(toRange<int32_t>(1, 12))}, toDimensions(1),
        {checkParameterEquals(1, std::vector<uint32_t>{2, 0, 3, 5, 4, 6, 7, 8, 9, 10, 0})}});

    // TODO result mismatch (emulator and hardware) for element 19 (rint(-1.5), expected -2, got -1)
    registerTest(TestData{"f2i", DataFilter::DISABLED, &test_other_cl_string, "", "test_f2i",
        {toScalarParameter(1.0f), toScalarParameter(1.1f), toScalarParameter(1.5f), toScalarParameter(1.9f),
            toBufferParameter(std::vector<int32_t>(32))},
        toDimensions(1),
        {checkParameterEquals(4,
            std::vector<int32_t>{1, 1, 1, 1, -1, -1, -1, -1, 1, -1, 2, -1, 1, -1, 1, -2, 1, -1, 2, -2, 1, 1, 2, 2, -1,
                -1, -2, -2, 1, -1})}});

    registerTest(TestData{"sfu", DataFilter::NONE, &test_sfu_cl_string, "", "test_sfu",
        {toBufferParameter(std::vector<float>{1.0f, 2.0f, 8.0f, 32.0f, 128.0f, 25.70f, 11.1f, 10.240f, 1.5f, 2.7f, 9.0f,
             124.340f, 112.2334455f, 56.7f, 74.1f, 0.00001f}),
            toBufferParameter(std::vector<float>(4 * 16))},
        toDimensions(1),
        {// hardware passes with 1024 but fails with 512
            checkParameter<CompareULP<1024>>(1,
                std::vector<float>{// recip
                    1.0f / 1.0f, 1.0f / 2.0f, 1.0f / 8.0f, 1.0f / 32.0f, 1.0f / 128.0f, 1.0f / 25.7f, 1.0f / 11.1f,
                    1.0f / 10.240f, 1.0f / 1.5f, 1.0f / 2.7f, 1.0f / 9.0f, 1.0f / 124.34f, 1.0f / 112.2334455f,
                    1.0f / 56.7f, 1.0f / 74.1f, 1.0f / 0.00001f,
                    // rsqrt
                    1.0f / std::sqrt(1.0f), 1.0f / std::sqrt(2.0f), 1.0f / std::sqrt(8.0f), 1.0f / std::sqrt(32.0f),
                    1.0f / std::sqrt(128.0f), 1.0f / std::sqrt(25.7f), 1.0f / std::sqrt(11.1f),
                    1.0f / std::sqrt(10.240f), 1.0f / std::sqrt(1.5f), 1.0f / std::sqrt(2.7f), 1.0f / std::sqrt(9.0f),
                    1.0f / std::sqrt(124.34f), 1.0f / std::sqrt(112.2334455f), 1.0f / std::sqrt(56.7f),
                    1.0f / std::sqrt(74.1f), 1.0f / std::sqrt(0.00001f),
                    // exp2
                    std::exp2(1.0f), std::exp2(2.0f), std::exp2(8.0f), std::exp2(32.0f), std::exp2(128.0f),
                    std::exp2(25.70f), std::exp2(11.1f), std::exp2(10.240f), std::exp2(1.5f), std::exp2(2.7f),
                    std::exp2(9.0f), std::exp2(124.340f), std::exp2(112.2334455f), std::exp2(56.7f), std::exp2(74.1f),
                    std::exp2(0.00001f),
                    // log2
                    std::log2(1.0f), std::log2(2.0f), std::log2(8.0f), std::log2(32.0f), std::log2(128.0f),
                    std::log2(25.70f), std::log2(11.1f), std::log2(10.240f), std::log2(1.5f), std::log2(2.7f),
                    std::log2(9.0f), std::log2(124.340f), std::log2(112.2334455f), std::log2(56.7f), std::log2(74.1f),
                    std::log2(0.00001f)})}});

    registerTest(TestData{"shuffle", DataFilter::VECTOR_OPERATIONS, &test_shuffle_cl_string, "", "test_shuffle",
        {toBufferParameter(toRange<uint8_t>(0, 32)), toBufferParameter(std::vector<uint8_t>(10 * 16))}, toDimensions(1),
        {checkParameterEquals(1,
            std::vector<uint8_t>{
                0x07, 0x06, 0x04, 0x08, 0x01, 0x0c, 0x0d, 0x01, 0x00, 0x09, 0x0e, 0x0f, 0x04, 0x03, 0x08,
                0x06, // out[0]
                0x01, 0x07, 0x0b, 0x12, 0x15, 0x0f, 0x08, 0x09, 0x00, 0x13, 0x02, 0x01, 0x11, 0x0d, 0x07,
                0x08, // out[1]
                0x1a, 0x1b, 0x02, 0x10, 0x04, 0x19, 0x06, 0x17, 0x08, 0x09, 0x1c, 0x13, 0x1a, 0x0d, 0x0e,
                0x0f, // out[2]
                0x11, 0x01, 0x02, 0x10, 0x11, 0x01, 0x02, 0x10, 0x11, 0x01, 0x02, 0x10, 0x11, 0x01, 0x02,
                0x10, // out[3]
                0x00, 0x00, 0x00, 0x01, 0x11, 0x00, 0x00, 0x10, 0x00, 0x11, 0x01, 0x02, 0x02, 0x10, 0x00,
                0x00, // out[4]
                0x00, 0x00, 0x00, 0x00, 0x04, 0x04, 0x04, 0x04, 0x08, 0x08, 0x08, 0x08, 0x0c, 0x0c, 0x0c,
                0x0c, // out[5]
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, // out[6]
                0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
                0x0f, // out[7]
                0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
                0x09, // out[8]
                0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01,
                0x00, // out[9]
            })}});

    registerTest(
        TestData{"shuffle_upcast", DataFilter::VECTOR_OPERATIONS, &test_shuffle_cl_string, "", "test_shuffle_upcast",
            {toBufferParameter(std::vector<uint8_t>{0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6,
                 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7}),
                toBufferParameter(std::vector<uint8_t>{4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1,
                    2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3}),
                toBufferParameter(std::vector<uint8_t>(6 * 8))},
            toDimensions(1),
            {checkParameterEquals(2,
                std::vector<uint8_t>{
                    0x17, 0x17, 0x17, 0x17, 0x02, 0x17, 0x17, 0x17, // out[0]
                    0x42, 0x42, 0x42, 0x04, 0x42, 0x42, 0x42, 0x01, // out[1]
                    0x07, 0x13, 0x02, 0x13, 0x13, 0x03, 0x13, 0x13, // out[2]
                    0xFF, 0x05, 0xFF, 0x06, 0x01, 0xFF, 0xFF, 0x03, // out[3]
                    0x06, 0x05, 0x01, 0x03, 0x71, 0x71, 0x71, 0x71, // out[4]
                    0x03, 0x31, 0x31, 0x31, 0x31, 0x31, 0x31, 0x31, // out[5]
                })}});

    registerTest(TestData{"shuffle_sample", DataFilter::VECTOR_OPERATIONS, &test_shuffle_cl_string, "", "sample_test",
        {toBufferParameter(toRange<uint8_t>(0, 32)), toBufferParameter(std::vector<uint8_t>(3 * 32))}, toDimensions(1),
        {checkParameterEquals(1,
            std::vector<uint32_t>{0x01000000, 0x00020000, 0x03000000, 0x00040000, 0x00000005, 0x07000006, 0x00080000,
                0x00000009, 0x000B000A, 0x0000000C, 0x0E00000D, 0x000F0000, 0x11001000, 0x00000000, 0x13000012,
                0x15001400, 0x00160000, 0x17000000, 0x00000018, 0x001A0019, 0x001B0000, 0x001C0000, 0x0000001D,
                0x00001F1E})}});

    registerTest(TestData{"struct", DataFilter::TYPE_HANDLING, &test_struct_cl_string, "", "test_struct",
        {toBufferParameter(std::vector<uint32_t>(20)), toBufferParameter(std::vector<uint32_t>(20))}, toDimensions(1),
        {checkParameterEquals(1, std::vector<uint32_t>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42, 0})}});

    registerTest(TestData{"vector_arithmetic", DataFilter::NONE, &test_vector_cl_string, "", "test_arithm",
        {toScalarParameter(2.0f), toBufferParameter(toRange(1.0f, 17.0f)), toBufferParameter(std::vector<float>(16))},
        toDimensions(1), {checkParameterEquals(2, toRange(3.0f, 19.0f))}});

    registerTest(TestData{"copy_vector", DataFilter::NONE, &test_vector_cl_string, "", "test_copy",
        {toBufferParameter(toRange(1, 17)), toBufferParameter(std::vector<uint32_t>(32))}, toDimensions(1),
        {checkParameterEquals(1,
            std::vector<uint32_t>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                11, 12, 13, 14, 15, 16})}});

    registerTest(TestData{"vector_param", DataFilter::VECTOR_PARAM | DataFilter::USES_LONG, &test_vector_cl_string, "",
        "test_param",
        {toVectorParameter(toRange<uint8_t>(0, 16)), toVectorParameter(toRange<uint32_t>(0, 4)),
            toVectorParameter(toRange<uint64_t>(0, 2)), toBufferParameter(std::vector<uint32_t>(4))},
        toDimensions(1),
        {checkParameterEquals(3, std::vector<uint32_t>{0x03020100, 0x07060505, 0x0B0A090B, 0x0F0E0D0F})}});

    registerTest(TestData{"vectorization1", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test1",
        {toBufferParameter(std::vector<float>(1000)), toBufferParameter(std::vector<float>(1000))}, toDimensions(1),
        {checkParameterEquals(0, toRange(-0.0f, -14.0f, -1.0f)),
            checkParameterEquals(1, toRange(-0.0f, -14.0f, -1.0f))}});

    registerTest(TestData{"vectorization2", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test2",
        {toBufferParameter(toRange(1.0f, 10.0f)), toBufferParameter(toRange(1.0f, 10.0f)), toScalarParameter(7.0f),
            toScalarParameter(1u), toScalarParameter(6u)},
        toDimensions(1),
        {checkParameterEquals(0, std::vector<float>{1.0f, 18.0f, 30.0f, 44.0f, 60.0f, 78.0f, 7.0f, 8.0f, 9.0f})}});

    registerTest(TestData{"vectorization3", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test3",
        {toBufferParameter(toRange(1.0f, 801.0f)), toBufferParameter(toRange(1.0f, 801.0f)), toScalarParameter(7.0f)},
        toDimensions(1),
        {checkParameterEquals(0, std::vector<float>{8.0f, 18.0f, 30.0f, 44.0f, 60.0f, 78.0f, 98.0f, 120.0f, 144.0f})}});

    registerTest(TestData{"vectorization4", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test4",
        {toBufferParameter(toRange<int>(0, 1024)), toBufferParameter(std::vector<uint32_t>(1))}, toDimensions(1),
        {checkParameterEquals(1, std::vector<uint32_t>{528896})}});

    registerTest(TestData{"vectorization5", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test5",
        {toBufferParameter(std::vector<float>(1024))}, toDimensions(1),
        {checkParameterEquals(0, toRange(0.0f, 1024.0f))}});

    registerTest(TestData{"vectorization8", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test8",
        {toBufferParameter(toRange<int>(0, 1024)), toBufferParameter(toRange<int>(0, 4096))}, toDimensions(1),
        {checkParameterEquals(0, std::vector<uint32_t>{0, 5, 10, 15, 20, 25, 30, 35, 40})}});

    registerTest(TestData{"vectorization9", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test9",
        {toBufferParameter(toRange<int>(0, 4096)), toBufferParameter(toRange<int>(0, 4096))}, toDimensions(1),
        {checkParameterEquals(0, std::vector<uint32_t>{0, 1, 2, 3, 5, 5, 6, 7, 10, 9, 10, 11, 15, 13, 14, 15, 20})}});

    registerTest(TestData{"vectorization10", DataFilter::CONTROL_FLOW, &test_vectorization_cl_string, "", "test10",
        {toBufferParameter(toRange<int>(0, 1024)), toBufferParameter(toRange<int>(0, 1024))}, toDimensions(1),
        {checkParameterEquals(0, std::vector<uint32_t>{0, 1, 2, 3, 8, 5, 6, 7, 16, 9, 10, 11, 24})}});

    registerTest(TestData{"vectorization11", DataFilter::CONTROL_FLOW | DataFilter::TYPE_HANDLING,
        &test_vectorization_cl_string, "", "test11", {toBufferParameter(toRange<int>(0, 256))}, toDimensions(1),
        {checkParameterEquals(0, std::vector<uint32_t>{100, 100, 100, 100, 100, 100, 100, 100, 100})}});

    registerTest(TestData{"work_item", DataFilter::WORK_GROUP, &test_work_item_cl_string, "", "test_work_item",
        {toBufferParameter(std::vector<uint32_t>(24 * 8 * 4, 0x42))}, toDimensions(8, 1, 1, 4, 1, 1),
        {checkParameterEquals(0,
            std::vector<uint32_t>{
                3, 8 * 4, 1, 1, 0, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 0, 0, 0, 0x43, 0x42,  // work-item (0, 0)
                3, 8 * 4, 1, 1, 1, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 1, 0, 0, 0x43, 0x42,  // work-item (0, 1)
                3, 8 * 4, 1, 1, 2, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 2, 0, 0, 0x43, 0x42,  // work-item (0, 2)
                3, 8 * 4, 1, 1, 3, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 3, 0, 0, 0x43, 0x42,  // work-item (0, 3)
                3, 8 * 4, 1, 1, 4, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 4, 0, 0, 0x43, 0x42,  // work-item (0, 4)
                3, 8 * 4, 1, 1, 5, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 5, 0, 0, 0x43, 0x42,  // work-item (0, 5)
                3, 8 * 4, 1, 1, 6, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 6, 0, 0, 0x43, 0x42,  // work-item (0, 6)
                3, 8 * 4, 1, 1, 7, 0, 0, 0, 0, 0, 4, 1, 1, 0, 0, 0, 8, 1, 1, 7, 0, 0, 0x43, 0x42,  // work-item (0, 7)
                3, 8 * 4, 1, 1, 8, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 0, 0, 0, 0x43, 0x42,  // work-item (1, 0)
                3, 8 * 4, 1, 1, 9, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 1, 0, 0, 0x43, 0x42,  // work-item (1, 1)
                3, 8 * 4, 1, 1, 10, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 2, 0, 0, 0x43, 0x42, // work-item (1, 2)
                3, 8 * 4, 1, 1, 11, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 3, 0, 0, 0x43, 0x42, // work-item (1, 3)
                3, 8 * 4, 1, 1, 12, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 4, 0, 0, 0x43, 0x42, // work-item (1, 4)
                3, 8 * 4, 1, 1, 13, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 5, 0, 0, 0x43, 0x42, // work-item (1, 5)
                3, 8 * 4, 1, 1, 14, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 6, 0, 0, 0x43, 0x42, // work-item (1, 6)
                3, 8 * 4, 1, 1, 15, 0, 0, 0, 0, 0, 4, 1, 1, 1, 0, 0, 8, 1, 1, 7, 0, 0, 0x43, 0x42, // work-item (1, 7)
                3, 8 * 4, 1, 1, 16, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 0, 0, 0, 0x43, 0x42, // work-item (2, 0)
                3, 8 * 4, 1, 1, 17, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 1, 0, 0, 0x43, 0x42, // work-item (2, 1)
                3, 8 * 4, 1, 1, 18, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 2, 0, 0, 0x43, 0x42, // work-item (2, 2)
                3, 8 * 4, 1, 1, 19, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 3, 0, 0, 0x43, 0x42, // work-item (2, 3)
                3, 8 * 4, 1, 1, 20, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 4, 0, 0, 0x43, 0x42, // work-item (2, 4)
                3, 8 * 4, 1, 1, 21, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 5, 0, 0, 0x43, 0x42, // work-item (2, 5)
                3, 8 * 4, 1, 1, 22, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 6, 0, 0, 0x43, 0x42, // work-item (2, 6)
                3, 8 * 4, 1, 1, 23, 0, 0, 0, 0, 0, 4, 1, 1, 2, 0, 0, 8, 1, 1, 7, 0, 0, 0x43, 0x42, // work-item (2, 7)
                3, 8 * 4, 1, 1, 24, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 0, 0, 0, 0x43, 0x42, // work-item (3, 0)
                3, 8 * 4, 1, 1, 25, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 1, 0, 0, 0x43, 0x42, // work-item (3, 1)
                3, 8 * 4, 1, 1, 26, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 2, 0, 0, 0x43, 0x42, // work-item (3, 2)
                3, 8 * 4, 1, 1, 27, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 3, 0, 0, 0x43, 0x42, // work-item (3, 3)
                3, 8 * 4, 1, 1, 28, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 4, 0, 0, 0x43, 0x42, // work-item (3, 4)
                3, 8 * 4, 1, 1, 29, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 5, 0, 0, 0x43, 0x42, // work-item (3, 5)
                3, 8 * 4, 1, 1, 30, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 6, 0, 0, 0x43, 0x42, // work-item (3, 6)
                3, 8 * 4, 1, 1, 31, 0, 0, 0, 0, 0, 4, 1, 1, 3, 0, 0, 8, 1, 1, 7, 0, 0, 0x43, 0x42, // work-item (3, 7)
            })}});

    registerTest(TestData{"work_item_global_offset", DataFilter::WORK_GROUP, &test_work_item_cl_string, "",
        "test_work_item", {toBufferParameter(std::vector<uint32_t>(24 * 2 * 2, 0x42))},
        toDimensions(2, 1, 1, 2, 1, 1, 0, 2, 3),
        {checkParameterEquals(0,
            std::vector<uint32_t>{
                3, 2 * 2, 1, 1, 0, 2, 3, 0, 2, 3, 2, 1, 1, 0, 0, 0, 2, 1, 1, 0, 0, 0, 0x43, 0x42, // work-item (0, 0)
                3, 2 * 2, 1, 1, 1, 2, 3, 0, 2, 3, 2, 1, 1, 0, 0, 0, 2, 1, 1, 1, 0, 0, 0x43, 0x42, // work-item (0, 1)
                3, 2 * 2, 1, 1, 2, 2, 3, 0, 2, 3, 2, 1, 1, 1, 0, 0, 2, 1, 1, 0, 0, 0, 0x43, 0x42, // work-item (1, 0)
                3, 2 * 2, 1, 1, 3, 2, 3, 0, 2, 3, 2, 1, 1, 1, 0, 0, 2, 1, 1, 1, 0, 0, 0x43, 0x42, // work-item (1, 1)
            })}});

    ////
    // Bug Regression Tests
    ////
    // TODO add for all/most bugs/ files
    registerTest(TestData{"bug_local_memory_dot3", DataFilter::WORK_GROUP, &bugs_30_local_memory_cl_string, "", "dot3",
        {toBufferParameter(toRange<float>(0.0f, 20.0f)),
            toBufferParameter(std::vector<float>{0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f,
                0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f}),
            toBufferParameter(std::vector<float>(24)), toBufferParameter(std::vector<float>(16))},
        toDimensions(10, 1, 1, 2, 1, 1),
        {checkParameter<CompareULP<1>>(2,
            std::vector<float>{0.1f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 2.1f, 1.1f, 1.2f, 1.3f, 1.4f,
                1.5f, 1.6f, 1.7f, 1.8f, 1.9f})}});

    registerTest(TestData{"bug_local_memory_dot3_local", DataFilter::WORK_GROUP, &bugs_30_local_memory_cl_string, "",
        "dot3_local",
        {toBufferParameter(toRange<float>(0.0f, 20.0f)),
            toBufferParameter(std::vector<float>{0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f,
                0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f}),
            toBufferParameter(std::vector<float>(24))},
        toDimensions(10, 1, 1, 2, 1, 1),
        {checkParameter<CompareULP<1>>(2,
            std::vector<float>{0.1f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 2.1f, 1.1f, 1.2f, 1.3f, 1.4f,
                1.5f, 1.6f, 1.7f, 1.8f, 1.9f})}});

    registerTest(TestData{"bug_float_add_redundancy", DataFilter::NONE, &bugs_33_floating_point_folding_cl_string, "",
        "add_redundancy", {toBufferParameter(std::vector<float>{5.0f})}, toDimensions(1),
        {checkParameterEquals(0, std::vector<float>{5.0f})}});

    registerTest(TestData{"bug_float_mul_redundancy", DataFilter::NONE, &bugs_33_floating_point_folding_cl_string, "",
        "mul_redundancy", {toBufferParameter(std::vector<float>{5.0f})}, toDimensions(1),
        {checkParameterEquals(0, std::vector<float>{0.0f})}});

    registerTest(TestData{"bug_read_write_memory", DataFilter::NONE, &bugs_vc4cl_27_wrong_result_cl_string, "", "hello",
        {toBufferParameter(toRange<float>(-15.0f, 15.0f))}, toDimensions(10, 1, 1, 3, 1, 1),
        {checkParameterEquals(0, toRange<float>(-30.0f, 30.0f, 2.0f))}});

    registerTest(TestData{"bug_const_assign", DataFilter::NONE, &bugs_54_invalid_results_cl_string, "", "sum",
        {toBufferParameter(std::vector<float>(9))}, toDimensions(3, 1, 1, 3, 1, 1),
        {checkParameterEquals(0, std::vector<float>{1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f})}});

    ////
    // OpenCL CTS Tests
    ////

    // XXX passes for SPIR-V locally, but fails in CI
    registerTest(TestData{"OpenCL_CTS_async_copy", DataFilter::ASYNC_BARRIER | DataFilter::SPIRV_DISABLED,
        &OpenCL_CTS_async_copy_global_to_local_cl_string, "", "test_async_copy_global_to_local",
        {toBufferParameter(toRange<uint32_t>(0, 64)), toBufferParameter(std::vector<uint32_t>(64)),
            toBufferParameter(std::vector<uint32_t>(64)), toScalarParameter(64), toScalarParameter(8)},
        toDimensions(8), {checkParameterEquals(1, toRange<uint32_t>(0, 64))}});

    registerTest(
        TestData{"OpenCL_CTS_barrier", DataFilter::ASYNC_BARRIER, &OpenCL_CTS_barrier_cl_string, "", "compute_sum",
            {toBufferParameter(toRange<uint32_t>(0, 24)), toScalarParameter(23),
                toBufferParameter(std::vector<uint32_t>(16)), toBufferParameter(std::vector<uint32_t>(1))},
            toDimensions(11), {checkParameterEquals(3, std::vector<uint32_t>{253})}});

    registerTest(TestData{"OpenCL_CTS_clamp", DataFilter::NONE, &OpenCL_CTS_clamp_cl_string, "", "test_clamp",
        {toBufferParameter(std::vector<float>{17.0f, 0.0f, 3.0f}),
            toBufferParameter(std::vector<float>{1.0f, 1.0f, 1.0f}),
            toBufferParameter(std::vector<float>{5.0f, 5.0f, 5.0f}), toBufferParameter(std::vector<float>(3))},
        toDimensions(3), {checkParameterEquals(3, std::vector<float>{5.0f, 1.0f, 3.0f})}});

    registerTest(TestData{"OpenCL_CTS_cross", DataFilter::VECTOR_OPERATIONS | DataFilter::FLOAT_ARITHMETIC,
        &OpenCL_CTS_cross_product_cl_string, "", "test_cross",
        {toBufferParameter(std::vector<float>{1.0f, 2.0f, 3.0f}),
            toBufferParameter(std::vector<float>{3.0f, 4.0f, 5.0f}), toBufferParameter(std::vector<float>(3))},
        toDimensions(1), {checkParameterEquals(2, std::vector<float>{-2.0f, 4.0f, -2.0f})}});

    registerTest(TestData{"OpenCL_CTS_add_sat_int3", DataFilter::VECTOR_OPERATIONS | DataFilter::INT_ARITHMETIC,
        &OpenCL_CTS_integer_add_sat_cl_string, "", "test_add_sat_int3",
        {toBufferParameter(std::vector<int32_t>{std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
             std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), static_cast<int32_t>(0x97c4aa2f),
             static_cast<int32_t>(0xa91a356a)}),
            toBufferParameter(std::vector<int>{-1, 1, std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
                static_cast<int32_t>(0xa91a356a), static_cast<int32_t>(0xa91a356a)}),
            toBufferParameter(std::vector<int>(6))},
        toDimensions(2),
        {checkParameterEquals(2,
            std::vector<int>{std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
                std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min(),
                std::numeric_limits<int>::min()})}});

    registerTest(
        TestData{"OpenCL_CTS_local_kernel_scope", DataFilter::MEMORY_ACCESS, &OpenCL_CTS_local_kernel_scope_cl_string,
            "", "test", {toBufferParameter(toRange<uint32_t>(0, 64)), toBufferParameter(std::vector<uint32_t>(8))},
            toDimensions(8, 1, 1, 8, 1, 1),
            {checkParameterEquals(1, std::vector<uint32_t>{7, 15, 23, 31, 39, 47, 55, 63})}});

    registerTest(TestData{"OpenCL_CTS_min_max_constant_args", DataFilter::CORNER_CASES,
        &OpenCL_CTS_min_max_constant_args_cl_string, "", "sample_test",
        // has 64 parameters, 63 inputs and 1 output
        {toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)), toBufferParameter(toRange(0, 8)),
            toBufferParameter(std::vector<uint32_t>(8))},
        toDimensions(4, 1, 1, 2),
        // dest[gid] = sum(src0[gid], src1[gid], ..., src63[gid])
        {checkParameterEquals(
            63, std::vector<uint32_t>{0 * 63, 1 * 63, 2 * 63, 3 * 63, 4 * 63, 5 * 63, 6 * 63, 7 * 63})}});

    registerTest(TestData{"OpenCL_CTS_pointer_cast", DataFilter::CORNER_CASES, &OpenCL_CTS_pointer_cast_cl_string, "",
        "test_pointer_cast",
        {toBufferParameter(std::vector<uint8_t>{4, 3, 2, 1}), toBufferParameter(std::vector<uint32_t>(1))},
        toDimensions(1), {checkParameterEquals(1, std::vector<uint32_t>{0x01020304})}});

    registerTest(TestData{"OpenCL_CTS_sub_buffers_read", DataFilter::NONE, &OpenCL_CTS_sub_buffers_read_write_cl_string,
        "", "readTest",
        {toBufferParameter(std::vector<uint8_t>{4, 3, 2, 1, 8, 7, 6, 5}), toScalarParameter<int8_t>(0x10)},
        toDimensions(4, 1, 1, 2), {checkParameterEquals(0, std::vector<uint8_t>{20, 19, 18, 17, 24, 23, 22, 21})}});

    registerTest(TestData{"OpenCL_CTS_sub_sat", DataFilter::INT_ARITHMETIC, &OpenCL_CTS_sub_sat_cl_string, "",
        "test_sub_sat_int",
        {toBufferParameter(std::vector<int32_t>{std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
             std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), static_cast<int32_t>(0x8c7f0aac)}),
            toBufferParameter(std::vector<int32_t>{1, -1, std::numeric_limits<int>::max(),
                std::numeric_limits<int>::min(), static_cast<int32_t>(0x1902f8c8)}),
            toBufferParameter(std::vector<int32_t>(5))},
        toDimensions(5),
        {checkParameterEquals(2,
            std::vector<int32_t>{std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
                std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min()})}});

    registerTest(TestData{"OpenCL_CTS_uchar_compare", DataFilter::COMPARISONS, &OpenCL_CTS_uchar_compare_cl_string, "",
        "test_select",
        {toBufferParameter(std::vector<uint8_t>{4, 3, 2, 1}), toBufferParameter(std::vector<uint8_t>{1, 3, 2, 4}),
            toBufferParameter(std::vector<uint8_t>(4))},
        toDimensions(1), {checkParameterEquals(2, std::vector<uint8_t>{1, 3, 2, 4})}});

    ////
    // Boost Compute Tests
    ////

    registerTest(TestData{"boost_adjacent_find", DataFilter::NONE, &boost_compute_adjacent_find_cl_string, "",
        "serial_adjacent_find",
        {toScalarParameter(16u), toBufferParameter(std::vector<uint32_t>(2)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 11, 12, 13})},
        toDimensions(1), {checkParameterEquals(1, std::vector<uint32_t>{7})}});

    registerTest(TestData{"boost_adjacent_find_with_atomics", DataFilter::ATOMIC_FUNCTIONS,
        &boost_compute_adjacent_find_cl_string, "", "adjacent_find_with_atomics",
        {toBufferParameter(std::vector<uint32_t>{0xFFFFFFFFu}),
            toBufferParameter(std::vector<uint32_t>{0, 1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 11, 12, 13, 0})},
        toDimensions(8, 1, 1, 2, 1, 1), {checkParameterEquals(0, std::vector<uint32_t>{7})}});

    registerTest(TestData{"boost_functional_popcount_long", DataFilter::USES_LONG,
        &boost_compute_test_functional_popcount_cl_string, "", "copy",
        {toBufferParameter(std::vector<uint32_t>(7, 0x42)),
            toBufferParameter(
                std::vector<uint64_t>{0, 1, 17, 0x200000000001, 0x100000F00000, 0xFFFFFFFFFFFFFFFF, 0x34000000001}),
            toScalarParameter(7)},
        toDimensions(8), {checkParameterEquals(0, std::vector<uint32_t>{0, 1, 2, 2, 5, 64, 4})}});

    registerTest(TestData{"boost_functional_popcount_int", DataFilter::NONE,
        &boost_compute_test_functional_popcount_cl_string, "", "popcount_uint",
        {toBufferParameter(std::vector<uint32_t>(6, 0x42)),
            toBufferParameter(std::vector<uint32_t>{0, 1, 17, 0x20001, 0x100F000, 0xFFFFFFFF}), toScalarParameter(6)},
        toDimensions(8), {checkParameterEquals(0, std::vector<uint32_t>{0, 1, 2, 2, 5, 32})}});

    registerTest(TestData{"boost_initial_reduce",
        DataFilter::CONTROL_FLOW | DataFilter::ASYNC_BARRIER | DataFilter::USES_LONG,
        &boost_compute_initial_reduce_cl_string, "", "initial_reduce",
        {toScalarParameter(static_cast<uint32_t>(32 * sizeof(uint32_t))), toBufferParameter(std::vector<uint64_t>(2)),
            toBufferParameter(std::vector<uint32_t>{
                0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0})},
        toDimensions(8, 1, 1, 2, 1, 1), {checkParameterEquals(1, std::vector<uint32_t>{8, 0, 7, 0})}});

    // TODO has random result mismatch errors
    registerTest(
        TestData{"boost_insertion_sort", DataFilter::CONTROL_FLOW | DataFilter::USES_LONG | DataFilter::DISABLED,
            &boost_compute_test_insertion_sort_cl_string, "", "serial_insertion_sort",
            {toBufferParameter(std::vector<uint64_t>(16)), toScalarParameter(16u),
                toBufferParameter(std::vector<uint64_t>{1, 0, 2, 15, 14, 3, 11, 12, 4, 8, 7, 5, 10, 6, 9, 13})},
            toDimensions(1),
            {checkParameterEquals(2, std::vector<uint64_t>{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15})}});

    registerTest(TestData{"boost_insertion_sort_short", DataFilter::CONTROL_FLOW,
        &boost_compute_test_insertion_sort_cl_string, "", "serial_insertion_sort_short",
        {toBufferParameter(std::vector<uint32_t>(8)), toScalarParameter(16u),
            toBufferParameter(
                std::vector<uint16_t>{0x1, 0x0, 0x2, 0xF, 0x3, 0xE, 0xC, 0xB, 0x4, 0x8, 0x7, 0x5, 0xA, 0x6, 0x9, 0xD})},
        toDimensions(1),
        {checkParameterEquals(2,
            std::vector<uint16_t>{0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF})}});

    registerTest(TestData{"boost_serial_merge", DataFilter::CONTROL_FLOW | DataFilter::TYPE_HANDLING,
        &boost_compute_test_merge_cl_string, "", "serial_merge",
        {toScalarParameter(16u), toScalarParameter(16u),
            toBufferParameter(std::vector<uint32_t>{1, 0, 3, 0, 5, 0, 7, 0, 9, 0, 11, 0, 13, 0, 15, 0, 17, 0, 19, 0, 21,
                0, 23, 0, 25, 0, 27, 0, 29, 0, 31, 0}),
            toBufferParameter(std::vector<uint32_t>{0, 0, 2, 0, 4, 0, 6, 0, 8, 0, 10, 0, 12, 0, 14, 0, 16, 0, 18, 0, 20,
                0, 22, 0, 24, 0, 26, 0, 28, 0, 30, 0}),
            toBufferParameter(std::vector<uint32_t>(64))},
        toDimensions(1),
        {checkParameterEquals(4,
            std::vector<uint32_t>{0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0, 10, 0, 11, 0, 12, 0, 13,
                0, 14, 0, 15, 0, 16, 0, 17, 0, 18, 0, 19, 0, 20, 0, 21, 0, 22, 0, 23, 0, 24, 0, 25, 0, 26, 0, 27, 0, 28,
                0, 29, 0, 30, 0, 31, 0})}});

    registerTest(TestData{"boost_reduce", DataFilter::CONTROL_FLOW | DataFilter::ASYNC_BARRIER,
        &boost_compute_test_reduce_cl_string, "", "reduce",
        {toBufferParameter(std::vector<uint32_t>{1, 5, 9, 13, 17}), toScalarParameter(0u), toScalarParameter(5u),
            toBufferParameter(std::vector<uint32_t>(1)), toScalarParameter(0u)},
        toDimensions(8), {checkParameterEquals(3, std::vector<uint32_t>{1 + 5 + 9 + 13 + 17})}});

    ////
    // Application Tests
    ////

    // TODO fails on CI, but works locally...
    registerTest(
        TestData{"clNN_upscale", DataFilter::COMPLEX_KERNEL | DataFilter::INT_ARITHMETIC | DataFilter::WORK_GROUP,
            &clNN_SpatialUpSamplingNearest_cl_string, "", "upscale",
            {toBufferParameter(toRange<float>(0.0f, 8.0f)), toScalarParameter(0),
                toBufferParameter(std::vector<float>(24, 42.0f)), toScalarParameter(0), toScalarParameter(19),
                toScalarParameter(2), toScalarParameter(2), toScalarParameter(2), toScalarParameter(2)},
            toDimensions(8, 1, 1, 3, 1, 1),
            {checkParameterEquals(
                2, std::vector<float>{0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 42, 42, 42, 42, 42})}});

    registerTest(TestData{"single_element_struct", DataFilter::TYPE_HANDLING, &pocl_test_structs_as_args_cl_string, "",
        "test_single",
        {toVectorParameter(std::vector<uint32_t>{0x01000102}), toBufferParameter(std::vector<uint32_t>(1))},
        toDimensions(1), {checkParameterEquals(1, std::vector<uint32_t>{0x01000102})}});

    registerTest(
        TestData{"two_element_struct", DataFilter::TYPE_HANDLING, &pocl_test_structs_as_args_cl_string, "", "test_pair",
            {toVectorParameter(std::vector<uint32_t>{0x01010101, 0x23232323, 0x45454545, 0x67676767}),
                toBufferParameter(std::vector<uint32_t>(2))},
            toDimensions(1), {checkParameterEquals(1, std::vector<uint32_t>{0x01010101, 0x45454545})}});

    registerTest(TestData{"multi_element_struct", DataFilter::TYPE_HANDLING, &pocl_test_structs_as_args_cl_string, "",
        "test_kernel",
        {toVectorParameter(std::vector<uint32_t>{0x01001001, 0x02002002, 0x03003003, 0x04004004, 0x05005005, 0xDEADDEAD,
             0x06006006, 0x07007007, 0x48008008, 0x09009009, 0x0A00A00A, 0x0B00B00B}),
            toBufferParameter(std::vector<uint32_t>(10))},
        toDimensions(1),
        {checkParameterEquals(1,
            std::vector<uint32_t>{
                0x01001001, 0x02002002, 0x03003003, 0x05, 0x06006006, 131584, 0xFFFF9009, 0x0A00A00A, 48, 8})}});

    registerTest(TestData{"pi", DataFilter::NONE, &HandsOnOpenCL_pi_ocl_cl_string, "", "pi",
        {toScalarParameter(32), toScalarParameter(1.0f / 1024.0f), toBufferParameter(std::vector<uint32_t>(8)),
            toBufferParameter(std::vector<uint32_t>(4))},
        toDimensions(8, 1, 1, 4, 1, 1), {checkParameter<float>(3, 4, checkPiSum)}});

    registerTest(TestData{"mmul", DataFilter::CONTROL_FLOW, &HandsOnOpenCL_matmul_cl_string, "", "mmul",
        {toScalarParameter(4), toBufferParameter(std::vector<float>(4 * 4, 3.0f)),
            toBufferParameter(std::vector<float>(4 * 4, 5.0f)), toBufferParameter(std::vector<float>(4 * 4, 0.0f))},
        toDimensions(2, 2, 1, 2, 2, 1), {checkParameterEquals(3, std::vector<float>(4 * 4, 4 * 3.0f * 5.0f))}});

    // TODO requires# include headers
    registerTest(TestData{"histogram_1C",
        DataFilter::DISABLED | DataFilter::ATOMIC_FUNCTIONS | DataFilter::ASYNC_BARRIER | DataFilter::COMPLEX_KERNEL,
        &OpenCLIPP_Histogram_cl_string, "", "histogram_1C",
        {toBufferParameter(
             std::vector<uint8_t>{0x02, 0x01, 0x00, 0x01, 0x01, 0x01, 0x06, 0x01, 0x05, 0x04, 0x03, 0x02}),
            toBufferParameter(std::vector<uint32_t>(8)), toScalarParameter(4)},
        toDimensions(4, 3),
        /* the value is the count of bytes <= the position */
        {checkParameterEquals(1, std::vector<uint32_t>{1, 6, 8, 9, 10, 11, 12, 12})}});

    registerTest(TestData{"VectorAdd", DataFilter::WORK_GROUP, &NVIDIA_VectorAdd_cl_string, "", "VectorAdd",
        {toBufferParameter(toRange<float>(0.0f, 18.0f)), toBufferParameter(toRange<float>(0.0f, 18.0f)),
            toBufferParameter(std::vector<uint32_t>(20)), toScalarParameter(16u)},
        toDimensions(12, 1, 1, 2, 1, 1),
        {checkParameterEquals(2,
            std::vector<float>{0.0f, 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f, 20.0f, 22.0f, 24.0f,
                26.0f, 28.0f, 30.0f, 0.0f})}});

    registerTest(TestData{"deepCL_copy", DataFilter::NONE, &deepCL_copy_cl_string, "", "copy",
        {toScalarParameter(10u), toBufferParameter(std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12}),
            toBufferParameter(std::vector<float>(16))},
        toDimensions(12), {checkParameterEquals(2, std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 0, 0})}});

    registerTest(TestData{"deepCL_multiplyConstant", DataFilter::NONE, &deepCL_copy_cl_string, "", "multiplyConstant",
        {toScalarParameter(10u), toScalarParameter(5.0f),
            toBufferParameter(std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12}),
            toBufferParameter(std::vector<float>(16))},
        toDimensions(12),
        {checkParameterEquals(
            3, std::vector<float>{5 * 1, 5 * 9, 5 * -2, 5 * 8, 5 * 3, 5 * 7, 5 * 4, 5 * 6, 5 * 5, 5 * 0, 0, 0})}});

    registerTest(TestData{"deepCL_multiplyInplace", DataFilter::NONE, &deepCL_copy_cl_string, "", "multiplyInplace",
        {toScalarParameter(10u), toScalarParameter(5.0f),
            toBufferParameter(std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12})},
        toDimensions(12),
        {checkParameterEquals(
            2, std::vector<float>{5 * 1, 5 * 9, 5 * -2, 5 * 8, 5 * 3, 5 * 7, 5 * 4, 5 * 6, 5 * 5, 5 * 0, 11, 12})}});

    registerTest(TestData{"deepCL_array_inv", DataFilter::FLOAT_ARITHMETIC, &deepCL_inv_cl_string, "", "array_inv",
        {toScalarParameter(10u),
            toBufferParameter(std::vector<float>{1, 9, -2, 8, 0.3f, 7, 0.4f, 6, -5, 0.1f, 11, 12})},
        toDimensions(12),
        {checkParameter<CompareULP<1>>(1,
            std::vector<float>{1, 1.0f / 9.0f, 1.0f / -2.0f, 1.0f / 8.0f, 1.0f / 0.3f, 1.0f / 7.0f, 1.0f / 0.4f,
                1.0f / 6.0f, 1.0f / -5.0f, 1.0f / 0.1f, 11, 12})}});

    registerTest(TestData{"deepCL_memset", DataFilter::NONE, &deepCL_memset_cl_string, "", "cl_memset",
        {toBufferParameter(std::vector<float>(16)), toScalarParameter(17.0f), toScalarParameter(10)}, toDimensions(12),
        {checkParameterEquals(
            0, std::vector<float>{17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 0, 0, 0, 0})}});

    // TODO has result mismatch in sqrt()
    registerTest(TestData{"NearestNeighbor", DataFilter::DISABLED | DataFilter::FLOAT_ARITHMETIC,
        &rodinia_nearestNeighbor_kernel_cl_string, "", "NearestNeighbor",
        {toBufferParameter(std::vector<float>{0, 0, 1, 1, 0, 1, 1, 0, -1, -1, -1, 0, 0, -1, 1, -1, -1, 1}),
            toBufferParameter(std::vector<float>(16)), toScalarParameter(9), toScalarParameter(0.5f),
            toScalarParameter(-0.5f)},
        toDimensions(12),
        {checkParameterEquals(0,
            std::vector<float>{
                0.707107f, 1.58114f, 1.58114f, 0.707107f, 1.58114f, 1.58114f, 0.707107f, 0.707107f, 2.12132f})}});

    // TODO has some ULP errors near zero
    registerTest(TestData{"BabelStream_mul", DataFilter::DISABLED, &BabelStream_OCLStream_cl_string, "", "mul",
        {toBufferParameter(std::vector<float>(16)), toBufferParameter(toRange<float>(-4, 4, 0.5f))},
        toDimensions(4, 1, 1, 4, 1, 1), {checkParameter<CompareULP<2>>(0, toRange<float>(-1.6f, 1.6f, 0.2f))}});

    registerTest(TestData{"BabelStream_add", DataFilter::NONE, &BabelStream_OCLStream_cl_string, "", "add",
        {toBufferParameter(std::vector<float>(16, 17.0f)), toBufferParameter(toRange<float>(-4, 4, 0.5f)),
            toBufferParameter(std::vector<float>(16))},
        toDimensions(4, 1, 1, 4, 1, 1), {checkParameterEquals(2, toRange<float>(13, 21, 0.5f))}});

    // TODO result error (2 ULP) on actual hardware
    registerTest(TestData{"BabelStream_triad", DataFilter::NONE, &BabelStream_OCLStream_cl_string, "", "triad",
        {toBufferParameter(std::vector<float>(16)), toBufferParameter(toRange<float>(-4, 4, 0.5f)),
            toBufferParameter(std::vector<float>(16, 17.0f))},
        toDimensions(4, 1, 1, 4, 1, 1),
        {checkParameter<CompareULP<1>>(0, toRange<float>(-4 + 0.4f * 17, 4 + 0.4f * 17, 0.5f))}});

    // TODO has some result mismatch errors, expected result could also be wrong?
    registerTest(TestData{"BabelStream_dot", DataFilter::DISABLED | DataFilter::CONTROL_FLOW,
        &BabelStream_OCLStream_cl_string, "", "stream_dot",
        {toBufferParameter(toRange<float>(0, 32)), toBufferParameter(std::vector<float>(32, 17.0f)),
            toBufferParameter(std::vector<float>(4)), toBufferParameter(std::vector<float>(4)), toScalarParameter(32)},
        toDimensions(4, 1, 1, 4, 1, 1),
        // lid 0 calculates a[0] * b[0] + a[16] * b [16]= 0 * 17 + 16 * 17 = 272
        // group 0 calculates lid 0 + ... + lid 3
        {checkParameterEquals(3,
            std::vector<float>{
                272 + 306 + 340 + 374, 408 + 442 + 476 + 510, 544 + 578 + 612 + 646, 680 + 714 + 748 + 782})}});
};

static void initializeTests()
{
    if(ALL_TESTS.empty())
    {
        registerGeneralTests();
        registerArithmeticTests();
        registerOpenCLCommonFunctionTests();
        registerOpenCLGeometricFunctionTests();
        registerOpenCLIntegerFunctionTests();
        registerOpenCLRelationalFunctionTests();
        registerMemoryTests();
        registerTypeConversionTests();
        registerVectorTests();
    }
}

std::map<std::string, const TestData*> test_data::getAllTests(DataFilter exclusionFilter)
{
    initializeTests();
    std::map<std::string, const TestData*> result;
    for(const auto& test : ALL_TESTS)
    {
        if((test.second.filter & exclusionFilter) != DataFilter::NONE)
            continue;
        result.emplace(test.first, &test.second);
    }
    return result;
}

const TestData* test_data::getTest(const std::string& name)
{
    initializeTests();
    auto it = ALL_TESTS.find(name);
    if(it != ALL_TESTS.end())
        return &it->second;
    return nullptr;
}

static std::regex createRegex(std::string pattern)
{
    if(!pattern.empty() && (pattern[0] == '"' || pattern[0] == '\''))
        pattern = pattern.substr(1, pattern.find_last_not_of(pattern[0] == '"' ? '"' : '\'') - 1);

    return std::regex{pattern, std::regex::basic};
}

bool test_data::parseTestDataParameter(const std::string& param, std::vector<std::string>& enabledTests)
{
    // TODO add support for data filter flags + add everything to --help
    // TODO add flag to list all tests + their flags they are added to + list all flags
    if(getTest(param))
    {
        enabledTests.emplace_back(param);
        return true;
    }

    // check whether parameter is regex and try to match strings
    if(param.find("--test-pattern=") == 0)
    {
        std::regex pattern = createRegex(param.substr(param.find('=') + 1));
        for(const auto& test : getAllTests())
        {
            if(std::regex_match(test.first, pattern))
            {
                enabledTests.emplace_back(test.first);
            }
        }
        return true;
    }
    return false;
}

Result test_data::execute(const TestData* data, TestRunner& runner)
try
{
    auto result = runner.compile(*data->sources, data->compilationOptions);
    if(!result)
        return result;
    result = runner.selectKernel(data->kernelName);
    if(!result)
        return result;
    for(std::size_t i = 0; i < data->kernelArguments.size(); ++i)
    {
        result = data->kernelArguments[i](i, runner);
        if(!result)
            return result;
    }
    result = runner.setWorkDimensions(data->workDimensions);
    if(!result)
        return result;
    result = runner.execute();
    if(!result)
        return result;
    for(const auto& verification : data->verifications)
    {
        result = verification(runner);
        if(!result)
            return result;
    }
    return RESULT_OK;
}
catch(const std::exception& err)
{
    return Result{false, err.what()};
}
