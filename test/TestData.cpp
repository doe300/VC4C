/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestData.h"

#include "TestEntries.h"
#include "test_files.h"

#include <algorithm>
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

    {
        TestDataBuilder<uint32_t, uint32_t, Buffer<uint32_t>> builder("fibonacci", fibonacci_cl_string, "fibonacci");
        builder.setParameter<0>(1);
        builder.setParameter<1>(1);
        builder.allocateParameter<2>(10);
        builder.checkParameterEquals<2>({2, 3, 5, 8, 13, 21, 34, 55, 89, 144});
    }

    {
        TestDataBuilder<Buffer<uint8_t>> builder("hello_world", hello_world_cl_string, "hello_world");
        builder.setDimensions(8);
        builder.allocateParameter<0>(8 * 16);
        builder.checkParameterEquals<0>({
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 1st work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 2nd work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 3rd work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 4th work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 5th work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 6th work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 7th work-item
            'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0', // 8th work-item
        });
    }

    {
        TestDataBuilder<Buffer<uint8_t>> builder("hello_world_constant", hello_world_constant_cl_string, "hello_world");
        builder.setDimensions(12);
        builder.allocateParameter<0>(16, 0x42);
        builder.checkParameterEquals<0>(
            {'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', 0x42, 0x42, 0x42, 0x42});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>> builder(
            "hello_world_vector", hello_world_vector_cl_string, "hello_world");
        builder.setParameter<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0'});
        builder.allocateParameter<1>(16);
        builder.checkParameterEquals<0>(
            {'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0'});
        builder.checkParameterEquals<1>(
            {'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0', '\0', '\0', '\0'});
    }

    {
        TestDataBuilder<Buffer<uint32_t>> builder("llvm_ir", test_cl_string, "test_llvm_ir");
        builder.allocateParameter<0>(1);
        builder.checkParameterEquals<0>({142});
    }

    {
        TestDataBuilder<int32_t, int32_t, float, float, Buffer<int32_t>, Buffer<float>> builder(
            "instructions", test_instructions_cl_string, "test_instructions");
        builder.setFlags(DataFilter::INT_ARITHMETIC | DataFilter::FLOAT_ARITHMETIC);
        builder.setParameter<0>(2);
        builder.setParameter<1>(4);
        builder.setParameter<2>(2.0f);
        builder.setParameter<3>(4.0f);
        builder.allocateParameter<4>(28);
        builder.allocateParameter<5>(7);
        builder.checkParameterEquals<4>(
            {6, -2, 8, 0, 2, 4, 2, 2, 32, 0, 0, 6, 6, -3, 30, 3, 3, 1, 1, 0, 0, 0, 1, 1, 1, 0, 4, 8});
        builder.checkParameterEquals<5>({6.0f, -2.0f, 8.0f, 0.5f, 4.0f, 2.0f, 2.0f});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint8_t>, Buffer<uint32_t>> builder(
            "SHA1", md5_cl_string, "sha1_crypt_kernel");
        builder.setFlags(DataFilter::DISABLED | DataFilter::COMPLEX_KERNEL);
        // parameter 0 is the control data
        builder.setParameter<0>({0 /* padding*/, 1 /* number of keys */});
        // parameter 1 is the salt, set to zero
        builder.allocateParameter<1>(24, 0x0);
        // parameter 2 is the "plain_key", the input TODO might need to invert the bytes in a word!
        builder.setParameter<2>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'});
        // parameter 3 is the digest
        builder.allocateParameter<3>(5);
        builder.checkParameterEquals<3>({0x2ef7bde6, 0x08ce5404, 0xe97d5f04, 0x2f95f89f, 0x1c232871});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint32_t>, uint32_t> builder(
            "SHA256", SHA_256_cl_string, "execute_sha256_cpu");
        builder.setFlags(DataFilter::COMPLEX_KERNEL);
        // parameter 0 is the input with a block-size of 16 words
        builder.setParameter<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '1', '1', '1', '1', 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
        // parameter 1 is the digest
        builder.allocateParameter<1>(128);
        // parameter 2 is the stride
        builder.setParameter<2>(0);
        builder.checkParameterPartialEquals<1>(
            {0xc24cbddc, 0xf26698b6, 0xf07dcadf, 0x7a8f4bb0, 0x87d4733b, 0xaa817cc2, 0x2d8f7709, 0x1e7442e6});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint32_t>, uint32_t> builder(
            "SHA256_vector", SHA_256_cl_string, "execute_sha256_gpu");
        builder.setFlags(DataFilter::COMPLEX_KERNEL);
        // parameter 0 is the input with a block-size of 16 words
        builder.setParameter<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '1', '1', '1', '1', 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
        // parameter 1 is the digest
        builder.allocateParameter<1>(128);
        // parameter 2 is the stride
        builder.setParameter<2>(0);
        builder.checkParameterPartialEquals<1>(
            {0xc24cbddc, 0xf26698b6, 0xf07dcadf, 0x7a8f4bb0, 0x87d4733b, 0xaa817cc2, 0x2d8f7709, 0x1e7442e6});
    }

    {
        TestDataBuilder<uint32_t, Buffer<uint32_t>> builder("prime_found", test_prime_cl_string, "test_prime");
        builder.setParameter<0>(17);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({true});
    }

    {
        TestDataBuilder<uint32_t, Buffer<uint32_t>> builder("prime_no_prime", test_prime_cl_string, "test_prime");
        builder.setParameter<0>(18);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({false});
    }

    ////
    // General Tests
    ////

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "async_copy", test_async_copy_cl_string, "test_async_copy");
        builder.setFlags(DataFilter::ASYNC_BARRIER);
        builder.setDimensions(12);
        builder.allocateParameterRange<0>(0, 12 * 16);
        builder.allocateParameter<1>(12 * 16);
        builder.allocateParameter<2>(12 * 16);
        // the __local arg might be lowered to VPM
        builder.checkParameterEquals<2>(toRange(0u, 12u * 16u));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "async_copy_general", test_async_copy_cl_string, "test_async_copy_general");
        builder.setFlags(DataFilter::ASYNC_BARRIER);
        builder.setDimensions(7);
        builder.allocateParameterRange<0>(0, 7 * 16);
        builder.allocateParameter<1>(7 * 16);
        builder.allocateParameter<2>(7 * 16);
        // the __local arg might be lowered to VPM
        builder.checkParameterEquals<2>(toRange(0u, 7u * 16u));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, uint32_t> builder(
            "async_copy_partial", test_async_copy_cl_string, "test_async_copy_partial");
        builder.setFlags(DataFilter::ASYNC_BARRIER);
        builder.setDimensions(12);
        builder.allocateParameterRange<0>(0, 7 * 16);
        builder.allocateParameter<1>(7 * 16);
        builder.allocateParameter<2>(7 * 16);
        builder.setParameter<3>(7);
        // the __local arg might be lowered to VPM
        builder.checkParameterEquals<2>(toRange(0u, 7u * 16u));
    }

    {
        TestDataBuilder<Buffer<uint32_t>> builder("barrier_dynamic_work_size", test_barrier_cl_string, "test_barrier");
        builder.setFlags(DataFilter::ASYNC_BARRIER);
        builder.setDimensions(8, 1, 1, 2);
        builder.allocateParameter<0>(12 * 8 * 2, 0x42);
        builder.checkParameterEquals<0>({
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
        });
    }

    {
        TestDataBuilder<Buffer<uint32_t>> builder(
            "barrier_fix_work_size", test_barrier_cl_string, "test_barrier", "-DFIXED_SIZE=1");
        builder.setFlags(DataFilter::ASYNC_BARRIER);
        builder.setDimensions(8, 1, 1, 2);
        builder.allocateParameter<0>(12 * 8 * 2, 0x42);
        builder.checkParameterEquals<0>({
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
        });
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "branches", test_branches_cl_string, "test_branches");
        builder.setFlags(DataFilter::CONTROL_FLOW);
        builder.setDimensions(6);
        builder.setParameter<0>({512, 1024, 256, 32, 64, 42});
        builder.allocateParameter<1>(6 * 12, 0x42);
        builder.checkParameterEquals<1>({
            109, 1849, 512, 100, 100, 512, 0x42, 109, 0x42, 0x42, 0x42, 0x42,       // in = 512
            1010, 2027, 1024, 1000, 1000, 1024, 1010, 0x42, 0x42, 0x42, 0x42, 0x42, // in = 1024
            108, 1833, 256, 100, 100, 256, 0x42, 0x42, 108, 0x42, 0x42, 0x42,       // in = 256
            15, 1401, 32, 10, 10, 32, 0x42, 0x42, 0x42, 0x42, 15, 0x42,             // in = 32
            16, 1465, 64, 10, 10, 64, 0x42, 0x42, 0x42, 16, 0x42, 0x42,             // in = 64
            11, 1145, 42, 10, 10, 42, 0x42, 0x42, 0x42, 0x42, 0x42, 11,             // in = 42
        });
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>> builder(
            "switch_short", test_branches_cl_string, "test_short_switch");
        builder.setFlags(DataFilter::CONTROL_FLOW);
        builder.setDimensions(6);
        builder.setParameter<0>({0x123, 0x124, 0x6432, 0x1345, -0x567, -0x7777});
        builder.allocateParameter<1>(6, 0x42);
        builder.checkParameterEquals<1>({11, 0x124, 17 + 0x1245, 0x1345 + 0x1245, 0x42 - 0x0FFF, 42});
    }

    {
        // TODO results are wrong for SPIR-V
        TestDataBuilder<Buffer<int64_t>, Buffer<int64_t>> builder(
            "switch_long", test_branches_cl_string, "test_long_switch");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.setDimensions(6);
        builder.setParameter<0>({0x12345678, 0x1F1F1F1F1F1F, 0x65432, 0x12345, -0x12345671234567, -0x12388887777});
        builder.allocateParameter<1>(6, 0x42);
        builder.checkParameterEquals<1>(
            {11, 0x1F1F1F1F1F1F, 17 + 0x1234500000000, 0x12345 + 0x1234500000000, 0x42 + 0x0FFF0000FFFF0000, 42});
    }

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint8_t>, uint32_t> builder("CRC16", test_hashes_cl_string, "crc16");
        builder.setFlags(DataFilter::COMPLEX_KERNEL);
        // output half-word
        builder.allocateParameter<0>(1);
        // data
        builder.setParameter<1>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'});
        // data size
        builder.setParameter<2>(12);
        // algorithm is CRC-16/ARC
        builder.checkParameterEquals<0>({0x57BE});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, uint32_t, Buffer<uint8_t>> builder(
            "Pearson16", test_hashes_cl_string, "Pearson16");
        builder.setFlags(DataFilter::COMPLEX_KERNEL);
        // data
        builder.setParameter<0>({'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'});
        // data size
        builder.setParameter<1>(12);
        // 8 byte result
        builder.allocateParameter<2>(8);
        // algorithm is CRC-16/ARC
        builder.checkParameterEquals<2>({92, 148, 236, 199, 49, 126, 212, 250});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder("atomics", test_other_cl_string, "test_atomics");
        builder.setFlags(DataFilter::ATOMIC_FUNCTIONS);
        builder.allocateParameterRange<0>(1, 12);
        builder.allocateParameterRange<1>(1, 12);
        builder.checkParameterEquals<1>({2, 0, 3, 5, 4, 6, 7, 8, 9, 10, 0});
    }

    {
        TestDataBuilder<float, float, float, float, Buffer<int32_t>> builder("f2i", test_other_cl_string, "test_f2i");
        builder.setFlags(DataFilter::TYPE_CONVERSIONS);
        builder.setParameter<0>(1.0f);
        builder.setParameter<1>(1.1f);
        builder.setParameter<2>(1.5f);
        builder.setParameter<3>(1.9f);
        builder.allocateParameter<4>(30);
        builder.checkParameterEquals<4>(
            {1, 1, 1, 1, -1, -1, -1, -1, 1, -1, 2, -1, 1, -1, 1, -2, 1, -1, 2, -2, 1, 1, 2, 2, -1, -1, -2, -2, 1, -1});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>> builder("sfu", test_sfu_cl_string, "test_sfu");
        builder.setParameter<0>({1.0f, 2.0f, 8.0f, 32.0f, 128.0f, 25.70f, 11.1f, 10.240f, 1.5f, 2.7f, 9.0f, 124.340f,
            112.2334455f, 56.7f, 74.1f, 0.00001f});
        builder.allocateParameter<1>(4 * 16);
        // hardware passes with 1024 but fails with 512
        builder.checkParameter<1, CompareULP<1024>>({// recip
            1.0f / 1.0f, 1.0f / 2.0f, 1.0f / 8.0f, 1.0f / 32.0f, 1.0f / 128.0f, 1.0f / 25.7f, 1.0f / 11.1f,
            1.0f / 10.240f, 1.0f / 1.5f, 1.0f / 2.7f, 1.0f / 9.0f, 1.0f / 124.34f, 1.0f / 112.2334455f, 1.0f / 56.7f,
            1.0f / 74.1f, 1.0f / 0.00001f,
            // rsqrt
            1.0f / std::sqrt(1.0f), 1.0f / std::sqrt(2.0f), 1.0f / std::sqrt(8.0f), 1.0f / std::sqrt(32.0f),
            1.0f / std::sqrt(128.0f), 1.0f / std::sqrt(25.7f), 1.0f / std::sqrt(11.1f), 1.0f / std::sqrt(10.240f),
            1.0f / std::sqrt(1.5f), 1.0f / std::sqrt(2.7f), 1.0f / std::sqrt(9.0f), 1.0f / std::sqrt(124.34f),
            1.0f / std::sqrt(112.2334455f), 1.0f / std::sqrt(56.7f), 1.0f / std::sqrt(74.1f),
            1.0f / std::sqrt(0.00001f),
            // exp2
            std::exp2(1.0f), std::exp2(2.0f), std::exp2(8.0f), std::exp2(32.0f), std::exp2(128.0f), std::exp2(25.70f),
            std::exp2(11.1f), std::exp2(10.240f), std::exp2(1.5f), std::exp2(2.7f), std::exp2(9.0f),
            std::exp2(124.340f), std::exp2(112.2334455f), std::exp2(56.7f), std::exp2(74.1f), std::exp2(0.00001f),
            // log2
            std::log2(1.0f), std::log2(2.0f), std::log2(8.0f), std::log2(32.0f), std::log2(128.0f), std::log2(25.70f),
            std::log2(11.1f), std::log2(10.240f), std::log2(1.5f), std::log2(2.7f), std::log2(9.0f),
            std::log2(124.340f), std::log2(112.2334455f), std::log2(56.7f), std::log2(74.1f), std::log2(0.00001f)});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>> builder("shuffle", test_shuffle_cl_string, "test_shuffle");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS);
        builder.allocateParameterRange<0>(0, 32);
        builder.allocateParameter<1>(10 * 16);
        builder.checkParameterEquals<1>({
            0x07, 0x06, 0x04, 0x08, 0x01, 0x0c, 0x0d, 0x01, 0x00, 0x09, 0x0e, 0x0f, 0x04, 0x03, 0x08, 0x06, // out[0]
            0x01, 0x07, 0x0b, 0x12, 0x15, 0x0f, 0x08, 0x09, 0x00, 0x13, 0x02, 0x01, 0x11, 0x0d, 0x07, 0x08, // out[1]
            0x1a, 0x1b, 0x02, 0x10, 0x04, 0x19, 0x06, 0x17, 0x08, 0x09, 0x1c, 0x13, 0x1a, 0x0d, 0x0e, 0x0f, // out[2]
            0x11, 0x01, 0x02, 0x10, 0x11, 0x01, 0x02, 0x10, 0x11, 0x01, 0x02, 0x10, 0x11, 0x01, 0x02, 0x10, // out[3]
            0x00, 0x00, 0x00, 0x01, 0x11, 0x00, 0x00, 0x10, 0x00, 0x11, 0x01, 0x02, 0x02, 0x10, 0x00, 0x00, // out[4]
            0x00, 0x00, 0x00, 0x00, 0x04, 0x04, 0x04, 0x04, 0x08, 0x08, 0x08, 0x08, 0x0c, 0x0c, 0x0c, 0x0c, // out[5]
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // out[6]
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, // out[7]
            0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, // out[8]
            0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00  // out[9]
        });
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>> builder(
            "shuffle_upcast", test_shuffle_cl_string, "test_shuffle_upcast");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS);
        builder.setParameter<0>({0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4,
            5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7});
        builder.setParameter<1>({4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0,
            1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3});
        builder.allocateParameter<2>(6 * 8);
        builder.checkParameterEquals<2>({
            0x17, 0x17, 0x17, 0x17, 0x02, 0x17, 0x17, 0x17, // out[0]
            0x42, 0x42, 0x42, 0x04, 0x42, 0x42, 0x42, 0x01, // out[1]
            0x07, 0x13, 0x02, 0x13, 0x13, 0x03, 0x13, 0x13, // out[2]
            0xFF, 0x05, 0xFF, 0x06, 0x01, 0xFF, 0xFF, 0x03, // out[3]
            0x06, 0x05, 0x01, 0x03, 0x71, 0x71, 0x71, 0x71, // out[4]
            0x03, 0x31, 0x31, 0x31, 0x31, 0x31, 0x31, 0x31, // out[5]
        });
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint32_t>> builder(
            "shuffle_sample3", test_shuffle_cl_string, "sample_test_char3");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS);
        builder.allocateParameterRange<0>(0, 32);
        builder.allocateParameter<1>(3 * 32 / 4);
        builder.checkParameterEquals<1>(
            {0x01000000, 0x00020000, 0x03000000, 0x00040000, 0x00000005, 0x07000006, 0x00080000, 0x00000009, 0x000B000A,
                0x0000000C, 0x0E00000D, 0x000F0000, 0x11001000, 0x00000000, 0x13000012, 0x15001400, 0x00160000,
                0x17000000, 0x00000018, 0x001A0019, 0x001B0000, 0x001C0000, 0x0000001D, 0x00001F1E});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint32_t>> builder(
            "shuffle_sample4", test_shuffle_cl_string, "sample_test_char4");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS);
        builder.allocateParameterRange<0>(0, 32);
        builder.allocateParameter<1>(32);
        builder.checkParameterEquals<1>({0x00000000, 0x01000000, 0x00020000, 0x03000000, 0x00000400, 0x05000000,
            0x00000600, 0x00000007, 0x00080000, 0x00000009, 0x00000A00, 0x0000000B, 0x0000000C, 0x0D000000, 0x00000E00,
            0x0F000000, 0x00000010, 0x11000000, 0x00000012, 0x00130000, 0x00140000, 0x00150000, 0x00000016, 0x00170000,
            0x00000018, 0x00190000, 0x00001A00, 0x001B0000, 0x1C000000, 0x001D0000, 0x1E000000, 0x00001F00});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder("struct", test_struct_cl_string, "test_struct");
        builder.setFlags(DataFilter::TYPE_HANDLING);
        builder.allocateParameter<0>(32, 0);
        builder.allocateParameter<1>(32 /* float16 + int + padding */, 0);
        builder.checkParameterEquals<1>(
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
    }

    {
        TestDataBuilder<float, Buffer<float>, Buffer<float>> builder(
            "vector_arithmetic", test_vector_cl_string, "test_arithm");
        builder.setParameter<0>(2.0f);
        builder.allocateParameterRange<1>(1.0f, 17.0f);
        builder.allocateParameter<2>(16);
        builder.checkParameterEquals<2>(toRange(3.0f, 19.0f));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder("copy_vector", test_vector_cl_string, "test_copy");
        builder.allocateParameterRange<0>(1, 17);
        builder.allocateParameter<1>(32);
        builder.checkParameterEquals<1>({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8,
            9, 10, 11, 12, 13, 14, 15, 16});
    }

    {
        TestDataBuilder<Vector<uint8_t, 16>, Vector<uint32_t, 4>, Vector<uint64_t, 2>, Buffer<uint32_t>> builder(
            "vector_param", test_vector_cl_string, "test_param");
        builder.setFlags(DataFilter::VECTOR_PARAM | DataFilter::USES_LONG);
        builder.setParameter<0>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15});
        builder.setParameter<1>({0, 1, 2, 3});
        builder.setParameter<2>({0, 1});
        builder.allocateParameter<3>(4);
        builder.checkParameterEquals<3>({0x03020100, 0x07060505, 0x0B0A090B, 0x0F0E0D0F});
    }

    {
        // XXX LLVM-SPIRV Translator does not support i3 type used for switch in test19
        TestDataBuilder<Buffer<float>, Buffer<float>> builder("vectorization1", test_vectorization_cl_string, "test1");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(1000);
        builder.allocateParameter<1>(1000);
        builder.checkParameterEquals<0>(toRange(-0.0f, -1000.0f, -1.0f));
        builder.checkParameterEquals<1>(toRange(-0.0f, -1000.0f, -1.0f));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, float, uint32_t, uint32_t> builder(
            "vectorization2", test_vectorization_cl_string, "test2");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(1.0f, 10.0f);
        builder.allocateParameterRange<1>(1.0f, 10.0f);
        builder.setParameter<2>(7.0f);
        builder.setParameter<3>(1);
        builder.setParameter<4>(6);
        builder.checkParameterEquals<0>({1.0f, 18.0f, 30.0f, 44.0f, 60.0f, 78.0f, 7.0f, 8.0f, 9.0f});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, float> builder(
            "vectorization3", test_vectorization_cl_string, "test3");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(1.0f, 801.0f);
        builder.allocateParameterRange<1>(1.0f, 801.0f);
        builder.setParameter<2>(7.0f);
        builder.checkParameterPartialEquals<0>(
            {8.0f, 18.0f, 30.0f, 44.0f, 60.0f, 78.0f, 98.0f, 120.0f, 144.0f /* ... */});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<uint32_t>> builder(
            "vectorization4", test_vectorization_cl_string, "test4");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({(1023 * 1024) / 2 + (5 * 1024)});
    }

    {
        TestDataBuilder<Buffer<float>> builder("vectorization5", test_vectorization_cl_string, "test5");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(1024);
        builder.checkParameterEquals<0>(toRange(0.0f, 1024.0f));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization6", test_vectorization_cl_string, "test6");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(1024, 2);
        builder.allocateParameterRange<1>(-510, 514);
        builder.checkParameterPartialEquals<1>({512 * (2 + 5), -509, -508, -507, -506 /* ... */});
    }

    {
        auto result = toRange(1, 1026);
        result[0] = 0;
        TestDataBuilder<Buffer<int32_t>> builder("vectorization7", test_vectorization_cl_string, "test7");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1025);
        builder.checkParameterEquals<0>(std::move(result));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization8", test_vectorization_cl_string, "test8");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameterRange<1>(0, 4096);
        builder.checkParameterPartialEquals<0>({0, 5, 10, 15, 20, 25, 30, 35, 40 /* ... */});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization9", test_vectorization_cl_string, "test9");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 4096);
        builder.allocateParameterRange<1>(0, 4096);
        builder.checkParameterPartialEquals<0>({0, 1, 2, 3, 5, 5, 6, 7, 10, 9, 10, 11, 15, 13, 14, 15, 20 /* ... */});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization10", test_vectorization_cl_string, "test10");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameterRange<1>(0, 1024);
        builder.checkParameterPartialEquals<0>({0, 1, 2, 3, 8, 5, 6, 7, 16, 9, 10, 11, 24 /* ... */});
    }

    {
        std::vector<int32_t> result(201, 0);
        std::fill_n(result.begin(), 100, 100);
        TestDataBuilder<Buffer<int32_t>> builder("vectorization11", test_vectorization_cl_string, "test11");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::TYPE_HANDLING | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 201);
        builder.checkParameterEquals<0>(std::move(result));
    }

    {
        auto result = toRange<float>(0.0f + 42.0f, 1024.0f + 42.0f);
        // only elements 200 to 500 are modified
        std::fill_n(result.begin(), 200, 13.0f);
        std::fill_n(result.begin() + 500, result.size() - 500, 13.0f);
        TestDataBuilder<Buffer<float>, Buffer<float>, float, uint32_t, uint32_t> builder(
            "vectorization12_partial", test_vectorization_cl_string, "test12");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(1024, 13.0f);
        builder.allocateParameterRange<1>(0.0f, 1024.0f);
        builder.setParameter<2>(42.0f);
        builder.setParameter<3>(200);
        builder.setParameter<4>(500);
        builder.checkParameterEquals<0>(std::move(result));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, float, uint32_t, uint32_t> builder(
            "vectorization12", test_vectorization_cl_string, "test12");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(1024, 13.0f);
        builder.allocateParameterRange<1>(0.0f, 1024.0f);
        builder.setParameter<2>(42.0f);
        builder.setParameter<3>(0);
        builder.setParameter<4>(1024);
        builder.checkParameterEquals<0>(toRange(0.0f + 42.0f, 1024.0f + 42.0f));
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, int32_t> builder(
            "vectorization13", test_vectorization_cl_string, "test13");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(1);
        builder.setParameter<2>(1000);
        builder.checkParameterEquals<1>({(999 * 1000) / 2 + (5 * 1000)});
    }

    {
        // Tests less than a full loop iteration used
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, int32_t> builder(
            "vectorization13_partial", test_vectorization_cl_string, "test13");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 5);
        builder.allocateParameter<1>(1);
        builder.setParameter<2>(5);
        builder.checkParameterEquals<1>({(4 * 5) / 2 + (5 * 5)});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, uint32_t> builder(
            "vectorization14", test_vectorization_cl_string, "test14");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(1024, 2);
        builder.allocateParameterRange<1>(-510, 514);
        builder.setParameter<2>(1024);
        builder.checkParameterPartialEquals<1>({512 * (2 + 5), -509, -508, -507, -506 /* ... */});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, uint32_t> builder(
            "vectorization14_partial", test_vectorization_cl_string, "test14");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameter<0>(5, 2);
        builder.allocateParameterRange<1>(0, 5);
        builder.setParameter<2>(5);
        builder.checkParameterEquals<1>({2 * (2 + 5), 1, 2, 3, 4});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization15", test_vectorization_cl_string, "test15");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({17 + (1023 * 1024) / 2 + (5 * 1024)});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<float>> builder(
            "vectorization16", test_vectorization_cl_string, "test16");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({(1023 * 1024) / 2 + (5 * 1024)});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization17", test_vectorization_cl_string, "test17");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({267264});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization18", test_vectorization_cl_string, "test18");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.setDimensions(3, 1, 1, 2);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(6, 0x42);
        builder.checkParameterEquals<1>({5 * 1024, (1023 * 1024) / 2, 5 * 1024, 5 * 1024, (1023 * 1024) / 2, 5 * 1024});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>> builder(
            "vectorization19", test_vectorization_cl_string, "test19");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::SPIRV_DISABLED);
        builder.allocateParameterRange<0>(0, 1024);
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({324088});
    }

    {
        TestDataBuilder<Buffer<uint32_t>> builder("work_item", test_work_item_cl_string, "test_work_item");
        builder.setFlags(DataFilter::WORK_GROUP);
        builder.setDimensions(8, 1, 1, 4, 1, 1);
        builder.allocateParameter<0>(24 * 8 * 4, 0x42);
        builder.checkParameterEquals<0>({
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
        });
    }

    {
        TestDataBuilder<Buffer<uint32_t>> builder(
            "work_item_global_offset", test_work_item_cl_string, "test_work_item");
        builder.setFlags(DataFilter::WORK_GROUP);
        builder.setDimensions(2, 1, 1, 2, 1, 1, 0, 2, 3);
        builder.allocateParameter<0>(24 * 2 * 2, 0x42);
        builder.checkParameterEquals<0>({
            3, 2 * 2, 1, 1, 0, 2, 3, 0, 2, 3, 2, 1, 1, 0, 0, 0, 2, 1, 1, 0, 0, 0, 0x43, 0x42, // work-item (0, 0)
            3, 2 * 2, 1, 1, 1, 2, 3, 0, 2, 3, 2, 1, 1, 0, 0, 0, 2, 1, 1, 1, 0, 0, 0x43, 0x42, // work-item (0, 1)
            3, 2 * 2, 1, 1, 2, 2, 3, 0, 2, 3, 2, 1, 1, 1, 0, 0, 2, 1, 1, 0, 0, 0, 0x43, 0x42, // work-item (1, 0)
            3, 2 * 2, 1, 1, 3, 2, 3, 0, 2, 3, 2, 1, 1, 1, 0, 0, 2, 1, 1, 1, 0, 0, 0x43, 0x42, // work-item (1, 1)
        });
    }

    ////
    // Bug Regression Tests
    ////
    // TODO add for all/most bugs/ files
    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "bug_local_memory_dot3", bugs_30_local_memory_cl_string, "dot3");
        builder.setFlags(DataFilter::WORK_GROUP);
        builder.setDimensions(10, 1, 1, 2, 1, 1);
        builder.allocateParameterRange<0>(0.0f, 20.0f);
        builder.setParameter<1>({0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f,
            0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f});
        builder.allocateParameter<2>(20);
        builder.allocateParameter<3>(16);
        builder.checkParameter<2, CompareULP<1>>({0.1f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 2.1f,
            1.1f, 1.2f, 1.3f, 1.4f, 1.5f, 1.6f, 1.7f, 1.8f, 1.9f});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "bug_local_memory_dot3_local", bugs_30_local_memory_cl_string, "dot3_local");
        builder.setFlags(DataFilter::WORK_GROUP);
        builder.setDimensions(10, 1, 1, 2, 1, 1);
        builder.allocateParameterRange<0>(0.0f, 20.0f);
        builder.setParameter<1>({0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f,
            0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f});
        builder.allocateParameter<2>(20);
        builder.checkParameter<2, CompareULP<1>>({0.1f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 2.1f,
            1.1f, 1.2f, 1.3f, 1.4f, 1.5f, 1.6f, 1.7f, 1.8f, 1.9f});
    }

    {
        TestDataBuilder<Buffer<float>> builder(
            "bug_float_add_redundancy", bugs_33_floating_point_folding_cl_string, "add_redundancy");
        builder.setParameter<0>({5.0f});
        builder.checkParameterEquals<0>({5.0f});
    }

    {
        TestDataBuilder<Buffer<float>> builder(
            "bug_float_mul_redundancy", bugs_33_floating_point_folding_cl_string, "mul_redundancy");
        builder.setParameter<0>({5.0f});
        builder.checkParameterEquals<0>({0.0f});
    }

    {
        TestDataBuilder<Buffer<float>> builder("bug_read_write_memory", bugs_vc4cl_27_wrong_result_cl_string, "hello");
        builder.setDimensions(10, 1, 1, 3, 1, 1);
        builder.allocateParameterRange<0>(-15.0f, 15.0f);
        builder.checkParameterEquals<0>(toRange(-30.0f, 30.0f, 2.0f));
    }

    {
        TestDataBuilder<Buffer<float>> builder("bug_const_assign", bugs_54_invalid_results_cl_string, "sum");
        builder.setDimensions(3, 1, 1, 3, 1, 1);
        builder.allocateParameter<0>(9);
        builder.checkParameterEquals<0>({1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f});
    }

    ////
    // OpenCL CTS Tests
    ////

    {
        // XXX passes for SPIR-V locally, but fails in CI
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>, uint32_t, uint32_t> builder(
            "OpenCL_CTS_async_copy", OpenCL_CTS_async_copy_global_to_local_cl_string,
            "test_async_copy_global_to_local");
        builder.setFlags(DataFilter::ASYNC_BARRIER | DataFilter::SPIRV_DISABLED);
        builder.setDimensions(4, 1, 1, 2, 1, 1);
        builder.allocateParameterRange<0>(0, 2 * 4 * 3 * 8);
        builder.allocateParameter<1>(2 * 4 * 3 * 8);
        builder.allocateParameter<2>(4 * 3 * 8);
        builder.setParameter<3>(4 * 3);
        builder.setParameter<4>(3);
        builder.checkParameterEquals<1>(toRange<uint8_t>(0, 2 * 4 * 3 * 8));
    }

    {
        TestDataBuilder<Buffer<uint32_t>, uint32_t, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "OpenCL_CTS_barrier", OpenCL_CTS_barrier_cl_string, "compute_sum");
        builder.setFlags(DataFilter::ASYNC_BARRIER);
        builder.setDimensions(11);
        builder.allocateParameterRange<0>(0, 24);
        builder.setParameter<1>(23);
        builder.allocateParameter<2>(16);
        builder.allocateParameter<3>(1);
        builder.checkParameterEquals<3>({253});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "OpenCL_CTS_clamp", OpenCL_CTS_clamp_cl_string, "test_clamp");
        builder.setDimensions(3);
        builder.setParameter<0>({17.0f, 0.0f, 3.0f});
        builder.setParameter<1>({1.0f, 1.0f, 1.0f});
        builder.setParameter<2>({5.0f, 5.0f, 5.0f});
        builder.allocateParameter<3>(3);
        builder.checkParameterEquals<3>({5.0f, 1.0f, 3.0f});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<int32_t>> builder(
            "OpenCL_CTS_constant", OpenCL_CTS_constant_cl_string, "constant_kernel");
        builder.calculateDimensions(32, 1);
        builder.allocateParameter<0>(32);
        builder.allocateParameterRange<1>(0.0f, 32.0f);
        builder.allocateParameterRange<2>(0, 32);
        builder.checkParameterEquals<0>(
            transform<float>(toRange<float>(0.0f, 32.0f), [](float f) -> float { return f * f; }));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, uint32_t> builder(
            "OpenCL_CTS_constant_loop", OpenCL_CTS_constant_cl_string, "loop_constant_kernel");
        builder.calculateDimensions(32, 1);
        builder.allocateParameter<0>(32);
        builder.allocateParameterRange<1>(0.0f, 32.0f);
        builder.setParameter<2>(2);
        builder.checkParameterEquals<0>(std::vector<float>(32, 0.0f + 3.0f));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "OpenCL_CTS_cross", OpenCL_CTS_cross_product_cl_string, "test_cross");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS | DataFilter::FLOAT_ARITHMETIC);
        builder.setParameter<0>({1.0f, 2.0f, 3.0f});
        builder.setParameter<1>({3.0f, 4.0f, 5.0f});
        builder.allocateParameter<2>(3);
        builder.checkParameterEquals<2>({-2.0f, 4.0f, -2.0f});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "OpenCL_CTS_add_sat_int3", OpenCL_CTS_integer_add_sat_cl_string, "test_add_sat_int3");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS | DataFilter::INT_ARITHMETIC);
        builder.setDimensions(2);
        builder.setParameter<0>(
            {std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min(),
                std::numeric_limits<int>::max(), static_cast<int32_t>(0x97c4aa2f), static_cast<int32_t>(0xa91a356a)});
        builder.setParameter<1>({-1, 1, std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
            static_cast<int32_t>(0xa91a356a), static_cast<int32_t>(0xa91a356a)});
        builder.allocateParameter<2>(6);
        builder.checkParameterEquals<2>(
            {std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min(),
                std::numeric_limits<int>::max(), std::numeric_limits<int>::min(), std::numeric_limits<int>::min()});
    }

    {
        TestDataBuilder<Buffer<uint16_t>, Buffer<uint16_t>, Buffer<uint16_t>> builder(
            "OpenCL_CTS_add_sat_ushort4", OpenCL_CTS_integer_add_sat_cl_string, "test_add_sat_ushort4");
        builder.setFlags(DataFilter::VECTOR_OPERATIONS | DataFilter::INT_ARITHMETIC);
        builder.setDimensions(2);
        builder.setParameter<0>({std::numeric_limits<uint16_t>::min(), std::numeric_limits<uint16_t>::max(),
            std::numeric_limits<uint16_t>::min(), std::numeric_limits<uint16_t>::max(), 0x2F, 0x8C7F, 0x8C7F, 0x1902});
        builder.setParameter<1>({std::numeric_limits<uint16_t>::max(), 1, std::numeric_limits<uint16_t>::min(),
            std::numeric_limits<uint16_t>::max(), 0x6A, 0x8C7F, 0x1902, 0x1902});
        builder.allocateParameter<2>(8);
        builder.checkParameterEquals<2>({std::numeric_limits<uint16_t>::max(), std::numeric_limits<uint16_t>::max(),
            std::numeric_limits<uint16_t>::min(), std::numeric_limits<uint16_t>::max(), 0x99,
            std::numeric_limits<uint16_t>::max(), 0xA581, 0x3204});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "OpenCL_CTS_local_kernel_scope", OpenCL_CTS_local_kernel_scope_cl_string, "test");
        builder.setFlags(DataFilter::MEMORY_ACCESS);
        builder.setDimensions(8, 1, 1, 8, 1, 1);
        builder.allocateParameterRange<0>(0, 64);
        builder.allocateParameter<1>(8);
        builder.checkParameterEquals<1>({7, 15, 23, 31, 39, 47, 55, 63});
    }

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

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint32_t>> builder(
            "OpenCL_CTS_pointer_cast", OpenCL_CTS_pointer_cast_cl_string, "test_pointer_cast");
        builder.setFlags(DataFilter::CORNER_CASES);
        builder.setParameter<0>({4, 3, 2, 1});
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({0x01020304});
    }

    {
        TestDataBuilder<Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>, Buffer<int16_t>> builder(
            "OpenCL_CTS_select_short", OpenCL_CTS_test_select_cl_string, "select_short_short");
        builder.setFlags(DataFilter::CORNER_CASES);
        builder.setDimensions(8);
        builder.allocateParameter<0>(8, 0x42);
        builder.allocateParameterRange<1>(0, 8);
        builder.allocateParameterRange<2>(8, 16);
        /* scalar has different comparison (comp ==/!=0) then vector versions (cmp </>= 0) */
        builder.setParameter<3>({0, 0, 42, 42, 0, 42, 42, 0});
        builder.checkParameterEquals<0>({0, 1, 10, 11, 4, 13, 14, 7});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<int32_t>> builder(
            "OpenCL_CTS_select_uint2", OpenCL_CTS_test_select_cl_string, "select_uint2_int2");
        builder.setFlags(DataFilter::CORNER_CASES);
        builder.calculateDimensions(8, 2);
        builder.allocateParameter<0>(8, 0x42);
        builder.allocateParameterRange<1>(0, 8);
        builder.allocateParameterRange<2>(8, 16);
        builder.setParameter<3>({1, 1, -1, -1, 1, -1, -1, 1});
        builder.checkParameterEquals<0>({0, 1, 10, 11, 4, 13, 14, 7});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "OpenCL_CTS_select_int3", OpenCL_CTS_test_select_cl_string, "select_int3_uint3");
        builder.setFlags(DataFilter::CORNER_CASES);
        builder.calculateDimensions(8, 3);
        builder.allocateParameter<0>(8, 0x42);
        builder.allocateParameterRange<1>(0, 8);
        builder.allocateParameterRange<2>(8, 16);
        builder.setParameter<3>({1, 1, -1, -1, 1, -1, -1, 1});
        builder.checkParameterEquals<0>({0, 1, 10, 11, 4, 13, 14, 7});
    }

    {
        TestDataBuilder<Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>, Buffer<int8_t>> builder(
            "OpenCL_CTS_select_char4", OpenCL_CTS_test_select_cl_string, "select_char4_char4");
        builder.setFlags(DataFilter::CORNER_CASES);
        builder.calculateDimensions(8, 4);
        builder.allocateParameter<0>(8, 0x42);
        builder.allocateParameterRange<1>(0, 8);
        builder.allocateParameterRange<2>(8, 16);
        builder.setParameter<3>({1, 1, -1, -1, 1, -1, -1, 1});
        builder.checkParameterEquals<0>({0, 1, 10, 11, 4, 13, 14, 7});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, int8_t> builder(
            "OpenCL_CTS_sub_buffers_read", OpenCL_CTS_sub_buffers_read_write_cl_string, "readTest");
        builder.setDimensions(4, 1, 1, 2);
        builder.setParameter<0>({4, 3, 2, 1, 8, 7, 6, 5});
        builder.setParameter<1>(0x10);
        builder.checkParameterEquals<0>({20, 19, 18, 17, 24, 23, 22, 21});
    }

    {
        TestDataBuilder<Buffer<int32_t>, Buffer<int32_t>, Buffer<int32_t>> builder(
            "OpenCL_CTS_sub_sat", OpenCL_CTS_sub_sat_cl_string, "test_sub_sat_int");
        builder.setFlags(DataFilter::INT_ARITHMETIC);
        builder.setDimensions(5);
        builder.setParameter<0>({std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
            std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), static_cast<int32_t>(0x8c7f0aac)});
        builder.setParameter<1>({1, -1, std::numeric_limits<int>::max(), std::numeric_limits<int>::min(),
            static_cast<int32_t>(0x1902f8c8)});
        builder.allocateParameter<2>(5);
        builder.checkParameterEquals<2>({std::numeric_limits<int>::min(), std::numeric_limits<int>::max(),
            std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min()});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, Buffer<uint8_t>> builder(
            "OpenCL_CTS_uchar_compare", OpenCL_CTS_uchar_compare_cl_string, "test_select");
        builder.setFlags(DataFilter::COMPARISONS);
        builder.setParameter<0>({4, 3, 2, 1});
        builder.setParameter<1>({1, 3, 2, 4});
        builder.allocateParameter<2>(4);
        builder.checkParameterEquals<2>({1, 3, 2, 4});
    }

    ////
    // Boost Compute Tests
    ////

    {
        TestDataBuilder<uint32_t, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "boost_adjacent_find", boost_compute_adjacent_find_cl_string, "serial_adjacent_find");
        builder.setParameter<0>(16);
        builder.allocateParameter<1>(1);
        builder.setParameter<2>({0, 1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 11, 12, 13});
        builder.checkParameterEquals<1>({7});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "boost_adjacent_find_with_atomics", boost_compute_adjacent_find_cl_string, "adjacent_find_with_atomics");
        builder.setFlags(DataFilter::ATOMIC_FUNCTIONS);
        builder.setDimensions(8, 1, 1, 2, 1, 1);
        builder.setParameter<0>({0xFFFFFFFFu});
        builder.setParameter<1>({0, 1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 11, 12, 13, 0});
        builder.checkParameterEquals<0>({7});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "boost_find_extrema_min", boost_compute_test_extrema_cl_string, "find_extrema_min_max");
        builder.setDimensions(7, 1, 1, 2, 1, 1);
        builder.setParameter<0>({17, 15, 45, 65, 3, 2, 7, 9, 11, 1300, 12, 6, 8, 200});
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({5});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>> builder("boost_find_extrema_max",
            boost_compute_test_extrema_cl_string, "find_extrema_min_max", "-DBOOST_COMPUTE_FIND_MAXIMUM=1");
        builder.setDimensions(7, 1, 1, 2, 1, 1);
        builder.setParameter<0>({17, 15, 45, 65, 3, 2, 7, 9, 11, 1300, 12, 6, 8, 200});
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({9});
    }

    {
        TestDataBuilder<uint32_t, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "boost_find_extrema_on_cpu_min", boost_compute_test_extrema_cl_string, "find_extrema_on_cpu_min_max");
        builder.setDimensions(2, 1, 1, 2, 1, 1);
        builder.setParameter<0>(15);
        builder.allocateParameter<1>(4);
        builder.allocateParameter<2>(4);
        builder.setParameter<3>({17, 15, 45, 65, 3, 2, 7, 9, 11, 1300, 12, 6, 8, 200, 65, 0});
        builder.checkParameterEquals<1>({15, 2, 6, 8});
        builder.checkParameterEquals<2>({1, 5, 11, 12});
    }

    {
        TestDataBuilder<uint32_t, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "boost_find_extrema_on_cpu_max", boost_compute_test_extrema_cl_string, "find_extrema_on_cpu_min_max",
            "-DBOOST_COMPUTE_FIND_MAXIMUM=1");
        builder.setDimensions(2, 1, 1, 2, 1, 1);
        builder.setParameter<0>(15);
        builder.allocateParameter<1>(4);
        builder.allocateParameter<2>(4);
        builder.setParameter<3>({17, 15, 45, 65, 3, 2, 7, 9, 11, 1300, 12, 6, 8, 200, 65, 0});
        builder.checkParameterEquals<1>({65, 9, 1300, 200});
        builder.checkParameterEquals<2>({3, 7, 9, 13});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint64_t>, uint32_t> builder(
            "boost_functional_popcount_long", boost_compute_test_functional_popcount_cl_string, "copy");
        builder.setFlags(DataFilter::USES_LONG);
        builder.setDimensions(8);
        builder.allocateParameter<0>(7, 0x42);
        builder.setParameter<1>({0, 1, 17, 0x200000000001, 0x100000F00000, 0xFFFFFFFFFFFFFFFF, 0x34000000001});
        builder.setParameter<2>(7);
        builder.checkParameterEquals<0>({0, 1, 2, 2, 5, 64, 4});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, Buffer<uint32_t>, uint32_t> builder(
            "boost_functional_popcount_int", boost_compute_test_functional_popcount_cl_string, "popcount_uint");
        builder.setFlags(DataFilter::USES_LONG);
        builder.setDimensions(8);
        builder.allocateParameter<0>(6, 0x42);
        builder.setParameter<1>({0, 1, 17, 0x20001, 0x100F000, 0xFFFFFFFF});
        builder.setParameter<2>(6);
        builder.checkParameterEquals<0>({0, 1, 2, 2, 5, 32});
    }

    {
        TestDataBuilder<uint32_t, Buffer<uint64_t>, Buffer<uint32_t>> builder(
            "boost_initial_reduce", boost_compute_initial_reduce_cl_string, "initial_reduce");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::ASYNC_BARRIER | DataFilter::USES_LONG);
        builder.setDimensions(8, 1, 1, 2, 1, 1);
        builder.setParameter<0>(static_cast<uint32_t>(32 * sizeof(uint32_t)));
        builder.allocateParameter<1>(2);
        builder.setParameter<2>(
            {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0});
        builder.checkParameterEquals<1>({8, 7});
    }

    {
        // TODO has random result mismatch errors
        TestDataBuilder<Buffer<uint64_t>, uint32_t, Buffer<uint64_t>> builder(
            "boost_insertion_sort", boost_compute_test_insertion_sort_cl_string, "serial_insertion_sort");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::USES_LONG | DataFilter::DISABLED);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(16);
        builder.setParameter<2>({1, 0, 2, 15, 14, 3, 11, 12, 4, 8, 7, 5, 10, 6, 9, 13});
        builder.checkParameterEquals<2>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, uint32_t, Buffer<uint16_t>> builder(
            "boost_insertion_sort_short", boost_compute_test_insertion_sort_cl_string, "serial_insertion_sort_short");
        builder.setFlags(DataFilter::CONTROL_FLOW);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(16);
        builder.setParameter<2>({0x1, 0x0, 0x2, 0xF, 0x3, 0xE, 0xC, 0xB, 0x4, 0x8, 0x7, 0x5, 0xA, 0x6, 0x9, 0xD});
        builder.checkParameterEquals<2>(
            {0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF});
    }

    {
        TestDataBuilder<uint32_t, uint32_t, Buffer<uint32_t>, Buffer<uint32_t>, Buffer<uint32_t>> builder(
            "boost_serial_merge", boost_compute_test_merge_cl_string, "serial_merge");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::TYPE_HANDLING);
        builder.setParameter<0>(16);
        builder.setParameter<1>(16);
        builder.setParameter<2>({1, 0, 3, 0, 5, 0, 7, 0, 9, 0, 11, 0, 13, 0, 15, 0, 17, 0, 19, 0, 21, 0, 23, 0, 25, 0,
            27, 0, 29, 0, 31, 0});
        builder.setParameter<3>({0, 0, 2, 0, 4, 0, 6, 0, 8, 0, 10, 0, 12, 0, 14, 0, 16, 0, 18, 0, 20, 0, 22, 0, 24, 0,
            26, 0, 28, 0, 30, 0});
        builder.allocateParameter<4>(64);
        builder.checkParameterEquals<4>({0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0, 10, 0, 11, 0, 12,
            0, 13, 0, 14, 0, 15, 0, 16, 0, 17, 0, 18, 0, 19, 0, 20, 0, 21, 0, 22, 0, 23, 0, 24, 0, 25, 0, 26, 0, 27, 0,
            28, 0, 29, 0, 30, 0, 31, 0});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, uint32_t, uint32_t, Buffer<uint32_t>, uint32_t> builder(
            "boost_reduce", boost_compute_test_reduce_cl_string, "reduce");
        builder.setFlags(DataFilter::CONTROL_FLOW | DataFilter::ASYNC_BARRIER);
        builder.setDimensions(8);
        builder.setParameter<0>({1, 5, 9, 13, 17});
        builder.setParameter<1>(0);
        builder.setParameter<2>(5);
        builder.allocateParameter<3>(1);
        builder.setParameter<4>(0);
        builder.checkParameterEquals<3>({1 + 5 + 9 + 13 + 17});
    }

    {
        TestDataBuilder<Buffer<uint32_t>, uint32_t> builder(
            "boost_fibonacci", boost_compute_test_transform2_cl_string, "copy");
        builder.setFlags(DataFilter::INT_ARITHMETIC | DataFilter::FLOAT_ARITHMETIC);
        builder.setDimensions(8);
        builder.allocateParameter<0>(25, 0x42);
        builder.setParameter<1>(25);
        builder.checkParameterEquals<0>({0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584,
            4181, 6765, 10946, 17711, 28657, 46368});
    }

    {
        TestDataBuilder<Buffer<int32_t>, uint32_t, Buffer<int32_t>> builder(
            "boost_user_defined_types", boost_compute_user_defined_types_cl_string, "serial_insertion_sort");
        builder.setFlags(DataFilter::TYPE_HANDLING | DataFilter::CONTROL_FLOW);
        builder.allocateParameter<0>(3 * 6, 0x42);
        builder.setParameter<1>(6);
        builder.setParameter<2>({
            7, 0, 1,   // UDD
            3, 2, 3,   // UDD
            2, 4, 5,   // UDD
            3, 6, 7,   // UDD
            1, 8, 9,   // UDD
            -3, 10, 11 // UDD
        });
        builder.checkParameterEquals<2>({
            -3, 10, 11, // UDD
            1, 8, 9,    // UDD
            2, 4, 5,    // UDD
            3, 2, 3,    // UDD
            3, 6, 7,    // UDD
            7, 0, 1     // UDD
        });
    }

    ////
    // Application Tests
    ////

    {
        // TODO fails on CI, but works locally...
        TestDataBuilder<Buffer<float>, uint32_t, Buffer<float>, uint32_t, uint32_t, uint32_t, uint32_t, uint32_t,
            uint32_t>
            builder("clNN_upscale", clNN_SpatialUpSamplingNearest_cl_string, "upscale");
        builder.setFlags(DataFilter::COMPLEX_KERNEL | DataFilter::INT_ARITHMETIC | DataFilter::WORK_GROUP);
        builder.setDimensions(8, 1, 1, 3, 1, 1);
        builder.allocateParameterRange<0>(0.0f, 8.0f);
        builder.setParameter<1>(0);
        builder.allocateParameter<2>(24, 42.0f);
        builder.setParameter<3>(0);
        builder.setParameter<4>(19);
        builder.setParameter<5>(2);
        builder.setParameter<6>(2);
        builder.setParameter<7>(2);
        builder.setParameter<8>(2);
        builder.checkParameterEquals<2>({0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 42, 42, 42, 42, 42});
    }

    {
        TestDataBuilder<Vector<uint32_t, 1>, Buffer<uint32_t>> builder(
            "single_element_struct", pocl_test_structs_as_args_cl_string, "test_single");
        builder.setFlags(DataFilter::TYPE_HANDLING);
        builder.setParameter<0>({0x01000102});
        builder.allocateParameter<1>(1);
        builder.checkParameterEquals<1>({0x01000102});
    }

    {
        TestDataBuilder<Vector<uint32_t, 4>, Buffer<uint32_t>> builder(
            "two_element_struct", pocl_test_structs_as_args_cl_string, "test_pair");
        builder.setFlags(DataFilter::TYPE_HANDLING);
        builder.setParameter<0>({0x01010101, 0x23232323, 0x45454545, 0x67676767});
        builder.allocateParameter<1>(2);
        builder.checkParameterEquals<1>({0x01010101, 0x45454545});
    }

    {
        TestDataBuilder<Vector<uint32_t, 12>, Buffer<uint32_t>> builder(
            "multi_element_struct", pocl_test_structs_as_args_cl_string, "test_kernel");
        builder.setFlags(DataFilter::TYPE_HANDLING);
        builder.setParameter<0>({0x01001001, 0x02002002, 0x03003003, 0x04004004, 0x05005005, 0xDEADDEAD, 0x06006006,
            0x07007007, 0x48008008, 0x09009009, 0x0A00A00A, 0x0B00B00B});
        builder.allocateParameter<1>(10);
        builder.checkParameterEquals<1>(
            {0x01001001, 0x02002002, 0x03003003, 0x05, 0x06006006, 131584, 0xFFFF9009, 0x0A00A00A, 48, 8});
    }

    {
        TestDataBuilder<uint32_t, float, Buffer<uint32_t>, Buffer<float>> builder(
            "pi", HandsOnOpenCL_pi_ocl_cl_string, "pi");
        builder.setDimensions(8, 1, 1, 4, 1, 1);
        builder.setParameter<0>(32);
        builder.setParameter<1>(1.0f / 1024.0f);
        builder.allocateParameter<2>(8);
        builder.allocateParameter<3>(4);
        builder.checkParameter<3>(4, &checkPiSum);
    }

    {
        TestDataBuilder<uint32_t, Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "mmul", HandsOnOpenCL_matmul_cl_string, "mmul");
        builder.setFlags(DataFilter::CONTROL_FLOW);
        builder.setDimensions(2, 2, 1, 2, 2, 1);
        builder.setParameter<0>(4);
        builder.allocateParameter<1>(4 * 4, 3.0f);
        builder.allocateParameter<2>(4 * 4, 5.0f);
        builder.allocateParameter<3>(4 * 4, 0.0f);
        builder.checkParameterEquals<3>(std::vector<float>(4 * 4, 4 * 3.0f * 5.0f));
    }

    {
        // TODO requires# include headers
        TestDataBuilder<Buffer<uint8_t>, Buffer<uint8_t>, uint32_t> builder(
            "histogram_1C", OpenCLIPP_Histogram_cl_string, "histogram_1C");
        builder.setFlags(DataFilter::DISABLED | DataFilter::ATOMIC_FUNCTIONS | DataFilter::ASYNC_BARRIER |
            DataFilter::COMPLEX_KERNEL);
        builder.setDimensions(4, 3);
        builder.setParameter<0>({0x02, 0x01, 0x00, 0x01, 0x01, 0x01, 0x06, 0x01, 0x05, 0x04, 0x03, 0x02});
        builder.allocateParameter<1>(8);
        builder.setParameter<2>(4);
        /* the value is the count of bytes <= the position */
        builder.checkParameterEquals<1>({1, 6, 8, 9, 10, 11, 12, 12});
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, uint32_t> builder(
            "VectorAdd", NVIDIA_VectorAdd_cl_string, "VectorAdd");
        builder.setFlags(DataFilter::WORK_GROUP);
        builder.setDimensions(12, 1, 1, 2, 1, 1);
        builder.allocateParameterRange<0>(0.0f, 18.0f);
        builder.allocateParameterRange<1>(0.0f, 18.0f);
        builder.allocateParameter<2>(17);
        builder.setParameter<3>(16);
        builder.checkParameterEquals<2>({0.0f, 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f, 20.0f, 22.0f,
            24.0f, 26.0f, 28.0f, 30.0f, 0.0f});
    }

    {
        TestDataBuilder<uint32_t, Buffer<float>, Buffer<float>> builder("deepCL_copy", deepCL_copy_cl_string, "copy");
        builder.setDimensions(12);
        builder.setParameter<0>(10);
        builder.setParameter<1>({1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12});
        builder.allocateParameter<2>(16);
        builder.checkParameterEquals<2>({1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 0, 0, 0, 0, 0, 0});
    }

    {
        TestDataBuilder<uint32_t, float, Buffer<float>, Buffer<float>> builder(
            "deepCL_multiplyConstant", deepCL_copy_cl_string, "multiplyConstant");
        builder.setDimensions(12);
        builder.setParameter<0>(10);
        builder.setParameter<1>(5.0f);
        builder.setParameter<2>({1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12});
        builder.allocateParameter<3>(12);
        builder.checkParameterEquals<3>({5 * 1, 5 * 9, 5 * -2, 5 * 8, 5 * 3, 5 * 7, 5 * 4, 5 * 6, 5 * 5, 5 * 0, 0, 0});
    }

    {
        TestDataBuilder<uint32_t, float, Buffer<float>> builder(
            "deepCL_multiplyInplace", deepCL_copy_cl_string, "multiplyInplace");
        builder.setDimensions(12);
        builder.setParameter<0>(10);
        builder.setParameter<1>(5.0f);
        builder.setParameter<2>({1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12});
        builder.checkParameterEquals<2>(
            {5 * 1, 5 * 9, 5 * -2, 5 * 8, 5 * 3, 5 * 7, 5 * 4, 5 * 6, 5 * 5, 5 * 0, 11, 12});
    }

    {
        TestDataBuilder<uint32_t, Buffer<float>> builder("deepCL_array_inv", deepCL_inv_cl_string, "array_inv");
        builder.setFlags(DataFilter::FLOAT_ARITHMETIC);
        builder.setDimensions(12);
        builder.setParameter<0>(10);
        builder.setParameter<1>({1, 9, -2, 8, 0.3f, 7, 0.4f, 6, -5, 0.1f, 11, 12});
        builder.checkParameter<1, CompareULP<1>>({1, 1.0f / 9.0f, 1.0f / -2.0f, 1.0f / 8.0f, 1.0f / 0.3f, 1.0f / 7.0f,
            1.0f / 0.4f, 1.0f / 6.0f, 1.0f / -5.0f, 1.0f / 0.1f, 11, 12});
    }

    {
        TestDataBuilder<Buffer<float>, float, uint32_t> builder("deepCL_memset", deepCL_memset_cl_string, "cl_memset");
        builder.setDimensions(12);
        builder.allocateParameter<0>(16);
        builder.setParameter<1>(17);
        builder.setParameter<2>(10);
        builder.checkParameterEquals<0>(
            {17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 0, 0, 0, 0, 0, 0});
    }

    {
        // TODO has result mismatch in sqrt()
        TestDataBuilder<Buffer<float>, Buffer<float>, uint32_t, float, float> builder(
            "NearestNeighbor", rodinia_nearestNeighbor_kernel_cl_string, "NearestNeighbor");
        builder.setFlags(DataFilter::DISABLED | DataFilter::FLOAT_ARITHMETIC);
        builder.setDimensions(12);
        builder.setParameter<0>({0, 0, 1, 1, 0, 1, 1, 0, -1, -1, -1, 0, 0, -1, 1, -1, -1, 1});
        builder.allocateParameter<1>(16);
        builder.setParameter<2>(9);
        builder.setParameter<3>(0.5f);
        builder.setParameter<4>(-0.5f);
        builder.checkParameterEquals<0>(
            {0.707107f, 1.58114f, 1.58114f, 0.707107f, 1.58114f, 1.58114f, 0.707107f, 0.707107f, 2.12132f});
    }

    {
        // TODO has some ULP errors near zero
        TestDataBuilder<Buffer<float>, Buffer<float>> builder(
            "BabelStream_mul", BabelStream_OCLStream_cl_string, "mul");
        builder.setFlags(DataFilter::DISABLED);
        builder.setDimensions(4, 1, 1, 4, 1, 1);
        builder.allocateParameter<0>(16);
        builder.allocateParameterRange<1>(-4.0f, 4.0f, 0.5f);
        builder.checkParameter<0, CompareULP<2>>(toRange<float>(-1.6f, 1.6f, 0.2f));
    }

    {
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "BabelStream_add", BabelStream_OCLStream_cl_string, "add");
        builder.setDimensions(4, 1, 1, 4, 1, 1);
        builder.allocateParameter<0>(16, 17.0f);
        builder.allocateParameterRange<1>(-4.0f, 4.0f, 0.5f);
        builder.allocateParameter<2>(16);
        builder.checkParameterEquals<2>(toRange<float>(13, 21, 0.5f));
    }

    {
        // TODO result error (2 ULP) on actual hardware
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>> builder(
            "BabelStream_triad", BabelStream_OCLStream_cl_string, "triad");
        builder.setDimensions(4, 1, 1, 4, 1, 1);
        builder.allocateParameter<0>(16);
        builder.allocateParameterRange<1>(-4.0f, 4.0f, 0.5f);
        builder.allocateParameter<2>(16, 17.0f);
        builder.checkParameter<0, CompareULP<1>>(toRange<float>(-4 + 0.4f * 17, 4 + 0.4f * 17, 0.5f));
    }

    {
        // TODO has some result mismatch errors, expected result could also be wrong?
        TestDataBuilder<Buffer<float>, Buffer<float>, Buffer<float>, Buffer<float>, uint32_t> builder(
            "BabelStream_dot", BabelStream_OCLStream_cl_string, "stream_dot");
        builder.setFlags(DataFilter::DISABLED | DataFilter::CONTROL_FLOW);
        builder.setDimensions(4, 1, 1, 4, 1, 1);
        builder.allocateParameterRange<0>(0.0f, 32.0f);
        builder.allocateParameter<1>(32, 17.0f);
        builder.allocateParameter<2>(4);
        builder.allocateParameter<3>(4);
        builder.setParameter<4>(32);
        // lid 0 calculates a[0] * b[0] + a[16] * b [16]= 0 * 17 + 16 * 17 = 272
        // group 0 calculates lid 0 + ... + lid 3
        builder.checkParameterEquals<3>(
            {272 + 306 + 340 + 374, 408 + 442 + 476 + 510, 544 + 578 + 612 + 646, 680 + 714 + 748 + 782});
    }

    {
        TestDataBuilder<Buffer<float>, int32_t, int32_t, Buffer<float>, int32_t, int32_t, int32_t, int32_t, int32_t,
            int32_t>
            builder("OpenCV_flip_rows_float2", OpenCV_flip_cl_string, "arithm_flip_rows",
                "-DT=float2 -DT1=float -Dkercn=2 -Dcn=1 -DPIX_PER_WI_Y=5");
        // one work-item per column (X) * DPIX_PER_WI_Y rows (Y) -> covers 10 * 10
        builder.setDimensions(10, 1, 1, 1, 2, 1);
        builder.setParameter<0>(toRange<float>(0, 8u /* width */ * 8u /* height */ * 2u /* components */));
        builder.setParameter<1>(2u * sizeof(float) * 8u); // source step = row stride
        builder.setParameter<2>(0);                       // source offset
        builder.allocateParameter<3>(8u /* width */ * 8u /* height */ * 2u /* components */);
        builder.setParameter<4>(2u * sizeof(float) * 8u); // destination step = row stride
        builder.setParameter<5>(0);                       // destination offset
        builder.setParameter<6>(8);                       // rows
        builder.setParameter<7>(8);                       // columns
        builder.setParameter<8>(8);                       // thread rows
        builder.setParameter<9>(8);                       // thread columns
        builder.checkParameterEquals<3>(
            {112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, //
                96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,  //
                80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,              //
                64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,              //
                48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,              //
                32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,              //
                16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,              //
                0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15});
    }

    {
        TestDataBuilder<Buffer<uint8_t>, int32_t, int32_t, Buffer<uint8_t>, int32_t, int32_t, int32_t, int32_t, int32_t,
            int32_t>
            builder("OpenCV_flip_rows_uchar3", OpenCV_flip_cl_string, "arithm_flip_rows",
                "-DT=uchar3 -DT1=uchar -Dkercn=3 -Dcn=3 -DPIX_PER_WI_Y=5");
        // one work-item per column (X) * DPIX_PER_WI_Y rows (Y) -> covers 10 * 10
        builder.setDimensions(10, 1, 1, 1, 2, 1);
        builder.setParameter<0>(toRange<uint8_t>(0, 8u /* width */ * 8u /* height */ * 3u /* components */));
        builder.setParameter<1>(3u * 8u); // source step = row stride
        builder.setParameter<2>(0);       // source offset
        builder.allocateParameter<3>(8u /* width */ * 8u /* height */ * 3u /* components */);
        builder.setParameter<4>(3u * 8u); // destination step = row stride
        builder.setParameter<5>(0);       // destination offset
        builder.setParameter<6>(8);       // rows
        builder.setParameter<7>(8);       // columns
        builder.setParameter<8>(8);       // thread rows
        builder.setParameter<9>(8);       // thread columns
        builder.checkParameterEquals<3>({168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
            184, 185, 186, 187, 188, 189, 190, 191, //
            144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
            165, 166, 167, //
            120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140,
            141, 142, 143, //
            96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
            118, 119,                                                                                       //
            72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, //
            48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, //
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, //
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23});
    }

    {
        // FIXME hangs sometimes in barrier across all work-items (at least with clang library front-end)
        TestDataBuilder<Buffer<float>, int32_t, int32_t, Buffer<float>, int32_t, int32_t, int32_t, int32_t, int32_t,
            int32_t>
            builder("OpenCV_flip_columns_float2", OpenCV_flip_cl_string, "arithm_flip_cols",
                "-DT=float2 -DT1=float -Dkercn=2 -Dcn=1 -DPIX_PER_WI_Y=5");
        // one work-item per column (X) * DPIX_PER_WI_Y rows (Y) -> covers 10 * 10
        builder.setDimensions(10, 1, 1, 1, 2, 1);
        builder.setParameter<0>(toRange<float>(0, 8u /* width */ * 8u /* height */ * 2u /* components */));
        builder.setParameter<1>(2u * sizeof(float) * 8u); // source step = row stride
        builder.setParameter<2>(0);                       // source offset
        builder.allocateParameter<3>(8u /* width */ * 8u /* height */ * 2u /* components */);
        builder.setParameter<4>(2u * sizeof(float) * 8u); // destination step = row stride
        builder.setParameter<5>(0);                       // destination offset
        builder.setParameter<6>(8);                       // rows
        builder.setParameter<7>(8);                       // columns
        builder.setParameter<8>(8);                       // thread rows
        builder.setParameter<9>(8);                       // thread columns
        builder.checkParameterEquals<3>({
            15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,                          //
            31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16,                //
            47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32,                //
            63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48,                //
            79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64,                //
            95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80,                //
            111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96,    //
            127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113, 112 //
        });
    }

    {
        TestDataBuilder<Buffer<uint8_t>, int32_t, int32_t, Buffer<uint8_t>, int32_t, int32_t, int32_t, int32_t, int32_t,
            int32_t>
            builder("OpenCV_flip_columns_uchar3", OpenCV_flip_cl_string, "arithm_flip_cols",
                "-DT=uchar3 -DT1=uchar -Dkercn=3 -Dcn=3 -DPIX_PER_WI_Y=5");
        // one work-item per column (X) * DPIX_PER_WI_Y rows (Y) -> covers 10 * 10
        builder.setDimensions(10, 1, 1, 1, 2, 1);
        builder.setParameter<0>(toRange<uint8_t>(0, 8u /* width */ * 8u /* height */ * 3u /* components */));
        builder.setParameter<1>(3u * 8u); // source step = row stride
        builder.setParameter<2>(0);       // source offset
        builder.allocateParameter<3>(8u /* width */ * 8u /* height */ * 3u /* components */);
        builder.setParameter<4>(3u * 8u); // destination step = row stride
        builder.setParameter<5>(0);       // destination offset
        builder.setParameter<6>(8);       // rows
        builder.setParameter<7>(8);       // columns
        builder.setParameter<8>(8);       // thread rows
        builder.setParameter<9>(8);       // thread columns
        builder.checkParameterEquals<3>(
            {21, 22, 23, 18, 19, 20, 15, 16, 17, 12, 13, 14, 9, 10, 11, 6, 7, 8, 3, 4, 5, 0, 1, 2,              //
                45, 46, 47, 42, 43, 44, 39, 40, 41, 36, 37, 38, 33, 34, 35, 30, 31, 32, 27, 28, 29, 24, 25, 26, //
                69, 70, 71, 66, 67, 68, 63, 64, 65, 60, 61, 62, 57, 58, 59, 54, 55, 56, 51, 52, 53, 48, 49, 50, //
                93, 94, 95, 90, 91, 92, 87, 88, 89, 84, 85, 86, 81, 82, 83, 78, 79, 80, 75, 76, 77, 72, 73, 74, //
                117, 118, 119, 114, 115, 116, 111, 112, 113, 108, 109, 110, 105, 106, 107, 102, 103, 104, 99, 100, 101,
                96, 97, 98, //
                141, 142, 143, 138, 139, 140, 135, 136, 137, 132, 133, 134, 129, 130, 131, 126, 127, 128, 123, 124, 125,
                120, 121, 122, //
                165, 166, 167, 162, 163, 164, 159, 160, 161, 156, 157, 158, 153, 154, 155, 150, 151, 152, 147, 148, 149,
                144, 145, 146, //
                189, 190, 191, 186, 187, 188, 183, 184, 185, 180, 181, 182, 177, 178, 179, 174, 175, 176, 171, 172, 173,
                168, 169, 170});
    }

    {
        TestDataBuilder<Buffer<float>, int32_t, int32_t, Buffer<float>, int32_t, int32_t, int32_t, int32_t, int32_t,
            int32_t>
            builder("OpenCV_flip_rows_columns_float2", OpenCV_flip_cl_string, "arithm_flip_rows_cols",
                "-DT=float2 -DT1=float -Dkercn=2 -Dcn=1 -DPIX_PER_WI_Y=5");
        // one work-item per column (X) * DPIX_PER_WI_Y rows (Y) -> covers 10 * 10
        builder.setDimensions(10, 1, 1, 1, 2, 1);
        builder.setParameter<0>(toRange<float>(0, 8u /* width */ * 8u /* height */ * 2u /* components */));
        builder.setParameter<1>(2u * sizeof(float) * 8u); // source step = row stride
        builder.setParameter<2>(0);                       // source offset
        builder.allocateParameter<3>(8u /* width */ * 8u /* height */ * 2u /* components */);
        builder.setParameter<4>(2u * sizeof(float) * 8u); // destination step = row stride
        builder.setParameter<5>(0);                       // destination offset
        builder.setParameter<6>(8);                       // rows
        builder.setParameter<7>(8);                       // columns
        builder.setParameter<8>(8);                       // thread rows
        builder.setParameter<9>(8);                       // thread columns
        builder.checkParameterEquals<3>(toRange<float>(8.0f * 8.0f * 2.0f - 1.0f, -1.0f, -1.0f));
    }

    {
        TestDataBuilder<Buffer<uint8_t>, int32_t, int32_t, Buffer<uint8_t>, int32_t, int32_t, int32_t, int32_t, int32_t,
            int32_t>
            builder("OpenCV_flip_rows_columns_uchar3", OpenCV_flip_cl_string, "arithm_flip_rows_cols",
                "-DT=uchar3 -DT1=uchar -Dkercn=3 -Dcn=3 -DPIX_PER_WI_Y=5");
        // one work-item per column (X) * DPIX_PER_WI_Y rows (Y) -> covers 10 * 10
        builder.setDimensions(10, 1, 1, 1, 2, 1);
        builder.setParameter<0>(toRange<uint8_t>(0, 8u /* width */ * 8u /* height */ * 3u /* components */));
        builder.setParameter<1>(3u * 8u); // source step = row stride
        builder.setParameter<2>(0);       // source offset
        builder.allocateParameter<3>(8u /* width */ * 8u /* height */ * 3u /* components */);
        builder.setParameter<4>(3u * 8u); // destination step = row stride
        builder.setParameter<5>(0);       // destination offset
        builder.setParameter<6>(8);       // rows
        builder.setParameter<7>(8);       // columns
        builder.setParameter<8>(8);       // thread rows
        builder.setParameter<9>(8);       // thread columns
        builder.checkParameterEquals<3>({
            189, 190, 191, 186, 187, 188, 183, 184, 185, 180, 181, 182, 177, 178, 179, 174, 175, 176, 171, 172, 173,
            168, 169, 170, 165, 166, 167, 162, 163, 164, 159, 160, 161, 156, 157, 158, 153, 154, 155, 150, 151, 152,
            147, 148, 149, 144, 145, 146, //
            141, 142, 143, 138, 139, 140, 135, 136, 137, 132, 133, 134, 129, 130, 131, 126, 127, 128, 123, 124, 125,
            120, 121, 122, //
            117, 118, 119, 114, 115, 116, 111, 112, 113, 108, 109, 110, 105, 106, 107, 102, 103, 104, 99, 100, 101, 96,
            97, 98,                                                                                         //
            93, 94, 95, 90, 91, 92, 87, 88, 89, 84, 85, 86, 81, 82, 83, 78, 79, 80, 75, 76, 77, 72, 73, 74, //
            69, 70, 71, 66, 67, 68, 63, 64, 65, 60, 61, 62, 57, 58, 59, 54, 55, 56, 51, 52, 53, 48, 49, 50, //
            45, 46, 47, 42, 43, 44, 39, 40, 41, 36, 37, 38, 33, 34, 35, 30, 31, 32, 27, 28, 29, 24, 25, 26, //
            21, 22, 23, 18, 19, 20, 15, 16, 17, 12, 13, 14, 9, 10, 11, 6, 7, 8, 3, 4, 5, 0, 1, 2            //
        });
    }

    {
        TestDataBuilder<Buffer<int16_t>, int32_t, int32_t, int32_t, int32_t, int32_t, Buffer<float>> builder(
            "OpenCV_mean_stddev", OpenCV_meanstddev_cl_string, "meanStdDev",
            "-DsrcT=short2 -DdstT=float2 -DsqdstT=float2 -DconvertToDT=convert_float2 -DconvertToSDT=convert_float2 "
            "-Dcn=2 -DWGS=12 -DWGS2_ALIGNED=8 -DHAVE_SRC_CONT");
        builder.setFlags(DataFilter::ASYNC_BARRIER | DataFilter::COMPLEX_KERNEL);
        builder.setDimensions(12, 1, 1, 2);
        builder.setParameter<0>(toRange<int16_t>(-20, 20));
        builder.setParameter<1>(1);  // source step
        builder.setParameter<2>(0);  // source offset
        builder.setParameter<3>(0);  // columns (ignored)
        builder.setParameter<4>(20); // total (apparently in short2 entries)
        builder.setParameter<5>(2);  // groups
        builder.allocateParameter<6>(2 /* float 2*/ * 2 /* sum and square sum */ * 2 /* 2 groups */);
        builder.checkParameterEquals<6>({
            -20 - 18 - 16 - 14 - 12 - 10 - 8 - 6 - 4 - 2 + 0 + 2,
            -19 - 17 - 15 - 13 - 11 - 9 - 7 - 5 - 3 - 1 + 1 + 3,
            4 + 6 + 8 + 10 + 12 + 14 + 16 + 18,
            5 + 7 + 9 + 11 + 13 + 15 + 17 + 19,
            (-20 * -20) + (-18 * -18) + (-16 * -16) + (-14 * -14) + (-12 * -12) + (-10 * -10) + (-8 * -8) + (-6 * -6) +
                (-4 * -4) + (-2 * -2) + (0 * 0) + (2 * 2),
            (-19 * -19) + (-17 * -17) + (-15 * -15) + (-13 * -13) + (-11 * -11) + (-9 * -9) + (-7 * -7) + (-5 * -5) +
                (-3 * -3) + (-1 * -1) + (1 * 1) + (3 * 3),
            (4 * 4) + (6 * 6) + (8 * 8) + (10 * 10) + (12 * 12) + (14 * 14) + (16 * 16) + (18 * 18),
            (5 * 5) + (7 * 7) + (9 * 9) + (11 * 11) + (13 * 13) + (15 * 15) + (17 * 17) + (19 * 19),
        });
    }

    {
        TestDataBuilder<Buffer<int16_t>, int32_t, int32_t, Buffer<uint8_t>, int32_t, int32_t, Buffer<float>, int32_t,
            int32_t, int32_t, int32_t, float, float>
            builder("OpenCV_normalize", OpenCV_normalize_cl_string, "normalizek",
                "-Dcn=2 -DsrcT=short2 -DdstT=float2 -DrowsPerWI=5 -DworkT=float2 -DconvertToWT=convert_float2 "
                "-DconvertToDT= -DHAVE_SCALE -DHAVE_DELTA");
        builder.setDimensions(10, 1, 1, 1, 2, 1);          // 10 columns / 2 * 5 rows
        builder.setParameter<0>(toRange<int16_t>(0, 200)); // source data, 10 * 10 * 2 entries
        builder.setParameter<1>(2 * sizeof(int16_t) * 10); // source step = row stride
        builder.setParameter<2>(0);                        // source offset
        builder.setParameter<3>({0x1});                    // mask data
        builder.setParameter<4>(0);                        // mask step
        builder.setParameter<5>(0);                        // mask offset
        builder.allocateParameter<6>(200);                 // destination data, 10 * 10 * 2 entries
        builder.setParameter<7>(2 * sizeof(float) * 10);   // destination step = row stride
        builder.setParameter<8>(0);                        // destination offset
        builder.setParameter<9>(10);                       // destination rows
        builder.setParameter<10>(10);                      // destination columns
        builder.setParameter<11>(1.0f / 256.0f);           // scale
        builder.setParameter<12>(3.0f);                    // delta
        builder.checkParameterEquals<6>(
            toRange(3.0f, 3.0f + 200.0f / 256.0f, 1.0f / 256.0f)); // out = in * scale + delta
    }

    {
        TestDataBuilder<Buffer<uint32_t>, int32_t, int32_t, int32_t, int32_t, Buffer<uint32_t>, int32_t, int32_t>
            builder("OpenCV_transpose", OpenCV_transpose_cl_string, "transpose",
                "-Dcn=1 -DT=uint -DTILE_DIM=10 -DBLOCK_ROWS=1 -DrowsPerWI=5");
        builder.setFlags(DataFilter::COMPLEX_KERNEL | DataFilter::CONTROL_FLOW);
        builder.setDimensions(10, 1, 1, 1, 2, 1);           // 10 columns / 2 * 5 rows
        builder.setParameter<0>(toRange<uint32_t>(0, 100)); // source data, 10 * 10 entries
        builder.setParameter<1>(sizeof(int32_t) * 10);      // source step = row stride
        builder.setParameter<2>(0);                         // source offset
        builder.setParameter<3>(10);                        // source rows
        builder.setParameter<4>(10);                        // source columns
        builder.allocateParameter<5>(100);                  // destination data, 10 * 10 entries
        builder.setParameter<6>(sizeof(int32_t) * 10);      // destination step = row stride
        builder.setParameter<7>(0);                         // destination offset
        builder.checkParameterEquals<5>({
            0, 10, 20, 30, 40, 50, 60, 70, 80, 90, //
            1, 11, 21, 31, 41, 51, 61, 71, 81, 91, //
            2, 12, 22, 32, 42, 52, 62, 72, 82, 92, //
            3, 13, 23, 33, 43, 53, 63, 73, 83, 93, //
            4, 14, 24, 34, 44, 54, 64, 74, 84, 94, //
            5, 15, 25, 35, 45, 55, 65, 75, 85, 95, //
            6, 16, 26, 36, 46, 56, 66, 76, 86, 96, //
            7, 17, 27, 37, 47, 57, 67, 77, 87, 97, //
            8, 18, 28, 38, 48, 58, 68, 78, 88, 98, //
            9, 19, 29, 39, 49, 59, 69, 79, 89, 99, //
        });
    }

    {
        TestDataBuilder<Buffer<uint32_t>, int32_t, int32_t, int32_t> builder("OpenCV_transpose_inplace",
            OpenCV_transpose_cl_string, "transpose_inplace",
            "-Dcn=1 -DT=uint -DTILE_DIM=10 -DBLOCK_ROWS=1 -DrowsPerWI=5");
        builder.setFlags(DataFilter::COMPLEX_KERNEL | DataFilter::CONTROL_FLOW);
        builder.setDimensions(10, 1, 1, 1, 2, 1);           // 10 columns / 2 * 5 rows
        builder.setParameter<0>(toRange<uint32_t>(0, 100)); // source data, 10 * 10 entries
        builder.setParameter<1>(sizeof(int32_t) * 10);      // source step = row stride
        builder.setParameter<2>(0);                         // source offset
        builder.setParameter<3>(10);                        // source rows
        builder.checkParameterEquals<0>({
            0, 10, 20, 30, 40, 50, 60, 70, 80, 90, //
            1, 11, 21, 31, 41, 51, 61, 71, 81, 91, //
            2, 12, 22, 32, 42, 52, 62, 72, 82, 92, //
            3, 13, 23, 33, 43, 53, 63, 73, 83, 93, //
            4, 14, 24, 34, 44, 54, 64, 74, 84, 94, //
            5, 15, 25, 35, 45, 55, 65, 75, 85, 95, //
            6, 16, 26, 36, 46, 56, 66, 76, 86, 96, //
            7, 17, 27, 37, 47, 57, 67, 77, 87, 97, //
            8, 18, 28, 38, 48, 58, 68, 78, 88, 98, //
            9, 19, 29, 39, 49, 59, 69, 79, 89, 99, //
        });
    }
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
        registerMathTests();
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
    auto result = runner.compile(*data->sources, data->compilationOptions, data->uniqueName);
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
        auto tmpResult = verification(runner);
        if(!tmpResult)
        {
            // combine the errors into one
            result.wasSuccess = false;
            if(!result.error.empty())
                result.error.append("\n");
            result.error.append(tmpResult.error);
        }
    }
    return result;
}
catch(const std::exception& err)
{
    return Result{false, err.what()};
}
