/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestEmulator.h"

#include "../src/Profiler.h"
#include "helper.h"

#include "test_cases.h"

#include <cstring>
#include <numeric>

static std::function<unsigned(unsigned)> get_local_id;
static std::function<unsigned(unsigned)> get_local_size;
static std::function<unsigned(unsigned)> get_group_id;
#define CLK_LOCAL_MEM_FENCE 0
void barrier(unsigned) {}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wold-style-cast"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#define __kernel static
#define __constant static const
#define __global
#define __local
#define __private
#include "../testing/HandsOnOpenCL/pi_ocl.cl"
#include "../testing/test_hashes.cl"
#include "../testing/test_partial_md5.cl"
#pragma GCC diagnostic pop

using namespace vc4c;
using namespace vc4c::tools;

TestEmulator::TestEmulator(const vc4c::Configuration& config) :
    TestCompilationHelper(config), cachePrecompilation(false)
{
    TEST_ADD(TestEmulator::testHelloWorld);
    TEST_ADD(TestEmulator::testHelloWorldVector);
    TEST_ADD(TestEmulator::testPrime);
    TEST_ADD(TestEmulator::testBarrier);
    TEST_ADD(TestEmulator::testBranches);
    TEST_ADD(TestEmulator::testWorkItem);
    // TODO fix result mismatch
    // TEST_ADD(TestEmulator::testSHA1);
    // TEST_ADD(TestEmulator::testSHA256);
    for(std::size_t i = 0; i < vc4c::test::integerTests.size(); ++i)
    {
        TEST_ADD_TWO_ARGUMENTS(TestEmulator::testIntegerEmulations, i, vc4c::test::integerTests.at(i).first.kernelName);
    }
    for(std::size_t i = 0; i < vc4c::test::floatTests.size(); ++i)
    {
        TEST_ADD_TWO_ARGUMENTS(TestEmulator::testFloatEmulations, i, vc4c::test::floatTests.at(i).first.kernelName);
    }
#ifndef SPIRV_FRONTEND
    // TODO "Unexpected llvm intrinsic: llvm.fshl.i32" in llvm-spirv
    TEST_ADD(TestEmulator::testPartialMD5);
#endif
    TEST_ADD(TestEmulator::testCRC16);
    TEST_ADD(TestEmulator::testPearson16);
    TEST_ADD(TestEmulator::testPi);
    TEST_ADD(TestEmulator::printProfilingInfo);
}

TestEmulator::TestEmulator(bool cachePrecompilation, const vc4c::Configuration& config) :
    TestCompilationHelper(config), cachePrecompilation(cachePrecompilation)
{
    // Constructor just, so the tests are not added to children
}

TestEmulator::~TestEmulator() = default;

void TestEmulator::testHelloWorld()
{
    std::stringstream buffer;
    compileFile(buffer, "./example/hello_world.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "hello_world";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {8, 1, 1};
    data.workGroup.numGroups = {1, 1, 1};
    // 16 characters per WI
    data.parameter.emplace_back(0u, std::vector<uint32_t>(data.calcNumWorkItems() * 16 / sizeof(uint32_t)));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(1u, result.results.size())

    const auto& out = *result.results.front().second;

    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()), 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 16, 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 32, 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 48, 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 64, 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 80, 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 96, 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 112, 16))
}

void TestEmulator::testHelloWorldVector()
{
    std::stringstream buffer;
    compileFile(buffer, "./example/hello_world_vector.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "hello_world";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    // 16 characters input
    data.parameter.emplace_back(0u, std::vector<uint32_t>(16 / sizeof(uint32_t)));
    memcpy(data.parameter[0].second->data(), "Hello World!", strlen("Hello World!"));
    // 16 characters output
    data.parameter.emplace_back(0u, std::vector<uint32_t>(16 / sizeof(uint32_t)));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(2u, result.results.size())

    const auto& in = *result.results.front().second;
    const auto& out = *result.results.back().second;

    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(in.data()), 16))
    TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()), 16))
}

void TestEmulator::testPrime()
{
    std::stringstream buffer;
    compileFile(buffer, "./example/test_prime.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "test_prime";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    {
        data.parameter.emplace_back(17u, Optional<std::vector<uint32_t>>{});
        data.parameter.emplace_back(0u, std::vector<uint32_t>(1));

        const auto result = emulate(data);
        TEST_ASSERT(result.executionSuccessful)
        TEST_ASSERT_EQUALS(2u, result.results.size())

        const auto& out = *result.results.back().second;
        TEST_ASSERT(*reinterpret_cast<const bool*>(out.data()))
    }
    {
        data.parameter.clear();
        data.parameter.emplace_back(18u, Optional<std::vector<uint32_t>>{});
        data.parameter.emplace_back(0u, std::vector<uint32_t>(1));

        const auto result = emulate(data);
        TEST_ASSERT(result.executionSuccessful)
        TEST_ASSERT_EQUALS(2u, result.results.size())

        const auto& out = *result.results.back().second;
        TEST_ASSERT(!*reinterpret_cast<const bool*>(out.data()))
    }
}

void TestEmulator::testBarrier()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_barrier.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "test_barrier";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.localSizes = {8, 1, 1};
    data.workGroup.numGroups = {2, 1, 1};

    // output parameter has size: 12 * sizes
    data.parameter.emplace_back(0u, std::vector<uint32_t>(12 * data.calcNumWorkItems()));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(1u, result.results.size())

    const auto& out = *result.results.front().second;

    for(uint32_t i = 0; i < data.calcNumWorkItems(); ++i)
    {
        TEST_ASSERT_EQUALS(0u, out[0 + i * 12])
        TEST_ASSERT_EQUALS(1u, out[1 + i * 12])
        TEST_ASSERT_EQUALS(2u, out[2 + i * 12])
        TEST_ASSERT_EQUALS(4u, out[4 + i * 12])
        TEST_ASSERT_EQUALS(5u, out[5 + i * 12])
        TEST_ASSERT_EQUALS(6u, out[6 + i * 12])
        TEST_ASSERT_EQUALS(7u, out[7 + i * 12])
        TEST_ASSERT_EQUALS(8u, out[8 + i * 12])
        TEST_ASSERT_EQUALS(9u, out[9 + i * 12])
        TEST_ASSERT_EQUALS(10u, out[10 + i * 12])
    }
}

void TestEmulator::testBranches()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_branches.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "test_branches";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // input value 512
    data.parameter.emplace_back(0u, std::vector<uint32_t>{});
    data.parameter.back().second->push_back(512);
    // output parameter (12 entries)
    data.parameter.emplace_back(0u, std::vector<uint32_t>(16));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(2u, result.results.size())

    const auto& out = *result.results.back().second;

    TEST_ASSERT_EQUALS(512u, out[2])
    TEST_ASSERT_EQUALS(100u, out[3])
    TEST_ASSERT_EQUALS(100u, out[4])
    TEST_ASSERT_EQUALS(512u, out[5])
    TEST_ASSERT_EQUALS(109u, out[7])
    TEST_ASSERT_EQUALS(109u, out[0])
    TEST_ASSERT_EQUALS(1849u, out[1])
}

void TestEmulator::testWorkItem()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_work_item.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "test_work_item";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {8, 1, 1};
    data.workGroup.numGroups = {4, 1, 1};
    // output buffer: 24 * work-items
    data.parameter.emplace_back(0, std::vector<uint32_t>(24 * data.calcNumWorkItems()));

    // TODO test with global offset != 0
    // TODO test with multi dimensions!

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(1u, result.results.size())

    const auto& out = *result.results.front().second;

    for(uint32_t xGroup = 0; xGroup < data.workGroup.numGroups.at(0); ++xGroup)
    {
        for(uint32_t xItem = 0; xItem < data.workGroup.localSizes.at(0); ++xItem)
        {
            ////global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
            auto globalID = data.workGroup.globalOffsets.at(0) + (xGroup * data.workGroup.localSizes.at(0) + xItem);
            const uint32_t* base = out.data() + (globalID * 24);

            TEST_ASSERT_EQUALS(3u, base[0])
            TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(0) * data.workGroup.localSizes.at(0), base[1])
            TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(1) * data.workGroup.localSizes.at(1), base[2])
            TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(2) * data.workGroup.localSizes.at(2), base[3])
            TEST_ASSERT_EQUALS(globalID, base[4])
            TEST_ASSERT_EQUALS(0u, base[5])
            TEST_ASSERT_EQUALS(0u, base[6])
            TEST_ASSERT_EQUALS(data.workGroup.globalOffsets.at(0), base[7])
            TEST_ASSERT_EQUALS(0u, base[8])
            TEST_ASSERT_EQUALS(0u, base[9])
            TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(0), base[10])
            TEST_ASSERT_EQUALS(1u, base[11])
            TEST_ASSERT_EQUALS(1u, base[12])
            TEST_ASSERT_EQUALS(xGroup, base[13])
            TEST_ASSERT_EQUALS(0u, base[14])
            TEST_ASSERT_EQUALS(0u, base[15])
            TEST_ASSERT_EQUALS(data.workGroup.localSizes.at(0), base[16])
            TEST_ASSERT_EQUALS(1u, base[17])
            TEST_ASSERT_EQUALS(1u, base[18])
            TEST_ASSERT_EQUALS(xItem, base[19])
            TEST_ASSERT_EQUALS(0u, base[20])
            TEST_ASSERT_EQUALS(0u, base[21])
            TEST_ASSERT_EQUALS(1u, base[22])
        }
    }
}

void TestEmulator::testSHA1()
{
    const std::string sample("Hello World!");
    const std::vector<uint32_t> digest = {0x2ef7bde6, 0x08ce5404, 0xe97d5f04, 0x2f95f89f, 0x1c232871};

    std::stringstream buffer;
    compileFile(buffer, "./example/md5.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "sha1_crypt_kernel";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // parameter 0 is the control data
    data.parameter.emplace_back(0, std::vector<uint32_t>{0 /* padding*/, 1 /* number of keys */});
    // parameter 1 is the salt, set to zero
    data.parameter.emplace_back(0, std::vector<uint32_t>(24));
    // parameter 2 is the "plain_key", the input
    data.parameter.emplace_back(0, std::vector<uint32_t>(sample.size() / sizeof(uint32_t)));
    memcpy(data.parameter.back().second->data(), sample.data(), sample.size());
    // parameter 3 is the digest
    data.parameter.emplace_back(0, std::vector<uint32_t>(8));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(4u, result.results.size())

    if(digest != result.results.at(3).second.value())
    {
        auto expectedIt = digest.begin();
        auto resultIt = result.results.at(3).second->begin();
        while(expectedIt != digest.end())
        {
            TEST_ASSERT_EQUALS(*expectedIt, *resultIt)

            ++resultIt;
            ++expectedIt;
        }
    }
}

void TestEmulator::testSHA256()
{
    const std::string sample("Hello World!1111");
    const std::vector<uint32_t> digest = {
        0xf90a1ef4, 0x422350ca, 0x8c448530, 0xa7d5d0b2, 0x35054803, 0xf7b2a73d, 0x86f4b639, 0x4b1329a5};

    std::stringstream buffer;
    compileFile(buffer, "./example/SHA-256.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "execute_sha256_cpu";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles * 4;
    data.module = std::make_pair("", &buffer);

    // parameter 0 is the input with a block-size of 16 words
    data.parameter.emplace_back(0, std::vector<uint32_t>(16));
    memcpy(data.parameter.back().second->data(), sample.data(), sample.size());
    // parameter 1 is the digest
    data.parameter.emplace_back(0, std::vector<uint32_t>(128));
    // parameter 2 is the stride
    data.parameter.emplace_back(0, Optional<std::vector<uint32_t>>{});

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    if(memcmp(digest.data(), result.results.at(1).second->data(), digest.size()) != 0)
    {
        auto expectedIt = digest.begin();
        auto resultIt = result.results.at(1).second->begin();
        while(expectedIt != digest.end())
        {
            TEST_ASSERT_EQUALS(*expectedIt, *resultIt)

            ++resultIt;
            ++expectedIt;
        }
    }
}

void TestEmulator::testIntegerEmulations(std::size_t index, std::string name)
{
    auto& data = vc4c::test::integerTests.at(index);
    testIntegerEmulation(data.first, data.second);
}

void TestEmulator::testFloatEmulations(std::size_t index, std::string name)
{
    testFloatingEmulation(vc4c::test::floatTests.at(index).first, vc4c::test::floatTests.at(index).second);
}

void TestEmulator::testIntegerEmulation(
    vc4c::tools::EmulationData& data, std::map<uint32_t, std::vector<uint32_t>>& expectedResults)
{
    std::stringstream buffer;
    compileFile(buffer, data.module.first, "", cachePrecompilation);
    data.module.second = &buffer;

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(data.parameter.size(), result.results.size())

    for(const auto& pair : expectedResults)
    {
        const auto& output = *result.results.at(pair.first).second;
        const auto& expected = pair.second;

        // we might write values we do not check
        TEST_ASSERT(expected.size() <= output.size())

        // general test equality
        if(output != expected)
        {
            // if that fails, test single elements
            for(std::size_t i = 0; i < expected.size(); ++i)
            {
                int e = bit_cast<uint32_t, int>(expected.at(i));
                int o = bit_cast<uint32_t, int>(output.at(i));
                TEST_ASSERT_EQUALS(e, o)
                if(e != o)
                {
                    TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
                }
            }
        }
    }
}

void TestEmulator::testFloatingEmulation(
    vc4c::tools::EmulationData& data, std::map<uint32_t, std::vector<uint32_t>>& expectedResults, unsigned maxULP)
{
    std::stringstream buffer;
    compileFile(buffer, data.module.first, "", cachePrecompilation);
    data.module.second = &buffer;

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(data.parameter.size(), result.results.size())

    for(const auto& pair : expectedResults)
    {
        const auto& output = *result.results.at(pair.first).second;
        const auto& expected = pair.second;

        // we might write values we do not check
        TEST_ASSERT(expected.size() <= output.size())

        // general test equality
        if(output != expected)
        {
            // if that fails, test single elements
            for(std::size_t i = 0; i < expected.size(); ++i)
            {
                float e = bit_cast<uint32_t, float>(expected.at(i));
                float o = bit_cast<uint32_t, float>(output.at(i));
                /*
                 * Testing for exact match is difficult, since one operand is a constant and the other is calculated.
                 * Thus for any value, which cannot be represented exactly, the values may differ.
                 * So we allow up to 1 ULP error
                 */
                TEST_ASSERT_ULP(e, o, maxULP)
                auto delta = e * static_cast<float>(maxULP) * std::numeric_limits<float>::epsilon();
                if(!Test::Comparisons::inMaxDistance(e, o, delta))
                {
                    TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
                }
            }
        }
    }
}

void TestEmulator::testPartialMD5()
{
    const std::string sample("Hello World!");

    std::stringstream buffer;
    compileFile(buffer, "./testing/test_partial_md5.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "calculate_md5";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    data.parameter.emplace_back(0, std::vector<uint32_t>(64));
    memcpy(data.parameter.back().second->data(), sample.data(), sample.size());
    data.parameter.emplace_back(sample.size(), Optional<std::vector<uint32_t>>{});
    data.parameter.emplace_back(0, std::vector<uint32_t>(8));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    // host-side calculation
    std::array<unsigned, 4> hostResult{};
    {
        std::array<char, 4096> in{};
        std::copy(sample.begin(), sample.end(), in.begin());

        hostResult.fill(0);
        calculate_md5(in.data(), static_cast<unsigned>(sample.size()), reinterpret_cast<char*>(hostResult.data()));
    }

    auto expectedIt = hostResult.begin();
    auto resultIt = result.results.at(2).second->begin();
    while(expectedIt != hostResult.end())
    {
        TEST_ASSERT_EQUALS(*expectedIt, *resultIt)

        ++resultIt;
        ++expectedIt;
    }
}

void TestEmulator::testCRC16()
{
    const std::string sample("Hello World!");

    std::stringstream buffer;
    compileFile(buffer, "./testing/test_hashes.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "crc16";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    data.parameter.emplace_back(0, std::vector<uint32_t>(1));
    data.parameter.emplace_back(0, std::vector<uint32_t>(sample.size() / sizeof(uint32_t)));
    memcpy(data.parameter.back().second->data(), sample.data(), sample.size());
    data.parameter.emplace_back(sample.size(), Optional<std::vector<uint32_t>>{});

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    // host-side calculation
    uint16_t hostResult{};
    crc16(&hostResult, reinterpret_cast<const uint8_t*>(sample.data()), static_cast<unsigned>(sample.size()));

    TEST_ASSERT_EQUALS(hostResult, result.results.at(0).second->front())
}

void TestEmulator::testPearson16()
{
    const std::string sample("Hello World!");

    std::stringstream buffer;
    compileFile(buffer, "./testing/test_hashes.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "Pearson16";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    data.parameter.emplace_back(0, std::vector<uint32_t>(sample.size() / sizeof(uint32_t)));
    memcpy(data.parameter.back().second->data(), sample.data(), sample.size());
    data.parameter.emplace_back(sample.size(), Optional<std::vector<uint32_t>>{});
    data.parameter.emplace_back(0, std::vector<uint32_t>(2));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    // host-side calculation
    std::array<uint8_t, 8> hostResult{};
    Pearson16(reinterpret_cast<const uint8_t*>(sample.data()), static_cast<unsigned>(sample.size()), hostResult.data());

    auto expectedIt = hostResult.begin();
    auto resultIt = reinterpret_cast<const uint8_t*>(&result.results.at(2).second->front());
    while(expectedIt != hostResult.end())
    {
        TEST_ASSERT_EQUALS(static_cast<unsigned>(*expectedIt), static_cast<unsigned>(*resultIt))

        ++resultIt;
        ++expectedIt;
    }
}

void TestEmulator::testPi()
{
    const uint32_t numIterations = 32;
    unsigned workGroupSize = 8;
    unsigned numSteps = 1024;
    auto numWorkGroups = numSteps / (numIterations * workGroupSize);
    if(numWorkGroups == 0)
    {
        numWorkGroups = 1;
        workGroupSize = numSteps / (numIterations * numWorkGroups);
    }
    numSteps = workGroupSize * numIterations * numWorkGroups;
    auto stepSize = 1.0f / static_cast<float>(numSteps);

    TEST_ASSERT_EQUALS(4u, numWorkGroups);
    TEST_ASSERT_EQUALS(1024u, numSteps);

    std::stringstream buffer;
    compileFile(buffer, "./testing/HandsOnOpenCL/pi_ocl.cl", "", cachePrecompilation);

    EmulationData data;
    data.kernelName = "pi";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.localSizes = {workGroupSize, 1, 1};
    data.workGroup.numGroups = {numWorkGroups, 1, 1};

    data.parameter.emplace_back(numIterations, Optional<std::vector<uint32_t>>{});
    data.parameter.emplace_back(vc4c::bit_cast<float, uint32_t>(stepSize), Optional<std::vector<uint32_t>>{});
    data.parameter.emplace_back(0, std::vector<uint32_t>(workGroupSize));
    data.parameter.emplace_back(0, std::vector<uint32_t>(numWorkGroups));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(4u, result.results.size())

    // host-side calculation
    std::vector<float> hostLocal(workGroupSize);
    std::vector<float> hostPartial(numWorkGroups);

    // execute all local IDs, but separate, since we need to fake the work-group functions
    get_local_size = [=](unsigned) { return workGroupSize; };
    for(unsigned group = 0; group < numWorkGroups; ++group)
    {
        get_group_id = [=](unsigned) { return group; };
        // iterate in descending order, since we have special handling for local ID 0
        for(int localId = static_cast<int>(workGroupSize); localId >= 0; --localId)
        {
            get_local_id = [=](unsigned) { return localId; };
            pi(numIterations, stepSize, hostLocal.data(), hostPartial.data());
        }
    }

    auto expectedIt = hostPartial.begin();
    auto& resultBuffer = *result.results.at(3).second;
    auto resultIt = resultBuffer.begin();
    while(expectedIt != hostPartial.end())
    {
        TEST_ASSERT_ULP(static_cast<float>(*expectedIt), (bit_cast<uint32_t, float>(*resultIt)), 4)

        ++resultIt;
        ++expectedIt;
    }

    auto hostPi = std::accumulate(hostPartial.begin(), hostPartial.end(), 0.0f);
    auto devicePi = std::accumulate(resultBuffer.begin(), resultBuffer.end(), 0.0f,
        [](float f, unsigned u) { return f + bit_cast<unsigned, float>(u); });
    hostPi *= stepSize;
    devicePi *= stepSize;
    std::cout << "Host calculated PI: " << hostPi << std::endl;
    std::cout << "(emulated) Device calculated PI: " << devicePi << std::endl;
}

void TestEmulator::printProfilingInfo()
{
#if DEBUG_MODE
    vc4c::profiler::dumpProfileResults(true);
#endif
}
