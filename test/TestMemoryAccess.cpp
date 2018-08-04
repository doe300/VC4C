/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestMemoryAccess.h"

#include "test_cases.h"

using namespace vc4c;
using namespace vc4c::tools;

TestMemoryAccess::TestMemoryAccess(const Configuration& config) : TestEmulator(false, config)
{
    TEST_ADD(TestMemoryAccess::testPrivateStorage);
    TEST_ADD(TestMemoryAccess::testLocalStorage);
    TEST_ADD(TestMemoryAccess::testConstantStorage);
    TEST_ADD(TestMemoryAccess::testRegisterStorage);

    TEST_ADD(TestMemoryAccess::testVPMWrites);
    TEST_ADD(TestMemoryAccess::testVPMReads);
}

void TestMemoryAccess::testPrivateStorage()
{
    const std::vector<uint32_t> expected{
        14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28};
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_private_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {2, 1, 1};

    // parameter 0 is the input
    data.parameter.emplace_back(0, std::vector<uint32_t>(24, 7));
    // parameter 1 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(24));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(2u, result.results.size());

    if(expected != result.results.at(1).second.value())
    {
        auto expectedIt = expected.begin();
        auto resultIt = result.results.at(1).second->end();
        while(expectedIt != expected.end())
        {
            TEST_ASSERT_EQUALS(*expectedIt, *resultIt);
            ++resultIt;
            ++expectedIt;
        }
    }
}

void TestMemoryAccess::testLocalStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_local_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {2, 1, 1};

    // parameter 0 is the input
    data.parameter.emplace_back(0, std::vector<uint32_t>(24, 7));
    // parameter 1 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(24));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(2u, result.results.size());

    // actual results are indeterministic, since the order of the loads/stores across work-items is not guaranteed
    for(auto res : result.results.at(1).second.value())
    {
        if(res % 7 != 0)
        {
            TEST_ASSERT_EQUALS(7, res);
        }
    }
}

void TestMemoryAccess::testConstantStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_constant_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {1, 1, 1};

    // parameter 0 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(12));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(1u, result.results.size());
    TEST_ASSERT_EQUALS(
        std::string("Hello World"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())));
}

void TestMemoryAccess::testRegisterStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_register_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {1, 1, 1};

    // parameter 0 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(12));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(1u, result.results.size());
    TEST_ASSERT_EQUALS(
        std::string("Hello World"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())));
}

void TestMemoryAccess::testVPMWrites()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_vpm_write.cl");

    EmulationData data;
    data.kernelName = "test_vpm_write";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // parameter 0 is input
    data.parameter.emplace_back(0, std::vector<uint32_t>(16));
    for(unsigned i = 0; i < data.parameter.back().second->size(); ++i)
        data.parameter.back().second->at(i) = i;
    // parameter 1 is integer output
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 16));
    // parameter 2 is short output
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 8));
    // parameter 3 is char output
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 4));
    // parameter 4 is integer output with stride
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 16 * 4));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(5u, result.results.size());

    auto& src = data.parameter[0].second.value();
    auto& res1 = result.results[1].second.value();
    auto& res2 = result.results[2].second.value();
    auto& res3 = result.results[3].second.value();
    auto& res4 = result.results[4].second.value();

    for(unsigned i = 0; i < 10 * 16; ++i)
    {
        TEST_ASSERT_EQUALS(src.at(i % 16), res1.at(i));
        TEST_ASSERT_EQUALS(static_cast<short>(src.at(i % 16)), reinterpret_cast<const short*>(res2.data())[i]);
        TEST_ASSERT_EQUALS(src.at(i % 16), static_cast<unsigned>(reinterpret_cast<const char*>(res3.data())[i]));
        TEST_ASSERT_EQUALS(src.at(i % 16), res4.at((i / 16) * 3 * 16 /* stride */ + (i % 16)));
    }
}

void TestMemoryAccess::testVPMReads()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_vpm_read.cl");

    EmulationData data;
    data.kernelName = "test_vpm_read";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // parameters 0 and 1 are both input/output
    data.parameter.emplace_back(0, std::vector<unsigned>(4 * 10));
    data.parameter.emplace_back(0, std::vector<unsigned>(4 * 10 * 3 /* stride */));

    for(unsigned i = 0; i < 4 * 10; ++i)
        data.parameter[0].second->at(i) = i;

    for(unsigned i = 0; i < 4 * 10 * 3; ++i)
        data.parameter[1].second->at(i) = i;

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(2u, result.results.size());

    auto& v1 = result.results[0].second.value();
    auto& v2 = result.results[1].second.value();

    // we copy (with stride) from v2 to v1 and then from v1 back to v2
    //-> v1 and v2 have same values (for first 10 int4 vectors)

    for(unsigned i = 0; i < 10 * 4; ++i)
    {
        TEST_ASSERT_EQUALS(v1.at(i), v2.at(i));
        TEST_ASSERT_EQUALS((i / 4) * 12 /*stride * elements*/ + (i % 4), v2[i]);
    }
}