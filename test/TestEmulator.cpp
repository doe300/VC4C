/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestEmulator.h"

#include "../src/Profiler.h"
#include "EmulationRunner.h"
#include "helper.h"

#include "TestData.h"

#include <cstring>
#include <numeric>

using namespace vc4c;
using namespace vc4c::tools;

auto defaultFilter = test_data::DataFilter::DISABLED | test_data::DataFilter::VECTOR_PARAM;

TestEmulator::TestEmulator(const vc4c::Configuration& config) :
    TestEmulator(config,
        test_data::getAllTests(defaultFilter |
            (config.frontend == Frontend::SPIR_V ? test_data::DataFilter::SPIRV_DISABLED :
                                                   test_data::DataFilter::NONE)))
{
}

TestEmulator::TestEmulator(const vc4c::Configuration& config,
    std::map<std::string, const test_data::TestData*>&& testData, std::vector<std::string>&& additionalTestNames) :
    TestCompilationHelper(config)
{
    for(auto additionalTest : additionalTestNames)
    {
        if(auto test = test_data::getTest(additionalTest))
            testData.emplace(additionalTest, test);
        else
        {
            TEST_ADD_WITH_STRING(TestEmulator::runNoSuchTestData, additionalTest);
        }
    }
    for(auto test : testData)
    {
        TEST_ADD_TWO_ARGUMENTS(TestEmulator::runTestData, std::string{test.first}, true);
    }
    if(!testData.empty())
    {
        TEST_ADD(TestEmulator::printProfilingInfo);
    }
}

TestEmulator::~TestEmulator() = default;

void TestEmulator::printProfilingInfo()
{
#ifndef NDEBUG
    vc4c::profiler::dumpProfileResults(true);
#endif
}

EmulationRunner::~EmulationRunner() noexcept = default;

void TestEmulator::runTestData(std::string dataName, bool useCompilationCache)
{
    std::unordered_map<std::string, vc4c::CompilationData> dummyCache;
    runTestData(dataName, useCompilationCache ? compilationCache : dummyCache);
}

void TestEmulator::runTestData(std::string dataName, vc4c::FastMap<std::string, vc4c::CompilationData>& cache)
{
    EmulationRunner runner(config, cache);
    auto test = test_data::getTest(dataName);
    auto result = test_data::execute(test, runner);
    TEST_ASSERT(result.wasSuccess)
    if(!result.error.empty())
        TEST_ASSERT_EQUALS("(no error)", result.error);
}

void TestEmulator::runNoSuchTestData(std::string dataName)
{
    TEST_ASSERT_EQUALS("(no error)", "There is no test data with the name '" + dataName + "'");
}

std::map<std::string, const test_data::TestData*> TestEmulator::getAllTestData()
{
    return test_data::getAllTests(defaultFilter);
}
