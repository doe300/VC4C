/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestEmulator.h"

#include "../src/Profiler.h"
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
#if DEBUG_MODE
    vc4c::profiler::dumpProfileResults(true);
#endif
}

struct EmulationRunner final : public test_data::TestRunner, protected TestCompilationHelper
{
    explicit EmulationRunner(const Configuration& config, FastMap<std::string, CompilationData>& cache) :
        TestCompilationHelper(config), compilationCache(cache)
    {
    }

    ~EmulationRunner() noexcept override;

    test_data::Result compile(
        const std::string& sourceCode, const std::string& options, const std::string& name) override
    try
    {
        auto it = compilationCache.find(sourceCode + options);
        CompilationData currentBinary{};
        if(it != compilationCache.end())
            currentBinary = it->second;
        else
            currentBinary = compileString(sourceCode, options, name);
        currentData.module = currentBinary;
        compilationCache.emplace(sourceCode + options, currentBinary);
        return test_data::RESULT_OK;
    }
    catch(const std::exception& err)
    {
        return test_data::Result{false, err.what()};
    }

    test_data::Result selectKernel(const std::string& name) override
    {
        currentData.kernelName = name;
        // reset kernel configuration
        currentData.parameter.clear();
        currentData.workGroup = {};
        return test_data::RESULT_OK;
    }

    test_data::Result setKernelArgument(
        std::size_t index, bool isLiteral, bool isVector, const void* byteData, std::size_t numBytes) override
    {
        if(currentData.parameter.size() <= index)
            currentData.parameter.resize(index + 1);

        if(isLiteral && !isVector)
        {
            // XXX For simplicity, treat literal vectors/structs as buffers, which is wrong for vectors, but we filter
            // those kernels out anyway beforehand
            uint32_t dummy = 0;
            std::memcpy(&dummy, byteData, std::min(numBytes, sizeof(uint32_t)));
            currentData.parameter[index] = std::make_pair(dummy, Optional<std::vector<uint32_t>>{});
        }
        else
        {
            auto bufferSize = numBytes / sizeof(uint32_t) + (numBytes % sizeof(uint32_t) != 0 ? 1 : 0);
            std::vector<uint32_t> tmp(bufferSize, 0x42424242);
            std::memcpy(tmp.data(), byteData, numBytes);
            currentData.parameter[index] = std::make_pair(0, std::move(tmp));
        }
        return test_data::RESULT_OK;
    }

    test_data::Result setWorkDimensions(const test_data::WorkDimensions& dimensions) override
    {
        currentData.workGroup.dimensions = dimensions.dimensions;
        currentData.workGroup.localSizes = dimensions.localSizes;
        currentData.workGroup.numGroups = dimensions.numGroups;
        currentData.workGroup.globalOffsets = dimensions.globalOffsets;
        return test_data::RESULT_OK;
    }

    test_data::Result execute() override
    try
    {
        currentResult.reset(new EmulationResult(emulate(currentData)));
        if(currentResult->executionSuccessful)
            return test_data::RESULT_OK;
        return test_data::Result{false, "Emulation failed!"};
    }
    catch(const std::exception& err)
    {
        return test_data::Result{false, err.what()};
    }

    test_data::Result getKernelArgument(std::size_t index, void* byteData, std::size_t bufferSize) override
    {
        if(!currentResult || index >= currentResult->results.size())
            return test_data::Result{false, "Argument index out of bounds!"};
        auto& arg = currentResult->results[index];
        if(arg.second)
        {
            const auto& vector = arg.second.value();
            std::memcpy(byteData, vector.data(), std::min(vector.size() * sizeof(uint32_t), bufferSize));
        }
        else
            std::memcpy(byteData, &arg.first, std::min(sizeof(arg.first), bufferSize));
        return test_data::RESULT_OK;
    }

    test_data::Result validateFinish() override
    {
        // nothing to be done here
        return test_data::RESULT_OK;
    }

    std::stringstream currentBinary;
    EmulationData currentData;
    std::unique_ptr<EmulationResult> currentResult;
    FastMap<std::string, CompilationData>& compilationCache;
};

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
