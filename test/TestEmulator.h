/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_EMULATOR_H
#define TEST_EMULATOR_H

#include "cpptest.h"

#include "TestCompilationHelper.h"
#include "config.h"
#include "performance.h"

#include <map>
#include <unordered_map>
#include <vector>

namespace test_data
{
    // Handle for the internal TestData structure
    class TestData;
} // namespace test_data

class TestEmulator : public Test::Suite, protected TestCompilationHelper
{
public:
    TestEmulator(const vc4c::Configuration& config = {});
    explicit TestEmulator(const vc4c::Configuration& config,
        std::map<std::string, const test_data::TestData*>&& testData,
        std::vector<std::string>&& additionalTestNames = {});
    TestEmulator(const vc4c::Configuration& config, std::vector<std::string>&& additionalTestNames) :
        TestEmulator(config, {}, std::move(additionalTestNames))
    {
    }
    ~TestEmulator() override;

    void printProfilingInfo();
    void runTestData(std::string dataName, bool useCompilationCache);
    void runTestData(std::string dataName, vc4c::FastMap<std::string, vc4c::CompilationData>& cache);
    void runNoSuchTestData(std::string dataName);

    static std::map<std::string, const test_data::TestData*> getAllTestData();

protected:
    std::unordered_map<std::string, vc4c::CompilationData> compilationCache;
};

#endif /* TEST_EMULATOR_H */
