/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_EMULATOR_H
#define TEST_EMULATOR_H

#include "cpptest.h"

#include "Precompiler.h"
#include "config.h"

#include <map>
#include <vector>

namespace vc4c
{
    namespace tools
    {
        struct EmulationData;
    }
} // namespace vc4c

class TestEmulator : public Test::Suite
{
public:
    TestEmulator(const vc4c::Configuration& config = {});
    explicit TestEmulator(bool cachePrecompilation, const vc4c::Configuration& config = {});
    ~TestEmulator() override;

    void testHelloWorld();
    void testHelloWorldVector();
    void testPrime();
    void testBarrier();
    void testBranches();
    void testWorkItem();
    void testSHA1();
    void testSHA256();
    void testIntegerEmulations(std::size_t index, std::string name);
    void testFloatEmulations(std::size_t index, std::string name);
    void testPartialMD5();
    void testCRC16();
    void testPearson16();
    void testPi();

    void printProfilingInfo();

protected:
    void testIntegerEmulation(
        vc4c::tools::EmulationData& data, std::map<uint32_t, std::vector<uint32_t>>& expectedResults);
    void testFloatingEmulation(vc4c::tools::EmulationData& data,
        std::map<uint32_t, std::vector<uint32_t>>& expectedResults, unsigned maxULP = 1);

    void compileFile(std::stringstream& buffer, const std::string& fileName, const std::string& options = "",
        bool cachePrecompilation = false);

    vc4c::Configuration config;
    bool cachePrecompilation;
    std::map<std::string, vc4c::TemporaryFile> cachedPrecompilations;
};

#endif /* TEST_EMULATOR_H */
