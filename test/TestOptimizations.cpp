/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestOptimizations.h"

#include "InstructionWalker.h"
#include "Method.h"
#include "Module.h"
#include "emulation_helper.h"
#include "intermediate/IntermediateInstruction.h"
#include "optimization/Optimizer.h"
#include "tools.h"

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
#include "../testing/test_hashes.cl"
#pragma GCC diagnostic pop

using namespace vc4c;
using namespace vc4c::tools;

// The emulator requires this optimization to execute more than 1 work group
// TODO can we somehow get rid of this?
static const std::string requiredOptimization = "loop-work-groups";

TestOptimizations::TestOptimizations(const vc4c::Configuration& config) : TestEmulator(config, {}, {})
{
    // test once all functions without optimizations enabled
    TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorld, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorldVector, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testBarrier, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testBranches, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testWorkItem, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCRC16, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testPearson16, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testFibonacci, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testStruct, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCopy, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testAtomics, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testF2I, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testGlobalData, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testSelect, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testDot3Local, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testVectorAdd, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testArithmetic, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testClamp, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCross, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCharPrivateStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCharLocalStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCharGlobalStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testShortPrivateStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testShortLocalStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testShortGlobalStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testIntPrivateStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testIntLocalStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testIntGlobalStorage, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testVectorizations, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testStructTypeHandling, "");

    for(const auto& pass : optimizations::Optimizer::ALL_PASSES)
    {
        TEST_ADD_WITH_STRING(TestOptimizations::testEmptyIterator, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testEmptyKernel, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorld, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorldVector, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testBarrier, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testBranches, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testWorkItem, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCRC16, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testPearson16, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testFibonacci, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testStruct, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCopy, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testAtomics, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testF2I, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testGlobalData, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testSelect, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testDot3Local, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testVectorAdd, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testArithmetic, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testClamp, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCross, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCharPrivateStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCharLocalStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCharGlobalStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testShortPrivateStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testShortLocalStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testShortGlobalStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testIntPrivateStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testIntLocalStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testIntGlobalStorage, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testVectorizations, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testStructTypeHandling, pass.parameterName);
    }
    TEST_ADD(TestOptimizations::printProfilingInfo);
}

TestOptimizations::~TestOptimizations() = default;

static const optimizations::OptimizationPass& getPass(std::string&& paramName)
{
    for(const auto& pass : optimizations::Optimizer::ALL_PASSES)
    {
        if(pass.parameterName == paramName)
        {
            return pass;
        }
    }
    throw CompilationError(CompilationStep::GENERAL, "Pass does not exist", paramName);
}

void TestOptimizations::testEmptyIterator(std::string passParamName)
{
    /*
     * In process of running the optimizations it can happen that an instruction is removed, but the iterator is not
     * erased. This can e.g. happen if the iterator need to remain stable for tracking purposes.
     *
     * This test just checks that no optimization steps throw exceptions or crash when finding an empty iterator.
     */
    Module mod{config};
    Method method{mod};
    method.appendToEnd(std::make_unique<intermediate::BranchLabel>(*method.addNewLocal(TYPE_LABEL).local()));
    method.appendToEnd(std::make_unique<intermediate::Nop>(intermediate::DelayType::WAIT_REGISTER));
    method.appendToEnd(std::make_unique<intermediate::BranchLabel>(*method.addNewLocal(TYPE_LABEL).local()));

    method.appendToEnd(nullptr);

    getPass(std::move(passParamName))(mod, method, {});
}

void TestOptimizations::testEmptyKernel(std::string passParamName)
{
    Module mod{config};
    Method method{mod};

    getPass(std::move(passParamName))(mod, method, {});
}

void TestOptimizations::testHelloWorld(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("hello_world", false);
}

void TestOptimizations::testHelloWorldVector(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("hello_world_vector", false);
}

void TestOptimizations::testBarrier(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    FastMap<std::string, CompilationData> cache{};
    TestEmulator::runTestData("barrier_dynamic_work_size", cache);
    TestEmulator::runTestData("barrier_fix_work_size", cache);
}

void TestOptimizations::testBranches(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("branches", false);
}

void TestOptimizations::testWorkItem(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    FastMap<std::string, CompilationData> cache{};
    TestEmulator::runTestData("work_item", cache);
    TestEmulator::runTestData("work_item_global_offset", cache);
}

void TestOptimizations::testCRC16(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("CRC16", false);
}

void TestOptimizations::testPearson16(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("Pearson16", false);
}

void TestOptimizations::testFibonacci(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("fibonacci", false);
}

void TestOptimizations::testStruct(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("struct", false);
}

void TestOptimizations::testCopy(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("copy_vector", false);
}

void TestOptimizations::testAtomics(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("atomics", false);
}

void TestOptimizations::testF2I(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("f2i", false);
}

void TestOptimizations::testGlobalData(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("global_data", false);
}

void TestOptimizations::testSelect(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("OpenCL_CTS_uchar_compare", false);
}

void TestOptimizations::testDot3Local(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("bug_local_memory_dot3_local", false);
}

void TestOptimizations::testVectorAdd(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("VectorAdd", false);
}

void TestOptimizations::testArithmetic(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("vector_arithmetic", false);
}

void TestOptimizations::testClamp(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("OpenCL_CTS_clamp", false);
}

void TestOptimizations::testCross(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("OpenCL_CTS_cross", false);
}

void TestOptimizations::testSHA1()
{
    const std::string sample("Hello World!");
    const std::vector<uint32_t> digest = {0x2ef7bde6, 0x08ce5404, 0xe97d5f04, 0x2f95f89f, 0x1c232871};

    auto module = compileFile("./example/md5.cl", "", true);

    EmulationData data;
    data.kernelName = "sha1_crypt_kernel";
    data.maxEmulationCycles = maxExecutionCycles;
    data.module = module;

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

void TestOptimizations::testSHA256()
{
    const std::string sample("Hello World!1111");
    const std::vector<uint32_t> digest = {
        0xf90a1ef4, 0x422350ca, 0x8c448530, 0xa7d5d0b2, 0x35054803, 0xf7b2a73d, 0x86f4b639, 0x4b1329a5};

    auto module = compileFile("./example/SHA-256.cl", "", true);

    EmulationData data;
    data.kernelName = "execute_sha256_cpu";
    data.maxEmulationCycles = maxExecutionCycles * 4;
    data.module = module;

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

void TestOptimizations::testCharPrivateStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_private_char", false);
}

void TestOptimizations::testCharLocalStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_local_char", false);
}

void TestOptimizations::testCharGlobalStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_global_char", false);
}

void TestOptimizations::testShortPrivateStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_private_short", false);
}

void TestOptimizations::testShortLocalStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_local_short", false);
}

void TestOptimizations::testShortGlobalStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_global_short", false);
}

void TestOptimizations::testIntPrivateStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_private_int", false);
}

void TestOptimizations::testIntLocalStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_local_int", false);
}

void TestOptimizations::testIntGlobalStorage(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("storage_global_int", false);
}

void TestOptimizations::testVectorizations(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName), requiredOptimization};
    config.optimizationLevel = OptimizationLevel::NONE;

    FastMap<std::string, CompilationData> cache{};
    TestEmulator::runTestData("vectorization1", cache);
    TestEmulator::runTestData("vectorization2", cache);
    TestEmulator::runTestData("vectorization3", cache);
    TestEmulator::runTestData("vectorization4", cache);
    TestEmulator::runTestData("vectorization5", cache);
    TestEmulator::runTestData("vectorization6", cache);
    TestEmulator::runTestData("vectorization7", cache);
    TestEmulator::runTestData("vectorization8", cache);
    TestEmulator::runTestData("vectorization9", cache);
    TestEmulator::runTestData("vectorization10", cache);
    TestEmulator::runTestData("vectorization11", cache);
    TestEmulator::runTestData("vectorization12", cache);
    TestEmulator::runTestData("vectorization12_partial", cache);
    TestEmulator::runTestData("vectorization13", cache);
    TestEmulator::runTestData("vectorization13_partial", cache);
    TestEmulator::runTestData("vectorization14", cache);
    TestEmulator::runTestData("vectorization14_partial", cache);
    TestEmulator::runTestData("vectorization15", cache);
    TestEmulator::runTestData("vectorization16", cache);
    TestEmulator::runTestData("vectorization17", cache);
    TestEmulator::runTestData("vectorization18", cache);
    TestEmulator::runTestData("vectorization19", cache);
}

void TestOptimizations::testStructTypeHandling(std::string passParamName)
{
    config.additionalEnabledOptimizations = {std::move(passParamName)};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::runTestData("boost_user_defined_types", false);
}
