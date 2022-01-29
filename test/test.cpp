/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <cstdlib>
#include <fstream>
#include <vector>

#include "cpptest.h"
#include "cpptest-main.h"
#include "TestEmulator.h"
#include "TestGraph.h"
#include "TestInstructions.h"
#include "TestOperators.h"
#include "TestFrontends.h"
#include "TestMathFunctions.h"
#include "TestOptimizations.h"
#include "TestIntrinsics.h"
#include "TestExpressions.h"
#include "TestPatternMatching.h"
#include "TestOptimizationSteps.h"
#include "TestCustomContainers.h"
#include "TestAnalyses.h"
#include "TestRegisterFixes.h"

#include "tools.h"
#include "logger.h"
#include "RegressionTest.h"
#include "TestData.h"

//TODO test for compact optional!

using namespace std;

static vc4c::Configuration config;

template<bool R>
static Test::Suite* newLLVMCompilationTest()
{
	return new RegressionTest(config, vc4c::Frontend::LLVM_IR, R);
}

template<bool R>
static Test::Suite* newSPIRVCompiltionTest()
{
	return new RegressionTest(config, vc4c::Frontend::SPIR_V, R);
}

template<bool R>
static Test::Suite* newCompilationTest()
{
	return new RegressionTest(config, vc4c::Frontend::DEFAULT, R);
}

static Test::Suite* newFastRegressionTest()
{
	return new RegressionTest(config, vc4c::Frontend::DEFAULT, true, true);
}

static Test::Suite* newEmulatorTest()
{
    return new TestEmulator(config);
}

static Test::Suite* newMathFunctionsTest()
{
    return new TestMathFunctions(config);
}

static Test::Suite* newIntrinsicsTest()
{
    return new TestIntrinsicFunctions(config);
}

static Test::Suite* newOptimizationsTest()
{
    return new TestOptimizations(config);
}

static Test::Suite* newRegisterFixupTest()
{
    return new TestRegisterFixes(config);
}

/*
 * 
 */
int main(int argc, char** argv)
{
    //only output errors
    logging::DEFAULT_LOGGER.reset(new logging::ConsoleLogger(logging::Level::WARNING));

    Test::registerSuite(Test::newInstance<TestOptimizationSteps>, "test-optimization-steps", "Runs unit tests on the single optimization steps");
    Test::registerSuite(newOptimizationsTest, "test-optimizations", "Runs smoke tests on the single optimization steps");
    Test::registerSuite(newRegisterFixupTest, "test-register-fixes", "Runs smoke tests on the register fix-up steps");
    Test::registerSuite(Test::newInstance<TestOperators>, "test-operators", "Tests the implementation of some operators");
    Test::registerSuite(Test::newInstance<TestInstructions>, "test-instructions", "Tests some common instruction handling");
    Test::registerSuite(Test::newInstance<TestFrontends>, "test-frontend", "Tests various functions of the default front-end");
    Test::registerSuite(Test::newInstance<TestExpressions>, "test-expressions", "Tests the internal expression handling");
    Test::registerSuite(newLLVMCompilationTest<true>, "regressions-llvm", "Runs the regression-test using the LLVM-IR front-end", false);
    Test::registerSuite(newSPIRVCompiltionTest<true>, "regressions-spirv", "Runs the regression-test using the SPIR-V front-end", false);
    Test::registerSuite(newCompilationTest<true>, "regressions", "Runs the regression-test using the default front-end", false);
    Test::registerSuite(newCompilationTest<false>, "test-compilation", "Runs all the compilation tests using the default front-end", true);
    Test::registerSuite(newLLVMCompilationTest<false>, "test-compilation-llvm", "Runs all the compilation tests using the LLVM-IR front-end", false);
    Test::registerSuite(newSPIRVCompiltionTest<false>, "test-compilation-spirv", "Runs all the compilation tests using the SPIR-V front-end", false);
    Test::registerSuite(newFastRegressionTest, "fast-regressions", "Runs regression test-cases marked as fast", false);
    
    Test::registerSuite(newEmulatorTest, "test-emulator", "Runs selected code-samples through the emulator");
    Test::registerSuite(newMathFunctionsTest, "emulate-math", "Runs emulation tests for the OpenCL standard-library math functions");
    Test::registerSuite(Test::newInstance<TestGraph>, "test-graph", "Runs basic test for the graph data structure");
    Test::registerSuite(newIntrinsicsTest, "test-intrinsics", "Runs tests on the code generated for intrinsic functions");
    Test::registerSuite(Test::newInstance<TestPatternMatching>, "test-patterns", "Runs tests on the pattern matching framework");
    Test::registerSuite(Test::newInstance<TestCustomContainers>, "test-container", "Runs tests on the custom container types");
    Test::registerSuite(Test::newInstance<TestAnalyses>, "test-analyses", "Runs tests on the analysis algorithms");

    auto args = std::vector<char*>();
    // we need this first argument, since the  cpptest-lite helper expects the first argument to be skipped (as if passed directly the main arguments)
    args.push_back(argv[0]);

    // manually put together the list of selected emulation tests
    std::vector<std::string> emulationTestNames;

    for(auto i = 1; i < argc; ++i)
    {
        if (!test_data::parseTestDataParameter(argv[i], emulationTestNames) && !vc4c::tools::parseConfigurationParameter(config, argv[i]))
            args.emplace_back(argv[i]);

        //TODO rewrite, actually print same help (parts of it) as VC4C
        if(std::string("--help") == argv[i] || std::string("-h") == argv[i])
            std::cout << "NOTE: This only lists the options for the 'cpptest-lite' test-suite. For more options see 'VC4C --help'!" << std::endl;
    }

    if(!emulationTestNames.empty())
    {
        args.emplace_back(const_cast<char*>("--custom-test-data"));
        Test::registerSuite([&]() -> Test::Suite* {
            return new TestEmulator(config, std::vector<std::string>(emulationTestNames.begin(), emulationTestNames.end()));
        }, "custom-test-data");
    }

    return Test::runSuites(int(args.size()), args.data());
}

