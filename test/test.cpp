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
#include "TestArithmetic.h"
#include "TestEmulator.h"
#include "TestGraph.h"
#include "TestInstructions.h"
#include "TestOperators.h"
#include "TestSPIRVFrontend.h"
#include "TestMathFunctions.h"
#include "TestIntegerFunctions.h"
#include "TestCommonFunctions.h"
#include "TestGeometricFunctions.h"
#include "TestRelationalFunctions.h"
#include "TestVectorFunctions.h"
#include "TestMemoryAccess.h"
#include "TestConversionFunctions.h"
#include "TestOptimizations.h"

#include "tools.h"
#include "../lib/cpplog/include/logger.h"
#include "RegressionTest.h"

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

static Test::Suite* newArithmeticTest()
{
    return new TestArithmetic(config);
}

static Test::Suite* newIntegerFunctionsTest()
{
    return new TestIntegerFunctions(config);
}

static Test::Suite* newCommonFunctionsTest()
{
    return new TestCommonFunctions(config);
}

static Test::Suite* newGometricFunctionsTest()
{
    return new TestGeometricFunctions(config);
}

static Test::Suite* newRelationalFunctionsTest()
{
    return new TestRelationalFunctions(config);
}

static Test::Suite* newVectorFunctionsTest()
{
    return new TestVectorFunctions(config);
}

static Test::Suite* newMemoryAccessTest()
{
    return new TestMemoryAccess(config);
}

static Test::Suite* newConversionFunctionsTest()
{
    return new TestConversionFuntions(config);
}

/*
 * 
 */
int main(int argc, char** argv)
{
    //only output errors
    logging::LOGGER.reset(new logging::ConsoleLogger(logging::Level::WARNING));

    Test::registerSuite(Test::newInstance<TestOptimizations>, "test-optimizations", "Runs smoke tests on the single optimization steps");
    Test::registerSuite(Test::newInstance<TestOperators>, "test-operators", "Tests the implementation of some operators");
    Test::registerSuite(Test::newInstance<TestInstructions>, "test-instructions", "Tests some common instruction handling");
    Test::registerSuite(Test::newInstance<TestSPIRVFrontend>, "test-spirv", "Tests the SPIR-V front-end");
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
    Test::registerSuite(newArithmeticTest, "emulate-arithmetic", "Runs emulation tests for various kind of operations");
    Test::registerSuite(newIntegerFunctionsTest, "emulate-integer", "Runs emulation tests for the OpenCL standard-library integer functions");
    Test::registerSuite(newCommonFunctionsTest, "emulate-common", "Runs emulation tests for the OpenCL standard-library common functions");
    Test::registerSuite(newGometricFunctionsTest, "emulate-geometric", "Runs emulation tests for the OpenCL standard-library geometric functions");
    Test::registerSuite(newRelationalFunctionsTest, "emulate-relational", "Runs emulation tests for the OpenCL standard-library relational functions");
    Test::registerSuite(newVectorFunctionsTest, "emulate-vector", "Runs emulation tests for the OpenCL standard-library vector functions");
    Test::registerSuite(newMemoryAccessTest, "emulate-memory", "Runs emulation tests for various functions testing different kinds of memory access");
    Test::registerSuite(newConversionFunctionsTest, "emulate-conversions", "Runs emulation tests for the OpenCL standard-library type conversion functions");

    auto args = std::vector<char*>();
    // we need this first argument, since the  cpptest-lite helper expects the first argument to be skipped (as if passed directly the main arguments)
    args.push_back(argv[0]);

    for(auto i = 1; i < argc; ++i)
    { 
        if (!vc4c::tools::parseConfigurationParameter(config, argv[i]))
            args.push_back(argv[i]);

        //TODO rewrite, actually print same help (parts of it) as VC4C
        if(std::string("--help") == argv[i] || std::string("-h") == argv[i])
            std::cout << "NOTE: This only lists the options for the 'cpptest-lite' test-suite. For more options see 'VC4C --help'!" << std::endl;
    }

    return Test::runSuites(int(args.size()), args.data());
}

