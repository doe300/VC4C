/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <cstdlib>
#include <fstream>

#include "cpptest.h"
#include "cpptest-main.h"
#include "TestEmulator.h"
#include "TestInstructions.h"
#include "TestOperators.h"
#include "TestParser.h"
#include "TestScanner.h"
#include "TestSPIRVFrontend.h"
#include "TestStdlib.h"

#include "../lib/cpplog/include/logger.h"
#include "RegressionTest.h"
#include "TestInternalLibs.h"

using namespace std;

template<bool R>
static Test::Suite* newLLVMCompilationTest()
{
	return new RegressionTest(vc4c::Frontend::LLVM_IR, R);
}

template<bool R>
static Test::Suite* newSPIRVCompiltionTest()
{
	return new RegressionTest(vc4c::Frontend::SPIR_V, R);
}

template<bool R>
static Test::Suite* newCompilationTest()
{
	return new RegressionTest(vc4c::Frontend::DEFAULT, R);
}

static Test::Suite* newFastRegressionTest()
{
	return new RegressionTest(vc4c::Frontend::DEFAULT, true, true);
}

/*
 * 
 */
int main(int argc, char** argv)
{
    //only output errors
    logging::LOGGER.reset(new logging::ConsoleLogger(logging::Level::WARNING));

    Test::registerSuite(Test::newInstance<TestOperators>, "test-operators", "Tests the implementation of some operators");
    Test::registerSuite(Test::newInstance<TestScanner>, "test-scanner", "Tests the LLVM IR scanner");
    Test::registerSuite(Test::newInstance<TestParser>, "test-parser", "Tests the LLVM IR parser");
    Test::registerSuite(Test::newInstance<TestInstructions>, "test-instructions", "Tests some common instruction handling");
    Test::registerSuite(Test::newInstance<TestSPIRVFrontend>, "test-spirv", "Tests the SPIR-V front-end");
    Test::registerSuite(newLLVMCompilationTest<true>, "regressions-llvm", "Runs the regression-test using the LLVM-IR front-end", false);
    Test::registerSuite(newSPIRVCompiltionTest<true>, "regressions-spirv", "Runs the regression-test using the SPIR-V front-end", false);
    Test::registerSuite(newCompilationTest<true>, "regressions", "Runs the regression-test using the default front-end", false);
    Test::registerSuite(newCompilationTest<false>, "test-compilation", "Runs all the compilation tests using the default front-end", true);
    Test::registerSuite(newLLVMCompilationTest<false>, "test-compilation-llvm", "Runs all the compilation tests using the LLVM-IR front-end", false);
    Test::registerSuite(newSPIRVCompiltionTest<false>, "test-compilation-spirv", "Runs all the compilation tests using the SPIR-V front-end", false);
    Test::registerSuite(newFastRegressionTest, "fast-regressions", "Runs regression test-cases marked as fast", false);
    Test::registerSuite(Test::newInstance<TestEmulator>, "test-emulator", "Runs selected code-samples through the emulator");
    Test::registerSuite(Test::newInstance<TestStdlib>, "test-stdlib", "Runs most of the VC4CL std-lib functions in emulator");
    Test::registerSuite(Test::newInstance<TestInternalLibs>, "test-internal-libs", "Test common data structure");

    return Test::runSuites(argc, argv);
}

