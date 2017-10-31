/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <cstdlib>
#include <fstream>

#include "cpptest.h"
#include "TestScanner.h"
#include "TestParser.h"
#include "TestInstructions.h"
#include "TestSPIRVFrontend.h"

#include "../lib/cpplog/include/logger.h"
#include "RegressionTest.h"

using namespace std;

/*
 * 
 */
int main(int argc, char** argv)
{

    #if TEST_OUTPUT_CONSOLE == 1
    Test::TextOutput output(Test::TextOutput::Verbose);
    #else
    std::ofstream file;
    file.open("testResult.log", std::ios_base::out | std::ios_base::trunc);

    Test::TextOutput output(Test::TextOutput::Verbose, file);
    #endif

    //only output errors
    logging::LOGGER.reset(new logging::ConsoleLogger(logging::Level::WARNING));

    TestScanner testScanner;
    testScanner.run(output);
    
    TestParser testParser;
    testParser.run(output);
    
    TestInstructions testInstructions;
    testInstructions.run(output);

    TestSPIRVFrontend testSPIRV;
    testSPIRV.run(output);

    vc4c::Frontend frontend = vc4c::Frontend::DEFAULT;

    bool runRegressions = false;
    for(int i = 1; i < argc; ++i)
    {
    	if(std::string("LLVMIR").compare(argv[i]) == 0)
    	{
    		frontend = vc4c::Frontend::LLVM_IR;
    		printf("Running with LLVM-IR frontend...\n");
    	}
    	else if(std::string("SPIRV").compare(argv[i]) == 0)
		{
			frontend = vc4c::Frontend::SPIR_V;
			printf("Running with SPIR-V frontend...\n");
		}
    	else if(std::string("regression").compare(argv[i]) == 0)
    		runRegressions = true;
    }
    
    if(runRegressions)
    {
    	printf("Running regression test for LLVM-IR and SPIR-V front-ends...\n");
    	RegressionTest llvmRegressions(vc4c::Frontend::LLVM_IR, true);
    	llvmRegressions.run(output);
		RegressionTest spirvRegressions(vc4c::Frontend::SPIR_V, true);
		spirvRegressions.run(output);
    }
    else
    {
    	RegressionTest regressionTest(frontend);
    	regressionTest.run(output);
    }
    
    return 0;
}

