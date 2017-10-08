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
    
    
    RegressionTest regressionTest;
    regressionTest.run(output);
    
    return 0;
}

