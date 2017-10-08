/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef REGRESSIONTEST_H
#define REGRESSIONTEST_H

#include "cpptest.h"
#include "Compiler.h"

class RegressionTest : public Test::Suite
{
public:
    RegressionTest();
    
    void testRegression(std::string clFile, std::string options);
    
    void testPending(std::string clFile, std::string options);
    void testSlowPending(std::string clFile, std::string options);

    void printProfilingInfo();
};

#endif /* REGRESSIONTEST_H */

