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
    RegressionTest(const vc4c::Frontend frontend, bool onlyRegressions = false, bool onlyFast = false);
    ~RegressionTest() override;
    
    void testRegression(std::string clFile, std::string options, vc4c::Frontend frontend);
    
    void testPending(std::string clFile, std::string options, vc4c::Frontend frontend);
    void testSlowPending(std::string clFile, std::string options, vc4c::Frontend frontend);

    void printProfilingInfo();
};

#endif /* REGRESSIONTEST_H */

