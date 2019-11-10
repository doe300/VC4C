/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_EXPRESSIONS_H
#define TEST_EXPRESSIONS_H

#include "cpptest.h"

class TestExpressions : public Test::Suite
{
public:
    TestExpressions();
    ~TestExpressions() override;
    
    void testCreation();
    void testCombination();
    void testConvergence();
    void testValueRange();
};

#endif /* TEST_EXPRESSIONS_H */
