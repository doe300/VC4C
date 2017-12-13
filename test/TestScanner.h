/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TESTSCANNER_H
#define TESTSCANNER_H

#include "llvm/Scanner.h"
#include "cpptest.h"

class TestScanner : public Test::Suite
{
public:
    TestScanner();
    ~TestScanner() override;
    
    void testEnd();
    void testInteger();
    void testFloat();
    void testString();
    void testBool();
private:

};

#endif /* TESTSCANNER_H */

