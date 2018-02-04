/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TESTPARSER_H
#define TESTPARSER_H

#include "cpptest.h"
#include "llvm/IRParser.h"

class TestParser : public Test::Suite
{
public:
    TestParser();
    
    bool setup() override;

    void testKernelMethod();
    void testGlobalData();
    void testStructDefinition();
    void testUnionDefinition();
    
private:
    vc4c::llvm2qasm::IRParser parser1;
    vc4c::llvm2qasm::IRParser parser2;
};

#endif /* TESTPARSER_H */

