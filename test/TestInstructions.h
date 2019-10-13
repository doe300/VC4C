/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_INSTRUCTIONS_H
#define TEST_INSTRUCTIONS_H

#include "cpptest.h"

// TODO rename (and all usages of --test-instructions) to something like TestBasic or TestCoreTypes
class TestInstructions : public Test::Suite
{
public:
    TestInstructions();
    ~TestInstructions() override;

    void testConditionCodes();
    void testUnpackModes();
    void testPackModes();
    void testConstantSaturations();
    void testBitfields();
    void testOpCodes();
    void testOpCodeProperties();
    void testHalfFloat();
    void testOpCodeFlags();

    void testImmediates();
    void testSIMDVector();
    void testValue();
    void testTypes();
    void testCompoundConstants();

    void testALUInstructions();
    void testLoadInstriction();
};

#endif /* TEST_INSTRUCTIONS_H */
