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
    void testSignals();
    void testUnpackModes();
    void testPackModes();
    void testConstantSaturations();
    void testBitfields();
    void testElementFlags();
    void testOpCodes();
    void testOpCodeProperties();
    void testHalfFloat();
    void testOpCodeFlags();
    void testOpCodeRanges();
    void testBranchConditions();

    void testRegister();
    void testImmediates();
    void testSIMDVector();
    void testValue();
    void testTypes();
    void testCompoundConstants();

    void testALUInstructions();
    void testLoadInstruction();

    void testValueRanges();
    
    void testInstructionEquality();
};

#endif /* TEST_INSTRUCTIONS_H */
