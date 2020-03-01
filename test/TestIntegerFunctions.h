/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_INTEGER_FUNCTIONS_H
#define VC4C_TEST_INTEGER_FUNCTIONS_H

#include "cpptest.h"

#include "config.h"

class TestIntegerFunctions : public Test::Suite
{
public:
    TestIntegerFunctions(const vc4c::Configuration& config = {});
    ~TestIntegerFunctions() override;
    
    void testAbsSignedInt();
    void testAbsSignedShort();
    void testAbsSignedChar();
    void testAbsUnsignedInt();
    void testAbsUnsignedShort();
    void testAbsUnsignedChar();
    
    void testAbsDiffSignedInt();
    void testAbsDiffSignedShort();
    void testAbsDiffSignedChar();
    void testAbsDiffUnsignedInt();
    void testAbsDiffUnsignedShort();
    void testAbsDiffUnsignedChar();
    
    void testAddSatSignedInt();
    void testAddSatSignedShort();
    void testAddSatSignedChar();
    void testAddSatUnsignedInt();
    void testAddSatUnsignedShort();
    void testAddSatUnsignedChar();
    
    void testHAddSignedInt();
    void testHAddSignedShort();
    void testHAddSignedChar();
    void testHAddUnsignedInt();
    void testHAddUnsignedShort();
    void testHAddUnsignedChar();
    
    void testRHAddSignedInt();
    void testRHAddSignedShort();
    void testRHAddSignedChar();
    void testRHAddUnsignedInt();
    void testRHAddUnsignedShort();
    void testRHAddUnsignedChar();
    
    void testClampSignedInt();
    void testClampSignedShort();
    void testClampSignedChar();
    void testClampUnsignedInt();
    void testClampUnsignedShort();
    void testClampUnsignedChar();
    
    void testClzSignedInt();
    void testClzSignedShort();
    void testClzSignedChar();
    void testClzUnsignedInt();
    void testClzUnsignedShort();
    void testClzUnsignedChar();
    
    void testMadHiSignedInt();
    void testMadHiSignedShort();
    void testMadHiSignedChar();
    void testMadHiUnsignedInt();
    void testMadHiUnsignedShort();
    void testMadHiUnsignedChar();
    
    void testMadSatSignedInt();
    void testMadSatSignedShort();
    void testMadSatSignedChar();
    void testMadSatUnsignedInt();
    void testMadSatUnsignedShort();
    void testMadSatUnsignedChar();
    
    void testMaxSignedInt();
    void testMaxSignedShort();
    void testMaxSignedChar();
    void testMaxUnsignedInt();
    void testMaxUnsignedShort();
    void testMaxUnsignedChar();
    void testMaxSignedLong();
    void testMaxUnsignedLong();
    
    void testMinSignedInt();
    void testMinSignedShort();
    void testMinSignedChar();
    void testMinUnsignedInt();
    void testMinUnsignedShort();
    void testMinUnsignedChar();
    void testMinSignedLong();
    void testMinUnsignedLong();
    
    void testMulHiSignedInt();
    void testMulHiSignedShort();
    void testMulHiSignedChar();
    void testMulHiUnsignedInt();
    void testMulHiUnsignedShort();
    void testMulHiUnsignedChar();
    
    void testRotateSignedInt();
    void testRotateSignedShort();
    void testRotateSignedChar();
    void testRotateUnsignedInt();
    void testRotateUnsignedShort();
    void testRotateUnsignedChar();
    
    void testSubSatSignedInt();
    void testSubSatSignedShort();
    void testSubSatSignedChar();
    void testSubSatUnsignedInt();
    void testSubSatUnsignedShort();
    void testSubSatUnsignedChar();
    
    void testUpsampleSignedShortToInt();
    void testUpsampleSignedCharToShort();
    void testUpsampleUnsignedShortToInt();
    void testUpsampleUnsignedCharToShort();
    void testUpsampleSignedIntToLong();
    void testUpsampleUnsignedIntToLong();
    
    void testPopcountSignedInt();
    void testPopcountSignedShort();
    void testPopcountSignedChar();
    void testPopcountUnsignedInt();
    void testPopcountUnsignedShort();
    void testPopcountUnsignedChar();
    void testPopcountSignedLong();
    void testPopcountUnsignedLong();
    
    void testMad24SignedInt();
    void testMad24UnsignedInt();
    
    void testMul24SignedInt();
    void testMul24UnsignedInt();

private:
    vc4c::Configuration config;
    
    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_INTEGER_FUNCTIONS_H */
