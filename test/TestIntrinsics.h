/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_INTRINSICS_H
#define VC4C_TEST_INTRINSICS_H

#include "cpptest.h"

#include "TestEmulator.h"

class TestIntrinsicFunctions : public TestEmulator
{
public:
    TestIntrinsicFunctions(const vc4c::Configuration& config = {});

    void testIntMultiplicationWithConstant();
    void testShortMultiplicationWithConstant();
    void testCharMultiplicationWithConstant();

    void testUnsignedIntMultiplicationWithConstant();
    void testUnsignedShortMultiplicationWithConstant();
    void testUnsignedCharMultiplicationWithConstant();

    void testIntDivisionByConstant();
    void testShortDivisionByConstant();
    void testCharDivisionByConstant();
    void testIntModuloByConstant();
    void testShortModuloByConstant();
    void testCharModuloByConstant();

    void testUnsignedIntDivisionByConstant();
    void testUnsignedShortDivisionByConstant();
    void testUnsignedCharDivisionByConstant();
    void testUnsignedIntModuloByConstant();
    void testUnsignedShortModuloByConstant();
    void testUnsignedCharModuloByConstant();

    void testFloatMultiplicationWithConstant();
    void testFloatDivisionByConstant();

    // TODO fptrunc

    void testIntToFloat();
    void testShortToFloat();
    void testCharToFloat();
    void testUnsignedIntToFloat();
    void testUnsignedShortToFloat();
    void testUnsignedCharToFloat();

    void testFloatToInt();
    void testFloatToShort();
    void testFloatToChar();
    void testFloatToUnsignedInt();
    void testFloatToUnsignedShort();
    void testFloatToUnsignedChar();

    void testFtoi();
    void testItof();
    void testClz();
    void testSfuRsqrt();
    void testSfuExp2();
    void testSfuLog2();
    void testSfuRecip();
    void testIsNaN();
    void testIsInfNaN();

    void testFmax();
    void testFmin();
    void testFmaxabs();
    void testFminabs();
    void testAsr();
    void testRor();
    void testMax();
    void testMin();
    void testAnd();
    void testMul24();
    void testV8Adds();
    void testV8Subs();
    void testV8Min();
    void testV8Max();

    void testBitcastUnsignedChar();
    void testBitcastChar();
    void testBitcastUnsignedShort();
    void testBitcastShort();
    void testBitcastUnsignedInt();
    void testBitcastInt();
    void testBitcastFloat();

private:
    void onMismatch(const std::string& expected, const std::string& result);
};
#endif /* VC4C_TEST_INTRINSICS_H */
