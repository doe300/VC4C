/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_ARITHMETIC_H
#define VC4C_TEST_ARITHMETIC_H

#include "cpptest.h"

#include "config.h"

class TestArithmetic : public Test::Suite
{
public:
    TestArithmetic(const vc4c::Configuration& config = {});

    // arithmetic operators
    void testSignedIntMultiplication();
    void testSignedShortMultiplication();
    void testSignedCharMultiplication();
    void testUnsignedIntMultiplication();
    void testUnsignedShortMultiplication();
    void testUnsignedCharMultiplication();
    void testSignedIntDivision();
    void testSignedShortDivision();
    void testSignedCharDivision();
    void testUnsignedIntDivision();
    void testUnsignedShortDivision();
    void testUnsignedCharDivision();
    void testSignedIntModulo();
    void testSignedShortModulo();
    void testSignedCharModulo();
    void testUnsignedIntModulo();
    void testUnsignedShortModulo();
    void testUnsignedCharModulo();
    void testFloatingPointDivision();

    // relational operators
    void testIntegerEquality();
    void testShortEquality();
    void testCharEquality();
    void testFloatEquality();
    void testIntegerInequality();
    void testShortInequality();
    void testCharInequality();
    void testFloatInequality();
    void testSignedIntGreater();
    void testSignedShortGreater();
    void testSignedCharGreater();
    void testUnsignedIntGreater();
    void testUnsignedShortGreater();
    void testUnsignedCharGreater();
    void testFloatGreater();
    void testSignedIntLess();
    void testSignedShortLess();
    void testSignedCharLess();
    void testUnsignedIntLess();
    void testUnsignedShortLess();
    void testUnsignedCharLess();
    void testFloatLess();
    void testSignedIntGreaterEquals();
    void testSignedShortGreaterEquals();
    void testSignedCharGreaterEquals();
    void testUnsignedIntGreaterEquals();
    void testUnsignedShortGreaterEquals();
    void testUnsignedCharGreaterEquals();
    void testFloatGreaterEquals();
    void testSignedIntLessEquals();
    void testSignedShortLessEquals();
    void testSignedCharLessEquals();
    void testUnsignedIntLessEquals();
    void testUnsignedShortLessEquals();
    void testUnsignedCharLessEquals();
    void testFloatLessEquals();

    // logical operators
    void testSignedIntSelection();
    void testSignedShortSelection();
    void testSignedCharSelection();
    void testUnsignedIntSelection();
    void testUnsignedShortSelection();
    void testUnsignedCharSelection();
    void testSignedIntAnd();
    void testSignedShortAnd();
    void testSignedCharAnd();
    void testUnsignedIntAnd();
    void testUnsignedShortAnd();
    void testUnsignedCharAnd();
    void testFloatAnd();
    void testSignedIntOr();
    void testSignedShortOr();
    void testSignedCharOr();
    void testUnsignedIntOr();
    void testUnsignedShortOr();
    void testUnsignedCharOr();
    void testFloatOr();
    //XXX
    void testSignedNot();
    void testUnsignedNot();
    void testFloatNot();
    
    //XXX vector instructions
    void testExtractElement();
    void testInsertElement();
    void testShuffleVector();
    
    //conversion instructions
    void testSignedTruncation();
    void testUnsignedTruncation();
    void testSignExtension();
    void testZeroExtension();
    void testSignedIntToFloat();
    void testSignedShortToFloat();
    void testSignedCharToFloat();
    void testUnsignedIntToFloat();
    void testUnsignedShortToFloat();
    void testUnsignedCharToFloat();
    void testFloatToSignedInt();
    void testFloatToSignedShort();
    void testFloatToSignedChar();
    void testFloatToUnsignedInt();
    void testFloatToUnsignedShort();
    void testFloatToUnsignedChar();
    //XXX
    void testVectorBitcastTruncation4To1();
    void testVectorBitcastTruncation2To1();
    void testVectorBitcastExtension1To4();
    void testVectorBitcastExtension1To2();

private:
    vc4c::Configuration config;
    
    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_ARITHMETIC_H */