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
    ~TestArithmetic() override;

    // arithmetic operators
    void testSignedIntUnaryPlus();
    void testUnsignedIntUnaryPlus();
    void testSignedShortUnaryPlus();
    void testUnsignedShortUnaryPlus();
    void testSignedCharUnaryPlus();
    void testUnsignedCharUnaryPlus();
    void testFloatUnaryPlus();
    void testSignedLongUnaryPlus();
    void testUnsignedLongUnaryPlus();

    void testSignedIntUnaryMinus();
    void testUnsignedIntUnaryMinus();
    void testSignedShortUnaryMinus();
    void testUnsignedShortUnaryMinus();
    void testSignedCharUnaryMinus();
    void testUnsignedCharUnaryMinus();
    void testFloatUnaryMinus();
    void testSignedLongUnaryMinus();
    void testUnsignedLongUnaryMinus();

    void testSignedIntAddition();
    void testUnsignedIntAddition();
    void testSignedShortAddition();
    void testUnsignedShortAddition();
    void testSignedCharAddition();
    void testUnsignedCharAddition();
    void testFloatAddition();
    void testSignedLongAddition();
    void testUnsignedLongAddition();

    void testSignedIntSubtraction();
    void testUnsignedIntSubtraction();
    void testSignedShortSubtraction();
    void testUnsignedShortSubtraction();
    void testSignedCharSubtraction();
    void testUnsignedCharSubtraction();
    void testFloatSubtraction();
    void testSignedLongSubtraction();
    void testUnsignedLongSubtraction();

    void testSignedIntMultiplication();
    void testSignedShortMultiplication();
    void testSignedCharMultiplication();
    void testUnsignedIntMultiplication();
    void testUnsignedShortMultiplication();
    void testUnsignedCharMultiplication();
    void testFloatingPointMultiplication();
    // TODO (u)long

    void testSignedIntDivision();
    void testSignedShortDivision();
    void testSignedCharDivision();
    void testUnsignedIntDivision();
    void testUnsignedShortDivision();
    void testUnsignedCharDivision();
    void testFloatingPointDivision();
    // TODO (u)long

    void testSignedIntModulo();
    void testSignedShortModulo();
    void testSignedCharModulo();
    void testUnsignedIntModulo();
    void testUnsignedShortModulo();
    void testUnsignedCharModulo();
    // for floating-point modulo, function needs to be used
    // TODO (u)long

    void testSignedIntIncrement();
    void testUnsignedIntIncrement();
    void testSignedShortIncrement();
    void testUnsignedShortIncrement();
    void testSignedCharIncrement();
    void testUnsignedCharIncrement();
    // "The arithmetic post- and pre-increment and decrement operators [...] except the built-in scalar and vector float
    // types" - OpenCL 1.2, section 6.3.b
    void testSignedLongIncrement();
    void testUnsignedLongIncrement();

    void testSignedIntDecrement();
    void testUnsignedIntDecrement();
    void testSignedShortDecrement();
    void testUnsignedShortDecrement();
    void testSignedCharDecrement();
    void testUnsignedCharDecrement();
    // "The arithmetic post- and pre-increment and decrement operators [...] except the built-in scalar and vector float
    // types" - OpenCL 1.2, section 6.3.b
    void testSignedLongDecrement();
    void testUnsignedLongDecrement();

    // relational operators
    void testIntegerEquality();
    void testShortEquality();
    void testCharEquality();
    void testFloatEquality();
    void testLongEquality();

    void testIntegerInequality();
    void testShortInequality();
    void testCharInequality();
    void testFloatInequality();
    void testLongInequality();

    void testSignedIntGreater();
    void testSignedShortGreater();
    void testSignedCharGreater();
    void testUnsignedIntGreater();
    void testUnsignedShortGreater();
    void testUnsignedCharGreater();
    void testFloatGreater();
    void testSignedLongGreater();
    void testUnsignedLongGreater();

    void testSignedIntLess();
    void testSignedShortLess();
    void testSignedCharLess();
    void testUnsignedIntLess();
    void testUnsignedShortLess();
    void testUnsignedCharLess();
    void testFloatLess();
    void testSignedLongLess();
    void testUnsignedLongLess();

    void testSignedIntGreaterEquals();
    void testSignedShortGreaterEquals();
    void testSignedCharGreaterEquals();
    void testUnsignedIntGreaterEquals();
    void testUnsignedShortGreaterEquals();
    void testUnsignedCharGreaterEquals();
    void testFloatGreaterEquals();
    void testSignedLongGreaterEquals();
    void testUnsignedLongGreaterEquals();

    void testSignedIntLessEquals();
    void testSignedShortLessEquals();
    void testSignedCharLessEquals();
    void testUnsignedIntLessEquals();
    void testUnsignedShortLessEquals();
    void testUnsignedCharLessEquals();
    void testFloatLessEquals();
    void testSignedLongLessEquals();
    void testUnsignedLongLessEquals();

    // logical operators
    void testSignedIntSelectScalar();
    void testSignedIntSelectVector();
    void testSignedShortSelectScalar();
    void testSignedShortSelectVector();
    void testSignedCharSelectScalar();
    void testSignedCharSelectVector();
    void testUnsignedIntSelectScalar();
    void testUnsignedIntSelectVector();
    void testUnsignedShortSelectScalar();
    void testUnsignedShortSelectVector();
    void testUnsignedCharSelectScalar();
    void testUnsignedCharSelectVector();
    void testSignedLongSelectScalar();
    void testSignedLongSelectVector();
    void testUnsignedLongSelectScalar();
    void testUnsignedLongSelectVector();

    void testSignedIntAnd();
    void testSignedShortAnd();
    void testSignedCharAnd();
    void testUnsignedIntAnd();
    void testUnsignedShortAnd();
    void testUnsignedCharAnd();
    void testFloatAnd();
    void testSignedLongAnd();
    void testUnsignedLongAnd();

    void testSignedIntOr();
    void testSignedShortOr();
    void testSignedCharOr();
    void testUnsignedIntOr();
    void testUnsignedShortOr();
    void testUnsignedCharOr();
    void testFloatOr();
    void testSignedLongOr();
    void testUnsignedLongOr();

    void testSignedIntNot();
    void testSignedShortNot();
    void testSignedCharNot();
    void testUnsignedIntNot();
    void testUnsignedShortNot();
    void testUnsignedCharNot();
    void testFloatNot();
    void testSignedLongNot();
    void testUnsignedLongNot();

    // bit operators
    void testSignedIntBitNot();
    void testUnsignedIntBitNot();
    void testSignedShortBitNot();
    void testUnsignedShortBitNot();
    void testSignedCharBitNot();
    void testUnsignedCharBitNot();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f
    void testSignedLongBitNot();
    void testUnsignedLongBitNot();

    void testSignedIntBitAnd();
    void testUnsignedIntBitAnd();
    void testSignedShortBitAnd();
    void testUnsignedShortBitAnd();
    void testSignedCharBitAnd();
    void testUnsignedCharBitAnd();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f
    void testSignedLongBitAnd();
    void testUnsignedLongBitAnd();

    void testSignedIntBitOr();
    void testUnsignedIntBitOr();
    void testSignedShortBitOr();
    void testUnsignedShortBitOr();
    void testSignedCharBitOr();
    void testUnsignedCharBitOr();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f
    void testSignedLongBitOr();
    void testUnsignedLongBitOr();

    void testSignedIntBitXor();
    void testUnsignedIntBitXor();
    void testSignedShortBitXor();
    void testUnsignedShortBitXor();
    void testSignedCharBitXor();
    void testUnsignedCharBitXor();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f
    void testSignedLongBitXor();
    void testUnsignedLongBitXor();

    void testSignedIntBitShiftLeft();
    void testUnsignedIntBitShiftLeft();
    void testSignedShortBitShiftLeft();
    void testUnsignedShortBitShiftLeft();
    void testSignedCharBitShiftLeft();
    void testUnsignedCharBitShiftLeft();
    // "The operators right-shift, left-shift [...] except the built-in scalar and vector float types" - OpenCL 1.2,
    // section 6.3.j
    void testSignedLongBitShiftLeft();
    void testUnsignedLongBitShiftLeft();

    void testSignedIntBitShiftRight();
    void testUnsignedIntBitShiftRight();
    void testSignedShortBitShiftRight();
    void testUnsignedShortBitShiftRight();
    void testSignedCharBitShiftRight();
    void testUnsignedCharBitShiftRight();
    // "The operators right-shift, left-shift [...] except the built-in scalar and vector float types" - OpenCL 1.2,
    // section 6.3.j
    void testSignedLongBitShiftRight();
    void testUnsignedLongBitShiftRight();

    void testSignedIntTrinaryScalar();
    void testSignedIntTrinaryVector();
    void testSignedShortTrinaryScalar();
    void testSignedShortTrinaryVector();
    void testSignedCharTrinaryScalar();
    void testSignedCharTrinaryVector();
    void testSignedLongTrinaryScalar();
    void testSignedLongTrinaryVector();

    void testUnsignedIntTrinaryScalar();
    void testUnsignedIntTrinaryVector();
    void testUnsignedShortTrinaryScalar();
    void testUnsignedShortTrinaryVector();
    void testUnsignedCharTrinaryScalar();
    void testUnsignedCharTrinaryVector();
    void testUnsignedLongTrinaryScalar();
    void testUnsignedLongTrinaryVector();

private:
    vc4c::Configuration config;

    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_ARITHMETIC_H */
