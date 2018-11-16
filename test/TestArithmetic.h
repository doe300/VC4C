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
    void testSignedIntUnaryPlus();
    void testUnsignedIntUnaryPlus();
    void testSignedShortUnaryPlus();
    void testUnsignedShortUnaryPlus();
    void testSignedCharUnaryPlus();
    void testUnsignedCharUnaryPlus();
    void testFloatUnaryPlus();

    void testSignedIntUnaryMinus();
    void testUnsignedIntUnaryMinus();
    void testSignedShortUnaryMinus();
    void testUnsignedShortUnaryMinus();
    void testSignedCharUnaryMinus();
    void testUnsignedCharUnaryMinus();
    void testFloatUnaryMinus();

    void testSignedIntAddition();
    void testUnsignedIntAddition();
    void testSignedShortAddition();
    void testUnsignedShortAddition();
    void testSignedCharAddition();
    void testUnsignedCharAddition();
    void testFloatAddition();

    void testSignedIntSubtraction();
    void testUnsignedIntSubtraction();
    void testSignedShortSubtraction();
    void testUnsignedShortSubtraction();
    void testSignedCharSubtraction();
    void testUnsignedCharSubtraction();
    void testFloatSubtraction();

    void testSignedIntMultiplication();
    void testSignedShortMultiplication();
    void testSignedCharMultiplication();
    void testUnsignedIntMultiplication();
    void testUnsignedShortMultiplication();
    void testUnsignedCharMultiplication();
    void testFloatingPointMultiplication();

    void testSignedIntDivision();
    void testSignedShortDivision();
    void testSignedCharDivision();
    void testUnsignedIntDivision();
    void testUnsignedShortDivision();
    void testUnsignedCharDivision();
    void testFloatingPointDivision();

    void testSignedIntModulo();
    void testSignedShortModulo();
    void testSignedCharModulo();
    void testUnsignedIntModulo();
    void testUnsignedShortModulo();
    void testUnsignedCharModulo();
    // for floating-point modulo, function needs to be used

    void testSignedIntIncrement();
    void testUnsignedIntIncrement();
    void testSignedShortIncrement();
    void testUnsignedShortIncrement();
    void testSignedCharIncrement();
    void testUnsignedCharIncrement();
    // "The arithmetic post- and pre-increment and decrement operators [...] except the built-in scalar and vector float
    // types" - OpenCL 1.2, section 6.3.b

    void testSignedIntDecrement();
    void testUnsignedIntDecrement();
    void testSignedShortDecrement();
    void testUnsignedShortDecrement();
    void testSignedCharDecrement();
    void testUnsignedCharDecrement();
    // "The arithmetic post- and pre-increment and decrement operators [...] except the built-in scalar and vector float
    // types" - OpenCL 1.2, section 6.3.b

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
    // XXX following block
    void testSignedIntNot();
    void testSignedShortNot();
    void testSignedCharNot();
    void testUnsignedIntNot();
    void testUnsignedShortNot();
    void testUnsignedCharNot();
    void testFloatNot();

    // bit operators
    void testSignedIntBitNot();
    void testUnsignedIntBitNot();
    void testSignedShortBitNot();
    void testUnsignedShortBitNot();
    void testSignedCharBitNot();
    void testUnsignedCharBitNot();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f

    void testSignedIntBitAnd();
    void testUnsignedIntBitAnd();
    void testSignedShortBitAnd();
    void testUnsignedShortBitAnd();
    void testSignedCharBitAnd();
    void testUnsignedCharBitAnd();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f

    void testSignedIntBitOr();
    void testUnsignedIntBitOr();
    void testSignedShortBitOr();
    void testUnsignedShortBitOr();
    void testSignedCharBitOr();
    void testUnsignedCharBitOr();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f

    void testSignedIntBitXor();
    void testUnsignedIntBitXor();
    void testSignedShortBitXor();
    void testUnsignedShortBitXor();
    void testSignedCharBitXor();
    void testUnsignedCharBitXor();
    // "The bitwise operators [...] except the built-in scalar and vector float types" - OpenCL 1.2, section 6.3.f

    // XXX following block
    void testSignedIntBitShiftLeft();
    void testUnsignedIntBitShiftLeft();
    void testSignedShortBitShiftLeft();
    void testUnsignedShortBitShiftLeft();
    void testSignedCharBitShiftLeft();
    void testUnsignedCharBitShiftLeft();
    // "The operators right-shift, left-shift [...] except the built-in scalar and vector float types" - OpenCL 1.2,
    // section 6.3.j

    // XXX following block
    void testSignedIntBitShiftRight();
    void testUnsignedIntBitShiftRight();
    void testSignedShortBitShiftRight();
    void testUnsignedShortBitShiftRight();
    void testSignedCharBitShiftRight();
    void testUnsignedCharBitShiftRight();
    // "The operators right-shift, left-shift [...] except the built-in scalar and vector float types" - OpenCL 1.2,
    // section 6.3.j

private:
    vc4c::Configuration config;

    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_ARITHMETIC_H */