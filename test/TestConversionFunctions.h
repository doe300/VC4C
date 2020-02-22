/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_CONVERSION_FUNCTIONS_H
#define VC4C_TEST_CONVERSION_FUNCTIONS_H

#include "cpptest.h"

#include "TestEmulator.h"

class TestConversionFuntions : public TestEmulator
{
public:
    explicit TestConversionFuntions(const vc4c::Configuration& config = {});
    ~TestConversionFuntions() override;

    // conversion instructions
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

    void testSaturateSignedIntToUnsignedInt();
    void testSaturateUnsignedIntToSignedInt();
    void testSaturateFloatToSignedInt();
    void testSaturateFloatToUnsignedInt();
    void testSaturateSignedIntToSignedShort();
    void testSaturateSignedIntToUnsignedShort();
    void testSaturateUnsignedIntToSignedShort();
    void testSaturateUnsignedIntToUnsignedShort();
    void testSaturateFloatToSignedShort();
    void testSaturateFloatToUnsignedShort();
    void testSaturateSignedIntToSignedChar();
    void testSaturateSignedIntToUnsignedChar();
    void testSaturateUnsignedIntToSignedChar();
    void testSaturateUnsignedIntToUnsignedChar();
    void testSaturateSignedShortToUnsignedShort();
    void testSaturateUnsignedShortToSignedShort();
    void testSaturateSignedShortToSignedChar();
    void testSaturateSignedShortToUnsignedChar();
    void testSaturateUnsignedShortToSignedChar();
    void testSaturateUnsignedShortToUnsignedChar();
    void testSaturateFloatToSignedChar();
    void testSaturateFloatToUnsignedChar();
    void testSaturateSignedCharToUnsignedChar();
    void testSaturateUnsignedCharToSignedChar();
    // TODO with rounding modes

    void testVectorBitcastTruncation8To1();
    void testVectorBitcastTruncation4To1();
    void testVectorBitcastTruncation2To1();
    void testVectorBitcastExtension1To8();
    void testVectorBitcastExtension1To4();
    void testVectorBitcastExtension1To2();

private:
    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_CONVERSION_FUNCTIONS_H */
