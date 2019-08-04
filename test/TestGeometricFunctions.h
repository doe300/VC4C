/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_GEOMETRIC_FUNCTIONS_H
#define VC4C_TEST_GEOMETRIC_FUNCTIONS_H

#include "cpptest.h"

#include "config.h"

class TestGeometricFunctions : public Test::Suite
{
public:
    TestGeometricFunctions(const vc4c::Configuration& config = {});
    ~TestGeometricFunctions() override;
    
    void testCross3();
    void testCross4();
    
    void testDotScalar();
    void testDot2();
    void testDot3();
    void testDot4();
    
    void testDistanceScalar();
    void testDistance2();
    void testDistance3();
    void testDistance4();
    
    void testLengthScalar();
    void testLength2();
    void testLength3();
    void testLength4();
    
    void testNormalizeScalar();
    void testNormalize2();
    void testNormalize3();
    void testNormalize4();

private:
    vc4c::Configuration config;
    
    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_GEOMETRIC_FUNCTIONS_H */
