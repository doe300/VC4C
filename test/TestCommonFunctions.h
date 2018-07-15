/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_COMMON_FUNCTIONS_H
#define VC4C_TEST_COMMON_FUNCTIONS_H

#include "cpptest.h"

#include "config.h"

class TestCommonFunctions : public Test::Suite
{
public:
  TestCommonFunctions(const vc4c::Configuration& config = {});
  
  void testClamp();
  void testDegrees();
  void testMax();
  void testMin();
  void testMix();
  void testRadians();
  void testStep();
  void testSmoothStep();
  void testSign();
  
private:
    vc4c::Configuration config;
    
    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_COMMON_FUNCTIONS_H */