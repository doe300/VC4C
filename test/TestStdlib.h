/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_STDLIB_H
#define VC4C_TEST_STDLIB_H

#include "TestEmulator.h"

class TestStdlib : public TestEmulator
{
public:
  TestStdlib();
  
  void testAsyncCopy();
  void testMathFunction(std::size_t index, std::string name);
};

#endif /* VC4C_TEST_STDLIB_H */