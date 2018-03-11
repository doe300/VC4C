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
  void testAtomicFunction(std::size_t index, std::string name);
  void testCommonFunction(std::size_t index, std::string name);
  void testGeometricFunction(std::size_t index, std::string name);
  void testIntegerFunction(std::size_t index, std::string name);
  void testMathFunction(std::size_t index, std::string name);
  void testRelationalFunction(std::size_t index, std::string name);
  void testVectorFunction(std::size_t index, std::string name);
};

#endif /* VC4C_TEST_STDLIB_H */