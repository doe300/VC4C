/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_OPTIMIZATIONS_H
#define VC4C_TEST_OPTIMIZATIONS_H

#include "cpptest.h"

#include "TestEmulator.h"

#include <set>
#include <string>

class TestOptimizations : public TestEmulator
{
public:
    TestOptimizations(const vc4c::Configuration& config = {});
    ~TestOptimizations() override;

    void testEmptyIterator(std::string passParamName);
    void testEmptyKernel(std::string passParamName);
    void testHelloWorld(std::string passParamName);
    void testHelloWorldVector(std::string passParamName);
    void testBarrier(std::string passParamName);
    void testBranches(std::string passParamName);
    void testWorkItem(std::string passParamName);
    void testCRC16(std::string passParamName);
    void testPearson16(std::string passParamName);

    void testFibonacci(std::string passParamName);
    void testStruct(std::string passParamName);
    void testCopy(std::string passParamName);
    void testAtomics(std::string passParamName);
    void testF2I(std::string passParamName);
    void testGlobalData(std::string passParamName);
    void testSelect(std::string passParamName);
    void testDot3Local(std::string passParamName);
    void testVectorAdd(std::string passParamName);
    void testArithmetic(std::string passParamName);
    void testClamp(std::string passParamName);
    void testCross(std::string passParamName);

    void testCharPrivateStorage(std::string passParamName);
    void testCharLocalStorage(std::string passParamName);
    void testCharGlobalStorage(std::string passParamName);
    void testShortPrivateStorage(std::string passParamName);
    void testShortLocalStorage(std::string passParamName);
    void testShortGlobalStorage(std::string passParamName);
    void testIntPrivateStorage(std::string passParamName);
    void testIntLocalStorage(std::string passParamName);
    void testIntGlobalStorage(std::string passParamName);

    void testVectorizations(std::string passParamName);
    void testStructTypeHandling(std::string passParamName);

    void checkTestQuality();

private:
    void testSHA1();
    void testSHA256();

    std::set<std::string> counterNames;
};

#endif /* VC4C_TEST_OPTIMIZATIONS_H */
