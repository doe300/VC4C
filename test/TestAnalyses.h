/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_TEST_ANALYSES_H
#define VC4C_TEST_ANALYSES_H

#include "cpptest.h"

#include "config.h"

class TestAnalyses : public Test::Suite
{
public:
    TestAnalyses(const vc4c::Configuration& config = {});

    void testAvailableExpressions();
    void testControlFlowGraph();
    void testControlFlowLoops();
    void testDataDependency();
    void testDependency();
    void testDominatorTree();
    void testInterference();
    void testLifetime();
    void testLiveness();
    void testMemory();
    void testRegister();
    void testValueRange();
    void testStaticFlags();
    void testIntegerComparisonDetection();
    void testActiveWorkItems();
};

#endif /* VC4C_TEST_ANALYSES_H */
