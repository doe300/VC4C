/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#pragma once

#include "cpptest.h"

namespace vc4c
{
    class Method;
}

class TestOptimizationSteps : public Test::Suite
{
public:
    TestOptimizationSteps();

    void testCombineSelectionWithZero();
    void testCombineSettingSameFlags();
    void testCombineSettingFlagsWithOutput();
    void testFoldConstants();
    void testSimplifyArithmetics();
    void testCombineArithmetics();
    void testRewriteConstantSFU();

    void testReorderBasicBlocks();
    void testSimplifyBranches();
    void testMergeBasicBlocks();
    void testCombineConstantLoads();
    void testEliminateBitOperations();
    void testCombineRotations();
    void testEliminateMoves();
    void testRemoveFlags();
    void testRemoveConditionalFlags();
    void testCombineVectorElementCopies();
    void testLoopInvariantCodeMotion();

private:
    void testMethodsEquals(vc4c::Method& m1, vc4c::Method& m2);
};
