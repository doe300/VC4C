/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestOptimizations.h"

#include "InstructionWalker.h"
#include "Method.h"
#include "Module.h"
#include "intermediate/IntermediateInstruction.h"
#include "optimization/Optimizer.h"

using namespace vc4c;

TestOptimizations::TestOptimizations() : TestEmulator(true)
{
    // test once all functions without optimizations enabled
    TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorld, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorldVector, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testBarrier, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testBranches, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testWorkItem, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testFibonacci, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testStruct, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCopy, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testAtomics, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testF2I, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testGlobalData, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testSelect, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testDot3Local, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testVectorAdd, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testArithmetic, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testClamp, "");
    TEST_ADD_WITH_STRING(TestOptimizations::testCross, "");
    
    for(const auto& pass : optimizations::Optimizer::ALL_PASSES)
    {
        TEST_ADD_WITH_STRING(TestOptimizations::testEmptyIterator, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testEmptyKernel, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorld, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testHelloWorldVector, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testBarrier, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testBranches, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testWorkItem, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testFibonacci, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testStruct, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCopy, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testAtomics, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testF2I, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testGlobalData, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testSelect, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testDot3Local, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testVectorAdd, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testArithmetic, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testClamp, pass.parameterName);
        TEST_ADD_WITH_STRING(TestOptimizations::testCross, pass.parameterName);
    }
    //TODO the profiling info is wrong, since all optimization counters get merged!
    TEST_ADD(TestEmulator::printProfilingInfo);
}

static const optimizations::OptimizationPass& getPass(const std::string& paramName)
{
    for(const auto& pass : optimizations::Optimizer::ALL_PASSES)
    {
        if(pass.parameterName == paramName)
        {
            return pass;
        }
    }
    throw CompilationError(CompilationStep::GENERAL, "Pass does not exist", paramName);
}

void TestOptimizations::testEmptyIterator(std::string passParamName)
{
    /*
     * In process of running the optimizations it can happen that an instruction is removed, but the iterator is not
     * erased. This can e.g. happen if the iterator need to remain stable for tracking purposes.
     *
     * This test just checks that no optimization steps throw exceptions or crash when finding an empty iterator.
     */
    Module mod{{}};
    Method method{mod};
    method.appendToEnd(new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL).local()));
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::WAIT_REGISTER));
    method.appendToEnd(new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL).local()));

    method.appendToEnd(nullptr);

    getPass(passParamName)(mod, method, {});
}

void TestOptimizations::testEmptyKernel(std::string passParamName)
{
    Module mod{{}};
    Method method{mod};

    getPass(passParamName)(mod, method, {});
}

void TestOptimizations::testHelloWorld(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testHelloWorld();
}

void TestOptimizations::testHelloWorldVector(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testHelloWorldVector();
}

void TestOptimizations::testBarrier(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testBarrier();
}

void TestOptimizations::testBranches(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testBranches();
}

void TestOptimizations::testWorkItem(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testWorkItem();
}

void TestOptimizations::testFibonacci(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(0, "fibonacci");
}

void TestOptimizations::testStruct(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(2, "test_struct");
}

void TestOptimizations::testCopy(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(3, "test_copy");
}

void TestOptimizations::testAtomics(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(4, "test_atomics");
}

void TestOptimizations::testF2I(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(5, "test_f2i");
}

void TestOptimizations::testGlobalData(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(6, "test_global_data");
}

void TestOptimizations::testSelect(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testIntegerEmulations(14, "test_select");
}

void TestOptimizations::testDot3Local(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testFloatEmulations(1, "dot3_local");
}

void TestOptimizations::testVectorAdd(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testFloatEmulations(4, "VectorAdd");
}

void TestOptimizations::testArithmetic(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testFloatEmulations(5, "test_arithm");
}

void TestOptimizations::testClamp(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testFloatEmulations(10, "test_clamp");
}

void TestOptimizations::testCross(std::string passParamName)
{
    config.additionalEnabledOptimizations = {passParamName};
    config.optimizationLevel = OptimizationLevel::NONE;

    TestEmulator::testFloatEmulations(11, "test_cross");
}