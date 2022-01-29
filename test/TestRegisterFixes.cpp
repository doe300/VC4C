/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestRegisterFixes.h"

#include "CompilerInstance.h"
#include "EmulationRunner.h"
#include "Precompiler.h"
#include "Profiler.h"
#include "TestData.h"
#include "TestEntries.h"
#include "asm/RegisterFixes.h"
#include "tools.h"

#include <set>
#include <sstream>

using namespace vc4c;
using namespace vc4c::tools;

static const std::string MESSAGE_REGISTER_FIX_FAILED =
    "Register allocation failed, remainder of test will be skipped: ";

class RegisterFixEmulationRunner final : public EmulationRunner
{
public:
    RegisterFixEmulationRunner(const vc4c::Configuration& config, std::vector<qpu_asm::RegisterFixupStep>&& fixupSteps,
        vc4c::FastMap<std::string, vc4c::CompilationData>& precompilationCache) :
        EmulationRunner(config, precompilationCache),
        steps(std::move(fixupSteps))
    {
    }

    test_data::Result compile(
        const std::string& sourceCode, const std::string& options, const std::string& name) override
    {
        auto it = compilationCache.find(sourceCode + options);
        vc4c::CompilationData precompilationData{};
        if(it != compilationCache.end())
            precompilationData = it->second;
        else
        {
            std::stringstream ss{sourceCode};
            precompilationData = Precompiler::precompile(CompilationData{ss}, config, options);
            compilationCache.emplace(sourceCode + options, precompilationData);
        }

        CompilerInstance instance(config);
        instance.parseInput(precompilationData);
        instance.normalize();
        instance.optimize();
        instance.adjust();

        try
        {
            std::stringstream ss;
            instance.generateCode(ss, steps);
            currentData.module = CompilationData{ss};
        }
        catch(const CompilationError& err)
        {
            // Since we test fix-up steps it might happen (probably quite often) that the single step we are executing
            // is not enough, so don't fail on register-association errors, just skip the emulation

            // TODO find a way to only allow some errors (e.g. register allocation failure) but still error on others
            return test_data::Result{false, MESSAGE_REGISTER_FIX_FAILED + err.what()};
        }
        return test_data::RESULT_OK;
    }

private:
    std::vector<qpu_asm::RegisterFixupStep> steps;
};

TestRegisterFixes::TestRegisterFixes(const vc4c::Configuration& config) : config(config)
{
    // Some register fix-ups have the same name (same fix-up is executed multiple times), there is no need to add
    // multiple tests for them
    std::set<std::string> appliedSteps;
    for(const auto& step : qpu_asm::FIXUP_STEPS)
    {
        if(appliedSteps.find(step.name) != appliedSteps.end())
            continue;
        appliedSteps.emplace(step.name);

        // NOTE need to find kernels which barely build with register fix-ups enabled (or: kernels which need register
        // fix-ups but are not too complicated that single fix-up steps can't make them work!)
        TEST_ADD_TWO_ARGUMENTS(
            TestRegisterFixes::testRegisterFix, std::string{"OpenCL_CTS_min_max_constant_args"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(
            TestRegisterFixes::testRegisterFix, std::string{"OpenCV_flip_columns_float2"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"OpenCV_mean_stddev"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"OpenCV_normalize"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"OpenCV_transpose"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"SHA256"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"atomics"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"boost_fibonacci"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"boost_initial_reduce"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"clNN_upscale"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"shuffle"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix, std::string{"shuffle_sample3"}, step.name);
        TEST_ADD_TWO_ARGUMENTS(TestRegisterFixes::testRegisterFix,
            std::string{"vstore_alias_private_register_strided_char_to_int"}, step.name);
    }
    TEST_ADD(TestRegisterFixes::checkTestQuality);
}

TestRegisterFixes::~TestRegisterFixes() = default;

void TestRegisterFixes::testRegisterFix(std::string entryName, std::string stepName)
{
    auto test = test_data::getTest(entryName);
    std::vector<qpu_asm::RegisterFixupStep> steps;

    for(const auto& step : qpu_asm::FIXUP_STEPS)
    {
        if(step.name == stepName)
            steps.emplace_back(step);
    }

    // make sure we have an entry in any case
    fixupPasses[stepName] += 0;

    RegisterFixEmulationRunner runner(config, std::move(steps), precompilationCache);
    auto result = test_data::execute(test, runner);
    if(!result && result.error.find(MESSAGE_REGISTER_FIX_FAILED) == 0)
    {
        // handle as "passed"
        return;
    }
    // the compilation succeeded, so track as that
    fixupPasses[stepName] += 1;
    TEST_ASSERT(result.wasSuccess)
    if(!result.error.empty())
        TEST_ASSERT_EQUALS("(no error)", result.error);
}

void TestRegisterFixes::checkTestQuality()
{
    // It is likely (and accepted) that some tests fail to compile with some register fix-up steps. To still be able to
    // make sure that the fix-up steps are tested at all, we check that for every one of them at least some compilation
    // passed (and where validated by emulation).
    // TODO Since the usage of the fix-up steps are not guaranteed, this does not guarantee that the steps have been
    // applied
    for(const auto& entry : fixupPasses)
    {
        if(entry.second == 0)
            TEST_ASSERT_EQUALS("",
                "All compilations failed for register fix-up step " + entry.first +
                    ", step correctness could not be tested!");
    }

#ifndef NDEBUG
    vc4c::profiler::dumpProfileResults(true);
#endif
}