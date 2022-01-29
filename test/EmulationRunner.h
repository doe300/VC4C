/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_EMULATION_RUNNER_H
#define VC4C_TEST_EMULATION_RUNNER_H

#include "TestCompilationHelper.h"
#include "TestData.h"
#include "tools.h"

#include <sstream>

class EmulationRunner : public test_data::TestRunner, protected TestCompilationHelper
{
public:
    explicit EmulationRunner(
        const vc4c::Configuration& config, vc4c::FastMap<std::string, vc4c::CompilationData>& cache) :
        TestCompilationHelper(config),
        compilationCache(cache)
    {
    }

    ~EmulationRunner() noexcept override;

    test_data::Result compile(
        const std::string& sourceCode, const std::string& options, const std::string& name) override
    try
    {
        auto it = compilationCache.find(sourceCode + options);
        vc4c::CompilationData currentBinary{};
        if(it != compilationCache.end())
            currentBinary = it->second;
        else
            currentBinary = compileString(sourceCode, options, name);
        currentData.module = currentBinary;
        compilationCache.emplace(sourceCode + options, currentBinary);
        return test_data::RESULT_OK;
    }
    catch(const std::exception& err)
    {
        return test_data::Result{false, err.what()};
    }

    test_data::Result selectKernel(const std::string& name) override
    {
        currentData.kernelName = name;
        // reset kernel configuration
        currentData.parameter.clear();
        currentData.workGroup = {};
        return test_data::RESULT_OK;
    }

    test_data::Result setKernelArgument(
        std::size_t index, bool isLiteral, bool isVector, const void* byteData, std::size_t numBytes) override
    {
        if(currentData.parameter.size() <= index)
            currentData.parameter.resize(index + 1);

        if(isLiteral && !isVector)
        {
            // XXX For simplicity, treat literal vectors/structs as buffers, which is wrong for vectors, but we filter
            // those kernels out anyway beforehand
            uint32_t dummy = 0;
            std::memcpy(&dummy, byteData, std::min(numBytes, sizeof(uint32_t)));
            currentData.parameter[index] = std::make_pair(dummy, vc4c::Optional<std::vector<uint32_t>>{});
        }
        else
        {
            auto bufferSize = numBytes / sizeof(uint32_t) + (numBytes % sizeof(uint32_t) != 0 ? 1 : 0);
            std::vector<uint32_t> tmp(bufferSize, 0x42424242);
            std::memcpy(tmp.data(), byteData, numBytes);
            currentData.parameter[index] = std::make_pair(0, std::move(tmp));
        }
        return test_data::RESULT_OK;
    }

    test_data::Result setWorkDimensions(const test_data::WorkDimensions& dimensions) override
    {
        currentData.workGroup.dimensions = dimensions.dimensions;
        currentData.workGroup.localSizes = dimensions.localSizes;
        currentData.workGroup.numGroups = dimensions.numGroups;
        currentData.workGroup.globalOffsets = dimensions.globalOffsets;
        return test_data::RESULT_OK;
    }

    test_data::Result execute() override
    try
    {
        currentResult.reset(new vc4c::tools::EmulationResult(emulate(currentData)));
        if(currentResult->executionSuccessful)
            return test_data::RESULT_OK;
        return test_data::Result{false, "Emulation failed!"};
    }
    catch(const std::exception& err)
    {
        return test_data::Result{false, err.what()};
    }

    test_data::Result getKernelArgument(std::size_t index, void* byteData, std::size_t bufferSize) override
    {
        if(!currentResult || index >= currentResult->results.size())
            return test_data::Result{false, "Argument index out of bounds!"};
        auto& arg = currentResult->results[index];
        if(arg.second)
        {
            const auto& vector = arg.second.value();
            std::memcpy(byteData, vector.data(), std::min(vector.size() * sizeof(uint32_t), bufferSize));
        }
        else
            std::memcpy(byteData, &arg.first, std::min(sizeof(arg.first), bufferSize));
        return test_data::RESULT_OK;
    }

    test_data::Result validateFinish() override
    {
        // nothing to be done here
        return test_data::RESULT_OK;
    }

protected:
    vc4c::tools::EmulationData currentData;
    std::unique_ptr<vc4c::tools::EmulationResult> currentResult;
    vc4c::FastMap<std::string, vc4c::CompilationData>& compilationCache;
};

#endif /* VC4C_TEST_EMULATION_RUNNER_H */
