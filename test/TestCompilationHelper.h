/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_COMPILATION_HELPER_H
#define VC4C_TEST_COMPILATION_HELPER_H

#include "VC4C.h"

#include "Compiler.h"
#include "Module.h"

#include <map>
#include <string>

/**
 * Base class for tests providing functions for easier (partial) compilation
 */
class TestCompilationHelper
{
protected:
    vc4c::Configuration config;
    std::map<std::string, vc4c::CompilationData> cachedPrecompilations;

    TestCompilationHelper(const vc4c::Configuration& config) : config(config) {}

    vc4c::CompilationData compileFile(const std::string& fileName, const std::string& options, bool cachePrecompilation)
    {
        config.outputMode = vc4c::OutputMode::BINARY;
        config.writeKernelInfo = true;
        auto tmpIt = cachedPrecompilations.find(fileName);
        if(tmpIt == cachedPrecompilations.end())
        {
            auto result = vc4c::Precompiler::precompile(vc4c::CompilationData{fileName}, config, options);
            tmpIt = cachedPrecompilations.emplace(fileName, result).first;
        }
        auto result = vc4c::Compiler::compile(tmpIt->second, config);
        if(!cachePrecompilation)
            cachedPrecompilations.erase(tmpIt);
        return result.first;
    }

    vc4c::CompilationData compileString(
        const std::string& input, const std::string& options, const std::string& name = "")
    {
        config.outputMode = vc4c::OutputMode::BINARY;
        config.writeKernelInfo = true;
        auto precompiled = vc4c::Precompiler::precompile(
            vc4c::CompilationData{input.begin(), input.end(), vc4c::SourceType::OPENCL_C, name}, config, options);
        return vc4c::Compiler::compile(precompiled, config).first;
    }
};

#endif /* VC4C_TEST_COMPILATION_HELPER_H */