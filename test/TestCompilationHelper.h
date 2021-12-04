/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_COMPILATION_HELPER_H
#define VC4C_TEST_COMPILATION_HELPER_H

#include "VC4C.h"

#include "../src/llvm/BitcodeReader.h"
#include "Compiler.h"
#include "Module.h"
#include "ThreadPool.h"
#include "asm/CodeGenerator.h"
#include "normalization/Normalizer.h"
#include "optimization/Optimizer.h"
#include "precompilation/FrontendCompiler.h"
#include "spirv/SPIRVLexer.h"
#include "tool_paths.h"

#include <fstream>
#include <map>
#include <memory>
#include <sstream>
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

    void precompileAndParse(
        vc4c::Module& module, const std::string& fileName, const std::string& options, bool cachePrecompilation)
    {
        auto tmpIt = cachedPrecompilations.find(fileName);
        if(tmpIt == cachedPrecompilations.end())
        {
            auto result = vc4c::Precompiler::precompile(vc4c::CompilationData{fileName}, config, options);
            tmpIt = cachedPrecompilations.emplace(fileName, result).first;
        }
        std::unique_ptr<vc4c::Parser> parser;
        switch(tmpIt->second.getType())
        {
        case vc4c::SourceType::LLVM_IR_BIN:
            if(vc4c::hasLLVMFrontend())
            {
                parser = std::make_unique<vc4c::llvm2qasm::BitcodeReader>(
                    dynamic_cast<const vc4c::precompilation::LLVMIRData&>(*tmpIt->second.inner()));
                break;
            }
            throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL,
                "Unhandled source type for partial compilation for input file", fileName);
        case vc4c::SourceType::SPIRV_BIN:
            parser = std::make_unique<vc4c::spirv::SPIRVLexer>(
                dynamic_cast<const vc4c::precompilation::SPIRVData&>(*tmpIt->second.inner()));
            break;
        default:
            throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL,
                "Unhandled source type for partial compilation for input file", fileName);
        }
        parser->parse(module);
        if(!cachePrecompilation)
            cachedPrecompilations.erase(tmpIt);
    }

    void precompileAndParse(vc4c::Module& module, std::istream& input, const std::string& options)
    {
        auto precompiled = vc4c::Precompiler::precompile(vc4c::CompilationData{input}, config, options);
        std::unique_ptr<vc4c::Parser> parser;
        switch(precompiled.getType())
        {
        case vc4c::SourceType::LLVM_IR_BIN:
            if(vc4c::hasLLVMFrontend())
            {
                parser = std::make_unique<vc4c::llvm2qasm::BitcodeReader>(
                    dynamic_cast<const vc4c::precompilation::LLVMIRData&>(*precompiled.inner()));
                break;
            }
            throw vc4c::CompilationError(
                vc4c::CompilationStep::GENERAL, "Unhandled source type for partial compilation for input");
        case vc4c::SourceType::SPIRV_BIN:
            parser = std::make_unique<vc4c::spirv::SPIRVLexer>(
                dynamic_cast<const vc4c::precompilation::SPIRVData&>(*precompiled.inner()));
            break;
        default:
            throw vc4c::CompilationError(
                vc4c::CompilationStep::GENERAL, "Unhandled source type for partial compilation for input");
        }
        parser->parse(module);
    }

    void normalize(vc4c::Module& module)
    {
        vc4c::normalization::Normalizer normalizer(config);
        normalizer.normalize(module);
    }

    void optimize(vc4c::Module& module)
    {
        vc4c::optimizations::Optimizer optimizer(config);
        optimizer.optimize(module);
    }

    void adjust(vc4c::Module& module)
    {
        vc4c::normalization::Normalizer normalizer(config);
        normalizer.adjust(module);
    }

    std::size_t toMachineCode(std::stringstream& buffer, vc4c::Module& module)
    {
        vc4c::qpu_asm::CodeGenerator codeGen(module, config);
        auto kernels = module.getKernels();
        const auto f = [&codeGen](vc4c::Method* kernelFunc) -> void { codeGen.toMachineCode(*kernelFunc); };
        vc4c::ThreadPool{"CodeGenerator"}.scheduleAll<vc4c::Method*>(kernels, f);

        // code generation
        return codeGen.writeOutput(buffer);
    }

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
        auto result = vc4c::Compiler::compile(tmpIt->second, config, "");
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
        return vc4c::Compiler::compile(precompiled, config, "").first;
    }
};

#endif /* VC4C_TEST_COMPILATION_HELPER_H */