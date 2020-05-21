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
#include "spirv/SPIRVParser.h"

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
    std::map<std::string, vc4c::TemporaryFile> cachedPrecompilations;

    TestCompilationHelper(const vc4c::Configuration& config) : config(config) {}

    void precompileAndParse(
        vc4c::Module& module, const std::string& fileName, const std::string& options, bool cachePrecompilation)
    {
        std::ifstream input(fileName);
        std::unique_ptr<std::istream> precompiled;
        auto tmpIt = cachedPrecompilations.find(fileName);
        if(tmpIt == cachedPrecompilations.end())
        {
            tmpIt = cachedPrecompilations.emplace(std::make_pair(fileName, vc4c::TemporaryFile{})).first;
            vc4c::Precompiler::precompile(input, precompiled, config, options, fileName, tmpIt->second.fileName);
        }
        std::unique_ptr<vc4c::Parser> parser;
        switch(vc4c::Precompiler::getSourceType(*precompiled))
        {
        case vc4c::SourceType::LLVM_IR_BIN:
            parser = std::make_unique<vc4c::llvm2qasm::BitcodeReader>(*precompiled, vc4c::SourceType::LLVM_IR_BIN);
            break;
        case vc4c::SourceType::SPIRV_BIN:
            parser = std::make_unique<vc4c::spirv::SPIRVParser>(*precompiled, false);
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
        std::unique_ptr<std::istream> precompiled;
        vc4c::Precompiler::precompile(input, precompiled, config, options);
        std::unique_ptr<vc4c::Parser> parser;
        switch(vc4c::Precompiler::getSourceType(*precompiled))
        {
        case vc4c::SourceType::LLVM_IR_BIN:
            parser = std::make_unique<vc4c::llvm2qasm::BitcodeReader>(*precompiled, vc4c::SourceType::LLVM_IR_BIN);
            break;
        case vc4c::SourceType::SPIRV_BIN:
            parser = std::make_unique<vc4c::spirv::SPIRVParser>(*precompiled, false);
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

    void compileFile(
        std::stringstream& buffer, const std::string& fileName, const std::string& options, bool cachePrecompilation)
    {
        config.outputMode = vc4c::OutputMode::BINARY;
        config.writeKernelInfo = true;
        std::ifstream input(fileName);
        std::unique_ptr<std::istream> precompiled;
        std::string precompiledFile;
        auto tmpIt = cachedPrecompilations.find(fileName);
        if(tmpIt == cachedPrecompilations.end())
        {
            tmpIt = cachedPrecompilations.emplace(std::make_pair(fileName, vc4c::TemporaryFile{})).first;
            vc4c::Precompiler::precompile(input, precompiled, config, options, fileName, tmpIt->second.fileName);
        }
        tmpIt->second.openInputStream(precompiled);
        vc4c::Compiler::compile(*precompiled, buffer, config, "", tmpIt->second.fileName);
        if(!cachePrecompilation)
            cachedPrecompilations.erase(tmpIt);
    }
};

#endif /* VC4C_TEST_COMPILATION_HELPER_H */