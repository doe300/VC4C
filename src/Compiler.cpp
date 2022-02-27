/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Compiler.h"

#include "CompilationError.h"
#include "CompilerInstance.h"
#include "Logger.h"
#include "Parser.h"
#include "Precompiler.h"
#include "Profiler.h"
#include "ThreadPool.h"
#include "asm/CodeGenerator.h"
#include "log.h"
#include "logger.h"
#include "normalization/Normalizer.h"
#include "optimization/Optimizer.h"
#include "precompilation/FrontendCompiler.h"
#include "spirv/SPIRVLexer.h"
#include "spirv/SPIRVToolsParser.h"
#include "llvm/BitcodeReader.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fcntl.h>
#include <fstream>
#include <iterator>
#include <memory>
#include <sstream>
#include <unistd.h>
#include <vector>

using namespace vc4c;

// out-of-line virtual method definition
Parser::~Parser() noexcept = default;

static std::unique_ptr<Parser> getParser(const CompilationData& source)
{
    switch(source.getType())
    {
    case SourceType::LLVM_IR_TEXT:
        logging::info() << "Using LLVM-IR frontend..." << logging::endl;
        if(hasLLVMFrontend())
            return std::unique_ptr<Parser>(
                new llvm2qasm::BitcodeReader(dynamic_cast<const precompilation::LLVMIRTextData&>(*source.inner())));
        else
            throw CompilationError(CompilationStep::GENERAL, "No LLVM IR text front-end available!");
    case SourceType::LLVM_IR_BIN:
        logging::info() << "Using LLVM module frontend..." << logging::endl;
        if(hasLLVMFrontend())
            return std::unique_ptr<Parser>(
                new llvm2qasm::BitcodeReader(dynamic_cast<const precompilation::LLVMIRData&>(*source.inner())));
        else
            throw CompilationError(CompilationStep::GENERAL, "No LLVM IR module front-end available!");
    case SourceType::SPIRV_TEXT:
        if(hasSPIRVToolsFrontend())
        {
            logging::info() << "Using SPIR-V Tools frontend..." << logging::endl;
            return std::unique_ptr<Parser>(
                new spirv::SPIRVToolsParser(dynamic_cast<const precompilation::SPIRVTextData&>(*source.inner())));
        }
        else
            throw CompilationError(
                CompilationStep::GENERAL, "SPIR-V text needs to be first converted to SPIR-V binary!");
    case SourceType::SPIRV_BIN:
        if(hasSPIRVToolsFrontend())
        {
            logging::info() << "Using SPIR-V Tools frontend..." << logging::endl;
            return std::unique_ptr<Parser>(
                new spirv::SPIRVToolsParser(dynamic_cast<const precompilation::SPIRVData&>(*source.inner())));
        }
        else
        {
            logging::info() << "Using builtin SPIR-V frontend..." << logging::endl;
            return std::unique_ptr<Parser>(
                new spirv::SPIRVLexer(dynamic_cast<const precompilation::SPIRVData&>(*source.inner())));
        }
    case SourceType::OPENCL_C:
        throw CompilationError(CompilationStep::GENERAL, "OpenCL code needs to be first compiled with CLang!");
    case SourceType::QPUASM_BIN:
    case SourceType::QPUASM_HEX:
        throw CompilationError(CompilationStep::GENERAL, "Input code is already compiled machine-code!");
    case SourceType::UNKNOWN:
        throw CompilationError(CompilationStep::GENERAL, "Unrecognized source code type!");
    }
    return nullptr;
}

CompilerInstance::CompilerInstance(const Configuration& config) : moduleConfig(config), module(moduleConfig) {}

void CompilerInstance::precompileAndParseInput(const CompilationData& input, const std::string& options)
{
    auto intermediate = Precompiler::precompile(input, moduleConfig, options);
    parseInput(intermediate);
}

void CompilerInstance::parseInput(const CompilationData& input)
{
    PROFILE_SCOPE(Parser);
    std::unique_ptr<Parser> parser = getParser(input);
    parser->parse(module);
}

void CompilerInstance::normalize(bool dropNonKernels)
{
    normalization::Normalizer norm(moduleConfig);

    PROFILE_START(Normalizer);
    norm.normalize(module);
    PROFILE_END(Normalizer);

    if(dropNonKernels)
        // remove all non-kernel functions, since we do not handle them anymore, to free up some memory
        module.dropNonKernels();
}

void CompilerInstance::optimize()
{
    optimizations::Optimizer opt(moduleConfig);

    PROFILE_START(Optimizer);
    opt.optimize(module);
    PROFILE_END(Optimizer);
}

void CompilerInstance::adjust()
{
    normalization::Normalizer norm(moduleConfig);

    PROFILE_START(SecondNormalizer);
    norm.adjust(module);
    PROFILE_END(SecondNormalizer);

    // TODO could discard unused globals
    // since they are exported, they are still in the intermediate code, even if not used (e.g. optimized away)
}

std::size_t CompilerInstance::generateCode(std::ostream& output)
{
    qpu_asm::CodeGenerator codeGen(module, moduleConfig);

    auto kernels = module.getKernels();
    const auto f = [&codeGen](Method* kernelFunc) -> void { codeGen.toMachineCode(*kernelFunc); };
    ThreadPool::scheduleAll<Method*>("CodeGenerator", kernels, f, THREAD_LOGGER.get());
    return codeGen.writeOutput(output);
}

std::size_t CompilerInstance::generateCode(
    std::ostream& output, const std::vector<qpu_asm::RegisterFixupStep>& customSteps)
{
    qpu_asm::CodeGenerator codeGen(module, customSteps, moduleConfig);

    auto kernels = module.getKernels();
    const auto f = [&codeGen](Method* kernelFunc) -> void { codeGen.toMachineCode(*kernelFunc); };
    ThreadPool::scheduleAll<Method*>("CodeGenerator", kernels, f, THREAD_LOGGER.get());
    return codeGen.writeOutput(output);
}

std::pair<CompilationData, std::size_t> CompilerInstance::generateCode(const Optional<std::string>& outputFile)
{
    // code generation
    CompilationData result{};
    std::size_t bytesWritten = 0;
    if(outputFile)
    {
        std::ofstream fos{*outputFile};
        bytesWritten = generateCode(fos);
        fos.flush();

        result = CompilationData{
            *outputFile, moduleConfig.outputMode == OutputMode::HEX ? SourceType::QPUASM_HEX : SourceType::QPUASM_BIN};
    }
    else
    {
        std::stringstream output;
        bytesWritten = generateCode(output);
        output.flush();

        result = CompilationData{output,
            moduleConfig.outputMode == OutputMode::HEX ? SourceType::QPUASM_HEX : SourceType::QPUASM_BIN,
            "compilation result"};
    }

    return std::make_pair(std::move(result), bytesWritten);
}

std::pair<CompilationData, std::size_t> Compiler::compile(const CompilationData& input, const Configuration& config,
    const std::string& options, const std::string& outputFile)
{
    try
    {
        CompilerInstance instance{config};

        // pre-compilation
        instance.precompileAndParseInput(input, options);

        // compilation
        instance.normalize();
        instance.optimize();
        instance.adjust();
        auto result = instance.generateCode(outputFile.empty() ? Optional<std::string>{} : outputFile);

        // clean-up
        std::wcout.flush();
        std::wcerr.flush();

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Compilation complete: " << result.second << " bytes written" << logging::endl);

        return result;
    }
    catch(const CompilationError& e)
    {
        // log exception to log
        logging::error() << "Compiler threw exception: " << e.what() << logging::endl;
        // re-throw, so caller gets notified
        throw;
    }
}

LCOV_EXCL_START
std::unique_ptr<logging::Logger> logging::DEFAULT_LOGGER(
    new logging::ColoredLogger(std::wcout, logging::Level::WARNING));

thread_local std::unique_ptr<logging::Logger> vc4c::THREAD_LOGGER;

void vc4c::setLogger(std::wostream& outputStream, const bool coloredOutput, const LogLevel level)
{
    // Only update the logger for this thread (and all tasks/threads) started by this thread to allow parallel logging
    // into different outputs
    if(coloredOutput)
        THREAD_LOGGER = std::make_unique<logging::ColoredLogger>(outputStream, static_cast<logging::Level>(level));
    else
        THREAD_LOGGER = std::make_unique<logging::StreamLogger>(outputStream, static_cast<logging::Level>(level));
    logging::setThreadLogger(THREAD_LOGGER.get());
}
LCOV_EXCL_STOP
