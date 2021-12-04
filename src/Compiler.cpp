/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Compiler.h"

#include "CompilationError.h"
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

static std::pair<CompilationData, std::size_t> runCompiler(
    const CompilationData& input, const Configuration& config, const Optional<std::string>& outputFile)
{
    Module module(config);

    {
        PROFILE_SCOPE(Parser);
        std::unique_ptr<Parser> parser = getParser(input);
        parser->parse(module);
        // early clean up the parser, since we do not need it anymore and it may use a lot of memory
    }

    normalization::Normalizer norm(config);
    optimizations::Optimizer opt(config);
    qpu_asm::CodeGenerator codeGen(module, config);

    PROFILE_START(Normalizer);
    norm.normalize(module);
    PROFILE_END(Normalizer);

    // remove all non-kernel functions, since we do not handle them anymore, to free up some memory
    module.dropNonKernels();

    PROFILE_START(Optimizer);
    opt.optimize(module);
    PROFILE_END(Optimizer);

    PROFILE_START(SecondNormalizer);
    norm.adjust(module);
    PROFILE_END(SecondNormalizer);

    auto kernels = module.getKernels();
    const auto f = [&codeGen](Method* kernelFunc) -> void { codeGen.toMachineCode(*kernelFunc); };
    ThreadPool::scheduleAll<Method*>("CodeGenerator", kernels, f, THREAD_LOGGER.get());

    // TODO could discard unused globals
    // since they are exported, they are still in the intermediate code, even if not used (e.g. optimized away)

    // code generation
    CompilationData result{};
    std::size_t bytesWritten = 0;
    if(outputFile)
    {
        std::ofstream fos{*outputFile};
        bytesWritten = codeGen.writeOutput(fos);
        fos.flush();

        result = CompilationData{
            *outputFile, config.outputMode == OutputMode::HEX ? SourceType::QPUASM_HEX : SourceType::QPUASM_BIN};
    }
    else
    {
        std::stringstream output;
        bytesWritten = codeGen.writeOutput(output);
        output.flush();

        result = CompilationData{output,
            config.outputMode == OutputMode::HEX ? SourceType::QPUASM_HEX : SourceType::QPUASM_BIN,
            "compilation result"};
    }

    return std::make_pair(std::move(result), bytesWritten);
}

std::size_t Compiler::compile(std::istream& input, std::ostream& output, const Configuration& config,
    const std::string& options, const Optional<std::string>& inputFile)
{
    auto inputData = inputFile ? CompilationData{*inputFile, Precompiler::getSourceType(input)} :
                                 CompilationData{input, Precompiler::getSourceType(input), "compilation source"};
    auto out = compile(inputData, config, options);
    out.first.readInto(output);
    return out.second;
}

std::pair<CompilationData, std::size_t> Compiler::compile(const CompilationData& input, const Configuration& config,
    const std::string& options, const Optional<std::string>& outputFile)
{
    try
    {
        // pre-compilation
        auto intermediate = Precompiler::precompile(input, config, options);

        // compilation
        auto result = runCompiler(intermediate, config, outputFile);

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
