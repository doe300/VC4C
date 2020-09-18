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
#include "spirv/SPIRVLexer.h"
#include "spirv/SPIRVToolsParser.h"
#include "llvm/BitcodeReader.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fcntl.h>
#include <iterator>
#include <memory>
#include <sstream>
#include <unistd.h>
#include <vector>

using namespace vc4c;

// out-of-line virtual method definition
Parser::~Parser() noexcept = default;

Compiler::Compiler(std::istream& stream, std::ostream& output) : input(stream), output(output), config()
{
    if(!input)
        // e.g. if pre-compilation failed
        throw CompilationError(CompilationStep::GENERAL, "Invalid input");
}

static std::unique_ptr<Parser> getParser(std::istream& stream)
{
    // determine which parser to use in which settings
    SourceType type = Precompiler::getSourceType(stream);
    switch(type)
    {
    case SourceType::LLVM_IR_TEXT:
        logging::info() << "Using LLVM-IR frontend..." << logging::endl;
#ifdef USE_LLVM_LIBRARY
        return std::unique_ptr<Parser>(new llvm2qasm::BitcodeReader(stream, SourceType::LLVM_IR_TEXT));
#else
        throw CompilationError(CompilationStep::GENERAL, "No LLVM IR text front-end available!");
#endif
    case SourceType::LLVM_IR_BIN:
        logging::info() << "Using LLVM module frontend..." << logging::endl;
#ifdef USE_LLVM_LIBRARY
        return std::unique_ptr<Parser>(new llvm2qasm::BitcodeReader(stream, SourceType::LLVM_IR_BIN));
#else
        throw CompilationError(CompilationStep::GENERAL, "No LLVM IR module front-end available!");
#endif
    case SourceType::SPIRV_TEXT:
#ifdef SPIRV_TOOLS_FRONTEND
        logging::info() << "Using SPIR-V Tools frontend..." << logging::endl;
        return std::unique_ptr<Parser>(new spirv::SPIRVToolsParser(stream, true));
#else
        throw CompilationError(CompilationStep::GENERAL, "SPIR-V text needs to be first converted to SPIR-V binary!");
#endif
    case SourceType::SPIRV_BIN:
#ifdef SPIRV_TOOLS_FRONTEND
        logging::info() << "Using SPIR-V Tools frontend..." << logging::endl;
        return std::unique_ptr<Parser>(new spirv::SPIRVToolsParser(stream, false));
#else
        logging::info() << "Using builtin SPIR-V frontend..." << logging::endl;
        return std::unique_ptr<Parser>(new spirv::SPIRVLexer(stream));
#endif
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

std::size_t Compiler::convert()
{
    Module module(config);

    {
        std::unique_ptr<Parser> parser = getParser(input);
        PROFILE_START(Parser);
        parser->parse(module);
        PROFILE_END(Parser);
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
    ThreadPool{"CodeGenerator"}.scheduleAll<Method*>(kernels, f, THREAD_LOGGER.get());

    // TODO could discard unused globals
    // since they are exported, they are still in the intermediate code, even if not used (e.g. optimized away)

    // code generation
    std::size_t bytesWritten = codeGen.writeOutput(output);
    output.flush();

    return bytesWritten;
}

Configuration& Compiler::getConfiguration()
{
    return config;
}

const Configuration& Compiler::getConfiguration() const
{
    return config;
}

std::size_t Compiler::compile(std::istream& input, std::ostream& output, const Configuration& config,
    const std::string& options, const Optional<std::string>& inputFile)
{
    try
    {
        // pre-compilation
        TemporaryFile tmpFile;
        std::unique_ptr<std::istream> in;
        Precompiler::precompile(input, in, config, options, inputFile, tmpFile.fileName);

        if(in == nullptr ||
            (dynamic_cast<std::istringstream*>(in.get()) != nullptr &&
                dynamic_cast<std::istringstream*>(in.get())->str().empty()))
            // replace only when pre-compiled (and not just linked output to input, e.g. if source-type is output-type)
            tmpFile.openInputStream(in);

        // compilation
        Compiler conv(*in, output);

        conv.getConfiguration() = config;
        std::size_t result = conv.convert();

        // clean-up
        std::wcout.flush();
        std::wcerr.flush();
        output.flush();

        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Compilation complete: " << result << " bytes written" << logging::endl);

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
