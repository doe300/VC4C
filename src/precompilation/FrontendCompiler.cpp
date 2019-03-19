/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "FrontendCompiler.h"

#include "../ProcessUtil.h"
#include "../Profiler.h"
#include "../helper.h"
#include "LibClang.h"
#include "log.h"

#ifdef SPIRV_FRONTEND
#include "../spirv/SPIRVHelper.h"
#endif

#include <cstdlib>
#include <fstream>
#include <iterator>
#include <numeric>

using namespace vc4c;
using namespace vc4c::precompilation;

static std::vector<std::string> buildClangCommand(const std::string& compiler, const std::string& defaultOptions,
    const std::string& options, const std::string& emitter, const std::string& outputFile,
    const std::string& inputFile = "-", bool usePCH = true)
{
    // check validity of options - we do not support all of them
    if(options.find("-create-library") != std::string::npos)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid compilation options", options);

    // build command
    std::vector<std::string> command;
    command.emplace_back(compiler);
    // splits (default) options by space and inserts into vector
    // TODO spaces in "", e.g. in paths!
    {
        std::istringstream tmp{defaultOptions};
        std::copy(
            std::istream_iterator<std::string>{tmp}, std::istream_iterator<std::string>{}, std::back_inserter(command));
    }
    {
        std::istringstream tmp{options};
        std::copy(
            std::istream_iterator<std::string>{tmp}, std::istream_iterator<std::string>{}, std::back_inserter(command));
    }

    // append default options
    if(options.find("-O") == std::string::npos)
    {
        // unroll loops, pre-calculate constants, inline functions, ...
        command.emplace_back("-O3");
    }
    if(options.find("-ffp-contract") == std::string::npos)
    {
        // disable fused floationg-point operations, since we do not support them anyway
        command.emplace_back("-ffp-contract=off");
    }

#if defined USE_CLANG_OPENCL || defined SPIRV_CLANG_PATH
    if(options.find("-cl-std") == std::string::npos)
    {
        // build OpenCL 1.2
        command.emplace_back("-cl-std=CL1.2");
    }
    if(options.find("-cl-kernel-arg-info") == std::string::npos)
    {
        // make sure infos for arguments are generated
        command.emplace_back("-cl-kernel-arg-info");
    }
    if(options.find("-cl-single-precision-constant") == std::string::npos)
    {
        // suppressed warnings about double constants
        command.emplace_back("-cl-single-precision-constant");
    }
#endif
    // link in our standard-functions
    command.emplace_back("-Wno-undefined-inline");
    command.emplace_back("-Wno-unused-parameter");
    command.emplace_back("-Wno-unused-local-typedef");
    command.emplace_back("-Wno-gcc-compat");
    if(usePCH)
    {
        command.emplace_back("-include-pch");
        command.emplace_back(Precompiler::findStandardLibraryFiles().precompiledHeader);
    }
    else
    {
        command.emplace_back("-finclude-default-header");
        // The #defines (esp. for extensions) from the default headers differ from the supported #defines,
        // so we need to include our #defines/undefines
        command.emplace_back("-include");
        command.emplace_back(Precompiler::findStandardLibraryFiles().configurationHeader);
    }
    if(options.find("-x cl") == std::string::npos)
    {
        // build OpenCL, required when input is from stdin, since clang can't determine from file-type
        command.emplace_back("-x");
        command.emplace_back("cl");
    }
    // use temporary file as output
    // use stdin as input
    {
        std::istringstream tmp{emitter};
        std::copy(
            std::istream_iterator<std::string>{tmp}, std::istream_iterator<std::string>{}, std::back_inserter(command));
    }
    command.emplace_back("-o");
    command.emplace_back(outputFile);
    command.emplace_back(inputFile);
    return command;
}

void runPrecompiler(const std::string& command, std::istream* inputStream, std::ostream* outputStream)
{
    std::ostringstream stderr;
    int status = runProcess(command, inputStream, outputStream, &stderr);
    if(status == 0) // success
    {
        logging::logLazy(logging::Level::WARNING, [&]() {
            if(!stderr.str().empty())
            {
                logging::warn() << "Warnings in precompilation:" << logging::endl;
                logging::warn() << stderr.str() << logging::endl;
            }
        });
        return;
    }
    if(!stderr.str().empty())
    {
        logging::error() << "Errors in precompilation:" << logging::endl;
        logging::error() << stderr.str() << logging::endl;
    }
    throw CompilationError(CompilationStep::PRECOMPILATION, "Error in precompilation", stderr.str());
}

static void compileOpenCLToLLVMIR0(std::istream* input, std::ostream* output, const std::string& options,
    bool toText = true, const Optional<std::string>& inputFile = {}, const Optional<std::string>& outputFile = {},
    bool withPCH = true)
{
#if not defined SPIRV_CLANG_PATH && not defined CLANG_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "No CLang configured for pre-compilation!");
#endif
#ifdef SPIRV_CLANG_PATH
    // just OpenCL C -> LLVM IR (but with Khronos CLang)
    const std::string compiler = SPIRV_CLANG_PATH;
#else
    const std::string compiler = CLANG_PATH;
#endif
    // in both instances, compile to SPIR to match the "architecture" the PCH was compiled for
    const std::string defaultOptions = "-cc1 -triple spir-unknown-unknown";
    // only run preprocessor and compilation, no linking and code-generation
    // emit LLVM IR
    auto command = buildClangCommand(compiler, defaultOptions, options,
        std::string("-S ").append(toText ? "-emit-llvm" : "-emit-llvm-bc"), outputFile.value_or("/dev/stdout"),
        inputFile.value_or("-"), withPCH);

    auto commandString = to_string<std::string>(command, " ");
    CPPLOG_LAZY(logging::Level::INFO, log << "Compiling OpenCL to LLVM-IR with: " << commandString << logging::endl);

#ifdef USE_LIBCLANG
    compileLibClang(command, inputFile ? nullptr : input, outputFile ? nullptr : output, inputFile, outputFile);
#else
    runPrecompiler(commandString, inputFile ? nullptr : input, outputFile ? nullptr : output);
#endif
}

static void compileLLVMIRToSPIRV0(std::istream* input, std::ostream* output, const std::string& options,
    const bool toText = false, const Optional<std::string>& inputFile = {},
    const Optional<std::string>& outputFile = {})
{
#if not defined SPIRV_LLVM_SPIRV_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not configured, can't compile to SPIR-V!");
#elif not defined SPIRV_FRONTEND
    throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-Tools not configured, can't process SPIR-V!");
#else
    std::string command = (std::string(SPIRV_LLVM_SPIRV_PATH) + (toText ? " -spirv-text" : "")) + " -o ";
    command.append(outputFile.value_or("/dev/stdout")).append(" ");
    command.append(inputFile.value_or("/dev/stdin"));

    CPPLOG_LAZY(logging::Level::INFO, log << "Converting LLVM-IR to SPIR-V with: " << command << logging::endl);

    runPrecompiler(command, inputFile ? nullptr : input, outputFile ? nullptr : output);
#endif
}

static void compileSPIRVToSPIRV(std::istream* input, std::ostream* output, const std::string& options,
    const bool toText = false, const Optional<std::string>& inputFile = {},
    const Optional<std::string>& outputFile = {})
{
#if not defined SPIRV_LLVM_SPIRV_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not configured, can't compile to SPIR-V!");
#elif not defined SPIRV_FRONTEND
    throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-Tools not configured, can't process SPIR-V!");
#else
    std::string command = (std::string(SPIRV_LLVM_SPIRV_PATH) + (toText ? " -to-text" : " -to-binary")) + " -o ";
    command.append(outputFile.value_or("/dev/stdout")).append(" ");
    command.append(inputFile.value_or("/dev/stdin"));

    CPPLOG_LAZY(logging::Level::INFO,
        log << "Converting between SPIR-V text and SPIR-V binary with: " << command << logging::endl);

    runPrecompiler(command, inputFile ? nullptr : input, outputFile ? nullptr : output);
#endif
}

void precompilation::compileOpenCLWithPCH(OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result)
{
    PROFILE_START(CompileOpenCLWithPCH);
    OpenCLSource src(std::forward<OpenCLSource>(source));
    compileOpenCLToLLVMIR0(src.stream, result.stream, userOptions, false, src.file, result.file);
    PROFILE_END(CompileOpenCLWithPCH);
}

void precompilation::compileOpenCLWithDefaultHeader(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result)
{
    PROFILE_START(CompileOpenCLWithDefaultHeader);
    OpenCLSource src(std::forward<OpenCLSource>(source));
    compileOpenCLToLLVMIR0(src.stream, result.stream, userOptions, false, src.file, result.file, false);
    PROFILE_END(CompileOpenCLWithDefaultHeader);
}

void precompilation::linkInStdlibModule(LLVMIRSource&& source, const std::string& userOptions, LLVMIRResult& result)
{
    if(Precompiler::findStandardLibraryFiles().llvmModule.empty())
        throw CompilationError(CompilationStep::LINKER, "LLVM IR module for VC4CL std-lib is not defined!");
    std::vector<LLVMIRSource> sources;
    sources.emplace_back(std::forward<LLVMIRSource>(source));
    sources.emplace_back(Precompiler::findStandardLibraryFiles().llvmModule);
    linkLLVMModules(std::move(sources), userOptions, result);
}

void precompilation::compileOpenCLToLLVMText(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRTextResult& result)
{
    PROFILE_START(CompileOpenCLToLLVMText);
    OpenCLSource src(std::forward<OpenCLSource>(source));
    compileOpenCLToLLVMIR0(src.stream, result.stream, userOptions, true, src.file, result.file);
    PROFILE_END(CompileOpenCLToLLVMText);
}

void precompilation::compileLLVMToSPIRV(LLVMIRSource&& source, const std::string& userOptions, SPIRVResult& result)
{
    PROFILE_START(CompileLLVMToSPIRV);
    LLVMIRSource src(std::forward<LLVMIRSource>(source));
    compileLLVMIRToSPIRV0(src.stream, result.stream, userOptions, false, src.file, result.file);
    PROFILE_END(CompileLLVMToSPIRV);
}

void precompilation::assembleSPIRV(SPIRVTextSource&& source, const std::string& userOptions, SPIRVResult& result)
{
    PROFILE_START(AssembleSPIRV);
    SPIRVTextSource src(std::forward<SPIRVTextSource>(source));
    compileSPIRVToSPIRV(src.stream, result.stream, userOptions, false, src.file, result.file);
    PROFILE_END(AssembleSPIRV);
}

void precompilation::compileLLVMToSPIRVText(
    LLVMIRSource&& source, const std::string& userOptions, SPIRVTextResult& result)
{
    PROFILE_START(CompileLLVMToSPIRVText);
    LLVMIRSource src(std::forward<LLVMIRSource>(source));
    compileLLVMIRToSPIRV0(src.stream, result.stream, userOptions, true, src.file, result.file);
    PROFILE_END(CompileLLVMToSPIRVText);
}

void precompilation::disassembleSPIRV(SPIRVSource&& source, const std::string& userOptions, SPIRVTextResult& result)
{
    PROFILE_START(DisassembleSPIRV);
    SPIRVSource src(std::forward<SPIRVSource>(source));
    compileSPIRVToSPIRV(src.stream, result.stream, userOptions, true, src.file, result.file);
    PROFILE_END(DisassembleSPIRV);
}

void precompilation::linkLLVMModules(
    std::vector<LLVMIRSource>&& sources, const std::string& userOptions, LLVMIRResult& result)
{
    // TODO add call to llvm-lto??!
    PROFILE_START(LinkLLVMModules);
#ifndef LLVM_LINK_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "llvm-link is not available!");
#else

    if(sources.empty())
        throw CompilationError(CompilationStep::PRECOMPILATION, "Cannot link without input files!");

    // only one input can be from a stream
    std::istream* inputStream = nullptr;
    const std::string out = result.file ? std::string("-o=") + result.file.value() : "";
    std::string inputs = std::accumulate(
        sources.begin(), sources.end(), std::string{}, [&](const std::string& a, const LLVMIRSource& b) -> std::string {
            if(b.file)
                return (a + " ") + b.file.value();
            if(inputStream != nullptr)
                throw CompilationError(CompilationStep::LINKER, "Cannot link with multiple stream-inputs!");
            inputStream = b.stream;
            return a + " -";
        });

    std::string command = std::string(LLVM_LINK_PATH " -only-needed -internalize ") + (out + " ") + inputs;

    // llvm-link does not like multiple white-spaces in the list of files (assumes file with empty name)
    std::size_t n = 0;
    while((n = command.find("  ", n)) != std::string::npos)
    {
        command.replace(n, 2, " ");
    }

    CPPLOG_LAZY(logging::Level::INFO, log << "Linking LLVM-IR modules with: " << command << logging::endl);

    runPrecompiler(command, inputStream, result.file ? nullptr : result.stream);
#endif
    PROFILE_END(LinkLLVMModules);
}

void precompilation::linkSPIRVModules(
    std::vector<SPIRVSource>&& sources, const std::string& userOptions, SPIRVResult& result)
{
    PROFILE_START(LinkSPIRVModules);
#ifndef SPIRV_FRONTEND
    throw CompilationError(CompilationStep::LINKER, "SPIR-V front-end is not provided!");
#else
    std::vector<std::istream*> convertedInputs;
    std::vector<std::unique_ptr<std::istream>> conversionBuffer;
    for(auto& source : sources)
    {
        if(source.file)
        {
            conversionBuffer.emplace_back(new std::ifstream(source.file.value()));
            convertedInputs.emplace_back(conversionBuffer.back().get());
        }
        else
            convertedInputs.emplace_back(source.stream);
    }

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Linking " << sources.size() << " input modules..." << logging::endl);
    spirv2qasm::linkSPIRVModules(convertedInputs, *result.stream);
#endif
    PROFILE_END(LinkSPIRVModules);
}

void precompilation::optimizeLLVMIR(LLVMIRSource&& source, const std::string& userOptions, LLVMIRResult& result)
{
    PROFILE_START(OptimizeLLVMIR);
#ifndef OPT_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "use_opt is not configured!");
#else
    std::string commandOpt = OPT_PATH;
    const std::string out = result.file ? std::string("-o=") + result.file.value() : "";
    const std::string in = source.file ? source.file.value() : "-";

    commandOpt.append(" -force-vector-width=16 -O3 ").append(out);

    char* envValue = getenv("VC4C_OPT");
    if(envValue)
    {
        commandOpt.append(" ");
        commandOpt.append(envValue);
    }
    if(!userOptions.empty())
    {
        // XXX opt does not support most of the "default" compiler options
        // commandOpt.append(" ").append(userOptions);
    }

    commandOpt.append(" ").append(in);

    CPPLOG_LAZY(logging::Level::INFO, log << "Optimizing LLVM IR module with opt: " << commandOpt << logging::endl);
    runPrecompiler(commandOpt, source.stream, result.stream);
#endif
    PROFILE_END(OptimizeLLVMIR);
}

void precompilation::optimizeLLVMText(
    LLVMIRTextSource&& source, const std::string& userOptions, LLVMIRTextResult& result)
{
    PROFILE_START(OptimizeLLVMText);
#ifndef OPT_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "use_opt is not configured!");
#else
    std::string commandOpt = OPT_PATH;
    const std::string out = result.file ? std::string("-o=") + result.file.value() : "";
    const std::string in = source.file ? source.file.value() : "-";

    commandOpt.append(" -force-vector-width=16 -O3 ").append(out);

    char* envValue = getenv("VC4C_OPT");
    if(envValue)
    {
        commandOpt.append(" ");
        commandOpt.append(envValue);
    }
    if(!userOptions.empty())
    {
        // XXX opt does not support most of the "default" compiler options
        // commandOpt.append(" ").append(userOptions);
    }

    commandOpt.append(" ").append(in);

    CPPLOG_LAZY(logging::Level::INFO, log << "Optimizing LLVM text with opt: " << commandOpt << logging::endl);
    runPrecompiler(commandOpt, source.stream, result.stream);
#endif
    PROFILE_END(OptimizeLLVMText);
}

void precompilation::compileOpenCLToLLVMIR(OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result)
{
// This check has the positive side-effect that if the VC4CLStdLib LLVM module is missing but the PCH exists, then
// the compilation with PCH (a bit slower but functional) will be used.
#ifdef LLVM_LINK_PATH
    if(!Precompiler::findStandardLibraryFiles().llvmModule.empty())
        return compileOpenCLAndLinkModule(std::move(source), userOptions, result);
#endif
    if(!Precompiler::findStandardLibraryFiles().precompiledHeader.empty())
        return compileOpenCLWithPCH(std::move(source), userOptions, result);
    throw CompilationError(
        CompilationStep::PRECOMPILATION, "Cannot include VC4CL standard library with neither PCH nor module defined");
}
