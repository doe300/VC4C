/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "FrontendCompiler.h"

#include "../ProcessUtil.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::precompilation;

static std::string buildClangCommand(const std::string& compiler, const std::string& defaultOptions,
    const std::string& options, const std::string& emitter, const std::string& outputFile,
    const std::string& inputFile = "-")
{
    // check validity of options - we do not support all of them
    if(options.find("-create-library") != std::string::npos)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid compilation options", options);

    // build command-string
    std::string command;
    command.append(compiler).append(" ").append(defaultOptions).append(" ").append(options).append(" ");

    // append default options
    if(options.find("-O") == std::string::npos)
    {
        // unroll loops, pre-calculate constants, inline functions, ...
        command.append("-O3 ");
    }
    if(options.find("-ffp-contract") == std::string::npos)
    {
        // disable fused floationg-point operations, since we do not support them anyway
        command.append("-ffp-contract=off ");
    }

#if defined USE_CLANG_OPENCL || defined SPIRV_CLANG_PATH
    if(options.find("-cl-std") == std::string::npos)
    {
        // build OpenCL 1.2
        command.append("-cl-std=CL1.2 ");
    }
    if(options.find("-cl-kernel-arg-info") == std::string::npos)
    {
        // make sure infos for arguments are generated
        command.append("-cl-kernel-arg-info ");
    }
    if(options.find("-cl-single-precision-constant") == std::string::npos)
    {
        // suppressed warnings about double constants
        command.append("-cl-single-precision-constant ");
    }
#endif
    // link in our standard-functions
    command.append(" -Wno-undefined-inline -Wno-unused-parameter -Wno-unused-local-typedef -Wno-gcc-compat ");
    command.append("-include-pch " VC4CL_STDLIB_HEADER " ");
    if(options.find("-x cl") == std::string::npos)
    {
        // build OpenCL, required when input is from stdin, since clang can't determine from file-type
        command.append("-x cl ");
    }
    // use temporary file as output
    // use stdin as input
    return command.append(emitter).append(" -o ").append(outputFile).append(" ").append(inputFile);
}

static void runPrecompiler(const std::string& command, std::istream* inputStream, std::ostream* outputStream)
{
    std::ostringstream stderr;
    int status = runProcess(command, inputStream, outputStream, &stderr);
    if(status == 0) // success
    {
        if(!stderr.str().empty())
        {
            logging::warn() << "Warnings in precompilation:" << logging::endl;
            logging::warn() << stderr.str() << logging::endl;
        }
        return;
    }
    if(!stderr.str().empty())
    {
        logging::error() << "Errors in precompilation:" << logging::endl;
        logging::error() << stderr.str() << logging::endl;
    }
    throw CompilationError(CompilationStep::PRECOMPILATION, "Error in precompilation", stderr.str());
}

static void compileOpenCLToLLVMIR(std::istream* input, std::ostream* output, const std::string& options,
    const bool toText = true, const Optional<std::string>& inputFile = {}, const Optional<std::string>& outputFile = {})
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
    const std::string command = buildClangCommand(compiler, defaultOptions, options,
        std::string("-S ").append(toText ? "-emit-llvm" : "-emit-llvm-bc"), outputFile.value_or("/dev/stdout"),
        inputFile.value_or("-"));

    logging::info() << "Compiling OpenCL to LLVM-IR with: " << command << logging::endl;

    runPrecompiler(command, inputFile ? nullptr : input, outputFile ? nullptr : output);
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

    logging::info() << "Converting LLVM-IR to SPIR-V with: " << command << logging::endl;

    runPrecompiler(command, inputFile ? nullptr : input, outputFile ? nullptr : output);
#endif
}

static void compileOpenCLToSPIRV(std::istream* input, std::ostream* output, const std::string& options,
    const bool toText = false, const Optional<std::string>& inputFile = {},
    const Optional<std::string>& outputFile = {})
{
#if not defined SPIRV_CLANG_PATH || not defined SPIRV_LLVM_SPIRV_PATH
    throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not configured, can't compile to SPIR-V!");
#elif not defined SPIRV_FRONTEND
    throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-Tools not configured, can't process SPIR-V!");
#endif

    TemporaryFile tmp;
    std::stringstream dummy;
    // 1) OpenCL C -> LLVM IR BC (with Khronos CLang)
    compileOpenCLToLLVMIR(input, &dummy, options, false, inputFile, tmp.fileName);
    // 2) LLVM IR BC -> SPIR-V
    try
    {
        compileLLVMIRToSPIRV0(&dummy, output, options, toText, tmp.fileName, outputFile);
    }
    catch(const CompilationError& e)
    {
        logging::warn() << "LLVM-IR to SPIR-V failed, trying to compile with the LLVM-IR front-end..." << logging::endl;
        logging::warn() << e.what() << logging::endl;
        compileOpenCLToLLVMIR(input, output, options, true, inputFile, outputFile);
    }
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

    logging::info() << "Converting between SPIR-V text and SPIR-V binary with: " << command << logging::endl;

    runPrecompiler(command, inputFile ? nullptr : input, outputFile ? nullptr : output);
#endif
}

void precompilation::compileOpenCLWithPCH(OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result)
{
    OpenCLSource src(std::forward<OpenCLSource>(source));
    compileOpenCLToLLVMIR(src.stream, result.stream, userOptions, false, src.file, result.getFile());
}

void precompilation::compileOpenCLToLLVMText(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRTextResult& result)
{
    OpenCLSource src(std::forward<OpenCLSource>(source));
    compileOpenCLToLLVMIR(src.stream, result.stream, userOptions, true, src.file, result.getFile());
}

void precompilation::compileLLVMToSPIRV(LLVMIRSource&& source, const std::string& userOptions, SPIRVResult& result)
{
    LLVMIRSource src(std::forward<LLVMIRSource>(source));
    compileLLVMIRToSPIRV0(src.stream, result.stream, userOptions, false, src.file, result.getFile());
}

void precompilation::assembleSPIRV(SPIRVTextSource&& source, const std::string& userOptions, SPIRVResult& result)
{
    SPIRVTextSource src(std::forward<SPIRVTextSource>(source));
    compileSPIRVToSPIRV(src.stream, result.stream, userOptions, false, src.file, result.getFile());
}

void precompilation::compileLLVMToSPIRVText(
    LLVMIRSource&& source, const std::string& userOptions, SPIRVTextResult& result)
{
    LLVMIRSource src(std::forward<LLVMIRSource>(source));
    compileLLVMIRToSPIRV0(src.stream, result.stream, userOptions, true, src.file, result.getFile());
}

void precompilation::disassembleSPIRV(SPIRVSource&& source, const std::string& userOptions, SPIRVTextResult& result)
{
    SPIRVSource src(std::forward<SPIRVSource>(source));
    compileSPIRVToSPIRV(src.stream, result.stream, userOptions, true, src.file, result.getFile());
}