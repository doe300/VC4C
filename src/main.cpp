/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "./optimization/Optimizer.h"
#include "Compiler.h"
#include "Precompiler.h"
#include "Profiler.h"
#include "concepts.h"
#include "config.h"
#include "log.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <unistd.h>
#include <unordered_map>

using namespace std;
using namespace vc4c;

extern void disassemble(const std::string& input, const std::string& output, const OutputMode outputMode);

static void printHelp()
{
    std::cout << "Usage: vc4c [flags] [options] -o <destination> <sources>" << std::endl;
    std::cout << "flags:" << std::endl;
    std::cout << "\t-h, --help\t\tPrints this help message and exits" << std::endl;
    std::cout << "\t-v, --version\t\tPrints version and build info and exists" << std::endl;
    std::cout << "\t-d, --debug\t\tEnables verbose debug output" << std::endl;
    std::cout << "\t--quiet\t\t\tQuiet verbose debug output" << std::endl;
    std::cout << "\t--hex\t\t\tGenerate hex output (e.g. included in source-code)" << std::endl;
    std::cout << "\t--bin\t\t\tGenerate binary output (as used by VC4CL run-time)" << std::endl;
    std::cout << "\t--asm\t\t\tGenerate assembler output (for analysis only)" << std::endl;
    std::cout << "optimizations:" << std::endl;
    std::cout << "\t-O0,-O1,-O2,-O3\t\tSwitches to the specific optimization level, defaults to -O2" << std::endl;
    for(const auto& pass : vc4c::optimizations::Optimizer::ALL_PASSES)
    {
        std::cout << "\t--f" << std::left << std::setw(28) << pass.parameterName << pass.description << std::endl;
        // TODO print which optimization level includes optimization
        std::cout << "\t--fno-" << std::left << std::setw(25) << pass.parameterName << "Disables the above optimization"
                  << std::endl;
    }
    std::cout << "optimization parameters:" << std::endl;
    for(const auto& param : vc4c::optimizations::OPTIMIZATION_PARAMETER_DESCRIPTIONS)
    {
        std::cout << "\t--f" << std::left << std::setw(28)
                  << ((param.first + "=") + vc4c::DEFAULT_OPTIMIZATION_PARAMETERS.at(param.first)) << param.second
                  << std::endl;
    }
    std::cout << "options:" << std::endl;
    std::cout << "\t--kernel-info\t\tWrite the kernel-info meta-data (as required by VC4CL run-time, default)"
              << std::endl;
    std::cout << "\t--no-kernel-info\tDont write the kernel-info meta-data" << std::endl;
    std::cout << "\t--spirv\t\t\tExplicitely use the SPIR-V front-end" << std::endl;
    std::cout << "\t--llvm\t\t\tExplicitely use the LLVM-IR front-end" << std::endl;
    std::cout << "\t--disassemble\t\tDisassembles the binary input to either hex or assembler output" << std::endl;
    std::cout << "\tany other option is passed to the pre-compiler" << std::endl;
}

#ifndef LLVM_LIBRARY_VERSION
#define LLVM_LIBRARY_VERSION 0
#endif
#ifndef VC4C_VERSION
#define VC4C_VERSION ""
#endif

static std::string toVersionString(unsigned version)
{
    std::stringstream s;
    s << (version / 10.0f);
    return s.str();
}

static void printInfo()
{
    std::cout << "Running VC4C in version: " << VC4C_VERSION << std::endl;
    std::cout << "Build configuration:" << std::endl;
    static const std::vector<std::string> infoString = {
#ifdef DEBUG_MODE
        "debug mode",
#endif
#ifdef MULTI_THREADED
        "multi-threaded optimization",
#endif
#ifdef USE_CLANG_OPENCL
        "clang 3.9+ OpenCL features",
#endif
#if defined SPIRV_CLANG_PATH
        "SPIRV-LLVM clang in " SPIRV_CLANG_PATH,
#if defined SPIRV_LLVM_SPIRV_PATH and defined SPIRV_FRONTEND
        "SPIR-V front-end",
#endif
#elif defined CLANG_PATH
        "clang in " CLANG_PATH,
#endif
#ifdef USE_LLVM_LIBRARY
        std::string("LLVM library front-end with libLLVM ") + toVersionString(LLVM_LIBRARY_VERSION),
#endif
#ifdef VC4CL_STDLIB_HEADER
        "VC4CL standard-library in " VC4CL_STDLIB_HEADER,
#endif
#ifdef SPIRV_FRONTEND
        "SPIR-V linker",
#endif
#ifdef VERIFIER_HEADER
        "vc4asm verification"
#endif
    };

    std::cout << vc4c::to_string<std::string>(infoString, "; ") << std::endl;
}

static auto availableOptimizations = vc4c::optimizations::Optimizer::getPasses(OptimizationLevel::FULL);

bool parseOptimizationFlag(const std::string& arg, Configuration& config)
{
    std::string passName;
    if(arg.find("--fno-") == 0)
    {
        passName = arg.substr(std::string("--f-no-").size() - 1);
        if(availableOptimizations.find(passName) != availableOptimizations.end())
        {
            config.additionalDisabledOptimizations.emplace(passName);
            logging::debug() << "Disabling optimization: " << passName << logging::endl;
            return true;
        }

        std::cerr << "Cannot disable unknown optimization: " << passName << std::endl;
        return false;
    }
    else if(arg.find("--f") == 0)
    {
        passName = arg.substr(std::string("--f").size());
        if(passName.find('=') != std::string::npos)
        {
            // optimization parameter
            std::string value = passName.substr(passName.find('=') + 1);
            const std::string paramName = passName.substr(0, passName.find('='));
            auto it = vc4c::optimizations::OPTIMIZATION_PARAMETER_DESCRIPTIONS.find(paramName);
            if(it != vc4c::optimizations::OPTIMIZATION_PARAMETER_DESCRIPTIONS.end())
            {
                config.additionalOptimizationParameters[paramName] = value;
                return true;
            }
            std::cerr << "Cannot set unknown optimization parameter: " << paramName << " to " << value << std::endl;
            return false;
        }
        else if(availableOptimizations.find(passName) != availableOptimizations.end())
        {
            config.additionalEnabledOptimizations.emplace(passName);
            logging::debug() << "Enabling optimization: " << passName << logging::endl;
            return true;
        }
        std::cerr << "Cannot enable unknown optimization: " << passName << std::endl;
        return false;
    }
    return false;
}

/*
 *
 */
int main(int argc, char** argv)
{
#if DEBUG_MODE
    setLogger(std::wcout, true, LogLevel::DEBUG);
#else
    setLogger(std::wcout, true, LogLevel::WARNING);
#endif

    Configuration config;
    std::vector<std::string> inputFiles;
    std::string outputFile;
    std::string options;
    bool runDisassembler = false;

    if(argc < 3)
    {
        for(int i = 1; i < argc; ++i)
        {
            if(strcmp("--help", argv[i]) == 0 || strcmp("-h", argv[i]) == 0)
            {
                printHelp();
                return 0;
            }
            else if(strcmp("--version", argv[i]) == 0 || strcmp("-v", argv[i]) == 0)
            {
                printInfo();
                return 0;
            }
        }
        // needs at least <program> <output> <input>
        printHelp();
        return 1;
    }

    int i = 1;
    for(; i < argc - 2; ++i)
    {
        // flags
        if(strcmp("--help", argv[i]) == 0 || strcmp("-h", argv[i]) == 0)
        {
            printHelp();
            return 0;
        }
        else if(strcmp("--version", argv[i]) == 0 || strcmp("-v", argv[i]) == 0)
        {
            printInfo();
            return 0;
        }
        else if(strcmp("--quiet", argv[i]) == 0)
            setLogger(std::wcout, true, LogLevel::WARNING);
        else if(strcmp("--debug", argv[i]) == 0 || strcmp("-d", argv[i]) == 0)
            setLogger(std::wcout, true, LogLevel::DEBUG);
        else if(strcmp("--hex", argv[i]) == 0)
            config.outputMode = OutputMode::HEX;
        else if(strcmp("--bin", argv[i]) == 0)
            config.outputMode = OutputMode::BINARY;
        else if(strcmp("--asm", argv[i]) == 0)
            config.outputMode = OutputMode::ASSEMBLER;
        else if(strcmp("--fast-math", argv[i]) == 0)
            config.mathType = MathType::FAST;
        else if(strcmp("--exact-math", argv[i]) == 0)
            config.mathType = MathType::EXACT;
        else if(strcmp("--strict-math", argv[i]) == 0)
            config.mathType = MathType::STRICT;
        else if(strcmp("--kernel-info", argv[i]) == 0)
            config.writeKernelInfo = true;
        else if(strcmp("--no-kernel-info", argv[i]) == 0)
            config.writeKernelInfo = false;
        else if(strcmp("--spirv", argv[i]) == 0)
            config.frontend = Frontend::SPIR_V;
        else if(strcmp("--llvm", argv[i]) == 0)
            config.frontend = Frontend::LLVM_IR;
        else if(strcmp("--disassemble", argv[i]) == 0)
            runDisassembler = true;
        else if(strcmp("-o", argv[i]) == 0)
        {
            outputFile = argv[i + 1];
            // any further parameter is an input-file
            i += 2;
            break;
        }
        else if(strcmp("-O0", argv[i]) == 0)
            config.optimizationLevel = OptimizationLevel::NONE;
        else if(strcmp("-O1", argv[i]) == 0)
            config.optimizationLevel = OptimizationLevel::BASIC;
        else if(strcmp("-O2", argv[i]) == 0)
            config.optimizationLevel = OptimizationLevel::MEDIUM;
        else if(strcmp("-O3", argv[i]) == 0)
            config.optimizationLevel = OptimizationLevel::FULL;
        else if(!parseOptimizationFlag(argv[i], config))
            options.append(argv[i]).append(" ");
    }

    for(; i < argc; ++i)
    {
        inputFiles.emplace_back(argv[i]);
    }

    if(inputFiles.empty())
    {
        std::cerr << "No input file(s) specified, aborting!" << std::endl;
        return 2;
    }
    if(outputFile.empty())
    {
        std::cerr << "No output file specified, aborting!" << std::endl;
        return 3;
    }

    if(runDisassembler)
    {
        if(inputFiles.size() != 1)
        {
            std::cerr << "For disassembling, a single input file must be specified, aborting!" << std::endl;
            return 4;
        }
        logging::debug() << "Disassembling '" << inputFiles[0] << "' into '" << outputFile << "'..." << logging::endl;
        disassemble(inputFiles[0], outputFile, config.outputMode);
        return 0;
    }

    logging::debug() << "Compiling '" << to_string<std::string>(inputFiles, "', '") << "' into '" << outputFile
                     << "' with optimization level " << static_cast<unsigned>(config.optimizationLevel)
                     << " and options '" << options << "' ..." << logging::endl;

    Optional<std::string> inputFile;
    std::unique_ptr<std::istream> input;
    // link if necessary
    if(inputFiles.size() > 1)
    {
        std::vector<std::unique_ptr<std::istream>> fileStreams;
        std::unordered_map<std::istream*, Optional<std::string>> inputs;
        for(const std::string& file : inputFiles)
        {
            auto ifs = new std::ifstream(file);
            if(!ifs->is_open())
                throw CompilationError(CompilationStep::PRECOMPILATION, "cannot find file: " + file);
            fileStreams.emplace_back(ifs);
            inputs.emplace(fileStreams.back().get(), Optional<std::string>(file));
        }
        input.reset(new std::stringstream());
        SourceType linkedType = Precompiler::linkSourceCode(inputs, *reinterpret_cast<std::ostream*>(input.get()));
        if(!isSupportedByFrontend(linkedType, config.frontend))
        {
            std::cerr << "Selected front-end does not support the input-format generated by the linker, aborting! "
                      << std::endl;
            return 5;
        }
    }
    else
    {
        const auto& file = inputFiles[0];
        auto ifs = new std::ifstream(file);
        if(!ifs->is_open())
            throw CompilationError(CompilationStep::PRECOMPILATION, "cannot find file: " + file);
        input.reset(ifs);
        inputFile = inputFiles[0];
    }

    std::ofstream output(outputFile, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    PROFILE_START(Compiler);
    Compiler::compile(*input.get(), output, config, options, inputFile);
    PROFILE_END(Compiler);

    PROFILE_RESULTS();
    return 0;
}
