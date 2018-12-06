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
#include "tools.h"

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
    vc4c::Configuration defaultConfig;
    std::cout << "Usage: vc4c [flags] [options] -o <destination> <sources>" << std::endl;
    std::cout << "flags:" << std::endl;
    std::cout << "\t-h, --help\t\tPrints this help message and exits" << std::endl;
    std::cout << "\t-v, --version\t\tPrints version and build info and exists" << std::endl;
    std::cout << "\t--verbose\t\tEnables verbose debug logging" << std::endl;
    std::cout << "\t--quiet\t\t\tQuiet verbose debug logging" << std::endl;
    std::cout << "\t-l, --log <file>\tWrite log output to the given file, defaults to stdout ('-')" << std::endl;
    std::cout << "\t--hex\t\t\tGenerate hex output (e.g. included in source-code)" << std::endl;
    std::cout << "\t--bin\t\t\tGenerate binary output (as used by VC4CL run-time)" << std::endl;
    std::cout << "\t--asm\t\t\tGenerate assembler output (for analysis only)" << std::endl;

    std::cout << "optimizations:" << std::endl;
    std::cout << "\t-O0,-O1,-O2,-O3\t\t\tSwitches to the specific optimization level, defaults to -O2" << std::endl;
    for(const auto& pass : vc4c::optimizations::Optimizer::ALL_PASSES)
    {
        std::cout << "\t--f" << std::left << std::setw(28) << pass.parameterName << pass.description << std::endl;
        // TODO print which optimization level includes optimization
        std::cout << "\t--fno-" << std::left << std::setw(25) << pass.parameterName << "Disables the above optimization"
                  << std::endl;
    }

    std::cout << "optimization parameters:" << std::endl;
    std::cout << "\t--fcombine-load-threshold=" << defaultConfig.additionalOptions.combineLoadThreshold
              << "\tThe maximum distance between two literal loads to combine" << std::endl;
    std::cout << "\t--faccumulator-threshold=" << defaultConfig.additionalOptions.accumulatorThreshold
              << "\tThe maximum live-range of a local still considered to be mapped to an accumulator" << std::endl;
    std::cout << "\t--freplace-nop-threshold=" << defaultConfig.additionalOptions.replaceNopThreshold
              << "\tThe number of instructions to search for a replacement for NOPs" << std::endl;
    std::cout << "\t--fregister-resolver-rounds=" << defaultConfig.additionalOptions.registerResolverMaxRounds
              << "\tThe maximum number of rows for the register allocator" << std::endl;
    std::cout << "\t--fmove-constants-depth=" << defaultConfig.additionalOptions.moveConstantsDepth
              << "\tThe maximum depth of nested loops to move constants out of" << std::endl;
    std::cout << "\t--foptimization-iterations=" << defaultConfig.additionalOptions.maxOptimizationIterations
              << "\tThe maximum number of iterations to repeat the optimizations in" << std::endl;

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

/*
 *
 */
int main(int argc, char** argv)
{
    std::unique_ptr<std::wofstream> fileLog;
    std::reference_wrapper<std::wostream> logStream = std::wcout;
    bool colorLog = true;
#if DEBUG_MODE
    LogLevel minLevel = LogLevel::DEBUG;
#else
    LogLevel minLevel = LogLevel::WARNING;
#endif

    Configuration config;
    std::vector<std::string> inputFiles;
    std::string outputFile;
    std::string options;
    bool runDisassembler = false;

    int i = 1;
    for(; i < argc; ++i)
    {
        // treat an argument, which first character isnt "-", as an input file
        if(argv[i][0] != '-')
        {
            inputFiles.emplace_back(argv[i]);
        }

        // flags
        else if(strcmp("--help", argv[i]) == 0 || strcmp("-h", argv[i]) == 0)
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
            minLevel = LogLevel::WARNING;
        else if(strcmp("--verbose", argv[i]) == 0)
            minLevel = LogLevel::DEBUG;
        else if(strcmp("--log", argv[i]) == 0 || strcmp("-l", argv[i]) == 0)
        {
            if(strcmp("-", argv[i + 1]) == 0)
            {
                colorLog = true;
                logStream = std::wcout;
            }
            else
            {
                colorLog = false;
                fileLog.reset(new std::wofstream(argv[i + 1]));
                logStream = *fileLog.get();
            }
            ++i;
        }
        else if(strcmp("--disassemble", argv[i]) == 0)
            runDisassembler = true;
        else if(strcmp("-o", argv[i]) == 0)
        {
            outputFile = argv[i + 1];
            // any further parameter is an input-file
            i += 2;
            break;
        }
        else if(!vc4c::tools::parseConfigurationParameter(config, argv[i]) || strstr(argv[i], "-cl") == argv[i])
            // pass every not understood option to the pre-compiler, as well as every OpenCL compiler option
            options.append(argv[i]).append(" ");
    }

    if(&logStream.get() == &std::wcout && outputFile == "-")
    {
        std::cerr << "Cannot write both log and data to stdout, aborting" << std::endl;
        return 6;
    }
    setLogger(logStream, colorLog, minLevel);

    if(inputFiles.empty())
    {
        std::cerr << "No input file(s) specified, aborting!" << std::endl;
        return 2;
    }
    if(outputFile.empty())
    {
        // special case: if input files is just one, we specify the implicit output file.
        std::string postfix;
        if(inputFiles.size() == 1)
        {
            switch(config.outputMode)
            {
            case OutputMode::BINARY:
                postfix = ".bin";
                break;
            case OutputMode::HEX:
                postfix = ".hex";
                break;
            case OutputMode::ASSEMBLER:
                postfix = ".s";
                break;
            }
            outputFile = inputFiles[0] + postfix;
        }
        else
        {
            std::cerr << "No output file specified, aborting!" << std::endl;
            return 3;
        }
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
                throw CompilationError(CompilationStep::PRECOMPILATION, "cannot find file", file);
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
            throw CompilationError(CompilationStep::PRECOMPILATION, "cannot find file", file);
        input.reset(ifs);
        inputFile = inputFiles[0];
    }

    std::ofstream output(outputFile == "-" ? "/dev/stdout" : outputFile,
        std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    PROFILE_START(Compiler);
    Compiler::compile(*input.get(), output, config, options, inputFile);
    PROFILE_END(Compiler);

    PROFILE_RESULTS();
    return 0;
}
