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
#include "git_commit.h"
#include "log.h"
#include "precompilation/FrontendCompiler.h"
#include "tool_paths.h"
#include "tools.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <string>
#include <unistd.h>
#include <unordered_map>

using namespace std;
using namespace vc4c;

extern void disassemble(const std::string& input, const std::string& output, OutputMode outputMode);

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
    std::map<std::string, std::string> sortedPasses;
    for(const auto& pass : vc4c::optimizations::Optimizer::ALL_PASSES)
        sortedPasses.emplace(pass.parameterName, pass.description);
    for(const auto& pass : sortedPasses)
    {
        std::cout << "\t--f" << std::left << std::setw(28) << pass.first << pass.second << std::endl;
        // TODO print which optimization level includes optimization
        std::cout << "\t--fno-" << std::left << std::setw(25) << pass.first << "Disables the above optimization"
                  << std::endl;
    }

    std::cout << "optimization parameters:" << std::endl;
    std::cout << "\t--fcombine-load-threshold=" << defaultConfig.additionalOptions.combineLoadThreshold
              << "\tThe maximum distance between two literal loads to combine" << std::endl;
    std::cout << "\t--faccumulator-threshold=" << defaultConfig.additionalOptions.accumulatorThreshold
              << "\tThe maximum live-range of a local still considered to be mapped to an accumulator" << std::endl;
    std::cout << "\t--freplace-nop-threshold=" << defaultConfig.additionalOptions.replaceNopThreshold
              << "\tThe number of instructions to search for a replacement for NOPs" << std::endl;
    std::cout << "\t--foptimization-iterations=" << defaultConfig.additionalOptions.maxOptimizationIterations
              << "\tThe maximum number of iterations to repeat the optimizations in" << std::endl;
    std::cout << "\t--fcommon-subexpression-threshold=" << defaultConfig.additionalOptions.maxCommonExpressionDinstance
              << "\tThe maximum distance for two common subexpressions to be combined" << std::endl;

    std::cout << "options:" << std::endl;
    std::cout << "\t--kernel-info\t\tWrite the kernel-info meta-data (as required by VC4CL run-time, default)"
              << std::endl;
    std::cout << "\t--no-kernel-info\tDont write the kernel-info meta-data" << std::endl;
    std::cout << "\t--spirv\t\t\tExplicitely use the SPIR-V front-end" << std::endl;
    std::cout << "\t--llvm\t\t\tExplicitely use the LLVM-IR front-end" << std::endl;
    std::cout << "\t--verification-error\tAbort if instruction verification failed" << std::endl;
    std::cout << "\t--no-verification-error\tContinue if instruction verification failed" << std::endl;
    std::cout << "\tany other option is passed to the pre-compiler" << std::endl;

    std::cout << "modes:" << std::endl;
    std::cout << "(modes completely change the behavior of the program and therefore only support a limited number of "
                 "options)"
              << std::endl;
    std::cout << "\t--disassemble\t\tDisassembles the binary input to either hex or assembler output. Only supports "
                 "the input-, output- and logging-flags listed above."
              << std::endl;
    std::cout << "\t--precompile-stdlib\tPre-compiles the the VC4CLStdLib.h header file given as input "
                 "into the folder specified as output. Ignores all other options except for the logging flags"
              << std::endl;
}

static std::string toVersionString(unsigned version)
{
    std::stringstream s;
    s << (static_cast<float>(version) / 10.0f);
    return s.str();
}

static void printInfo()
{
    std::cout << "Running VC4C in version: " << VC4C_VERSION << " (" << GIT_COMMIT << ')' << std::endl;
    std::cout << "Build configuration: ";
    static const std::vector<std::string> infoString = {
#ifndef NDEBUG
        "debug mode",
#endif
#ifdef SPIRV_TOOLS_FRONTEND
        "SPIR-V Tools front-end",
#else
        "builtin SPIR-V front-end",
#endif
#ifdef USE_LLVM_LIBRARY
        std::string("LLVM library front-end with libLLVM ") + toVersionString(LLVM_LIBRARY_VERSION),
#endif
#ifdef SPIRV_TOOLS_FRONTEND
        "SPIR-V linker",
#endif
#ifdef VERIFIER_HEADER
        "vc4asm verification",
#endif
#ifdef USE_CLANG_LIBRARY
        "compilation with clang library"
#endif
    };
    std::cout << vc4c::to_string<std::string>(infoString, std::string{"; "}) << '\n' << std::endl;

    std::cout << "Standard library location:" << std::endl;
    try
    {
        auto stdlib = precompilation::findStandardLibraryFiles();
        std::cout << "\theader in " << stdlib.configurationHeader << std::endl;
        std::cout << "\tPCH in " << stdlib.precompiledHeader << std::endl;
        std::cout << "\tLLVM module in " << stdlib.llvmModule << std::endl;
        std::cout << "\tSPIR-V module in " << stdlib.spirvModule << std::endl;
    }
    catch(const std::exception& err)
    {
        std::cout << "Failed to find standard library files!" << std::endl;
        std::cout << err.what() << std::endl;
    }

    std::cout << "Tool locations:" << std::endl;
    for(const auto& tool : std::vector<std::pair<std::string, std::string>>{{"clang", CLANG_PATH},
            {"llvm-spirv", SPIRV_LLVM_SPIRV_PATH}, {"llvm-link", LLVM_LINK_PATH}, {"llvm-dis", LLVM_DIS_PATH},
            {"llvm-as", LLVM_AS_PATH}, {"spirv-link", SPIRV_LINK_PATH}})
    {
        if(auto tool_found = precompilation::findToolLocation(tool.first, tool.second))
        {
            std::cout << "\t" << tool.first << " in " << *tool_found << " (default"
                      << (tool_found == tool.second ? ")" : (" '" + tool.second + "')")) << std::endl;
        }
        else
            std::cout << "\t" << tool.first << " not in "
                      << (!tool.second.empty() ? (tool.second + ", neither in ") : "") << "$PATH" << std::endl;
    }
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
#ifndef NDEBUG
    LogLevel minLevel = LogLevel::DEBUG;
#else
    LogLevel minLevel = LogLevel::WARNING;
#endif

    Configuration config;
    std::vector<std::string> inputFiles;
    std::string outputFile;
    std::string options;
    bool runDisassembler = false;
    bool precompileStdlib = false;
    std::wstringstream dummyLogOutput;

    if(argc == 1)
    {
        printHelp();
        return 0;
    }

    for(int i = 1; i < argc; ++i)
    {
        // treat an argument, which first character isnt "-", as an input file
        if(strcmp(argv[i - 1], "-D") == 0 || strcmp(argv[i - 1], "-U") == 0 || strcmp(argv[i - 1], "-I") == 0)
        {
            // ... with exception if they are preceded by an '-D', '-U' or '-I', then forward them to the front-end
            options.append(argv[i]).append(" ");
        }
        else if(argv[i][0] != '-')
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
            // disable all logging to not break our version output
            setLogger(dummyLogOutput, false, LogLevel::SEVERE);
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
                fileLog = std::make_unique<std::wofstream>(argv[i + 1]);
                logStream = *fileLog;
            }
            ++i;
        }
        else if(strcmp("--disassemble", argv[i]) == 0)
            runDisassembler = true;
        else if(strcmp("--precompile-stdlib", argv[i]) == 0)
            precompileStdlib = true;
        else if(strcmp("-o", argv[i]) == 0)
        {
            if(i + 1 == argc)
            {
                std::cerr << "No output file specified after -o, aborting!" << std::endl;
                return 7;
            }

            outputFile = argv[i + 1];

            // increment `i` more than usual, because argv[i + 1] is already consumed
            i += 1;
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
            std::cerr << "NOTE: If multiple files are inputed, specifying output file is MUST." << std::endl;
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
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Disassembling '" << inputFiles[0] << "' into '" << outputFile << "'..." << logging::endl);
        disassemble(inputFiles[0], outputFile, config.outputMode);
        return 0;
    }
    if(precompileStdlib)
    {
        if(inputFiles.size() != 1)
        {
            std::cerr
                << "For pre-compiling the VC4CL standard library, a single input file must be specified, aborting!"
                << std::endl;
            return 5;
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Pre-compiling '" << inputFiles[0] << "' into '" << outputFile << "'..." << logging::endl);
        precompilation::precompileStandardLibraryFiles(inputFiles[0], outputFile);
        return 0;
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Compiling '" << to_string<std::string>(inputFiles, std::string{"', '"}) << "' into '" << outputFile
            << "' with optimization level " << static_cast<unsigned>(config.optimizationLevel) << " and options '"
            << options << "' ..." << logging::endl);

    CompilationData input{};
    // link if necessary
    if(inputFiles.size() > 1)
    {
        std::vector<CompilationData> inputs;
        for(const std::string& file : inputFiles)
        {
            inputs.emplace_back(CompilationData{file});
        }
        input = Precompiler::linkSourceCode(inputs);
        if(!isSupportedByFrontend(input.getType(), config.frontend))
        {
            std::cerr << "Selected front-end does not support the input-format generated by the linker, aborting! "
                      << std::endl;
            return 5;
        }
    }
    else
    {
        input = CompilationData{inputFiles[0]};
    }

    PROFILE_START(Compiler);
    std::ignore = Compiler::compile(input, config, options, outputFile == "-" ? "/dev/stdout" : outputFile);
    PROFILE_END(Compiler);

    PROFILE_RESULTS();
    return 0;
}
