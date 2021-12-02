/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "FrontendCompiler.h"

#include "../ProcessUtil.h"
#include "../Profiler.h"
#include "../helper.h"
#include "../performance.h"
#include "../spirv/SPIRVToolsParser.h"
#include "LLVMLibrary.h"
#include "log.h"

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iterator>
#include <mutex>
#include <numeric>
#include <sstream>
#include <unistd.h>

using namespace vc4c;
using namespace vc4c::precompilation;

CompilationDataPrivate::~CompilationDataPrivate() noexcept = default;

static std::vector<std::string> buildClangCommand(const std::string& compiler, const std::string& defaultOptions,
    const std::string& options, const std::string& emitter, const std::string& outputFile, const std::string& inputFile,
    bool usePCH)
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

    /*
     * Problem:
     * "In C99, inline means that a function's definition is provided only for inlining, and that there is another
     * definition (without inline) somewhere else in the program." - https://clang.llvm.org/compatibility.html#inline
     *
     * -> Unless the function is actually inlined (which the compiler decides on its own), it will only be declared, not
     * defined resulting in compilation error.
     *
     * See https://clang.llvm.org/compatibility.html#inline
     *
     * As solution, we set inline behavior to C89, even OpenCL is based on C99
     */
    if(options.find("-cl-std=CLC++") == std::string::npos && options.find("-cl-std=clc++") == std::string::npos &&
        options.find("-std=CLC++") == std::string::npos && options.find("-std=clc++") == std::string::npos)
        // apparently this flag is not allowed for "C++ for OpenCL" mode or probably C++ in general
        command.emplace_back("-fgnu89-inline");
    // link in our standard-functions
    command.emplace_back("-Wno-undefined-inline");
    command.emplace_back("-Wno-unused-parameter");
    command.emplace_back("-Wno-unused-local-typedef");
    command.emplace_back("-Wno-gcc-compat");
    if(usePCH)
    {
        command.emplace_back("-include-pch");
        command.emplace_back(findStandardLibraryFiles().precompiledHeader);
    }
    else
    {
        command.emplace_back("-finclude-default-header");
        // The #defines (esp. for extensions) from the default headers differ from the supported #defines,
        // so we need to include our #defines/undefines
        command.emplace_back("-include");
        command.emplace_back(findStandardLibraryFiles().configurationHeader);
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

static void runPrecompiler(const std::string& command, std::unique_ptr<std::istream>&& inputStream,
    std::unique_ptr<std::ostream>&& outputStream)
{
    std::ostringstream stderr;
    int status = runProcess(command, inputStream.get(), outputStream.get(), &stderr);
    if(status == 0) // success
    {
        LCOV_EXCL_START
        logging::logLazy(logging::Level::WARNING, [&]() {
            if(!stderr.str().empty())
            {
                logging::warn() << "Warnings in precompilation:" << logging::endl;
                logging::warn() << stderr.str() << logging::endl;
            }
        });
        LCOV_EXCL_STOP
        return;
    }
    if(!stderr.str().empty())
    {
        logging::error() << "Errors in precompilation:" << logging::endl;
        logging::error() << stderr.str() << logging::endl;
    }
    throw CompilationError(CompilationStep::PRECOMPILATION, "Error in precompilation", stderr.str());
}

template <typename EmitterTag, SourceType OutputType>
static void compileOpenCLToLLVMIR0(
    const OpenCLSource& input, PrecompilationResult<OutputType>& output, const std::string& options, bool withPCH)
{
    // in both instances, compile to SPIR to match the "architecture" the PCH was compiled for
    const std::string defaultOptions = "-cc1 -triple spir-unknown-unknown";
    // only run preprocessor and compilation, no linking and code-generation
    // emit LLVM IR
    auto command =
        buildClangCommand(CLANG_PATH, defaultOptions, options, std::string("-S ").append(EmitterTag::argument),
            output.getOutputPath("/dev/stdout"), input.getInputPath("-"), withPCH);

    auto commandString = to_string<std::string>(command, std::string{" "});

    if(hasClangLibrary())
    {
        CPPLOG_LAZY(logging::Level::INFO,
            log << "Compiling OpenCL to LLVM-IR via clang library with: " << commandString << logging::endl);
        compileClangLibrary(command, input.inner(), output, EmitterTag{});
    }
    else
    {
        CPPLOG_LAZY(
            logging::Level::INFO, log << "Compiling OpenCL to LLVM-IR with: " << commandString << logging::endl);
        runPrecompiler(commandString, input.getBufferReader(), output.getBufferWriter());
    }
}

template <SourceType OutputType>
static void compileLLVMIRToSPIRV0(const LLVMIRSource& input, PrecompilationResult<OutputType>& output,
    const std::string& options, const bool toText = false)
{
    auto llvm_spirv = findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH);
    if(!llvm_spirv)
        throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not found, can't compile to SPIR-V!");

    std::string command = (*llvm_spirv + (toText ? " -spirv-text" : "")) + " -o ";
    command.append(output.getOutputPath("/dev/stdout")).append(" ");
    command.append(input.getInputPath("/dev/stdin"));

    CPPLOG_LAZY(logging::Level::INFO, log << "Converting LLVM-IR to SPIR-V with: " << command << logging::endl);

    runPrecompiler(command, input.getBufferReader(), output.getBufferWriter());
}

template <SourceType InputType, SourceType OutputType>
static void compileSPIRVToSPIRV(const PrecompilationSource<InputType>& input, PrecompilationResult<OutputType>& output,
    const std::string& options, const bool toText = false)
{
    auto llvm_spirv = findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH);
    if(!llvm_spirv)
        throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not found, can't compile to SPIR-V!");

    std::string command = (*llvm_spirv + " " + (toText ? " -to-text" : " -to-binary")) + " -o ";
    command.append(output.getOutputPath("/dev/stdout")).append(" ");
    command.append(input.getInputPath("/dev/stdin"));

    CPPLOG_LAZY(logging::Level::INFO,
        log << "Converting between SPIR-V text and SPIR-V binary with: " << command << logging::endl);

    runPrecompiler(command, input.getBufferReader(), output.getBufferWriter());
}

template <SourceType Type>
static PrecompilationResult<Type> forwardOrCreateResult(PrecompilationResult<Type>&& result)
{
    if(result)
        return std::move(result);
    return PrecompilationResult<Type>{std::make_unique<TemporaryFileCompilationData<Type>>()};
}

template <>
LLVMIRResult forwardOrCreateResult<SourceType::LLVM_IR_BIN>(LLVMIRResult&& result)
{
    if(result)
        return std::move(result);
    if(hasClangLibrary())
        return LLVMIRResult{createLLVMCompilationData()};
    else
        return LLVMIRResult{std::make_unique<TemporaryFileCompilationData<SourceType::LLVM_IR_BIN>>()};
}

template <SourceType Type>
static PrecompilationResult<Type> forwardOrCreateFileResult(PrecompilationResult<Type>&& result)
{
    if(dynamic_cast<const FileCompilationData<Type>*>(&result.inner()))
        return std::move(result);
    return PrecompilationResult<Type>{std::make_unique<TemporaryFileCompilationData<Type>>()};
}

LLVMIRResult precompilation::compileOpenCLWithPCH(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    PROFILE_SCOPE(CompileOpenCLWithPCH);
    OpenCLSource src(std::forward<OpenCLSource>(source));
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileOpenCLToLLVMIR0<OutputLLVMModuleTag>(src, result, userOptions, true);
    return result;
}

/**
 * For the llvm-link hack/workaround below (see #getEmptyModule()) always build at least a second time without
 * any user options set. To avoid running this step at least twice, remove some user options which do not influence our
 * OpenCL C header PCH build, e.g. include directories in a hope to have more matches
 */
static std::string cleanOptions(std::string userOptions)
{
    const std::string includeFlag = "-I ";
    std::string::size_type pos;
    while((pos = userOptions.find(includeFlag)) != std::string::npos)
    {
        // remove the -I and the actual include path while heeding quotes
        auto endPos = pos + includeFlag.size();
        if(userOptions[endPos] == '"')
            endPos = userOptions.find('"', endPos + 1) + 1;
        else if(userOptions[endPos] == '\'')
            endPos = userOptions.find('\'', endPos + 1) + 1;
        else
            endPos = userOptions.find(' ', endPos + 1);
        userOptions.erase(pos, endPos - pos);
    }

    /*
     * To greatly reduce the number of PCHs to build, also remove any macro definition without an underscore in it.
     *
     * All macros used by clang's opencl-c.h have at least a single underscore, although the header defines
     * macros without one (e.g. NAN, INFINITY).
     */
    for(auto keyword : {"-D", "-U"})
    {
        pos = 0;
        while((pos = userOptions.find(keyword, pos)) != std::string::npos)
        {
            auto endPos = userOptions.find(' ', pos);
            auto macro = userOptions.substr(pos, endPos - pos);
            if(macro == keyword)
            {
                // clang allows spaces between '-D'/'-U' and the actual macro, e.g. "-D TYPE=long", so heed them!
                endPos = userOptions.find(' ', endPos + 1);
                macro = userOptions.substr(pos, endPos - pos);
            }
            if(macro.find('_') == std::string::npos)
                userOptions.erase(pos, macro.size() + 1);
            else
                pos = endPos;
        }
    }

    // trim leading and trailing zeroes
    userOptions.erase(0, userOptions.find_first_not_of(' '));
    if(!userOptions.empty())
        userOptions.erase(userOptions.find_last_not_of(' ') + 1);
    return userOptions;
}

/**
 * Most of the time of a "normal" compilation for simple kernels is consumed by reading the clang provided OpenCL C
 * header file (e.g. in /usr/lib/clang/<version>/opencl-c.h), as reported by the clang "-ftime-trace" flag.
 * Since the file is always the same for all successive compilations, we precompile the header into a PCH (depending
 * on the user-flags, to correctly handle extensions and optimizations) and include this PCH to speed up all but the
 * first compilations.
 *
 * To precompile the default OpenCL C header to a PCH, we simply precompile an empty OpenCL C kernel into PCH while
 * including the default header.
 */
static Optional<std::string> getDefaultHeadersPCHPath(const std::string& userOptions)
{
    static std::mutex pchsMutex;
    // we keep a usage count to always discard the least used entries
    static FastMap<std::string, std::pair<TemporaryFile, std::size_t>> cachedPCHs;

    auto checkOptions = cleanOptions(userOptions);
    std::lock_guard<std::mutex> guard(pchsMutex);
    auto it = cachedPCHs.find(checkOptions);
    PROFILE_COUNTER(vc4c::profiler::COUNTER_FRONTEND, "OpenCL C header PCH builds", it == cachedPCHs.end());
    if(it != cachedPCHs.end())
    {
        ++it->second.second;
        return it->second.first.fileName;
    }

    // keep a limit on the maximum cached PCHs to not run out of space in /tmp if a lot of kernels are compiled (e.g.
    // for VC4CL tests)
    if(cachedPCHs.size() >= 32)
    {
        // remove all entries with single use at once to not have to do this check too often
        for(auto it = cachedPCHs.begin(); it != cachedPCHs.end();)
        {
            if(it->second.second <= 1)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Dropping precompiled OpenCL C header from cache due to cache full: "
                        << it->second.first.fileName << " (options: '" << it->first
                        << "', usages: " << it->second.second << ")" << logging::endl);
                it = cachedPCHs.erase(it);
            }
            else
                ++it;
        }

        // if still too many entries, remove least used
        if(cachedPCHs.size() > 16)
        {
            auto it = std::min_element(cachedPCHs.begin(), cachedPCHs.end(),
                [](const auto& one, const auto& other) -> bool { return one.second.second < other.second.second; });
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Dropping precompiled OpenCL C header from cache due to cache full: "
                    << it->second.first.fileName << " (options: '" << it->first << "', usages: " << it->second.second
                    << ")" << logging::endl);
            cachedPCHs.erase(it);
        }
    }

    it = cachedPCHs.emplace(checkOptions, std::make_pair(TemporaryFile{"/tmp/vc4c-openclc-pch-XXXXXX", true}, 1)).first;
    try
    {
        OpenCLSource emptySource{std::make_unique<RawCompilationData<SourceType::OPENCL_C>>()};
        LLVMIRResult result{it->second.first.fileName};
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Precompiling default OpenCL C header to PCH to speed up further clang front-end runs for "
                   "compilation flags: "
                << checkOptions << logging::endl);
        compileOpenCLToLLVMIR0<OutputPCHTag>(emptySource, result, checkOptions, false);
        return it->second.first.fileName;
    }
    catch(const std::exception&)
    {
        // if we fail, just try to do the "normal" compilation without the PCH
        cachedPCHs.erase(it);
        return {};
    }
}

LLVMIRResult precompilation::compileOpenCLWithDefaultHeader(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    PROFILE_START(PrecompileOpenCLCHeaderToPCH);
    auto pchPath = getDefaultHeadersPCHPath(userOptions);
    PROFILE_END(PrecompileOpenCLCHeaderToPCH);

    PROFILE_START(CompileOpenCLWithDefaultHeader);
    OpenCLSource src(std::forward<OpenCLSource>(source));
    auto actualOptions = pchPath ? userOptions + " -include-pch " + *pchPath : userOptions;
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileOpenCLToLLVMIR0<OutputLLVMModuleTag>(src, result, actualOptions, false);
    PROFILE_END(CompileOpenCLWithDefaultHeader);
    return result;
}

LLVMIRResult precompilation::linkInStdlibModule(
    LLVMIRSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    if(findStandardLibraryFiles().llvmModule.empty())
        throw CompilationError(CompilationStep::LINKER, "LLVM IR module for VC4CL std-lib is not defined!");
    std::vector<LLVMIRSource> sources;
    sources.emplace_back(std::forward<LLVMIRSource>(source));
    sources.emplace_back(findStandardLibraryFiles().llvmModule);
    // set options to reduce output module size by only linking in required std-lib symbols
    /*
     * We could also set "-internalize" which makes all non-kernel functions to internal functions (internal linkage).
     * This only helps us however, if we do further processing and not if we read the module directly.
     * Also, this causes the SPIRV-LLVM translator to discard the function definitions for VC4CL std-lib implentations
     * of OpenCL C standard library functions converted back to OpExtInst operations.
     */
    return linkLLVMModules(std::move(sources), "-only-needed", std::move(desiredOutput));
}

LLVMIRTextResult precompilation::compileOpenCLToLLVMText(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRTextResult&& desiredOutput)
{
    PROFILE_SCOPE(CompileOpenCLToLLVMText);
    OpenCLSource src(std::forward<OpenCLSource>(source));
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileOpenCLToLLVMIR0<OutputLLVMTextTag>(src, result, userOptions, true);
    return result;
}

SPIRVResult precompilation::compileLLVMToSPIRV(
    LLVMIRSource&& source, const std::string& userOptions, SPIRVResult&& desiredOutput)
{
    PROFILE_SCOPE(CompileLLVMToSPIRV);
    LLVMIRSource src(std::forward<LLVMIRSource>(source));
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileLLVMIRToSPIRV0(src, result, userOptions, false);
    return result;
}

SPIRVResult precompilation::assembleSPIRV(
    SPIRVTextSource&& source, const std::string& userOptions, SPIRVResult&& desiredOutput)
{
    PROFILE_SCOPE(AssembleSPIRV);
    SPIRVTextSource src(std::forward<SPIRVTextSource>(source));
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileSPIRVToSPIRV(src, result, userOptions, false);
    return result;
}

SPIRVTextResult precompilation::compileLLVMToSPIRVText(
    LLVMIRSource&& source, const std::string& userOptions, SPIRVTextResult&& desiredOutput)
{
    PROFILE_SCOPE(CompileLLVMToSPIRVText);
    LLVMIRSource src(std::forward<LLVMIRSource>(source));
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileLLVMIRToSPIRV0(src, result, userOptions, true);
    return result;
}

SPIRVTextResult precompilation::disassembleSPIRV(
    SPIRVSource&& source, const std::string& userOptions, SPIRVTextResult&& desiredOutput)
{
    PROFILE_SCOPE(DisassembleSPIRV);
    SPIRVSource src(std::forward<SPIRVSource>(source));
    auto result = forwardOrCreateResult(std::move(desiredOutput));
    compileSPIRVToSPIRV(src, result, userOptions, true);
    return result;
}

LLVMIRTextResult precompilation::disassembleLLVM(
    LLVMIRSource&& source, const std::string& userOptions, LLVMIRTextResult&& desiredOutput)
{
    PROFILE_SCOPE(DisassembleLLVM);
    auto llvm_dis = findToolLocation("llvm-dis", LLVM_DIS_PATH);
    if(!llvm_dis)
        throw CompilationError(CompilationStep::PRECOMPILATION, "llvm-dis not found, can't disassemble LLVM IR!");

    auto result = forwardOrCreateFileResult(std::move(desiredOutput));
    std::string command = *llvm_dis;
    command.append(" -o ").append(result.getFilePath().value()).append(" ");
    command.append(source.getInputPath("/dev/stdin"));
    CPPLOG_LAZY(
        logging::Level::INFO, log << "Disassembling LLVM IR to LLVM IR text with: " << command << logging::endl);
    runPrecompiler(command, source.getBufferReader(), nullptr);
    return result;
}

LLVMIRResult precompilation::assembleLLVM(
    LLVMIRTextSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    PROFILE_SCOPE(AssembleLLVM);
    auto llvm_as = findToolLocation("llvm-as", LLVM_AS_PATH);
    if(!llvm_as)
        throw CompilationError(CompilationStep::PRECOMPILATION, "llvm-as not found, can't assemble LLVM IR!");

    auto result = forwardOrCreateFileResult(std::move(desiredOutput));
    std::string command = *llvm_as;
    command.append(" -o ").append(result.getFilePath().value()).append(" ");
    command.append(source.getInputPath("/dev/stdin"));
    CPPLOG_LAZY(logging::Level::INFO, log << "Assembling LLVM IR text to LLVM IR with: " << command << logging::endl);
    runPrecompiler(command, source.getBufferReader(), nullptr);
    return result;
}

static std::string getEmptyModule()
{
    static TemporaryFile emptyModule = []() {
        TemporaryFile empty{"/tmp/vc4c-empty-XXXXXX", true};

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Compiling empty module to work around llvm-link bug/feature..." << logging::endl);
        std::stringstream ss{""};
        compileOpenCLWithDefaultHeader(OpenCLSource{ss}, "", LLVMIRResult{empty.fileName});

        return empty;
    }();

    return emptyModule.fileName;
}

template <typename T>
std::string convertSourcesToFiles(std::unique_ptr<std::istream>& inputStream, std::vector<T>& sources,
    std::vector<std::unique_ptr<TemporaryFile>>& tempFiles, bool addOverride)
{
    return std::accumulate(
        sources.begin(), sources.end(), std::string{}, [&](const std::string& a, const T& b) -> std::string {
            /*
             * If we have multiple input files compiled with the VC4CC compiler, then they might all contain the
             * definition/implementation of one or more VC4CL std-lib functions (e.g. get_global_id()).
             * To not fail on ODR violations, we allow all but the first linked in modules to simply override
             * already defined symbols from the previous modules.
             * TODO is there a better solution?
             */
            auto separator = a.empty() || !addOverride ? " " : " -override=";
            if(auto path = b.getFilePath())
                return a + separator + path.value();
            if(sources.size() > 1 || inputStream)
            {
                /*
                 * We can only have at most a single input from stdin for the link process.
                 *
                 * Also, if we have multiple inputs, at some point the linker process (at least for llvm-link) has
                 * troubles correctly handling the stdin (at least the way we set it), so just always use input
                 * files in this case.
                 */
                tempFiles.emplace_back(std::make_unique<TemporaryFile>());
                std::unique_ptr<std::ostream> s;
                auto& file = tempFiles.back();
                file->openOutputStream(s);
                (*s) << b.getBufferReader()->rdbuf();
                return a + separator + file->fileName;
            }
            else
            {
                inputStream = b.getBufferReader();
                // can't use "-" for stdin, since spirv-link does not recognize it
                return a + " /dev/stdin";
            }
        });
}

LLVMIRResult precompilation::linkLLVMModules(
    std::vector<LLVMIRSource>&& sources, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    // TODO add call to llvm-lto??!
    PROFILE_SCOPE(LinkLLVMModules);

    auto llvm_link = findToolLocation("llvm-link", LLVM_LINK_PATH);
    if(!llvm_link)
        throw CompilationError(CompilationStep::PRECOMPILATION, "llvm-link not found, can't link LLVM IR modules!");

    if(sources.empty())
        throw CompilationError(CompilationStep::PRECOMPILATION, "Cannot link without input files!");

    // only one input can be from a stream
    std::unique_ptr<std::istream> inputStream = nullptr;
    // this is needed, since we can use a maximum of 1 stream input
    std::vector<std::unique_ptr<TemporaryFile>> tempFiles;
    std::string inputs = convertSourcesToFiles(inputStream, sources, tempFiles, true);

    /*
     * There is a feature/bug in llvm-link which discards all flags (e.g. the "-only-needed" flag set for linking in the
     * VC4CL std-lib) for the first module with and the first module without the "-override" flag, see
     * https://github.com/llvm/llvm-project/blob/master/llvm/tools/llvm-link/llvm-link.cpp#L279.
     *
     * This results i.e. in all std-lib functions being linked in the resulting modules from the VC4CL std-lib module,
     * which then results is much larger LLVM module parsing times.
     *
     * To circumvent this, we link in an empty module as first module with the "-override" flag, which results in the
     * other flags being correctly applied for all successive modules.
     */
    auto emptyInput = " -override=" + getEmptyModule();
    auto result = forwardOrCreateFileResult(std::move(desiredOutput));

    /*
     * NOTE: cannot use " -only-needed -internalize" in general case, since symbols used across module boundaries are
     * otherwise optimized away. " -only-needed -internalize" is now only used when linking in the standard-library.
     */
    const std::string out = std::string("-o=") + result.getFilePath().value();
    std::string command = *llvm_link + " " + userOptions + " " + out + " " + emptyInput + " " + inputs;

    // llvm-link does not like multiple white-spaces in the list of files (assumes file with empty name)
    std::size_t n = 0;
    while((n = command.find("  ", n)) != std::string::npos)
    {
        command.replace(n, 2, " ");
    }

    CPPLOG_LAZY(logging::Level::INFO, log << "Linking LLVM-IR modules with: " << command << logging::endl);

    runPrecompiler(command, std::move(inputStream), nullptr);
    return result;
}

SPIRVResult precompilation::linkSPIRVModules(
    std::vector<SPIRVSource>&& sources, const std::string& userOptions, SPIRVResult&& desiredOutput)
{
    PROFILE_SCOPE(LinkSPIRVModules);
    if(auto spirv_link = findToolLocation("spirv-link", SPIRV_LINK_PATH))
    {
        // only one input can be from a stream
        std::unique_ptr<std::istream> inputStream = nullptr;
        // this is needed, since we can use a maximum of 1 stream input
        std::vector<std::unique_ptr<TemporaryFile>> tempFiles;
        std::string inputs = convertSourcesToFiles(inputStream, sources, tempFiles, false);

        auto result = forwardOrCreateFileResult(std::move(desiredOutput));
        auto out = std::string("-o=") + result.getFilePath().value();
        // the VC4CL intrinsics are not provided by any input module
        auto customOptions = "--allow-partial-linkage --verify-ids --target-env opencl1.2embedded";
        std::string command = *spirv_link + " " + userOptions + " " + customOptions + " " + out + " " + inputs;

        // spirv-link does not like multiple white-spaces in the list of files (assumes file with empty name)
        std::size_t n = 0;
        while((n = command.find("  ", n)) != std::string::npos)
        {
            command.replace(n, 2, " ");
        }

        CPPLOG_LAZY(logging::Level::INFO, log << "Linking SPIR-V modules with: " << command << logging::endl);
        runPrecompiler(command, std::move(inputStream), nullptr);
        return result;
    }
    else if(hasSPIRVToolsFrontend())
    {
        std::vector<std::istream*> convertedInputs;
        std::vector<std::unique_ptr<std::istream>> conversionBuffer;
        for(auto& source : sources)
        {
            conversionBuffer.emplace_back(source.inner().readStream());
            convertedInputs.emplace_back(conversionBuffer.back().get());
        }

        CPPLOG_LAZY(logging::Level::DEBUG, log << "Linking " << sources.size() << " input modules..." << logging::endl);
        std::stringstream ss;
        spirv::linkSPIRVModules(convertedInputs, ss);
        if(!desiredOutput)
            desiredOutput = SPIRVResult{std::make_unique<RawCompilationData<SourceType::SPIRV_BIN>>()};
        desiredOutput.inner().writeFrom(ss);
        return std::move(desiredOutput);
    }
    else
        throw CompilationError(CompilationStep::LINKER, "SPIR-V Tools front-end is not provided!");
}

LLVMIRResult precompilation::optimizeLLVMIR(
    LLVMIRSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    PROFILE_SCOPE(OptimizeLLVMIR);

    auto opt = findToolLocation("opt", OPT_PATH);
    if(!opt)
        throw CompilationError(CompilationStep::PRECOMPILATION, "opt not found, can't optimize LLVM IR!");

    auto result = forwardOrCreateFileResult(std::move(desiredOutput));
    std::string commandOpt = *opt;
    const std::string out = std::string("-o=") + result.getFilePath().value();
    const std::string in = source.getInputPath("-");

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
    runPrecompiler(commandOpt, source.getBufferReader(), nullptr);
    return result;
}

LLVMIRResult precompilation::compileOpenCLToLLVMIR(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    // This check has the positive side-effect that if the VC4CLStdLib LLVM module is missing but the PCH exists,
    // then the compilation with PCH (a bit slower but functional) will be used.
    auto llvm_link = findToolLocation("llvm-link", LLVM_LINK_PATH, true);
    if(llvm_link && !findStandardLibraryFiles().llvmModule.empty())
        return compileOpenCLAndLinkModule(std::move(source), userOptions, std::move(desiredOutput));

    if(!findStandardLibraryFiles().precompiledHeader.empty())
        return compileOpenCLWithPCH(std::move(source), userOptions, std::move(desiredOutput));
    throw CompilationError(
        CompilationStep::PRECOMPILATION, "Cannot include VC4CL standard library with neither PCH nor module defined");
}

LLVMIRResult precompilation::compileOpenCLAndLinkModule(
    OpenCLSource&& source, const std::string& userOptions, LLVMIRResult&& desiredOutput)
{
    auto tmp = compileOpenCLWithDefaultHeader(std::move(source), userOptions);
    return linkInStdlibModule(LLVMIRSource{std::move(tmp)}, userOptions, std::move(desiredOutput));
}

SPIRVResult precompilation::compileOpenCLToSPIRV(
    OpenCLSource&& source, const std::string& userOptions, SPIRVResult&& desiredOutput)
{
    if(linkLLVMModulesForSPIRVCompilation())
    {
        // Use LLVM linker instead of PCH for faster compilation
        auto tmp = compileOpenCLAndLinkModule(std::move(source), userOptions);
        return compileLLVMToSPIRV(LLVMIRSource{std::move(tmp)}, userOptions, std::move(desiredOutput));
    }
    else
    {
        auto tmp = compileOpenCLWithPCH(std::move(source), userOptions);
        return compileLLVMToSPIRV(LLVMIRSource{std::move(tmp)}, userOptions, std::move(desiredOutput));
    }
}

SPIRVTextResult precompilation::compileOpenCLToSPIRVText(
    OpenCLSource&& source, const std::string& userOptions, SPIRVTextResult&& desiredOutput)
{
    if(linkLLVMModulesForSPIRVCompilation())
    {
        // Use LLVM linker instead of PCH for faster compilation
        auto tmp = compileOpenCLAndLinkModule(std::move(source), userOptions);
        return compileLLVMToSPIRVText(LLVMIRSource{std::move(tmp)}, userOptions, std::move(desiredOutput));
    }
    else
    {
        auto tmp = compileOpenCLWithPCH(std::move(source), userOptions);
        return compileLLVMToSPIRVText(LLVMIRSource{std::move(tmp)}, userOptions, std::move(desiredOutput));
    }
}

Optional<std::string> precompilation::findToolLocation(
    const std::string& name, const std::string& preferredPath, bool skipPathLookup)
{
    static std::mutex cacheLock;
    static FastMap<std::string, Optional<std::string>> cachedTools;

    std::lock_guard<std::mutex> guard(cacheLock);
    if(cachedTools.find(name) != cachedTools.end())
        return cachedTools.at(name);

    if(!preferredPath.empty() && access(preferredPath.data(), X_OK) == 0)
    {
        cachedTools.emplace(name, preferredPath);
        return preferredPath;
    }

    if(!preferredPath.empty())
        logging::warn() << "Failed to find executable file for tool '" << name
                        << "' at configured location: " << preferredPath << logging::endl;

    if(skipPathLookup)
        // don't cache here, since we might want to lookup another time, this time with searching in PATH
        return {};

    std::ostringstream outputStream;
    std::ostringstream errorStream;
    int status = runProcess("which " + name, nullptr, &outputStream, &errorStream);
    if(status == 0) // success
    {
        auto result = outputStream.str();
        result = result.substr(0, result.find_first_of('\n'));
        CPPLOG_LAZY(logging::Level::WARNING,
            log << "Using detected executable for tool '" << name << "', might cause version conflicts: " << result
                << logging::endl);
        cachedTools.emplace(name, result);
        return result;
    }
    if(!errorStream.str().empty())
    {
        logging::error() << "Errors trying to find executable path for: " << name << logging::endl;
        logging::error() << errorStream.str() << logging::endl;
    }

    // so we also don't look up again on failure
    cachedTools.emplace(name, Optional<std::string>{});
    return {};
}

static std::string determineFilePath(const std::string& fileName, const std::vector<std::string>& folders)
{
    for(const auto& folder : folders)
    {
        auto fullPath = folder + (!folder.empty() && folder.back() == '/' ? "" : "/") + fileName;
        if(access(fullPath.data(), R_OK) == 0)
        {
            // the file exists (including resolving sym-links, etc.) and can be read
            return fullPath;
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Could not access VC4CL standard-library file: " << fullPath << " (" << strerror(errno) << ')'
                    << logging::endl);
        }
    }
    return "";
}

const StdlibFiles& precompilation::findStandardLibraryFiles(const std::vector<std::string>& additionalFolders)
{
    static const StdlibFiles paths = [&]() {
        std::vector<std::string> allPaths(additionalFolders);
        if(!VC4CL_STDLIB_FOLDER.empty())
            allPaths.emplace_back(VC4CL_STDLIB_FOLDER);
        if(auto homeDir = std::getenv("HOME"))
        {
            allPaths.emplace_back(std::string(homeDir) + "/.cache/vc4c");
        }
        allPaths.emplace_back(VC4CL_STDLIB_CACHE_DIR);
        // for backwards compatibility as well as to find the header
        allPaths.emplace_back("/usr/local/include/vc4cl-stdlib/");
        allPaths.emplace_back("/usr/include/vc4cl-stdlib/");
        StdlibFiles tmp;
        tmp.configurationHeader = determineFilePath("defines.h", allPaths);
        tmp.llvmModule = determineFilePath("VC4CLStdLib.bc", allPaths);
        tmp.precompiledHeader = determineFilePath("VC4CLStdLib.h.pch", allPaths);
        tmp.spirvModule = determineFilePath("VC4CLStdLib.spv", allPaths);
        if(tmp.configurationHeader.empty() || (tmp.llvmModule.empty() && tmp.precompiledHeader.empty()))
        {
            throw CompilationError(CompilationStep::PRECOMPILATION,
                "Required VC4CL standard library file not found in any of the provided paths",
                to_string<std::string>(allPaths));
        }
        return tmp;
    }();
    return paths;
}

void precompilation::precompileStandardLibraryFiles(const std::string& sourceFile, const std::string& destinationFolder)
{
    PROFILE_SCOPE(PrecompileStandardLibraryFiles);
    // TODO merge with creating of parameters in FrontendCompiler#buildClangCommand
    auto pchArgs =
        " -cc1 -triple spir-unknown-unknown -O3 -ffp-contract=off -cl-std=CL1.2 -cl-kernel-arg-info "
        "-cl-single-precision-constant -fgnu89-inline -Wno-all -Wno-gcc-compat -Wdouble-promotion "
        "-Wno-undefined-inline "
        "-Wno-unknown-attributes -x cl "
        "-emit-pch -o ";
    auto moduleArgs =
        " -cc1 -triple spir-unknown-unknown -O3 -ffp-contract=off -cl-std=CL1.2 -cl-kernel-arg-info "
        "-cl-single-precision-constant -fgnu89-inline -Wno-all -Wno-gcc-compat -Wdouble-promotion "
        "-Wno-undefined-inline "
        "-Wno-unknown-attributes -x cl "
        "-emit-llvm-bc -o ";
    auto spirvArgs = " --spirv-lower-const-expr --spirv-mem2reg -o ";

    auto pchCommand = CLANG_PATH + pchArgs + destinationFolder + "/VC4CLStdLib.h.pch " + sourceFile;
    auto moduleCommand = CLANG_PATH + moduleArgs + destinationFolder + "/VC4CLStdLib.bc " + sourceFile;
    auto spirvCommand = CLANG_PATH + moduleArgs + "/dev/stdout " + sourceFile + " | " + SPIRV_LLVM_SPIRV_PATH +
        spirvArgs + destinationFolder + "/VC4CLStdLib.spv -";
    // SPIRV-LLVM-Translator does not support (all) LLVM intrinsic functions, so we need to create the temporary
    // LLVM module without any optimization enabled. Instead we at least run some optimizations in the
    // SPIRV-LLVM-Translator.
    spirvCommand.replace(spirvCommand.find("-O3"), 3, "-O0");

    CPPLOG_LAZY(logging::Level::INFO, log << "Pre-compiling standard library with: " << pchCommand << logging::endl);
    runPrecompiler(pchCommand, nullptr, nullptr);

    CPPLOG_LAZY(logging::Level::INFO, log << "Pre-compiling standard library with: " << moduleCommand << logging::endl);
    runPrecompiler(moduleCommand, nullptr, nullptr);

    if(!SPIRV_LLVM_SPIRV_PATH.empty())
    {
        CPPLOG_LAZY(
            logging::Level::INFO, log << "Pre-compiling standard library with: " << spirvCommand << logging::endl);
        runPrecompiler(spirvCommand, nullptr, nullptr);
    }
}
