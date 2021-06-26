/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Precompiler.h"

#include "../Profiler.h"
#include "../helper.h"
#include "../shared/BinaryHeader.h"
#include "FrontendCompiler.h"
#include "log.h"

#include <algorithm>
#include <array>
#include <cstring>
#include <fstream>
#include <iterator>
#include <libgen.h>
#include <sstream>
#include <unistd.h>

using namespace vc4c;
using namespace vc4c::precompilation;

bool vc4c::isSupportedByFrontend(SourceType inputType, Frontend frontend)
{
    switch(inputType)
    {
    case SourceType::LLVM_IR_BIN:
        FALL_THROUGH
    case SourceType::LLVM_IR_TEXT:
        FALL_THROUGH
    case SourceType::OPENCL_C:
        return true;
    case SourceType::SPIRV_BIN:
        FALL_THROUGH
    case SourceType::SPIRV_TEXT:
        return frontend == Frontend::SPIR_V || frontend == Frontend::DEFAULT;
    case SourceType::QPUASM_BIN:
        FALL_THROUGH
    case SourceType::QPUASM_HEX:
        FALL_THROUGH
    case SourceType::UNKNOWN:
    default:
        return false;
    }
}

void Precompiler::precompile(std::istream& input, std::unique_ptr<std::istream>& output, Configuration config,
    const std::string& options, const Optional<std::string>& inputFile, const Optional<std::string>& outputFile)
{
    PROFILE_SCOPE(Precompile);
    Precompiler precompiler(config, input, Precompiler::getSourceType(input), inputFile);
    if(config.frontend != Frontend::DEFAULT)
        precompiler.run(output, config.frontend == Frontend::LLVM_IR ? SourceType::LLVM_IR_BIN : SourceType::SPIRV_BIN,
            options, outputFile);
    else
    {
#if defined USE_LLVM_LIBRARY
        // we have both front-ends, select the front-end which can handle the input type
        if(isSupportedByFrontend(precompiler.inputType, Frontend::LLVM_IR))
            // prefer LLVM library front-end
            precompiler.run(output, SourceType::LLVM_IR_BIN, options, outputFile);
        else if(findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH))
            precompiler.run(output, SourceType::SPIRV_BIN, options, outputFile);
#else
        if(findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH))
            precompiler.run(output, SourceType::SPIRV_BIN, options, outputFile);
        throw CompilationError(CompilationStep::PRECOMPILATION, "No matching precompiler available!");
#endif
    }
}

SourceType Precompiler::getSourceType(std::istream& stream)
{
    PROFILE_SCOPE(GetSourceType);
    // http://llvm.org/docs/BitCodeFormat.html#magic-numbers
    static constexpr std::array<char, 2> LLVM_BITCODE_MAGIC_NUMBER = {0x42, 0x43};
    static constexpr uint32_t SPIRV_MAGIC_NUMBER = 0x07230203;
    static constexpr std::array<char, 4> SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN = {0x07, 0x23, 0x02, 0x03};
    static constexpr std::array<char, 4> SPIRV_MAGIC_NUMBER_BIG_ENDIAN = {0x03, 0x02, 0x23, 0x07};
    static const std::string QPUASM_MAGIC = []() {
        std::stringstream s;
        s << "0x" << std::hex << ModuleHeader::QPUASM_MAGIC_NUMBER;
        return s.str();
    }();
    std::array<char, 1024> buffer{};
    stream.read(buffer.data(), 1000);
    const std::string s(buffer.data(), static_cast<std::size_t>(stream.gcount()));

    SourceType type = SourceType::UNKNOWN;
    if(s.find("ModuleID") != std::string::npos || s.find("\ntarget triple") != std::string::npos)
        type = SourceType::LLVM_IR_TEXT;
    else if(memcmp(buffer.data(), LLVM_BITCODE_MAGIC_NUMBER.data(), LLVM_BITCODE_MAGIC_NUMBER.size()) == 0)
        type = SourceType::LLVM_IR_BIN;
    else if(memcmp(buffer.data(), SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN.data(), SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN.size()) ==
            0 ||
        memcmp(buffer.data(), SPIRV_MAGIC_NUMBER_BIG_ENDIAN.data(), SPIRV_MAGIC_NUMBER_BIG_ENDIAN.size()) == 0)
        type = SourceType::SPIRV_BIN;
    else if(std::atol(buffer.data()) == SPIRV_MAGIC_NUMBER)
        type = SourceType::SPIRV_TEXT;
    else if(memcmp(buffer.data(), &ModuleHeader::QPUASM_MAGIC_NUMBER, 4) == 0)
        type = SourceType::QPUASM_BIN;
    else if(s.find(QPUASM_MAGIC) != std::string::npos)
        type = SourceType::QPUASM_HEX;
    else if(s.find("kernel") != std::string::npos || s.find("/**") != std::string::npos ||
        s.find("//") != std::string::npos || s.find("#include") != std::string::npos ||
        s.find("#define") != std::string::npos || s.find("typedef") != std::string::npos ||
        s.find("extern") != std::string::npos || s.find("struct") != std::string::npos ||
        s.find("return") != std::string::npos || s.find("static") != std::string::npos ||
        s.find('\n') != std::string::npos)
        // TODO need better check, or simply default to OpenCL C??
        type = SourceType::OPENCL_C;

    // reset flags (e.g. if we were at the end of the file)
    stream.clear();
    // reset stream position
    stream.seekg(0);

    return type;
}

static std::pair<bool, bool> determinePossibleLinkers(
    const std::unordered_map<std::istream*, Optional<std::string>>& inputs)
{
    bool llvmLinkerPossible = findToolLocation("llvm-link", LLVM_LINK_PATH).has_value();
#ifdef SPIRV_TOOLS_FRONTEND
    bool spirvLinkerPossible = true;
#else
    bool spirvLinkerPossible = findToolLocation("spirv-link", SPIRV_LINK_PATH).has_value();
#endif

    for(const auto& input : inputs)
    {
        auto type = Precompiler::getSourceType(*input.first);
        if(type > SourceType::LLVM_IR_BIN)
            llvmLinkerPossible = false;
        if(type > SourceType::SPIRV_TEXT)
            spirvLinkerPossible = false;
    }

    return std::make_pair(llvmLinkerPossible, spirvLinkerPossible);
}

static NODISCARD Optional<TemporaryFile> compileToSPIRV(const std::pair<std::istream*, Optional<std::string>>& source)
{
    auto type = Precompiler::getSourceType(*source.first);
    if(type == SourceType::OPENCL_C)
    {
        TemporaryFile f;
        SPIRVResult res(f.fileName);
        compileOpenCLToSPIRV(
            source.second ? OpenCLSource(source.second.value()) : OpenCLSource(*source.first), "", res);
        return f;
    }
    else if(type == SourceType::LLVM_IR_BIN)
    {
        TemporaryFile f;
        SPIRVResult res(f.fileName);
        compileLLVMToSPIRV(source.second ? LLVMIRSource(source.second.value()) : LLVMIRSource(*source.first), "", res);
        return f;
    }
    else if(type == SourceType::SPIRV_TEXT)
    {
        TemporaryFile f;
        SPIRVResult res(f.fileName);
        assembleSPIRV(source.second ? SPIRVTextSource(source.second.value()) : SPIRVTextSource(*source.first), "", res);
        return f;
    }
    else if(type == SourceType::SPIRV_BIN)
    {
        return {};
    }
    else
        throw CompilationError(CompilationStep::LINKER, "No known conversion from source-code type to SPIR-V binary",
            std::to_string(static_cast<unsigned>(type)));
}

static NODISCARD Optional<TemporaryFile> compileToLLVM(const std::pair<std::istream*, Optional<std::string>>& source)
{
    auto type = Precompiler::getSourceType(*source.first);
    if(type == SourceType::OPENCL_C)
    {
        TemporaryFile f;
        LLVMIRResult res(f.fileName);
        compileOpenCLToLLVMIR(
            source.second ? OpenCLSource(source.second.value()) : OpenCLSource(*source.first), "", res);
        return f;
    }
    else if(type == SourceType::LLVM_IR_TEXT && findToolLocation("llvm-as", LLVM_AS_PATH))
    {
        TemporaryFile f;
        LLVMIRResult res(f.fileName);
        assembleLLVM(
            source.second ? LLVMIRTextSource(source.second.value()) : LLVMIRTextSource(*source.first), "", res);
        return f;
    }
    else if(type == SourceType::LLVM_IR_BIN)
    {
        return {};
    }
    else
        throw CompilationError(CompilationStep::LINKER, "No known conversion from source-code type to LLVM IR binary",
            std::to_string(static_cast<unsigned>(type)));
}

static void copyToOutputStream(std::ostream& output, TemporaryFile& tmpFile)
{
    std::unique_ptr<std::istream> tmpStream;
    tmpFile.openInputStream(tmpStream);
    output << tmpStream->rdbuf();
}

SourceType Precompiler::linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs,
    std::ostream& output, bool includeStandardLibrary)
{
    // XXX the inputs is actually a variant of file and stream
    PROFILE_SCOPE(linkSourceCode);

    bool llvmLinkerPossible = false;
    bool spirvLinkerPossible = false;
    std::vector<std::unique_ptr<TemporaryFile>> tempFiles;
    std::tie(llvmLinkerPossible, spirvLinkerPossible) = determinePossibleLinkers(inputs);

    // prefer LLVM IR linker - although it requires spawning an extra process - since LLVM-SPIRV translation is not
    // complete (e.g. some LLVM intrinsics are not supported)
    if(llvmLinkerPossible)
    {
        std::vector<LLVMIRSource> sources;
        sources.reserve(inputs.size());
        std::transform(
            inputs.begin(), inputs.end(), std::back_inserter(sources), [&](const auto& pair) -> LLVMIRSource {
                if(auto temp = compileToLLVM(pair))
                {
                    tempFiles.emplace_back(std::make_unique<TemporaryFile>(std::move(temp.value())));
                    return LLVMIRSource(tempFiles.back()->fileName);
                }
                if(pair.second)
                    return LLVMIRSource(pair.second.value());
                return LLVMIRSource(*pair.first);
            });
        if(includeStandardLibrary)
        {
            // need to link the std-lib module with the special function to set the correct flags (e.g. to not fail if
            // std-lib module was already linked into one of the sources)
            auto tmpFile = std::make_unique<TemporaryFile>();
            LLVMIRResult tmp(tmpFile->fileName);
            linkLLVMModules(std::move(sources), "", tmp);

            TemporaryPrecompilationResult<SourceType::LLVM_IR_BIN> result{};
            linkInStdlibModule(LLVMIRSource(tmp), "", result);
            copyToOutputStream(output, result.tmpFile);
        }
        else
        {
            TemporaryPrecompilationResult<SourceType::LLVM_IR_BIN> result{};
            linkLLVMModules(std::move(sources), "", result);
            copyToOutputStream(output, result.tmpFile);
        }
        return SourceType::LLVM_IR_BIN;
    }
    else if(spirvLinkerPossible)
    {
        std::vector<SPIRVSource> sources;
        sources.reserve(inputs.size());
        std::transform(inputs.begin(), inputs.end(), std::back_inserter(sources), [&](const auto& pair) -> SPIRVSource {
            if(auto temp = compileToSPIRV(pair))
            {
                tempFiles.emplace_back(std::make_unique<TemporaryFile>(std::move(temp.value())));
                return SPIRVSource(tempFiles.back()->fileName);
            }
            if(pair.second)
                return SPIRVSource(pair.second.value());
            return SPIRVSource(*pair.first);
        });

        if(includeStandardLibrary)
        {
            if(!findStandardLibraryFiles().spirvModule.empty())
                sources.emplace_back(SPIRVSource(findStandardLibraryFiles().spirvModule));
            else if(auto llvm_spirv = findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH))
            {
                tempFiles.emplace_back(std::make_unique<TemporaryFile>());
                SPIRVResult stdLib(tempFiles.back()->fileName);
                compileLLVMToSPIRV(LLVMIRSource(findStandardLibraryFiles().llvmModule), "", stdLib);
                sources.emplace_back(stdLib);
            }
            else
                throw CompilationError(
                    CompilationStep::LINKER, "Neither SPIR-V module nor llvm-spirv executable found!");
        }

        TemporaryPrecompilationResult<SourceType::SPIRV_BIN> result{};
        linkSPIRVModules(std::move(sources), "", result);
        copyToOutputStream(output, result.tmpFile);
        return SourceType::SPIRV_BIN;
    }
    throw CompilationError(CompilationStep::LINKER, "Cannot find a linker which can be used for all inputs!");
}

bool Precompiler::isLinkerAvailable(const std::unordered_map<std::istream*, Optional<std::string>>& inputs)
{
    if(!isLinkerAvailable())
        return false;
    return std::all_of(inputs.begin(), inputs.end(), [](const auto& input) -> bool {
        switch(getSourceType(*input.first))
        {
        case SourceType::OPENCL_C:
        case SourceType::LLVM_IR_BIN:
            return findToolLocation("llvm-link", LLVM_LINK_PATH).has_value();
        case SourceType::LLVM_IR_TEXT:
        case SourceType::SPIRV_BIN:
        case SourceType::SPIRV_TEXT:
#ifdef SPIRV_TOOLS_FRONTEND
            return true;
#else
            return findToolLocation("spirv-link", SPIRV_LINK_PATH).has_value();
#endif
        default:
            return false;
        }
    });
}

bool Precompiler::isLinkerAvailable()
{
#if defined(SPIRV_TOOLS_FRONTEND)
    return true;
#endif
    if(findToolLocation("llvm-link", LLVM_LINK_PATH))
        return true;
    if(findToolLocation("spirv-link", SPIRV_LINK_PATH))
        return true;
    return false;
}

Precompiler::Precompiler(
    Configuration& config, std::istream& input, const SourceType inputType, const Optional<std::string>& inputFile) :
    inputType(inputType),
    inputFile(inputFile), config(config), input(input)
{
    if(inputType == SourceType::QPUASM_BIN || inputType == SourceType::QPUASM_HEX || inputType == SourceType::UNKNOWN)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid input-type for pre-compilation",
            std::to_string(static_cast<unsigned>(inputType)));
}

void Precompiler::run(std::unique_ptr<std::istream>& output, const SourceType outputType, const std::string& options,
    Optional<std::string> outputFile)
{
    if(outputType == SourceType::QPUASM_BIN || outputType == SourceType::QPUASM_HEX ||
        outputType == SourceType::UNKNOWN)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid output-type for pre-compilation",
            std::to_string(static_cast<unsigned>(outputType)));

    std::string extendedOptions = options;
    if(inputFile)
    {
        // for resolving relative includes
        std::array<char, 1024> buffer{};
        buffer.fill(0);
        strncpy(buffer.data(), inputFile->data(), std::min(buffer.size(), inputFile->size()));
        std::string tmp = dirname(buffer.data());
        extendedOptions.append(" -I ").append(tmp);
    }

    if(inputType == outputType)
    {
        const std::string buffer(std::istreambuf_iterator<char>(input), {});
        output = std::make_unique<std::istringstream>(buffer);
        return;
    }

    std::ostringstream tempStream;

    if(inputType == SourceType::OPENCL_C)
    {
        OpenCLSource src = inputFile ? OpenCLSource(inputFile.value()) : OpenCLSource(input);
        if(outputType == SourceType::LLVM_IR_TEXT)
        {
            auto res = createResult<SourceType::LLVM_IR_TEXT>(tempStream, outputFile);
            if(config.useOpt)
            {
                auto steps = chainSteps<SourceType::LLVM_IR_TEXT, SourceType::OPENCL_C, SourceType::LLVM_IR_TEXT>(
                    compileOpenCLToLLVMText, optimizeLLVMText);
                steps(std::move(src), extendedOptions, *res);
            }
            else
            {
                compileOpenCLToLLVMText(std::move(src), extendedOptions, *res);
            }
        }
        else if(outputType == SourceType::LLVM_IR_BIN)
        {
            auto res = createResult<SourceType::LLVM_IR_BIN>(tempStream, outputFile);
            if(config.useOpt)
            {
                auto steps = chainSteps<SourceType::LLVM_IR_BIN, SourceType::OPENCL_C, SourceType::LLVM_IR_BIN>(
                    compileOpenCLToLLVMIR, optimizeLLVMIR);
                steps(std::move(src), extendedOptions, *res);
            }
            else
            {
                compileOpenCLToLLVMIR(std::move(src), extendedOptions, *res);
            }
        }
        else if(outputType == SourceType::SPIRV_BIN)
        {
            auto res = createResult<SourceType::SPIRV_BIN>(tempStream, outputFile);
            compileOpenCLToSPIRV(std::move(src), extendedOptions, *res);
        }
        else if(outputType == SourceType::SPIRV_TEXT)
        {
            auto res = createResult<SourceType::SPIRV_TEXT>(tempStream, outputFile);
            compileOpenCLToSPIRVText(std::move(src), extendedOptions, *res);
        }
    }
    else if(inputType == SourceType::LLVM_IR_TEXT)
    {
        LLVMIRTextSource src = inputFile ? LLVMIRTextSource(inputFile.value()) : LLVMIRTextSource(input);
        if(outputType == SourceType::SPIRV_BIN && findToolLocation("llvm-as", LLVM_AS_PATH))
        {
            TemporaryFile tmpFile;
            LLVMIRResult tmpResult(tmpFile.fileName);
            assembleLLVM(std::move(src), extendedOptions, tmpResult);
            auto res = createResult<SourceType::SPIRV_BIN>(tempStream, outputFile);
            LLVMIRSource tmpSource(tmpResult);
            compileLLVMToSPIRV(std::move(tmpSource), extendedOptions, *res);
        }
        else if(outputType == SourceType::LLVM_IR_BIN && findToolLocation("llvm-as", LLVM_AS_PATH))
        {
            auto res = createResult<SourceType::LLVM_IR_BIN>(tempStream, outputFile);
            assembleLLVM(std::move(src), extendedOptions, *res);
        }
    }
    else if(inputType == SourceType::LLVM_IR_BIN)
    {
        LLVMIRSource src = inputFile ? LLVMIRSource(inputFile.value()) : LLVMIRSource(input);
        if(outputType == SourceType::SPIRV_BIN)
        {
            auto res = createResult<SourceType::SPIRV_BIN>(tempStream, outputFile);
            compileLLVMToSPIRV(std::move(src), extendedOptions, *res);
        }
        else if(outputType == SourceType::SPIRV_TEXT)
        {
            auto res = createResult<SourceType::SPIRV_TEXT>(tempStream, outputFile);
            compileLLVMToSPIRVText(std::move(src), extendedOptions, *res);
        }
        else if(outputType == SourceType::LLVM_IR_TEXT && findToolLocation("llvm-dis", LLVM_DIS_PATH))
        {
            auto res = createResult<SourceType::LLVM_IR_TEXT>(tempStream, outputFile);
            disassembleLLVM(std::move(src), extendedOptions, *res);
        }
    }
    else if(inputType == SourceType::SPIRV_BIN && outputType == SourceType::SPIRV_TEXT)
    {
        SPIRVSource src = inputFile ? SPIRVSource(inputFile.value()) : SPIRVSource(input);
        auto res = createResult<SourceType::SPIRV_TEXT>(tempStream, outputFile);
        disassembleSPIRV(std::move(src), extendedOptions, *res);
    }
    else if(inputType == SourceType::SPIRV_TEXT && outputType == SourceType::SPIRV_BIN)
    {
        SPIRVTextSource src = inputFile ? SPIRVTextSource(inputFile.value()) : SPIRVTextSource(input);
        auto res = createResult<SourceType::SPIRV_BIN>(tempStream, outputFile);
        assembleSPIRV(std::move(src), extendedOptions, *res);
    }
    else
        throw CompilationError(CompilationStep::PRECOMPILATION, "Unhandled pre-compilation");

    CPPLOG_LAZY(logging::Level::INFO, log << "Compilation complete!" << logging::endl);

    output = std::make_unique<std::istringstream>(tempStream.str());
}
