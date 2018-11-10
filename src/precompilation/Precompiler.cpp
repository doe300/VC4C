/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Precompiler.h"

#include "../Profiler.h"
#include "../helper.h"
#include "FrontendCompiler.h"
#include "log.h"

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iterator>
#include <libgen.h>
#include <sstream>

using namespace vc4c;
using namespace vc4c::precompilation;

bool vc4c::isSupportedByFrontend(SourceType inputType, Frontend frontend)
{
    switch(inputType)
    {
    case SourceType::LLVM_IR_BIN:  // fall-through
    case SourceType::LLVM_IR_TEXT: // fall-through
    case SourceType::OPENCL_C:
        return true;
    case SourceType::SPIRV_BIN: // fall-through
    case SourceType::SPIRV_TEXT:
        return frontend == Frontend::SPIR_V || frontend == Frontend::DEFAULT;
    case SourceType::QPUASM_BIN: // fall-through
    case SourceType::QPUASM_HEX: // fall-through
    case SourceType::UNKNOWN:
    default:
        return false;
    }
}

void Precompiler::precompile(std::istream& input, std::unique_ptr<std::istream>& output, Configuration config,
    const std::string& options, const Optional<std::string>& inputFile, Optional<std::string> outputFile)
{
    PROFILE_START(Precompile);
    Precompiler precompiler(config, input, Precompiler::getSourceType(input), inputFile);
    if(config.frontend != Frontend::DEFAULT)
        precompiler.run(output, config.frontend == Frontend::LLVM_IR ? SourceType::LLVM_IR_TEXT : SourceType::SPIRV_BIN,
            options, outputFile);
    else
    {
#if defined USE_LLVM_LIBRARY and defined SPIRV_CLANG_PATH and defined SPIRV_LLVM_SPIRV_PATH and defined SPIRV_FRONTEND
        // we have both front-ends, select the front-end which can handle the input type
        if(isSupportedByFrontend(precompiler.inputType, Frontend::LLVM_IR))
            // prefer LLVM library front-end
            precompiler.run(output, SourceType::LLVM_IR_BIN, options, outputFile);
        else
            precompiler.run(output, SourceType::SPIRV_BIN, options, outputFile);
#elif defined USE_LLVM_LIBRARY
        precompiler.run(output, SourceType::LLVM_IR_BIN, options, outputFile);
#elif defined SPIRV_CLANG_PATH and defined SPIRV_LLVM_SPIRV_PATH and defined SPIRV_FRONTEND
        precompiler.run(output, SourceType::SPIRV_BIN, options, outputFile);
#elif defined CLANG_PATH
        precompiler.run(output, SourceType::LLVM_IR_TEXT, options, outputFile);
#else
        throw CompilationError(CompilationStep::PRECOMPILATION, "No matching precompiler available!");
#endif
    }
    PROFILE_END(Precompile);
}

SourceType Precompiler::getSourceType(std::istream& stream)
{
    // http://llvm.org/docs/BitCodeFormat.html#magic-numbers
    static const char LLVM_BITCODE_MAGIC_NUMBER[2] = {0x42, 0x43};
    static const uint32_t SPIRV_MAGIC_NUMBER = 0x07230203;
    static const char SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN[4] = {0x07, 0x23, 0x02, 0x03};
    static const char SPIRV_MAGIC_NUMBER_BIG_ENDIAN[4] = {0x03, 0x02, 0x23, 0x07};
    std::array<char, 1024> buffer;
    stream.read(buffer.data(), 1000);
    const std::string s(buffer.data(), stream.gcount());

    SourceType type = SourceType::UNKNOWN;
    if(s.find("ModuleID") != std::string::npos || s.find("\ntarget triple") != std::string::npos)
        type = SourceType::LLVM_IR_TEXT;
    else if(memcmp(buffer.data(), LLVM_BITCODE_MAGIC_NUMBER, 2) == 0)
        type = SourceType::LLVM_IR_BIN;
    else if(memcmp(buffer.data(), SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN, 4) == 0 ||
        memcmp(buffer.data(), SPIRV_MAGIC_NUMBER_BIG_ENDIAN, 4) == 0)
        type = SourceType::SPIRV_BIN;
    else if(std::atol(buffer.data()) == SPIRV_MAGIC_NUMBER)
        type = SourceType::SPIRV_TEXT;
    else if(memcmp(buffer.data(), &QPUASM_MAGIC_NUMBER, 4) == 0 || memcmp(buffer.data(), &QPUASM_NUMBER_MAGIC, 4) == 0)
        type = SourceType::QPUASM_BIN;
    else if(std::atol(buffer.data()) == QPUASM_MAGIC_NUMBER || std::atol(buffer.data()) == QPUASM_NUMBER_MAGIC)
        type = SourceType::QPUASM_HEX;
    else if(s.find_first_of(" \n\t") != std::string::npos || s.find("kernel") != std::string::npos)
        // XXX better check
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
#ifdef LLVM_LINK_PATH
    bool llvmLinkerPossible = true;
#else
    bool llvmLinkerPossible = false;
#endif
#ifdef SPIRV_FRONTEND
    bool spirvLinkerPossible = true;
#else
    bool spirvLinkerPossible = false;
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
    else if(type == SourceType::LLVM_IR_BIN)
    {
        return {};
    }
    else
        throw CompilationError(CompilationStep::LINKER, "No known conversion from source-code type to LLVM IR binary",
            std::to_string(static_cast<unsigned>(type)));
}

SourceType Precompiler::linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs,
    std::ostream& output, bool includeStandardLibrary)
{
    // XXX the inputs is actually a variant of file and stream
    PROFILE_START(linkSourceCode);

    bool llvmLinkerPossible;
    bool spirvLinkerPossible;
    std::vector<std::unique_ptr<TemporaryFile>> tempFiles;
    std::tie(llvmLinkerPossible, spirvLinkerPossible) = determinePossibleLinkers(inputs);

    // prefer SPIR-V linker, since it a) does not require an extra process and b) supports more source code types
    if(spirvLinkerPossible)
    {
        std::vector<SPIRVSource> sources;
        sources.reserve(inputs.size());
        std::transform(inputs.begin(), inputs.end(), std::back_inserter(sources),
            [&](const std::pair<std::istream*, Optional<std::string>>& pair) -> SPIRVSource {
                auto temp = compileToSPIRV(pair);

                if(temp)
                {
                    tempFiles.emplace_back(new TemporaryFile(std::move(temp.value())));
                    return SPIRVSource(tempFiles.back()->fileName);
                }
                if(pair.second)
                    return SPIRVSource(pair.second.value());
                return SPIRVSource(*pair.first);
            });

        if(includeStandardLibrary)
        {
            // FIXME this does not work, since the SPIRV-LLVM does not generate a correct VC4CL standard-library module
            tempFiles.emplace_back(new TemporaryFile());
            SPIRVResult stdLib(tempFiles.back()->fileName);
            compileLLVMToSPIRV(LLVMIRSource(VC4CL_STDLIB_MODULE ""), "", stdLib);
            sources.emplace_back(stdLib);
        }

        SPIRVResult result(&output);
        linkSPIRVModules(std::move(sources), "", result);
        PROFILE_END(linkSourceCode);
        return SourceType::SPIRV_BIN;
    }
    else if(llvmLinkerPossible)
    {
        std::vector<LLVMIRSource> sources;
        sources.reserve(inputs.size());
        std::transform(inputs.begin(), inputs.end(), std::back_inserter(sources),
            [&](const std::pair<std::istream*, Optional<std::string>>& pair) -> LLVMIRSource {
                auto temp = compileToLLVM(pair);

                if(temp)
                {
                    tempFiles.emplace_back(new TemporaryFile(std::move(temp.value())));
                    return LLVMIRSource(tempFiles.back()->fileName);
                }
                if(pair.second)
                    return LLVMIRSource(pair.second.value());
                return LLVMIRSource(*pair.first);
            });
        if(includeStandardLibrary)
        {
            sources.emplace_back(VC4CL_STDLIB_MODULE "");
        }

        LLVMIRResult result(&output);
        linkLLVMModules(std::move(sources), "", result);
        PROFILE_END(linkSourceCode);
        return SourceType::LLVM_IR_BIN;
    }
    throw CompilationError(CompilationStep::LINKER, "Cannot find a linker which can be used for all inputs!");
}

bool Precompiler::isLinkerAvailable(const std::unordered_map<std::istream*, Optional<std::string>>& inputs)
{
    if(!isLinkerAvailable())
        return false;
    return std::all_of(
        inputs.begin(), inputs.end(), [](const std::pair<std::istream*, Optional<std::string>>& input) -> bool {
            switch(getSourceType(*input.first))
            {
            case SourceType::OPENCL_C:
            case SourceType::LLVM_IR_BIN:
#ifdef LLVM_LINK_PATH
                return true;
#endif
            case SourceType::LLVM_IR_TEXT:
            case SourceType::SPIRV_BIN:
            case SourceType::SPIRV_TEXT:
#ifdef SPIRV_FRONTEND
                return true;
#endif
            default:
                return false;
            }
        });
}

bool Precompiler::isLinkerAvailable()
{
#if defined(SPIRV_FRONTEND) || defined(LLVM_LINK_PATH)
    return true;
#else
    return false;
#endif
}

Precompiler::Precompiler(
    Configuration& config, std::istream& input, const SourceType inputType, const Optional<std::string>& inputFile) :
    inputType(inputType),
    inputFile(inputFile), input(input), config(config)
{
    if(inputType == SourceType::QPUASM_BIN || inputType == SourceType::QPUASM_HEX || inputType == SourceType::UNKNOWN)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid input-type for pre-compilation!");
}

void Precompiler::run(std::unique_ptr<std::istream>& output, const SourceType outputType, const std::string& options,
    Optional<std::string> outputFile)
{
    if(outputType == SourceType::QPUASM_BIN || outputType == SourceType::QPUASM_HEX ||
        outputType == SourceType::UNKNOWN)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid output-type for pre-compilation!");

    if(!outputFile)
        logging::warn() << "When running the pre-compiler with root rights and writing to /dev/stdout, the compiler "
                           "might delete the /dev/stdout symlink!"
                        << logging::endl;

    std::string extendedOptions = options;
    if(inputFile)
    {
        // for resolving relative includes
        std::array<char, 1024> buffer;
        strcpy(buffer.data(), inputFile->data());
        std::string tmp = dirname(buffer.data());
        extendedOptions.append(" -I ").append(tmp);
    }

    if(inputType == outputType)
    {
        const std::string buffer(std::istreambuf_iterator<char>(input), {});
        output.reset(new std::istringstream(buffer));
        return;
    }

    std::ostringstream tempStream;

    if(inputType == SourceType::OPENCL_C)
    {
        OpenCLSource src = inputFile ? OpenCLSource(inputFile.value()) : OpenCLSource(input);
        if(outputType == SourceType::LLVM_IR_TEXT)
        {
            LLVMIRTextResult res = outputFile ? LLVMIRTextResult(outputFile.value()) : LLVMIRTextResult(&tempStream);
            if(config.useOpt)
            {
                auto steps = chainSteps<SourceType::LLVM_IR_TEXT, SourceType::OPENCL_C, SourceType::LLVM_IR_TEXT>(
                    compileOpenCLToLLVMText, optimizeLLVMText);
                steps(std::move(src), extendedOptions, res);
            }
            else
            {
                compileOpenCLToLLVMText(std::move(src), extendedOptions, res);
            }
        }
        else if(outputType == SourceType::LLVM_IR_BIN)
        {
            LLVMIRResult res = outputFile ? LLVMIRResult(outputFile.value()) : LLVMIRResult(&tempStream);
            if(config.useOpt)
            {
                auto steps = chainSteps<SourceType::LLVM_IR_BIN, SourceType::OPENCL_C, SourceType::LLVM_IR_BIN>(
                    compileOpenCLToLLVMIR, optimizeLLVMIR);
                steps(std::move(src), extendedOptions, res);
            }
            else
            {
                compileOpenCLToLLVMIR(std::move(src), extendedOptions, res);
            }
        }
        else if(outputType == SourceType::SPIRV_BIN)
        {
            SPIRVResult res = outputFile ? SPIRVResult(outputFile.value()) : SPIRVResult(&tempStream);
            compileOpenCLToSPIRV(std::move(src), extendedOptions, res);
        }
        else if(outputType == SourceType::SPIRV_TEXT)
        {
            SPIRVTextResult res = outputFile ? SPIRVTextResult(outputFile.value()) : SPIRVTextResult(&tempStream);
            compileOpenCLToSPIRVText(std::move(src), extendedOptions, res);
        }
    }
    else if(inputType == SourceType::LLVM_IR_TEXT)
    {
        // the result of this does not have the correct output-format (but can be handled by the LLVM front-end)
        tempStream << input.rdbuf();
    }
    else if(inputType == SourceType::LLVM_IR_BIN)
    {
        LLVMIRSource src = inputFile ? LLVMIRSource(inputFile.value()) : LLVMIRSource(input);
        if(outputType == SourceType::SPIRV_BIN)
        {
            SPIRVResult res = outputFile ? SPIRVResult(outputFile.value()) : SPIRVResult(&tempStream);
            compileLLVMToSPIRV(std::move(src), extendedOptions, res);
        }
        else if(outputType == SourceType::SPIRV_TEXT)
        {
            SPIRVTextResult res = outputFile ? SPIRVTextResult(outputFile.value()) : SPIRVTextResult(&tempStream);
            compileLLVMToSPIRVText(std::move(src), extendedOptions, res);
        }
    }
    else if(inputType == SourceType::SPIRV_BIN && outputType == SourceType::SPIRV_TEXT)
    {
        SPIRVSource src = inputFile ? SPIRVSource(inputFile.value()) : SPIRVSource(input);
        SPIRVTextResult res = outputFile ? SPIRVTextResult(outputFile.value()) : SPIRVTextResult(&tempStream);
        disassembleSPIRV(std::move(src), extendedOptions, res);
    }
    else if(inputType == SourceType::SPIRV_TEXT && outputType == SourceType::SPIRV_BIN)
    {
        SPIRVTextSource src = inputFile ? SPIRVTextSource(inputFile.value()) : SPIRVTextSource(input);
        SPIRVResult res = outputFile ? SPIRVResult(outputFile.value()) : SPIRVResult(&tempStream);
        assembleSPIRV(std::move(src), extendedOptions, res);
    }
    else
        throw CompilationError(CompilationStep::PRECOMPILATION, "Unhandled pre-compilation");

    logging::info() << "Compilation complete!" << logging::endl;

    output.reset(new std::istringstream(tempStream.str()));
}
