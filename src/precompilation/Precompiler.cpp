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

CompilationData::CompilationData() = default;

CompilationData::CompilationData(const std::string& fileName, SourceType type)
{
    if(type == SourceType::UNKNOWN)
    {
        std::ifstream fis{fileName};
        type = Precompiler::getSourceType(fis);
    }

    switch(type)
    {
    case SourceType::OPENCL_C:
        data = std::make_unique<FileCompilationData<SourceType::OPENCL_C>>(fileName);
        break;
    case SourceType::LLVM_IR_BIN:
        data = std::make_unique<FileCompilationData<SourceType::LLVM_IR_BIN>>(fileName);
        break;
    case SourceType::LLVM_IR_TEXT:
        data = std::make_unique<FileCompilationData<SourceType::LLVM_IR_TEXT>>(fileName);
        break;
    case SourceType::SPIRV_BIN:
        data = std::make_unique<FileCompilationData<SourceType::SPIRV_BIN>>(fileName);
        break;
    case SourceType::SPIRV_TEXT:
        data = std::make_unique<FileCompilationData<SourceType::SPIRV_TEXT>>(fileName);
        break;
    case SourceType::QPUASM_BIN:
        data = std::make_unique<FileCompilationData<SourceType::QPUASM_BIN>>(fileName);
        break;
    case SourceType::QPUASM_HEX:
        data = std::make_unique<FileCompilationData<SourceType::QPUASM_HEX>>(fileName);
        break;
    case SourceType::UNKNOWN:
        data = std::make_unique<FileCompilationData<SourceType::UNKNOWN>>(fileName);
        break;
    }
}

CompilationData::CompilationData(std::istream& rawData, SourceType type, const std::string& name)
{
    if(type == SourceType::UNKNOWN)
        type = Precompiler::getSourceType(rawData);

    switch(type)
    {
    case SourceType::OPENCL_C:
        data = std::make_unique<RawCompilationData<SourceType::OPENCL_C>>(rawData, name);
        break;
    case SourceType::LLVM_IR_BIN:
        data = std::make_unique<RawCompilationData<SourceType::LLVM_IR_BIN>>(rawData, name);
        break;
    case SourceType::LLVM_IR_TEXT:
        data = std::make_unique<RawCompilationData<SourceType::LLVM_IR_TEXT>>(rawData, name);
        break;
    case SourceType::SPIRV_BIN:
        data = std::make_unique<RawCompilationData<SourceType::SPIRV_BIN>>(rawData, name);
        break;
    case SourceType::SPIRV_TEXT:
        data = std::make_unique<RawCompilationData<SourceType::SPIRV_TEXT>>(rawData, name);
        break;
    case SourceType::QPUASM_BIN:
        data = std::make_unique<RawCompilationData<SourceType::QPUASM_BIN>>(rawData, name);
        break;
    case SourceType::QPUASM_HEX:
        data = std::make_unique<RawCompilationData<SourceType::QPUASM_HEX>>(rawData, name);
        break;
    case SourceType::UNKNOWN:
        data = std::make_unique<RawCompilationData<SourceType::UNKNOWN>>(rawData, name);
        break;
    }
}

CompilationData::CompilationData(std::vector<uint8_t>&& rawData, SourceType type, const std::string& name)
{
    if(type == SourceType::UNKNOWN)
    {
        std::stringstream ss{std::string{reinterpret_cast<const char*>(rawData.data()),
            reinterpret_cast<const char*>(rawData.data()) + rawData.size()}};
        type = Precompiler::getSourceType(ss);
    }

    switch(type)
    {
    case SourceType::OPENCL_C:
        data = std::make_unique<RawCompilationData<SourceType::OPENCL_C>>(std::move(rawData), name);
        break;
    case SourceType::LLVM_IR_BIN:
        data = std::make_unique<RawCompilationData<SourceType::LLVM_IR_BIN>>(std::move(rawData), name);
        break;
    case SourceType::LLVM_IR_TEXT:
        data = std::make_unique<RawCompilationData<SourceType::LLVM_IR_TEXT>>(std::move(rawData), name);
        break;
    case SourceType::SPIRV_BIN:
        data = std::make_unique<RawCompilationData<SourceType::SPIRV_BIN>>(std::move(rawData), name);
        break;
    case SourceType::SPIRV_TEXT:
        data = std::make_unique<RawCompilationData<SourceType::SPIRV_TEXT>>(std::move(rawData), name);
        break;
    case SourceType::QPUASM_BIN:
        data = std::make_unique<RawCompilationData<SourceType::QPUASM_BIN>>(std::move(rawData), name);
        break;
    case SourceType::QPUASM_HEX:
        data = std::make_unique<RawCompilationData<SourceType::QPUASM_HEX>>(std::move(rawData), name);
        break;
    case SourceType::UNKNOWN:
        data = std::make_unique<RawCompilationData<SourceType::UNKNOWN>>(std::move(rawData), name);
        break;
    }
}

CompilationData::CompilationData(std::shared_ptr<CompilationDataPrivate>&& data) : data(std::move(data)) {}

SourceType CompilationData::getType() const noexcept
{
    return data ? data->getType() : SourceType::UNKNOWN;
}

bool CompilationData::getFilePath(std::string& outPath) const
{
    if(auto path = (data ? data->getFilePath() : Optional<std::string>{}))
    {
        outPath = *std::move(path);
        return true;
    }
    return false;
}

bool CompilationData::getRawData(std::vector<uint8_t>& outData) const
{
    if(auto rawData = data ? data->getRawData() : Optional<std::vector<uint8_t>>{})
    {
        outData = *std::move(rawData);
        return true;
    }
    return false;
}

void CompilationData::readInto(std::ostream& out) const
{
    if(data)
        data->readInto(out);
}

CompilationData::operator bool() const noexcept
{
    return data != nullptr;
}

const std::shared_ptr<CompilationDataPrivate>& CompilationData::inner() const noexcept
{
    return data;
}

static NODISCARD CompilationData runPrecompiler(const CompilationData& input, const Configuration& config,
    SourceType outputType, const std::string& options, std::unique_ptr<CompilationDataPrivate>&& desiredOutput);

CompilationData Precompiler::precompile(const CompilationData& input, Configuration config, const std::string& options)
{
    if(config.frontend != Frontend::DEFAULT)
    {
        auto outputType = config.frontend == Frontend::LLVM_IR ? SourceType::LLVM_IR_BIN : SourceType::SPIRV_BIN;
        return runPrecompiler(input, config, outputType, options, nullptr);
    }
    // we have both front-ends, select the front-end which can handle the input type
    else if(hasLLVMFrontend() && isSupportedByFrontend(input.getType(), Frontend::LLVM_IR))
        // prefer LLVM library front-end
        return runPrecompiler(input, config, SourceType::LLVM_IR_BIN, options, nullptr);
    else if(findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH))
        return runPrecompiler(input, config, SourceType::SPIRV_BIN, options, nullptr);
    else
        throw CompilationError(CompilationStep::PRECOMPILATION, "No matching precompiler available!");
}

CompilationData Precompiler::precompile(
    const CompilationData& input, SourceType outputType, Configuration config, const std::string& options)
{
    return runPrecompiler(input, config, outputType, options, nullptr);
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

static std::pair<bool, bool> determinePossibleLinkers(const std::vector<CompilationData>& inputs)
{
    bool llvmLinkerPossible = findToolLocation("llvm-link", LLVM_LINK_PATH).has_value();
    bool spirvLinkerPossible = hasSPIRVToolsFrontend() || findToolLocation("spirv-link", SPIRV_LINK_PATH);

    for(const auto& input : inputs)
    {
        if(input.getType() > SourceType::LLVM_IR_BIN)
            llvmLinkerPossible = false;
        if(input.getType() > SourceType::SPIRV_TEXT)
            spirvLinkerPossible = false;
    }

    return std::make_pair(llvmLinkerPossible, spirvLinkerPossible);
}

template <SourceType Type>
static PrecompilationSource<Type> assertSource(const CompilationData& source)
{
    if(auto ptr = std::dynamic_pointer_cast<TypedCompilationData<Type>>(source.inner()))
        return PrecompilationSource<Type>{std::move(ptr)};
    throw CompilationError(CompilationStep::PRECOMPILATION, "Compilation data is of wrong type");
}

static NODISCARD SPIRVSource compileToSPIRV(const CompilationData& source)
{
    if(source.getType() == SourceType::OPENCL_C)
    {
        return SPIRVSource(compileOpenCLToSPIRV(assertSource<SourceType::OPENCL_C>(source), ""));
    }
    else if(source.getType() == SourceType::LLVM_IR_BIN)
    {
        return SPIRVSource(compileLLVMToSPIRV(assertSource<SourceType::LLVM_IR_BIN>(source), ""));
    }
    else if(source.getType() == SourceType::SPIRV_TEXT)
    {
        return SPIRVSource(assembleSPIRV(assertSource<SourceType::SPIRV_TEXT>(source), ""));
    }
    else if(source.getType() == SourceType::SPIRV_BIN)
    {
        return assertSource<SourceType::SPIRV_BIN>(source);
    }
    else
        throw CompilationError(CompilationStep::LINKER, "No known conversion from source-code type to SPIR-V binary",
            std::to_string(static_cast<unsigned>(source.getType())));
}

static NODISCARD LLVMIRSource compileToLLVM(const CompilationData& source)
{
    if(source.getType() == SourceType::OPENCL_C)
    {
        return LLVMIRSource(compileOpenCLToLLVMIR(assertSource<SourceType::OPENCL_C>(source), ""));
    }
    else if(source.getType() == SourceType::LLVM_IR_TEXT && findToolLocation("llvm-as", LLVM_AS_PATH))
    {
        return LLVMIRSource(assembleLLVM(assertSource<SourceType::LLVM_IR_TEXT>(source), ""));
    }
    else if(source.getType() == SourceType::LLVM_IR_BIN)
    {
        return assertSource<SourceType::LLVM_IR_BIN>(source);
    }
    else
        throw CompilationError(CompilationStep::LINKER, "No known conversion from source-code type to LLVM IR binary",
            std::to_string(static_cast<unsigned>(source.getType())));
}

CompilationData Precompiler::linkSourceCode(const std::vector<CompilationData>& inputs, bool includeStandardLibrary)
{
    PROFILE_SCOPE(linkSourceCode);

    bool llvmLinkerPossible = false;
    bool spirvLinkerPossible = false;
    std::tie(llvmLinkerPossible, spirvLinkerPossible) = determinePossibleLinkers(inputs);

    // prefer LLVM IR linker - although it requires spawning an extra process - since LLVM-SPIRV translation is not
    // complete (e.g. some LLVM intrinsics are not supported)
    if(llvmLinkerPossible)
    {
        std::vector<LLVMIRSource> sources;
        sources.reserve(inputs.size());
        std::transform(inputs.begin(), inputs.end(), std::back_inserter(sources), compileToLLVM);
        if(includeStandardLibrary)
        {
            // need to link the std-lib module with the special function to set the correct flags (e.g. to not fail if
            // std-lib module was already linked into one of the sources)
            auto tmp = linkLLVMModules(sources, "");
            return linkInStdlibModule(LLVMIRSource(std::move(tmp)), "").publish();
        }
        return linkLLVMModules(sources, "").publish();
    }
    else if(spirvLinkerPossible)
    {
        std::vector<SPIRVSource> sources;
        sources.reserve(inputs.size());
        std::transform(inputs.begin(), inputs.end(), std::back_inserter(sources), compileToSPIRV);

        if(includeStandardLibrary)
        {
            if(!findStandardLibraryFiles().spirvModule.empty())
                sources.emplace_back(SPIRVSource(findStandardLibraryFiles().spirvModule));
            else if(auto llvm_spirv = findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH))
                sources.emplace_back(compileLLVMToSPIRV(LLVMIRSource(findStandardLibraryFiles().llvmModule), ""));
            else
                throw CompilationError(
                    CompilationStep::LINKER, "Neither SPIR-V module nor llvm-spirv executable found!");
        }

        return linkSPIRVModules(sources, "").publish();
    }
    throw CompilationError(CompilationStep::LINKER, "Cannot find a linker which can be used for all inputs!");
}

bool Precompiler::isLinkerAvailable(const std::vector<CompilationData>& inputs)
{
    if(!isLinkerAvailable())
        return false;
    return std::all_of(inputs.begin(), inputs.end(), [](const CompilationData& input) -> bool {
        switch(input.getType())
        {
        case SourceType::OPENCL_C:
        case SourceType::LLVM_IR_BIN:
            return findToolLocation("llvm-link", LLVM_LINK_PATH).has_value();
        case SourceType::LLVM_IR_TEXT:
        case SourceType::SPIRV_BIN:
        case SourceType::SPIRV_TEXT:
            return hasSPIRVToolsFrontend() || findToolLocation("spirv-link", SPIRV_LINK_PATH);
        default:
            return false;
        }
    });
}

bool Precompiler::isLinkerAvailable()
{
    if(hasSPIRVToolsFrontend())
        return true;
    if(findToolLocation("llvm-link", LLVM_LINK_PATH))
        return true;
    if(findToolLocation("spirv-link", SPIRV_LINK_PATH))
        return true;
    return false;
}

template <SourceType Type>
static PrecompilationResult<Type> getResult(std::unique_ptr<CompilationDataPrivate>&& result)
{
    if(auto ptr = dynamic_cast<TypedCompilationData<Type>*>(result.get()))
    {
        result.release();
        return PrecompilationResult<Type>{std::unique_ptr<TypedCompilationData<Type>>{ptr}};
    }
    return PrecompilationResult<Type>{};
}

static CompilationData runPrecompiler(const CompilationData& input, const Configuration& config, SourceType outputType,
    const std::string& options, std::unique_ptr<CompilationDataPrivate>&& desiredOutput)
{
    if(outputType == SourceType::QPUASM_BIN || outputType == SourceType::QPUASM_HEX ||
        outputType == SourceType::UNKNOWN)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid output-type for pre-compilation",
            std::to_string(static_cast<unsigned>(outputType)));

    std::string extendedOptions = options;
    std::string inputFile;
    if(input.getFilePath(inputFile))
    {
        // for resolving relative includes
        std::array<char, 1024> buffer{};
        buffer.fill(0);
        strncpy(buffer.data(), inputFile.data(), std::min(buffer.size(), inputFile.size()));
        std::string tmp = dirname(buffer.data());
        extendedOptions.append(" -I ").append(tmp);
    }

    if(input.getType() == outputType)
    {
        if(desiredOutput)
        {
            std::stringstream ss;
            input.readInto(ss);
            desiredOutput->writeFrom(ss);
            return CompilationData{std::move(desiredOutput)};
        }
        else
            return input;
    }

    if(input.getType() == SourceType::OPENCL_C)
    {
        if(outputType == SourceType::LLVM_IR_TEXT)
        {
            return compileOpenCLToLLVMText(assertSource<SourceType::OPENCL_C>(input), extendedOptions,
                getResult<SourceType::LLVM_IR_TEXT>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::LLVM_IR_BIN)
        {
            return compileOpenCLToLLVMIR(assertSource<SourceType::OPENCL_C>(input), extendedOptions,
                getResult<SourceType::LLVM_IR_BIN>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::SPIRV_BIN)
        {
            return compileOpenCLToSPIRV(assertSource<SourceType::OPENCL_C>(input), extendedOptions,
                getResult<SourceType::SPIRV_BIN>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::SPIRV_TEXT)
        {
            return compileOpenCLToSPIRVText(assertSource<SourceType::OPENCL_C>(input), extendedOptions,
                getResult<SourceType::SPIRV_TEXT>(std::move(desiredOutput)))
                .publish();
        }
    }
    else if(input.getType() == SourceType::LLVM_IR_TEXT)
    {
        if(outputType == SourceType::SPIRV_BIN && findToolLocation("llvm-as", LLVM_AS_PATH))
        {
            auto tmp = assembleLLVM(assertSource<SourceType::LLVM_IR_TEXT>(input), extendedOptions);
            return compileLLVMToSPIRV(LLVMIRSource(std::move(tmp)), extendedOptions,
                getResult<SourceType::SPIRV_BIN>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::SPIRV_TEXT && findToolLocation("llvm-as", LLVM_AS_PATH))
        {
            auto tmp = assembleLLVM(assertSource<SourceType::LLVM_IR_TEXT>(input), extendedOptions);
            return compileLLVMToSPIRVText(LLVMIRSource(std::move(tmp)), extendedOptions,
                getResult<SourceType::SPIRV_TEXT>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::LLVM_IR_BIN && findToolLocation("llvm-as", LLVM_AS_PATH))
        {
            return assembleLLVM(assertSource<SourceType::LLVM_IR_TEXT>(input), extendedOptions,
                getResult<SourceType::LLVM_IR_BIN>(std::move(desiredOutput)))
                .publish();
        }
    }
    else if(input.getType() == SourceType::LLVM_IR_BIN)
    {
        if(outputType == SourceType::SPIRV_BIN)
        {
            return compileLLVMToSPIRV(assertSource<SourceType::LLVM_IR_BIN>(input), extendedOptions,
                getResult<SourceType::SPIRV_BIN>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::SPIRV_TEXT)
        {
            return compileLLVMToSPIRVText(assertSource<SourceType::LLVM_IR_BIN>(input), extendedOptions,
                getResult<SourceType::SPIRV_TEXT>(std::move(desiredOutput)))
                .publish();
        }
        else if(outputType == SourceType::LLVM_IR_TEXT && findToolLocation("llvm-dis", LLVM_DIS_PATH))
        {
            return disassembleLLVM(assertSource<SourceType::LLVM_IR_BIN>(input), extendedOptions,
                getResult<SourceType::LLVM_IR_TEXT>(std::move(desiredOutput)))
                .publish();
        }
    }
    else if(input.getType() == SourceType::SPIRV_BIN && outputType == SourceType::SPIRV_TEXT)
    {
        return disassembleSPIRV(assertSource<SourceType::SPIRV_BIN>(input), extendedOptions,
            getResult<SourceType::SPIRV_TEXT>(std::move(desiredOutput)))
            .publish();
    }
    else if(input.getType() == SourceType::SPIRV_TEXT && outputType == SourceType::SPIRV_BIN)
    {
        return assembleSPIRV(assertSource<SourceType::SPIRV_TEXT>(input), extendedOptions,
            getResult<SourceType::SPIRV_BIN>(std::move(desiredOutput)))
            .publish();
    }
    throw CompilationError(CompilationStep::PRECOMPILATION, "Unhandled pre-compilation");
}
