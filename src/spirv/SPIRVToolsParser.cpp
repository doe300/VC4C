/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVToolsParser.h"

#include "SPIRVHelper.h"
#include "log.h"

#ifdef SPIRV_FRONTEND

#if __has_include("spirv-tools/linker.hpp")
#include "spirv-tools/linker.hpp"
#endif

using namespace vc4c;
using namespace vc4c::spirv;

SPIRVToolsInstruction::~SPIRVToolsInstruction() noexcept = default;

spv::Op SPIRVToolsInstruction::getOpcode() const noexcept
{
    return static_cast<spv::Op>(inst->opcode);
}

uint32_t SPIRVToolsInstruction::getResultId() const noexcept
{
    return inst->result_id;
}

uint32_t SPIRVToolsInstruction::getTypeId() const noexcept
{
    return inst->type_id;
}

std::size_t SPIRVToolsInstruction::getNumWords() const noexcept
{
    return inst->num_words;
}

uint32_t SPIRVToolsInstruction::getWord(std::size_t wordIndex) const
{
    if(inst->num_words <= wordIndex)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(wordIndex));
    return inst->words[wordIndex];
}

std::vector<uint32_t> SPIRVToolsInstruction::parseArguments(std::size_t startIndex) const
{
    std::vector<uint32_t> args;
    if(inst->num_words <= startIndex)
        return args;
    args.reserve(inst->num_words - startIndex);
    for(std::size_t i = startIndex; i < inst->num_words; ++i)
        args.push_back(inst->words[i]);
    return args;
}

uint32_t SPIRVToolsInstruction::getExtendedInstructionType() const noexcept
{
    return inst->ext_inst_type;
}

std::string SPIRVToolsInstruction::readLiteralString(std::size_t operandIndex) const
{
    auto& operand = inst->operands[operandIndex];
    if(inst->num_words <= operand.offset)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(operand.offset));
    const size_t length =
        strnlen(reinterpret_cast<const char*>(inst->words + operand.offset), sizeof(uint32_t) * operand.num_words);
    return std::string(reinterpret_cast<const char*>(inst->words + operand.offset), length);
}

static std::string getErrorMessage(spv_result_t error)
{
    switch(error)
    {
    case SPV_UNSUPPORTED:
        return "Unsupported operation";
    case SPV_END_OF_STREAM:
        return "End of Stream";
    case SPV_WARNING:
        return "Warning";
    case SPV_FAILED_MATCH:
        return "Failed match";
    case SPV_REQUESTED_TERMINATION:
        return "Requested Termination";
    case SPV_ERROR_INTERNAL:
        return "Internal Error";
    case SPV_ERROR_OUT_OF_MEMORY:
        return "Out of memory";
    case SPV_ERROR_INVALID_POINTER:
        return "Invalid pointer";
    case SPV_ERROR_INVALID_BINARY:
        return "Invalid binary input";
    case SPV_ERROR_INVALID_TEXT:
        return "Invalid text input";
    case SPV_ERROR_INVALID_TABLE:
        return "Invalid table";
    case SPV_ERROR_INVALID_VALUE:
        return "Invalid value";
    case SPV_ERROR_INVALID_DIAGNOSTIC:
        return "Invalid diagnostic";
    case SPV_ERROR_INVALID_LOOKUP:
        return "Invalid lookup";
    case SPV_ERROR_INVALID_ID:
        return "Invalid ID";
    case SPV_ERROR_INVALID_CFG:
        return "Invalid configuration";
    case SPV_ERROR_INVALID_LAYOUT:
        return "Invalid layout";
    case SPV_ERROR_INVALID_CAPABILITY:
        return "Invalid capability";
    case SPV_ERROR_INVALID_DATA:
        return "Invalid data";
    default:
        return "General error";
    }
}

static void consumeSPIRVMessage(
    spv_message_level_t level, const char* source, const spv_position_t& position, const char* message)
{
    std::string levelText;
    switch(level)
    {
    case SPV_MSG_DEBUG:
        levelText = "Debug";
        break;
    case SPV_MSG_ERROR:
        levelText = "Error";
        break;
    case SPV_MSG_FATAL:
        levelText = "Fatal";
        break;
    case SPV_MSG_INFO:
        levelText = "Info";
        break;
    case SPV_MSG_INTERNAL_ERROR:
        levelText = "Internal Error";
        break;
    case SPV_MSG_WARNING:
        levelText = "Warning";
        break;
    }
    CPPLOG_LAZY(logging::Level::INFO,
        log << "SPIR-V Tools: " << levelText << " message in '" << source << "' at position " << position.line << ":"
            << position.column << ": " << message << logging::endl);
}

static spv_result_t toSPIRVResult(ParseResultCode code)
{
    switch(code)
    {
    case ParseResultCode::SUCCESS:
        return SPV_SUCCESS;
    case ParseResultCode::UNSUPPORTED:
        return SPV_UNSUPPORTED;
    case ParseResultCode::INTERNAL_ERROR:
        return SPV_ERROR_INTERNAL;
    }

    return SPV_ERROR_INTERNAL;
}

static spv_result_t parsedHeaderCallback(void* user_data, spv_endianness_t endian, uint32_t magic, uint32_t version,
    uint32_t generator, uint32_t id_bound, uint32_t reserved)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "SPIR-V header parsed: magic-number 0x" << std::hex << magic << ", version 0x" << version
            << ", generator " << generator << ", max-ID " << std::dec << id_bound << logging::endl);
    SPIRVToolsParser* parser = static_cast<SPIRVToolsParser*>(user_data);
    return toSPIRVResult(parser->parseHeader(magic, version, generator, id_bound));
}

static spv_result_t parsedInstructionCallback(void* user_data, const spv_parsed_instruction_t* parsed_instruction)
{
    SPIRVToolsParser* parser = static_cast<SPIRVToolsParser*>(user_data);
    if(parsed_instruction == nullptr)
        return SPV_ERROR_INTERNAL;
    SPIRVToolsInstruction inst{parsed_instruction};
    return toSPIRVResult(parser->parseInstruction(inst));
}

static std::string getErrorPosition(spv_diagnostic diagnostics)
{
    if(diagnostics == nullptr)
        return "?";
    return std::to_string(diagnostics->position.line).append(":") + std::to_string(diagnostics->position.column);
}

SPIRVToolsParser::~SPIRVToolsParser() = default;

std::vector<uint32_t> SPIRVToolsParser::assembleTextToBinary(const std::vector<uint32_t>& module)
{
    spvtools::SpirvTools tools(SPV_ENV_OPENCL_EMBEDDED_1_2);
    tools.SetMessageConsumer(consumeSPIRVMessage);
    std::vector<uint32_t> binaryData;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Read SPIR-V text with " << module.size() * sizeof(uint32_t) << " characters" << logging::endl);
    if(tools.Assemble(reinterpret_cast<const char*>(module.data()), module.size() * sizeof(uint32_t), &binaryData))
        return binaryData;
    return {};
}

void SPIRVToolsParser::doParse(const std::vector<uint32_t>& module)
{
    spv_diagnostic diagnostics = nullptr;
    spv_context context = spvContextCreate(SPV_ENV_OPENCL_EMBEDDED_1_2);
    if(context == nullptr)
    {
        throw CompilationError(CompilationStep::PARSER, "Failed to create SPIR-V context");
    }

    spv_result_t result;
    result = spvBinaryParse(
        context, this, module.data(), module.size(), parsedHeaderCallback, parsedInstructionCallback, &diagnostics);

    if(result != SPV_SUCCESS)
    {
        logging::error() << getErrorMessage(result) << ": "
                         << (diagnostics != nullptr ? diagnostics->error : errorExtra) << " at "
                         << getErrorPosition(diagnostics) << logging::endl;
        spvContextDestroy(context);
        throw CompilationError(CompilationStep::PARSER, getErrorMessage(result),
            (diagnostics != nullptr ? diagnostics->error : errorExtra));
    }
    CPPLOG_LAZY(logging::Level::DEBUG, log << "SPIR-V binary successfully parsed" << logging::endl);
    spvContextDestroy(context);
}

void spirv::linkSPIRVModules(const std::vector<std::istream*>& inputModules, std::ostream& output)
{
    std::vector<std::vector<uint32_t>> binaries;
    binaries.reserve(inputModules.size());
    std::transform(inputModules.begin(), inputModules.end(), std::back_inserter(binaries), readStreamOfWords);

    spvtools::LinkerOptions options;
    options.SetCreateLibrary(false);
    options.SetVerifyIds(true);
    // the VC4CL intrinsics are not provided by any input module
    options.SetAllowPartialLinkage(true);

    spvtools::Context spvContext(SPV_ENV_OPENCL_EMBEDDED_1_2);

    std::vector<uint32_t> linkedModules;
    spv_result_t result = spvtools::Link(spvContext, binaries, &linkedModules, options);

    if(result != SPV_SUCCESS)
        throw CompilationError(CompilationStep::PARSER, getErrorMessage(result));

    for(const uint32_t u : linkedModules)
    {
        output.write(reinterpret_cast<const char*>(&u), sizeof(uint32_t));
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Linked " << inputModules.size() << " modules into a single module with " << linkedModules.size()
            << " words of data." << logging::endl);
}

#endif
