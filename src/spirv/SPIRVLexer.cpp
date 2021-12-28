/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVLexer.h"

#include "../Optional.h"
#include "CompilationError.h"
#include "SPIRVHelper.h"
#include "SPIRVOperation.h"
#include "log.h"

#include <array>
#include <cstdint>
#include <cstring>
#include <functional>
#include <vector>

using namespace vc4c;
using namespace vc4c::spirv;

// The endianess detection and conversion trick is adapted from
// https://github.com/KhronosGroup/SPIRV-Tools/blob/master/source/spirv_endian.cpp
// licensed under Apache License 2.0 (https://github.com/KhronosGroup/SPIRV-Tools/blob/master/LICENSE)

static constexpr std::array<uint8_t, 4> SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN = {0x03, 0x02, 0x23, 0x07};
static constexpr std::array<uint8_t, 4> SPIRV_MAGIC_NUMBER_BIG_ENDIAN = {0x07, 0x23, 0x02, 0x03};

enum HostEndian
{
    ENDIAN_LITTLE = 0x03020100ul,
    ENDIAN_BIG = 0x00010203ul,
};

static const union
{
    uint8_t bytes[4];
    uint32_t value;
} host_order = {{0, 1, 2, 3}};

static bool isHostOrder(uint32_t magicNumber)
{
    std::array<uint8_t, 4> buffer = {0};
    memcpy(buffer.data(), &magicNumber, buffer.size());
    if(buffer == SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN)
        return host_order.value == HostEndian::ENDIAN_LITTLE;
    if(buffer == SPIRV_MAGIC_NUMBER_BIG_ENDIAN)
        return host_order.value == HostEndian::ENDIAN_BIG;
    throw CompilationError(CompilationStep::SCANNER, "Invalid SPIR-V magic value", std::to_string(magicNumber));
}

static uint32_t dummyConvert(uint32_t streamOrder)
{
    return streamOrder;
}

static uint32_t swapEndianess(uint32_t streamOrder)
{
    return (streamOrder & 0x000000FF) << 24 | (streamOrder & 0x0000FF00) << 8 | (streamOrder & 0x00FF0000) >> 8 |
        (streamOrder & 0xFF000000) >> 24;
}

ModuleOperation::~ModuleOperation() noexcept = default;

spv::Op ModuleOperation::getOpcode() const noexcept
{
    return opCode;
}

uint32_t ModuleOperation::getResultId() const noexcept
{
    return resultId;
}

uint32_t ModuleOperation::getTypeId() const noexcept
{
    return typeId;
}

std::size_t ModuleOperation::getNumWords() const noexcept
{
    return words.size();
}

uint32_t ModuleOperation::getWord(std::size_t wordIndex) const
{
    if(words.size() <= wordIndex)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(wordIndex));
    return words[wordIndex];
}

std::vector<uint32_t> ModuleOperation::parseArguments(std::size_t startIndex) const
{
    std::vector<uint32_t> args;
    if(words.size() <= startIndex)
        return args;
    args.reserve(words.size() - startIndex);
    for(std::size_t i = startIndex; i < words.size(); ++i)
        args.push_back(words[i]);
    return args;
}

std::string ModuleOperation::readLiteralString(std::size_t operandIndex) const
{
    // XXX this is not necessarily true, but for all instruction we currently access, all previous operands are single
    // word. I.e. this would be wrong when accessing second (or further) string operand!
    auto startWord = 1 /* opcode + size */ + operandIndex;
    const size_t length =
        strnlen(reinterpret_cast<const char*>(words.data() + startWord), sizeof(uint32_t) * (words.size() - startWord));
    return std::string(reinterpret_cast<const char*>(words.data() + startWord), length);
}

static std::pair<spv::Op, uint16_t> splitFirstWord(uint32_t word) noexcept
{
    return std::make_pair(static_cast<spv::Op>(word & 0xFFFF), static_cast<uint16_t>(word >> 16));
}

static Optional<uint32_t> extractTypeId(spv::Op opCode, const uint32_t* opBase) noexcept
{
    bool hasResult = false;
    bool hasType = false;
    spv::HasResultAndType(opCode, &hasResult, &hasType);
    return hasType ? opBase[1 /* opcode */] : Optional<uint32_t>{};
}

static Optional<uint32_t> extractResultId(spv::Op opCode, const uint32_t* opBase) noexcept
{
    bool hasResult = false;
    bool hasType = false;
    spv::HasResultAndType(opCode, &hasResult, &hasType);
    return hasResult ? opBase[hasType ? 2 /* opcode + type-id */ : 1 /* opcode */] : Optional<uint32_t>{};
}

static std::string getErrorMessage(ParseResultCode error)
{
    switch(error)
    {
    case ParseResultCode::UNSUPPORTED:
        return "Unsupported operation";
    case ParseResultCode::INTERNAL_ERROR:
        return "Internal Error";
    default:
        return "General error";
    }
}

SPIRVLexer::~SPIRVLexer() = default;

void SPIRVLexer::doParse(const std::vector<uint32_t>& module)
{
    auto start = parseHeader(module);
    parseBody(module, start.second, start.first);
}

std::pair<EndinanessConverter, std::vector<uint32_t>::const_iterator> SPIRVLexer::parseHeader(
    const std::vector<uint32_t>& input)
{
    if(input.size() < 6)
        return std::make_pair(nullptr, input.end());
    auto it = input.begin();
    // first word -> magic number
    uint32_t magicNumber = *(it++);
    // second word -> version number with one byte per (high to low): 0 | major | minor | 0
    uint32_t versionNumber = *(it++);
    // third word -> generator ID, defaults to zero
    uint32_t generatorId = *(it++);
    // fourth word -> bound, upper ID limit
    uint32_t idBound = *(it++);
    // fifth word -> reserved
    ++it;

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "SPIR-V header parsed: magic-number 0x" << std::hex << magicNumber << ", version 0x" << versionNumber
            << ", generator " << generatorId << ", max-ID " << std::dec << idBound << logging::endl);
    SPIRVParserBase::parseHeader(magicNumber, versionNumber, generatorId, idBound);

    auto converter = isHostOrder(magicNumber) ? dummyConvert : swapEndianess;
    return std::make_pair(converter, it);
}

bool SPIRVLexer::parseBody(const std::vector<uint32_t>& input, std::vector<uint32_t>::const_iterator startIt,
    EndinanessConverter convertEndianess)
{
    ModuleOperation currentOp;
    auto it = startIt;
    while(it != input.end())
    {
        auto opcode = splitFirstWord(convertEndianess(*it));
        currentOp.opCode = opcode.first;
        currentOp.typeId = convertEndianess(extractTypeId(opcode.first, &*it).value_or(UNDEFINED_ID));
        currentOp.resultId = convertEndianess(extractResultId(opcode.first, &*it).value_or(UNDEFINED_ID));
        currentOp.words.resize(opcode.second);

        uint16_t i = 0;
        for(; i < opcode.second && it != input.end(); ++i, ++it)
            currentOp.words[i] = convertEndianess(*it);

        if(it == input.end() && i != opcode.second)
            throw CompilationError(CompilationStep::PARSER, "Reached end-of-stream while parsing operation");
        auto result = parseInstruction(currentOp);
        if(result != ParseResultCode::SUCCESS)
        {
            auto wordOffset = it - input.begin();
            logging::error() << getErrorMessage(result) << ": " << errorExtra << " at word " << wordOffset
                             << logging::endl;
            throw CompilationError(CompilationStep::PARSER, getErrorMessage(result), errorExtra);
        }
    }
    return it == input.end();
}
