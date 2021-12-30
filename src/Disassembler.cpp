/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "VC4C.h"

#include "GlobalValues.h"
#include "Locals.h"
#include "asm/ALUInstruction.h"
#include "asm/Instruction.h"
#include "asm/KernelInfo.h"
#include "asm/LoadInstruction.h"
#include "log.h"

#include <fstream>
#include <sstream>

using namespace vc4c;

// Is located in Types.cpp
extern TypeHolder GLOBAL_TYPE_HOLDER;

LCOV_EXCL_START
static std::vector<std::string> createUniformValues(const KernelHeader& kernel)
{
    /* the order of implicit/explicit parameters needs to match the order of parameters in
     * optimizations::addStartStopSegment:
     * - work_dim: number of dimensions
     * - local_sizes: local number of work-items in its work-group per dimension
     * - local_ids: local id of this work-item within its work-group
     * - num_groups (x,y,z): global number of work-groups per dimension
     * - group_id (x, y, z): id of this work-group
     * - global_offset (x, y, z): global initial offset per dimension
     * - address of global data / to load the global data from
     * - parameters
     * - re-run counter
     */
    const KernelUniforms& uniformsUsed = kernel.uniformsUsed;
    std::vector<std::string> values;
    values.reserve(uniformsUsed.countUniforms() + kernel.getParamCount());
    if(uniformsUsed.getWorkDimensionsUsed())
        values.emplace_back("work dimensions");
    if(uniformsUsed.getLocalSizesUsed())
        values.emplace_back("local sizes");
    if(uniformsUsed.getLocalIDsUsed())
        values.emplace_back("local IDs");
    if(uniformsUsed.getNumGroupsXUsed())
        values.emplace_back("number of groups X");
    if(uniformsUsed.getNumGroupsYUsed())
        values.emplace_back("number of groups Y");
    if(uniformsUsed.getNumGroupsZUsed())
        values.emplace_back("number of groups Z");
    if(uniformsUsed.getGroupIDXUsed())
        values.emplace_back("group ID X");
    if(uniformsUsed.getGroupIDYUsed())
        values.emplace_back("group ID Y");
    if(uniformsUsed.getGroupIDZUsed())
        values.emplace_back("group ID Z");
    if(uniformsUsed.getGlobalOffsetXUsed())
        values.emplace_back("global offset X");
    if(uniformsUsed.getGlobalOffsetYUsed())
        values.emplace_back("global offset Y");
    if(uniformsUsed.getGlobalOffsetZUsed())
        values.emplace_back("global offset Z");
    if(uniformsUsed.getGlobalDataAddressUsed())
        values.emplace_back("global data address");
    for(const auto& param : kernel.parameters)
    {
        // To correctly annotate multi-UNIFORM parameters (e.g. literal vectors)
        for(uint16_t i = 0; i < param.getVectorElements(); ++i)
            values.emplace_back(param.typeName + " " + param.name);
    }
    if(uniformsUsed.getUniformAddressUsed())
        values.emplace_back("uniform address");
    if(uniformsUsed.getMaxGroupIDXUsed())
        values.emplace_back("maximum group ID X");
    if(uniformsUsed.getMaxGroupIDYUsed())
        values.emplace_back("maximum group ID Y");
    if(uniformsUsed.getMaxGroupIDZUsed())
        values.emplace_back("maximum group ID Z");

    return values;
}

static bool readsUniform(Register reg)
{
    return reg.file != RegisterFile::ACCUMULATOR && reg.num == REG_UNIFORM.num;
}

static std::string getUniform(unsigned index, const std::vector<std::string>& values)
{
    if(index >= values.size())
    {
        return "UNIFORM out of bounds!";
    }
    return values[index];
}

static std::string annotateRegisters(const qpu_asm::Instruction& instr, std::size_t index, const ModuleHeader& module)
{
    static FastMap<Register, std::string> currentRegisterMapping;
    static const KernelHeader* currentKernel = nullptr;
    static unsigned currentUniformsRead = 0;
    static std::vector<std::string> currentUniformValues;
    if(instr.getSig() == SIGNAL_END_PROGRAM)
    {
        // all following registers belong to new kernel
        currentRegisterMapping.clear();
        currentKernel = nullptr;
        currentUniformsRead = 0;
        currentUniformValues.clear();
        return "";
    }
    if(currentKernel == nullptr)
    {
        // determine the next kernel with the largest offset smaller than the current index as current kernel
        for(const auto& kernel : module.kernels)
        {
            if(kernel.getOffset() <= index)
            {
                if(currentKernel && currentKernel->getOffset() > kernel.getOffset())
                    continue;
                currentKernel = &kernel;
            }
        }
        if(currentKernel == nullptr)
            return "";
        else
            // TODO order of kernels extracted/kernel-code association is wrong, see test_vector.cl
            currentUniformValues = createUniformValues(*currentKernel);
    }

    std::set<std::string> annotations;
    // the first few UNIFORMs are the work-item and work-group info
    if(auto op = instr.as<qpu_asm::ALUInstruction>())
    {
        if(readsUniform(op->getAddFirstOperand()) || readsUniform(op->getAddSecondOperand()))
            annotations.emplace("uniform read: " + getUniform(currentUniformsRead, currentUniformValues));

        if(readsUniform(op->getMulFirstOperand()) || readsUniform(op->getMulSecondOperand()))
            annotations.emplace("uniform read: " + getUniform(currentUniformsRead, currentUniformValues));

        FastMap<Register, std::string>::const_iterator regIt;
        if((regIt = currentRegisterMapping.find(op->getAddFirstOperand())) != currentRegisterMapping.end())
            annotations.emplace("read " + regIt->second);
        if((regIt = currentRegisterMapping.find(op->getAddSecondOperand())) != currentRegisterMapping.end())
            annotations.emplace("read " + regIt->second);
        if((regIt = currentRegisterMapping.find(op->getMulFirstOperand())) != currentRegisterMapping.end())
            annotations.emplace("read " + regIt->second);
        if((regIt = currentRegisterMapping.find(op->getMulSecondOperand())) != currentRegisterMapping.end())
            annotations.emplace("read " + regIt->second);

        // TODO handle both writing the same register
        if(op->getAddition() == OP_OR.opAdd && op->getAddFirstOperand() == op->getAddSecondOperand() &&
            op->getAddCondition() == COND_ALWAYS)
        {
            // propagate moves
            if(readsUniform(op->getAddFirstOperand()))
                currentRegisterMapping[op->getAddOutput()] = getUniform(currentUniformsRead, currentUniformValues);
            else if((regIt = currentRegisterMapping.find(op->getAddFirstOperand())) != currentRegisterMapping.end())
                currentRegisterMapping[op->getAddOutput()] = regIt->second;
        }
        else
            currentRegisterMapping.erase(op->getAddOutput());
        if(op->getMultiplication() == OP_V8MIN.opMul && op->getMulFirstOperand() == op->getMulSecondOperand() &&
            op->getMulCondition() == COND_ALWAYS)
        {
            if(readsUniform(op->getMulFirstOperand()))
                currentRegisterMapping[op->getMulOutput()] = getUniform(currentUniformsRead, currentUniformValues);
            else if((regIt = currentRegisterMapping.find(op->getMulFirstOperand())) != currentRegisterMapping.end())
                currentRegisterMapping[op->getMulOutput()] = regIt->second;
        }
        else
            currentRegisterMapping.erase(op->getMulOutput());

        if(readsUniform(op->getAddFirstOperand()) || readsUniform(op->getAddSecondOperand()) ||
            readsUniform(op->getMulFirstOperand()) || readsUniform(op->getMulSecondOperand()))
            ++currentUniformsRead;
    }
    else if(auto load = instr.as<qpu_asm::LoadInstruction>())
    {
        switch(load->getType())
        {
        case OpLoad::LOAD_IMM_32:
            if(load->getAddCondition() == COND_ALWAYS)
                currentRegisterMapping[load->getAddOutput()] = std::to_string(load->getImmediateInt());
            if(load->getMulCondition() == COND_ALWAYS)
                currentRegisterMapping[load->getMulOutput()] = std::to_string(load->getImmediateInt());
            break;
        case OpLoad::LOAD_SIGNED:
            if(load->getAddCondition() == COND_ALWAYS)
                currentRegisterMapping[load->getAddOutput()] = "(" + std::to_string(load->getImmediateSignedShort0()) +
                    "," + std::to_string(load->getImmediateSignedShort1()) + ")";
            if(load->getMulCondition() == COND_ALWAYS)
                currentRegisterMapping[load->getMulOutput()] = "(" + std::to_string(load->getImmediateSignedShort0()) +
                    "," + std::to_string(load->getImmediateSignedShort1()) + ")";
            break;
        case OpLoad::LOAD_UNSIGNED:
            if(load->getAddCondition() == COND_ALWAYS)
                currentRegisterMapping[load->getAddOutput()] = "(" + std::to_string(load->getImmediateShort0()) + "," +
                    std::to_string(load->getImmediateShort1()) + ")";
            if(load->getMulCondition() == COND_ALWAYS)
                currentRegisterMapping[load->getMulOutput()] = "(" + std::to_string(load->getImmediateShort0()) + "," +
                    std::to_string(load->getImmediateShort1()) + ")";
            break;
        }
    }
    // TODO some more annotations, e.g. mark parameters, annotate all offsets accordingly (e.g. "%in + 72" or "%in +
    // offset"). Do same for global data, determine global referenced by offset?

    return annotations.empty() ? "" : (" // " + vc4c::to_string<std::string, std::set<std::string>>(annotations));
}
LCOV_EXCL_STOP

void extractBinary(const CompilationData& binary, ModuleHeader& module, StableList<Global>& globals,
    std::vector<qpu_asm::Instruction>& instructions)
{
    std::vector<uint64_t> binaryData;
    std::vector<uint8_t> rawData;
    if(binary.getRawData(rawData))
    {
        binaryData.resize(rawData.size() / sizeof(uint64_t));
        std::memcpy(binaryData.data(), rawData.data(), (rawData.size() / sizeof(uint64_t)) * sizeof(uint64_t));
    }
    else
    {
        // read whole stream into 64-bit words
        std::stringstream binaryStream;
        binary.readInto(binaryStream);
        binaryStream.seekg(0, binaryStream.end);
        auto numBytes = static_cast<std::size_t>(std::max(binaryStream.tellg(), std::streampos{8192}));
        binaryStream.seekg(0);
        binaryStream.clear();
        binaryData.reserve(numBytes / sizeof(uint64_t));
        uint64_t tmp = 0;
        while(binaryStream.read(reinterpret_cast<char*>(&tmp), sizeof(tmp)))
            binaryData.push_back(tmp);
    }

    module = ModuleHeader::fromBinaryData(binaryData);

    auto initialInstructionOffset = std::numeric_limits<std::size_t>::max();
    std::size_t totalInstructions = 0;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Extracted module with " << module.getKernelCount() << " kernels, " << module.getGlobalDataSize()
            << " words of global data and " << module.getStackFrameSize() << " words of stack-frames" << logging::endl);

    for(const auto& kernel : module.kernels)
    {
        totalInstructions += kernel.getLength();
        initialInstructionOffset = std::min(initialInstructionOffset, kernel.getOffset());

        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Extracted kernel '" << kernel.name << "' with " << kernel.getParamCount()
                             << " parameters and " << kernel.getLength() << " instructions starting at offset "
                             << kernel.getOffset() << logging::endl;
            for(const auto& param : kernel.parameters)
                logging::debug() << "Extracted parameter '" << param.typeName << " " << param.name << logging::endl;
        });
    }

    if(module.getGlobalDataSize() > 0)
    {
        // since we don't know the number, sizes and types of the original globals, we build a single global containing
        // all the data
        auto num32BitWords = module.getGlobalDataSize() * 2;

        const DataType type =
            DataType(GLOBAL_TYPE_HOLDER.createArrayType(TYPE_INT32, static_cast<unsigned>(num32BitWords)));
        std::vector<CompoundConstant> elements;
        elements.reserve(num32BitWords);
        for(std::size_t i = 0; i < module.getGlobalDataSize(); ++i)
        {
            // words are already in little endian
            auto word = binaryData[module.getGlobalDataOffset() + i];
            elements.emplace_back(TYPE_INT32, Literal(static_cast<unsigned>(word & 0xFFFFFFFF)));
            elements.emplace_back(TYPE_INT32, Literal(static_cast<unsigned>((word >> 32) & 0xFFFFFFFF)));
        }

        globals.emplace_back("globalData", DataType(GLOBAL_TYPE_HOLDER.createPointerType(type, AddressSpace::GLOBAL)),
            CompoundConstant(type, std::move(elements)), false);

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Extracted " << module.getGlobalDataSize() << " words of global data" << logging::endl);
    }

    // the remainder is kernel-code
    // we don't need to associate it to any particular kernel
    instructions.reserve(totalInstructions);

    for(auto i = initialInstructionOffset; i < totalInstructions + initialInstructionOffset; ++i)
    {
        uint64_t tmp64 = binaryData[i];
        qpu_asm::Instruction instr(tmp64);
        if(!instr.isValidInstruction())
            throw CompilationError(CompilationStep::GENERAL, "Unrecognized instruction", std::to_string(tmp64));
        instructions.emplace_back(instr);
        logging::logLazy(logging::Level::DEBUG, [&](std::wostream& os) {
            os << instr.toASMString() << annotateRegisters(instr, i, module) << logging::endl;
        });
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Extracted " << totalInstructions << " machine-code instructions" << logging::endl);
}

static std::size_t generateOutput(std::ostream& stream, ModuleHeader& module, const StableList<Global>& globals,
    const std::vector<qpu_asm::Instruction>& instructions, const OutputMode outputMode)
{
    std::size_t numBytes = qpu_asm::writeModule(stream, module, outputMode, globals, Byte(0)) * sizeof(uint64_t);

    for(const auto& instr : instructions)
    {
        switch(outputMode)
        {
        case OutputMode::ASSEMBLER:
            stream << instr.toASMString() << std::endl;
            numBytes += 0; // doesn't matter here, since the number of bytes is unused for assembler output
            break;
        case OutputMode::HEX:
            stream << instr.toHexString(true) << std::endl;
            numBytes += 8; // doesn't matter here, since the number of bytes is unused for hexadecimal output
            break;
        default:
            throw CompilationError(
                CompilationStep::GENERAL, "Invalid output mode", std::to_string(static_cast<unsigned>(outputMode)));
        }
    }
    stream.flush();
    return numBytes;
}

std::size_t vc4c::disassembleModule(const CompilationData& binaryData, std::ostream& output, OutputMode outputMode)
{
    if(binaryData.getType() != SourceType::QPUASM_BIN)
        throw CompilationError(CompilationStep::GENERAL, "Invalid input binary for disassembling!");
    if(outputMode == OutputMode::BINARY)
    {
        binaryData.readInto(output);
        return 0;
    }

    ModuleHeader module;
    StableList<Global> globals;
    std::vector<qpu_asm::Instruction> instructions;
    extractBinary(binaryData, module, globals, instructions);

    return generateOutput(output, module, globals, instructions, outputMode);
}

std::size_t vc4c::disassembleCodeOnly(
    std::istream& binary, std::ostream& output, std::size_t numInstructions, const OutputMode outputMode)
{
    std::size_t numBytes = 0;
    for(std::size_t i = 0; i < numInstructions; ++i)
    {
        uint64_t tmp64 = 0;
        binary.read(reinterpret_cast<char*>(&tmp64), sizeof(tmp64));
        qpu_asm::Instruction instr(tmp64);
        if(!instr.isValidInstruction())
            throw CompilationError(CompilationStep::GENERAL, "Unrecognized instruction", std::to_string(tmp64));
        switch(outputMode)
        {
        case OutputMode::ASSEMBLER:
            output << instr.toASMString() << std::endl;
            numBytes += 0; // doesn't matter here, since the number of bytes is unused for assembler output
            break;
        case OutputMode::HEX:
            output << instr.toHexString(true) << std::endl;
            numBytes += 8; // doesn't matter here, since the number of bytes is unused for hexadecimal output
            break;
        default:
            throw CompilationError(
                CompilationStep::GENERAL, "Invalid output mode", std::to_string(static_cast<unsigned>(outputMode)));
        }
    }
    return numBytes;
}

// command-line version
void disassemble(const std::string& input, const std::string& output, const OutputMode outputMode)
{
    CompilationData inputData{};
    std::unique_ptr<std::ostream> outputFile;
    std::ostream* os = nullptr;

    if(input == "-" || input == "/dev/stdin")
        inputData = CompilationData{std::cin, SourceType::UNKNOWN, "stdin"};
    else
        inputData = CompilationData{input};

    if(output.empty() || output == "-" || output == "/dev/stdout")
        os = &std::cout;
    else
    {
        outputFile = std::make_unique<std::ofstream>(output);
        os = outputFile.get();
    }

    disassembleModule(inputData, *os, outputMode);
}
