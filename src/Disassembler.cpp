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
static std::vector<std::string> createUniformValues(const qpu_asm::KernelInfo& kernel)
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
    values.emplace_back("re-run flag");

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

static std::string annotateRegisters(
    const qpu_asm::Instruction& instr, uint64_t index, const qpu_asm::ModuleInfo& module)
{
    static FastMap<Register, std::string> currentRegisterMapping;
    static const qpu_asm::KernelInfo* currentKernel = nullptr;
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
        for(const auto& kernel : module.kernelInfos)
        {
            if(kernel.getOffset().getValue() <= index)
            {
                if(currentKernel && currentKernel->getOffset().getValue() > kernel.getOffset().getValue())
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

static std::string readString(std::istream& binary, uint64_t stringLength)
{
    std::array<char, 1024> buffer;

    binary.read(buffer.data(), static_cast<std::streamsize>(stringLength));
    const std::string name(buffer.data(), stringLength);
    uint64_t numPaddingBytes = Byte(stringLength).getPaddingTo(sizeof(uint64_t));
    // skip padding after kernel name
    binary.read(buffer.data(), static_cast<std::streamsize>(numPaddingBytes));

    return name;
}

void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo, StableList<Global>& globals,
    std::vector<qpu_asm::Instruction>& instructions)
{
    // skip magic number
    binary.seekg(8);

    uint64_t initialInstructionOffset = std::numeric_limits<uint64_t>::max();
    uint64_t totalInstructions = 0;
    binary.read(reinterpret_cast<char*>(&moduleInfo.value), sizeof(moduleInfo.value));
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Extracted module with " << moduleInfo.getInfoCount() << " kernels, "
            << moduleInfo.getGlobalDataSize().getValue() << " words of global data and "
            << moduleInfo.getStackFrameSize().getValue() << " words of stack-frames" << logging::endl);

    for(uint16_t k = 0; k < moduleInfo.getInfoCount(); ++k)
    {
        qpu_asm::KernelInfo kernelInfo(4);
        binary.read(reinterpret_cast<char*>(&kernelInfo.value), sizeof(kernelInfo.value));
        binary.read(reinterpret_cast<char*>(&kernelInfo.workGroupSize), sizeof(kernelInfo.workGroupSize));
        binary.read(reinterpret_cast<char*>(&kernelInfo.uniformsUsed.value), sizeof(kernelInfo.uniformsUsed.value));
        kernelInfo.name = readString(binary, kernelInfo.getNameLength().getValue());
        totalInstructions += kernelInfo.getLength().getValue();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Extracted kernel '" << kernelInfo.name << "' with " << kernelInfo.getParamCount() << " parameters"
                << logging::endl);

        for(uint16_t p = 0; p < kernelInfo.getParamCount(); ++p)
        {
            qpu_asm::ParamInfo paramInfo;
            binary.read(reinterpret_cast<char*>(&paramInfo.value), sizeof(paramInfo.value));
            paramInfo.name = readString(binary, paramInfo.getNameLength().getValue());
            paramInfo.typeName = readString(binary, paramInfo.getTypeNameLength().getValue());
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Extracted parameter '" << paramInfo.typeName << " " << paramInfo.name << logging::endl);
            kernelInfo.parameters.push_back(paramInfo);
        }
        moduleInfo.kernelInfos.push_back(kernelInfo);
        initialInstructionOffset = std::min(initialInstructionOffset, kernelInfo.getOffset().getValue());
    }

    // skip zero-word between kernels and globals
    binary.seekg(sizeof(uint64_t), std::ios_base::cur);

    if(moduleInfo.getGlobalDataSize().getValue() > 0)
    {
        // since we don't know the number, sizes and types of the original globals, we build a single global containing
        // all the data
        std::vector<uint32_t> tmp;
        tmp.resize(moduleInfo.getGlobalDataSize().getValue() * 2);
        binary.read(reinterpret_cast<char*>(tmp.data()),
            static_cast<std::streamsize>(moduleInfo.getGlobalDataSize().toBytes().getValue()));

        const DataType type = DataType(GLOBAL_TYPE_HOLDER.createArrayType(
            TYPE_INT32, static_cast<unsigned>(moduleInfo.getGlobalDataSize().getValue()) * 2));
        std::vector<CompoundConstant> elements;
        elements.reserve(tmp.size());
        for(uint32_t t : tmp)
            // words are already in little endian
            elements.emplace_back(TYPE_INT32, Literal(t));

        globals.emplace_back("globalData", DataType(GLOBAL_TYPE_HOLDER.createPointerType(type, AddressSpace::GLOBAL)),
            CompoundConstant(type, std::move(elements)), false);

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Extracted " << moduleInfo.getGlobalDataSize().getValue() << " words of global data"
                << logging::endl);
    }

    // skip zero-word between globals and kernel-code
    binary.seekg(sizeof(uint64_t), std::ios_base::cur);

    // the remainder is kernel-code
    // we don't need to associate it to any particular kernel
    instructions.reserve(totalInstructions);

    for(uint64_t i = initialInstructionOffset; i < totalInstructions + initialInstructionOffset; ++i)
    {
        uint64_t tmp64;
        binary.read(reinterpret_cast<char*>(&tmp64), sizeof(tmp64));
        qpu_asm::Instruction instr(tmp64);
        if(!instr.isValidInstruction())
            throw CompilationError(CompilationStep::GENERAL, "Unrecognized instruction", std::to_string(tmp64));
        instructions.emplace_back(instr);
        logging::logLazy(logging::Level::DEBUG, [&](std::wostream& os) {
            os << instr.toASMString() << annotateRegisters(instr, i, moduleInfo) << logging::endl;
        });
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Extracted " << totalInstructions << " machine-code instructions" << logging::endl);
}

static std::size_t generateOutput(std::ostream& stream, qpu_asm::ModuleInfo& moduleInfo,
    const StableList<Global>& globals, const std::vector<qpu_asm::Instruction>& instructions,
    const OutputMode outputMode)
{
    std::size_t numBytes = moduleInfo.write(stream, outputMode, globals, Byte(0)) * sizeof(uint64_t);

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

std::size_t vc4c::disassembleModule(std::istream& binary, std::ostream& output, const OutputMode outputMode)
{
    if(Precompiler::getSourceType(binary) != SourceType::QPUASM_BIN)
        throw CompilationError(CompilationStep::GENERAL, "Invalid input binary for disassembling!");
    if(outputMode == OutputMode::BINARY)
    {
        output << binary.rdbuf();
        return 0;
    }

    qpu_asm::ModuleInfo moduleInfo;
    StableList<Global> globals;
    std::vector<qpu_asm::Instruction> instructions;
    extractBinary(binary, moduleInfo, globals, instructions);

    return generateOutput(output, moduleInfo, globals, instructions, outputMode);
}

std::size_t vc4c::disassembleCodeOnly(
    std::istream& binary, std::ostream& output, std::size_t numInstructions, const OutputMode outputMode)
{
    std::size_t numBytes = 0;
    for(std::size_t i = 0; i < numInstructions; ++i)
    {
        uint64_t tmp64;
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
    std::unique_ptr<std::istream> inputFile;
    std::unique_ptr<std::ostream> outputFile;
    std::istream* is = nullptr;
    std::ostream* os = nullptr;

    if(input == "-" || input == "/dev/stdin")
        is = &std::cin;
    else
    {
        inputFile = std::make_unique<std::ifstream>(input, std::ios_base::in | std::ios_base::binary);
        is = inputFile.get();
    }

    if(output.empty() || output == "-" || output == "/dev/stdout")
        os = &std::cout;
    else
    {
        outputFile = std::make_unique<std::ofstream>(output);
        os = outputFile.get();
    }

    disassembleModule(*is, *os, outputMode);
}
