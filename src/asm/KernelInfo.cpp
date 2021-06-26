/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "KernelInfo.h"

#include "../GlobalValues.h"
#include "../intermediate/IntermediateInstruction.h"
#include "Instruction.h"
#include "log.h"

#include <array>
#include <cstring>
#include <numeric>

using namespace vc4c;
using namespace vc4c::qpu_asm;

// TODO can also remove address/uniform for lowered parameter on both sides!

static void writeStream(std::ostream& stream, uint64_t value, const OutputMode mode)
{
    if(mode == OutputMode::BINARY)
    {
        stream.write(reinterpret_cast<const char*>(&value), static_cast<std::streamsize>(sizeof(value)));
    }
    else if(mode == OutputMode::HEX)
    {
        std::array<char, 64> buffer{};
        snprintf(buffer.data(), buffer.size(), "0x%08x, 0x%08x, ", static_cast<uint32_t>(value & 0xFFFFFFFFLL),
            static_cast<uint32_t>((value & 0xFFFFFFFF00000000LL) >> 32));
        stream << std::string(buffer.data()) << std::endl;
    }
}

static void toBinary(const CompoundConstant& val, std::vector<uint8_t>& queue)
{
    if(auto container = val.getCompound())
    {
        for(const auto& element : *container)
            toBinary(element, queue);
    }
    else if(auto lit = val.getScalar())
    {
        switch(lit->type)
        {
        case LiteralType::BOOL:
            for(std::size_t i = 0; i < val.type.getVectorWidth(true); ++i)
                queue.push_back(static_cast<uint8_t>(lit->isTrue()));
            break;
        case LiteralType::INTEGER:
        case LiteralType::REAL:
            for(std::size_t i = 0; i < val.type.getVectorWidth(true); ++i)
            {
                // little endian (LSB has lowest address -> is first in memory)
                queue.push_back(static_cast<uint8_t>(lit->toImmediate() & 0xFF));
                if(val.type.getElementType().getScalarBitCount() > 8)
                    queue.push_back(static_cast<uint8_t>((lit->toImmediate() & 0xFF00) >> 8));
                if(val.type.getElementType().getScalarBitCount() > 16)
                    queue.push_back(static_cast<uint8_t>((lit->toImmediate() & 0xFF0000) >> 16));
                if(val.type.getElementType().getScalarBitCount() > 24)
                    queue.push_back(static_cast<uint8_t>((lit->toImmediate() & 0xFF000000) >> 24));
            }
            break;
        default:
            throw CompilationError(CompilationStep::CODE_GENERATION, "Unrecognized literal-type!", val.to_string());
        }
    }
    else if(val.isUndefined())
    {
        // e.g. for array <type> undefined, need to reserve enough bytes
        for(std::size_t s = 0; s < val.type.getInMemoryWidth(); ++s)
            queue.push_back(0);
    }
    else
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Can't map value-type to binary literal", val.to_string());
}

static std::vector<uint8_t> generateDataSegment(const StableList<Global>& globalData, Byte totalStackFrameSize)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Writing data segment for " << globalData.size() << " values..." << logging::endl);
    std::vector<uint8_t> bytes;
    bytes.reserve(2048);
    for(const Global& global : globalData)
    {
        // add alignment per element
        const unsigned alignment = global.type.getPointerType()->getAlignment();
        while(bytes.size() % alignment != 0)
        {
            bytes.push_back(0);
        }
        toBinary(global.initialValue, bytes);
    }

    // allocate space for stack
    if(totalStackFrameSize.getValue() > 0)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reserving " << totalStackFrameSize << " bytes for stack-frames..." << logging::endl);
        for(std::size_t s = 0; s < totalStackFrameSize.getValue(); ++s)
            bytes.push_back(0);
    }

    while((bytes.size() % 8) != 0)
    {
        bytes.push_back(0);
    }
    return bytes;
}

std::size_t qpu_asm::writeModule(std::ostream& stream, ModuleHeader& module, const OutputMode mode,
    const StableList<Global>& globalData, Byte totalStackFrameSize)
{
    std::size_t numWords = 0;
    if(mode == OutputMode::HEX || mode == OutputMode::ASSEMBLER)
    {
        stream << "// Module with " << module.getKernelCount() << " kernels, global data with "
               << module.getGlobalDataSize() << " words (64-bit each), starting at offset "
               << module.getGlobalDataOffset() << " words and " << module.getStackFrameSize() << " words of stack-frame"
               << std::endl;
    }
    if(mode == OutputMode::BINARY || mode == OutputMode::HEX)
    {
        // write magic number
        auto magic =
            uint64_t{ModuleHeader::QPUASM_MAGIC_NUMBER} | (uint64_t{ModuleHeader::QPUASM_MAGIC_NUMBER} << uint64_t{32});
        writeStream(stream, magic, mode);
        ++numWords;

        // write module info
        writeStream(stream, module.value, mode);
        ++numWords;
    }
    // write kernel-infos
    for(const auto& kernel : module.kernels)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << kernel.to_string() << logging::endl);
        if(mode == OutputMode::HEX || mode == OutputMode::ASSEMBLER)
            stream << "// " << kernel.to_string() << std::endl;
        std::vector<uint64_t> buffer;
        kernel.toBinaryData(buffer);
        for(auto word : buffer)
            writeStream(stream, word, mode);
        numWords += buffer.size();
    }
    // write kernel-info-to-global-data delimiter
    writeStream(stream, 0, mode);
    ++numWords;

    // update global data offset
    module.setGlobalDataOffset(numWords);

    // write global data, padded to multiples of 8 Byte
    switch(mode)
    {
    case OutputMode::ASSEMBLER:
    {
        for(const Global& global : globalData)
            stream << global.to_string(true) << std::endl;
        break;
    }
    case OutputMode::BINARY:
    {
        const auto binary = generateDataSegment(globalData, totalStackFrameSize);
        stream.write(reinterpret_cast<const char*>(binary.data()), static_cast<std::streamsize>(binary.size()));
        numWords += binary.size() / sizeof(uint64_t);
        break;
    }
    case OutputMode::HEX:
    {
        const auto binary = generateDataSegment(globalData, totalStackFrameSize);
        for(const Global& global : globalData)
            stream << "//" << global.to_string(true) << std::endl;
        if(totalStackFrameSize.getValue() > 0)
            stream << "//" << totalStackFrameSize << " bytes of stack" << std::endl;
        for(std::size_t i = 0; i < binary.size(); i += 8)
            stream << toHexString((static_cast<uint64_t>(binary[i]) << 56) |
                          (static_cast<uint64_t>(binary[i + 1]) << 48) | (static_cast<uint64_t>(binary[i + 2]) << 40) |
                          (static_cast<uint64_t>(binary[i + 3]) << 32) | (static_cast<uint64_t>(binary[i + 4]) << 24) |
                          (static_cast<uint64_t>(binary[i + 5]) << 16) | (static_cast<uint64_t>(binary[i + 6]) << 8) |
                          static_cast<uint64_t>(binary[i + 7]))
                   << std::endl;
        numWords += binary.size() / sizeof(uint64_t);
        break;
    }
    }

    // update global data size
    module.setGlobalDataSize(numWords - module.getGlobalDataOffset());

    // write global-data-to-kernel-instructions delimiter
    writeStream(stream, 0, mode);
    ++numWords;

    return numWords;
}

KernelHeader qpu_asm::createKernelHeader(
    const Method& method, const std::size_t initialOffset, const std::size_t numInstructions)
{
    KernelHeader kernel(method.parameters.size());
    kernel.setOffset(initialOffset);
    kernel.setLength(numInstructions);
    kernel.setName(method.name[0] == '@' ? method.name.substr(1) : method.name);
    kernel.workGroupSize.fill(0);
    kernel.workItemMergeFactor = method.metaData.mergedWorkItemsFactor;
    kernel.uniformsUsed = method.metaData.uniformsUsed;
    {
        for(std::size_t i = 0; i < method.metaData.workGroupSizes.size(); ++i)
            kernel.workGroupSize[i] = static_cast<uint16_t>(method.metaData.workGroupSizes[i]);
        auto maxNumInstances = method.metaData.getMaximumInstancesCount();
        if(maxNumInstances > NUM_QPUS)
        {
            logging::error() << "Required number of instances " << maxNumInstances << " exceeds the limit of "
                             << NUM_QPUS << logging::endl;
        }
    }
    {
        uint32_t sizeHint = std::accumulate(method.metaData.workGroupSizeHints.begin(),
            method.metaData.workGroupSizeHints.end(), 1u, std::multiplies<uint32_t>());
        auto sizeFactor = std::max(method.metaData.mergedWorkItemsFactor, uint8_t{1});
        sizeHint = (sizeHint / sizeFactor) + (sizeHint % sizeFactor != 0);
        if(sizeHint > NUM_QPUS)
        {
            logging::warn() << "Work-group size hint " << sizeHint << " exceeds the limit of " << NUM_QPUS
                            << logging::endl;
        }
    }
    for(const Parameter& param : method.parameters)
    {
        std::string paramName = param.parameterName;
        paramName = paramName.empty() ? param.name : paramName;
        auto paramType = param.type;
        std::string typeName = param.origTypeName;
        ParamHeader paramHeader;
        paramHeader.setSize(static_cast<uint16_t>(paramType.getInMemoryWidth()));
        paramHeader.setPointer(paramType.getPointerType() || paramType.getImageType());
        paramHeader.setImage(!!paramType.getImageType());
        paramHeader.setDecorations(static_cast<uint16_t>(param.decorations));
        paramHeader.setName(paramName[0] == '%' ? paramName.substr(1) : paramName);
        paramHeader.setVectorElements(
            (paramType.getPointerType() ? static_cast<uint8_t>(1) : paramType.getVectorWidth()));
        paramHeader.setAddressSpace(paramType.getPointerType() ?
                paramType.getPointerType()->addressSpace :
                paramType.getImageType() ? AddressSpace::GLOBAL : AddressSpace::PRIVATE);
        paramHeader.setFloatingType(paramType.isFloatingType());
        // FIXME signedness is only recognized correctly for non-32 bit scalar types (e.g. (u)char, (u)short), not for
        // pointers or even vector-types
        paramHeader.setSigned(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND));
        paramHeader.setUnsigned(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND));
        paramHeader.setTypeName(
            typeName.empty() ? paramType.getTypeName(paramHeader.getSigned(), paramHeader.getUnsigned()) : typeName);
        paramHeader.setLowered(param.isLowered);

        if(paramType.getPointerType() && has_flag(param.decorations, ParameterDecorations::BY_VALUE))
        {
            // since the client passes the actual (struct) type to as argument, the VC4CL run-time needs to know that
            // size
            paramHeader.setSize(static_cast<uint16_t>(paramType.getPointerType()->elementType.getInMemoryWidth()));
            // we also need to fix-up the other decorations and qualifiers we modified:
            // direct struct parameters are always in the __private address space (since they are function-local
            // values), we just "moved" them to the __constant address space for a) better optimization and b) kernels
            // do not allow __private parameters
            paramHeader.setAddressSpace(AddressSpace::PRIVATE);
        }
        kernel.addParameter(paramHeader);
    }

    if(!method.stackAllocations.empty())
    {
        LCOV_EXCL_START
        logging::logLazy(logging::Level::DEBUG, [&]() {
            logging::debug() << "Kernel " << method.name << ":" << logging::endl;
            for(const auto& s : method.stackAllocations)
                logging::debug() << "Stack-Entry: " << s.to_string() << ", size: " << s.size
                                 << ", alignment: " << s.alignment << ", offset: " << s.offset
                                 << (s.isLowered ? " (lowered)" : "") << logging::endl;
        });
        LCOV_EXCL_STOP
    }

    return kernel;
}
