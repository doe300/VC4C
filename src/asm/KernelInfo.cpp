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

static void writeStream(std::ostream& stream, const std::array<uint8_t, 8>& buf, const OutputMode mode)
{
    if(mode == OutputMode::BINARY)
    {
        stream.write(reinterpret_cast<const char*>(buf.data()), static_cast<std::streamsize>(buf.size()));
    }
    else if(mode == OutputMode::HEX)
    {
        const uint64_t binary = *reinterpret_cast<const uint64_t*>(buf.data());
        std::array<char, 64> buffer{};
        snprintf(buffer.data(), buffer.size(), "0x%08x, 0x%08x, ", static_cast<uint32_t>(binary & 0xFFFFFFFFLL),
            static_cast<uint32_t>((binary & 0xFFFFFFFF00000000LL) >> 32));
        stream << std::string(buffer.data()) << std::endl;
    }
}

static NODISCARD std::size_t copyName(
    std::ostream& stream, const std::string& name, const OutputMode mode, std::size_t bytesInFirstBlock = 8)
{
    std::array<uint8_t, 8> buf{};
    std::size_t numWords = 0;
    for(std::size_t i = 0; i < name.size(); i += buf.size())
    {
        const std::size_t bytesInBlock = i == 0 ? bytesInFirstBlock : buf.size();
        std::size_t l = std::min(name.size() - i, bytesInBlock);
        // copy name in multiples of 8 byte
        memcpy(buf.data(), name.data() + i, l);
        // pad with zeroes
        memset(buf.data() + l, '\0', bytesInBlock - l);
        writeStream(stream, buf, mode);
        ++numWords;
    }
    return numWords;
}

LCOV_EXCL_START
std::string ParamInfo::to_string() const
{
    // address space
    return std::string((getPointer() && getAddressSpace() == AddressSpace::CONSTANT) ? "__constant " : "") +
        std::string((getPointer() && getAddressSpace() == AddressSpace::GLOBAL) ? "__global " : "") +
        std::string((getPointer() && getAddressSpace() == AddressSpace::LOCAL) ? "__local " : "") +
        std::string((getPointer() && getAddressSpace() == AddressSpace::PRIVATE) ? "__private " : "") +
        // access qualifier
        (getDecorations() != ParameterDecorations::NONE ? toString(getDecorations()) + " " : "") +
        // type + name
        ((typeName) + " ") + (name + " (") + (std::to_string(getSize()) + " B, ") +
        std::to_string(getVectorElements()) + " items)" + (getLowered() ? " (lowered)" : "");
}
LCOV_EXCL_STOP

std::size_t ParamInfo::write(std::ostream& stream, const OutputMode mode) const
{
    std::size_t numWords = 0;
    std::array<uint8_t, 8> buf{};
    if(mode == OutputMode::BINARY || mode == OutputMode::HEX)
    {
        *reinterpret_cast<uint64_t*>(buf.data()) = value;
        writeStream(stream, buf, mode);
        ++numWords;
        numWords += copyName(stream, name, mode);
        numWords += copyName(stream, typeName, mode);
    }
    return numWords;
}

KernelInfo::KernelInfo(const std::size_t& numParameters) : Bitfield(0), workGroupSize(0)
{
    parameters.reserve(numParameters);
}

std::size_t KernelInfo::write(std::ostream& stream, const OutputMode mode) const
{
    std::size_t numWords = 0;
    if(mode == OutputMode::HEX || mode == OutputMode::ASSEMBLER)
    {
        const std::string s = to_string();
        stream << "// " << s << std::endl;
    }
    if(mode == OutputMode::BINARY || mode == OutputMode::HEX)
    {
        std::array<uint8_t, 8> buf{};
        *reinterpret_cast<uint64_t*>(buf.data()) = value;
        writeStream(stream, buf, mode);
        ++numWords;
        *reinterpret_cast<uint64_t*>(buf.data()) = workGroupSize;
        writeStream(stream, buf, mode);
        ++numWords;
        *reinterpret_cast<uint64_t*>(buf.data()) = uniformsUsed.value;
        writeStream(stream, buf, mode);
        ++numWords;
        numWords += copyName(stream, name, mode);
        for(const ParamInfo& info : parameters)
        {
            // for each parameter, copy infos and name
            numWords += info.write(stream, mode);
        }
    }
    return numWords;
}

LCOV_EXCL_START
std::string KernelInfo::to_string() const
{
    std::vector<std::string> uniformsSet;
    if(uniformsUsed.getWorkDimensionsUsed())
        uniformsSet.emplace_back("dims");
    if(uniformsUsed.getLocalSizesUsed())
        uniformsSet.emplace_back("lSize");
    if(uniformsUsed.getLocalIDsUsed())
        uniformsSet.emplace_back("lids");
    if(uniformsUsed.getNumGroupsXUsed())
        uniformsSet.emplace_back("numX");
    if(uniformsUsed.getNumGroupsYUsed())
        uniformsSet.emplace_back("numY");
    if(uniformsUsed.getNumGroupsZUsed())
        uniformsSet.emplace_back("numZ");
    if(uniformsUsed.getGroupIDXUsed())
        uniformsSet.emplace_back("gidX");
    if(uniformsUsed.getGroupIDYUsed())
        uniformsSet.emplace_back("gidY");
    if(uniformsUsed.getGroupIDZUsed())
        uniformsSet.emplace_back("gidZ");
    if(uniformsUsed.getGlobalOffsetXUsed())
        uniformsSet.emplace_back("offX");
    if(uniformsUsed.getGlobalOffsetYUsed())
        uniformsSet.emplace_back("offY");
    if(uniformsUsed.getGlobalOffsetZUsed())
        uniformsSet.emplace_back("offZ");
    if(uniformsUsed.getGlobalDataAddressUsed())
        uniformsSet.emplace_back("global");
    if(uniformsUsed.getUniformAddressUsed())
        uniformsSet.emplace_back("unifAddr");
    if(uniformsUsed.getMaxGroupIDXUsed())
        uniformsSet.emplace_back("maxGidX");
    if(uniformsUsed.getMaxGroupIDYUsed())
        uniformsSet.emplace_back("maxGidY");
    if(uniformsUsed.getMaxGroupIDZUsed())
        uniformsSet.emplace_back("maxGidZ");
    const std::string uniformsString =
        uniformsSet.empty() ? "" : (std::string(" (") + vc4c::to_string<std::string>(uniformsSet) + ")");

    return std::string("Kernel '") + (name + "' with ") +
        (std::to_string(getLength().getValue()) + " instructions, offset ") +
        (std::to_string(getOffset().getValue()) + ", with following parameters: ") +
        ::to_string<ParamInfo>(parameters) + uniformsString;
}
LCOV_EXCL_STOP

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

std::size_t ModuleInfo::write(
    std::ostream& stream, const OutputMode mode, const StableList<Global>& globalData, Byte totalStackFrameSize)
{
    std::size_t numWords = 0;
    if(mode == OutputMode::HEX || mode == OutputMode::ASSEMBLER)
    {
        stream << "// Module with " << getInfoCount() << " kernels, global data with " << getGlobalDataSize()
               << " words (64-bit each), starting at offset " << getGlobalDataOffset() << " words and "
               << getStackFrameSize() << " words of stack-frame" << std::endl;
    }
    std::array<uint8_t, 8> buf{};
    if(mode == OutputMode::BINARY || mode == OutputMode::HEX)
    {
        // write magic number
        reinterpret_cast<uint32_t*>(buf.data())[0] = QPUASM_MAGIC_NUMBER;
        reinterpret_cast<uint32_t*>(buf.data())[1] = QPUASM_MAGIC_NUMBER;
        writeStream(stream, buf, mode);
        ++numWords;

        // write module info
        *reinterpret_cast<uint64_t*>(buf.data()) = value;
        writeStream(stream, buf, mode);
        ++numWords;
    }
    // write kernel-infos
    for(const KernelInfo& info : kernelInfos)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << info.to_string() << logging::endl);
        numWords += info.write(stream, mode);
    }
    // write kernel-info-to-global-data delimiter
    buf.fill(0);
    writeStream(stream, buf, mode);
    ++numWords;

    // update global data offset
    setGlobalDataOffset(Word(numWords));

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
    setGlobalDataSize(Word(numWords) - getGlobalDataOffset());

    // write global-data-to-kernel-instructions delimiter
    buf.fill(0);
    writeStream(stream, buf, mode);
    ++numWords;

    return numWords;
}

KernelInfo qpu_asm::getKernelInfos(
    const Method& method, const std::size_t initialOffset, const std::size_t numInstructions)
{
    KernelInfo info(method.parameters.size());
    info.setOffset(Word(initialOffset));
    info.setLength(Word(numInstructions));
    info.setName(method.name[0] == '@' ? method.name.substr(1) : method.name);
    info.workGroupSize = 0;
    info.uniformsUsed = method.metaData.uniformsUsed;
    {
        size_t requiredSize = 1;
        unsigned offset = 0;
        for(uint32_t size : method.metaData.workGroupSizes)
        {
            info.workGroupSize |= static_cast<uint64_t>(size) << offset;
            offset += 16;
            requiredSize *= size;
        }
        if(requiredSize > KernelInfo::MAX_WORK_GROUP_SIZES)
        {
            logging::error() << "Required work-group size " << requiredSize << " exceeds the limit of "
                             << KernelInfo::MAX_WORK_GROUP_SIZES << logging::endl;
        }
    }
    {
        uint32_t requiredSize = std::accumulate(method.metaData.workGroupSizeHints.begin(),
            method.metaData.workGroupSizeHints.end(), 1u, std::multiplies<uint32_t>());
        if(requiredSize > KernelInfo::MAX_WORK_GROUP_SIZES)
        {
            logging::warn() << "Work-group size hint " << requiredSize << " exceeds the limit of "
                            << KernelInfo::MAX_WORK_GROUP_SIZES << logging::endl;
        }
    }
    for(const Parameter& param : method.parameters)
    {
        std::string paramName = param.parameterName;
        paramName = paramName.empty() ? param.name : paramName;
        auto paramType = param.type;
        std::string typeName = param.origTypeName;
        ParamInfo paramInfo;
        paramInfo.setSize(static_cast<uint16_t>(paramType.getInMemoryWidth()));
        paramInfo.setPointer(paramType.getPointerType() || paramType.getImageType());
        paramInfo.setImage(!!paramType.getImageType());
        paramInfo.setDecorations(param.decorations);
        paramInfo.setName(paramName[0] == '%' ? paramName.substr(1) : paramName);
        paramInfo.setVectorElements(
            (paramType.getPointerType() ? static_cast<uint8_t>(1) : paramType.getVectorWidth()));
        paramInfo.setAddressSpace(paramType.getPointerType() ?
                paramType.getPointerType()->addressSpace :
                paramType.getImageType() ? AddressSpace::GLOBAL : AddressSpace::PRIVATE);
        paramInfo.setFloatingType(paramType.isFloatingType());
        // FIXME signedness is only recognized correctly for non-32 bit scalar types (e.g. (u)char, (u)short), not for
        // pointers or even vector-types
        paramInfo.setSigned(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND));
        paramInfo.setUnsigned(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND));
        paramInfo.setTypeName(
            typeName.empty() ? paramType.getTypeName(paramInfo.getSigned(), paramInfo.getUnsigned()) : typeName);
        paramInfo.setLowered(param.isLowered);

        if(paramType.getPointerType() && has_flag(param.decorations, ParameterDecorations::BY_VALUE))
        {
            // since the client passes the actual (struct) type to as argument, the VC4CL run-time needs to know that
            // size
            paramInfo.setSize(static_cast<uint16_t>(paramType.getPointerType()->elementType.getInMemoryWidth()));
            // we also need to fix-up the other decorations and qualifiers we modified:
            // direct struct parameters are always in the __private address space (since they are function-local
            // values), we just "moved" them to the __constant address space for a) better optimization and b) kernels
            // do not allow __private parameters
            paramInfo.setAddressSpace(AddressSpace::PRIVATE);
        }
        info.addParameter(paramInfo);
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

    return info;
}
