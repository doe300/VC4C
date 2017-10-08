/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <string.h>

#include "KernelInfo.h"
#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

static void writeStream(std::ostream& stream, uint8_t buf[8], const OutputMode mode)
{
    if(mode == OutputMode::BINARY)
    {
        stream.write((const char*)buf, 8);
    }
    else if(mode == OutputMode::HEX)
    {
        uint64_t binary = *(uint64_t*)buf;
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "0x%08x, 0x%08x, ", static_cast<uint32_t>(binary & 0xFFFFFFFFLL), static_cast<uint32_t>((binary & 0xFFFFFFFF00000000LL) >> 32));
        stream << std::string(buffer) << std::endl;
    }
}

static uint8_t copyName(std::ostream& stream, const std::string& name, const OutputMode mode, std::size_t bytesInFirstBlock = 8)
{
    uint8_t buf[8];
    uint8_t numWords = 0;
    for(std::size_t i = 0; i < name.size(); i += 8)
    {
        const std::size_t bytesInBlock = i == 0 ? bytesInFirstBlock : 8;
        std::size_t l = std::min(name.size() - i, bytesInBlock);
        //copy name in multiples of 8 byte
        memcpy(buf, name.data() + i, l);
        //pad with zeroes
        memset(buf + l, '\0', bytesInBlock - l);
        writeStream(stream, buf, mode);
        ++numWords;
    }
    return numWords;
}

std::string ParamInfo::to_string() const
{
	//address space
	return std::string((isPointer && addressSpace == AddressSpace::CONSTANT) ? "__constant " : "") + std::string((isPointer && addressSpace == AddressSpace::GLOBAL) ? "__global " : "") +
			std::string((isPointer && addressSpace == AddressSpace::LOCAL) ? "__local " : "") + std::string((isPointer && addressSpace == AddressSpace::PRIVATE) ? "__private " : "") +
			//access qualifier
			std::string((isPointer && isConst) ? "const " : "") + std::string((isPointer && isRestricted) ? "restrict " : "") + std::string((isPointer && isVolatile) ? "volatile " : "") +
			//input/output
			((isPointer && isInput) ? "in " : "") + ((isPointer && isOutput) ? "out " : "") +
			//type + name
			((typeName) + " ") + (name + " (") + (std::to_string(size) + " B, ") + std::to_string(elements) + " items)";
}

uint8_t KernelInfo::write(std::ostream& stream, const OutputMode mode) const
{
    std::size_t numWords = 0;
    if(mode == OutputMode::HEX || mode == OutputMode::ASSEMBLER)
	{
		const std::string s = to_string();
		stream << "// " << s << std::endl;
	}
    if(mode == OutputMode::BINARY || mode == OutputMode::HEX)
    {
        uint8_t buf[8];
        ((uint16_t*)buf)[0] = offset;
        ((uint16_t*)buf)[1] = length;
        ((uint16_t*)buf)[2] = name.size();
        ((uint16_t*)buf)[3] = parameters.size();
        writeStream(stream, buf, mode);
        ++numWords;
        *((uint64_t*)buf) = workGroupSize;
        writeStream(stream, buf, mode);
        ++numWords;
        numWords += copyName(stream, name, mode);
        for(uint16_t i = 0; i < parameters.size(); ++i)
        {
            //for each parameter, copy infos and name
            ((uint16_t*)buf)[0] = parameters[i].elements << 8 | parameters[i].size;
            ((uint16_t*)buf)[1] = parameters[i].name.size();
            ((uint16_t*)buf)[2] = parameters[i].typeName.size();
            ((uint16_t*)buf)[3] = parameters[i].isPointer << 12 | parameters[i].isOutput << 9 | parameters[i].isInput << 8 | (static_cast<unsigned char>(parameters[i].addressSpace) & 0xF) << 4 | parameters[i].isConst | parameters[i].isRestricted << 1 | parameters[i].isVolatile << 2;
            writeStream(stream, buf, mode);
            ++numWords;
            numWords += copyName(stream, parameters[i].name, mode);
            numWords += copyName(stream, parameters[i].typeName, mode);
        }
    }
    return numWords;
}

std::string KernelInfo::to_string() const
{
	return std::string("Kernel '") + (name + "', offset ") + (std::to_string(offset) + ", with following parameters: ") + ::to_string<ParamInfo>(parameters);
}

static std::string getMetaData(const std::map<MetaDataType, std::vector<std::string>>& metaData, const MetaDataType type, const std::size_t index)
{
    if(metaData.find(type) == metaData.end())
        return "";
    if(metaData.at(type).size() <= index)
        return "";
    return metaData.at(type).at(index);
}

KernelInfo qpu_asm::getKernelInfos(const Method& method, const std::size_t initialOffset, const std::size_t numInstructions)
{
    KernelInfo info;
    info.offset = initialOffset;
    info.length = numInstructions;
    info.name = method.name[0] == '@' ? method.name.substr(1) : method.name;
    info.parameters.reserve(method.parameters.size());
    info.workGroupSize = 0;
    if(method.metaData.find(MetaDataType::WORK_GROUP_SIZES) != method.metaData.end())
    {
        uint32_t requiredSize = 1;
        unsigned char offset = 0;
        for(const std::string& s : method.metaData.at(MetaDataType::WORK_GROUP_SIZES))
        {
            int size = std::atoi(s.data());
            info.workGroupSize |= size << offset;
            offset += 16;
            requiredSize *= size;
        }
        if(requiredSize > KernelInfo::MAX_WORK_GROUP_SIZES)
        {
            logging::error() << "Required work-group size " << requiredSize << " exceeds the limit of " << KernelInfo::MAX_WORK_GROUP_SIZES << logging::endl;
        }
    }
    if(method.metaData.find(MetaDataType::WORK_GROUP_SIZES_HINT) != method.metaData.end())
    {
        uint32_t requiredSize = 1;
        for(const std::string& s : method.metaData.at(MetaDataType::WORK_GROUP_SIZES_HINT))
        {
            requiredSize *= std::atoi(s.data());
        }
        if(requiredSize > KernelInfo::MAX_WORK_GROUP_SIZES)
        {
            logging::warn() << "Work-group size hint " << requiredSize << " exceeds the limit of " << KernelInfo::MAX_WORK_GROUP_SIZES << logging::endl;
        }
    }
    for(std::size_t i = 0; i < method.parameters.size(); ++i)
    {
    	std::string paramName = getMetaData(method.metaData, MetaDataType::ARG_NAMES, i);
    	paramName = paramName.empty() ? method.parameters.at(i).parameterName : paramName;
    	paramName = paramName.empty() ? method.parameters.at(i).name : paramName;
        const DataType& paramType = method.parameters.at(i).type;
        std::string typeName = getMetaData(method.metaData, MetaDataType::ARG_TYPE_NAMES, i);
        info.parameters.push_back(ParamInfo{static_cast<uint8_t>(paramType.getPhysicalWidth()), paramType.isPointerType() || paramType.getImageType().hasValue,
                method.parameters.at(i).isOutputParameter(), method.parameters.at(i).isInputParameter(),
                getMetaData(method.metaData, MetaDataType::ARG_TYPE_QUALIFIERS, i).find("const") != std::string::npos || has_flag(method.parameters.at(i).decorations, ParameterDecorations::READ_ONLY),
				getMetaData(method.metaData, MetaDataType::ARG_TYPE_QUALIFIERS, i).find("restrict") != std::string::npos || has_flag(method.parameters.at(i).decorations, ParameterDecorations::RESTRICT),
				getMetaData(method.metaData, MetaDataType::ARG_TYPE_QUALIFIERS, i).find("volatile") != std::string::npos || has_flag(method.parameters.at(i).decorations, ParameterDecorations::VOLATILE),
				paramName[0] == '%' ? paramName.substr(1) : paramName,
                typeName.empty() ? paramType.to_string() : typeName,
				(paramType.isPointerType() ? (uint8_t)1 : paramType.num),
				paramType.isPointerType() ? paramType.getPointerType().get()->addressSpace : AddressSpace::PRIVATE
        });
    }
    
    return info;
}

void qpu_asm::writeKernelInfos(const std::vector<KernelInfo>& info, std::ostream& output, const OutputMode mode)
{
    logging::debug() << "Writing kernel infos..." << logging::endl;
    for(const KernelInfo& item : info)
    {
        logging::debug() << item.to_string() << logging::endl;
        item.write(output, mode);
    }
}
