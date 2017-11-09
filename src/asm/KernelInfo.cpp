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
        stream.write(reinterpret_cast<const char*>(buf), 8);
    }
    else if(mode == OutputMode::HEX)
    {
        uint64_t binary = *reinterpret_cast<uint64_t*>(buf);
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
	return std::string((getPointer() && getAddressSpace() == AddressSpace::CONSTANT) ? "__constant " : "") + std::string((getPointer() && getAddressSpace() == AddressSpace::GLOBAL) ? "__global " : "") +
			std::string((getPointer() && getAddressSpace() == AddressSpace::LOCAL) ? "__local " : "") + std::string((getPointer() && getAddressSpace() == AddressSpace::PRIVATE) ? "__private " : "") +
			//access qualifier
			std::string((getPointer() && getConstant()) ? "const " : "") + std::string((getPointer() && getRestricted()) ? "restrict " : "") + std::string((getPointer() && getVolatile()) ? "volatile " : "") +
			//input/output
			((getPointer() && getInput()) ? "in " : "") + ((getPointer() && getOutput()) ? "out " : "") +
			//type + name
			((typeName) + " ") + (name + " (") + (std::to_string(getSize()) + " B, ") + std::to_string(getElements()) + " items)";
}

KernelInfo::KernelInfo(const std::size_t& numParameters) : Bitfield(0), workGroupSize(0)
{
	parameters.reserve(numParameters);
}

uint8_t KernelInfo::write(std::ostream& stream, const OutputMode mode) const
{
    uint8_t numWords = 0;
    if(mode == OutputMode::HEX || mode == OutputMode::ASSEMBLER)
	{
		const std::string s = to_string();
		stream << "// " << s << std::endl;
	}
    if(mode == OutputMode::BINARY || mode == OutputMode::HEX)
    {
        uint8_t buf[8];
        *reinterpret_cast<uint64_t*>(buf) = value;
        writeStream(stream, buf, mode);
        ++numWords;
        *reinterpret_cast<uint64_t*>(buf) = workGroupSize;
        writeStream(stream, buf, mode);
        ++numWords;
        numWords += copyName(stream, name, mode);
        for(uint16_t i = 0; i < parameters.size(); ++i)
        {
            //for each parameter, copy infos and name
        	*reinterpret_cast<uint64_t*>(buf) = parameters[i].value;
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
	return std::string("Kernel '") + (name + "', offset ") + (std::to_string(getOffset()) + ", with following parameters: ") + ::to_string<ParamInfo>(parameters);
}

static std::string getMetaData(const std::map<MetaDataType, std::vector<std::string>>& metaData, const MetaDataType type, const std::size_t index)
{
    if(metaData.find(type) == metaData.end())
        return "";
    if(metaData.at(type).size() <= index)
        return "";
    return metaData.at(type).at(index);
}

KernelInfo qpu_asm::getKernelInfos(const Method& method, const uint16_t initialOffset, const uint16_t numInstructions)
{
    KernelInfo info(method.parameters.size());
    info.setOffset(initialOffset);
    info.setLength(numInstructions);
    info.setName(method.name[0] == '@' ? method.name.substr(1) : method.name);
    info.workGroupSize = 0;
    if(method.metaData.find(MetaDataType::WORK_GROUP_SIZES) != method.metaData.end())
    {
        size_t requiredSize = 1;
        unsigned offset = 0;
        for(const std::string& s : method.metaData.at(MetaDataType::WORK_GROUP_SIZES))
        {
            int64_t size = std::atoi(s.data());
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
        ParamInfo paramInfo;
        paramInfo.setSize(static_cast<uint8_t>(paramType.getPhysicalWidth()));
        paramInfo.setPointer(paramType.isPointerType() || paramType.getImageType().hasValue);
        paramInfo.setOutput(method.parameters.at(i).isOutputParameter());
        paramInfo.setInput(method.parameters.at(i).isInputParameter());
        paramInfo.setConstant(getMetaData(method.metaData, MetaDataType::ARG_TYPE_QUALIFIERS, i).find("const") != std::string::npos || has_flag(method.parameters.at(i).decorations, ParameterDecorations::READ_ONLY));
        paramInfo.setRestricted(getMetaData(method.metaData, MetaDataType::ARG_TYPE_QUALIFIERS, i).find("restrict") != std::string::npos || has_flag(method.parameters.at(i).decorations, ParameterDecorations::RESTRICT));
        paramInfo.setVolatile(getMetaData(method.metaData, MetaDataType::ARG_TYPE_QUALIFIERS, i).find("volatile") != std::string::npos || has_flag(method.parameters.at(i).decorations, ParameterDecorations::VOLATILE));
        paramInfo.setName(paramName[0] == '%' ? paramName.substr(1) : paramName);
        paramInfo.setTypeName(typeName.empty() ? paramType.to_string() : typeName);
        paramInfo.setElements((paramType.isPointerType() ? static_cast<uint8_t>(1) : paramType.num));
        paramInfo.setAddressSpace(paramType.isPointerType() ? paramType.getPointerType().get()->addressSpace : AddressSpace::PRIVATE);
        info.addParameter(paramInfo);
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
