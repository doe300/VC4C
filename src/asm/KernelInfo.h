/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef KERNELINFO_H
#define KERNELINFO_H

#include "../Bitfield.h"
#include "config.h"
#include "../performance.h"

#include <iostream>
#include <vector>

namespace vc4c
{
	enum class AddressSpace;
	class Method;
	struct Global;

	namespace qpu_asm
	{

		class ParamInfo : public Bitfield<uint64_t>
		{
		public:
			//size of the parameter in bytes
			BITFIELD_ENTRY(Size, uint8_t, 0, Byte)
			BITFIELD_ENTRY(Elements, uint8_t, 8, Byte)
			BITFIELD_ENTRY(NameLength, uint16_t, 16, Short)
			BITFIELD_ENTRY(TypeNameLength, uint16_t, 32, Short)
			BITFIELD_ENTRY(Constant, bool, 48, Bit)
			BITFIELD_ENTRY(Restricted, bool, 49, Bit)
			BITFIELD_ENTRY(Volatile, bool, 50, Bit)
			BITFIELD_ENTRY(AddressSpace, AddressSpace, 52, Quadruple)
			BITFIELD_ENTRY(Input, bool, 56, Bit)
			BITFIELD_ENTRY(Output, bool, 57, Bit)
			BITFIELD_ENTRY(Pointer, bool, 60, Bit)
			BITFIELD_ENTRY(FloatingType, bool, 61, Bit)
			BITFIELD_ENTRY(Signed, bool, 62, Bit)
			BITFIELD_ENTRY(Unsigned, bool, 63, Bit)

			inline void setName(const std::string& name)
			{
				this->name = name;
				setNameLength(static_cast<uint16_t>(name.size()));
			}

			inline void setTypeName(const std::string& name)
			{
				typeName = name;
				setTypeNameLength(static_cast<uint16_t>(name.size()));
			}

			std::string to_string() const;

			uint16_t write(std::ostream& stream, OutputMode mode) const;

			std::string name;
			std::string typeName;
		};

		/*
		 * Binary layout:
		 *
		 * | offset | length | name-length | parameter count |
		 * | work-group size compilation hint                |
		 * | name ...
		 *   ...                                             |
		 *
		 */
		class KernelInfo : public Bitfield<uint64_t>
		{
		public:

			explicit KernelInfo(const std::size_t& numParameters);

			//offset in multiple of 64-bit
			BITFIELD_ENTRY(Offset, uint16_t, 0, Short)
			//number of 64-bit instructions
			BITFIELD_ENTRY(Length, uint16_t, 16, Short)
			//the length of the kernel-name (in bytes) excluding padding bytes. NOTE: Do not set this value manually!
			BITFIELD_ENTRY(NameLength, uint16_t, 32, Short)
			//the number of parameters. NOTE: Do not set this value manually!
			BITFIELD_ENTRY(ParamCount, uint16_t, 48, Short)
			//the 3 dimensions for the work-group size specified in the source code
			uint64_t workGroupSize;

			uint16_t write(std::ostream& stream, OutputMode mode) const;
			std::string to_string() const;

			//The maximum work group sizes specified in the VC4CL runtime library
			static constexpr uint32_t MAX_WORK_GROUP_SIZES = 12;

			inline void setName(const std::string& name)
			{
				this->name = name;
				setNameLength(static_cast<uint16_t>(name.size()));
			}

			inline void addParameter(const ParamInfo& param)
			{
				parameters.push_back(param);
				setParamCount(static_cast<uint16_t>(parameters.size()));
			}


			std::string name;
			std::vector<ParamInfo> parameters;
		};

		/*
		 * Binary layout:
		 *
		 * | num kernel-infos | global-data offset | global-data size | stack-frame size |
		 */
		class ModuleInfo : public Bitfield<uint64_t>
		{
		public:
			//the number of kernel-infos in this module. NOTE: Do not set this number manually!
			BITFIELD_ENTRY(InfoCount, uint16_t, 0, Short)
			//offset of global-data in multiples of 64-bit
			BITFIELD_ENTRY(GlobalDataOffset, uint16_t, 16, Short)
			//size of the global data segment in multiples of 64-bit
			BITFIELD_ENTRY(GlobalDataSize, uint16_t, 32, Short)
			//size of a single stack-frame, appended to the global-data segment. In multiples of 64-bit
			BITFIELD_ENTRY(StackFrameSize, uint16_t, 48, Short)

			/*
			 * NOTE: Writing once sets the global-data offset and size, so they are correct for the second write
			 */
			uint16_t write(std::ostream& stream, OutputMode mode, const ReferenceRetainingList<Global>& globalData);

			inline void addKernelInfo(const KernelInfo& info)
			{
				kernelInfos.push_back(info);
				setInfoCount(static_cast<uint16_t>(kernelInfos.size()));
			}

			std::vector<KernelInfo> kernelInfos;
		};

		KernelInfo getKernelInfos(const Method& method, uint16_t initialOffset, uint16_t numInstructions);
	} // namespace qpu_asm
} // namespace vc4c

#endif /* KERNELINFO_H */

