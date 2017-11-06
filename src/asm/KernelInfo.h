/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef KERNELINFO_H
#define KERNELINFO_H

#include <vector>
#include <iostream>

#include "../Module.h"
#include "config.h"

namespace vc4c
{

	namespace qpu_asm
	{

		class KernelInfo;

		class ParamInfo : public Bitfield<uint64_t>
		{
		public:
			//size of the parameter in bytes
			BITFIELD_ENTRY(Size, uint8_t, 0, Byte)
			BITFIELD_ENTRY(Elements, uint8_t, 8, Byte)
			BITFIELD_ENTRY(Constant, bool, 48, Bit)
			BITFIELD_ENTRY(Restricted, bool, 49, Bit)
			BITFIELD_ENTRY(Volatile, bool, 50, Bit)
			BITFIELD_ENTRY(AddressSpace, AddressSpace, 52, Quadruple)
			BITFIELD_ENTRY(Input, bool, 56, Bit)
			BITFIELD_ENTRY(Output, bool, 57, Bit)
			BITFIELD_ENTRY(Pointer, bool, 60, Bit)

			inline void setName(const std::string& name)
			{
				this->name = name;
				setNameLength(name.size());
			}

			inline void setTypeName(const std::string& name)
			{
				typeName = name;
				setTypeNameLength(name.size());
			}

			std::string to_string() const;
		private:
			BITFIELD_ENTRY(NameLength, uint16_t, 16, Short)
			BITFIELD_ENTRY(TypeNameLength, uint16_t, 32, Short)
			std::string name;
			std::string typeName;

			friend class KernelInfo;
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
		class KernelInfo : private Bitfield<uint64_t>
		{
		public:

			KernelInfo(const std::size_t& numParameters);

			//offset in multiple of 64-bit
			BITFIELD_ENTRY(Offset, uint16_t, 0, Short)
			//number of 64-bit instructions
			BITFIELD_ENTRY(Length, uint16_t, 16, Short)
			//the 3 dimensions for the work-group size specified in the source code
			uint64_t workGroupSize;

			uint8_t write(std::ostream& stream, const OutputMode mode) const;
			std::string to_string() const;

			//The maximum work group sizes specified in the VC4CL runtime library
			static constexpr uint32_t MAX_WORK_GROUP_SIZES = 12;

			inline void setName(const std::string& name)
			{
				this->name = name;
				setNameLength(name.size());
			}

			inline void addParameter(const ParamInfo& param)
			{
				setParamCount(getParamCount() + 1);
				parameters.push_back(param);
			}

		private:
			BITFIELD_ENTRY(NameLength, uint16_t, 32, Short)
			BITFIELD_ENTRY(ParamCount, uint16_t, 48, Short)
			std::string name;
			std::vector<ParamInfo> parameters;
		};

		KernelInfo getKernelInfos(const Method& method, const std::size_t initialOffset, const std::size_t numInstructions);
		void writeKernelInfos(const std::vector<KernelInfo>& info, std::ostream& output, const OutputMode mode);
	}
}

#endif /* KERNELINFO_H */

