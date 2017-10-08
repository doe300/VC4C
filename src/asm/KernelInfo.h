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

		struct ParamInfo
		{
			//size of the parameter in bytes
			uint8_t size;
			bool isPointer;
			bool isOutput;
			bool isInput;
			bool isConst;
			bool isRestricted;
			bool isVolatile;
			std::string name;
			std::string typeName;
			uint8_t elements;
			AddressSpace addressSpace;

			std::string to_string() const;
		};

		struct KernelInfo
		{
			//offset in multiple of 64-bit
			uint16_t offset;
			//number of 64-bit instructions
			uint16_t length;
			std::string name;
			std::vector<ParamInfo> parameters;
			//the 3 dimensions for the work-group size specified in the source code
			uint64_t workGroupSize;

			uint8_t write(std::ostream& stream, const OutputMode mode) const;
			std::string to_string() const;

			//The maximum work group sizes specified in the VC4CL runtime library
			static constexpr uint32_t MAX_WORK_GROUP_SIZES = 12;
		};

		KernelInfo getKernelInfos(const Method& method, const std::size_t initialOffset, const std::size_t numInstructions);
		void writeKernelInfos(const std::vector<KernelInfo>& info, std::ostream& output, const OutputMode mode);
	}
}

#endif /* KERNELINFO_H */

