/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef KERNELINFO_H
#define KERNELINFO_H

#include "../Bitfield.h"
#include "../KernelMetaData.h"
#include "../Units.h"
#include "../performance.h"
#include "config.h"

#include <iostream>
#include <vector>

namespace vc4c
{
    enum class AddressSpace : unsigned char;
    class Method;
    struct Global;
    enum class ParameterDecorations : unsigned char;

    namespace qpu_asm
    {
        class ParamInfo : public Bitfield<uint64_t>
        {
        public:
            /*
             * The size of the parameter in bytes
             *
             * Type considerations: The maximum vector type-size is int16 (long16), which has 64(128) bytes, minimum a
             * size of 1 byte (char). 13 bits fit also structure parameters up to 8 KB.
             */
            BITFIELD_ENTRY(Size, uint16_t, 0, Tredecuple)
            /*
             * The number of vector-elements for the parameter
             *
             * Type considerations: 1 to 16 elements are supported, fits in 5 bits
             */
            BITFIELD_ENTRY(VectorElements, uint8_t, 13, Quintuple)
            /*
             * The length of the parameter name in characters
             *
             * Type considerations: byte can contain 255 characters, ushort 64k, 2^12 4096
             */
            BITFIELD_ENTRY(NameLength, Byte, 18, Duodecuple)
            /*
             * The length of the parameter type-name in characters
             *
             * Type considerations: byte can contain 255 characters, ushort 64k, 2^12 4096
             */
            BITFIELD_ENTRY(TypeNameLength, Byte, 30, Duodecuple)
            /*
             * Whether this parameter is constant (read-only), only useful for pointers/images
             */
            BITFIELD_ENTRY(Decorations, ParameterDecorations, 42, Decuple)

            //// 2 Bits unused

            /*
             * Whether this parameter lowered into shared VPM memory and therefore no temporary buffers needs to
             * be allocated for it.
             *
             * NOTE: This is only valid for __local parameters where the VC4C compiler can deduce the maximum accessed
             * range.
             */
            BITFIELD_ENTRY(Lowered, bool, 54, Bit)

            /*
             * The address space for this parameter
             *
             * Type considerations: There are 5 address-spaces, fit into 4 bits
             */
            BITFIELD_ENTRY(AddressSpace, AddressSpace, 55, Quadruple)
            /*
             * Whether this parameter is an image
             */
            BITFIELD_ENTRY(Image, bool, 59, Bit)
            /*
             * Whether the parameter is a pointer-type
             */
            BITFIELD_ENTRY(Pointer, bool, 60, Bit)
            /*
             * Whether the parameter is a floating-point type
             */
            BITFIELD_ENTRY(FloatingType, bool, 61, Bit)
            /*
             * Whether this parameter is known to be a signed integer type
             */
            BITFIELD_ENTRY(Signed, bool, 62, Bit)
            /*
             * Whether this parameter is known to be an unsigned integer type
             */
            BITFIELD_ENTRY(Unsigned, bool, 63, Bit)

            inline void setName(const std::string& name)
            {
                this->name = name;
                setNameLength(Byte(name.size()));
            }

            inline void setTypeName(const std::string& name)
            {
                typeName = name;
                setTypeNameLength(Byte(name.size()));
            }

            std::string to_string() const;

            std::size_t write(std::ostream& stream, OutputMode mode) const;

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

            /*
             * The offset in multiple of 64-bit from the start of the module
             *
             * Type considerations: ushort supports a maximum kernel offset (of the last kernel) of 512 kB (64k * 8
             * byte), 24-bit integer up to 128 MB
             */
            BITFIELD_ENTRY(Offset, Word, 0, Quattuorvigintuple)
            /*
             * The number of 64-bit instructions
             *
             * Type considerations: ushort supports a maximum kernel size of 64k instructions (512 kB), 22-bit integer
             * up to 32 MB
             */
            BITFIELD_ENTRY(Length, Word, 24, Duovigintuple)
            /*
             * The length of the kernel-name (in bytes) excluding padding bytes. NOTE: Do not set this value manually!
             *
             * Type considerations: byte can contain 255 characters, decuple up to 1023, ushort 64k
             */
            BITFIELD_ENTRY(NameLength, Byte, 46, Decuple)
            /*
             * The number of parameters. NOTE: Do not set this value manually!
             *
             * Type considerations: Since we read all parameters at the start, only ~70 parameters (number of registers)
             * are supported
             */
            BITFIELD_ENTRY(ParamCount, uint8_t, 56, Byte)
            /*
             * The 3 dimensions for the work-group size specified in the source code
             */
            uint64_t workGroupSize;
            /*
             * Bit-field determining the implicit UNIFORMs used by this kernel. Depending on this field, the
             * UNIFORM-values are created host-side
             */
            KernelUniforms uniformsUsed;

            std::size_t write(std::ostream& stream, OutputMode mode) const;
            std::string to_string() const;

            // The maximum work group sizes specified in the VC4CL runtime library
            static constexpr uint32_t MAX_WORK_GROUP_SIZES = NUM_QPUS;

            inline void setName(const std::string& name)
            {
                this->name = name;
                setNameLength(Byte(name.size()));
            }

            inline void addParameter(const ParamInfo& param)
            {
                parameters.push_back(param);
                setParamCount(static_cast<uint8_t>(parameters.size()));
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
            /*
             * The number of kernel-infos in this module. NOTE: Do not set this number manually!
             *
             * Type considerations: byte supports 255 kernels, decuple 1024 and ushort up to 64k
             */
            BITFIELD_ENTRY(InfoCount, uint16_t, 0, Decuple)
            /*
             * The offset of global-data in multiples of 64-bit from the start of the compilation unit
             *
             * Type considerations: ushort supports an offset of 512 kB, vigintuple (20 bits) up to 8 MB and 24-bit
             * integer up to 128 MB
             */
            BITFIELD_ENTRY(GlobalDataOffset, Word, 10, Short)
            /*
             * The size of the global data segment in multiples of 64-bit
             *
             * Type considerations: ushort supports 512 kB of global data, vigintuple (20 bits) up to 8 MB and 24-bit
             * integer up to 128 MB
             */
            BITFIELD_ENTRY(GlobalDataSize, Word, 26, Vigintuple)
            /*
             * The size of a single stack-frame, appended to the global-data segment. In multiples of 64-bit
             *
             * Type considerations: ushort supports 512 kB of stack-frame, vigintuple (20 bits) up to 8 MB and 24-bit
             * integer up to 128 MB
             */
            BITFIELD_ENTRY(StackFrameSize, Word, 46, Short)

            /*
             * NOTE: Writing once sets the global-data offset and size, so they are correct for the second write
             */
            std::size_t write(
                std::ostream& stream, OutputMode mode, const StableList<Global>& globalData, Byte totalStackFrameSize);

            inline void addKernelInfo(const KernelInfo& info)
            {
                kernelInfos.push_back(info);
                setInfoCount(static_cast<uint16_t>(kernelInfos.size()));
            }

            std::vector<KernelInfo> kernelInfos;
        };

        KernelInfo getKernelInfos(const Method& method, std::size_t initialOffset, std::size_t numInstructions);
    } // namespace qpu_asm
} // namespace vc4c

#endif /* KERNELINFO_H */
