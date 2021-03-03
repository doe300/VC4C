/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef IMAGES_H
#define IMAGES_H

#include "../InstructionWalker.h"

#include <cstdint>

namespace vc4c
{
    namespace intermediate
    {
        /*
         * Constants are taken form CLang opencl-c.h
         *
         * !!These values need to match the runtime/kernel equivalent!!
         *
         * See OpenCL 1.2 specification, table 6.22
         */
        enum class AddressingMode : unsigned char
        {
            /*
             * "for this addressing mode the programmer guarantees that the image coordinates used to sample elements of
             * the image refer to a location inside the image; otherwise the results are undefined."
             */
            NONE = 0,
            /*
             * "out-of-range image coordinates are clamped to the extent."
             */
            CLAMP_TO_EDGE = 1,
            /*
             * "out-of-range image coordinates will return a border color."
             */
            CLAMP = 2,
            /*
             * "out-of-range image coordinates are wrapped to the valid range. This addressing mode can only be used
             * with normalized coordinates. If normalized coordinates are not used, this addressing mode may generate
             * image coordinates that are undefined."
             */
            REPEAT = 3,
            /*
             * "Flip the image coordinate at every integer junction. This addressing mode can only be used with
             * normalized coordinates. If normalized coordinates are not used, this addressing mode may generate image
             * coordinates that are undefined."
             */
            MIRRORED_REPEAT = 4
        };

        /*
         * Constants are taken form CLang opencl-c.h
         *
         * !!These values need to match the runtime/kernel equivalent!!
         *
         * See OpenCL 1.2 specification, pages 329+
         */
        enum class FilterMode : unsigned char
        {
            /*
             * "When filter mode is CLK_FILTER_NEAREST , the image element in the image that is nearest (in Manhattan
             * distance) to that specified by (u,v,w) is obtained."
             */
            NEAREST = 1,
            /*
             * "When filter mode is CLK_FILTER_LINEAR , a 2 x 2 square of image elements for a 2D image or a 2 x 2 x 2
             * cube of image elements for a 3D image is selected."
             */
            LINEAR = 2
        };

        struct Sampler : private Bitfield<uint8_t>
        {
            static constexpr uint32_t MASK_NORMALIZED_COORDS{0x1};
            static constexpr uint32_t MASK_ADDRESSING_MODE{0xE};
            static constexpr uint32_t MASK_FILTER_MODE{0x30};

            explicit constexpr Sampler(uint8_t val) : Bitfield(val) {}

            Sampler(AddressingMode addressingMode, bool normalizedCoords, FilterMode filterMode) : Bitfield(0)
            {
                setAddressingMode(addressingMode);
                setNormalizedCoordinates(normalizedCoords);
                setFilterMode(filterMode);
            }

            BITFIELD_ENTRY(AddressingMode, AddressingMode, 1, Triple)
            BITFIELD_ENTRY(NormalizedCoordinates, bool, 0, Bit)
            BITFIELD_ENTRY(FilterMode, FilterMode, 4, Tuple)

            constexpr operator unsigned char() const
            {
                return value;
            }
        };

        /*
         * Prepares a segment of the global data to be used as buffer for the image-configuration for this image
         *
         * On setting the image as kernel-parameter, the host implementation writes the image-configuration into this
         * buffer.
         *
         * Returns the global the buffer is allocated at, to be used to set the UNIFORM pointer as well as to be set
         * into the parameter-info.
         */
        Global* reserveImageConfiguration(Module& module, Parameter& image);

        bool intrinsifyImageFunction(InstructionWalker it, Method& method);

        NODISCARD InstructionWalker insertQueryChannelDataType(
            InstructionWalker it, Method& method, const Value& image, const Value& dest);
        NODISCARD InstructionWalker insertQueryChannelOrder(
            InstructionWalker it, Method& method, const Value& image, const Value& dest);
        NODISCARD InstructionWalker insertQueryMeasurements(
            InstructionWalker it, Method& method, const Value& image, const Value& dest);
    } // namespace intermediate
} // namespace vc4c

#endif /* IMAGES_H */
