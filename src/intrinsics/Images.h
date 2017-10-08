/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef IMAGES_H
#define IMAGES_H

#include <stdint.h>

#include "../Module.h"
#include "../InstructionWalker.h"
#include "../periphery/TMU.h"

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
		enum class AddressingMode
		{
			/*
			 * "for this addressing mode the programmer guarantees that the image coordinates used to sample elements of the image refer to a location inside the image; otherwise the results are undefined."
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
			 * "out-of-range image coordinates are wrapped to the valid range. This addressing mode can only be used with normalized coordinates. If normalized coordinates are not used,
			 * this addressing mode may generate image coordinates that are undefined."
			 */
			REPEAT = 3,
			/*
			 * "Flip the image coordinate at every integer junction. This addressing mode can only be used with normalized coordinates.
			 * If normalized coordinates are not used, this addressing mode may generate image coordinates that are undefined."
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
		enum class FilterMode
		{
			/*
			 * "When filter mode is CLK_FILTER_NEAREST , the image element in the image that is nearest (in Manhattan distance) to that specified by (u,v,w) is obtained."
			 */
			NEAREST = 1,
			/*
			 * "When filter mode is CLK_FILTER_LINEAR , a 2 x 2 square of image elements for a 2D image or a 2 x 2 x 2 cube of image elements for a 3D image is selected."
			 */
			LINEAR = 2
		};

		struct Sampler : private Bitfield<uint8_t>
		{
		
			static constexpr unsigned long MASK_NORMALIZED_COORDS { 0x1 };
			static constexpr unsigned long MASK_ADDRESSING_MODE { 0xE };
			static constexpr unsigned long MASK_FILTER_MODE { 0x30 };

			constexpr Sampler(const uint8_t val) : Bitfield(val)
			{

			}

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

		/*  0                   1                   2                   3
		 *  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
		 * | channel-type  | channel-order | dimensions    | ???           |
		 * | width (run-time limit is 4k)  | height (run-time limit is 4k) |
		 * | depth (run-time limit is 4k)  | array-size (limit see others) |
		 * | row pitch (distance y-coord)  | slice pitch (distance z-coord)|
		 */

		//TODO sizes/positions/padding needs to match runtime
		static const DataType IMAGE_INFO_TYPE = TYPE_INT8;
		//offset of 0 bytes
		static const Value IMAGE_CHANNEL_TYPE_OFFSET = INT_ZERO;
		//offset of 1 byte
		static const Value IMAGE_CHANNEL_ORDER_OFFSET = INT_ZERO;
		//offset of 2 bytes
		static const Value IMAGE_DIMENSIONS_OFFSET(Literal(2L), TYPE_INT8);
		static const DataType IMAGE_SIZE_TYPE = TYPE_INT16;
		//offset of 4 bytes
		static const Value IMAGE_WIDTH_OFFSET(Literal(4L), TYPE_INT8);
		//offset of 6 bytes
		static const Value IMAGE_HEIGHT_OFFSET(Literal(6L), TYPE_INT8);
		//offset of 8 bytes
		static const Value IMAGE_DEPTH_OFFSET(Literal(8L), TYPE_INT8);
		//offset of 10 bytes
		static const Value IMAGE_ARRAY_SIZE_OFFSET(Literal(10L), TYPE_INT8);
		//offset of 12 bytes
		static const Value IMAGE_ROW_PITCH_OFFSET(Literal(12L), TYPE_INT8);
		//offset of 14 bytes
		static const Value IMAGE_SLICE_PITCH_OFFSET(Literal(14L), TYPE_INT8);
		//offset of 16 bytes
		static const Value IMAGE_DATA_OFFSET(Literal(16L), TYPE_INT8);

		InstructionWalker intrinsifyImageFunction(InstructionWalker it, Method& method);

		//TODO rewrite all query info to read from global representing the image-config (need to know layout!!), in VC4CLStdLib via intrinsics?
		//TODO rewrite all queries to TMU call
		//TODO VC4CLStdLib: rewrite all image-queries to use float-coordinate format with ranges [0, 1]

		InstructionWalker insertQueryChannelDataType(InstructionWalker it, Method& method, const Value& image, const Value& dest);
		InstructionWalker insertQueryChannelOrder(InstructionWalker it, Method& method, const Value& image, const Value& dest);
		InstructionWalker insertQueryMeasurements(InstructionWalker it, Method& method, const Value& image, const Value& dest);
	}
}

#endif /* IMAGES_H */

