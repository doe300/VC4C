/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef BITFIELD_H
#define BITFIELD_H

#include "CompilationError.h"
#include "helper.h"

#include <cstdint>
#include <type_traits>

namespace vc4c
{
	/*
	 * A bit-field is an integral type with a specific binary layout where different parts of the binary code have different meanings.
	 *
	 * The Macro BITFIELD_ENTRY can be used to generate getter/setter for the various parts of the binary representation with arbitrary offset and bit-size.
	 */
	template<typename UnderlyingType>
	struct Bitfield
	{
		static constexpr uint8_t MASK_Bit { 0x1 };
		static constexpr uint8_t MASK_Tuple { 0x3 };
		static constexpr uint8_t MASK_Triple { 0x7 };
		static constexpr uint8_t MASK_Quadruple { 0xF };
		static constexpr uint8_t MASK_Quintuple { 0x1F };
		static constexpr uint8_t MASK_Sextuple { 0x3F };
		static constexpr uint8_t MASK_Septuple { 0x7F };
		static constexpr uint8_t MASK_Byte { 0xFF };
		static constexpr uint16_t MASK_Nonuple = { 0x1FF };
		static constexpr uint16_t MASK_Decuple = { 0x3FF };
		static constexpr uint16_t MASK_Undecuple = { 0x7FF };
		static constexpr uint16_t MASK_Duodecuple = { 0xFFF };
		static constexpr uint16_t MASK_Tredecuple = { 0x1FFF };
		static constexpr uint16_t MASK_Short { 0xFFFF };
		static constexpr uint16_t MASK_SignedShort { 0xFFFF };
		static constexpr uint32_t MASK_Vigintuple { 0xFFFFF };
		static constexpr uint32_t MASK_Quattuorvigintuple { 0xFFFFFF };
		static constexpr uint32_t MASK_Duovigintuple { 0x3FFFFF };
		static constexpr uint32_t MASK_Int { 0xFFFFFFFF };
		static constexpr uint32_t MASK_SignedInt { 0xFFFFFFFF };

		explicit constexpr Bitfield(UnderlyingType val = 0) noexcept : value(val)
		{ }

		constexpr bool operator<(const Bitfield<UnderlyingType> other) const noexcept
		{
			return value < other.value;
		}

		constexpr bool operator==(const Bitfield<UnderlyingType> other) const noexcept
		{
			return value == other.value;
		}

		template<typename T>
		inline void setEntry(T val, uint8_t pos, UnderlyingType mask)
		{
			/* since for some fields, the "default" or "not set" value has not the bit-mask 0..0, we need to clear them first */
			value &= ~(mask << static_cast<UnderlyingType>(pos));
			value |= (mask & static_cast<UnderlyingType>(val)) << static_cast<UnderlyingType>(pos);
		}

		template<typename T>
		constexpr inline T getEntry(uint8_t pos, UnderlyingType mask) const
		{
			return static_cast<T>(mask & getValue(pos));
		}

		template<typename T>
		constexpr inline typename std::enable_if<!std::is_signed<T>::value, bool>::type fitsEntry(T val, UnderlyingType mask) const
		{
			return (static_cast<UnderlyingType>(mask) & static_cast<UnderlyingType>(val)) == static_cast<UnderlyingType>(val);
		}

		template<typename T>
		constexpr inline typename std::enable_if<std::is_signed<T>::value, bool>::type fitsEntry(T val, UnderlyingType mask) const
		{
			typedef typename std::make_unsigned<T>::type UT;
			return (static_cast<UnderlyingType>(mask) & static_cast<UnderlyingType>(bit_cast<T, UT>(val))) == static_cast<UnderlyingType>(bit_cast<T, UT>(val));
		}

#define BITFIELD_ENTRY(name, Type, pos, length) \
		inline bool fits##name##Field(Type val) \
		{ \
			return fitsEntry<Type>(val, Bitfield::MASK_##length); \
		} \
		inline Type get##name() const { \
			return getEntry<Type>(pos, Bitfield::MASK_##length); \
		} \
		inline void set##name(Type val) { \
			if(!fits##name##Field(val)) \
				throw CompilationError(CompilationStep::GENERAL, "Value is out of bounds for bit-field type");\
			setEntry<Type>(val, pos, Bitfield::MASK_##length); \
		}

		UnderlyingType value;

		constexpr inline UnderlyingType getValue(uint8_t startPos) const
		{
			return value >> static_cast<UnderlyingType>(startPos);
		}
	};

	/*
	 * Values which are part of an instruction and can be uniquely represented by some kind of integral value.
	 */
	struct InstructionPart
	{
		unsigned char value;

		explicit constexpr InstructionPart(unsigned char val) noexcept : value(val)
		{ }

		constexpr operator unsigned char() const
		{
			return value;
		}

		/*
		 * Two objects are the same, if their integral values match
		 */
		constexpr bool operator==(const InstructionPart& other) const
		{
			return value == other.value;
		}

		constexpr bool operator!=(const InstructionPart& other) const
		{
			return value != other.value;
		}
	};
} // namespace vc4c

#endif /* BITFIELD_H */
