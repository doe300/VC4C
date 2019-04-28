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
#include <cstring>
#include <limits>
#include <string>
#include <tuple>
#include <type_traits>

namespace vc4c
{
    /*
     * A bit-field is an integral type with a specific binary layout where different parts of the binary code have
     * different meanings.
     *
     * The Macro BITFIELD_ENTRY can be used to generate getter/setter for the various parts of the binary representation
     * with arbitrary offset and bit-size.
     */
    template <typename UnderlyingType>
    struct Bitfield
    {
        using LayoutType = std::tuple<uint8_t, uint8_t, const char*>;
        static constexpr uint8_t MASK_Bit{0x1};
        static constexpr uint8_t SIZE_Bit{1};
        static constexpr uint8_t MASK_Tuple{0x3};
        static constexpr uint8_t SIZE_Tuple{2};
        static constexpr uint8_t MASK_Triple{0x7};
        static constexpr uint8_t SIZE_Triple{3};
        static constexpr uint8_t MASK_Quadruple{0xF};
        static constexpr uint8_t SIZE_Quadruple{4};
        static constexpr uint8_t MASK_Quintuple{0x1F};
        static constexpr uint8_t SIZE_Quintuple{5};
        static constexpr uint8_t MASK_Sextuple{0x3F};
        static constexpr uint8_t SIZE_Sextuple{6};
        static constexpr uint8_t MASK_Septuple{0x7F};
        static constexpr uint8_t SIZE_Septuple{7};
        static constexpr uint8_t MASK_Byte{0xFF};
        static constexpr uint8_t SIZE_Byte{8};
        static constexpr uint16_t MASK_Nonuple = {0x1FF};
        static constexpr uint16_t SIZE_Nonuple = {9};
        static constexpr uint16_t MASK_Decuple = {0x3FF};
        static constexpr uint16_t SIZE_Decuple = {10};
        static constexpr uint16_t MASK_Undecuple = {0x7FF};
        static constexpr uint16_t SIZE_Undecuple = {11};
        static constexpr uint16_t MASK_Duodecuple = {0xFFF};
        static constexpr uint16_t SIZE_Duodecuple = {12};
        static constexpr uint16_t MASK_Tredecuple = {0x1FFF};
        static constexpr uint16_t SIZE_Tredecuple = {13};
        static constexpr uint16_t MASK_Short{0xFFFF};
        static constexpr uint16_t SIZE_Short{16};
        static constexpr uint16_t MASK_SignedShort{0xFFFF};
        static constexpr uint16_t SIZE_SignedShort{16};
        static constexpr uint32_t MASK_Vigintuple{0xFFFFF};
        static constexpr uint32_t SIZE_Vigintuple{20};
        static constexpr uint32_t MASK_Quattuorvigintuple{0xFFFFFF};
        static constexpr uint32_t SIZE_Quattuorvigintuple{24};
        static constexpr uint32_t MASK_Duovigintuple{0x3FFFFF};
        static constexpr uint32_t SIZE_Duovigintuple{22};
        static constexpr uint32_t MASK_Int{0xFFFFFFFF};
        static constexpr uint32_t SIZE_Int{32};
        static constexpr uint32_t MASK_SignedInt{0xFFFFFFFF};
        static constexpr uint32_t SIZE_SignedInt{32};
        static constexpr uintptr_t MASK_Pointer{std::numeric_limits<uintptr_t>::max()};
        static constexpr uintptr_t SIZE_Pointer{sizeof(uintptr_t) * 8};

        explicit constexpr Bitfield(UnderlyingType val = 0) noexcept : value(val) {}

        constexpr bool operator<(Bitfield<UnderlyingType> other) const noexcept
        {
            return value < other.value;
        }

        constexpr bool operator==(Bitfield<UnderlyingType> other) const noexcept
        {
            return value == other.value;
        }

        constexpr bool operator!=(Bitfield<UnderlyingType> other) const noexcept
        {
            return value != other.value;
        }

        template <typename T>
        constexpr inline void setEntry(T val, UnderlyingType pos, UnderlyingType mask) noexcept
        {
            /* since for some fields, the "default" or "not set" value has not the bit-mask 0..0, we need to clear them
             * first */
            value = static_cast<UnderlyingType>(value & (~(mask << pos)));
            value = static_cast<UnderlyingType>(value | ((mask & static_cast<UnderlyingType>(val)) << pos));
        }

        template <typename T>
        constexpr inline T getEntry(UnderlyingType pos, UnderlyingType mask) const noexcept
        {
            return static_cast<T>(mask & getValue(pos));
        }

        template <typename T>
        constexpr inline typename std::enable_if<!std::is_signed<T>::value, bool>::type fitsEntry(
            T val, UnderlyingType mask) const noexcept
        {
            return static_cast<UnderlyingType>(mask & static_cast<UnderlyingType>(val)) ==
                static_cast<UnderlyingType>(val);
        }

        template <typename T>
        constexpr inline typename std::enable_if<std::is_signed<T>::value, bool>::type fitsEntry(
            T val, UnderlyingType mask) const noexcept
        {
            using UT = typename std::make_unsigned<T>::type;
            return static_cast<UnderlyingType>(mask & static_cast<UnderlyingType>(bit_cast<T, UT>(val))) ==
                static_cast<UnderlyingType>(bit_cast<T, UT>(val));
        }

#define BITFIELD_ENTRY(name, Type, pos, length)                                                                        \
    inline bool fits##name##Field(Type val) const noexcept                                                             \
    {                                                                                                                  \
        return fitsEntry<Type>(val, Bitfield::MASK_##length);                                                          \
    }                                                                                                                  \
    inline Type get##name() const noexcept                                                                             \
    {                                                                                                                  \
        return getEntry<Type>(pos, Bitfield::MASK_##length);                                                           \
    }                                                                                                                  \
    inline void set##name(Type val)                                                                                    \
    {                                                                                                                  \
        if(!fits##name##Field(val))                                                                                    \
            throw CompilationError(CompilationStep::GENERAL, "Value is out of bounds for bit-field type");             \
        setEntry<Type>(val, pos, Bitfield::MASK_##length);                                                             \
    }                                                                                                                  \
    static constexpr LayoutType layout##name{pos, Bitfield::SIZE_##length, #name};

#define BITFIELD_ENTRY_CONSTEXPR(name, Type, pos, length)                                                              \
    constexpr inline bool fits##name##Field(Type val) const noexcept                                                   \
    {                                                                                                                  \
        return fitsEntry<Type>(val, Bitfield::MASK_##length);                                                          \
    }                                                                                                                  \
    constexpr inline Type get##name() const noexcept                                                                   \
    {                                                                                                                  \
        return getEntry<Type>(pos, Bitfield::MASK_##length);                                                           \
    }                                                                                                                  \
    constexpr inline void set##name(Type val)                                                                          \
    {                                                                                                                  \
        if(!fits##name##Field(val))                                                                                    \
            throw CompilationError(CompilationStep::GENERAL, "Value is out of bounds for bit-field type");             \
        setEntry<Type>(val, pos, Bitfield::MASK_##length);                                                             \
    }                                                                                                                  \
    static constexpr LayoutType layout##name{pos, Bitfield::SIZE_##length, #name};

        UnderlyingType value;

        constexpr inline UnderlyingType getValue(UnderlyingType startPos) const noexcept
        {
            return static_cast<UnderlyingType>(value >> startPos);
        }

        /**
         * Creates a string representation of the bit-layout with the given elements to be considered
         */
        LCOV_EXCL_START
        std::string createLayout(std::initializer_list<LayoutType> elements)
        {
            // 2 characters per bit
            constexpr std::size_t charsPerBit = 2;
            std::string tmp(sizeof(UnderlyingType) * 8 * charsPerBit, '.');

            for(const auto& element : elements)
            {
                auto startPos = static_cast<std::size_t>(charsPerBit * std::get<0>(element));
                auto maxLength = static_cast<std::size_t>(charsPerBit * std::get<1>(element));
                auto nameLength = std::min(std::strlen(std::get<2>(element)), maxLength);
                tmp.replace(startPos, nameLength, std::get<2>(element), nameLength);
                if(nameLength < maxLength)
                    tmp.replace(startPos + nameLength, maxLength - nameLength, maxLength - nameLength, '_');
                tmp[startPos + maxLength - 1] = '|';
            }
            return tmp;
        }
        LCOV_EXCL_STOP
    };

    /*
     * Values which are part of an instruction and can be uniquely represented by some kind of integral value.
     */
    struct InstructionPart
    {
        unsigned char value;

        explicit constexpr InstructionPart(unsigned char val) noexcept : value(val) {}

        constexpr operator unsigned char() const noexcept
        {
            return value;
        }

        /*
         * Two objects are the same, if their integral values match
         */
        constexpr bool operator==(const InstructionPart& other) const noexcept
        {
            return value == other.value;
        }

        constexpr bool operator!=(const InstructionPart& other) const noexcept
        {
            return value != other.value;
        }
    };
} // namespace vc4c

#endif /* BITFIELD_H */
