/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#pragma once

#include "Bitfield.h"

#include <string>

namespace vc4c
{
    class Literal;

    /**
     * Represents a mask of the bits of a register (a word).
     *
     * This mask can be used to optimize away modifications of parts of a register not read or to correctly handle
     * pack-modes only writing registers partially.
     */
    struct BitMask : public Bitfield<uint32_t>
    {
        explicit constexpr BitMask(uint32_t val = 0) noexcept : Bitfield<uint32_t>(val) {}

        BITFIELD_ENTRY(FullWord, uint32_t, 0, Int)

        BITFIELD_ENTRY(LowerHalfWord, uint16_t, 0, Short)
        BITFIELD_ENTRY(UpperHalfWord, uint16_t, 16, Short)

        BITFIELD_ENTRY(Byte0, uint8_t, 0, Byte)
        BITFIELD_ENTRY(Byte1, uint8_t, 8, Byte)
        BITFIELD_ENTRY(Byte2, uint8_t, 16, Byte)
        BITFIELD_ENTRY(Byte3, uint8_t, 24, Byte)

        /**
         * Creates a new literal value by taking all bits from the new value where the corresponding bit in the mask
         * is set and taking the bits from the old value otherwise.
         */
        Literal operator()(Literal newValue, Literal oldValue) const noexcept;

        constexpr operator uint32_t() const noexcept
        {
            return value;
        }

        explicit constexpr operator bool() const noexcept
        {
            return value != 0;
        }

        constexpr BitMask operator&(const BitMask& other) const noexcept
        {
            return BitMask{value & other.value};
        }

        constexpr BitMask operator|(const BitMask& other) const noexcept
        {
            return BitMask{value | other.value};
        }

        constexpr BitMask operator<<(uint8_t offset) const noexcept
        {
            return BitMask{value << offset};
        }

        constexpr BitMask operator>>(uint8_t offset) const noexcept
        {
            return BitMask{value >> offset};
        }

        std::string to_string() const;
    };

    constexpr BitMask BITMASK_ALL{0xFFFFFFFF};
    constexpr BitMask BITMASK_NONE{0x0};
} // namespace vc4c
