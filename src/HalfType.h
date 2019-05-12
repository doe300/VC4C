/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_HALF_TYPE_H
#define VC4C_HALF_TYPE_H

#include <cinttypes>
#include <limits>

namespace vc4c
{
    /**
     * 16-bit floating point type.
     *
     * Implemented after:
     * https://en.wikipedia.org/wiki/Half-precision_floating-point_format
     */
    struct Binary16
    {
        uint16_t sign : 1;
        uint16_t exponent : 5;
        uint16_t fraction : 10;

        constexpr explicit Binary16(uint16_t val = 0) :
            sign(static_cast<uint16_t>(val >> 15)), exponent(static_cast<uint16_t>((val & 0x7C00) >> 10)),
            fraction(static_cast<uint16_t>(val & 0x3FF))
        {
        }

        constexpr explicit Binary16(uint16_t sign, uint16_t exp, uint16_t mantissa) :
            sign(sign), exponent(exp), fraction(mantissa)
        {
        }

        Binary16(float val);

        operator float() const;

        inline constexpr operator uint16_t() const
        {
            return static_cast<uint16_t>((sign << 15) | (exponent << 10) | (fraction));
        }

        inline constexpr bool isZero() const
        {
            return exponent == 0 && fraction == 0;
        }

        inline constexpr bool isSubnormal() const
        {
            return exponent == 0 && fraction != 0;
        }

        inline constexpr bool isInf() const
        {
            return exponent == 0x1F && fraction == 0;
        }

        inline constexpr bool isNaN() const
        {
            return exponent == 0x1F && fraction != 0;
        }
    };

    constexpr Binary16 HALF_ZERO(static_cast<uint16_t>(0));
    constexpr Binary16 HALF_ONE(0, 15 /* exponent bias */, 0);
    constexpr Binary16 HALF_INF(0, 0x1F, 0);

    static_assert(sizeof(Binary16) == 2, "Binary16 type has wrong size!");
    static_assert(static_cast<uint16_t>(Binary16(static_cast<uint16_t>(0x1234))) == static_cast<uint16_t>(0x1234),
        "Bit conversion failed!");
    static_assert((Binary16(static_cast<uint16_t>(0))).isZero(), "Bitwise check failed!");
    static_assert((Binary16(static_cast<uint16_t>(0x123))).isSubnormal(), "Bitwise check failed!");
    static_assert((Binary16(static_cast<uint16_t>(0x7C00))).isInf(), "Bitwise check failed!");
    static_assert((Binary16(static_cast<uint16_t>(0xFFFF))).isNaN(), "Bitwise check failed!");

    inline Binary16 operator""_h(long double val)
    {
        return Binary16(static_cast<float>(val));
    }

    using half_t = Binary16;
} // namespace vc4c

namespace std
{
    template <>
    struct numeric_limits<vc4c::half_t>
    {
        static constexpr bool is_specialized = true;
        static constexpr vc4c::half_t min()
        {
            return vc4c::half_t(0, 1, 0);
        }
        static constexpr vc4c::half_t max()
        {
            return vc4c::half_t(0, 0x1E, 0x3FF);
        }
        static constexpr vc4c::half_t lowest()
        {
            return vc4c::half_t(1, 0x1E, 0x3FF);
        }
        static constexpr int digits = 10;
        static constexpr int digits10 = 3;     // XXX ??
        static constexpr int max_digits10 = 4; // XXX ??
        static constexpr bool is_signed = true;
        static constexpr bool is_integer = false;
        static constexpr bool is_exact = false;
        static constexpr int radix = 2;
        static constexpr vc4c::half_t epsilon()
        {
            return vc4c::half_t(0, 1, 1); /*XXX ?? */
        }
        static constexpr vc4c::half_t round_error()
        {
            // 0.5h
            return vc4c::half_t(0, 14, 0); /* XXX ?? */
        }
        static constexpr int min_exponent = -15;
        static constexpr int min_exponent10 = -4; // XXX ??
        static constexpr int max_exponent = 15;
        static constexpr int max_exponent10 = 4; // XXX ??
        static constexpr bool has_infinity = true;
        static constexpr bool has_quiet_NaN = true;
        static constexpr bool has_signaling_NaN = true;
        static constexpr float_denorm_style has_denorm = float_denorm_style::denorm_present;
        static constexpr bool has_denorm_loss = false; // XXX ??
        static constexpr vc4c::half_t infinity()
        {
            return vc4c::half_t(0, 0x1F, 0);
        }
        static constexpr vc4c::half_t queit_NaN()
        {
            return vc4c::half_t(0, 0x1F, 1);
        }
        static constexpr vc4c::half_t signaling_NaN()
        {
            return vc4c::half_t(0, 0x1F, 0x3FF); /* XXX ?? */
        }
        static constexpr vc4c::half_t denorm_min()
        {
            return vc4c::half_t(0, 0, 1);
        }
        static constexpr bool is_iec559 = true;
        static constexpr bool is_bounded = true;
        static constexpr bool is_modulo = false;
        static constexpr bool traps = false;
        static constexpr bool tinyness_before = false;                                        // XXX ??
        static constexpr float_round_style round_style = float_round_style::round_to_nearest; // XXX ??
    };
} // namespace std

#endif /* VC4C_HALF_TYPE_H */
