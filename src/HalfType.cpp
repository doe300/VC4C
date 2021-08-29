
#include "HalfType.h"

#include "helper.h"

using namespace vc4c;

static uint16_t extractHalfExponent(float f)
{
    if(f == 0.0f || f == -0.0f)
        return 0;
    auto exp = (bit_cast<float, uint32_t>(f) & 0x7F800000u) >> 23u;
    if(exp == 0)
        // subnormal value
        return 0;
    if(exp == 0xFF)
        // Inf, NaN
        return 0x1F;

    return std::min(
        static_cast<uint16_t>(std::max(
            static_cast<int32_t>(exp) - 127 /* floating-point exponent bias*/ + 15 /* half exponent bias */, 0)),
        uint16_t{0x1F});
}

static uint16_t extractHalfMantissa(float f)
{
    auto exp = ((bit_cast<float, uint32_t>(f) & 0x7F800000u) >> 23u) - 127 + 15;
    if(exp >= 0x1F)
        // will be converted to +-Inf -> mantissa of zero
        return 0;
    // XXX the VideoCore IV hardware always rounds to +/- Inf (away from zero), which is not compliant with the standard
    auto floatMantissa = bit_cast<float, uint32_t>(f) & 0x007FFFFFu;
    auto halfMantissa = static_cast<uint16_t>(floatMantissa >> 13u);
    return halfMantissa + ((floatMantissa & 0x1FFFu) != 0);
}

Binary16::Binary16(float val) :
    fraction(extractHalfMantissa(val)), exponent(extractHalfExponent(val)),
    sign(static_cast<uint16_t>(bit_cast<float, uint32_t>(val) >> 31u))
{
}

Binary16::operator float() const
{
    if(isZero())
        return sign ? -0.0f : 0.0f;
    if(isInf())
        return sign ? -std::numeric_limits<float>::infinity() : std::numeric_limits<float>::infinity();
    if(isNaN())
        return sign ? -std::numeric_limits<float>::quiet_NaN() : std::numeric_limits<float>::quiet_NaN();
    uint32_t tmp = (static_cast<uint32_t>(sign) << 31u) | ((static_cast<uint32_t>(exponent) + (127 - 15)) << 23u) |
        (static_cast<uint32_t>(fraction) << 13u);
    // TODO rewrite (value-mapping instead of bit-wise?) and make constexpr,
    // also constructor
    return bit_cast<uint32_t, float>(tmp);
}
