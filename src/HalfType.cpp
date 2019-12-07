
#include "HalfType.h"

#include "helper.h"

using namespace vc4c;

static uint16_t extractHalfExponent(float f)
{
    if(f == 0.0f || f == -0.0f)
        return 0;
    auto exp = (bit_cast<float, uint32_t>(f) & 0x7F800000u) >> 23u;
    if(exp == 0)
        // subnormal value -> flush to zero
        return 0;
    if(exp == 0x1F)
        // Inf, NaN
        return 0x1F;

    return std::min(static_cast<uint16_t>(exp - 127 /* floating-point exponent bias*/ + 15 /* half exponent bias */),
        uint16_t{0x1F});
}

static uint16_t extractHalfMantissa(float f)
{
    auto exp = ((bit_cast<float, uint32_t>(f) & 0x7F800000u) >> 23u) - 127 + 15;
    if(exp >= 0x1F)
        // will be converted to +-Inf -> mantissa of zero
        return 0;
    // TODO wrong for subnormals, will convert x * 2^-127 to x*2^-15
    return static_cast<uint16_t>((bit_cast<float, uint32_t>(f) & 0x007FFFFFu) >> 13u);
}

Binary16::Binary16(float val) :
    sign(static_cast<uint16_t>(bit_cast<float, uint32_t>(val) >> 31u)), exponent(extractHalfExponent(val)),
    fraction(extractHalfMantissa(val))
{
}

Binary16::operator float() const
{
    if(isZero())
        return sign ? -0.0f : 0.0f;
    if(isInf())
        return sign ? -std::numeric_limits<float>::infinity() : std::numeric_limits<float>::infinity();
    if(isNaN())
        return std::numeric_limits<float>::quiet_NaN();
    uint32_t tmp = (static_cast<uint32_t>(sign) << 31u) | ((static_cast<uint32_t>(exponent) + (127 - 15)) << 23u) |
        (static_cast<uint32_t>(fraction) << 13u);
    // TODO rewrite (value-mapping instead of bit-wise?) and make constexpr,
    // also constructor
    return bit_cast<uint32_t, float>(tmp);
}
