/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SIMDVector.h"

#include <numeric>

using namespace vc4c;

SIMDVectorHolder vc4c::GLOBAL_VECTOR_HOLDER{};

bool SIMDVector::isAllSame() const noexcept
{
    Literal firstElement = elements[0];
    return std::all_of(elements.begin(), elements.end(),
        // if items are UNDEFINED, ignore them, since maybe the remaining items all have the same value
        [=](Literal lit) -> bool { return lit.isUndefined() || lit == firstElement; });
}

bool SIMDVector::isElementNumber(bool withOffset, bool withFactor, bool ignoreUndefined) const noexcept
{
    if(withOffset && withFactor)
        return false;
    if(withOffset)
    {
        Optional<int32_t> offset;
        for(std::size_t i = 0; i < elements.size(); ++i)
        {
            if(elements[i].isUndefined())
            {
                if(ignoreUndefined)
                    continue;
                else
                    return false;
            }
            if(!offset)
                // get offset from first non-undefined element
                offset = elements[i].signedInt() - static_cast<int32_t>(i);
            if(elements[i].signedInt() != (static_cast<int32_t>(i) + *offset))
                return false;
        }
        return true;
    }
    if(withFactor)
    {
        Optional<int32_t> factor;
        for(std::size_t i = 0; i < elements.size(); ++i)
        {
            if(elements[i].isUndefined())
            {
                if(ignoreUndefined)
                    continue;
                else
                    return false;
            }
            if(!factor && i != 0)
                // get factor from first non-undefined element
                factor = elements[i].signedInt() / static_cast<int32_t>(i);
            if(elements[i].signedInt() != (static_cast<int32_t>(i) * *factor))
                return false;
        }
        return true;
    }

    for(std::size_t i = 0; i < elements.size(); ++i)
    {
        if(elements[i].isUndefined())
        {
            if(ignoreUndefined)
                continue;
            else
                return false;
        }
        if(elements[i].signedInt() != static_cast<int32_t>(i))
            return false;
    }
    return true;
}

bool SIMDVector::isUndefined() const
{
    return std::all_of(elements.begin(), elements.end(), [](Literal lit) -> bool { return lit.isUndefined(); });
}

SIMDVector SIMDVector::transform(const std::function<Literal(Literal)>& transformOp) const&
{
    SIMDVector copy;
    for(std::size_t i = 0; i < elements.size(); ++i)
    {
        copy.elements[i] = transformOp(elements[i]);
    }
    return copy;
}

SIMDVector SIMDVector::transform(const std::function<Literal(Literal)>& transformOp) &&
{
    std::transform(elements.begin(), elements.end(), elements.begin(), transformOp);
    return std::move(*this);
}

SIMDVector SIMDVector::rotate(uint8_t offset) const&
{
    return SIMDVector{*this}.rotate(offset);
}

SIMDVector SIMDVector::rotate(uint8_t offset) &&
{
    //"Rotates the order of the elements in the range [first,last), in such a way that the element pointed by middle
    // becomes the new first element."
    // -> this rotates downwards by the offset (middle - start), so we need to invert the offset
    // rotate_up(vec, offset) = rotate_down(vec, len(vec) - offset)
    offset = static_cast<uint8_t>(NATIVE_VECTOR_SIZE - offset);
    std::rotate(begin(), begin() + offset, end());
    return std::move(*this);
}

SIMDVector SIMDVector::rotatePerQuad(uint8_t offset) const&
{
    return SIMDVector{*this}.rotatePerQuad(offset);
}

SIMDVector SIMDVector::rotatePerQuad(uint8_t offset) &&
{
    std::rotate(begin() + 0, begin() + 4 - offset, begin() + 4);
    std::rotate(begin() + 4, begin() + 8 - offset, begin() + 8);
    std::rotate(begin() + 8, begin() + 12 - offset, begin() + 12);
    std::rotate(begin() + 12, end() - offset, end());
    return std::move(*this);
}

LCOV_EXCL_START
std::string SIMDVector::to_string(bool withLiterals) const
{
    if(withLiterals)
    {
        std::string tmp;
        for(const auto& lit : elements)
            tmp.append(lit.to_string()).append(", ");
        return "<" + tmp.substr(0, tmp.length() - 2) + ">";
    }
    if(isUndefined())
        return "<undefined>";
    if(isAllSame() && at(0).unsignedInt() == 0)
        return "zerointializer";
    return "SIMD vector";
}
LCOV_EXCL_STOP

Value SIMDVectorHolder::storeVector(SIMDVector&& vec, DataType type)
{
    if(vec.isAllSame() &&
        std::none_of(vec.begin(), vec.end(), [](const Literal& lit) -> bool { return lit.isUndefined(); }))
    {
        // if all elements are a non-undefined same value, just use that value right now, since we would convert the
        // vector to that value later on anyway. This saves us from later having to check that as well as needing to
        // store a SIMDVector entry.
        return Value(vec[0], type);
    }
    // TODO does this mutex lock too often??
    std::lock_guard<std::mutex> guard(accessMutex);
    auto it = constantVectors.emplace(std::move(vec)).first;
    const_cast<SIMDVectorHolder*&>(it->storage) = this;
    return Value(&*it, type);
}

Value SIMDVectorHolder::storeVector(SIMDVector&& vec, DataType type, SIMDVectorHolder* storage)
{
    // TODO try to reduce the number of global vectors stored
    return (storage ? storage : &GLOBAL_VECTOR_HOLDER)->storeVector(std::move(vec), type);
}

std::size_t std::hash<vc4c::SIMDVector>::operator()(vc4c::SIMDVector const& val) const noexcept
{
    static const std::hash<Literal> elementHash;
    return std::accumulate(val.begin(), val.end(), static_cast<std::size_t>(0),
        [&](std::size_t s, const Literal& val) -> std::size_t { return s + elementHash(val); });
}
