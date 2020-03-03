/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_SIMD_VECTOR_H
#define VC4C_SIMD_VECTOR_H 1

#include "Values.h"
#include "performance.h"

#include <mutex>

namespace std
{
    template <>
    struct hash<vc4c::SIMDVector>
    {
        size_t operator()(const vc4c::SIMDVector& val) const noexcept;
    };
} /* namespace std */

namespace vc4c
{
    struct SIMDVectorHolder;

    /*
     * Content type for Values containing a whole SIMD vector
     *
     * This type has multiple usages:
     * 1. A representation of a vector of literal values as part of a Value. In this case, the SIMDVector objects are
     *    stored in shared memory and the Value objects maintain pointers to the vectors.
     * 2. As data type for the emulation. In this case, the SIMDVector is used as literal type to be stored and
     *    processed directly in the emulated components.
     */
    class SIMDVector
    {
        using Elements = std::array<Literal, NATIVE_VECTOR_SIZE>;

    public:
        constexpr explicit SIMDVector(Literal defaultValue = UNDEFINED_LITERAL) noexcept :
            elements({defaultValue, defaultValue, defaultValue, defaultValue, defaultValue, defaultValue, defaultValue,
                defaultValue, defaultValue, defaultValue, defaultValue, defaultValue, defaultValue, defaultValue,
                defaultValue, defaultValue})
        {
        }

        constexpr SIMDVector(std::initializer_list<Literal> list) noexcept : SIMDVector()
        {
            std::copy_n(list.begin(), std::min(NATIVE_VECTOR_SIZE, list.size()), elements.begin());
        }

        SIMDVector(const SIMDVector&) = default;
        SIMDVector(SIMDVector&&) noexcept = default;
        ~SIMDVector() noexcept = default;

        SIMDVector& operator=(const SIMDVector&) = default;
        SIMDVector& operator=(SIMDVector&&) noexcept = default;

        inline bool operator==(const SIMDVector& other) const noexcept
        {
            return elements == other.elements;
        }

        inline bool operator!=(const SIMDVector& other) const noexcept
        {
            return elements != other.elements;
        }

        Elements::iterator begin() noexcept
        {
            return elements.begin();
        }

        inline Elements::const_iterator begin() const noexcept
        {
            return elements.begin();
        }

        Elements::iterator end() noexcept
        {
            return elements.end();
        }

        inline Elements::const_iterator end() const noexcept
        {
            return elements.end();
        }

        inline Literal& operator[](std::size_t index) noexcept
        {
            return elements[index];
        }

        inline Literal operator[](std::size_t index) const noexcept
        {
            return elements[index];
        }

        inline Literal& at(std::size_t index)
        {
            return elements.at(index);
        }

        inline Literal at(std::size_t index) const
        {
            return elements.at(index);
        }

        constexpr uint8_t size() const noexcept
        {
            return NATIVE_VECTOR_SIZE;
        }

        /**
         * Determines whether all elements of this vector have the same value and returns that value.
         *
         * NOTE: undefined elements are considered "equal" to any other element. In case the vector contains undefined
         * and "defined" elements, the defined element comparing equal to all other elements is returned.
         *
         * NOTE: Since the undefined literal doubles as empty compact_optional<Literal>, a vector containing only
         * undefined elements will report as having no "same element". In that case #isUndefined() needs to be checked!
         */
        Optional<Literal> getAllSame() const noexcept;

        /**
         * Determines whether all element-values correspond to their element number,  e.g. 1, 2, 3, 4, ...
         *
         * If withOffset is set to true, an arbitrary initial offset is allowed, e.g. 5, 6, 7, 8, ...
         *
         * If withFactor is set to true, an arbitrary factor is allowed, e.g. 0, 4, 8, 12, 16, ...
         *
         * If ignoreUndefined is set, elements which are undefined are assumed to have the value matching the element
         * number (with offset or factor), e.g. 0, 1, 2, 3, undefined, 5, ... will return true.
         *
         * NOTE: The parameters withOffset and withFactor cannot be set at the same time
         */
        bool isElementNumber(bool withOffset = false, bool withFactor = false, bool ignoreUndefined = true) const
            noexcept;

        /**
         * Returns whether all elements contained are undefined
         */
        bool isUndefined() const;

        SIMDVector transform(const std::function<Literal(Literal)>& transformOp) const&;
        SIMDVector transform(const std::function<Literal(Literal)>& transformOp) &&;

        /*
         * Rotates the elements of this vector UPWARDS by the given offset.
         */
        SIMDVector rotate(uint8_t offset) const&;
        SIMDVector rotate(uint8_t offset) &&;
        SIMDVector rotatePerQuad(uint8_t offset) const&;
        SIMDVector rotatePerQuad(uint8_t offset) &&;

        std::string to_string(bool withLiterals = false) const;

        SIMDVectorHolder* getStorage() const
        {
            return storage;
        }

    private:
        Elements elements;
        SIMDVectorHolder* storage = nullptr;
        friend struct SIMDVectorHolder;
    };

    /*
     * Container which holds and manages SIMD vectors
     *
     * NOTE: a type-holder object MUST live longer than all vectors stored in it!
     */
    struct SIMDVectorHolder
    {
    public:
        /**
         * Moves the given temporary vector into the global storage and a value containing either the inserted vector,
         * an equivalent vector already stored or an equivalent literal value.
         */
        Value storeVector(SIMDVector&& vec, DataType type);

        /**
         * Uses the given store (if set) to store the vector, otherwise uses the global storage.
         *
         * See #storeVector(SIMDVector&&)
         */
        static Value storeVector(SIMDVector&& vec, DataType type, SIMDVectorHolder* storage);

    private:
        /*
         * The global cache of SIMDVectors.
         *
         * The contents of set are immutable!
         * This is a hashmap so a) we can quickly check for and avoid duplicates and b) the references to the elements
         * are stable.
         */
        FastSet<SIMDVector> constantVectors;
        std::mutex accessMutex;
    };

    /**
     * Default global SIMDVector holder.
     *
     * NOTE: Since this is a static object and the contained vectors will not be cleaned up, this should be used as
     * little as possible! The vector holder in the Module should be preferred!
     */
    extern SIMDVectorHolder GLOBAL_VECTOR_HOLDER;
} /* namespace vc4c */

#endif /* VC4C_SIMD_VECTOR_H */