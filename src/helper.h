/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef HELPER_H
#define HELPER_H

#include <functional>
#include <set>
#include <vector>

#include "CompilationError.h"

#if __cplusplus > 201402L
#define NODISCARD [[nodiscard]]
#else
#define NODISCARD __attribute__((warn_unused_result))
#endif

#if __cplusplus > 201402L
#define FALL_THROUGH [[fallthrough]];
#elif defined(__clang_major__) && __clang_major__ >= 4
#define FALL_THROUGH [[clang::fallthrough]];
#elif defined(__GNUC__) && __GNUC__ >= 7
#define FALL_THROUGH __attribute__((fallthrough));
#else
#define FALL_THROUGH /* fall through */
#endif

// The function only reads its arguments and global state, has no side effect except its return value
#define PURE __attribute__((pure))
// The function only reads its arguments, has no side effect except its return value
// Calling this function several times with the same arguments is guaranteed to return the same value
#define CONST __attribute__((const))

/*
 * For usage with LCOV, the occurrence of the strings "LCOV_EXCL_START" and "LCOV_EXCL_STOP" inside the source file
 * excludes the contained block from coverage.
 * NOTE: The strings must be in the source file before macro preprocessing, e.g. as macro-name or comment.
 *
 * To make their meaning clear, we define them as macros here to be used in the code blocks to skip coverage collection
 * for.
 */
#define LCOV_EXCL_START /* << LCOV_EXCL_START - This block is excluded from code coverage via lcov */
#define LCOV_EXCL_STOP  /* << LCOV_EXCL_STOP - end of lcov exclusion block */

namespace vc4c
{
    LCOV_EXCL_START
    /*
     * Converts a container (defaults to std::vector) of values to a single string separated by the given string.
     *
     * NOTE: To use this function, the objects stored in the container need to support the to_string() method
     */
    template <typename T, typename VT = std::vector<T>,
        typename std::enable_if<!std::is_arithmetic<T>::value, void*>::type = nullptr>
    inline std::string to_string(const VT& values, const std::string& separator = ", ")
    {
        std::string tmp;
        for(const T& val : values)
        {
            tmp.append(val.to_string()).append(separator);
        }
        return tmp.substr(0, tmp.size() - separator.size());
    }

    /*
     * Specialization of above function to print containers of std::strings
     */
    template <>
    inline std::string to_string<std::string>(const std::vector<std::string>& values, const std::string& separator)
    {
        std::string tmp;
        for(const std::string& val : values)
        {
            tmp.append(val).append(separator);
        }
        return tmp.substr(0, tmp.size() - separator.size());
    }

    template <>
    inline std::string to_string<std::string>(const std::set<std::string>& values, const std::string& separator)
    {
        std::string tmp;
        for(const std::string& val : values)
        {
            tmp.append(val).append(separator);
        }
        return tmp.substr(0, tmp.size() - separator.size());
    }

    template <typename T, typename VT = std::vector<T>,
        typename std::enable_if<std::is_arithmetic<T>::value, void*>::type = nullptr>
    inline std::string to_string(const VT& values, const std::string& separator = ", ")
    {
        std::string tmp;
        for(auto val : values)
        {
            tmp.append(std::to_string(val)).append(separator);
        }
        return tmp.substr(0, tmp.size() - separator.size());
    }
    LCOV_EXCL_STOP

    /*
     * Asserts a pointer to be non-null
     */
    template <typename T>
    CONST inline const T* checkPointer(const T* ptr)
    {
        if(ptr == nullptr)
            throw CompilationError(CompilationStep::GENERAL, "Tried to access a null-pointer!");
        return ptr;
    }

    /*
     * Asserts a pointer to be non-null
     */
    template <typename T>
    CONST inline T* checkPointer(T* ptr)
    {
        if(ptr == nullptr)
            throw CompilationError(CompilationStep::GENERAL, "Tried to access a null-pointer!");
        return ptr;
    }

    /*
     * Bit-casts a value between two types with the same bit-width
     */
    template <typename From, typename To>
    CONST inline To bit_cast(From in) noexcept
    {
        static_assert(sizeof(From) == sizeof(To), "Can't convert types from different sizes!");
        union {
            From f;
            To t;
        } bit_cast;
        bit_cast.f = in;
        return bit_cast.t;
    }

    /*
     * Returns the bit-field with the additional flag set
     */
    template <typename Bitfield>
    CONST constexpr inline NODISCARD Bitfield add_flag(Bitfield orig, Bitfield flag) noexcept
    {
        return static_cast<Bitfield>(static_cast<unsigned>(orig) | static_cast<unsigned>(flag));
    }

    template <typename Bitfield>
    CONST constexpr inline NODISCARD Bitfield add_flag(Bitfield orig, Bitfield flag0, Bitfield flag1) noexcept
    {
        return static_cast<Bitfield>(
            static_cast<unsigned>(orig) | static_cast<unsigned>(flag0) | static_cast<unsigned>(flag1));
    }

    /*
     * Returns the bit-field with the additional flag being cleared
     */
    template <typename Bitfield>
    CONST constexpr inline NODISCARD Bitfield remove_flag(Bitfield orig, Bitfield flag) noexcept
    {
        return static_cast<Bitfield>(static_cast<unsigned>(orig) & ~static_cast<unsigned>(flag));
    }

    /*
     * Returns whether the given flag is set in the bit-field
     */
    template <typename Bitfield>
    CONST constexpr inline bool has_flag(Bitfield field, Bitfield flag) noexcept
    {
        return (static_cast<unsigned>(field) & static_cast<unsigned>(flag)) == static_cast<unsigned>(flag);
    }

    /*
     * Returns a bit-field containing only the intersecting flags of the operands
     */
    template <typename Bitfield>
    CONST constexpr inline NODISCARD Bitfield intersect_flags(Bitfield field0, Bitfield field1) noexcept
    {
        return static_cast<Bitfield>(static_cast<unsigned>(field0) & static_cast<unsigned>(field1));
    }

    /*
     * Returns a bit-field containing all flags set in either of the operands
     */
    template <typename Bitfield>
    CONST constexpr inline NODISCARD Bitfield combine_flags(Bitfield field0, Bitfield field1) noexcept
    {
        return static_cast<Bitfield>(static_cast<unsigned>(field0) | static_cast<unsigned>(field1));
    }

    template <typename T, typename R, typename... Args>
    std::function<R(const T&)> toFunction(R (T::*memberFunc)(Args...) const, Args&&... args)
    {
        return [memberFunc, args...](const T& val) -> R { return (val.*memberFunc)(std::forward<Args>(args)...); };
    }

    CONST constexpr inline bool isPowerTwo(uint32_t val) noexcept
    {
        // https://en.wikipedia.org/wiki/Power_of_two#Fast_algorithm_to_check_if_a_positive_number_is_a_power_of_two
        return val > 0 && (val & (val - 1)) == 0;
    }

    /*
     * To explicitly ignore return values of NODISCARD functions
     */
    template <typename T>
    inline void ignoreReturnValue(T&& val) noexcept
    {
        (void) val;
    }
} // namespace vc4c

#endif /* HELPER_H */
