/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef HELPER_H
#define HELPER_H

#include <functional>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "CompilationError.h"
#include "Optional.h"

#if __cplusplus > 201402L
#define NODISCARD [[nodiscard]]
#else
#define NODISCARD __attribute__((warn_unused_result))
#endif

#if __cplusplus > 201402L
#define FALL_THROUGH [[fallthrough]];
#elif defined(__clang_major__) && (__clang_major__ >= 4 || (__clang_major__ == 3 && __clang_minor__ == 9))
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
        typename std::enable_if<!std::is_arithmetic<T>::value && !std::is_pointer<T>::value, void*>::type = nullptr>
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

    template <typename T, typename VT = std::vector<T>,
        typename std::enable_if<std::is_pointer<T>::value, void*>::type = nullptr>
    inline std::string to_string(const VT& values, const std::string& separator = ", ")
    {
        std::string tmp;
        for(auto val : values)
        {
            tmp.append(val->to_string()).append(separator);
        }
        return tmp.substr(0, tmp.size() - separator.size());
    }
    /*
     * Converts a container (defaults to std::vector) of values to a single string separated by the given string using
     * the given custom converter.
     */
    template <typename T, typename VT = std::vector<T>>
    inline std::string to_string(
        const VT& values, const std::function<std::string(const T&)>& func, const std::string& separator = ", ")
    {
        std::string tmp;
        for(const T& val : values)
        {
            tmp.append(func(val)).append(separator);
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
        union
        {
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

    template <typename Return, typename Argument>
    std::unique_ptr<Return> staticPointerCast(std::unique_ptr<Argument>&& arg)
    {
        if(auto val = dynamic_cast<Return*>(arg.get()))
        {
            arg.release();
            return std::unique_ptr<Return>{val};
        }
        throw CompilationError(CompilationStep::GENERAL, "Failed to cast unique_ptr to expected type!");
    }

    template <typename Return, typename Argument>
    std::unique_ptr<Return> dynamicPointerCast(std::unique_ptr<Argument>& arg)
    {
        if(auto val = dynamic_cast<Return*>(arg.get()))
        {
            arg.release();
            return std::unique_ptr<Return>{val};
        }
        return nullptr;
    }

    template <typename Signature>
    using FunctionPointer = std::add_pointer_t<Signature>;

    /**
     * Helper type to allow short-cutting checks on pointers, similar to the operators supported for Optional
     */
    template <typename T>
    struct Pointer
    {
        T* ptr;

        explicit constexpr operator bool() const noexcept
        {
            return ptr;
        }

        constexpr operator T*() const noexcept
        {
            return ptr;
        }

        std::string to_string() const
        {
            return ptr ? ptr->to_string() : "-";
        }

        // These operators are defined as member operators to not interfere with any other operator, since any Type is
        // implicitly convertible to Pointer<Type>
        bool operator&(const std::function<bool(const T&)>& func) const
        {
            return ptr && func(*ptr);
        }

        template <typename R>
        Pointer<R> operator&(const std::function<Pointer<R>(const T&)>& func) const
        {
            return ptr ? func(*ptr) : Pointer<R>{};
        }

        template <typename R>
        Pointer<R> operator&(const std::function<R*(const T&)>& func) const
        {
            return Pointer<R>{ptr ? func(*ptr) : nullptr};
        }

        template <typename R>
        Optional<R> operator&(const std::function<R(const T&)>& func) const
        {
            return ptr ? func(*ptr) : Optional<R>{};
        }

        template <typename S = T>
        typename std::enable_if<std::is_class<S>::value, bool>::type operator&(bool (S::*func)() const) const
        {
            return ptr && ((*ptr).*func)();
        }

        template <typename R, typename S = T>
        typename std::enable_if<std::is_class<S>::value, Pointer<R>>::type operator&(
            Pointer<R> (S::*func)() const) const
        {
            return ptr ? ((*ptr).*func)() : Pointer<R>{};
        }

        template <typename R, typename S = T>
        typename std::enable_if<std::is_class<S>::value, Pointer<R>>::type operator&(R* (S::*func)() const) const
        {
            return Pointer<R>{ptr ? ((*ptr).*func)() : nullptr};
        }

        template <typename R, typename S = T>
        typename std::enable_if<std::is_class<S>::value, Optional<R>>::type operator&(R (S::*func)() const) const
        {
            return ptr ? ((*ptr).*func)() : Optional<R>{};
        }

        template <typename R, typename S = T>
        typename std::enable_if<std::is_class<S>::value, Optional<R>>::type operator&(
            Optional<R> (S::*func)() const) const
        {
            return ptr ? ((*ptr).*func)() : Optional<R>{};
        }

        const Pointer<T>& operator|(const Pointer<T>& other) const&
        {
            return ptr ? *this : other;
        }

        Pointer<T> operator|(Pointer<T>&& other) &&
        {
            return ptr ? *this : other;
        }

        constexpr bool operator==(const Pointer& other) const noexcept
        {
            return ptr == other.ptr;
        }

        constexpr bool operator==(const T* other) const noexcept
        {
            return ptr == other;
        }

        constexpr bool operator==(std::nullptr_t other) const noexcept
        {
            return ptr == other;
        }

        constexpr bool operator!=(const Pointer& other) const noexcept
        {
            return ptr != other.ptr;
        }

        constexpr bool operator!=(const T* other) const noexcept
        {
            return ptr != other;
        }

        constexpr bool operator!=(std::nullptr_t other) const noexcept
        {
            return ptr != other;
        }

        constexpr bool operator!() const noexcept
        {
            return !ptr;
        }

        constexpr const T& operator*() const noexcept
        {
            return *ptr;
        }

        constexpr const T* operator->() const noexcept
        {
            return ptr;
        }

        constexpr T& operator*() noexcept
        {
            return *ptr;
        }

        constexpr T* operator->() noexcept
        {
            return ptr;
        }

        constexpr T* get() const noexcept
        {
            return ptr;
        }
    };

    template <typename T>
    inline Pointer<T> check(T* ptr) noexcept
    {
        return Pointer<T>{ptr};
    }

    static_assert(std::is_trivial<Pointer<int32_t>>::value, "");
    static_assert(std::is_literal_type<Pointer<int32_t>>::value, "");
    static_assert(sizeof(Pointer<int32_t>) == sizeof(int32_t*), "");
} // namespace vc4c

#endif /* HELPER_H */
