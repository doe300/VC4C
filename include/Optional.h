/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_OPTIONAL_H
#define VC4C_OPTIONAL_H

#include "CompilationError.h"

#include <functional>
#include <type_traits>
#if __has_include(<optional>) && __cplusplus >= 201703L
#include <optional>
template <typename T>
using OptionalBase = std::optional<T>;
#elif __has_include(<experimental/optional>)
#include <experimental/optional>
template <typename T>
using OptionalBase = std::experimental::optional<T>;
#endif

namespace vc4c
{
    /*
     * Implementation of an optional value.
     *
     * This class extends the functionality of std::(experimental::)optional
     */
    template <typename T>
    class Optional : public OptionalBase<T>
    {
        using Base = OptionalBase<T>;

    public:
        constexpr Optional() : Base() {}
        constexpr Optional(const T& value) : Base(value) {}
        constexpr Optional(T&& value) : Base(value) {}
        constexpr Optional(const Optional<T>& other) : Base(other) {}
        constexpr Optional(Optional<T>&& other) : Base(other) {}

        Optional& operator=(const Optional&) = default;
        Optional& operator=(Optional&&) = default;

        T value_or(const T& defaultValue) const
        {
            return has_value() ? **this : std::forward<const T>(defaultValue);
        }

        bool is(const T& value) const
        {
            return has_value() && **this == value;
        }

        Optional<T> orOther(const Optional<T>& other) const
        {
            return has_value() ? **this : other;
        }

        std::string to_string() const
        {
            return has_value() ? (*this)->to_string() : "-";
        }

        bool ifPresent(const std::function<bool(const T&)>& predicate) const
        {
            return has_value() && predicate(**this);
        }

        template <typename R>
        Optional<R> map(const std::function<R(const T&)>& mapper, const Optional<R>& defaultValue = {}) const
        {
            if(has_value())
                return Optional<R>(mapper(**this));
            return defaultValue;
        }

#if __cplusplus < 201703L
        bool has_value() const
        {
            return static_cast<bool>(*this);
        }
#endif
    };

    /*
     * Helper basis type to assert a type not being copyable
     */
    struct NonCopyable
    {
        constexpr NonCopyable() = default;
        ~NonCopyable() = default;

        NonCopyable(NonCopyable&&) noexcept = default;
        NonCopyable& operator=(NonCopyable&&) noexcept = default;
        NonCopyable(const NonCopyable&) = delete;
        NonCopyable& operator=(const NonCopyable&) = delete;
    };
}

#endif /* VC4C_OPTIONAL_H */
