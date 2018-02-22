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
#if __cplusplus >= 201703L
#include <optional>
#define OPTIONAL_BASE std::optional
#elif __cplusplus >= 201402L
#include <experimental/optional>
#define OPTIONAL_BASE std::experimental::optional
#endif

namespace vc4c
{
	/*
	 * Implementation of an optional value.
	 *
	 * This class is mostly compatible with C++17 std::optional and its used should be replaced by std::optional when C++17 support is enabled
	 */
	template<typename T>
	class Optional
	{
	public:
	    Optional() : hasValue(false)
		{
	    	static_assert(std::is_default_constructible<T>::value, "Cannot use default constructor on non default-constructible type!");
		}
	    Optional(const T& value) : hasValue(true), val(value)
	    {
	    	static_assert(std::is_copy_constructible<T>::value, "Cannot use copy constructor on non copy-constructible type!");
	    }
	    Optional(T&& value) : hasValue(true), val(value)  /* matches C++17 std::optional */
	    {
	    	static_assert(std::is_move_constructible<T>::value, "Cannot use move constructor on non move-constructible type!");
	    }
	    Optional(const Optional<T>& other) : hasValue(other.hasValue), val(other.val)  /* matches C++17 std::optional */
	    {
	    	static_assert(std::is_copy_constructible<T>::value, "Cannot use copy constructor on non copy-constructible type!");
	    }
	    Optional(Optional<T>&& other) : hasValue(other.hasValue), val(std::forward<T>(other.val))  /* matches C++17 std::optional */
		{
	    	static_assert(std::is_move_constructible<T>::value, "Cannot use move constructor on non move-constructible type!");
		}

	    Optional(bool hasValue, const T& dummyValue = {}) : hasValue(hasValue), val(dummyValue) { }

	    Optional<T>& operator=(const Optional<T>& other) /* matches C++17 std::optional */
	    {
	    	static_assert(std::is_copy_assignable<T>::value, "Cannot use copy assignment operator on non copy-assignable type!");
	    	hasValue = other.hasValue;
	    	val = other.val;
	    	return *this;
	    }

	    Optional<T>& operator=(Optional<T>&& other) /* matches C++17 std::optional */
		{
	    	static_assert(std::is_move_assignable<T>::value, "Cannot use move assignment operator on non copy-assignable type!");
			hasValue = other.hasValue;
			val = other.val;
			return *this;
		}

	    Optional<T>& operator=(const T& value) /* matches C++17 std::optional */
	    {
	    	static_assert(std::is_copy_assignable<T>::value, "Cannot use copy assignment operator on non copy-assignable type!");
	        hasValue = true;
	        this->val = value;
	        return *this;
	    }

	    Optional<T>& operator=(T&& value) /* matches C++17 std::optional */
	    {
	    	static_assert(std::is_move_assignable<T>::value, "Cannot use move assignment operator on non copy-assignable type!");
	    	hasValue = true;
	    	this->val = std::forward<T>(value);
	    	return *this;
	    }

	    //"contextual conversion operator"
	    explicit operator bool() const /* matches C++17 std::optional */
		{
			return hasValue;
		}

	    bool has_value() const /* matches C++17 std::optional */
	    {
	    	return hasValue;
	    }

	    T& value() /* matches C++17 std::optional */
	    {
	    	if(!hasValue)
				throw CompilationError(CompilationStep::GENERAL, "Invalid read of optional value!");
			return val;
	    }

	    const T& value() const /* matches C++17 std::optional */
	    {
	    	if(!hasValue)
				throw CompilationError(CompilationStep::GENERAL, "Invalid read of optional value!");
			return val;
	    }

	    T value_or(T&& defaultValue) const /* matches C++17 std::optional */
	    {
	    	return hasValue ? val : std::forward<T>(defaultValue);
	    }

	    T value_or(const T& defaultValue) const
	    {
	    	return hasValue ? val : std::forward<const T>(defaultValue);
	    }

	    const T* operator->() const /* matches C++17 std::optional */
	    {
	    	return &value();
	    }

	    T* operator->() /* matches C++17 std::optional */
	    {
	    	return &value();
	    }

	    const T& operator*() const /* matches C++17 std::optional */
	    {
	    	return value();
	    }

	    T& operator*() /* matches C++17 std::optional */
	    {
	    	return value();
	    }

	    bool is(const T& value) const
	    {
	    	return hasValue && this->val == value;
	    }

	    Optional<T> orOther(const Optional<T>& other) const
		{
	    	return hasValue ? *this : other;
		}

	    std::string to_string() const
	    {
	        return hasValue ? val.to_string() : "-";
	    }

	    bool ifPresent(const std::function<bool(const T&)>& predicate) const
	    {
	    	return hasValue && predicate(val);
	    }

	    template<typename R>
	    Optional<R> map(const std::function<R(const T&)>& mapper, const Optional<R>& defaultValue = {}) const
		{
	    	if(hasValue)
	    		return Optional<R>(mapper(val));
	    	return defaultValue;
		}

	private:
	    bool hasValue;
	    T val;
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

