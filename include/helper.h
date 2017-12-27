/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef HELPER_H
#define HELPER_H

#include <functional>
#include <vector>

#include "CompilationError.h"

namespace vc4c
{
	template<typename T, typename VT = std::vector<T>>
	inline const std::string to_string(const VT& values, const std::string& separator = ", ")
	{
	    std::string tmp;
	    for(const T& val : values)
	    {
	        tmp.append(val.to_string()).append(separator);
	    }
	    return tmp.substr(0, tmp.size() - separator.size());
	}

	template<>
	inline const std::string to_string<std::string>(const std::vector<std::string>& values, const std::string& separator)
	{
		std::string tmp;
		for(const std::string& val : values)
		{
			tmp.append(val).append(separator);
		}
		return tmp.substr(0, tmp.size() - separator.size());
	}

	template<typename T>
	inline const T* checkPointer(const T* ptr)
	{
		if(ptr == nullptr)
			throw CompilationError(CompilationStep::GENERAL, "Tried to access a null-pointer!");
		return ptr;
	}

	template<typename T>
	inline T* checkPointer(T* ptr)
	{
		if(ptr == nullptr)
			throw CompilationError(CompilationStep::GENERAL, "Tried to access a null-pointer!");
		return ptr;
	}

	template<typename From, typename To>
	inline To bit_cast(From in)
	{
	    static_assert(sizeof(From) == sizeof(To), "Can't convert types from different sizes!");
	    union {
	        From f;
	        To t;
	    }bit_cast;
	    bit_cast.f = in;
	    return bit_cast.t;
	}

	template<typename Bitfield>
	inline __attribute__((warn_unused_result)) Bitfield add_flag(Bitfield orig, Bitfield flag)
	{
	    return static_cast<Bitfield>(static_cast<unsigned>(orig) | static_cast<unsigned>(flag));
	}

	template<typename Bitfield>
	inline __attribute__((warn_unused_result)) Bitfield remove_flag(Bitfield orig, Bitfield flag)
	{
	    return static_cast<Bitfield>(static_cast<unsigned>(orig) & ~static_cast<unsigned>(flag));
	}

	template<typename Bitfield>
	inline bool has_flag(Bitfield field, Bitfield flag)
	{
	    return (static_cast<unsigned>(field) & static_cast<unsigned>(flag)) == static_cast<unsigned>(flag);
	}

	template<typename Bitfield>
	inline __attribute__((warn_unused_result)) Bitfield intersect_flags(Bitfield field0, Bitfield field1)
	{
		return static_cast<Bitfield>(static_cast<unsigned>(field0) & static_cast<unsigned>(field1));
	}

	template<typename Bitfield>
	inline __attribute__((warn_unused_result)) Bitfield combine_flags(Bitfield field0, Bitfield field1)
	{
		return static_cast<Bitfield>(static_cast<unsigned>(field0) | static_cast<unsigned>(field1));
	}

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

	    bool is(const T& val) const
	    {
	    	return hasValue && val == val;
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

	struct NonCopyable
	{
		constexpr NonCopyable() = default;
		~NonCopyable() = default;

		NonCopyable(NonCopyable&&) noexcept = default;
		NonCopyable& operator=(NonCopyable&&) noexcept = default;
		NonCopyable(const NonCopyable&) = delete;
		NonCopyable& operator=(const NonCopyable&) = delete;
	};

	// Variadic captures are not supported by Raspbian GCC
//	template<typename T, typename R, typename... Args>
//	std::function<R(const T&)> toFunction(R (T::*memberFunc)(Args...) const, Args&&... args)
//	{
//		return [memberFunc,args...](const T& val) -> R
//		{
//			return (val.*memberFunc)(std::forward<Args>(args)...);
//		};
//	}

	template<typename T, typename R>
	std::function<R(const T&)> toFunction(R (T::*memberFunc)() const)
	{
		return [memberFunc](const T& val) -> R
		{
			return (val.*memberFunc)();
		};
	}

	template<typename T, typename R, typename Arg>
	std::function<R(const T&)> toFunction(R (T::*memberFunc)(Arg) const, Arg&& arg)
	{
		return [memberFunc,arg](const T& val) -> R
		{
			return (val.*memberFunc)(std::forward<Arg>(arg));
		};
	}

	template<typename T, typename R, typename Arg0, typename Arg1>
	std::function<R(const T&)> toFunction(R (T::*memberFunc)(Arg0, Arg1) const, Arg0&& arg0, Arg1&& arg1)
	{
		return [memberFunc,arg0, arg1](const T& val) -> R
		{
			return (val.*memberFunc)(std::forward<Arg0>(arg0), std::forward<Arg1>(arg1));
		};
	}

	template<typename T, typename R, typename Arg0, typename Arg1, typename Arg2>
	std::function<R(const T&)> toFunction(R (T::*memberFunc)(Arg0,Arg1,Arg2) const, Arg0&& arg0, Arg1&& arg1, Arg2&& arg2)
	{
		return [memberFunc,arg0,arg1,arg2](const T& val) -> R
		{
			return (val.*memberFunc)(std::forward<Arg0>(arg0), std::forward<Arg1>(arg1), std::forward<Arg2>(arg2));
		};
	}


} // namespace vc4c

#endif /* HELPER_H */

