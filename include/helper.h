/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef HELPER_H
#define HELPER_H

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

	template<typename T>
	class Optional
	{
	public:
	    Optional() : hasValue(false) { }
	    Optional(const T& value) : hasValue(true), value(value) { }
	    Optional(T&& value) : hasValue(true), value(value) { }

	    Optional(bool hasValue, const T& dummyValue = {}) : hasValue(hasValue), value(dummyValue) { }

	    Optional<T>& operator=(const T& value)
	    {
	        hasValue = true;
	        this->value = value;
	        return *this;
	    }

	    //"contextual conversion operator"
	    explicit operator bool() const
		{
			return hasValue;
		}

	    operator T() const
	    {
	        return get();
	    }

	    operator T()
	    {
	        return get();
	    }

	    const T& get() const
	    {
	        if(!hasValue)
	            throw CompilationError(CompilationStep::GENERAL, "Invalid read of optional value!");
	        return value;
	    }

	    T& get()
	    {
	        if(!hasValue)
	            throw CompilationError(CompilationStep::GENERAL, "Invalid read of optional value!");
	        return value;
	    }

	    T orElse(const T& other) const
	    {
	        return hasValue ? value : other;
	    }

	    Optional<T> orOther(const Optional<T>& other) const
		{
	    	return hasValue ? *this : other;
		}

	    std::string to_string() const
	    {
	        return hasValue ? value.to_string() : "-";
	    }

	    bool hasValue;
	private:
	    T value;
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
} // namespace vc4c

#endif /* HELPER_H */

