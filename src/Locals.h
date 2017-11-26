/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LOCALS_H
#define LOCALS_H

#include "Values.h"

#include <functional>
#include <utility>

namespace vc4c
{
	struct LocalUser
	{
		enum class Type
		{
			NONE = 0, READER = 1, WRITER = 2, BOTH = 3
		};

		LocalUser() = default;
		virtual ~LocalUser() = default;

		virtual FastMap<const Local*, Type> getUsedLocals() const = 0;
		virtual void forUsedLocals(const std::function<void(const Local*, LocalUser::Type)>& consumer) const = 0;
		virtual bool readsLocal(const Local* local) const;
		virtual bool writesLocal(const Local* local) const;
		virtual void replaceLocal(const Local* oldLocal, const Local* newLocal, Type type = add_flag(Type::READER, Type::WRITER)) = 0;

		virtual std::string to_string() const = 0;
	};

	class Method;

	struct LocalUse
	{
		uint32_t numWrites = 0;
		uint32_t numReads = 0;

		bool writesLocal() const
		{
			return numWrites > 0;
		}

		bool readsLocal() const
		{
			return numReads > 0;
		}
	};

	class Local : private NonCopyable
	{
	public:
		Local(const Local&) = delete;
		Local(Local&&) = default;
		virtual ~Local() = default;

		Local& operator=(const Local&) = delete;
		Local& operator=(Local&&) = default;
		bool operator<(const Local& other);
		bool operator==(const Local& other);

		const Value createReference(int index = WHOLE_OBJECT) const;

		const OrderedMap<const LocalUser*, LocalUse>& getUsers() const;
		FastSet<const LocalUser*> getUsers(LocalUser::Type type) const;
		void forUsers(const LocalUser::Type type, const std::function<void(const LocalUser*)>& consumer) const;
		void removeUser(const LocalUser& user, LocalUser::Type type);
		void addUser(const LocalUser& user, LocalUser::Type type);
		/*
		 * Returns the only instruction writing to this local, if there is exactly one
		 */
		const LocalUser* getSingleWriter() const;

		template<typename T>
		bool is() const
		{
			return dynamic_cast<const T*>(this) != nullptr;
		}

		template<typename T>
		const T* as() const
		{
			return dynamic_cast<const T*>(this);
		}

		template<typename T>
		T* as()
		{
			return dynamic_cast<T*>(this);
		}

		virtual std::string to_string(bool withContent = false) const;
		virtual bool residesInMemory() const;

		const DataType type;
		const std::string name;
		//Another local (e.g. parameter, global) referenced by this local with the index, if it is a scalar index, otherwise ANY_ELEMENT
		const std::pair<Local*, int> reference;
	protected:
		Local(const DataType& type, const std::string& name);
	private:
		//FIXME unordered_map randomly throws SEGFAULT somewhere in stdlib in #removeUser called by IntermediateInstruction#erase
		OrderedMap<const LocalUser*, LocalUse> users;

		friend class Method;
	};

	enum class ParameterDecorations
	{
		/*
		 * Absence of any other flag
		 */
		NONE = 0x0,
		/*
		 * Data is read through this parameter. Only valid for pointers.
		 */
		INPUT = 0x1,
		/*
		 * Data is written into this parameter. Only valid for pointers.
		 */
		OUTPUT = 0x2,
		/*
		 * Parameter must be zero-extended. Only valid for scalar values.
		 */
		ZERO_EXTEND = 0x4,
		/*
		 * Parameter must be sign-extended. Only valid for scalar values.
		 */
		SIGN_EXTEND = 0x8,
		/*
		 * Parameter is read-only / constant. Only valid for pointers.
		 */
		READ_ONLY = 0x10,
		/*
		 * Parameter is not allowed to be aliased (e.g. two pointer parameter are not allowed to point to overlapping memory regions). Only valid for pointers.
		 */
		RESTRICT = 0x20,
		/*
		 * Parameter points to volatile memory, accesses to this parameter cannot be reordered/eliminated/duplicated or combined. Only valid for pointers.
		 */
		VOLATILE = 0x30
	};

	struct Parameter : public Local
	{
		Parameter(const std::string& name, const DataType& type, ParameterDecorations decorations = ParameterDecorations::NONE);
		Parameter(Parameter&&) = default;
		~Parameter() override;

		bool isInputParameter() const;
		bool isOutputParameter() const;

		ParameterDecorations decorations;
		std::size_t maxByteOffset = SIZE_MAX;
		//the "real" parameter-name from the source-code
		std::string parameterName;
		//the "real" type-name from the source-code
		std::string origTypeName;
	};

	/*
	 * Global data, can be accessed by all kernel-functions in a module and is consistent across kernel-invocations.
	 * The global data segment is part of the module binary code and contains the initial values for all global data.
	 */
	struct Global : public Local
	{
		Global(const std::string& name, const DataType& globalType, const Value& value);
		Global(Global&&) = default;
		~Global() override = default;

		std::string to_string(bool withContent = false) const override;
		bool residesInMemory() const override;

		Value value;
	};

	/*
	 * Allocation on the "stack". Since call stacks are not supported, the allocations are located on a special area of the global data.
	 * Contrary to global data, stack-allocated locals have a finite life-time and need to be located in separate memory areas for each QPU,
	 * since their values are not shared between the kernel invocations, but private to the QPU.
	 */
	struct StackAllocation : public Local
	{
		StackAllocation(const std::string& name, const DataType& type, std::size_t size = 0, std::size_t alignment = 1);
		StackAllocation(StackAllocation&&) = default;
		~StackAllocation() override = default;

		bool residesInMemory() const override;

		//the offset from the start of the stack-allocations area (per QPU)
		std::size_t offset;
		//the alignment of the data, in bytes
		std::size_t alignment;
		//the size of the data (for an execution), in bytes
		std::size_t size;
	};

} /* namespace vc4c */

#endif /* LOCALS_H */
