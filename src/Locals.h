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
	/*
	 * Base-class for all objects (currently only Instructions) using locals.
	 *
	 * A LocalUser automatically tracks the used locals and provides easy access to them.
	 */
	struct LocalUser
	{
		/*
		 * The type of the local-use
		 */
		enum class Type
		{
			NONE = 0,
			/*
			 * The user reads the local
			 */
			READER = 1,
			/*
			 * The user writes the local
			 */
			WRITER = 2,
			/*
			 * The user reads and writes the local
			 */
			BOTH = 3
		};

		LocalUser() = default;
		LocalUser(const LocalUser&) = delete;
		LocalUser(LocalUser&&) = delete;
		virtual ~LocalUser() = default;

		LocalUser& operator=(const LocalUser&) = delete;
		LocalUser& operator=(LocalUser&&) = delete;

		/*
		 * Returns all locals used by this object.
		 *
		 * NOTE: This function always assembles a new map-object. For performance-reasons, one of the other accessor-functions should be preferred.
		 */
		virtual FastMap<const Local*, Type> getUsedLocals() const = 0;
		/*
		 * Executes the given consumer for all locals used by this object.
		 */
		virtual void forUsedLocals(const std::function<void(const Local*, LocalUser::Type)>& consumer) const = 0;
		/*
		 * Whether this object reads the given local
		 */
		virtual bool readsLocal(const Local* local) const;
		/*
		 * Whether this object writes to the given local
		 */
		virtual bool writesLocal(const Local* local) const;
		/*
		 * Replaces all uses of the Local given in oldLocal with the Local in newLocal.
		 *
		 * The optional type parameter specifies to e.g. only replace reads or writes of the oldLocal. By default, all accesses are replaced
		 */
		virtual void replaceLocal(const Local* oldLocal, const Local* newLocal, Type type = Type::BOTH) = 0;

		virtual std::string to_string() const = 0;
	};

	class Method;

	/*
	 * Represents a edge between a Local and a LocalUser.
	 *
	 * Since a LocalUser can use the same Local "several times" (e.g. uses it as both operands), the number of reads and writes need to be tracked.
	 */
	struct LocalUse
	{
		uint32_t numWrites = 0;
		uint32_t numReads = 0;

		/*
		 * Whether this use is a writing use
		 */
		bool writesLocal() const
		{
			return numWrites > 0;
		}

		/*
		 * Whether the corresponding user reads the corresponding Local
		 */
		bool readsLocal() const
		{
			return numReads > 0;
		}
	};

	/*
	 * A Local is a Value stored in a (name) variable and represents any Value which is neither a Register, a constant value nor a compound constant value.
	 *
	 * "Default" locals are a representation of function-private variables and are mapped to phyiscal registers in the code-generator.
	 *
	 * Similarly to LocalUsers tracking the Locals used, Locals track their users. This allows for easier finding of reading/writing access to locals.
	 */
	class Local : private NonCopyable
	{
	public:
		Local(const Local&) = delete;
		Local(Local&&) = default;
		virtual ~Local() = default;

		Local& operator=(const Local&) = delete;
		Local& operator=(Local&&) = delete;
		bool operator<(const Local& other) const;
		bool operator==(const Local& other) const;

		/*
		 * Creates a Value referencing this Local at the given index (e.g. for compound-types).
		 *
		 * An index of WHOLE_OBJECT creates a reference to the whole Local
		 */
		const Value createReference(int index = WHOLE_OBJECT) const;

		/*
		 * Returns all the LocalUsers accessing this object
		 */
		const OrderedMap<const LocalUser*, LocalUse>& getUsers() const;
		/*
		 * Returns the users of the given kind (reading or writing) accessing this Local
		 */
		FastSet<const LocalUser*> getUsers(LocalUser::Type type) const;
		/*
		 * Executes the consumer for all users of the type specified
		 */
		void forUsers(const LocalUser::Type type, const std::function<void(const LocalUser*)>& consumer) const;
		/*
		 * Removes an instance of use for the given user and usage-type.
		 *
		 * If a user e.g. reads a Local several times, it needs to be removed as reader the correct number of times to be completely removed as reader.
		 */
		void removeUser(const LocalUser& user, LocalUser::Type type);
		/*
		 * Adds an instance of use for the given user and usage-type.
		 *
		 * A usage needs to be added the correct times the local is actually read (e.g. twice as reader when used in both operands and once as writer when also written to)
		 */
		void addUser(const LocalUser& user, LocalUser::Type type);
		/*
		 * Returns the only instruction writing to this local, if there is exactly one
		 */
		const LocalUser* getSingleWriter() const;

		/*
		 * Whether this local has the given sub-type
		 */
		template<typename T>
		bool is() const
		{
			return dynamic_cast<const T*>(this) != nullptr;
		}

		/*
		 * Casts this local to the given sub-type
		 */
		template<typename T>
		const T* as() const
		{
			return dynamic_cast<const T*>(this);
		}

		/*
		 * Casts this local to the given sub-type
		 */
		template<typename T>
		T* as()
		{
			return dynamic_cast<T*>(this);
		}

		virtual std::string to_string(bool withContent = false) const;

		/*
		 * Whether this local is stored in memory.
		 *
		 * "Default" locals are always stored in registers
		 */
		virtual bool residesInMemory() const;

		/*
		 * Returns the base local, this local refers to.
		 * Returns this, if this local does not refer any other local
		 *
		 * \param includeOffsets whether to also follow references with offsets != 0
		 */
		const Local* getBase(bool includeOffsets) const;

		/*
		 * The type of the data represented by this local
		 */
		const DataType type;
		/*
		 * The name of this object
		 */
		const std::string name;
		/*
		 * Another local (e.g. parameter, global) referenced by this local with the index, if it is a scalar index, otherwise ANY_ELEMENT.
		 *
		 * Locals referring to each other is used for calculation of indices, where the index (or pointer to an element) refers to the original object for easier tracking of memory accessed.
		 */
		const std::pair<Local*, int> reference;
	protected:
		Local(const DataType& type, const std::string& name);
	private:
		//FIXME unordered_map randomly throws SEGFAULT somewhere in stdlib in #removeUser called by IntermediateInstruction#erase
		OrderedMap<const LocalUser*, LocalUse> users;

		friend class Method;
	};

	/*
	 * Additional information which can be present on a parameter
	 */
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

	/*
	 * Specialization of a Local which is passed as parameter to a function
	 */
	struct Parameter : public Local
	{
		Parameter(const std::string& name, const DataType& type, ParameterDecorations decorations = ParameterDecorations::NONE);
		Parameter(Parameter&&) = default;
		~Parameter() override;

		/*
		 * Whether this parameter is read from, only meaningful for pointer-types
		 */
		bool isInputParameter() const;
		/*
		 * Whether this parameter is written to, only meaningful for pointer-types
		 */
		bool isOutputParameter() const;

		/*
		 * The decorations for this parameter
		 */
		ParameterDecorations decorations;
		/*
		 * For pointer-types, this value specifies the maximum offset in bytes from the base-address this parameter is accessed.
		 *
		 * In other words: Any access to this parameter is guaranteed to lie in the offset-range [0, maxByteOffset].
		 * This value can be used to determine the "size" of the object stored in the pointed-to location and whether it can be loaded into VPM completely.
		 */
		std::size_t maxByteOffset = SIZE_MAX;
		/*
		 * The "original" parameter-name from the source-code.
		 *
		 * This value is only used by the VC4CL host-library to give as information in clGetKernelArgInfo and defaults to the Parameter's name
		 */
		std::string parameterName;
		/*
		 * the "original" type-name from the source-code.
		 *
		 * This value is only used by the VC4CL host-library to give as information in clGetKernelArgInfo and defaults to the to_string() representation of the data-type
		 */
		std::string origTypeName;
	};

	/*
	 * Global data, can be accessed by all kernel-functions in a module and is consistent across kernel-invocations.
	 * The global data segment is part of the module binary code and contains the initial values for all global data.
	 */
	struct Global : public Local
	{
		Global(const std::string& name, const DataType& globalType, const Value& value, bool isConstant);
		Global(Global&&) = default;
		~Global() override = default;

		std::string to_string(bool withContent = false) const override;

		/*
		 * Returns true, since global data resides in memory
		 */
		bool residesInMemory() const override;

		/*
		 * The initial value, usually defaults to a zero-initializer
		 */
		Value value;

		/*
		 * Whether this global value is a constant
		 */
		const bool isConstant;
	};

	/*
	 * Allocation on the "stack".
	 *
	 * Since call stacks are not supported, the allocations are located on a special area of the global data.
	 * Contrary to global data, stack-allocated locals have a finite life-time and need to be located in separate memory areas for each QPU,
	 * since their values are not shared between the kernel invocations, but private to the QPU.
	 */
	struct StackAllocation : public Local
	{
		StackAllocation(const std::string& name, const DataType& type, std::size_t size = 0, std::size_t alignment = 1);
		StackAllocation(StackAllocation&&) = default;
		~StackAllocation() override = default;

		/*
		 * Since the "stack" is located in memory, so are allocations on it
		 */
		bool residesInMemory() const override;

		/*
		 * The offset from the start of the stack-allocations area (per QPU).
		 *
		 * E.g. the first stack-allocation has a per-QPU offset of zero, the second the size of the first, etc.
		 */
		std::size_t offset;
		/*
		 * The alignment of the data, in bytes.
		 *
		 * The alignment defaults to the size of the data stored
		 */
		const std::size_t alignment;
		/*
		 * The size of the data (for an execution), in bytes.
		 *
		 * If not set, the alignment defaults to 1 byte
		 */
		std::size_t size;
	};

	/*
	 * Orders stack-allocations by their alignment (largest first) and their name.
	 *
	 * This ordering is used to position stack-allocations in a way to waste as little memory as possible on aligning stack-allocations
	 */
	struct order_by_alignment_and_name
	{
		bool operator()(const StackAllocation& sa1, const StackAllocation& sa2) const;
	};


} /* namespace vc4c */

#endif /* LOCALS_H */
