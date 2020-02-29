/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LOCALS_H
#define LOCALS_H

#include "CompilationError.h"
#include "Values.h"

#include <functional>
#include <memory>
#include <utility>

namespace vc4c
{
    /*
     * Represents a edge between a Local and a LocalUser.
     *
     * Since a LocalUser can use the same Local "several times" (e.g. uses it as both operands), the number of reads and
     * writes need to be tracked.
     */
    struct LocalUse
    {
        /*
         * The type of the local-use
         */
        enum class Type : unsigned char
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

        uint32_t numWrites = 0;
        uint32_t numReads = 0;

        /*
         * Whether this use is a writing use
         */
        bool writesLocal() const noexcept
        {
            return numWrites > 0;
        }

        /*
         * Whether the corresponding user reads the corresponding Local
         */
        bool readsLocal() const noexcept
        {
            return numReads > 0;
        }
    };

    /*
     * Base class for additional data associated with Locals.
     *
     * The data contained here is only present/required in a small subset of Locals and therefore keeping it in the
     * Local type itself would waste memory. Also, depending on the type of Local (e.g. the DataType stored), different
     * types of additional information might be of interest.
     */
    struct LocalData
    {
        LocalData() = default;
        LocalData(const LocalData&) = default;
        LocalData(LocalData&&) noexcept = default;
        virtual ~LocalData() noexcept;

        LocalData& operator=(const LocalData&) = default;
        LocalData& operator=(LocalData&&) noexcept = default;

        virtual std::string to_string() const;
    };

    /*
     * A Local is a Value stored in a (named) variable and represents any Value which is neither a Register, a constant
     * value nor a compound constant value.
     *
     * "Default" locals are a representation of function-private variables and are mapped to physical registers in the
     * code-generator.
     *
     * Similarly to LocalUsers tracking the Locals used, Locals track their users. This allows for easier finding of
     * reading/writing access to locals.
     */
    class Local : private NonCopyable
    {
    public:
        Local(const Local&) = delete;
        Local(Local&&) noexcept = default;
        virtual ~Local() noexcept = default;

        Local& operator=(const Local&) = delete;
        Local& operator=(Local&&) noexcept = delete;

        bool operator<(const Local& other) const noexcept;
        bool operator==(const Local& other) const noexcept;

        /*
         * Creates a Value referencing this Local.
         */
        Value createReference() const;

        /*
         * Returns all the LocalUsers accessing this object
         */
        const SortedMap<const LocalUser*, LocalUse>& getUsers() const;
        /*
         * Returns the users of the given kind (reading or writing) accessing this Local
         */
        FastSet<const LocalUser*> getUsers(LocalUse::Type type) const;
        /*
         * Executes the consumer for all users of the type specified
         */
        void forUsers(LocalUse::Type type, const std::function<void(const LocalUser*)>& consumer) const;
        /*
         * Removes an instance of use for the given user and usage-type.
         *
         * If a user e.g. reads a Local several times, it needs to be removed as reader the correct number of times to
         * be completely removed as reader.
         */
        void removeUser(const LocalUser& user, LocalUse::Type type);
        /*
         * Adds an instance of use for the given user and usage-type.
         *
         * A usage needs to be added the correct times the local is actually read (e.g. twice as reader when used in
         * both operands and once as writer when also written to)
         */
        void addUser(const LocalUser& user, LocalUse::Type type);
        /*
         * Returns the only instruction writing to this local, if there is exactly one
         */
        const LocalUser* getSingleWriter() const;

        /*
         * Whether this local has the given sub-type
         */
        template <typename T>
        bool is() const
        {
            return dynamic_cast<const T*>(this) != nullptr;
        }

        /*
         * Casts this local to the given sub-type
         */
        template <typename T>
        const T* as() const
        {
            return dynamic_cast<const T*>(this);
        }

        /*
         * Casts this local to the given sub-type
         */
        template <typename T>
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

        template <typename T>
        T* get()
        {
            return dynamic_cast<T*>(data.get());
        }

        template <typename T>
        const T* get() const
        {
            return dynamic_cast<const T*>(data.get());
        }

        template <typename T>
        T& set(T&& val)
        {
            if(data && !get<T>())
                throw CompilationError(
                    CompilationStep::GENERAL, "Cannot overwrite LocalData of different type", to_string(true));
            data = std::make_unique<T>(std::move(val));
            return dynamic_cast<T&>(*data.get());
        }

        template <typename T>
        static const T* getLocalData(const Local* loc)
        {
            return loc ? loc->get<T>() : nullptr;
        }

        template <typename T>
        static T* getLocalData(Local* loc)
        {
            return loc ? loc->get<T>() : nullptr;
        }

        /*
         * The type of the data represented by this local
         */
        const DataType type;
        /*
         * The name of this object
         */
        std::string name;

    protected:
        Local(DataType type, const std::string& name);

        struct RAIILock
        {
            using UnlockFunc = std::function<void()>;
            explicit RAIILock(UnlockFunc&& f = nullptr) : func(std::move(f)) {}
            RAIILock(const RAIILock&) = delete;
            RAIILock(RAIILock&& other) noexcept : func(std::move(other.func))
            {
                other.func = nullptr;
            }
            ~RAIILock() noexcept;

            RAIILock& operator=(const RAIILock&) = delete;
            RAIILock& operator=(RAIILock&&) noexcept = delete;

        private:
            UnlockFunc func;
        };

        // To be implemented by locals shared across kernels (and therefore across threads) to prevent concurrent
        // modifications
        virtual RAIILock getUsersLock() const;

    private:
        // FIXME unordered_map randomly throws SEGFAULT somewhere in stdlib in #removeUser called by
        // IntermediateInstruction#erase
        SortedMap<const LocalUser*, LocalUse> users;
        // Additional data for this local
        std::unique_ptr<LocalData> data;

        friend class Method;
    };

    /*
     * Additional information which can be present on a parameter
     *
     * NOTE: The positions of the decorations MUST not be re-ordered or removed, since the bit-mask is directly
     * propagated to the VC4CL run-time. (Adding new entries at the end is allowed)
     */
    enum class ParameterDecorations : unsigned char
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
         * Parameter is not allowed to be aliased (e.g. two pointer parameter are not allowed to point to overlapping
         * memory regions). Only valid for pointers.
         */
        RESTRICT = 0x20,
        /*
         * Parameter points to volatile memory, accesses to this parameter cannot be reordered/eliminated/duplicated or
         * combined. Only valid for pointers.
         */
        VOLATILE = 0x40,
        /*
         * Equivalent of LLVM byval attribute. This is intended to indicate that a pointer parameter to be passed "by
         * value" and a copy should be created by the callee. This also implies the memory to be read-only and constant.
         *
         * We keep the parameter as a pointer to the source to not have to rewrite all accesses to it. Therefore, the
         * VC4CL run-time needs to convert the direct data (e.g. a struct) passed to the clSetKernelArg to a
         * pointer-to-data parameter.
         */
        BY_VALUE = 0x80
    };
    std::string toString(ParameterDecorations deco);

    /*
     * Specialization of a Local which is passed as parameter to a function
     */
    struct Parameter final : public Local
    {
        Parameter(
            const std::string& name, DataType type, ParameterDecorations decorations = ParameterDecorations::NONE);
        Parameter(Parameter&&) noexcept = default;
        ~Parameter() noexcept override = default;

        std::string to_string(bool withContent = false) const override;

        /*
         * Whether this parameter is read from, only meaningful for pointer-types and images
         */
        bool isInputParameter() const;
        /*
         * Whether this parameter is written to, only meaningful for pointer-types and images
         */
        bool isOutputParameter() const;

        /*
         * The decorations for this parameter
         */
        ParameterDecorations decorations;
        /*
         * For pointer-types, this value specifies the maximum offset in bytes from the base-address this parameter is
         * accessed.
         *
         * In other words: Any access to this parameter is guaranteed to lie in the offset-range [0, maxByteOffset].
         * This value can be used to determine the "size" of the object stored in the pointed-to location and whether it
         * can be loaded into VPM completely.
         */
        std::size_t maxByteOffset = SIZE_MAX;
        /*
         * The "original" parameter-name from the source-code.
         *
         * This value is only used by the VC4CL host-library to give as information in clGetKernelArgInfo and defaults
         * to the Parameter's name
         */
        std::string parameterName;
        /*
         * the "original" type-name from the source-code.
         *
         * This value is only used by the VC4CL host-library to give as information in clGetKernelArgInfo and defaults
         * to the to_string() representation of the data-type
         */
        std::string origTypeName;
        /*
         * Whether this parameter is lowered into shared VPM area. For lowered parameters, no buffer needs to be
         * allocated at run-time.
         *
         * NOTE: Only __local parameters (where the temporary buffers are managed by the OpenCL implementations) can be
         * lowered into VPM.
         */
        bool isLowered = false;
    };

    /*
     * Allocation on the "stack".
     *
     * Since call stacks are not supported, the allocations are located on a special area of the global data.
     * Contrary to global data, stack-allocated locals have a finite life-time and need to be located in separate memory
     * areas for each QPU, since their values are not shared between the kernel invocations, but private to the QPU.
     */
    struct StackAllocation final : public Local
    {
        StackAllocation(const std::string& name, DataType type, std::size_t size = 0, std::size_t alignment = 1);
        StackAllocation(StackAllocation&&) noexcept = default;
        ~StackAllocation() noexcept override = default;

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
        /*
         * Whether this stack allocation is lowered into VPM or register. For lowered stack allocations, no area in the
         * "constant" memory block needs to be reserved.
         */
        bool isLowered = false;
    };

    /*
     * Orders stack-allocations by their alignment (largest first) and their name.
     *
     * This ordering is used to position stack-allocations in a way to waste as little memory as possible on aligning
     * stack-allocations
     */
    struct order_by_alignment_and_name
    {
        bool operator()(const StackAllocation& sa1, const StackAllocation& sa2) const;
    };

    /**
     * Type for special locals which are not under the control of the source program but are inserted by the VC4C
     * compiler to access e.g. work-group information and global data locations.
     */
    class BuiltinLocal final : public Local
    {
    public:
        /**
         * Constants for the different types of builtin (e.g. work-group related) locals
         */
        enum class Type : unsigned char
        {
            WORK_DIMENSIONS = 0,
            LOCAL_SIZES,
            LOCAL_IDS,
            NUM_GROUPS_X,
            NUM_GROUPS_Y,
            NUM_GROUPS_Z,
            GROUP_ID_X,
            GROUP_ID_Y,
            GROUP_ID_Z,
            GLOBAL_OFFSET_X,
            GLOBAL_OFFSET_Y,
            GLOBAL_OFFSET_Z,
            GLOBAL_DATA_ADDRESS,
            UNIFORM_ADDRESS,
            MAX_GROUP_ID_X,
            MAX_GROUP_ID_Y,
            MAX_GROUP_ID_Z,
            // NOTE: This must be last!!
            NUM_ENTRIES
        };
        static constexpr auto NUM_LOCALS = static_cast<std::size_t>(Type::NUM_ENTRIES);

        BuiltinLocal(const std::string& name, DataType dataType, Type builtinType);
        BuiltinLocal(BuiltinLocal&&) noexcept = delete;
        ~BuiltinLocal() noexcept override;

        /**
         * The type of builtin represented by this local
         */
        const Type builtinType;
    };

    /**
     * Additional data type for locals which do not fit into a single registers, e.g. 64-bit values.
     *
     * This type tracks the "upper" and a "lower" parts which will be used for all the actual caculations.
     */
    struct MultiRegisterData : public LocalData
    {
        MultiRegisterData(const Local* lowerPart, const Local* upperPart);
        ~MultiRegisterData() noexcept override;

        std::string to_string() const override;

        /** The local storing the lower 32 bit of this local value */
        const Local* lower;

        /** The local storing the upper 32 bit of this local value */
        const Local* upper;
    };

    /**
     * Additional data type for locals which refer to one or multiple other locals (i.e. pointers to memory areas).
     *
     * This type tracks the local's pointed-to locals.
     */
    struct ReferenceData : public LocalData
    {
        ReferenceData(const Local& ref, int index);
        ~ReferenceData() noexcept override;

        std::string to_string() const override;

        /**
         * The local representing the (memory location) this pointer local refers to.
         *
         * This is never NULL.
         */
        const Local* base;
        /**
         * The offset (in elements of the pointed-to data).
         *
         * This might be ANY_ELEMENT, if it is not a constant literal.
         */
        int offset;
    };

} /* namespace vc4c */

#endif /* LOCALS_H */
