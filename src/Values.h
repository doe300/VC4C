/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VALUES_H
#define VALUES_H

#include "BitMask.h"
#include "Optional.h"
#include "Register.h"
#include "Types.h"
#include "Variant.h"
#include "config.h"

#include <functional>
#include <string>

namespace vc4c
{
    /*
     * The arithmetic type of a literal value
     */
    enum class LiteralType : unsigned char
    {
        INTEGER,
        REAL,
        BOOL,
        // "literal type" indicating no literal present
        TOMBSTONE,
        // special version of "INTEGER" which indicates that the upper word are all ones
        LONG_LEADING_ONES,
    };

    struct SmallImmediate;

    /*
     * A literal value (a constant), directly used as operand.
     *
     * Literals cannot be handled by machine-code and must be converted before code-generation.
     * While "smaller" literal values can be directly used by ALU instructions in form of SmallImmediates,
     * "larger" values need to be loaded via a load-immediate instruction.
     */
    class Literal
    {
    public:
        LiteralType type;

        explicit constexpr Literal(optional_tombstone_tag) noexcept : type(LiteralType::TOMBSTONE), u() {}
        explicit constexpr Literal(int32_t integer) noexcept : type(LiteralType::INTEGER), i(integer) {}
        explicit constexpr Literal(uint32_t integer) noexcept : type(LiteralType::INTEGER), u(integer) {}
        explicit constexpr Literal(float real) noexcept : type(LiteralType::REAL), f(real) {}
        explicit constexpr Literal(bool flag) noexcept : type(LiteralType::BOOL), u(flag) {}
        ~Literal() = default;

        Literal(const Literal&) = default;
        Literal(Literal&&) noexcept = default;

        Literal& operator=(const Literal&) = default;
        Literal& operator=(Literal&&) noexcept = default;

        bool operator==(const Literal& other) const noexcept;
        inline bool operator!=(const Literal& other) const noexcept
        {
            return !(*this == other);
        }
        bool operator<(const Literal& other) const noexcept;

        std::string to_string() const;

        /*
         * Whether this literal represents the boolean value true
         */
        bool isTrue() const noexcept;
        /*
         * Bit-casts the stored value to a floating-point value
         */
        float real() const noexcept;
        /*
         * Bit-casts the stored value to a signed value
         */
        int32_t signedInt() const noexcept;
        /*
         * Bit-casts the stored value to an unsigned value
         */
        uint32_t unsignedInt() const noexcept;

        /*
         * Converts the stored value to a immediate-value which can be used in a load-immediate instruction.
         */
        uint32_t toImmediate() const noexcept;

        /*
         * A Literal is "undefined" if it is of TOMBSTONE type. the internal value can be any of the 2^32 possible
         * values
         */
        bool isUndefined() const noexcept;

        /**
         * Returns the mask of bits set
         */
        BitMask getBitMask() const noexcept;

    private:
        /*
         * The bit-wise representation of this literal
         */
        union
        {
            int32_t i;
            uint32_t u;
            float f;
        };

        static_assert(sizeof(int32_t) == sizeof(uint32_t) && sizeof(uint32_t) == sizeof(float),
            "Sizes of literal types do not match!");
    };

    /*
     * A literal value with undefined value, doubles as not-set literal for compact optionals
     */
    static constexpr Literal UNDEFINED_LITERAL{optional_tombstone_tag{}};

    template <>
    struct tombstone_traits<Literal>
    {
        static constexpr bool is_specialized = true;
        static constexpr Literal tombstone = Literal(optional_tombstone_tag{});

        static constexpr bool isTombstone(const Literal& val) noexcept
        {
            return val.type == LiteralType::TOMBSTONE;
        }
    };

    /**
     * Tries to convert the given 64-bit value to a literal.
     *
     * Returns an empty optional on failure
     */
    Optional<Literal> toLongLiteral(uint64_t val);

    /*!
     * A SmallImmediate value is a literal (constant) value which can be loaded directly into an ALU instruction.
     *
     * SmallImmediates are no "standard" integer values, but are mapped to represent some value (e.g. using a
     * SmallImmediate value of 16 actually loads the integer value -15). Additionally, some SmallImmediate values are
     * used to load floating-point constants as well as represent the offset for vector-rotations.
     *
     * "The 6 bit small immediate field encodes either an immediate integer/float value used in place of the register
     * file b input, or a vector rotation to apply to the mul ALU output, according to Table 5."
     * - Broadcom VideoCore IV specification, page 29
     */
    struct SmallImmediate : public InstructionPart<SmallImmediate>
    {
        /*
         * Creates an object with the given field-value (not the value actually loaded!)
         */
        explicit constexpr SmallImmediate(unsigned char val) noexcept : InstructionPart(val) {}

        std::string to_string() const;

        // the "real" values being loaded with this small immediate

        /*
         * Returns the integer-value represented by this object, if this object represents an integer value.
         *
         * The field-values 0 - 15 map to the integer values 0 - 15 correspondingly,
         * the field-values 16 - 31 map to the integer values -16 to -1.
         * So the range [-16, 15] can be represented by SmallImmediate objects.
         */
        Optional<int32_t> getIntegerValue() const noexcept;
        /*
         * Returns the floating-point value represented by this object, if this object represents a floating-point
         * value.
         *
         * The field-values 32 - 29 represent the floating-point constants 1.0, 2.0, 4.0, ..., 128.0,
         * the field-values 40 - 47 represent the constants 1/256.0, 1/128.0, ..., 1/2.
         * So every power of two from 1/256.0 to 128.0 can be represented by SmallImmediates.
         */
        Optional<float> getFloatingValue() const noexcept;
        /*
         * Returns whether the field-value represents a vector-rotation
         */
        bool isVectorRotation() const noexcept;
        /*
         * Returns the constant offset for vector-rotations, if this object represents one.
         *
         * The field-value of VECTOR_ROTATE_R5 represents a vector-rotation by the value stored in SIMD-element 0 of r5,
         * bits [3:0], while the field-values 49 - 63 represent vector-rotations by an offset of 1 - 15 upwards (so
         * element 0 moves to element 1 - 15)
         *
         * NOTE: vector-rotations can only be performed by the multiplication ALU.
         */
        Optional<unsigned char> getRotationOffset() const noexcept;

        /*
         * Returns the Literal value which is represented by this SmallImmediate object.
         *
         * For vector-rotation field-values, no value us returned.
         */
        Optional<Literal> toLiteral() const noexcept;

        /*
         * Tries to create a SmallImmediate object from the given integer-value.
         *
         * Returns a new object for an argument in the range [-16, 15] and an empty optional-value otherwise.
         */
        static Optional<SmallImmediate> fromInteger(signed char val) noexcept;
        /*
         * Creates a new object from the given constant vector-rotation offset.
         *
         * The given vector-rotation offset must lie in the range [1, 15]
         */
        static SmallImmediate fromRotationOffset(unsigned char offset);
    };

    template <>
    struct tombstone_traits<SmallImmediate>
    {
        static constexpr bool is_specialized = true;
        static constexpr SmallImmediate tombstone = SmallImmediate(255);

        static constexpr bool isTombstone(SmallImmediate val) noexcept
        {
            return val.value == 255;
        }
    };

    constexpr SmallImmediate VECTOR_ROTATE_R5{48};

    class Local;
    class SIMDVector;
    namespace intermediate
    {
        class IntermediateInstruction;
    } /* namespace intermediate */

    using LocalUser = intermediate::IntermediateInstruction;

    /*
     * The main type representing all values being operated on
     */
    struct Value
    {
        /*
         * Contains the data actually stored in this Value
         */
        Variant<Literal, Register, Local*, SmallImmediate, const SIMDVector*, VariantNamespace::monostate> data;
        /*
         * The data-type of the Value
         */
        DataType type;

        Value(const Literal& lit, DataType type) noexcept;
        Value(Register reg, DataType type) noexcept;
        Value(const SIMDVector* vector, DataType type);
        Value(const Value& val) = default;
        Value(Value&& val) noexcept = default;
        Value(Local* local, DataType type) noexcept;
        Value(DataType type) noexcept;
        Value(SmallImmediate immediate, DataType type) noexcept;
        ~Value() = default;

        Value& operator=(const Value& right) = default;
        Value& operator=(Value&& right) = default;

        bool operator==(const Value& other) const;
        inline bool operator!=(const Value& other) const
        {
            return !(*this == other);
        }

        /*
         * Returns a pointer to the data of the given type or nullptr if the data is not of the requested type.
         */
        Register* checkRegister() noexcept
        {
            return VariantNamespace::get_if<Register>(&data);
        }

        const Register* checkRegister() const noexcept
        {
            return VariantNamespace::get_if<Register>(&data);
        }

        Literal* checkLiteral() noexcept
        {
            return VariantNamespace::get_if<Literal>(&data);
        }

        const Literal* checkLiteral() const noexcept
        {
            return VariantNamespace::get_if<Literal>(&data);
        }

        SmallImmediate* checkImmediate() noexcept
        {
            return VariantNamespace::get_if<SmallImmediate>(&data);
        }

        const SmallImmediate* checkImmediate() const noexcept
        {
            return VariantNamespace::get_if<SmallImmediate>(&data);
        }

        Local* checkLocal() noexcept
        {
            auto loc = VariantNamespace::get_if<Local*>(&data);
            return loc ? *loc : nullptr;
        }

        const Local* checkLocal() const noexcept
        {
            auto loc = VariantNamespace::get_if<Local*>(&data);
            return loc ? *loc : nullptr;
        }

        const SIMDVector* checkVector() const noexcept
        {
            auto vec = VariantNamespace::get_if<const SIMDVector*>(&data);
            return vec ? *vec : nullptr;
        }

        /*
         * Whether this object has the given local
         */
        bool hasLocal(const Local* local) const;
        /*
         * Whether this object has the given register
         */
        bool hasRegister(Register reg) const;
        /*
         * Whether this object has the given literal value.
         *
         * This function also accepts, if this object contains a SmallImmediate with the same integer- or
         * floating-point-value (depending on the data-type) as the parameter
         */
        bool hasLiteral(const Literal& lit) const;
        /*
         * Whether this object contains the given SmallImmediate value.
         *
         * This function also accepts, if this object contains a Literal with the same integer- or floating-point-value
         * (depending on the data-type) as the parameter
         */
        bool hasImmediate(SmallImmediate immediate) const;

        /*
         * Whether this Value is undefined, e.g. by having the TYPE_UNDEFINED data-type
         */
        bool isUndefined() const;
        /*
         * Whether this object is a constant (scalar or composite) and has the literal-value zero in all its elements
         */
        bool isZeroInitializer() const;
        /*
         * Whether this Value represents a literal value (e.g. contains a Literal or SmallImmediate)
         */
        bool isLiteralValue() const noexcept;
        /*
         * Returns the Literal stored in this Value.
         *
         * This function also converts a stored SmallImmediate into the corresponding Literal value
         */
        Optional<Literal> getLiteralValue() const noexcept;

        std::string to_string(bool writeAccess = false, bool withLiterals = false) const;

        /*
         * Whether this Value can be written to.
         *
         * Constant values of any kind cannot be written to, neither can registers which are not writeable
         */
        bool isWriteable() const;
        /*
         * Whether this object can be read from.
         *
         * Almost all value-types can be read, except for some write-only registers
         */
        bool isReadable() const;
        /*
         * Returns this object, if it is writeable. Throws an exception otherwise
         */
        const Value& assertWriteable() const;
        Value& assertWriteable();
        /*
         * Returns this object if it is readable. Throws an exception otherwise
         */
        const Value& assertReadable() const;
        Value& assertReadable();

        /*
         * Wrapper for Local#getSingleWriter() for easier access
         */
        const LocalUser* getSingleWriter() const;

        /**
         * Return whether this value is guaranteed to be an unsigned (positive) integer.
         *
         * Unsigned integers are among others:
         * - positive integer constants
         * - unsigned registers (QPU number, element number)
         * - locals where all writes are decorated as unsigned
         */
        bool isUnsignedInteger() const;

        /**
         * Returns the constant value "contained" in this value, if any.
         *
         * A value is considered constant, if it matches one of the conditions:
         * - it is a literal value
         * - it is a SIMD vector
         * - it is a small immediate
         * - it is a constant register value
         * - it is a local with a single writer writing a constant value (only if transitive flag set)
         *
         * If the resulting Value is set, it is guaranteed to be either a literal value, a small immediate or a
         * register.
         */
        Optional<Value> getConstantValue(bool transitive = true) const;

        /*
         * Creates a zero-initializer Value for the given data-type.
         *
         * For scalar types, a simple INT_ZERO is returned, for compound types, a container containing the correct
         * amount of zero-elements is created
         */
        static Optional<Value> createZeroInitializer(DataType type);

        /**
         * Returns whether all SIMD elements of this value contain the same value.
         *
         * This is e.g. true for SIMDVectors with identical elements, literal values, SmallImmediates and some
         * registers.
         */
        bool isAllSame() const;

        /**
         * Returns the mask of (possible) non-zero bits in this value when read
         */
        BitMask getReadMask() const noexcept;

        /*
         * Returns the stored data of the given type, if it matches the stored type.
         * Throws error otherwise
         */
        Literal& literal()
        {
            return VariantNamespace::get<Literal>(data);
        }

        const Literal& literal() const
        {
            return VariantNamespace::get<Literal>(data);
        }

        Register& reg()
        {
            return VariantNamespace::get<Register>(data);
        }

        const Register& reg() const
        {
            return VariantNamespace::get<Register>(data);
        }

        Local*& local()
        {
            return VariantNamespace::get<Local*>(data);
        }

        Local* const& local() const
        {
            return VariantNamespace::get<Local*>(data);
        }

        SmallImmediate& immediate()
        {
            return VariantNamespace::get<SmallImmediate>(data);
        }

        const SmallImmediate& immediate() const
        {
            return VariantNamespace::get<SmallImmediate>(data);
        }

        const SIMDVector& vector() const
        {
            return *VariantNamespace::get<const SIMDVector*>(data);
        }
    };

    /*
     * The boolean-value true
     */
    const Value BOOL_TRUE(Literal(true), TYPE_BOOL);
    /*
     * The boolean-value false
     */
    const Value BOOL_FALSE(Literal(false), TYPE_BOOL);
    /*
     * The integer value of zero
     */
    const Value INT_ZERO(Literal(static_cast<uint32_t>(0)), TYPE_INT8);
    /*
     * The integer value of one
     */
    const Value INT_ONE(Literal(static_cast<uint32_t>(1)), TYPE_INT8);
    /*
     * The integer value of minus one
     */
    const Value INT_MINUS_ONE(Literal(static_cast<uint32_t>(0xFFFFFFFF)), TYPE_INT32);
    /*
     * The floating-point value of zero
     */
    const Value FLOAT_ZERO(Literal(0.0f), TYPE_FLOAT);
    /*
     * The floating-point value of one
     */
    const Value FLOAT_ONE(Literal(1.0f), TYPE_FLOAT);
    /*
     * The floating-point constant representing INF
     */
    const Value FLOAT_INF(Literal(static_cast<uint32_t>(0x7F800000)), TYPE_FLOAT);
    /*
     * The floating-point constant representing -INF
     */
    const Value FLOAT_NEG_INF(Literal(static_cast<uint32_t>(0xFF800000)), TYPE_FLOAT);
    /*
     * The floating-point constant representing NAN
     *
     * NOTE: There are different representations of a NaN!
     */
    const Value FLOAT_NAN(Literal(static_cast<uint32_t>(0x7FFFFFFF)), TYPE_FLOAT);
    /*
     * A undefined value
     */
    const Value UNDEFINED_VALUE(TYPE_UNKNOWN);
    /*
     * A constant Optional value containing no value
     */
    const Optional<Value> NO_VALUE;
    /*
     * All 32 bits are set
     */
    const Value VALUE_ALL_BITS_SET = INT_MINUS_ONE;

    /*
     * The Value representing the REG_UNIFORM register to read UNIFORMs
     */
    const Value UNIFORM_REGISTER(REG_UNIFORM, TYPE_INT32.toVectorType(16));
    /*
     * The Value representing the NOP-register
     */
    const Value NOP_REGISTER(REG_NOP, TYPE_UNKNOWN);
    /*
     * The Value representing the REG_ELEMENT_NUMBER
     */
    const Value ELEMENT_NUMBER_REGISTER(REG_ELEMENT_NUMBER, TYPE_INT8.toVectorType(16));
    /*
     * The element numbers (0, 1, 2, 3, ...) returned when querying the element-number register
     */
    extern const Value ELEMENT_NUMBERS;
    /*
     * The Value representing the r5 rotation offset register
     */
    const Value ROTATION_REGISTER(REG_ACC5, TYPE_INT8);

} /* namespace vc4c */

namespace std
{
    template <>
    struct hash<vc4c::Literal> : public std::hash<int32_t>
    {
        inline size_t operator()(const vc4c::Literal& lit) const noexcept
        {
            return std::hash<int32_t>::operator()(lit.signedInt());
        }
    };

    template <>
    struct hash<vc4c::SmallImmediate> : public std::hash<unsigned char>
    {
        inline size_t operator()(const vc4c::SmallImmediate& val) const noexcept
        {
            return std::hash<unsigned char>::operator()(val.value);
        }
    };

    template <>
    struct hash<vc4c::Value>
    {
        size_t operator()(const vc4c::Value& val) const noexcept;
    };
} /* namespace std */

#endif /* VALUES_H */
