/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef TYPES_H
#define TYPES_H

#include "Bitfield.h"

#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace vc4c
{
    static constexpr int WHOLE_OBJECT{-1};
    static constexpr int ANY_ELEMENT{-1};

    /*
     * Base class for "complex" data types. A ComplexType contains additional information not contained in the standard
     * DataType object.
     *
     *
     * Current complex types include pointer-, array-, struct- and image-types
     */
    struct ComplexType
    {
        ComplexType() = default;
        ComplexType(const ComplexType&) = delete;
        ComplexType(ComplexType&&) noexcept = delete;
        virtual ~ComplexType() noexcept;

        ComplexType& operator=(const ComplexType&) = delete;
        ComplexType& operator=(ComplexType&&) noexcept = delete;

        virtual bool operator==(const ComplexType& other) const = 0;

        virtual unsigned getAlignmentInBytes() const = 0;
        virtual std::string getTypeName() const = 0;
    };

    /*
     * The address space the memory-location of a pointer-type lies in.
     */
    enum class AddressSpace : unsigned char
    {
        // the generic address space (SPIR-V StorageClassGeneric)
        GENERIC = 0,
        // private memory, only for the single execution, OpenCL defaults to this (SPIR-V StorageClassFunction)
        PRIVATE = 1,
        // global memory pool, shared between all kernel executions (SPIR-V StorageClassCrossWorkgroup)
        GLOBAL = 2,
        // constant memory, usually as part of the binary (global data segment) (SPIR-V StorageClassUniformConstant)
        CONSTANT = 3,
        // local memory, shared between all work-items in the work-group (SPIR-V StorageClassWorkgroup)
        LOCAL = 4
    };

    std::string toString(AddressSpace space, bool shortName = false);

    struct PointerType;
    struct ArrayType;
    struct StructType;
    struct ImageType;

    /*
     * Basic data-type class.
     *
     * Each value contains its own DataType object. Values of the same type share "equal" DataType objects.
     */
    class DataType : private Bitfield<uintptr_t>
    {
    public:
        constexpr DataType(unsigned char scalarBitWidth, unsigned char numVectorElements, bool isFloatingPointType) :
            Bitfield(0)
        {
            setSimpleFlag(true);
            setBitWidth(scalarBitWidth);
            setNumElements(numVectorElements);
            setFloatingPoint(isFloatingPointType);
        }
        explicit DataType(const ComplexType* complexType);
        DataType(const DataType&) = default;
        DataType(DataType&&) noexcept = default;
        ~DataType() noexcept = default;

        DataType& operator=(const DataType&) = default;
        DataType& operator=(DataType&&) noexcept = default;

        std::string to_string() const;

        /*
         * Tries to reconstruct the original type-name from the signedness-information passed.
         * Otherwise, returns #to_string()
         */
        std::string getTypeName(bool isSigned = false, bool isUnsigned = false) const;

        bool operator==(DataType right) const noexcept;
        bool operator!=(DataType right) const noexcept
        {
            return !(*this == right);
        }

        constexpr bool operator<(DataType right) const noexcept
        {
            return value < right.value;
        }

        //"simple" types
        /*
         * Whether this type is not a complex type
         */
        bool isSimpleType() const noexcept;
        // vector can only be vector of scalars, so its counts as simple type!
        /*
         * Whether this is a non-complex type with a single element
         */
        bool isScalarType() const noexcept;
        /*
         * Whether this is a non-complex type with more than one elements
         */
        bool isVectorType() const noexcept;

        //"complex" types
        // These functions return nullptr if this DataType is not of the requested type and can therefore be used to
        // check for the complex type.
        const PointerType* getPointerType() const noexcept;
        const ArrayType* getArrayType() const noexcept;
        const StructType* getStructType() const noexcept;
        const ImageType* getImageType() const noexcept;

        /*
         * Whether this is a scalar- or vector-variant of a floating-point type
         */
        bool isFloatingType() const noexcept;
        /*
         * Whether this is a scalar- or vector-variant of an integral type (e.g. integers, boolean, pointers)
         */
        bool isIntegralType() const noexcept;
        /*
         * Whether this type is the unknown-type
         */
        bool isUnknown() const noexcept;
        /*
         * Whether this type is a label
         */
        bool isLabelType() const noexcept;
        /*
         * Whether this type is void
         */
        bool isVoidType() const noexcept;

        /*
         * Returns the element-type for the given index.
         *
         * For scalar types, this returns the scalar type itself, for pointer-types the pointed-to type.
         * For uniform types (arrays, vectors), where all elements are of the same type, ANY_ELEMENT can be passed as
         * index.
         */
        DataType getElementType(int index = ANY_ELEMENT) const;
        /*
         * Creates a new vector-type with the given vector-width and the same element-type as this type
         *
         * Vector-types of complex types are not supported, neither are vector-types with more than 16 elements
         */
        DataType toVectorType(unsigned char vectorWidth) const;
        /*
         * Creates a new array-type with this type as element type and the given array-length
         */
        [[deprecated]] DataType toArrayType(unsigned int numElements) const;

        /*
         * Whether this type contains the other type.
         *
         * A type always contains itself and an integral type contains all integral types of lesser bit-width.
         */
        bool containsType(DataType other) const;
        /*
         * Returns the union type of the both types.
         *
         * The union type of two arithmetic types is the type with the largest bit- and vector-width.
         * Union-types are only supported for two integral types or two floating-point types.
         */
        DataType getUnionType(DataType other) const;

        /*
         * The number of bits in a scalar version of this type.
         *
         * Scalar bit-width of array- an struct-types are not supported, while pointer- and image-types return a
         * bit-width of 32-bit.
         */
        unsigned char getScalarBitCount() const;
        /*
         * Returns the bit-mask to limit a value to the scalar bit-width of this type.
         */
        uint32_t getScalarWidthMask() const;

        /*
         * Returns the width of this type in bytes.
         *
         * For vector-types, this is the scalar bit-count times the vector-width.
         */
        unsigned int getPhysicalWidth() const;

        /*
         * Returns the logical or physical vector width of this type.
         *
         * The logical vector width is the number of SIMD elements used
         * The physical vector width is the number of elements used in RAM.
         *
         * NOTE: as per OpenCL 1.2 standard, the physical-width of a 3-element vector equals the physical/logical-width
         * of a 4-element vector with same scalar type.
         */
        unsigned char getVectorWidth(bool physicalWidth = false) const noexcept;

        /*
         * Returns the alignment of an object of this type
         */
        unsigned getAlignmentInBytes() const;

        enum TypeWidths : unsigned char
        {
            VOID = 0,
            BIT = 1,
            BYTE = 8,
            HALF_WORD = 16,
            WORD = 32,
            LONG_WORD = 64,
            LABEL = 254,
            UNKNOWN = 255
        };

    private:
        /*
         * The value is an union of the pointer to a complex type and the fields for the "simple" types.
         */

        // the pointer to the complex type object
        BITFIELD_ENTRY_CONSTEXPR(ComplexType, uintptr_t, 0, Pointer)

        /*
         * Whether the type is simple or complex
         *
         * The value lies within the alignment of the ComplexType pointer on purpose, since the LSB bit can never be set
         * for complex pointers.
         */
        BITFIELD_ENTRY_CONSTEXPR(SimpleFlag, bool, 0, Bit)
        // whether this type is a floating point type
        BITFIELD_ENTRY_CONSTEXPR(FloatingPoint, bool, 4, Bit)
        /*
         * the number of bits a scalar value of this type used on the QPU.
         * NOTE: This is not the physical bit-width used in memory!
         */
        BITFIELD_ENTRY_CONSTEXPR(BitWidth, uint8_t, 8, Byte)
        // the number of elements for vector-types
        BITFIELD_ENTRY_CONSTEXPR(NumElements, uint8_t, 16, Byte)

        friend struct std::hash<DataType>;
    };

    /*
     * 8-bit integer type (e.g. char, uchar)
     */
    static constexpr DataType TYPE_INT8{DataType::BYTE, 1, false};
    /*
     * 16-bit integer type (e.g. short, ushort)
     */
    static constexpr DataType TYPE_INT16{DataType::HALF_WORD, 1, false};
    /*
     * 32-bit integer type (e.g. int, uint)
     */
    static constexpr DataType TYPE_INT32{DataType::WORD, 1, false};
    /*
     * 64-bit integer type (e.g. long, ulong)
     *
     * NOTE: Is only supported for constants
     */
    static constexpr DataType TYPE_INT64{DataType::LONG_WORD, 1, false};
    /*
     * 32-bit floating-point type
     */
    static constexpr DataType TYPE_FLOAT{DataType::WORD, 1, true};
    /*
     * 16-bit floating-point type
     */
    static constexpr DataType TYPE_HALF{DataType::HALF_WORD, 1, true};
    /*
     * 64-bit floating-point type
     *
     * NOTE: Is only supported for constants
     */
    static constexpr DataType TYPE_DOUBLE{DataType::LONG_WORD, 1, true};
    /*
     * 1-bit boolean type
     */
    static constexpr DataType TYPE_BOOL{DataType::BIT, 1, false};
    /*
     * Void-type, only valid as pointed-to type
     */
    static constexpr DataType TYPE_VOID{DataType::VOID, 1, false};
    /*
     * Unknown type, e.g. in unknown-values or as type of periphery-registers
     */
    static constexpr DataType TYPE_UNKNOWN{DataType::UNKNOWN, 1, false};
    /*
     * Data-type for labels as destination for branches
     */
    static constexpr DataType TYPE_LABEL{DataType::LABEL, 1, false};
    /*
     * Data-type for OpenCL samplers, is equivalent to 32-bit integers
     */
    static constexpr DataType TYPE_SAMPLER = TYPE_INT32;
    /*
     * Data-type for OpenCL events, equivalent to 32-bit integers
     */
    static constexpr DataType TYPE_EVENT = TYPE_INT32;

    /*
     * Additional information for pointer-type
     *
     * Not really "complex", but this allows e.g. pointer of pointers
     */
    struct PointerType final : public ComplexType
    {
    public:
        /*
         * The pointed-to type
         */
        DataType elementType;
        /*
         * The address-space of the pointed-to memory location
         */
        AddressSpace addressSpace;
        /*
         * The alignment of the memory location in bytes.
         */
        unsigned alignment;

        ~PointerType() override = default;
        bool operator==(const ComplexType& other) const override;

        /*
         * Returns the alignment of the pointed-to data.
         *
         * If not specified, the alignment defaults to the physical size of the pointed-to type.
         */
        unsigned getAlignment() const;

        unsigned getAlignmentInBytes() const override;
        std::string getTypeName() const override;

    private:
        explicit PointerType(DataType type, AddressSpace addrSpace = AddressSpace::PRIVATE, unsigned align = 0) :
            elementType(type), addressSpace(addrSpace), alignment(align)
        {
        }

        friend struct TypeHolder;
    };

    /*
     * Additional information for structure-types
     */
    struct StructType final : public ComplexType
    {
    public:
        /*
         * The name of the struct
         */
        std::string name;
        /*
         * The data-types of the single elements
         */
        std::vector<DataType> elementTypes;
        /*
         * Whether the structure is packed
         *
         * "which indicate that the alignment of the struct is one byte, and that there is no padding between the
         * elements" - LLVM IR "Apply to a structure type, to marks it as "packed", indicating that the alignment of the
         * structure is one and that there is no padding between structure members." - SPIR-V
         */
        bool isPacked;

        ~StructType() override = default;
        bool operator==(const ComplexType& other) const override;

        /*
         * Calculates the size of the struct up to the given index (of all elements excluding the given index) in Bytes.
         * If index is -1 (WHOLE_OBJECT), the complete size of the struct is returned
         */
        unsigned int getStructSize(int index = WHOLE_OBJECT) const;

        unsigned getAlignmentInBytes() const override;
        std::string getTypeName() const override;

        std::string getContent() const;

    private:
        StructType(const std::string& name, const std::vector<DataType>& elementTypes, bool isPacked = false) :
            name(name), elementTypes(elementTypes), isPacked(isPacked)
        {
        }

        friend struct TypeHolder;
    };

    /*
     * Additional data for array-types
     */
    struct ArrayType final : public ComplexType
    {
    public:
        /*
         * The data-type for all elements
         */
        DataType elementType;
        /*
         * The number of elements in the array
         */
        unsigned int size;

        ~ArrayType() override = default;
        bool operator==(const ComplexType& other) const override;

        unsigned getAlignmentInBytes() const override;
        std::string getTypeName() const override;

    private:
        ArrayType(DataType elementType, unsigned int size) : elementType(elementType), size(size) {}

        friend struct TypeHolder;
    };

    /*
     * Additional details for image-types
     */
    struct ImageType final : public ComplexType
    {
    public:
        /*
         * The number of dimensions: 1D, 2D or 3D
         */
        uint8_t dimensions;
        /*
         * Whether this type is an 1D image-array
         */
        bool isImageArray;
        /*
         * Whether this type is an 1D or 2D image-buffer type
         */
        bool isImageBuffer;
        /*
         * Whether this type is sampled
         */
        bool isSampled;

        ~ImageType() override = default;
        bool operator==(const ComplexType& other) const override;

        unsigned getAlignmentInBytes() const override;
        /*
         * Reconstructs the OpenCL C image-type name out of the image-info stored
         */
        std::string getTypeName() const override;

        /*
         * Creates a name for a new local storing the image-configuration
         */
        static std::string toImageConfigurationName(const std::string& localName);

    private:
        explicit ImageType(
            uint8_t dimensions, bool isImageArray = false, bool isImageBuffer = false, bool isSampled = false) :
            dimensions(dimensions),
            isImageArray(isImageArray), isImageBuffer(isImageBuffer), isSampled(isSampled)
        {
        }

        friend struct TypeHolder;
    };

    /*
     * Container which holds and manages complex types
     *
     * NOTE: a type-holder object MUST live longer than all complex types generated from it!
     */
    struct TypeHolder
    {
    public:
        // These pointers are non-const on purpose, so a creator can modify the complex types
        // All access via DataType is then constant

        PointerType* createPointerType(
            DataType elementType, AddressSpace addressSpace = AddressSpace::PRIVATE, unsigned alignment = 0);
        StructType* createStructType(
            const std::string& name, const std::vector<DataType>& elementTypes, bool isPacked = false);
        ArrayType* createArrayType(DataType elementType, unsigned int size);
        ImageType* createImageType(
            uint8_t dimensions, bool isImageArray = false, bool isImageBuffer = false, bool isSampled = false);

        static std::unique_ptr<ComplexType> voidPtr;

    private:
        std::vector<std::unique_ptr<ComplexType>> complexTypes;
        std::mutex accessMutex;
    };

    /*
     * Void pointer type in the default address space
     */
    static const DataType TYPE_VOID_POINTER = DataType{TypeHolder::voidPtr.get()};
} // namespace vc4c

namespace std
{
    template <>
    struct hash<vc4c::DataType>
    {
        inline std::size_t operator()(vc4c::DataType type) const noexcept
        {
            return static_cast<size_t>(type.value);
        }
    };
} /* namespace std */
#endif /* TYPES_H */
