/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TYPES_H
#define TYPES_H

#include "Optional.h"

#include <memory>
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
        virtual ~ComplexType();

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
    class DataType
    {
    public:
        constexpr DataType(unsigned char scalarBitWidth, unsigned char numVectorElements, bool isFloatingPointType) :
            complexType(), bitWidth(scalarBitWidth), numElements(numVectorElements),
            isFloatingPoint(isFloatingPointType)
        {
        }
        explicit DataType(std::shared_ptr<ComplexType>&& complexType);
        explicit DataType(ComplexType* complexType) : DataType(std::shared_ptr<ComplexType>(complexType)) {}

        std::string to_string() const;

        /*
         * Tries to reconstruct the original type-name from the signedness-information passed.
         * Otherwise, returns #to_string()
         */
        std::string getTypeName(bool isSigned = false, bool isUnsigned = false) const;

        bool operator==(const DataType& right) const;
        inline bool operator!=(const DataType& right) const
        {
            return !(*this == right);
        }
        bool operator<(const DataType& other) const;

        //"simple" types
        /*
         * Whether this type is not a complex type
         */
        bool isSimpleType() const;
        // vector can only be vector of scalars, so its counts as simple type!
        /*
         * Whether this is a non-complex type with a single element
         */
        bool isScalarType() const;
        /*
         * Whether this is a non-complex type with more than one elements
         */
        bool isVectorType() const;

        //"complex" types
        bool isPointerType() const;
        PointerType* getPointerType();
        const PointerType* getPointerType() const;
        ArrayType* getArrayType();
        const ArrayType* getArrayType() const;
        StructType* getStructType();
        const StructType* getStructType() const;
        ImageType* getImageType();
        const ImageType* getImageType() const;

        /*
         * Whether this is a scalar- or vector-variant of a floating-point type
         */
        bool isFloatingType() const;
        /*
         * Whether this is a scalar- or vector-variant of an integral type (e.g. integers, boolean, pointers)
         */
        bool isIntegralType() const;
        /*
         * Whether this type is the unknown-type
         */
        bool isUnknown() const;
        /*
         * Whether this type is a label
         */
        bool isLabelType() const;
        /*
         * Whether this type is void
         */
        bool isVoidType() const;

        /*
         * Returns the element-type for the given index.
         *
         * For scalar types, this returns the scalar type itself, for pointer-types the pointed-to type.
         * For uniform types (arrays, vectors), where all elements are of the same type, ANY_ELEMENT can be passed as
         * index.
         */
        DataType getElementType(int index = ANY_ELEMENT) const;
        /*
         * Creates a new type representing a pointer-type to this type.
         */
        DataType toPointerType(AddressSpace addressSpace = AddressSpace::PRIVATE) const;
        /*
         * Creates a new vector-type with the given vector-width and the same element-type as this type
         *
         * Vector-types of complex types are not supported, neither are vector-types with more than 16 elements
         */
        DataType toVectorType(unsigned char vectorWidth) const;
        /*
         * Creates a new array-type with this type as element type and the given array-length
         */
        DataType toArrayType(unsigned int numElements) const;

        /*
         * Whether this type contains the other type.
         *
         * A type always contains itself and an integral type contains all integral types of lesser bit-width.
         */
        bool containsType(const DataType& other) const;
        /*
         * Returns the union type of the both types.
         *
         * The union type of two arithmetic types is the type with the largest bit- and vector-width.
         * Union-types are only supported for two integral types or two floating-point types.
         */
        const DataType getUnionType(const DataType& other) const;

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
        unsigned char getVectorWidth(bool physicalWidth = false) const;

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
            COMPLEX = 253,
            LABEL = 254,
            UNKNOWN = 255
        };

    private:
        std::shared_ptr<ComplexType> complexType;
        /*
         * the number of bits a scalar value of this type used on the QPU.
         * NOTE: This is not the physical bit-width used in memory!
         */
        unsigned char bitWidth;
        // the number of elements for vector-types
        unsigned char numElements;
        // whether this type is a floating point type
        bool isFloatingPoint;

        friend struct std::hash<DataType>;
    };

    /*
     * 8-bit integer type (e.g. char, uchar)
     */
    static const DataType TYPE_INT8 = DataType{DataType::BYTE, 1, false};
    /*
     * 16-bit integer type (e.g. short, ushort)
     */
    static const DataType TYPE_INT16 = DataType{DataType::HALF_WORD, 1, false};
    /*
     * 32-bit integer type (e.g. int, uint)
     */
    static const DataType TYPE_INT32 = DataType{DataType::WORD, 1, false};
    /*
     * 64-bit integer type (e.g. long, ulong)
     *
     * NOTE: Is only supported for constants
     */
    static const DataType TYPE_INT64 = DataType{DataType::LONG_WORD, 1, false};
    /*
     * 32-bit floating-point type
     */
    static const DataType TYPE_FLOAT = DataType{DataType::WORD, 1, true};
    /*
     * 16-bit floating-point type
     */
    static const DataType TYPE_HALF = DataType{DataType::HALF_WORD, 1, true};
    /*
     * 64-bit floating-point type
     *
     * NOTE: Is only supported for constants
     */
    static const DataType TYPE_DOUBLE = DataType{DataType::LONG_WORD, 1, true};
    /*
     * 1-bit boolean type
     */
    static const DataType TYPE_BOOL = DataType{DataType::BIT, 1, false};
    /*
     * Void-type, only valid as pointed-to type
     */
    static const DataType TYPE_VOID = DataType{DataType::VOID, 1, false};
    /*
     * Unknown type, e.g. in unknown-values or as type of periphery-registers
     */
    static const DataType TYPE_UNKNOWN = DataType{DataType::UNKNOWN, 1, false};
    /*
     * Data-type for labels as destination for branches
     */
    static const DataType TYPE_LABEL = DataType{DataType::LABEL, 1, false};
    /*
     * Data-type for OpenCL samplers, is equivalent to 32-bit integers
     */
    static const DataType TYPE_SAMPLER = TYPE_INT32;
    /*
     * Data-type for OpenCL events, equivalent to 32-bit integers
     */
    static const DataType TYPE_EVENT = TYPE_INT32;

    /*
     * Additional information for pointer-type
     *
     * Not really "complex", but this allows e.g. pointer of pointers
     */
    struct PointerType final : public ComplexType
    {
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

        PointerType(
            const DataType& elementType, AddressSpace addressSpace = AddressSpace::PRIVATE, unsigned alignment = 0);
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
    };

    /*
     * Additional information for structure-types
     */
    struct StructType final : public ComplexType
    {
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

        StructType(const std::string& name, const std::vector<DataType>& elementTypes, bool isPacked = false);
        ~StructType() override = default;
        bool operator==(const ComplexType& other) const override;

        /*
         * Calculates the size of the struct up to the given index (of all elements excluding the given index) in Bytes.
         * If index is -1 (WHOLE_OBJECT), the complete size of the struct is returned
         */
        unsigned int getStructSize(const int index = WHOLE_OBJECT) const;

        unsigned getAlignmentInBytes() const override;
        std::string getTypeName() const override;

        std::string getContent() const;
    };

    /*
     * Additional data for array-types
     */
    struct ArrayType final : public ComplexType
    {
        /*
         * The data-type for all elements
         */
        DataType elementType;
        /*
         * The number of elements in the array
         */
        unsigned int size;

        ArrayType(const DataType& elementType, unsigned int size);
        ~ArrayType() override = default;
        bool operator==(const ComplexType& other) const override;

        unsigned getAlignmentInBytes() const override;
        std::string getTypeName() const override;
    };

    /*
     * Additional details for image-types
     */
    struct ImageType final : public ComplexType
    {
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
    };
} // namespace vc4c

namespace std
{
    template <>
    struct hash<vc4c::DataType>
    {
        std::size_t operator()(const vc4c::DataType& type) const noexcept;
    };
} /* namespace std */

#endif /* TYPES_H */
