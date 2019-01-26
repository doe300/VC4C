/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Types.h"

#include "CompilationError.h"
#include "helper.h"

#include <cstdlib>
#include <limits>
#include <sstream>
#include <stdexcept>

using namespace vc4c;

ComplexType::~ComplexType() {}

DataType::DataType(std::shared_ptr<ComplexType>&& complexType) :
    complexType(complexType), bitWidth(DataType::COMPLEX), numElements(1), isFloatingPoint(false)
{
}

static std::string toSignedIntegerTypeName(unsigned char bitWidth, const std::string& typeName)
{
    switch(bitWidth)
    {
    case DataType::LONG_WORD:
        return "long";
    case DataType::WORD:
        return "int";
    case DataType::HALF_WORD:
        return "short";
    case DataType::BYTE:
        return "char";
    default:
        return typeName;
    }
}

static std::string toUnsignedIntegerTypeName(unsigned char bitWidth, const std::string& typeName)
{
    switch(bitWidth)
    {
    case DataType::LONG_WORD:
        return "ulong";
    case DataType::WORD:
        return "uint";
    case DataType::HALF_WORD:
        return "ushort";
    case DataType::BYTE:
        return "uchar";
    default:
        return typeName;
    }
}

static std::string toFloatingPointTypeName(unsigned char bitWidth, const std::string& typeName)
{
    switch(bitWidth)
    {
    case DataType::LONG_WORD:
        return "double";
    case DataType::WORD:
        return "float";
    case DataType::HALF_WORD:
        return "half";
    default:
        return typeName;
    }
}

static std::string toSimpleTypeName(unsigned char bitWidth, bool isFloat)
{
    if(bitWidth == DataType::VOID)
        return "void";
    if(bitWidth == DataType::LABEL)
        return "label";
    if(bitWidth == DataType::UNKNOWN)
        return "";
    if(bitWidth == DataType::BIT)
        return "bool";
    if(isFloat)
        return std::string("f") + std::to_string(bitWidth);
    return std::string("i") + std::to_string(bitWidth);
}

std::string vc4c::toString(AddressSpace space, bool shortName)
{
    switch(space)
    {
    case AddressSpace::CONSTANT:
        return shortName ? "(c)" : "__constant";
    case AddressSpace::GENERIC:
        return shortName ? "(?)" : "__generic";
    case AddressSpace::GLOBAL:
        return shortName ? "(g)" : "__global";
    case AddressSpace::LOCAL:
        return shortName ? "(l)" : "__local";
    case AddressSpace::PRIVATE:
        return shortName ? "(p)" : "__private";
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled address space", std::to_string(static_cast<int>(space)));
}

std::string DataType::to_string() const
{
    if(complexType)
        return complexType->getTypeName();
    const std::string braceLeft(isVectorType() ? "<" : "");
    const std::string braceRight(isVectorType() ? ">" : "");
    if(numElements > 1)
        return braceLeft + (std::to_string(numElements) + " x ") +
            (toSimpleTypeName(bitWidth, isFloatingPoint) + braceRight);
    return toSimpleTypeName(bitWidth, isFloatingPoint);
}

std::string DataType::getTypeName(bool isSigned, bool isUnsigned) const
{
    if(complexType != nullptr || (!isFloatingType() && !isSigned && !isUnsigned))
        return to_string();
    const std::string simpleName = toSimpleTypeName(bitWidth, isFloatingPoint);
    const std::string tName = isFloatingPoint ?
        toFloatingPointTypeName(bitWidth, simpleName) :
        (isSigned ? toSignedIntegerTypeName(bitWidth, simpleName) :
                    isUnsigned ? toUnsignedIntegerTypeName(bitWidth, simpleName) : simpleName);
    if(numElements > 1)
        return tName + std::to_string(numElements);
    return tName;
}

bool DataType::operator==(const DataType& right) const
{
    if(this == &right)
        return true;
    return bitWidth == right.bitWidth && numElements == right.numElements && isFloatingPoint == right.isFloatingPoint &&
        (complexType == nullptr) == (right.complexType == nullptr) &&
        (complexType != nullptr ? (*complexType.get()) == (*right.complexType.get()) : true);
}

bool DataType::operator<(const DataType& other) const
{
    return complexType < other.complexType || bitWidth < other.bitWidth || isFloatingPoint < other.isFloatingPoint ||
        numElements < other.numElements;
}

bool DataType::isSimpleType() const
{
    return !complexType;
}

bool DataType::isScalarType() const
{
    return !complexType && numElements == 1;
}

bool DataType::isVectorType() const
{
    return !complexType && numElements > 1;
}

bool DataType::isPointerType() const
{
    return getPointerType() != nullptr;
}

PointerType* DataType::getPointerType()
{
    return dynamic_cast<PointerType*>(complexType.get());
}

const PointerType* DataType::getPointerType() const
{
    return dynamic_cast<const PointerType*>(complexType.get());
}

ArrayType* DataType::getArrayType()
{
    return dynamic_cast<ArrayType*>(complexType.get());
}

const ArrayType* DataType::getArrayType() const
{
    return dynamic_cast<const ArrayType*>(complexType.get());
}

StructType* DataType::getStructType()
{
    return dynamic_cast<StructType*>(complexType.get());
}

const StructType* DataType::getStructType() const
{
    return dynamic_cast<const StructType*>(complexType.get());
}

ImageType* DataType::getImageType()
{
    return dynamic_cast<ImageType*>(complexType.get());
}

const ImageType* DataType::getImageType() const
{
    return dynamic_cast<const ImageType*>(complexType.get());
}

bool DataType::isFloatingType() const
{
    if(complexType)
        return false;
    return isFloatingPoint;
}

bool DataType::isIntegralType() const
{
    if(isPointerType())
        return true;
    if(complexType)
        return false;
    return !isFloatingPoint;
}

bool DataType::isUnknown() const
{
    return bitWidth == DataType::UNKNOWN;
}

bool DataType::isLabelType() const
{
    return bitWidth == DataType::LABEL;
}

bool DataType::isVoidType() const
{
    return bitWidth == DataType::VOID;
}

DataType DataType::getElementType(const int index) const
{
    if(isPointerType())
        return getPointerType()->elementType;
    if(getArrayType())
        return getArrayType()->elementType;
    if(getStructType() && index >= 0)
        return getStructType()->elementTypes.at(static_cast<std::size_t>(index));
    if(complexType)
        throw CompilationError(
            CompilationStep::GENERAL, "Can't get element-type of heterogeneous complex type", to_string());
    if(numElements == 1)
        return *this;
    return DataType{bitWidth, 1, isFloatingPoint};
}

DataType DataType::toPointerType(AddressSpace addressSpace) const
{
    return DataType(new PointerType(*this, addressSpace));
}

DataType DataType::toVectorType(unsigned char vectorWidth) const
{
    if(complexType)
        throw CompilationError(CompilationStep::GENERAL, "Can't form vector-type of complex type", to_string());
    if(vectorWidth > 16 || vectorWidth == 0)
        throw CompilationError(CompilationStep::GENERAL, "Invalid width for SIMD vector",
            std::to_string(static_cast<unsigned>(vectorWidth)));
    if(numElements == vectorWidth)
        return *this;
    return DataType(bitWidth, vectorWidth, isFloatingPoint);
}

DataType DataType::toArrayType(unsigned int numElements) const
{
    return DataType(new ArrayType(*this, numElements));
}

bool DataType::containsType(const DataType& other) const
{
    if(*this == other)
        return true;
    if(complexType && !getPointerType())
        throw CompilationError(CompilationStep::GENERAL, "Can't check type hierarchy for complex type", to_string());
    if(!isFloatingPoint && !other.isFloatingPoint && bitWidth <= other.bitWidth)
    {
        // FIXME correct? doesn't the comparison need to be the other way round?
        return true;
    }
    return false;
}

const DataType DataType::getUnionType(const DataType& other) const
{
    if(*this == other)
        return *this;
    // doesn't work for heterogeneous types
    if(complexType || other.complexType)
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type of distinct complex types!");
    if(isFloatingPoint != other.isFloatingPoint)
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type of floating-point and integer types!");
    if(isVoidType() || isUnknown() || isLabelType())
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type with this type", to_string());
    return DataType(std::max(getScalarBitCount(), other.getScalarBitCount()), std::max(numElements, other.numElements),
        isFloatingPoint);
}

unsigned char DataType::getScalarBitCount() const
{
    if(isPointerType())
        // 32-bit pointer
        return 32;
    if(getImageType())
        // images are pointers to the image-data
        return 32;
    if(complexType)
        throw CompilationError(CompilationStep::GENERAL, "Can't get bit-width of complex type", to_string());
    if(isVoidType())
        // single byte
        return 8;
    if(isUnknown())
        return 32;
    return bitWidth;
}

uint32_t DataType::getScalarWidthMask() const
{
    return static_cast<uint32_t>((static_cast<uint64_t>(1) << getScalarBitCount()) - 1);
}

unsigned int DataType::getPhysicalWidth() const
{
    if(isPointerType())
        // 32-bit pointer
        return 4;
    if(getArrayType())
        return getArrayType()->elementType.getPhysicalWidth() * getArrayType()->size;
    if(getStructType())
        return getStructType()->getStructSize();
    if(getImageType())
        // images are just pointers to data
        // 32-bit pointer
        return 4;
    if(complexType)
        // any other complex type
        throw CompilationError(CompilationStep::GENERAL, "Can't get width of complex type", to_string());
    return getVectorWidth(true) * getScalarBitCount() / 8;
}

unsigned char DataType::getVectorWidth(bool physicalWidth) const
{
    if(complexType && numElements != 1)
        throw CompilationError(CompilationStep::GENERAL, "Can't have vectors of complex types", to_string());
    if(physicalWidth && numElements == 3)
        // OpenCL 1.2, page 203:
        //"For 3-component vector data types, the size of the data type is 4 * sizeof(component)"
        return 4;
    return numElements;
}

unsigned DataType::getAlignmentInBytes() const
{
    if(complexType)
        return complexType->getAlignmentInBytes();
    return getPhysicalWidth();
}

std::size_t std::hash<DataType>::operator()(const DataType& type) const noexcept
{
    const std::hash<std::shared_ptr<ComplexType>> complexHash;
    const std::hash<unsigned char> numHash;
    return complexHash(type.complexType) ^ numHash(type.bitWidth) ^ numHash(type.numElements) ^
        numHash(type.isFloatingPoint);
}

PointerType::PointerType(const DataType& elementType, const AddressSpace addressSpace, unsigned alignment) :
    elementType(elementType), addressSpace(addressSpace), alignment(alignment)
{
}

bool PointerType::operator==(const ComplexType& other) const
{
    if(this == &other)
        return true;
    const PointerType* right = dynamic_cast<const PointerType*>(&other);
    if(right == nullptr)
        return false;
    return elementType == right->elementType;
}

unsigned PointerType::getAlignment() const
{
    if(alignment != 0)
        return alignment;
    return elementType.getPhysicalWidth();
}

unsigned PointerType::getAlignmentInBytes() const
{
    // pointer are words, so they are aligned as such
    return 4;
}

std::string PointerType::getTypeName() const
{
    return toString(addressSpace, true) + " " + elementType.to_string() + "*";
}

StructType::StructType(const std::string& name, const std::vector<DataType>& elementTypes, const bool isPacked) :
    name(name), elementTypes(elementTypes), isPacked(isPacked)
{
}

bool StructType::operator==(const ComplexType& other) const
{
    if(this == &other)
        return true;
    const StructType* right = dynamic_cast<const StructType*>(&other);
    if(right == nullptr)
        return false;
    return isPacked == right->isPacked && elementTypes == right->elementTypes;
}

unsigned int StructType::getStructSize(const int index) const
{
    unsigned int size = 0;
    if(isPacked)
    {
        // packed -> no padding
        for(std::size_t i = 0; i < elementTypes.size(); ++i)
        {
            if(i == static_cast<std::size_t>(index))
                break;
            size += elementTypes[i].getPhysicalWidth();
        }
        return size;
    }
    // calculates size of struct including padding (!!NEED TO MATCH THE HOST!!)
    // see also: http://stackoverflow.com/a/2749096
    unsigned int alignment = 1;
    for(std::size_t i = 0; i < elementTypes.size(); ++i)
    {
        if(i == static_cast<std::size_t>(index))
            break;
        auto elementAlignment = elementTypes[i].getAlignmentInBytes();
        alignment = std::max(alignment, elementAlignment);
        // OpenCL 1.2, page 203
        //"A data item declared to be a data type in memory is always aligned to the size of the data type in bytes."
        if(size % elementAlignment != 0)
        {
            // alignment is added before the next type, not after the last
            size += elementAlignment - (size % elementAlignment);
        }
        size += elementTypes[i].getPhysicalWidth();
    }
    if(index < 0)
    {
        // whole object
        // padding at end of struct to align to alignment of largest
        if(size % alignment != 0)
        {
            size += alignment - (size % alignment);
        }
    }
    else
    {
        // add padding for element which is retrieved
        auto elementAlignment = elementTypes.at(static_cast<std::size_t>(index)).getAlignmentInBytes();
        if(size % elementAlignment != 0)
        {
            // alignment is added before the next type, not after the last
            size += elementAlignment - (size % elementAlignment);
        }
    }

    return size;
}

unsigned StructType::getAlignmentInBytes() const
{
    return getStructSize();
}

std::string StructType::getTypeName() const
{
    return name;
}

std::string StructType::getContent() const
{
    std::ostringstream s;
    s << '{';
    for(std::size_t i = 0; i < elementTypes.size(); ++i)
    {
        if(i != 0)
            s << ", ";
        s << elementTypes[i].to_string();
    }
    s << '}';
    return s.str();
}

ArrayType::ArrayType(const DataType& elementType, const unsigned int size) : elementType(elementType), size(size) {}

bool ArrayType::operator==(const ComplexType& other) const
{
    if(this == &other)
        return true;
    const ArrayType* right = dynamic_cast<const ArrayType*>(&other);
    if(right == nullptr)
        return false;
    return size == right->size && elementType == right->elementType;
}

unsigned ArrayType::getAlignmentInBytes() const
{
    // arrays are only aligned to their element-type size, not their complete size
    return elementType.getPhysicalWidth();
}

std::string ArrayType::getTypeName() const
{
    return (elementType.to_string() + "[") + std::to_string(size) + "]";
}

bool ImageType::operator==(const ComplexType& other) const
{
    if(this == &other)
        return true;
    const ImageType* right = dynamic_cast<const ImageType*>(&other);
    if(right == nullptr)
        return false;
    return dimensions == right->dimensions && isImageArray == right->isImageArray &&
        isImageBuffer == right->isImageBuffer && isSampled == right->isSampled;
}

std::string ImageType::getTypeName() const
{
    std::string name = "image";
    name.append(std::to_string(dimensions)).append("D");
    if(isImageArray)
        name.append("_array");
    if(isImageBuffer)
        name.append("_buffer");
    return name;
}

unsigned ImageType::getAlignmentInBytes() const
{
    throw CompilationError(CompilationStep::GENERAL, "Alignment for images is not implemented yet");
}

std::string ImageType::toImageConfigurationName(const std::string& localName)
{
    return localName + ".image_config";
}
