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
#include <stdexcept>

using namespace vc4c;

ComplexType::~ComplexType() {}

DataType::DataType(const std::string& name, const unsigned char num, const std::shared_ptr<ComplexType>& complexType) :
    typeName(name), num(num), complexType(complexType)
{
}

std::string DataType::to_string() const
{
    const std::string braceLeft = isVectorType() ? "<" : "";
    const std::string braceRight = isVectorType() ? ">" : "";
    if(num > 1)
        return braceLeft + (std::to_string(num) + " x ") + (typeName + braceRight);
    return typeName;
}

static std::string toSignedTypeName(const std::string& typeName)
{
    if(typeName == "i64")
        return "long";
    if(typeName == "i32")
        return "int";
    if(typeName == "i16")
        return "short";
    if(typeName == "i8")
        return "char";
    return typeName;
}

static std::string toUnsignedTypeName(const std::string& typeName)
{
    if(typeName == "i64")
        return "ulong";
    if(typeName == "i32")
        return "uint";
    if(typeName == "i16")
        return "ushort";
    if(typeName == "i8")
        return "uchar";
    return typeName;
}

std::string DataType::getTypeName(bool isSigned, bool isUnsigned) const
{
    if(complexType != nullptr || (!isFloatingType() && !isSigned && !isUnsigned))
        return to_string();
    const std::string tName =
        isSigned ? toSignedTypeName(typeName) : isUnsigned ? toUnsignedTypeName(typeName) : typeName;
    if(num > 1)
        return tName + std::to_string(num);
    return tName;
}

bool DataType::operator==(const DataType& right) const
{
    if(this == &right)
        return true;
    return typeName.compare(right.typeName) == 0 && num == right.num &&
        (complexType == nullptr) == (right.complexType == nullptr) &&
        (complexType != nullptr ? (*complexType.get()) == (*right.complexType.get()) : true);
}

bool DataType::operator<(const DataType& other) const
{
    return complexType < other.complexType || typeName < other.typeName || num < other.num;
}

bool DataType::isScalarType() const
{
    return !complexType && num == 1;
}

bool DataType::isVectorType() const
{
    return !complexType && num > 1;
}

bool DataType::isPointerType() const
{
    return getPointerType().has_value();
}

Optional<PointerType*> DataType::getPointerType() const
{
    PointerType* pointerType = dynamic_cast<PointerType*>(complexType.get());
    if(pointerType != nullptr)
        return pointerType;
    return {};
}

Optional<ArrayType*> DataType::getArrayType() const
{
    ArrayType* arrayType = dynamic_cast<ArrayType*>(complexType.get());
    if(arrayType != nullptr)
        return arrayType;
    return {};
}

Optional<StructType*> DataType::getStructType() const
{
    StructType* structType = dynamic_cast<StructType*>(complexType.get());
    if(structType != nullptr)
        return structType;
    return {};
}

Optional<ImageType*> DataType::getImageType() const
{
    ImageType* imageType = dynamic_cast<ImageType*>(complexType.get());
    if(imageType != nullptr)
        return imageType;
    return {};
}

bool DataType::isFloatingType() const
{
    if(complexType)
        return false;
    return typeName == "float" || typeName == "double" || typeName == "half";
}

bool DataType::isIntegralType() const
{
    if(isPointerType())
        return true;
    return !complexType && (typeName.at(0) == 'i' || typeName == TYPE_BOOL.typeName);
}

bool DataType::isUnknown() const
{
    return typeName[0] == '?';
}

const DataType DataType::getElementType(const int index) const
{
    if(isPointerType())
        return getPointerType().value()->elementType;
    if(getArrayType())
        return getArrayType().value()->elementType;
    if(getStructType() && index >= 0)
        return getStructType().value()->elementTypes.at(static_cast<std::size_t>(index));
    if(complexType)
        throw CompilationError(
            CompilationStep::GENERAL, "Can't get element-type of heterogeneous complex type", to_string());
    if(num == 1)
        return *this;
    return DataType{typeName, 1};
}

const DataType DataType::toPointerType() const
{
    std::shared_ptr<ComplexType> c(new PointerType(*this));
    return DataType(to_string() + "*", 1, c);
}

const DataType DataType::toVectorType(unsigned char vectorWidth) const
{
    if(complexType)
        throw CompilationError(CompilationStep::GENERAL, "Can't form vector-type of complex type", to_string());
    if(vectorWidth > 16 || vectorWidth == 0)
        throw CompilationError(CompilationStep::GENERAL, "Invalid width for SIMD vector",
            std::to_string(static_cast<unsigned>(vectorWidth)));
    if(num == vectorWidth)
        return *this;
    return DataType(typeName, vectorWidth, complexType);
}

bool DataType::containsType(const DataType& other) const
{
    if(*this == other)
        return true;
    if(complexType)
        throw CompilationError(CompilationStep::GENERAL, "Can't check type hierarchy for complex type", to_string());
    if(typeName[0] == 'i' && other.typeName[0] == 'i')
    {
        if(getScalarBitCount() <= other.getScalarBitCount())
        {
            // FIXME correct? doesn't the comparison need to be the other way round?
            return true;
        }
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
    if(isFloatingType() != other.isFloatingType())
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type of floating-point and integer types!");
    return DataType(
        getScalarBitCount() > other.getScalarBitCount() ? typeName : other.typeName, std::max(num, other.num));
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
    if(typeName[0] == 'i')
    {
        int bitCount = atoi(typeName.substr(1).data());
        return static_cast<unsigned char>(bitCount);
    }
    if(typeName.compare("half") == 0)
        // 16-bit floating point type
        return 16;
    if(typeName.compare("double") == 0)
        // 64-bit floating point type
        return 64;
    if(typeName.compare("void") == 0)
        // single byte
        return 8;
    return 32;
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
        return getArrayType().value()->elementType.getPhysicalWidth() * getArrayType().value()->size;
    if(getStructType())
        return getStructType().value()->getStructSize();
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
    if(complexType && num != 1)
        throw CompilationError(CompilationStep::GENERAL, "Can't have vectors of complex types", to_string());
    if(physicalWidth && num == 3)
        // OpenCL 1.2, page 203:
        //"For 3-component vector data types, the size of the data type is 4 * sizeof(component)"
        return 4;
    return num;
}

unsigned DataType::getAlignmentInBytes() const
{
    if(complexType)
        return complexType->getAlignmentInBytes();
    return getPhysicalWidth();
}

std::size_t vc4c::hash<DataType>::operator()(const DataType& type) const noexcept
{
    const std::hash<std::string> nameHash;
    const std::hash<std::shared_ptr<ComplexType>> complexHash;
    const std::hash<unsigned char> numHash;
    return nameHash(type.typeName) ^ complexHash(type.complexType) ^ numHash(type.num);
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

StructType::StructType(const std::vector<DataType>& elementTypes, const bool isPacked) :
    elementTypes(elementTypes), isPacked(isPacked)
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

std::string ImageType::getImageTypeName() const
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
    // TODO ??
    throw CompilationError(CompilationStep::GENERAL, "Alignment for images is not implemented yet");
}

std::string ImageType::toImageConfigurationName(const std::string& localName)
{
    return localName + ".image_config";
}
