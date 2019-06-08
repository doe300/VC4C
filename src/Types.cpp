/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Types.h"

#include "CompilationError.h"
#include "helper.h"

#include <algorithm>
#include <cstdlib>
#include <limits>
#include <sstream>
#include <stdexcept>

using namespace vc4c;

// TODO remove if possible!
TypeHolder GLOBAL_TYPE_HOLDER;
std::unique_ptr<ComplexType> TypeHolder::voidPtr{new PointerType(TYPE_VOID)};
const DataType vc4c::TYPE_VOID_POINTER{TypeHolder::voidPtr.get()};

// just to make sure, the last bits are always zero for pointers
static_assert(alignof(ComplexType) > Bitfield<uint8_t>::MASK_Bit, "");

ComplexType::~ComplexType() noexcept = default;

LCOV_EXCL_START
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
        return "?";
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
LCOV_EXCL_STOP

CONST static ComplexType* toPointer(uintptr_t val) noexcept
{
    return reinterpret_cast<ComplexType*>(val);
}

DataType::DataType(const ComplexType* complexType) : Bitfield(reinterpret_cast<uintptr_t>(complexType))
{
    if(getSimpleFlag())
        throw CompilationError(
            CompilationStep::GENERAL, "Internal error: Complex type has simple flag set", to_string());
    if(!complexType)
        throw CompilationError(
            CompilationStep::GENERAL, "Internal error: Cannot create complex type without a complex type!");
}

LCOV_EXCL_START
std::string DataType::to_string() const
{
    if(!getSimpleFlag())
        return toPointer(getComplexType())->getTypeName();
    const std::string braceLeft(isVectorType() ? "<" : "");
    const std::string braceRight(isVectorType() ? ">" : "");
    auto isFloatingPoint = getFloatingPoint();
    auto bitWidth = getBitWidth();
    auto numElements = getNumElements();
    if(numElements > 1)
        return braceLeft + (std::to_string(numElements) + " x ") +
            (toSimpleTypeName(bitWidth, isFloatingPoint) + braceRight);
    return toSimpleTypeName(bitWidth, isFloatingPoint);
}

std::string DataType::getTypeName(bool isSigned, bool isUnsigned) const
{
    if(!getSimpleFlag() || (!isFloatingType() && !isSigned && !isUnsigned))
        return to_string();
    auto isFloatingPoint = getFloatingPoint();
    auto bitWidth = getBitWidth();
    auto numElements = getNumElements();
    const std::string simpleName = toSimpleTypeName(bitWidth, isFloatingPoint);
    const std::string tName = isFloatingPoint ?
        toFloatingPointTypeName(bitWidth, simpleName) :
        (isSigned ? toSignedIntegerTypeName(bitWidth, simpleName) :
                    isUnsigned ? toUnsignedIntegerTypeName(bitWidth, simpleName) : simpleName);
    if(numElements > 1)
        return tName + std::to_string(numElements);
    return tName;
}
LCOV_EXCL_STOP

bool DataType::operator==(DataType right) const noexcept
{
    if(isSimpleType() || right.isSimpleType())
        return value == right.value;
    auto leftComplex = toPointer(getComplexType());
    auto rightComplex = toPointer(right.getComplexType());
    return leftComplex == rightComplex || *leftComplex == *rightComplex;
}

bool DataType::isSimpleType() const noexcept
{
    return getSimpleFlag();
}

bool DataType::isScalarType() const noexcept
{
    return getSimpleFlag() && getNumElements() == 1;
}

bool DataType::isVectorType() const noexcept
{
    return getSimpleFlag() && getNumElements() > 1;
}

const PointerType* DataType::getPointerType() const noexcept
{
    if(getSimpleFlag())
        return nullptr;
    return dynamic_cast<const PointerType*>(toPointer(getComplexType()));
}

const ArrayType* DataType::getArrayType() const noexcept
{
    if(getSimpleFlag())
        return nullptr;
    return dynamic_cast<const ArrayType*>(toPointer(getComplexType()));
}

const StructType* DataType::getStructType() const noexcept
{
    if(getSimpleFlag())
        return nullptr;
    return dynamic_cast<const StructType*>(toPointer(getComplexType()));
}

const ImageType* DataType::getImageType() const noexcept
{
    if(getSimpleFlag())
        return nullptr;
    return dynamic_cast<const ImageType*>(toPointer(getComplexType()));
}

bool DataType::isFloatingType() const noexcept
{
    return getSimpleFlag() && getFloatingPoint();
}

bool DataType::isIntegralType() const noexcept
{
    if(getPointerType())
        return true;
    return getSimpleFlag() && !getFloatingPoint();
}

bool DataType::isUnknown() const noexcept
{
    return getSimpleFlag() && getBitWidth() == DataType::UNKNOWN;
}

bool DataType::isLabelType() const noexcept
{
    return getSimpleFlag() && getBitWidth() == DataType::LABEL;
}

bool DataType::isVoidType() const noexcept
{
    return getSimpleFlag() && getBitWidth() == DataType::VOID;
}

DataType DataType::getElementType(const int index) const
{
    if(!getSimpleFlag())
    {
        if(auto ptrType = getPointerType())
            return ptrType->elementType;
        if(auto arrayType = getArrayType())
            return arrayType->elementType;
        if(getStructType() && index >= 0)
            return getStructType()->elementTypes.at(static_cast<std::size_t>(index));
        throw CompilationError(
            CompilationStep::GENERAL, "Can't get element-type of heterogeneous complex type", to_string());
    }
    if(getNumElements() == 1)
        return *this;
    return DataType{getBitWidth(), 1, getFloatingPoint()};
}

DataType DataType::toVectorType(unsigned char vectorWidth) const
{
    if(!getSimpleFlag())
        throw CompilationError(CompilationStep::GENERAL, "Can't form vector-type of complex type", to_string());
    if(vectorWidth > 16 || vectorWidth == 0)
        throw CompilationError(CompilationStep::GENERAL, "Invalid width for SIMD vector",
            std::to_string(static_cast<unsigned>(vectorWidth)));
    if(getNumElements() == vectorWidth)
        return *this;
    return DataType{getBitWidth(), vectorWidth, getFloatingPoint()};
}

DataType DataType::toArrayType(unsigned int numElements) const
{
    return DataType(GLOBAL_TYPE_HOLDER.createArrayType(*this, numElements));
}

bool DataType::containsType(DataType other) const
{
    if(*this == other)
        return true;
    auto thisBitWidth = getBitWidth();
    auto otherBitWidth = other.getBitWidth();
    auto thisFloatType = getFloatingPoint();
    auto otherFloatType = other.getFloatingPoint();
    if(!getSimpleFlag())
    {
        thisFloatType = false;
        if(getPointerType())
            thisBitWidth = 32;
        else
            throw CompilationError(
                CompilationStep::GENERAL, "Can't check type hierarchy for complex type", to_string());
    }
    if(!other.getSimpleFlag())
    {
        otherFloatType = false;
        if(other.getPointerType())
            otherBitWidth = 32;
        else
            throw CompilationError(
                CompilationStep::GENERAL, "Can't check type hierarchy for complex type", other.to_string());
    }
    if(!thisFloatType && !otherFloatType && thisBitWidth <= otherBitWidth)
    {
        // FIXME correct? doesn't the comparison need to be the other way round?
        return true;
    }
    return false;
}

DataType DataType::getUnionType(DataType other) const
{
    if(*this == other)
        return *this;
    // doesn't work for heterogeneous types
    if(!getSimpleFlag() || !other.getSimpleFlag())
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type of distinct complex types!");
    if(getFloatingPoint() != other.getFloatingPoint())
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type of floating-point and integer types!");
    if(isVoidType() || isUnknown() || isLabelType())
        throw CompilationError(CompilationStep::GENERAL, "Can't form union type with this type", to_string());
    return DataType{std::max(getScalarBitCount(), other.getScalarBitCount()),
        std::max(getNumElements(), other.getNumElements()), getFloatingPoint()};
}

unsigned char DataType::getScalarBitCount() const
{
    if(!getSimpleFlag())
    {
        if(getPointerType())
            // 32-bit pointer
            return 32;
        if(getImageType())
            // images are pointers to the image-data
            return 32;
        throw CompilationError(CompilationStep::GENERAL, "Can't get bit-width of complex type", to_string());
    }
    if(isVoidType())
        // single byte
        return 8;
    if(isUnknown())
        return 32;
    return getBitWidth();
}

uint32_t DataType::getScalarWidthMask() const
{
    return static_cast<uint32_t>((static_cast<uint64_t>(1) << getScalarBitCount()) - 1);
}

unsigned int DataType::getLogicalWidth() const
{
    if(!getSimpleFlag())
    {
        if(getPointerType())
            // 32-bit pointer
            return 4;
        if(auto arrayType = getArrayType())
            return arrayType->elementType.getLogicalWidth() * arrayType->size;
        if(auto structType = getStructType())
            return structType->getStructSize();
        if(getImageType())
            // images are just pointers to data
            // 32-bit pointer
            return 4;
        // any other complex type
        throw CompilationError(CompilationStep::GENERAL, "Can't get width of complex type", to_string());
    }
    return getVectorWidth(false) * getScalarBitCount() / 8;
}

unsigned int DataType::getInMemoryWidth() const
{
    if(!getSimpleFlag())
    {
        if(getPointerType())
            // 32-bit pointer
            return 4;
        if(auto arrayType = getArrayType())
            return arrayType->elementType.getInMemoryWidth() * arrayType->size;
        if(auto structType = getStructType())
            return structType->getStructSize();
        if(getImageType())
            // images are just pointers to data
            // 32-bit pointer
            return 4;
        // any other complex type
        throw CompilationError(CompilationStep::GENERAL, "Can't get width of complex type", to_string());
    }
    return getVectorWidth(true) * getScalarBitCount() / 8;
}

unsigned char DataType::getVectorWidth(bool physicalWidth) const noexcept
{
    auto numElements = getNumElements();
    if(!getSimpleFlag())
        // Vectors of complex types are not allowed
        return 1;
    if(physicalWidth && numElements == 3)
        // OpenCL 1.2, page 203:
        //"For 3-component vector data types, the size of the data type is 4 * sizeof(component)"
        return 4;
    return numElements;
}

unsigned DataType::getInMemoryAlignment() const
{
    if(!getSimpleFlag())
        return toPointer(getComplexType())->getInMemoryAlignment();
    return getInMemoryWidth();
}

bool PointerType::operator==(const ComplexType& other) const
{
    if(this == &other)
        return true;
    const PointerType* right = dynamic_cast<const PointerType*>(&other);
    if(right == nullptr)
        return false;
    // TODO do we need also check for address space?!
    return elementType == right->elementType;
}

unsigned PointerType::getAlignment() const
{
    if(alignment != 0)
        return alignment;
    return elementType.getInMemoryWidth();
}

unsigned PointerType::getInMemoryAlignment() const
{
    // pointer are words, so they are aligned as such
    return 4;
}

LCOV_EXCL_START
std::string PointerType::getTypeName() const
{
    return toString(addressSpace, true) + " " + elementType.to_string() + "*";
}
LCOV_EXCL_STOP

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
            size += elementTypes[i].getInMemoryWidth();
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
        auto elementAlignment = elementTypes[i].getInMemoryAlignment();
        alignment = std::max(alignment, elementAlignment);
        // OpenCL 1.2, page 203
        //"A data item declared to be a data type in memory is always aligned to the size of the data type in bytes."
        if(size % elementAlignment != 0)
        {
            // alignment is added before the next type, not after the last
            size += elementAlignment - (size % elementAlignment);
        }
        size += elementTypes[i].getInMemoryWidth();
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
        auto elementAlignment = elementTypes.at(static_cast<std::size_t>(index)).getInMemoryAlignment();
        if(size % elementAlignment != 0)
        {
            // alignment is added before the next type, not after the last
            size += elementAlignment - (size % elementAlignment);
        }
    }

    return size;
}

unsigned StructType::getInMemoryAlignment() const
{
    return getStructSize();
}

LCOV_EXCL_START
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
LCOV_EXCL_STOP

bool ArrayType::operator==(const ComplexType& other) const
{
    if(this == &other)
        return true;
    const ArrayType* right = dynamic_cast<const ArrayType*>(&other);
    if(right == nullptr)
        return false;
    return size == right->size && elementType == right->elementType;
}

unsigned ArrayType::getInMemoryAlignment() const
{
    // arrays are only aligned to their element-type size, not their complete size
    return elementType.getInMemoryWidth();
}

LCOV_EXCL_START
std::string ArrayType::getTypeName() const
{
    return (elementType.to_string() + "[") + std::to_string(size) + "]";
}
LCOV_EXCL_STOP

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

LCOV_EXCL_START
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
LCOV_EXCL_STOP

unsigned ImageType::getInMemoryAlignment() const
{
    throw CompilationError(CompilationStep::GENERAL, "Alignment for images is not implemented yet");
}

std::string ImageType::toImageConfigurationName(const std::string& localName)
{
    return localName + ".image_config";
}

PointerType* TypeHolder::createPointerType(DataType elementType, AddressSpace addressSpace, unsigned alignment)
{
    std::lock_guard<std::mutex> guard(accessMutex);
    std::unique_ptr<ComplexType> tmp(new PointerType(elementType, addressSpace, alignment));
    auto it = std::find_if(complexTypes.begin(), complexTypes.end(), [&](const auto& type) -> bool {
        auto ptrType = dynamic_cast<PointerType*>(type.get());
        // PointerType::operator== only checks for elementType, which is too little for this here
        return ptrType && ptrType->elementType == elementType && ptrType->addressSpace == addressSpace;
    });
    if(it != complexTypes.end())
        return dynamic_cast<PointerType*>(it->get());
    complexTypes.emplace_back(std::move(tmp));
    return dynamic_cast<PointerType*>(complexTypes.back().get());
}

StructType* TypeHolder::createStructType(
    const std::string& name, const std::vector<DataType>& elementTypes, bool isPacked)
{
    std::lock_guard<std::mutex> guard(accessMutex);
    std::unique_ptr<ComplexType> tmp(new StructType(name, elementTypes, isPacked));
    auto it = std::find_if(complexTypes.begin(), complexTypes.end(), [&](const auto& type) -> bool {
        auto structType = dynamic_cast<StructType*>(type.get());
        return structType && *structType == *tmp;
    });
    if(it != complexTypes.end())
        return dynamic_cast<StructType*>(it->get());
    complexTypes.emplace_back(std::move(tmp));
    return dynamic_cast<StructType*>(complexTypes.back().get());
}

ArrayType* TypeHolder::createArrayType(DataType elementType, unsigned int size)
{
    std::lock_guard<std::mutex> guard(accessMutex);
    std::unique_ptr<ComplexType> tmp(new ArrayType(elementType, size));
    auto it = std::find_if(complexTypes.begin(), complexTypes.end(), [&](const auto& type) -> bool {
        auto arrayType = dynamic_cast<ArrayType*>(type.get());
        return arrayType && *arrayType == *tmp;
    });
    if(it != complexTypes.end())
        return dynamic_cast<ArrayType*>(it->get());
    complexTypes.emplace_back(std::move(tmp));
    return dynamic_cast<ArrayType*>(complexTypes.back().get());
}

ImageType* TypeHolder::createImageType(uint8_t dimensions, bool isImageArray, bool isImageBuffer, bool isSampled)
{
    std::lock_guard<std::mutex> guard(accessMutex);
    std::unique_ptr<ComplexType> tmp(new ImageType(dimensions, isImageArray, isImageBuffer, isSampled));
    auto it = std::find_if(complexTypes.begin(), complexTypes.end(), [&](const auto& type) -> bool {
        auto imgType = dynamic_cast<ImageType*>(type.get());
        return imgType && *imgType == *tmp;
    });
    if(it != complexTypes.end())
        return dynamic_cast<ImageType*>(it->get());
    complexTypes.emplace_back(std::move(tmp));
    return dynamic_cast<ImageType*>(complexTypes.back().get());
}
