/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Types.h"
#include "CompilationError.h"
#include "helper.h"

#include <stdexcept>
#include <stdlib.h>
#include <limits>

using namespace vc4c;

ComplexType::~ComplexType()
{

}

DataType::DataType(const std::string& name, const unsigned char num, const std::shared_ptr<ComplexType>& complexType) :
    typeName(name), num(num), complexType(complexType)
{

}

DataType::~DataType()
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

bool DataType::operator==(const DataType& right) const
{
	if(this == &right)
		return true;
    return typeName.compare(right.typeName) == 0 && num == right.num && (complexType.get() == nullptr) == (right.complexType.get() == nullptr) && (complexType.get() != nullptr ? (*complexType.get()) == (*right.complexType.get()) : true);
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
	return getPointerType().hasValue;
}

Optional<PointerType*> DataType::getPointerType() const
{
	PointerType* pointerType = dynamic_cast<PointerType*>(complexType.get());
	if(pointerType)
			return pointerType;
	return Optional<PointerType*>(false, nullptr);
}

Optional<ArrayType*> DataType::getArrayType() const
{
	ArrayType* arrayType = dynamic_cast<ArrayType*>(complexType.get());
	if(arrayType)
		return arrayType;
	return Optional<ArrayType*>(false, nullptr);
}

Optional<StructType*> DataType::getStructType() const
{
	StructType* structType = dynamic_cast<StructType*>(complexType.get());
	if(structType)
		return structType;
	return Optional<StructType*>(false, nullptr);
}


Optional<ImageType*> DataType::getImageType() const
{
	ImageType* imageType = dynamic_cast<ImageType*>(complexType.get());
	if(imageType)
		return imageType;
	return Optional<ImageType*>(false, nullptr);
}

bool DataType::isFloatingType() const
{
	if(complexType)
		return false;
    return typeName.compare("float") == 0 || typeName.compare("double") == 0 || typeName.compare("half") == 0;
}

bool DataType::isUnknown() const
{
    return typeName[0] == '?';
}

const DataType DataType::getElementType(const int index) const
{
	if(isPointerType())
		return getPointerType().get()->elementType;
	if(getArrayType().hasValue)
		return getArrayType().get()->elementType;
	if(getStructType().hasValue && index >= 0)
		return getStructType().get()->elementTypes.at(index);
	if(complexType)
		throw CompilationError(CompilationStep::GENERAL, "Can't get element-type of heterogeneous complex type", to_string());
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
		throw CompilationError(CompilationStep::GENERAL, "Invalid width for SIMD vector", std::to_string(static_cast<unsigned>(vectorWidth)));
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
            return true;
        }
    }
    return false;
}

const DataType DataType::getUnionType(const DataType& other) const
{
	if(*this == other)
		return *this;
	//doesn't work for heterogeneous types
	if(complexType || other.complexType)
		throw CompilationError(CompilationStep::GENERAL, "Can't form union type of distinct complex types!");
	if(isFloatingType() != other.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can't form union type of floating-point and integer types!");
	return DataType(getScalarBitCount() > other.getScalarBitCount() ? typeName : other.typeName, std::max(num, other.num));
}

unsigned char DataType::getScalarBitCount() const
{
	if(isPointerType())
		//32-bit pointer
		return 32;
	if(getImageType().hasValue)
		//images are pointers to the image-data
		return 32;
	if(complexType)
		throw CompilationError(CompilationStep::GENERAL, "Can't get bit-width of complex type", to_string());
    if(typeName[0] == 'i')
    {
        int bitCount = atoi(typeName.substr(1).data());
        return bitCount;
    }
    if(typeName.compare("half") == 0)
    	//16-bit floating point type
    	return 16;
    if(typeName.compare("void") == 0)
    	//single byte
    	return 8;
    return 32;
}

uint64_t DataType::getScalarWidthMask() const
{
	return (static_cast<uint64_t>(1) << getScalarBitCount()) - 1;
}

unsigned int DataType::getPhysicalWidth() const
{
	if(isPointerType())
		//32-bit pointer
		return 4;
	if(getArrayType().hasValue)
		return getArrayType().get()->elementType.getPhysicalWidth() * getArrayType().get()->size;
	if(getStructType().hasValue)
		return getStructType().get()->getStructSize();
	if(getImageType().hasValue)
		//images are just pointers to data
		//32-bit pointer
		return 4;
	if(complexType)
		//any other complex type
		throw CompilationError(CompilationStep::GENERAL, "Can't get width of complex type", to_string());
	return getVectorWidth(true) * getScalarBitCount() / 8;
}

unsigned int DataType::getVectorWidth(bool physicalWidth) const
{
	if(complexType && num != 1)
		throw CompilationError(CompilationStep::GENERAL, "Can't have vectors of complex types", to_string());
	if(physicalWidth && num == 3)
		//OpenCL 1.2, page 203:
		//"For 3-component vector data types, the size of the data type is 4 * sizeof(component)"
		return 4;
	return num;
}

PointerType::PointerType(const DataType& elementType, const AddressSpace addressSpace, unsigned alignment) : elementType(elementType), addressSpace(addressSpace), alignment(alignment)
{

}

PointerType::~PointerType()
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

StructType::StructType(const std::vector<DataType>& elementTypes, const bool isPacked) : elementTypes(elementTypes), isPacked(isPacked)
{

}

StructType::~StructType()
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
        //packed -> no padding
        for(std::size_t i = 0; i < elementTypes.size(); ++i)
        {
        	if(i == static_cast<std::size_t>(index))
        		break;
            size += elementTypes.at(i).getPhysicalWidth();
        }
        return size;
    }
    //TODO calculate size of struct (!!SAME AS HOST!!) including padding -> correct??
    //http://stackoverflow.com/a/2749096
    unsigned int alignment = 0;
    for(std::size_t i = 0; i < elementTypes.size(); ++i)
    {
    	if(i == static_cast<std::size_t>(index))
    		break;
    	auto elementSize = elementTypes.at(i).getPhysicalWidth();
    	alignment = std::max(alignment, elementSize);
    	//OpenCL 1.2, page 203
		//"A data item declared to be a data type in memory is always aligned to the size of the data type in bytes."
    	if(size % elementSize != 0)
    	{
    		//alignment is added before the next type, not after the last
    		size += elementSize - (size % elementSize);
    	}
    	size += elementTypes.at(i).getPhysicalWidth();
    }
    if(index < 0)
    {
    	//whole object
    	//padding at end of struct to align to alignment of largest
		if(size % alignment != 0)
		{
			size += alignment - (size % alignment);
		}
    }
    else
    {
    	//add padding for element which is retrieved
    	auto elementSize = elementTypes.at(index).getPhysicalWidth();
    	if(size % elementSize != 0)
		{
			//alignment is added before the next type, not after the last
			size += elementSize - (size % elementSize);
		}
    }

    return size;
}

ArrayType::ArrayType(const DataType& elementType, const unsigned int size) : elementType(elementType), size(size)
{

}

ArrayType::~ArrayType()
{

}

bool ArrayType::operator==(const ComplexType& other) const
{
	if(this == &other)
		return true;
	const ArrayType* right = dynamic_cast<const ArrayType*>(&other);
	if(right == nullptr)
		return false;
	return size == right->size && elementType == right->elementType;
}

ImageType::~ImageType()
{

}

bool ImageType::operator==(const ComplexType& other) const
{
	if(this == &other)
		return true;
	const ImageType* right = dynamic_cast<const ImageType*>(&other);
	if(right == nullptr)
		return false;
	return colorType == right->colorType && dimensions == right->dimensions && isImageArray == right->isImageArray && isImageBuffer == right->isImageBuffer && isSampled == right->isSampled;
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

std::string ImageType::toImageConfigurationName(const std::string& localName)
{
	return localName + ".image_config";
}
