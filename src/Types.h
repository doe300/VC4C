/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <vector>
#include <memory>

#include "helper.h"

namespace vc4c
{
	static constexpr int WHOLE_OBJECT { -1 };
	static constexpr int ANY_ELEMENT { -1 };

	struct ComplexType
	{
		virtual ~ComplexType();

		virtual bool operator==(const ComplexType& other) const = 0;
	};

	struct PointerType;
	struct ArrayType;
	struct StructType;
	struct ImageType;

	struct DataType
	{
		std::string typeName;
		//the number of elements for vector-types
		unsigned char num;
		std::shared_ptr<ComplexType> complexType;

		DataType(const std::string& name = "", const unsigned char num = 1, const std::shared_ptr<ComplexType>& complexType = nullptr);
		virtual ~DataType();

		std::string to_string() const;

		bool operator==(const DataType& right) const;
		inline bool operator!=(const DataType& right) const
		{
			return !(*this == right);
		}

		//"simple" types
		//vector can only be vector of scalars, so its counts as simple type!
		bool isScalarType() const;
		bool isVectorType() const;

		//"complex" types
		bool isPointerType() const;
		Optional<PointerType*> getPointerType() const;
		Optional<ArrayType*> getArrayType() const;
		Optional<StructType*> getStructType() const;
		Optional<ImageType*> getImageType() const;

		bool isFloatingType() const;
		bool isUnknown() const;

		const DataType getElementType(const int index = ANY_ELEMENT) const;
		const DataType toPointerType() const;
		const DataType toVectorType(unsigned char vectorWidth) const;

		bool containsType(const DataType& other) const;
		const DataType getUnionType(const DataType& other) const;

		unsigned char getScalarBitCount() const;
		uint64_t getScalarWidthMask() const;

		/*
		 * Returns the width of this type in bytes
		 */
		unsigned int getPhysicalWidth() const;

		/*
		 * Returns the logical or physical vector width of this type.
		 * The logical vector width is the number of SIMD elements used
		 * The physical vector width is the number of elements used in RAM
		 */
		unsigned int getVectorWidth(bool physicalWidth = false) const;
	};

	static const DataType TYPE_INT8 = DataType { "i8", 1};
	static const DataType TYPE_INT16 = DataType { "i16", 1 };
	static const DataType TYPE_INT32 = DataType { "i32", 1 };
	static const DataType TYPE_INT64 = DataType { "i64", 1 };
	static const DataType TYPE_FLOAT = DataType { "float", 1 };
	static const DataType TYPE_HALF = DataType { "half", 1 };
	static const DataType TYPE_BOOL = DataType { "bool", 1 };
	static const DataType TYPE_VOID = DataType { "void", 1 };
	static const DataType TYPE_UNKNOWN = DataType { "?", 1 };
	static const DataType TYPE_LABEL = DataType { "label", 1 };
//sampler is uint32
	static const DataType TYPE_SAMPLER = TYPE_INT32;
//event is uint32
	static const DataType TYPE_EVENT = TYPE_INT32;

	enum class AddressSpace
	{
		//the generic address space (SPIR-V StorageClassGeneric)
		GENERIC = 0,
		//private memory, only for the single execution, OpenCL defaults to this (SPIR-V StorageClassFunction)
		PRIVATE = 1,
		//global memory pool, shared between all kernel executions (SPIR-V StorageClassCrossWorkgroup)
		GLOBAL = 2,
		//constant memory, usually as part of the binary (global data segment) (SPIR-V StorageClassUniformConstant)
		CONSTANT = 3,
		//local memory, shared between all work-items in the work-group (SPIR-V StorageClassWorkgroup)
		LOCAL = 4
	};

	//not really that complex, but this allows e.g. pointer of pointers
	struct PointerType : public ComplexType
	{
		DataType elementType;
		AddressSpace addressSpace;
		unsigned alignment;

		PointerType(const DataType& elementType, const AddressSpace addressSpace = AddressSpace::PRIVATE, unsigned alignment = 0);
		virtual ~PointerType();
		bool operator==(const ComplexType& other) const override;
		unsigned getAlignment() const;

	};

	struct StructType: public ComplexType
	{
		std::vector<DataType> elementTypes;
		//"which indicate that the alignment of the struct is one byte, and that there is no padding between the elements" - LLVM IR
		//"Apply to a structure type, to marks it as "packed", indicating that the alignment of the structure is one and that there is no padding between structure members." - SPIR-V
		bool isPacked;

		StructType(const std::vector<DataType>& elementTypes, const bool isPacked = false);
		virtual ~StructType();
		bool operator==(const ComplexType& other) const override;

		/*
		 * Calculates the size of the struct up to the given index (of all elements excluding the given index) in Bytes.
		 * If index is -1 (WHOLE_OBJECT), the complete size of the struct is returned
		 */
		unsigned int getStructSize(const int index = WHOLE_OBJECT) const;
	};

	struct ArrayType: public ComplexType
	{
		DataType elementType;
		unsigned int size;

		ArrayType(const DataType& elementType, const unsigned int size);
		virtual ~ArrayType();
		bool operator==(const ComplexType& other) const override;
	};

	struct ImageType: public ComplexType
	{
		DataType colorType;
		uint8_t dimensions;
		bool isImageArray;
		bool isImageBuffer;
		bool isSampled;

		virtual ~ImageType();
		bool operator==(const ComplexType& other) const override;

		std::string getImageTypeName() const;

		static std::string toImageConfigurationName(const std::string& localName);
	};

	//TODO move somewhere else?

	enum class Semaphore
	{
		BARRIER_WORK_ITEM_0 = 0,
		BARRIER_WORK_ITEM_1 = 1,
		BARRIER_WORK_ITEM_2 = 2,
		BARRIER_WORK_ITEM_3 = 3,
		BARRIER_WORK_ITEM_4 = 4,
		BARRIER_WORK_ITEM_5 = 5,
		BARRIER_WORK_ITEM_6 = 6,
		BARRIER_WORK_ITEM_7 = 7,
		BARRIER_WORK_ITEM_8 = 8,
		BARRIER_WORK_ITEM_9 = 9,
		BARRIER_WORK_ITEM_10 = 10,
		BARRIER_WORK_ITEM_11 = 11,
		BARRIER_SFU_SLICE_0 = 12,
		BARRIER_SFU_SLICE_1 = 13,
		BARRIER_SFU_SLICE_2 = 14,
		BARRIER_SFU_SLICE_3 = 15
	};

}

#endif /* TYPES_H */

