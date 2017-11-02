/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRVOPERATION_H
#define SPIRVOPERATION_H
#ifdef SPIRV_HEADER

#include <memory>
#include <map>
#include <vector>

#include "../Module.h"
#include "helper.h"
#include "../intermediate/IntermediateInstruction.h"

namespace vc4c
{
	namespace spirv2qasm
	{
		static constexpr uint32_t UNDEFINED_ID { 0 };

		struct SPIRVMethod
		{
			const uint32_t id;
			std::unique_ptr<Method> method;
			std::vector<std::pair<uint32_t, uint32_t>> parameters;

			SPIRVMethod(const uint32_t id, const Module& module) : id(id), method(new Method(module))
			{

			}
		};

		class SPIRVOperation
		{
		public:
			SPIRVOperation(const uint32_t id, SPIRVMethod& method, const intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE);
			virtual ~SPIRVOperation();

			virtual void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods,
					std::map<uint32_t, Global*>& globals) const = 0;
			virtual Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const = 0;

		protected:
			const uint32_t id;
			SPIRVMethod& method;
			intermediate::InstructionDecorations decorations;

		};

		static const std::string OP_NEGATE("negate");

		class SPIRVInstruction: public SPIRVOperation
		{
		public:

			SPIRVInstruction(const uint32_t id, SPIRVMethod& method, const std::string& opcode, const uint32_t resultType, const std::vector<uint32_t>& operands, const intermediate::InstructionDecorations decorations =
					intermediate::InstructionDecorations::NONE);
			virtual ~SPIRVInstruction();

			virtual void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		protected:
			uint32_t typeID;
			std::string opcode;
			std::vector<uint32_t> operands;
		};

		class SPIRVComparison: public SPIRVInstruction
		{
		public:

			SPIRVComparison(const uint32_t id, SPIRVMethod& method, const std::string& opcode, const uint32_t resultType, const std::vector<uint32_t>& operands, const intermediate::InstructionDecorations decorations =
					intermediate::InstructionDecorations::NONE);
			virtual ~SPIRVComparison();

			virtual void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		};

		class SPIRVCallSite: public SPIRVOperation
		{
		public:
			SPIRVCallSite(const uint32_t id, SPIRVMethod& method, const uint32_t methodID, const uint32_t resultType, const std::vector<uint32_t>& arguments);
			SPIRVCallSite(const uint32_t id, SPIRVMethod& method, const std::string& methodName, const uint32_t resultType, const std::vector<uint32_t>& arguments);
			virtual ~SPIRVCallSite();

			virtual void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			Optional<uint32_t> methodID;
			const uint32_t typeID;
			Optional<std::string> methodName;
			std::vector<uint32_t> arguments;
		};

		class SPIRVReturn: public SPIRVOperation
		{
		public:
			SPIRVReturn(SPIRVMethod& method);
			SPIRVReturn(const uint32_t returnValue, SPIRVMethod& method);
			virtual ~SPIRVReturn();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			Optional<uint32_t> returnValue;
		};

		class SPIRVBranch: public SPIRVOperation
		{
		public:
			SPIRVBranch(SPIRVMethod& method, const uint32_t labelID);
			SPIRVBranch(SPIRVMethod& method, const uint32_t conditionID, const uint32_t trueLabelID, const uint32_t falseLabelID);
			virtual ~SPIRVBranch();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		private:
			const uint32_t defaultLabelID;
			const Optional<uint32_t> conditionID;
			const Optional<uint32_t> falseLabelID;
		};

		class SPIRVLabel: public SPIRVOperation
		{
		public:
			SPIRVLabel(const uint32_t id, SPIRVMethod& method);
			virtual ~SPIRVLabel();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		};

		enum class ConversionType
		{
			SIGNED, UNSIGNED, FLOATING, BITCAST
		};

		class SPIRVConversion: public SPIRVOperation
		{
		public:
			SPIRVConversion(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID, const ConversionType type, const intermediate::InstructionDecorations decorations, bool isSaturated = false);
			virtual ~SPIRVConversion();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		private:
			const uint32_t typeID;
			const uint32_t sourceID;
			const ConversionType type;
			const bool isSaturated;
		};

		enum class MemoryAccess
		{
			NONE = 0x0, READ = 0x1, WRITE = 0x2, READ_WRITE = 0x3
		};

		class SPIRVCopy: public SPIRVOperation
		{
		public:
			//copies whole object
			SPIRVCopy(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID, const MemoryAccess memoryAccess = MemoryAccess::NONE, const uint32_t size = UNDEFINED_ID);
			//copies single parts
			SPIRVCopy(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID, const std::vector<uint32_t>& destIndices, const std::vector<uint32_t>& sourceIndices);
			virtual ~SPIRVCopy();
			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			const uint32_t typeID;
			const uint32_t sourceID;
			const MemoryAccess memoryAccess;
			const Optional<uint32_t> sizeID;
			const Optional<std::vector<uint32_t>> destIndices;
			const Optional<std::vector<uint32_t>> sourceIndices;
		};

		class SPIRVShuffle: public SPIRVOperation
		{
		public:
			SPIRVShuffle(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID0, const uint32_t sourceID1, const std::vector<uint32_t>& indices);
			SPIRVShuffle(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID0, const uint32_t sourceID1, const uint32_t compositeIndex);
			virtual ~SPIRVShuffle();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			const uint32_t typeID;
			const uint32_t source0;
			const uint32_t source1;
			const std::vector<uint32_t> indices;
			const bool compositeIndex;
		};

		class SPIRVIndexOf: public SPIRVOperation
		{
		public:
			SPIRVIndexOf(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t containerID, const std::vector<uint32_t>& indices, const bool isPtrAcessChain);
			virtual ~SPIRVIndexOf();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			const uint32_t typeID;
			const uint32_t container;
			const std::vector<uint32_t> indices;
			const bool isPtrAcessChain;
		};

		class SPIRVPhi: public SPIRVOperation
		{
		public:
			SPIRVPhi(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const std::vector<std::pair<uint32_t, uint32_t>>& sources);
			virtual ~SPIRVPhi();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			const uint32_t typeID;
			const std::vector<std::pair<uint32_t, uint32_t>> sources;
		};

		class SPIRVSelect: public SPIRVOperation
		{
		public:
			SPIRVSelect(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t conditionID, const uint32_t trueObj, const uint32_t falseObj);
			virtual ~SPIRVSelect();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		private:
			const uint32_t typeID;
			const uint32_t condID;
			const uint32_t trueID;
			const uint32_t falseID;
		};

		class SPIRVSwitch: public SPIRVOperation
		{
		public:
			SPIRVSwitch(const uint32_t id, SPIRVMethod& method, const uint32_t selectorID, const uint32_t defaultID, const std::vector<std::pair<uint32_t, uint32_t>>& destinations);
			virtual ~SPIRVSwitch();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			const uint32_t selectorID;
			const uint32_t defaultID;
			const std::vector<std::pair<uint32_t, uint32_t>> destinations;
		};

		enum class ImageQuery
		{
			CHANNEL_DATA_TYPE, CHANNEL_ORDER, SIZES, SIZES_LOD, MIPMAP_LEVELS, SAMPLES_PER_TEXEL
		};

		class SPIRVImageQuery: public SPIRVOperation
		{
		public:
			SPIRVImageQuery(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const ImageQuery value, const uint32_t imageID, const uint32_t lodOrCoordinate = UNDEFINED_ID);
			virtual ~SPIRVImageQuery();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;

		private:
			const uint32_t typeID;
			const ImageQuery valueID;
			const uint32_t imageID;
			const uint32_t lodOrCoordinate;
		};

		class SPIRVMemoryBarrier : public SPIRVOperation
		{
		public:
			SPIRVMemoryBarrier(SPIRVMethod& method, const uint32_t scopeID, const uint32_t semanticsID);
			virtual ~SPIRVMemoryBarrier();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		private:
			const uint32_t scopeID;
			const uint32_t semanticsID;
		};

		class SPIRVLifetimeInstruction : public SPIRVOperation
		{
		public:
			SPIRVLifetimeInstruction(const uint32_t id, SPIRVMethod& method, uint32_t size, bool lifetimeEnd, const intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE);
			virtual ~SPIRVLifetimeInstruction();

			void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
					override;
			Optional<Value> precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const override;
		private:
			const uint32_t sizeInBytes;
			const bool isLifetimeEnd;
		};
	}
}



#endif
#endif /* SPIRVOPERATION_H */

