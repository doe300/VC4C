/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SPIRVOPERATION_H
#define SPIRVOPERATION_H
#ifdef SPIRV_FRONTEND

#include "../Module.h"
#include "Optional.h"

#include <map>
#include <memory>
#include <vector>

namespace vc4c
{
    namespace intermediate
    {
        enum class InstructionDecorations;
    } // namespace intermediate

    namespace spirv2qasm
    {
        static constexpr uint32_t UNDEFINED_ID{0};
        static constexpr uint32_t UNDEFINED_LITERAL{0xFFFFFFFF};

        struct SPIRVMethod
        {
            std::unique_ptr<Method> method;
            std::vector<std::pair<uint32_t, uint32_t>> parameters;
            const uint32_t id;

            SPIRVMethod(uint32_t id, const Module& module) : method(new Method(module)), id(id) {}
        };

        using TypeMapping = std::map<uint32_t, DataType>;
        using ConstantMapping = std::map<uint32_t, Value>;
        using LocalTypeMapping = std::map<uint32_t, uint32_t>;
        using MethodMapping = std::map<uint32_t, SPIRVMethod>;
        using AllocationMapping = std::map<uint32_t, Local*>;

        class SPIRVOperation
        {
        public:
            SPIRVOperation(uint32_t id, SPIRVMethod& method,
                intermediate::InstructionDecorations decorations = static_cast<intermediate::InstructionDecorations>(
                    0));
            virtual ~SPIRVOperation();

            virtual void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const = 0;
            virtual Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const = 0;

        protected:
            const uint32_t id;
            SPIRVMethod& method;
            intermediate::InstructionDecorations decorations;
        };

        static const std::string OP_NEGATE("negate");

        class SPIRVInstruction : public SPIRVOperation
        {
        public:
            SPIRVInstruction(uint32_t id, SPIRVMethod& method, const std::string& opcode, uint32_t resultType,
                const std::vector<uint32_t>& operands,
                intermediate::InstructionDecorations decorations = static_cast<intermediate::InstructionDecorations>(
                    0));
            ~SPIRVInstruction() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        protected:
            uint32_t typeID;
            std::string opcode;
            std::vector<uint32_t> operands;
        };

        class SPIRVComparison : public SPIRVInstruction
        {
        public:
            SPIRVComparison(uint32_t id, SPIRVMethod& method, const std::string& opcode, uint32_t resultType,
                const std::vector<uint32_t>& operands,
                intermediate::InstructionDecorations decorations = static_cast<intermediate::InstructionDecorations>(
                    0));
            ~SPIRVComparison() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;
        };

        class SPIRVCallSite : public SPIRVOperation
        {
        public:
            SPIRVCallSite(uint32_t id, SPIRVMethod& method, uint32_t methodID, uint32_t resultType,
                const std::vector<uint32_t>& arguments);
            SPIRVCallSite(uint32_t id, SPIRVMethod& method, const std::string& methodName, uint32_t resultType,
                const std::vector<uint32_t>& arguments);
            ~SPIRVCallSite() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            Optional<uint32_t> methodID;
            const uint32_t typeID;
            Optional<std::string> methodName;
            std::vector<uint32_t> arguments;
        };

        class SPIRVReturn : public SPIRVOperation
        {
        public:
            explicit SPIRVReturn(SPIRVMethod& method);
            SPIRVReturn(uint32_t returnValue, SPIRVMethod& method);
            ~SPIRVReturn() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            Optional<uint32_t> returnValue;
        };

        class SPIRVBranch : public SPIRVOperation
        {
        public:
            SPIRVBranch(SPIRVMethod& method, uint32_t labelID);
            SPIRVBranch(SPIRVMethod& method, uint32_t conditionID, uint32_t trueLabelID, uint32_t falseLabelID);
            ~SPIRVBranch() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t defaultLabelID;
            const Optional<uint32_t> conditionID;
            const Optional<uint32_t> falseLabelID;
        };

        class SPIRVLabel : public SPIRVOperation
        {
        public:
            SPIRVLabel(uint32_t id, SPIRVMethod& method);
            ~SPIRVLabel() override = default;

            void mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants,
                std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods,
                std::map<uint32_t, Local*>& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;
        };

        enum class ConversionType
        {
            SIGNED,
            UNSIGNED,
            FLOATING,
            BITCAST
        };

        class SPIRVConversion : public SPIRVOperation
        {
        public:
            SPIRVConversion(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t sourceID,
                ConversionType type, intermediate::InstructionDecorations decorations, bool isSaturated = false);
            ~SPIRVConversion() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const uint32_t sourceID;
            const ConversionType type;
            const bool isSaturated;
        };

        enum class MemoryAccess
        {
            NONE = 0x0,
            READ = 0x1,
            WRITE = 0x2,
            READ_WRITE = 0x3
        };

        class SPIRVCopy : public SPIRVOperation
        {
        public:
            // copies whole object
            SPIRVCopy(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t sourceID,
                MemoryAccess memoryAccess = MemoryAccess::NONE, uint32_t size = UNDEFINED_ID);
            // copies single parts
            SPIRVCopy(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t sourceID,
                const std::vector<uint32_t>& destIndices, const std::vector<uint32_t>& sourceIndices);
            ~SPIRVCopy() override = default;
            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const uint32_t sourceID;
            const MemoryAccess memoryAccess;
            const Optional<uint32_t> sizeID;
            const Optional<std::vector<uint32_t>> destIndices;
            const Optional<std::vector<uint32_t>> sourceIndices;
        };

        class SPIRVShuffle : public SPIRVOperation
        {
        public:
            SPIRVShuffle(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t sourceID0, uint32_t sourceID1,
                const std::vector<uint32_t>& indices);
            SPIRVShuffle(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t sourceID0, uint32_t sourceID1,
                uint32_t compositeIndex);
            ~SPIRVShuffle() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const uint32_t source0;
            const uint32_t source1;
            const std::vector<uint32_t> indices;
            const bool compositeIndex;
        };

        class SPIRVIndexOf : public SPIRVOperation
        {
        public:
            SPIRVIndexOf(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t containerID,
                const std::vector<uint32_t>& indices, bool isPtrAcessChain);
            ~SPIRVIndexOf() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const uint32_t container;
            const std::vector<uint32_t> indices;
            const bool isPtrAcessChain;
        };

        class SPIRVPhi : public SPIRVOperation
        {
        public:
            SPIRVPhi(uint32_t id, SPIRVMethod& method, uint32_t resultType,
                const std::vector<std::pair<uint32_t, uint32_t>>& sources);
            ~SPIRVPhi() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const std::vector<std::pair<uint32_t, uint32_t>> sources;
        };

        class SPIRVSelect : public SPIRVOperation
        {
        public:
            SPIRVSelect(uint32_t id, SPIRVMethod& method, uint32_t resultType, uint32_t conditionID, uint32_t trueObj,
                uint32_t falseObj);
            ~SPIRVSelect() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const uint32_t condID;
            const uint32_t trueID;
            const uint32_t falseID;
        };

        class SPIRVSwitch : public SPIRVOperation
        {
        public:
            SPIRVSwitch(uint32_t id, SPIRVMethod& method, uint32_t selectorID, uint32_t defaultID,
                const std::vector<std::pair<uint32_t, uint32_t>>& destinations);
            ~SPIRVSwitch() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t selectorID;
            const uint32_t defaultID;
            const std::vector<std::pair<uint32_t, uint32_t>> destinations;
        };

        enum class ImageQuery
        {
            CHANNEL_DATA_TYPE,
            CHANNEL_ORDER,
            SIZES,
            SIZES_LOD,
            MIPMAP_LEVELS,
            SAMPLES_PER_TEXEL
        };

        class SPIRVImageQuery : public SPIRVOperation
        {
        public:
            SPIRVImageQuery(uint32_t id, SPIRVMethod& method, uint32_t resultType, ImageQuery value, uint32_t imageID,
                uint32_t lodOrCoordinate = UNDEFINED_ID);
            ~SPIRVImageQuery() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t typeID;
            const ImageQuery valueID;
            const uint32_t imageID;
            const uint32_t lodOrCoordinate;
        };

        class SPIRVMemoryBarrier : public SPIRVOperation
        {
        public:
            SPIRVMemoryBarrier(SPIRVMethod& method, uint32_t scopeID, uint32_t semanticsID);
            ~SPIRVMemoryBarrier() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t scopeID;
            const uint32_t semanticsID;
        };

        class SPIRVLifetimeInstruction : public SPIRVOperation
        {
        public:
            SPIRVLifetimeInstruction(uint32_t id, SPIRVMethod& method, uint32_t size, bool lifetimeEnd,
                intermediate::InstructionDecorations decorations = static_cast<intermediate::InstructionDecorations>(
                    0));
            ~SPIRVLifetimeInstruction() override = default;

            void mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
                MethodMapping& methods, AllocationMapping& memoryAllocated) const override;
            Optional<Value> precalculate(const TypeMapping& types, const ConstantMapping& constants,
                const AllocationMapping& memoryAllocated) const override;

        private:
            const uint32_t sizeInBytes;
            const bool isLifetimeEnd;
        };
    } // namespace spirv2qasm
} // namespace vc4c

#endif
#endif /* SPIRVOPERATION_H */
