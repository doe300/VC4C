/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_NORMALIZATION_MEMORY_MAPPING_H
#define VC4C_NORMALIZATION_MEMORY_MAPPING_H

#include "../tools/SmallSet.h"
#include "AddressCalculation.h"

namespace vc4c
{
    namespace periphery
    {
        struct VPMArea;
    } // namespace periphery

    namespace normalization
    {
        /*
         * Container for all the information required for a memory area to be mapped to any of the possible storage
         * locations
         */
        struct MemoryInfo
        {
            const Local* local;
            // the type of how to lower the memory represented by the local
            MemoryAccessType type;
            // the optional VPM area. If this is set, the memory is lowered to VPM
            const periphery::VPMArea* area = nullptr;
            // the optional access ranges. If set, the memory is located in RAM but cached in VPM
            const Optional<std::vector<MemoryAccessRange>> ranges = {};
            // the constant value or mapped register this local represents.
            Optional<Value> mappedRegisterOrConstant = NO_VALUE;
            // e.g. for arrays converted to vectors, this is the resulting vector type
            Optional<DataType> convertedRegisterType = {};
            // flags which TMU to be used for reading
            bool tmuFlag = false;

            std::string to_string() const;
        };

        struct MemoryAccess
        {
            /**
             * The map of the instructions accessing the memory location and the local the memory location is accessed
             * as.
             *
             * In most cases, the local will be the actual memory local (e.g. Parameter, Global, StackAllocation).
             * However, if the corresponding memory access is conditional (e.g. the address is assigned via phi-node or
             * selection), then the value in this mapping will represent the conditionally assigned address local.
             */
            FastMap<InstructionWalker, const Local*> accessInstructions;
            MemoryAccessType preferred;
            MemoryAccessType fallback;
            /**
             * Whether the associated memory area can be cached in VPM at all. This flag is only valid for the
             * RAM_READ_WRITE_VPM access type.
             */
            bool canBeCachedInVPM = true;
        };

        using GroupedAccessRanges =
            FastMap<const Local*, std::pair<FastAccessList<MemoryAccessRange>, const periphery::VPMArea*>>;
        using MemoryAccessMap = SortedMap<const Local*, MemoryAccess, analysis::LocalUsageOrdering>;

        /**
         * Container for information returned from the memory access check
         */
        struct MemoryAccessInfo
        {
            // The mapping from accessed memory areas to the types of accesses
            MemoryAccessMap memoryAccesses;
            // The instructions accessing memory
            FastSet<InstructionWalker> accessInstructions;
            // Additional mapping from the locals accessed as memory areas to the actual memory areas accessed. This is
            // relevant e.g. if the memory area accessed is determined via a phi-node or a select-statement (?:).
            FastMap<const Local*, FastSet<const Local*>> additionalAreaMappings;
        };

        /*
         * Basic algorithm to determine the preferred and fall-back (e.g. if access-types not supported by preferred)
         * way of
         * a) mapping the memory regions used by this method to the available "memory" (registers, VPM, RAM) and
         * b) mapping the memory access types (read, write, copy, fill) to the available memory access types (TMU, VPM,
         * etc.)
         */
        MemoryAccessInfo determineMemoryAccess(Method& method);

        /*
         * Returns the constant value which will be read from the given memory access instruction.
         *
         * The value is constant if:
         * - the source memory location is constant
         * - the index is constant or the value can be determined without knowing the exact index (e.g. all elements are
         * the same)
         *
         * NOTE: This function returns the whole container value!
         */
        Optional<Value> getConstantContainerValue(const Value& source);

        /*
         * Returns the constant value which will be read from the given memory access instruction.
         *
         * The value is constant if:
         * - the source memory location is constant
         * - the index is constant or the value can be determined without knowing the exact index (e.g. all elements are
         * the same)
         *
         * NOTE: This function only returns single element values!
         */
        Optional<Value> getConstantElementValue(const Value& source);

        /*
         * Checks whether the memory location can be mapped to the preferred location specified in the MemoryAccess
         * parameter. If so, required resources will be reserved. If not, the check will be performed with the fall-back
         * storage location.
         */
        MemoryInfo checkMemoryMapping(Method& method, const Local* baseAddr, MemoryAccess& access);

        /*
         * Maps the given memory access instruction to hardware instructions according to the given source and
         * destination information.
         */
        InstructionWalker mapMemoryAccess(Method& method, InstructionWalker it, intermediate::MemoryInstruction* mem,
            const tools::SmallSortedPointerSet<const MemoryInfo*>& srcInfos,
            const tools::SmallSortedPointerSet<const MemoryInfo*>& destInfos);

        struct CacheMemoryData
        {
            const MemoryInfo* info;
            bool insertPreload;
            bool insertWriteBack;
        };

        void insertCacheSynchronizationCode(Method& method, const FastMap<const Local*, CacheMemoryData>& cachedLocals);
    } // namespace normalization
} // namespace vc4c

#endif /* VC4C_NORMALIZATION_MEMORY_MAPPING_H */
