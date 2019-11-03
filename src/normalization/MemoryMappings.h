/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_NORMALIZATION_MEMORY_MAPPING_H
#define VC4C_NORMALIZATION_MEMORY_MAPPING_H

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

        using GroupedAccessRanges =
            FastMap<const Local*, std::pair<FastAccessList<MemoryAccessRange>, const periphery::VPMArea*>>;
        using MemoryAccessMap = SortedMap<const Local*, MemoryAccess, analysis::LocalUsageOrdering>;

        /*
         * Basic algorithm to determine the preferred and fall-back (e.g. if access-types not supported by preferred)
         * way of
         * a) mapping the memory regions used by this method to the available "memory" (registers, VPM, RAM) and
         * b) mapping the memory access types (read, write, copy, fill) to the available memory access types (TMU, VPM,
         * etc.)
         */
        std::pair<MemoryAccessMap, FastSet<InstructionWalker>> determineMemoryAccess(Method& method);

        /*
         * Returns the constant value which will be read from the given memory access instruction.
         *
         * The value is constant if:
         * - the source memory location is constant
         * - the index is constant or the value can be determined without knowing the exact index (e.g. all elements are
         * the same)
         */
        Optional<Value> getConstantValue(const Value& source);

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
            const MemoryInfo& srcInfo, const MemoryInfo& destInfo);
    } // namespace normalization
} // namespace vc4c

#endif /* VC4C_NORMALIZATION_MEMORY_MAPPING_H */
