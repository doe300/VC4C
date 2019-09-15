/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_MEMORY_ANALYSIS
#define VC4C_MEMORY_ANALYSIS 1

#include "../InstructionWalker.h"
#include "../Values.h"
#include "../performance.h"
#include "ValueRange.h"

namespace vc4c
{
    namespace analysis
    {
        /**
         * Analysis data for the range of memory accessed per memory object
         *
         * The final address is calculated as following:
         *
         * A = groupUniformAddressParts + dynamicAddressParts
         * B = typeSizeShift ? A << typeSizeShift : A
         * C = baseAddressAdd + B
         * D = constantOffset ? C + constantOffset : C
         * addressWrite <- D
         */
        struct MemoryAccessRange
        {
            // the underlying memory object which is accessed
            const Local* memoryObject = nullptr;
            // the instruction writing the address to VPR_ADDR or VPW_ADDR
            InstructionWalker addressWrite{};
            // the optional constant offset added after the base address was calculated, e.g. for element index
            Optional<Value> constantOffset;
            // the instruction adding the offset to the base pointer, could be the same as addressWrite
            InstructionWalker baseAddressAdd{};
            // the instruction converting the address offset from element offset to byte offset
            Optional<InstructionWalker> typeSizeShift{};
            // the work-group uniform parts of which the address offset is calculated from
            FastMap<Value, intermediate::InstructionDecorations> groupUniformAddressParts{};
            // the dynamic parts of which the address offset is calculated from
            FastMap<Value, intermediate::InstructionDecorations> dynamicAddressParts{};
            // the maximum range (in elements!) the memory is accessed in
            analysis::IntegerRange offsetRange{0, 0};

            std::string to_string() const;
        };

        struct LocalUsageOrdering
        {
            bool operator()(const Local* l1, const Local* l2) const;
        };

        using AccessRanges = SortedMap<const Local*, FastAccessList<analysis::MemoryAccessRange>, LocalUsageOrdering>;

        /**
         * Determines all the limited access ranges of the single memory-bases locals.
         *
         * For any local not present in the resulting map, the access range could not be determined
         */
        AccessRanges determineAccessRanges(Method& method);

    } // namespace analysis

} // namespace vc4c

#endif /* VC4C_MEMORY_ANALYSIS */
