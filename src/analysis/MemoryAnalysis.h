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

#include <memory>

namespace vc4c
{
    struct Expression;

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
            // the instruction writing the address to VPR_ADDR or VPW_ADDR or the intermediate::MemoryInstruction
            InstructionWalker addressWrite{};
            // the optional constant offset added after the base address was calculated, e.g. for element index
            Optional<Value> constantOffset;
            // the instruction adding the offset to the base pointer, could be the same as addressWrite
            const intermediate::Operation* baseAddressAdd = nullptr;
            // the instruction converting the address offset from element offset to byte offset
            const intermediate::Operation* typeSizeShift = nullptr;
            // the work-group uniform parts of which the address offset is calculated from
            FastMap<Value, intermediate::InstructionDecorations> groupUniformAddressParts{};
            // the dynamic parts (specific to the work-item) of which the address offset is calculated from
            FastMap<Value, intermediate::InstructionDecorations> dynamicAddressParts{};
            // the maximum range (in elements!) the memory is accessed in
            Optional<analysis::ValueRange> offsetRange;
            /*
             * The optional expression calculating the memory address.
             *
             * If typeSizeShift is set, this is the input to that instruction (the address offset calculation in
             * elements), otherwise if baseAddressAdd is set, this is the input to that instruction (the address offset
             * calculation in bytes), otherwise this is the input to the addressWrite instruction (the full address
             * calculation in bytes).
             */
            std::shared_ptr<Expression> addressExpression;

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
         *
         * NOTE: This function is intended to be run AFTER the MemoryInstructions have been lowered!
         */
        AccessRanges determineAccessRanges(Method& method);

        /**
         * Determines all the limited access ranges of the given local.
         *
         * if the access range could not be determined for all memory access operations, an empty list is returned.
         *
         * NOTE: This function is intended to be run BEFORE the MemoryInstructions have been lowered!
         */
        FastAccessList<MemoryAccessRange> determineAccessRanges(
            Method& method, const Local* baseAddr, FastMap<InstructionWalker, const Local*>& accessInstructions);

        /**
         * Checks whether all work-group uniform parts of the memory accesses are equal.
         *
         * If not, tries to convert the work-group uniform address offset parts to dynamic offsets and continues
         * determining the range of dynamic offsets.
         *
         * Returns whether all work-group uniform parts of the address calculation are equal and the non-uniform access
         * range. E.g. the memory location is accessed only in <group uniform parts> + [access range].
         */
        std::pair<bool, analysis::ValueRange> checkWorkGroupUniformParts(
            FastAccessList<MemoryAccessRange>& accessRanges, bool allowConstantOffsets = true);

        /**
         * Returns the single writer of a value (usually an address local).
         *
         * In contrast to Value#getSingleWriter(), this function skips all MemoryInstructions which write into the
         * memory stored at the given address and therefore only heeds the actual writes of the address value.
         *
         * @return the single writer or the defaultInst, if not exactly 1 matching writer was found
         */
        const intermediate::IntermediateInstruction* getSingleWriter(
            const Value& val, const intermediate::IntermediateInstruction* defaultInst = nullptr);

    } // namespace analysis

} // namespace vc4c

#endif /* VC4C_MEMORY_ANALYSIS */
