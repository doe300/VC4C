/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_MEMORY_ANALYSIS
#define VC4C_MEMORY_ANALYSIS 1

#include "../Expression.h"
#include "../InstructionWalker.h"
#include "../Values.h"
#include "../performance.h"
#include "ValueRange.h"

#include <memory>

namespace vc4c
{
    namespace analysis
    {
        /**
         * Analysis data for the range of memory accessed per memory object
         *
         * The final address is calculated as following:
         *
         * A = groupUniformOffset + dynamicOffset
         * B = baseAddress + A
         * addressWrite <- B
         */
        struct MemoryAccessRange
        {
            // the underlying memory object (or a conditionally written address variable) which is accessed
            const Local* baseAddress = nullptr;
            // the instruction writing the address to VPR_ADDR or VPW_ADDR or the intermediate::MemoryInstruction
            TypedInstructionWalker<intermediate::MemoryInstruction> addressWrite{};
            // the work-group uniform part of which the address offset is calculated from
            SubExpression groupUniformOffset;
            // the dynamic part (specific to the work-item) of which the address offset is calculated from
            SubExpression dynamicOffset;
            // the maximum range (in bytes!) the memory is actually accessed in (at a given offset)
            // NOTE: This is the range of widths, e.g. if this value is 32, then the associated access is always 32
            // bytes wide, if this value is [1, 32], then the associated access can access between 1 and 32 bytes!
            Optional<analysis::ValueRange> accessRange;
            // the element type of the access, this is used to calculate the access-range in elements
            // NOTE: Elements do not have to be scalar, but can be of any type!
            DataType accessElementType = TYPE_UNKNOWN;

            // This is the range of the dynamic offset and access width (in bytes!) and thus the whole range (excluding
            // any work-group uniform offset) of memory that is actually accessed.
            Optional<ValueRange> getDynamicAccessByteRange() const;
            Optional<ValueRange> getDynamicAccessElementRange() const;

            uint32_t getAccessAlignment(uint32_t assumedBaseAlignment = 0) const;

            // checks whether it is guaranteed that multiple "instances" of this memory access (e.g. by different
            // work-items) do not access the same bits.
            // NOTE: An identical "instance" of this memory access (e.g. using the same address-calculating values)
            // obviously accesses the same bits!
            bool hasNoOverlapBetweenAccesses() const;
            bool mayOverlap(const MemoryAccessRange& other) const;

            std::string to_string() const;
        };

        struct LocalUsageOrdering
        {
            bool operator()(const Local* l1, const Local* l2) const;
        };

        using AccessRanges = SortedMap<const Local*, FastAccessList<analysis::MemoryAccessRange>, LocalUsageOrdering>;

        /**
         * Determines all the limited access ranges of the given local.
         *
         * if the access range could not be determined for all memory access operations, an empty list is returned.
         *
         * NOTE: This function is intended to be run BEFORE the MemoryInstructions have been lowered!
         */
        FastAccessList<MemoryAccessRange> determineAccessRanges(Method& method, const Local* baseAddr,
            FastMap<TypedInstructionWalker<intermediate::MemoryInstruction>, const Local*>& accessInstructions);

        struct IdenticalWorkGroupUniformPartsResult
        {
            ValueRange dynamicElementRange;
            DataType elementType;
        };

        /**
         * Checks whether all work-group uniform parts of the memory accesses are equal.
         *
         * If not, tries to convert the work-group uniform address offset parts to dynamic offsets and continues
         * determining the range of dynamic offsets.
         *
         * Returns the non-uniform element access range and element types if all work-group uniform parts of the address
         * calculation are equal. E.g. if the memory location is accessed only in <group uniform parts> + [access range]
         * elements.
         */
        Optional<IdenticalWorkGroupUniformPartsResult> checkWorkGroupUniformParts(
            FastAccessList<MemoryAccessRange>& accessRanges);

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
