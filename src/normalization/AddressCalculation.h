/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_NORMALIZATION_ADDRESS_CALCULATION_H
#define VC4C_NORMALIZATION_ADDRESS_CALCULATION_H

#include "../InstructionWalker.h"
#include "../Values.h"
#include "../analysis/MemoryAnalysis.h"
#include "../analysis/ValueRange.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../performance.h"
#include "../tools/SmallSet.h"

namespace vc4c
{
    namespace periphery
    {
        enum class VPMUsage : unsigned char;
    } // namespace periphery

    namespace normalization
    {
        using MemoryAccessRange = analysis::MemoryAccessRange;
        using MemoryAccessType = analysis::MemoryAccessType;
        using MemoryAccess = analysis::MemoryAccess;

        MemoryAccessType toMemoryAccessType(periphery::VPMUsage usage);

        /*
         * Converts an address (e.g. an index chain) and the corresponding base pointer to the pointer difference
         *
         * If the given baseAddresses set has more than one entry, the most fitting baseAddress is chosen at run-time.
         * More exactly, the largest candidate base address which is smaller than the given pointer value is selected.
         *
         * NOTE: The result itself is still in "memory-address mode", meaning the offset is the number of bytes
         *
         * Returns (char*)address - (char*)baseAddress
         */
        NODISCARD InstructionWalker insertAddressToOffset(InstructionWalker it, Method& method, Value& out,
            const tools::SmallSortedPointerSet<const Local*>& baseAddresses, const intermediate::MemoryInstruction* mem,
            const Value& ptrValue);

        /*
         * Converts an address (e.g. an index-chain) and a base-address to the offset of the vector denoting the element
         * accessed by the index-chain. In addition to #insertAddressToOffset, this function also handles multiple
         * stack-frames.
         *
         * NOTE: The result is still the offset in bytes, since VPM#insertReadVPM and VPM#insertWriteVPM take the offset
         * in bytes!
         *
         * Returns ((char*)address - (char*)baseAddress) + (typeSizeInBytes * stackIndex), where stackIndex is always
         * zero (and therefore the second part omitted) for shared memory
         */
        NODISCARD InstructionWalker insertAddressToStackOffset(InstructionWalker it, Method& method, Value& out,
            const Local* baseAddress, MemoryAccessType type, const intermediate::MemoryInstruction* mem,
            const Value& ptrValue);

        /*
         * Converts an address (e.g. index-chain) and the corresponding base-address to the element offset for an
         * element of the type used in the container
         *
         * Return ((char*)address - (char*)baseAddress) / sizeof(elementType)
         */
        NODISCARD InstructionWalker insertAddressToElementOffset(InstructionWalker it, Method& method, Value& out,
            const FastMap<const Local*, Value>& baseAddressesAndContainers, Value& outContainer,
            const intermediate::MemoryInstruction* mem, const Value& ptrValue);

        /*
         * Converts an address (e.g. index-chain) which contains work-group uniform and work-item specific parts (as
         * specified in range) to the work-item specific part only.
         * This can be seen as a specialization of #insertAddressToOffset
         *
         * NOTE: The result itself is still in "memory-address mode", meaning the offset is the number of bytes
         *
         * * E.g. get_global_id(0) (= get_group_id(0) + get_local_id(0)) will be converted to get_local_id(0)
         */
        NODISCARD InstructionWalker insertAddressToWorkItemSpecificOffset(
            InstructionWalker it, Method& method, Value& out, MemoryAccessRange& range);

    } /* namespace normalization */
} /* namespace vc4c */
#endif /* VC4C_NORMALIZATION_ADDRESS_CALCULATION_H */
