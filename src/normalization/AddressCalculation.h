/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_NORMALIZATION_ADDRESS_CALCULATION_H
#define VC4C_NORMALIZATION_ADDRESS_CALCULATION_H

#include "../InstructionWalker.h"
#include "../Values.h"
#include "../analysis/ValueRange.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../periphery/VPM.h"

namespace vc4c
{
    namespace normalization
    {
        enum class MemoryType
        {
            // lower the value into a register and replace all loads with moves
            QPU_REGISTER_READONLY,
            // lower the value into a register and replace all loads/stores with moves
            QPU_REGISTER_READWRITE,
            // store in VPM in extra space per QPU!!
            VPM_PER_QPU,
            // store in VPM, QPUs share access to common data
            VPM_SHARED_ACCESS,
            // keep in RAM/global data segment, read via TMU
            RAM_LOAD_TMU,
            // keep in RAM/global data segment, access via VPM
            RAM_READ_WRITE_VPM
        };

        struct MemoryAccess
        {
            FastSet<InstructionWalker> accessInstructions;
            MemoryType preferred;
            MemoryType fallback;
        };

        MemoryType toMemoryType(periphery::VPMUsage usage);

        /*
         * Converts an address (e.g. an index chain) and the corresponding base pointer to the pointer difference
         *
         * NOTE: The result itself is still in "memory-address mode", meaning the offset is the number of bytes
         *
         * Returns (char*)address - (char*)baseAddress
         */
        InstructionWalker insertAddressToOffset(InstructionWalker it, Method& method, Value& out,
            const Local* baseAddress, const intermediate::MemoryInstruction* mem);

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
        InstructionWalker insertAddressToStackOffset(InstructionWalker it, Method& method, Value& out,
            const Local* baseAddress, MemoryType type, const intermediate::MemoryInstruction* mem);

        /*
         * Converts an address (e.g. index-chain) and the corresponding base-address to the element offset for an
         * element of the type used in the container
         *
         * Return ((char*)address - (char*)baseAddress) / sizeof(elementType)
         */
        InstructionWalker insertAddressToElementOffset(InstructionWalker it, Method& method, Value& out,
            const Local* baseAddress, const Value& container, const intermediate::MemoryInstruction* mem);

        // represents analysis data for the range of memory accessed per memory object
        struct MemoryAccessRange
        {
            const Local* memoryObject;
            // the memory instruction accessing the memory object
            InstructionWalker memoryInstruction;
            // the instruction adding the offset to the base pointer, could be the same as addressWrite
            InstructionWalker baseAddressAdd;
            // the instruction converting the address offset from element offset to byte offset
            Optional<InstructionWalker> typeSizeShift;
            // the work-group uniform parts of which the address offset is calculated from
            FastMap<Value, intermediate::InstructionDecorations> groupUniformAddressParts;
            // the dynamic parts of which the address offset is calculated from
            FastMap<Value, intermediate::InstructionDecorations> dynamicAddressParts;
            // the maximum range (in elements!) the memory is accessed in
            analysis::IntegerRange offsetRange{0, 0};

            std::string to_string() const;
        };

        /*
         * Converts an address (e.g. index-chain) which contains work-group uniform and work-item specific parts (as
         * specified in range) to the work-item specific part only.
         * This can be seen as a specialization of #insertAddressToOffset
         *
         * NOTE: The result itself is still in "memory-address mode", meaning the offset is the number of bytes
         *
         * * E.g. get_global_id(0) (= get_group_id(0) + get_local_id(0)) will be converted to get_local_id(0)
         */
        InstructionWalker insertAddressToWorkItemSpecificOffset(
            InstructionWalker it, Method& method, Value& out, MemoryAccessRange& range);

        /*
         * Converts an address (e.g. index-chain) which contains work-group uniform and work-item specific parts (as
         * specified in range) to the work-group uniform part only.
         * Adding the result of this function and #insertAddressToWorkItemSpecificOffset will result in the original
         * address
         *
         * NOTE: The result itself is in "memory-address mode", meaning the offset is the number of bytes, since it is
         * meant for addressing memory
         *
         * * E.g. get_global_id(0) (= get_group_id(0) + get_local_id(0)) will be converted to get_group_id(0)
         */
        InstructionWalker insertAddressToWorkGroupUniformOffset(
            InstructionWalker it, Method& method, Value& out, MemoryAccessRange& range);

        struct LocalUsageOrdering
        {
            bool operator()(const Local* l1, const Local* l2) const;
        };
    } /* namespace normalization */
} /* namespace vc4c */
#endif /* VC4C_NORMALIZATION_ADDRESS_CALCULATION_H */