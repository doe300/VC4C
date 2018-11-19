/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_KERNEL_METADATA_H
#define VC4C_KERNEL_METADATA_H

#include "Bitfield.h"
#include "config.h"

#include <algorithm>
#include <array>
#include <bitset>
#include <numeric>

namespace vc4c
{
    /*
     * Contains information about the implicit UNIFORMs (work-group info, etc.) actually used in the kernel
     */
    struct KernelUniforms : public Bitfield<uint64_t>
    {
        BITFIELD_ENTRY(WorkDimensionsUsed, bool, 0, Bit)
        BITFIELD_ENTRY(LocalSizesUsed, bool, 1, Bit)
        BITFIELD_ENTRY(LocalIDsUsed, bool, 2, Bit)
        BITFIELD_ENTRY(NumGroupsXUsed, bool, 3, Bit)
        BITFIELD_ENTRY(NumGroupsYUsed, bool, 4, Bit)
        BITFIELD_ENTRY(NumGroupsZUsed, bool, 5, Bit)
        BITFIELD_ENTRY(GroupIDXUsed, bool, 6, Bit)
        BITFIELD_ENTRY(GroupIDYUsed, bool, 7, Bit)
        BITFIELD_ENTRY(GroupIDZUsed, bool, 8, Bit)
        BITFIELD_ENTRY(GlobalOffsetXUsed, bool, 9, Bit)
        BITFIELD_ENTRY(GlobalOffsetYUsed, bool, 10, Bit)
        BITFIELD_ENTRY(GlobalOffsetZUsed, bool, 11, Bit)
        BITFIELD_ENTRY(GlobalDataAddressUsed, bool, 12, Bit)

        inline size_t countUniforms() const
        {
            std::bitset<64> tmp(value);
            return tmp.count();
        }
    };

    /*
     * Container for additional meta-data of kernel-functions
     */
    struct KernelMetaData
    {
        /*
         * The implicit UNIFORMs actually used
         */
        KernelUniforms uniformsUsed;
        /*
         * The compilation-time work-group size, specified by the reqd_work_group_size attribute
         */
        std::array<uint32_t, 3> workGroupSizes;
        /*
         * The compilation-time preferred work-group size, specified by the work_group_size_hint attribute
         */
        std::array<uint32_t, 3> workGroupSizeHints;

        KernelMetaData() : uniformsUsed(), workGroupSizes(), workGroupSizeHints()
        {
            workGroupSizes.fill(0);
            workGroupSizeHints.fill(0);
        }

        /*
         * Retuns whether the explicit work-group size is set
         */
        inline bool isWorkGroupSizeSet() const
        {
            return std::any_of(workGroupSizes.begin(), workGroupSizes.end(), [](uint32_t u) -> bool { return u > 0; });
        }

        uint32_t getWorkGroupSize()
        {
            if(isWorkGroupSizeSet())
                return std::accumulate(workGroupSizes.begin(), workGroupSizes.end(), 1u, std::multiplies<uint32_t>{});
            // we don't know - assume "worst"
            return NUM_QPUS;
        }
    };
} // namespace vc4c

#endif /* VC4C_KERNEL_METADATA_H */
