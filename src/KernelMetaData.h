/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_KERNEL_METADATA_H
#define VC4C_KERNEL_METADATA_H

#include "config.h"
#include "shared/BinaryHeader.h"

#include <algorithm>
#include <array>
#include <bitset>
#include <numeric>

namespace vc4c
{
    /**
     * Container for additional meta-data of kernel-functions
     */
    struct KernelMetaData
    {
        /**
         * The implicit UNIFORMs actually used
         */
        KernelUniforms uniformsUsed;
        /**
         * The compilation-time work-group size, specified by the reqd_work_group_size attribute
         */
        std::array<uint32_t, 3> workGroupSizes;
        /**
         * The compilation-time preferred work-group size, specified by the work_group_size_hint attribute
         */
        std::array<uint32_t, 3> workGroupSizeHints;
        /**
         * The factor with which the work-items are merged, e.g. 16 if 16 work-items are merged into one QPU execution.
         */
        uint8_t mergedWorkItemsFactor;

        KernelMetaData() : uniformsUsed(), workGroupSizes(), workGroupSizeHints(), mergedWorkItemsFactor(0)
        {
            workGroupSizes.fill(0);
            workGroupSizeHints.fill(0);
        }

        /**
         * Returns the explicit work-group size, if it is set
         */
        inline Optional<uint32_t> getFixedWorkGroupSize() const
        {
            if(std::any_of(workGroupSizes.begin(), workGroupSizes.end(), [](uint32_t u) -> bool { return u > 0; }))
                return std::accumulate(workGroupSizes.begin(), workGroupSizes.end(), 1u, std::multiplies<uint32_t>{});
            return {};
        }

        /**
         * Returns the maximum number of work-items in a work-group for this kernel
         */
        inline uint32_t getMaximumWorkGroupSize() const
        {
            if(auto fixedSize = getFixedWorkGroupSize())
                return *fixedSize;
            return NUM_QPUS * std::max(mergedWorkItemsFactor, uint8_t{1});
        }

        /**
         * Returns the maximum number of kernel instances to be executed (the maximum number of QPUs required) for a
         * single work-group
         */
        inline uint32_t getMaximumInstancesCount() const
        {
            auto factor = std::max(mergedWorkItemsFactor, uint8_t{1});
            if(auto fixedSize = getFixedWorkGroupSize())
                // round up if the fixed number of work-items do not match exactly
                return (*fixedSize / factor) + (*fixedSize % factor != 0);
            return NUM_QPUS;
        }
    };
} // namespace vc4c

#endif /* VC4C_KERNEL_METADATA_H */
