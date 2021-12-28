/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_CACHE_ENTRY_H
#define VC4C_CACHE_ENTRY_H

#include "../Optional.h"
#include "../Types.h"
#include "../tools/SmallSet.h"

#include <string>

namespace vc4c
{
    namespace intermediate
    {
        struct MemoryAccessInstruction;
        struct RAMAccessInstruction;
        struct CacheAccessInstruction;
    } // namespace intermediate

    namespace periphery
    {
        /**
         * Represents an entry within a cache.
         *
         * For e.g. VPM, this represents the (dynamic/static) address in rows/columns/sub-words within the VPM as well
         * as the dynamic/static number of rows/columns covered by this entry. For TMU, this is an abstract concept,
         * since the actual location within the TMU cache cannot be controlled and is of no consequence (except for
         * cache collisions).
         */
        struct CacheEntry
        {
            virtual ~CacheEntry() noexcept;
            virtual std::string to_string() const = 0;
            virtual bool isReadOnly() const = 0;

            tools::SmallSortedPointerSet<intermediate::RAMAccessInstruction*> getMemoryAccesses();
            tools::SmallSortedPointerSet<const intermediate::RAMAccessInstruction*> getMemoryAccesses() const;
            tools::SmallSortedPointerSet<intermediate::CacheAccessInstruction*> getQPUAccesses();
            tools::SmallSortedPointerSet<const intermediate::CacheAccessInstruction*> getQPUAccesses() const;

            tools::SmallSortedPointerSet<intermediate::MemoryAccessInstruction*> accesses;
        };

    } // namespace periphery
} // namespace vc4c

#endif /* VC4C_CACHE_ENTRY_H */
