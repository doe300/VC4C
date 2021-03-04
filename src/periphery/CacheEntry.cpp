/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CacheEntry.h"

#include "../intermediate/IntermediateInstruction.h"

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::intermediate;

CacheEntry::~CacheEntry() noexcept = default;

tools::SmallSortedPointerSet<RAMAccessInstruction*> CacheEntry::getMemoryAccesses()
{
    tools::SmallSortedPointerSet<RAMAccessInstruction*> result;
    for(auto access : accesses)
    {
        if(auto acc = dynamic_cast<RAMAccessInstruction*>(access))
            result.emplace(acc);
    }
    return result;
}

tools::SmallSortedPointerSet<const RAMAccessInstruction*> CacheEntry::getMemoryAccesses() const
{
    tools::SmallSortedPointerSet<const RAMAccessInstruction*> result;
    for(auto access : accesses)
    {
        if(auto acc = dynamic_cast<const RAMAccessInstruction*>(access))
            result.emplace(acc);
    }
    return result;
}

tools::SmallSortedPointerSet<CacheAccessInstruction*> CacheEntry::getQPUAccesses()
{
    tools::SmallSortedPointerSet<CacheAccessInstruction*> result;
    for(auto access : accesses)
    {
        if(auto acc = dynamic_cast<CacheAccessInstruction*>(access))
            result.emplace(acc);
    }
    return result;
}

tools::SmallSortedPointerSet<const CacheAccessInstruction*> CacheEntry::getQPUAccesses() const
{
    tools::SmallSortedPointerSet<const CacheAccessInstruction*> result;
    for(auto access : accesses)
    {
        if(auto acc = dynamic_cast<const CacheAccessInstruction*>(access))
            result.emplace(acc);
    }
    return result;
}
