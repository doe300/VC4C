/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LocalCompression.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../intermediate/Helper.h"
#include "CompilationError.h"
#include "log.h"

#include <string>
#include <vector>

using namespace vc4c;
using namespace vc4c::optimizations;

static const std::vector<std::string> workGroupLocalNames = {Method::LOCAL_IDS, Method::LOCAL_SIZES, Method::GROUP_ID_X,
    Method::GROUP_ID_Y, Method::GROUP_ID_Z, Method::NUM_GROUPS_X, Method::NUM_GROUPS_Y, Method::NUM_GROUPS_Z,
    Method::GLOBAL_OFFSET_X, Method::GLOBAL_OFFSET_Y, Method::GLOBAL_OFFSET_Z, Method::WORK_DIMENSIONS,
    Method::GLOBAL_DATA_ADDRESS, Method::GROUP_LOOP_SIZE};

static InstructionWalker compressLocalWrite(
    Method& method, InstructionWalker it, const Local& local, const Local& container, unsigned char index)
{
    logging::debug() << "Compressing write of local '" << local.name << "' into container '" << container.name
                     << "' at position " << index << " at: " << it->to_string() << logging::endl;

    const Value tmp = method.addNewLocal(local.type);
    it->replaceLocal(&local, tmp.local, LocalUse::Type::WRITER);
    it.nextInBlock();

    return intermediate::insertVectorInsertion(
        it, method, container.createReference(), Value(SmallImmediate(index), TYPE_INT8), tmp);
}

static InstructionWalker compressLocalRead(
    Method& method, InstructionWalker it, const Local& local, const Local& container, unsigned char index)
{
    logging::debug() << "Compressing read of local '" << local.name << "' from container '" << container.name
                     << "' at position " << index << " at: " << it->to_string() << logging::endl;

    const Value tmp = method.addNewLocal(local.type);

    it = intermediate::insertVectorExtraction(
        it, method, container.createReference(), Value(SmallImmediate(index), TYPE_INT8), tmp);

    it->replaceLocal(&local, tmp.local, LocalUse::Type::READER);
    return it;
}

static void compressLocalIntoRegister(Method& method, const Local& local, const Local& container, unsigned char index)
{
    if(index > 15)
        throw CompilationError(CompilationStep::OPTIMIZER, "Container index out of bounds", std::to_string(index));
    if(local.type.num != 1)
        throw CompilationError(CompilationStep::OPTIMIZER, "Can't compress local of vector-type: ", local.to_string());

    // TODO most efficient way of finding iterator for instruction?
    for(BasicBlock& bb : method)
    {
        auto it = bb.begin();
        while(!it.isEndOfBlock())
        {
            if(it.get() && it->writesLocal(&local))
                // replace all writes with write to container at given position
                it = compressLocalWrite(method, it, local, container, index);
            else if(it.get() && it->readsLocal(&local))
                // replace all reads with reads of container at given position
                it = compressLocalRead(method, it, local, container, index);
            else
                it.nextInBlock();
        }
    }
}

void optimizations::compressWorkGroupLocals(const Module& module, Method& method, const Configuration& config)
{
    unsigned char index = 0;
    const Value container = method.addNewLocal(TYPE_INT32.toVectorType(16), "%work_group_info");
    method.begin()->begin().nextInBlock().emplace(new intermediate::MoveOperation(container, INT_ZERO));
    for(const std::string& name : workGroupLocalNames)
    {
        const Local* local = method.findLocal(name);
        if(local != nullptr)
        {
            compressLocalIntoRegister(method, *local, *container.local, index);
            ++index;
        }
    }
}