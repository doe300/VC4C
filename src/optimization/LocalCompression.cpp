/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LocalCompression.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "CompilationError.h"
#include "log.h"

#include <string>
#include <vector>

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::operators;

static const std::vector<BuiltinLocal::Type> workGroupLocalNames = {BuiltinLocal::Type::LOCAL_IDS,
    BuiltinLocal::Type::LOCAL_SIZES, BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y,
    BuiltinLocal::Type::GROUP_ID_Z, BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y,
    BuiltinLocal::Type::NUM_GROUPS_Z, BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
    BuiltinLocal::Type::GLOBAL_OFFSET_Z, BuiltinLocal::Type::WORK_DIMENSIONS, BuiltinLocal::Type::GLOBAL_DATA_ADDRESS};

static NODISCARD InstructionWalker compressLocalWrite(
    Method& method, InstructionWalker it, const BuiltinLocal& local, const Local& container, unsigned char index)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Compressing write of local '" << local.name << "' into container '" << container.name
            << "' at position " << index << " at: " << it->to_string() << logging::endl);

    if(auto move = it.get<intermediate::MoveOperation>())
    {
        // directly use the source of the assignment and insert it into vector
        const Value& src = move->getSource();
        it = intermediate::insertVectorInsertion(
            it, method, container.createReference(), Value(SmallImmediate(index), TYPE_INT8), src);
        it.erase();
        return it;
    }
    if(auto op = it.get<intermediate::Operation>())
    {
        if(op->writesLocal(&local) && op->readsLocal(&local) && op->readsLiteral() && !op->hasSideEffects() &&
            !op->hasConditionalExecution())
        {
            // replace local with index in container and make calculation only applicable for this index,  e.g. for
            // work-group loop group-id increment
            auto cond = assignNop(it) = selectSIMDElement(index);
            op->replaceLocal(&local, &container, LocalUse::Type::BOTH);
            op->setCondition(cond);
            it.nextInBlock();
            return it;
        }
    }

    const Value tmp = method.addNewLocal(local.type);
    it->replaceLocal(&local, tmp.local(), LocalUse::Type::WRITER);
    it.nextInBlock();

    return intermediate::insertVectorInsertion(
        it, method, container.createReference(), Value(SmallImmediate(index), TYPE_INT8), tmp);
}

static NODISCARD InstructionWalker compressLocalRead(
    Method& method, InstructionWalker it, const BuiltinLocal& local, const Local& container, unsigned char index)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Compressing read of local '" << local.name << "' from container '" << container.name << "' at position "
            << index << " at: " << it->to_string() << logging::endl);

    const Value tmp = method.addNewLocal(local.type);

    it = intermediate::insertVectorExtraction(
        it, method, container.createReference(), Value(SmallImmediate(index), TYPE_INT8), tmp);

    it->replaceLocal(&local, tmp.local(), LocalUse::Type::READER);
    return it;
}

static void compressLocalIntoRegister(
    Method& method, const BuiltinLocal& local, const Local& container, unsigned char index)
{
    if(index > 15)
        throw CompilationError(CompilationStep::OPTIMIZER, "Container index out of bounds", std::to_string(index));
    if(local.type.getVectorWidth() != 1)
        throw CompilationError(CompilationStep::OPTIMIZER, "Can't compress local of vector-type: ", local.to_string());

    // TODO most efficient way of finding iterator for instruction?
    for(BasicBlock& bb : method)
    {
        auto it = bb.walk();
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

bool optimizations::compressWorkGroupLocals(const Module& module, Method& method, const Configuration& config)
{
    if(method.size() == 0 || method.begin()->empty())
        return false;
    unsigned char index = 0;
    const Value container = method.addNewLocal(TYPE_INT32.toVectorType(16), "%work_group_info");
    method.begin()->walk().nextInBlock().emplace(new intermediate::MoveOperation(container, INT_ZERO));
    for(auto type : workGroupLocalNames)
    {
        if(auto local = method.findBuiltin(type))
        {
            compressLocalIntoRegister(method, *local, *container.local(), index);
            ++index;
        }
    }

    return false;
}
