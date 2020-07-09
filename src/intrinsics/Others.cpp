/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Others.h"

#include "../Module.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "log.h"

#include <bitset>
#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

static void insertSingleSecondaryBlockCode(Method& method, BasicBlock& block, uint32_t index,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel)
{
    // decrement "own" semaphore to block until previous work-item released it
    auto it = block.walkEnd();
    it.emplace(new MethodCall("vc4cl_semaphore_decrement", {Value(Literal(index), TYPE_INT8)}));
    it.nextInBlock();

    // if applicable, increment the semaphore of the next work-item to release it
    if(index + 1 < maxGroupSize)
    {
        // just copy to ease register association
        auto tmp = assign(it, localSizeScalar.type, "%local_size_scalar") = localSizeScalar;
        auto cond = assignNop(it) = as_signed{tmp} > as_signed{Value(Literal(index + 1), TYPE_INT8)};
        tmp = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, tmp) = (BOOL_FALSE, cond.invert());
        auto branchIt = it.copy().previousInBlock();

        // we need to create the other block first to be able to correctly update the CFG
        auto block = method.addNewLocal(TYPE_LABEL, "%barrier_next_release").local();
        auto blockIt = method.emplaceLabel(it, new BranchLabel(*block));
        {
            blockIt.nextInBlock();
            blockIt.emplace(new MethodCall("vc4cl_semaphore_increment", {Value(Literal(index + 1), TYPE_INT8)}));
            blockIt.nextInBlock();
            blockIt.emplace(new Branch(afterLabel));
        }

        branchIt.nextInBlock();
        BranchCond branchCond = BRANCH_ALWAYS;
        std::tie(branchIt, branchCond) = insertBranchCondition(method, branchIt, tmp);
        branchIt.emplace(new Branch(block, branchCond));
        branchIt.nextInBlock();
        branchIt.emplace(new Branch(afterLabel, branchCond.invert()));
    }
    else
        it.emplace(new Branch(afterLabel));
}

static void insertNonPrimaryBarrierCode(Method& method, BasicBlock& block, const Value& localIdScalar,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel)
{
    // all but the first work-item increment semaphore 0, so the first work-item can continue, wait on their own
    // semaphore and then increment the next one (if any)
    auto it = block.walkEnd();
    it.emplace(new MethodCall("vc4cl_semaphore_increment", {0_val}));
    it.nextInBlock();
    // just copy to ease register association
    auto localId = assign(it, localIdScalar.type, "%local_id_scalar") = localIdScalar;
    auto switchIt = it;

    // we need to create the other blocks first to be able to correctly update the CFG
    SortedMap<unsigned, const Local*> singleBlocks;
    for(unsigned i = 1; i < maxGroupSize; ++i)
    {
        auto label = method.addNewLocal(TYPE_LABEL, "%barrier_single_block").local();
        it = method.emplaceLabel(it, new BranchLabel(*label));
        singleBlocks.emplace(i, label);
        insertSingleSecondaryBlockCode(method, *it.getBasicBlock(), i, localSizeScalar, maxGroupSize, afterLabel);
    }

    // insert switch-case for all possible local IDs
    BranchCond cond = BRANCH_ALWAYS;
    for(unsigned i = 1; i < maxGroupSize; ++i)
    {
        auto tmp = assign(switchIt, TYPE_INT8) = localId ^ Value(Literal(i), TYPE_INT8);
        std::tie(switchIt, cond) = insertBranchCondition(method, switchIt, tmp);
        switchIt.emplace(new Branch(singleBlocks.at(i), cond.invert()));
        switchIt.nextInBlock();
    }
    switchIt.emplace(new Branch(afterLabel));
}

static void insertPrimaryBarrierCode(Method& method, BasicBlock& block, const Value& localIdScalar,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel)
{
    // the first work-item waits on its semaphore for all other to have increased it (size -1 times) and then wakes up
    // the second work-item
    auto it = block.walkEnd();
    // just copy to ease register association
    auto locaSize = assign(it, localSizeScalar.type, "%local_id_scalar") = localSizeScalar;
    auto numRepetitions = assign(it, TYPE_INT8) = (locaSize - 1_val, InstructionDecorations::PHI_NODE);
    auto& loopBlock = insertLoop(method, it, numRepetitions, "%barrier_primary_loop");
    {
        // inside loop
        auto loopIt = loopBlock.walk().nextInBlock();
        loopIt.emplace(new MethodCall("vc4cl_semaphore_decrement", {0_val}));
        loopIt.nextInBlock();
        assign(loopIt, numRepetitions) = (numRepetitions - 1_val, InstructionDecorations::PHI_NODE);
    }

    // after loop
    it.nextInBlock();
    it.emplace(new MethodCall("vc4cl_semaphore_increment", {1_val}));
    it.nextInBlock();
    it.emplace(new Branch(afterLabel));
}

InstructionWalker intrinsics::intrinsifyBarrier(Method& method, InstructionWalker it, const MethodCall* callSite)
{
    /*
     * "All work-items in a work-group executing the kernel on a processor must execute this function
     *  before any are allowed to continue execution beyond the barrier."
     *
     * -> Make sure, any work-item blocks until all are ready
     *
     * Barriers are implemented via semaphores.
     *
     * At the beginning, all semaphores are assumed to be at the default-value of zero.
     *
     * When encountering the barrier-function, do the following:
     * - For all but the first work-item
     *   1. increment the semaphore for the first work-item by one
     *   2. decrement the semaphore for the current work-item index by one (thus blocking until released by previous
     *      work-item index)
     *   3. increment and thus release semaphore for the next work-item index (if there is one)
     * - For the first work-item
     *   1. decrement the semaphore for this first work-item once per other work-item being executed (thus blocking
     *      until all other work-items have entered the barrier() code)
     *   2. increment and thus release semaphore for the second work-item index (if there is one)
     * -> This makes sure, that every work-item blocks until all other work-items increased its semaphore
     *
     * Since the semaphores have 4-bit (16 values) and there is a maximum of 12 work-items, they can't stall because of
     * overflow beyond 15
     */
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Intrinsifying control flow barrier: " << callSite->to_string() << logging::endl);

    Optional<Value> localSizeScalar = NO_VALUE;
    auto maximumWorkGroupSize = NUM_QPUS;

    if(method.metaData.isWorkGroupSizeSet())
    {
        if(method.metaData.getWorkGroupSize() == 1)
        {
            // for a single work-item there is no need to synchronize
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Control flow barrier with single work-item is a no-op!" << logging::endl);
            // erase barrier() function call
            it.erase();
            // so next instruction is not skipped
            it.previousInBlock();
            return it;
        }
        // for other (fixed known) work-group sized, we can at least simplify some of the checks and skip some blocks
        maximumWorkGroupSize = method.metaData.getWorkGroupSize();
        localSizeScalar = Value(Literal(maximumWorkGroupSize), TYPE_INT8);
    }

    // since we do insert functions that needs intrinsification, we need to go over all of them again
    auto origIt = it.copy().previousInBlock();

    // calculate the scalar local ID and size
    auto localIdX = method.addNewLocal(TYPE_INT8, "%local_id_x");
    it.emplace(new MethodCall(Value(localIdX), "vc4cl_local_id", {0_val}));
    it.nextInBlock();
    auto localIdY = method.addNewLocal(TYPE_INT8, "%local_id_y");
    it.emplace(new MethodCall(Value(localIdY), "vc4cl_local_id", {1_val}));
    it.nextInBlock();
    auto localIdZ = method.addNewLocal(TYPE_INT8, "%local_id_z");
    it.emplace(new MethodCall(Value(localIdZ), "vc4cl_local_id", {2_val}));
    it.nextInBlock();
    auto localSizeX = method.addNewLocal(TYPE_INT8, "%local_size_x");
    it.emplace(new MethodCall(Value(localSizeX), "vc4cl_local_size", {0_val}));
    it.nextInBlock();
    auto localSizeY = method.addNewLocal(TYPE_INT8, "%local_size_y");
    it.emplace(new MethodCall(Value(localSizeY), "vc4cl_local_size", {1_val}));
    it.nextInBlock();
    auto localSizeZ = method.addNewLocal(TYPE_INT8, "%local_size_z");
    it.emplace(new MethodCall(Value(localSizeZ), "vc4cl_local_size", {2_val}));
    it.nextInBlock();

    // local_id_scalar = local_id_z * local_size_y * local_size_x + local_id_y * local_size_x + local_id_x
    // => (local_id_z * local_size_y + local_id_y) * local_size_x + local_id_x
    auto tmp = assign(it, TYPE_INT8, "%local_id_scalar") = mul24(localIdZ, localSizeY);
    tmp = assign(it, TYPE_INT8, "%local_id_scalar") = tmp + localIdY;
    tmp = assign(it, TYPE_INT8, "%local_id_scalar") = mul24(tmp, localSizeX);
    auto localIdScalar = assign(it, TYPE_INT8, "%local_id_scalar") =
        (tmp + localIdX, InstructionDecorations::UNSIGNED_RESULT);

    if(!localSizeScalar)
    {
        // local_size_scalar = local_size_z * local_size_y * local_size_x
        tmp = assign(it, TYPE_INT8, "%local_size_scalar") = mul24(localSizeZ, localSizeY);
        localSizeScalar = assign(it, TYPE_INT8, "%local_size_scalar") = mul24(tmp, localSizeX);
    }

    auto branchIt = it.copy().previousInBlock();

    // we need to create the other blocks first to be able to correctly update the CFG
    auto otherLabel = method.addNewLocal(TYPE_LABEL, "%barrier_other").local();
    it = method.emplaceLabel(it, new BranchLabel(*otherLabel));
    auto otherBlock = it.getBasicBlock();
    it.nextInBlock();

    auto secondaryLabel = method.addNewLocal(TYPE_LABEL, "%barrier_secondary").local();
    it = method.emplaceLabel(it, new BranchLabel(*secondaryLabel));
    auto secondaryBlock = it.getBasicBlock();
    it.nextInBlock();

    auto primaryLabel = method.addNewLocal(TYPE_LABEL, "%barrier_primary").local();
    it = method.emplaceLabel(it, new BranchLabel(*primaryLabel));
    auto primaryBlock = it.getBasicBlock();
    it.nextInBlock();

    auto afterLabel = method.addNewLocal(TYPE_LABEL, "%barrier_after").local();
    it = method.emplaceLabel(it, new BranchLabel(*afterLabel));
    it.nextInBlock();
    // erase barrier() function call
    it.erase();

    // insert conditional jump for:
    // - local ID != 0 -> secondary block
    // - local ID == 0 && local size > 1 -> primary block
    // - local ID == 0 && local size == 0 -> after block
    branchIt.nextInBlock();
    BranchCond cond = BRANCH_ALWAYS;
    std::tie(branchIt, cond) = insertBranchCondition(method, branchIt, localIdScalar /* local ID != 0 */);
    branchIt.emplace(new Branch(secondaryLabel, cond));
    branchIt.nextInBlock();
    branchIt.emplace(new Branch(otherLabel, cond.invert()));

    branchIt = otherBlock->walkEnd();
    tmp = assign(branchIt, TYPE_INT8) = (*localSizeScalar ^ 1_val);
    std::tie(branchIt, cond) = insertBranchCondition(method, branchIt, tmp /* local size != 1 */);
    branchIt.emplace(new Branch(primaryLabel, cond));
    branchIt.nextInBlock();
    branchIt.emplace(new Branch(afterLabel, cond.invert()));
    branchIt.nextInBlock();

    // insert the actual content
    insertNonPrimaryBarrierCode(
        method, *secondaryBlock, localIdScalar, *localSizeScalar, maximumWorkGroupSize, afterLabel);
    insertPrimaryBarrierCode(method, *primaryBlock, localIdScalar, *localSizeScalar, maximumWorkGroupSize, afterLabel);

    return origIt.nextInBlock();
}
