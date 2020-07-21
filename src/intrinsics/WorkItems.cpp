/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "WorkItems.h"

#include "../Module.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "log.h"

#include <bitset>
#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

const std::string intrinsics::FUNCTION_NAME_LOCAL_SIZE = "vc4cl_local_size";
const std::string intrinsics::FUNCTION_NAME_LOCAL_ID = "vc4cl_local_id";
const std::string intrinsics::FUNCTION_NAME_NUM_DIMENSIONS = "vc4cl_work_dimensions";
const std::string intrinsics::FUNCTION_NAME_NUM_GROUPS = "vc4cl_num_groups";
const std::string intrinsics::FUNCTION_NAME_GROUP_ID = "vc4cl_group_id";
const std::string intrinsics::FUNCTION_NAME_GLOBAL_OFFSET = "vc4cl_global_offset";
const std::string intrinsics::FUNCTION_NAME_GLOBAL_SIZE = "vc4cl_global_size";
const std::string intrinsics::FUNCTION_NAME_GLOBAL_ID = "vc4cl_global_id";

static NODISCARD InstructionWalker intrinsifyReadWorkGroupInfo(Method& method, InstructionWalker it, const Value& arg,
    const std::vector<BuiltinLocal::Type>& locals, const Value& defaultValue, const InstructionDecorations decoration)
{
    if(auto lit = arg.getLiteralValue())
    {
        Value src = UNDEFINED_VALUE;
        switch(lit->unsignedInt())
        {
        case 0:
            src = method.findOrCreateBuiltin(locals.at(0))->createReference();
            break;
        case 1:
            src = method.findOrCreateBuiltin(locals.at(1))->createReference();
            break;
        case 2:
            src = method.findOrCreateBuiltin(locals.at(2))->createReference();
            break;
        default:
            src = defaultValue;
        }
        return it.reset((new MoveOperation(it->getOutput().value(), src))->copyExtrasFrom(it.get()));
    }
    // set default value first and always, so a path for the destination local is guaranteed
    assign(it, it->getOutput().value()) = defaultValue;
    // dim == 0 -> return first value
    assign(it, NOP_REGISTER) = (arg ^ 0_val, SetFlag::SET_FLAGS);
    it.emplace(new MoveOperation(
        it->getOutput().value(), method.findOrCreateBuiltin(locals.at(0))->createReference(), COND_ZERO_SET));
    it->addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION));
    it.nextInBlock();
    // dim == 1 -> return second value
    assign(it, NOP_REGISTER) = (arg ^ 1_val, SetFlag::SET_FLAGS);
    it.emplace(new MoveOperation(
        it->getOutput().value(), method.findOrCreateBuiltin(locals.at(1))->createReference(), COND_ZERO_SET));
    it->addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION));
    it.nextInBlock();
    // dim == 2 -> return third value
    assign(it, NOP_REGISTER) = (arg ^ 2_val, SetFlag::SET_FLAGS);
    it.reset((new MoveOperation(
        it->getOutput().value(), method.findOrCreateBuiltin(locals.at(2))->createReference(), COND_ZERO_SET)));
    it->addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION));
    return it;
}

static NODISCARD InstructionWalker intrinsifyReadWorkItemInfo(Method& method, InstructionWalker it, const Value& arg,
    BuiltinLocal::Type local, const InstructionDecorations decoration)
{
    /*
     * work-item infos (id, size) are stored within a single UNIFORM:
     * high <-> low byte
     * 00 | 3.dim | 2.dim | 1.dim
     * -> res = (UNIFORM >> (dim * 8)) & 0xFF
     */
    const Local* itemInfo = method.findOrCreateBuiltin(local);
    if(auto literalDim = (arg.getConstantValue() & &Value::getLiteralValue))
    {
        // NOTE: This forces the local_ids/local_sizes values to be on register-file A, but safes an instruction per
        // read
        switch(literalDim->unsignedInt())
        {
        case 0:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8A_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        case 1:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8B_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        case 2:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8C_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        case 3:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8D_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        default:
            return it.reset((new MoveOperation(it->getOutput().value(), INT_ZERO))
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        }
    }
    Value tmp0 = assign(it, TYPE_INT8) = mul24(arg, 8_val);
    Value tmp1 = assign(it, TYPE_INT8) = as_unsigned{itemInfo->createReference()} >> tmp0;
    return it.reset(
        (new Operation(OP_AND, it->getOutput().value(), tmp1, Value(Literal(static_cast<uint32_t>(0xFF)), TYPE_INT8)))
            ->copyExtrasFrom(it.get())
            ->addDecorations(decoration));
}

static NODISCARD InstructionWalker intrinsifyReadLocalSize(Method& method, InstructionWalker it, const Value& arg)
{
    auto decorations =
        add_flag(add_flag(InstructionDecorations::BUILTIN_LOCAL_SIZE, InstructionDecorations::UNSIGNED_RESULT),
            InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    /*
     * Use the value set via reqd_work_group_size(x, y, z) - if set - and return here.
     * This is valid, since the OpenCL standard states: "is the work-group size that must be used as the local_work_size
     * argument to clEnqueueNDRangeKernel." (page 231)
     */

    if(method.metaData.isWorkGroupSizeSet())
    {
        const auto& workGroupSizes = method.metaData.workGroupSizes;
        Optional<Literal> immediate;
        if(arg.isLiteralValue())
            // the dimension is a literal value -> look this dimension up
            immediate = arg.getLiteralValue();
        else if(std::all_of(workGroupSizes.begin(), workGroupSizes.end(), [](uint32_t u) -> bool { return u == 1; }))
            // all dimensions are 1 (for any set or not explicitly set dimension) -> take any of them
            immediate = Literal(0u);
        if(immediate)
        {
            if(immediate->unsignedInt() > workGroupSizes.size() || workGroupSizes.at(immediate->unsignedInt()) == 0)
            {
                return it.reset((new MoveOperation(it->getOutput().value(), INT_ONE))->addDecorations(decorations));
            }
            return it.reset((new MoveOperation(it->getOutput().value(),
                                 Value(Literal(workGroupSizes.at(immediate->unsignedInt())), TYPE_INT8)))
                                ->addDecorations(decorations));
        }
    }
    // TODO needs to have a size of 1 for all higher dimensions (instead of currently implicit 0)
    return intrinsifyReadWorkItemInfo(method, it, arg, BuiltinLocal::Type::LOCAL_SIZES, decorations);
}

static NODISCARD InstructionWalker intrinsifyReadLocalID(Method& method, InstructionWalker it, const Value& arg)
{
    if(method.metaData.isWorkGroupSizeSet() &&
        std::all_of(method.metaData.workGroupSizes.begin(), method.metaData.workGroupSizes.end(),
            [](uint32_t u) -> bool { return u == 1; }))
    {
        // if all the work-group sizes are 1, the ID is always 0 for all dimensions
        return it.reset((new MoveOperation(it->getOutput().value(), INT_ZERO))
                            ->addDecorations(add_flag(
                                InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT)));
    }
    return intrinsifyReadWorkItemInfo(method, it, arg, BuiltinLocal::Type::LOCAL_IDS,
        add_flag(InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT));
}

bool intrinsics::intrinsifyWorkItemFunction(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
        return false;
    if(callSite->getArguments().size() > 1)
        return false;

    if(callSite->methodName == FUNCTION_NAME_NUM_DIMENSIONS && callSite->getArguments().empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of work-item dimensions" << logging::endl);
        // setting the type to int8 allows us to optimize e.g. multiplications with work-item values
        Value out = callSite->getOutput().value();
        out.type = TYPE_INT8;
        it.reset(
            (new MoveOperation(out, method.findOrCreateBuiltin(BuiltinLocal::Type::WORK_DIMENSIONS)->createReference()))
                ->copyExtrasFrom(callSite)
                ->addDecorations(add_flag(callSite->decoration,
                    add_flag(add_flag(InstructionDecorations::BUILTIN_WORK_DIMENSIONS,
                                 InstructionDecorations::UNSIGNED_RESULT),
                        InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_NUM_GROUPS && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying reading of the number of work-groups" << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y, BuiltinLocal::Type::NUM_GROUPS_Z},
            INT_ONE,
            add_flag(add_flag(InstructionDecorations::BUILTIN_NUM_GROUPS, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_GROUP_ID && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of the work-group ids" << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z}, INT_ZERO,
            add_flag(add_flag(InstructionDecorations::BUILTIN_GROUP_ID, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_GLOBAL_OFFSET && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of the global offsets" << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
                BuiltinLocal::Type::GLOBAL_OFFSET_Z},
            INT_ZERO,
            add_flag(add_flag(InstructionDecorations::BUILTIN_GLOBAL_OFFSET, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_LOCAL_SIZE && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of local work-item sizes" << logging::endl);
        it = intrinsifyReadLocalSize(method, it, callSite->assertArgument(0));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_LOCAL_ID && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of local work-item ids" << logging::endl);
        it = intrinsifyReadLocalID(method, it, callSite->assertArgument(0));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_GLOBAL_SIZE && callSite->getArguments().size() == 1)
    {
        // global_size(dim) = local_size(dim) * num_groups(dim)
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of global work-item sizes" << logging::endl);

        const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
        const Value tmpNumGroups = method.addNewLocal(TYPE_INT32, "%num_groups");
        // emplace dummy instructions to be replaced
        it.emplace(new MoveOperation(tmpLocalSize, NOP_REGISTER));
        it = intrinsifyReadLocalSize(method, it, callSite->assertArgument(0));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpNumGroups, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y, BuiltinLocal::Type::NUM_GROUPS_Z},
            INT_ONE, add_flag(InstructionDecorations::BUILTIN_NUM_GROUPS, InstructionDecorations::UNSIGNED_RESULT));
        it.nextInBlock();
        it.reset((new Operation(OP_MUL24, callSite->getOutput().value(), tmpLocalSize, tmpNumGroups))
                     ->copyExtrasFrom(callSite)
                     ->addDecorations(add_flag(callSite->decoration,
                         add_flag(add_flag(InstructionDecorations::BUILTIN_GLOBAL_SIZE,
                                      InstructionDecorations::UNSIGNED_RESULT),
                             InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))));
        return true;
    }
    if(callSite->methodName == FUNCTION_NAME_GLOBAL_ID && callSite->getArguments().size() == 1)
    {
        // global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of global work-item ids" << logging::endl);

        const Value tmpGroupID = method.addNewLocal(TYPE_INT32, "%group_id");
        const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
        const Value tmpGlobalOffset = method.addNewLocal(TYPE_INT32, "%global_offset");
        const Value tmpLocalID = method.addNewLocal(TYPE_INT8, "%local_id");
        const Value tmpRes0 = method.addNewLocal(TYPE_INT32, "%group_global_id");
        const Value tmpRes1 = method.addNewLocal(TYPE_INT32, "%group_global_id");
        // emplace dummy instructions to be replaced
        it.emplace(new MoveOperation(tmpGroupID, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z}, INT_ZERO,
            add_flag(InstructionDecorations::BUILTIN_GROUP_ID, InstructionDecorations::UNSIGNED_RESULT));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpLocalSize, NOP_REGISTER));
        it = intrinsifyReadLocalSize(method, it, callSite->assertArgument(0));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpGlobalOffset, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
                BuiltinLocal::Type::GLOBAL_OFFSET_Z},
            INT_ZERO, add_flag(InstructionDecorations::BUILTIN_GLOBAL_OFFSET, InstructionDecorations::UNSIGNED_RESULT));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpLocalID, NOP_REGISTER));
        it = intrinsifyReadLocalID(method, it, callSite->assertArgument(0));
        it.nextInBlock();
        assign(it, tmpRes0) = mul24(tmpGroupID, tmpLocalSize);
        assign(it, tmpRes1) = tmpGlobalOffset + tmpRes0;
        it.reset(
            (new Operation(OP_ADD, callSite->getOutput().value(), tmpRes1, tmpLocalID))
                ->copyExtrasFrom(callSite)
                ->addDecorations(add_flag(callSite->decoration,
                    add_flag(InstructionDecorations::BUILTIN_GLOBAL_ID, InstructionDecorations::UNSIGNED_RESULT))));
        return true;
    }
    return false;
}

static void insertSingleSecondaryBlockCode(Method& method, BasicBlock& block, uint32_t index,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel)
{
    // decrement "own" semaphore to block until previous work-item released it
    auto it = block.walkEnd();
    it.emplace(new SemaphoreAdjustment(static_cast<Semaphore>(index), false));
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
            blockIt.emplace(new SemaphoreAdjustment(static_cast<Semaphore>(index + 1), true));
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
    it.emplace(new SemaphoreAdjustment(static_cast<Semaphore>(0), true));
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
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel,
    const std::function<InstructionWalker(InstructionWalker)>& insertFirstWorkItemOnlyCode)
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
        loopIt.emplace(new SemaphoreAdjustment(static_cast<Semaphore>(0), false));
        loopIt.nextInBlock();
        assign(loopIt, numRepetitions) = (numRepetitions - 1_val, InstructionDecorations::PHI_NODE);
    }
    // after loop
    it.nextInBlock();

    // at this point, local ID is the only work-item running, the rest is blocked on the semaphores
    if(insertFirstWorkItemOnlyCode)
        it = insertFirstWorkItemOnlyCode(it);

    it.emplace(new SemaphoreAdjustment(static_cast<Semaphore>(1), true));
    it.nextInBlock();
    it.emplace(new Branch(afterLabel));
}

static void lowerBarrier(Method& method, InstructionWalker it, const MethodCall* callSite,
    const std::function<InstructionWalker(InstructionWalker)>& insertFirstWorkItemOnlyCode)
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

    Optional<Value> localSizeScalar = NO_VALUE;
    auto maximumWorkGroupSize = NUM_QPUS;

    if(method.metaData.isWorkGroupSizeSet())
    {
        if(method.metaData.getWorkGroupSize() == 1)
        {
            // for a single work-item there is no need to synchronize
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Control flow barrier with single work-item is a no-op!" << logging::endl);
            if(insertFirstWorkItemOnlyCode)
                it = insertFirstWorkItemOnlyCode(it);
            // erase barrier() function call
            it.erase();
            return;
        }
        // for other (fixed known) work-group sized, we can at least simplify some of the checks and skip some blocks
        maximumWorkGroupSize = method.metaData.getWorkGroupSize();
        localSizeScalar = Value(Literal(maximumWorkGroupSize), TYPE_INT8);
    }

    // calculate the scalar local ID and size
    auto localIdX = method.addNewLocal(TYPE_INT8, "%local_id_x");
    it.emplace(new MethodCall(Value(localIdX), "vc4cl_local_id", {0_val}));
    it = intrinsifyReadLocalID(method, it, 0_val);
    it.nextInBlock();
    auto localIdY = method.addNewLocal(TYPE_INT8, "%local_id_y");
    it.emplace(new MethodCall(Value(localIdY), "vc4cl_local_id", {1_val}));
    it = intrinsifyReadLocalID(method, it, 1_val);
    it.nextInBlock();
    auto localIdZ = method.addNewLocal(TYPE_INT8, "%local_id_z");
    it.emplace(new MethodCall(Value(localIdZ), "vc4cl_local_id", {2_val}));
    it = intrinsifyReadLocalID(method, it, 2_val);
    it.nextInBlock();
    auto localSizeX = method.addNewLocal(TYPE_INT8, "%local_size_x");
    it.emplace(new MethodCall(Value(localSizeX), "vc4cl_local_size", {0_val}));
    it = intrinsifyReadLocalSize(method, it, 0_val);
    it.nextInBlock();
    auto localSizeY = method.addNewLocal(TYPE_INT8, "%local_size_y");
    it.emplace(new MethodCall(Value(localSizeY), "vc4cl_local_size", {1_val}));
    it = intrinsifyReadLocalSize(method, it, 1_val);
    it.nextInBlock();
    auto localSizeZ = method.addNewLocal(TYPE_INT8, "%local_size_z");
    it.emplace(new MethodCall(Value(localSizeZ), "vc4cl_local_size", {2_val}));
    it = intrinsifyReadLocalSize(method, it, 2_val);
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

    auto beforeAfterIt = it.copy().previousInMethod();
    auto afterLabel = method.addNewLocal(TYPE_LABEL, "%barrier_after").local();
    it = method.emplaceLabel(it, new BranchLabel(*afterLabel));
    it.nextInBlock();

    auto skipBarrierLabel = afterLabel;
    if(insertFirstWorkItemOnlyCode)
    {
        // if we insert some first work-item only code, we need to also execute it if we have a single work-item
        // (work-group size is one)!
        skipBarrierLabel = method.addNewLocal(TYPE_LABEL, "%barrier_skip").local();
        beforeAfterIt.nextInMethod();
        beforeAfterIt = method.emplaceLabel(beforeAfterIt, new BranchLabel(*skipBarrierLabel));
        beforeAfterIt.nextInBlock();
        beforeAfterIt = insertFirstWorkItemOnlyCode(beforeAfterIt);
        beforeAfterIt.emplace(new Branch(afterLabel));
        beforeAfterIt.nextInBlock();
    }

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
    branchIt.emplace(new Branch(skipBarrierLabel, cond.invert()));
    branchIt.nextInBlock();

    // insert the actual content
    insertNonPrimaryBarrierCode(
        method, *secondaryBlock, localIdScalar, *localSizeScalar, maximumWorkGroupSize, afterLabel);
    insertPrimaryBarrierCode(method, *primaryBlock, localIdScalar, *localSizeScalar, maximumWorkGroupSize, afterLabel,
        insertFirstWorkItemOnlyCode);
}

InstructionWalker intrinsics::intrinsifyBarrier(Method& method, InstructionWalker it, const MethodCall* callSite)
{
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Intrinsifying control flow barrier: " << callSite->to_string() << logging::endl);
    // since we do insert functions that needs intrinsification, we need to go over all of them again
    auto origIt = it.copy().previousInBlock();
    lowerBarrier(method, it, callSite, {});
    return origIt.nextInBlock();
}

void intrinsics::insertControlFlowBarrier(Method& method, InstructionWalker it,
    const std::function<InstructionWalker(InstructionWalker)>& insertFirstWorkItemOnlyCode)
{
    it.emplace(new intermediate::MethodCall("dummy", {}));
    lowerBarrier(method, it, it.get<intermediate::MethodCall>(), insertFirstWorkItemOnlyCode);
}
