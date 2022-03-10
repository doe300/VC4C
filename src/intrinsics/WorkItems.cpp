/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "WorkItems.h"

#include "../Module.h"
#include "../intermediate/Helper.h"
#include "../intermediate/VectorHelper.h"
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
const std::string intrinsics::FUNCTION_NAME_LOCAL_LINEAR_ID = "vc4cl_local_linear_id";
const std::string intrinsics::FUNCTION_NAME_GLOBAL_LINEAR_ID = "vc4cl_global_linear_id";

static InstructionDecorations getDimension(uint32_t dimension) noexcept
{
    if(dimension == 0)
        return InstructionDecorations::DIMENSION_X;
    if(dimension == 1)
        return InstructionDecorations::DIMENSION_Y;
    if(dimension == 2)
        return InstructionDecorations::DIMENSION_Z;
    return InstructionDecorations::NONE;
}

static InstructionDecorations getDimension(const Value& val)
{
    if(auto literal = val.getConstantValue() & &Value::getLiteralValue)
        return getDimension(literal->unsignedInt());
    return InstructionDecorations::NONE;
}

static NODISCARD InstructionWalker intrinsifyReadWorkGroupInfo(Method& method, InstructionWalker it, const Value& arg,
    const std::vector<BuiltinLocal::Type>& locals, const Value& defaultValue, const InstructionDecorations decoration)
{
    if(auto lit = (arg.getConstantValue() & &Value::getLiteralValue))
    {
        Value src = UNDEFINED_VALUE;
        InstructionDecorations dimension = InstructionDecorations::NONE;
        switch(lit->unsignedInt())
        {
        case 0:
            src = method.findOrCreateBuiltin(locals.at(0))->createReference();
            dimension = InstructionDecorations::DIMENSION_X;
            break;
        case 1:
            src = method.findOrCreateBuiltin(locals.at(1))->createReference();
            dimension = InstructionDecorations::DIMENSION_Y;
            break;
        case 2:
            src = method.findOrCreateBuiltin(locals.at(2))->createReference();
            dimension = InstructionDecorations::DIMENSION_Z;
            break;
        default:
            src = defaultValue;
        }
        it.reset(createWithExtras<MoveOperation>(*it.get(), it->getOutput().value(), src));
        it->addDecorations(decoration).addDecorations(dimension);
        return it;
    }
    // set default value first and always, so a path for the destination local is guaranteed
    assign(it, it->getOutput().value()) = defaultValue;
    // dim == 0 -> return first value
    assign(it, NOP_REGISTER) = (arg ^ 0_val, SetFlag::SET_FLAGS);
    it.emplace(std::make_unique<MoveOperation>(
                   it->getOutput().value(), method.findOrCreateBuiltin(locals.at(0))->createReference(), COND_ZERO_SET))
        .addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION))
        .addDecorations(InstructionDecorations::DIMENSION_X);
    it.nextInBlock();
    // dim == 1 -> return second value
    assign(it, NOP_REGISTER) = (arg ^ 1_val, SetFlag::SET_FLAGS);
    it.emplace(std::make_unique<MoveOperation>(
                   it->getOutput().value(), method.findOrCreateBuiltin(locals.at(1))->createReference(), COND_ZERO_SET))
        .addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION))
        .addDecorations(InstructionDecorations::DIMENSION_Y);
    it.nextInBlock();
    // dim == 2 -> return third value
    assign(it, NOP_REGISTER) = (arg ^ 2_val, SetFlag::SET_FLAGS);
    it.reset(std::make_unique<MoveOperation>(
                 it->getOutput().value(), method.findOrCreateBuiltin(locals.at(2))->createReference(), COND_ZERO_SET))
        .addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION))
        .addDecorations(InstructionDecorations::DIMENSION_Z);
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
            it.reset(createWithExtras<MoveOperation>(*it.get(), it->getOutput().value(), itemInfo->createReference()))
                .setUnpackMode(UNPACK_8A_32)
                .addDecorations(decoration)
                .addDecorations(InstructionDecorations::DIMENSION_X);
            break;
        case 1:
            it.reset(createWithExtras<MoveOperation>(*it.get(), it->getOutput().value(), itemInfo->createReference()))
                .setUnpackMode(UNPACK_8B_32)
                .addDecorations(decoration)
                .addDecorations(InstructionDecorations::DIMENSION_Y);
            break;
        case 2:
            it.reset(createWithExtras<MoveOperation>(*it.get(), it->getOutput().value(), itemInfo->createReference()))
                .setUnpackMode(UNPACK_8C_32)
                .addDecorations(decoration)
                .addDecorations(InstructionDecorations::DIMENSION_Z);
            break;
        case 3:
            it.reset(createWithExtras<MoveOperation>(*it.get(), it->getOutput().value(), itemInfo->createReference()))
                .setUnpackMode(UNPACK_8D_32)
                .addDecorations(decoration);
            break;
        default:
            it.reset(createWithExtras<MoveOperation>(*it.get(), it->getOutput().value(), INT_ZERO))
                .addDecorations(decoration);
            break;
        }
        return it;
    }
    Value tmp0 = assign(it, TYPE_INT8) = mul24(arg, 8_val);
    Value tmp1 = assign(it, TYPE_INT8) = as_unsigned{itemInfo->createReference()} >> tmp0;
    it.reset(createWithExtras<Operation>(*it.get(), OP_AND, it->getOutput().value(), tmp1,
                 Value(Literal(static_cast<uint32_t>(0xFF)), TYPE_INT8)))
        .addDecorations(decoration);
    return it;
}

static NODISCARD InstructionWalker intrinsifyReadLocalSize(Method& method, InstructionWalker it, const Value& arg)
{
    auto decorations = add_flag(InstructionDecorations::BUILTIN_LOCAL_SIZE, InstructionDecorations::UNSIGNED_RESULT,
        InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    /*
     * Use the value set via reqd_work_group_size(x, y, z) - if set - and return here.
     * This is valid, since the OpenCL standard states: "is the work-group size that must be used as the local_work_size
     * argument to clEnqueueNDRangeKernel." (page 231)
     */

    if(auto fixedSize = method.metaData.getFixedWorkGroupSize())
    {
        const auto& workGroupSizes = method.metaData.workGroupSizes;
        Optional<Literal> immediate;
        if(auto lit = (arg.getConstantValue() & &Value::getLiteralValue))
            // the dimension is a literal value -> look this dimension up
            immediate = lit;
        else if(fixedSize == 1u)
            // all dimensions are 1 (for any set or not explicitly set dimension) -> take any of them
            immediate = Literal(0u);
        if(immediate)
        {
            if(immediate->unsignedInt() > workGroupSizes.size() || workGroupSizes.at(immediate->unsignedInt()) == 0)
                it.reset(std::make_unique<MoveOperation>(it->getOutput().value(), INT_ONE)).addDecorations(decorations);
            else
                it.reset(std::make_unique<MoveOperation>(it->getOutput().value(),
                             Value(Literal(workGroupSizes.at(immediate->unsignedInt())), TYPE_INT8)))
                    .addDecorations(decorations)
                    .addDecorations(getDimension(immediate->unsignedInt()));
            return it;
        }
    }
    // TODO needs to have a size of 1 for all higher dimensions (instead of currently implicit 0)
    return intrinsifyReadWorkItemInfo(method, it, arg, BuiltinLocal::Type::LOCAL_SIZES, decorations);
}

static NODISCARD InstructionWalker intrinsifyReadLocalID(Method& method, InstructionWalker it, const Value& arg)
{
    if(method.metaData.getFixedWorkGroupSize() == 1u)
    {
        // Needs to be queried before the instruction is overwritten (and thus the argument is freed)
        auto dimension = getDimension(arg);
        // if all the work-group sizes are 1, the ID is always 0 for all dimensions
        it.reset(std::make_unique<MoveOperation>(it->getOutput().value(), INT_ZERO))
            .addDecorations(add_flag(InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT))
            .addDecorations(dimension);
        return it;
    }
    return intrinsifyReadWorkItemInfo(method, it, arg, BuiltinLocal::Type::LOCAL_IDS,
        add_flag(InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT));
}

static NODISCARD InstructionWalker intrinsifyReadLocalLinearID(Method& method, InstructionWalker it,
    Value* outLocalSizeX = nullptr, Value* outLocalSizeY = nullptr, Value* outLocalSizeZ = nullptr)
{
    if(method.metaData.getFixedWorkGroupSize() == 1u)
    {
        // if all the work-group sizes are 1, the ID is always 0 for all dimensions
        if(outLocalSizeX)
            *outLocalSizeX = 1_val;
        if(outLocalSizeY)
            *outLocalSizeY = 1_val;
        if(outLocalSizeZ)
            *outLocalSizeZ = 1_val;

        it.reset(std::make_unique<MoveOperation>(it->getOutput().value(), INT_ZERO))
            .addDecorations(add_flag(InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT))
            .addDecorations(intermediate::InstructionDecorations::DIMENSION_SCALAR);
        return it;
    }
    // TODO can be optimized if we know the number of dimensions, i.e. can omit higher dimensions

    // calculate the scalar local ID and size
    auto localIdX = method.addNewLocal(TYPE_INT8, "%local_id_x");
    it.emplace(std::make_unique<MethodCall>(
        Value(localIdX), std::string(intrinsics::FUNCTION_NAME_LOCAL_ID), std::vector<Value>{0_val}));
    it = intrinsifyReadLocalID(method, it, 0_val);
    it.nextInBlock();
    auto localIdY = method.addNewLocal(TYPE_INT8, "%local_id_y");
    it.emplace(std::make_unique<MethodCall>(
        Value(localIdY), std::string(intrinsics::FUNCTION_NAME_LOCAL_ID), std::vector<Value>{1_val}));
    it = intrinsifyReadLocalID(method, it, 1_val);
    it.nextInBlock();
    auto localIdZ = method.addNewLocal(TYPE_INT8, "%local_id_z");
    it.emplace(std::make_unique<MethodCall>(
        Value(localIdZ), std::string(intrinsics::FUNCTION_NAME_LOCAL_ID), std::vector<Value>{2_val}));
    it = intrinsifyReadLocalID(method, it, 2_val);
    it.nextInBlock();
    auto localSizeX = method.addNewLocal(TYPE_INT8, "%local_size_x");
    it.emplace(std::make_unique<MethodCall>(
        Value(localSizeX), std::string(intrinsics::FUNCTION_NAME_LOCAL_SIZE), std::vector<Value>{0_val}));
    it = intrinsifyReadLocalSize(method, it, 0_val);
    it.nextInBlock();
    if(outLocalSizeX)
        *outLocalSizeX = localSizeX;
    auto localSizeY = method.addNewLocal(TYPE_INT8, "%local_size_y");
    it.emplace(std::make_unique<MethodCall>(
        Value(localSizeY), std::string(intrinsics::FUNCTION_NAME_LOCAL_SIZE), std::vector<Value>{1_val}));
    it = intrinsifyReadLocalSize(method, it, 1_val);
    it.nextInBlock();
    if(outLocalSizeY)
        *outLocalSizeY = localSizeY;
    if(outLocalSizeZ)
    {
        *outLocalSizeZ = method.addNewLocal(TYPE_INT8, "%local_size_z");
        it.emplace(std::make_unique<MethodCall>(
            Value(*outLocalSizeZ), std::string(intrinsics::FUNCTION_NAME_LOCAL_SIZE), std::vector<Value>{2_val}));
        it = intrinsifyReadLocalSize(method, it, 2_val);
        it.nextInBlock();
    }

    // local_id_scalar = local_id_z * local_size_y * local_size_x + local_id_y * local_size_x + local_id_x
    // => (local_id_z * local_size_y + local_id_y) * local_size_x + local_id_x
    auto tmp = assign(it, TYPE_INT8, "%local_id_scalar") = mul24(localIdZ, localSizeY);
    tmp = assign(it, TYPE_INT8, "%local_id_scalar") = tmp + localIdY;
    tmp = assign(it, TYPE_INT8, "%local_id_scalar") = mul24(tmp, localSizeX);
    it.reset(std::make_unique<Operation>(OP_ADD, it->getOutput().value(), tmp, localIdX))
        .addDecorations(add_flag(InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT,
            InstructionDecorations::DIMENSION_SCALAR));
    return it;
}

static NODISCARD InstructionWalker intrinsifyReadGlobalID(
    Method& method, InstructionWalker it, const Value& arg, bool includeOffset = true)
{
    const Value tmpGroupID = method.addNewLocal(TYPE_INT32, "%group_id");
    const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
    const Value tmpGlobalOffset = method.addNewLocal(TYPE_INT32, "%global_offset");
    const Value tmpLocalID = method.addNewLocal(TYPE_INT8, "%local_id");

    // emplace dummy instructions to be replaced
    it.emplace(std::make_unique<MoveOperation>(tmpGroupID, NOP_REGISTER));
    it = intrinsifyReadWorkGroupInfo(method, it, arg,
        {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z}, INT_ZERO,
        add_flag(InstructionDecorations::BUILTIN_GROUP_ID, InstructionDecorations::UNSIGNED_RESULT,
            InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
    it.nextInBlock();
    it.emplace(std::make_unique<MoveOperation>(tmpLocalSize, NOP_REGISTER));
    it = intrinsifyReadLocalSize(method, it, arg);
    it.nextInBlock();
    if(includeOffset)
    {
        it.emplace(std::make_unique<MoveOperation>(tmpGlobalOffset, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, arg,
            {BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
                BuiltinLocal::Type::GLOBAL_OFFSET_Z},
            INT_ZERO,
            add_flag(InstructionDecorations::BUILTIN_GLOBAL_OFFSET, InstructionDecorations::UNSIGNED_RESULT,
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        it.nextInBlock();
    }
    it.emplace(std::make_unique<MoveOperation>(tmpLocalID, NOP_REGISTER));
    it = intrinsifyReadLocalID(method, it, arg);
    it.nextInBlock();
    auto tmp = assign(it, TYPE_INT32, "%group_global_id") =
        (mul24(tmpGroupID, tmpLocalSize), InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    if(includeOffset)
        tmp = assign(it, TYPE_INT32, "%group_global_id") =
            (tmpGlobalOffset + tmp, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);

    // Needs to be queried before the instruction is overwritten (and thus the argument is freed)
    auto dimension = getDimension(arg);
    it.reset(createWithExtras<Operation>(*it.get(), OP_ADD, it->getOutput().value(), tmp, tmpLocalID))
        .addDecorations(add_flag(
            it->decoration, InstructionDecorations::BUILTIN_GLOBAL_ID, InstructionDecorations::UNSIGNED_RESULT))
        .addDecorations(dimension);
    return it;
}

static NODISCARD InstructionWalker intrinsifyReadGlobalSize(Method& method, InstructionWalker it, const Value& arg)
{
    const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
    const Value tmpNumGroups = method.addNewLocal(TYPE_INT32, "%num_groups");
    // Needs to be queried before the instruction is overwritten (and thus the argument is freed)
    auto dimension = getDimension(arg);
    // emplace dummy instructions to be replaced
    it.emplace(std::make_unique<MoveOperation>(tmpLocalSize, NOP_REGISTER));
    it = intrinsifyReadLocalSize(method, it, arg);
    it.nextInBlock();
    it.emplace(std::make_unique<MoveOperation>(tmpNumGroups, NOP_REGISTER));
    it = intrinsifyReadWorkGroupInfo(method, it, arg,
        {BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y, BuiltinLocal::Type::NUM_GROUPS_Z}, INT_ONE,
        add_flag(InstructionDecorations::BUILTIN_NUM_GROUPS, InstructionDecorations::UNSIGNED_RESULT,
            InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
    it.nextInBlock();
    it.reset(createWithExtras<Operation>(*it.get(), OP_MUL24, it->getOutput().value(), tmpLocalSize, tmpNumGroups))
        .addDecorations(add_flag(it->decoration,
            add_flag(InstructionDecorations::BUILTIN_GLOBAL_SIZE, InstructionDecorations::UNSIGNED_RESULT,
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE)))
        .addDecorations(dimension);
    return it;
}

bool intrinsics::intrinsifyWorkItemFunction(Method& method, TypedInstructionWalker<intermediate::MethodCall> inIt)
{
    const auto& callSite = *inIt.get();
    InstructionWalker it = inIt;
    if(callSite.getArguments().size() > 1)
        return false;
    auto decoration = callSite.decoration;

    if(callSite.methodName == FUNCTION_NAME_NUM_DIMENSIONS && callSite.getArguments().empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of work-item dimensions into: " << callSite.getOutput().to_string()
                << logging::endl);
        // setting the type to int8 allows us to optimize e.g. multiplications with work-item values
        Value out = callSite.getOutput().value();
        out.type = TYPE_INT8;
        it.reset(createWithExtras<MoveOperation>(
                     callSite, out, method.findOrCreateBuiltin(BuiltinLocal::Type::WORK_DIMENSIONS)->createReference()))
            .addDecorations(add_flag(decoration,
                add_flag(InstructionDecorations::BUILTIN_WORK_DIMENSIONS, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_NUM_GROUPS && callSite.getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of the number of work-groups into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite.assertArgument(0),
            {BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y, BuiltinLocal::Type::NUM_GROUPS_Z},
            INT_ONE,
            add_flag(InstructionDecorations::BUILTIN_NUM_GROUPS, InstructionDecorations::UNSIGNED_RESULT,
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_GROUP_ID && callSite.getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of the work-group ids into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite.assertArgument(0),
            {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z}, INT_ZERO,
            add_flag(InstructionDecorations::BUILTIN_GROUP_ID, InstructionDecorations::UNSIGNED_RESULT,
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_GLOBAL_OFFSET && callSite.getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of the global offsets into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite.assertArgument(0),
            {BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
                BuiltinLocal::Type::GLOBAL_OFFSET_Z},
            INT_ZERO,
            add_flag(InstructionDecorations::BUILTIN_GLOBAL_OFFSET, InstructionDecorations::UNSIGNED_RESULT,
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_LOCAL_SIZE && callSite.getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of local work-item sizes into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadLocalSize(method, it, callSite.assertArgument(0));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_LOCAL_ID && callSite.getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of local work-item ids into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadLocalID(method, it, callSite.assertArgument(0));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_GLOBAL_SIZE && callSite.getArguments().size() == 1)
    {
        // global_size(dim) = local_size(dim) * num_groups(dim)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of global work-item sizes into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadGlobalSize(method, it, callSite.assertArgument(0));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_GLOBAL_ID && callSite.getArguments().size() == 1)
    {
        // global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of global work-item ids into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadGlobalID(method, it, callSite.assertArgument(0));
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_LOCAL_LINEAR_ID && callSite.getArguments().empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of local linear work-item ids into: " << callSite.getOutput().to_string()
                << logging::endl);
        it = intrinsifyReadLocalLinearID(method, it);
        return true;
    }
    if(callSite.methodName == FUNCTION_NAME_GLOBAL_LINEAR_ID && callSite.getArguments().empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying reading of global linear work-item ids into: " << callSite.getOutput().to_string()
                << logging::endl);

        // TODO can be optimized if we know the number of dimensions, i.e. can omit higher dimensions
        auto globalIdScalarZ = method.addNewLocal(TYPE_INT8, "%global_id_scalar");
        it.emplace(std::make_unique<MethodCall>(
            Value(globalIdScalarZ), std::string(intrinsics::FUNCTION_NAME_GLOBAL_ID), std::vector<Value>{2_val}));
        it = intrinsifyReadGlobalID(method, it, 2_val, false);
        it.nextInBlock();
        auto globalIdScalarY = method.addNewLocal(TYPE_INT8, "%global_id_scalar");
        it.emplace(std::make_unique<MethodCall>(
            Value(globalIdScalarY), std::string(intrinsics::FUNCTION_NAME_GLOBAL_ID), std::vector<Value>{1_val}));
        it = intrinsifyReadGlobalID(method, it, 1_val, false);
        it.nextInBlock();
        auto globalIdScalarX = method.addNewLocal(TYPE_INT8, "%global_id_scalar");
        it.emplace(std::make_unique<MethodCall>(
            Value(globalIdScalarX), std::string(intrinsics::FUNCTION_NAME_GLOBAL_ID), std::vector<Value>{0_val}));
        it = intrinsifyReadGlobalID(method, it, 0_val, false);
        it.nextInBlock();
        auto globalSizeY = method.addNewLocal(TYPE_INT8, "%global_size_y");
        it.emplace(std::make_unique<MethodCall>(
            Value(globalSizeY), std::string(intrinsics::FUNCTION_NAME_GLOBAL_ID), std::vector<Value>{1_val}));
        it = intrinsifyReadGlobalSize(method, it, 1_val);
        it.nextInBlock();
        auto globalSizeX = method.addNewLocal(TYPE_INT8, "%global_size_x");
        it.emplace(std::make_unique<MethodCall>(
            Value(globalSizeX), std::string(intrinsics::FUNCTION_NAME_GLOBAL_ID), std::vector<Value>{0_val}));
        it = intrinsifyReadGlobalSize(method, it, 0_val);
        it.nextInBlock();

        // global_linear_id = ((global_id(2) - global_offset(2)) * global_size(1) * global_size(0)) +
        //                    ((global_id(1) - global_offset(1)) * global_size(0)) + (global_id(0) - global_offset(0))
        //                  = ((global_id(2) - global_offset(2)) * global_size(1) + (global_id(1) - global_offset(1)) *
        //                    global_size(0)) + (global_id(0) - global_offset(0))
        //                  = (global_id_no_offset_z * global_size_y + global_id_no_offset_y) * global_size_x +
        //                    global_id_no_offset_x
        // XXX Is mul24 enough? Is more than 2^23 work-items realistic on VC4?
        auto tmp = assign(it, TYPE_INT32, "%global_id_scalar") = mul24(globalIdScalarZ, globalSizeY);
        tmp = assign(it, TYPE_INT32, "%global_id_scalar") = tmp + globalIdScalarY;
        tmp = assign(it, TYPE_INT32, "%global_id_scalar") = mul24(tmp, globalSizeX);
        it.reset(createWithExtras<Operation>(callSite, OP_ADD, callSite.getOutput().value(), tmp, globalIdScalarX))
            .addDecorations(add_flag(decoration,
                add_flag(InstructionDecorations::BUILTIN_GLOBAL_ID, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::DIMENSION_SCALAR));
        return true;
    }
    return false;
}

static void insertSingleSecondaryBlockCode(Method& method, BasicBlock& block, uint32_t index,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel)
{
    // decrement "own" semaphore to block until previous work-item released it
    auto it = block.walkEnd();
    it.emplace(std::make_unique<SemaphoreAdjustment>(static_cast<Semaphore>(index), false));
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
        auto releaseBlock = method.addNewLocal(TYPE_LABEL, "%barrier_next_release").local();
        InstructionWalker blockIt = method.emplaceLabel(it, std::make_unique<BranchLabel>(*releaseBlock));
        {
            blockIt.nextInBlock();
            blockIt.emplace(std::make_unique<SemaphoreAdjustment>(static_cast<Semaphore>(index + 1), true));
            blockIt.nextInBlock();
            blockIt.emplace(std::make_unique<Branch>(afterLabel));
        }

        branchIt.nextInBlock();
        BranchCond branchCond = BRANCH_ALWAYS;
        std::tie(branchIt, branchCond) = insertBranchCondition(method, branchIt, tmp);
        branchIt.emplace(std::make_unique<Branch>(releaseBlock, branchCond));
        branchIt.nextInBlock();
        branchIt.emplace(std::make_unique<Branch>(afterLabel, branchCond.invert()));
    }
    else
        it.emplace(std::make_unique<Branch>(afterLabel));
}

static void insertNonPrimaryBarrierCode(Method& method, BasicBlock& block, const Value& localIdScalar,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel)
{
    // all but the first work-item increment semaphore 0, so the first work-item can continue, wait on their own
    // semaphore and then increment the next one (if any)
    auto it = block.walkEnd();
    it.emplace(std::make_unique<SemaphoreAdjustment>(static_cast<Semaphore>(0), true));
    it.nextInBlock();
    // replicate to make sure the SIMD element 15 which is actually used by the branch is set correctly
    auto localId = method.addNewLocal(localIdScalar.type, "%local_id_scalar");
    auto switchIt = intermediate::insertReplication(it, localIdScalar, localId);
    if(auto writer = localIdScalar.getSingleWriter())
        switchIt.copy().previousInBlock()->addDecorations(writer->decoration);

    // we need to create the other blocks first to be able to correctly update the CFG
    SortedMap<unsigned, const Local*> singleBlocks;
    for(unsigned i = 1; i < maxGroupSize; ++i)
    {
        auto label = method.addNewLocal(TYPE_LABEL, "%barrier_single_block").local();
        it = method.emplaceLabel(it, std::make_unique<BranchLabel>(*label));
        singleBlocks.emplace(i, label);
        insertSingleSecondaryBlockCode(method, *it.getBasicBlock(), i, localSizeScalar, maxGroupSize, afterLabel);
    }

    // insert switch-case for all possible local IDs
    auto branchTarget = method.addNewLocal(TYPE_CODE_ADDRESS, "%barrier_switch");
    switchIt.emplace(std::make_unique<CodeAddress>(branchTarget, afterLabel));
    switchIt.nextInBlock();
    for(unsigned i = 1; i < maxGroupSize; ++i)
    {
        auto cond = assignNop(switchIt) = as_unsigned{localId} == as_unsigned{Value(Literal(i), TYPE_INT8)};
        switchIt.emplace(std::make_unique<CodeAddress>(branchTarget, singleBlocks.at(i), cond));
        switchIt.nextInBlock();
    }
    switchIt.emplace(std::make_unique<Branch>(branchTarget.local()));
}

static void insertPrimaryBarrierCode(Method& method, BasicBlock& block, const Value& localIdScalar,
    const Value& localSizeScalar, uint32_t maxGroupSize, const Local* afterLabel,
    const std::function<InstructionWalker(InstructionWalker)>& insertFirstWorkItemOnlyCode)
{
    // the first work-item waits on its semaphore for all other to have increased it (size -1 times) and then wakes up
    // the second work-item
    auto it = block.walkEnd();
    // just copy to ease register association
    auto localSize = assign(it, localSizeScalar.type, "%local_size_scalar") = localSizeScalar;
    auto numRepetitions = assign(it, TYPE_INT8) = (localSize - 1_val, InstructionDecorations::PHI_NODE);
    auto& loopBlock = insertLoop(method, it, numRepetitions, "%barrier_primary_loop");
    {
        // inside loop
        auto loopIt = loopBlock.walk().nextInBlock();
        loopIt.emplace(std::make_unique<SemaphoreAdjustment>(static_cast<Semaphore>(0), false));
        loopIt.nextInBlock();
        assign(loopIt, numRepetitions) = (numRepetitions - 1_val, InstructionDecorations::PHI_NODE);
    }
    // after loop
    it.nextInBlock();

    // at this point, local ID is the only work-item running, the rest is blocked on the semaphores
    if(insertFirstWorkItemOnlyCode)
        it = insertFirstWorkItemOnlyCode(it);

    it.emplace(std::make_unique<SemaphoreAdjustment>(static_cast<Semaphore>(1), true));
    it.nextInBlock();
    it.emplace(std::make_unique<Branch>(afterLabel));
}

static void lowerBarrier(Method& method, InstructionWalker it,
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

    if(auto fixedSize = method.metaData.getFixedWorkGroupSize())
    {
        if(fixedSize == 1u)
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
        localSizeScalar = Value(Literal(*fixedSize), TYPE_INT8);
    }

    // calculate the scalar local ID and size
    Value localSizeX = UNDEFINED_VALUE;
    Value localSizeY = UNDEFINED_VALUE;
    Value localSizeZ = UNDEFINED_VALUE;

    auto localIdScalar = method.addNewLocal(TYPE_INT8, "%local_id_scalar");
    it.emplace(std::make_unique<MethodCall>(
        Value(localIdScalar), std::string(intrinsics::FUNCTION_NAME_LOCAL_LINEAR_ID), std::vector<Value>{}));
    it = intrinsifyReadLocalLinearID(method, it, &localSizeX, &localSizeY, &localSizeZ);
    it.nextInBlock();

    if(!localSizeScalar)
    {
        // local_size_scalar = local_size_z * local_size_y * local_size_x
        auto tmp = assign(it, TYPE_INT8, "%local_size_scalar") = mul24(localSizeZ, localSizeY);
        localSizeScalar = assign(it, TYPE_INT8, "%local_size_scalar") = (mul24(tmp, localSizeX),
            InstructionDecorations::UNSIGNED_RESULT, InstructionDecorations::BUILTIN_LOCAL_SIZE,
            InstructionDecorations::DIMENSION_SCALAR, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    }

    auto branchIt = it.copy().previousInBlock();

    // we need to create the other blocks first to be able to correctly update the CFG
    auto otherLabel = method.addNewLocal(TYPE_LABEL, "%barrier_other").local();
    it = method.emplaceLabel(it, std::make_unique<BranchLabel>(*otherLabel));
    auto otherBlock = it.getBasicBlock();
    it.nextInBlock();

    auto secondaryLabel = method.addNewLocal(TYPE_LABEL, "%barrier_secondary").local();
    it = method.emplaceLabel(it, std::make_unique<BranchLabel>(*secondaryLabel));
    auto secondaryBlock = it.getBasicBlock();
    it.nextInBlock();

    auto primaryLabel = method.addNewLocal(TYPE_LABEL, "%barrier_primary").local();
    it = method.emplaceLabel(it, std::make_unique<BranchLabel>(*primaryLabel));
    auto primaryBlock = it.getBasicBlock();
    it.nextInBlock();

    auto beforeAfterIt = it.copy().previousInMethod();
    auto afterLabel = method.addNewLocal(TYPE_LABEL, "%barrier_after").local();
    it = method.emplaceLabel(it, std::make_unique<BranchLabel>(*afterLabel));
    it.nextInBlock();

    auto skipBarrierLabel = afterLabel;
    if(insertFirstWorkItemOnlyCode)
    {
        // if we insert some first work-item only code, we need to also execute it if we have a single work-item
        // (work-group size is one)!
        skipBarrierLabel = method.addNewLocal(TYPE_LABEL, "%barrier_skip").local();
        beforeAfterIt.nextInMethod();
        beforeAfterIt = method.emplaceLabel(beforeAfterIt, std::make_unique<BranchLabel>(*skipBarrierLabel));
        beforeAfterIt.nextInBlock();
        beforeAfterIt = insertFirstWorkItemOnlyCode(beforeAfterIt);
        beforeAfterIt.emplace(std::make_unique<Branch>(afterLabel));
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
    branchIt.emplace(std::make_unique<Branch>(secondaryLabel, cond));
    branchIt.nextInBlock();
    branchIt.emplace(std::make_unique<Branch>(otherLabel, cond.invert()));

    branchIt = otherBlock->walkEnd();
    auto tmp = assign(branchIt, TYPE_INT8) = (*localSizeScalar ^ 1_val);
    std::tie(branchIt, cond) = insertBranchCondition(method, branchIt, tmp /* local size != 1 */);
    branchIt.emplace(std::make_unique<Branch>(primaryLabel, cond));
    branchIt.nextInBlock();
    branchIt.emplace(std::make_unique<Branch>(skipBarrierLabel, cond.invert()));
    branchIt.nextInBlock();

    // insert the actual content
    insertNonPrimaryBarrierCode(
        method, *secondaryBlock, localIdScalar, *localSizeScalar, maximumWorkGroupSize, afterLabel);
    insertPrimaryBarrierCode(method, *primaryBlock, localIdScalar, *localSizeScalar, maximumWorkGroupSize, afterLabel,
        insertFirstWorkItemOnlyCode);
}

InstructionWalker intrinsics::intrinsifyBarrier(Method& method, TypedInstructionWalker<intermediate::MethodCall> inIt)
{
    const auto& callSite = *inIt.get();
    InstructionWalker it = inIt;
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Intrinsifying control flow barrier: " << callSite.to_string() << logging::endl);
    // since we do insert functions that needs intrinsification, we need to go over all of them again
    auto origIt = it.copy().previousInBlock();
    lowerBarrier(method, it, {});
    return origIt.nextInBlock();
}

void intrinsics::insertControlFlowBarrier(Method& method, InstructionWalker it,
    const std::function<InstructionWalker(InstructionWalker)>& insertFirstWorkItemOnlyCode)
{
    it.emplace(std::make_unique<intermediate::MethodCall>("dummy", std::vector<Value>{}));
    lowerBarrier(method, it, insertFirstWorkItemOnlyCode);
}
