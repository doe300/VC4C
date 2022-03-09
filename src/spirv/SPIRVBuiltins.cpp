/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVBuiltins.h"

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/VectorHelper.h"
#include "../intrinsics/WorkItems.h"
#include "log.h"

#include "spirv/unified1/spirv.hpp11"

using namespace vc4c;
using namespace vc4c::spirv;

// get_work_dim - scalar integer
static const SPIRVBuiltin BUILTIN_WORK_DIMENSIONS{
    TYPE_INT8, "%builtin_work_dimensions", std::make_pair(intrinsics::FUNCTION_NAME_NUM_DIMENSIONS, false)};
// get_global_size - int3 vector
static const SPIRVBuiltin BUILTIN_GLOBAL_SIZE{
    TYPE_INT32.toVectorType(3), "%builtin_global_size", std::make_pair(intrinsics::FUNCTION_NAME_GLOBAL_SIZE, true)};
// get_global_id - int3 vector
static const SPIRVBuiltin BUILTIN_GLOBAL_ID{
    TYPE_INT32.toVectorType(3), "%builtin_global_id", std::make_pair(intrinsics::FUNCTION_NAME_GLOBAL_ID, true)};
// get_local_size - int3 vector
static const SPIRVBuiltin BUILTIN_LOCAL_SIZE{
    TYPE_INT8.toVectorType(3), "%builtin_local_size", std::make_pair(intrinsics::FUNCTION_NAME_LOCAL_SIZE, true)};
// get_local_id - int3 vector
static const SPIRVBuiltin BUILTIN_LOCAL_ID{
    TYPE_INT8.toVectorType(3), "%builtin_local_id", std::make_pair(intrinsics::FUNCTION_NAME_LOCAL_ID, true)};
// get_num_groups - int3 vector
static const SPIRVBuiltin BUILTIN_NUM_GROUPS{
    TYPE_INT32.toVectorType(3), "%builtin_num_groups", std::make_pair(intrinsics::FUNCTION_NAME_NUM_GROUPS, true)};
// get_group_id - int3 vector
static const SPIRVBuiltin BUILTIN_GROUP_ID{
    TYPE_INT32.toVectorType(3), "%builtin_group_id", std::make_pair(intrinsics::FUNCTION_NAME_GROUP_ID, true)};
// get_global_offset - int3 vector
static const SPIRVBuiltin BUILTIN_GLOBAL_OFFSET{TYPE_INT32.toVectorType(3), "%builtin_global_offset",
    std::make_pair(intrinsics::FUNCTION_NAME_GLOBAL_OFFSET, true)};
// get_local_linear_id - scalar size_t
static const SPIRVBuiltin BUILTIN_LOCAL_LINEAR_ID{
    TYPE_INT32, "%builtin_local_linear_id", std::make_pair(intrinsics::FUNCTION_NAME_LOCAL_LINEAR_ID, false)};
// get_global_linear_id - scalar size_t
static const SPIRVBuiltin BUILTIN_GLOBAL_LINEAR_ID{
    TYPE_INT32, "%builtin_global_linear_id", std::make_pair(intrinsics::FUNCTION_NAME_GLOBAL_LINEAR_ID, false)};

std::string spirv::BUILTIN_INTRINSIC{"load_builtin"};

const SPIRVBuiltin* spirv::mapToBuiltinLocal(spv::BuiltIn builtin)
{
    switch(builtin)
    {
    case spv::BuiltIn::WorkDim:
        return &BUILTIN_WORK_DIMENSIONS;
    case spv::BuiltIn::GlobalSize:
        return &BUILTIN_GLOBAL_SIZE;
    case spv::BuiltIn::GlobalInvocationId:
        return &BUILTIN_GLOBAL_ID;
    case spv::BuiltIn::WorkgroupSize:
        return &BUILTIN_LOCAL_SIZE;
    case spv::BuiltIn::LocalInvocationId:
        return &BUILTIN_LOCAL_ID;
    case spv::BuiltIn::NumWorkgroups:
        return &BUILTIN_NUM_GROUPS;
    case spv::BuiltIn::WorkgroupId:
        return &BUILTIN_GROUP_ID;
    case spv::BuiltIn::GlobalOffset:
        return &BUILTIN_GLOBAL_OFFSET;
    case spv::BuiltIn::LocalInvocationIndex:
        return &BUILTIN_LOCAL_LINEAR_ID;
    case spv::BuiltIn::GlobalLinearId:
        return &BUILTIN_GLOBAL_LINEAR_ID;
    default:
        return nullptr;
    }
}

static Optional<Value> getDimensionalArgument(const intermediate::IntrinsicOperation& intrinsicOp)
{
    // if the intrinsic function calls are converted from a SPIR-V variable with built-in decoration, they do not have
    // any argument, but they return a vector and in the next instructions, the correct element is extracted from that
    // vector via a vector rotation.
    if(auto out = intrinsicOp.checkOutputLocal())
    {
        // TODO does not support dynamic dimension parameter
        FastSet<uint8_t> rotationOffsets;
        for(const auto& reader : out->getUsers(LocalUse::Type::READER))
        {
            if(auto rot = reader->getVectorRotation())
            {
                auto offset = rot->offset;
                rotationOffsets.emplace(offset.getRotationOffset().value_or(offset));
            }
            else if(reader->isSimpleMove() && reader->getOutput() && reader->getOutput()->type.isScalarType())
            {
                // "rotation" by 0
                rotationOffsets.emplace(0);
            }
            else
            {
                rotationOffsets.clear();
                break;
            }
        }

        if(rotationOffsets.size() == 1)
        {
            // all reads have the same offset, the corresponding element is read
            auto offset = *rotationOffsets.begin();
            if(offset != 0)
                offset = static_cast<uint8_t>(NATIVE_VECTOR_SIZE - offset);
            return Value(Literal(offset), TYPE_INT8);
        }
    }
    return NO_VALUE;
}

void spirv::lowerBuiltins(Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    auto intrinsicOp = it.get<intermediate::IntrinsicOperation>();
    const SPIRVBuiltin* builtInt = nullptr;
    if(intrinsicOp && intrinsicOp->opCode == BUILTIN_INTRINSIC &&
        (builtInt = dynamic_cast<const SPIRVBuiltin*>(intrinsicOp->getArgument(0) & &Value::checkLocal)))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Lowering reading of SPIR-V built-in to intrinsic function call: " << it->to_string()
                << logging::endl);
        if(!builtInt->getValue().second)
        {
            // built-in has no dimensional argument -> simply convert to intrinsic function
            it.reset(intermediate::createWithExtras<intermediate::MethodCall>(
                *intrinsicOp, Value{*it->getOutput()}, std::string{builtInt->getValue().first}));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Replaced reading of SPIR-V built-in with intrinsic function call: " << it->to_string()
                    << logging::endl);
            return;
        }
        auto arg = getDimensionalArgument(*intrinsicOp);
        if(!arg)
        {
            throw CompilationError(CompilationStep::NORMALIZER,
                "Failed to determine dimensional argument for work-item built-in", it->to_string());
        }

        // insert intrinsic function call to temporary
        auto tmp = method.addNewLocal(builtInt->type.getElementType(), builtInt->name);
        it.emplace(std::make_unique<intermediate::MethodCall>(
            Value{tmp}, std::string{builtInt->getValue().first}, std::vector<Value>{*arg}));

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Replaced reading of SPIR-V built-in with intrinsic function call and vector rotation: "
                << it->to_string() << logging::endl);

        // replace this instruction with vector rotation in opposite direction of the result to balance out the now
        // wrong rotations of the output vector for element extraction
        it.nextInBlock();
        it = intermediate::insertVectorRotation(
            it, tmp, *arg, *intrinsicOp->getOutput(), intermediate::Direction::UP, true);
        it.erase();
    }
}
