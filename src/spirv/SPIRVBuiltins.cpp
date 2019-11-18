/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVBuiltins.h"

#include "../InstructionWalker.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/VectorHelper.h"
#include "log.h"

#ifdef SPIRV_FRONTEND

using namespace vc4c;
using namespace vc4c::spirv2qasm;

SPIRVBuiltin::~SPIRVBuiltin() noexcept = default;

// get_work_dim - scalar integer
SPIRVBuiltin spirv2qasm::BUILTIN_WORK_DIMENSIONS{
    spv::BuiltIn::WorkDim, TYPE_INT8, "%builtin_work_dimensions", "vc4cl_work_dimensions", false};
// get_global_size - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_GLOBAL_SIZE{
    spv::BuiltIn::GlobalSize, TYPE_INT32.toVectorType(3), "%builtin_global_size", "vc4cl_global_size", true};
// get_global_id - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_GLOBAL_ID{
    spv::BuiltIn::GlobalInvocationId, TYPE_INT32.toVectorType(3), "%builtin_global_id", "vc4cl_global_id", true};
// get_local_size - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_LOCAL_SIZE{
    spv::BuiltIn::WorkgroupSize, TYPE_INT8.toVectorType(3), "%builtin_local_size", "vc4cl_local_size", true};
// get_local_id - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_LOCAL_ID{
    spv::BuiltIn::LocalInvocationId, TYPE_INT8.toVectorType(3), "%builtin_local_id", "vc4cl_local_id", true};
// get_num_groups - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_NUM_GROUPS{
    spv::BuiltIn::NumWorkgroups, TYPE_INT32.toVectorType(3), "%builtin_num_groups", "vc4cl_num_groups", true};
// get_group_id - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_GROUP_ID{
    spv::BuiltIn::WorkgroupId, TYPE_INT32.toVectorType(3), "%builtin_group_id", "vc4cl_group_id", true};
// get_global_offset - int3 vector
SPIRVBuiltin spirv2qasm::BUILTIN_GLOBAL_OFFSET{
    spv::BuiltIn::GlobalOffset, TYPE_INT32.toVectorType(3), "%builtin_global_offset", "vc4cl_global_offset", true};

std::string spirv2qasm::BUILTIN_INTRINSIC{"load_builtin"};

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
            if(auto rot = dynamic_cast<const intermediate::VectorRotation*>(reader))
            {
                if(auto offset = rot->getOffset().checkImmediate())
                    rotationOffsets.emplace(offset->getRotationOffset().value_or(*offset));
                else
                {
                    rotationOffsets.clear();
                    break;
                }
            }
            else if(auto move = dynamic_cast<const intermediate::MoveOperation*>(reader))
            {
                if(move->isSimpleMove() && move->getOutput() && move->getOutput()->type.isScalarType())
                    // "rotation" by 0
                    rotationOffsets.emplace(0);
                else
                {
                    rotationOffsets.clear();
                    break;
                }
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
                offset = NATIVE_VECTOR_SIZE - offset;
            return Value(Literal(offset), TYPE_INT8);
        }
    }
    return NO_VALUE;
}

void spirv2qasm::lowerBuiltins(Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    auto intrinsicOp = it.get<intermediate::IntrinsicOperation>();
    const SPIRVBuiltin* builtInt = nullptr;
    if(intrinsicOp && intrinsicOp->opCode == BUILTIN_INTRINSIC &&
        (builtInt = dynamic_cast<const SPIRVBuiltin*>(intrinsicOp->getArgument(0) & &Value::checkLocal)))
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Lowering reading of SPIR-V built-in to intrinsic function call: " << it->to_string()
                << logging::endl);
        if(!builtInt->hasDimensionalArgument)
        {
            // built-in has no dimensional argument -> simply convert to intrinsic function
            it.reset((new intermediate::MethodCall(Value{*it->getOutput()}, std::string{builtInt->intrinsicFunction}))
                         ->copyExtrasFrom(intrinsicOp));
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
        it.emplace(new intermediate::MethodCall(Value{tmp}, std::string{builtInt->intrinsicFunction}, {*arg}));

        // replace this instruction with vector rotation in opposite direction of the result to balance out the now
        // wrong rotations of the output vector for element extraction
        it.nextInBlock();
        it = intermediate::insertVectorRotation(
            it, tmp, *arg, *intrinsicOp->getOutput(), intermediate::Direction::UP, true);
        it.erase();
    }
}
#endif /* SPIRV_FRONTEND */
