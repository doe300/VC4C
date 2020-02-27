/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVOperation.h"

#ifdef SPIRV_FRONTEND
#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "../intrinsics/Images.h"
#include "../intrinsics/Operators.h"
#include "SPIRVBuiltins.h"
#include "log.h"

#include <algorithm>
#include <cstdbool>

using namespace vc4c;
using namespace vc4c::spirv;

static Value toNewLocal(Method& method, const uint32_t id, const uint32_t typeID, const TypeMapping& typeMappings,
    LocalTypeMapping& localTypes, LocalMapping& localMapping)
{
    localTypes[id] = typeID;
    auto loc = method.createLocal(typeMappings.at(typeID), std::string("%") + std::to_string(id));
    localMapping.emplace(id, loc);
    return loc->createReference();
}

static DataType getType(const uint32_t id, const TypeMapping& types, const ConstantMapping& constants,
    const LocalTypeMapping& localTypes, const LocalMapping& localMapping)
{
    auto tit = types.find(id);
    if(tit != types.end())
        return tit->second;
    auto cit = constants.find(id);
    if(cit != constants.end())
        return cit->second.type;
    auto mit = localMapping.find(id);
    if(mit != localMapping.end())
        return mit->second->type;
    return types.at(localTypes.at(id));
}

static Value getValue(const uint32_t id, Method& method, const TypeMapping& types, const ConstantMapping& constants,
    const LocalTypeMapping& localTypes, LocalMapping& localMapping)
{
    if(id == UNDEFINED_ID)
        return UNDEFINED_VALUE;
    auto cit = constants.find(id);
    if(cit != constants.end())
        return cit->second.toValue().value_or(Value(cit->second.type));
    auto it = localMapping.find(id);
    if(it != localMapping.end())
        return it->second->createReference();
    auto loc = method.createLocal(getType(id, types, constants, localTypes, localMapping), "%" + std::to_string(id));
    localMapping.emplace(id, loc);
    return loc->createReference();
}

SPIRVOperation::SPIRVOperation(
    const uint32_t id, SPIRVMethod& method, const intermediate::InstructionDecorations decorations) :
    id(id),
    method(method), decorations(decorations)
{
}

SPIRVOperation::~SPIRVOperation() {}

SPIRVInstruction::SPIRVInstruction(const uint32_t id, SPIRVMethod& method, const std::string& opcode,
    const uint32_t resultType, std::vector<uint32_t>&& operands,
    const intermediate::InstructionDecorations decorations) :
    SPIRVOperation(id, method, decorations),
    typeID(resultType), opcode(opcode), operands(std::move(operands))
{
}

void SPIRVInstruction::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    Value arg0 = getValue(operands.at(0), *method.method, types, constants, localTypes, localMapping);
    Optional<Value> arg1(NO_VALUE);
    std::string opCode = opcode;
    if(OP_NEGATE == opCode)
    {
        opCode = dest.type.isFloatingType() ? "fsub" : "sub";
        arg1 = arg0;
        arg0 = INT_ZERO;
    }
    else if(operands.size() > 1)
    {
        arg1 = getValue(operands[1], *method.method, types, constants, localTypes, localMapping);
    }
    if(!arg1) // unary
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate unary operation '" << opcode << "' with " << arg0.to_string(false)
                << " into " << dest.to_string(true) << logging::endl);
        auto& op = OpCode::findOpCode(opCode);
        if(op != OP_NOP)
            method.method->appendToEnd(
                (new intermediate::Operation(op, std::move(dest), std::move(arg0)))->addDecorations(decorations));
        else
            method.method->appendToEnd(
                (new intermediate::IntrinsicOperation(std::move(opCode), std::move(dest), std::move(arg0)))
                    ->addDecorations(decorations));
    }
    else // binary
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate binary operation '" << opcode << "' with " << arg0.to_string(false)
                << " and " << arg1.to_string() << " into " << dest.to_string(true) << logging::endl);
        auto& op = OpCode::findOpCode(opCode);
        if(op != OP_NOP)
            method.method->appendToEnd(
                (new intermediate::Operation(op, std::move(dest), std::move(arg0), std::move(arg1.value())))
                    ->addDecorations(decorations));
        else
            method.method->appendToEnd((new intermediate::IntrinsicOperation(std::move(opCode), std::move(dest),
                                            std::move(arg0), std::move(arg1.value())))
                                           ->addDecorations(decorations));
    }
}

Optional<Value> SPIRVInstruction::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    const Value op1 = constants.at(operands.at(0)).toValue().value_or(UNDEFINED_VALUE);
    const Value op2 =
        operands.size() > 1 ? constants.at(operands[1]).toValue().value_or(UNDEFINED_VALUE) : UNDEFINED_VALUE;

    if(opcode == "fptoui")
        return Value(Literal(static_cast<uint32_t>(op1.literal().real())), TYPE_INT32);
    if(opcode == "fptosi")
        return Value(Literal(static_cast<int32_t>(op1.literal().real())), TYPE_INT32);
    if(opcode == "sitofp")
        return Value(Literal(static_cast<float>(op1.literal().signedInt())), TYPE_FLOAT);
    if(opcode == "uitofp")
        return Value(Literal(static_cast<float>(op1.literal().unsignedInt())), TYPE_FLOAT);
    if(opcode == OP_NEGATE)
        return op1.type.isFloatingType() ? Value(Literal(-op1.literal().real()), TYPE_FLOAT) :
                                           Value(Literal(-op1.literal().signedInt()), TYPE_INT32);
    if(opcode == "add")
        return Value(Literal(op1.literal().signedInt() + op2.literal().signedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "fadd")
        return Value(Literal(op1.literal().real() + op2.literal().real()), op1.type.getUnionType(op2.type));
    if(opcode == "sub")
        return Value(Literal(op1.literal().signedInt() - op2.literal().signedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "fsub")
        return Value(Literal(op1.literal().real() - op2.literal().real()), op1.type.getUnionType(op2.type));
    if(opcode == "mul")
        return Value(Literal(op1.literal().signedInt() * op2.literal().signedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "fmul")
        return Value(Literal(op1.literal().real() * op2.literal().real()), op1.type.getUnionType(op2.type));
    if(opcode == "udiv")
        return Value(
            Literal(op1.literal().unsignedInt() / op2.literal().unsignedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "sdiv")
        return Value(Literal(op1.literal().signedInt() / op2.literal().signedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "fdiv")
        return Value(Literal(op1.literal().real() / op2.literal().real()), op1.type.getUnionType(op2.type));
    if(opcode == "umod")
        return Value(
            Literal(op1.literal().unsignedInt() % op2.literal().unsignedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "srem")
        return Value(intrinsics::srem(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "smod")
        return Value(intrinsics::smod(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "frem")
        return Value(intrinsics::frem(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "fmod")
        return Value(intrinsics::fmod(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "or")
        return Value(
            Literal(op1.literal().unsignedInt() | op2.literal().unsignedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "and")
        return Value(
            Literal(op1.literal().unsignedInt() & op2.literal().unsignedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "xor")
        return Value(
            Literal(op1.literal().unsignedInt() ^ op2.literal().unsignedInt()), op1.type.getUnionType(op2.type));
    if(opcode == "not")
        return Value(Literal(~op1.literal().unsignedInt()), op1.type);
    if(opcode == "shr")
        // in C++, unsigned right shift is logical (fills with zeroes)
        return Value(Literal(op1.literal().unsignedInt() >> op2.literal().signedInt()), op1.type);
    if(opcode == "asr")
        return Value(intrinsics::asr(op1.literal(), op2.literal()), op1.type);
    if(opcode == "shl")
        return Value(Literal(op1.literal().unsignedInt() << op2.literal().signedInt()), op1.type);

    return NO_VALUE;
}

SPIRVComparison::SPIRVComparison(const uint32_t id, SPIRVMethod& method, const std::string& opcode,
    const uint32_t resultType, std::vector<uint32_t>&& operands,
    const intermediate::InstructionDecorations decorations) :
    SPIRVInstruction(id, method, opcode, resultType, std::move(operands), decorations)
{
}

void SPIRVComparison::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    Value arg0 = getValue(operands.at(0), *method.method, types, constants, localTypes, localMapping);
    Value arg1 = getValue(operands.at(1), *method.method, types, constants, localTypes, localMapping);
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate comparison '" << opcode << "' of " << arg0.to_string(false) << " and "
            << arg1.to_string(false) << " into " << dest.to_string(true) << logging::endl);
    method.method->appendToEnd(
        (new intermediate::Comparison(std::move(opcode), std::move(dest), std::move(arg0), std::move(arg1)))
            ->addDecorations(decorations));
}

Optional<Value> SPIRVComparison::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    const Value op1 = constants.at(operands.at(0)).toValue().value_or(UNDEFINED_VALUE);
    const Value op2 = constants.at(operands.at(1)).toValue().value_or(UNDEFINED_VALUE);

    if(intermediate::COMP_EQ == opcode)
        return op1 == op2 ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_FALSE == opcode)
        return BOOL_FALSE;
    if(intermediate::COMP_NEQ == opcode)
        return op1 != op2 ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_TRUE == opcode)
        return BOOL_TRUE;
    if(intermediate::COMP_SIGNED_GE == opcode)
        return op1.literal().signedInt() >= op2.literal().signedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_SIGNED_GT == opcode)
        return op1.literal().signedInt() > op2.literal().signedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_SIGNED_LE == opcode)
        return op1.literal().signedInt() <= op2.literal().signedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_SIGNED_LT == opcode)
        return op1.literal().signedInt() < op2.literal().signedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_UNSIGNED_GE == opcode)
        return op1.literal().unsignedInt() >= op2.literal().unsignedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_UNSIGNED_GT == opcode)
        return op1.literal().unsignedInt() > op2.literal().unsignedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_UNSIGNED_LE == opcode)
        return op1.literal().unsignedInt() <= op2.literal().unsignedInt() ? BOOL_TRUE : BOOL_FALSE;
    if(intermediate::COMP_UNSIGNED_LT == opcode)
        return op1.literal().unsignedInt() < op2.literal().unsignedInt() ? BOOL_TRUE : BOOL_FALSE;

    return NO_VALUE;
}

SPIRVCallSite::SPIRVCallSite(const uint32_t id, SPIRVMethod& method, const uint32_t methodID, const uint32_t resultType,
    std::vector<uint32_t>&& arguments) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    methodID(methodID), typeID(resultType), arguments(std::move(arguments))
{
}

SPIRVCallSite::SPIRVCallSite(const uint32_t id, SPIRVMethod& method, const std::string& methodName,
    const uint32_t resultType, std::vector<uint32_t>&& arguments) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), methodName(methodName), arguments(std::move(arguments))
{
}

SPIRVCallSite::SPIRVCallSite(SPIRVMethod& method, const std::string& methodName, std::vector<uint32_t>&& arguments) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE), typeID(UNDEFINED_ID),
    methodName(methodName), arguments(std::move(arguments))
{
}

void SPIRVCallSite::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value dest =
        id == UNDEFINED_ID ? UNDEFINED_VALUE : toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    std::string calledFunction = methodName.value_or("");
    if(methodID)
        calledFunction = methods.at(methodID.value()).method->name;
    std::vector<Value> args;
    for(const uint32_t op : arguments)
    {
        args.push_back(getValue(op, *method.method, types, constants, localTypes, localMapping));
    }
    // For some built-in OpenCL instruction, we need some special fix-up.
    // E.g. for vector load/stores the SPIR-V OpenCL built-in operations are called vloadn/vstoren, but the function
    // names are vloadN/vstoreN (where N is the vector size)
    if(calledFunction == "vloadn")
    {
        // the last parameter is the vector width, remove it and write it into the name
        auto num = arguments.back();
        args.pop_back();
        calledFunction = "vload" + std::to_string(num);
    }
    else if(calledFunction == "vstoren")
    {
        // the vector width is not explicitly given, so extract it from the argument types
        // arguments are: <value>, <index>, <address>
        auto num = args.front().type.getVectorWidth();
        if(num == 1 && args.size() == 3)
        {
            // For some instructions, an vstore1 is generated -> manually convert to index calculation and store
            // instruction
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Generating intermediate storage of " << args[0].to_string() << " into " << args[2].to_string()
                    << " at index " << args[1].to_string() << logging::endl);

            auto tmp = method.method->addNewLocal(args[2].type);
            ignoreReturnValue(intermediate::insertCalculateIndices(
                method.method->appendToEnd(), *method.method, args[2], tmp, {args[1]}));
            method.method->appendToEnd(new intermediate::MemoryInstruction(
                intermediate::MemoryOperation::WRITE, std::move(tmp), std::move(args[0])));
            return;
        }
        calledFunction = "vstore" + std::to_string(num);
    }
    if(dest.isUndefined())
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate call-site to void-function '" << calledFunction << "' with " << args.size()
                << " parameters" << logging::endl);
        method.method->appendToEnd(
            (new intermediate::MethodCall(std::move(calledFunction), std::move(args)))->addDecorations(decorations));
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate call-site to '" << calledFunction << "' with " << args.size()
                << " parameters into " << dest.to_string(true) << logging::endl);
        method.method->appendToEnd(
            (new intermediate::MethodCall(std::move(dest), std::move(calledFunction), std::move(args)))
                ->addDecorations(decorations));
    }
}

Optional<Value> SPIRVCallSite::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVBoolCallSite::SPIRVBoolCallSite(
    uint32_t id, SPIRVMethod& method, uint32_t methodID, uint32_t resultType, std::vector<uint32_t>&& arguments) :
    SPIRVCallSite(id, method, methodID, resultType, std::move(arguments))
{
}

SPIRVBoolCallSite::SPIRVBoolCallSite(uint32_t id, SPIRVMethod& method, const std::string& methodName,
    uint32_t resultType, std::vector<uint32_t>&& arguments) :
    SPIRVCallSite(id, method, methodName, resultType, std::move(arguments))
{
}

void SPIRVBoolCallSite::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    std::string calledFunction = methodName.value_or("");
    if(methodID)
        calledFunction = methods.at(methodID.value()).method->name;
    std::vector<Value> args;
    for(const uint32_t op : arguments)
    {
        args.push_back(getValue(op, *method.method, types, constants, localTypes, localMapping));
    }

    // Simply emitting a call-site/method-call would not be correct, since the SPIR-V operations being mapped return
    // type is bool (or a vector of bool), while the OpenCL C standard functions being mapped return int/vector of int.
    // Doing a simple map will result in inliner failing, since an integer return value cannot safely be assigned to a
    // boolean value.
    // Additionally, the OpenCL C functions return a 0 (false) and 1 (true) for scalar and a 0 (false) and -1 (true) for
    // vector-versions, which cannot be mapped 1:1 to returning a false/true value.
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate call-site with boolean conversion to '" << calledFunction << "' with "
            << args.size() << " parameters into " << dest.to_string(true) << logging::endl);
    auto tmp = method.method->addNewLocal(TYPE_INT32.toVectorType(dest.type.getVectorWidth()));
    method.method->appendToEnd((new intermediate::MethodCall(Value(tmp), std::move(calledFunction), std::move(args)))
                                   ->addDecorations(decorations));
    if(dest.type.isVectorType())
    {
        // returns 0 for false and -1 for true
        // 0 & 1 -> 0 and -1 & 1 -> 1
        method.method->appendToEnd(new intermediate::Operation(OP_AND, std::move(dest), std::move(tmp), INT_ONE));
    }
    else
    {
        // returns 0 for false and 1 for true
        method.method->appendToEnd(new intermediate::MoveOperation(std::move(dest), std::move(tmp)));
    }
}

SPIRVReturn::SPIRVReturn(SPIRVMethod& method) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE)
{
}

SPIRVReturn::SPIRVReturn(const uint32_t returnValue, SPIRVMethod& method) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE), returnValue(returnValue)
{
}

void SPIRVReturn::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    if(returnValue)
    {
        Value value = getValue(returnValue.value(), *method.method, types, constants, localTypes, localMapping);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate return of value: " << value.to_string(false) << logging::endl);
        method.method->appendToEnd(new intermediate::Return(std::move(value)));
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating intermediate return" << logging::endl);
        method.method->appendToEnd(new intermediate::Return());
    }
}

Optional<Value> SPIRVReturn::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    if(returnValue && constants.find(returnValue.value()) != constants.end())
        return constants.at(returnValue.value()).toValue();
    return NO_VALUE;
}

SPIRVBranch::SPIRVBranch(SPIRVMethod& method, const uint32_t labelID) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE), defaultLabelID(labelID)
{
}

SPIRVBranch::SPIRVBranch(
    SPIRVMethod& method, const uint32_t conditionID, const uint32_t trueLabelID, const uint32_t falseLabelID) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE),
    defaultLabelID(trueLabelID), conditionID(conditionID), falseLabelID(falseLabelID)
{
}

void SPIRVBranch::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    auto trueLabel = getValue(defaultLabelID, *method.method, types, constants, localTypes, localMapping);
    if(conditionID)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate conditional branch on %" << conditionID.value() << " to either %"
                << defaultLabelID << " or %" << falseLabelID.value() << logging::endl);
        const Value cond = getValue(conditionID.value(), *method.method, types, constants, localTypes, localMapping);
        auto falseLabel = getValue(falseLabelID.value(), *method.method, types, constants, localTypes, localMapping);
        method.method->appendToEnd(
            new intermediate::Branch(trueLabel.local(), COND_ZERO_CLEAR /* condition is true */, cond));
        method.method->appendToEnd(
            new intermediate::Branch(falseLabel.local(), COND_ZERO_SET /* condition is false */, cond));
    }
    else
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Generating intermediate branch to %" << defaultLabelID << logging::endl);
        method.method->appendToEnd(new intermediate::Branch(trueLabel.local(), COND_ALWAYS, BOOL_TRUE));
    }
}

Optional<Value> SPIRVBranch::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVLabel::SPIRVLabel(const uint32_t id, SPIRVMethod& method) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE)
{
}

void SPIRVLabel::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating intermediate label %" << id << logging::endl);
    auto label = getValue(id, *method.method, types, constants, localTypes, localMapping);
    method.method->appendToEnd(new intermediate::BranchLabel(*label.local()));
}

Optional<Value> SPIRVLabel::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVConversion::SPIRVConversion(const uint32_t id, SPIRVMethod& method, const uint32_t resultType,
    const uint32_t sourceID, const ConversionType type, const intermediate::InstructionDecorations decorations) :
    SPIRVOperation(id, method, decorations),
    typeID(resultType), sourceID(sourceID), type(type)
{
}

void SPIRVConversion::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value source = getValue(sourceID, *method.method, types, constants, localTypes, localMapping);
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    bool isSaturated = has_flag(decorations, intermediate::InstructionDecorations::SATURATED_CONVERSION);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate conversion from " << source.to_string(false) << " to " << dest.to_string(true)
            << logging::endl);

    if(type == ConversionType::BITCAST)
    {
        ignoreReturnValue(
            intermediate::insertBitcast(method.method->appendToEnd(), *method.method.get(), source, dest, decorations));
        return;
    }
    if(type == ConversionType::FLOATING)
    {
        if(isSaturated)
            throw CompilationError(
                CompilationStep::LLVM_2_IR, "Saturated floating-point conversion is not yet supported");
        method.method->appendToEnd((new intermediate::IntrinsicOperation("fptrunc", std::move(dest), std::move(source)))
                                       ->addDecorations(decorations));
        return;
    }

    // extend input value to 32-bit integer type with proper sign
    Value tmp = source;
    if(source.type.getScalarBitCount() < 32 &&
        (type == ConversionType::SIGNED_TO_SIGNED || type == ConversionType::SIGNED_TO_UNSIGNED))
    {
        tmp = method.method->addNewLocal(TYPE_INT32.toVectorType(source.type.getVectorWidth()));
        method.method->appendToEnd((new intermediate::IntrinsicOperation("sext", Value(tmp), std::move(source))));
    }
    if(source.type.getScalarBitCount() < 32 &&
        (type == ConversionType::UNSIGNED_TO_SIGNED || type == ConversionType::UNSIGNED_TO_UNSIGNED))
    {
        tmp = method.method->addNewLocal(TYPE_INT32.toVectorType(source.type.getVectorWidth()));
        method.method->appendToEnd((new intermediate::IntrinsicOperation("zext", Value(tmp), std::move(source)))
                                       ->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT));
    }

    // truncate/saturate to correctly sized type
    if(isSaturated)
    {
        intermediate::ConversionType outType;
        switch(type)
        {
        case ConversionType::SIGNED_TO_SIGNED:
            outType = intermediate::ConversionType::SIGNED_TO_SIGNED;
            break;
        case ConversionType::SIGNED_TO_UNSIGNED:
            outType = intermediate::ConversionType::SIGNED_TO_UNSIGNED;
            break;
        case ConversionType::UNSIGNED_TO_SIGNED:
            outType = intermediate::ConversionType::UNSIGNED_TO_SIGNED;
            break;
        case ConversionType::UNSIGNED_TO_UNSIGNED:
            outType = intermediate::ConversionType::UNSIGNED_TO_UNSIGNED;
            break;
        default:
            throw CompilationError(CompilationStep::LLVM_2_IR, "Unhandled conversion type!");
        }
        ignoreReturnValue(
            intermediate::insertSaturation(method.method->appendToEnd(), *method.method.get(), tmp, dest, outType));
        return;
    }
    else if(type == ConversionType::SIGNED_TO_SIGNED)
    {
        // signed 32-bit to signed other size -> move to retain 32-bit sign
        if(Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
            method.method->appendToEnd((new intermediate::IntrinsicOperation("sext", std::move(dest), std::move(tmp))));
        else if(Local::getLocalData<MultiRegisterData>(source.checkLocal()))
            method.method->appendToEnd(
                (new intermediate::MethodCall(std::move(dest), "vc4cl_long_to_int", {std::move(tmp)})));
        else
            method.method->appendToEnd(
                (new intermediate::MoveOperation(std::move(dest), std::move(tmp)))->addDecorations(decorations));
        return;
    }
    else if(type == ConversionType::SIGNED_TO_UNSIGNED || type == ConversionType::UNSIGNED_TO_UNSIGNED)
    {
        // (un)signed 32-bit to unsigned some size -> trunc (since negative values are UB anyway)
        // TODO correct??
        if(Local::getLocalData<MultiRegisterData>(dest.checkLocal()) &&
            Local::getLocalData<MultiRegisterData>(source.checkLocal()))
            method.method->appendToEnd(
                (new intermediate::MethodCall(std::move(dest), "vc4cl_bitcast_ulong", {std::move(tmp)})));
        else if(Local::getLocalData<MultiRegisterData>(dest.checkLocal()))
            method.method->appendToEnd((new intermediate::IntrinsicOperation("zext", std::move(dest), std::move(tmp))));
        else if(Local::getLocalData<MultiRegisterData>(source.checkLocal()))
            method.method->appendToEnd(
                (new intermediate::MethodCall(std::move(dest), "vc4cl_long_to_int", {std::move(tmp)})));
        else
            method.method->appendToEnd(
                (new intermediate::IntrinsicOperation("trunc", std::move(dest), std::move(tmp)))
                    ->addDecorations(add_flag(decorations, intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return;
    }
    else if(type == ConversionType::UNSIGNED_TO_SIGNED)
    {
        // unsigned 32-bit to signed some size -> trunc + sext
        auto tmp2 = method.method->addNewLocal(dest.type);
        method.method->appendToEnd(
            (new intermediate::IntrinsicOperation("trunc", Value(tmp2), std::move(tmp)))
                ->addDecorations(add_flag(decorations, intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        method.method->appendToEnd((new intermediate::IntrinsicOperation("sext", std::move(dest), std::move(tmp2))));
        return;
    }
    throw CompilationError(CompilationStep::LLVM_2_IR, "This type of conversion is not yet implemented!");
}

Optional<Value> SPIRVConversion::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    auto it = constants.find(sourceID);
    if(it != constants.end())
    {
        auto source = it->second.toValue();
        if(!source)
            return NO_VALUE;
        Value dest(UNDEFINED_VALUE);
        auto destType = types.at(typeID);
        switch(type)
        {
        case ConversionType::BITCAST:
            dest = *source;
            dest.type = destType;
            return dest;
        case ConversionType::FLOATING:
            // double representation of all floating-point values is the same for the same value
            return source;
        case ConversionType::SIGNED_TO_SIGNED:
            // TODO trunc/sext + saturation
            break;
        case ConversionType::UNSIGNED_TO_UNSIGNED:
            // TODO trunc/zext + saturation
            break;
        case ConversionType::SIGNED_TO_UNSIGNED:
        case ConversionType::UNSIGNED_TO_SIGNED:
            break;
        }
    }
    return NO_VALUE;
}

SPIRVCopy::SPIRVCopy(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID,
    const MemoryAccess memoryAccess, const uint32_t size) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), sourceID(sourceID), memoryAccess(memoryAccess), sizeID(size)
{
}

void SPIRVCopy::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value source = getValue(sourceID, *method.method, types, constants, localTypes, localMapping);
    Value dest(UNDEFINED_VALUE);
    if(typeID == UNDEFINED_ID)
    {
        // globals may have other names than their ID, so check them first
        LocalMapping::iterator it;
        if(memoryAccess == MemoryAccess::READ)
            it = localMapping.find(sourceID);
        else
            it = localMapping.find(id);
        if(it != localMapping.end())
            dest = it->second->createReference(ANY_ELEMENT);
        else
        {
            dest = method.method->createLocal(source.type, std::string("%") + std::to_string(id))->createReference();
            localMapping.emplace(id, dest.local());
        }
    }
    else
        dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    if(auto builtin = dynamic_cast<SPIRVBuiltin*>(source.checkLocal()))
    {
        // this is a "load" from a built-in variable -> convert to move which will then be handled by
        // SPIRVBuiltins#lowerBuiltins
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating reading of " << builtin->to_string() << " into " << dest.to_string() << logging::endl);
        method.method->appendToEnd(
            (new intermediate::IntrinsicOperation(std::string{BUILTIN_INTRINSIC}, std::move(dest), std::move(source)))
                ->addDecorations(decorations));
    }
    else if(memoryAccess != MemoryAccess::NONE)
    {
        // FIXME can't handle I/O of complex types, e.g. array (bigger than 16 elements), see
        // JohnTheRipper/DES_bs_kernel.cl  need to split in I/O of scalar type (use VPM cache, multi-line VPM)
        if(memoryAccess == MemoryAccess::READ)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Generating reading of " << source.to_string() << " into " << dest.to_string() << logging::endl);
            method.method->appendToEnd((new intermediate::MemoryInstruction(
                                            intermediate::MemoryOperation::READ, std::move(dest), std::move(source)))
                                           ->addDecorations(decorations));
        }
        else if(memoryAccess == MemoryAccess::WRITE)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Generating writing of " << source.to_string() << " into " << dest.to_string() << logging::endl);
            method.method->appendToEnd((new intermediate::MemoryInstruction(
                                            intermediate::MemoryOperation::WRITE, std::move(dest), std::move(source)))
                                           ->addDecorations(decorations));
        }
        else if(memoryAccess == MemoryAccess::READ_WRITE)
        {
            if(sizeID.value() == UNDEFINED_ID)
            {
                // copy single object
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Generating copying of " << source.to_string() << " into " << dest.to_string()
                        << logging::endl);
                method.method->appendToEnd((new intermediate::MemoryInstruction(intermediate::MemoryOperation::COPY,
                                                std::move(dest), std::move(source)))
                                               ->addDecorations(decorations));
            }
            else
            {
                // copy area of memory
                Value size = getValue(sizeID.value(), *method.method, types, constants, localTypes, localMapping);
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Generating copying of " << size.to_string() << " bytes from " << source.to_string()
                        << " into " << dest.to_string() << logging::endl);
                if(size.getLiteralValue())
                {
                    method.method->appendToEnd((new intermediate::MemoryInstruction(intermediate::MemoryOperation::COPY,
                                                    std::move(dest), std::move(source),
                                                    Value(Literal(size.getLiteralValue()->unsignedInt() /
                                                              (source.type.getElementType().getScalarBitCount() / 8)),
                                                        TYPE_INT32)))
                                                   ->addDecorations(decorations));
                }
                else if(source.type.getElementType() == TYPE_INT8)
                    // if the element-size is 1 byte, the number of elements is the byte-size to copy
                    method.method->appendToEnd((new intermediate::MemoryInstruction(intermediate::MemoryOperation::COPY,
                                                    std::move(dest), std::move(source), std::move(size)))
                                                   ->addDecorations(decorations));
                else
                    // TODO in any case, loop over copies, up to the size specified
                    throw CompilationError(CompilationStep::LLVM_2_IR,
                        "Copying dynamically sized memory is not yet implemented", size.to_string());
            }
        }
    }
    else
    {
        // simple move
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate move from " << source.to_string() << " into " << dest.to_string(true)
                << logging::endl);
        method.method->appendToEnd(
            (new intermediate::MoveOperation(std::move(dest), std::move(source)))->addDecorations(decorations));
    }
}

Optional<Value> SPIRVCopy::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    auto it = constants.find(sourceID);
    if(it != constants.end())
        return it->second.toValue();
    return NO_VALUE;
}

SPIRVInsertionExtraction::SPIRVInsertionExtraction(uint32_t id, SPIRVMethod& method, uint32_t resultType,
    uint32_t srcContainerId, uint32_t srcElementId, std::vector<uint32_t>&& indices, bool literalIndices) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::ELEMENT_INSERTION),
    typeID(resultType), containerId(srcContainerId), elementId(srcElementId), indices(std::move(indices)),
    indicesAreLiteral(literalIndices)
{
}

SPIRVInsertionExtraction::SPIRVInsertionExtraction(uint32_t id, SPIRVMethod& method, uint32_t resultType,
    uint32_t srcContainerId, std::vector<uint32_t>&& indices, bool literalIndices) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::ELEMENT_INSERTION),
    typeID(resultType), containerId(srcContainerId), indices(std::move(indices)), indicesAreLiteral(literalIndices)
{
}

void SPIRVInsertionExtraction::mapInstruction(TypeMapping& types, ConstantMapping& constants,
    LocalTypeMapping& localTypes, MethodMapping& methods, LocalMapping& localMapping)
{
    Value container = getValue(containerId, *method.method, types, constants, localTypes, localMapping);
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    auto element =
        elementId ? getValue(*elementId, *method.method, types, constants, localTypes, localMapping) : NO_VALUE;

    if(indices.size() != 1)
        throw CompilationError(CompilationStep::LLVM_2_IR, "Multi level indices are not implemented yet");

    auto index = indicesAreLiteral ? Value(Literal(indices[0]), TYPE_INT8) :
                                     getValue(indices[0], *method.method, types, constants, localTypes, localMapping);

    if(element) // we have source element -> insertions
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate insertion of " << element->to_string() << " into element "
                << index.to_string() << " of " << dest.to_string(true) << logging::endl);
        // 1. copy whole composite
        method.method->appendToEnd(new intermediate::MoveOperation(Value(dest), std::move(container)));
        // 2. insert object at given index
        ignoreReturnValue(
            intermediate::insertVectorInsertion(method.method->appendToEnd(), *method.method, dest, index, *element));
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate extraction of index " << index.to_string() << " from "
                << container.to_string() << " into " << dest.to_string(true) << logging::endl);
        ignoreReturnValue(
            intermediate::insertVectorExtraction(method.method->appendToEnd(), *method.method, container, index, dest));
    }
}

Optional<Value> SPIRVInsertionExtraction::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVShuffle::SPIRVShuffle(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID0,
    const uint32_t sourceID1, std::vector<uint32_t>&& indices) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), source0(sourceID0), source1(sourceID1), indices(std::move(indices)), compositeIndex(false)
{
}

SPIRVShuffle::SPIRVShuffle(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID0,
    const uint32_t sourceID1, const uint32_t compositeIndex) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), source0(sourceID0), source1(sourceID1), indices(1, compositeIndex), compositeIndex(true)
{
}

void SPIRVShuffle::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    // shuffling = iteration over all elements in both vectors and re-ordering in order given
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    Value src0 = getValue(source0, *method.method, types, constants, localTypes, localMapping);
    Value src1 = getValue(source1, *method.method, types, constants, localTypes, localMapping);
    Value index(UNDEFINED_VALUE);
    if(compositeIndex)
    {
        // there is just one index, which is a composite
        index = getValue(indices.at(0), *method.method, types, constants, localTypes, localMapping);
    }
    else // all indices are literal values
    {
        SIMDVector indices;
        bool allIndicesUndef = true;
        bool allIndicesZero = true;
        unsigned char numIndex = 0;
        for(const uint32_t index : this->indices)
        {
            //"A Component literal may also be FFFFFFFF, which means the corresponding result component has no source
            // and is undefined"
            if(index == UNDEFINED_SCALAR)
            {
                indices[numIndex] = UNDEFINED_LITERAL;
            }
            else
            {
                allIndicesUndef = false;
                if(index != 0 && index != UNDEFINED_SCALAR)
                    // accept UNDEF as zero, so i.e. (0,0,0,UNDEF) can be simplified as all-zero
                    allIndicesZero = false;
                indices[numIndex] = Literal(index);
            }
            ++numIndex;
        }

        if(allIndicesUndef)
            index = UNDEFINED_VALUE;
        else
            index = method.method->module.storeVector(std::move(indices), TYPE_INT8.toVectorType(numIndex));
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate operations for mixing " << src0.to_string() << " and " << src1.to_string()
            << " into " << dest.to_string() << " with mask " << index.to_string(false, true) << logging::endl);

    ignoreReturnValue(
        intermediate::insertVectorShuffle(method.method->appendToEnd(), *method.method, dest, src0, src1, index));
}

Optional<Value> SPIRVShuffle::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVIndexOf::SPIRVIndexOf(const uint32_t id, SPIRVMethod& method, const uint32_t resultType,
    const uint32_t containerID, std::vector<uint32_t>&& indices, const bool isPtrAcessChain) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), container(containerID), indices(std::move(indices)), isPtrAcessChain(isPtrAcessChain)
{
}

void SPIRVIndexOf::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    // need to get pointer/address -> reference to content
    // a[i] of type t is at position &a + i * sizeof(t)
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    Value container = getValue(this->container, *method.method, types, constants, localTypes, localMapping);

    std::vector<Value> indexValues;
    indexValues.reserve(indices.size());
    for(const uint32_t indexID : indices)
        indexValues.push_back(getValue(indexID, *method.method, types, constants, localTypes, localMapping));

    // TODO for some reason, now it passes a lot more tests with this set to false by default...
    bool ptrAccessChain = false; // FIXME isPtrAcessChain;

    // FIXME isn't there some proper way of doing this??!!
    // Looks like we need to invert the handling, by default don't treat first index as element and only for
    // exceptions?! -> more test passes for memory access tests
    // TODO for some tests (e.g. vload/vstore), this does not work -> need to set access chain
    // TODO which criteria?? If pointer to scalar/vector? Or enable for all, but disable for all pointer to
    // struct/array??

    // XXX work-around for problem matching resulting type to actual output type for vload3/vstore3
    // For some reason, SPIR-V does not introduce a pointer cast operation, but "casts" in the index chain. But this
    // casting does not happen at the end, but in between, and how are we supposed to know that?
    if(indexValues.size() == 2 && isPtrAcessChain &&
        dest.type != method.method->createPointerType(container.type.getElementType()) &&
        dest.type.getElementType().isSimpleType() && container.type.getElementType().isSimpleType() &&
        dest.type.getElementType() == container.type.getElementType().toVectorType(1))
    {
        // if we have an index-chain for a <n x type>* into a type* with element-flag set and 1 actual offset, we would
        // get: <n x type>* -> <n x type>* -> <n x type>, but we need <n x type>* -> <n x type> -> type, so just unmark
        // the element-flag to resolve the element-type of the first pointer too.
        ptrAccessChain = false;
    }

    // XXX work-around for global arrays which are represented as pointer to array. If now the first index is considered
    // the element, the second index is in multiple of the whole array size, but should address an array element.
    if(indexValues.size() == 2 && isPtrAcessChain && container.checkLocal() && container.local()->is<Global>() &&
        container.type.getElementType().getArrayType())
    {
        // if we have an index-chain for a type[n]* into a type* with element-flag set and 1 actual offset, we would
        // get: type[n]* -> type[n]* -> type[n], but we need type[n]* -> type[n] -> type, so just unmark the
        // element-flag to resolve the element-type of the first pointer too.
        ptrAccessChain = false;
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating calculating indices of " << container.to_string() << " into " << dest.to_string()
            << " with indices: " << to_string<Value>(indexValues) << (ptrAccessChain ? " (first index is element)" : "")
            << logging::endl);

    ignoreReturnValue(intermediate::insertCalculateIndices(
        method.method->appendToEnd(), *method.method.get(), container, dest, indexValues, ptrAccessChain));
}

Optional<Value> SPIRVIndexOf::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    Value container(UNDEFINED_VALUE);
    auto cit = constants.find(this->container);
    if(cit != constants.end())
    {
        if(auto val = cit->second.toValue())
            container = *val;
        else
            return NO_VALUE;
    }
    else if(memoryAllocated.find(this->container) != memoryAllocated.end())
        container = memoryAllocated.at(this->container)->createReference();
    else
    {
        logging::error() << this->container << " " << this->id << logging::endl;
        throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid constant container!");
    }

    std::vector<Value> indexValues;
    indexValues.reserve(indices.size());
    std::for_each(indices.begin(), indices.end(),
        [&indexValues, &constants](uint32_t index) { indexValues.push_back(*constants.at(index).toValue()); });

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Pre-calculating indices of " << container.to_string() << logging::endl);

    // TODO regard isPtrAcessChain, if set, type of first index is original type

    Value offset = INT_ZERO;
    DataType subContainerType = container.type;
    for(const Value& index : indexValues)
    {
        Value subOffset(UNDEFINED_VALUE);
        if(subContainerType.getPointerType() || subContainerType.getArrayType())
        {
            // index is index in pointer/array
            //-> add offset of element at given index to global offset
            if(index.getLiteralValue())
            {
                subOffset = Value(Literal(index.getLiteralValue()->signedInt() *
                                      static_cast<int>(subContainerType.getElementType().getInMemoryWidth())),
                    TYPE_INT32);
            }
            else
            {
                throw CompilationError(
                    CompilationStep::LLVM_2_IR, "Invalid index for constant expression", index.to_string());
            }

            subContainerType = subContainerType.getElementType();
        }
        else if(subContainerType.getStructType())
        {
            // index is element in struct -> MUST be literal
            if(!index.getLiteralValue())
                throw CompilationError(CompilationStep::LLVM_2_IR, "Can't access struct-element with non-literal index",
                    index.to_string());

            subOffset = Value(Literal(container.type.getStructType()->getStructSize(
                                  static_cast<int>(index.getLiteralValue()->unsignedInt()))),
                TYPE_INT32);
            subContainerType = subContainerType.getElementType(index.getLiteralValue()->signedInt());
        }
        else
            throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid container-type to retrieve element via index",
                subContainerType.to_string());

        if(offset.checkLiteral() && subOffset.getLiteralValue())
            offset.literal() = Literal(offset.literal().signedInt() + subOffset.getLiteralValue()->signedInt());
        else
            throw CompilationError(
                CompilationStep::LLVM_2_IR, "Invalid index for constant expression", offset.to_string());
    }

    return offset;
}

SPIRVPhi::SPIRVPhi(const uint32_t id, SPIRVMethod& method, const uint32_t resultType,
    std::vector<std::pair<uint32_t, uint32_t>>&& sources) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), sources(std::move(sources))
{
}

void SPIRVPhi::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating Phi-Node with " << sources.size() << " options into " << dest.to_string() << logging::endl);
    // https://stackoverflow.com/questions/11485531/what-exactly-phi-instruction-does-and-how-to-use-it-in-llvm#11485946
    // sets the output value according to where from this instructions is executed/jumped from
    std::vector<std::pair<Value, const Local*>> labelPairs;
    labelPairs.reserve(sources.size());
    for(const std::pair<uint32_t, uint32_t>& option : sources)
    {
        const Value source = getValue(option.second, *method.method, types, constants, localTypes, localMapping);
        const Value val = getValue(option.first, *method.method, types, constants, localTypes, localMapping);
        labelPairs.emplace_back(val, source.local());
    }
    method.method->appendToEnd(new intermediate::PhiNode(std::move(dest), std::move(labelPairs)));
}

Optional<Value> SPIRVPhi::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVSelect::SPIRVSelect(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t conditionID,
    const uint32_t trueObj, const uint32_t falseObj) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), condID(conditionID), trueID(trueObj), falseID(falseObj)
{
}

void SPIRVSelect::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    const Value sourceTrue = getValue(trueID, *method.method, types, constants, localTypes, localMapping);
    const Value sourceFalse = getValue(falseID, *method.method, types, constants, localTypes, localMapping);
    const Value condition = getValue(condID, *method.method, types, constants, localTypes, localMapping);
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate select on " << condition.to_string() << " whether to write "
            << sourceTrue.to_string() << " or " << sourceFalse.to_string() << " into " << dest.to_string(true)
            << logging::endl);

    if(condition.type.isScalarType() && (!sourceTrue.type.isScalarType() || !sourceFalse.type.isScalarType()))
    {
        // if a vector is selected on a scalar value, the whole vector needs to be selected -> replicate the condition
        // to all elements
        auto it = intermediate::insertReplication(method.method->appendToEnd(), condition, NOP_REGISTER, true);
        it.previousInBlock()->setFlags = SetFlag::SET_FLAGS;
    }
    else
        method.method->appendToEnd(
            new intermediate::MoveOperation(NOP_REGISTER, condition, COND_ALWAYS, SetFlag::SET_FLAGS));

    method.method->appendToEnd(new intermediate::MoveOperation(dest, sourceTrue, COND_ZERO_CLEAR));
    method.method->appendToEnd(new intermediate::MoveOperation(dest, sourceFalse, COND_ZERO_SET));
}

Optional<Value> SPIRVSelect::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    auto it = constants.find(condID);
    if(it != constants.end())
    {
        if(it->second.getScalar()->isTrue() && constants.find(trueID) != constants.end())
        {
            return constants.at(trueID).toValue();
        }
        if(!it->second.getScalar()->isTrue() && constants.find(falseID) != constants.end())
        {
            return constants.at(falseID).toValue();
        }
    }
    return NO_VALUE;
}

SPIRVSwitch::SPIRVSwitch(const uint32_t id, SPIRVMethod& method, const uint32_t selectorID, const uint32_t defaultID,
    std::vector<std::pair<uint32_t, uint32_t>>&& destinations) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    selectorID(selectorID), defaultID(defaultID), destinations(std::move(destinations))
{
}

void SPIRVSwitch::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    const Value selector = getValue(selectorID, *method.method, types, constants, localTypes, localMapping);
    const Value defaultLabel = getValue(defaultID, *method.method, types, constants, localTypes, localMapping);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate switched jump on " << selector.to_string() << " to " << destinations.size()
            << " destinations with default " << defaultLabel.to_string() << logging::endl);

    for(const auto& pair : destinations)
    {
        // comparison value is a literal
        Value comparison(Literal(pair.first), selector.type);
        const Value destination = getValue(pair.second, *method.method, types, constants, localTypes, localMapping);
        // for every case, if equal,branch to given label
        const Value tmp = method.method->addNewLocal(TYPE_BOOL, "%switch");
        method.method->appendToEnd(
            new intermediate::Comparison(intermediate::COMP_EQ, Value(tmp), Value(selector), std::move(comparison)));
        method.method->appendToEnd(new intermediate::Branch(destination.local(), COND_ZERO_CLEAR, tmp));
    }
    // branch default label
    method.method->appendToEnd(new intermediate::Branch(defaultLabel.local(), COND_ALWAYS, BOOL_TRUE));
}

Optional<Value> SPIRVSwitch::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    auto it = constants.find(selectorID);
    if(it != constants.end())
    {
        auto val = it->second.toValue();
        if(!val)
            return NO_VALUE;
        const Value& selector = *val;
        for(const auto& pair : destinations)
        {
            it = constants.find(pair.second);
            if(selector.hasLiteral(Literal(pair.first)) && it != constants.end())
            {
                return it->second.toValue();
            }
        }
    }
    return NO_VALUE;
}

SPIRVImageQuery::SPIRVImageQuery(const uint32_t id, SPIRVMethod& method, const uint32_t resultType,
    const ImageQuery value, const uint32_t imageID, const uint32_t lodOrCoordinate) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), valueID(value), imageID(imageID), lodOrCoordinate(lodOrCoordinate)
{
}

void SPIRVImageQuery::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    const Value image = getValue(imageID, *method.method, types, constants, localTypes, localMapping);
    Value param(UNDEFINED_VALUE);
    if(lodOrCoordinate != UNDEFINED_ID)
    {
        param = getValue(lodOrCoordinate, *method.method, types, constants, localTypes, localMapping);
    }

    switch(valueID)
    {
    case ImageQuery::CHANNEL_DATA_TYPE:
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating query of image's channel data-type for image: " << image.to_string() << logging::endl);
        ignoreReturnValue(
            intermediate::insertQueryChannelDataType(method.method->appendToEnd(), *method.method, image, dest));
        return;
    case ImageQuery::CHANNEL_ORDER:
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating query of image's channel order for image: " << image.to_string() << logging::endl);
        ignoreReturnValue(
            intermediate::insertQueryChannelOrder(method.method->appendToEnd(), *method.method, image, dest));
        return;
    case ImageQuery::SIZES:
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating query of image's measurements for image: " << image.to_string() << logging::endl);
        ignoreReturnValue(
            intermediate::insertQueryMeasurements(method.method->appendToEnd(), *method.method, image, dest));
        return;
    case ImageQuery::SIZES_LOD:
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating query of image's measurements for image with LOD: " << image.to_string()
                << logging::endl);
        if(param.hasLiteral(INT_ZERO.literal()))
        {
            // same as above
            ignoreReturnValue(
                intermediate::insertQueryMeasurements(method.method->appendToEnd(), *method.method, image, dest));
        }
        else
            throw CompilationError(CompilationStep::LLVM_2_IR, "Images with LOD are not supported", image.to_string());
        return;
    case ImageQuery::MIPMAP_LEVELS:
        // mipmaps are not yet supported in OpenCL 1.2
        throw CompilationError(CompilationStep::LLVM_2_IR, "Mipmap levels are not yet supported by OpenCL 1.2");
    case ImageQuery::SAMPLES_PER_TEXEL:
        // multi-sample images are not yet supported in OpenCL 1.2
        throw CompilationError(CompilationStep::LLVM_2_IR, "Multi-sample images are not yet supported by OpenCL 1.2");
    }
}

Optional<Value> SPIRVImageQuery::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

vc4c::spirv::SPIRVMemoryBarrier::SPIRVMemoryBarrier(
    SPIRVMethod& method, const uint32_t scopeID, const uint32_t semanticsID) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE),
    scopeID(scopeID), semanticsID(semanticsID)
{
}

void vc4c::spirv::SPIRVMemoryBarrier::mapInstruction(TypeMapping& types, ConstantMapping& constants,
    LocalTypeMapping& localTypes, MethodMapping& methods, LocalMapping& localMapping)
{
    const Value scope = getValue(scopeID, *method.method, types, constants, localTypes, localMapping);
    const Value semantics = getValue(semanticsID, *method.method, types, constants, localTypes, localMapping);

    if(!scope.getLiteralValue() || !semantics.getLiteralValue())
        throw CompilationError(CompilationStep::LLVM_2_IR,
            "Memory barriers with non-constant scope or memory semantics are not supported!");

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating memory barrier" << logging::endl);
    method.method->appendToEnd(
        new intermediate::MemoryBarrier(static_cast<intermediate::MemoryScope>(scope.getLiteralValue()->unsignedInt()),
            static_cast<intermediate::MemorySemantics>(semantics.getLiteralValue()->unsignedInt())));
}

Optional<Value> vc4c::spirv::SPIRVMemoryBarrier::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVLifetimeInstruction::SPIRVLifetimeInstruction(const uint32_t id, SPIRVMethod& method, uint32_t size,
    bool lifetimeEnd, const intermediate::InstructionDecorations decorations) :
    SPIRVOperation(id, method, decorations),
    sizeInBytes(size), isLifetimeEnd(lifetimeEnd)
{
}

void SPIRVLifetimeInstruction::mapInstruction(TypeMapping& types, ConstantMapping& constants,
    LocalTypeMapping& localTypes, MethodMapping& methods, LocalMapping& localMapping)
{
    Value pointer = getValue(id, *method.method, types, constants, localTypes, localMapping);

    if(pointer.checkLocal())
    {
        if(auto alloc = const_cast<StackAllocation*>(pointer.local()->getBase(true)->as<StackAllocation>()))
            pointer = alloc->createReference();
    }

    //"If Size is non-zero, it is the number of bytes of memory whose lifetime is starting"
    if(sizeInBytes != 0 && pointer.checkLocal())
    {
        if(auto alloc = pointer.local()->as<StackAllocation>())
            alloc->size = std::max(alloc->size, static_cast<std::size_t>(sizeInBytes));
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating life-time " << (isLifetimeEnd ? "end" : "start") << " for " << pointer.to_string()
            << (sizeInBytes != 0 ? " with size of " + std::to_string(sizeInBytes) + " bytes" : "") << logging::endl);
    method.method->appendToEnd(new intermediate::LifetimeBoundary(pointer, isLifetimeEnd));
}

Optional<Value> SPIRVLifetimeInstruction::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVFoldInstruction::SPIRVFoldInstruction(uint32_t id, SPIRVMethod& method, uint32_t resultType,
    const std::string& foldOperation, uint32_t sourceID, intermediate::InstructionDecorations decorations) :
    SPIRVOperation(id, method, decorations),
    typeID(resultType), sourceID(sourceID), foldOperation(foldOperation)
{
}

void SPIRVFoldInstruction::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, LocalMapping& localMapping)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes, localMapping);
    Value src = getValue(sourceID, *method.method, types, constants, localTypes, localMapping);
    auto code = OpCode::toOpCode(foldOperation);
    ignoreReturnValue(
        intermediate::insertFoldVector(method.method->appendToEnd(), *method.method, dest, src, code, decorations));
}

Optional<Value> SPIRVFoldInstruction::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const LocalMapping& memoryAllocated) const
{
    // XXX could implement for all-constant operand
    return NO_VALUE;
}

#endif
