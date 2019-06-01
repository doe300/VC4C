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
#include "log.h"

#include <algorithm>
#include <cstdbool>

using namespace vc4c;
using namespace vc4c::spirv2qasm;

static Value toNewLocal(Method& method, const uint32_t id, const uint32_t typeID, const TypeMapping& typeMappings,
    LocalTypeMapping& localTypes)
{
    localTypes[id] = typeID;
    return method.findOrCreateLocal(typeMappings.at(typeID), std::string("%") + std::to_string(id))->createReference();
}

static DataType getType(const uint32_t id, const TypeMapping& types, const ConstantMapping& constants,
    const AllocationMapping& memoryAllocated, const LocalTypeMapping& localTypes)
{
    auto tit = types.find(id);
    if(tit != types.end())
        return tit->second;
    auto cit = constants.find(id);
    if(cit != constants.end())
        return cit->second.type;
    auto mit = memoryAllocated.find(id);
    if(mit != memoryAllocated.end())
        return mit->second->type;
    return types.at(localTypes.at(id));
}

static Value getValue(const uint32_t id, Method& method, const TypeMapping& types, const ConstantMapping& constants,
    const AllocationMapping& memoryAllocated, const LocalTypeMapping& localTypes)
{
    if(id == UNDEFINED_ID)
        return UNDEFINED_VALUE;
    auto cit = constants.find(id);
    if(cit != constants.end())
        return cit->second.toValue().value_or(UNDEFINED_VALUE);
    auto mit = memoryAllocated.find(id);
    if(mit != memoryAllocated.end())
        return mit->second->createReference();
    return method
        .findOrCreateLocal(
            getType(id, types, constants, memoryAllocated, localTypes), std::string("%") + std::to_string(id))
        ->createReference();
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    Value arg0 = getValue(operands.at(0), *method.method, types, constants, memoryAllocated, localTypes);
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
        arg1 = getValue(operands[1], *method.method, types, constants, memoryAllocated, localTypes);
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
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
        return Value(intermediate::srem(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "smod")
        return Value(intermediate::smod(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "frem")
        return Value(intermediate::frem(op1.type, op1.literal(), op2.literal()), op1.type);
    if(opcode == "fmod")
        return Value(intermediate::fmod(op1.type, op1.literal(), op2.literal()), op1.type);
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
        return Value(intermediate::asr(op1.literal(), op2.literal()), op1.type);
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    Value arg0 = getValue(operands.at(0), *method.method, types, constants, memoryAllocated, localTypes);
    Value arg1 = getValue(operands.at(1), *method.method, types, constants, memoryAllocated, localTypes);
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate comparison '" << opcode << "' of " << arg0.to_string(false) << " and "
            << arg1.to_string(false) << " into " << dest.to_string(true) << logging::endl);
    method.method->appendToEnd(
        (new intermediate::Comparison(std::move(opcode), std::move(dest), std::move(arg0), std::move(arg1)))
            ->addDecorations(decorations));
}

Optional<Value> SPIRVComparison::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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

void SPIRVCallSite::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    std::string calledFunction = methodName.value_or("");
    if(methodID)
        calledFunction = methods.at(methodID.value()).method->name;
    std::vector<Value> args;
    for(const uint32_t op : arguments)
    {
        args.push_back(getValue(op, *method.method, types, constants, memoryAllocated, localTypes));
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate call-site to '" << calledFunction << "' with " << args.size()
            << " parameters into " << dest.to_string(true) << logging::endl);
    method.method->appendToEnd(
        (new intermediate::MethodCall(std::move(dest), std::move(calledFunction), std::move(args)))
            ->addDecorations(decorations));
}

Optional<Value> SPIRVCallSite::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
{
    return NO_VALUE;
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    if(returnValue)
    {
        Value value = getValue(returnValue.value(), *method.method, types, constants, memoryAllocated, localTypes);
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
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    if(conditionID)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate conditional branch on %" << conditionID.value() << " to either %"
                << defaultLabelID << " or %" << falseLabelID.value() << logging::endl);
        const Value cond = getValue(conditionID.value(), *method.method, types, constants, memoryAllocated, localTypes);
        const Local* trueLabel =
            method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(defaultLabelID));
        const Local* falseLabel =
            method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(falseLabelID.value()));
        method.method->appendToEnd(new intermediate::Branch(trueLabel, COND_ZERO_CLEAR /* condition is true */, cond));
        method.method->appendToEnd(new intermediate::Branch(falseLabel, COND_ZERO_SET /* condition is false */, cond));
    }
    else
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Generating intermediate branch to %" << defaultLabelID << logging::endl);
        const Local* label =
            method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(defaultLabelID));
        method.method->appendToEnd(new intermediate::Branch(label, COND_ALWAYS, BOOL_TRUE));
    }
}

Optional<Value> SPIRVBranch::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVLabel::SPIRVLabel(const uint32_t id, SPIRVMethod& method) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE)
{
}

void SPIRVLabel::mapInstruction(std::map<uint32_t, DataType>& types, ConstantMapping& constants,
    std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods,
    std::map<uint32_t, Local*>& memoryAllocated)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating intermediate label %" << id << logging::endl);
    method.method->appendToEnd(new intermediate::BranchLabel(
        *method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(id))));
}

Optional<Value> SPIRVLabel::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
{
    return NO_VALUE;
}

SPIRVConversion::SPIRVConversion(const uint32_t id, SPIRVMethod& method, const uint32_t resultType,
    const uint32_t sourceID, const ConversionType type, const intermediate::InstructionDecorations decorations,
    bool isSaturated) :
    SPIRVOperation(id, method, decorations),
    typeID(resultType), sourceID(sourceID), type(type), isSaturated(isSaturated)
{
}

void SPIRVConversion::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    Value source = getValue(sourceID, *method.method, types, constants, memoryAllocated, localTypes);
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const uint8_t sourceWidth = source.type.getScalarBitCount();
    const uint8_t destWidth = dest.type.getScalarBitCount();

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate conversion from " << source.to_string(false) << " to " << dest.to_string(true)
            << logging::endl);
    switch(type)
    {
    case ConversionType::BITCAST:
        ignoreReturnValue(
            intermediate::insertBitcast(method.method->appendToEnd(), *method.method.get(), source, dest, decorations));
        break;
    case ConversionType::FLOATING:
        method.method->appendToEnd((new intermediate::IntrinsicOperation("fptrunc", std::move(dest), std::move(source)))
                                       ->addDecorations(decorations));
        break;
    case ConversionType::SIGNED:
        if(isSaturated)
            ignoreReturnValue(
                intermediate::insertSaturation(method.method->appendToEnd(), *method.method.get(), source, dest, true));
        if(sourceWidth < destWidth)
            method.method->appendToEnd(
                (new intermediate::IntrinsicOperation("sext", std::move(dest), std::move(source)))
                    ->addDecorations(decorations));
        else
            // for |dest| > |source|, we do nothing (just move), since truncating would cut off the leading 1-bits for
            // negative numbers  and since the ALU only calculates 32-bit operations, we need 32-bit negative numbers
            // TODO completely correct? Since we do not truncate out-of-bounds values! (Same for bitcast-intrinsics)
            method.method->appendToEnd(
                (new intermediate::MoveOperation(std::move(dest), std::move(source)))->addDecorations(decorations));
        break;
    case ConversionType::UNSIGNED:
        if(isSaturated)
            ignoreReturnValue(intermediate::insertSaturation(
                method.method->appendToEnd(), *method.method.get(), source, dest, false));
        else if(sourceWidth > destWidth)
            method.method->appendToEnd(
                (new intermediate::IntrinsicOperation("trunc", std::move(dest), std::move(source)))
                    ->addDecorations(decorations));
        else if(sourceWidth == destWidth)
            method.method->appendToEnd((new intermediate::MoveOperation(dest, source))->addDecorations(decorations));
        else // |source| < |dest|
            method.method->appendToEnd(
                (new intermediate::IntrinsicOperation("zext", std::move(dest), std::move(source)))
                    ->addDecorations(add_flag(decorations, intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        break;
    }
}

Optional<Value> SPIRVConversion::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
        case ConversionType::SIGNED:
            // TODO trunc/sext + saturation
            break;
        case ConversionType::UNSIGNED:
            // TODO trunc/zext + saturation
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

SPIRVCopy::SPIRVCopy(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID,
    std::vector<uint32_t>&& destIndices, std::vector<uint32_t>&& sourceIndices) :
    SPIRVOperation(id, method, intermediate::InstructionDecorations::NONE),
    typeID(resultType), sourceID(sourceID), memoryAccess(MemoryAccess::NONE), destIndices(std::move(destIndices)),
    sourceIndices(std::move(sourceIndices))
{
}

void SPIRVCopy::mapInstruction(TypeMapping& types, ConstantMapping& constants, LocalTypeMapping& localTypes,
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    Value source = getValue(sourceID, *method.method, types, constants, memoryAllocated, localTypes);
    Value dest(UNDEFINED_VALUE);
    if(typeID == UNDEFINED_ID)
    {
        // globals may have other names than their ID, so check them first
        AllocationMapping::iterator it;
        if(memoryAccess == MemoryAccess::READ)
            it = memoryAllocated.find(sourceID);
        else
            it = memoryAllocated.find(id);
        if(it != memoryAllocated.end())
            dest = it->second->createReference(destIndices && !destIndices->empty() ? destIndices->at(0) : ANY_ELEMENT);
        else
            dest =
                method.method->findOrCreateLocal(source.type, std::string("%") + std::to_string(id))->createReference();
    }
    else
        dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    if(memoryAccess != MemoryAccess::NONE)
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
                Value size = getValue(sizeID.value(), *method.method, types, constants, memoryAllocated, localTypes);
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
    else if(!destIndices && !sourceIndices)
    {
        // simple move
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate move from " << source.to_string() << " into " << dest.to_string(true)
                << logging::endl);
        method.method->appendToEnd(
            (new intermediate::MoveOperation(std::move(dest), std::move(source)))->addDecorations(decorations));
    }
    else if(sourceIndices && dest.type.isScalarType())
    {
        if(sourceIndices->size() > 1)
            throw CompilationError(CompilationStep::LLVM_2_IR, "Multi level indices are not implemented yet");
        // index is literal
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate extraction of index " << sourceIndices->at(0) << " from "
                << source.to_string() << " into " << dest.to_string(true) << logging::endl);
        ignoreReturnValue(intermediate::insertVectorExtraction(method.method->appendToEnd(), *method.method, source,
            Value(Literal(sourceIndices->at(0)), TYPE_INT8), dest));
    }
    else if((!sourceIndices || (sourceIndices->at(0) == 0)) && destIndices)
    {
        if(destIndices->size() > 1)
            throw CompilationError(CompilationStep::LLVM_2_IR, "Multi level indices are not implemented yet");
        // add element to vector to element
        // index is literal
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating intermediate insertion of " << source.to_string() << " into element "
                << destIndices->at(0) << " of " << dest.to_string(true) << logging::endl);
        ignoreReturnValue(intermediate::insertVectorInsertion(
            method.method->appendToEnd(), *method.method, dest, Value(Literal(destIndices->at(0)), TYPE_INT8), source));
    }
    else
    {
        throw std::runtime_error("This version of copy is not implemented yet!");
        // TODO indices are already literals!!
    }
}

Optional<Value> SPIRVCopy::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
{
    auto it = constants.find(sourceID);
    if(it != constants.end())
        return it->second.toValue();
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    // shuffling = iteration over all elements in both vectors and re-ordering in order given
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    Value src0 = getValue(source0, *method.method, types, constants, memoryAllocated, localTypes);
    Value src1 = getValue(source1, *method.method, types, constants, memoryAllocated, localTypes);
    Value index(UNDEFINED_VALUE);
    if(compositeIndex)
    {
        // there is just one index, which is a composite
        index = getValue(indices.at(0), *method.method, types, constants, memoryAllocated, localTypes);
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
        {
            index = Value(std::move(indices), TYPE_INT8);
        }
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate operations for mixing " << src0.to_string() << " and " << src1.to_string()
            << " into " << dest.to_string() << " with mask " << index.to_string(false, true) << logging::endl);

    ignoreReturnValue(
        intermediate::insertVectorShuffle(method.method->appendToEnd(), *method.method, dest, src0, src1, index));
}

Optional<Value> SPIRVShuffle::precalculate(const std::map<uint32_t, DataType>& types, const ConstantMapping& constants,
    const std::map<uint32_t, Local*>& memoryAllocated) const
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    // need to get pointer/address -> reference to content
    // a[i] of type t is at position &a + i * sizeof(t)
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    Value container = getValue(this->container, *method.method, types, constants, memoryAllocated, localTypes);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating calculating indices of " << container.to_string() << " into " << dest.to_string()
            << logging::endl);
    std::vector<Value> indexValues;
    indexValues.reserve(indices.size());
    for(const uint32_t indexID : indices)
    {
        indexValues.push_back(getValue(indexID, *method.method, types, constants, memoryAllocated, localTypes));
    }

    ignoreReturnValue(intermediate::insertCalculateIndices(
        method.method->appendToEnd(), *method.method.get(), container, dest, indexValues, isPtrAcessChain));
}

Optional<Value> SPIRVIndexOf::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
                                      subContainerType.getElementType().getInMemoryWidth()),
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

            subOffset =
                Value(Literal(container.type.getStructType()->getStructSize(index.getLiteralValue()->unsignedInt())),
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating Phi-Node with " << sources.size() << " options into " << dest.to_string() << logging::endl);
    // https://stackoverflow.com/questions/11485531/what-exactly-phi-instruction-does-and-how-to-use-it-in-llvm#11485946
    // sets the output value according to where from this instructions is executed/jumped from
    std::vector<std::pair<Value, const Local*>> labelPairs;
    labelPairs.reserve(sources.size());
    for(const std::pair<uint32_t, uint32_t>& option : sources)
    {
        const Value source = getValue(option.second, *method.method, types, constants, memoryAllocated, localTypes);
        const Value val = getValue(option.first, *method.method, types, constants, memoryAllocated, localTypes);
        labelPairs.emplace_back(val, source.local());
    }
    method.method->appendToEnd(new intermediate::PhiNode(std::move(dest), std::move(labelPairs)));
}

Optional<Value> SPIRVPhi::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    const Value sourceTrue = getValue(trueID, *method.method, types, constants, memoryAllocated, localTypes);
    const Value sourceFalse = getValue(falseID, *method.method, types, constants, memoryAllocated, localTypes);
    const Value condition = getValue(condID, *method.method, types, constants, memoryAllocated, localTypes);
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);

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

Optional<Value> SPIRVSelect::precalculate(const std::map<uint32_t, DataType>& types, const ConstantMapping& constants,
    const std::map<uint32_t, Local*>& memoryAllocated) const
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    const Value selector = getValue(selectorID, *method.method, types, constants, memoryAllocated, localTypes);
    const Value defaultLabel = getValue(defaultID, *method.method, types, constants, memoryAllocated, localTypes);

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating intermediate switched jump on " << selector.to_string() << " to " << destinations.size()
            << " destinations with default " << defaultLabel.to_string() << logging::endl);

    for(const auto& pair : destinations)
    {
        // comparison value is a literal
        Value comparison(Literal(pair.first), selector.type);
        const Value destination = getValue(pair.second, *method.method, types, constants, memoryAllocated, localTypes);
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
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
    MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const Value image = getValue(imageID, *method.method, types, constants, memoryAllocated, localTypes);
    Value param(UNDEFINED_VALUE);
    if(lodOrCoordinate != UNDEFINED_ID)
    {
        param = getValue(lodOrCoordinate, *method.method, types, constants, memoryAllocated, localTypes);
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
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
{
    return NO_VALUE;
}

vc4c::spirv2qasm::SPIRVMemoryBarrier::SPIRVMemoryBarrier(
    SPIRVMethod& method, const uint32_t scopeID, const uint32_t semanticsID) :
    SPIRVOperation(UNDEFINED_ID, method, intermediate::InstructionDecorations::NONE),
    scopeID(scopeID), semanticsID(semanticsID)
{
}

void vc4c::spirv2qasm::SPIRVMemoryBarrier::mapInstruction(TypeMapping& types, ConstantMapping& constants,
    LocalTypeMapping& localTypes, MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    const Value scope = getValue(scopeID, *method.method, types, constants, memoryAllocated, localTypes);
    const Value semantics = getValue(semanticsID, *method.method, types, constants, memoryAllocated, localTypes);

    if(!scope.getLiteralValue() || !semantics.getLiteralValue())
        throw CompilationError(CompilationStep::LLVM_2_IR,
            "Memory barriers with non-constant scope or memory semantics are not supported!");

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating memory barrier" << logging::endl);
    method.method->appendToEnd(
        new intermediate::MemoryBarrier(static_cast<intermediate::MemoryScope>(scope.getLiteralValue()->unsignedInt()),
            static_cast<intermediate::MemorySemantics>(semantics.getLiteralValue()->unsignedInt())));
}

Optional<Value> vc4c::spirv2qasm::SPIRVMemoryBarrier::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
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
    LocalTypeMapping& localTypes, MethodMapping& methods, AllocationMapping& memoryAllocated)
{
    const Value pointer = getValue(id, *method.method, types, constants, memoryAllocated, localTypes);

    //"If Size is non-zero, it is the number of bytes of memory whose lifetime is starting"
    if(sizeInBytes != 0)
        pointer.local()->as<StackAllocation>()->size = sizeInBytes;

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating life-time " << (isLifetimeEnd ? "end" : "start") << " for " << pointer.to_string()
            << logging::endl);
    method.method->appendToEnd(new intermediate::LifetimeBoundary(pointer, isLifetimeEnd));
}

Optional<Value> SPIRVLifetimeInstruction::precalculate(
    const TypeMapping& types, const ConstantMapping& constants, const AllocationMapping& memoryAllocated) const
{
    return NO_VALUE;
}

#endif
