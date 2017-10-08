/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <stdbool.h>
#include <algorithm>

#include "SPIRVOperation.h"
#ifdef SPIRV_HEADER

#include "../intermediate/Helper.h"
#include "../periphery/VPM.h"
#include "log.h"
#include "../intrinsics/Images.h"
#include "helper.h"

using namespace vc4c;
using namespace vc4c::spirv2qasm;

Value toNewLocal(Method& method, const uint32_t id, const uint32_t typeID, const std::map<uint32_t, DataType>& typeMappings, std::map<uint32_t, uint32_t>& localTypes)
{
    localTypes[id] = typeID;
    return method.findOrCreateLocal(typeMappings.at(typeID), std::string("%") + std::to_string(id))->createReference();
}

DataType getType(const uint32_t id, const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals, const std::map<uint32_t, uint32_t>& localTypes)
{
    if(types.find(id) != types.end())
        return types.at(id);
    if(constants.find(id) != constants.end())
        return constants.at(id).type;
    if(globals.find(id) != globals.end())
        return globals.at(id)->type;
    return types.at(localTypes.at(id));
}

Value getValue(const uint32_t id, Method& method, const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals, const std::map<uint32_t, uint32_t>& localTypes)
{
    if(constants.find(id) != constants.end())
        return constants.at(id);
    if(globals.find(id) != globals.end())
        return globals.at(id)->createReference();
    return method.findOrCreateLocal(getType(id, types, constants, globals, localTypes), std::string("%") + std::to_string(id))->createReference();
}

SPIRVOperation::SPIRVOperation(const uint32_t id, SPIRVMethod& method, const intermediate::InstructionDecorations decorations) : id(id), method(method), decorations(decorations)
{

}

SPIRVOperation::~SPIRVOperation()
{

}

SPIRVInstruction::SPIRVInstruction(const uint32_t id, SPIRVMethod& method, const std::string& opcode, const uint32_t resultType, const std::vector<uint32_t>& operands, const intermediate::InstructionDecorations decorations) :
SPIRVOperation(id, method, decorations), typeID(resultType), opcode(opcode), operands(operands)
{
}

SPIRVInstruction::~SPIRVInstruction()
{

}

void SPIRVInstruction::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    Value arg0 = getValue(operands.at(0), *method.method, types, constants, globals, localTypes);
    Optional<Value> arg1(NO_VALUE);
    std::string opCode = opcode;
    if(OP_NEGATE.compare(opcode) == 0)
    {
        opCode = dest.type.isFloatingType() ? "fsub" : "sub";
        arg1 = arg0;
        arg0 = INT_ZERO;
    }
    else if(operands.size() > 1)
    {
        arg1 = getValue(operands.at(1), *method.method, types, constants, globals, localTypes);
    }
    if(!arg1)   //unary
    {
        logging::debug() << "Generating intermediate unary operation '" << opcode << "' with " << arg0.to_string(false) << " into " << dest.to_string(true) << logging::endl;
        method.method->appendToEnd((new intermediate::Operation(opCode, dest, arg0))->setDecorations(decorations));
    }
    else    //binary
    {
        logging::debug() << "Generating intermediate binary operation '" << opcode << "' with " << arg0.to_string(false) << " and " << arg1.to_string() << " into " << dest.to_string(true) << logging::endl;
        method.method->appendToEnd((new intermediate::Operation(opCode, dest, arg0, arg1))->setDecorations(decorations));
    }
}

Optional<Value> SPIRVInstruction::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	const Value op1 = constants.at(operands.at(0));
	const Value op2 = operands.size() > 1 ? constants.at(operands.at(1)) : UNDEFINED_VALUE;

	if(opcode.compare("fptoui") == 0)
		return Value(Literal(static_cast<unsigned long>(op1.literal.real)), TYPE_INT32);
	if(opcode.compare("fptosi") == 0)
		return Value(Literal(static_cast<long>(op1.literal.real)), TYPE_INT32);
	if(opcode.compare("sitofp") == 0)
		return Value(Literal(static_cast<double>(op1.literal.integer)), TYPE_FLOAT);
	if(opcode.compare("uitofp") == 0)
		return Value(Literal(static_cast<double>(bit_cast<long, unsigned long>(op1.literal.integer))), TYPE_FLOAT);
	if(opcode.compare(OP_NEGATE) == 0)
		return op1.type.isFloatingType() ? Value(Literal(-op1.literal.real), TYPE_FLOAT) : Value(Literal(-op1.literal.integer), TYPE_INT32);
	if(opcode.compare("add") == 0)
		return Value(Literal(op1.literal.integer + op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("fadd") == 0)
		return Value(Literal(op1.literal.real + op2.literal.real), op1.type.getUnionType(op2.type));
	if(opcode.compare("sub") == 0)
		return Value(Literal(op1.literal.integer - op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("fsub") == 0)
		return Value(Literal(op1.literal.real - op2.literal.real), op1.type.getUnionType(op2.type));
	if(opcode.compare("mul") == 0)
		return Value(Literal(op1.literal.integer * op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("fmul") == 0)
		return Value(Literal(op1.literal.real * op2.literal.real), op1.type.getUnionType(op2.type));
	if(opcode.compare("udiv") == 0)
		return Value(Literal(bit_cast<long, unsigned long>(op1.literal.integer) / bit_cast<long, unsigned long>(op2.literal.integer)), op1.type.getUnionType(op2.type));
	if(opcode.compare("sdiv") == 0)
		return Value(Literal(op1.literal.integer / op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("fdiv") == 0)
		return Value(Literal(op1.literal.real / op2.literal.real), op1.type.getUnionType(op2.type));
	if(opcode.compare("umod") == 0)
		return Value(Literal(bit_cast<long, unsigned long>(op1.literal.integer) % bit_cast<long, unsigned long>(op2.literal.integer)), op1.type.getUnionType(op2.type));
	//TODO srem, smod, frem, fmod
	if(opcode.compare("or") == 0)
		return Value(Literal(op1.literal.integer | op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("and") == 0)
		return Value(Literal(op1.literal.integer & op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("xor") == 0)
		return Value(Literal(op1.literal.integer ^ op2.literal.integer), op1.type.getUnionType(op2.type));
	if(opcode.compare("not") == 0)
		return Value(Literal(~op1.literal.integer), op1.type);
	//TODO shr, asr
	if(opcode.compare("shl") == 0)
		return Value(Literal(op1.literal.integer << op2.literal.integer), op1.type);

	return NO_VALUE;
}

SPIRVComparison::SPIRVComparison(const uint32_t id, SPIRVMethod& method, const std::string& opcode, const uint32_t resultType, const std::vector<uint32_t>& operands, const intermediate::InstructionDecorations decorations) :
    SPIRVInstruction(id, method, opcode, resultType, operands, decorations)
{

}

SPIRVComparison::~SPIRVComparison()
{

}

void SPIRVComparison::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const Value arg0 = getValue(operands.at(0), *method.method, types, constants, globals, localTypes);
    const Value arg1 = getValue(operands.at(1), *method.method, types, constants, globals, localTypes);
    logging::debug() << "Generating intermediate comparison '" << opcode << "' of " << arg0.to_string(false) << " and " << arg1.to_string(false) << " into " << dest.to_string(true) << logging::endl;
    method.method->appendToEnd((new intermediate::Comparison(opcode, dest, arg0, arg1))->setDecorations(decorations));
}

Optional<Value> SPIRVComparison::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	const Value op1 = constants.at(operands.at(0));
	const Value op2 = constants.at(operands.at(1));

	if(intermediate::COMP_EQ.compare(opcode) == 0)
		return op1 == op2 ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_FALSE.compare(opcode) == 0)
		return BOOL_FALSE;
	if(intermediate::COMP_NEQ.compare(opcode) == 0)
		return op1 != op2 ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_TRUE.compare(opcode) == 0)
		return BOOL_TRUE;
	if(intermediate::COMP_SIGNED_GE.compare(opcode) == 0)
		return op1.literal.integer >= op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_SIGNED_GT.compare(opcode) == 0)
		return op1.literal.integer > op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_SIGNED_LE.compare(opcode) == 0)
		return op1.literal.integer <= op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_SIGNED_LT.compare(opcode) == 0)
		return op1.literal.integer < op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_UNSIGNED_GE.compare(opcode) == 0)
		return op1.literal.integer >= op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_UNSIGNED_GT.compare(opcode) == 0)
		return op1.literal.integer > op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_UNSIGNED_LE.compare(opcode) == 0)
		return op1.literal.integer <= op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;
	if(intermediate::COMP_UNSIGNED_LT.compare(opcode) == 0)
		return op1.literal.integer < op2.literal.integer ? BOOL_TRUE : BOOL_FALSE;

	return NO_VALUE;
}

SPIRVCallSite::SPIRVCallSite(const uint32_t id, SPIRVMethod& method, const uint32_t methodID, const uint32_t resultType, const std::vector<uint32_t>& arguments) :
            SPIRVOperation(id, method), methodID(methodID), typeID(resultType), arguments(arguments)
{
}

SPIRVCallSite::SPIRVCallSite(const uint32_t id, SPIRVMethod& method, const std::string& methodName, const uint32_t resultType, const std::vector<uint32_t>& arguments) :
            SPIRVOperation(id, method), typeID(resultType), methodName(methodName), arguments(arguments)
{
}

SPIRVCallSite::~SPIRVCallSite()
{

}

void SPIRVCallSite::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    std::string calledFunction = methodName.orElse("");
    if(methodID)
        calledFunction = methods.at(methodID).method->name;
    std::vector<Value> args;
    for(const uint32_t op : arguments)
    {
        args.push_back(getValue(op, *method.method, types, constants, globals, localTypes));
    }
    logging::debug() << "Generating intermediate call-site to '" << calledFunction << "' with " << args.size() << " parameters into " << dest.to_string(true) << logging::endl;
    method.method->appendToEnd((new intermediate::MethodCall(dest, calledFunction, args))->setDecorations(decorations));
}

Optional<Value> SPIRVCallSite::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

SPIRVReturn::SPIRVReturn(SPIRVMethod& method) : SPIRVOperation(UNDEFINED_ID, method)
{

}

SPIRVReturn::SPIRVReturn(const uint32_t returnValue, SPIRVMethod& method) : SPIRVOperation(UNDEFINED_ID, method), returnValue(returnValue)
{

}

SPIRVReturn::~SPIRVReturn()
{

}

void SPIRVReturn::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    if(returnValue)
    {
        const Value value = getValue(returnValue, *method.method, types, constants, globals, localTypes);
        logging::debug() << "Generating intermediate return of value: " << value.to_string(false) << logging::endl;
        method.method->appendToEnd(new intermediate::Return(value));
    }
    else
    {
        logging::debug() << "Generating intermediate return" << logging::endl;
        method.method->appendToEnd(new intermediate::Return());
    }
}

Optional<Value> SPIRVReturn::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	if(returnValue && constants.find(returnValue) != constants.end())
		return constants.at(returnValue);
	return NO_VALUE;
}

SPIRVBranch::SPIRVBranch(SPIRVMethod& method, const uint32_t labelID) : SPIRVOperation(UNDEFINED_ID, method), defaultLabelID(labelID)
{

}

SPIRVBranch::SPIRVBranch(SPIRVMethod& method, const uint32_t conditionID, const uint32_t trueLabelID, const uint32_t falseLabelID) : 
    SPIRVOperation(UNDEFINED_ID, method), defaultLabelID(trueLabelID), conditionID(conditionID), falseLabelID(falseLabelID)
{

}

SPIRVBranch::~SPIRVBranch()
{

}

void SPIRVBranch::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    if(conditionID)
    {
        logging::debug() << "Generating intermediate conditional branch on %" << conditionID.get() << " to either %" << defaultLabelID << " or %" << falseLabelID.get() << logging::endl;
        const Value cond = getValue(conditionID, *method.method, types, constants, globals, localTypes);
        const Local* trueLabel = method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(defaultLabelID));
        const Local*  falseLabel = method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(falseLabelID.get()));
        method.method->appendToEnd(new intermediate::Branch(trueLabel, COND_ZERO_CLEAR, cond));
        method.method->appendToEnd(new intermediate::Branch(falseLabel, COND_ZERO_SET, cond));
    }
    else
    {
        logging::debug() << "Generating intermediate branch to %" << defaultLabelID << logging::endl;
        const Local* label = method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(defaultLabelID));
        method.method->appendToEnd(new intermediate::Branch(label, COND_ALWAYS, BOOL_TRUE));
    }
}

Optional<Value> SPIRVBranch::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

SPIRVLabel::SPIRVLabel(const uint32_t id, SPIRVMethod& method) : SPIRVOperation(id, method)
{

}

SPIRVLabel::~SPIRVLabel()
{

}

void SPIRVLabel::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    logging::debug() << "Generating intermediate label %" << id << logging::endl;
    method.method->appendToEnd(new intermediate::BranchLabel(*method.method->findOrCreateLocal(TYPE_LABEL, std::string("%") + std::to_string(id))));
}

Optional<Value> SPIRVLabel::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

SPIRVConversion::SPIRVConversion(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID, const ConversionType type, const intermediate::InstructionDecorations decorations, bool isSaturated) :
SPIRVOperation(id, method, decorations), typeID(resultType), sourceID(sourceID), type(type), isSaturated(isSaturated)
{

}

SPIRVConversion::~SPIRVConversion()
{

}

void SPIRVConversion::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value source = getValue(sourceID, *method.method, types, constants, globals, localTypes);
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const uint8_t sourceWidth = source.type.getScalarBitCount();
    const uint8_t destWidth = dest.type.getScalarBitCount();
    
    logging::debug() << "Generating intermediate conversion from " << source.to_string(false) << " to " << dest.to_string(true) << logging::endl;
    if(isSaturated)
    	intermediate::insertSaturation(method.method->appendToEnd(), *method.method.get(), source, dest, type == ConversionType::SIGNED);
    else if(type == ConversionType::BITCAST || sourceWidth == destWidth)
    	method.method->appendToEnd((new intermediate::MoveOperation(dest, source))->setDecorations(decorations));
    else if(sourceWidth > destWidth)
    	//TODO SConvert ?? Signed truncation! Anything special to do?
    	method.method->appendToEnd((new intermediate::Operation(type == ConversionType::FLOATING ? "fptrunc" : "trunc", dest, source))->setDecorations(decorations));
    else    // |source| < |dest|
    {
        if(type == ConversionType::SIGNED)
        	method.method->appendToEnd((new intermediate::Operation("sext", dest, source))->setDecorations(decorations));
        else if(type == ConversionType::UNSIGNED)
        	method.method->appendToEnd((new intermediate::Operation("zext", dest, source))->setDecorations(add_flag(decorations, intermediate::InstructionDecorations::UNSIGNED_RESULT)));
    }
}

Optional<Value> SPIRVConversion::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	if(constants.find(sourceID) != constants.end())
	{
		const Value source = constants.at(sourceID);
		Value dest(UNDEFINED_VALUE);
		const DataType destType = types.at(typeID);
		switch(type)
		{
			case ConversionType::BITCAST:
				dest = source;
				dest.type = destType;
				return source;
			case ConversionType::FLOATING:
				//TODO fptrunc/extend
				break;
			case ConversionType::SIGNED:
				//TODO trunc/sext + saturation
				break;
			case ConversionType::UNSIGNED:
				//TODO trunc/zext + saturation
				break;

		}
	}
	return NO_VALUE;
}

SPIRVCopy::SPIRVCopy(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID, const MemoryAccess memoryAccess, const uint32_t size) :
            SPIRVOperation(id, method), typeID(resultType), sourceID(sourceID), memoryAccess(memoryAccess), sizeID(size)
{

}

SPIRVCopy::SPIRVCopy(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID, const std::vector<uint32_t>& destIndices, const std::vector<uint32_t>& sourceIndices) :
            SPIRVOperation(id, method), typeID(resultType), sourceID(sourceID), memoryAccess(MemoryAccess::NONE), destIndices(destIndices), sourceIndices(sourceIndices)
{

}

SPIRVCopy::~SPIRVCopy()
{

}

void SPIRVCopy::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value source = getValue(sourceID, *method.method, types, constants, globals, localTypes);
    Value dest(UNDEFINED_VALUE);
    if(typeID == UNDEFINED_ID)
    {
    	//globals may have other names than their ID, so check them first
    	if(globals.find(id) != globals.end())
    		dest = globals.at(id)->createReference(destIndices.hasValue && !destIndices.get().empty() ? destIndices.get().at(0) : ANY_ELEMENT);
    	else
    		dest = method.method->findOrCreateLocal(source.type, std::string("%") + std::to_string(id))->createReference();
    }
    else
        dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    if(memoryAccess != MemoryAccess::NONE)
    {
    	//FIXME can't handle I/O of complex types, e.g. array (bigger than 16 elements), see JohnTheRipper/DES_bs_kernel.cl
    	//need to split in I/O of scalar type (use VPM cache, multi-line VPM)
        if(memoryAccess == MemoryAccess::READ)
        {
            logging::debug() << "Generating reading of " << source.to_string() << " into " << dest.to_string() << logging::endl;
            periphery::insertReadDMA(method.method->appendToEnd(), dest, source);
        }
        else if(memoryAccess == MemoryAccess::WRITE)
        {
            logging::debug() << "Generating writing of " << source.to_string() << " into " << dest.to_string() << logging::endl;
            periphery::insertWriteDMA(method.method->appendToEnd(), source, dest);
        }
        else if(memoryAccess == MemoryAccess::READ_WRITE)
        {
        	if(sizeID == UNDEFINED_ID)
        	{
        		//copy single object
				logging::debug() << "Generating copying of " << source.to_string() << " into " << dest.to_string() << logging::endl;
				const Value tmp = method.method->addNewLocal(source.type, "%copy_tmp");
				periphery::insertReadDMA(method.method->appendToEnd(), tmp, source);
				periphery::insertWriteDMA(method.method->appendToEnd(), tmp, dest);
        	}
        	else
        	{
        		//copy area of memory
        		const Value size = getValue(sizeID, *method.method, types, constants, globals, localTypes);
        		logging::debug() << "Generating copying of " << size.to_string() << " bytes from " << source.to_string() << " into " << dest.to_string() << logging::endl;
        		if(size.hasType(ValueType::LITERAL))
        		{
        			method.method->vpm->insertCopyRAM(*method.method, method.method->appendToEnd(), dest, source, size.literal.integer, true);
        		}
        		else
        			//TODO in any case, loop over copies, up to the size specified
        			throw CompilationError(CompilationStep::LLVM_2_IR, "Copying dynamically sized memory is not yet implemented", size.to_string());
        	}
        }
    }
    else if(!destIndices && !sourceIndices)
    {
        //simple move
        logging::debug() << "Generating intermediate move from " << source.to_string() << " into " << dest.to_string(true) << logging::endl;
        method.method->appendToEnd((new intermediate::MoveOperation(dest, source))->setDecorations(decorations));
    }
    else if(sourceIndices && dest.type.isScalarType())
    {
    	if(sourceIndices.get().size() > 1)
    		throw CompilationError(CompilationStep::LLVM_2_IR, "Multi level indices are not implemented yet");
        //index is literal
        logging::debug() << "Generating intermediate extraction of index " << sourceIndices.get().at(0) << " from " << source.to_string() << " into " << dest.to_string(true) << logging::endl;
        intermediate::insertVectorExtraction(method.method->appendToEnd(), *method.method, source, Value(Literal(static_cast<long>(sourceIndices.get().at(0))), TYPE_INT8), dest);
    }
    else if((!sourceIndices || (sourceIndices.get().at(0) == 0)) && destIndices)
    {
    	if(destIndices.get().size() > 1)
			throw CompilationError(CompilationStep::LLVM_2_IR, "Multi level indices are not implemented yet");
        //add element to vector to element
        //index is literal
        logging::debug() << "Generating intermediate insertion of " << source.to_string() << " into element " << destIndices.get().at(0) << " of " << dest.to_string(true) << logging::endl;
        intermediate::insertVectorInsertion(method.method->appendToEnd(), *method.method, dest, Value(Literal(static_cast<long>(destIndices.get().at(0))), TYPE_INT8), source);
    }
    else
    {
        throw std::runtime_error("This version of copy is not implemented yet!");
        //TODO indices are already literals!!
    }
}

Optional<Value> SPIRVCopy::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	if(constants.find(sourceID) != constants.end())
		return constants.at(sourceID);
	return NO_VALUE;
}

SPIRVShuffle::SPIRVShuffle(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID0, const uint32_t sourceID1, const std::vector<uint32_t>& indices) :
            SPIRVOperation(id, method), typeID(resultType), source0(sourceID0), source1(sourceID1), indices(indices), compositeIndex(false)
{

}

SPIRVShuffle::SPIRVShuffle(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t sourceID0, const uint32_t sourceID1, const uint32_t compositeIndex) :
		SPIRVOperation(id, method), typeID(resultType), source0(sourceID0), source1(sourceID1), indices(1, compositeIndex), compositeIndex(true)
{

}

SPIRVShuffle::~SPIRVShuffle()
{

}

void SPIRVShuffle::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    //shuffling = iteration over all elements in both vectors and re-ordering in order given
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const Value src0 = getValue(source0, *method.method, types, constants, globals, localTypes);
    const Value src1 = getValue(source1, *method.method, types, constants, globals, localTypes);
    Value index(UNDEFINED_VALUE);
    if(compositeIndex)
    {
    	//there is just one index, which is a composite
    	index = getValue(indices.at(0), *method.method, types, constants, globals, localTypes);
    }
    else	//all indices are literal values
    {
    	ContainerValue indices;
		bool allIndicesUndef = true;
		bool allIndicesZero = true;
		for(const uint32_t index : this->indices)
		{
			//"A Component literal may also be FFFFFFFF, which means the corresponding result component has no source and is undefined"
			if(index == 0xFFFFFFFFU)
			{
				indices.elements.emplace_back(UNDEFINED_VALUE);
			}
			else
			{
				allIndicesUndef = false;
				if(index != 0 && index != 0xFFFFFFFFU)
					//accept UNDEF as zero, so i.e. (0,0,0,UNDEF) can be simplified as all-zero
					allIndicesZero = false;
				indices.elements.emplace_back(Literal(static_cast<long>(index)), TYPE_INT8);
			}
		}

		if(allIndicesUndef)
			index = UNDEFINED_VALUE;
		else if(allIndicesZero)
			index = ZERO_INITIALIZER;
		else
		{
			index = Value(indices, TYPE_INT8);
		}
    }
    logging::debug() << "Generating intermediate operations for mixing " << src0.to_string() << " and " << src1.to_string() << " into " << dest.to_string() << " with mask " << index.to_string(false, true) << logging::endl;
    
    intermediate::insertVectorShuffle(method.method->appendToEnd(), *method.method, dest, src0, src1, index);
}

Optional<Value> SPIRVShuffle::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

SPIRVIndexOf::SPIRVIndexOf(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t containerID, const std::vector<uint32_t>& indices) :
            SPIRVOperation(id, method), typeID(resultType), container(containerID), indices(indices)
{

}

SPIRVIndexOf::~SPIRVIndexOf()
{

}

void SPIRVIndexOf::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    //need to get pointer/address -> reference to content
    //a[i] of type t is at position &a + i * sizeof(t)
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const Value container = getValue(this->container, *method.method, types, constants, globals, localTypes);

    logging::debug() << "Generating calculating indices of " << container.to_string() << " into " << dest.to_string() << logging::endl;
    std::vector<Value> indexValues;
    indexValues.reserve(indices.size());
    for(const uint32_t indexID : indices)
    {
    	indexValues.push_back(getValue(indexID, *method.method, types, constants, globals, localTypes));
    }

    intermediate::insertCalculateIndices(method.method->appendToEnd(), *method.method.get(), container, dest, indexValues);
}

Optional<Value> SPIRVIndexOf::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	Value container(UNDEFINED_VALUE);
	if(constants.find(this->container) != constants.end())
		container = constants.at(this->container);
	else if(globals.find(this->container) != globals.end())
		container = globals.at(this->container)->createReference();
	else
	{
		logging::error() << this->container << " " << this->id << logging::endl;
		throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid constant container!");
	}

	std::vector<Value> indexValues;
	indexValues.reserve(indices.size());
	std::for_each(indices.begin(), indices.end(), [&indexValues, &constants](uint32_t index) {indexValues.push_back(constants.at(index));});

	logging::debug() << "Pre-calculating indices of " << container.to_string()  << logging::endl;

	Value offset = INT_ZERO;
	DataType subContainerType = container.type;
	for(const Value& index : indexValues)
	{
		Value subOffset(UNDEFINED_VALUE);
		if(subContainerType.isPointerType() || subContainerType.getArrayType().hasValue)
		{
			//index is index in pointer/array
			//-> add offset of element at given index to global offset
			if(index.hasType(ValueType::LITERAL))
			{
				subOffset = Value(Literal(index.literal.integer * subContainerType.getElementType().getPhysicalWidth()), TYPE_INT32);
			}
			else
			{
				throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid index for constant expression", index.to_string());
			}

			subContainerType = subContainerType.getElementType();
		}
		else if(subContainerType.getStructType().hasValue)
		{
			//index is element in struct -> MUST be literal
			if(!index.hasType(ValueType::LITERAL))
				throw CompilationError(CompilationStep::LLVM_2_IR, "Can't access struct-element with non-literal index", index.to_string());

			subOffset = Value(Literal(static_cast<unsigned long>(container.type.getStructType().get()->getStructSize(index.literal.integer))), TYPE_INT32);
			subContainerType = subContainerType.getElementType(index.literal.integer);
		}
		else
			throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid container-type to retrieve element via index", subContainerType.to_string());

		if(offset.hasType(ValueType::LITERAL) && subOffset.hasType(ValueType::LITERAL))
			offset.literal.integer += subOffset.literal.integer;
		else
			throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid index for constant expression", offset.to_string());
	}

	return offset;
}

SPIRVPhi::SPIRVPhi(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const std::vector<std::pair<uint32_t, uint32_t> >& sources) :
            SPIRVOperation(id, method), typeID(resultType), sources(sources)
{

}

SPIRVPhi::~SPIRVPhi()
{

}

void SPIRVPhi::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    
    logging::debug() << "Generating Phi-Node with " << sources.size() << " options into " << dest.to_string() << logging::endl;
    //https://stackoverflow.com/questions/11485531/what-exactly-phi-instruction-does-and-how-to-use-it-in-llvm#11485946
    //sets the output value according to where from this instructions is executed/jumped from
    std::vector<std::pair<Value, const Local*>> labelPairs;
    labelPairs.reserve(sources.size());
    for(const std::pair<uint32_t, uint32_t>& option : sources)
    {
    	const Value source = getValue(option.second, *method.method, types, constants, globals, localTypes);
		const Value val = getValue(option.first, *method.method, types, constants, globals, localTypes);
		labelPairs.emplace_back(val, source.local);
    }
    method.method->appendToEnd(new intermediate::PhiNode(dest, labelPairs));
}

Optional<Value> SPIRVPhi::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

SPIRVSelect::SPIRVSelect(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const uint32_t conditionID, const uint32_t trueObj, const uint32_t falseObj) :
        SPIRVOperation(id, method), typeID(resultType), condID(conditionID), trueID(trueObj), falseID(falseObj)
{

}

SPIRVSelect::~SPIRVSelect()
{

}

void SPIRVSelect::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value sourceTrue = getValue(trueID, *method.method, types, constants, globals, localTypes);
    const Value sourceFalse = getValue(falseID, *method.method, types, constants, globals, localTypes);
    const Value condition = getValue(condID, *method.method, types, constants, globals, localTypes);
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    
    logging::debug() << "Generating intermediate select on " << condition.to_string() << " whether to write " << sourceTrue.to_string() << " or " << sourceFalse.to_string() << " into " << dest.to_string(true) << logging::endl;
    
    method.method->appendToEnd(new intermediate::MoveOperation(NOP_REGISTER, condition, COND_ALWAYS, SetFlag::SET_FLAGS));
    method.method->appendToEnd(new intermediate::MoveOperation(dest, sourceTrue, COND_ZERO_CLEAR));
    method.method->appendToEnd(new intermediate::MoveOperation(dest, sourceFalse, COND_ZERO_SET));
}

Optional<Value> SPIRVSelect::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	if(constants.find(condID) != constants.end())
	{
		if(constants.at(condID).literal.flag && constants.find(trueID) != constants.end())
		{
			return constants.at(trueID);
		}
		if(constants.at(condID).literal.flag == false && constants.find(falseID) != constants.end())
		{
			return constants.at(falseID);
		}
	}
	return NO_VALUE;
}

SPIRVSwitch::SPIRVSwitch(const uint32_t id, SPIRVMethod& method, const uint32_t selectorID, const uint32_t defaultID, const std::vector<std::pair<uint32_t, uint32_t>>& destinations) :
        SPIRVOperation(id, method), selectorID(selectorID), defaultID(defaultID), destinations(destinations)
{

}

SPIRVSwitch::~SPIRVSwitch()
{

}

void SPIRVSwitch::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value selector = getValue(selectorID, *method.method, types, constants, globals, localTypes);
    const Value defaultLabel = getValue(defaultID, *method.method, types, constants, globals, localTypes);
    
    logging::debug() << "Generating intermediate switched jump on " << selector.to_string() << " to " << destinations.size() << " destinations with default " << defaultLabel.to_string() << logging::endl;
    
    for(const auto& pair : destinations)
    {
        //comparison value is a literal
        const Value comparison(Literal(static_cast<long>(pair.first)), selector.type);
        const Value destination = getValue(pair.second, *method.method, types, constants, globals, localTypes);
        //for every case, if equal,branch to given label
        const Value tmp = method.method->addNewLocal(TYPE_BOOL, "%switch");
        method.method->appendToEnd(new intermediate::Comparison(intermediate::COMP_EQ, tmp, selector, comparison));
        method.method->appendToEnd(new intermediate::Branch(destination.local, COND_ZERO_CLEAR, tmp));
    }
    //branch default label
    method.method->appendToEnd(new intermediate::Branch(defaultLabel.local, COND_ALWAYS, BOOL_TRUE));
}

Optional<Value> SPIRVSwitch::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	if(constants.find(selectorID) != constants.end())
	{
		Value selector = constants.at(selectorID);
		for(const auto& pair : destinations)
		{
			if(selector.hasLiteral(Literal(static_cast<long>(pair.first))) && constants.find(pair.second) != constants.end())
			{
				return constants.at(pair.second);
			}
		}
	}
	return NO_VALUE;
}

SPIRVImageQuery::SPIRVImageQuery(const uint32_t id, SPIRVMethod& method, const uint32_t resultType, const ImageQuery value, const uint32_t imageID, const uint32_t lodOrCoordinate) :
        SPIRVOperation(id, method), typeID(resultType), valueID(value), imageID(imageID), lodOrCoordinate(lodOrCoordinate)
{

}

SPIRVImageQuery::~SPIRVImageQuery()
{

}

void SPIRVImageQuery::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods, std::map<uint32_t, Global*>& globals) const
{
    const Value dest = toNewLocal(*method.method, id, typeID, types, localTypes);
    const Value image = getValue(imageID, *method.method, types, constants, globals, localTypes);
    Value param(UNDEFINED_VALUE);
    if(lodOrCoordinate != UNDEFINED_ID)
    {
    	param = getValue(lodOrCoordinate, *method.method, types, constants, globals, localTypes);
    }
    
    switch(valueID)
    {
        case ImageQuery::CHANNEL_DATA_TYPE:
            logging::debug() << "Generating query of image's channel data-type for image: " << image.to_string() << logging::endl;
            intermediate::insertQueryChannelDataType(method.method->appendToEnd(), *method.method, image, dest);
            return;
        case ImageQuery::CHANNEL_ORDER:
            logging::debug() << "Generating query of image's channel order for image: " << image.to_string() << logging::endl;
            intermediate::insertQueryChannelOrder(method.method->appendToEnd(), *method.method, image, dest);
            return;
        case ImageQuery::SIZES:
            logging::debug() << "Generating query of image's measurements for image: " << image.to_string() << logging::endl;
            intermediate::insertQueryMeasurements(method.method->appendToEnd(), *method.method, image, dest);
            return;
        case ImageQuery::SIZES_LOD:
        	logging::debug() << "Generating query of image's measurements for image with LOD: " << image.to_string() << logging::endl;
        	if(param.hasLiteral(INT_ZERO.literal))
        	{
        		//same as above
        		intermediate::insertQueryMeasurements(method.method->appendToEnd(), *method.method, image, dest);
        	}
        	else
        		throw CompilationError(CompilationStep::LLVM_2_IR, "Images with LOD are not supported", image.to_string());
        	return;
        case ImageQuery::MIPMAP_LEVELS:
        	//mipmaps are not yet supported in OpenCL 1.2
        	throw CompilationError(CompilationStep::LLVM_2_IR, "Mipmap levels are not yet supported by OpenCL 1.2");
        case ImageQuery::SAMPLES_PER_TEXEL:
        	//multi-sample images are not yet supported in OpenCL 1.2
        	throw CompilationError(CompilationStep::LLVM_2_IR, "Multi-sample images are not yet supported by OpenCL 1.2");
    }
    
}

Optional<Value> SPIRVImageQuery::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

vc4c::spirv2qasm::SPIRVMemoryBarrier::SPIRVMemoryBarrier(SPIRVMethod& method, const uint32_t scopeID, const uint32_t semanticsID) :
		SPIRVOperation(UNDEFINED_ID, method), scopeID(scopeID), semanticsID(semanticsID)
{
}

vc4c::spirv2qasm::SPIRVMemoryBarrier::~SPIRVMemoryBarrier()
{
}

void vc4c::spirv2qasm::SPIRVMemoryBarrier::mapInstruction(std::map<uint32_t, DataType>& types, std::map<uint32_t, Value>& constants, std::map<uint32_t, uint32_t>& localTypes, std::map<uint32_t, SPIRVMethod>& methods,
		std::map<uint32_t, Global*>& globals) const
{
	const Value scope = getValue(scopeID, *method.method, types, constants, globals, localTypes);
	const Value semantics = getValue(semanticsID, *method.method, types, constants, globals, localTypes);

	if(!scope.hasType(ValueType::LITERAL) || !semantics.hasType(ValueType::LITERAL))
		throw CompilationError(CompilationStep::LLVM_2_IR, "Memory barriers with non-constant scope or memory semantics are not supported!");

	method.method->appendToEnd(new intermediate::MemoryBarrier(static_cast<intermediate::MemoryScope>(scope.literal.integer), static_cast<intermediate::MemorySemantics>(semantics.literal.integer)));
}

Optional<Value> vc4c::spirv2qasm::SPIRVMemoryBarrier::precalculate(const std::map<uint32_t, DataType>& types, const std::map<uint32_t, Value>& constants, const std::map<uint32_t, Global*>& globals) const
{
	return NO_VALUE;
}

#endif
