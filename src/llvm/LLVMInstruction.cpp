/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <config.h>
#include <algorithm>

#include "LLVMInstruction.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/Helper.h"
#include "../periphery/VPM.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::llvm2qasm;

ValueType llvm2qasm::toValueType(const TokenType type)
{
    switch(type)
    {
    case TokenType::BOOLEAN:
        return ValueType::LITERAL;
    case TokenType::NUMBER:
        return ValueType::LITERAL;
    case TokenType::STRING:
        return ValueType::LOCAL;
    default:;
    }
    throw CompilationError(CompilationStep::PARSER, "Unhandled token-to-value type!");
}

LLVMInstruction::LLVMInstruction() : decorations(intermediate::InstructionDecorations::NONE)
{
}

LLVMInstruction::~LLVMInstruction()
{
}

const Local* LLVMInstruction::getDeclaredLocal() const
{
	return nullptr;
}

std::vector<const Local*> LLVMInstruction::getAllLocals() const
{
    const auto tmp = getDeclaredLocal();
    if(tmp)
    {
        return {tmp};
    }
    return {};
}

LLVMInstruction* LLVMInstruction::setDecorations(const intermediate::InstructionDecorations decorations)
{
	this->decorations = decorations;
	return this;
}

CallSite::CallSite(const Local* dest, const std::string& methodName, const DataType& returnType, const std::vector<Value>& args) :
        dest(dest), methodName(methodName), returnType(returnType), arguments(args)
{

}

CallSite::CallSite(const Local* dest, const Method& method, const std::vector<Value>& args) : dest(dest), methodName(method.name), returnType(method.returnType), arguments(args)
{
    if(method.parameters.size() != args.size())
    {
        throw CompilationError(CompilationStep::PARSER, "Invalid numbers of method arguments", std::string("Got ") + (std::to_string(args.size()) + ", expected ") + std::to_string(method.parameters.size()));
    }
}

CallSite::CallSite(const std::string& methodName, const DataType& returnType, const std::vector<Value>& args) :
        dest(), methodName(methodName), returnType(returnType), arguments(args)
{

}

CallSite::CallSite(const Method& method, const std::vector<Value>& args) : dest(), methodName(method.name), returnType(method.returnType), arguments(args)
{
    if(method.parameters.size() != args.size())
    {
        throw CompilationError(CompilationStep::PARSER, "Invalid numbers of method arguments", std::string("Got ") + (std::to_string(args.size()) + ", expected ") + std::to_string(method.parameters.size()));
    }
}

CallSite::~CallSite()
{

}

const Local* CallSite::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> CallSite::getAllLocals() const
{
    std::vector<const Local*> res;
    if(!dest->name.empty())
    {
        res.push_back(dest);
    }
    for(const Value& val : arguments)
    {
        if(val.hasType(ValueType::LOCAL))
        {
            res.push_back(val.local);
        }
    }
    return res;
}

bool CallSite::mapInstruction(Method& method) const
{
    //remove calls to @llvm.lifetime.start / @llvm.lifetime.end
    if(methodName.compare("llvm.lifetime.start") == 0 || methodName.compare("llvm.lifetime.end") == 0)
    {
        logging::debug() << "Dropping intrinsic method call to " << methodName << logging::endl;
        return true;
    }
    const Value output = dest == nullptr ? NOP_REGISTER : Value(dest, returnType);
    //handle other llvm.* intrinsics
    if(methodName.compare("llvm.fmuladd.f32") == 0)
    {
    	logging::debug() << "Converting intrinsic method call '" << methodName << "' to operations" << logging::endl;
    	const Value tmp = method.addNewLocal(returnType, "%fmuladd");
    	method.appendToEnd(new intermediate::Operation("fmul", tmp, arguments.at(0), arguments.at(1)));
    	method.appendToEnd(new intermediate::Operation("fadd", output, tmp, arguments.at(2)));
    	return true;
    }
    if(methodName.find("llvm.memcpy") == 0 && arguments.at(2).hasType(ValueType::LITERAL))
    {
    	//FIXME for now skip unsupported case, since errors here seem to crash the test-runner, but errors later on dont??
    	//@llvm.memcpy.p0i8.p0i8.i32(i8* <dest>, i8* <src>, i32 <len>, i32 <align>, i1 <isvolatile>)
    	logging::debug() << "Intrinsifying llvm.memcpy function-call" << logging::endl;
    	method.vpm->insertCopyRAM(method, method.appendToEnd(), arguments.at(0), arguments.at(1), arguments.at(2).literal.integer, true);
    	return true;
    }
    if(methodName.find("llvm.memset") == 0 && arguments.at(2).hasType(ValueType::LITERAL))
	{
		//declare void @llvm.memset.p0i8.i32(i8* <dest>, i8 <val>, i32|i64 <len>, i32 <align>, i1 <isvolatile>)
		logging::debug() << "Intrinsifying llvm.memset with DMA writes" << logging::endl;
		const Value& memAddr = arguments.at(0);
		const Value& fillByte = arguments.at(1);
		const Value& numBytes = arguments.at(2);
		const Value& volatileAccess = arguments.at(4);
		method.appendToEnd(new intermediate::MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
		method.vpm->insertWriteVPM(method.appendToEnd(), fillByte, false);
		//TODO could be optimized, write multiple bytes at once
		for(long i = 0; i < numBytes.literal.integer; ++i)
		{
			const Value tmp = method.addNewLocal(memAddr.type, "%memset_offset");
			method.appendToEnd(new intermediate::Operation("add", tmp, memAddr, Value(Literal(i), TYPE_INT32)));
			method.vpm->insertWriteRAM(method.appendToEnd(), tmp, TYPE_INT8, false);
		}
		method.appendToEnd( new intermediate::MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
	}
    logging::debug() << "Generating immediate call to " << methodName << " -> " << returnType.to_string() << logging::endl;
    if(dest == nullptr)
    	method.appendToEnd((new intermediate::MethodCall(methodName, arguments))->setDecorations(decorations));
    else
    	method.appendToEnd((new intermediate::MethodCall(output, methodName, arguments))->setDecorations(decorations));
    return true;
}

const std::vector<Value>& CallSite::getArguments() const
{
    return arguments;
}

const std::string& CallSite::getMethodName() const
{
    return methodName;
}

Copy::Copy(const Local* dest, const Value& orig, const Value& index, const bool isRead) : dest(dest), orig(orig), index(index), isRead(isRead)
{

}

Copy::~Copy()
{

}

const Local* Copy::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> Copy::getAllLocals() const
{
    if(orig.hasType(ValueType::LOCAL))
    {
        return {dest, orig.local};
    }
    return {dest};
}

bool Copy::mapInstruction(Method& method) const
{
    if(!(index.type == TYPE_UNKNOWN))
    {
        if(isRead)
        {
            logging::debug() << "Generating reading of " << orig.to_string() << " from index " << index.to_string() << " into " << dest->to_string() << logging::endl;
            periphery::insertReadDMA(method.appendToEnd(), Value(dest, orig.type), index);
        }
        else
        {
            logging::debug() << "Generating writing of " << orig.to_string() << " into " << index.to_string() << logging::endl;
            periphery::insertWriteDMA(method.appendToEnd(), orig, index);
        }
    }
    else
    {
        logging::debug() << "Generating copy of " << orig.to_string() << " into " << dest->name << logging::endl;
        method.appendToEnd(new intermediate::MoveOperation(Value(dest, orig.type), orig));
    }
    return true;
}

UnaryOperator::UnaryOperator(const std::string& opCode, const Value& dest, const Value& arg) : dest(dest), opCode(opCode), arg(arg)
{

}

UnaryOperator::~UnaryOperator()
{

}

const Local* UnaryOperator::getDeclaredLocal() const
{
    if(dest.hasType(ValueType::LOCAL))
        return dest.local;
    return nullptr;
}

std::vector<const Local*> UnaryOperator::getAllLocals() const
{
    std::vector<const Local*> locals;
    if(dest.hasType(ValueType::LOCAL))
        locals.push_back(dest.local);
    if(arg.hasType(ValueType::LOCAL))
    {
        locals.push_back(arg.local);
    }
    return locals;
}

bool UnaryOperator::mapInstruction(Method& method) const
{
    logging::debug() << "Generating unary operation " << opCode << " with " << arg.to_string() << " into " << dest.to_string() << logging::endl;
    method.appendToEnd((new intermediate::Operation(opCode, dest, arg))->setDecorations(decorations));
    return true;
}

BinaryOperator::BinaryOperator(const std::string& opCode, const Local* dest, const Value& arg0, const Value& arg1) :
UnaryOperator(opCode, Value(dest, TYPE_UNKNOWN), arg0), arg2(arg1)
{

}

BinaryOperator::~BinaryOperator()
{

}

std::vector<const Local*> BinaryOperator::getAllLocals() const
{
    auto tmp = UnaryOperator::getAllLocals();
    if(arg2.hasType(ValueType::LOCAL))
    {
        tmp.push_back(arg2.local);
    }
    return tmp;
}

bool BinaryOperator::mapInstruction(Method& method) const
{
    logging::debug() << "Generating binary operation " << opCode << " with " << arg.to_string() << " and " << arg2.to_string() << " into " << dest.to_string() <<logging::endl;
    method.appendToEnd((new intermediate::Operation(opCode, dest, arg, arg2))->setDecorations(decorations));
    return true;
}

IndexOf::IndexOf(const Local* dest,  const Value& container, const std::vector<Value>& indices) : dest(dest), container(container), indices(indices)
{

}

IndexOf::~IndexOf()
{

}

const Local* IndexOf::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> IndexOf::getAllLocals() const
{
    std::vector<const Local*> res = {dest};
    if(container.hasType(ValueType::LOCAL))
        res.push_back(container.local);
    for(const Value& index : indices)
    {
        if(index.hasType(ValueType::LOCAL))
            res.push_back(index.local);
    }
    return res;
}

bool IndexOf::mapInstruction(Method& method) const
{
    //need to get pointer/address -> reference to content
    //a[i] of type t is at position &a + i * sizeof(t)
    logging::debug() << "Generating calculating index " << to_string<Value>(indices) << " of " << container.to_string() << " into " << dest->to_string() << logging::endl;
    
    //TODO firstIndexIsElement is not true for all cases!! (E.g. not for pointers to pointers?)
    intermediate::insertCalculateIndices(method.appendToEnd(), method, container, dest->createReference(), indices, true);
    return true;
}

const Value IndexOf::getContainer() const
{
    return container;
}

Comparison::Comparison(const Local* dest, const std::string& comp, const Value& op1, const Value& op2, const bool isFloat) :
dest(dest), comp(comp), isFloat(isFloat), op1(op1), op2(op2)
{

}

Comparison::~Comparison()
{

}

const Local* Comparison::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> Comparison::getAllLocals() const
{
    std::vector<const Local*> tmp = {dest};
    if(op1.hasType(ValueType::LOCAL))
    {
        tmp.push_back(op1.local);
    }
    if(op2.hasType(ValueType::LOCAL))
    {
        tmp.push_back(op2.local);
    }
    return tmp;
}

bool Comparison::mapInstruction(Method& method) const
{
    logging::debug() << "Generating comparison " << comp << " with " << op1.to_string() << " and " << op2.to_string() << " into " << dest->name << logging::endl;
    method.appendToEnd((new intermediate::Comparison(comp, Value(dest, TYPE_BOOL), op1, op2))->setDecorations(decorations));
    return true;
}

ContainerInsertion::ContainerInsertion(const Local* dest, const Value& container, const Value& newValue, const Value& index) :
dest(dest), container(container), newValue(newValue), index(index)
{

}

ContainerInsertion::~ContainerInsertion()
{

}

const Local* ContainerInsertion::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> ContainerInsertion::getAllLocals() const
{
    std::vector<const Local*> tmp = {dest};
    if(container.hasType(ValueType::LOCAL))
        tmp.push_back(container.local);
    if(newValue.hasType(ValueType::LOCAL))
    {
        tmp.push_back(newValue.local);
    }
    if(index.hasType(ValueType::LOCAL))
    {
        tmp.push_back(index.local);
    }
    return tmp;
}

bool ContainerInsertion::mapInstruction(Method& method) const
{
    logging::debug() << "Generating insertion of " << newValue.to_string() << " at " << index.to_string() << " into " << container.to_string() << " into " << dest->to_string() << logging::endl;
    //1. copy whole container
    method.appendToEnd(new intermediate::MoveOperation(Value(dest, container.type), container));
    //2. insert new element
    //either into vector or into scalar at "element 0"
    if(container.type.isVectorType() || index.hasLiteral(Literal(0L)))
    {
        //insert element at given index into vector
        intermediate::insertVectorInsertion(method.appendToEnd(), method, Value(dest, container.type), index, newValue);
    }
    else
    {
        throw CompilationError(CompilationStep::LLVM_2_IR, "Not implemented!");
        //TODO handle arrays
        //instructions.emplace_back(new intermediate::MoveOperation(Value(element, elementType), newValue));
    }
    return true;
}

ContainerExtraction::ContainerExtraction(const Local* dest, const Value& container, const Value& index) :
dest(dest), container(container), index(index)
{

}

ContainerExtraction::~ContainerExtraction()
{

}


const Local* ContainerExtraction::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> ContainerExtraction::getAllLocals() const
{
    std::vector<const Local*> tmp = {dest};
    if(container.hasType(ValueType::LOCAL))
        tmp.push_back(container.local);
    if(index.hasType(ValueType::LOCAL))
    {
        tmp.push_back(index.local);
    }
    return tmp;
}

bool ContainerExtraction::mapInstruction(Method& method) const
{
    const DataType elementType = container.type.getElementType();
    logging::debug() << "Generation extraction of " << elementType.to_string() << " at " << index.to_string() << " from " << container.to_string() << " into " << dest->to_string() << logging::endl;
    
    if(container.type.isVectorType() || index.hasLiteral(Literal(0L)))
    {
        intermediate::insertVectorExtraction(method.appendToEnd(), method, container, index, Value(dest, elementType));
    }
    else
    {
        throw CompilationError(CompilationStep::LLVM_2_IR, "Not implemented!");
        //TODO handle arrays
        //instructions.emplace_back(new intermediate::MoveOperation(Value(element, elementType), newValue));
    }
    
    return true;
}

ValueReturn::ValueReturn() :hasValue(false), val(Literal(false), TYPE_VOID)
{

}

ValueReturn::ValueReturn(const Value& val): hasValue(true), val(val)
{

}

ValueReturn::~ValueReturn()
{

}


std::vector<const Local*> ValueReturn::getAllLocals() const
{
    if(hasValue && val.hasType(ValueType::LOCAL))
        return {val.local};
    return {};
}

bool ValueReturn::mapInstruction(Method& method) const
{
    if(hasValue)
    {
        logging::debug() << "Generating return of " << val.to_string() << logging::endl;
        method.appendToEnd(new intermediate::Return(val));
    }
    else
    {
        logging::debug() << "Generating return nothing" << logging::endl;
        method.appendToEnd(new intermediate::Return());
    }
    return true;
}

ShuffleVector::ShuffleVector(const Local* dest, const Value& v1, const Value& v2, const Value& mask) :
        dest(dest), v1(v1), v2(v2), mask(mask)
{

}

ShuffleVector::~ShuffleVector()
{

}


const Local* ShuffleVector::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> ShuffleVector::getAllLocals() const
{
    std::vector<const Local*> tmp = {dest};
    if(v1.hasType(ValueType::LOCAL))
        tmp.push_back(v1.local);
    if(v2.hasType(ValueType::LOCAL))
        tmp.push_back(v2.local);
    if(mask.hasType(ValueType::LOCAL))
    {
        tmp.push_back(mask.local);
    }
    return tmp;
}

bool ShuffleVector::mapInstruction(Method& method) const
{
    //shuffling = iteration over all elements in both vectors and re-ordering in order given
    logging::debug() << "Generating operations mixing " << v1.to_string() << " and " << v2.to_string() << " into " << dest->name << logging::endl;
    DataType destType = v1.type;
    destType.num = mask.type.num;
    intermediate::insertVectorShuffle(method.appendToEnd(), method, Value(dest, destType), v1, v2, mask);
    return true;
}

LLVMLabel::LLVMLabel(const std::string& name) : label(name)
{

}

LLVMLabel::~LLVMLabel()
{

}

bool LLVMLabel::mapInstruction(Method& method) const
{
    logging::debug() << "Generating label " << label << logging::endl;
    method.appendToEnd(new intermediate::BranchLabel(*method.findOrCreateLocal(TYPE_LABEL, label)));
    return true;
}

PhiNode::PhiNode(const Local* dest, const std::vector<std::pair<Value, const Local*> >& labels) : dest(dest), labels(labels)
{

}

PhiNode::~PhiNode()
{

}

const Local* PhiNode::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> PhiNode::getAllLocals() const
{
    std::vector<const Local*> refs = {dest};
    for(const auto& pair : labels)
    {
        if(pair.first.hasType(ValueType::LOCAL))
            refs.push_back(pair.first.local);
    }
    return refs;
}

bool PhiNode::mapInstruction(Method& method) const
{
    logging::debug() << "Generating Phi-Node with " << labels.size() << " options into " << dest->to_string() << logging::endl;
    method.appendToEnd(new intermediate::PhiNode(dest->createReference(), labels));
    return true;
}

Selection::Selection(const Local* dest, const Value& cond, const Value& opt1, const Value& opt2) : dest(dest), cond(cond), opt1(opt1), opt2(opt2)
{

}

Selection::~Selection()
{

}

const Local* Selection::getDeclaredLocal() const
{
    return dest;
}

std::vector<const Local*> Selection::getAllLocals() const
{
    std::vector<const Local*> refs = {dest};
    if(cond.hasType(ValueType::LOCAL))
        refs.push_back(cond.local);
    if(opt1.hasType(ValueType::LOCAL))
        refs.push_back(opt1.local);
    if(opt2.hasType(ValueType::LOCAL))
        refs.push_back(opt2.local);
    return refs;
}

bool Selection::mapInstruction(Method& method) const
{
    logging::debug() << "Generating moves for selection " << opt1.to_string() << " or " << opt2.to_string() << " according to " << cond.to_string() << logging::endl;
    //if cond == 1 -> first else second
    //makes sure, the flags are set for the correction value
    method.appendToEnd(new intermediate::MoveOperation(NOP_REGISTER, cond, COND_ALWAYS, SetFlag::SET_FLAGS));
    method.appendToEnd(new intermediate::MoveOperation(Value(dest, opt1.type), opt1, COND_ZERO_CLEAR));
    method.appendToEnd(new intermediate::MoveOperation(Value(dest, opt2.type), opt2, COND_ZERO_SET));
    return true;
}

Branch::Branch(const std::string& label) : thenLabel(label), elseLabel(""), cond(BOOL_TRUE)
{

}

Branch::Branch(const Value& cond, const std::string& thenLabel, const std::string& elseLabel) : 
        thenLabel(thenLabel), elseLabel(elseLabel), cond(cond)
{

}

Branch::~Branch()
{

}

std::vector<const Local*> Branch::getAllLocals() const
{
    if(cond.hasType(ValueType::LOCAL))
        return {cond.local};
    return {};
}

bool Branch::mapInstruction(Method& method) const
{
    logging::debug() << "Generating branch on condition " << cond.to_string() << " to either " << thenLabel << " or " << elseLabel << logging::endl;
    method.appendToEnd(new intermediate::Branch(method.findOrCreateLocal(TYPE_LABEL, thenLabel), COND_ZERO_SET, cond));
    if(!elseLabel.empty())
    {
    	method.appendToEnd(new intermediate::Branch(method.findOrCreateLocal(TYPE_LABEL, elseLabel),COND_ZERO_CLEAR, cond));
    }
    return true;
}

Switch::Switch(const Value& cond, const std::string& defaultLabel, const std::map<int, std::string>& cases) :
        cond(cond), defaultLabel(defaultLabel), jumpLabels(cases)
{

}

Switch::~Switch()
{

}

std::vector<const Local*> Switch::getAllLocals() const
{
    if(cond.hasType(ValueType::LOCAL))
        return {cond.local};
    return {};
}

bool Switch::mapInstruction(Method& method) const
{
    logging::debug() << "Generating branches for switch on " << cond.to_string() << " with " << jumpLabels.size() << " options and the default " << defaultLabel << logging::endl;
    for(const std::pair<int, std::string>& option : jumpLabels)
    {
        //for every case, if equal,branch to given label
        const Value tmp = method.addNewLocal(TYPE_BOOL, "%switch");
        method.appendToEnd(new intermediate::Comparison(intermediate::COMP_EQ, tmp, cond, Value(Literal(static_cast<long>(option.first)), TYPE_INT32)));
        method.appendToEnd(new intermediate::Branch(method.findOrCreateLocal(TYPE_LABEL, option.second), COND_ZERO_CLEAR, tmp));
    }
    //branch default label
    method.appendToEnd(new intermediate::Branch(method.findOrCreateLocal(TYPE_LABEL, defaultLabel), COND_ALWAYS, BOOL_TRUE));
    
    return true;
}
