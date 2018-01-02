/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "BitcodeReader.h"

#ifdef LLVM_INCLUDE_PATH

#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

#include <llvm-c/Core.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>

using namespace vc4c;
using namespace vc4c::llvm2qasm;

extern AddressSpace toAddressSpace(int num);

BitcodeReader::BitcodeReader(std::istream& stream, SourceType sourceType) : context()
{
	//required, since LLVM cannot read from std::istreams
	const std::string tmp((std::istreambuf_iterator<char>(stream)), (std::istreambuf_iterator<char>()));
	std::unique_ptr<llvm::MemoryBuffer> buf(llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(tmp)));
	llvm::SMDiagnostic error;
	if(sourceType == SourceType::LLVM_IR_BIN)
	{
		logging::debug() << "Reading LLVM module from bit-code..." << logging::endl;
		auto expected = llvm::parseBitcodeFile(buf->getMemBufferRef(), context);
		if(!expected)
		{
			llvm::Error error = expected.takeError();
			//TODO handle error
		}
		else
			llvmModule.swap(expected.get());
	}
	else if(sourceType == SourceType::LLVM_IR_TEXT)
	{
		logging::debug() << "Reading LLVM module from IR..." << logging::endl;
		llvmModule = llvm::parseIR(buf->getMemBufferRef(), error, context);
		//TODO handle error
	}
	else
		throw CompilationError(CompilationStep::PARSER, "Unhandled source-type for LLVM bitcode reader", std::to_string(static_cast<unsigned>(sourceType)));
}

static void extractKernelMetadata(Method& kernel, const llvm::Function& func)
{
	llvm::MDNode* metadata = func.getMetadata("kernel_arg_addr_space");
	if(metadata != nullptr)
	{
		//address spaces for kernel pointer arguments, e.g. "!2 = !{i32 1, i32 1}"
		for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
		{
			if(kernel.parameters.at(i).type.getPointerType())
			{
				const llvm::Metadata* operand = metadata->getOperand(i).get();
				if(operand->getMetadataID() == llvm::Metadata::ConstantAsMetadataKind)
				{
					const llvm::ConstantAsMetadata* constant = llvm::cast<const llvm::ConstantAsMetadata>(operand);
					kernel.parameters.at(i).type.getPointerType().value()->addressSpace = toAddressSpace(llvm::cast<const llvm::ConstantInt>(constant->getValue())->getSExtValue());
				}
				else
					throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
			}
		}
	}
	metadata = func.getMetadata("kernel_arg_access_qual");
	if(metadata != nullptr)
	{
		//access qualifiers for image arguments, e.g. "!3 = !{!"none", !"none"}"
		//XXX what to do with them? Only valid for images
	}
	metadata = func.getMetadata("kernel_arg_type");
	if(metadata != nullptr)
	{
		//original type-names for kernel arguments, e.g. "!4 = !{!"float*", !"float*"}"
		for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
		{
			const llvm::Metadata* operand = metadata->getOperand(i).get();
			if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
			{
				const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
				kernel.parameters.at(i).origTypeName = name->getString();
			}
			else
				throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
		}
	}
	metadata = func.getMetadata("kernel_arg_base_type");
	if(metadata != nullptr)
	{
		//base types, e.g. for type-defs, e.g. "!4 = !{!"float*", !"float*"}"
		//is not used
	}
	metadata = func.getMetadata("kernel_arg_type_qual");
	if(metadata != nullptr)
	{
		//additional type qualifiers, e.g. "!5 = !{!"", !""}"
		for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
		{
			const llvm::Metadata* operand = metadata->getOperand(i).get();
			if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
			{
				const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
				Parameter& param = kernel.parameters.at(i);
				if(name->getString().find("const") != std::string::npos)
					param.decorations = add_flag(param.decorations, ParameterDecorations::READ_ONLY);
				if(name->getString().find("restrict") != std::string::npos)
					param.decorations = add_flag(param.decorations, ParameterDecorations::RESTRICT);
				if(name->getString().find("volatile") != std::string::npos)
					param.decorations = add_flag(param.decorations, ParameterDecorations::VOLATILE);
			}
			else
				throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
		}
	}
	metadata = func.getMetadata("kernel_arg_name");
	if(metadata != nullptr)
	{
		//the original argument names, e.g. "!6 = !{!"a", !"b"}"
		for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
		{
			const llvm::Metadata* operand = metadata->getOperand(i).get();
			if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
			{
				const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
				kernel.parameters.at(i).parameterName = name->getString();
			}
			else
				throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
		}
	}
}

void BitcodeReader::parse(Module& module)
{
	const llvm::Module::FunctionListType& functions = llvmModule->getFunctionList();
	for(const llvm::Function& func : functions)
	{
		if(func.getCallingConv() == llvm::CallingConv::SPIR_KERNEL)
		{
			logging::debug() << "Found SPIR kernel-function: " << func.getName() << logging::endl;
			Method& kernelFunc = parseFunction(module, func);
			extractKernelMetadata(kernelFunc, func);
			kernelFunc.isKernel = true;
		}
	}
}

static DataType toDataType(const llvm::Type* type)
{
	if(type == nullptr || type->isEmptyTy())
		return TYPE_UNKNOWN;
	if(type->isVectorTy())
	{
		return toDataType(type->getVectorElementType()).toVectorType(static_cast<unsigned char>(llvm::cast<const llvm::VectorType>(type)->getVectorNumElements()));
	}
	if(type->isVoidTy())
		return TYPE_VOID;
	if(type->isHalfTy())
		return TYPE_HALF;
	if(type->isFloatTy())
		return TYPE_FLOAT;
	if(type->isLabelTy())
		return TYPE_LABEL;
	if(type->isIntegerTy(8))
		return TYPE_INT8;
	if(type->isIntegerTy(16))
		return TYPE_INT16;
	if(type->isIntegerTy(32))
		return TYPE_INT32;
	if(type->isIntegerTy(64))
		return TYPE_INT64;
	if(type->isStructTy())
	{
		std::vector<DataType> elementTypes;
		for(unsigned i = 0; i < type->getStructNumElements(); ++i)
		{
			elementTypes.emplace_back(toDataType(type->getStructElementType(i)));
		}
		std::shared_ptr<ComplexType> structType(new StructType(elementTypes, llvm::cast<const llvm::StructType>(type)->isPacked()));
		return DataType(type->getStructName(), 1, structType);
	}
	if(type->isArrayTy())
	{
		const DataType elementType = toDataType(type->getArrayElementType());
		std::shared_ptr<ComplexType> c(new ArrayType(elementType, type->getArrayNumElements()));
		return DataType((elementType.to_string() + "[") + std::to_string(type->getArrayNumElements()) +"]", 1, c);
	}
	if(type->isPointerTy())
	{
		DataType pointerType = toDataType(type->getPointerElementType()).toPointerType();
		pointerType.getPointerType().value()->addressSpace = toAddressSpace(type->getPointerAddressSpace());
		return pointerType;
	}
	throw CompilationError(CompilationStep::PARSER, "Unknown LLVM type", std::to_string(type->getTypeID()));
}

static ParameterDecorations toParameterDecorations(const llvm::Argument& arg)
{
	ParameterDecorations deco = ParameterDecorations::NONE;
	if(arg.hasAttribute(llvm::Attribute::SExt))
		deco = add_flag(deco, ParameterDecorations::SIGN_EXTEND);
	if(arg.hasAttribute(llvm::Attribute::ZExt))
		deco = add_flag(deco, ParameterDecorations::ZERO_EXTEND);
	if(arg.hasAttribute(llvm::Attribute::NoAlias))
		deco = add_flag(deco, ParameterDecorations::RESTRICT);
	return deco;
}

Method& BitcodeReader::parseFunction(Module& module, const llvm::Function& func)
{
	if(parsedFunctions.find(&func) != parsedFunctions.end())
		return *parsedFunctions.at(&func).first;

	Method* method = new Method(module);
	module.methods.emplace_back(method);
	parsedFunctions[&func] = std::make_pair(method, LLVMInstructionList{});

	method->name = func.getName();
	method->returnType = toDataType(func.getReturnType());

	logging::debug() << "Reading function " << method->returnType.to_string() << " " << method->name << "(...)" << logging::endl;

	for(const llvm::Argument& arg : func.getArgumentList())
	{
		method->parameters.emplace_back(Parameter(arg.getName(), toDataType(arg.getType()), toParameterDecorations(arg)));
		logging::debug() << "Reading parameter " << method->parameters.back().to_string() << logging::endl;
	}

	parseFunctionBody(module, *method, parsedFunctions.at(&func).second, func);

	return *method;
}

void BitcodeReader::parseFunctionBody(Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Function& func)
{
	for(const llvm::BasicBlock& block : func)
	{
		// need to extract label from basic block
		instructions.emplace_back(new LLVMLabel(toValue(method, &block).local->name));
		for(const llvm::Instruction& inst : block)
		{
			parseInstruction(module, method, instructions, inst);
		}
	}
}

static intermediate::InstructionDecorations toInstructionDecorations(const llvm::Instruction& inst)
{
	intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;

	if(inst.hasNoNaNs())
		deco = add_flag(deco, intermediate::InstructionDecorations::NO_NAN);
	if(inst.hasNoInfs())
		deco = add_flag(deco, intermediate::InstructionDecorations::NO_INF);
	if(inst.hasAllowReciprocal())
		deco = add_flag(deco, intermediate::InstructionDecorations::ALLOW_RECIP);
	if(inst.hasUnsafeAlgebra())
		//XXX correct?
		deco = add_flag(deco, intermediate::InstructionDecorations::FAST_MATH);

	return deco;
}

static std::pair<std::string, bool> toComparison(llvm::CmpInst::Predicate pred)
{
	using P = llvm::CmpInst::Predicate;
	switch(pred)
	{
		case P::FCMP_FALSE:
			return std::make_pair("false", true);
		case P::FCMP_OEQ:
			return std::make_pair("oeq", true);
		case P::FCMP_OGE:
			return std::make_pair("oge", true);
		case P::FCMP_OGT:
			return std::make_pair("ogt", true);
		case P::FCMP_OLE:
			return std::make_pair("ole", true);
		case P::FCMP_OLT:
			return std::make_pair("olt", true);
		case P::FCMP_ONE:
			return std::make_pair("one", true);
		case P::FCMP_ORD:
			return std::make_pair("ord", true);
		case P::FCMP_TRUE:
			return std::make_pair("true", true);
		case P::FCMP_UEQ:
			return std::make_pair("ueq", true);
		case P::FCMP_UGE:
			return std::make_pair("uge", true);
		case P::FCMP_UGT:
			return std::make_pair("ugt", true);
		case P::FCMP_ULE:
			return std::make_pair("ule", true);
		case P::FCMP_ULT:
			return std::make_pair("ult", true);
		case P::FCMP_UNE:
			return std::make_pair("une", true);
		case P::FCMP_UNO:
			return std::make_pair("uno", true);
		case P::ICMP_EQ:
			return std::make_pair("eq", false);
		case P::ICMP_NE:
			return std::make_pair("neq", false);
		case P::ICMP_SGE:
			return std::make_pair("sge", false);
		case P::ICMP_SGT:
			return std::make_pair("sgt", false);
		case P::ICMP_SLE:
			return std::make_pair("sle", false);
		case P::ICMP_SLT:
			return std::make_pair("slt", false);
		case P::ICMP_UGE:
			return std::make_pair("uge", false);
		case P::ICMP_UGT:
			return std::make_pair("ugt", false);
		case P::ICMP_ULE:
			return std::make_pair("ule", false);
		case P::ICMP_ULT:
			return std::make_pair("ult", false);
		default:throw CompilationError(CompilationStep::PARSER, "Unhandled comparison predicate", std::to_string(pred));
	}
}

void BitcodeReader::parseInstruction(Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Instruction& inst)
{
	using TermOps = llvm::Instruction::TermOps;
	using BinaryOps = llvm::Instruction::BinaryOps;
	using MemoryOps = llvm::Instruction::MemoryOps;
	using CastOps = llvm::Instruction::CastOps;
	using OtherOps = llvm::Instruction::OtherOps;

	intermediate::InstructionDecorations deco = toInstructionDecorations(inst);

	switch(inst.getOpcode())
	{
		case TermOps::Br:
		{
			const llvm::BranchInst* br = llvm::cast<const llvm::BranchInst>(&inst);
			if(br->isUnconditional())
				instructions.emplace_back(new Branch(toValue(method, br->getSuccessor(0)).local->name));
			else
				instructions.emplace_back(new Branch(toValue(method, br->getCondition()), toValue(method, br->getSuccessor(0)).local->name, toValue(method, br->getSuccessor(1)).local->name));
			instructions.back()->setDecorations(deco);
			break;
		}
		case TermOps::Ret:
		{
			const llvm::ReturnInst* ret = llvm::cast<const llvm::ReturnInst>(&inst);
			if(ret->getReturnValue() == nullptr)
				instructions.emplace_back(new ValueReturn());
			else
				instructions.emplace_back(new ValueReturn(toValue(method, ret->getReturnValue())));
			instructions.back()->setDecorations(deco);
			break;
		}
		case TermOps::Switch:
		{
			const llvm::SwitchInst* switchIns = llvm::cast<const llvm::SwitchInst>(&inst);
			const Value cond = toValue(method, switchIns->getCondition());
			const Value defaultLabel = toValue(method, switchIns->getDefaultDest());
			std::map<int, std::string> caseLabels;
			for(auto& casePair : const_cast<llvm::SwitchInst*>(switchIns)->cases())
			{
				caseLabels.emplace(static_cast<int>(casePair.getCaseValue()->getSExtValue()), toValue(method, casePair.getCaseSuccessor()).local->name);
			}
			instructions.emplace_back(new Switch(cond, defaultLabel.local->name, caseLabels));
			instructions.back()->setDecorations(deco);
			break;
		}
		case BinaryOps::AShr: //fall-through
		case BinaryOps::Add:  //fall-through
		case BinaryOps::And:  //fall-through
		case BinaryOps::FAdd: //fall-through
		case BinaryOps::FDiv: //fall-through
		case BinaryOps::FMul: //fall-through
		case BinaryOps::FRem: //fall-through
		case BinaryOps::FSub: //fall-through
		case BinaryOps::LShr: //fall-through
		case BinaryOps::Mul:  //fall-through
		case BinaryOps::Or:   //fall-through
		case BinaryOps::SDiv: //fall-through
		case BinaryOps::SRem: //fall-through
		case BinaryOps::Shl:  //fall-through
		case BinaryOps::Sub:  //fall-through
		case BinaryOps::UDiv: //fall-through
		case BinaryOps::URem: //fall-through
		case BinaryOps::Xor:  //fall-through
		{
			const llvm::BinaryOperator* binOp = llvm::cast<const llvm::BinaryOperator>(&inst);
			instructions.emplace_back(new BinaryOperator(binOp->getOpcodeName(), toValue(method, binOp), toValue(method, binOp->getOperand(0)), toValue(method, binOp->getOperand(1))));
			instructions.back()->setDecorations(deco);
			break;
		}
		case MemoryOps::Alloca:
		{
			const llvm::AllocaInst* alloca = llvm::cast<const llvm::AllocaInst>(&inst);
			const DataType contentType = toDataType(alloca->getAllocatedType());
			const DataType pointerType = toDataType(alloca->getType());
			unsigned alignment = alloca->getAlignment();
			//XXX need to heed the array-size?
			method.stackAllocations.emplace(StackAllocation(alloca->getName(), pointerType, contentType.getPhysicalWidth(), alignment));
			break;
		}
		case MemoryOps::GetElementPtr:
		{
			const llvm::GetElementPtrInst* indexOf = llvm::cast<const llvm::GetElementPtrInst>(&inst);
			std::vector<Value> indices;
			std::for_each(indexOf->idx_begin(), indexOf->idx_end(), [this,&method,&indices](const llvm::Value* val) -> void {indices.emplace_back(toValue(method, val));});
			instructions.emplace_back(new IndexOf(toValue(method, indexOf).local, toValue(method, indexOf->getPointerOperand()), indices));
			instructions.back()->setDecorations(deco);
			break;
		}
		case MemoryOps::Load:
		{
			const llvm::LoadInst* load = llvm::cast<const llvm::LoadInst>(&inst);
			Value src = toValue(method, load->getPointerOperand());
			if(load->isVolatile() && src.hasType(ValueType::LOCAL) && src.local->is<Parameter>())
				src.local->as<Parameter>()->decorations = add_flag(src.local->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
			instructions.emplace_back(new Copy(toValue(method, load), src, true, true));
			instructions.back()->setDecorations(deco);
			break;
		}
		case MemoryOps::Store:
		{
			const llvm::StoreInst* store = llvm::cast<const llvm::StoreInst>(&inst);
			Value src = toValue(method, store->getPointerOperand());
			if(store->isVolatile() && src.hasType(ValueType::LOCAL) && src.local->is<Parameter>())
				src.local->as<Parameter>()->decorations = add_flag(src.local->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
			instructions.emplace_back(new Copy(toValue(method, store), src, true, false));
			instructions.back()->setDecorations(deco);
			break;
		}
		case CastOps::AddrSpaceCast: //fall-through
		case CastOps::BitCast:
			instructions.emplace_back(new Copy(toValue(method, &inst), toValue(method, inst.getOperand(0))));
			instructions.back()->setDecorations(deco);
			break;
		case CastOps::FPExt:
		case CastOps::FPToSI:
		case CastOps::FPToUI:
		case CastOps::FPTrunc:
		case CastOps::IntToPtr:
		case CastOps::PtrToInt:
		case CastOps::SExt:
		case CastOps::SIToFP:
		case CastOps::Trunc:
		case CastOps::UIToFP:
		case CastOps::ZExt:
		case OtherOps::Call:
		{
			const llvm::CallInst* call = llvm::cast<const llvm::CallInst>(&inst);
			Method& dest = parseFunction(module, *call->getFunction());
			std::vector<Value> args;
			std::for_each(call->arg_begin(), call->arg_end(), [this, &method, &args](const llvm::Value* val) -> void { args.emplace_back(toValue(method, val));});
			instructions.emplace_back(new CallSite(toValue(method, call).local, dest, args));
			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::ExtractElement:
		case OtherOps::ExtractValue:
			break;
		case OtherOps::FCmp: //fall-through
		case OtherOps::ICmp:
		{
			const llvm::CmpInst* comp = llvm::cast<const llvm::CmpInst>(&inst);
			const auto tmp = toComparison(comp->getPredicate());
			instructions.emplace_back(new Comparison(toValue(method, &inst).local, tmp.first, toValue(method, inst.getOperand(0)), toValue(method, inst.getOperand(1)), tmp.second));
			instructions.back()->setDecorations(deco);
			break;
		}

		case OtherOps::InsertElement: //fall-through
		case OtherOps::InsertValue:
			//TODO correct for either instruction type?
			//XXX multi-level indices!
			instructions.emplace_back(new ContainerInsertion(toValue(method, &inst).local, toValue(method, inst.getOperand(0)), toValue(method, inst.getOperand(1)), toValue(method, inst.getOperand(2))));
			instructions.back()->setDecorations(deco);
			break;
		case OtherOps::PHI:
		{
			const llvm::PHINode* phi = llvm::cast<const llvm::PHINode>(&inst);
			std::vector<std::pair<Value, const Local*> > labels;
			for(unsigned i = 0; i < phi->getNumIncomingValues(); ++i)
			{
				labels.emplace_back(std::make_pair(toValue(method, phi->getIncomingValue(i)), toValue(method, phi->getIncomingBlock(i)).local));
			}
			instructions.emplace_back(new PhiNode(toValue(method, phi).local, labels));
			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::Select:
		{
			const llvm::SelectInst* selection = llvm::cast<const llvm::SelectInst>(&inst);
			instructions.emplace_back(new Selection(toValue(method, selection).local, toValue(method, selection->getCondition()), toValue(method, selection->getTrueValue()), toValue(method, selection->getFalseValue())));
			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::ShuffleVector:
		{
			const llvm::ShuffleVectorInst* shuffle = llvm::cast<const llvm::ShuffleVectorInst>(&inst);
			instructions.emplace_back(new ShuffleVector(toValue(method, shuffle), toValue(method, shuffle->getOperand(0)), toValue(method, shuffle->getOperand(1)), toValue(method, shuffle->getMask())));
			instructions.back()->setDecorations(deco);
			break;
		}
		default:
			throw CompilationError(CompilationStep::PARSER, "Unhandled LLVM op-code", inst.getOpcodeName());
	}
}

Value BitcodeReader::toValue(Method& method, const llvm::Value* val)
{
	if(labelMap.find(val) != labelMap.end())
	{
		return labelMap.at(val)->createReference();
	}
	if(llvm::dyn_cast<const llvm::BranchInst>(val) != nullptr)
	{
		//label
		Local* loc = method.addNewLocal(TYPE_LABEL, "%label").local;
		labelMap[val] = loc;
		return loc->createReference();
	}
	const DataType type = toDataType(val->getType());
	if(llvm::dyn_cast<const llvm::Constant>(val) != nullptr)
	{
		//TODO literal constants (float, integer, vectors, structs/arrays?)
	}
	//other locals
	return method.findOrCreateLocal(type, val->getName())->createReference();
}
#endif
