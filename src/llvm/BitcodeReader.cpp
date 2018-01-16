/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "BitcodeReader.h"

#ifdef USE_LLVM_LIBRARY

#define SPIRV_LLVM_LIBRARY 2
#define DEFAULT_LLVM_LIBRARY 1

#include "../intermediate/IntermediateInstruction.h"
#include "../intrinsics/Images.h"
#include "log.h"

#include "llvm-c/Core.h"
#if USE_LLVM_LIBRARY == SPIRV_LLVM_LIBRARY /* SPIR-V LLVM */
#include "llvm/Bitcode/ReaderWriter.h"
#else
#include "llvm/Bitcode/BitcodeReader.h"
#endif
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"

#include <system_error>

using namespace vc4c;
using namespace vc4c::llvm2qasm;

extern AddressSpace toAddressSpace(int num);
extern std::string cleanMethodName(const std::string& name);

BitcodeReader::BitcodeReader(std::istream& stream, SourceType sourceType) : context()
{
	//required, since LLVM cannot read from std::istreams
	const std::string tmp((std::istreambuf_iterator<char>(stream)), (std::istreambuf_iterator<char>()));
	std::unique_ptr<llvm::MemoryBuffer> buf(llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(tmp)));
	if(sourceType == SourceType::LLVM_IR_BIN)
	{
		logging::debug() << "Reading LLVM module from bit-code..." << logging::endl;
		auto expected = llvm::parseBitcodeFile(buf->getMemBufferRef(), context);
		if(!expected)
		{
#if USE_LLVM_LIBRARY == SPIRV_LLVM_LIBRARY /* SPIR-V LLVM */
			throw std::system_error(expected.getError(), "Error parsing LLVM module");
#else
			throw std::system_error(llvm::errorToErrorCode(expected.takeError()), "Error parsing LLVM module");
#endif
		}
		else
		{
			//expected.get() is either std::unique_ptr<llvm::Module> or llvm::Module*
			std::unique_ptr<llvm::Module> tmp(std::move(expected.get()));
			llvmModule.swap(tmp);
		}
	}
	else if(sourceType == SourceType::LLVM_IR_TEXT)
	{
		logging::debug() << "Reading LLVM module from IR..." << logging::endl;
		llvm::SMDiagnostic error;
		llvmModule = llvm::parseIR(buf->getMemBufferRef(), error, context);
		if(!llvmModule)
			throw CompilationError(CompilationStep::PARSER, "Error parsing LLVM IR module", error.getMessage());
	}
	else
		throw CompilationError(CompilationStep::PARSER, "Unhandled source-type for LLVM bitcode reader", std::to_string(static_cast<unsigned>(sourceType)));
}

#if USE_LLVM_LIBRARY == SPIRV_LLVM_LIBRARY /* SPIR-V LLVM */
static void extractKernelMetadata(Method& kernel, const llvm::Function& func, const llvm::Module& llvmModule, const llvm::LLVMContext& context)
{
	llvm::NamedMDNode* kernelsMetaData = llvmModule.getNamedMetadata("opencl.kernels");
	if(kernelsMetaData != nullptr)
	{
		//each kernel is a single meta-data entry
		for(const llvm::MDNode* entry : kernelsMetaData->operands())
		{
			//each kernel-entry has the the function as well as additional links to the kernel meta-data
			const llvm::Metadata* function = entry->getOperand(0).get();
			if(function->getMetadataID() == llvm::Metadata::ConstantAsMetadataKind && llvm::cast<const llvm::ConstantAsMetadata>(function)->getValue() == &func)
			{
				for(unsigned i = 1; i < entry->getNumOperands(); ++i)
				{
					const llvm::MDTuple* node = llvm::cast<const llvm::MDTuple>(entry->getOperand(i).get());
					if(node->getOperand(0)->getMetadataID() == llvm::Metadata::MDStringKind && llvm::cast<const llvm::MDString>(node->getOperand(0).get())->getString() == "kernel_arg_addr_space")
					{
						//address spaces for kernel pointer arguments, e.g. "!1 = !{!"kernel_arg_addr_space", i32 1, i32 1}"
						for(unsigned i = 1; i < node->getNumOperands(); ++i)
						{
							if(kernel.parameters.at(i - 1).type.getPointerType())
							{
								const llvm::Metadata* operand = node->getOperand(i).get();
								if(operand->getMetadataID() == llvm::Metadata::ConstantAsMetadataKind)
								{
									const llvm::ConstantAsMetadata* constant = llvm::cast<const llvm::ConstantAsMetadata>(operand);
									kernel.parameters.at(i - 1).type.getPointerType().value()->addressSpace = toAddressSpace(static_cast<int>(llvm::cast<const llvm::ConstantInt>(constant->getValue())->getSExtValue()));
								}
								else
									throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
							}
						}
					}
					else if(node->getOperand(0)->getMetadataID() == llvm::Metadata::MDStringKind && llvm::cast<const llvm::MDString>(node->getOperand(0).get())->getString() == "kernel_arg_access_qual")
					{
						//access qualifiers for image arguments, e.g. "!2 = !{!"kernel_arg_access_qual", !"none", !"none"}"
					}
					else if(node->getOperand(0)->getMetadataID() == llvm::Metadata::MDStringKind && llvm::cast<const llvm::MDString>(node->getOperand(0).get())->getString() == "kernel_arg_type")
					{
						//original type-names for kernel arguments, e.g. "!3 = !{!"kernel_arg_type", !"float*", !"float*"}"
						for(unsigned i = 1; i < node->getNumOperands(); ++i)
						{
							const llvm::Metadata* operand = node->getOperand(i).get();
							if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
							{
								const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
								kernel.parameters.at(i - 1).origTypeName = name->getString();
							}
							else
								throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
						}
					}
					else if(node->getOperand(0)->getMetadataID() == llvm::Metadata::MDStringKind && llvm::cast<const llvm::MDString>(node->getOperand(0).get())->getString() == "kernel_arg_type_qual")
					{
						//additional type qualifiers, e.g. "!5 = !{!"kernel_arg_type_qual", !"", !""}"
						for(unsigned i = 1; i < node->getNumOperands(); ++i)
						{
							const llvm::Metadata* operand = node->getOperand(i).get();
							if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
							{
								const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
								Parameter& param = kernel.parameters.at(i - 1);
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
					else if(node->getOperand(0)->getMetadataID() == llvm::Metadata::MDStringKind && llvm::cast<const llvm::MDString>(node->getOperand(0).get())->getString() == "kernel_arg_name")
					{
						//the original argument names, e.g. "!6 = !{!"kernel_arg_name", !"a", !"b"}"
						for(unsigned i = 1; i < node->getNumOperands(); ++i)
						{
							const llvm::Metadata* operand = node->getOperand(i).get();
							if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
							{
								const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
								kernel.parameters.at(i - 1).parameterName = name->getString();
							}
							else
								throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
						}
					}
				}
			}
		}
	}
}
#else /* "standard" LLVM */
static void extractKernelMetadata(Method& kernel, const llvm::Function& func, const llvm::Module& llvmModule, const llvm::LLVMContext& context)
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
#endif

void BitcodeReader::parse(Module& module)
{
	const llvm::Module::FunctionListType& functions = llvmModule->getFunctionList();

	//The global data is explicitly not read here, but on #toConstant() only resolving global data actually used

	//parse functions
	//Starting with kernel-functions, recursively parse all included functions (and only those)
	for(const llvm::Function& func : functions)
	{
		if(func.getCallingConv() == llvm::CallingConv::SPIR_KERNEL)
		{
			logging::debug() << "Found SPIR kernel-function: " << func.getName() << logging::endl;
			Method& kernelFunc = parseFunction(module, func);
			extractKernelMetadata(kernelFunc, func, *llvmModule.get(), context);
			kernelFunc.isKernel = true;
		}
	}

	//map instructions to intermediate representation
	for(auto& method : parsedFunctions)
	{
		logging::debug() << "Mapping function '" << method.second.first->name << "'..." << logging::endl;
		for(LLVMInstructionList::value_type& inst : method.second.second)
		{
			inst->mapInstruction(*method.second.first);
		}
	}
}

static DataType& addToMap(DataType&& dataType, const llvm::Type* type, FastMap<const llvm::Type*, DataType>& typesMap)
{
	return typesMap.emplace(type, dataType).first->second;
}

DataType BitcodeReader::toDataType(const llvm::Type* type)
{
	if(type == nullptr)
		return TYPE_UNKNOWN;
	if(typesMap.find(type) != typesMap.end())
		return typesMap.at(type);
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
	if(type->isDoubleTy())
		return TYPE_DOUBLE;
	if(type->isLabelTy())
		return TYPE_LABEL;
	if(type->isIntegerTy(1))
		return TYPE_BOOL;
	if(type->isIntegerTy(8))
		return TYPE_INT8;
	if(type->isIntegerTy(16))
		return TYPE_INT16;
	if(type->isIntegerTy(32))
		return TYPE_INT32;
	if(type->isIntegerTy(64))
		return TYPE_INT64;
	if(type->isPointerTy() && type->getPointerElementType()->isStructTy())
	{
		//recognize image types - taken from https://github.com/KhronosGroup/SPIRV-LLVM/blob/khronos/spirv-3.6.1/lib/SPIRV/SPIRVUtil.cpp (#isOCLImageType)
		const llvm::StructType* str = llvm::cast<const llvm::StructType>(type->getPointerElementType());
		if(str->isOpaque() && str->getName().find("opencl.image") == 0)
		{
			ImageType* imageType = new ImageType();
			imageType->dimensions = str->getName().find('3') != llvm::StringRef::npos ? 3 : str->getName().find('2') != llvm::StringRef::npos ? 2 : 1;
			imageType->isImageArray = str->getName().find("array") != llvm::StringRef::npos;
			imageType->isImageBuffer = str->getName().find("buffer") != llvm::StringRef::npos;
			imageType->isSampled = false;

			std::shared_ptr<ComplexType> c(imageType);
			return addToMap(DataType(imageType->getImageTypeName(), 1, c), type, typesMap);
		}
	}
	if(type->isStructTy())
	{
		//detect SPIRV sampler-type
		if(type->getStructName() == "spirv.Sampler" || type->getStructName() == "spirv.ConstantSampler")
		{
			return TYPE_SAMPLER;
		}
		//need to be added to the map before iterating over the children to prevent stack-overflow
		DataType& dataType = addToMap(DataType(type->getStructName(), 1), type, typesMap);
		std::vector<DataType> elementTypes;
		elementTypes.reserve(type->getStructNumElements());
		for(unsigned i = 0; i < type->getStructNumElements(); ++i)
		{
			elementTypes.emplace_back(toDataType(type->getStructElementType(i)));
		}
		std::shared_ptr<ComplexType> structType(new StructType(elementTypes, llvm::cast<const llvm::StructType>(type)->isPacked()));
		dataType.complexType = structType;
		return dataType;
	}
	if(type->isArrayTy())
	{
		const DataType elementType = toDataType(type->getArrayElementType());
		std::shared_ptr<ComplexType> c(new ArrayType(elementType, static_cast<unsigned>(type->getArrayNumElements())));
		return addToMap(DataType((elementType.to_string() + "[") + std::to_string(type->getArrayNumElements()) +"]", 1, c), type, typesMap);
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
	if(arg.hasSExtAttr())
		deco = add_flag(deco, ParameterDecorations::SIGN_EXTEND);
	if(arg.hasZExtAttr())
		deco = add_flag(deco, ParameterDecorations::ZERO_EXTEND);
	if(arg.hasNoAliasAttr())
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

	method->name = cleanMethodName(func.getName());
	method->returnType = toDataType(func.getReturnType());

	logging::debug() << "Reading function " << method->returnType.to_string() << " " << method->name << "(...)" << logging::endl;

	method->parameters.reserve(func.getArgumentList().size());
	for(const llvm::Argument& arg : func.getArgumentList())
	{
		method->parameters.emplace_back(Parameter((std::string("%") + arg.getName()).str(), toDataType(arg.getType()), toParameterDecorations(arg)));
		logging::debug() << "Reading parameter " << method->parameters.back().to_string() << logging::endl;
		if(method->parameters.back().type.getImageType())
			intermediate::reserveImageConfiguration(module, method->parameters.back().createReference());
	}

	parseFunctionBody(module, *method, parsedFunctions.at(&func).second, func);

	return *method;
}

void BitcodeReader::parseFunctionBody(Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Function& func)
{
	for(const llvm::BasicBlock& block : func)
	{
		// need to extract label from basic block
		instructions.emplace_back(new LLVMLabel(toValue(method, &block).local));
		for(const llvm::Instruction& inst : block)
		{
			parseInstruction(module, method, instructions, inst);
		}
	}
}

static intermediate::InstructionDecorations toInstructionDecorations(const llvm::Instruction& inst)
{
	intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;

	if(llvm::isa<llvm::FPMathOperator>(&inst) && inst.hasNoNaNs())
		deco = add_flag(deco, intermediate::InstructionDecorations::NO_NAN);
	if(llvm::isa<llvm::FPMathOperator>(&inst) && inst.hasNoInfs())
		deco = add_flag(deco, intermediate::InstructionDecorations::NO_INF);
	if(llvm::isa<llvm::FPMathOperator>(&inst) && inst.hasAllowReciprocal())
		deco = add_flag(deco, intermediate::InstructionDecorations::ALLOW_RECIP);
	if(llvm::isa<llvm::FPMathOperator>(&inst) && inst.hasUnsafeAlgebra())
		deco = add_flag(deco, intermediate::InstructionDecorations::FAST_MATH);
	return deco;
}

static std::pair<std::string, bool> toComparison(llvm::CmpInst::Predicate pred)
{
	using P = llvm::CmpInst::Predicate;
	switch(pred)
	{
		case P::FCMP_FALSE:
			return std::make_pair(intermediate::COMP_FALSE, true);
		case P::FCMP_OEQ:
			return std::make_pair(intermediate::COMP_ORDERED_EQ, true);
		case P::FCMP_OGE:
			return std::make_pair(intermediate::COMP_ORDERED_GE, true);
		case P::FCMP_OGT:
			return std::make_pair(intermediate::COMP_ORDERED_GT, true);
		case P::FCMP_OLE:
			return std::make_pair(intermediate::COMP_ORDERED_LE, true);
		case P::FCMP_OLT:
			return std::make_pair(intermediate::COMP_ORDERED_LT, true);
		case P::FCMP_ONE:
			return std::make_pair(intermediate::COMP_ORDERED_NEQ, true);
		case P::FCMP_ORD:
			return std::make_pair(intermediate::COMP_ORDERED, true);
		case P::FCMP_TRUE:
			return std::make_pair(intermediate::COMP_TRUE, true);
		case P::FCMP_UEQ:
			return std::make_pair(intermediate::COMP_UNORDERED_EQ, true);
		case P::FCMP_UGE:
			return std::make_pair(intermediate::COMP_UNORDERED_GE, true);
		case P::FCMP_UGT:
			return std::make_pair(intermediate::COMP_UNORDERED_GT, true);
		case P::FCMP_ULE:
			return std::make_pair(intermediate::COMP_UNORDERED_LE, true);
		case P::FCMP_ULT:
			return std::make_pair(intermediate::COMP_UNORDERED_LT, true);
		case P::FCMP_UNE:
			return std::make_pair(intermediate::COMP_UNORDERED_NEQ, true);
		case P::FCMP_UNO:
			return std::make_pair(intermediate::COMP_UNORDERED, true);
		case P::ICMP_EQ:
			return std::make_pair(intermediate::COMP_EQ, false);
		case P::ICMP_NE:
			return std::make_pair(intermediate::COMP_NEQ, false);
		case P::ICMP_SGE:
			return std::make_pair(intermediate::COMP_SIGNED_GE, false);
		case P::ICMP_SGT:
			return std::make_pair(intermediate::COMP_SIGNED_GT, false);
		case P::ICMP_SLE:
			return std::make_pair(intermediate::COMP_SIGNED_LE, false);
		case P::ICMP_SLT:
			return std::make_pair(intermediate::COMP_SIGNED_LT, false);
		case P::ICMP_UGE:
			return std::make_pair(intermediate::COMP_UNSIGNED_GE, false);
		case P::ICMP_UGT:
			return std::make_pair(intermediate::COMP_UNSIGNED_GT, false);
		case P::ICMP_ULE:
			return std::make_pair(intermediate::COMP_UNSIGNED_LE, false);
		case P::ICMP_ULT:
			return std::make_pair(intermediate::COMP_UNSIGNED_LT, false);
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
				instructions.emplace_back(new Branch(toValue(method, br->getSuccessor(0)).local));
			else
				instructions.emplace_back(new Branch(toValue(method, br->getCondition()), toValue(method, br->getSuccessor(0)).local, toValue(method, br->getSuccessor(1)).local));
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
			method.stackAllocations.emplace(StackAllocation(("%" + alloca->getName()).str(), pointerType, contentType.getPhysicalWidth(), alignment));
			break;
		}
		case MemoryOps::GetElementPtr:
		{
			const llvm::GetElementPtrInst* indexOf = llvm::cast<const llvm::GetElementPtrInst>(&inst);
			std::vector<Value> indices;
			std::for_each(indexOf->idx_begin(), indexOf->idx_end(), [this,&method,&indices](const llvm::Value* val) -> void {indices.emplace_back(toValue(method, val));});
			instructions.emplace_back(new IndexOf(toValue(method, indexOf), toValue(method, indexOf->getPointerOperand()), indices));
			instructions.back()->setDecorations(deco);
			break;
		}
		case MemoryOps::Load:
		{
			const llvm::LoadInst* load = llvm::cast<const llvm::LoadInst>(&inst);
			Value src = UNDEFINED_VALUE;
			if(load->getPointerOperand()->getValueID() == llvm::Constant::ConstantExprVal)
			{
				//the source is given as an in-line getelementptr instruction, insert as extra instruction calculating indices
				llvm::ConstantExpr* constExpr = const_cast<llvm::ConstantExpr*>(llvm::cast<const llvm::ConstantExpr>(load->getPointerOperand()));
				if(constExpr->getOpcode() != llvm::Instruction::MemoryOps::GetElementPtr)
					throw CompilationError(CompilationStep::PARSER, "Invalid constant operation for load-instruction!");
				llvm::GetElementPtrInst* indexOf = llvm::cast<llvm::GetElementPtrInst>(constExpr->getAsInstruction());
				parseInstruction(module, method, instructions, *indexOf);
				src = toValue(method, indexOf);
				//required so LLVM can clean up the constant expression correctly
				indexOf->dropAllReferences();
			}
			else
				src = toValue(method, load->getPointerOperand());
			if(load->isVolatile() && src.hasType(ValueType::LOCAL) && src.local->is<Parameter>())
				src.local->as<Parameter>()->decorations = add_flag(src.local->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
			instructions.emplace_back(new Copy(toValue(method, load), src, true, true));
			instructions.back()->setDecorations(deco);
			break;
		}
		case MemoryOps::Store:
		{
			const llvm::StoreInst* store = llvm::cast<const llvm::StoreInst>(&inst);
			Value dest = UNDEFINED_VALUE;
			if(store->getPointerOperand()->getValueID() == llvm::Constant::ConstantExprVal)
			{
				//the destination is given as an in-line getelementptr instruction, insert as extra instruction
				llvm::ConstantExpr* constExpr = const_cast<llvm::ConstantExpr*>(llvm::cast<const llvm::ConstantExpr>(store->getPointerOperand()));
				if(constExpr->getOpcode() != llvm::Instruction::MemoryOps::GetElementPtr)
					throw CompilationError(CompilationStep::PARSER, "Invalid constant operation for store-instruction!");
				llvm::GetElementPtrInst* indexOf = llvm::cast<llvm::GetElementPtrInst>(constExpr->getAsInstruction());
				parseInstruction(module, method, instructions, *indexOf);
				dest = toValue(method, indexOf);
				//required so LLVM can clean up the constant expression correctly
				indexOf->dropAllReferences();
			}
			else
				dest = toValue(method, store->getPointerOperand());
			if(store->isVolatile() && dest.hasType(ValueType::LOCAL) && dest.local->is<Parameter>())
				dest.local->as<Parameter>()->decorations = add_flag(dest.local->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
			instructions.emplace_back(new Copy(dest, toValue(method, store->getValueOperand()), true, false));
			instructions.back()->setDecorations(deco);
			break;
		}
		case CastOps::AddrSpaceCast: //fall-through
		case CastOps::BitCast:
		{
			instructions.emplace_back(new Copy(toValue(method, &inst), toValue(method, inst.getOperand(0)), false, false, true));
			instructions.back()->setDecorations(deco);
			break;
		}
		case CastOps::FPExt:    //fall-through
		case CastOps::FPToSI:   //fall-through
		case CastOps::FPToUI:   //fall-through
		case CastOps::FPTrunc:  //fall-through
		case CastOps::IntToPtr: //fall-through
		case CastOps::PtrToInt: //fall-through
		case CastOps::SExt:     //fall-through
		case CastOps::SIToFP:   //fall-through
		case CastOps::Trunc:    //fall-through
		case CastOps::UIToFP:   //fall-through
		case CastOps::ZExt:     //fall-through
		{
			instructions.emplace_back(new UnaryOperator(inst.getOpcodeName(), toValue(method, &inst), toValue(method, inst.getOperand(0))));
			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::Call:
		{
			const llvm::CallInst* call = llvm::cast<const llvm::CallInst>(&inst);
			std::vector<Value> args;
			for(unsigned i = 0; i < call->getNumArgOperands(); ++i)
			{
				args.emplace_back(toValue(method, call->getArgOperand(i)));
			}
			if(call->getCalledFunction()->isDeclaration())
			{
				//functions without definitions (e.g. intrinsic functions)
				std::string funcName = call->getCalledFunction()->getName();
				instructions.emplace_back(new CallSite(toValue(method, call).local, cleanMethodName(funcName.find("_Z") == 0 ? std::string("@") + funcName : funcName), toDataType(call->getCalledFunction()->getReturnType()), args));
			}
			else
			{
				Method& dest = parseFunction(module, *call->getCalledFunction());
				instructions.emplace_back(new CallSite(toValue(method, call).local, dest, args));
			}

			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::ExtractElement:
		{
			instructions.emplace_back(new ContainerExtraction(toValue(method, &inst).local, toValue(method, inst.getOperand(0)), toValue(method, inst.getOperand(1))));
			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::ExtractValue:
		{
			const llvm::ExtractValueInst* extraction = llvm::cast<const llvm::ExtractValueInst>(&inst);
			if(extraction->getIndices().size() != 1)
			{
				throw CompilationError(CompilationStep::PARSER, "Container extraction with multi-level indices is not yet implemented!");
			}
			instructions.emplace_back(new ContainerExtraction(toValue(method, extraction).local, toValue(method, extraction->getAggregateOperand()), Value(Literal(static_cast<int64_t>(extraction->getIndices().front())), TYPE_INT32)));
			instructions.back()->setDecorations(deco);
			break;
		}
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

		case OtherOps::InsertElement:
		{
			instructions.emplace_back(new ContainerInsertion(toValue(method, &inst).local, toValue(method, inst.getOperand(0)), toValue(method, inst.getOperand(1)), toValue(method, inst.getOperand(2))));
			instructions.back()->setDecorations(deco);
			break;
		}
		case OtherOps::InsertValue:
		{
			const llvm::InsertValueInst* insertion = llvm::cast<const llvm::InsertValueInst>(&inst);
			if(insertion->getIndices().size() != 1)
			{
				throw CompilationError(CompilationStep::PARSER, "Container insertion with multi-level indices is not yet implemented!");
			}
			instructions.emplace_back(new ContainerInsertion(toValue(method, insertion).local, toValue(method, insertion->getAggregateOperand()), toValue(method, insertion->getInsertedValueOperand()), Value(Literal(static_cast<int64_t>(insertion->getIndices().front())), TYPE_INT32)));
			instructions.back()->setDecorations(deco);
			break;
		}
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
	const Local* loc;
	const std::string valueName = val->getName().empty() ? "" : (std::string("%") + val->getName()).str();
	if((loc = method.findParameter(valueName)) != nullptr ||
			(loc = method.findStackAllocation(valueName)) != nullptr ||
			(loc = method.findGlobal((std::string("@") + val->getName()).str())) != nullptr)
	{
		return loc->createReference();
	}
	if(localMap.find(val) != localMap.end())
	{
		return localMap.at(val)->createReference();
	}
	if(llvm::dyn_cast<const llvm::BranchInst>(val) != nullptr)
	{
		//label
		Local* loc = method.addNewLocal(TYPE_LABEL, "%label").local;
		localMap[val] = loc;
		return loc->createReference();
	}
	const DataType type = toDataType(val->getType());
	if(llvm::dyn_cast<const llvm::Constant>(val) != nullptr)
	{
		return toConstant(const_cast<Module&>(method.module), val);
	}
	//locals of any kind have no name (most of the time)
	if(!val->getName().empty())
		loc = method.findOrCreateLocal(type, valueName);
	else
		loc = method.addNewLocal(type).local;
	localMap.emplace(val, const_cast<Local*>(loc));
	return loc->createReference();
}

Value BitcodeReader::toConstant(Module& module, const llvm::Value* val)
{
	const DataType type = toDataType(val->getType());
	if(llvm::dyn_cast<const llvm::ConstantInt>(val) != nullptr)
	{
		return Value(Literal(llvm::cast<const llvm::ConstantInt>(val)->getSExtValue()), type);
	}
	/*
	 * DO NOT COMBINE THE NEXT ELSE-CLAUSES
	 *
	 * For "standard" LLVM (4.0+), ConstantVector, ConstantArray and ConstantStruct have a common super-type
	 * ConstantAggregate, but not yet for SPIRV-LLVM (~3.6)
	 */
	else if(llvm::dyn_cast<const llvm::ConstantVector>(val) != nullptr)
	{
		//element types are stored as operands
		const llvm::ConstantVector* constant = llvm::cast<const llvm::ConstantVector>(val);
		Value aggregate(ContainerValue(), type);
		for(unsigned i = 0; i < constant->getNumOperands(); ++i)
		{
			aggregate.container.elements.push_back(toConstant(module, constant->getOperand(i)));
		}
		return aggregate;
	}
	else if(llvm::dyn_cast<const llvm::ConstantArray>(val) != nullptr)
	{
		//element types are stored as operands
		const llvm::ConstantArray* constant = llvm::cast<const llvm::ConstantArray>(val);
		Value aggregate(ContainerValue(), type);
		for(unsigned i = 0; i < constant->getNumOperands(); ++i)
		{
			aggregate.container.elements.push_back(toConstant(module, constant->getOperand(i)));
		}
		return aggregate;
	}
	else if(llvm::dyn_cast<const llvm::ConstantStruct>(val) != nullptr)
	{
		const llvm::ConstantStruct* constant = llvm::cast<const llvm::ConstantStruct>(val);

		//special treatment for spirv.ConstantSampler type
		if(type == TYPE_SAMPLER)
		{
			intermediate::Sampler sampler(static_cast<intermediate::AddressingMode>(toConstant(module, constant->getOperand(0)).getLiteralValue()->integer),
					toConstant(module, constant->getOperand(1)).getLiteralValue()->isTrue(),
					static_cast<intermediate::FilterMode>(toConstant(module, constant->getOperand(2)).getLiteralValue()->integer));
			return Value(Literal(static_cast<int64_t>(sampler)), TYPE_SAMPLER);
		}
		//element types are stored as operands
		Value aggregate(ContainerValue(), type);
		for(unsigned i = 0; i < constant->getNumOperands(); ++i)
		{
			aggregate.container.elements.push_back(toConstant(module, constant->getOperand(i)));
		}
		return aggregate;
	}
	else if(llvm::dyn_cast<const llvm::ConstantDataSequential>(val) != nullptr)
	{
		//vector/array constant, but packed in storage
		const llvm::ConstantDataSequential* constant = llvm::cast<const llvm::ConstantDataSequential>(val);
		Value aggregate(ContainerValue(), type);
		for(unsigned i = 0; i < constant->getNumElements(); ++i)
		{
			aggregate.container.elements.push_back(toConstant(module, constant->getElementAsConstant(i)));
		}
		return aggregate;
	}
	else if(llvm::dyn_cast<const llvm::ConstantAggregateZero>(val) != nullptr)
	{
		const llvm::ConstantAggregateZero* constant = llvm::cast<const llvm::ConstantAggregateZero>(val);
		Value aggregate(ContainerValue(), type);
		for(unsigned i = 0; i < constant->getNumElements(); ++i)
		{
			aggregate.container.elements.push_back(toConstant(module, constant->getElementValue(i)));
		}
		return aggregate;
	}
	else if(llvm::dyn_cast<const llvm::ConstantFP>(val) != nullptr)
	{
		return Value(Literal(llvm::cast<const llvm::ConstantFP>(val)->getValueAPF().convertToFloat()), type);
	}
	else if(llvm::dyn_cast<const llvm::ConstantPointerNull>(val) != nullptr)
	{
		return Value(INT_ZERO.literal, type);
	}
	else if(llvm::dyn_cast<const llvm::ConstantExpr>(val) != nullptr)
	{
		return precalculateConstantExpression(module, llvm::cast<const llvm::ConstantExpr>(val));
	}
	else if(llvm::dyn_cast<const llvm::GlobalVariable>(val) != nullptr)
	{
		const std::string name = ("@" + val->getName()).str();
		auto globalIt = std::find_if(module.globalData.begin(), module.globalData.end(), [&name](const Global& global) -> bool { return global.name == name;});
		if(globalIt != module.globalData.end())
			return globalIt->createReference();

		const llvm::GlobalVariable* global = llvm::cast<const llvm::GlobalVariable>(val);
		module.globalData.emplace_back(Global(name, toDataType(global->getType()), global->hasInitializer() ? toConstant(module, global->getInitializer()) : UNDEFINED_VALUE, global->isConstant()));
		logging::debug() << "Global read: " << module.globalData.back().to_string() << logging::endl;
		return module.globalData.back().createReference();
	}
	else if(llvm::dyn_cast<const llvm::UndefValue>(val) != nullptr)
	{
		Value res = UNDEFINED_VALUE;
		res.type = type;
		return res;
	}
	else
	{
		val->dump();
		throw CompilationError(CompilationStep::PARSER, "Unhandled constant type", std::to_string(val->getValueID()));
	}
}

Value BitcodeReader::precalculateConstantExpression(Module& module, const llvm::ConstantExpr* expr)
{
	if(expr->getOpcode() == llvm::Instruction::CastOps::BitCast || expr->getOpcode() == llvm::Instruction::CastOps::AddrSpaceCast)
	{
		Value result = toConstant(module, expr->getOperand(0));
		if(expr->getOperand(0)->getType()->getScalarSizeInBits() != expr->getType()->getScalarSizeInBits())
		{
			throw CompilationError(CompilationStep::PARSER, "Bit-casts over different type-sizes are not yet implemented!");
		}
		result.type = toDataType(expr->getType());
		return result;
	}
	if(expr->getOpcode() == llvm::Instruction::MemoryOps::GetElementPtr)
	{
		//e.g. in-line getelementptr in load or store instructions
		if(llvm::dyn_cast<const llvm::GlobalVariable>(expr->getOperand(0)) != nullptr)
		{
			Value srcGlobal = toConstant(module, expr->getOperand(0));

			//for now, we only support indices of all zero -> reference to the global itself (or the first entry)
			for(unsigned i = 1; i < expr->getNumOperands(); ++i)
			{
				if(!toConstant(module, expr->getOperand(i)).isZeroInitializer())
				{
					expr->dump();
					throw CompilationError(CompilationStep::PARSER, "Only constant getelementptr without offsets are supported for now");
				}
			}

			//we need to make the type of the value fit the return-type expected
			srcGlobal.type = toDataType(expr->getType());
			//TODO is this correct or would we need a new local referring to the global?
			return srcGlobal;
		}
	}
	OpCode opCode = OpCode::findOpCode(expr->getOpcodeName());

	Optional<Value> result = NO_VALUE;
	if(opCode.numOperands == 1)
		result = opCode.calculate(toConstant(module, expr->getOperand(0)), NO_VALUE, [](const Value&) -> Optional<Value>{return NO_VALUE;});
	else if (opCode.numOperands == 2)
		result = opCode.calculate(toConstant(module, expr->getOperand(0)), toConstant(module, expr->getOperand(1)), [](const Value&) -> Optional<Value>{return NO_VALUE;});

	if(result)
		return result.value();
	expr->dump();
	throw CompilationError(CompilationStep::PARSER, "This type of constant expression is not supported yet", expr->getOpcodeName());
}

#endif
