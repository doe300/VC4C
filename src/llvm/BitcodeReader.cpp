/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "BitcodeReader.h"

#ifdef USE_LLVM_LIBRARY

#include "../intermediate/IntermediateInstruction.h"
#include "../intrinsics/Images.h"
#include "log.h"

#include "llvm-c/Core.h"
#if LLVM_LIBRARY_VERSION >= 40
#include "llvm/Bitcode/BitcodeReader.h"
#else
#include "llvm/Bitcode/ReaderWriter.h"
#endif
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"

#include <limits>
#include <regex>
#include <system_error>

using namespace vc4c;
using namespace vc4c::llvm2qasm;

// also used by LibClang
// NOTE: The buffer string needs to outlive the MemoryBuffer object!
std::unique_ptr<llvm::MemoryBuffer> fromInputStream(std::istream& stream, std::string& buffer)
{
    // required, since LLVM cannot read from std::istreams
    if(auto sstream = dynamic_cast<std::istringstream*>(&stream))
        buffer = sstream->str();
    else if(auto sstream = dynamic_cast<std::stringstream*>(&stream))
        buffer = sstream->str();
    else
    {
        std::stringstream ss;
        ss << stream.rdbuf();
        buffer = ss.str();
    }
    return std::unique_ptr<llvm::MemoryBuffer>(llvm::MemoryBuffer::getMemBufferCopy(llvm::StringRef(buffer)));
}

static AddressSpace toAddressSpace(int num)
{
    // See also https://github.com/llvm-mirror/clang/blob/master/lib/Basic/Targets/SPIR.h for the mapping order
    // XXX somehow the values from the kernel meta data and the parameter pointer types differ?! (e.g.
    // NVIDIA/oclSimpleTexture3D_kernel.cl)
    switch(num)
    {
    case 0:
        // According to the documentation for 'alloca', "The object is always allocated in the generic address space
        // (address space zero)"
        return AddressSpace::PRIVATE;
    case 1:
        return AddressSpace::GLOBAL;
    case 2:
        return AddressSpace::CONSTANT;
    case 3:
        return AddressSpace::LOCAL;
    case 4:
        return AddressSpace::GENERIC;
    default:
        return AddressSpace::GENERIC;
    }
}

static std::string cleanMethodName(const std::string& name)
{
    static const std::regex leadingRegex("@(_Z\\d+)?");
    static const std::regex trailingRegex("Dv\\d+_");
    // truncate prefix and postfix added by LLVM
    std::string tmp = std::regex_replace(name, leadingRegex, "");
    return std::regex_replace(tmp, trailingRegex, "");
}

LCOV_EXCL_START
template <typename T>
static void dumpLLVM(const T* val)
{
#if LLVM_LIBRARY_VERSION >= 50
    val->print(llvm::errs());
#else
    val->dump();
#endif
}
LCOV_EXCL_STOP

BitcodeReader::BitcodeReader(std::istream& stream, SourceType sourceType) : context()
{
    std::string tmp;
    auto buf = fromInputStream(stream, tmp);
    if(sourceType == SourceType::LLVM_IR_BIN)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading LLVM module from bit-code..." << logging::endl);
        auto expected = llvm::parseBitcodeFile(buf->getMemBufferRef(), context);
        if(!expected)
        {
#if LLVM_LIBRARY_VERSION >= 40
            std::string error = "";
            llvm::handleAllErrors(
                expected.takeError(), [&error](const llvm::ErrorInfoBase& base) { error = base.message(); });
            throw std::runtime_error("Error parsing LLVM module" + error);
#else
            throw std::system_error(expected.getError(), "Error parsing LLVM module");
#endif
        }
        else
        {
            // expected.get() is either std::unique_ptr<llvm::Module> or llvm::Module*
            std::unique_ptr<llvm::Module> tmp(std::move(expected.get()));
            llvmModule.swap(tmp);
        }
    }
    else if(sourceType == SourceType::LLVM_IR_TEXT)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading LLVM module from IR..." << logging::endl);
        llvm::SMDiagnostic error;
        llvmModule = llvm::parseIR(buf->getMemBufferRef(), error, context);
        if(!llvmModule)
            throw CompilationError(
                CompilationStep::PARSER, "Error parsing LLVM IR module", static_cast<std::string>(error.getMessage()));
    }
    else
        throw CompilationError(CompilationStep::PARSER, "Unhandled source-type for LLVM bitcode reader",
            std::to_string(static_cast<unsigned>(sourceType)));
}

static void extractKernelMetadata(
    Method& kernel, const llvm::Function& func, const llvm::Module& llvmModule, const llvm::LLVMContext& context)
{
    if(auto metadata = func.getMetadata("kernel_arg_addr_space"))
    {
        // address spaces for kernel pointer arguments, e.g. "!2 = !{i32 1, i32 1}"
        for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
        {
            if(auto ptrType = kernel.parameters.at(i).type.getPointerType())
            {
                const llvm::Metadata* operand = metadata->getOperand(i).get();
                if(operand->getMetadataID() == llvm::Metadata::ConstantAsMetadataKind)
                {
                    const llvm::ConstantAsMetadata* constant = llvm::cast<const llvm::ConstantAsMetadata>(operand);
                    auto& addrSpace = const_cast<AddressSpace&>(ptrType->addressSpace);
                    if(addrSpace == AddressSpace::GENERIC)
                        addrSpace = toAddressSpace(static_cast<int32_t>(
                            llvm::cast<const llvm::ConstantInt>(constant->getValue())->getSExtValue()));
                }
                else
                {
                    dumpLLVM(operand);
                    throw CompilationError(
                        CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
                }
            }
        }
    }
    if(auto metadata = func.getMetadata("kernel_arg_access_qual"))
    {
        // access qualifiers for image arguments, e.g. "!3 = !{!"none", !"none"}"
        // XXX what to do with them? Only valid for images
        // if we don't use image-config for writing images, we could e.g. don't write it for write-only images
    }
    if(auto metadata = func.getMetadata("kernel_arg_type"))
    {
        // original type-names for kernel arguments, e.g. "!4 = !{!"float*", !"float*"}"
        for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
        {
            const llvm::Metadata* operand = metadata->getOperand(i).get();
            if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
            {
                const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
                kernel.parameters.at(i).origTypeName = static_cast<std::string>(name->getString());
            }
            else
            {
                dumpLLVM(operand);
                throw CompilationError(
                    CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
            }
        }
    }
    if(auto metadata = func.getMetadata("kernel_arg_base_type"))
    {
        // base types, e.g. for type-defs, e.g. "!4 = !{!"float*", !"float*"}"
        // is not used
    }
    if(auto metadata = func.getMetadata("kernel_arg_type_qual"))
    {
        // additional type qualifiers, e.g. "!5 = !{!"", !""}"
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
            {
                dumpLLVM(operand);
                throw CompilationError(
                    CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
            }
        }
    }
    if(auto metadata = func.getMetadata("kernel_arg_name"))
    {
        // the original argument names, e.g. "!6 = !{!"a", !"b"}"
        for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
        {
            const llvm::Metadata* operand = metadata->getOperand(i).get();
            if(operand->getMetadataID() == llvm::Metadata::MDStringKind)
            {
                const llvm::MDString* name = llvm::cast<const llvm::MDString>(operand);
                kernel.parameters.at(i).parameterName = static_cast<std::string>(name->getString());
            }
            else
            {
                dumpLLVM(operand);
                throw CompilationError(
                    CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
            }
        }
    }
    if(auto metadata = func.getMetadata("reqd_work_group_size"))
    {
        // compile time work-group size, e.g. "!2 = !{i32 1, i32 1, i32 1}"
        for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
        {
            const llvm::Metadata* operand = metadata->getOperand(i).get();
            if(operand->getMetadataID() == llvm::Metadata::ConstantAsMetadataKind)
            {
                const llvm::ConstantAsMetadata* constant = llvm::cast<const llvm::ConstantAsMetadata>(operand);
                kernel.metaData.workGroupSizes.at(i) =
                    static_cast<uint32_t>(llvm::cast<const llvm::ConstantInt>(constant->getValue())->getZExtValue());
            }
            else
            {
                dumpLLVM(operand);
                throw CompilationError(
                    CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
            }
        }
    }
    if(auto metadata = func.getMetadata("work_group_size_hint"))
    {
        // compile time work-group size hint, e.g. "!2 = !{i32 1, i32 1, i32 1}"
        for(unsigned i = 0; i < metadata->getNumOperands(); ++i)
        {
            const llvm::Metadata* operand = metadata->getOperand(i).get();
            if(operand->getMetadataID() == llvm::Metadata::ConstantAsMetadataKind)
            {
                const llvm::ConstantAsMetadata* constant = llvm::cast<const llvm::ConstantAsMetadata>(operand);
                kernel.metaData.workGroupSizeHints.at(i) =
                    static_cast<uint32_t>(llvm::cast<const llvm::ConstantInt>(constant->getValue())->getZExtValue());
            }
            else
            {
                dumpLLVM(operand);
                throw CompilationError(
                    CompilationStep::PARSER, "Unhandled meta-data kind", std::to_string(operand->getMetadataID()));
            }
        }
    }
}

void BitcodeReader::parse(Module& module)
{
    const llvm::Module::FunctionListType& functions = llvmModule->getFunctionList();

    // The global data is explicitly not read here, but on #toConstant() only resolving global data actually used

    // parse functions
    // Starting with kernel-functions, recursively parse all included functions (and only those)
    for(const llvm::Function& func : functions)
    {
        if(func.getCallingConv() == llvm::CallingConv::SPIR_KERNEL)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found SPIR kernel-function: " << static_cast<std::string>(func.getName()) << logging::endl);
            Method& kernelFunc = parseFunction(module, func);
            extractKernelMetadata(kernelFunc, func, *llvmModule, context);
            kernelFunc.flags = add_flag(kernelFunc.flags, MethodFlags::KERNEL);
        }
    }

    // map instructions to intermediate representation
    for(auto& method : parsedFunctions)
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Mapping function '" << method.second.first->name << "'..." << logging::endl);
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

DataType BitcodeReader::toDataType(Module& module, const llvm::Type* type, Optional<AddressSpace> overrideAddressSpace)
{
    if(type == nullptr)
        return TYPE_UNKNOWN;
    auto it = typesMap.find(type);
    if(it != typesMap.end())
        return it->second;
    if(type->isVectorTy())
    {
        const llvm::VectorType* vec = llvm::cast<const llvm::VectorType>(type);
        return toDataType(module, vec->getElementType())
            .toVectorType(static_cast<unsigned char>(vec->getNumElements()));
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
    if(type->isIntegerTy() && type->getIntegerBitWidth() <= 8)
    {
        // ./example/md5.cl uses i2 (with LLVM 6.0+) for 4-element selects
        if(type->getIntegerBitWidth() != 8)
            logging::warn() << "Irregular integer type will be extended to next larger regular integer type: i"
                            << type->getIntegerBitWidth() << logging::endl;
        return TYPE_INT8;
    }
    if(type->isIntegerTy() && type->getIntegerBitWidth() <= 16)
    {
        if(type->getIntegerBitWidth() != 16)
            logging::warn() << "Irregular integer type will be extended to next larger regular integer type: i"
                            << type->getIntegerBitWidth() << logging::endl;
        return TYPE_INT16;
    }
    if(type->isIntegerTy() && type->getIntegerBitWidth() <= 32)
    {
        if(type->getIntegerBitWidth() != 32)
            logging::warn() << "Irregular integer type will be extended to next larger regular integer type: i"
                            << type->getIntegerBitWidth() << logging::endl;
        return TYPE_INT32;
    }
    if(type->isIntegerTy(33))
    {
        // i33 - some special type to not overflow on arithmetic operations. Currently only used in
        // ./testing/boost-compute/test_accumulate.cl
        logging::warn() << "33-bit extended integer type is not supported, falling back to 32-bit integer. Results may "
                           "be inaccurate!"
                        << logging::endl;
        return TYPE_INT32;
    }
    if(type->isIntegerTy() && type->getIntegerBitWidth() <= 64)
    {
        if(type->getIntegerBitWidth() != 64)
            logging::warn() << "Irregular integer type will be extended to next larger regular integer type: i"
                            << type->getIntegerBitWidth() << logging::endl;
        return TYPE_INT64;
    }
    if(type->isPointerTy() && type->getPointerElementType()->isStructTy())
    {
        // recognize image types - taken from
        // https://github.com/KhronosGroup/SPIRV-LLVM/blob/khronos/spirv-3.6.1/lib/SPIRV/SPIRVUtil.cpp (#isOCLImageType)
        const llvm::StructType* str = llvm::cast<const llvm::StructType>(type->getPointerElementType());
        if(str->isOpaque() && str->getName().find("opencl.image") == 0)
        {
            auto dimensions = str->getName().find('3') != llvm::StringRef::npos ?
                3 :
                str->getName().find('2') != llvm::StringRef::npos ? 2 : 1;
            auto isImageArray = str->getName().find("array") != llvm::StringRef::npos;
            auto isImageBuffer = str->getName().find("buffer") != llvm::StringRef::npos;
            auto isSampled = false;

            return addToMap(DataType(module.createImageType(
                                static_cast<uint8_t>(dimensions), isImageArray, isImageBuffer, isSampled)),
                type, typesMap);
        }
    }
    if(type->isStructTy())
    {
        // detect special OpenCL types
        if(type->getStructName() == "spirv.Sampler" || type->getStructName() == "spirv.ConstantSampler")
            return TYPE_SAMPLER;
        if(type->getStructName() == "opencl.event_t")
            return TYPE_EVENT;
        // need to be added to the map before iterating over the children to prevent stack-overflow
        // (since the type itself could be recursive)
        auto structType = module.createStructType(
            static_cast<std::string>(type->getStructName()), {}, llvm::cast<const llvm::StructType>(type)->isPacked());
        DataType& dataType = addToMap(DataType(structType), type, typesMap);
        structType->elementTypes.reserve(type->getStructNumElements());
        for(unsigned i = 0; i < type->getStructNumElements(); ++i)
        {
            structType->elementTypes.emplace_back(toDataType(module, type->getStructElementType(i)));
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Struct " << static_cast<std::string>(type->getStructName()) << ": " << structType->getContent()
                << logging::endl);
        return dataType;
    }
    if(type->isArrayTy())
    {
        DataType elementType = toDataType(module, type->getArrayElementType());
        return addToMap(
            DataType(module.createArrayType(elementType, static_cast<uint32_t>(type->getArrayNumElements()))), type,
            typesMap);
    }
    if(type->isPointerTy())
    {
        DataType elementType = toDataType(module, type->getPointerElementType());
        return DataType(module.createPointerType(elementType,
            overrideAddressSpace.value_or(toAddressSpace(static_cast<int32_t>(type->getPointerAddressSpace())))));
    }
    dumpLLVM(type);
    throw CompilationError(CompilationStep::PARSER, "Unknown LLVM type", std::to_string(type->getTypeID()));
}

static ParameterDecorations toParameterDecorations(const llvm::Argument& arg, DataType type, bool isKernel)
{
    ParameterDecorations deco = ParameterDecorations::NONE;
    if(arg.hasSExtAttr())
        deco = add_flag(deco, ParameterDecorations::SIGN_EXTEND);
    if(arg.hasZExtAttr())
        deco = add_flag(deco, ParameterDecorations::ZERO_EXTEND);
    if(arg.hasNoAliasAttr())
        deco = add_flag(deco, ParameterDecorations::RESTRICT);
    if(arg.onlyReadsMemory())
        // XXX this sets the CL_KERNEL_ARG_TYPE_QUALIFIER for the parameter to CL_KERNEL_ARG_TYPE_CONST, which it
        // should not. Somehow this seems to be no problem anymore for LLVM 6.0 and up?!
        deco = add_flag(deco, ParameterDecorations::READ_ONLY);
    if(type.getImageType())
    {
        const llvm::StructType* str = llvm::cast<const llvm::StructType>(arg.getType()->getPointerElementType());
        if(str->getName().find("ro_t") != std::string::npos)
            deco = add_flag(add_flag(deco, ParameterDecorations::READ_ONLY), ParameterDecorations::INPUT);
        else if(str->getName().find("wo_t") != std::string::npos)
            deco = add_flag(deco, ParameterDecorations::OUTPUT);
    }
    if(arg.hasInAllocaAttr() && isKernel)
    {
        dumpLLVM(&arg);
        throw CompilationError(CompilationStep::PARSER, "Kernel parameter decorated with inalloca are not supported",
            static_cast<std::string>(arg.getName()));
    }
    if(arg.hasByValAttr())
    {
        //"This indicates that the pointer parameter should really be passed by value to the function. The attribute
        // implies that a hidden copy of the pointee is made between the caller and the callee, so the callee is
        // unable to modify the value in the caller."
        // This is e.g. used direct struct parameters passed to kernels
        deco = add_flag(deco, ParameterDecorations::BY_VALUE);
    }
    return deco;
}

static std::string toParameterName(const llvm::Argument& arg, unsigned& counter)
{
    if(arg.getName().empty())
        return std::string("%") + std::to_string(counter++);
    return std::string("%") + arg.getName().str();
}

Method& BitcodeReader::parseFunction(Module& module, const llvm::Function& func)
{
    auto it = parsedFunctions.find(&func);
    if(it != parsedFunctions.end())
        return *it->second.first;

    Method* method = new Method(module);
    module.methods.emplace_back(method);
    parsedFunctions[&func] = std::make_pair(method, LLVMInstructionList{});

    method->name = cleanMethodName(static_cast<std::string>(func.getName()));
    method->returnType = toDataType(module, func.getReturnType());

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Reading function " << method->returnType.to_string() << " " << method->name << "(...)"
            << logging::endl);

    // for some functions, the parameters have no name, but are addressed with their index, so we need to give them
    // their index as name
    unsigned paramCounter = 0;
#if LLVM_LIBRARY_VERSION >= 50
    method->parameters.reserve(func.arg_size());
    for(const llvm::Argument& arg : func.args())
#else
    method->parameters.reserve(func.getArgumentList().size());
    for(const llvm::Argument& arg : func.getArgumentList())
#endif
    {
        // by-val parameter is always read-only, and the address-space initially set is __private, which we cannot have
        // for pointer Parameters
        auto overrideAddressSpace = arg.hasByValAttr() ? AddressSpace::CONSTANT : Optional<AddressSpace>{};
        auto type = toDataType(module, arg.getType(), overrideAddressSpace);

        auto& param = method->addParameter(Parameter(toParameterName(arg, paramCounter), type,
            toParameterDecorations(arg, type, func.getCallingConv() == llvm::CallingConv::SPIR_KERNEL)));
        if(arg.getDereferenceableBytes() != 0)
            param.maxByteOffset = static_cast<std::size_t>(arg.getDereferenceableBytes());
        if(arg.getDereferenceableOrNullBytes() != 0)
            param.maxByteOffset = static_cast<std::size_t>(arg.getDereferenceableOrNullBytes());
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading parameter " << param.to_string(true) << logging::endl);
        if(param.type.getImageType() && func.getCallingConv() == llvm::CallingConv::SPIR_KERNEL)
            intermediate::reserveImageConfiguration(module, param);
        localMap[&arg] = &param;
    }

    parseFunctionBody(module, *method, parsedFunctions.at(&func).second, func);

    return *method;
}

void BitcodeReader::parseFunctionBody(
    Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Function& func)
{
    auto numInstructions = std::accumulate(
        func.begin(), func.end(), 0u, [](const std::size_t subtotal, const llvm::BasicBlock& block) -> std::size_t {
            return subtotal + 1 /* label */ + block.size();
        });
    instructions.reserve(
        numInstructions + 32 /* some space for additional inserted instructions, e.g. in-line getelementptr */);
    for(const llvm::BasicBlock& block : func)
    {
        // need to extract label from basic block
        instructions.emplace_back(new LLVMLabel(toValue(method, &block, &instructions)));
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
#if LLVM_LIBRARY_VERSION >= 60
    if(llvm::isa<llvm::FPMathOperator>(&inst) && inst.isFast())
#else
    if(llvm::isa<llvm::FPMathOperator>(&inst) && inst.hasUnsafeAlgebra())
#endif
        deco = add_flag(deco, intermediate::InstructionDecorations::FAST_MATH);
    if(inst.hasNoSignedWrap())
        deco = add_flag(deco, intermediate::InstructionDecorations::SIGNED_OVERFLOW_IS_UB);
    if(inst.hasNoUnsignedWrap())
        deco = add_flag(deco, intermediate::InstructionDecorations::UNSIGNED_OVERFLOW_IS_UB);
    // XXX this is sometimes added to add and getelementptr operations, but not present in the LLVM IR output!?
    if(inst.isExact())
        deco = add_flag(deco, intermediate::InstructionDecorations::EXACT_OPERATION);
    return deco;
}

static std::string toComparison(llvm::CmpInst::Predicate pred)
{
    using P = llvm::CmpInst::Predicate;
    static constexpr std::array<const char*, P::LAST_FCMP_PREDICATE + 1> floatComparisons{
        intermediate::COMP_FALSE,
        intermediate::COMP_ORDERED_EQ,
        intermediate::COMP_ORDERED_GT,
        intermediate::COMP_ORDERED_GE,
        intermediate::COMP_ORDERED_LT,
        intermediate::COMP_ORDERED_LE,
        intermediate::COMP_ORDERED_NEQ,
        intermediate::COMP_ORDERED,
        intermediate::COMP_UNORDERED,
        intermediate::COMP_UNORDERED_EQ,
        intermediate::COMP_UNORDERED_GT,
        intermediate::COMP_UNORDERED_GE,
        intermediate::COMP_UNORDERED_LT,
        intermediate::COMP_UNORDERED_LE,
        intermediate::COMP_UNORDERED_NEQ,
        intermediate::COMP_TRUE,
    };
    static constexpr std::array<const char*, P::LAST_ICMP_PREDICATE - P::FIRST_ICMP_PREDICATE + 1> intComparisons{
        intermediate::COMP_EQ, intermediate::COMP_NEQ, intermediate::COMP_UNSIGNED_GT, intermediate::COMP_UNSIGNED_GE,
        intermediate::COMP_UNSIGNED_LT, intermediate::COMP_UNSIGNED_LE, intermediate::COMP_SIGNED_GT,
        intermediate::COMP_SIGNED_GE, intermediate::COMP_SIGNED_LT, intermediate::COMP_SIGNED_LE};
    static_assert(floatComparisons[P::FCMP_FALSE] == intermediate::COMP_FALSE, "");
    static_assert(floatComparisons[P::FCMP_OEQ] == intermediate::COMP_ORDERED_EQ, "");
    static_assert(floatComparisons[P::FCMP_OGT] == intermediate::COMP_ORDERED_GT, "");
    static_assert(floatComparisons[P::FCMP_OGE] == intermediate::COMP_ORDERED_GE, "");
    static_assert(floatComparisons[P::FCMP_OLT] == intermediate::COMP_ORDERED_LT, "");
    static_assert(floatComparisons[P::FCMP_OLE] == intermediate::COMP_ORDERED_LE, "");
    static_assert(floatComparisons[P::FCMP_ONE] == intermediate::COMP_ORDERED_NEQ, "");
    static_assert(floatComparisons[P::FCMP_ORD] == intermediate::COMP_ORDERED, "");
    static_assert(floatComparisons[P::FCMP_UNO] == intermediate::COMP_UNORDERED, "");
    static_assert(floatComparisons[P::FCMP_UEQ] == intermediate::COMP_UNORDERED_EQ, "");
    static_assert(floatComparisons[P::FCMP_UGT] == intermediate::COMP_UNORDERED_GT, "");
    static_assert(floatComparisons[P::FCMP_UGE] == intermediate::COMP_UNORDERED_GE, "");
    static_assert(floatComparisons[P::FCMP_ULT] == intermediate::COMP_UNORDERED_LT, "");
    static_assert(floatComparisons[P::FCMP_ULE] == intermediate::COMP_UNORDERED_LE, "");
    static_assert(floatComparisons[P::FCMP_UNE] == intermediate::COMP_UNORDERED_NEQ, "");
    static_assert(floatComparisons[P::FCMP_TRUE] == intermediate::COMP_TRUE, "");

    static_assert(intComparisons[P::ICMP_EQ ^ 32] == intermediate::COMP_EQ, "");
    static_assert(intComparisons[P::ICMP_NE ^ 32] == intermediate::COMP_NEQ, "");
    static_assert(intComparisons[P::ICMP_UGT ^ 32] == intermediate::COMP_UNSIGNED_GT, "");
    static_assert(intComparisons[P::ICMP_UGE ^ 32] == intermediate::COMP_UNSIGNED_GE, "");
    static_assert(intComparisons[P::ICMP_ULT ^ 32] == intermediate::COMP_UNSIGNED_LT, "");
    static_assert(intComparisons[P::ICMP_ULE ^ 32] == intermediate::COMP_UNSIGNED_LE, "");
    static_assert(intComparisons[P::ICMP_SGT ^ 32] == intermediate::COMP_SIGNED_GT, "");
    static_assert(intComparisons[P::ICMP_SGE ^ 32] == intermediate::COMP_SIGNED_GE, "");
    static_assert(intComparisons[P::ICMP_SLT ^ 32] == intermediate::COMP_SIGNED_LT, "");
    static_assert(intComparisons[P::ICMP_SLE ^ 32] == intermediate::COMP_SIGNED_LE, "");

    if(pred <= P::LAST_FCMP_PREDICATE)
        return floatComparisons[pred];
    if(pred >= P::FIRST_ICMP_PREDICATE && pred <= P::LAST_ICMP_PREDICATE)
        return intComparisons[pred ^ 32];

    throw CompilationError(CompilationStep::PARSER, "Unhandled comparison predicate", std::to_string(pred));
}

void BitcodeReader::parseInstruction(
    Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Instruction& inst)
{
    using TermOps = llvm::Instruction::TermOps;
#if LLVM_LIBRARY_VERSION >= 80
    using UnaryOps = llvm::Instruction::UnaryOps;
#endif
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
            instructions.emplace_back(new Branch(toValue(method, br->getSuccessor(0), &instructions)));
        else
            instructions.emplace_back(new Branch(toValue(method, br->getCondition(), &instructions),
                toValue(method, br->getSuccessor(0), &instructions),
                toValue(method, br->getSuccessor(1), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case TermOps::Ret:
    {
        const llvm::ReturnInst* ret = llvm::cast<const llvm::ReturnInst>(&inst);
        if(ret->getReturnValue() == nullptr)
            instructions.emplace_back(new ValueReturn());
        else
            instructions.emplace_back(new ValueReturn(toValue(method, ret->getReturnValue(), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case TermOps::Switch:
    {
        const llvm::SwitchInst* switchIns = llvm::cast<const llvm::SwitchInst>(&inst);
        Value cond = toValue(method, switchIns->getCondition(), &instructions);
        Value defaultLabel = toValue(method, switchIns->getDefaultDest(), &instructions);
        FastMap<uint64_t, Value> caseLabels;
        // NOTE: cannot be const auto&, since older LLVM versions have getCaseValue() and getCaseSuccessor() only as
        // non-const member
        for(auto& casePair : switchIns->cases())
        {
            auto caseValue = casePair.getCaseValue()->getZExtValue();
            caseValue = caseValue & casePair.getCaseValue()->getType()->getBitMask();
            caseLabels.emplace(caseValue, toValue(method, casePair.getCaseSuccessor(), &instructions));
        }
        instructions.emplace_back(new Switch(std::move(cond), std::move(defaultLabel), std::move(caseLabels)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case BinaryOps::AShr:
        FALL_THROUGH
    case BinaryOps::Add:
        FALL_THROUGH
    case BinaryOps::And:
        FALL_THROUGH
    case BinaryOps::FAdd:
        FALL_THROUGH
    case BinaryOps::FDiv:
        FALL_THROUGH
    case BinaryOps::FMul:
        FALL_THROUGH
    case BinaryOps::FRem:
        FALL_THROUGH
    case BinaryOps::FSub:
        FALL_THROUGH
    case BinaryOps::LShr:
        FALL_THROUGH
    case BinaryOps::Mul:
        FALL_THROUGH
    case BinaryOps::Or:
        FALL_THROUGH
    case BinaryOps::SDiv:
        FALL_THROUGH
    case BinaryOps::SRem:
        FALL_THROUGH
    case BinaryOps::Shl:
        FALL_THROUGH
    case BinaryOps::Sub:
        FALL_THROUGH
    case BinaryOps::UDiv:
        FALL_THROUGH
    case BinaryOps::URem:
        FALL_THROUGH
    case BinaryOps::Xor:
    {
        const llvm::BinaryOperator* binOp = llvm::cast<const llvm::BinaryOperator>(&inst);
        instructions.emplace_back(new BinaryOperator(binOp->getOpcodeName(), toValue(method, binOp, &instructions),
            toValue(method, binOp->getOperand(0), &instructions),
            toValue(method, binOp->getOperand(1), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case MemoryOps::Alloca:
    {
        const llvm::AllocaInst* alloca = llvm::cast<const llvm::AllocaInst>(&inst);
        // for arrays, the allocated type is the array type, so we don't need to handle them special here
        const DataType contentType = toDataType(module, alloca->getAllocatedType());
        const DataType pointerType = toDataType(module, alloca->getType());
        unsigned alignment = alloca->getAlignment();
        auto it = method.stackAllocations.emplace(
            StackAllocation(("%" + alloca->getName()).str(), pointerType, contentType.getInMemoryWidth(), alignment));
        localMap[alloca] = &(*it.first);
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Reading stack allocation: " << it.first->to_string() << logging::endl);
        break;
    }
    case MemoryOps::GetElementPtr:
    {
        const llvm::GetElementPtrInst* indexOf = llvm::cast<const llvm::GetElementPtrInst>(&inst);
        std::vector<Value> indices;
        // TODO for isInBounds(), mark all indices as IN_BOUNDS, can be used for memory access?
        std::for_each(indexOf->idx_begin(), indexOf->idx_end(),
            [this, &method, &indices, &instructions](
                const llvm::Value* val) -> void { indices.emplace_back(toValue(method, val, &instructions)); });
        instructions.emplace_back(new IndexOf(toValue(method, indexOf, &instructions),
            toValue(method, indexOf->getPointerOperand(), &instructions), std::move(indices)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case MemoryOps::Load:
    {
        const llvm::LoadInst* load = llvm::cast<const llvm::LoadInst>(&inst);
        Value src = UNDEFINED_VALUE;
        if(load->getPointerOperand()->getValueID() == llvm::Constant::ConstantExprVal)
            src = parseInlineGetElementPtr(module, method, instructions, load->getPointerOperand());
        else
            src = toValue(method, load->getPointerOperand(), &instructions);
        // TODO for volatile, mark argument pointer as VOLATILE
        if(load->isVolatile() && src.checkLocal() && src.local()->is<Parameter>())
            src.local()->as<Parameter>()->decorations =
                add_flag(src.local()->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
        instructions.emplace_back(new Copy(toValue(method, load, &instructions), std::move(src), true, true));
        instructions.back()->setDecorations(deco);
        break;
    }
    case MemoryOps::Store:
    {
        const llvm::StoreInst* store = llvm::cast<const llvm::StoreInst>(&inst);
        Value dest = UNDEFINED_VALUE;
        if(store->getPointerOperand()->getValueID() == llvm::Constant::ConstantExprVal)
            dest = parseInlineGetElementPtr(module, method, instructions, store->getPointerOperand());
        else
            dest = toValue(method, store->getPointerOperand(), &instructions);
        // TODO for volatile, mark argument pointer as VOLATILE
        if(store->isVolatile() && dest.checkLocal() && dest.local()->is<Parameter>())
            dest.local()->as<Parameter>()->decorations =
                add_flag(dest.local()->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
        instructions.emplace_back(
            new Copy(std::move(dest), toValue(method, store->getValueOperand(), &instructions), true, false));
        instructions.back()->setDecorations(deco);
        break;
    }
    case CastOps::AddrSpaceCast:
        // TODO need to handle somehow special? E.g. need to track address spaces? Or is it allowed at all?
        FALL_THROUGH
    case CastOps::BitCast:
    {
        instructions.emplace_back(new Copy(toValue(method, &inst, &instructions),
            toValue(method, inst.getOperand(0), &instructions), false, false, true));
        instructions.back()->setDecorations(deco);
        break;
    }
#if LLVM_LIBRARY_VERSION >= 80
    case UnaryOps::FNeg:
#endif
    case CastOps::FPExt:
        FALL_THROUGH
    case CastOps::FPToSI:
        FALL_THROUGH
    case CastOps::FPToUI:
        FALL_THROUGH
    case CastOps::FPTrunc:
        FALL_THROUGH
    case CastOps::SExt:
        FALL_THROUGH
    case CastOps::SIToFP:
        FALL_THROUGH
    case CastOps::Trunc:
        FALL_THROUGH
    case CastOps::UIToFP:
    {
        instructions.emplace_back(new UnaryOperator(inst.getOpcodeName(), toValue(method, &inst, &instructions),
            toValue(method, inst.getOperand(0), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case CastOps::IntToPtr:
        FALL_THROUGH
    case CastOps::PtrToInt:
        FALL_THROUGH
    case CastOps::ZExt:
    {
        /*
         * Both inttoptr-cast and ptrtoint-case say:
         * "by applying either a zero extension or a truncation"
         */
        instructions.emplace_back(new UnaryOperator(
            "zext", toValue(method, &inst, &instructions), toValue(method, inst.getOperand(0), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::Call:
    {
        const llvm::CallInst* call = llvm::cast<const llvm::CallInst>(&inst);
        std::vector<Value> args;
        for(unsigned i = 0; i < call->getNumArgOperands(); ++i)
        {
            args.emplace_back(toValue(method, call->getArgOperand(i), &instructions));
        }
        const llvm::Function* func = call->getCalledFunction();
        if(func == nullptr)
        {
// e.g. for alias - see https://stackoverflow.com/questions/22143143/
#if LLVM_LIBRARY_VERSION >= 100
            if(auto alias = llvm::dyn_cast<const llvm::GlobalAlias>(call->getCalledOperand()))
#else
            if(auto alias = llvm::dyn_cast<const llvm::GlobalAlias>(call->getCalledValue()))
#endif
            {
                if(auto constExpr = llvm::dyn_cast<const llvm::ConstantExpr>(alias->getAliasee()))
                {
                    // e.g. function-pointer bit-cast for aliasing __generic address space (OpenCL C 2.x, C++ for
                    // OpenCL) to __global address space
                    if(constExpr->getOpcode() == llvm::Instruction::CastOps::BitCast)
                        func = llvm::dyn_cast<const llvm::Function>(constExpr->getOperand(0));
                }
                else
                    func = llvm::dyn_cast<const llvm::Function>(alias->getAliasee());
                if(func)
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Aliasing function '" << static_cast<std::string>(alias->getName())
                            << "' to function: " << static_cast<std::string>(func->getName()) << logging::endl);
            }
        }
        if(func == nullptr)
        {
            dumpLLVM(call);
            throw CompilationError(CompilationStep::PARSER, "Unhandled type of indirect function call!",
                static_cast<std::string>(call->getName()));
        }
        if(func->isDeclaration())
        {
            // functions without definitions (e.g. intrinsic functions)
            std::string funcName = static_cast<std::string>(func->getName());
            instructions.emplace_back(new CallSite(toValue(method, call, &instructions),
                cleanMethodName(funcName.find("_Z") == 0 ? std::string("@") + funcName : funcName), std::move(args)));
        }
        else
        {
            Method& dest = parseFunction(module, *func);
            instructions.emplace_back(
                new CallSite(toValue(method, call, &instructions), dest, std::move(args), func->isVarArg()));
        }

        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::ExtractElement:
    {
        instructions.emplace_back(new ContainerExtraction(toValue(method, &inst, &instructions),
            toValue(method, inst.getOperand(0), &instructions), toValue(method, inst.getOperand(1), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::ExtractValue:
    {
        const llvm::ExtractValueInst* extraction = llvm::cast<const llvm::ExtractValueInst>(&inst);
        if(extraction->getIndices().size() != 1)
        {
            dumpLLVM(extraction);
            throw CompilationError(
                CompilationStep::PARSER, "Container extraction with multi-level indices is not yet implemented!");
        }
        instructions.emplace_back(new ContainerExtraction(toValue(method, extraction, &instructions),
            toValue(method, extraction->getAggregateOperand(), &instructions),
            Value(Literal(extraction->getIndices().front()), TYPE_INT32)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::FCmp:
        FALL_THROUGH
    case OtherOps::ICmp:
    {
        const llvm::CmpInst* comp = llvm::cast<const llvm::CmpInst>(&inst);
        auto compName = toComparison(comp->getPredicate());
        instructions.emplace_back(new Comparison(toValue(method, &inst, &instructions), std::move(compName),
            toValue(method, inst.getOperand(0), &instructions), toValue(method, inst.getOperand(1), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }

    case OtherOps::InsertElement:
    {
        instructions.emplace_back(new ContainerInsertion(toValue(method, &inst, &instructions),
            toValue(method, inst.getOperand(0), &instructions), toValue(method, inst.getOperand(1), &instructions),
            toValue(method, inst.getOperand(2), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::InsertValue:
    {
        const llvm::InsertValueInst* insertion = llvm::cast<const llvm::InsertValueInst>(&inst);
        if(insertion->getIndices().size() != 1)
        {
            dumpLLVM(insertion);
            throw CompilationError(
                CompilationStep::PARSER, "Container insertion with multi-level indices is not yet implemented!");
        }
        instructions.emplace_back(new ContainerInsertion(toValue(method, insertion, &instructions),
            toValue(method, insertion->getAggregateOperand(), &instructions),
            toValue(method, insertion->getInsertedValueOperand(), &instructions),
            Value(Literal(insertion->getIndices().front()), TYPE_INT32)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::PHI:
    {
        const llvm::PHINode* phi = llvm::cast<const llvm::PHINode>(&inst);
        std::vector<std::pair<Value, const Local*>> labels;
        for(unsigned i = 0; i < phi->getNumIncomingValues(); ++i)
        {
            labels.emplace_back(std::make_pair(toValue(method, phi->getIncomingValue(i), &instructions),
                toValue(method, phi->getIncomingBlock(i), &instructions).local()));
        }
        instructions.emplace_back(new PhiNode(toValue(method, phi, &instructions), std::move(labels)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::Select:
    {
        const llvm::SelectInst* selection = llvm::cast<const llvm::SelectInst>(&inst);
        instructions.emplace_back(new Selection(toValue(method, selection, &instructions),
            toValue(method, selection->getCondition(), &instructions),
            toValue(method, selection->getTrueValue(), &instructions),
            toValue(method, selection->getFalseValue(), &instructions)));
        instructions.back()->setDecorations(deco);
        break;
    }
    case OtherOps::ShuffleVector:
    {
        const llvm::ShuffleVectorInst* shuffle = llvm::cast<const llvm::ShuffleVectorInst>(&inst);
        instructions.emplace_back(new ShuffleVector(toValue(method, shuffle, &instructions),
            toValue(method, shuffle->getOperand(0), &instructions),
            toValue(method, shuffle->getOperand(1), &instructions),
#if LLVM_LIBRARY_VERSION >= 110
            toValue(method, shuffle->getShuffleMaskForBitcode(), &instructions)));
#else
            toValue(method, shuffle->getMask(), &instructions)));
#endif
        instructions.back()->setDecorations(deco);
        break;
    }
    case TermOps::Unreachable:
    {
        // since this instruction can never be reached, we do not need to emit it. If it every gets reached, this is
        // UB anyway
        break;
    }
    default:
        dumpLLVM(&inst);
        throw CompilationError(CompilationStep::PARSER, "Unhandled LLVM op-code", inst.getOpcodeName());
    }
}

Value BitcodeReader::parseInlineGetElementPtr(
    Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Value* pointerOperand)
{
    // the value is given as an in-line getelementptr instruction, insert as extra instruction calculating
    // indices
    // the const_cast is required for the #getAsInstruction() call below for support of LLVM version <10
    auto constExpr = const_cast<llvm::ConstantExpr*>(llvm::cast<const llvm::ConstantExpr>(pointerOperand));
    if(constExpr->getOpcode() == llvm::Instruction::CastOps::BitCast)
    {
        // bitcast of address can simply be replace by loading of source address
        // the source could be a constant or a constant expression
        if(llvm::isa<llvm::ConstantExpr>(constExpr->getOperand(0)))
            constExpr = llvm::cast<llvm::ConstantExpr>(constExpr->getOperand(0));
        else
        {
            return toValue(method, constExpr->getOperand(0), &instructions);
        }
    }
    if(constExpr != nullptr)
    {
        if(constExpr->getOpcode() != llvm::Instruction::MemoryOps::GetElementPtr)
        {
            dumpLLVM(constExpr);
            throw CompilationError(CompilationStep::PARSER, "Invalid constant operation for load-instruction!");
        }
        // FIXME this leaks the indexOf instruction. But if we wrap this in a unique_ptr, some memory access tests start
        // to fail... ?!? Probably occurs because toValue(...) needs the Value pointer to be stable, since it is stored
        // in a global map...
        auto indexOf = llvm::cast<llvm::GetElementPtrInst>(constExpr->getAsInstruction());
        parseInstruction(module, method, instructions, *indexOf);
        auto tmp = toValue(method, indexOf, &instructions);
        // required so LLVM can clean up the constant expression correctly
        indexOf->dropAllReferences();
        return tmp;
    }
    return UNDEFINED_VALUE;
}

Value BitcodeReader::toValue(Method& method, const llvm::Value* val, LLVMInstructionList* instructions)
{
    auto it = localMap.find(val);
    if(it != localMap.end())
    {
        return it->second->createReference();
    }
    const Local* loc = nullptr;
    const std::string valueName = val->getName().empty() ? "" : (std::string("%") + val->getName()).str();
    if((loc = method.findParameter(valueName)) != nullptr || (loc = method.findStackAllocation(valueName)) != nullptr ||
        (loc = method.findGlobal((std::string("@") + val->getName()).str())) != nullptr)
    {
        return loc->createReference();
    }
    if(llvm::dyn_cast<const llvm::BranchInst>(val) != nullptr)
    {
        // label
        auto loc = method.addNewLocal(TYPE_LABEL, "%label").local();
        localMap[val] = loc;
        return loc->createReference();
    }
    const DataType type = toDataType(method.module, val->getType());
    if(llvm::dyn_cast<const llvm::Constant>(val) != nullptr)
    {
        return toConstant(method.module, val, &method, instructions);
    }
    // locals of any kind have no name (most of the time)
    if(!val->getName().empty())
        loc = method.createLocal(type, valueName);
    else
        loc = method.addNewLocal(type).local();
    localMap.emplace(val, loc);
    return loc->createReference();
}

Value BitcodeReader::toConstant(
    Module& module, const llvm::Value* val, Method* method, LLVMInstructionList* instructions)
{
    const DataType type = toDataType(module, val->getType());
    if(auto constant = llvm::dyn_cast<const llvm::ConstantInt>(val))
    {
        if(constant->getSExtValue() > std::numeric_limits<uint32_t>::max() ||
            constant->getSExtValue() < std::numeric_limits<int32_t>::min())
        {
            if(method && instructions)
            {
                // insert instruction loading the 64-bit constant, since we cannot handle 64-bit literals
                auto constantValue = method->addNewLocal(type, "%constant");
                instructions->emplace_back(new LongConstant(Value(constantValue), constant->getSExtValue()));
                return constantValue;
            }
            dumpLLVM(constant);
            throw CompilationError(CompilationStep::PARSER, "Constant global value is out of valid range",
                std::to_string(constant->getSExtValue()));
        }
        if(constant->isNegative())
            return Value(toLongLiteral(bit_cast<int64_t, uint64_t>(constant->getSExtValue())).value(), type);
        return Value(Literal(static_cast<uint32_t>(constant->getZExtValue())), type);
    }
    /*
     * DO NOT COMBINE THE NEXT ELSE-CLAUSES
     *
     * For "standard" LLVM (4.0+), ConstantVector, ConstantArray and ConstantStruct have a common super-type
     * ConstantAggregate, but not yet for SPIRV-LLVM (~3.6)
     */
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantVector>(val))
    {
        // element types are stored as operands
        SIMDVector aggregate;
        // to allow for 64-bit vectors, we need to handle elements not fitting into literals
        std::vector<Value> elements;
        elements.reserve(constant->getNumOperands());
        bool allElementsAreLiterals = true;
        for(unsigned i = 0; i < constant->getNumOperands(); ++i)
        {
            auto element = toConstant(module, constant->getOperand(i), method, instructions);
            // don't check for Value#getLiteralValue(), since we want to allow UNDEFINED literal elements
            if(element.checkLocal())
                allElementsAreLiterals = false;
            else
                aggregate[i] = *element.getLiteralValue();
            elements.emplace_back(std::move(element));
        }
        if(allElementsAreLiterals)
            return module.storeVector(std::move(aggregate), type);
        else if(method && instructions)
        {
            // e.g. for 64-bit vectors, the elements do not fit into literals, so we need to build up the vector
            // manually
            auto constantValue = method->addNewLocal(type, "%constant");
            instructions->emplace_back(new LongConstant(Value(constantValue), std::move(elements)));
            return constantValue;
        }
        else
        {
            dumpLLVM(constant);
            throw CompilationError(CompilationStep::PARSER, "64-bit constant global vectors are not yet implemented");
        }
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantDataSequential>(val))
    {
        // vector/array constant, but packed in storage
        SIMDVector aggregate;
        // to allow for 64-bit vectors, we need to handle elements not fitting into literals
        std::vector<Value> elements;
        elements.reserve(constant->getNumElements());
        bool allElementsAreLiterals = true;
        for(unsigned i = 0; i < constant->getNumElements(); ++i)
        {
            auto element = toConstant(module, constant->getElementAsConstant(i), method, instructions);
            // don't check for Value#getLiteralValue(), since we want to allow UNDEFINED literal elements
            if(element.checkLocal())
                allElementsAreLiterals = false;
            else
                aggregate[i] = *element.getLiteralValue();
            elements.emplace_back(std::move(element));
        }
        if(allElementsAreLiterals)
            return module.storeVector(std::move(aggregate), type);
        else if(method && instructions)
        {
            // e.g. for 64-bit vectors, the elements do not fit into literals, so we need to build up the vector
            // manually
            auto constantValue = method->addNewLocal(type, "%constant");
            instructions->emplace_back(new LongConstant(Value(constantValue), std::move(elements)));
            return constantValue;
        }
        else
        {
            dumpLLVM(constant);
            throw CompilationError(CompilationStep::PARSER, "64-bit constant global vectors are not yet implemented");
        }
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantAggregateZero>(val))
    {
        return module.storeVector(SIMDVector(Literal(0u)), type);
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantFP>(val))
    {
        return Value(Literal(constant->getValueAPF().convertToFloat()), type);
    }
    else if(llvm::dyn_cast<const llvm::ConstantPointerNull>(val) != nullptr)
    {
        return Value(Literal(0u), type);
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantExpr>(val))
    {
        return precalculateConstantExpression(module, constant, method, instructions);
    }
    else if(auto global = llvm::dyn_cast<const llvm::GlobalVariable>(val))
    {
        const std::string name = ("@" + val->getName()).str();
        auto globalIt = std::find_if(module.globalData.begin(), module.globalData.end(),
            [&name](const Global& global) -> bool { return global.name == name; });
        if(globalIt != module.globalData.end())
            return globalIt->createReference();

        module.globalData.emplace_back(name, toDataType(module, global->getType()),
            global->hasInitializer() ? toConstantGlobal(module, global->getInitializer()) :
                                       CompoundConstant(TYPE_UNKNOWN, UNDEFINED_LITERAL),
            global->isConstant());
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Global read: " << module.globalData.back().to_string() << logging::endl);
        localMap[val] = &module.globalData.back();
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
        dumpLLVM(val);
        throw CompilationError(CompilationStep::PARSER, "Unhandled constant type for non-global constant",
            std::to_string(val->getValueID()));
    }
}

Value BitcodeReader::precalculateConstantExpression(
    Module& module, const llvm::ConstantExpr* expr, Method* method, LLVMInstructionList* instructions)
{
    if(expr->getOpcode() == llvm::Instruction::CastOps::BitCast ||
        expr->getOpcode() == llvm::Instruction::CastOps::AddrSpaceCast)
    {
        Value result = toConstant(module, expr->getOperand(0), method, instructions);
        if(expr->getOperand(0)->getType()->getScalarSizeInBits() != expr->getType()->getScalarSizeInBits())
        {
            dumpLLVM(expr);
            throw CompilationError(
                CompilationStep::PARSER, "Bit-casts over different type-sizes are not yet implemented!");
        }
        result.type = toDataType(module, expr->getType());
        return result;
    }
    if(expr->getOpcode() == llvm::Instruction::MemoryOps::GetElementPtr)
    {
        // e.g. in-line getelementptr in load or store instructions
        Value srcConstant = toConstant(module, expr->getOperand(0), method, instructions);
        std::vector<Value> indices;
        bool allIndicesAreZero = true;
        indices.reserve(expr->getNumOperands());
        for(unsigned i = 1; i < expr->getNumOperands(); ++i)
        {
            indices.emplace_back(toConstant(module, expr->getOperand(i), method, instructions));
            if(!indices.back().isZeroInitializer())
                allIndicesAreZero = false;
        }

        if(allIndicesAreZero)
        {
            // indices of all zero -> reference to the global itself (or the first entry)
            // we need to make the type of the value fit the return-type expected
            srcConstant.type = toDataType(module, expr->getType());
            return srcConstant;
        }
        else if(method && instructions)
        {
            // insert dynamic calculation of indices
            auto elementOffset = method->addNewLocal(toDataType(module, expr->getType()), "%constant_offset");
            instructions->emplace_back(new IndexOf(Value(elementOffset), std::move(srcConstant), std::move(indices)));
            return elementOffset;
        }
        else
        {
            dumpLLVM(expr);
            throw CompilationError(CompilationStep::PARSER,
                "Only constant global getelementptr without offsets are supported for now",
                static_cast<std::string>(expr->getName()));
        }
    }
    if(expr->getOpcode() == llvm::Instruction::CastOps::PtrToInt ||
        expr->getOpcode() == llvm::Instruction::CastOps::IntToPtr ||
        expr->getOpcode() == llvm::Instruction::CastOps::ZExt)
    {
        // int <-> pointer cast is a zext (+ type conversion)
        const Value src = toConstant(module, expr->getOperand(0), method, instructions);
        const DataType destType = toDataType(module, expr->getType());
        // if the bit-widths of the source and destination are the the same width, simply return the source
        // otherwise, simply truncate or zero-extend
        Value dest(src);
        dest.type = destType;
        switch(src.type.getScalarBitCount())
        {
        case 32:
        {
            switch(destType.getScalarBitCount())
            {
            case 64:
                // this is e.g. pointer to long conversion, the actual writing of the lower part only and filling of the
                // upper part is handled while lowering long operations
                dest.type = TYPE_INT32.toVectorType(dest.type.getVectorWidth());
                FALL_THROUGH
            case 32:
                return dest;
            case 16:
                return PACK_INT_TO_SHORT_TRUNCATE(dest, {}).value();
            case 8:
                return PACK_INT_TO_CHAR_TRUNCATE(dest, {}).value();
            }
            break;
        }
        case 16:
            switch(destType.getScalarBitCount())
            {
            case 16:
                return dest;
            case 8:
                return UNPACK_CHAR_TO_INT_ZEXT(dest).value();
            }
            break;
        case 8:
            switch(destType.getScalarBitCount())
            {
            case 32:
                FALL_THROUGH
            case 16:
                return UNPACK_CHAR_TO_INT_ZEXT(dest).value();
            case 8:
                return dest;
            }
        }
        dumpLLVM(expr);
        throw CompilationError(CompilationStep::PARSER, "Unhandled bit-width of type", src.to_string());
    }
    if(expr->getOpcode() == llvm::Instruction::OtherOps::ICmp || expr->getOpcode() == llvm::Instruction::OtherOps::FCmp)
    {
        const DataType destType = toDataType(module, expr->getType());
        const DataType boolType = TYPE_BOOL.toVectorType(destType.getVectorWidth());

        const Value src0 = toConstant(module, expr->getOperand(0), method, instructions);
        const Value src1 = toConstant(module, expr->getOperand(1), method, instructions);

        auto predicate = expr->getPredicate();

        switch(predicate)
        {
        case llvm::CmpInst::FCMP_FALSE:
            return Value(Literal(false), boolType);
        case llvm::CmpInst::FCMP_OEQ:
            return Value(Literal(src0.getLiteralValue()->real() == src1.getLiteralValue()->real()), boolType);
        case llvm::CmpInst::FCMP_OGT:
            return Value(Literal(src0.getLiteralValue()->real() > src1.getLiteralValue()->real()), boolType);
        case llvm::CmpInst::FCMP_OGE:
            return Value(Literal(src0.getLiteralValue()->real() >= src1.getLiteralValue()->real()), boolType);
        case llvm::CmpInst::FCMP_OLT:
            return Value(Literal(src0.getLiteralValue()->real() < src1.getLiteralValue()->real()), boolType);
        case llvm::CmpInst::FCMP_OLE:
            return Value(Literal(src0.getLiteralValue()->real() <= src1.getLiteralValue()->real()), boolType);
        case llvm::CmpInst::FCMP_ONE:
            return Value(Literal(src0.getLiteralValue()->real() != src1.getLiteralValue()->real()), boolType);
        case llvm::CmpInst::FCMP_ORD:
        case llvm::CmpInst::FCMP_UNO:
        case llvm::CmpInst::FCMP_UEQ:
        case llvm::CmpInst::FCMP_UGT:
        case llvm::CmpInst::FCMP_UGE:
        case llvm::CmpInst::FCMP_ULT:
        case llvm::CmpInst::FCMP_ULE:
        case llvm::CmpInst::FCMP_UNE:
            break;
        case llvm::CmpInst::FCMP_TRUE:
            return Value(Literal(true), destType);
        case llvm::CmpInst::ICMP_EQ:
            return Value(
                Literal(src0.getLiteralValue()->unsignedInt() == src1.getLiteralValue()->unsignedInt()), boolType);
        case llvm::CmpInst::ICMP_NE:
            return Value(
                Literal(src0.getLiteralValue()->unsignedInt() != src1.getLiteralValue()->unsignedInt()), boolType);
        case llvm::CmpInst::ICMP_UGT:
            return Value(
                Literal(src0.getLiteralValue()->unsignedInt() > src1.getLiteralValue()->unsignedInt()), boolType);
        case llvm::CmpInst::ICMP_UGE:
            return Value(
                Literal(src0.getLiteralValue()->unsignedInt() >= src1.getLiteralValue()->unsignedInt()), boolType);
        case llvm::CmpInst::ICMP_ULT:
            return Value(
                Literal(src0.getLiteralValue()->unsignedInt() < src1.getLiteralValue()->unsignedInt()), boolType);
        case llvm::CmpInst::ICMP_ULE:
            return Value(
                Literal(src0.getLiteralValue()->unsignedInt() <= src1.getLiteralValue()->unsignedInt()), boolType);
        case llvm::CmpInst::ICMP_SGT:
            return Value(Literal(src0.getLiteralValue()->signedInt() > src1.getLiteralValue()->signedInt()), boolType);
        case llvm::CmpInst::ICMP_SGE:
            return Value(Literal(src0.getLiteralValue()->signedInt() >= src1.getLiteralValue()->signedInt()), boolType);
        case llvm::CmpInst::ICMP_SLT:
            return Value(Literal(src0.getLiteralValue()->signedInt() < src1.getLiteralValue()->signedInt()), boolType);
        case llvm::CmpInst::ICMP_SLE:
            return Value(Literal(src0.getLiteralValue()->signedInt() <= src1.getLiteralValue()->signedInt()), boolType);
        default:
            break;
        }
    }

    OpCode opCode = OpCode::findOpCode(expr->getOpcodeName());

    Optional<Value> result = NO_VALUE;
    if(opCode.numOperands == 1)
        result = opCode(toConstant(module, expr->getOperand(0), method, instructions), NO_VALUE).first;
    else if(opCode.numOperands == 2)
        result = opCode(toConstant(module, expr->getOperand(0), method, instructions),
            toConstant(module, expr->getOperand(1), method, instructions))
                     .first;

    if(result)
        return result.value();
    dumpLLVM(expr);
    throw CompilationError(
        CompilationStep::PARSER, "This type of constant expression is not supported yet", expr->getOpcodeName());
}

CompoundConstant BitcodeReader::toConstantGlobal(Module& module, const llvm::Value* val)
{
    const DataType type = toDataType(module, val->getType());
    if(auto constant = llvm::dyn_cast<const llvm::ConstantInt>(val))
    {
        if(constant->getSExtValue() > std::numeric_limits<uint32_t>::max() ||
            constant->getSExtValue() < std::numeric_limits<int32_t>::min())
        {
            dumpLLVM(constant);
            throw CompilationError(CompilationStep::PARSER, "Constant value is out of valid range",
                std::to_string(constant->getSExtValue()));
        }
        if(constant->isNegative())
            return CompoundConstant(type, Literal(static_cast<int32_t>(constant->getSExtValue())));
        return CompoundConstant(type, Literal(static_cast<uint32_t>(constant->getZExtValue())));
    }
    /*
     * DO NOT COMBINE THE NEXT ELSE-CLAUSES
     *
     * For "standard" LLVM (4.0+), ConstantVector, ConstantArray and ConstantStruct have a common super-type
     * ConstantAggregate, but not yet for SPIRV-LLVM (~3.6)
     */
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantVector>(val))
    {
        // element types are stored as operands
        std::vector<CompoundConstant> aggregate;
        aggregate.reserve(constant->getNumOperands());
        for(unsigned i = 0; i < constant->getNumOperands(); ++i)
        {
            aggregate.emplace_back(toConstantGlobal(module, constant->getOperand(i)));
        }
        return CompoundConstant(type, std::move(aggregate));
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantArray>(val))
    {
        // element types are stored as operands
        std::vector<CompoundConstant> aggregate;
        aggregate.reserve(constant->getNumOperands());
        for(unsigned i = 0; i < constant->getNumOperands(); ++i)
        {
            aggregate.emplace_back(toConstantGlobal(module, constant->getOperand(i)));
        }
        return CompoundConstant(type, std::move(aggregate));
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantStruct>(val))
    {
        // special treatment for spirv.ConstantSampler type
        if(type == TYPE_SAMPLER)
        {
            intermediate::Sampler sampler(
                static_cast<intermediate::AddressingMode>(
                    toConstant(module, constant->getOperand(0)).getLiteralValue()->unsignedInt()),
                toConstant(module, constant->getOperand(1)).getLiteralValue()->isTrue(),
                static_cast<intermediate::FilterMode>(
                    toConstant(module, constant->getOperand(2)).getLiteralValue()->unsignedInt()));
            return CompoundConstant(TYPE_SAMPLER, Literal(static_cast<uint32_t>(sampler)));
        }
        // element types are stored as operands
        std::vector<CompoundConstant> aggregate;
        aggregate.reserve(constant->getNumOperands());
        for(unsigned i = 0; i < constant->getNumOperands(); ++i)
        {
            aggregate.push_back(toConstantGlobal(module, constant->getOperand(i)));
        }
        return CompoundConstant(type, std::move(aggregate));
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantDataSequential>(val))
    {
        // vector/array constant, but packed in storage
        std::vector<CompoundConstant> aggregate;
        aggregate.reserve(constant->getNumElements());
        for(unsigned i = 0; i < constant->getNumElements(); ++i)
        {
            aggregate.emplace_back(toConstantGlobal(module, constant->getElementAsConstant(i)));
        }
        return CompoundConstant(type, std::move(aggregate));
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantAggregateZero>(val))
    {
        std::vector<CompoundConstant> aggregate;
        aggregate.reserve(constant->getNumElements());
        for(unsigned i = 0; i < constant->getNumElements(); ++i)
        {
            aggregate.emplace_back(toConstantGlobal(module, constant->getElementValue(i)));
        }
        return CompoundConstant(type, std::move(aggregate));
    }
    else if(auto constant = llvm::dyn_cast<const llvm::ConstantFP>(val))
    {
        return CompoundConstant(type, Literal(constant->getValueAPF().convertToFloat()));
    }
    else if(llvm::dyn_cast<const llvm::ConstantPointerNull>(val) != nullptr)
    {
        return CompoundConstant(type, Literal(0u));
    }
    else if(llvm::dyn_cast<const llvm::UndefValue>(val) != nullptr)
    {
        return CompoundConstant(type, UNDEFINED_LITERAL);
    }
    else
    {
        dumpLLVM(val);
        throw CompilationError(
            CompilationStep::PARSER, "Unhandled constant type for global constant", std::to_string(val->getValueID()));
    }
}

#endif
