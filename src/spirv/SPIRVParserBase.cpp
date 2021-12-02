/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVParserBase.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../intrinsics/Images.h"
#include "SPIRVBuiltins.h"
#include "SPIRVHelper.h"
#include "log.h"

#include <chrono>
#include <cstdint>
#include <cstring>

using namespace vc4c;
using namespace vc4c::spirv;

// out-of-line destructor
ParsedInstruction::~ParsedInstruction() noexcept = default;

SPIRVParserBase::SPIRVParserBase(std::istream& input, const bool isSPIRVText) :
    isTextInput(isSPIRVText), inputWords(readStreamOfWords(input)), currentMethod(nullptr), module(nullptr)
{
}

SPIRVParserBase::SPIRVParserBase(std::vector<uint32_t>&& input, bool isSPIRVText) :
    isTextInput(isSPIRVText), inputWords(std::move(input)), currentMethod(nullptr), module(nullptr)
{
}

SPIRVParserBase::~SPIRVParserBase() = default;

void SPIRVParserBase::parse(Module& module)
{
    this->module = &module;

    // if input is SPIR-V text, convert to binary representation
    if(isTextInput)
    {
        inputWords = assembleTextToBinary(inputWords);
    }
    else
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Read SPIR-V binary with " << inputWords.size() << " words" << logging::endl);
    }

    // parse input
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Starting parsing..." << logging::endl);
    doParse(inputWords);
    CPPLOG_LAZY(logging::Level::DEBUG, log << "SPIR-V binary successfully parsed" << logging::endl);

    // resolve method parameters
    // set names, e.g. for methods, parameters
    for(auto& m : methods)
    {
        auto it = names.find(m.first);
        if(it != names.end())
            m.second.method->name = it->second;
        m.second.method->parameters.reserve(m.second.parameters.size());
        std::istringstream parameterTypeNames{};
        {
            // This is e.g. used to specify the original kernel parameter names, at least for more recent clang/SPIR-V
            // compilers, see
            // https://github.com/KhronosGroup/SPIRV-LLVM-Translator/blob/5de39350b76246609a2b233e9453e54f48f0a9c6/lib/SPIRV/SPIRVWriter.cpp#L1910
            // this is in the format "kernel_arg_type.%kernel_name%.typename0,typename1,..."
            auto searchText = "kernel_arg_type." + m.second.method->name;
            auto it = std::find_if(
                strings.begin(), strings.end(), [&](const auto& s) -> bool { return s.find(searchText) == 0; });
            if(it != strings.end() && it->find('.') != std::string::npos)
                parameterTypeNames.str(it->substr(it->find_last_of('.') + 1));
        }
        for(const auto& pair : m.second.parameters)
        {
            auto type = typeMappings.at(pair.second);
            Parameter param(std::string("%") + std::to_string(pair.first), type);
            auto it2 = decorationMappings.find(pair.first);
            if(it2 != decorationMappings.end())
                setParameterDecorations(param, it2->second);
            it = names.find(pair.first);
            if(it != names.end())
                // parameters are referenced by their IDs, not their names, but for meta-data the names are better
                param.parameterName = it->second;

            if(param.type.getImageType())
                intermediate::reserveImageConfiguration(module, param);

            std::string parameterType{};
            if(parameterTypeNames && std::getline(parameterTypeNames, parameterType, ','))
                param.origTypeName = parameterType;

            auto& ptr = m.second.method->addParameter(std::move(param));
            memoryAllocatedData.emplace(pair.first, &ptr);
        }

        // to support OpenCL built-in operations, we need to demangle all VC4CL std-lib definitions of the OpenCL C
        // standard functions to be able to map them correctly.
        m.second.method->name = demangleFunctionName(m.second.method->name);
    }

    // map SPIRVOperations to IntermediateInstructions
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Mapping instructions to intermediate..." << logging::endl);
    for(const auto& op : instructions)
    {
        op->mapInstruction(typeMappings, constantMappings, localTypes, methods, memoryAllocatedData);
    }

    // apply kernel meta-data, decorations, ...
    for(const auto& pair : metadataMappings)
    {
        Method& method = *methods.at(pair.first).method.get();
        for(const auto& meta : pair.second)
        {
            switch(meta.first)
            {
            case MetaDataType::WORK_GROUP_SIZES:
            {
                method.metaData.workGroupSizes = meta.second;
                MetaData entry{};
                entry.setValue<MetaData::Type::KERNEL_WORK_GROUP_SIZE>(meta.second);
                method.metaData.entries.push_back(std::move(entry));
                break;
            }
            case MetaDataType::WORK_GROUP_SIZES_HINT:
            {
                method.metaData.workGroupSizeHints = meta.second;
                MetaData entry{};
                entry.setValue<MetaData::Type::KERNEL_WORK_GROUP_SIZE_HINT>(meta.second);
                method.metaData.entries.push_back(std::move(entry));
                break;
            }
            case MetaDataType::VECTOR_TYPE_HINT:
            {
                method.metaData.workGroupSizeHints = meta.second;
                MetaData entry{};
                auto numElements = meta.second[0];
                auto typeId = meta.second[1];
                /*
                 * These are the legal data type values:
                 * 0 represents an 8-bit integer value.
                 * 1 represents a 16-bit integer value.
                 * 2 represents a 32-bit integer value.
                 * 3 represents a 64-bit integer value.
                 * 4 represents a 16-bit float value.
                 * 5 represents a 32-bit float value.
                 * 6 represents a 64-bit float value.
                 */
                // TODO signedness?
                const std::vector<std::string> types = {"char", "short", "int", "long", "half", "float", "double"};
                entry.setValue<MetaData::Type::KERNEL_VECTOR_TYPE_HINT>(
                    types.at(typeId) + (numElements > 1 ? std::to_string(numElements) : ""));
                method.metaData.entries.push_back(std::move(entry));
                break;
            }
            default:
                throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data type",
                    std::to_string(static_cast<uint32_t>(meta.first)));
            }
        }
    }

    // delete empty functions (e.g. declared, but not defined, externally linked, intrinsics)
    auto it = methods.begin();
    while(it != methods.end())
    {
        if(it->second.method->countInstructions() == 0)
            it = methods.erase(it);
        else if(it->second.method->countInstructions() == 1 && it->second.method->begin()->empty())
        {
            // only instruction is the label (which is automatically added)
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Dropping empty function: " << it->second.method->name << logging::endl);
            it = methods.erase(it);
        }
        else
            ++it;
    }

    module.methods.reserve(methods.size());
    for(auto& method : methods)
    {
        module.methods.emplace_back(std::move(method.second.method));
    }

    addFunctionAliases(module);
}

ParseResultCode SPIRVParserBase::parseHeader(uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound)
{
    // see:
    // https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#_a_id_physicallayout_a_physical_layout_of_a_spir_v_module_and_instruction
    // not completely true, since the header is not mapped to instructions, but still better than increasing every X new
    // instruction
    instructions.reserve(id_bound);

    return ParseResultCode::SUCCESS;
}

intermediate::InstructionDecorations toInstructionDecoration(const spv::FPFastMathModeMask mode)
{
    intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE;
    if(has_flag(mode, spv::FPFastMathModeMask::AllowRecip))
        decorations = add_flag(decorations, intermediate::InstructionDecorations::ALLOW_RECIP);
    if(has_flag(mode, spv::FPFastMathModeMask::Fast))
        decorations = add_flag(decorations, intermediate::InstructionDecorations::FAST_MATH);
    if(has_flag(mode, spv::FPFastMathModeMask::NotInf))
        decorations = add_flag(decorations, intermediate::InstructionDecorations::NO_INF);
    if(has_flag(mode, spv::FPFastMathModeMask::NotNaN))
        decorations = add_flag(decorations, intermediate::InstructionDecorations::NO_NAN);
    return decorations;
}

intermediate::InstructionDecorations SPIRVParserBase::toInstructionDecorations(const uint32_t id)
{
    intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;
    auto decoIt = decorationMappings.find(id);
    if(decoIt == decorationMappings.end())
    {
        return intermediate::InstructionDecorations::NONE;
    }
    for(auto decoration : getDecorations(decoIt->second, spv::Decoration::FPFastMathMode))
        deco = add_flag(deco, toInstructionDecoration(static_cast<spv::FPFastMathModeMask>(decoration)));
    if(getDecoration(decoIt->second, spv::Decoration::SaturatedConversion))
        deco = add_flag(deco, intermediate::InstructionDecorations::SATURATED_CONVERSION);
    // these are provided by the SPV_KHR_no_integer_wrap_decoration extension
    if(getDecoration(decoIt->second, spv::Decoration::NoSignedWrap))
        deco = add_flag(deco, intermediate::InstructionDecorations::SIGNED_OVERFLOW_IS_UB);
    if(getDecoration(decoIt->second, spv::Decoration::NoUnsignedWrap))
        deco = add_flag(deco, intermediate::InstructionDecorations::UNSIGNED_OVERFLOW_IS_UB);
    return deco;
}

ParseResultCode SPIRVParserBase::parseDecoration(const ParsedInstruction& parsed_instruction, uint32_t value)
{
    const uint32_t target = parsed_instruction.getWord(1);
    switch(static_cast<spv::Decoration>(parsed_instruction.getWord(2)))
    {
    case spv::Decoration::CPacked:             // struct is "packed"
    case spv::Decoration::SaturatedConversion: // out-of range results are clamped, do not overflow
    case spv::Decoration::FuncParamAttr:
    case spv::Decoration::FPFastMathMode: // allows fast-math modes
    case spv::Decoration::SpecId:         // constant as specialization-value of OpSpecXXX
    case spv::Decoration::Alignment:      // known alignment of pointer
    case spv::Decoration::MaxByteOffset:  // known maximum offset (in bytes) from base address, this pointer is accessed
                                          // at
    case spv::Decoration::Constant:       // global data is constant, is never written
    case spv::Decoration::Restrict:       // memory behind pointer is restricted, e.g. not aliased to another pointer
    case spv::Decoration::Volatile:       // data behind pointer is volatile
        decorationMappings[target].emplace_back(static_cast<spv::Decoration>(parsed_instruction.getWord(2)), value);
        return ParseResultCode::SUCCESS;
    case spv::Decoration::AlignmentId: // known alignment of pointer, but as constant, not literal value
        decorationMappings[target].emplace_back(spv::Decoration::Alignment, value);
        return ParseResultCode::SUCCESS;
    case spv::Decoration::MaxByteOffsetId: // known maximum offset (in bytes) from base address, this pointer is
                                           // accessed at, but as constant, not literal value
        decorationMappings[target].emplace_back(spv::Decoration::MaxByteOffset, value);
        return ParseResultCode::SUCCESS;
    case spv::Decoration::BuiltIn: // entity (object, struct-member) represents given built-in
        switch(static_cast<spv::BuiltIn>(value))
        {
        case spv::BuiltIn::WorkDim:
        case spv::BuiltIn::GlobalSize:
        case spv::BuiltIn::GlobalInvocationId:
        case spv::BuiltIn::WorkgroupSize:
        case spv::BuiltIn::LocalInvocationId:
        case spv::BuiltIn::NumWorkgroups:
        case spv::BuiltIn::WorkgroupId:
        case spv::BuiltIn::GlobalOffset:
            decorationMappings[target].emplace_back(spv::Decoration::BuiltIn, value);
            return ParseResultCode::SUCCESS;
        default:
            logging::error() << "Met unsupported builtin decoration " << value << logging::endl;
            return ParseResultCode::UNSUPPORTED;
        }
    case spv::Decoration::Aliased: // pointer needs to be accessed with aliased access in mind. XXX how/is it handled?
    case spv::Decoration::FPRoundingMode: // explicit rounding mode
        // handle unsupported decorations which can't be simply ignored
        logging::error() << "Met unsupported instruction-decoration " << parsed_instruction.getWord(2) << logging::endl;
        return ParseResultCode::UNSUPPORTED;
    default:
        // simply ignore all other decorations
        return ParseResultCode::SUCCESS;
    }
}

static intermediate::Sampler parseSampler(const ParsedInstruction& instruction)
{
    intermediate::Sampler tmp(0);
    switch(static_cast<spv::SamplerAddressingMode>(instruction.getWord(3)))
    {
    case spv::SamplerAddressingMode::None:
        tmp.setAddressingMode(intermediate::AddressingMode::NONE);
        break;
    case spv::SamplerAddressingMode::ClampToEdge:
        tmp.setAddressingMode(intermediate::AddressingMode::CLAMP_TO_EDGE);
        break;
    case spv::SamplerAddressingMode::Clamp:
        tmp.setAddressingMode(intermediate::AddressingMode::CLAMP);
        break;
    case spv::SamplerAddressingMode::Repeat:
        tmp.setAddressingMode(intermediate::AddressingMode::REPEAT);
        break;
    case spv::SamplerAddressingMode::RepeatMirrored:
        tmp.setAddressingMode(intermediate::AddressingMode::MIRRORED_REPEAT);
        break;
    default:
        throw CompilationError(
            CompilationStep::PARSER, "Unknown sampler addressing mode", std::to_string(instruction.getWord(3)));
    }

    tmp.setNormalizedCoordinates(instruction.getWord(4));

    switch(static_cast<spv::SamplerFilterMode>(instruction.getWord(5)))
    {
    case spv::SamplerFilterMode::Nearest:
        tmp.setFilterMode(intermediate::FilterMode::NEAREST);
        break;
    case spv::SamplerFilterMode::Linear:
        tmp.setFilterMode(intermediate::FilterMode::LINEAR);
        break;
    default:
        throw CompilationError(
            CompilationStep::PARSER, "Unknown sampler filter mode", std::to_string(instruction.getWord(5)));
    }

    return tmp;
}

static CompoundConstant parseConstant(const ParsedInstruction& instruction, const TypeMapping& typeMappings)
{
    CompoundConstant constant(typeMappings.at(instruction.getTypeId()), UNDEFINED_LITERAL);
    if(instruction.getNumWords() > 3)
    {
        //"Types 32 bits wide or smaller take one word."
        uint64_t val = instruction.getWord(3);
        if(instruction.getNumWords() > 4)
            //"[...] Larger types take multiple words, with low-order words appearing first."
            // e.g. for long/double constants
            val |= static_cast<uint64_t>(instruction.getWord(4)) << 32;
        if(auto lit = toLongLiteral(val))
        {
            constant = CompoundConstant(typeMappings.at(instruction.getTypeId()), *lit);

            if(constant.type.isFloatingType())
                // set correct type, just for cosmetic purposes
                constant = CompoundConstant(typeMappings.at(instruction.getTypeId()),
                    Literal(bit_cast<uint32_t, float>(static_cast<uint32_t>(val))));
        }
        else
        {
            // 64-bit word which does not fit into 32-bit literal, so make a compound constant
            auto compoundType = typeMappings.at(instruction.getTypeId());
            auto elementType = TYPE_INT32.toVectorType(compoundType.getVectorWidth());
            constant = CompoundConstant(compoundType,
                {
                    CompoundConstant(elementType, Literal(instruction.getWord(3))),
                    CompoundConstant(elementType, Literal(instruction.getWord(4))),
                });
        }
    }
    return constant;
}

static CompoundConstant parseConstantComposite(
    const ParsedInstruction& instruction, const TypeMapping& typeMappings, const ConstantMapping& constantMappings)
{
    DataType containerType = typeMappings.at(instruction.getWord(1));
    std::vector<CompoundConstant> constants;
    for(std::size_t i = 3; i < instruction.getNumWords(); ++i)
    {
        //"Result Type must be a composite type, whose top-level members/elements/components/columns have the same type
        // as the types of the Constituents."
        // -> no heterogeneous composite possible
        auto element = constantMappings.at(instruction.getWord(i));
        constants.push_back(element);
    }
    return CompoundConstant(containerType, std::move(constants));
}

static Optional<CompoundConstant> specializeConstant(const uint32_t resultID, DataType type,
    const FastMap<uint32_t, std::vector<std::pair<spv::Decoration, uint32_t>>>& decorations)
{
    auto it = decorations.find(resultID);
    if(it != decorations.end())
    {
        if(auto res = getDecoration(it->second, spv::Decoration::SpecId))
            return CompoundConstant(type, Literal(res.value()));
    }
    return {};
}

static SPIRVMethod& getOrCreateMethod(Module& module, MethodMapping& methods, const uint32_t id)
{
    if(methods.find(id) == methods.end())
        methods.emplace(id, SPIRVMethod(id, module));
    return methods.at(id);
}

#define UNSUPPORTED_INSTRUCTION(name)                                                                                  \
    (errorExtra = (name), logging::error() << "Unsupported SPIR-V instruction: " << (name) << logging::endl,           \
        ParseResultCode::UNSUPPORTED)

ParseResultCode SPIRVParserBase::parseInstruction(const ParsedInstruction& parsed_instruction)
{
    /*
     * Type instructions are resolved immediately
     * Constants are resolved immediately
     * Specializations are immediately mapped to constants
     * Names are resolved immediately
     * All instructions are enqueued to be mapped later
     *
     * Only opcodes for supported capabilities (or standard-opcodes) are listed here
     */

    // see: https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html#_a_id_instructions_a_instructions
    switch(parsed_instruction.getOpcode())
    {
    case spv::Op::OpNop:
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUndef:
        constantMappings.emplace(parsed_instruction.getResultId(),
            CompoundConstant(typeMappings.at(parsed_instruction.getTypeId()), UNDEFINED_LITERAL));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSourceContinued:
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSource:
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSourceExtension:
        break;
    case spv::Op::OpName: // e.g. method-name, parameters
        names[parsed_instruction.getWord(1)] = parsed_instruction.readLiteralString(1);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpMemberName: // name of struct-member
        return ParseResultCode::SUCCESS;
    case spv::Op::OpString: // e.g. kernel original parameter type names
        strings.emplace_back(parsed_instruction.readLiteralString(1));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLine: // source level debug info
        return ParseResultCode::SUCCESS;
    case spv::Op::OpExtension:
    {
        const std::string extension = parsed_instruction.readLiteralString(0);
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Using extension: " << extension << logging::endl);
        auto ret = checkExtension(extension);
        if(ret != ParseResultCode::SUCCESS)
            throw CompilationError(CompilationStep::PARSER, "Use of unsupported SPIR-V extension", extension);
        return ret;
    }
    case spv::Op::OpExtInstImport: // adds a new set of instructions
    {
        auto instructionSet = parsed_instruction.readLiteralString(1);
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Importing extended instruction set: " << instructionSet << logging::endl);
        if(instructionSet == "OpenCL.std")
            // https://www.khronos.org/registry/spir-v/specs/unified1/OpenCL.ExtendedInstructionSet.100.html
            extensionConsumers.emplace(parsed_instruction.getResultId(), &SPIRVParserBase::consumeOpenCLInstruction);
        else if(instructionSet == "DebugInfo")
            // https://www.khronos.org/registry/spir-v/specs/unified1/DebugInfo.html
            extensionConsumers.emplace(parsed_instruction.getResultId(), &SPIRVParserBase::consumeDebugInfoInstruction);
        else if(instructionSet == "OpenCL.DebugInfo.100")
            // https://www.khronos.org/registry/spir-v/specs/unified1/OpenCL.DebugInfo.100.html
            extensionConsumers.emplace(
                parsed_instruction.getResultId(), &SPIRVParserBase::consumeOpenCLDebugInfoInstruction);
        else if(instructionSet.find("NonSemantic."))
            // added by SPV_KHR_non_semantic_info extension, these extended instruction sets have only instructions
            // without semantic meaning and therefore can simply be ignored.
            extensionConsumers.emplace(
                parsed_instruction.getResultId(), &SPIRVParserBase::consumeNonSemanticInstruction);
        else
            throw CompilationError(CompilationStep::PARSER, "Unsupported extended instruction set", instructionSet);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpExtInst: // executes instruction from extended instruction set
    {
        auto extensionIt = extensionConsumers.find(parsed_instruction.getWord(3));
        if(extensionIt == extensionConsumers.end())
            throw CompilationError(CompilationStep::PARSER, "Invalid extended instruction set",
                std::to_string(parsed_instruction.getWord(3)));
        return (this->*(extensionIt->second))(parsed_instruction);
    }
    case spv::Op::OpMemoryModel:
    {
        auto addressingModel = static_cast<spv::AddressingModel>(parsed_instruction.getWord(1));
        auto memoryModel = static_cast<spv::MemoryModel>(parsed_instruction.getWord(2));
        if(addressingModel != spv::AddressingModel::Logical && addressingModel != spv::AddressingModel::Physical32)
            throw CompilationError(CompilationStep::PARSER, "Invalid addressing-mode");
        if(memoryModel != spv::MemoryModel::Simple && memoryModel != spv::MemoryModel::OpenCL)
            throw CompilationError(CompilationStep::PARSER, "Invalid memory model");
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using a " << (addressingModel == spv::AddressingModel::Logical ? "logical" : "physical") << " "
                << (memoryModel == spv::MemoryModel::Simple ? "simple" : "OpenCL") << " memory model" << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpEntryPoint: // an entry point is an OpenCL kernel
    {
        if(static_cast<spv::ExecutionModel>(parsed_instruction.getWord(1)) != spv::ExecutionModel::Kernel)
            throw CompilationError(CompilationStep::PARSER, "Invalid execution model");
        SPIRVMethod& m = getOrCreateMethod(*module, methods, parsed_instruction.getWord(2));
        m.method->flags = add_flag(m.method->flags, MethodFlags::KERNEL);
        m.method->name = parsed_instruction.readLiteralString(2);
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Kernel-method found: " << m.method->name << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpExecutionMode:
    {
        auto executionMode = static_cast<spv::ExecutionMode>(parsed_instruction.getWord(2));
        // only Kernel or native execution modes are supported
        if(executionMode == spv::ExecutionMode::LocalSize)
            //"Indicates the work-group size in the x, y, and z dimensions"
            metadataMappings[parsed_instruction.getWord(1)][MetaDataType::WORK_GROUP_SIZES] = {
                parsed_instruction.getWord(3), parsed_instruction.getWord(4), parsed_instruction.getWord(5)};
        else if(executionMode == spv::ExecutionMode::LocalSizeHint)
            //"A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z
            // dimensions"
            metadataMappings[parsed_instruction.getWord(1)][MetaDataType::WORK_GROUP_SIZES_HINT] = {
                parsed_instruction.getWord(3), parsed_instruction.getWord(4), parsed_instruction.getWord(5)};
        else if(executionMode == spv::ExecutionMode::VecTypeHint)
            /*
             * "A hint to the compiler, which indicates that most operations used in the entry point are explicitly
             * vectorized using a particular vector type. The 16 high-order bits of Vector Type operand specify the
             * number of components of the vector. The 16 low-order bits of Vector Type operand specify the data type of
             * the vector."
             */
            metadataMappings[parsed_instruction.getWord(1)][MetaDataType::VECTOR_TYPE_HINT] = {
                parsed_instruction.getWord(3) >> 16, parsed_instruction.getWord(3) & 0xFFFF, 0};
        else if(executionMode != spv::ExecutionMode::ContractionOff)
            throw CompilationError(CompilationStep::PARSER, "Invalid execution mode");
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpExecutionModeId:
    {
        auto executionMode = static_cast<spv::ExecutionMode>(parsed_instruction.getWord(2));
        // only Kernel or native execution modes are supported
        if(executionMode == spv::ExecutionMode::LocalSizeId)
            //"Indicates the work-group size in the x, y, and z dimensions"
            metadataMappings[parsed_instruction.getWord(1)][MetaDataType::WORK_GROUP_SIZES] = {
                constantMappings.at(parsed_instruction.getWord(3)).getScalar()->unsignedInt(),
                constantMappings.at(parsed_instruction.getWord(4)).getScalar()->unsignedInt(),
                constantMappings.at(parsed_instruction.getWord(5)).getScalar()->unsignedInt()};
        else if(executionMode == spv::ExecutionMode::LocalSizeHintId)
            //"A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z
            // dimensions"
            metadataMappings[parsed_instruction.getWord(1)][MetaDataType::WORK_GROUP_SIZES_HINT] = {
                constantMappings.at(parsed_instruction.getWord(3)).getScalar()->unsignedInt(),
                constantMappings.at(parsed_instruction.getWord(4)).getScalar()->unsignedInt(),
                constantMappings.at(parsed_instruction.getWord(5)).getScalar()->unsignedInt()};
        else
            throw CompilationError(CompilationStep::PARSER, "Invalid execution mode");
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpCapability:
        return checkCapability(static_cast<spv::Capability>(parsed_instruction.getWord(1)));
    case spv::Op::OpTypeVoid:
        typeMappings.emplace(parsed_instruction.getWord(1), TYPE_VOID);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeBool:
        typeMappings.emplace(parsed_instruction.getWord(1), TYPE_BOOL);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeInt:
        typeMappings.emplace(parsed_instruction.getWord(1),
            getIntegerType(parsed_instruction.getWord(2), parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeFloat:
        if(parsed_instruction.getWord(2) == 32)
            typeMappings.emplace(parsed_instruction.getWord(1), TYPE_FLOAT);
        else if(parsed_instruction.getWord(2) == 16)
            typeMappings.emplace(parsed_instruction.getWord(1), TYPE_HALF);
        else if(parsed_instruction.getWord(2) == 64)
            typeMappings.emplace(parsed_instruction.getWord(1), TYPE_DOUBLE);
        else
            throw CompilationError(CompilationStep::PARSER, "Unsupported floating-point type");
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeVector:
    {
        DataType type = typeMappings.at(parsed_instruction.getWord(2));
        type = type.toVectorType(static_cast<unsigned char>(parsed_instruction.getWord(3)));
        typeMappings.emplace(parsed_instruction.getWord(1), type);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpTypeImage:
    {
        auto dimensions = static_cast<uint8_t>(parsed_instruction.getWord(3) + 1);
        bool isImageArray = parsed_instruction.getWord(5);
        bool isImageBuffer = false;
        if(dimensions == 6 /* buffered */)
        {
            // there are only buffered 1D images
            dimensions = 1;
            isImageBuffer = true;
        }
        bool isSampled = false;
        auto image = module->createImageType(dimensions, isImageArray, isImageBuffer, isSampled);
        typeMappings.emplace(parsed_instruction.getResultId(), DataType(image));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading image-type '" << image->getTypeName() << "' with " << image->dimensions << " dimensions"
                << (isImageArray ? " (array)" : "") << (image->isImageBuffer ? " (buffer)" : "") << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpTypeSampler:
        typeMappings.emplace(parsed_instruction.getWord(1), TYPE_SAMPLER);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeSampledImage:
    {
        const ImageType* image = typeMappings.at(parsed_instruction.getWord(2)).getImageType();
        auto dimensions = image->dimensions;
        auto isImageArray = image->isImageArray;
        auto isImageBuffer = image->isImageBuffer;
        auto isSampled = true;
        auto sampledImage = module->createImageType(dimensions, isImageArray, isImageBuffer, isSampled);

        typeMappings.emplace(parsed_instruction.getResultId(), DataType(sampledImage));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading sampled image-type '" << image->getTypeName() << "' with " << image->dimensions
                << " dimensions" << (image->isImageArray ? " (array)" : "") << (image->isImageBuffer ? " (buffer)" : "")
                << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpTypeArray:
    {
        DataType elementType = typeMappings.at(parsed_instruction.getWord(2));
        typeMappings.emplace(parsed_instruction.getWord(1),
            module->createArrayType(elementType,
                static_cast<unsigned>(constantMappings.at(parsed_instruction.getWord(3)).getScalar()->unsignedInt())));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpTypeStruct:
    {
        auto it = names.find(parsed_instruction.getResultId());
        std::string structName;
        if(it != names.end())
            structName = it->second;
        else
        {
            logging::warn() << "Struct type " << parsed_instruction.getResultId()
                            << " has no name, generating default name!";
            structName =
                std::string("%struct.") + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
        }
        auto structDef = module->createStructType(structName, {});

        // add reference to this struct-type to type-mappings
        typeMappings.emplace(parsed_instruction.getResultId(), DataType(structDef));
        const auto typeIDs = parsed_instruction.parseArguments(2);
        for(const uint32_t typeID : typeIDs)
        {
            structDef->elementTypes.push_back(typeMappings.at(typeID));
        }
        // set "packed" decoration
        auto it2 = decorationMappings.find(parsed_instruction.getResultId());
        if(it2 != decorationMappings.end())
        {
            if(getDecoration(it2->second, spv::Decoration::CPacked))
            {
                structDef->isPacked = true;
            }
        }
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Struct " << structName << ": " << structDef->getContent() << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpTypeOpaque:
        // Since there is no memory-layout to them, they can only be used as pointers
        // TODO how to handle them better (e.g. use their name)? Own complex type for opaque types?
        typeMappings.emplace(parsed_instruction.getWord(1), TYPE_VOID);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypePointer:
    {
        DataType type = typeMappings.at(parsed_instruction.getWord(3));
        type = DataType(module->createPointerType(
            type, toAddressSpace(static_cast<spv::StorageClass>(parsed_instruction.getWord(2)))));
        typeMappings.emplace(parsed_instruction.getWord(1), type);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpTypeFunction:
        //"OpFunction is the only valid use of OpTypeFunction."
        //-> so we can ignore this
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeEvent:
        typeMappings.emplace(parsed_instruction.getWord(1), TYPE_EVENT);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpTypeDeviceEvent:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypeDeviceEvent");
    case spv::Op::OpTypeReserveId:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypeReserveID");
    case spv::Op::OpTypeQueue:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypeQueue");
    case spv::Op::OpTypePipe:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypePipe");
    case spv::Op::OpTypeForwardPointer:
        //" Declare the Storage Class for a forward reference to a pointer."
        // TODO we actually are interested, currently at least to provide it as information for clGetKernelArgInfo
        // but so far, this instruction was never encountered
        return UNSUPPORTED_INSTRUCTION("OpTypeForwardPointer");
    case spv::Op::OpConstantTrue:
        constantMappings.emplace(parsed_instruction.getResultId(), CompoundConstant(TYPE_BOOL, Literal(true)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConstantFalse:
        constantMappings.emplace(parsed_instruction.getResultId(), CompoundConstant(TYPE_BOOL, Literal(false)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConstant: // integer or floating point scalar constant
        constantMappings.emplace(parsed_instruction.getResultId(), parseConstant(parsed_instruction, typeMappings));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConstantComposite: // constant of array, matrix or vector-type
        constantMappings.emplace(parsed_instruction.getResultId(),
            parseConstantComposite(parsed_instruction, typeMappings, constantMappings));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConstantSampler: // convert to 32-bit integer constant
    {
        CompoundConstant sampler(typeMappings.at(parsed_instruction.getTypeId()),
            Literal(static_cast<uint32_t>(parseSampler(parsed_instruction))));
        constantMappings.emplace(parsed_instruction.getResultId(), sampler);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpConstantNull:
    {
        //"The null value is type dependent, defined as follows:
        // - Scalar Boolean: false
        // - Scalar integer: 0
        // - Scalar floating point: +0.0 (all bits 0)
        // - All other scalars: Abstract
        // - Composites: Members are set recursively to the null constant according to the null value of their
        // constituent types."
        auto type = typeMappings.at(parsed_instruction.getTypeId());
        if(type.isScalarType() || type.getPointerType())
            constantMappings.emplace(parsed_instruction.getResultId(), CompoundConstant(type, Literal(0u)));
        else if(type.isVectorType())
        {
            auto element = CompoundConstant(type.getElementType(), Literal(0u));
            constantMappings.emplace(parsed_instruction.getResultId(),
                CompoundConstant(type,
                    {element, element, element, element, element, element, element, element, element, element, element,
                        element, element, element, element, element}));
        }
        else if(type.getArrayType())
            // TODO correct?
            constantMappings.emplace(parsed_instruction.getResultId(), CompoundConstant(type, Literal(0u)));
        else
            throw CompilationError(CompilationStep::LLVM_2_IR, "Unhandled type for null constant", type.to_string());

        return ParseResultCode::SUCCESS;
    }
    /*
     * "Specialization enables creating a portable SPIR-V module outside the target execution environment,
     *  based on constant values that won’t be known until inside the execution environment"
     *
     * "A SPIR-V module containing specialization constants can consume one or more externally provided specializations:
     *  A set of final constant values for some subset of the module’s specialization constants.
     *  Applying these final constant values yields a new module having fewer remaining specialization constants.
     *  A module also contains default values for any specialization constants that never get externally specialized."
     *
     *  We simply ignore external specializations and always use the default value:
     *  OpSpecConstantTrue -> OpConstantTrue
     *  OpSpecConstantFalse -> OpConstantFalse
     *  OpSpecConstant -> OpConstant
     *  OpSpecConstantComposite -> OpConstantComposite
     *  OpSpecConstantOp -> result of given op
     */
    case spv::Op::OpSpecConstantTrue:
    {
        //"[...] Similarly, the "True" and "False" parts of OpSpecConstantTrue and OpSpecConstantFalse provide the
        // default Boolean specialization constants."
        auto spec = specializeConstant(parsed_instruction.getResultId(), TYPE_BOOL, decorationMappings);
        constantMappings.emplace(
            parsed_instruction.getResultId(), spec.value_or(CompoundConstant(TYPE_BOOL, Literal(true))));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpSpecConstantFalse:
    {
        //"[...] Similarly, the "True" and "False" parts of OpSpecConstantTrue and OpSpecConstantFalse provide the
        // default Boolean specialization constants."
        auto spec = specializeConstant(parsed_instruction.getResultId(), TYPE_BOOL, decorationMappings);
        constantMappings.emplace(
            parsed_instruction.getResultId(), spec.value_or(CompoundConstant(TYPE_BOOL, Literal(false))));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpSpecConstant:
    {
        //"The literal operands to OpSpecConstant are the default numerical specialization constants."
        auto constant = parseConstant(parsed_instruction, typeMappings);
        auto spec = specializeConstant(parsed_instruction.getResultId(), constant.type, decorationMappings);
        constantMappings.emplace(parsed_instruction.getResultId(), spec.value_or(constant));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpSpecConstantComposite:
    {
        auto constant = parseConstantComposite(parsed_instruction, typeMappings, constantMappings);
        auto spec = specializeConstant(parsed_instruction.getResultId(), constant.type, decorationMappings);
        constantMappings.emplace(parsed_instruction.getResultId(), spec.value_or(constant));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpSpecConstantOp:
    {
        //"The OpSpecConstantOp instruction is specialized by executing the operation and replacing the instruction with
        // the result."
        const DataType type = typeMappings.at(parsed_instruction.getTypeId());
        auto spec = specializeConstant(parsed_instruction.getResultId(), type, decorationMappings);
        if(spec)
        {
            constantMappings.emplace(parsed_instruction.getResultId(), spec.value());
            return ParseResultCode::SUCCESS;
        }
        return UNSUPPORTED_INSTRUCTION("OpSpecConstantOp");
    }
    case spv::Op::OpFunction: // new current method -> add to list of all methods
    {
        currentMethod = &getOrCreateMethod(*module, methods, parsed_instruction.getResultId());
        currentMethod->method->returnType = typeMappings.at(parsed_instruction.getTypeId());
        auto it = names.find(parsed_instruction.getResultId());
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading function: %" << parsed_instruction.getResultId() << " ("
                << (it != names.end() ? it->second : currentMethod->method->name) << ")" << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpFunctionParameter:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        currentMethod->parameters.emplace_back(parsed_instruction.getResultId(), parsed_instruction.getTypeId());
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading parameter: " << typeMappings.at(parsed_instruction.getTypeId()).to_string() << " %"
                << parsed_instruction.getResultId() << logging::endl);
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFunctionEnd:
        currentMethod = nullptr;
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFunctionCall:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getWord(3), parsed_instruction.getTypeId(), parsed_instruction.parseArguments(4)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpVariable:
    {
        //"Allocate an object in memory, resulting in a pointer to it"
        // used for global values as well as stack-allocations
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        auto it = names.find(parsed_instruction.getResultId());
        std::string name =
            it != names.end() ? it->second : (std::string("%") + std::to_string(parsed_instruction.getResultId()));
        auto type = typeMappings.at(parsed_instruction.getTypeId());
        CompoundConstant val(type.getElementType(), UNDEFINED_LITERAL);
        if(parsed_instruction.getNumWords() > 4)
            val = constantMappings.at(parsed_instruction.getWord(4));
        unsigned alignment = 0;
        bool isConstant = false;
        spv::BuiltIn builtinId = spv::BuiltIn::Max;
        auto it2 = decorationMappings.find(parsed_instruction.getTypeId());
        if(it2 != decorationMappings.end())
        {
            alignment = getDecoration(it2->second, spv::Decoration::Alignment).value_or(0);
            isConstant = getDecoration(it2->second, spv::Decoration::Constant).has_value();
        }
        it2 = decorationMappings.find(parsed_instruction.getResultId());
        if(it2 != decorationMappings.end())
        {
            if(alignment == 0)
                alignment = getDecoration(it2->second, spv::Decoration::Alignment).value_or(0);
            builtinId = static_cast<spv::BuiltIn>(getDecoration(it2->second, spv::Decoration::BuiltIn)
                                                      .value_or(static_cast<uint32_t>(spv::BuiltIn::Max)));
        }

        // the type of OpVariable is the pointer
        //... but the global data/stack allocation needs to have the real type (is re-set in #parse())
        if(currentMethod != nullptr &&
            AddressSpace::PRIVATE == toAddressSpace(static_cast<spv::StorageClass>(parsed_instruction.getWord(3))))
        {
            // OpVariables within a function body are stack allocations
            //"All OpVariable instructions in a function must have a Storage Class of Function."
            name = name.find('%') == 0 ? name : ("%" + name);
            auto pos = currentMethod->method->stackAllocations.emplace(StackAllocation(name, type, 0, alignment));
            // TODO set initial value!. Allowed for OpVariables with storage-class Function?
            memoryAllocatedData.emplace(parsed_instruction.getResultId(), &(*pos.first));
        }
        else if(builtinId != spv::BuiltIn::Max)
        {
            // This is a built-in, do not generate a global variable, but reference to the UNIFORM/convert to vector3.
            /*
             * From SPIR-V view, they are constants (OpVariable in UniformConstant address space) which are read as such
             * (e.g. via OpLoad instruction). Since in OpenCL C source code, only a single dimension can be accessed at
             * any time, the loaded value will be converted to one of the elements (assuming no optimizations occur
             * here) (e.g. via OpCompositeExtract instruction).
             */
            if(auto builtin = mapToBuiltinLocal(builtinId))
                memoryAllocatedData.emplace(parsed_instruction.getResultId(), builtin);
            else
            {
                logging::error() << "Met unsupported builtin " << static_cast<uint32_t>(builtinId) << logging::endl;
                return ParseResultCode::UNSUPPORTED;
            }
        }
        else
        {
            // OpVariables outside of any function are global data
            isConstant = isConstant ||
                static_cast<spv::StorageClass>(parsed_instruction.getWord(3)) == spv::StorageClass::UniformConstant;
            // replace the '%' and add the leading '@' to match the LLVM front-end
            name = "@" + (name.find('%') == 0 ? name.substr(1) : name);
            module->globalData.emplace_back(name, type, CompoundConstant(val), isConstant);
            const_cast<unsigned&>(module->globalData.back().type.getPointerType()->alignment) = alignment;
            memoryAllocatedData.emplace(parsed_instruction.getResultId(), &module->globalData.back());
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading variable: " << type.to_string() << " " << name << " with value: " << val.to_string(true)
                << logging::endl);
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpImageTexelPointer:
        //"Form a pointer to a texel of an image. Use of such a pointer is limited to atomic operations."
        return UNSUPPORTED_INSTRUCTION("OpImageTexelPointer");
    case spv::Op::OpLoad:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCopy>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), MemoryAccess::READ));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpStore:
        instructions.emplace_back(std::make_unique<SPIRVCopy>(parsed_instruction.getWord(1), *currentMethod,
            UNDEFINED_ID, parsed_instruction.getWord(2), MemoryAccess::WRITE));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpCopyMemory:
        instructions.emplace_back(std::make_unique<SPIRVCopy>(parsed_instruction.getWord(1), *currentMethod,
            UNDEFINED_ID, parsed_instruction.getWord(2), MemoryAccess::READ_WRITE));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpCopyMemorySized:
        instructions.emplace_back(std::make_unique<SPIRVCopy>(parsed_instruction.getWord(1), *currentMethod,
            UNDEFINED_ID, parsed_instruction.getWord(2), MemoryAccess::READ_WRITE, parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAccessChain: // pointer into element(s) of composite
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVIndexOf>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), parsed_instruction.parseArguments(4),
            false));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpInBoundsAccessChain:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVIndexOf>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), parsed_instruction.parseArguments(4),
            false));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpPtrAccessChain:
        // For pointers, the "Element" field is the first (top-level) index (see SPIR-V specification,
        // OpPtrAccessChain): "Element is used to do the initial dereference of Base: Base is treated as the address of
        // the first element of an array, and the Element element’s address is computed to be the base for the Indexes
        //[...]"
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVIndexOf>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), parsed_instruction.parseArguments(4), true));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpGenericPtrMemSemantics:
        //"Result is a valid Memory Semantics which includes mask bits set for the Storage Class for the specific
        //(non-Generic) Storage Class of Pointer. "
        // not used -> ignore
        return ParseResultCode::SUCCESS;
    case spv::Op::OpInBoundsPtrAccessChain:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVIndexOf>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), parsed_instruction.parseArguments(4), true));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpNoLine: // source level debug info -> skip
        return ParseResultCode::SUCCESS;
    case spv::Op::OpModuleProcessed:
        //"Document a process that was applied to a module. This has no semantic impact and can safely be removed from a
        // module."
        return ParseResultCode::SUCCESS;
    case spv::Op::OpDecorate:
        //"Target is the <id> to decorate. It can potentially be any <id> that is a forward reference"
        // -> decorations are always forward references
        // -> can be applied to instruction/parameter/type on parsing it
        return parseDecoration(parsed_instruction,
            parsed_instruction.getNumWords() > 3 ? parsed_instruction.getWord(3) : UNDEFINED_SCALAR);
    case spv::Op::OpDecorateId:
        //"Target is the <id> to decorate. It can potentially be any <id> that is a forward reference"
        // -> decorations are always forward references
        // -> can be applied to instruction/parameter/type on parsing it
        // In this version, the decoration operands are not literals, but specified by their IDs
        return parseDecoration(parsed_instruction,
            parsed_instruction.getNumWords() > 3 ?
                constantMappings.at(parsed_instruction.getWord(3)).getScalar()->unsignedInt() :
                UNDEFINED_SCALAR);
    case spv::Op::OpMemberDecorate:
        return UNSUPPORTED_INSTRUCTION("OpMemberDecorate");
    case spv::Op::OpDecorationGroup:
        //"A collector for Decorations from OpDecorate instructions. All such OpDecorate instructions targeting this
        // OpDecorationGroup instruction must precede it." "Subsequent OpGroupDecorate and OpGroupMemberDecorate
        // instructions that consume this instruction�s Result <id> will apply these decorations to their targets."
        // nothing needs to be done, since decorations are added up independent of target and are applied with
        // "OpGroupDecorate"
        return ParseResultCode::SUCCESS;
    case spv::Op::OpGroupDecorate:
    {
        // apply group of decorations to IDs
        const std::vector<uint32_t> targets = parsed_instruction.parseArguments(2);
        const uint32_t decorationGroup = parsed_instruction.getWord(1);
        auto it = decorationMappings.find(decorationGroup);
        if(it != decorationMappings.end())
        {
            const std::vector<Decoration>& decorations = it->second;
            for(const uint32_t target : targets)
            {
                for(const Decoration& deco : decorations)
                {
                    decorationMappings[target].push_back(deco);
                }
            }
        }
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpGroupMemberDecorate:
        return UNSUPPORTED_INSTRUCTION("OpGroupMemberDecorate");
    case spv::Op::OpVectorExtractDynamic:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInsertionExtraction>(parsed_instruction.getResultId(),
            *currentMethod, parsed_instruction.getTypeId(), parsed_instruction.getWord(3),
            parsed_instruction.parseArguments(4), false));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpVectorInsertDynamic:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInsertionExtraction>(parsed_instruction.getResultId(),
            *currentMethod, parsed_instruction.getTypeId(), parsed_instruction.getWord(3),
            parsed_instruction.getWord(4), parsed_instruction.parseArguments(5), false));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpVectorShuffle:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVShuffle>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), parsed_instruction.getWord(4),
            parsed_instruction.parseArguments(5)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpCompositeConstruct:
        return UNSUPPORTED_INSTRUCTION("OpCompositeConstruct");
    case spv::Op::OpCompositeExtract:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInsertionExtraction>(parsed_instruction.getResultId(),
            *currentMethod, parsed_instruction.getTypeId(), parsed_instruction.getWord(3),
            parsed_instruction.parseArguments(4), true));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpCompositeInsert:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInsertionExtraction>(parsed_instruction.getResultId(),
            *currentMethod, parsed_instruction.getTypeId(), parsed_instruction.getWord(4),
            parsed_instruction.getWord(3), parsed_instruction.parseArguments(5), true));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpCopyObject:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCopy>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSampledImage:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        // this is not really an instruction for the runtime, but to associate images <-> sampled-image
        sampledImages[parsed_instruction.getResultId()] = {
            parsed_instruction.getWord(3), parsed_instruction.getWord(4)};
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageSampleExplicitLod:
    {
        //"Sample an image using an explicit level of detail."
        // is handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpImageSampleExplicitLod");
    }
    case spv::Op::OpImageFetch:
    {
        //"Fetch a single texel from a sampled image."
        // is handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpImageFetch");
    }
    case spv::Op::OpImageRead:
    {
        //"Read a texel from an image without a sampler."
        // is handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpImageRead");
    }
    case spv::Op::OpImageWrite:
    {
        //"Write a texel to an image without a sampler."
        // is handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpImageWrite");
    }
    case spv::Op::OpImage:
        //"Extract the image from a sampled image."
        // this is not really an instruction for the runtime, but to associate images <-> sampled-image
        localTypes[parsed_instruction.getResultId()] = localTypes.at(parsed_instruction.getWord(3));
        // this is not quite correct (to store a sampled-image where an image is supposed to go), but we extract the
        // image in the image-accessing methods
        sampledImages[parsed_instruction.getResultId()] = sampledImages.at(parsed_instruction.getWord(3));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageQueryFormat:
        //"Query the image format of an image [...]."
        //"The resulting value is an enumerant from Image Channel Data Type."
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVImageQuery>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), ImageQuery::CHANNEL_DATA_TYPE, parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageQueryOrder:
        //"Query the channel order of an image [...]."
        //"The resulting value is an enumerant from Image Channel Order."
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVImageQuery>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), ImageQuery::CHANNEL_ORDER, parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageQuerySizeLod:
        //"Query the dimensions of Image for mipmap level for Level of Detail."
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVImageQuery>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), ImageQuery::SIZES_LOD, parsed_instruction.getWord(3),
            parsed_instruction.getWord(4)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageQuerySize:
        //"Query the dimensions of Image, with no level of detail."
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVImageQuery>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), ImageQuery::SIZES, parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageQueryLevels:
        //"Query the number of mipmap levels accessible through Image."
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVImageQuery>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), ImageQuery::MIPMAP_LEVELS, parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpImageQuerySamples:
        //"Query the number of samples available per texel fetch in a multisample image."
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVImageQuery>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), ImageQuery::SAMPLES_PER_TEXEL, parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConvertFToU:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fptoui", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConvertFToS:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fptosi", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConvertSToF:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "sitofp", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConvertUToF:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "uitofp", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUConvert: // change bit-width (type) of value
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::UNSIGNED_TO_UNSIGNED,
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSConvert: // change bit-width (type) of value
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::SIGNED_TO_SIGNED,
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFConvert: // change bit-width (type) of value
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::FLOATING,
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConvertPtrToU: // pointer to unsigned -> same as OpUConvert
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::UNSIGNED_TO_UNSIGNED,
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSatConvertSToU: // signed to unsigned (with saturation)
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::SIGNED_TO_UNSIGNED,
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                add_flag(intermediate::InstructionDecorations::UNSIGNED_RESULT,
                    intermediate::InstructionDecorations::SATURATED_CONVERSION))));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSatConvertUToS: // unsigned to signed (with saturation)
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::UNSIGNED_TO_SIGNED,
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::SATURATED_CONVERSION)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpConvertUToPtr: // unsigned to pointer -> same as OpUConvert
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::UNSIGNED_TO_UNSIGNED,
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpPtrCastToGeneric:
        //"Convert a pointer’s Storage Class to Generic."
        // -> simply copy the pointer
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::BITCAST,
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpGenericCastToPtr:
        //"Convert a pointer’s Storage Class to a non-Generic class."
        // -> simple copy the pointer
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::BITCAST,
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpGenericCastToPtrExplicit:
        //"Attempts to explicitly convert Pointer to Storage storage-class pointer value."
        // -> simple copy the pointer
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::BITCAST,
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBitcast:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVConversion>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), ConversionType::BITCAST,
            intermediate::InstructionDecorations::NONE));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSNegate:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            OP_NEGATE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFNegate:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            OP_NEGATE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIAdd:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "add", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFAdd:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fadd", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpISub:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "sub", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFSub:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fsub", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIMul:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "mul", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFMul:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fmul", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUDiv:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "udiv", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSDiv:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "sdiv", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFDiv:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fdiv", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUMod:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "umod", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSRem:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "srem", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSMod:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "smod", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFRem:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "frem", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFMod:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fmod", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpVectorTimesScalar: // type must be floating point
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "fmul", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpDot:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "dot", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIAddCarry:
        return UNSUPPORTED_INSTRUCTION("OpIAddCarry");
    case spv::Op::OpISubBorrow:
        return UNSUPPORTED_INSTRUCTION("OpISubBorrow");
    case spv::Op::OpUMulExtended:
        return UNSUPPORTED_INSTRUCTION("OpUMulExtended");
    case spv::Op::OpSMulExtended:
        return UNSUPPORTED_INSTRUCTION("OpSMulExtended");
    case spv::Op::OpAny:
        // This is NOT the OpenCL any(...), it does NOT check the MSB, instead it takes a vector of boolean values and
        // checks whether any of them is true!
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVFoldInstruction>(parsed_instruction.getResultId(),
            *currentMethod, parsed_instruction.getTypeId(), "or", parsed_instruction.getWord(3),
            intermediate::InstructionDecorations::UNSIGNED_RESULT));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAll:
        // This is NOT the OpenCL all(...), it does NOT check the MSB, instead it takes a vector of boolean values and
        // checks whether all of them are true!
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVFoldInstruction>(parsed_instruction.getResultId(),
            *currentMethod, parsed_instruction.getTypeId(), "and", parsed_instruction.getWord(3),
            intermediate::InstructionDecorations::UNSIGNED_RESULT));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIsNan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVBoolCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "vc4cl_is_nan", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIsInf:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVBoolCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "isinf", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIsFinite:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVBoolCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "isfinite", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIsNormal:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVBoolCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "isnormal", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSignBitSet:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementations are reverted back to the SPIR-V
        // opcodes, create a function call to the VC4CL function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVBoolCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "signbit", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLessOrGreater:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVBoolCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "islessgreater", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpOrdered:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUnordered:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLogicalEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_EQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLogicalNotEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_NEQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLogicalOr:
        //"Result Type must be a scalar or vector of Boolean type."
        // -> same as bitwise OR
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "or", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLogicalAnd:
        //"Result Type must be a scalar or vector of Boolean type."
        // -> same as bitwise AND
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "and", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLogicalNot:
        //"Result Type must be a scalar or vector of Boolean type."
        // -> same as bitwise NOT
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "not", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSelect:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVSelect>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getTypeId(), parsed_instruction.getWord(3), parsed_instruction.getWord(4),
            parsed_instruction.getWord(5)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpIEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_EQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpINotEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_NEQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUGreaterThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNSIGNED_GT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSGreaterThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_SIGNED_GT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUGreaterThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNSIGNED_GE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSGreaterThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_SIGNED_GE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpULessThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNSIGNED_LT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSLessThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_SIGNED_LT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpULessThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNSIGNED_LE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            add_flag(toInstructionDecorations(parsed_instruction.getResultId()),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSLessThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_SIGNED_LE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFOrdEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED_EQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFUnordEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED_EQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFOrdNotEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED_NEQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFUnordNotEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED_NEQ, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFOrdLessThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED_LT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFUnordLessThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED_LT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFOrdGreaterThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED_GT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFUnordGreaterThan:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED_GT, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFOrdLessThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED_LE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFUnordLessThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED_LE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFOrdGreaterThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_ORDERED_GE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpFUnordGreaterThanEqual:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVComparison>(parsed_instruction.getResultId(), *currentMethod,
            intermediate::COMP_UNORDERED_GE, parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpShiftRightLogical:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "shr", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpShiftRightArithmetic:
        // Instead of directly mapping to "asr" operation, we let the intrinsics lowering handle the shift to apply
        // sign-extension for non-32-bit types.
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "ashr", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpShiftLeftLogical:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "shl", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBitwiseOr:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "or", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBitwiseXor:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "xor", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBitwiseAnd:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "and", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpNot:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(parsed_instruction.getResultId(), *currentMethod,
            "not", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3),
            toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBitCount:
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "vc4cl_popcount", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpControlBarrier:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation of "barrier()" is reverted back
        // to this OpControlBarrier, create a call to the intrinsified version of the "barrier()" function
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(
            *currentMethod, "vc4cl_barrier", std::vector<uint32_t>{parsed_instruction.getWord(1)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpMemoryBarrier:
        instructions.emplace_back(std::make_unique<SPIRVMemoryBarrier>(
            *currentMethod, parsed_instruction.getWord(1), parsed_instruction.getWord(2)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicLoad:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicLoad");
    case spv::Op::OpAtomicStore:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicStore");
    case spv::Op::OpAtomicExchange:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_xchg", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicCompareExchangeWeak:
        // OpenCL 2.x feature
        // since SPIR-V 1.3 deprecated in favor of OpAtomicCompareExchange (identical behavior)
    case spv::Op::OpAtomicCompareExchange:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_cmpxchg", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{
                parsed_instruction.getWord(3), parsed_instruction.getWord(8), parsed_instruction.getWord(7)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicIIncrement:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_inc", parsed_instruction.getTypeId(), std::vector<uint32_t>{parsed_instruction.getWord(3)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicIDecrement:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_dec", parsed_instruction.getTypeId(), std::vector<uint32_t>{parsed_instruction.getWord(3)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicIAdd:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_add", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicISub:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_sub", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicSMin:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_min", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicUMin:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_min", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicSMax:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_max", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicUMax:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_max", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicAnd:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_and", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicOr:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_or", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicXor:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "atomic_xor", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(3), parsed_instruction.getWord(6)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpPhi:
    {
        const std::vector<uint32_t> args = parsed_instruction.parseArguments(3);
        std::vector<std::pair<uint32_t, uint32_t>> sources;
        sources.reserve(args.size() / 2);
        for(std::size_t i = 0; i < args.size(); i += 2)
        {
            sources.emplace_back(args[i], args[i + 1]);
        }
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVPhi>(
            parsed_instruction.getResultId(), *currentMethod, parsed_instruction.getTypeId(), std::move(sources)));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpLoopMerge:
        // XXX As far as I can see it this instruction contains information useful for loop detection and optimization,
        // but has no semantic meaning -> so we can ignore this for now
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSelectionMerge:
        return UNSUPPORTED_INSTRUCTION("OpSelectionMerge");
    case spv::Op::OpLabel:
        typeMappings.emplace(parsed_instruction.getResultId(), TYPE_LABEL);
        instructions.emplace_back(std::make_unique<SPIRVLabel>(parsed_instruction.getResultId(), *currentMethod));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBranch:
        instructions.emplace_back(std::make_unique<SPIRVBranch>(*currentMethod, parsed_instruction.getWord(1)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpBranchConditional:
        instructions.emplace_back(std::make_unique<SPIRVBranch>(*currentMethod, parsed_instruction.getWord(1),
            parsed_instruction.getWord(2), parsed_instruction.getWord(3)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpSwitch:
    {
        std::vector<uint32_t> args = parsed_instruction.parseArguments(3);
        instructions.emplace_back(std::make_unique<SPIRVSwitch>(parsed_instruction.getResultId(), *currentMethod,
            parsed_instruction.getWord(1), parsed_instruction.getWord(2), std::move(args)));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpReturn:
        instructions.emplace_back(std::make_unique<SPIRVReturn>(*currentMethod));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpReturnValue:
        instructions.emplace_back(std::make_unique<SPIRVReturn>(parsed_instruction.getWord(1), *currentMethod));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpUnreachable:
        return ParseResultCode::SUCCESS;
    case spv::Op::OpLifetimeStart:
    {
        // for temporary variables (e.g. Function-Scope), the size is set via the OpLifetimeStart, since the OpVariable
        // is of type void
        instructions.emplace_back(
            std::make_unique<SPIRVLifetimeInstruction>(parsed_instruction.getWord(1), *currentMethod,
                parsed_instruction.getWord(2), false, toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    }
    case spv::Op::OpLifetimeStop:
        instructions.emplace_back(
            std::make_unique<SPIRVLifetimeInstruction>(parsed_instruction.getWord(1), *currentMethod,
                parsed_instruction.getWord(2), true, toInstructionDecorations(parsed_instruction.getResultId())));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpGroupAsyncCopy:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation of "async_work_group_copy()" is
        // reverted back to this OpGroupAsyncCopy, create a function call to the "async_work_group_copy()" function
        // TODO support for strided version (word 7), map to async_work_group_strided_copy
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "async_work_group_copy", parsed_instruction.getTypeId(),
            std::vector<uint32_t>{parsed_instruction.getWord(4), parsed_instruction.getWord(5),
                parsed_instruction.getWord(6), parsed_instruction.getWord(8)}));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpGroupWaitEvents:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation of "wait_group_events()" is
        // reverted back to this OpGroupWaitEvents, create a function call to the "wait_group_events()" function
        localTypes[parsed_instruction.getResultId()] = parsed_instruction.getTypeId();
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(parsed_instruction.getResultId(), *currentMethod,
            "wait_group_events", parsed_instruction.getTypeId(), parsed_instruction.parseArguments(2)));
        return ParseResultCode::SUCCESS;
    case spv::Op::OpAtomicFlagTestAndSet:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicFlagTestAndSet");
    case spv::Op::OpAtomicFlagClear:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicFlagClear");
    case spv::Op::OpSizeOf:
        return UNSUPPORTED_INSTRUCTION("OpSizeOf");
    default:
        // prevents warnings
        break;
    }

    // unhandled op-code
    logging::warn() << "Unhandled instruction-type: " << static_cast<unsigned>(parsed_instruction.getOpcode())
                    << logging::endl;
    return ParseResultCode::UNSUPPORTED;
}

ParseResultCode SPIRVParserBase::consumeOpenCLInstruction(const ParsedInstruction& instruction)
{
    localTypes[instruction.getResultId()] = instruction.getTypeId();
    if(instruction.getWord(4) == OpenCLLIB::Entrypoints::Shuffle2)
    {
        instructions.emplace_back(std::make_unique<SPIRVShuffle>(instruction.getResultId(), *currentMethod,
            instruction.getTypeId(), instruction.getWord(5), instruction.getWord(6), instruction.getWord(7)));
        return ParseResultCode::SUCCESS;
    }
    if(instruction.getWord(4) == OpenCLLIB::Entrypoints::Shuffle)
    {
        instructions.emplace_back(std::make_unique<SPIRVShuffle>(instruction.getResultId(), *currentMethod,
            instruction.getTypeId(), instruction.getWord(5), UNDEFINED_ID, instruction.getWord(6)));
        return ParseResultCode::SUCCESS;
    }
    auto resultTypeIt = typeMappings.find(instruction.getTypeId());
    if((instruction.getWord(4) == OpenCLLIB::Entrypoints::Fmax ||
           instruction.getWord(4) == OpenCLLIB::Entrypoints::FMax_common) &&
        resultTypeIt != typeMappings.end() && resultTypeIt->second.getScalarBitCount() == 32)
    {
        // map directly to opcode to avoid fmax()/max() function naming issues
        // FIXME this is wrong, since fmax/fmin opcodes do not handle NaN correctly
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(instruction.getResultId(), *currentMethod, "fmax",
            instruction.getTypeId(), instruction.parseArguments(5),
            toInstructionDecorations(instruction.getResultId())));
    }
    else if((instruction.getWord(4) == OpenCLLIB::Entrypoints::Fmin ||
                instruction.getWord(4) == OpenCLLIB::Entrypoints::FMin_common) &&
        resultTypeIt != typeMappings.end() && resultTypeIt->second.getScalarBitCount() == 32)
    {
        // map directly to opcode to avoid fmin()/min() function naming issues
        instructions.emplace_back(std::make_unique<SPIRVInstruction>(instruction.getResultId(), *currentMethod, "fmin",
            instruction.getTypeId(), instruction.parseArguments(5),
            toInstructionDecorations(instruction.getResultId())));
    }
    else
    {
        // the OpenCL built-in operations are not supported directly, but there might be a function definition for them
        // here, we simply map them to function calls and resolve the possible matching definitions later
        instructions.emplace_back(std::make_unique<SPIRVCallSite>(instruction.getResultId(), *currentMethod,
            getOpenCLMethodName(instruction.getWord(4)), instruction.getTypeId(), instruction.parseArguments(5)));
    }
    return ParseResultCode::SUCCESS;
}

ParseResultCode SPIRVParserBase::consumeDebugInfoInstruction(const ParsedInstruction& instruction)
{
    // These instructions are guaranteed to be non-semantic, so we can just ignore them
    return ParseResultCode::SUCCESS;
}

ParseResultCode SPIRVParserBase::consumeOpenCLDebugInfoInstruction(const ParsedInstruction& instruction)
{
    // These instructions are guaranteed to be non-semantic, so we can just ignore them
    return ParseResultCode::SUCCESS;
}

ParseResultCode SPIRVParserBase::consumeNonSemanticInstruction(const ParsedInstruction& instruction)
{
    // These instructions are guaranteed to be non-semantic, so we can just ignore them
    return ParseResultCode::SUCCESS;
}
