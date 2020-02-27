/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVParser.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../intrinsics/Images.h"
#include "SPIRVBuiltins.h"
#include "SPIRVHelper.h"
#include "log.h"

#include <chrono>
#include <cstdint>
#include <cstring>

// out-of-line destructor
vc4c::spirv::SPIRVParser::~SPIRVParser() = default;

#ifdef SPIRV_FRONTEND

using namespace vc4c;
using namespace vc4c::spirv;

SPIRVParser::SPIRVParser(std::istream& input, const bool isSPIRVText) :
    isTextInput(isSPIRVText), input(input), currentMethod(nullptr), module(nullptr)
{
}

static spv_result_t parsedHeaderCallback(void* user_data, spv_endianness_t endian, uint32_t magic, uint32_t version,
    uint32_t generator, uint32_t id_bound, uint32_t reserved)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "SPIR-V header parsed: magic-number 0x" << std::hex << magic << ", version 0x" << version
            << ", generator " << generator << ", max-ID " << std::dec << id_bound << logging::endl);
    SPIRVParser* parser = static_cast<SPIRVParser*>(user_data);
    return parser->parseHeader(endian, magic, version, generator, id_bound, reserved);
}

static spv_result_t parsedInstructionCallback(void* user_data, const spv_parsed_instruction_t* parsed_instruction)
{
    SPIRVParser* parser = static_cast<SPIRVParser*>(user_data);
    return parser->parseInstruction(parsed_instruction);
}

static std::string getErrorPosition(spv_diagnostic diagnostics)
{
    if(diagnostics == nullptr)
        return "?";
    return std::to_string(diagnostics->position.line).append(":") + std::to_string(diagnostics->position.column);
}

// to relay names of unsupported operations
static thread_local std::string errorExtra;

void SPIRVParser::parse(Module& module)
{
    this->module = &module;
    spv_diagnostic diagnostics = nullptr;
    spv_context context = spvContextCreate(SPV_ENV_OPENCL_EMBEDDED_1_2);
    if(context == nullptr)
    {
        throw CompilationError(CompilationStep::PARSER, "Failed to create SPIR-V context");
    }

    // read input and map into buffer
    std::vector<uint32_t> words = readStreamOfWords(&input);

    // if input is SPIR-V text, convert to binary representation
    spv_result_t result;
    if(isTextInput)
    {
        spvtools::SpirvTools tools(SPV_ENV_OPENCL_EMBEDDED_1_2);
        tools.SetMessageConsumer(consumeSPIRVMessage);
        std::vector<uint32_t> binaryData;
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Read SPIR-V text with " << words.size() * sizeof(uint32_t) << " characters" << logging::endl);
        if(tools.Assemble(reinterpret_cast<char*>(words.data()), words.size() * sizeof(uint32_t), &binaryData))
            words.swap(binaryData);
    }
    else
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Read SPIR-V binary with " << words.size() << " words" << logging::endl);
    }

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Starting parsing..." << logging::endl);

    // parse input
    result = spvBinaryParse(
        context, this, words.data(), words.size(), parsedHeaderCallback, parsedInstructionCallback, &diagnostics);

    if(result != SPV_SUCCESS)
    {
        logging::error() << getErrorMessage(result) << ": "
                         << (diagnostics != nullptr ? diagnostics->error : errorExtra) << " at "
                         << getErrorPosition(diagnostics) << logging::endl;
        spvContextDestroy(context);
        throw CompilationError(CompilationStep::PARSER, getErrorMessage(result),
            (diagnostics != nullptr ? diagnostics->error : errorExtra));
    }
    CPPLOG_LAZY(logging::Level::DEBUG, log << "SPIR-V binary successfully parsed" << logging::endl);
    spvContextDestroy(context);

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
                method.metaData.workGroupSizes = meta.second;
                break;
            case MetaDataType::WORK_GROUP_SIZES_HINT:
                method.metaData.workGroupSizeHints = meta.second;
                break;
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
        module.methods.emplace_back(method.second.method.release());
    }

    addFunctionAliases(module);
}

spv_result_t SPIRVParser::parseHeader(
    spv_endianness_t endian, uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound, uint32_t reserved)
{
    // see:
    // https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#_a_id_physicallayout_a_physical_layout_of_a_spir_v_module_and_instruction
    // not completely true, since the header is not mapped to instructions, but still better than increasing every X new
    // instruction
    instructions.reserve(id_bound);

    return SPV_SUCCESS;
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

intermediate::InstructionDecorations SPIRVParser::toInstructionDecorations(const uint32_t id)
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

static uint32_t getWord(const spv_parsed_instruction_t* instruction, std::size_t index)
{
    if(instruction->num_words <= index)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(index));
    return instruction->words[index];
}

static std::string readLiteralString(const spv_parsed_instruction_t* instruction, const spv_parsed_operand_t* operand)
{
    if(instruction->num_words <= operand->offset)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(operand->offset));
    const size_t length = strnlen(
        reinterpret_cast<const char*>(instruction->words + operand->offset), sizeof(uint32_t) * operand->num_words);
    return std::string(reinterpret_cast<const char*>(instruction->words + operand->offset), length);
}

spv_result_t SPIRVParser::parseDecoration(const spv_parsed_instruction_t* parsed_instruction, uint32_t value)
{
    const uint32_t target = getWord(parsed_instruction, 1);
    switch(static_cast<spv::Decoration>(getWord(parsed_instruction, 2)))
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
        decorationMappings[target].push_back({static_cast<spv::Decoration>(getWord(parsed_instruction, 2)), value});
        return SPV_SUCCESS;
    case spv::Decoration::AlignmentId: // known alignment of pointer, but as constant, not literal value
        decorationMappings[target].push_back({spv::Decoration::Alignment, value});
        return SPV_SUCCESS;
    case spv::Decoration::MaxByteOffsetId: // known maximum offset (in bytes) from base address, this pointer is
                                           // accessed at, but as constant, not literal value
        decorationMappings[target].push_back({spv::Decoration::MaxByteOffset, value});
        return SPV_SUCCESS;
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
            decorationMappings[target].push_back({spv::Decoration::BuiltIn, value});
            return SPV_SUCCESS;
        default:
            logging::error() << "Met unsupported builtin decoration " << value << logging::endl;
            return SPV_UNSUPPORTED;
        }
    case spv::Decoration::Aliased: // pointer needs to be accessed with aliased access in mind. XXX how/is it handled?
    case spv::Decoration::FPRoundingMode: // explicit rounding mode
        // handle unsupported decorations which can't be simply ignored
        logging::error() << "Met unsupported instruction-decoration " << getWord(parsed_instruction, 2)
                         << logging::endl;
        return SPV_UNSUPPORTED;
    default:
        // simply ignore all other decorations
        return SPV_SUCCESS;
    }
}

static std::vector<uint32_t> parseArguments(const spv_parsed_instruction_t* instruction, const uint32_t startWord)
{
    std::vector<uint32_t> args;
    if(instruction->num_words <= startWord)
        return args;
    args.reserve(instruction->num_words - startWord);
    for(std::size_t i = startWord; i < instruction->num_words; ++i)
        args.push_back(instruction->words[i]);
    return args;
}

static intermediate::Sampler parseSampler(const spv_parsed_instruction_t* instruction)
{
    intermediate::Sampler tmp(0);
    switch(static_cast<spv::SamplerAddressingMode>(getWord(instruction, 3)))
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
            CompilationStep::PARSER, "Unknown sampler addressing mode", std::to_string(getWord(instruction, 3)));
    }

    tmp.setNormalizedCoordinates(getWord(instruction, 4));

    switch(static_cast<spv::SamplerFilterMode>(getWord(instruction, 5)))
    {
    case spv::SamplerFilterMode::Nearest:
        tmp.setFilterMode(intermediate::FilterMode::NEAREST);
        break;
    case spv::SamplerFilterMode::Linear:
        tmp.setFilterMode(intermediate::FilterMode::LINEAR);
        break;
    default:
        throw CompilationError(
            CompilationStep::PARSER, "Unknown sampler filter mode", std::to_string(getWord(instruction, 5)));
    }

    return tmp;
}

static CompoundConstant parseConstant(const spv_parsed_instruction_t* instruction, const TypeMapping& typeMappings)
{
    CompoundConstant constant(typeMappings.at(instruction->type_id), UNDEFINED_LITERAL);
    if(instruction->num_words > 3)
    {
        //"Types 32 bits wide or smaller take one word."
        uint64_t val = getWord(instruction, 3);
        if(instruction->num_words > 4)
            //"[...] Larger types take multiple words, with low-order words appearing first."
            // e.g. for long/double constants
            val |= static_cast<uint64_t>(getWord(instruction, 4)) << 32;
        auto lit = toLongLiteral(val);
        if(!lit)
            throw CompilationError(
                CompilationStep::PARSER, "Constant value is out of valid range", std::to_string(val));
        constant = CompoundConstant(typeMappings.at(instruction->type_id), *lit);

        if(constant.type.isFloatingType())
            // set correct type, just for cosmetic purposes
            constant = CompoundConstant(
                typeMappings.at(instruction->type_id), Literal(bit_cast<uint32_t, float>(static_cast<uint32_t>(val))));
    }
    return constant;
}

static CompoundConstant parseConstantComposite(const spv_parsed_instruction_t* instruction,
    const TypeMapping& typeMappings, const ConstantMapping& constantMappings)
{
    DataType containerType = typeMappings.at(getWord(instruction, 1));
    std::vector<CompoundConstant> constants;
    for(std::size_t i = 3; i < instruction->num_words; ++i)
    {
        //"Result Type must be a composite type, whose top-level members/elements/components/columns have the same type
        // as the types of the Constituents."
        // -> no heterogeneous composite possible
        auto element = constantMappings.at(instruction->words[i]);
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

static std::string toScalarType(uint16_t vectorType)
{
    switch(vectorType)
    {
    case 0:
        return "i8";
    case 1:
        return "i16";
    case 2:
        return "i32";
    case 3:
        return "i64";
    case 4:
        return "half";
    case 5:
        return "float";
    case 6:
        return "double";
    default:
        throw CompilationError(CompilationStep::PARSER, "Unsupported vector-type", std::to_string(vectorType));
    }
}

#define UNSUPPORTED_INSTRUCTION(name)                                                                                  \
    (errorExtra = (name), logging::error() << "Unsupported SPIR-V instruction: " << (name) << logging::endl,           \
        SPV_UNSUPPORTED)

spv_result_t SPIRVParser::parseInstruction(const spv_parsed_instruction_t* parsed_instruction)
{
    if(parsed_instruction == nullptr)
        return SPV_ERROR_INTERNAL;

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
    switch(static_cast<spv::Op>(parsed_instruction->opcode))
    {
    case spv::Op::OpNop:
        return SPV_SUCCESS;
    case spv::Op::OpUndef:
        constantMappings.emplace(parsed_instruction->result_id,
            CompoundConstant(typeMappings.at(parsed_instruction->type_id), UNDEFINED_LITERAL));
        return SPV_SUCCESS;
    case spv::Op::OpSourceContinued:
        return SPV_SUCCESS;
    case spv::Op::OpSource:
        return SPV_SUCCESS;
    case spv::Op::OpSourceExtension:
        break;
    case spv::Op::OpName: // e.g. method-name, parameters
        names[getWord(parsed_instruction, 1)] = readLiteralString(parsed_instruction, &parsed_instruction->operands[1]);
        return SPV_SUCCESS;
    case spv::Op::OpMemberName: // name of struct-member
        return SPV_SUCCESS;
    case spv::Op::OpString: // e.g. kernel original parameter type names
        strings.emplace_back(readLiteralString(parsed_instruction, &parsed_instruction->operands[1]));
        return SPV_SUCCESS;
    case spv::Op::OpLine: // source level debug info
        return SPV_SUCCESS;
    case spv::Op::OpExtension:
    {
        const std::string extension = readLiteralString(parsed_instruction, &parsed_instruction->operands[0]);
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Using extension: " << extension << logging::endl);
        auto ret = checkExtension(extension);
        if(ret != SPV_SUCCESS)
            throw CompilationError(CompilationStep::PARSER, "Use of unsupported SPIR-V extension", extension);
        return ret;
    }
    case spv::Op::OpExtInstImport: // adds a new set of instructions
    {
        const std::string instructionSet(readLiteralString(parsed_instruction, &parsed_instruction->operands[1]));
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Importing extended instruction set: " << instructionSet << logging::endl);
        if(instructionSet == "OpenCL.std")
            // https://www.khronos.org/registry/spir-v/specs/unified1/OpenCL.ExtendedInstructionSet.100.html
            extensionConsumers.emplace(parsed_instruction->result_id, &SPIRVParser::consumeOpenCLInstruction);
        else if(instructionSet == "DebugInfo")
            // https://www.khronos.org/registry/spir-v/specs/unified1/DebugInfo.html
            extensionConsumers.emplace(parsed_instruction->result_id, &SPIRVParser::consumeDebugInfoInstruction);
        else if(instructionSet == "OpenCL.DebugInfo.100")
            // https://www.khronos.org/registry/spir-v/specs/unified1/OpenCL.DebugInfo.100.html
            extensionConsumers.emplace(parsed_instruction->result_id, &SPIRVParser::consumeOpenCLDebugInfoInstruction);
        else if(instructionSet.find("NonSemantic."))
            // added by SPV_KHR_non_semantic_info extension, these extended instruction sets have only instructions
            // without semantic meaning and therefore can simply be ignored.
            extensionConsumers.emplace(parsed_instruction->result_id, &SPIRVParser::consumeNonSemanticInstruction);
        else
            throw CompilationError(CompilationStep::PARSER, "Unsupported extended instruction set", instructionSet);
        return SPV_SUCCESS;
    }
    case spv::Op::OpExtInst: // executes instruction from extended instruction set
    {
        auto extensionIt = extensionConsumers.find(getWord(parsed_instruction, 3));
        if(extensionIt == extensionConsumers.end())
            throw CompilationError(CompilationStep::PARSER, "Invalid extended instruction set",
                std::to_string(parsed_instruction->ext_inst_type));
        return (this->*(extensionIt->second))(parsed_instruction);
    }
    case spv::Op::OpMemoryModel:
    {
        auto addressingModel = static_cast<spv::AddressingModel>(getWord(parsed_instruction, 1));
        auto memoryModel = static_cast<spv::MemoryModel>(getWord(parsed_instruction, 2));
        if(addressingModel != spv::AddressingModel::Logical && addressingModel != spv::AddressingModel::Physical32)
            throw CompilationError(CompilationStep::PARSER, "Invalid addressing-mode");
        if(memoryModel != spv::MemoryModel::Simple && memoryModel != spv::MemoryModel::OpenCL)
            throw CompilationError(CompilationStep::PARSER, "Invalid memory model");
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Using a " << (addressingModel == spv::AddressingModel::Logical ? "logical" : "physical") << " "
                << (memoryModel == spv::MemoryModel::Simple ? "simple" : "OpenCL") << " memory model" << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpEntryPoint: // an entry point is an OpenCL kernel
    {
        if(static_cast<spv::ExecutionModel>(getWord(parsed_instruction, 1)) != spv::ExecutionModel::Kernel)
            throw CompilationError(CompilationStep::PARSER, "Invalid execution model");
        SPIRVMethod& m = getOrCreateMethod(*module, methods, getWord(parsed_instruction, 2));
        m.method->isKernel = true;
        m.method->name = readLiteralString(parsed_instruction, &parsed_instruction->operands[2]);
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Kernel-method found: " << m.method->name << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpExecutionMode:
    {
        auto executionMode = static_cast<spv::ExecutionMode>(getWord(parsed_instruction, 2));
        // only Kernel or native execution modes are supported
        if(executionMode == spv::ExecutionMode::LocalSize)
            //"Indicates the work-group size in the x, y, and z dimensions"
            metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES] = {
                getWord(parsed_instruction, 3), getWord(parsed_instruction, 4), getWord(parsed_instruction, 5)};
        else if(executionMode == spv::ExecutionMode::LocalSizeHint)
            //"A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z
            // dimensions"
            metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES_HINT] = {
                getWord(parsed_instruction, 3), getWord(parsed_instruction, 4), getWord(parsed_instruction, 5)};
        else if(executionMode == spv::ExecutionMode::VecTypeHint)
            /*
             * "A hint to the compiler, which indicates that most operations used in the entry point are explicitly
             * vectorized using a particular vector type. The 16 high-order bits of Vector Type operand specify the
             * number of components of the vector. The 16 low-order bits of Vector Type operand specify the data type of
             * the vector."
             */
            CPPLOG_LAZY(logging::Level::INFO,
                log << "Vector type hint is currently not supported: "
                    << static_cast<uint16_t>(getWord(parsed_instruction, 3) >> 16) << " x "
                    << toScalarType(static_cast<uint16_t>(getWord(parsed_instruction, 3) & 0xFFFF)) << logging::endl);
        else if(executionMode != spv::ExecutionMode::ContractionOff)
            throw CompilationError(CompilationStep::PARSER, "Invalid execution mode");
        return SPV_SUCCESS;
    }
    case spv::Op::OpExecutionModeId:
    {
        auto executionMode = static_cast<spv::ExecutionMode>(getWord(parsed_instruction, 2));
        // only Kernel or native execution modes are supported
        if(executionMode == spv::ExecutionMode::LocalSizeId)
            //"Indicates the work-group size in the x, y, and z dimensions"
            metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES] = {
                constantMappings.at(getWord(parsed_instruction, 3)).getScalar()->unsignedInt(),
                constantMappings.at(getWord(parsed_instruction, 4)).getScalar()->unsignedInt(),
                constantMappings.at(getWord(parsed_instruction, 5)).getScalar()->unsignedInt()};
        else if(executionMode == spv::ExecutionMode::LocalSizeHintId)
            //"A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z
            // dimensions"
            metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES_HINT] = {
                constantMappings.at(getWord(parsed_instruction, 3)).getScalar()->unsignedInt(),
                constantMappings.at(getWord(parsed_instruction, 4)).getScalar()->unsignedInt(),
                constantMappings.at(getWord(parsed_instruction, 5)).getScalar()->unsignedInt()};
        else
            throw CompilationError(CompilationStep::PARSER, "Invalid execution mode");
        return SPV_SUCCESS;
    }
    case spv::Op::OpCapability:
        return checkCapability(static_cast<spv::Capability>(getWord(parsed_instruction, 1)));
    case spv::Op::OpTypeVoid:
        typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_VOID);
        return SPV_SUCCESS;
    case spv::Op::OpTypeBool:
        typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_BOOL);
        return SPV_SUCCESS;
    case spv::Op::OpTypeInt:
        typeMappings.emplace(getWord(parsed_instruction, 1),
            getIntegerType(getWord(parsed_instruction, 2), getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpTypeFloat:
        if(getWord(parsed_instruction, 2) == 32)
            typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_FLOAT);
        else if(getWord(parsed_instruction, 2) == 16)
            typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_HALF);
        else if(getWord(parsed_instruction, 2) == 64)
            typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_DOUBLE);
        else
            throw CompilationError(CompilationStep::PARSER, "Unsupported floating-point type");
        return SPV_SUCCESS;
    case spv::Op::OpTypeVector:
    {
        DataType type = typeMappings.at(getWord(parsed_instruction, 2));
        type = type.toVectorType(static_cast<unsigned char>(getWord(parsed_instruction, 3)));
        typeMappings.emplace(getWord(parsed_instruction, 1), type);
        return SPV_SUCCESS;
    }
    case spv::Op::OpTypeImage:
    {
        auto dimensions = static_cast<uint8_t>(getWord(parsed_instruction, 3) + 1);
        bool isImageArray = getWord(parsed_instruction, 5);
        bool isImageBuffer = false;
        if(dimensions == 6 /* buffered */)
        {
            // there are only buffered 1D images
            dimensions = 1;
            isImageBuffer = true;
        }
        bool isSampled = false;
        auto image = module->createImageType(dimensions, isImageArray, isImageBuffer, isSampled);
        typeMappings.emplace(parsed_instruction->result_id, DataType(image));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading image-type '" << image->getTypeName() << "' with " << image->dimensions << " dimensions"
                << (isImageArray ? " (array)" : "") << (image->isImageBuffer ? " (buffer)" : "") << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpTypeSampler:
        typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_SAMPLER);
        return SPV_SUCCESS;
    case spv::Op::OpTypeSampledImage:
    {
        const ImageType* image = typeMappings.at(getWord(parsed_instruction, 2)).getImageType();
        auto dimensions = image->dimensions;
        auto isImageArray = image->isImageArray;
        auto isImageBuffer = image->isImageBuffer;
        auto isSampled = true;
        auto sampledImage = module->createImageType(dimensions, isImageArray, isImageBuffer, isSampled);

        typeMappings.emplace(parsed_instruction->result_id, DataType(sampledImage));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading sampled image-type '" << image->getTypeName() << "' with " << image->dimensions
                << " dimensions" << (image->isImageArray ? " (array)" : "") << (image->isImageBuffer ? " (buffer)" : "")
                << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpTypeArray:
    {
        DataType elementType = typeMappings.at(getWord(parsed_instruction, 2));
        typeMappings.emplace(getWord(parsed_instruction, 1),
            module->createArrayType(elementType,
                static_cast<unsigned>(constantMappings.at(getWord(parsed_instruction, 3)).getScalar()->unsignedInt())));
        return SPV_SUCCESS;
    }
    case spv::Op::OpTypeStruct:
    {
        auto it = names.find(parsed_instruction->result_id);
        std::string structName;
        if(it != names.end())
            structName = it->second;
        else
        {
            logging::warn() << "Struct type " << parsed_instruction->result_id
                            << " has no name, generating default name!";
            structName =
                std::string("%struct.") + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
        }
        auto structDef = module->createStructType(structName, {});

        // add reference to this struct-type to type-mappings
        typeMappings.emplace(parsed_instruction->result_id, DataType(structDef));
        const auto typeIDs = parseArguments(parsed_instruction, 2);
        for(const uint32_t typeID : typeIDs)
        {
            structDef->elementTypes.push_back(typeMappings.at(typeID));
        }
        // set "packed" decoration
        auto it2 = decorationMappings.find(parsed_instruction->result_id);
        if(it2 != decorationMappings.end())
        {
            if(getDecoration(it2->second, spv::Decoration::CPacked))
            {
                structDef->isPacked = true;
            }
        }
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Struct " << structName << ": " << structDef->getContent() << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpTypeOpaque:
        // Since there is no memory-layout to them, they can only be used as pointers
        // TODO how to handle them better (e.g. use their name)? Own complex type for opaque types?
        typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_VOID);
        return SPV_SUCCESS;
    case spv::Op::OpTypePointer:
    {
        DataType type = typeMappings.at(getWord(parsed_instruction, 3));
        type = DataType(module->createPointerType(
            type, toAddressSpace(static_cast<spv::StorageClass>(getWord(parsed_instruction, 2)))));
        typeMappings.emplace(getWord(parsed_instruction, 1), type);
        return SPV_SUCCESS;
    }
    case spv::Op::OpTypeFunction:
        //"OpFunction is the only valid use of OpTypeFunction."
        //-> so we can ignore this
        return SPV_SUCCESS;
    case spv::Op::OpTypeEvent:
        typeMappings.emplace(getWord(parsed_instruction, 1), TYPE_EVENT);
        return SPV_SUCCESS;
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
        constantMappings.emplace(parsed_instruction->result_id, CompoundConstant(TYPE_BOOL, Literal(true)));
        return SPV_SUCCESS;
    case spv::Op::OpConstantFalse:
        constantMappings.emplace(parsed_instruction->result_id, CompoundConstant(TYPE_BOOL, Literal(false)));
        return SPV_SUCCESS;
    case spv::Op::OpConstant: // integer or floating point scalar constant
        constantMappings.emplace(parsed_instruction->result_id, parseConstant(parsed_instruction, typeMappings));
        return SPV_SUCCESS;
    case spv::Op::OpConstantComposite: // constant of array, matrix or vector-type
        constantMappings.emplace(
            parsed_instruction->result_id, parseConstantComposite(parsed_instruction, typeMappings, constantMappings));
        return SPV_SUCCESS;
    case spv::Op::OpConstantSampler: // convert to 32-bit integer constant
    {
        CompoundConstant sampler(typeMappings.at(parsed_instruction->type_id),
            Literal(static_cast<uint32_t>(parseSampler(parsed_instruction))));
        constantMappings.emplace(parsed_instruction->result_id, sampler);
        return SPV_SUCCESS;
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
        auto type = typeMappings.at(parsed_instruction->type_id);
        if(type.isScalarType() || type.getPointerType())
            constantMappings.emplace(parsed_instruction->result_id, CompoundConstant(type, Literal(0u)));
        else if(type.isVectorType())
        {
            auto element = CompoundConstant(type.getElementType(), Literal(0u));
            constantMappings.emplace(parsed_instruction->result_id,
                CompoundConstant(type,
                    {element, element, element, element, element, element, element, element, element, element, element,
                        element, element, element, element, element}));
        }
        else if(type.getArrayType())
            // TODO correct?
            constantMappings.emplace(parsed_instruction->result_id, CompoundConstant(type, Literal(0u)));
        else
            throw CompilationError(CompilationStep::LLVM_2_IR, "Unhandled type for null constant", type.to_string());

        return SPV_SUCCESS;
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
        auto spec = specializeConstant(parsed_instruction->result_id, TYPE_BOOL, decorationMappings);
        constantMappings.emplace(
            parsed_instruction->result_id, spec.value_or(CompoundConstant(TYPE_BOOL, Literal(true))));
        return SPV_SUCCESS;
    }
    case spv::Op::OpSpecConstantFalse:
    {
        //"[...] Similarly, the "True" and "False" parts of OpSpecConstantTrue and OpSpecConstantFalse provide the
        // default Boolean specialization constants."
        auto spec = specializeConstant(parsed_instruction->result_id, TYPE_BOOL, decorationMappings);
        constantMappings.emplace(
            parsed_instruction->result_id, spec.value_or(CompoundConstant(TYPE_BOOL, Literal(false))));
        return SPV_SUCCESS;
    }
    case spv::Op::OpSpecConstant:
    {
        //"The literal operands to OpSpecConstant are the default numerical specialization constants."
        auto constant = parseConstant(parsed_instruction, typeMappings);
        auto spec = specializeConstant(parsed_instruction->result_id, constant.type, decorationMappings);
        constantMappings.emplace(parsed_instruction->result_id, spec.value_or(constant));
        return SPV_SUCCESS;
    }
    case spv::Op::OpSpecConstantComposite:
    {
        auto constant = parseConstantComposite(parsed_instruction, typeMappings, constantMappings);
        auto spec = specializeConstant(parsed_instruction->result_id, constant.type, decorationMappings);
        constantMappings.emplace(parsed_instruction->result_id, spec.value_or(constant));
        return SPV_SUCCESS;
    }
    case spv::Op::OpSpecConstantOp:
    {
        //"The OpSpecConstantOp instruction is specialized by executing the operation and replacing the instruction with
        // the result."
        const DataType type = typeMappings.at(parsed_instruction->type_id);
        auto spec = specializeConstant(parsed_instruction->result_id, type, decorationMappings);
        if(spec)
        {
            constantMappings.emplace(parsed_instruction->result_id, spec.value());
            return SPV_SUCCESS;
        }
        else
        {
            const auto result = calculateConstantOperation(parsed_instruction);
            if(result.first == SPV_SUCCESS && result.second)
                // FIXME how to get Value to CompoundConstant??
                // constantMappings.emplace(parsed_instruction->result_id, result.second.value());
                throw CompilationError(CompilationStep::PARSER, "OpSpecConstantOp is currently not supported!");
            else if(result.first == SPV_SUCCESS)
                // can't fall back to inserting the instruction, since there is no method associated with it!
                throw CompilationError(CompilationStep::PARSER, "Failed to pre-calculate specialization value!");
            return result.first;
        }
    }
    case spv::Op::OpFunction: // new current method -> add to list of all methods
    {
        currentMethod = &getOrCreateMethod(*module, methods, parsed_instruction->result_id);
        currentMethod->method->returnType = typeMappings.at(parsed_instruction->type_id);
        auto it = names.find(parsed_instruction->result_id);
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading function: %" << parsed_instruction->result_id << " ("
                << (it != names.end() ? it->second : currentMethod->method->name) << ")" << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpFunctionParameter:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        currentMethod->parameters.push_back(std::make_pair(parsed_instruction->result_id, parsed_instruction->type_id));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading parameter: " << typeMappings.at(parsed_instruction->type_id).to_string() << " %"
                << parsed_instruction->result_id << logging::endl);
        return SPV_SUCCESS;
    case spv::Op::OpFunctionEnd:
        currentMethod = nullptr;
        return SPV_SUCCESS;
    case spv::Op::OpFunctionCall:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod,
            getWord(parsed_instruction, 3), parsed_instruction->type_id, parseArguments(parsed_instruction, 4)));
        return SPV_SUCCESS;
    case spv::Op::OpVariable:
    {
        //"Allocate an object in memory, resulting in a pointer to it"
        // used for global values as well as stack-allocations
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        auto it = names.find(parsed_instruction->result_id);
        std::string name =
            it != names.end() ? it->second : (std::string("%") + std::to_string(parsed_instruction->result_id));
        auto type = typeMappings.at(parsed_instruction->type_id);
        CompoundConstant val(type.getElementType(), UNDEFINED_LITERAL);
        if(parsed_instruction->num_words > 4)
            val = constantMappings.at(getWord(parsed_instruction, 4));
        unsigned alignment = 0;
        bool isConstant = false;
        spv::BuiltIn builtinId = spv::BuiltIn::Max;
        auto it2 = decorationMappings.find(parsed_instruction->type_id);
        if(it2 != decorationMappings.end())
        {
            alignment = getDecoration(it2->second, spv::Decoration::Alignment).value_or(0);
            isConstant = getDecoration(it2->second, spv::Decoration::Constant).has_value();
        }
        it2 = decorationMappings.find(parsed_instruction->result_id);
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
            AddressSpace::PRIVATE == toAddressSpace(static_cast<spv::StorageClass>(getWord(parsed_instruction, 3))))
        {
            // OpVariables within a function body are stack allocations
            //"All OpVariable instructions in a function must have a Storage Class of Function."
            name = name.find('%') == 0 ? name : ("%" + name);
            auto pos = currentMethod->method->stackAllocations.emplace(StackAllocation(name, type, 0, alignment));
            // TODO set initial value!. Allowed for OpVariables with storage-class Function?
            memoryAllocatedData.emplace(parsed_instruction->result_id, &const_cast<StackAllocation&>(*pos.first));
        }
        else if(builtinId != spv::BuiltIn::Max)
        {
            // This is a built-in, do not generate a global variable, but reference to the UNIFORM/convert to vector3.
            /*
             * From SPI-V view, they are constants (OpVariable in UniformConstant address space) which are read as such
             * (e.g. via OpLoad instruction). Since in OpenCL C source code, only a single dimension can be accessed at
             * any time, the loaded value will be converted to one of the elements (assuming no optimizations occur
             * here) (e.g. via OpCompositeExtract instruction).
             */
            switch(builtinId)
            {
            case spv::BuiltIn::WorkDim:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_WORK_DIMENSIONS);
                break;
            case spv::BuiltIn::GlobalSize:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_GLOBAL_SIZE);
                break;
            case spv::BuiltIn::GlobalInvocationId:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_GLOBAL_ID);
                break;
            case spv::BuiltIn::WorkgroupSize:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_LOCAL_SIZE);
                break;
            case spv::BuiltIn::LocalInvocationId:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_LOCAL_ID);
                break;
            case spv::BuiltIn::NumWorkgroups:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_NUM_GROUPS);
                break;
            case spv::BuiltIn::WorkgroupId:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_GROUP_ID);
                break;
            case spv::BuiltIn::GlobalOffset:
                memoryAllocatedData.emplace(parsed_instruction->result_id, &BUILTIN_GLOBAL_OFFSET);
                break;
            default:
                logging::error() << "Met unsupported builtin " << static_cast<uint32_t>(builtinId) << logging::endl;
                return SPV_UNSUPPORTED;
            }
        }
        else
        {
            // OpVariables outside of any function are global data
            isConstant = isConstant ||
                static_cast<spv::StorageClass>(getWord(parsed_instruction, 3)) == spv::StorageClass::UniformConstant;
            // replace the '%' and add the leading '@' to match the LLVM front-end
            name = "@" + (name.find('%') == 0 ? name.substr(1) : name);
            module->globalData.emplace_back(name, type, CompoundConstant(val), isConstant);
            const_cast<unsigned&>(module->globalData.back().type.getPointerType()->alignment) = alignment;
            memoryAllocatedData.emplace(parsed_instruction->result_id, &module->globalData.back());
        }
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading variable: " << type.to_string() << " " << name << " with value: " << val.to_string(true)
                << logging::endl);
        return SPV_SUCCESS;
    }
    case spv::Op::OpImageTexelPointer:
        //"Form a pointer to a texel of an image. Use of such a pointer is limited to atomic operations."
        return UNSUPPORTED_INSTRUCTION("OpImageTexelPointer");
    case spv::Op::OpLoad:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), MemoryAccess::READ));
        return SPV_SUCCESS;
    case spv::Op::OpStore:
        instructions.emplace_back(new SPIRVCopy(getWord(parsed_instruction, 1), *currentMethod, UNDEFINED_ID,
            getWord(parsed_instruction, 2), MemoryAccess::WRITE));
        return SPV_SUCCESS;
    case spv::Op::OpCopyMemory:
        instructions.emplace_back(new SPIRVCopy(getWord(parsed_instruction, 1), *currentMethod, UNDEFINED_ID,
            getWord(parsed_instruction, 2), MemoryAccess::READ_WRITE));
        return SPV_SUCCESS;
    case spv::Op::OpCopyMemorySized:
        instructions.emplace_back(new SPIRVCopy(getWord(parsed_instruction, 1), *currentMethod, UNDEFINED_ID,
            getWord(parsed_instruction, 2), MemoryAccess::READ_WRITE, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpAccessChain: // pointer into element(s) of composite
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), false));
        return SPV_SUCCESS;
    case spv::Op::OpInBoundsAccessChain:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), false));
        return SPV_SUCCESS;
    case spv::Op::OpPtrAccessChain:
        // For pointers, the "Element" field is the first (top-level) index (see SPIR-V specification,
        // OpPtrAccessChain): "Element is used to do the initial dereference of Base: Base is treated as the address of
        // the first element of an array, and the Element element’s address is computed to be the base for the Indexes
        //[...]"
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), true));
        return SPV_SUCCESS;
    case spv::Op::OpGenericPtrMemSemantics:
        //"Result is a valid Memory Semantics which includes mask bits set for the Storage Class for the specific
        //(non-Generic) Storage Class of Pointer. "
        // not used -> ignore
        return SPV_SUCCESS;
    case spv::Op::OpInBoundsPtrAccessChain:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), true));
        return SPV_SUCCESS;
    case spv::Op::OpNoLine: // source level debug info -> skip
        return SPV_SUCCESS;
    case spv::Op::OpModuleProcessed:
        //"Document a process that was applied to a module. This has no semantic impact and can safely be removed from a
        // module."
        return SPV_SUCCESS;
    case spv::Op::OpDecorate:
        //"Target is the <id> to decorate. It can potentially be any <id> that is a forward reference"
        // -> decorations are always forward references
        // -> can be applied to instruction/parameter/type on parsing it
        return parseDecoration(
            parsed_instruction, parsed_instruction->num_words > 3 ? getWord(parsed_instruction, 3) : UNDEFINED_SCALAR);
    case spv::Op::OpDecorateId:
        //"Target is the <id> to decorate. It can potentially be any <id> that is a forward reference"
        // -> decorations are always forward references
        // -> can be applied to instruction/parameter/type on parsing it
        // In this version, the decoration operands are not literals, but specified by their IDs
        return parseDecoration(parsed_instruction,
            parsed_instruction->num_words > 3 ?
                constantMappings.at(getWord(parsed_instruction, 3)).getScalar()->unsignedInt() :
                UNDEFINED_SCALAR);
    case spv::Op::OpMemberDecorate:
        return UNSUPPORTED_INSTRUCTION("OpMemberDecorate");
    case spv::Op::OpDecorationGroup:
        //"A collector for Decorations from OpDecorate instructions. All such OpDecorate instructions targeting this
        // OpDecorationGroup instruction must precede it." "Subsequent OpGroupDecorate and OpGroupMemberDecorate
        // instructions that consume this instruction�s Result <id> will apply these decorations to their targets."
        // nothing needs to be done, since decorations are added up independent of target and are applied with
        // "OpGroupDecorate"
        return SPV_SUCCESS;
    case spv::Op::OpGroupDecorate:
    {
        // apply group of decorations to IDs
        const std::vector<uint32_t> targets = parseArguments(parsed_instruction, 2);
        const uint32_t decorationGroup = getWord(parsed_instruction, 1);
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
        return SPV_SUCCESS;
    }
    case spv::Op::OpGroupMemberDecorate:
        return UNSUPPORTED_INSTRUCTION("OpGroupMemberDecorate");
    case spv::Op::OpVectorExtractDynamic:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInsertionExtraction(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), false));
        return SPV_SUCCESS;
    case spv::Op::OpVectorInsertDynamic:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInsertionExtraction(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), getWord(parsed_instruction, 4),
            parseArguments(parsed_instruction, 5), false));
        return SPV_SUCCESS;
    case spv::Op::OpVectorShuffle:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVShuffle(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id,
                getWord(parsed_instruction, 3), getWord(parsed_instruction, 4), parseArguments(parsed_instruction, 5)));
        return SPV_SUCCESS;
    case spv::Op::OpCompositeConstruct:
        return UNSUPPORTED_INSTRUCTION("OpCompositeConstruct");
    case spv::Op::OpCompositeExtract:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInsertionExtraction(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), true));
        return SPV_SUCCESS;
    case spv::Op::OpCompositeInsert:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInsertionExtraction(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 4), getWord(parsed_instruction, 3),
            parseArguments(parsed_instruction, 5), true));
        return SPV_SUCCESS;
    case spv::Op::OpCopyObject:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpSampledImage:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        // this is not really an instruction for the runtime, but to associate images <-> sampled-image
        sampledImages[parsed_instruction->result_id] = {getWord(parsed_instruction, 3), getWord(parsed_instruction, 4)};
        return SPV_SUCCESS;
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
        localTypes[parsed_instruction->result_id] = localTypes.at(getWord(parsed_instruction, 3));
        // this is not quite correct (to store a sampled-image where an image is supposed to go), but we extract the
        // image in the image-accessing methods
        sampledImages[parsed_instruction->result_id] = sampledImages.at(getWord(parsed_instruction, 3));
        return SPV_SUCCESS;
    case spv::Op::OpImageQueryFormat:
        //"Query the image format of an image [...]."
        //"The resulting value is an enumerant from Image Channel Data Type."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, ImageQuery::CHANNEL_DATA_TYPE, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpImageQueryOrder:
        //"Query the channel order of an image [...]."
        //"The resulting value is an enumerant from Image Channel Order."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, ImageQuery::CHANNEL_ORDER, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpImageQuerySizeLod:
        //"Query the dimensions of Image for mipmap level for Level of Detail."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id,
                ImageQuery::SIZES_LOD, getWord(parsed_instruction, 3), getWord(parsed_instruction, 4)));
        return SPV_SUCCESS;
    case spv::Op::OpImageQuerySize:
        //"Query the dimensions of Image, with no level of detail."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, ImageQuery::SIZES, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpImageQueryLevels:
        //"Query the number of mipmap levels accessible through Image."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, ImageQuery::MIPMAP_LEVELS, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpImageQuerySamples:
        //"Query the number of samples available per texel fetch in a multisample image."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, ImageQuery::SAMPLES_PER_TEXEL, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpConvertFToU:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fptoui",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpConvertFToS:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fptosi", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpConvertSToF:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "sitofp", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpConvertUToF:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "uitofp", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpUConvert: // change bit-width (type) of value
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED_TO_UNSIGNED,
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSConvert: // change bit-width (type) of value
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::SIGNED_TO_SIGNED,
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFConvert: // change bit-width (type) of value
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::FLOATING,
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpConvertPtrToU: // pointer to unsigned -> same as OpUConvert
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED_TO_UNSIGNED,
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSatConvertSToU: // signed to unsigned (with saturation)
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::SIGNED_TO_UNSIGNED,
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                add_flag(intermediate::InstructionDecorations::UNSIGNED_RESULT,
                    intermediate::InstructionDecorations::SATURATED_CONVERSION))));
        return SPV_SUCCESS;
    case spv::Op::OpSatConvertUToS: // unsigned to signed (with saturation)
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED_TO_SIGNED,
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::SATURATED_CONVERSION)));
        return SPV_SUCCESS;
    case spv::Op::OpConvertUToPtr: // unsigned to pointer -> same as OpUConvert
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED_TO_UNSIGNED,
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpPtrCastToGeneric:
        //"Convert a pointer’s Storage Class to Generic."
        // -> simply copy the pointer
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::BITCAST,
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpGenericCastToPtr:
        //"Convert a pointer’s Storage Class to a non-Generic class."
        // -> simple copy the pointer
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::BITCAST,
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpGenericCastToPtrExplicit:
        //"Attempts to explicitly convert Pointer to Storage storage-class pointer value."
        // -> simple copy the pointer
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod,
            parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::BITCAST,
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpBitcast:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id,
                getWord(parsed_instruction, 3), ConversionType::BITCAST, intermediate::InstructionDecorations::NONE));
        return SPV_SUCCESS;
    case spv::Op::OpSNegate:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, OP_NEGATE, parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFNegate:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, OP_NEGATE, parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpIAdd:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "add", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFAdd:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fadd", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpISub:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "sub", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFSub:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fsub", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpIMul:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "mul", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFMul:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fmul", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpUDiv:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "udiv",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSDiv:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "sdiv", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFDiv:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fdiv", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpUMod:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "umod",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSRem:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "srem", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpSMod:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "smod", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFRem:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "frem", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFMod:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fmod", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpVectorTimesScalar: // type must be floating point
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fmul", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpDot:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "dot",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
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
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVFoldInstruction(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, "or",
                getWord(parsed_instruction, 3), intermediate::InstructionDecorations::UNSIGNED_RESULT));
        return SPV_SUCCESS;
    case spv::Op::OpAll:
        // This is NOT the OpenCL all(...), it does NOT check the MSB, instead it takes a vector of boolean values and
        // checks whether all of them are true!
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVFoldInstruction(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, "and",
                getWord(parsed_instruction, 3), intermediate::InstructionDecorations::UNSIGNED_RESULT));
        return SPV_SUCCESS;
    case spv::Op::OpIsNan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVBoolCallSite(parsed_instruction->result_id, *currentMethod, "vc4cl_is_nan",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpIsInf:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVBoolCallSite(parsed_instruction->result_id, *currentMethod, "isinf",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpIsFinite:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVBoolCallSite(parsed_instruction->result_id, *currentMethod, "isfinite",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpIsNormal:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVBoolCallSite(parsed_instruction->result_id, *currentMethod, "isnormal",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpSignBitSet:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementations are reverted back to the SPIR-V
        // opcodes, create a function call to the VC4CL function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVBoolCallSite(parsed_instruction->result_id, *currentMethod, "signbit",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpLessOrGreater:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVBoolCallSite(parsed_instruction->result_id, *currentMethod, "islessgreater",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpOrdered:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpUnordered:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpLogicalEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpLogicalNotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpLogicalOr:
        //"Result Type must be a scalar or vector of Boolean type."
        // -> same as bitwise OR
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "or", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpLogicalAnd:
        //"Result Type must be a scalar or vector of Boolean type."
        // -> same as bitwise AND
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "and", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpLogicalNot:
        //"Result Type must be a scalar or vector of Boolean type."
        // -> same as bitwise NOT
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "not", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpSelect:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVSelect(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id,
                getWord(parsed_instruction, 3), getWord(parsed_instruction, 4), getWord(parsed_instruction, 5)));
        return SPV_SUCCESS;
    case spv::Op::OpIEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpINotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpUGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNSIGNED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_SIGNED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpUGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNSIGNED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_SIGNED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpULessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNSIGNED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSLessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_SIGNED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpULessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNSIGNED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            add_flag(toInstructionDecorations(parsed_instruction->result_id),
                intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        return SPV_SUCCESS;
    case spv::Op::OpSLessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_SIGNED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFOrdEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFUnordEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFOrdNotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFUnordNotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFOrdLessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFUnordLessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFOrdGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFUnordGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFOrdLessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFUnordLessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFOrdGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_ORDERED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpFUnordGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod,
            intermediate::COMP_UNORDERED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3),
            toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpShiftRightLogical:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "shr", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpShiftRightArithmetic:
        // Instead of directly mapping to "asr" operation, we let the intrinsics lowering handle the shift to apply
        // sign-extension for non-32-bit types.
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "ashr", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpShiftLeftLogical:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "shl", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpBitwiseOr:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "or", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpBitwiseXor:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "xor", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpBitwiseAnd:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "and", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpNot:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(
            new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "not", parsed_instruction->type_id,
                parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpBitCount:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "popcount",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpControlBarrier:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation of "barrier()" is reverted back
        // to this OpControlBarrier, create a function call to the "barrier()" function
        instructions.emplace_back(new SPIRVCallSite(*currentMethod, "barrier", {getWord(parsed_instruction, 1)}));
        return SPV_SUCCESS;
    case spv::Op::OpMemoryBarrier:
        instructions.emplace_back(
            new SPIRVMemoryBarrier(*currentMethod, getWord(parsed_instruction, 1), getWord(parsed_instruction, 2)));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicLoad:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicLoad");
    case spv::Op::OpAtomicStore:
        // OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicStore");
    case spv::Op::OpAtomicExchange:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_xchg",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicCompareExchangeWeak:
        // OpenCL 2.x feature
        // since SPIR-V 1.3 deprecated in favor of OpAtomicCompareExchange (identical behavior)
    case spv::Op::OpAtomicCompareExchange:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_cmpxchg",
            parsed_instruction->type_id,
            {getWord(parsed_instruction, 3), getWord(parsed_instruction, 8), getWord(parsed_instruction, 7)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicIIncrement:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_inc",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicIDecrement:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_dec",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicIAdd:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_add",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicISub:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_sub",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicSMin:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_min",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicUMin:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_min",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicSMax:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_max",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicUMax:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_max",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicAnd:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_and",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicOr:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_or",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpAtomicXor:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation is reverted back to the SPIR-V
        // opcode, create a function call to the function definition
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "atomic_xor",
            parsed_instruction->type_id, {getWord(parsed_instruction, 3), getWord(parsed_instruction, 6)}));
        return SPV_SUCCESS;
    case spv::Op::OpPhi:
    {
        const std::vector<uint32_t> args = parseArguments(parsed_instruction, 3);
        std::vector<std::pair<uint32_t, uint32_t>> sources;
        sources.reserve(args.size() / 2);
        for(std::size_t i = 0; i < args.size(); i += 2)
        {
            sources.emplace_back(args[i], args[i + 1]);
        }
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVPhi(
            parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, std::move(sources)));
        return SPV_SUCCESS;
    }
    case spv::Op::OpLoopMerge:
        return UNSUPPORTED_INSTRUCTION("OpLoopMerge");
    case spv::Op::OpSelectionMerge:
        return UNSUPPORTED_INSTRUCTION("OpSelectionMerge");
    case spv::Op::OpLabel:
        typeMappings.emplace(parsed_instruction->result_id, TYPE_LABEL);
        instructions.emplace_back(new SPIRVLabel(parsed_instruction->result_id, *currentMethod));
        return SPV_SUCCESS;
    case spv::Op::OpBranch:
        instructions.emplace_back(new SPIRVBranch(*currentMethod, getWord(parsed_instruction, 1)));
        return SPV_SUCCESS;
    case spv::Op::OpBranchConditional:
        instructions.emplace_back(new SPIRVBranch(*currentMethod, getWord(parsed_instruction, 1),
            getWord(parsed_instruction, 2), getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case spv::Op::OpSwitch:
    {
        const std::vector<uint32_t> args = parseArguments(parsed_instruction, 3);
        std::vector<std::pair<uint32_t, uint32_t>> destinations;
        destinations.reserve(args.size() / 2);
        for(std::size_t i = 0; i < args.size(); i += 2)
        {
            destinations.emplace_back(args[i], args[i + 1]);
        }
        instructions.emplace_back(new SPIRVSwitch(parsed_instruction->result_id, *currentMethod,
            getWord(parsed_instruction, 1), getWord(parsed_instruction, 2), std::move(destinations)));
        return SPV_SUCCESS;
    }
    case spv::Op::OpReturn:
        instructions.emplace_back(new SPIRVReturn(*currentMethod));
        return SPV_SUCCESS;
    case spv::Op::OpReturnValue:
        instructions.emplace_back(new SPIRVReturn(getWord(parsed_instruction, 1), *currentMethod));
        return SPV_SUCCESS;
    case spv::Op::OpUnreachable:
        return SPV_SUCCESS;
    case spv::Op::OpLifetimeStart:
    {
        // for temporary variables (e.g. Function-Scope), the size is set via the OpLifetimeStart, since the OpVariable
        // is of type void
        instructions.emplace_back(new SPIRVLifetimeInstruction(getWord(parsed_instruction, 1), *currentMethod,
            getWord(parsed_instruction, 2), false, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    }
    case spv::Op::OpLifetimeStop:
        instructions.emplace_back(new SPIRVLifetimeInstruction(getWord(parsed_instruction, 1), *currentMethod,
            getWord(parsed_instruction, 2), true, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case spv::Op::OpGroupAsyncCopy:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation of "async_work_group_copy()" is
        // reverted back to this OpGroupAsyncCopy, create a function call to the "async_work_group_copy()" function
        // TODO support for strided version (word 7), map to async_work_group_strided_copy
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod,
            "async_work_group_copy", parsed_instruction->type_id,
            {getWord(parsed_instruction, 4), getWord(parsed_instruction, 5), getWord(parsed_instruction, 6),
                getWord(parsed_instruction, 8)}));
        return SPV_SUCCESS;
    case spv::Op::OpGroupWaitEvents:
        // for usage with SPIRV-LLVM translator, where the VC4CL std-lib implementation of "wait_group_events()" is
        // reverted back to this OpGroupWaitEvents, create a function call to the "wait_group_events()" function
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "wait_group_events",
            parsed_instruction->type_id, parseArguments(parsed_instruction, 2)));
        return SPV_SUCCESS;
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
    logging::warn() << "Unhandled instruction-type: " << parsed_instruction->opcode << logging::endl;
    return SPV_UNSUPPORTED;
}

std::pair<spv_result_t, Optional<Value>> SPIRVParser::calculateConstantOperation(
    const spv_parsed_instruction_t* instruction)
{
    /*
     * "Opcode must be one of the following opcodes:
     * OpSConvert, OpFConvert, OpSNegate, OpNot, OpIAdd, OpISub, OpIMul, OpUDiv, OpSDiv, OpUMod, OpSRem, OpSMod,
     * OpShiftRightLogical, OpShiftRightArithmetic, OpShiftLeftLogical, OpBitwiseOr, OpBitwiseXor, OpBitwiseAnd,
     * OpVectorShuffle, OpCompositeExtract, OpCompositeInsert, OpLogicalOr, OpLogicalAnd, OpLogicalNot,
     * OpLogicalEqual, OpLogicalNotEqual, OpSelect, OpIEqual, OpINotEqual, OpULessThan, OpSLessThan, OpUGreaterThan,
     * OpSGreaterThan, OpULessThanEqual, OpSLessThanEqual, OpUGreaterThanEqual, OpSGreaterThanEqual"
     *
     * "If the Kernel capability was declared, the following opcodes are also valid:
     * OpConvertFToS, OpConvertSToF, OpConvertFToU, OpConvertUToF, OpUConvert, OpConvertPtrToU, OpConvertUToPtr,
     * OpGenericCastToPtr, OpPtrCastToGeneric, OpBitcast, OpFNegate, OpFAdd, OpFSub, OpFMul, OpFDiv, OpFRem, OpFMod,
     * OpAccessChain, OpInBoundsAccessChain, OpPtrAccessChain, OpInBoundsPtrAccessChain
     */
    SPIRVMethod* methodBackup = currentMethod;
    std::vector<std::unique_ptr<SPIRVOperation>> instructionsBackup;
    instructions.swap(instructionsBackup);
    currentMethod = nullptr;
    uint32_t dummyWords[12] = {};
    spv_parsed_instruction_t dummyInstruction;

    dummyInstruction.words = dummyWords;
    dummyInstruction.num_words = static_cast<uint16_t>(instruction->num_words - 1);
    dummyInstruction.ext_inst_type = SPV_EXT_INST_TYPE_NONE;
    dummyInstruction.num_operands = 0;
    dummyInstruction.opcode = static_cast<uint16_t>(getWord(instruction, 3));
    dummyInstruction.operands = nullptr;
    dummyInstruction.result_id = instruction->result_id;
    dummyInstruction.type_id = instruction->type_id;

    // only skip length/op-code. The other words are required, so the number/position of words fit
    memcpy(dummyWords, instruction->words + 1, (instruction->num_words - 1) * sizeof(uint32_t));
    dummyWords[0] = dummyInstruction.opcode;
    dummyWords[1] = dummyInstruction.type_id;
    dummyWords[2] = dummyInstruction.result_id;

    spv_result_t result = parseInstruction(&dummyInstruction);
    if(result != SPV_SUCCESS || instructions.empty())
    {
        return std::make_pair(result, NO_VALUE);
    }

    const Optional<Value> value = instructions.at(0)->precalculate(typeMappings, constantMappings, memoryAllocatedData);

    // swap back
    currentMethod = methodBackup;
    instructions.swap(instructionsBackup);

    return std::make_pair(SPV_SUCCESS, value);
}

spv_result_t SPIRVParser::consumeOpenCLInstruction(const spv_parsed_instruction_t* instruction)
{
    localTypes[instruction->result_id] = instruction->type_id;
    if(getWord(instruction, 4) == OpenCLLIB::Entrypoints::Shuffle2)
    {
        instructions.emplace_back(new SPIRVShuffle(instruction->result_id, *currentMethod, instruction->type_id,
            getWord(instruction, 5), getWord(instruction, 6), getWord(instruction, 7)));
        return SPV_SUCCESS;
    }
    if(getWord(instruction, 4) == OpenCLLIB::Entrypoints::Shuffle)
    {
        instructions.emplace_back(new SPIRVShuffle(instruction->result_id, *currentMethod, instruction->type_id,
            getWord(instruction, 5), UNDEFINED_ID, getWord(instruction, 6)));
        return SPV_SUCCESS;
    }
    // the OpenCL built-in operations are not supported directly, but there might be a function definition for them
    // here, we simply map them to function calls and resolve the possible matching definitions later
    instructions.emplace_back(new SPIRVCallSite(instruction->result_id, *currentMethod,
        getOpenCLMethodName(getWord(instruction, 4)), instruction->type_id, parseArguments(instruction, 5)));
    return SPV_SUCCESS;
}

spv_result_t SPIRVParser::consumeDebugInfoInstruction(const spv_parsed_instruction_t* instruction)
{
    // These instructions are guaranteed to be non-semantic, so we can just ignore them
    return SPV_SUCCESS;
}

spv_result_t SPIRVParser::consumeOpenCLDebugInfoInstruction(const spv_parsed_instruction_t* instruction)
{
    // These instructions are guaranteed to be non-semantic, so we can just ignore them
    return SPV_SUCCESS;
}

spv_result_t SPIRVParser::consumeNonSemanticInstruction(const spv_parsed_instruction_t* instruction)
{
    // These instructions are guaranteed to be non-semantic, so we can just ignore them
    return SPV_SUCCESS;
}

#endif
