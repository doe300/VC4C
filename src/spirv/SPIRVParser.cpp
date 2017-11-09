/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <string.h>
#include <stdint.h>

#include "SPIRVParser.h"
#include "log.h"
#include "SPIRVHelper.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../intrinsics/Images.h"
#ifdef SPIRV_HEADER

#ifdef SPIRV_OPTIMIZER_HEADER
#include SPIRV_OPTIMIZER_HEADER
#endif

using namespace vc4c;
using namespace vc4c::spirv2qasm;

SPIRVParser::SPIRVParser(std::istream& input, const bool isSPIRVText) : isTextInput(isSPIRVText), input(input), currentMethod(nullptr), module(nullptr)
{

}

static spv_result_t parsedHeaderCallback(void* user_data, spv_endianness_t endian, uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound, uint32_t reserved)
{
    logging::debug() << "SPIR-V header parsed: magic-number 0x" << std::hex << magic << ", version 0x" << version << ", generator " << generator << ", max-ID " << std::dec << id_bound << logging::endl;
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
    if(diagnostics == NULL)
        return "?";
    return std::to_string(diagnostics->position.line).append(":") + std::to_string(diagnostics->position.column);
}

static std::vector<uint32_t> runSPRVToolsOptimizer(const std::vector<uint32_t>& input)
{
#ifdef SPIRV_OPTIMIZER_HEADER
	logging::debug() << "Running SPIR-V Tools optimizations..." << logging::endl;
	spvtools::Optimizer opt(SPV_ENV_OPENCL_2_1);
	opt.SetMessageConsumer(consumeSPIRVMessage);
	//converts OpSpecConstant(True/False) to OpConstant(True/False)
	opt.RegisterPass(spvtools::CreateFreezeSpecConstantValuePass());
	//converts OpSpecConstantOp and OpSpecConstantComposite to OpConstants
	opt.RegisterPass(spvtools::CreateFoldSpecConstantOpAndCompositePass());
	//unified duplicate constants
	opt.RegisterPass(spvtools::CreateUnifyConstantPass());
	//removed obsolete constants
	opt.RegisterPass(spvtools::CreateEliminateDeadConstantPass());
	//inline methods
	opt.RegisterPass(spvtools::CreateInlinePass());
	//converts access-chain with constant indices
	opt.RegisterPass(spvtools::CreateLocalAccessChainConvertPass());
	//replaces access to local memory with register-usage
	opt.RegisterPass(spvtools::CreateLocalSingleBlockLoadStoreElimPass());

	std::vector<uint32_t> optimizedWords;
	if(!opt.Run(input.data(), input.size(), &optimizedWords))
	{
		logging::warn() << "Error running SPIR-V Tools optimizer!" << logging::endl;
	}
	else if(optimizedWords.size() > 0)
	{
		logging::debug() << "SPIR-V Tools optimizations complete, changed number of words from " << input.size() << " to " << optimizedWords.size() << logging::endl;
		return optimizedWords;
	}
	else
		logging::debug() << "SPIR-V Tools optimizations complete, no changes." << logging::endl;
#endif
	return input;
}


//to relay names of unsupported operations
static thread_local std::string errorExtra;

void SPIRVParser::parse(Module& module)
{
	this->module = &module;
    spv_diagnostic diagnostics = NULL;
    spv_context context = spvContextCreate(SPV_ENV_OPENCL_2_1);
    if (context == NULL) {
        throw CompilationError(CompilationStep::PARSER, "Failed to create SPIR-V context");
    }

    //read input and map into buffer
    std::vector<uint32_t> words = readStreamOfWords(input);

    //if input is SPIR-V text, convert to binary representation
    spv_result_t result;
    if (isTextInput) {
    	spvtools::SpirvTools tools(SPV_ENV_OPENCL_2_1);
    	tools.SetMessageConsumer(consumeSPIRVMessage);
    	std::vector<uint32_t> binaryData;
    	logging::debug() << "Read SPIR-V text with " << words.size() * sizeof (uint32_t) << " characters" << logging::endl;
    	if(tools.Assemble(reinterpret_cast<char*>(words.data()), words.size() * sizeof(uint32_t), &binaryData))
    		words.swap(binaryData);
    }
    else {
        logging::debug() << "Read SPIR-V binary with " << words.size() << " words" << logging::endl;
    }

    //run SPIR-V Tools optimizations
#ifdef SPIRV_OPTIMIZER_HEADER
    words = runSPRVToolsOptimizer(words);
#endif

    logging::debug() << "Starting parsing..." << logging::endl;

    //parse input
    result = spvBinaryParse(context, this, words.data(), words.size(), parsedHeaderCallback, parsedInstructionCallback, &diagnostics);

    if (result != SPV_SUCCESS) {
        logging::error() << getErrorMessage(result) << ": " << (diagnostics != NULL ? diagnostics->error : errorExtra) << " at " << getErrorPosition(diagnostics) << logging::endl;
        spvContextDestroy(context);
        throw CompilationError(CompilationStep::PARSER, getErrorMessage(result), (diagnostics != NULL ? diagnostics->error : errorExtra));
    }
    logging::debug() << "SPIR-V binary successfully parsed" << logging::endl;
    spvContextDestroy(context);

    // resolve method parameters
    //set names, e.g. for methods, parameters
    for (auto& m : methods) {
        if (names.find(m.first) != names.end())
            m.second.method->name = names.at(m.first);
        m.second.method->parameters.reserve(m.second.parameters.size());
        for (const auto& pair : m.second.parameters)
        {
            std::string name;
            if (names.find(pair.first) != names.end())
            {
            	//parameters are referenced by their IDs, not their names, but for meta-data the names are better
                name = names.at(pair.first);
                m.second.method->metaData[MetaDataType::ARG_NAMES].push_back(name);
            }
            const DataType& type = typeMappings.at(pair.second);
            Parameter param(std::string("%") + std::to_string(pair.first), type);
            if(decorationMappings.find(pair.first) != decorationMappings.end())
            {
            	setParameterDecorations(param, decorationMappings.at(pair.first));
            }
            m.second.method->parameters.emplace_back(std::move(param));
        }
    }

    //map SPIRVOperations to IntermediateInstructions
    logging::debug() << "Mapping instructions to intermediate..." << logging::endl;
    for (const std::unique_ptr<SPIRVOperation>& op : instructions) {
        op->mapInstruction(typeMappings, constantMappings, localTypes, methods, memoryAllocatedData);
    }

    //apply kernel meta-data, decorations, ...
    for (const auto& pair : metadataMappings) {
        methods.at(pair.first).method->metaData.insert(pair.second.begin(), pair.second.end());
    }

    //delete empty functions (e.g. declared, but not defined, externally linked, intrinsics)
    auto it = methods.begin();
    while (it != methods.end()) {
        if ((*it).second.method->countInstructions() == 0)
            it = methods.erase(it);
        else if((*it).second.method->countInstructions() == 1 && (*it).second.method->getBasicBlocks().front().empty())
        {
            //only instruction is the label (which is automatically added)
        	//need to erase the label first, so the local can be correctly removed
        	(*it).second.method->getBasicBlocks().front().begin().erase();
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
}

spv_result_t SPIRVParser::parseHeader(spv_endianness_t endian, uint32_t magic, uint32_t version, uint32_t generator, uint32_t id_bound, uint32_t reserved)
{
    //see: https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#_a_id_physicallayout_a_physical_layout_of_a_spir_v_module_and_instruction
	//not completely true, since the header is not mapped to instructions, but still better than increasing every X new instruction
	instructions.reserve(id_bound);

    return SPV_SUCCESS;
}

intermediate::InstructionDecorations toInstructionDecoration(const SpvFPFastMathModeMask mode)
{
    intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE;
    if(mode & SpvFPFastMathModeAllowRecipMask)
    	decorations = add_flag(decorations, intermediate::InstructionDecorations::ALLOW_RECIP);
    if(mode & SpvFPFastMathModeFastMask)
    	decorations = add_flag(decorations, intermediate::InstructionDecorations::FAST_MATH);
    if(mode & SpvFPFastMathModeNotInfMask)
    	decorations = add_flag(decorations, intermediate::InstructionDecorations::NO_INF);
    if(mode & SpvFPFastMathModeNotNaNMask)
    	decorations = add_flag(decorations, intermediate::InstructionDecorations::NO_NAN);
    return decorations;
}

intermediate::InstructionDecorations SPIRVParser::toInstructionDecorations(const uint32_t id)
{
	intermediate::InstructionDecorations deco = intermediate::InstructionDecorations::NONE;
    if(decorationMappings.find(id) == decorationMappings.end())
    {
        return intermediate::InstructionDecorations::NONE;
    }
    const auto decoration = getDecoration(decorationMappings.at(id), SpvDecorationFPFastMathMode);
    if(decoration)
    	deco = add_flag(deco, toInstructionDecoration(static_cast<SpvFPFastMathModeMask>(decoration.get())));
    if(getDecoration(decorationMappings.at(id), SpvDecorationSaturatedConversion))
    	deco = add_flag(deco, intermediate::InstructionDecorations::SATURATED_CONVERSION);
    return deco;
}

static uint32_t getWord(const spv_parsed_instruction_t* instruction, std::size_t index)
{
    if (instruction->num_words <= index)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(index));
    return instruction->words[index];
}

static std::string readLiteralString(const spv_parsed_instruction_t* instruction, const spv_parsed_operand_t* operand)
{
    if (instruction->num_words <= operand->offset)
        throw CompilationError(CompilationStep::PARSER, "Word index out of bounds", std::to_string(operand->offset));
    const size_t length = strnlen(reinterpret_cast<const char*>(instruction->words + operand->offset), sizeof (uint32_t) * operand->num_words);
    return std::string(reinterpret_cast<const char*>(instruction->words + operand->offset), length);
}

spv_result_t SPIRVParser::parseDecoration(const spv_parsed_instruction_t* parsed_instruction)
{
    const uint32_t target = getWord(parsed_instruction, 1);
    switch (static_cast<SpvDecoration>(getWord(parsed_instruction, 2))) {
    case SpvDecorationCPacked:  //struct is "packed"
    case SpvDecorationBuiltIn:  //entity (object, struct-member) represents given built-in	//TODO map to built-in?
    case SpvDecorationSaturatedConversion: //out-of range results are clamped, do not overflow
    case SpvDecorationFuncParamAttr:
    case SpvDecorationFPRoundingMode: //explicit rounding mode
    case SpvDecorationFPFastMathMode: //allows fast-math modes
    case SpvDecorationSpecId:	//constant as specialization-value of OpSpecXXX
    case SpvDecorationAlignment: //known alignment of pointer
    case SpvDecorationMaxByteOffset: //known maximum offset (in bytes) from base address, this pointer is accessed at
    case SpvDecorationConstant:	//global data is constant, is never written
    case SpvDecorationRestrict: //memory behind pointer is restricted, e.g. not aliased to another pointer
    case SpvDecorationVolatile: //data behind pointer is volatile
        decorationMappings[target].push_back({static_cast<SpvDecoration>(getWord(parsed_instruction, 2)), parsed_instruction->num_words > 3 ? getWord(parsed_instruction, 3) : 0xFFFFFFFFU});
        return SPV_SUCCESS;
    case SpvDecorationAlignmentId:	//known alignment of pointer, but as constant, not literal value
    	decorationMappings[target].push_back({SpvDecorationAlignment, parsed_instruction->num_words > 3 ? constantMappings.at(getWord(parsed_instruction, 3)).literal.integer : 0xFFFFFFFFU});
		return SPV_SUCCESS;
    case SpvDecorationMaxByteOffsetId:	//known maximum offset (in bytes) from base address, this pointer is accessed at, but as constant, not literal value
    	decorationMappings[target].push_back({SpvDecorationMaxByteOffset, parsed_instruction->num_words > 3 ? constantMappings.at(getWord(parsed_instruction, 3)).literal.integer : 0xFFFFFFFFU});
		return SPV_SUCCESS;
    default:
        //simply ignore all other decorations
        return SPV_SUCCESS;
    }
}

static std::vector<uint32_t> parseArguments(const spv_parsed_instruction_t* instruction, const uint32_t startWord)
{
    std::vector<uint32_t> args;
    if(instruction->num_words <= startWord)
    	return args;
    args.reserve(instruction->num_words - startWord);
    for (std::size_t i = startWord; i < instruction->num_words; ++i)
        args.push_back(instruction->words[i]);
    return args;
}

static intermediate::Sampler parseSampler(const spv_parsed_instruction_t* instruction)
{
    intermediate::Sampler tmp(0);
    switch(static_cast<SpvSamplerAddressingMode>(getWord(instruction, 3)))
    {
        case SpvSamplerAddressingModeNone:
            tmp.setAddressingMode(intermediate::AddressingMode::NONE);
            break;
        case SpvSamplerAddressingModeClampToEdge:
            tmp.setAddressingMode(intermediate::AddressingMode::CLAMP_TO_EDGE);
            break;
        case SpvSamplerAddressingModeClamp:
            tmp.setAddressingMode(intermediate::AddressingMode::CLAMP);
            break;
        case SpvSamplerAddressingModeRepeat:
            tmp.setAddressingMode(intermediate::AddressingMode::REPEAT);
            break;
        case SpvSamplerAddressingModeRepeatMirrored:
            tmp.setAddressingMode(intermediate::AddressingMode::MIRRORED_REPEAT);
            break;
        default:
            throw CompilationError(CompilationStep::PARSER, "Unknown sampler addressing mode", std::to_string(getWord(instruction, 3)));
    }
    
    tmp.setNormalizedCoordinates(getWord(instruction, 4));
    
    switch(static_cast<SpvSamplerFilterMode>(getWord(instruction, 5)))
    {
        case SpvSamplerFilterModeNearest:
            tmp.setFilterMode(intermediate::FilterMode::NEAREST);
            break;
        case SpvSamplerFilterModeLinear:
            tmp.setFilterMode(intermediate::FilterMode::LINEAR);
            break;
        default:
            throw CompilationError(CompilationStep::PARSER, "Unknown sampler filter mode", std::to_string(getWord(instruction, 5)));
    }
    
    return tmp;
}

static Value parseConstant(const spv_parsed_instruction_t* instruction, const std::map<uint32_t, DataType>& typeMappings)
{
	Value constant(typeMappings.at(instruction->type_id));
	if (instruction->num_words > 3) {
		constant.valueType = ValueType::LITERAL;
		//"Types 32 bits wide or smaller take one word."
		constant.literal = Literal(static_cast<int64_t> (getWord(instruction, 3)));
		if (instruction->num_words > 4)
			//"[...] Larger types take multiple words, with low-order words appearing first."
			//e.g. for long/double constants
			constant.literal.integer |= static_cast<int64_t> (getWord(instruction, 4)) << 32;
	}
	return constant;
}

static Value parseConstantComposite(const spv_parsed_instruction_t* instruction, const std::map<uint32_t, DataType>& typeMappings, const std::map<uint32_t, Value>& constantMappings)
{
	DataType containerType = typeMappings.at(getWord(instruction, 1));
	std::vector<Value> constants;
	for (std::size_t i = 3; i < instruction->num_words; ++i) {
		//"Result Type must be a composite type, whose top-level members/elements/components/columns have the same type as the types of the Constituents."
		// -> no heterogeneous composite possible
		const Value element = constantMappings.at(instruction->words[i]);
		constants.push_back(element);
	}
	return Value(ContainerValue{constants}, containerType);
}

static Optional<Value> specializeConstant(const uint32_t resultID, const DataType& type, const FastMap<uint32_t, std::vector<std::pair<SpvDecoration, uint32_t>>>& decorations)
{
	if(decorations.find(resultID) != decorations.end())
	{
		Optional<uint32_t> res(getDecoration(decorations.at(resultID), SpvDecorationSpecId));
		if(res.hasValue)
			return Value(Literal(static_cast<int64_t>(res.get())), type);
	}
	return NO_VALUE;
}

static SPIRVMethod& getOrCreateMethod(const Module& module, std::map<uint32_t, SPIRVMethod>& methods, const uint32_t id)
{
	if(methods.find(id) == methods.end())
		methods.emplace(id, SPIRVMethod(id, module));
	return methods.at(id);
}

#define UNSUPPORTED_INSTRUCTION(name) (errorExtra = name, logging::error() << "Unsupported SPIR-V instruction: " << name << logging::endl, SPV_UNSUPPORTED)

spv_result_t SPIRVParser::parseInstruction(const spv_parsed_instruction_t* parsed_instruction)
{
    if (parsed_instruction == NULL)
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

    //see: https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html#_a_id_instructions_a_instructions
    switch (static_cast<SpvOp>(parsed_instruction->opcode)) {
    case SpvOpNop:
        return SPV_SUCCESS;
    case SpvOpUndef:
        constantMappings.emplace(parsed_instruction->result_id, UNDEFINED_VALUE);
		constantMappings.at(parsed_instruction->result_id).type = typeMappings.at(parsed_instruction->type_id);
        return SPV_SUCCESS;
    case SpvOpSourceContinued:
        return SPV_SUCCESS;
    case SpvOpSource:
        return SPV_SUCCESS;
    case SpvOpSourceExtension:
        break;
    case SpvOpName: //e.g. method-name, parameters
        names[getWord(parsed_instruction, 1)] = readLiteralString(parsed_instruction, &parsed_instruction->operands[1]);
        return SPV_SUCCESS;
    case SpvOpMemberName: //name of struct-member
        return SPV_SUCCESS;
    case SpvOpString:
        break;
    case SpvOpLine: //source level debug info
        return SPV_SUCCESS;
    case SpvOpExtension:
    {
        const std::string extension = readLiteralString(parsed_instruction, &parsed_instruction->operands[0]);
        logging::debug() << "Using extension: " << extension << logging::endl;
        //there are currently no extensions supported
        //a list of all extension: https://www.khronos.org/registry/spir-v/
        throw CompilationError(CompilationStep::PARSER, "Use of unsupported SPIR-V extension", extension);
    }
    case SpvOpExtInstImport: //adds a new set of instructions
    {
        const std::string instructionSet(readLiteralString(parsed_instruction, &parsed_instruction->operands[1]));
        logging::debug() << "Importing extended instruction set: " << instructionSet << logging::endl;
        if (instructionSet.find("OpenCL") == std::string::npos)
            throw CompilationError(CompilationStep::PARSER, "Unsupported extended instruction set", instructionSet);
        return SPV_SUCCESS;
    }
    case SpvOpExtInst: //executes instruction from extended instruction set
    {
        if (parsed_instruction->ext_inst_type != SPV_EXT_INST_TYPE_OPENCL_STD)
            throw CompilationError(CompilationStep::PARSER, "Invalid extended instruction set", std::to_string(parsed_instruction->ext_inst_type));
        if(getWord(parsed_instruction, 4) == OpenCLLIB::Entrypoints::Shuffle2)
        {
        	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
			instructions.emplace_back(new SPIRVShuffle(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 5), getWord(parsed_instruction, 6), getWord(parsed_instruction, 7)));
			return SPV_SUCCESS;
        }
        //these instructions are not really handled -> throw error here (where we know the method-name)
        throw CompilationError(CompilationStep::PARSER, "OpenCL standard-function seems to be not implemented", getOpenCLMethodName(getWord(parsed_instruction, 4)));
    }
    case SpvOpMemoryModel:
        if (getWord(parsed_instruction, 1) != SpvAddressingModelLogical && getWord(parsed_instruction, 1) != SpvAddressingModelPhysical32)
            throw CompilationError(CompilationStep::PARSER, "Invalid addressing-mode");
        if (getWord(parsed_instruction, 2) != SpvMemoryModelSimple && getWord(parsed_instruction, 2) != SpvMemoryModelOpenCL)
            throw CompilationError(CompilationStep::PARSER, "Invalid memory model");
        logging::debug() << "Using a " << (getWord(parsed_instruction, 1) == SpvAddressingModelLogical ? "logical" : "physical")
                << " " << (getWord(parsed_instruction, 2) == SpvMemoryModelSimple ? "simple" : "OpenCL")
                << " memory model" << logging::endl;
        return SPV_SUCCESS;
    case SpvOpEntryPoint: //an entry point is an OpenCL kernel
    {
        if (getWord(parsed_instruction, 1) != SpvExecutionModelKernel)
            throw CompilationError(CompilationStep::PARSER, "Invalid execution model");
        SPIRVMethod& m = getOrCreateMethod(*module, methods, getWord(parsed_instruction, 2));
        m.method->isKernel = true;
        m.method->name = readLiteralString(parsed_instruction, &parsed_instruction->operands[2]);
        logging::debug() << "Kernel-method found: " << m.method->name << logging::endl;
        return SPV_SUCCESS;
    }
    case SpvOpExecutionMode:
        //only Kernel or native execution modes are supported
        if (getWord(parsed_instruction, 2) == SpvExecutionModeLocalSize)
            //"Indicates the work-group size in the x, y, and z dimensions"
            metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES] = {std::to_string(getWord(parsed_instruction, 3)), std::to_string(getWord(parsed_instruction, 4)), std::to_string(getWord(parsed_instruction, 5))};
        else if (getWord(parsed_instruction, 2) == SpvExecutionModeLocalSizeHint)
            //"A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z dimensions"
            metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES_HINT] = {std::to_string(getWord(parsed_instruction, 3)), std::to_string(getWord(parsed_instruction, 4)), std::to_string(getWord(parsed_instruction, 5))};
        else if (getWord(parsed_instruction, 2) != SpvExecutionModeVecTypeHint && getWord(parsed_instruction, 2) != SpvExecutionModeContractionOff)
            throw CompilationError(CompilationStep::PARSER, "Invalid execution mode");
        return SPV_SUCCESS;
    case SpvOpExecutionModeId:
    	//only Kernel or native execution modes are supported
		if (getWord(parsed_instruction, 2) == SpvExecutionModeLocalSizeId)
			//"Indicates the work-group size in the x, y, and z dimensions"
			metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES] = {std::to_string(constantMappings.at(getWord(parsed_instruction, 3)).literal.integer), std::to_string(constantMappings.at(getWord(parsed_instruction, 4)).literal.integer), std::to_string(constantMappings.at(getWord(parsed_instruction, 5)).literal.integer)};
		else if (getWord(parsed_instruction, 2) == SpvExecutionModeLocalSizeHintId)
			//"A hint to the compiler, which indicates the most likely to be used work-group size in the x, y, and z dimensions"
			metadataMappings[getWord(parsed_instruction, 1)][MetaDataType::WORK_GROUP_SIZES_HINT] = {std::to_string(constantMappings.at(getWord(parsed_instruction, 3)).literal.integer), std::to_string(constantMappings.at(getWord(parsed_instruction, 4)).literal.integer), std::to_string(constantMappings.at(getWord(parsed_instruction, 5)).literal.integer)};
		else
			throw CompilationError(CompilationStep::PARSER, "Invalid execution mode");
		return SPV_SUCCESS;
    case SpvOpCapability:
        return checkCapability(static_cast<SpvCapability> (getWord(parsed_instruction, 1)));
    case SpvOpTypeVoid:
        typeMappings[getWord(parsed_instruction, 1)] = TYPE_VOID;
        return SPV_SUCCESS;
    case SpvOpTypeBool:
        typeMappings[getWord(parsed_instruction, 1)] = TYPE_BOOL;
        return SPV_SUCCESS;
    case SpvOpTypeInt:
        typeMappings[getWord(parsed_instruction, 1)] = getIntegerType(getWord(parsed_instruction, 2), getWord(parsed_instruction, 3));
        return SPV_SUCCESS;
    case SpvOpTypeFloat:
    	if(getWord(parsed_instruction, 2) == 32)
    		typeMappings[getWord(parsed_instruction, 1)] = TYPE_FLOAT;
    	else if(getWord(parsed_instruction, 2) == 16)
    		typeMappings[getWord(parsed_instruction, 1)] = TYPE_HALF;
    	else
            throw CompilationError(CompilationStep::PARSER, "Unsupported floating-point type");
        return SPV_SUCCESS;
    case SpvOpTypeVector:
    {
        DataType type = typeMappings.at(getWord(parsed_instruction, 2));
        type.num = getWord(parsed_instruction, 3);
        typeMappings[getWord(parsed_instruction, 1)] = type;
        return SPV_SUCCESS;
    }
    case SpvOpTypeImage:
    {
        ImageType* image = new ImageType();
        //TODO if color-type is always unknown (is it? SPIR-V specification doesn't state that it has to be void for OpenCL), remove to avoid confusion/wrong conclusions
        image->colorType = typeMappings.at(getWord(parsed_instruction, 2));
        image->dimensions = static_cast<uint8_t>(getWord(parsed_instruction, 3)) + 1;
        //XXX depth?? (already contained the dimensions)
        image->isImageArray = getWord(parsed_instruction, 5);
        image->isImageBuffer = getWord(parsed_instruction, 7);
        image->isSampled = false;
        typeMappings[parsed_instruction->result_id] = DataType(image->getImageTypeName(), image->colorType.num);
        typeMappings.at(parsed_instruction->result_id).complexType.reset(image);
        return SPV_SUCCESS;
    }
    case SpvOpTypeSampler:
		typeMappings[getWord(parsed_instruction, 1)] = TYPE_SAMPLER;
		return SPV_SUCCESS;
    case SpvOpTypeSampledImage:
    {
    	const ImageType* image = typeMappings.at(getWord(parsed_instruction, 2)).getImageType();
    	ImageType* sampledImage = new ImageType();
    	sampledImage->colorType = image->colorType;
    	sampledImage->dimensions = image->dimensions;
    	sampledImage->isImageArray = image->isImageArray;
    	sampledImage->isImageBuffer = image->isImageBuffer;
    	sampledImage->isSampled = true;

        typeMappings[parsed_instruction->result_id] = typeMappings.at(getWord(parsed_instruction, 2));
        typeMappings.at(parsed_instruction->result_id).complexType.reset(sampledImage);
        return SPV_SUCCESS;
    }
    case SpvOpTypeArray:
    {
        const DataType elementType = typeMappings.at(getWord(parsed_instruction, 2));
        DataType arrayType((elementType.to_string() + "[") + std::to_string(constantMappings.at(getWord(parsed_instruction, 3)).literal.integer) + "]");
        arrayType.complexType.reset(new ArrayType(elementType, constantMappings.at(getWord(parsed_instruction, 3)).literal.integer));
        typeMappings[getWord(parsed_instruction, 1)] = arrayType;
        return SPV_SUCCESS;
    }
    case SpvOpTypeStruct:
    {
        StructType* structDef = new StructType({});
        DataType type;
        type.complexType.reset(structDef);
        if(names.find(parsed_instruction->result_id) != names.end())
            type.typeName = names.at(parsed_instruction->result_id);
        //add reference to this struct-type to type-mappings
        typeMappings[parsed_instruction->result_id] = type;
        const auto typeIDs = parseArguments(parsed_instruction, 2);
        for(const uint32_t typeID : typeIDs)
        {
            structDef->elementTypes.push_back(typeMappings.at(typeID));
        }
        //set "packed" decoration
        if(decorationMappings.find(parsed_instruction->result_id) != decorationMappings.end())
        {
            if(getDecoration(decorationMappings.at(parsed_instruction->result_id), SpvDecorationCPacked))
            {
                structDef->isPacked = true;
            }
        }
        return SPV_SUCCESS;
    }
    case SpvOpTypeOpaque:
        typeMappings[getWord(parsed_instruction, 1)] = TYPE_UNKNOWN;
        typeMappings.at(getWord(parsed_instruction, 1)).typeName = readLiteralString(parsed_instruction, &parsed_instruction->operands[1]);
        //Since there is no memory-layout to them, they can only be used as pointers
        return SPV_SUCCESS;
    case SpvOpTypePointer:
    {
        DataType type = typeMappings.at(getWord(parsed_instruction, 3));
        std::shared_ptr<ComplexType> elemType(new PointerType(type, toAddressSpace(static_cast<SpvStorageClass>(getWord(parsed_instruction, 2)))));
        type = DataType(type.to_string() + "*", 1, elemType);
        typeMappings[getWord(parsed_instruction, 1)] = type;
        return SPV_SUCCESS;
    }
    case SpvOpTypeFunction:
    	//"OpFunction is the only valid use of OpTypeFunction."
    	//-> so we can ignore this
        return SPV_SUCCESS;
    case SpvOpTypeEvent:
    	typeMappings[getWord(parsed_instruction, 1)] = TYPE_EVENT;
    	return SPV_SUCCESS;
    case SpvOpTypeDeviceEvent:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypeDeviceEvent");
    case SpvOpTypeReserveId:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypeReserveID");
    case SpvOpTypeQueue:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypeQueue");
    case SpvOpTypePipe:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpTypePipe");
    case SpvOpTypeForwardPointer:
    	//" Declare the Storage Class for a forward reference to a pointer."
    	// we are not interested in the storage-class -> skip
    	//TODO we actually are interested, currently at least to provide it as information for clGetKernelArgInfo
    	//but so far, this instruction was never encountered
        return UNSUPPORTED_INSTRUCTION("SpvOpTypeForwardPointer");
    case SpvOpConstantTrue:
        constantMappings.emplace(parsed_instruction->result_id, BOOL_TRUE);
        return SPV_SUCCESS;
    case SpvOpConstantFalse:
        constantMappings.emplace(parsed_instruction->result_id, BOOL_FALSE);
        return SPV_SUCCESS;
    case SpvOpConstant: //integer or floating point scalar constant
        constantMappings.emplace(parsed_instruction->result_id, parseConstant(parsed_instruction, typeMappings));
        return SPV_SUCCESS;
    case SpvOpConstantComposite: //constant of array, matrix or vector-type
        constantMappings.emplace(parsed_instruction->result_id, parseConstantComposite(parsed_instruction, typeMappings, constantMappings));
        return SPV_SUCCESS;
    case SpvOpConstantSampler: //convert to 32-bit integer constant
    {
        Value sampler(typeMappings.at(parsed_instruction->type_id));
        sampler.valueType = ValueType::LITERAL;
        sampler.literal = Literal(static_cast<int64_t>(parseSampler(parsed_instruction)));
        constantMappings.emplace(parsed_instruction->result_id, sampler);
        return SPV_SUCCESS;
    }
    case SpvOpConstantNull:
    {
    	//"The null value is type dependent, defined as follows:
    	// - Scalar Boolean: false
    	// - Scalar integer: 0
    	// - Scalar floating point: +0.0 (all bits 0)
    	// - All other scalars: Abstract
    	// - Composites: Members are set recursively to the null constant according to the null value of their constituent types."
    	const DataType& type = typeMappings.at(parsed_instruction->type_id);
    	if(type.isScalarType() || type.isVectorType() || type.isPointerType())
    		constantMappings.emplace(parsed_instruction->result_id, Value(INT_ZERO.literal, type));
    	else if(type.getArrayType().hasValue)
    		constantMappings.emplace(parsed_instruction->result_id, INT_ZERO);
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
    case SpvOpSpecConstantTrue:
    {
    	//"[...] Similarly, the "True" and "False" parts of OpSpecConstantTrue and OpSpecConstantFalse provide the default Boolean specialization constants."
    	const Optional<Value> spec = specializeConstant(parsed_instruction->result_id, TYPE_BOOL, decorationMappings);
		constantMappings.emplace(parsed_instruction->result_id, spec ? spec.get() : BOOL_TRUE);
    	return SPV_SUCCESS;
    }
    case SpvOpSpecConstantFalse:
    {
    	//"[...] Similarly, the "True" and "False" parts of OpSpecConstantTrue and OpSpecConstantFalse provide the default Boolean specialization constants."
    	const Optional<Value> spec = specializeConstant(parsed_instruction->result_id, TYPE_BOOL, decorationMappings);
		constantMappings.emplace(parsed_instruction->result_id, spec ? spec.get() : BOOL_FALSE);
		return SPV_SUCCESS;
    }
    case SpvOpSpecConstant:
    {
    	//"The literal operands to OpSpecConstant are the default numerical specialization constants."
    	const Value constant = parseConstant(parsed_instruction, typeMappings);
    	const Optional<Value> spec = specializeConstant(parsed_instruction->result_id, constant.type, decorationMappings);
    	constantMappings.emplace(parsed_instruction->result_id, spec ? spec.get() : constant);
		return SPV_SUCCESS;
    }
    case SpvOpSpecConstantComposite:
    {
    	const Value constant = parseConstantComposite(parsed_instruction, typeMappings, constantMappings);
    	const Optional<Value> spec = specializeConstant(parsed_instruction->result_id, constant.type, decorationMappings);
    	constantMappings.emplace(parsed_instruction->result_id, spec ? spec.get() : constant);
		return SPV_SUCCESS;
    }
    case SpvOpSpecConstantOp:
    {
    	//"The OpSpecConstantOp instruction is specialized by executing the operation and replacing the instruction with the result."
    	const DataType type = typeMappings.at(parsed_instruction->type_id);
    	const Optional<Value> spec = specializeConstant(parsed_instruction->result_id, type, decorationMappings);
    	if(spec)
    	{
    		constantMappings.emplace(parsed_instruction->result_id, spec.get());
    		return SPV_SUCCESS;
    	}
    	else
    	{
			const auto result = calculateConstantOperation(parsed_instruction);
			if(result.first == SPV_SUCCESS && result.second)
				constantMappings.emplace(parsed_instruction->result_id, result.second);
			else if(result.first == SPV_SUCCESS)
				//can't fall back to inserting the instruction, since there is no method associated with it!
				throw CompilationError(CompilationStep::PARSER, "Failed to pre-calculate specialization value!");
			return result.first;
    	}
    }
    case SpvOpFunction: //new current method -> add to list of all methods
    {
        currentMethod = &getOrCreateMethod(*module, methods, parsed_instruction->result_id);
        currentMethod->method->returnType = typeMappings.at(parsed_instruction->type_id);
        //add label %0 to the beginning of the method
        //XXX maybe this is not necessary? If so, it is removed anyway
        typeMappings[0] = TYPE_LABEL;
        instructions.emplace_back(new SPIRVLabel(0, *currentMethod));
        logging::debug() << "Reading function: %" << parsed_instruction->result_id << " (" << (names.find(parsed_instruction->result_id) != names.end() ? names.at(parsed_instruction->result_id) : currentMethod->method->name) << ")" << logging::endl;
        return SPV_SUCCESS;
    }
    case SpvOpFunctionParameter:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        currentMethod->parameters.push_back(std::make_pair(parsed_instruction->result_id, parsed_instruction->type_id));
        logging::debug() << "Reading parameter: " << typeMappings.at(parsed_instruction->type_id).to_string() << " %" << parsed_instruction->result_id << logging::endl;
        return SPV_SUCCESS;
    case SpvOpFunctionEnd:
        currentMethod = nullptr;
        return SPV_SUCCESS;
    case SpvOpFunctionCall:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, getWord(parsed_instruction, 3), parsed_instruction->type_id, parseArguments(parsed_instruction, 4)));
        return SPV_SUCCESS;
    case SpvOpVariable:
    {
    	//"Allocate an object in memory, resulting in a pointer to it"
    	 //used for global values as well as stack-allocations
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        const std::string name = names.find(parsed_instruction->result_id) != names.end() ? names.at(parsed_instruction->result_id) : (std::string("%") + std::to_string(parsed_instruction->result_id));
        const DataType& type = typeMappings.at(parsed_instruction->type_id);
        Value val(type.getElementType());
        if(parsed_instruction->num_words > 4)
        	val = constantMappings.at(getWord(parsed_instruction, 4));
        unsigned alignment = 0;
        if(decorationMappings.find(parsed_instruction->type_id) != decorationMappings.end())
        	alignment = getDecoration(decorationMappings.at(parsed_instruction->type_id), SpvDecorationAlignment).orElse(0);
        if(alignment == 0 && decorationMappings.find(parsed_instruction->result_id) != decorationMappings.end())
        	alignment = getDecoration(decorationMappings.at(parsed_instruction->result_id), SpvDecorationAlignment).orElse(0);

        //the type of OpVariable is the pointer
        //... but the global data/stack allocation needs to have the real type (is re-set in #parse())
        if(currentMethod != nullptr && AddressSpace::PRIVATE == toAddressSpace(static_cast<SpvStorageClass>(getWord(parsed_instruction, 3))))
        {
        	//OpVariables within a function body are stack allocations
        	//"All OpVariable instructions in a function must have a Storage Class of Function."
        	currentMethod->method->stackAllocations.push_back(StackAllocation(name, type, 0, alignment == 0 ? 1 : 0));
        	//TODO set initial value!. Allowed for OpVariables with storage-class Function?
        	memoryAllocatedData.emplace(parsed_instruction->result_id, &currentMethod->method->stackAllocations.back());
        }
        else
        {
        	//OpVariables outside of any function are global data
        	module->globalData.emplace_back(Global(name, type, val));
        	module->globalData.back().type.getPointerType().get()->alignment = alignment;
			memoryAllocatedData.emplace(parsed_instruction->result_id, &module->globalData.back());
        }
        logging::debug() << "Reading variable: " << type.to_string() << " " << name << " with value: " << val.to_string(false, true) << logging::endl;
        return SPV_SUCCESS;
    }
    case SpvOpImageTexelPointer:
    	//"Form a pointer to a texel of an image. Use of such a pointer is limited to atomic operations."
    	return UNSUPPORTED_INSTRUCTION("OpImageTexelPointer");
    case SpvOpLoad:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), MemoryAccess::READ));
        return SPV_SUCCESS;
    case SpvOpStore:
        instructions.emplace_back(new SPIRVCopy(getWord(parsed_instruction, 1), *currentMethod, UNDEFINED_ID, getWord(parsed_instruction, 2), MemoryAccess::WRITE));
        return SPV_SUCCESS;
    case SpvOpCopyMemory:
        instructions.emplace_back(new SPIRVCopy(getWord(parsed_instruction, 1), *currentMethod, UNDEFINED_ID, getWord(parsed_instruction, 2), MemoryAccess::READ_WRITE));
        return SPV_SUCCESS;
    case SpvOpCopyMemorySized:
    	instructions.emplace_back(new SPIRVCopy(getWord(parsed_instruction, 1), *currentMethod, UNDEFINED_ID, getWord(parsed_instruction, 2), MemoryAccess::READ_WRITE, getWord(parsed_instruction, 3)));
		return SPV_SUCCESS;
    case SpvOpAccessChain: //pointer into element(s) of composite
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), false));
        return SPV_SUCCESS;
    case SpvOpInBoundsAccessChain:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), false));
        return SPV_SUCCESS;
    case SpvOpPtrAccessChain:
    	//For pointers, the "Element" field is the first (top-level) index (see SPIR-V specification, OpPtrAccessChain):
		//"Element is used to do the initial dereference of Base: Base is treated as the address of the first element of an array, and the Element element’s address is computed to be the base for the Indexes [...]"
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), true));
        return SPV_SUCCESS;
    case SpvOpGenericPtrMemSemantics:
    	//"Result is a valid Memory Semantics which includes mask bits set for the Storage Class for the specific (non-Generic) Storage Class of Pointer. "
    	// not used -> ignore
        return SPV_SUCCESS;
    case SpvOpInBoundsPtrAccessChain:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        //FIXME according to sanitizer, currentMethod seems to be null sometimes (/opt/SPIRV-LLVM/tools/clang/test/SemaOpenCL/str_literals.cl)
        instructions.emplace_back(new SPIRVIndexOf(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 4), true));
        return SPV_SUCCESS;
    case SpvOpNoLine: //source level debug info -> skip
        return SPV_SUCCESS;
    case SpvOpModuleProcessed:
    	//"Document a process that was applied to a module. This has no semantic impact and can safely be removed from a module."
    	return SPV_SUCCESS;
    case SpvOpDecorate:
        //"Target is the <id> to decorate. It can potentially be any <id> that is a forward reference"
        // -> decorations are always forward references
        // -> can be applied to instruction/parameter/type on parsing it
        return parseDecoration(parsed_instruction);
    case SpvOpDecorateId:
    	//"Target is the <id> to decorate. It can potentially be any <id> that is a forward reference"
    	// -> decorations are always forward references
		// -> can be applied to instruction/parameter/type on parsing it
    	//In this version, the decoration operands are not literals, but specified by their IDs
    	return parseDecoration(parsed_instruction);
    case SpvOpMemberDecorate:
        return UNSUPPORTED_INSTRUCTION("OpMemberDecorate");
    case SpvOpDecorationGroup:
        //"A collector for Decorations from OpDecorate instructions. All such OpDecorate instructions targeting this OpDecorationGroup instruction must precede it."
        //"Subsequent OpGroupDecorate and OpGroupMemberDecorate instructions that consume this instruction�s Result <id> will apply these decorations to their targets."
        //nothing needs to be done, since decorations are added up independent of target and are applied with "OpGroupDecorate"
        return SPV_SUCCESS;
    case SpvOpGroupDecorate:
    {
        //apply group of decorations to IDs
        const std::vector<uint32_t> targets = parseArguments(parsed_instruction, 2);
        const uint32_t decorationGroup = getWord(parsed_instruction, 1);
        if(decorationMappings.find(decorationGroup) != decorationMappings.end())
        {
            const std::vector<Decoration>& decorations = decorationMappings.at(decorationGroup);
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
    case SpvOpGroupMemberDecorate:
        return UNSUPPORTED_INSTRUCTION("OpGroupMemberDecorate");
    case SpvOpVectorExtractDynamic:
        return UNSUPPORTED_INSTRUCTION("OpVectorExtractDynamic");
    case SpvOpVectorInsertDynamic:
        return UNSUPPORTED_INSTRUCTION("OpVectorInsertDynamic");
    case SpvOpVectorShuffle:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVShuffle(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), getWord(parsed_instruction, 4), parseArguments(parsed_instruction, 5)));
        return SPV_SUCCESS;
    case SpvOpCompositeConstruct:
        return UNSUPPORTED_INSTRUCTION("OpCompositeConstruct");
    case SpvOpCompositeExtract:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), {0}, parseArguments(parsed_instruction, 4)));
        return SPV_SUCCESS;
    case SpvOpCompositeInsert:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        //1. copy whole composite
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 4)));
        //2. insert object at given index
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), parseArguments(parsed_instruction, 5), {0}));
        return SPV_SUCCESS;
    case SpvOpCopyObject:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpSampledImage:
    	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
    	//this is not really an instruction for the runtime, but to associate images <-> sampled-image
    	sampledImages[parsed_instruction->result_id] = {getWord(parsed_instruction, 3), getWord(parsed_instruction, 4)};
    	return SPV_SUCCESS;
    case SpvOpImageSampleExplicitLod:
    {
    	//"Sample an image using an explicit level of detail."
    	//is handled via intrinsics
		return UNSUPPORTED_INSTRUCTION("OpImageSampleExplicitLod");
    }
    case SpvOpImageFetch:
    {
        //"Fetch a single texel from a sampled image."
    	//is handled via intrinsics
    	return UNSUPPORTED_INSTRUCTION("OpImageFetch");
    }
    case SpvOpImageRead:
    {
        //"Read a texel from an image without a sampler."
    	//is handled via intrinsics
		return UNSUPPORTED_INSTRUCTION("OpImageRead");
    }
    case SpvOpImageWrite:
    {
        //"Write a texel to an image without a sampler."
    	//is handled via intrinsics
		return UNSUPPORTED_INSTRUCTION("OpImageWrite");
    }
    case SpvOpImage:
        //"Extract the image from a sampled image."
    	//this is not really an instruction for the runtime, but to associate images <-> sampled-image
    	//XXX type-association correct??
    	localTypes[parsed_instruction->result_id] = localTypes.at(getWord(parsed_instruction, 3));
    	//this is not quite correct (to store a sampled-image where an image is supposed to go), but we extract the image in the image-accessing methods
    	sampledImages[parsed_instruction->result_id] = sampledImages.at(getWord(parsed_instruction, 3));
    	return SPV_SUCCESS;
    case SpvOpImageQueryFormat:
        //"Query the image format of an image [...]."
        //"The resulting value is an enumerant from Image Channel Data Type."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, ImageQuery::CHANNEL_DATA_TYPE, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpImageQueryOrder:
        //"Query the channel order of an image [...]."
        //"The resulting value is an enumerant from Image Channel Order."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, ImageQuery::CHANNEL_ORDER, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpImageQuerySizeLod:
        //"Query the dimensions of Image for mipmap level for Level of Detail."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, ImageQuery::SIZES_LOD, getWord(parsed_instruction, 3), getWord(parsed_instruction, 4)));
        return SPV_SUCCESS;
    case SpvOpImageQuerySize:
        //"Query the dimensions of Image, with no level of detail."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, ImageQuery::SIZES, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpImageQueryLevels:
        //"Query the number of mipmap levels accessible through Image."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, ImageQuery::MIPMAP_LEVELS, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpImageQuerySamples:
        //"Query the number of samples available per texel fetch in a multisample image."
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVImageQuery(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, ImageQuery::SAMPLES_PER_TEXEL, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpConvertFToU:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fptoui", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpConvertFToS:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fptosi", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpConvertSToF:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "sitofp", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpConvertUToF:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "uitofp", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpUConvert: //change bit-width (type) of value
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSConvert: //change bit-width (type) of value
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::SIGNED, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFConvert: //change bit-width (type) of value
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::FLOATING, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpConvertPtrToU: //pointer to unsigned -> same as OpUConvert
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSatConvertSToU: //signed to unsigned (with saturation)
    	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
		instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED, toInstructionDecorations(parsed_instruction->result_id), true));
		return SPV_SUCCESS;
    case SpvOpSatConvertUToS: //unsigned to signed (with saturation)
    	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
		instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::SIGNED, toInstructionDecorations(parsed_instruction->result_id), true));
		return SPV_SUCCESS;
    case SpvOpConvertUToPtr: //unsigned to pointer -> same as OpUConvert
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::UNSIGNED, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpPtrCastToGeneric:
    	//"Convert a pointer’s Storage Class to Generic."
    	// -> simply copy the pointer
    	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
    	instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpGenericCastToPtr:
    	//"Convert a pointer’s Storage Class to a non-Generic class."
    	// -> simple copy the pointer
    	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
		instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3)));
		return SPV_SUCCESS;
    case SpvOpGenericCastToPtrExplicit:
    	//"Attempts to explicitly convert Pointer to Storage storage-class pointer value."
    	// -> simple copy the pointer
    	localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
		instructions.emplace_back(new SPIRVCopy(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3)));
		return SPV_SUCCESS;
    case SpvOpBitcast:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVConversion(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), ConversionType::BITCAST, intermediate::InstructionDecorations::NONE));
        return SPV_SUCCESS;
    case SpvOpSNegate:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, OP_NEGATE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFNegate:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, OP_NEGATE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpIAdd:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "add", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFAdd:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fadd", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpISub:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "sub", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFSub:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fsub", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpIMul:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "mul", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFMul:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fmul", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpUDiv:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "udiv", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSDiv:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "sdiv", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFDiv:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fdiv", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpUMod:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "umod", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSRem:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "srem", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSMod:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "smod", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFRem:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "frem", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFMod:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fmod", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpVectorTimesScalar: //type must be floating point
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "fmul", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpDot:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "dot", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpIAddCarry:
        return UNSUPPORTED_INSTRUCTION("OpIAddCarry");
    case SpvOpISubBorrow:
        return UNSUPPORTED_INSTRUCTION("OpISubBorrow");
    case SpvOpUMulExtended:
        return UNSUPPORTED_INSTRUCTION("OpUMulExtended");
    case SpvOpSMulExtended:
        return UNSUPPORTED_INSTRUCTION("OpSMulExtended");
    case SpvOpAny:
        return UNSUPPORTED_INSTRUCTION("OpAny");
    case SpvOpAll:
        return UNSUPPORTED_INSTRUCTION("OpAll");
    case SpvOpIsNan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "isnan", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpIsInf:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "ifinf", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpIsFinite:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "isfinite", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpIsNormal:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "isnormal", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpSignBitSet:
        return UNSUPPORTED_INSTRUCTION("OpSignBitSet");
    case SpvOpLessOrGreater:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "islessgreater", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpOrdered:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpUnordered:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpLogicalEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpLogicalNotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpLogicalOr:
    	//"Result Type must be a scalar or vector of Boolean type."
    	// -> same as bitwise OR
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "or", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpLogicalAnd:
    	//"Result Type must be a scalar or vector of Boolean type."
		// -> same as bitwise AND
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "and", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpLogicalNot:
    	//"Result Type must be a scalar or vector of Boolean type."
    	// -> same as bitwise NOT
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "not", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSelect:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVSelect(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, getWord(parsed_instruction, 3), getWord(parsed_instruction, 4), getWord(parsed_instruction, 5)));
        return SPV_SUCCESS;
    case SpvOpIEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpINotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpUGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNSIGNED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_SIGNED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpUGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNSIGNED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_SIGNED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpULessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNSIGNED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSLessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_SIGNED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpULessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNSIGNED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpSLessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_SIGNED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFOrdEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFUnordEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED_EQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFOrdNotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFUnordNotEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED_NEQ, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFOrdLessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFUnordLessThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED_LT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFOrdGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFUnordGreaterThan:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED_GT, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFOrdLessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFUnordLessThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED_LE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFOrdGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_ORDERED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpFUnordGreaterThanEqual:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVComparison(parsed_instruction->result_id, *currentMethod, intermediate::COMP_UNORDERED_GE, parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpShiftRightLogical:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "shr", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpShiftRightArithmetic:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "asr", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpShiftLeftLogical:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "shl", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpBitwiseOr:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "or", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpBitwiseXor:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "xor", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpBitwiseAnd:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "and", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpNot:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVInstruction(parsed_instruction->result_id, *currentMethod, "not", parsed_instruction->type_id, parseArguments(parsed_instruction, 3), toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    case SpvOpBitCount:
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVCallSite(parsed_instruction->result_id, *currentMethod, "popcount", parsed_instruction->type_id, parseArguments(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpControlBarrier:
    	//"barrier()" is handled via header-function
        return UNSUPPORTED_INSTRUCTION("OpControlBarrier");
    case SpvOpMemoryBarrier:
    	instructions.emplace_back(new SPIRVMemoryBarrier(*currentMethod, getWord(parsed_instruction, 1), getWord(parsed_instruction, 2)));
        return SPV_SUCCESS;
    case SpvOpAtomicLoad:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicLoad");
    case SpvOpAtomicStore:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicStore");
    case SpvOpAtomicExchange:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicExchange");
    case SpvOpAtomicCompareExchange:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicCompareExchange");
    case SpvOpAtomicCompareExchangeWeak:
    	//OpenCL 2.x feature
        return UNSUPPORTED_INSTRUCTION("OpAtomicCompareExchangeWeak");
    case SpvOpAtomicIIncrement:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicIIncrement");
    case SpvOpAtomicIDecrement:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicIDecrement");
    case SpvOpAtomicIAdd:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicIAdd");
    case SpvOpAtomicISub:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicISub");
    case SpvOpAtomicSMin:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicSMin");
    case SpvOpAtomicUMin:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicUMin");
    case SpvOpAtomicSMax:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicSMax");
    case SpvOpAtomicUMax:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicUMax");
    case SpvOpAtomicAnd:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicAnd");
    case SpvOpAtomicOr:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicOr");
    case SpvOpAtomicXor:
    	//handled via intrinsics
        return UNSUPPORTED_INSTRUCTION("OpAtomicXor");
    case SpvOpPhi:
    {
        const std::vector<uint32_t> args = parseArguments(parsed_instruction, 3);
        std::vector<std::pair<uint32_t, uint32_t>> sources;
        sources.reserve(args.size() / 2);
        for (std::size_t i = 0; i < args.size(); i += 2) {
            sources.emplace_back(args[i], args[i + 1]);
        }
        localTypes[parsed_instruction->result_id] = parsed_instruction->type_id;
        instructions.emplace_back(new SPIRVPhi(parsed_instruction->result_id, *currentMethod, parsed_instruction->type_id, sources));
        return SPV_SUCCESS;
    }
    case SpvOpLoopMerge:
        return UNSUPPORTED_INSTRUCTION("OpLoopMerge");
    case SpvOpSelectionMerge:
        return UNSUPPORTED_INSTRUCTION("OpSelectionMerge");
    case SpvOpLabel:
        typeMappings[parsed_instruction->result_id] = TYPE_LABEL;
        instructions.emplace_back(new SPIRVLabel(parsed_instruction->result_id, *currentMethod));
        return SPV_SUCCESS;
    case SpvOpBranch:
        instructions.emplace_back(new SPIRVBranch(*currentMethod, getWord(parsed_instruction, 1)));
        return SPV_SUCCESS;
    case SpvOpBranchConditional:
        instructions.emplace_back(new SPIRVBranch(*currentMethod, getWord(parsed_instruction, 1), getWord(parsed_instruction, 2), getWord(parsed_instruction, 3)));
        return SPV_SUCCESS;
    case SpvOpSwitch:
    {
        const std::vector<uint32_t> args = parseArguments(parsed_instruction, 3);
        std::vector<std::pair<uint32_t, uint32_t>> destinations;
        destinations.reserve(args.size() / 2);
        for (std::size_t i = 0; i < args.size(); i += 2) {
            destinations.emplace_back(args[i], args[i + 1]);
        }
        instructions.emplace_back(new SPIRVSwitch(parsed_instruction->result_id, *currentMethod, getWord(parsed_instruction, 1), getWord(parsed_instruction, 2), destinations));
        return SPV_SUCCESS;
    }
    case SpvOpReturn:
        instructions.emplace_back(new SPIRVReturn(*currentMethod));
        return SPV_SUCCESS;
    case SpvOpReturnValue:
        instructions.emplace_back(new SPIRVReturn(getWord(parsed_instruction, 1), *currentMethod));
        return SPV_SUCCESS;
    case SpvOpUnreachable:
        return SPV_SUCCESS;
    case SpvOpLifetimeStart:
    {
    	//for temporary variables (e.g. Function-Scope), the size is set via the OpLifetimeStart, since the OpVariable is of type void
    	instructions.emplace_back(new SPIRVLifetimeInstruction(getWord(parsed_instruction, 1), *currentMethod, getWord(parsed_instruction, 2), false, toInstructionDecorations(parsed_instruction->result_id)));
        return SPV_SUCCESS;
    }
    case SpvOpLifetimeStop:
    	instructions.emplace_back(new SPIRVLifetimeInstruction(getWord(parsed_instruction, 1), *currentMethod, getWord(parsed_instruction, 2), true, toInstructionDecorations(parsed_instruction->result_id)));
		return SPV_SUCCESS;
    case SpvOpGroupAsyncCopy:
        return UNSUPPORTED_INSTRUCTION("OpGroupAsyncCopy");
    case SpvOpGroupWaitEvents:
        return UNSUPPORTED_INSTRUCTION("OpGroupWaitEvents");
    case SpvOpAtomicFlagTestAndSet:
    	//OpenCL 2.x feature
    	return UNSUPPORTED_INSTRUCTION("OpAtomicFlagTestAndSet");
    case SpvOpAtomicFlagClear:
    	//OpenCL 2.x feature
    	return UNSUPPORTED_INSTRUCTION("OpAtomicFlagClear");
    case SpvOpSizeOf:
    	return UNSUPPORTED_INSTRUCTION("OpSizeOf");
    default:
        //prevents warnings
        break;
    }

    //unhandled op-code
    logging::warn() << "Unhandled instruction-type: " << static_cast<SpvOp>(parsed_instruction->opcode) << logging::endl;
    return SPV_UNSUPPORTED;
}

std::pair<spv_result_t, Optional<Value>> SPIRVParser::calculateConstantOperation(const spv_parsed_instruction_t* instruction)
{
	/*
	 * "Opcode must be one of the following opcodes:
	 * OpSConvert, OpFConvert, OpSNegate, OpNot, OpIAdd, OpISub, OpIMul, OpUDiv, OpSDiv, OpUMod, OpSRem, OpSMod,
	 * OpShiftRightLogical, OpShiftRightArithmetic, OpShiftLeftLogical, OpBitwiseOr, OpBitwiseXor, OpBitwiseAnd,
	 * OpVectorShuffle, OpCompositeExtract, OpCompositeInsert, OpLogicalOr, OpLogicalAnd, OpLogicalNot,
	 * OpLogicalEqual, OpLogicalNotEqual, OpSelect, OpIEqual, OpINotEqual, OpULessThan, OpSLessThan, OpUGreaterThan, OpSGreaterThan,
	 * OpULessThanEqual, OpSLessThanEqual, OpUGreaterThanEqual, OpSGreaterThanEqual"
	 *
	 * "If the Kernel capability was declared, the following opcodes are also valid:
	 * OpConvertFToS, OpConvertSToF, OpConvertFToU, OpConvertUToF, OpUConvert, OpConvertPtrToU, OpConvertUToPtr, OpGenericCastToPtr, OpPtrCastToGeneric,
	 * OpBitcast, OpFNegate, OpFAdd, OpFSub, OpFMul, OpFDiv, OpFRem, OpFMod, OpAccessChain, OpInBoundsAccessChain, OpPtrAccessChain, OpInBoundsPtrAccessChain
	 */
	SPIRVMethod* methodBackup = currentMethod;
	std::vector<std::unique_ptr<SPIRVOperation>> instructionsBackup;
	instructions.swap(instructionsBackup);
	currentMethod = nullptr;
	uint32_t dummyWords[12] = {};
	spv_parsed_instruction_t dummyInstruction;

	dummyInstruction.words = dummyWords;
	dummyInstruction.num_words = instruction->num_words - 1;
	dummyInstruction.ext_inst_type = SPV_EXT_INST_TYPE_NONE;
	dummyInstruction.num_operands = 0;
	dummyInstruction.opcode = getWord(instruction, 3);
	dummyInstruction.operands = NULL;
	dummyInstruction.result_id = instruction->result_id;
	dummyInstruction.type_id = instruction->type_id;

	//only skip length/op-code. The other words are required, so the number/position of words fit
	memcpy(dummyWords, instruction->words +1, (instruction->num_words - 1) * sizeof(uint32_t));
	dummyWords[0] = dummyInstruction.opcode;
	dummyWords[1] = dummyInstruction.type_id;
	dummyWords[2] = dummyInstruction.result_id;

	spv_result_t result = parseInstruction(&dummyInstruction);
	if(result != SPV_SUCCESS || instructions.empty())
	{
		return std::make_pair(result, NO_VALUE);
	}

	const Optional<Value> value = instructions.at(0)->precalculate(typeMappings, constantMappings, memoryAllocatedData);

	//swap back
	currentMethod = methodBackup;
	instructions.swap(instructionsBackup);

	return std::make_pair(SPV_SUCCESS, value);
}

#endif
