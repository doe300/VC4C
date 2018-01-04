/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IRParser.h"

#include "../intermediate/IntermediateInstruction.h"
#include "Token.h"
#include "log.h"

#include <algorithm>
#include <cstdbool>
#include <cstring>
#include <regex>
#include <vector>

using namespace vc4c;
using namespace vc4c::llvm2qasm;

IRParser::IRParser(Scanner& scanner) : scanner(scanner), module(nullptr), currentMethod(nullptr)
{

}

IRParser::IRParser(std::istream& stream) : scanner(stream), module(nullptr), currentMethod(nullptr)
{

}

std::string cleanMethodName(const std::string& name)
{
    //truncate prefix and postfix added by LLVM
    std::string tmp = std::regex_replace(name, std::regex("@(_Z\\d+)?"), std::string(""));
    return std::regex_replace(tmp, std::regex("Dv\\d+_"), std::string(""));
}

static bool skipToken(Scanner& scanner, const std::string& token)
{
	if(scanner.peek().hasValue(token))
	{
		scanner.pop();
		return true;
	}
	return false;
}

static bool skipToken(Scanner& scanner, const char token)
{
	if(scanner.peek().hasValue(token))
	{
		scanner.pop();
		return true;
	}
	return false;
}

static void expectSkipToken(Scanner& scanner, const std::string& token)
{
	if(scanner.peek().hasValue(token))
	{
		scanner.pop();
	}
	else
	{
		std::stringstream s;
		s << "Expected '" << token << "', got '" << scanner.peek().to_string() << "'";
		throw CompilationError(CompilationStep::PARSER, "Expected token not found", s.str());
	}
}

static void expectSkipToken(Scanner& scanner, const char token)
{
	if(scanner.peek().hasValue(token))
	{
		scanner.pop();
	}
	else
	{
		std::stringstream s;
		s << "Expected '" << token << "', got '" << scanner.peek().to_string() << "'";
		throw CompilationError(CompilationStep::PARSER, "Expected token not found", s.str());
	}
}

Value IRParser::toValue(const Token& token, const DataType& type)
{
    Value val(Literal(false), type);
    val.valueType = toValueType(token.type);
    if (token.type == TokenType::STRING)
    {
    	if(token.hasValue("undef"))
    		val.valueType = ValueType::UNDEFINED;
    	else if(token.hasValue("zeroinitializer"))
    		return Value::createZeroInitializer(type);
    	else if(token.hasValue("null"))
    		//"The identifier ‘null‘ is recognized as a null pointer constant and must be of pointer type."
    		return Value::createZeroInitializer(type);
    	else if(currentMethod != nullptr)
    		//FIXME somehow, parameters are not found, but they are there (at least at a later point!)
    		val.local = const_cast<Local*>(currentMethod->findOrCreateLocal(type, token.getText().value()));
    	else
    	{
    		module->globalData.emplace_back(Global(token.getText().value(), type.toPointerType(), type));
    		val.local = &module->globalData.back();
    	}
    }
    else if (token.type == TokenType::BOOLEAN) {
        val.literal = Literal(token.flag);
    }
    else if (token.type == TokenType::NUMBER) {
        if(type.isFloatingType())
            val.literal = Literal(token.real);
        else
            val.literal = Literal(static_cast<int64_t>(token.integer));
    }
    else
        throw CompilationError(CompilationStep::PARSER, "Unhandled type", token.to_string());

    return val;
}

void IRParser::parse(Module& module)
{
	this->module = &module;
    const std::string declarationKeyword = "declare";
    const std::string methodKeyword = "define";
    while (scanner.hasInput()) {
        Token nextToken{};
        do {
            nextToken = scanner.peek();
            if (nextToken.hasValue('%')) {
                complexTypes.emplace(nextToken.getText().value(), parseStructDefinition());
            }
            if (nextToken.hasValue('@')) {
                //read global
                parseGlobalData();
            }
            else if (nextToken.hasValue(declarationKeyword)) {
                scanner.pop();
                scanner.readLine();
            }
            else if (nextToken.hasValue('!'))
                parseMetaData();
            else
                scanner.pop();
        }
        while (scanner.hasInput() && !nextToken.hasValue(methodKeyword));
        //struct definitions allow forward references, so we need to re-resolve them here
        for(auto& pair : complexTypes)
        {
        	if(pair.second.getStructType())
        	{
        		for(DataType& childType : pair.second.getStructType().value()->elementTypes)
        		{
        			if(childType.complexType == nullptr && complexTypes.find(childType.typeName) != complexTypes.end())
        			{
        				childType = complexTypes.at(childType.typeName);
        			}
        		}
        	}
        }
        if (!scanner.hasInput()) {
            break;
        }
        parseMethod();
    }
    //map meta-data to kernels
    extractKernelInfo();

    module.methods.reserve(methods.size());
    for(LLVMMethod& method: methods)
    	module.methods.emplace_back(method.method.release());
}

static ParameterDecorations parseParameterDecorations(Scanner& scanner)
{
    ParameterDecorations decorations = ParameterDecorations::NONE;
    while(scanner.peek().hasValue("readonly") || scanner.peek().hasValue("zeroext") || scanner.peek().hasValue("signext"))
    {
        Token nextToken = scanner.pop();
        if (nextToken.hasValue("readonly"))
        	decorations = add_flag(decorations, ParameterDecorations::READ_ONLY);
        else if (nextToken.hasValue("zeroext"))
        	decorations = add_flag(decorations, ParameterDecorations::ZERO_EXTEND);
        else if (nextToken.hasValue("signext"))
        	decorations = add_flag(decorations, ParameterDecorations::SIGN_EXTEND);
    }
    return decorations;
}

AddressSpace toAddressSpace(int num)
{
	//XXX this mapping is not guaranteed, mapping only determined by experiment
	switch(num)
	{
		case 0:
			//According to the documentation for 'alloca', "The object is always allocated in the generic address space (address space zero)"
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

static Optional<AddressSpace> readAddressSpace(Scanner& scanner)
{
	int64_t addressSpace = -1;
	if(skipToken(scanner, "addrspace"))
	{
		//pop '('
		scanner.pop();
		//pop address space
		addressSpace = scanner.pop().integer;
		//pop ')'
		scanner.pop();
	}
	if(addressSpace < 0)
		return Optional<AddressSpace>(false);
	return toAddressSpace(static_cast<int>(addressSpace));
}

DataType IRParser::parseType()
{
    //support for singular types (i64, float8, ...)
    //as well as vector types (<2 x float>, <4 x int8>)
    //and arrays ([3 x <4 x float>], [5 * i32])
    unsigned num = 1;
    bool isArray = false;
    bool isVector = false;
    //'exact <type>'
    skipToken(scanner, "exact");
    isArray = skipToken(scanner, '[');
    isVector = skipToken(scanner, '<');
    DataType childType = TYPE_UNKNOWN;
    std::string typeName;
    if (isArray || isVector)
    {
        num = static_cast<unsigned>(scanner.pop().integer);
        //pop 'x'
        expectSkipToken(scanner, "x");

        childType = parseType();

		//pop '>'/']'
		scanner.pop();
    }
    else
    {
		if (skipToken(scanner, "zeroext")) {
			//for method return types, this comes before the type
			//XXX could be used as flag ??
		}
		typeName = scanner.pop().getText().value();
		//for any other type (parameter), "zeroext" comes afterwards, but is handled outside of this function
    }

    std::size_t numPointerTypes = 0;
    Optional<AddressSpace> addressSpace;
    while(true)
    {
		//if the pointed to type is a pointer with an address space, the pointer can have multiple address space declarations
    	//e.g. "%struct.Node.0 addrspace(1)* addrspace(1)* %pNext"
		if(scanner.peek().hasValue("addrspace"))
		{
			//the outmost address space is the one of this pointer, so always overwrite
			addressSpace = readAddressSpace(scanner);
		}
		if (scanner.peek().hasValue('*')) {
			//pop '*'s, there can be several '*'s, so we need to count them
			const std::string ptrs = scanner.pop().getText().value();
			numPointerTypes += std::count(ptrs.begin(), ptrs.end(), '*');
		}
		else
			break;
    }
    while (!typeName.empty() && typeName.back() == '*') {
        //since '*' is now considered a part of a string, it is concatenated to the type-name
        ++numPointerTypes;
        typeName = typeName.substr(0, typeName.size() - 1);
    }
    //correct i1 to bool
    if(typeName.compare("i1") == 0)
    	typeName = TYPE_BOOL.typeName;
    DataType type = TYPE_UNKNOWN;
    if(isArray)
    {
    	type.num = 1;
    	type.complexType.reset(new ArrayType{childType, num});
    	type.typeName = (childType.to_string() + "[") + std::to_string(num) +"]";
    }
    else if(isVector)
    {
    	type = childType;
    	type.num = static_cast<uint8_t>(num);
    }
    else //scalar or complex type
    	type = DataType(typeName, static_cast<uint8_t>(num));
    if(complexTypes.find(typeName) != complexTypes.end())
    	type.complexType = complexTypes.at(typeName).complexType;
    for(unsigned i = 0; i < numPointerTypes; ++i)
    {
    	//wrap in pointer type
    	std::shared_ptr<ComplexType> elementType(new PointerType(type, addressSpace.value_or(AddressSpace::PRIVATE)));
    	type = DataType(type.to_string() + "*", 1, elementType);
    }
    return type;
}

std::vector<std::pair<Value, ParameterDecorations>> IRParser::parseParameters()
{
    Token nextToken{};
    std::vector<std::pair<Value, ParameterDecorations>> res;
    std::size_t numParenthesis = 0;
    do {
        nextToken = scanner.peek();
        if (skipToken(scanner, '(')) {
            ++numParenthesis;
        }
        else if (skipToken(scanner, ')')) {
            --numParenthesis;
        }
        else if (skipToken(scanner, ',')) {
        }
        else {
            //<type> [attributes] <name/value> [','... , ')']
            const DataType type(parseType());
            ParameterDecorations decorations = ParameterDecorations::NONE;
            while (scanner.hasInput() && scanner.peek().type == TokenType::STRING && !(scanner.peek().hasValue(',') || scanner.peek().hasValue('<') || scanner.peek().hasValue('%')))
            {
                //reads attributes
                nextToken = scanner.pop();
                if (nextToken.hasValue("readonly"))
                	decorations = add_flag(decorations, ParameterDecorations::READ_ONLY);
                else if (nextToken.hasValue("zeroext"))
                	decorations = add_flag(decorations, ParameterDecorations::ZERO_EXTEND);
                else if (nextToken.hasValue("signext"))
                	decorations = add_flag(decorations, ParameterDecorations::SIGN_EXTEND);
                else if (nextToken.hasValue(')'))
                {
                    --numParenthesis;
                    break;
                }
            }
            if(!scanner.peek().isEnd() && (scanner.peek().hasValue('<') || scanner.peek().hasValue('%') || scanner.peek().type != TokenType::STRING))
            {
            	const Value arg = parseValue(false, type);
            	logging::debug() << "Parameter " << arg.to_string() << logging::endl;
				res.push_back(std::make_pair(arg, decorations));
            }
            else
            {
            	//as of CLang 3.9, parameters seem to not (always) have explicit names anymore
				//if this is the case, assign the number of the parameter
				if (nextToken.type == TokenType::STRING && !nextToken.hasValue('%')) {
					nextToken.type = TokenType::STRING;
					strncpy(nextToken.text.data(), (std::string("%") + std::to_string(res.size())).data(), TOKEN_BUFFER_SIZE);
				}
				logging::debug() << "Parameter " << type.to_string() << ' ' << nextToken.to_string() << logging::endl;
				res.push_back(std::make_pair(toValue(nextToken, type), decorations));
            }
        }
    }
    while (scanner.hasInput() && numParenthesis != 0);
    return res;
}

static std::vector<Value> parseStringConstant(const std::string& constant, const std::size_t numEntries)
{
	//see constant "u8 Td4[256]" in ./testing/JohnTheRipper/opencl_aes.h (e.g. 7z_kernel.cl)
	//example string: c"R\09j\D506\A58\BF@\A3\9E\81\F3\D7\FB|\E39\82\9B/\FF\874\8ECD\C4\DE\E9\CBT{\942\A6\C2#=\EEL\95\0BB\FA\C3N\08.\A1f(\D9$\B2v[\A2Im\8B\D1%r\F8\F6d\86h\98\16\D4\A4\5C\CC]e\B6\92lpHP\FD\ED\B9\DA^\15FW\A7\8D\9D\84\90\D8\AB\00\8C\BC\D3\0A\F7\E4X\05\B8\B3E\06\D0,\1E\8F\CA?\0F\02\C1\AF\BD\03\01\13\8Ak:\91\11AOg\DC\EA\97\F2\CF\CE\F0\B4\E6s\96\ACt\22\E7\AD5\85\E2\F97\E8\1Cu\DFnG\F1\1Aq\1D)\C5\89o\B7b\0E\AA\18\BE\1B\FCV>K\C6\D2y \9A\DB\C0\FEx\CDZ\F4\1F\DD\A83\88\07\C71\B1\12\10Y'\80\EC_`Q\7F\A9\19\B5J\0D-\E5z\9F\93\C9\9C\EF\A0\E0;M\AE*\F5\B0\C8\EB\BB<\83S\99a\17+\04~\BAw\D6&\E1i\14cU!\0C}"
	//character values: 0x52U, 0x09U, 0x6aU, 0xd5U, 0x30U, 0x36U, 0xa5U, 0x38U, 0xbfU, 0x40U, 0xa3U, 0x9eU, 0x81U, 0xf3U, 0xd7U, 0xfbU,	0x7cU, 0xe3U, 0x39U, 0x82U, 0x9bU, 0x2fU, 0xffU, 0x87U,	0x34U, 0x8eU, 0x43U, 0x44U, 0xc4U, 0xdeU, 0xe9U, 0xcbU, ...
	std::vector<Value> elements;
	elements.reserve(numEntries);
	//initially skip 'c' and '"'
	std::size_t index = 2;
	while(index < constant.size())
	{
		if(constant.at(index) == '\\')
		{
			// "\XY" is a hexadecimal representation of a character
			++index;
			std::string tmp(constant.substr(index, 2));
			unsigned char c = strtoul(tmp.data(), nullptr, 0x10);
			elements.emplace_back(Literal(static_cast<int64_t>(c)), TYPE_INT8);
			index += 2;
		}
		else if(constant.at(index) == '"')
			//end of string reached
			break;
		else
		{
			elements.emplace_back(Literal(static_cast<int64_t>(constant.at(index))), TYPE_INT8);
			++index;
		}
	}
	return elements;
}

Value IRParser::parseValue(bool withType, const DataType& typeArg)
{
    const DataType type = withType ? parseType() : typeArg;
    Value val(nullptr, type);
    Token nextToken = scanner.pop();
    //vector-, array- or struct-container
    if (nextToken.hasValue('<') || nextToken.hasValue('[') || nextToken.hasValue('{')) {
        val.valueType = ValueType::CONTAINER;
        //'<'|'[' <typ> <val> [, <typ> <val>] '>'|']'
        while (!scanner.peek().hasValue('>') && !scanner.peek().hasValue(']') && !scanner.peek().hasValue('}')) {
        	//this is required, e.g. for arrays/structs of vectors
        	//e.g. "@grads3d = addrspace(2) constant [16 x <4 x i8>] [<4 x i8> <i8 1, i8 1, i8 0, i8 0>, <4 x i8> <i8 -1, i8 1, i8 0, i8 0>, ...]"
        	val.container.elements.push_back(parseValue());
            skipToken(scanner, ',');
        }
        scanner.pop();
        return val;
    }
    else if(nextToken.hasValue('c') && nextToken.getText()->find("c\"") == 0)
    {
    	//special case: string-constant (of type char[]), see https://llvm.org/docs/LangRef.html#complex-constants
    	const std::string stringConstant = nextToken.getText().value();
    	return Value(ContainerValue{parseStringConstant(stringConstant, type.getArrayType().value()->size)}, type);
    }
    else
        return toValue(nextToken, type);
}

DataType IRParser::parseStructDefinition()
{
    //<name> = type { <type0>, <type1>, ... }
    //<name> = type <{ <type0>, <type1>, ... }> is a "packed" struct
    std::string name(scanner.pop().getText().value());
    expectSkipToken(scanner, '=');
    expectSkipToken(scanner, "type");
    DataType type;
    type.typeName = name;
    type.complexType.reset(new StructType({}));
    if (skipToken(scanner, '<')) {
        type.getStructType().value()->isPacked = true;
    }
    if(skipToken(scanner, "opaque"))
    {
    	//e.g. "%struct.Node.0 = type opaque"
    }
    else
    {
		do
		{
			//pops initial '{' and following ','s
			scanner.pop();
			type.getStructType().value()->elementTypes.push_back(parseType());
		}
		while (scanner.peek().hasValue(','));
		expectSkipToken(scanner, '}');
    }
    if (type.getStructType().value()->isPacked) {
    	expectSkipToken(scanner, '>');
    }
    logging::debug() << "Struct type: " << type.to_string() << (type.getStructType().value()->isPacked ? " (packed)" : "") << logging::endl;
    if(!type.getStructType().value()->elementTypes.empty())
    	logging::debug() << "with elements: " << to_string<DataType>(type.getStructType().value()->elementTypes) << logging::endl;
    return type;
}

static void skipLinkage(Scanner& scanner)
{
	//http://llvm.org/docs/LangRef.html#linkage
	while(scanner.peek().hasValue("private") || scanner.peek().hasValue("internal") || scanner.peek().hasValue("available_externally") || scanner.peek().hasValue("linkonce") ||
			scanner.peek().hasValue("weak") || scanner.peek().hasValue("common") || scanner.peek().hasValue("appending") || scanner.peek().hasValue("extern_weak") ||
			scanner.peek().hasValue("external") || scanner.peek().hasValue("linkonce_odr") || scanner.peek().hasValue("weak_odr"))
	{
		scanner.pop();
	}
}

static void skipVisibility(Scanner& scanner)
{
	while(scanner.peek().hasValue("default") || scanner.peek().hasValue("hidden") || scanner.peek().hasValue("protected"))
	{
		scanner.pop();
	}
}

bool IRParser::parseGlobalData()
{
	/*
	 * @<GlobalVarName> = [Linkage] [Visibility] [DLLStorageClass] [ThreadLocal]
	 * 			[(unnamed_addr|local_unnamed_addr)] [AddrSpace]
	 * 			[ExternallyInitialized]
	 * 			<global | constant> <Type> [<InitializerConstant>]
	 * 			[, section "name"] [, comdat [($name)]]
	 * 			[, align <Alignment>] (, !name !N)*
	 */
	const std::string name(scanner.pop().getText().value());
	expectSkipToken(scanner, '=');
    bool isExternal = scanner.peek().hasValue("external") || scanner.peek().hasValue("extern_weak");
    skipLinkage(scanner);
    skipVisibility(scanner);
    skipToken(scanner, "unnamed_addr");
    skipToken(scanner, "local_unnamed_addr");
    readAddressSpace(scanner);
    while(scanner.peek().hasValue("global") || scanner.peek().hasValue("constant"))
    {
    	scanner.pop();
    }
    Value val(UNDEFINED_VALUE);
    if(isExternal)
    {
    	//external globals have no content
    	val = Value(parseType());
    }
    else
    {
    	const DataType type(parseType());
    	//TODO global as bitcast of address of of other global (e.g. in /opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/opencl_types.cl). How to handle??
		//would need to retain in the same address of the other global?
    	if(scanner.peek().hasValue("bitcast"))
    		throw CompilationError(CompilationStep::PARSER, "Global as bitcast to other global is not implemented yet", name);
    	val = parseValue(false, type);
    }

    logging::debug() << "Reading global data '" << name << "' with " << val.to_string(false, true) << logging::endl;
    //store local + value
    module->globalData.push_back(Global(name, val.type.toPointerType(), val));
    return true;
}

bool IRParser::parseMethod()
{
	//define [linkage] [visibility] [DLLStorageClass] [cconv] [ret attrs] <ResultType> @<FunctionName> ([argument list]) [(unnamed_addr|local_unnamed_addr)] [fn Attrs] [section "name"]
	bool isKernelSet = false;
    logging::debug() << "-----" << logging::endl;
    skipLinkage(scanner);
    skipVisibility(scanner);
    //skip keywords
    while (scanner.peek().hasValue("signext") || scanner.peek().hasValue("zeroext") || scanner.peek().hasValue("spir_func") || scanner.peek().hasValue("spir_kernel")) {
        if (scanner.pop().hasValue("spir_kernel"))
            isKernelSet = true;
    }
    const DataType returnType(parseType());
    const std::string methodName(cleanMethodName(scanner.pop().getText().value()));
    logging::debug() << "Reading method '" << methodName << "' -> " << returnType.to_string() << ':' << logging::endl;
    methods.emplace_back(LLVMMethod(*module));
    auto& method = methods.back();
    method.method->name = methodName;
    method.method->returnType = returnType;
    currentMethod = method.method.get();
    const auto params = parseParameters();
    for (const auto& param : params) {
    	method.method->parameters.emplace_back(param.first.local->name, param.first.type, param.second);
    	//since with creating the Value for the parameter, a new local is allocated, we need to remove it
    	const_cast<OrderedMap<std::string, Local>&>(method.method->readLocals()).erase(param.first.local->name);
    }
    if (!scanner.hasInput()) {
        return false;
    }
    //skip everything up to '{'
    Token nextToken{};
    do {
        nextToken = scanner.pop();
        //since CLang 3.9, kernel-attributes are appended to the function-definition
        //see http://releases.llvm.org/3.9.0/tools/clang/docs/ReleaseNotes.html#opencl-c-language-changes-in-clang
        if (nextToken.hasValue("!kernel_arg_addr_space")) {
            //set as kernel
            isKernelSet = true;
            method.metaDataMapping[MetaDataType::ARG_ADDR_SPACES] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!kernel_arg_access_qual")) {
            //set as kernel
            isKernelSet = true;
            method.metaDataMapping[MetaDataType::ARG_ACCESS_QUALIFIERS] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!kernel_arg_type")) {
            //set as kernel
            isKernelSet = true;
            method.metaDataMapping[MetaDataType::ARG_TYPE_NAMES] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!kernel_arg_type_qual")) {
            //set as kernel
            isKernelSet = true;
            method.metaDataMapping[MetaDataType::ARG_TYPE_QUALIFIERS] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!reqd_work_group_size")) {
            //set as kernel
            isKernelSet = true;
            method.metaDataMapping[MetaDataType::WORK_GROUP_SIZES] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!work_group_size_hint")) {
            //set as kernel
            isKernelSet = true;
            method.metaDataMapping[MetaDataType::WORK_GROUP_SIZES_HINT] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!kernel_arg_name")) {
        	//set as kernel
			isKernelSet = true;
			method.metaDataMapping[MetaDataType::ARG_NAMES] = scanner.pop().getText().value();
        }
        else if (nextToken.hasValue("!vec_type_hint")) {
        	//vector-type hint, currently unsupported
        	logging::info() << "Vector type hint is currently not supported: " << scanner.pop().getText().value() << logging::endl;
        }
    }
    while (!nextToken.hasValue('{'));
    if (isKernelSet)
    	kernelNames.push_back(methodName);
    //skip remainder of '{' line
    while (scanner.peek().type == TokenType::END) {
        scanner.pop();
    }
    logging::debug() << "-----" << logging::endl;
    parseMethodBody(method);
    logging::debug() << "-----" << logging::endl;
    mapInstructions(method);
    logging::debug() << "-----" << logging::endl;

    return true;
}

void IRParser::parseMethodBody(LLVMMethod& method)
{
    logging::debug() << "Reading method body: " << logging::endl;
    //    //add label %0 to the beginning of the method
    //    //as of CLang 3.9, the first parameter can have the name %0
    //this is not true, e.g. for OpenCL-CTS/kernel_memory_alignment_private.cl
    /*
     * NOTE: This is required by "default" CLang 3.9 and 4.0.
     * SPIRV-LLVM/CLang adds an additional label to the beginning, so this introduces two labels with the same position (an empty basic block), which has no negative effect (maybe a little loss in performance)
     */
    if(method.method->findParameter("%0") == nullptr && method.method->findLocal("%0") == nullptr)
    	method.instructions.emplace_back(new LLVMLabel("%0"));
    do {
        parseInstruction(method, method.instructions);
    }
    while (scanner.hasInput() && !scanner.peek().hasValue('}'));
    expectSkipToken(scanner, '}');

    logging::debug() << "Done, " << method.instructions.size() << " instructions" << logging::endl;
}

void IRParser::parseInstruction(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions)
{
	//http://releases.llvm.org/3.9.0/docs/LangRef.html
    Token nextToken = scanner.pop();
    bool skipEndofLine = true;
    if (nextToken.isEnd()) {
        return;
    }
    else if (nextToken.hasValue('%')) {
        //variable at left side
        parseAssignment(method, instructions, nextToken);
    }
    else if (nextToken.hasValue("store")) {
        //store instruction
        parseStore(method, instructions);
    }
    else if (nextToken.hasValue("tail") || nextToken.hasValue("call")) {
        //XXX put method-call handling (with and without return) together
        if (nextToken.hasValue("tail"))
        	//pop following 'call'
            scanner.pop();
        //method invocation
        parseMethodCall(method, instructions);
    }
    else if (nextToken.hasValue("br")) {
        //branch
        parseBranch(method, instructions);
    }
    else if (nextToken.hasValue("ret")) {
        //return statement
        parseReturn(method, instructions);
    }
    else if (nextToken.hasValue("switch")) {
        //switch statement
        parseSwitch(method, instructions);
    }
    else if (scanner.peek().hasValue(':')) {
        //label type 1
        parseLabel(method, instructions, nextToken);
    }
    else if (nextToken.hasValue(';') && nextToken.to_string().find("<label>") != std::string::npos) {
        //label type 2
        parseLabel(method, instructions, nextToken);
        //label is already full line, don't skip anything more
        skipEndofLine = false;
    }
    else if (nextToken.hasValue(';')) {
        //comment, skip
        skipEndofLine = true;
    }
    else {
        logging::error() << nextToken.to_string() << logging::endl;
        throw CompilationError(CompilationStep::PARSER, scanner.getLineNumber(), std::string("Unknown instruction: ") + nextToken.to_string());
    }
    //skip to end of line
    while (skipEndofLine && !(nextToken = scanner.peek()).isEnd()) {
        nextToken = scanner.pop();
    }
}

static LLVMInstruction* findInstruction(const LLVMMethod& method, const Local* output)
{
    for (const auto& ptr : method.instructions) {
        if (ptr->getDeclaredLocal() == output)
            return ptr.get();
    }
    return nullptr;
}

std::vector<Value> IRParser::parseIndices()
{
    std::vector<Value> indices;
    do {
        expectSkipToken(scanner, ',');
        skipToken(scanner, "inrange");
        indices.push_back(parseValue());
    }
    while (!scanner.peek().isEnd() && !scanner.peek().hasValue(')'));
    return indices;
}

static intermediate::InstructionDecorations parseFastMathFlags(Scanner& scanner)
{
    intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE;
    while (scanner.peek().hasValue("nsw") || scanner.peek().hasValue("nuw") || scanner.peek().hasValue("nnan") ||
           scanner.peek().hasValue("ninf") || scanner.peek().hasValue("nsz") || scanner.peek().hasValue("arcp") || scanner.peek().hasValue("fast") ||
		   scanner.peek().hasValue("exact")) {
        //http://llvm.org/docs/LangRef.html#fast-math-flags
        if (scanner.peek().hasValue("nnan"))
        	decorations = add_flag(decorations, intermediate::InstructionDecorations::NO_NAN);
        else if (scanner.peek().hasValue("ninf"))
        	decorations = add_flag(decorations, intermediate::InstructionDecorations::NO_INF);
        else if (scanner.peek().hasValue("arcp"))
        	decorations = add_flag(decorations, intermediate::InstructionDecorations::ALLOW_RECIP);
        else if (scanner.peek().hasValue("fast"))
        	decorations = add_flag(decorations, intermediate::InstructionDecorations::FAST_MATH);
        scanner.pop();
    }
    return decorations;
}

static std::vector<DataType> getElementTypes(const std::vector<Value>& indices, const DataType& containerType)
{
	std::vector<DataType> elementTypes;
	elementTypes.reserve(indices.size());
	DataType subContainerType = containerType;
	std::size_t curIndex = 0;
	while(curIndex < indices.size())
	{
		if(subContainerType.isPointerType())
		{
			elementTypes.push_back(subContainerType.getPointerType().value()->elementType);
		}
		else if(subContainerType.getArrayType())
		{
			elementTypes.push_back(subContainerType.getArrayType().value()->elementType);
		}
		else if(subContainerType.getStructType())
		{
			//for structs, the index MUST be scalar
			const Value idxVal = indices.at(curIndex);
			if(!idxVal.hasType(ValueType::LITERAL))
				throw CompilationError(CompilationStep::LLVM_2_IR, "Cannot access struct-element with non-scalar index", idxVal.to_string());
			elementTypes.push_back(subContainerType.getStructType().value()->elementTypes.at(idxVal.literal.integer));
		}
		else if(subContainerType.isVectorType())
		{
			//This branch is e.g. used to get the pointer for lifetime-start instructions on vectors
			//Since the lifetime-start instruction always takes an i8*, for vectors of i8 elements, the pointer of the first element is taken
			//XXX not sure, if this is correct!
			elementTypes.push_back(subContainerType.getElementType(curIndex).toPointerType());
		}
		else
			throw CompilationError(CompilationStep::LLVM_2_IR, "Cannot access index of type", subContainerType.to_string());

		++curIndex;
		subContainerType = elementTypes.back();
	}
	return elementTypes;
}

//removes the characters added to identify the parameter type, e.g. 'ff' in powff to identify 2 float-values
static std::string cleanMethodNameParameters(const std::string& name, const std::vector<Value>& args)
{
	std::string result(name);

	auto it = args.rbegin();
	while(it != args.rend())
	{
		if(it->type.isScalarType() || it->type.isVectorType())
		{
			if(it->type.getElementType() == TYPE_FLOAT && result.back() == 'f')
				result = result.substr(0, result.size() - 1);
			else if((it->type.getElementType() == TYPE_INT32 || it->type.getElementType() == TYPE_INT16 || it->type.getElementType() == TYPE_INT8) && (result.back() == 'i' || result.back() == 'j'))
				result = result.substr(0, result.size() - 1);
		}
		++it;
	}

	return result;
}

IndexOf* IRParser::parseGetElementPtr(LLVMMethod& method, const std::string& destination)
{
	/*
	 * Standalone:
	 * <result> = getelementptr <ty>, <ty>* <ptrval>{, <ty> <idx>}*
	 * <result> = getelementptr inbounds <ty>, <ty>* <ptrval>{, <ty> <idx>}*
	 * <result> = getelementptr <ty>, <ptr vector> <ptrval>, <vector index type> <idx>
	 * 
	 * In load-instruction:
	 * e.g: "getelementptr inbounds ([256 x i32] addrspace(2)* @noise_table, i32 0, i32 0)"
	 */
	
	skipToken(scanner, "inbounds");
	bool hadBracket = skipToken(scanner, '(');
	const DataType ptrElementType = parseType();
	Value pointer(UNDEFINED_VALUE);
	//element type, is same as type (just without the pointer)
	//for Khronos CLang, the element type is not listed extra -> check if this has a pointer
	if(ptrElementType.isPointerType())
	{
		pointer = parseValue(false, ptrElementType);
	}
	else
	{
		//pop ','
		expectSkipToken(scanner, ',');
		pointer = parseValue();
	}
	const std::vector<Value> indices = parseIndices();
	const std::vector<DataType> elementTypes = getElementTypes(indices, pointer.type);
	DataType elementType = elementTypes.back();
	//the output type needs to be a pointer to the last elementType!!
	if(!elementType.isPointerType())
	{
		elementType = elementType.toPointerType();
	}

	if(hadBracket)
		expectSkipToken(scanner, ')');

	const Value dest = method.method->findOrCreateLocal(elementType, destination)->createReference();
    logging::debug() << "Getting element " << to_string<Value>(indices) << " from " << pointer.to_string() << " into " << dest.to_string(true) << logging::endl;
	return new IndexOf(dest.local, pointer, indices);
}

void IRParser::parseAssignment(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions, const Token& dest)
{
    const std::string destination(dest.getText().value());
    intermediate::InstructionDecorations decorations = intermediate::InstructionDecorations::NONE;

    expectSkipToken(scanner, '=');

    Token nextToken = scanner.pop();

    if (nextToken.hasValue("alloca")) {
    	/*
    	 * <result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     ; yields type addrspace(num)*:result
    	 */
        //allocation -> determine type
    	skipToken(scanner, "inalloca");
        DataType type(parseType());
        if(skipToken(scanner, ',') && !scanner.peek().hasValue("align") && !scanner.peek().hasValue("addrspace"))
        {
        	//[, <ty> <NumElements>]
        	const Value numEntries = parseValue();
        	if(!numEntries.hasType(ValueType::LITERAL))
        		throw CompilationError(CompilationStep::PARSER, "Cannot allocate a non-constant number of entries", numEntries.to_string());
        	const DataType childType = type;
        	type.num = 1;
			type.complexType.reset(new ArrayType{childType, static_cast<unsigned>(numEntries.literal.integer)});
			type.typeName = (childType.to_string() + "[") + std::to_string(numEntries.literal.integer) +"]";
        }
        std::size_t alignment = 1;
        skipToken(scanner, ',');
        if(skipToken(scanner, "align"))
        {
        	alignment = scanner.pop().integer;
        }
        logging::debug() << "Stack-allocation for " << type.to_string() << " " << destination << " with alignment of " << alignment << " bytes" << logging::endl;
        method.method->stackAllocations.emplace(StackAllocation(destination, type.toPointerType(), type.getPhysicalWidth(), alignment));
        return;
    }
    else if (nextToken.hasValue("bitcast")) {
        //<result> = bitcast <ty> <value> to <ty2> 
        //convert bits -> determine source address
        const DataType type(parseType());
        const std::string source(scanner.pop().getText().value());

        //pop 'to'
        expectSkipToken(scanner, "to");

        const DataType destType(parseType());
        const Value src = method.method->findOrCreateLocal(type, source)->createReference();
        const Value dest = method.method->findOrCreateLocal(destType, destination)->createReference();

        if(dest.hasType(ValueType::LOCAL) && src.hasType(ValueType::LOCAL) && dest.type.isPointerType() && src.type.isPointerType())
        	//this helps recognizing lifetime-starts of bit-cast stack-allocations
        	const_cast<std::pair<Local*, int>&>(dest.local->reference) = std::make_pair(src.local, ANY_ELEMENT);

        logging::debug() << "Making reference from bitcast from " << src.to_string() << " to " << dest.to_string() << logging::endl;
        //simply associate new and original
        instructions.emplace_back(new Copy(dest, src));
        return;
    }
    else if(nextToken.hasValue("ptrtoint") || nextToken.hasValue("inttoptr")) {
    	//<result> = ptrtoint <ty> <value> to <ty2>
    	//<result> = inttoptr <ty> <value> to <ty2>
    	//convert bits with truncation/zero extension
    	const Value src(parseValue());

    	//pop 'to'
    	expectSkipToken(scanner, "to");

    	const DataType destType(parseType());
    	const Value dest = method.method->findOrCreateLocal(destType, destination)->createReference();

    	logging::debug() << "Casting between pointer and integer: " << src.to_string() << " to " << dest.to_string() << logging::endl;
    	if(destType.getScalarBitCount() > src.type.getScalarBitCount())
    		instructions.emplace_back(new UnaryOperator("zext", dest, src));
    	else
    		instructions.emplace_back(new Copy(dest, src));
    	return;
    }
    else if (nextToken.hasValue("load")) {
        //load from memory
    	/*
    	 * <result> = load [volatile] <ty>, <ty>* <pointer>[, align <alignment>][, !nontemporal !<index>][, !invariant.load !<index>][, !invariant.group !<index>][, !nonnull !<index>][, !dereferenceable !<deref_bytes_node>][, !dereferenceable_or_null !<deref_bytes_node>][, !align !<align_node>]
    	 * <result> = load atomic [volatile] <ty>, <ty>* <pointer> [singlethread] <ordering>, align <alignment> [, !invariant.group !<index>]
    	 */
    	skipToken(scanner, "atomic");
    	skipToken(scanner, "volatile");

        DataType type(parseType());
        DataType sourceType(TYPE_UNKNOWN);
        //Khronos CLang does not list pointer- and value-type separately
        if (type.isPointerType()) {
            sourceType = type;
            type = type.getPointerType().value()->elementType;
        }
        else {
            //pop ','
            expectSkipToken(scanner, ',');
            //pop second type
            DataType sourceType(parseType());
        }

        IndexOf* index = nullptr;
        std::unique_ptr<IndexOf> tmpIndex;
        Value src(UNDEFINED_VALUE);
        if(scanner.peek().hasValue("getelementptr"))
        {
        	//pop 'getelementptr'
        	expectSkipToken(scanner, "getelementptr");
        	index = parseGetElementPtr(method, destination);
        	if(index != nullptr)
        	{
        		instructions.emplace_back(index);
        		src = index->getDeclaredLocal()->createReference();
        	}
        	return;
        	
        }
        else
        {
        	const std::string sourceName(scanner.pop().getText().value());
        	src = method.method->findOrCreateLocal(sourceType, sourceName)->createReference();
        }
        logging::debug() << "Copying by loading of " << type.to_string() << " from " << src.to_string() << " into " << destination << logging::endl;

        //TODO overhaul, remove srcIndex/srcContainer
        Value srcContainer(TYPE_UNKNOWN);
        //check whether read from in-parameter
        if(index == nullptr)
        {
        	index = dynamic_cast<IndexOf*> (findInstruction(method, src.local));
        }
        if (src.hasType(ValueType::LOCAL) && src.local->is<Global>()) {
            //load from global -> copy value
            instructions.emplace_back(new ContainerExtraction(method.method->findOrCreateLocal(type, destination), src, INT_ZERO));
            return;
        }
        else if (index != nullptr && index->getDeclaredLocal() == src.local) {
            //load via getelementptr (any previous instruction)
            srcContainer = index->getContainer();
        }
        else if (src.hasType(ValueType::LOCAL) && src.local->is<Parameter>()) {
            //load directly from input (e.g. index 0)
            srcContainer = src;
        }
        else if (src.hasType(ValueType::LOCAL)) {
            //load from local
            //TODO what to do??
        }
        else {
            //TODO load without getelementptr
            //reserves memory on the stack?? (e.g. test_other)
        	//TODO also jumps here for loading struct-members
            throw CompilationError(CompilationStep::PARSER, scanner.getLineNumber(), "Loading without 'getelementptr' is not implemented!");
        }
        //for (almost) any load from an array/pointer position, the previous instruction is a getelementptr, which gets the index
        //only exception is the first load into ptr[0]
        if (srcContainer.hasType(ValueType::LOCAL)) {
            if (srcContainer.local->is<Parameter>()) {
                //loads from input parameter
                // -> make extra instruction/flag to be used later
                instructions.emplace_back(new Copy(method.method->findOrCreateLocal(type, destination)->createReference(), src, true, true));
                return;
            }
        }
        instructions.emplace_back(new Copy(method.method->findOrCreateLocal(type, destination)->createReference(), src, true, true));
        return;
    }
    else if (nextToken.hasValue("call") || nextToken.hasValue("tail") || nextToken.hasValue("notail") || nextToken.hasValue("musttail")) {
        /*
         * <result> = [tail | musttail | notail ] call [fast-math flags] [cconv] [ret attrs] <ty>|<fnty> <fnptrval>(<function args>) [fn attrs] [ operand bundles ]
         */
    	//pop following 'call'
        skipToken(scanner, "call");
        //pop 'spir_func'
        skipToken(scanner, "spir_func");
        //pop 'fast-math'
        if(skipToken(scanner, "fast-math")) {
            //parse flags
            decorations = add_flag(decorations, parseFastMathFlags(scanner));
        }
        skipToken(scanner, "cconv");
        if(skipToken(scanner, "ret"))
        {
        	//pop 'zeroext', signext' or 'inreg'
        	scanner.pop();
        }
        const DataType returnType(parseType());
        if (scanner.peek().hasValue('(')) {
            //XXX what does the argument-types before the name do??
            do {
                scanner.pop();
            }
            while (!scanner.peek().hasValue(')'));
            //pop ')'
            expectSkipToken(scanner, ')');
        }
        std::string name;
        if (scanner.peek().hasValue("bitcast")) {
            //XXX what does the bitcast-part do??
            do {
                scanner.pop();
            }
            while (!scanner.peek().hasValue(')'));
            //pop ')'
            expectSkipToken(scanner, ')');
            skipToken(scanner, '*');
            name = scanner.pop().getText().value();
            std::size_t bracket_level = 1;
            while (bracket_level > 0) {
                if (scanner.peek().hasValue('('))
                    ++bracket_level;
                if (scanner.peek().hasValue(')'))
                    --bracket_level;
                scanner.pop();
            }
        }
        else
            name = scanner.pop().getText().value();
        name = cleanMethodName(name);
        std::vector<Value> args;
        const auto params = parseParameters();
        args.reserve(params.size());
        std::for_each(params.begin(), params.end(), [&args](const std::pair<Value, ParameterDecorations>& pair)
        {
            args.push_back(pair.first);
        });
        name = cleanMethodNameParameters(name, args);
        logging::debug() << "Method call to " << name << " storing " << returnType.to_string() << " into " << destination << logging::endl;
        method.method->findOrCreateLocal(returnType, destination);
        instructions.emplace_back((new CallSite(method.method->findOrCreateLocal(returnType, destination), name, returnType, args))->setDecorations(decorations));
        return;
    }
    else if (nextToken.hasValue("getelementptr")) {
    	IndexOf* index = parseGetElementPtr(method, destination);
    	if(index != nullptr)
    		instructions.emplace_back(index);
    	return;
    }
    else if (nextToken.hasValue("icmp") || nextToken.hasValue("fcmp")) {
        //compare
        //(icmp|fcmp) [fast-math flags] <flag> <type> <op1>, <op2>
        while (skipToken(scanner, "fast-math")) {
            decorations = add_flag(decorations, parseFastMathFlags(scanner));
        }
        const std::string flag(scanner.pop().getText().value());
        const Value op1(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        const Value op2(parseValue(false, op1.type));

        logging::debug() << "Comparison " << flag << " between " << op1.to_string() << " and " << op2.to_string() << " into " << destination << logging::endl;
        instructions.emplace_back((new Comparison(method.method->findOrCreateLocal(TYPE_BOOL, destination), flag, op1, op2, nextToken.hasValue("fcmp")))->setDecorations(decorations));
        return;
    }
    else if (nextToken.hasValue("insertelement") || nextToken.hasValue("insertvalue")) {
    	/*
    	 * <result> = insertelement <n x <ty>> <val>, <ty> <elt>, <ty2> <idx>    ; yields <n x <ty>>
    	 *
    	 * <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*    ; yields <aggregate type>
    	 */
        const Value container(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        const Value newValue(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        Value index(UNDEFINED_VALUE);
		if(nextToken.hasValue("insertvalue"))
			//the indices of insertvalue are literals, without a type
			index = parseValue(false, TYPE_INT32);
		else
			index = parseValue();

        logging::debug() << "Setting container element " << index.to_string() << " of " << container.to_string() << " to " << newValue.to_string() << logging::endl;
        instructions.emplace_back(new ContainerInsertion(method.method->findOrCreateLocal(container.type, destination), container, newValue, index));
        return;
    }
    else if (nextToken.hasValue("extractelement") || nextToken.hasValue("extractvalue")) {
    	/*
    	 * <result> = extractelement <n x <ty>> <val>, <ty2> <idx>  ; yields <ty>
    	 *
    	 * <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}*
    	 */
        const Value container(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        Value index(TYPE_UNKNOWN);
        if(nextToken.hasValue("extractvalue"))
        	//the indices of extractvalue are literals, without a type
        	index = parseValue(false, TYPE_INT32);
        else
        	index = parseValue();

        logging::debug() << "Reading container element " << index.to_string() << " of " << container.to_string() << " into " << destination << logging::endl;
        instructions.emplace_back(new ContainerExtraction(method.method->findOrCreateLocal(container.type.getElementType(), destination), container, index));
        return;
    }
    else if (nextToken.hasValue("shufflevector")) {
    	/*
    	 * <result> = shufflevector <n x <ty>> <v1>, <n x <ty>> <v2>, <m x i32> <mask>    ; yields <m x <ty>>
    	 */
        const Value container1(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        const Value container2(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        //"The shuffle mask operand is required to be a constant vector with either constant integer or undef values."
        //mask is in value, e.g. "<i32 0, i32 2>"
        const Value shuffleMask = parseValue();

        logging::debug() << "Shuffling vectors " << container1.to_string() << " and " << container2.to_string()
                << " with mask " << shuffleMask.to_string() << " into " << destination << logging::endl;

        DataType resultType = container1.type;
        resultType.num = shuffleMask.type.num;
        instructions.emplace_back(new ShuffleVector(method.method->findOrCreateLocal(resultType, destination)->createReference(), container1, container2, shuffleMask));
        return;
    }
    else if (nextToken.hasValue("phi")) {
    	/*
    	 * <result> = phi <ty> [ <val0>, <label0>], ...
    	 */
        const DataType type(parseType());
        std::vector<std::pair<Value, const Local*>> labels;
        do {
        	skipToken(scanner, ',');
            //pop '['
            expectSkipToken(scanner, '[');
            const Value val(parseValue(false, type));
            //pop ','
            expectSkipToken(scanner, ',');
            const std::string label(scanner.pop().getText().value());
            //pop ']'
            expectSkipToken(scanner, ']');
            labels.emplace_back(val, method.method->findOrCreateLocal(TYPE_LABEL, label));
        }
        while (scanner.peek().hasValue(','));
        logging::debug() << "Phi-Node into " << destination << logging::endl;
        instructions.emplace_back(new PhiNode(method.method->findOrCreateLocal(type, destination), labels));
        return;
    }
    else if (nextToken.hasValue("select")) {
    	/*
    	 * <result> = select selty <cond>, <ty> <val1>, <ty> <val2>             ; yields ty
    	 */
        const Value cond(parseValue());
        //skip ','
        expectSkipToken(scanner, ',');
        const Value val1(parseValue());
        //skip ','
        expectSkipToken(scanner, ',');
        const Value val2(parseValue());

        logging::debug() << "Selection of " << val1.to_string() << " or " << val2.to_string() << " according to " << cond.to_string() << " into " << destination << logging::endl;
        instructions.emplace_back(new Selection(method.method->findOrCreateLocal(val1.type, destination), cond, val1, val2));
        return;
    }
    else {
        //assume this format: <opcode> <format> <in1> [to | ,] [<in2>]
        //add, fadd, sub, fsub, mul, fmul, udiv, sdiv, fdiv, urem, srem, frem, shl, lshr, ashr, and, or, xor
        //trunc, zext, sext, fptrunc, fpext, fptoui, fptosi, uitofp, sitofp, ptrtoint, inttoptr, addrspacecast
        const std::string opCode(nextToken.getText().value());
        nextToken = scanner.peek();
        if (nextToken.type != TokenType::STRING) {
            throw CompilationError(CompilationStep::PARSER, scanner.getLineNumber(), std::string("Unhandled instruction: ") + nextToken.to_string());
        }
        decorations = add_flag(decorations, parseFastMathFlags(scanner));
        const DataType type(parseType());

        const Value arg1(parseValue(false, type));
        nextToken = scanner.peek();
        if (!nextToken.isEnd()) {
            bool isConversion = false;
            if (nextToken.hasValue("to")) {
                isConversion = true;
            }
            //skip 'to' or ','
            scanner.pop();
            if (isConversion)
            {
            	const DataType destType = parseType();
                logging::debug() << "Convert (" << opCode << ") " << arg1.to_string() << " to " << destType.to_string() << ' ' << destination << logging::endl;
                instructions.emplace_back((new UnaryOperator(opCode, method.method->findOrCreateLocal(destType, destination)->createReference(), arg1))->setDecorations(decorations));
            }
            else
            {
            	const Value arg2 = parseValue(false, type);
                logging::debug() << "Binary-Operator " << opCode << " with " << arg1.to_string() << " and " << arg2.to_string() << " into " << destination << logging::endl;
                instructions.emplace_back((new BinaryOperator(opCode, method.method->findOrCreateLocal(type, destination)->createReference(), arg1, arg2))->setDecorations(decorations));
            }
        }
        else {
            //unary instruction
            logging::debug() << "Unary-Operator " << opCode << " with " << type.to_string() << ' ' << arg1.to_string() << " into " << destination << logging::endl;
            instructions.emplace_back((new UnaryOperator(opCode, method.method->findOrCreateLocal(type, destination)->createReference(), arg1))->setDecorations(decorations));
        }
        return;
    }
}

void IRParser::parseMethodCall(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions)
{
	//pop 'spir_func'
	skipToken(scanner, "spir_func");
    const DataType returnType(parseType());
    std::string name(cleanMethodName(scanner.pop().getText().value()));

    std::vector<Value> args;
    const auto params = parseParameters();
    args.reserve(params.size());
    std::for_each(params.begin(), params.end(), [&args](const std::pair<Value, ParameterDecorations>& pair)
    {
        args.push_back(pair.first);
    });
    name = cleanMethodNameParameters(name, args);
    logging::debug() << "Method call to " << name << " -> " << returnType.to_string() << logging::endl;
    instructions.emplace_back(new CallSite(name, returnType, args));
}

void IRParser::parseStore(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions)
{
	/*
	 * store [volatile] <ty> <value>, <ty>* <pointer>[, align <alignment>][, !nontemporal !<index>][, !invariant.group !<index>]        ; yields void
	 * store atomic [volatile] <ty> <value>, <ty>* <pointer> [singlethread] <ordering>, align <alignment> [, !invariant.group !<index>] ; yields void
	 */
	skipToken(scanner, "atomic");
	//XXX handle volatile?
	skipToken(scanner, "volatile");

    const Value value(parseValue());
    //pop ','
    expectSkipToken(scanner, ',');

    Value destination(parseValue());

    //TODO overhaul, fix, remove destIndex, destContainer
    logging::debug() << "Copying by storing " << value.to_string() << " into " << destination.to_string() << logging::endl;
    Value destContainer(UNDEFINED_VALUE);
    //check whether write to out-parameter
    IndexOf* index = dynamic_cast<IndexOf*> (findInstruction(method, destination.local));
    if (destination.hasType(ValueType::LOCAL) && destination.local->residesInMemory()) {
        //store into global
        destContainer = destination;
    }
    else if (index != nullptr && index->getDeclaredLocal() == destination.local) {
        destContainer = index->getContainer();
    }
    else if (destination.hasType(ValueType::LOCAL) && destination.local->is<Parameter>()) {
        destContainer = destination;
    }
    else {
        //write without (direct access to) getelementptr
        //check bitcast of pointer
        //TODO doesn't find bitcast
        LLVMInstruction* cast = findInstruction(method, destination.local);
        if (cast != nullptr) {
            index = dynamic_cast<IndexOf*> (findInstruction(method, cast->getAllLocals()[1]));
            if (index != nullptr && index->getDeclaredLocal() == destination.local) {
                destContainer = index->getContainer();
            }
        }
        else if (destination.hasType(ValueType::LOCAL) && value.type == destination.type) {
            //store into locally allocated object (with alloca)
            instructions.emplace_back(new Copy(destination, value, true));
            return;
        }
        else
        {
            throw CompilationError(CompilationStep::PARSER, scanner.getLineNumber(), "This type of storing is not implemented!");
        }
    }
    //for (almost) any store to an array/pointer position, the previous instruction is a getelementptr, which sets the index
    //only exception is the first store into ptr[0]
    if (destContainer.hasType(ValueType::LOCAL)) {
        //check whether output is a parameter
    	//FIXME rewrite
//    	throw CompilationError(CompilationStep::PARSER, "This type of storing is not implemented yet", destContainer.to_string());
//        if (!destContainer.local->is<Parameter>()) {
//            //if not, check whether output is a reference to a parameter
//            //TODO improve, so this works for arbitrary depth of referencing ??!
//            const auto tmp = method.method.findLocal(destContainer.local);
//            if (tmp && tmp.get().value.hasType(ValueType::LOCAL) && method.method.findParameter(tmp.get().value.local)) {
//                param = method.method.findParameter(tmp.get().value.local).get();
//            }
//        }
//        if (param) {
//            //stores to output parameter
//            // -> make extra instruction/flag to be used later
//            return new Copy(destContainer.local, value, destIndex);
//        }
    }
    //FIXME is this correct??
    instructions.emplace_back(new Copy(destination, value, true));
}

void IRParser::parseBranch(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions)
{
	/*
	 * br i1 <cond>, label <iftrue>, label <iffalse>
	 * br label <dest>          ; Unconditional branch
	 *
	 */
    if (skipToken(scanner, "label")) {
        //unconditional branch
        const std::string label(scanner.pop().getText().value());

        logging::debug() << "Unconditional branch to " << label << logging::endl;
        instructions.emplace_back(new Branch(label));
    }
    else {
        //conditional branch
        const Value cond(parseValue());
        //pop ','
        expectSkipToken(scanner, ',');
        //pop 'label'
        expectSkipToken(scanner, "label");
        const std::string trueLabel(scanner.pop().getText().value());
        //pop ','
        expectSkipToken(scanner, ',');
        //pop 'label'
        expectSkipToken(scanner, "label");
        const std::string falseLabel(scanner.pop().getText().value());

        logging::debug() << "Branch on " << cond.to_string() << " to either " << trueLabel << " or " << falseLabel << logging::endl;
        instructions.emplace_back(new Branch(cond, trueLabel, falseLabel));
    }
}

void IRParser::parseReturn(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions)
{
	/*
	 * ret <type> <value>       ; Return a value from a non-void function
	 * ret void                 ; Return from void function
	 */
    const DataType type(parseType());
    const Token value = scanner.peek();

    logging::debug() << "Returning " << type.to_string() << ' ' << value.to_string() << logging::endl;
    if (!value.isEnd()) {
        scanner.pop();
        instructions.emplace_back(new ValueReturn(toValue(value, type)));
    }
    else
    	instructions.emplace_back(new ValueReturn());
}

void IRParser::parseLabel(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions, const Token& label)
{
    //pop ':' (only present in type-1 labels)
	skipToken(scanner, ':');
    std::string labelName = label.getText().value();
    if (labelName.find("<label>") != std::string::npos) {
        labelName = labelName.substr(labelName.find(':') + 1);
    }
    //trunc trailing comment
    labelName = labelName.substr(0, labelName.find("  "));
    labelName = std::string("%") + labelName;
    //truncate training ':' (for type-2 labels)
    if(labelName.find(':') != std::string::npos)
    	labelName = labelName.substr(0, labelName.find(':'));
    logging::debug() << "Setting label " << labelName << logging::endl;
    instructions.emplace_back(new LLVMLabel(labelName));
}

void IRParser::parseSwitch(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions)
{
	/*
	 * switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]
	 */
    //generalization of the branch instruction
    const Value cond(parseValue());
    //skip ','
    expectSkipToken(scanner, ',');
    //skip 'label'
    expectSkipToken(scanner, "label");
    const std::string defaultLabel(scanner.pop().getText().value());
    //skip '['
    expectSkipToken(scanner, '[');
    if(scanner.peek().isEnd())
    {
		//skip new-line, if any
		scanner.pop();
    }
    std::map<int, std::string> cases;
    do {
        const Value matchVal(parseValue());
        //skip ','
        expectSkipToken(scanner, ',');
        //skip 'label'
        expectSkipToken(scanner, "label");
        const std::string matchLabel(scanner.pop().getText().value());

        // "[...] an array of pairs of comparison value constants and ‘label‘s"
        cases[matchVal.literal.integer] = matchLabel;
        if(scanner.peek().isEnd())
        {
        	//skip new-line, if any
        	scanner.pop();
        }
    }
    while (!scanner.peek().hasValue(']'));

    logging::debug() << "Switching on " << cond.to_string() << " with " << cases.size() << " labels, defaulting to " << defaultLabel << logging::endl;
    instructions.emplace_back(new Switch(cond, defaultLabel, cases));
}

void IRParser::parseMetaData()
{
    //http://llvm.org/docs/LangRef.html#metadata
    //e.g. "!foo = !{!4, !3}", "!42 = !{ i32 1234567 }", "!opencl.kernels = !{!0, !6, !12, !18, !21, !27, !30}"
    if (scanner.peek().type != TokenType::STRING)
        return;
    std::string name(scanner.pop().getText().value());
    std::vector<std::string>& values = metaData[name];

    //pop '='
    expectSkipToken(scanner, '=');
    //pop 'distinct', if there: "Metadata nodes that aren’t uniqued use the distinct keyword"
    skipToken(scanner, "distinct");
    //pop '!'
    expectSkipToken(scanner, '!');
    Token nextToken{};
    while (scanner.hasInput() && !(nextToken = scanner.pop()).isEnd() && (nextToken.hasValue(',') || nextToken.hasValue('{'))) {
        if (scanner.peek().hasValue('!') || scanner.peek().hasValue('"')) {
            //string
            std::string val(scanner.pop().getText().value());
            //don't cut off anything for references to IDs
            if (val[0] == '!' && std::isdigit(val[1]))
                values.push_back(val);
            else
                //cut off leading '!"' and trailing '"'
                values.push_back(val.substr(2, val.size() - 3));
        }
        else
        {
            //index 'i32' or '64' before numbers
            skipToken(scanner, 'i');
        }
        if (scanner.peek().type == TokenType::NUMBER)
            values.push_back(std::to_string(scanner.pop().integer));
        else {
            //e.g. kernel method-header
            while (!(nextToken = scanner.peek()).isEnd() && !nextToken.hasValue(',')) {
                if (nextToken.hasValue('@'))
                    values.push_back(nextToken.getText().value());
                scanner.pop();
            }
        }
    }
}

static std::vector<std::string> supportedOpenCLExtensions = {
};

static std::vector<std::string> supportedOpenCLOptionalCoreFeatures = {
		"cl_images",
};

void IRParser::extractKernelInfo()
{
    //1. if the input is LLVM 3.8 or older, the kernel-names must be retrieved from "!opencl.kernels"
    if (metaData.find("!opencl.kernels") != metaData.end()) {
        kernelIDs.insert(kernelIDs.begin(), metaData.at("!opencl.kernels").begin(), metaData.at("!opencl.kernels").end());
    }
    //check used OpenCL extensions and optional core features
    if (metaData.find("!opencl.used.extensions") != metaData.end()) {
    	std::vector<std::string> extensions;
    	for(const std::string& exID : metaData.at("!opencl.used.extensions"))
    	{
    		const std::vector<std::string>& ex = metaData.at(exID);
    		extensions.insert(extensions.end(), ex.begin(), ex.end());
    	}
    	for(const std::string& extension : extensions)
    	{
    		logging::debug() << "Using OpenCL extension: " << extension << logging::endl;
    		bool extensionFound = false;
    		for(const std::string& supportedExtension : supportedOpenCLExtensions)
    		{
    			if(supportedExtension.compare(extension) == 0)
    			{
    				extensionFound = true;
    				break;
    			}
    		}
    		if(!extensionFound)
    			throw CompilationError(CompilationStep::PARSER, "Unsupported OpenCL extension used", extension);
    	}
    }
    if (metaData.find("!opencl.used.optional.core.features") != metaData.end()) {
    	std::vector<std::string> features;
		for(const std::string& exID : metaData.at("!opencl.used.optional.core.features"))
		{
			const std::vector<std::string>& feat = metaData.at(exID);
			features.insert(features.end(), feat.begin(), feat.end());
		}
		for(const std::string& feature : features)
		{
			logging::debug() << "Using optional OpenCL core-feature: " << feature << logging::endl;
			bool featureFound = true;
			for(const std::string& supportedFeature : supportedOpenCLOptionalCoreFeatures)
			{
				if(supportedFeature.compare(feature) == 0)
				{
					featureFound = true;
					break;
				}
			}
			if(!featureFound)
				throw CompilationError(CompilationStep::PARSER, "Unsupported optional core feature used", feature);
		}
    }
    //2. associate kernel-IDs with kernel-names and meta-data (for CLang < 3.9)
    //2.1 extract meta-data-types from IDs
    std::map<std::string, MetaDataType> typeMapping;
    for (auto& pair : metaData) {
        if (pair.second.size() > 0) {
            if (pair.second[0] == "kernel_arg_addr_space") {
                pair.second.erase(pair.second.begin());
                typeMapping[pair.first] = MetaDataType::ARG_ADDR_SPACES;
            }
            else if (pair.second[0] == "kernel_arg_access_qual") {
                pair.second.erase(pair.second.begin());
                typeMapping[pair.first] = MetaDataType::ARG_ACCESS_QUALIFIERS;
            }
            else if (pair.second[0] == "kernel_arg_type_qual") {
                pair.second.erase(pair.second.begin());
                typeMapping[pair.first] = MetaDataType::ARG_TYPE_QUALIFIERS;
            }
            else if (pair.second[0] == "kernel_arg_type") {
                pair.second.erase(pair.second.begin());
                typeMapping[pair.first] = MetaDataType::ARG_TYPE_NAMES;
            }
            else if (pair.second[0] == "reqd_work_group_size") {
                pair.second.erase(pair.second.begin());
                typeMapping[pair.first] = MetaDataType::WORK_GROUP_SIZES;
            }
            else if (pair.second[0] == "work_group_size_hint") {
                pair.second.erase(pair.second.begin());
                typeMapping[pair.first] = MetaDataType::WORK_GROUP_SIZES_HINT;
            }
            else if (pair.second[0] == "kernel_arg_name") {
            	pair.second.erase(pair.second.begin());
				typeMapping[pair.first] = MetaDataType::ARG_NAMES;
            }
            else if(pair.second[0] == "vec_type_hint") {
            	pair.second.erase(pair.second.begin());
            	//vector-type hint, currently unsupported
				logging::info() << "Vector type hint is currently not supported: " << to_string<std::string>(pair.second) << logging::endl;
            }
        }
    }
    //2.2 extract kernel-names and kernel-infos
    if (!kernelIDs.empty()) {
        LLVMMethod* method = nullptr;
        for (const std::string& id : kernelIDs) {
            bool nameFound = false;
            auto it = metaData.at(id).begin();
            while (it != metaData.at(id).end()) {
                if ((*it).find("@") == 0) {
                	const std::string methodName = cleanMethodName(*it);
                    nameFound = true;
                    kernelNames.push_back(methodName);
                    for (LLVMMethod& m : methods) {
                        if (m.method->name == methodName)
                            method = &m;
                    }
                }
                else if (nameFound && typeMapping.find(*it) != typeMapping.end()) {
                    //all values beyond the kernel-name are kernel-info
                    method->metaDataMapping[typeMapping.at(*it)] = *it;
                }
                ++it;
            }
        }
    }
    //3. map all meta-data-type -> ID associations to the data
    for (LLVMMethod& method : methods) {
        for (const auto& pair : method.metaDataMapping) {
        	const std::vector<std::string> values = metaData.at(pair.second);
            switch(pair.first)
            {
            	case MetaDataType::ARG_ACCESS_QUALIFIERS:
            		//XXX what to do with them? Only valid for images?
					break;
            	case MetaDataType::ARG_ADDR_SPACES:
            	{
            		for(std::size_t i = 0; i < values.size(); ++i)
            		{
            			Optional<PointerType*> ptrType = method.method->parameters.at(i).type.getPointerType();
            			if(ptrType && ptrType.value()->addressSpace == AddressSpace::GENERIC)
            				ptrType.value()->addressSpace = toAddressSpace(std::atoi(values.at(i).data()));
            		}
            		break;
            	}
            	case MetaDataType::ARG_TYPE_QUALIFIERS:
            	{
            		for(std::size_t i = 0; i < values.size(); ++i)
            		{
            			Parameter& param = method.method->parameters.at(i);
            			if(values.at(i).find("const") != std::string::npos)
            				param.decorations = add_flag(param.decorations, ParameterDecorations::READ_ONLY);
            			if(values.at(i).find("restrict") != std::string::npos)
            				param.decorations = add_flag(param.decorations, ParameterDecorations::RESTRICT);
            			if(values.at(i).find("volatile") != std::string::npos)
							param.decorations = add_flag(param.decorations, ParameterDecorations::VOLATILE);
            		}
            		break;
            	}
            	case MetaDataType::ARG_NAMES:
            	{
            		for(std::size_t i = 0; i < values.size(); ++i)
					{
						Parameter& param = method.method->parameters.at(i);
						param.parameterName = values.at(i);
					}
            		break;
            	}
            	case MetaDataType::ARG_TYPE_NAMES:
            	{
            		for(std::size_t i = 0; i < values.size(); ++i)
					{
						Parameter& param = method.method->parameters.at(i);
						param.origTypeName = values.at(i);
						if(!param.type.isPointerType() && (values.at(i).find("uint") == 0 || values.at(i).find("ushort") == 0 || values.at(i).find("uchar") == 0))
							param.decorations = add_flag(param.decorations, ParameterDecorations::ZERO_EXTEND);
						else if(!param.type.isPointerType() && (values.at(i).find("int") == 0 || values.at(i).find("short") == 0 || values.at(i).find("char") == 0))
							param.decorations = add_flag(param.decorations, ParameterDecorations::SIGN_EXTEND);
					}
            		break;
            	}
            	case MetaDataType::WORK_GROUP_SIZES:
            		method.method->metaData.workGroupSizes = {static_cast<uint32_t>(std::atoi(values.at(0).data())), static_cast<uint32_t>(std::atoi(values.at(1).data())), static_cast<uint32_t>(std::atoi(values.at(2).data()))};
            		break;
            	case MetaDataType::WORK_GROUP_SIZES_HINT:
					method.method->metaData.workGroupSizeHints = {static_cast<uint32_t>(std::atoi(values.at(0).data())), static_cast<uint32_t>(std::atoi(values.at(1).data())), static_cast<uint32_t>(std::atoi(values.at(2).data()))};
					break;
            	default:
            		throw CompilationError(CompilationStep::PARSER, "Unhandled meta-data", to_string<std::string>(values));
            }
        }
    }

    //4. set kernel-flags
	for (LLVMMethod& m : methods) {
		for (const std::string& kernelName : kernelNames) {
			if (m.method->name.compare(kernelName) == 0) {
				m.method->isKernel = true;
				break;
			}
		}
	}
}

void IRParser::mapInstructions(LLVMMethod& method) const
{
    logging::debug() << "Mapping LLVM instructions to immediates: " << logging::endl;
    for (const auto& instr : method.instructions) {
        instr->mapInstruction(*method.method);
    }
    logging::debug() << "Done, generated " << method.method->countInstructions() << " immediate instructions from " << method.instructions.size() << " LLVM instructions" << logging::endl;
}
