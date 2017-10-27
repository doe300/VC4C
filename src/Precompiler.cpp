/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <string.h>
#include <sstream>
#include <stdlib.h>
#include <fstream>
#include <libgen.h>
#include <iterator>

#include "Precompiler.h"
#include "log.h"
#include "ProcessUtil.h"
#ifdef SPIRV_HEADER
#include "spirv/SPIRVHelper.h"
#endif

using namespace vc4c;

SourceType Precompiler::getSourceType(std::istream& stream)
{
	//http://llvm.org/docs/BitCodeFormat.html#magic-numbers
	static const char LLVM_BITCODE_MAGIC_NUMBER[2] = {0x42, 0x43};
    static const uint32_t SPIRV_MAGIC_NUMBER = 0x07230203;
    static const char SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN[4] = {0x07, 0x23, 0x02, 0x03};
    static const char SPIRV_MAGIC_NUMBER_BIG_ENDIAN[4] = {0x03, 0x02, 0x23, 0x07};
    char buffer[1024];
    stream.read(buffer, 1000);
    const std::string s(buffer, stream.gcount());

    SourceType type = SourceType::UNKNOWN;
    if(s.find("ModuleID") != std::string::npos || s.find("\ntarget triple") != std::string::npos)
        type = SourceType::LLVM_IR_TEXT;
    else if(memcmp(buffer, LLVM_BITCODE_MAGIC_NUMBER, 2) == 0)
    	type = SourceType::LLVM_IR_BIN;
    else if(memcmp(buffer, SPIRV_MAGIC_NUMBER_LITTLE_ENDIAN, 4) == 0 || memcmp(buffer, SPIRV_MAGIC_NUMBER_BIG_ENDIAN, 4) == 0)
        type = SourceType::SPIRV_BIN;
    else if(std::atol(buffer) == SPIRV_MAGIC_NUMBER)
        type = SourceType::SPIRV_TEXT;
    else if(memcmp(buffer, &QPUASM_MAGIC_NUMBER, 4) == 0 || memcmp(buffer, &QPUASM_NUMBER_MAGIC, 4) == 0)
        type = SourceType::QPUASM_BIN;
    else if(std::atol(buffer) == QPUASM_MAGIC_NUMBER || std::atol(buffer) == QPUASM_NUMBER_MAGIC)
        type = SourceType::QPUASM_HEX;
    else if(s.find_first_of(" \n\t") != std::string::npos || s.find("kernel") != std::string::npos)
        //XXX better check
        type = SourceType::OPENCL_C;

    //reset flags (e.g. if we were at the end of the file)
    stream.clear();
    //reset stream position
    stream.seekg(0);

    return type;
}

void Precompiler::linkSourceCode(const std::unordered_map<std::istream*, Optional<std::string>>& inputs, std::ostream& output)
{
#ifndef SPIRV_HEADER
	throw CompilationError(CompilationStep::LINKER, "SPIR-V front-end is not provided!");
	//TODO also allow to link via llvm-link for "normal" LLVM (or generally link with (SPIR-V) LLVM?)
	//currently fails for "arm_get_core_id" being defined twice
#else
	std::vector<std::istream*> convertedInputs;
	std::vector<std::unique_ptr<std::istream>> conversionBuffer;
	for(auto& pair : inputs)
	{
		const SourceType type = getSourceType(*pair.first);
		if(type == SourceType::SPIRV_BIN)
		{
			convertedInputs.push_back(pair.first);
		}
		else
		{
			Precompiler comp(*pair.first, type, pair.second);
			conversionBuffer.emplace_back(new std::stringstream());
			comp.run(conversionBuffer.back(), SourceType::SPIRV_BIN);
			convertedInputs.push_back(conversionBuffer.back().get());
		}
	}

	logging::debug() << "Linking " << inputs.size() << " input modules..." << logging::endl;
	spirv2qasm::linkSPIRVModules(convertedInputs, output);
#endif
}

static std::string buildCommand(const std::string& compiler, const std::string& defaultOptions, const std::string& options, const std::string& emitter, const std::string& outputFile, const std::string& inputFile = "-")
{
	//check validity of options - we do not support all of them
	if(options.find("-create-library") != std::string::npos)
		throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid compilation options", options);

	//build command-string
	std::string command;
	command.append(compiler).append(" ").append(defaultOptions).append(" ").append(options).append(" ");

	//append default options
	if(options.find("-O") == std::string::npos)
	{
		//unroll loops, pre-calculate constants, inline functions, ...
		command.append("-O3 ");
	}

#if defined USE_CLANG_OPENCL || defined SPIRV_CLANG_PATH
	if(options.find("-cl-std") == std::string::npos)
	{
		//build OpenCL 1.2
		command.append("-cl-std=CL1.2 ");
	}
	if(options.find("-cl-kernel-arg-info") == std::string::npos)
	{
		//make sure infos for arguments are generated
		command.append("-cl-kernel-arg-info ");
	}
	if(options.find("-cl-single-precision-constant") == std::string::npos)
	{
		//suppressed warnings about double constants
		command.append("-cl-single-precision-constant ");
	}
#endif
	//link in our standard-functions
	command.append(" -Wno-undefined-inline -Wno-unused-parameter -Wno-unused-local-typedef -Wno-gcc-compat ");
	command.append("-include-pch " VC4CL_STDLIB_HEADER " ");
	if(options.find("-x cl") == std::string::npos)
	{
		//build OpenCL, required when input is from stdin, since clang can't determine from file-type
		command.append("-x cl ");
	}
	//use temporary file as output
	//use stdin as input
	return command.append(emitter).append(" -o ").append(outputFile).append(" ").append(inputFile);
}

static void runPrecompiler(const std::string& command, std::istream* inputStream, std::ostream* outputStream, const Optional<std::string>& tempFile)
{
	std::ostringstream stderr;
	int status = runProcess(command, inputStream, outputStream, &stderr);
	if(status == 0)	//success
	{
		if(!stderr.str().empty())
		{
			logging::warn() << "Warnings in precompilation:" << logging::endl;
			logging::warn() << stderr.str() << logging::endl;
		}
		return;
	}
	if(!stderr.str().empty())
	{
		logging::error() << "Errors in precompilation:" << logging::endl;
		logging::error() << stderr.str() << logging::endl;
	}
	throw CompilationError(CompilationStep::PRECOMPILATION, "Error in precompilation", stderr.str());
}

static void compileOpenCLToLLVMIR(std::istream& input, std::ostream& output, const std::string& options, const bool toText = true, const Optional<std::string>& inputFile = {}, const Optional<std::string>& outputFile ={})
{
#if not defined SPIRV_CLANG_PATH && not defined CLANG_PATH
	throw CompilationError(CompilationStep::PRECOMPILATION, "No CLang configured for pre-compilation!");
#endif
#ifdef SPIRV_CLANG_PATH
	//just OpenCL C -> LLVM IR (but with Khronos CLang)
	const std::string compiler = SPIRV_CLANG_PATH;
	const std::string defaultOptions = "-cc1 -triple spir-unknown-unknown";
#else
	const std::string compiler = CLANG_PATH;
	const std::string defaultOptions = "-m32";
#endif
	//only run preprocessor and compilation, no linking and code-generation
	//emit LLVM IR
	const std::string command = buildCommand(compiler, defaultOptions, options, std::string("-S ").append(toText ? "-emit-llvm": "-emit-llvm-bc"), outputFile.hasValue ? outputFile.get() : "/dev/stdout", inputFile.hasValue ? inputFile.get() : "-");

	logging::info() << "Compiling OpenCL to LLVM-IR with :" << command << logging::endl;

	runPrecompiler(command, inputFile.hasValue ? nullptr : &input, outputFile.hasValue ? nullptr : &output, outputFile);
}

static void compileLLVMIRToSPIRV(std::istream& input, std::ostream& output, const std::string& options, const bool toText = false, const Optional<std::string>& inputFile = {}, const Optional<std::string>& outputFile ={})
{
#if not defined SPIRV_LLVM_SPIRV_PATH
	throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not configured, can't compile to SPIR-V!");
#elif not defined SPIRV_PARSER_HEADER
	throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-Tools not configured, can't process SPIR-V!");
#else
	std::string command = (std::string(SPIRV_LLVM_SPIRV_PATH) + (toText ? " -spirv-text" : "")) + " -o ";
	command.append(outputFile.hasValue ? outputFile.get() : "/dev/stdout").append(" ");
	command.append(inputFile.hasValue ? inputFile.get() : "/dev/stdin");

	logging::info() << "Converting LLVM-IR to SPIR-V with :" << command << logging::endl;

	runPrecompiler(command, inputFile.hasValue ? nullptr : &input, outputFile.hasValue ? nullptr : &output, outputFile);
#endif
}

static void compileOpenCLToSPIRV(std::istream& input, std::ostream& output, const std::string& options, const bool toText = false, const Optional<std::string>& inputFile = {}, const Optional<std::string>& outputFile ={})
{
#if not defined SPIRV_CLANG_PATH || not defined SPIRV_LLVM_SPIRV_PATH
	throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not configured, can't compile to SPIR-V!");
#elif not defined SPIRV_PARSER_HEADER
	throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-Tools not configured, can't process SPIR-V!");
#endif

	std::stringstream tmp;
	//1) OpenCL C -> LLVM IR BC (with Khronos CLang)
	compileOpenCLToLLVMIR(input, tmp, options, false, inputFile);
	//2) LLVM IR BC -> SPIR-V
	try
	{
		compileLLVMIRToSPIRV(tmp, output, options, toText, {}, outputFile);
	}
	catch(const CompilationError& e)
	{
		logging::warn() << "LLVM-IR to SPIR-V failed, trying to compile with the LLVM-IR front-end..." << logging::endl;
		compileOpenCLToLLVMIR(input, output, options, true, inputFile, outputFile);
	}
}

static void compileSPIRVToSPIRV(std::istream& input, std::ostream& output, const std::string& options, const bool toText = false, const Optional<std::string>& inputFile = {}, const Optional<std::string>& outputFile ={})
{
#if not defined SPIRV_LLVM_SPIRV_PATH
	throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-LLVM not configured, can't compile to SPIR-V!");
#elif not defined SPIRV_PARSER_HEADER
	throw CompilationError(CompilationStep::PRECOMPILATION, "SPIRV-Tools not configured, can't process SPIR-V!");
#else
	std::string command = (std::string(SPIRV_LLVM_SPIRV_PATH) + (toText ? " -to-text" : " -to-binary")) + " -o ";
	command.append(outputFile.hasValue ? outputFile.get() : "/dev/stdout").append(" ");
	command.append(inputFile.hasValue ? inputFile.get() : "/dev/stdin");

	logging::info() << "Converting between SPIR-V text and SPIR-V binary with :" << command << logging::endl;

	runPrecompiler(command, inputFile.hasValue ? nullptr : &input, outputFile.hasValue ? nullptr : &output, outputFile);
#endif
}

Precompiler::Precompiler(std::istream& input, const SourceType inputType, const Optional<std::string> inputFile) :
		input(input), inputType(inputType), inputFile(inputFile)
{
	//FIXME on Raspberry, errors in precompiler (sometimes) deletes /dev/stdout
	//the next compilation then replaces it with normal file
	if(inputType == SourceType::QPUASM_BIN || inputType == SourceType::QPUASM_HEX || inputType == SourceType::UNKNOWN)
		throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid input-type for pre-compilation!");
}

void Precompiler::run(std::unique_ptr<std::istream>& output, const SourceType outputType, const std::string& options, Optional<std::string> outputFile)
{
	if(outputType == SourceType::QPUASM_BIN || outputType == SourceType::QPUASM_HEX || outputType == SourceType::UNKNOWN)
		throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid output-type for pre-compilation!");

	std::string extendedOptions = options;
	if(inputFile.hasValue)
	{
		//for resolving relative includes
		char buffer[1024];
		strcpy(buffer, inputFile.get().data());
		std::string tmp = dirname(buffer);
		extendedOptions.append(" -I ").append(tmp);
	}

	if(inputType == outputType)
	{
		const std::string buffer(std::istreambuf_iterator<char>(input), {});
		output.reset(new std::istringstream(buffer));
		return;
	}

	std::ostringstream tempStream;

	if(inputType == SourceType::OPENCL_C)
	{
		if(outputType == SourceType::LLVM_IR_TEXT)
			compileOpenCLToLLVMIR(input, tempStream, extendedOptions, true, inputFile, outputFile);
		else if(outputType == SourceType::LLVM_IR_BIN)
			compileOpenCLToLLVMIR(input, tempStream, extendedOptions, false, inputFile, outputFile);
		else if(outputType == SourceType::SPIRV_BIN)
			compileOpenCLToSPIRV(input, tempStream, extendedOptions, false, inputFile, outputFile);
		else if(outputType ==SourceType::SPIRV_TEXT)
			compileOpenCLToSPIRV(input, tempStream, extendedOptions, true, inputFile, outputFile);
	}
	else if(inputType == SourceType::LLVM_IR_TEXT)
	{
		//TODO the result of this does not have the correct output-format (but can be handled by the LLVM front-end)
		tempStream << input.rdbuf();
	}
	else if(inputType == SourceType::LLVM_IR_BIN)
	{
		if(outputType == SourceType::SPIRV_BIN)
			compileLLVMIRToSPIRV(input, tempStream, extendedOptions, false, inputFile, outputFile);
		else if(outputType ==SourceType::SPIRV_TEXT)
			compileLLVMIRToSPIRV(input, tempStream, extendedOptions, true, inputFile, outputFile);
	}
	else if(inputType == SourceType::SPIRV_BIN && outputType == SourceType::SPIRV_TEXT)
		compileSPIRVToSPIRV(input, tempStream, extendedOptions, true, inputFile, outputFile);
	else if(inputType == SourceType::SPIRV_TEXT && outputType== SourceType::SPIRV_BIN)
		compileSPIRVToSPIRV(input, tempStream, extendedOptions, false, inputFile, outputFile);
	else
		throw CompilationError(CompilationStep::PRECOMPILATION, "Unhandled pre-compilation");

	logging::info() << "Compilation complete!" << logging::endl;

	output.reset(new std::istringstream(tempStream.str()));
}
