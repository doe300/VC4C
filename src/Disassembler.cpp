/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "VC4C.h"

#include "Locals.h"
#include "log.h"
#include "asm/Instruction.h"
#include "asm/KernelInfo.h"

#include <fstream>
#include <sstream>

using namespace vc4c;

static std::string readString(std::istream& binary, uint64_t stringLength)
{
	std::array<char, 1024> buffer;

	binary.read(buffer.data(), stringLength);
	const std::string name(buffer.data(), stringLength);
	uint64_t numPaddingBytes = Byte(stringLength).getPaddingTo(sizeof(uint64_t));
	//skip padding after kernel name
	binary.read(buffer.data(), numPaddingBytes);

	return name;
}

void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo, ReferenceRetainingList<Global>& globals, std::vector<std::unique_ptr<qpu_asm::Instruction>>& instructions)
{
	uint64_t tmp64;

	//skip magic number
	binary.seekg(8);

	uint64_t totalInstructions = 0;
	binary.read(reinterpret_cast<char*>(&moduleInfo.value), sizeof(moduleInfo.value));
	logging::debug() << "Extracted module with " << moduleInfo.getInfoCount() << " kernels, " << moduleInfo.getGlobalDataSize().getValue() << " words of global data and " << moduleInfo.getStackFrameSize().getValue() << " words of stack-frames" << logging::endl;

	for(uint16_t k = 0; k < moduleInfo.getInfoCount(); ++k)
	{
		qpu_asm::KernelInfo kernelInfo(4);
		binary.read(reinterpret_cast<char*>(&kernelInfo.value), sizeof(kernelInfo.value));
		binary.read(reinterpret_cast<char*>(&kernelInfo.workGroupSize), sizeof(kernelInfo.workGroupSize));
		kernelInfo.name = readString(binary, kernelInfo.getNameLength().getValue());
		totalInstructions += kernelInfo.getLength().getValue();
		logging::debug() << "Extracted kernel '" << kernelInfo.name << "' with " << kernelInfo.getParamCount() << " parameters" << logging::endl;

		for(uint16_t p = 0; p < kernelInfo.getParamCount(); ++p)
		{
			qpu_asm::ParamInfo paramInfo;
			binary.read(reinterpret_cast<char*>(&paramInfo.value), sizeof(paramInfo.value));
			paramInfo.name = readString(binary, paramInfo.getNameLength().getValue());
			paramInfo.typeName = readString(binary, paramInfo.getTypeNameLength().getValue());
			logging::debug() << "Extracted parameter '" << paramInfo.typeName << " " << paramInfo.name << logging::endl;
			kernelInfo.parameters.push_back(paramInfo);
		}
		moduleInfo.kernelInfos.push_back(kernelInfo);
	}

	//skip zero-word between kernels and globals
	binary.seekg(sizeof(uint64_t), std::ios_base::cur);

	if(moduleInfo.getGlobalDataSize().getValue() > 0)
	{
		//since we don't know the number, sizes and types of the original globals, we build a single global containing all the data
		std::vector<uint32_t> tmp;
		tmp.resize(moduleInfo.getGlobalDataSize().getValue() * 2);
		binary.read(reinterpret_cast<char*>(tmp.data()), moduleInfo.getGlobalDataSize().toBytes().getValue());

		std::shared_ptr<ComplexType> elementType(new ArrayType(TYPE_INT32, static_cast<unsigned>(moduleInfo.getGlobalDataSize().getValue()) * 2));
		const DataType type("i32[]", 1, elementType);
		globals.emplace_back("globalData", type.toPointerType(), Value(ContainerValue(), type), false);

		auto& elements = globals.begin()->value.container.elements;
		elements.reserve(tmp.size());
		for(uint32_t t : tmp)
		{
			//need to byte-swap to value
			uint32_t correctVal = ((t >> 24) & 0xFF) | ((t >> 8) & 0xFF00) | ((t << 8) & 0xFF0000) | ((t << 24) & 0xFF000000);
			elements.emplace_back(Literal(correctVal), TYPE_INT32);
		}

		logging::debug() << "Extracted " << moduleInfo.getGlobalDataSize().getValue() << " words of global data" << logging::endl;
	}

	//skip zero-word between globals and kernel-code
	binary.seekg(sizeof(uint64_t), std::ios_base::cur);

	//the remainder is kernel-code
	//we don't need to associate it to any particular kernel
	instructions.reserve(totalInstructions);

	for(uint64_t i = 0; i < totalInstructions; ++i)
	{
		binary.read(reinterpret_cast<char*>(&tmp64), sizeof(tmp64));
		qpu_asm::Instruction* instr = qpu_asm::Instruction::readFromBinary(tmp64);
		if(instr == nullptr)
			throw CompilationError(CompilationStep::GENERAL, "Unrecognized instruction", std::to_string(tmp64));
		instructions.emplace_back(instr);
		logging::debug() << instr->toASMString() << logging::endl;
	}

	logging::debug() << "Extracted " << totalInstructions << " machine-code instructions" << logging::endl;
}

static std::size_t generateOutput(std::ostream& stream, qpu_asm::ModuleInfo& moduleInfo, const ReferenceRetainingList<Global>& globals, const std::vector<std::unique_ptr<qpu_asm::Instruction>>& instructions, const OutputMode outputMode)
{
	std::size_t numBytes = moduleInfo.write(stream, outputMode, globals) * sizeof(uint64_t);

	for(const auto& instr : instructions)
	{
		switch (outputMode)
		{
		case OutputMode::ASSEMBLER:
			stream << instr->toASMString() << std::endl;
			numBytes += 0; //doesn't matter here, since the number of bytes is unused for assembler output
			break;
		case OutputMode::HEX:
			stream << instr->toHexString(true) << std::endl;
			numBytes += 8; //doesn't matter here, since the number of bytes is unused for hexadecimal output
			break;
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid output mode", std::to_string(static_cast<unsigned>(outputMode)));
		}
	}
	stream.flush();
	return numBytes;
}

std::size_t vc4c::disassembleModule(std::istream& binary, std::ostream& output, const OutputMode outputMode)
{
	if(Precompiler::getSourceType(binary) != SourceType::QPUASM_BIN)
		throw CompilationError(CompilationStep::GENERAL, "Invalid input binary for disassembling!");
	if(outputMode == OutputMode::BINARY)
	{
		output << binary.rdbuf();
		return 0;
	}

	qpu_asm::ModuleInfo moduleInfo;
	ReferenceRetainingList<Global> globals;
	std::vector<std::unique_ptr<qpu_asm::Instruction>> instructions;
	extractBinary(binary, moduleInfo, globals, instructions);

	return generateOutput(output, moduleInfo, globals, instructions, outputMode);
}

std::size_t vc4c::disassembleCodeOnly(std::istream& binary, std::ostream& output, std::size_t numInstructions, const OutputMode outputMode)
{
	uint64_t tmp64;
	std::size_t numBytes = 0;
	for(std::size_t i = 0; i < numInstructions; ++i)
	{
		binary.read(reinterpret_cast<char*>(&tmp64), sizeof(tmp64));
		qpu_asm::Instruction* instr = qpu_asm::Instruction::readFromBinary(tmp64);
		if(instr == nullptr)
			throw CompilationError(CompilationStep::GENERAL, "Unrecognized instruction", std::to_string(tmp64));
		switch (outputMode)
		{
		case OutputMode::ASSEMBLER:
			output << instr->toASMString() << std::endl;
			numBytes += 0; //doesn't matter here, since the number of bytes is unused for assembler output
			break;
		case OutputMode::HEX:
			output << instr->toHexString(true) << std::endl;
			numBytes += 8; //doesn't matter here, since the number of bytes is unused for hexadecimal output
			break;
		default:
			throw CompilationError(CompilationStep::GENERAL, "Invalid output mode", std::to_string(static_cast<unsigned>(outputMode)));
		}
	}
	return numBytes;
}

//command-line version
void disassemble(const std::string& input, const std::string& output, const OutputMode outputMode)
{
	std::unique_ptr<std::istream> inputFile;
	std::unique_ptr<std::ostream> outputFile;
	std::istream* is = nullptr;
	std::ostream* os = nullptr;

	if(input == "-" || input == "/dev/stdin")
		is = &std::cin;
	else
	{
		inputFile.reset(new std::ifstream(input, std::ios_base::in|std::ios_base::binary));
		is = inputFile.get();
	}

	if(output.empty() || output == "-" || output == "/dev/stdout")
		os = &std::cout;
	else
	{
		outputFile.reset(new std::ofstream(output));
		os = outputFile.get();
	}

	disassembleModule(*is, *os, outputMode);
}
