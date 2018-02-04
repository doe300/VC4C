/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Compiler.h"
#include "Locals.h"
#include "tools/Emulator.h"
#include "asm/Instruction.h"
#include "asm/KernelInfo.h"

#include "log.h"

#include <cstring>
#include <iomanip>
#include <iostream>
#include <fstream>

using namespace vc4c;
using namespace vc4c::tools;

extern void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo, ReferenceRetainingList<Global>& globals, std::vector<std::unique_ptr<qpu_asm::Instruction>>& instructions);

static std::vector<tools::Word> readBinaryFile(std::string fileName)
{
	std::ifstream s(fileName);
	std::vector<tools::Word> res;

	tools::Word word;
	while(s.read(reinterpret_cast<char*>(&word), sizeof(tools::Word)))
	{
		res.push_back(word);
	}

	return res;
}

static std::vector<tools::Word> readDirectData(std::string data)
{
	std::vector<tools::Word> words;

	tools::Word currentWord;
	for(std::size_t i = 0; i < data.size(); ++i)
	{
		currentWord |= static_cast<tools::Word>(data.at(i)) << ((i % 4) * 8);
		if(i % 4 == 3)
		{
			words.push_back(currentWord);
			currentWord = 0;
		}
	}
	// last word
	if(data.size() % sizeof(tools::Word) != 0)
		words.push_back(currentWord);

	return words;
}

static void dumpMemory(const Memory& memory, const std::string& fileName, MemoryAddress uniformAddress, bool before)
{
	std::ofstream f(fileName, !before ? std::ios::app : std::ios::trunc);
	if(before)
		f << "Before: " << std::endl;
	else
		f << std::endl << "After: " << std::endl;
	MemoryAddress addr = 0;
	while(addr != memory.getMaximumAddress())
	{
		if(uniformAddress == addr)
			f << "Uniforms: " << std::endl;
		if(addr % (sizeof(tools::Word) * 8) == 0)
			f << std::hex << "0x" << addr << "\t";
		f << " " << std::hex << std::setfill('0') << std::setw(8) << static_cast<tools::Word>(memory.readWord(addr).getLiteralValue()->integer);
		if(addr % (sizeof(tools::Word) * 8) == (sizeof(tools::Word) * 7))
			f << std::endl;
		addr += sizeof(tools::Word);
	}
	f << std::endl;
	logging::debug() << std::dec << "Dumped " << addr << " words of memory into " << fileName << logging::endl;
}


int main(int argc, char** argv)
{
#if DEBUG_MODE
    setLogger(std::wcout, true, LogLevel::DEBUG);
#else
    setLogger(std::wcout, true, LogLevel::WARNING);
#endif

	if(argc == 1)
	{
		std::cout << "Usage: emulator [-k <kernel-name>] [-d <dump-file>] [-l <local-sizes>] [-g <global-sizes>] [args] input-file" << std::endl;
		std::cout << "\t-k <kernel-name>\t\tSpecifies the kernel to run, defaults to the first/only kernel in the module" << std::endl;
		std::cout << "\t-d <dump-file>\t\tWrites the memory contents into the file specified, before and after the execution" << std::endl;
		std::cout << "\t-l <local-sizes>\t\tUses the given local sizes in the format \"x y z\" (3 parameter), defaults to single execution" << std::endl;
		std::cout << "\t-g <num-groups>\t\tUses the given number of work-groups in the format \"x y z\" (3 parameter), defaults to single execution" << std::endl;
		std::cout << "[args] specify the values for the input parameters and can take following values:" << std::endl;
		std::cout << "\t-f <file-name>\t\tRead <file-name> as binary file" << std::endl;
		std::cout << "\t-s <string>\t\tUse <string> as input string" << std::endl;
		std::cout << "\t-b <num>\t\tAllocate <num> words of buffer" << std::endl;
		std::cout << "\t<data>\t\t\tUse <data> as input word" << std::endl;
		return 0;
	}

	std::string memoryDumpFile;
	std::vector<std::vector<tools::Word>> parameterValues;
	std::string kernelName;
	std::size_t memorySize = 0;

	WorkGroupConfig config;
	config.dimensions = 1;
	config.globalOffsets = { 0, 0, 0};
	config.localSizes = {1, 1, 1};
	config.numGroups = {1, 1, 1};

	for(int i = 1; i < argc - 1; ++i)
	{
		if(std::string("-l") == argv[i])
		{
			++i;
			config.localSizes.at(0) = static_cast<tools::Word>(std::atol(argv[i]));
			++i;
			config.localSizes.at(1) = static_cast<tools::Word>(std::atol(argv[i]));
			++i;
			config.localSizes.at(2) = static_cast<tools::Word>(std::atol(argv[i]));
			config.dimensions = 3;
		}
		else if(std::string("-g") == argv[i])
		{
			++i;
			config.numGroups.at(0) = static_cast<tools::Word>(std::atol(argv[i]));
			++i;
			config.numGroups.at(1) = static_cast<tools::Word>(std::atol(argv[i]));
			++i;
			config.numGroups.at(2) = static_cast<tools::Word>(std::atol(argv[i]));
			config.dimensions = 3;
		}
		else if(std::string("-d") == argv[i])
		{
			++i;
			memoryDumpFile = argv[i];
		}
		else if(std::string("-k") == argv[i])
		{
			++i;
			kernelName = argv[i];
		}
		else if(std::string("-f") == argv[i])
		{
			++i;
			parameterValues.push_back(readBinaryFile(argv[i]));
			memorySize += parameterValues.back().size();
		}
		else if(std::string("-s") == argv[i])
		{
			++i;
			parameterValues.push_back(readDirectData(argv[i]));
			memorySize += parameterValues.back().size();
		}
		else if(std::string("-b") == argv[i])
		{
			++i;
			parameterValues.emplace_back(std::vector<tools::Word>(std::atol(argv[i]), 0x0));
			memorySize += parameterValues.back().size();
		}
		else
			parameterValues.emplace_back(std::vector<tools::Word>{static_cast<tools::Word>(std::atol(argv[i]))});
	}

	std::ifstream input(argv[argc - 1]);
	qpu_asm::ModuleInfo moduleInfo;
	ReferenceRetainingList<Global> globals;
	std::vector<std::unique_ptr<qpu_asm::Instruction>> instructions;

	extractBinary(input, moduleInfo, globals, instructions);

	if(moduleInfo.kernelInfos.empty())
	{
		std::cout << "No kernels found in module, aborting" << std::endl;
		return 0;
	}

	const qpu_asm::KernelInfo* kernel = &moduleInfo.kernelInfos.front();
	if(!kernelName.empty())
	{
		for(const auto& kernelInfo : moduleInfo.kernelInfos)
		{
			if(kernelInfo.name == kernelName)
			{
				kernel = &kernelInfo;
				break;
			}
		}
	}

	if(parameterValues.size() != kernel->parameters.size())
	{
		std::cerr << "The number of parameters specified does not match the number of kernel arguments, aborting!" << std::endl;
		return 1;
	}

	auto numReruns = config.numGroups.at(0) * config.numGroups.at(1) * config.numGroups.at(2);
	std::size_t numUniformWords = (config.localSizes.at(0) * config.localSizes.at(1) * config.localSizes.at(2)) * (14 + parameterValues.size()) * numReruns;

	Memory memory(memorySize + moduleInfo.getGlobalDataSize().toBytes().getValue() / sizeof(tools::Word) + numUniformWords);
	std::vector<MemoryAddress> parameters;
	parameters.reserve(parameterValues.size());

	//fill Memory
	MemoryAddress currentAddress = 0;
	//TODO write global data
	for(std::size_t i = 0; i < kernel->parameters.size(); ++i)
	{
		if(kernel->parameters.at(i).getPointer())
		{
			parameters.push_back(currentAddress);
			memcpy(memory.getWordAddress(currentAddress), parameterValues.at(i).data(), parameterValues.at(i).size() * sizeof(tools::Word));
			logging::debug() << "Writing parameter " << i << " into memory at address: " << currentAddress << logging::endl;
			currentAddress += static_cast<MemoryAddress>(parameterValues.at(i).size() * sizeof(tools::Word));
			//align to word-boundary
			if(currentAddress % sizeof(tools::Word) != 0)
				currentAddress += static_cast<MemoryAddress>(sizeof(tools::Word) - (currentAddress % sizeof(tools::Word)));
		}
		else
			parameters.push_back(parameterValues.at(i).at(0));
	}

	//align to boundary of memory dump
	if(currentAddress % (sizeof(tools::Word) * 8) != 0)
		currentAddress += static_cast<MemoryAddress>((sizeof(tools::Word) * 8) - (currentAddress % (sizeof(tools::Word) * 8)));

	logging::info() << "Running emulator with " << parameterValues.size() << " parameters on kernel " << kernel->name << " with " << kernel->getLength().getValue() << " instructions" << logging::endl;

	if(!memoryDumpFile.empty())
		dumpMemory(memory, memoryDumpFile, currentAddress, true);

	const auto uniformAddresses = buildUniforms(memory, currentAddress, parameters, config, 0);
	emulate(instructions.begin() + (kernel->getOffset() - moduleInfo.kernelInfos.front().getOffset()).getValue(), memory, uniformAddresses);

	if(!memoryDumpFile.empty())
		dumpMemory(memory, memoryDumpFile, currentAddress, false);

	return 0;
}
