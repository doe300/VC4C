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

#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <fstream>

using namespace vc4c;
using namespace vc4c::tools;

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

static void printHelp()
{
	std::cout << "Usage: emulator [-k <kernel-name>] [-d <dump-file>] [-l <local-sizes>] [-g <global-sizes>] [args] input-file" << std::endl;
	std::cout << "\t-k <kernel-name>\tSpecifies the kernel to run, defaults to the first/only kernel in the module" << std::endl;
	std::cout << "\t-d <dump-file>\t\tWrites the memory contents into the file specified, before and after the execution" << std::endl;
	std::cout << "\t-l <local-sizes>\tUses the given local sizes in the format \"x y z\" (3 parameter), defaults to single execution" << std::endl;
	std::cout << "\t-g <num-groups>\t\tUses the given number of work-groups in the format \"x y z\" (3 parameter), defaults to single execution" << std::endl;
	std::cout << "\t-i <dump-file>\t\tWrites the result of the instrumentation into the file specified" << std::endl;
	std::cout << "\t-h, --help\t\tPrint this help message" << std::endl;
	std::cout << "[args] specify the values for the input parameters and can take following values:" << std::endl;
	std::cout << "\t-f <file-name>\t\tRead <file-name> as binary file" << std::endl;
	std::cout << "\t-s <string>\t\tUse <string> as input string" << std::endl;
	std::cout << "\t-b <num>\t\tAllocate anempty buffer with <num> words of size" << std::endl;
	std::cout << "\t<data>\t\t\tUse <data> as input word" << std::endl;
}

int main(int argc, char** argv)
{
#if DEBUG_MODE
    setLogger(std::wcout, true, LogLevel::DEBUG);
#else
    setLogger(std::wcout, true, LogLevel::WARNING);
#endif

	if(argc == 1 || (argc == 2 && (std::string("-h") == argv[1] || std::string("--help") == argv[1])))
	{
		printHelp();
		return 0;
	}

	EmulationData data;

	data.workGroup.dimensions = 1;
	data.workGroup.globalOffsets = { 0, 0, 0};
	data.workGroup.localSizes = {1, 1, 1};
	data.workGroup.numGroups = {1, 1, 1};

	for(int i = 1; i < argc - 1; ++i)
	{
		if(std::string("-h") == argv[i] || std::string("--help") == argv[i])
		{
			printHelp();
			return 0;
		}
		else if(std::string("-l") == argv[i])
		{
			++i;
			data.workGroup.localSizes.at(0) = static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0));
			++i;
			data.workGroup.localSizes.at(1) = static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0));
			++i;
			data.workGroup.localSizes.at(2) = static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0));
			data.workGroup.dimensions = 3;
		}
		else if(std::string("-g") == argv[i])
		{
			++i;
			data.workGroup.numGroups.at(0) = static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0));
			++i;
			data.workGroup.numGroups.at(1) = static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0));
			++i;
			data.workGroup.numGroups.at(2) = static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0));
			data.workGroup.dimensions = 3;
		}
		else if(std::string("-d") == argv[i])
		{
			++i;
			data.memoryDump = argv[i];
		}
		else if(std::string("-k") == argv[i])
		{
			++i;
			data.kernelName = argv[i];
		}
		else if(std::string("-i") == argv[i])
		{
			++i;
			data.instrumentationDump = argv[i];
		}
		else if(std::string("-f") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, readBinaryFile(argv[i]));
		}
		else if(std::string("-s") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, readDirectData(argv[i]));
		}
		else if(std::string("-b") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, std::vector<tools::Word>(std::strtol(argv[i], nullptr, 0), 0x0));
		}
		else
			data.parameter.emplace_back(static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0)), Optional<std::vector<uint32_t>>{});
	}

	std::ifstream input(argv[argc - 1]);
	data.module = std::make_pair("", &input);

	logging::info() << "Running emulator with " << data.parameter.size() << " parameters on kernel " << data.kernelName << logging::endl;
	emulate(data);

	return 0;
}
