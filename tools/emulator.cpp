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
#include "Profiler.h"

#include "log.h"

#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <limits>
#include <sstream>
#include <type_traits>

using namespace vc4c;
using namespace vc4c::tools;

enum class BufferType
{
	INT, FLOAT, BINARY, CHARACTER
};

void printValue(uint32_t val, BufferType type)
{
	switch(type)
	{
	case BufferType::INT:
	{
		int i = *reinterpret_cast<int*>(&val);
		std::cout << i;
		break;
	}
	case BufferType::FLOAT:
	{
		float f = *reinterpret_cast<float*>(&val);
		std::cout << f;
		break;
	}
	case BufferType::CHARACTER:
	{
		std::array<char, 4> c;
		memcpy(c.data(), &val, c.size());
		std::cout << c[0] << c[1] << c[2] << c[3];
		break;
	}
	case BufferType::BINARY:
	default:
	{
		std::cout << "0x" << std::hex << val << std::dec;
	}
	}
}

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

	tools::Word currentWord{};
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

template<typename T>
struct HexHelper : public std::make_unsigned<T> {};

template<>
struct HexHelper<float> {
	using type = float;
};

template<typename T>
static std::vector<tools::Word> readDirectBuffer(std::string data)
{
	std::vector<tools::Word> words;
	std::stringstream ss(data);
	T t = 0;
	while((ss.peek() == 'i') || (ss.peek() == 'n') || (ss >> t))
	{
		if(ss.peek() == 'i' || ss.peek() == 'n')
		{
			// inf / nan
			std::string tmp;
			ss >> tmp;
			if(tmp == "inf")
				t = std::numeric_limits<T>::infinity();
			else if(tmp == "nan")
				t = std::numeric_limits<T>::quiet_NaN();
		}
		else if(t == 0 && ss.peek() == 'x')
		{
			// skip x in (0x...)
			ss.get();
			using HexType = typename HexHelper<T>::type;
			HexType tmp = 0;
			// read number as hexadecimal unsigned
			ss >> std::hex >> tmp >> std::dec;
			t = bit_cast<HexType, T>(tmp);
		}
		words.emplace_back(bit_cast<T, uint32_t>(t));
		while(ss.peek() == ' ')
			ss.get();
	}
	
	return words;
}

static void printHelp()
{
	std::cout << "Usage: emulator [-k <kernel-name>] [-d <dump-file>] [-l <local-sizes>] [-g <global-sizes>] [args] input-file" << std::endl;
	std::cout << "\t-k <kernel-name>\tSpecifies the kernel to run, defaults to the first/only kernel in the module" << std::endl;
	std::cout << "\t-d <dump-file>\t\tWrites the memory contents into the file specified, before and after the execution" << std::endl;
	std::cout << "\t-l <local-sizes>\tUses the given local sizes in the format x y z (3 parameter), defaults to single execution" << std::endl;
	std::cout << "\t-g <num-groups>\t\tUses the given number of work-groups in the format x y z (3 parameter), defaults to single execution" << std::endl;
	std::cout << "\t-i <dump-file>\t\tWrites the result of the instrumentation into the file specified" << std::endl;
	std::cout << "\t-o <number>\t\tSpecifies the given parameter index as output and prints it when finished" << std::endl;
	std::cout << "\t-h, --help\t\tPrint this help message" << std::endl;
	std::cout << "\t-q, --quiet\t\tQuiet all debug output" << std::endl;
	std::cout << "\t--verbose\t\tPrint verbose debug output" << std::endl;
	std::cout << "[args] specify the values for the input parameters and can take following values:" << std::endl;
	std::cout << "\t-f <file-name>\t\tRead <file-name> as binary file" << std::endl;
	std::cout << "\t-s <string>\t\tUse <string> as input string" << std::endl;
	std::cout << "\t-b <num>\t\tAllocate an empty buffer with <num> words of size" << std::endl;
	std::cout << "\t-ib <values>\t\tAllocate a buffer containing the given values. The values are passed space-separated inside a string (double-quotes, e.g. \"0 1 2 3 ...\")" << std::endl;
	std::cout << "\t-fb <values>\t\tAllocate a buffer containing the given values. The values are passed space-separated inside a string (double-quotes, e.g. \"0.0 1.0 2.0 3.0 ...\")" << std::endl;
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

	int outParam = -1;
	std::vector<BufferType> bufferTypes;

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
			bufferTypes.push_back(BufferType::BINARY);
		}
		else if(std::string("-s") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, readDirectData(argv[i]));
			bufferTypes.push_back(BufferType::CHARACTER);
		}
		else if(std::string("-b") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, std::vector<tools::Word>(std::strtol(argv[i], nullptr, 0), 0x0));
			bufferTypes.push_back(BufferType::BINARY);
		}
		else if(std::string("-ib") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, readDirectBuffer<int>(argv[i]));
			bufferTypes.push_back(BufferType::INT);
		}
		else if(std::string("-fb") == argv[i])
		{
			++i;
			data.parameter.emplace_back(0u, readDirectBuffer<float>(argv[i]));
			bufferTypes.push_back(BufferType::FLOAT);
		}
		else if(std::string("-o") == argv[i])
		{
			++i;
			outParam = std::atoi(argv[i]);
		}
		else if(std::string("-q") == argv[i] || std::string("--quiet") == argv[i])
		{
			setLogger(std::wcout, true, LogLevel::WARNING);
		}
		else if(std::string("--verbose") == argv[i])
		{
			setLogger(std::wcout, true, LogLevel::DEBUG);
		}
		else
			//TODO hexadecimal support, float support
			data.parameter.emplace_back(static_cast<tools::Word>(std::strtol(argv[i], nullptr, 0)), Optional<std::vector<uint32_t>>{});
	}

	std::ifstream input(argv[argc - 1]);
	data.module = std::make_pair("", &input);

	logging::info() << "Running emulator with " << data.parameter.size() << " parameters on kernel " << data.kernelName << logging::endl;
	auto result = emulate(data);
	if(outParam >= 0 && outParam < result.results.size())
	{
		std::cout << "Result (buffer " << outParam << "): ";
		const auto& out = result.results[outParam];
		if(out.second)
		{
			std::for_each(out.second->begin(), out.second->end(), [&bufferTypes, outParam](uint32_t val) {
				printValue(val, bufferTypes[outParam]);
				std::cout << " ";
			});
			std::cout << "(" << out.second->size() << " entries)" << std::endl;
		}
		else
		{
			printValue(out.first, bufferTypes[outParam]);
			std::cout << std::endl;
		}
	}
	
#ifdef DEBUG_MODE
	vc4c::profiler::dumpProfileResults(true);
#endif

	return 0;
}
