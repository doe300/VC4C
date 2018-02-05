/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestEmulator.h"

#include "Compiler.h"
#include "Locals.h"
#include "tools/Emulator.h"
#include "asm/Instruction.h"
#include "asm/KernelInfo.h"

#include <cstring>
#include <fstream>
#include <sstream>

using namespace vc4c;
using namespace vc4c::tools;

static constexpr uint32_t maxExecutionCycles{1 << 16};

extern void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo, ReferenceRetainingList<Global>& globals, std::vector<std::unique_ptr<qpu_asm::Instruction>>& instructions);

TestEmulator::TestEmulator()
{
	TEST_ADD(TestEmulator::testHelloWorld);
	TEST_ADD(TestEmulator::testHelloWorldVector);
	TEST_ADD(TestEmulator::testPrime);
	TEST_ADD(TestEmulator::testBarrier);
	TEST_ADD(TestEmulator::testBranches);
	TEST_ADD(TestEmulator::testWorkItem);
	TEST_ADD(TestEmulator::testBug30);
}

static std::vector<std::unique_ptr<qpu_asm::Instruction>> compileFile(const std::string& fileName, qpu_asm::ModuleInfo& moduleInfo, ReferenceRetainingList<Global>& globals)
{
	Configuration config;
	config.outputMode = OutputMode::BINARY;
	config.writeKernelInfo = true;
	std::stringstream buffer;
	std::ifstream input(fileName);
	Compiler::compile(input, buffer, config, "", fileName);
	std::vector<std::unique_ptr<qpu_asm::Instruction>> instructions;
	extractBinary(buffer, moduleInfo, globals, instructions);
	return instructions;
}


void TestEmulator::testHelloWorld()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./example/hello_world.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("hello_world"), module.kernelInfos.front().name);

	WorkGroupConfig config;
	config.globalOffsets = {0, 0, 0};
	config.localSizes = {8, 1, 1};
	config.numGroups = {1, 1, 1};

	const std::vector<MemoryAddress> outLocs = {0, 16, 32, 48, 64, 80, 96, 112};

	Memory memory(512);
	auto uniformAddresses = buildUniforms(memory, 128, outLocs, config, 0);

	emulate(instructions.begin(), memory, uniformAddresses, maxExecutionCycles);

	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(0)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(16)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(32)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(48)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(64)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(80)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(96)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(112)), 16));
}

void TestEmulator::testHelloWorldVector()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./example/hello_world_vector.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("hello_world"), module.kernelInfos.front().name);

	Memory memory(64);
	memcpy(memory.getWordAddress(0), "Hello World!", strlen("Hello World!"));

	emulateTask(instructions.begin(), {0, 16}, memory, 32 * sizeof(tools::Word), 0, maxExecutionCycles);

	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(0)), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(memory.getWordAddress(16)), 16));
}

void TestEmulator::testPrime()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./example/test_prime.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("test_prime"), module.kernelInfos.front().name);

	Memory memory(64);

	//TODO needs v8adds with arbitrary value to be implemented

//	emulateTask(instructions.begin(), {3, 0}, memory, 16 * sizeof(tools::Word), 0);
//	TEST_ASSERT_EQUALS(true, *reinterpret_cast<const bool*>(memory.getWordAddress(0)));
//
//	emulateTask(instructions.begin(), {6, 0}, memory, 16 * sizeof(tools::Word), 0);
//	TEST_ASSERT_EQUALS(false, *reinterpret_cast<const bool*>(memory.getWordAddress(0)));
}

void TestEmulator::testBarrier()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./testing/test_barrier.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("test_barrier"), module.kernelInfos.front().name);

	WorkGroupConfig config;
	config.dimensions = 3;
	config.globalOffsets = {0, 0, 0};
	config.localSizes = {8, 1, 1};
	config.numGroups = {2, 1, 1};

	auto globalSize = config.localSizes.at(0) * config.localSizes.at(1) * config.localSizes.at(2) * config.numGroups.at(0) * config.numGroups.at(1) * config.numGroups.at(2);
	//output parameter has size: 16 * Word * sizes
	//UNIFORM have size: <= 16 * Word * sizes
	MemoryAddress uniformOffset = 16 * globalSize;
	Memory memory(2 * uniformOffset);
	auto uniformAddresses = buildUniforms(memory, uniformOffset * sizeof(tools::Word), {0}, config, 0);

	emulate(instructions.begin(), memory, uniformAddresses, maxExecutionCycles);

	for(uint32_t i = 0; i < globalSize; ++i)
	{
		tools::Word* base = memory.getWordAddress(i * 12 * sizeof(tools::Word));

		TEST_ASSERT_EQUALS(0u, base[0]);
		TEST_ASSERT_EQUALS(1u, base[1]);
		TEST_ASSERT_EQUALS(2u, base[2]);
		TEST_ASSERT_EQUALS(4u, base[4]);
		TEST_ASSERT_EQUALS(5u, base[5]);
		TEST_ASSERT_EQUALS(6u, base[6]);
		TEST_ASSERT_EQUALS(7u, base[7]);
		TEST_ASSERT_EQUALS(8u, base[8]);
		TEST_ASSERT_EQUALS(9u, base[9]);
		TEST_ASSERT_EQUALS(10u, base[10]);
	}
}

void TestEmulator::testBranches()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./testing/test_branches.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("test_branches"), module.kernelInfos.front().name);

	Memory memory(64);
	memory.getWordAddress(0)[0] = 512;

	emulateTask(instructions.begin(), {0, 16}, memory, 32 * sizeof(tools::Word), 0, maxExecutionCycles);

	TEST_ASSERT_EQUALS(512u, memory.getWordAddress(16)[2]);
	TEST_ASSERT_EQUALS(100u, memory.getWordAddress(16)[3]);
	TEST_ASSERT_EQUALS(100u, memory.getWordAddress(16)[4]);
	TEST_ASSERT_EQUALS(512u, memory.getWordAddress(16)[5]);
	TEST_ASSERT_EQUALS(109u, memory.getWordAddress(16)[7]);
	TEST_ASSERT_EQUALS(109u, memory.getWordAddress(16)[0]);
	TEST_ASSERT_EQUALS(1849u, memory.getWordAddress(16)[1]);
}

void TestEmulator::testWorkItem()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./testing/test_work_item.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("test_work_item"), module.kernelInfos.front().name);

	WorkGroupConfig config;
	config.dimensions = 3;
	config.globalOffsets = {0, 0, 0};
	config.localSizes = {8, 1, 1};
	config.numGroups = {4, 1, 1};

	//FIXME test with global offset != 0

	auto globalSize = config.localSizes.at(0) * config.localSizes.at(1) * config.localSizes.at(2) * config.numGroups.at(0) * config.numGroups.at(1) * config.numGroups.at(2);
	//num outputs = sizes * 24
	//num uniforms = sizes * 16
	Memory memory(globalSize * (24 + 16 + 17));
	auto uniformAddresses = buildUniforms(memory, globalSize * 24 * sizeof(tools::Word), {0}, config, 0);

	emulate(instructions.begin(), memory, uniformAddresses, maxExecutionCycles);

	for(uint32_t xGroup = 0; xGroup < config.numGroups.at(0); ++xGroup)
	{
		for(uint32_t xItem = 0; xItem < config.localSizes.at(0); ++xItem)
		{
			////global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
			auto globalID = config.globalOffsets.at(0) + (xGroup * config.localSizes.at(0) + xItem);
			tools::Word* base = memory.getWordAddress(sizeof(tools::Word) * globalID * 24);

			TEST_ASSERT_EQUALS(3u, base[0]);
			TEST_ASSERT_EQUALS(config.numGroups.at(0) * config.localSizes.at(0), base[1]);
			TEST_ASSERT_EQUALS(config.numGroups.at(1) * config.localSizes.at(1), base[2]);
			TEST_ASSERT_EQUALS(config.numGroups.at(2) * config.localSizes.at(2), base[3]);
			TEST_ASSERT_EQUALS(globalID, base[4]);
			TEST_ASSERT_EQUALS(0u, base[5]);
			TEST_ASSERT_EQUALS(0u, base[6]);
			TEST_ASSERT_EQUALS(config.globalOffsets.at(0), base[7]);
			TEST_ASSERT_EQUALS(0u, base[8]);
			TEST_ASSERT_EQUALS(0u, base[9]);
			TEST_ASSERT_EQUALS(config.numGroups.at(0), base[10]);
			TEST_ASSERT_EQUALS(1u, base[11]);
			TEST_ASSERT_EQUALS(1u, base[12]);
			TEST_ASSERT_EQUALS(xGroup, base[13]);
			TEST_ASSERT_EQUALS(0u, base[14]);
			TEST_ASSERT_EQUALS(0u, base[15]);
			TEST_ASSERT_EQUALS(config.localSizes.at(0), base[16]);
			TEST_ASSERT_EQUALS(1u, base[17]);
			TEST_ASSERT_EQUALS(1u, base[18]);
			TEST_ASSERT_EQUALS(xItem, base[19]);
			TEST_ASSERT_EQUALS(0u, base[20]);
			TEST_ASSERT_EQUALS(0u, base[21]);
			TEST_ASSERT_EQUALS(1u, base[22]);
		}
	}
}

void TestEmulator::testBug30()
{
	qpu_asm::ModuleInfo module;
	ReferenceRetainingList<Global> globals;
	const auto instructions = compileFile("./testing/bugs/30_local_memory.cl", module, globals);

	TEST_ASSERT_EQUALS(1u, module.kernelInfos.size());
	TEST_ASSERT(globals.empty());
	TEST_ASSERT_EQUALS(std::string("dot3"), module.kernelInfos.front().name);

	WorkGroupConfig config;
	config.dimensions = 3;
	config.globalOffsets = {0, 0, 0};
	config.localSizes = {8, 1, 1};
	config.numGroups = {4, 1, 1};

	auto globalSize = config.localSizes.at(0) * config.localSizes.at(1) * config.localSizes.at(2) * config.numGroups.at(0) * config.numGroups.at(1) * config.numGroups.at(2);
	//num inputs = 2 * globalSize + localSize
	//num outputs = globalSize
	//num uniforms = globalSize * 18
	Memory memory(globalSize * (3 + 1 + 18));

	//fill parameters
	for(uint32_t i = 0; i < globalSize; ++i)
	{
		reinterpret_cast<float*>(memory.getWordAddress(0))[i] = static_cast<float>(i);
		reinterpret_cast<float*>(memory.getWordAddress(static_cast<MemoryAddress>(globalSize * sizeof(tools::Word))))[i] = 0.1f;
	}

	auto uniformAddresses = buildUniforms(memory, static_cast<MemoryAddress>(globalSize *sizeof(tools::Word) * 4),
			{0, static_cast<MemoryAddress>(globalSize * sizeof(tools::Word)), static_cast<MemoryAddress>(globalSize * sizeof(tools::Word) * 2), static_cast<MemoryAddress>(globalSize *sizeof(tools::Word) * 3)},
			config, 0);

	emulate(instructions.begin(), memory, uniformAddresses, maxExecutionCycles);

	MemoryAddress outputBase = static_cast<MemoryAddress>(globalSize * sizeof(tools::Word) * 2);

	//TODO local parameter as well as output are all-zero
	for(uint32_t i = 0; i < globalSize; ++i)
		std::cout << (reinterpret_cast<float*>(memory.getWordAddress(outputBase))[i]) << std::endl;
}
