/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestEmulator.h"

#include "Compiler.h"
#include "Locals.h"
#include "asm/Instruction.h"
#include "asm/KernelInfo.h"
#include "helper.h"
#include "tools.h"

#include <cstring>
#include <fstream>
#include <sstream>

using namespace vc4c;
using namespace vc4c::tools;

static constexpr uint32_t maxExecutionCycles{1 << 16};

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

static std::stringstream compileFile(const std::string& fileName, const std::string& options = "")
{
	Configuration config;
	config.outputMode = OutputMode::BINARY;
	config.writeKernelInfo = true;
	std::stringstream buffer;
	std::ifstream input(fileName);
	Compiler::compile(input, buffer, config, "", fileName);
	return buffer;
}


void TestEmulator::testHelloWorld()
{
	std::stringstream buffer(compileFile("./example/hello_world.cl"));

	EmulationData data;
	data.kernelName = "hello_world";
	data.maxEmulationCycles = maxExecutionCycles;
	data.module = std::make_pair("", &buffer);
	data.workGroup.globalOffsets = {0, 0, 0};
	data.workGroup.localSizes = {8, 1, 1};
	data.workGroup.numGroups = {1, 1, 1};
	//16 characters per WI
	data.parameter.emplace_back(0u, std::vector<uint32_t>(data.calcNumWorkItems() * 16 / sizeof(uint32_t)));

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(1u, result.results.size());

	const auto& out = *result.results.front().second;

	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 16, 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 32, 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 48, 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 64, 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 80, 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 96, 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()) + 112, 16));
}

void TestEmulator::testHelloWorldVector()
{
	std::stringstream buffer(compileFile("./example/hello_world_vector.cl"));

	EmulationData data;
	data.kernelName = "hello_world";
	data.maxEmulationCycles = maxExecutionCycles;
	data.module = std::make_pair("", &buffer);
	//16 characters input
	data.parameter.emplace_back(0u, std::vector<uint32_t>(16 / sizeof(uint32_t)));
	memcpy(data.parameter[0].second->data(), "Hello World!", strlen("Hello World!"));
	//16 characters output
	data.parameter.emplace_back(0u, std::vector<uint32_t>(16 / sizeof(uint32_t)));

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(2u, result.results.size());

	const auto& in = *result.results.front().second;
	const auto& out = *result.results.back().second;

	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(in.data()), 16));
	TEST_ASSERT_EQUALS(0, strncmp("Hello World!", reinterpret_cast<const char*>(out.data()), 16));
}

void TestEmulator::testPrime()
{
	std::stringstream buffer(compileFile("./example/test_prime.cl"));
	//TODO needs v8adds with arbitrary value to be implemented
}

void TestEmulator::testBarrier()
{
	std::stringstream buffer(compileFile("./testing/test_barrier.cl"));

	EmulationData data;
	data.kernelName = "test_barrier";
	data.maxEmulationCycles = maxExecutionCycles;
	data.module = std::make_pair("", &buffer);
	data.workGroup.localSizes = {8, 1, 1};
	data.workGroup.numGroups = {2, 1, 1};

	//output parameter has size: 12 * sizes
	data.parameter.emplace_back(0u, std::vector<uint32_t>(12  * data.calcNumWorkItems()));

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(1u, result.results.size());

	const auto& out = *result.results.front().second;

	for(uint32_t i = 0; i < data.calcNumWorkItems(); ++i)
	{
		TEST_ASSERT_EQUALS(0u, out[0 + i * 12]);
		TEST_ASSERT_EQUALS(1u, out[1 + i * 12]);
		TEST_ASSERT_EQUALS(2u, out[2 + i * 12]);
		TEST_ASSERT_EQUALS(4u, out[4 + i * 12]);
		TEST_ASSERT_EQUALS(5u, out[5 + i * 12]);
		TEST_ASSERT_EQUALS(6u, out[6 + i * 12]);
		TEST_ASSERT_EQUALS(7u, out[7 + i * 12]);
		TEST_ASSERT_EQUALS(8u, out[8 + i * 12]);
		TEST_ASSERT_EQUALS(9u, out[9 + i * 12]);
		TEST_ASSERT_EQUALS(10u, out[10 + i * 12]);
	}
}

void TestEmulator::testBranches()
{
	std::stringstream buffer(compileFile("./testing/test_branches.cl"));

	EmulationData data;
	data.kernelName = "test_branches";
	data.maxEmulationCycles = maxExecutionCycles;
	data.module = std::make_pair("", &buffer);

	//input value 512
	data.parameter.emplace_back(0u, std::vector<uint32_t>{});
	data.parameter.back().second->push_back(512);
	//output parameter (12 entries)
	data.parameter.emplace_back(0u, std::vector<uint32_t>(16));

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(2u, result.results.size());

	const auto& out = *result.results.back().second;

	TEST_ASSERT_EQUALS(512u, out[2]);
	TEST_ASSERT_EQUALS(100u, out[3]);
	TEST_ASSERT_EQUALS(100u, out[4]);
	TEST_ASSERT_EQUALS(512u, out[5]);
	TEST_ASSERT_EQUALS(109u, out[7]);
	TEST_ASSERT_EQUALS(109u, out[0]);
	TEST_ASSERT_EQUALS(1849u, out[1]);
}

void TestEmulator::testWorkItem()
{
	std::stringstream buffer(compileFile("./testing/test_work_item.cl"));

	EmulationData data;
	data.kernelName = "test_work_item";
	data.maxEmulationCycles = maxExecutionCycles;
	data.module = std::make_pair("", &buffer);
	data.workGroup.dimensions = 3;
	data.workGroup.globalOffsets = {0, 0, 0};
	data.workGroup.localSizes = {8, 1, 1};
	data.workGroup.numGroups = {4, 1, 1};
	//output buffer: 24 * work-items
	data.parameter.emplace_back(0, std::vector<uint32_t>(24 * data.calcNumWorkItems()));

	//FIXME test with global offset != 0

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(1u, result.results.size());

	const auto& out = *result.results.front().second;

	for(uint32_t xGroup = 0; xGroup < data.workGroup.numGroups.at(0); ++xGroup)
	{
		for(uint32_t xItem = 0; xItem < data.workGroup.localSizes.at(0); ++xItem)
		{
			////global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
			auto globalID = data.workGroup.globalOffsets.at(0) + (xGroup * data.workGroup.localSizes.at(0) + xItem);
			const uint32_t* base = out.data() + (globalID * 24);

			TEST_ASSERT_EQUALS(3u, base[0]);
			TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(0) * data.workGroup.localSizes.at(0), base[1]);
			TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(1) * data.workGroup.localSizes.at(1), base[2]);
			TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(2) * data.workGroup.localSizes.at(2), base[3]);
			TEST_ASSERT_EQUALS(globalID, base[4]);
			TEST_ASSERT_EQUALS(0u, base[5]);
			TEST_ASSERT_EQUALS(0u, base[6]);
			TEST_ASSERT_EQUALS(data.workGroup.globalOffsets.at(0), base[7]);
			TEST_ASSERT_EQUALS(0u, base[8]);
			TEST_ASSERT_EQUALS(0u, base[9]);
			TEST_ASSERT_EQUALS(data.workGroup.numGroups.at(0), base[10]);
			TEST_ASSERT_EQUALS(1u, base[11]);
			TEST_ASSERT_EQUALS(1u, base[12]);
			TEST_ASSERT_EQUALS(xGroup, base[13]);
			TEST_ASSERT_EQUALS(0u, base[14]);
			TEST_ASSERT_EQUALS(0u, base[15]);
			TEST_ASSERT_EQUALS(data.workGroup.localSizes.at(0), base[16]);
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
	std::stringstream buffer(compileFile("./testing/bugs/30_local_memory.cl"));

	EmulationData data;
	data.kernelName = "dot3";
	data.maxEmulationCycles = maxExecutionCycles;
	data.module = std::make_pair("", &buffer);
	data.workGroup.localSizes = {8, 1, 1};
	data.workGroup.numGroups = {4, 1, 1};

	//num inputs = 2 * globalSize + localSize
	data.parameter.emplace_back(0, std::vector<uint32_t>(data.calcNumWorkItems()));
	data.parameter.emplace_back(0, std::vector<uint32_t>(data.calcNumWorkItems()));
	//num outputs = globalSize
	data.parameter.emplace_back(0, std::vector<uint32_t>(data.calcNumWorkItems()));
	data.parameter.emplace_back(0, std::vector<uint32_t>(data.calcNumWorkItems()));

	//fill parameters
	for(uint32_t i = 0; i < data.calcNumWorkItems(); ++i)
	{
		reinterpret_cast<float*>(data.parameter[0].second->data())[i] = static_cast<float>(i);
		reinterpret_cast<float*>(data.parameter[1].second->data())[i] = 0.1f;
	}

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(4u, result.results.size());

	//TODO local parameter as well as output are all-zero
	for(uint32_t i = 0; i < data.calcNumWorkItems(); ++i)
		std::cout << (reinterpret_cast<const float*>(result.results[2].second->data())[i]) << std::endl;

	//TODO also test __local memory version
}
