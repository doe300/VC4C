/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestStdlib.h"

#include "helper.h"

#include "test_stdlib.h"

using namespace vc4c::tools;

extern void compileFile(std::stringstream &buffer, const std::string &fileName, const std::string &options = "");

TestStdlib::TestStdlib()
	: TestEmulator(false)
{
	TEST_ADD(TestStdlib::testAsyncCopy);
	for(std::size_t i = 0; i < vc4c::test::atomicTests.size(); ++i)
	{
		TEST_ADD_TWO_ARGUMENTS(
			TestStdlib::testAtomicFunction, i, std::get<0>(vc4c::test::atomicTests.at(i)).kernelName);
	}
	for(std::size_t i = 0; i < vc4c::test::commonTests.size(); ++i)
	{
		TEST_ADD_TWO_ARGUMENTS(
			TestStdlib::testCommonFunction, i, std::get<0>(vc4c::test::commonTests.at(i)).kernelName);
	}
	for(std::size_t i = 0; i < vc4c::test::geometricTests.size(); ++i)
	{
		TEST_ADD_TWO_ARGUMENTS(
			TestStdlib::testGeometricFunction, i, std::get<0>(vc4c::test::geometricTests.at(i)).kernelName);
	}
	for(std::size_t i = 0; i < vc4c::test::integerFunctionTests.size(); ++i)
	{
		TEST_ADD_TWO_ARGUMENTS(
			TestStdlib::testIntegerFunction, i, std::get<0>(vc4c::test::integerFunctionTests.at(i)).kernelName);
	}
	for(std::size_t i = 0; i < vc4c::test::mathTests.size(); ++i)
	{
		TEST_ADD_TWO_ARGUMENTS(TestStdlib::testMathFunction, i, std::get<0>(vc4c::test::mathTests.at(i)).kernelName);
	}
	TEST_ADD(TestEmulator::printProfilingInfo);
}

void TestStdlib::testAsyncCopy()
{
	std::stringstream buffer;
	compileFile(buffer, "./testing/test_async_copy.cl");

	EmulationData data;
	data.kernelName = "test_aync_copy";
	data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
	data.workGroup.localSizes[0] = 12;
	data.module = std::make_pair("", &buffer);
	// 12 * 16 integer input
	data.parameter.emplace_back(0u, std::vector<uint32_t>{});
	data.parameter[0].second->resize(16 * 12, 0xDEADBEEF);
	// 12 * 16 integer output 1
	data.parameter.emplace_back(0u, std::vector<uint32_t>(16 * 12));
	// 12 * 16 integer output 2
	data.parameter.emplace_back(0u, std::vector<uint32_t>(16 * 12));

	const auto result = emulate(data);
	TEST_ASSERT(result.executionSuccessful);
	TEST_ASSERT_EQUALS(3u, result.results.size());

	const auto &in = *result.results.at(0).second;
	const auto &out0 = *result.results.at(1).second;
	const auto &out1 = *result.results.at(2).second;

	for(std::size_t i = 0; i < in.size(); ++i)
	{
		TEST_ASSERT_EQUALS(in[i], out0[i]);
		TEST_ASSERT_EQUALS(in[i], out1[i]);
	}
}

void TestStdlib::testAtomicFunction(std::size_t index, std::string name)
{
	auto &data = vc4c::test::atomicTests.at(index);
	testIntegerEmulation(data.first, data.second);
}

void TestStdlib::testCommonFunction(std::size_t index, std::string name)
{
	testFloatingEmulation(
		std::get<0>(vc4c::test::commonTests.at(index)), std::get<1>(vc4c::test::commonTests.at(index)), 0);
}

void TestStdlib::testGeometricFunction(std::size_t index, std::string name)
{
	testFloatingEmulation(std::get<0>(vc4c::test::geometricTests.at(index)),
		std::get<1>(vc4c::test::geometricTests.at(index)), std::get<2>(vc4c::test::geometricTests.at(index)));
}

void TestStdlib::testIntegerFunction(std::size_t index, std::string name)
{
	auto &data = vc4c::test::integerFunctionTests.at(index);
	testIntegerEmulation(data.first, data.second);
}

void TestStdlib::testMathFunction(std::size_t index, std::string name)
{
	// same code, just different test-case name to differentiate
	testFloatingEmulation(std::get<0>(vc4c::test::mathTests.at(index)), std::get<1>(vc4c::test::mathTests.at(index)),
		std::get<2>(vc4c::test::mathTests.at(index)));
}

void TestStdlib::testRelationalFunction(std::size_t index, std::string name) {}

void TestStdlib::testVectorFunction(std::size_t index, std::string name) {}