/*
 * Contains test-cases for emulator (and execution) test
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_CASES_H
#define VC4C_TEST_CASES_H

#ifdef VC4C_TOOLS_HEADER
#include VC4C_TOOLS_HEADER
#else
#include "tools.h"
#endif

#include <map>
#include <vector>

#ifndef VC4C_ROOT_PATH
#define VC4C_ROOT_PATH "./"
#endif

namespace vc4c
{
	namespace test
	{
		using namespace vc4c::tools;

		constexpr uint32_t maxExecutionCycles{1 << 16};

		template<typename T>
		std::map<uint32_t, std::vector<uint32_t>> addVector(std::map<uint32_t, std::vector<uint32_t>> input, uint32_t index, std::vector<T> values)
		{
			std::vector<uint32_t> buffer;
			buffer.reserve(values.size());
			for(T val : values)
				buffer.push_back(bit_cast<T, uint32_t>(val));
			input.emplace(index, buffer);
			return input;
		}

		template<>
		std::map<uint32_t, std::vector<uint32_t>> addVector(std::map<uint32_t, std::vector<uint32_t>> input, uint32_t index, std::vector<uint32_t> values)
		{
			input.emplace(index, values);
			return input;
		}

		template<typename T>
		std::pair<uint32_t, Optional<std::vector<uint32_t>>> toParameter(const std::vector<T>& values)
		{
			std::vector<uint32_t> buffer;
			buffer.reserve(values.size());
			for(T val : values)
				buffer.push_back(bit_cast<T, uint32_t>(val));

			return std::make_pair(0, buffer);
		}

		template<typename T>
		std::pair<uint32_t, Optional<std::vector<uint32_t>>> toScalarParameter(T val)
		{
			return std::make_pair(bit_cast<T, uint32_t>(val), Optional<std::vector<uint32_t>>{});
		}

		template<typename T>
		std::vector<T> toRange(T start, T end)
		{
			std::vector<T> out;
			for(T val = start; val != end; ++val)
				out.push_back(val);
			return out;
		}

		WorkGroupConfig toConfig(uint32_t localSizeX, uint32_t localSizeY = 1, uint32_t localSizeZ = 1, uint32_t numGroupsX = 1, uint32_t numGroupsY = 1, uint32_t numGroupsZ = 1)
		{
			WorkGroupConfig config;
			config.dimensions = 3;
			config.globalOffsets.fill(0);
			config.localSizes[0] = localSizeX;
			config.localSizes[1] = localSizeY;
			config.localSizes[2] = localSizeZ;
			config.numGroups[0] = numGroupsX;
			config.numGroups[1] = numGroupsY;
			config.numGroups[2] = numGroupsZ;

			return config;
		}

		std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> integerTests = {
				std::make_pair(
					EmulationData(VC4C_ROOT_PATH "example/fibonacci.cl", "fibonacci",
					{toScalarParameter(1u), toScalarParameter(1u), toParameter(std::vector<uint32_t>(10))},
					{}, maxExecutionCycles),
					addVector({}, 2, std::vector<uint32_t>{2, 3, 5, 8, 13, 21, 34, 55, 89, 144})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "example/test.cl", "test_llvm_ir",
					{toParameter(std::vector<uint32_t>(1))}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<uint32_t>{142})
				),
				//TODO requires v8muld support
//				std::make_pair(EmulationData(VC4C_ROOT_PATH "example/test_instructions.cl", "test_instructions",
//					{toScalarParameter(2u), toScalarParameter(4u), toScalarParameter(2.0f), toScalarParameter(4.0f), toParameter(std::vector<uint32_t>(32)), toParameter(std::vector<uint32_t>(32))}, {}, maxExecutionCycles),
//					addVector({}, 4, std::vector<uint32_t>{6, bit_cast<int32_t, uint32_t>(-2), 8, 0, 2, 4, 2, 2, 32, 0, 0, 6, 6, bit_cast<int32_t, uint32_t>(-3), 30, 3, 3, 1, 1, 0, 0, 0, 1, 1, 1, 0, 4, 8})
//				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_struct.cl", "test_struct",
					{toParameter(std::vector<uint32_t>(20)), toParameter(std::vector<uint32_t>(20))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<uint32_t>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42, 0})
				),
				//TODO requires bit-cast of different vector-sizes
//				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vector.cl", "test_copy",
//					{toParameter(std::vector<uint32_t>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}), std::make_pair(0, std::vector<uint32_t>(32))}, {}, maxExecutionCycles),
//					addVector({}, 1, std::vector<uint32_t>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16})
//				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_other.cl", "test_atomics",
					{toParameter(toRange<int32_t>(1, 12)), toParameter(toRange<int32_t>(1, 12))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<int32_t>{2, 0, 3, 5, 4, 6, 7, 8, 9, 10, 0})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_other.cl", "test_f2i",
					{toScalarParameter(1.0f), toScalarParameter(1.1f), toScalarParameter(1.5f), toScalarParameter(1.9f), toParameter(std::vector<int>(32))}, {}, maxExecutionCycles),
					addVector({}, 4, std::vector<int>{1, 1, 1, 1, -1, -1, -1, -1, 1, -1, 2, -1, 1, -1, 1, -2, 1, -1, 2, -2, 1, 1, 2, 2, -1, -1, -2, -2, 1, -1})
				),
				//TODO global data is not yet supported correctly
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_other.cl", "test_global_data",
					{toScalarParameter(1), toParameter(std::vector<int32_t>(2))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<int32_t>{2, 21})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test4",
					{toParameter(toRange<int>(0, 1024)), toParameter(std::vector<int>(1))}, {}, maxExecutionCycles * 2),
					addVector({}, 1, std::vector<int>{528896})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test8",
					{toParameter(toRange<int>(0, 1024)), toParameter(toRange<int>(0, 4096))}, {}, maxExecutionCycles * 2),
					addVector({}, 0, std::vector<int>{0, 5, 10, 15, 20, 25, 30, 35, 40})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test9",
					{toParameter(toRange<int>(0, 4096)), toParameter(toRange<int>(0, 4096))}, {}, maxExecutionCycles * 2),
					addVector({}, 0, std::vector<int>{0, 1, 2, 3, 5, 5, 6, 7, 9, 9, 10, 11, 13, 13, 14, 15})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test10",
					{toParameter(toRange<int>(0, 1024)), toParameter(toRange<int>(0, 1024))}, {}, maxExecutionCycles * 2),
					addVector({}, 0, std::vector<int>{0, 5, 10, 15, 20, 25, 30, 35, 40})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test11",
					{toParameter(toRange<int>(0, 256))}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<int>{201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211})
				)
		};

		//TODO NVIDIA/matrixMul, NVIDIA/transpose, OpenCLIPP/Arithmetic, OpenCLIPP/Logic, OpenCLIPP/Thresholding, test_vector, test_signedness, test_shuffle, test_conversions, local_private_storage

		std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> floatTests = {
//				std::make_pair(EmulationData(VC4C_ROOT_PATH "example/test_instructions.cl", "test_instructions",
//					{std::make_pair(2, Optional<std::vector<uint32_t>>{}), std::make_pair(4, Optional<std::vector<uint32_t>>{}), std::make_pair(bit_cast<float, uint32_t>(2.0f), Optional<std::vector<uint32_t>>{}), std::make_pair(bit_cast<float, uint32_t>(4.0f), Optional<std::vector<uint32_t>>{}), std::make_pair(0, std::vector<uint32_t>(32)), std::make_pair(0, std::vector<uint32_t>(32))}, {}, maxExecutionCycles),
//					addVector({}, 5, std::vector<float>{6.0f, -2.0f, 8.0f, 0.5f, 4.0f, 2.0f, 2.0f})
//				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/bugs/30_local_memory.cl", "dot3",
					{toParameter(toRange<float>(0.0f, 20.0f)), toParameter(std::vector<float>{0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f}), toParameter(std::vector<float>(24)), toParameter(std::vector<float>(16))},
					toConfig(10, 1, 1, 2, 1, 1), maxExecutionCycles),
					addVector({}, 2, std::vector<float>{0.1f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 2.1f, 1.1f, 1.2f, 1.3f, 1.4f, 1.5f, 1.6f, 1.7f, 1.8f, 1.9f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/bugs/30_local_memory.cl", "dot3_local",
					{toParameter(toRange<float>(0.0f, 20.0f)), toParameter(std::vector<float>{0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f}), toParameter(std::vector<float>(24))},
					toConfig(10, 1, 1, 2, 1, 1), maxExecutionCycles),
					addVector({}, 2, std::vector<float>{0.1f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 2.1f, 1.1f, 1.2f, 1.3f, 1.4f, 1.5f, 1.6f, 1.7f, 1.8f, 1.9f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/bugs/33_floating_point_folding.cl", "add_redundancy",
					{toParameter(std::vector<float>{5.0f})}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<float>{5.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/bugs/33_floating_point_folding.cl", "mul_redundancy",
					{toParameter(std::vector<float>{5.0f})}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<float>{0.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/NVIDIA/VectorAdd.cl", "VectorAdd",
					{toParameter(toRange<float>(0.0f, 18.0f)), toParameter(toRange<float>(0.0f, 18.0f)), toParameter(std::vector<uint32_t>(20)), toScalarParameter(16)},
					toConfig(12, 1, 1, 2, 1, 1), maxExecutionCycles),
					addVector({}, 2, std::vector<float>{0.0f, 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f, 20.0f, 22.0f, 24.0f, 26.0f, 28.0f, 30.0f, 0.0f})
				),
//				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vector.cl", "test_arithm",
//					{toScalarParameter(2.0f), toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f, 10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f}), toParameter(std::vector<float>(16))}, {}, maxExecutionCycles),
//					addVector({}, 2, std::vector<float>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f, 10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 17.0f, 18.0f, 19.0f})
//				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test1",
					{toParameter(std::vector<float>(1000)), toParameter(std::vector<float>(1000))}, {}, maxExecutionCycles),
					addVector(addVector({}, 0, std::vector<float>{-0.0f, -1.0f, -2.0f, -3.0f, -4.0f, -5.0f, -6.0f, -7.0f, -8.0f, -9.0f, -10.0f, -11.0f, -12.0f, -13.0f}), 1, std::vector<float>{-0.0f, -1.0f, -2.0f, -3.0f, -4.0f, -5.0f, -6.0f, -7.0f, -8.0f, -9.0f, -10.0f, -11.0f, -12.0f, -13.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test2",
					{toParameter(toRange<float>(1.0f, 10.0f)), toParameter(toRange<float>(1.0f, 10.0f)), toScalarParameter(7.0f), toScalarParameter(1u), toScalarParameter(6u)}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<float>{1.0f, 18.0f, 30.0f, 44.0f, 60.0f, 98.0f, 7.0f, 8.0f, 9.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test3",
					{toParameter(toRange<float>(1.0f, 801.0f)), toParameter(toRange<float>(1.0f, 801.0f)), toScalarParameter(7.0f)}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<float>{8.0f, 18.0f, 30.0f, 44.0f, 60.0f, 98.0f, 7.0f, 8.0f, 9.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test5",
					{toParameter(std::vector<float>(1024))}, {}, maxExecutionCycles * 2),
					addVector({}, 0, toRange<float>(0.0f, 1024.0f))
				)
		};

	} /* namespace test */
} /* namespace vc4c */

#endif /* VC4C_TEST_CASES_H */
