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
#include "helper.h"
#endif

#include <algorithm>
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
		static std::map<uint32_t, std::vector<uint32_t>> addVector(std::map<uint32_t, std::vector<uint32_t>> input, uint32_t index, std::vector<T> values)
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
		std::vector<T> toRange(T start, T end, T step = 1)
		{
			std::vector<T> out;
			for(T val = start; val != end; val += step)
				out.push_back(val);
			return out;
		}
		
		template<typename T, typename R>
		std::vector<R> transfer(std::vector<T> in, const std::function<R(T)>& func)
		{
			std::vector<R> out;
			out.resize(in.size());
			std::transform(in.begin(), in.end(), out.begin(), func);
			return out;
		}

		static WorkGroupConfig toConfig(uint32_t localSizeX, uint32_t localSizeY = 1, uint32_t localSizeZ = 1, uint32_t numGroupsX = 1, uint32_t numGroupsY = 1, uint32_t numGroupsZ = 1)
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

		static std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> integerTests = {
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
				std::make_pair(EmulationData(VC4C_ROOT_PATH "example/test_instructions.cl", "test_instructions",
					{toScalarParameter(2u), toScalarParameter(4u), toScalarParameter(2.0f), toScalarParameter(4.0f), toParameter(std::vector<uint32_t>(32)), toParameter(std::vector<uint32_t>(32))}, {}, maxExecutionCycles),
					addVector({}, 4, std::vector<uint32_t>{6, bit_cast<int32_t, uint32_t>(-2), 8, 0, 2, 4, 2, 2, 32, 0, 0, 6, 6, bit_cast<int32_t, uint32_t>(-3), 30, 3, 3, 1, 1, 0, 0, 0, 1, 1, 1, 0, 4, 8})
				),
#ifndef SPIRV_FRONTEND
				// SEGFAULT in SPIRV-LLVM used in CI
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_struct.cl", "test_struct",
					{toParameter(std::vector<uint32_t>(20)), toParameter(std::vector<uint32_t>(20))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<uint32_t>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42, 0})
				),
#endif
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vector.cl", "test_copy",
					{toParameter(toRange(1, 17)), std::make_pair(0, std::vector<uint32_t>(32))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<uint32_t>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_other.cl", "test_atomics",
					{toParameter(toRange<int32_t>(1, 12)), toParameter(toRange<int32_t>(1, 12))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<int32_t>{2, 0, 3, 5, 4, 6, 7, 8, 9, 10, 0})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_other.cl", "test_f2i",
					{toScalarParameter(1.0f), toScalarParameter(1.1f), toScalarParameter(1.5f), toScalarParameter(1.9f), toParameter(std::vector<int>(32))}, {}, maxExecutionCycles),
					addVector({}, 4, std::vector<int>{1, 1, 1, 1, -1, -1, -1, -1, 1, -1, 2, -1, 1, -1, 1, -2, 1, -1, 2, -2, 1, 1, 2, 2, -1, -1, -2, -2, 1, -1})
				),
#ifndef SPIRV_FRONTEND
				// TODO fix register association error in CI, does not happen with local LLVM-SPIRV-Translator
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_other.cl", "test_global_data",
					{toScalarParameter(1), toParameter(std::vector<int32_t>(2))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<int32_t>{2, 21})
				),
#endif
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
					addVector({}, 0, std::vector<int>{0, 1, 2, 3, 5, 5, 6, 7, 10, 9, 10, 11, 15, 13, 14, 15, 20})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test10",
					{toParameter(toRange<int>(0, 1024)), toParameter(toRange<int>(0, 1024))}, {}, maxExecutionCycles * 2),
					addVector({}, 0, std::vector<int>{0, 1, 2, 3, 8, 5, 6, 7, 16, 9, 10, 11, 24})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test11",
					{toParameter(toRange<int>(0, 256))}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<int>{100,100,100,100,100,100,100,100,100})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/pointer_cast.cl", "test_pointer_cast",
					{toParameter(std::vector<unsigned>{0x01020304}), toParameter(std::vector<unsigned>(1))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<unsigned>{0x01020304})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/sub_sat.cl", "test_sub_sat_int",
					{toParameter(std::vector<int>{std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min(), std::numeric_limits<int>::max()}),
							toParameter(std::vector<int>{1, -1, std::numeric_limits<int>::max(), std::numeric_limits<int>::min()}), toParameter(std::vector<int>(4))
					}, toConfig(4, 1, 1, 1, 1, 1), maxExecutionCycles),
					addVector({}, 2, std::vector<int>{std::numeric_limits<int>::min(), std::numeric_limits<int>::max(), std::numeric_limits<int>::min(), std::numeric_limits<int>::max()})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/uchar_compare.cl", "test_select",
					{toParameter(std::vector<unsigned>{0x01020304}), toParameter(std::vector<unsigned>{0x04020301}), toParameter(std::vector<unsigned>(1))}, {}, maxExecutionCycles),
					addVector({}, 2, std::vector<unsigned>{0x04020301})
				),
				// FIXME SEGFAULTs
				// std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_async_copy.cl", "test_async_copy",
				//     {toParameter(toRange<unsigned>(0, 12*16)), toParameter(std::vector<unsigned>(12*16)), toParameter(std::vector<unsigned>(12*16))}, toConfig(12), maxExecutionCycles),
				//     addVector(/*the __local arg might be lowered to VPM addVector({}, 1, toRange<unsigned>(0, 12*16))*/ {}, 2, toRange<unsigned>(0, 12*16))
				// ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_shuffle.cl", "test_shuffle",
				    {toParameter(std::vector<unsigned>{0x03020100, 0x07060504, 0x0b0a0908, 0x0f0e0d0c, 0x13121110, 0x17161514, 0x1b1a1918, 0x1f1e1d1c}), toParameter(std::vector<unsigned>(10*16/sizeof(int32_t)))}, toConfig(1), maxExecutionCycles),
				    addVector({}, 1, std::vector<unsigned>{0x08040607, 0x010d0c01, 0x0f0e0900, 0x06080304, 0x120b0701, 0x09080f15, 0x01021300, 0x08070d11, 0x10021b1a, 0x17061904, 0x131c0908, 0x0f0e0d1a, 0x10020111, 0x10020111, 0x10020111, 0x10020111,
				                                           0x01000000, 0x10000011, 0x02011100, 0x00001002, 0x00000000, 0x04040404, 0x08080808, 0x0c0c0c0c, 0, 0, 0, 0, 0x03020100, 0x07060504, 0x0b0a0908, 0x0f0e0d0c, 0x0d0c0b0a, 0x01000f0e, 0x05040302, 0x09080706, 0x0c0d0e0f, 0x08090a0b, 0x04050607, 0x00010203})
                ),
				// TODO need to pass parameter as literal vectors, not buffers
				// std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vector.cl", "test_param",
				//     {toParameter(std::vector<unsigned>{0x40, 0, 0, 0, 0x41, 0, 0, 0, 0x42, 0, 0, 0, 0x43, 0, 0, 0}), toParameter(std::vector<unsigned>{0x15, 0x16, 0x17, 0x18}), toParameter(std::vector<unsigned>(4))}, toConfig(1), maxExecutionCycles),
				//     addVector({}, 2, std::vector<unsigned>{0x55, 0x57, 0x59, 0x61})
				// )
#ifndef SPIRV_FRONTEND
				// LLVM 3.6 used by LLVM-SPIRV compiler used in CI cannot compile "(event_t)0"
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/async_copy_global_to_local.cl", "test_async_copy_global_to_local",
					{toParameter(toRange<unsigned>(0, 64)), toParameter(std::vector<unsigned>(64)), toParameter(std::vector<unsigned>(64)), toScalarParameter(64), toScalarParameter(8)}, toConfig(8), maxExecutionCycles),
					addVector({}, 1, toRange<unsigned>(0, 64))
				),
#endif
				// TODO fix result error
				// std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCLIPP/Histogram.cl", "histogram_1C",
				// 	{toParameter(std::vector<unsigned>{0x01000102, 0x01060101, 0x02030405}), toParameter(std::vector<unsigned>(8)), toScalarParameter(4)}, toConfig(4, 3), maxExecutionCycles),
	            //  /* the value is the count of bytes <= the position */
				// 	addVector({}, 1, std::vector<unsigned>{1, 6, 8, 9, 10, 11, 12, 12})
				// ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/pocl/test_structs_as_args.cl", "test_single",
					{toParameter(std::vector<unsigned>{0x01000102}), toParameter(std::vector<unsigned>(1))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<unsigned>{0x01000102})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/pocl/test_structs_as_args.cl", "test_pair",
					{toParameter(std::vector<unsigned>{0x01010101, 0x23232323, 0x45454545, 0x67676767}), toParameter(std::vector<unsigned>(2))}, {}, maxExecutionCycles),
					addVector({}, 1, std::vector<unsigned>{0x01010101, 0x45454545})
				),
				// TODO fix result error
				// std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/pocl/test_structs_as_args.cl", "test_kernel",
				// 	{toParameter(std::vector<unsigned>{0x01001001, 0x02002002, 0x03003003, 0x04004004, 0x05005005, 0x06006006, 0x07007007, 0x48008008, 0x09009009, 0x0A00A00A, 0x0B00B00B, 0x0C00C00C}), toParameter(std::vector<unsigned>(10))}, {}, maxExecutionCycles),
				// 	addVector({}, 1, std::vector<unsigned>{0x01001001, 0x02002002, 0x03003003, 0x05, 0x06006006, 131584, 0x9009, 0x0A00A00A, 48, 8})
				// ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/min_max_constant_args.cl", "sample_test",
					// has 65 parameters, 64 inputs and 1 output
					{toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)), toParameter(toRange(0, 8)),
						toParameter(std::vector<unsigned>(8))
					}, toConfig(4, 1, 1, 2), maxExecutionCycles),
				    // dest[gid] = sum(src0[gid], src1[gid], ..., src63[gid])
	                addVector({}, 64, std::vector<unsigned>{0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64})
				)
		};

		//TODO NVIDIA/matrixMul, NVIDIA/transpose, OpenCLIPP/Arithmetic, OpenCLIPP/Logic, OpenCLIPP/Thresholding, test_signedness

		static std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> floatTests = {
				std::make_pair(EmulationData(VC4C_ROOT_PATH "example/test_instructions.cl", "test_instructions",
					{std::make_pair(2, Optional<std::vector<uint32_t>>{}), std::make_pair(4, Optional<std::vector<uint32_t>>{}), std::make_pair(bit_cast<float, uint32_t>(2.0f), Optional<std::vector<uint32_t>>{}), std::make_pair(bit_cast<float, uint32_t>(4.0f), Optional<std::vector<uint32_t>>{}), std::make_pair(0, std::vector<uint32_t>(32)), std::make_pair(0, std::vector<uint32_t>(32))}, {}, maxExecutionCycles),
					addVector({}, 5, std::vector<float>{6.0f, -2.0f, 8.0f, 0.5f, 4.0f, 2.0f, 2.0f})
				),
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
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vector.cl", "test_arithm",
					{toScalarParameter(2.0f), toParameter(toRange(1.0f, 17.0f)), toParameter(std::vector<float>(16))}, {}, maxExecutionCycles),
					addVector({}, 2, toRange(3.0f, 19.0f))
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test1",
					{toParameter(std::vector<float>(1000)), toParameter(std::vector<float>(1000))}, {}, maxExecutionCycles),
					addVector(addVector({}, 0, toRange(-0.0f, -14.0f, -1.0f)), 1, toRange(-0.0f, -14.0f, -1.0f))
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test2",
					{toParameter(toRange<float>(1.0f, 10.0f)), toParameter(toRange<float>(1.0f, 10.0f)), toScalarParameter(7.0f), toScalarParameter(1u), toScalarParameter(6u)}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<float>{1.0f, 18.0f, 30.0f, 44.0f, 60.0f, 78.0f, 7.0f, 8.0f, 9.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test3",
					{toParameter(toRange<float>(1.0f, 801.0f)), toParameter(toRange<float>(1.0f, 801.0f)), toScalarParameter(7.0f)}, {}, maxExecutionCycles),
					addVector({}, 0, std::vector<float>{8.0f, 18.0f, 30.0f, 44.0f, 60.0f, 78.0f, 98.0f, 120.0f, 144.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/test_vectorization.cl", "test5",
					{toParameter(std::vector<float>(1024))}, {}, maxExecutionCycles * 2),
					addVector({}, 0, toRange<float>(0.0f, 1024.0f))
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/clamp.cl", "test_clamp",
					{toParameter(std::vector<float>{17.0f, 0.0f, 3.0f}), toParameter(std::vector<float>{1.0f, 1.0f, 1.0f}), toParameter(std::vector<float>{5.0f, 5.0f, 5.0f}), toParameter(std::vector<float>(3))},
					toConfig(3, 1, 1, 1, 1, 1), maxExecutionCycles),
					addVector({}, 3, std::vector<float>{5.0f, 1.0f, 3.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/OpenCL-CTS/cross_product.cl", "test_cross",
					{toParameter(std::vector<float>{1.0f, 2.0f, 3.0f}), toParameter(std::vector<float>{3.0f, 4.0f, 5.0f}), toParameter(std::vector<float>(3))}, {}, maxExecutionCycles),
					addVector({}, 2, std::vector<float>{-2.0f, 4.0f, -2.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/bugs/vc4cl_27_wrong_result.cl", "hello",
                    {toParameter(toRange<float>(-15.0f, 15.0f))}, toConfig(10, 1, 1, 3, 1, 1), maxExecutionCycles),
				    addVector({}, 0, toRange<float>(-30.0f, 30.0f, 2.0f))
                ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/bugs/54_invalid_results.cl", "sum",
                    {toParameter(std::vector<float>(9))}, toConfig(3, 1, 1, 3, 1, 1), maxExecutionCycles),
				    addVector({}, 0, std::vector<float>{1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f})
				),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/deepCL/copy.cl", "copy",
                    {toScalarParameter(10), toParameter(std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12}), toParameter(std::vector<float>(16))}, toConfig(12), maxExecutionCycles),
				    addVector({}, 2, std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 0, 0})
                ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/deepCL/copy.cl", "multiplyConstant",
                    {toScalarParameter(10), toScalarParameter(5.0f), toParameter(std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12}), toParameter(std::vector<float>(16))}, toConfig(12), maxExecutionCycles),
				    addVector({}, 3, std::vector<float>{5*1, 5*9, 5*-2, 5*8, 5*3, 5*7, 5*4, 5*6, 5*5, 5*0, 0, 0})
                ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/deepCL/copy.cl", "multiplyInplace",
                    {toScalarParameter(10), toScalarParameter(5.0f), toParameter(std::vector<float>{1, 9, -2, 8, 3, 7, 4, 6, 5, 0, 11, 12})}, toConfig(12), maxExecutionCycles),
				    addVector({}, 2, std::vector<float>{5*1, 5*9, 5*-2, 5*8, 5*3, 5*7, 5*4, 5*6, 5*5, 5*0, 11, 12})
                ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/deepCL/inv.cl", "array_inv",
                    {toScalarParameter(10), toParameter(std::vector<float>{1, 9, -2, 8, 0.3f, 7, 0.4f, 6, -5, 0.1f, 11, 12})}, toConfig(12), maxExecutionCycles),
				    addVector({}, 1, std::vector<float>{1, 1.0f/9.0f, 1.0f/-2.0f, 1.0f/8.0f, 1.0f/0.3f, 1.0f/7.0f, 1.0f/0.4f, 1.0f/6.0f, 1.0f/-5.0f, 1.0f/0.1f, 11, 12})
                ),
				std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/deepCL/memset.cl", "cl_memset",
                    {toParameter(std::vector<float>(16)), toScalarParameter(17.0f), toScalarParameter(10)}, toConfig(12), maxExecutionCycles),
				    addVector({}, 0, std::vector<float>{17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 17.0f, 0, 0, 0, 0})
                ),
				// TODO fix result mismatch
				// std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/rodinia/nearestNeighbor_kernel.cl", "NearestNeighbor",
                //   {toParameter(std::vector<float>{0,0, 1, 1, 0, 1, 1, 0, -1, -1, -1, 0, 0, -1, 1, -1, -1, 1}), toParameter(std::vector<float>(16)), toScalarParameter(9), toScalarParameter(0.5f), toScalarParameter(-0.5f)}, toConfig(12), maxExecutionCycles),
				//   addVector({}, 0, std::vector<float>{0.707107f, 1.58114f, 1.58114f, 0.707107f, 1.58114f, 1.58114f, 0.707107f, 0.707107f, 2.12132f})
                // ),
                std::make_pair(EmulationData(VC4C_ROOT_PATH "testing/HandsOnOpenCL/matmul.cl", "mmul",
                    {toScalarParameter(4), toParameter(std::vector<float>(4 * 4, 3.0f)), toParameter(std::vector<float>(4 * 4, 5.0f)), toParameter(std::vector<float>(4 * 4, 0.0f))}, toConfig(2, 2, 1, 2, 2, 1), maxExecutionCycles),
                    addVector({}, 3, std::vector<float>(4 * 4, 4 * 3.0f * 5.0f))
                )
		};
	} /* namespace test */
} /* namespace vc4c */

#endif /* VC4C_TEST_CASES_H */
