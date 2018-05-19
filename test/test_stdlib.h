/*
* Author: doe300
*
* See the file "LICENSE" for the full license governing this code.
*/

#ifndef VC4C_TEST_STDLIB_TESTS_H
#define VC4C_TEST_STDLIB_TESTS_H

#include "test_cases.h"

namespace vc4c
{
  namespace test {

    const static std::string atomicTestFile = VC4C_ROOT_PATH "testing/test_atomic.cl";
    std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> atomicTests = {
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_add",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{7 + 12 * 1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_sub",
        {toParameter(std::vector<unsigned>{77}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{77 - 12 * 1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_xchg",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_inc",
        {toParameter(std::vector<unsigned>{7})}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{7 + 12 * 1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_dec",
        {toParameter(std::vector<unsigned>{77})}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{77 - 12 * 1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_min",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_max",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{7})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_and",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{7 & 1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_or",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{7 | 1})
      ),
      std::make_pair(EmulationData(atomicTestFile, "test_atomic_xor",
        {toParameter(std::vector<unsigned>{7}), toScalarParameter(1)}, toConfig(12), maxExecutionCycles),
        addVector({}, 0, std::vector<unsigned>{((((((((((((7 ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1) ^ 1)})
      )
    };

    const static std::string commonTestsFile = VC4C_ROOT_PATH "testing/test_common.cl";
    std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> commonTests = {
      std::make_pair(EmulationData(commonTestsFile, "test_clamp",
        {toParameter(toRange<float>(0, 4)), toParameter(std::vector<float>{1, 1, 1, 1}), toParameter(std::vector<float>{2, 2, 2, 2}), toParameter(std::vector<float>(4))}, toConfig(4), maxExecutionCycles),
        addVector({}, 3, std::vector<float>{1, 1, 2, 2})
      ),
      //TODO degrees
      std::make_pair(EmulationData(commonTestsFile, "test_max",
        {toParameter(toRange<float>(0, 3)), toParameter(std::vector<float>{1, 1, 1}), toParameter(std::vector<float>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<float>{1, 1, 2})
      ),
      std::make_pair(EmulationData(commonTestsFile, "test_min",
        {toParameter(toRange<float>(0, 3)), toParameter(std::vector<float>{1, 1, 1}), toParameter(std::vector<float>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<float>{0, 1, 1})
      ),
      //TODO radians
      std::make_pair(EmulationData(commonTestsFile, "test_step",
        {toParameter(std::vector<float>{1, 1, 1}), toParameter(toRange<float>(0, 3)), toParameter(std::vector<float>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<float>{0, 1, 1})
      ),
      std::make_pair(EmulationData(commonTestsFile, "test_smoothstep",
        {toParameter(std::vector<float>{1, 1, 1, 1, 1}), toParameter(std::vector<float>{3, 3, 3, 3, 3}), toParameter(toRange(0, 5)), toParameter(std::vector<float>(5))}, toConfig(5), maxExecutionCycles),
        addVector({}, 3, std::vector<float>{0, 0, 0.5f, 1, 1})
      ),
      std::make_pair(EmulationData(commonTestsFile, "test_sign",
        {toParameter(toRange<float>(-1, 2)), toParameter(std::vector<float>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 1, std::vector<float>{-1, 0, 1})
      )
    };

    //TODO test conversions

    const static std::string geometricTestsFile = VC4C_ROOT_PATH "testing/test_geometric.cl";
    std::vector<std::tuple<EmulationData, std::map<uint32_t, std::vector<uint32_t>>, unsigned>> geometricTests = {
      std::make_tuple(EmulationData(geometricTestsFile, "test_cross",
	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 3.0f, 2.0f, 1.0f}), toParameter(std::vector<float>{3.0f, 2.0f, 1.0f, 3.0f, 2.0f, 1.0f}),
	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
	    addVector({}, 2, std::vector<float>{-4.0f, 8.0f, -1.0f, 0.0f, 0.0f, 0.0f}),
	    1
      ),
      //TODO all following failures are based on error in dot(a,b)
      std::make_tuple(EmulationData(geometricTestsFile, "test_dot",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 3.0f, 2.0f, 1.0f}), toParameter(std::vector<float>{3.0f, 2.0f, 1.0f, 3.0f, 2.0f, 1.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 2, std::vector<float>{10.0f, 10.0f, 10.0f, 14.0f, 14.0f, 14.0f}),
      	    1
      ),
      std::make_tuple(EmulationData(geometricTestsFile, "test_length",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 3.0f, 2.0f, 1.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 1, std::vector<float>{sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f)}),
      	    1
      ),
      std::make_tuple(EmulationData(geometricTestsFile, "test_distance",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 3.0f, 2.0f, 1.0f}), toParameter(std::vector<float>{3.0f, 2.0f, 1.0f, 3.0f, 2.0f, 1.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 2, std::vector<float>{sqrtf(8.0f), sqrtf(8.0f), sqrtf(8.0f), 0.0f, 0.0f, 0.0f}),
      	    1
      ),
      std::make_tuple(EmulationData(geometricTestsFile, "test_normalize",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 1.0f, 0.0f, 0.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 1, std::vector<float>{1.0f / sqrtf(14.0f), 2.0f / sqrtf(14.0f), 3.0f / sqrtf(14.0f), 1.0f, 0.0f, 0.0f}),
      	    1
      ),
      std::make_tuple(EmulationData(geometricTestsFile, "test_fast_length",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 3.0f, 2.0f, 1.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 1, std::vector<float>{sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f), sqrtf(14.0f)}),
      	    8192
      ),
      std::make_tuple(EmulationData(geometricTestsFile, "test_fast_distance",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 3.0f, 2.0f, 1.0f}), toParameter(std::vector<float>{3.0f, 2.0f, 1.0f, 3.0f, 2.0f, 1.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 2, std::vector<float>{sqrtf(8.0f), sqrtf(8.0f), sqrtf(8.0f), 0.0f, 0.0f, 0.0f}),
      	    8192
      ),
      std::make_tuple(EmulationData(geometricTestsFile, "test_fast_normalize",
      	    {toParameter(std::vector<float>{1.0f, 2.0f, 3.0f, 1.0f, 0.0f, 0.0f}),
      	    toParameter(std::vector<float>(6))}, toConfig(2), maxExecutionCycles),
      	    addVector({}, 1, std::vector<float>{1.0f / sqrtf(14.0f), 2.0f / sqrtf(14.0f), 3.0f / sqrtf(14.0f), 1.0f, 0.0f, 0.0f}),
      	    8192
      )
    };

    const static std::string integerTestsFile = VC4C_ROOT_PATH "testing/test_integer.cl";
    std::vector<std::pair<EmulationData, std::map<uint32_t, std::vector<uint32_t>>>> integerFunctionTests = {
      std::make_pair(EmulationData(integerTestsFile, "test_abs",
        {toParameter(std::vector<int>{-7, 0, 7}), toParameter(std::vector<int>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 1, std::vector<int>{7, 0, 7})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_abs_diff",
        {toParameter(std::vector<int>{-7, -7, -7, 7}), toParameter(std::vector<int>{-7, 0, 7, -7}), toParameter(std::vector<int>(4))}, toConfig(4), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{0, 7, 14, 14})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_add_sat",
        {toParameter(std::vector<int>{17000, -17000, 17000}), toParameter(std::vector<int>{17000, -17000, -17000}), toParameter(std::vector<int>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{std::numeric_limits<short>::max(), std::numeric_limits<short>::min(), 0})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_hadd",
        {toParameter(std::vector<int>{17000, -17000, 17000}), toParameter(std::vector<int>{17000, -17000, -17000}), toParameter(std::vector<int>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{17000, -17000, 0})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_rhadd",
        {toParameter(std::vector<int>{16999, -16999, 16999}), toParameter(std::vector<int>{17000, -17000, -17000}), toParameter(std::vector<int>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{17000, -16999, 0})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_clamp",
        {toParameter(toRange(0, 4)), toParameter(std::vector<int>{1, 1, 1, 1}), toParameter(std::vector<int>{2, 2, 2, 2}), toParameter(std::vector<int>(4))}, toConfig(4), maxExecutionCycles),
        addVector({}, 3, std::vector<int>{1, 1, 2, 2})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_clz",
        {toParameter(std::vector<int>{-7, 1, 7, std::numeric_limits<short>::max()}), toParameter(std::vector<int>(4))}, toConfig(4), maxExecutionCycles),
        addVector({}, 1, std::vector<int>{0, 15, 13, 1})
      ),
      //TODO mad_hi
      //TODO mad_sat: saturate the temporary result too or just the end-result? Compare with pocl/beignet/libclc
      std::make_pair(EmulationData(integerTestsFile, "test_mad_sat",
        {toParameter(std::vector<int>{10000, 10000, 10000, 10000, 10000, 10000}), toParameter(std::vector<int>{4, 3, -4, -3, 3, -3}), toParameter(std::vector<int>{2, 10000, 2, -10000, 2, -2}), toParameter(std::vector<int>(6))}, toConfig(6), maxExecutionCycles),
        addVector({}, 3, std::vector<int>{std::numeric_limits<short>::max(), std::numeric_limits<short>::max(), std::numeric_limits<short>::min(), std::numeric_limits<short>::min(), 30002, -30002})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_max",
        {toParameter(std::vector<int>{-7, -7, -7, 7}), toParameter(std::vector<int>{-7, 0, 7, -7}), toParameter(std::vector<int>(4))}, toConfig(4), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{-7, 0, 7, 7})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_min",
        {toParameter(std::vector<int>{-7, -7, -7, 7}), toParameter(std::vector<int>{-7, 0, 7, -7}), toParameter(std::vector<int>(4))}, toConfig(4), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{-7, -7, -7, -7})
      ),
      //TODO mul_hi
      std::make_pair(EmulationData(integerTestsFile, "test_rotate",
        {toParameter(std::vector<int>{-1, 1}), toParameter(std::vector<int>{5, 5}), toParameter(std::vector<int>(2))}, toConfig(2), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{-1, 1 << 5})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_sub_sat",
        {toParameter(std::vector<int>{17000, -17000, 17000}), toParameter(std::vector<int>{-17000, 17000, 17000}), toParameter(std::vector<int>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{std::numeric_limits<short>::max(), std::numeric_limits<short>::min(), 0})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_upsample",
        {toParameter(std::vector<int>{-1, 1}), toParameter(std::vector<int>{5, 5}), toParameter(std::vector<int>(2))}, toConfig(2), maxExecutionCycles),
        addVector({}, 2, std::vector<int>{bit_cast<unsigned, int>(0xFFFF0005), 0x00010005})
      ),
      std::make_pair(EmulationData(integerTestsFile, "test_popcount",
        {toParameter(std::vector<int>{-1, 0, 7}), toParameter(std::vector<int>(3))}, toConfig(3), maxExecutionCycles),
        addVector({}, 1, std::vector<int>{16, 0, 3})
      )
    };

    const static std::string mathTestsFile = VC4C_ROOT_PATH "testing/test_math.cl";
		constexpr float M_PI_F = static_cast<float>(M_PI);
		std::vector<std::tuple<EmulationData, std::map<uint32_t, std::vector<uint32_t>>, unsigned>> mathTests = {
			//acos = cos^-1
			std::make_tuple(EmulationData(mathTestsFile, "test_acos",
			    {toParameter(std::vector<float>{1.0f, sqrtf(3.0f)/2.0f, sqrtf(2.0f)/2.0f, 0.5f, 0, -0.5f, -sqrtf(2.0f)/2.0f, -sqrtf(3.0f)/2.0f, -1.0f}),
			    toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, M_PI_F/2.0f, 2.0f * M_PI_F/ 3.0f, 3.0f * M_PI_F / 4.0f, 5.0f * M_PI_F / 6.0f, M_PI_F}),
			    4
			),
			//acosh = cosh^-1
			std::make_tuple(EmulationData(mathTestsFile, "test_acosh",
			    {toParameter(std::vector<float>{1.0f, 0.5f, 1.0f, -0.5f, -1.0f}), toParameter(std::vector<float>(12))}, toConfig(5), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{0.0f, M_PI_F / 3.0f, 0.0f, (2.0f * M_PI_F) / 3.0f, M_PI}),
			    4
			),
			//asin = sin^-1
			std::make_tuple(EmulationData(mathTestsFile, "test_asin",
			    {toParameter(std::vector<float>{0, 0.5f, sqrtf(2.0f)/2.0f, sqrtf(3.0f)/2.0f, 1.0f, sqrtf(3.0f)/2.0f, sqrtf(2.0f)/2.0f, 0.5f, 0}),
			    toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, M_PI_F/2.0f, 2.0f * M_PI_F / 3.0f, 3.0f * M_PI_F / 4.0f, 5.0f * M_PI_F / 6.0f, M_PI_F}),
			    4
			),
			//atan = tan^-1
			std::make_tuple(EmulationData(mathTestsFile, "test_atan",
			    {toParameter(std::vector<float>{0, sqrtf(3.0f)/3.0f, 1.0f, sqrtf(3.0f), -sqrtf(3.0f), -1.0f, -sqrtf(3.0f)/3.0f}),
			    toParameter(std::vector<float>(12))}, toConfig(7), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, 2.0f * M_PI_F / 3.0f, 3.0f * M_PI_F / 4.0f}),
			    5
			),
			//atan = tan^-1
			std::make_tuple(EmulationData(mathTestsFile, "test_atan2",
			    {toParameter(std::vector<float>{0, sqrtf(3.0f), 1.0f, sqrtf(3.0f), -sqrtf(3.0f), -1.0f, -sqrtf(3.0f)}),
			    toParameter(std::vector<float>{1, 3.0f, 1.0f, 1.0f, 1.0f, 1.0f, 3.0f}), toParameter(std::vector<float>(12))}, toConfig(7), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, 2.0f * M_PI_F / 3.0f, 3.0f * M_PI_F / 4.0f}),
			    6
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_cbrt",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), std::cbrtf)),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_ceil",
			    {toParameter(std::vector<float>{-1.9f, -1.4f, -1.0f, -0.9f, -0.1f, 0.0f, 0.5f, 0.7f, 1.0f}), toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{-1.0f, -1.0f, -1.0f, -0.0f, -0.0f, 0.0f, 1.0f, 1.0f, 1.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_copysign",
			    {toParameter(std::vector<float>{-1.0f, -1.0f, 1.0f, 1.0f}),
			    toParameter(std::vector<float>{-1.0f, 1.0f, -1.0f, 1.0f}), toParameter(std::vector<float>(12))}, toConfig(4), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{-1.0f, 1.0f, -1.0f, 1.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_cos",
			    {toParameter(std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, M_PI_F/2.0f, 2.0f * M_PI_F/ 3.0f, 3.0f * M_PI_F / 4.0f, 5.0f * M_PI_F / 6.0f, M_PI_F}),
			    toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{1.0f, sqrtf(3.0f)/2.0f, sqrtf(2.0f)/2.0f, 0.5f, 0, -0.5f, -sqrtf(2.0f)/2.0f, -sqrtf(3.0f)/2.0f, -1.0f}),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_cosh",
			    {toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-6.0f, 6.0f), [](float f) -> float {return std::cosh(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_erf",
			    {toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-6.0f, 6.0f), [](float f) -> float {return std::erf(f);})),
			    16
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_erfc",
			    {toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-6.0f, 6.0f), [](float f) -> float {return std::erfc(f);})),
			    16
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_exp",
			    {toParameter(toRange(-50.0f, 50.0f)), toParameter(std::vector<float>(100))}, toConfig(10, 1, 1, 10), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-50.0f, 50.0f), [](float f) -> float {return std::exp(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_exp2",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::exp2(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_exp10",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::pow(10.0f, f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_expm1",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::exp(f) - 1.0f;})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_fabs",
			    {toParameter(toRange(-12.0f, 0.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-12.0f, 0.0f), [](float f) -> float {return std::abs(f);})),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_fdim",
			    {toParameter(std::vector<float>{7.0f, 1.0f, -7.0f}),
			    toParameter(std::vector<float>{1.0f, 1.0f, 1.0f}), toParameter(std::vector<float>(12))}, toConfig(3), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{6.0f, 0.0f, 0.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_floor",
			    {toParameter(std::vector<float>{-1.9f, -1.4f, -1.0f, -0.9f, -0.1f, 0.0f, 0.5f, 0.7f, 1.0f}), toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{-2.0f, -2.0f, -1.0f, -1.0f, -1.0f, 0.0f, 0.0f, 0.0f, 1.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_fmax",
			    {toParameter(std::vector<float>{7.0f, 1.0f, -7.0f}),
			    toParameter(std::vector<float>{1.0f, 1.0f, 5.0f}), toParameter(std::vector<float>(12))}, toConfig(3), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{7.0f, 1.0f, 5.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_fmin",
			    {toParameter(std::vector<float>{7.0f, 1.0f, -7.0f}),
			    toParameter(std::vector<float>{1.0f, 1.0f, 5.0f}), toParameter(std::vector<float>(12))}, toConfig(3), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{1.0f, 1.0f, -7.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_fmod",
			    {toParameter(toRange(0.0f, 12.0f)),
			    toParameter(std::vector<float>{7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f}), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 0.5f, 1.5f, 2.5f, 3.5f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_hypot",
			    {toParameter(toRange(-6.0f, 6.0f)), toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{sqrtf(36.0f), sqrtf(25.0f + 1.0f), sqrtf(16.0f + 4.0f), sqrtf(9.0f + 9.0f), sqrtf(4.0f + 16.0f), sqrtf(1.0f + 25.0f), sqrtf(36.0f), sqrtf(1.0f + 49.0f), sqrtf(4.0f + 64.0f), sqrtf(9.0f + 81.0f), sqrtf(16.0f + 100.0f), sqrtf(25.0f + 121.0f)}),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_ilogb",
			    {toParameter(std::vector<float>{0.25f, 0.5f, 1.0f, 4.0f, 8.0f, 128.0f, 1024.0f}), toParameter(std::vector<float>(12))}, toConfig(7), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{-2.0f, -1.0f, 0.0f, 2.0f, 3.0f, 7.0f, 10.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_ldexp",
			    {toParameter(std::vector<float>{7.5f, 7.5f, 7.5f}),
			    toParameter(std::vector<float>{-2.0f, 1.0f, 5.0f}), toParameter(std::vector<float>(12))}, toConfig(3), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{1.875f, 15.0f, 240.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_log",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::log(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_lgamma",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::lgamma(f);})),
			    8192 /* ULP value is not defined */
			),
			//TODO lgamma_r
			std::make_tuple(EmulationData(mathTestsFile, "test_log",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::log(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_log2",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::log2(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_log10",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::log(f)/std::log(10.0f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_log1p",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::log(f + 1.0f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_logb",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::logb(f);})),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_maxmag",
			    {toParameter(std::vector<float>{-7.5f, -7.5f, -7.5f, -7.5f}),
			    toParameter(std::vector<float>{-2.0f, 1.0f, 14.0f, -8.0f}), toParameter(std::vector<float>(12))}, toConfig(4), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{-7.5f, -7.5f, 14.0f, -8.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_minmag",
			    {toParameter(std::vector<float>{-7.5f, -7.5f, -7.5f, -7.5f}),
			    toParameter(std::vector<float>{-2.0f, 1.0f, 14.0f, -8.0f}), toParameter(std::vector<float>(12))}, toConfig(4), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{-2.0f, 1.0f, -7.5f, -7.5f}),
			    0
			),
			//TODO nan
			std::make_tuple(EmulationData(mathTestsFile, "test_nextafter",
			    {toParameter(std::vector<float>{-7.5f, -7.5f, 7.5f, 7.5f}),
			    toParameter(std::vector<float>{-8.0f, 8.0f, -8.0f, 8.0f}), toParameter(std::vector<float>(12))}, toConfig(4), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{std::nextafter(-7.5f, -8.0f), std::nextafter(-7.5f, 8.0f), std::nextafter(7.5f, -8.0f), std::nextafter(7.5f, 8.0f)}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_pow",
			    {toParameter(toRange(-6.0f, 6.0f)),
			    toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{std::pow(-6.0f, -6.0f), std::pow(-5.0f, -5.0f), std::pow(-4.0f, -4.0f), std::pow(-3.0f, -3.0f), std::pow(-2.0f, -2.0f), std::pow(-1.0f, -1.0f), std::pow(0.0f, 0.0f), std::pow(1.0f, 1.0f), std::pow(2.0f, 2.0f), std::pow(3.0f, 3.0f), std::pow(4.0f, 4.0f), std::pow(5.0f, 5.0f)}),
			    16
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_pown",
			    {toParameter(toRange(-6.0f, 6.0f)),
			    toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{std::pow(-6.0f, -6.0f), std::pow(-5.0f, -5.0f), std::pow(-4.0f, -4.0f), std::pow(-3.0f, -3.0f), std::pow(-2.0f, -2.0f), std::pow(-1.0f, -1.0f), std::pow(0.0f, 0.0f), std::pow(1.0f, 1.0f), std::pow(2.0f, 2.0f), std::pow(3.0f, 3.0f), std::pow(4.0f, 4.0f), std::pow(5.0f, 5.0f)}),
			    16
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_powr",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{std::pow(0.0f, -6.0f), std::pow(1.0f, -5.0f), std::pow(2.0f, -4.0f), std::pow(3.0f, -3.0f), std::pow(4.0f, -2.0f), std::pow(5.0f, -1.0f), std::pow(6.0f, 0.0f), std::pow(7.0f, 1.0f), std::pow(8.0f, 2.0f), std::pow(9.0f, 3.0f), std::pow(10.0f, 4.0f), std::pow(11.0f, 5.0f)}),
			    16
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_remainder",
			    {toParameter(toRange(0.0f, 12.0f)),
			    toParameter(std::vector<float>{7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f}),
			    toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 2, std::vector<float>{0.0f, 1.0f, 2.0f, 3.0f, -3.5f, -2.5f, -1.5f, -0.5f, 0.5f, 1.5f, 2.5f, 3.5f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_rint",
			    {toParameter(std::vector<float>{-1.9f, -1.4f, -1.0f, -0.9f, -0.1f, 0.0f, 0.5f, 0.7f, 1.0f}), toParameter(std::vector<float>(12))},
			    toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{-2.0f, -2.0f, -1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f}),
			    0
			),
			//TODO rootn
			std::make_tuple(EmulationData(mathTestsFile, "test_round",
			    {toParameter(std::vector<float>{-1.9f, -1.4f, -1.0f, -0.9f, -0.1f, 0.0f, 0.5f, 0.7f, 1.0f}),
			    toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{-2.0f, -1.0f, -1.0f, -1.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f}),
			    0
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_rsqrt",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return 1.0f / std::sqrt(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_sin",
			    {toParameter(std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, M_PI_F/2.0f, 2.0f * M_PI_F/ 3.0f, 3.0f * M_PI_F / 4.0f, 5.0f * M_PI_F / 6.0f, M_PI_F}),
			    toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{0.0f, 0.5f, sqrtf(2.0f)/2.0f, sqrtf(3.0f)/2.0f, 1, sqrtf(3.0f)/2.0f, sqrtf(2.0f)/2.0f, 0.5f, 0.0f}),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_sinh",
			    {toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-6.0f, 6.0f), [](float f) -> float {return std::sinh(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_sqrt",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::sqrt(f);})),
			    4
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_tan",
			    {toParameter(std::vector<float>{0, M_PI_F/6.0f, M_PI_F/4.0f, M_PI_F/3.0f, 2.0f * M_PI_F/ 3.0f, 3.0f * M_PI_F / 4.0f, 5.0f * M_PI_F / 6.0f, M_PI_F}),
			    toParameter(std::vector<float>(12))}, toConfig(8), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{0.0f, sqrtf(3.0f)/3.0f, 1, sqrtf(3.0f), -sqrtf(3.0f), -1, -sqrtf(3.0f)/3.0f, 0.0f}),
			    5
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_tanh",
			    {toParameter(toRange(-6.0f, 6.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(-6.0f, 6.0f), [](float f) -> float {return std::tanh(f);})),
			    5
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_tgamma",
			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::tgamma(f);})),
			    16
			),
			std::make_tuple(EmulationData(mathTestsFile, "test_trunc",
			    {toParameter(std::vector<float>{-1.9f, -1.4f, -1.0f, -0.9f, -0.1f, 0.0f, 0.5f, 0.7f, 1.0f}), toParameter(std::vector<float>(12))}, toConfig(9), maxExecutionCycles),
			    addVector({}, 1, std::vector<float>{-1.0f, -1.0f, -1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f}),
			    0
			)
		};

		//TODO relational, vector
  } /* namespace test */
} /* namespace vc4c */

#endif /* VC4C_TEST_STDLIB_TESTS_H */
