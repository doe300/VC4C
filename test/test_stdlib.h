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
    			    {toParameter(toRange(0.0f, 12.0f)), toParameter(std::vector<float>(12))}, toConfig(12), maxExecutionCycles),
    			    addVector({}, 1, transfer<float, float>(toRange(0.0f, 12.0f), [](float f) -> float {return std::exp(f);})),
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
    			    addVector({}, 2, std::vector<float>{sqrt(36.0f), sqrt(25.0f + 1.0f), sqrt(16.0f + 4.0f), sqrt(9.0f + 9.0f), sqrt(4.0f + 16.0f), sqrt(1.0f + 25.0f), sqrt(36.0f), sqrt(1.0f + 49.0f), sqrt(4.0f + 64.0f), sqrt(9.0f + 81.0f), sqrt(16.0f + 100.0f), sqrt(25.0f + 121.0f)}),
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
    			    addVector({}, 2, std::vector<float>{std::pow(0.0f, -6.0f), std::pow(1.0f, -5.0f), std::pow(2.0f, -4.0f), std::pow(3.0f, -3.0f), std::pow(4.0f, -2.0f), std::pow(5.0f, -1.0f), std::pow(6.0f, 0.0f), std::pow(7.0f, 1.0f), std::pow(8.0f, 2.0f), std::pow(9.0f, 3.0f), std::pow(10.0f, 4.0f), std::pow(11.0, 5.0f)}),
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
  } /* namespace test */
} /* namespace vc4c */

#endif /* VC4C_TEST_STDLIB_TESTS_H */