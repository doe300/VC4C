/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_EMULATION_HELPER_H
#define VC4C_TEST_EMULATION_HELPER_H

#include "cpptest.h"

#include "../src/helper.h"
#include "Compiler.h"
#include "VC4C.h"
#include "tools.h"

#include <array>
#include <cstring>
#include <functional>
#include <limits>
#include <random>
#include <sstream>

// just to check that really the long- and double-versions are used
template <typename T>
struct UniformDistribution
{
};

template <>
struct UniformDistribution<long> : public std::uniform_int_distribution<long>
{
    explicit UniformDistribution(long min, long max) : std::uniform_int_distribution<long>(min, max) {}
};

template <>
struct UniformDistribution<double> : public std::uniform_real_distribution<double>
{
    explicit UniformDistribution(double min, double max) : std::uniform_real_distribution<double>(min, max) {}
};

/*
 * Normal distribution with additional limiting the values to be in a specific range
 *
 * NOTE: To not discard too many values, the mean and deviation should be set to match the range
 */
template <long Mean = 0, long Deviation = std::numeric_limits<long>::max()>
struct NormalDistribution : public std::normal_distribution<double>
{
    explicit NormalDistribution(double min, double max) :
        std::normal_distribution<double>(static_cast<double>(Mean), static_cast<double>(Deviation)), min(min), max(max)
    {
    }

    template <typename RNG>
    double operator()(RNG& rng)
    {
        // discards all values outside of the limits
        while(true)
        {
            auto t = std::normal_distribution<double>::operator()(rng);
            if(t > min && t < max)
                return t;
        }
    }

    double min;
    double max;
};

template <typename T>
using InternalType = typename std::conditional<std::is_floating_point<T>::value, double, long>::type;

template <typename T, std::size_t N, typename Limits = T, typename Distribution = UniformDistribution<InternalType<T>>,
    typename Internal = InternalType<T>>
std::array<T, N> generateInput(bool allowNull,
    Internal min = static_cast<Internal>(std::numeric_limits<Limits>::lowest()),
    Internal max = static_cast<Internal>(std::numeric_limits<Limits>::max()))
{
    std::array<T, N> arr;
    std::random_device rd;
    std::mt19937 gen(rd());
    // NOTE: double here allows for float::lowest() and float::max() to be used, see also
    // https://stackoverflow.com/a/36826730/8720655
    Distribution dis(min, max);

    for(std::size_t i = 0; i < N; ++i)
    {
        T tmp;
        do
        {
            // to prevent division by zero
            tmp = static_cast<T>(dis(gen));
        } while(!allowNull && tmp == static_cast<T>(0));
        arr[i] = tmp;
    }

    return arr;
}

template <typename Result, typename Input, std::size_t N, typename Comparison = std::equal_to<Result>>
void checkUnaryResults(const std::array<Input, N>& input, const std::array<Result, N>& output,
    const std::function<Result(Input)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Comparison c;
    for(std::size_t i = 0; i < N; ++i)
    {
        if(!c(output[i], op(input[i])))
        {
            auto result = std::to_string(output[i]);
            auto expected = opName + " " + std::to_string(input[i]) + " = " + std::to_string(op(input[i]));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, typename Comparison = std::equal_to<Result>, typename Input2 = Input>
void checkBinaryResults(const std::array<Input, N>& input0, const std::array<Input2, N>& input1,
    const std::array<Result, N>& output, const std::function<Result(Input, Input2)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Comparison c;
    for(std::size_t i = 0; i < N; ++i)
    {
        if(!c(output[i], op(input0[i], input1[i])))
        {
            auto result = std::to_string(output[i]);
            auto expected = std::to_string(input0[i]) + " " + opName + " " + std::to_string(input1[i]) + " = " +
                std::to_string(op(input0[i], input1[i]));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, typename Comparison = std::equal_to<Result>>
void checkTernaryResults(const std::array<Input, N>& input0, const std::array<Input, N>& input1,
    const std::array<Input, N>& input2, const std::array<Result, N>& output,
    const std::function<Result(Input, Input, Input)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Comparison c;
    for(std::size_t i = 0; i < N; ++i)
    {
        if(!c(output[i], op(input0[i], input1[i], input2[i])))
        {
            auto result = std::to_string(output[i]);
            auto expected = std::to_string(input0[i]) + " " + opName + " " + std::to_string(input1[i]) + ", " +
                std::to_string(input2[i]) + " = " + std::to_string(op(input0[i], input1[i], input2[i]));
            onError(expected, result);
        }
    }
}

template <typename C>
static std::string toString(const C& container)
{
    return std::accumulate(container.begin(), container.end(), std::string{},
        [](const std::string s, const typename C::value_type v) -> std::string {
            return s + ", " + std::to_string(v);
        });
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = std::equal_to<Result>>
void checkUnaryReducedResults(const std::array<Input, N>& input, const std::array<Result, N>& output,
    const std::function<Result(const std::array<Input, GroupSize>&)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    static_assert(N >= GroupSize && N % GroupSize == 0, "The elements are not a multiple of the group size");
    Comparison c;
    for(std::size_t i = 0; i < N; i += GroupSize)
    {
        auto group = reinterpret_cast<const std::array<Input, GroupSize>*>(&input[i]);
        if(!c(output[i / GroupSize], op(*group)))
        {
            auto result = std::to_string(output[i / GroupSize]);
            auto expected = opName + " " + toString(*group) + " = " + std::to_string(op(*group));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = std::equal_to<Result>>
void checkBinaryReducedResults(const std::array<Input, N>& input0, const std::array<Input, N>& input1,
    const std::array<Result, N>& output,
    const std::function<Result(const std::array<Input, GroupSize>&, const std::array<Input, GroupSize>&)>& op,
    const std::string& opName, const std::function<void(const std::string&, const std::string&)>& onError)
{
    static_assert(N >= GroupSize && N % GroupSize == 0, "The elements are not a multiple of the group size");
    Comparison c;
    for(std::size_t i = 0; i < N; i += GroupSize)
    {
        auto group0 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input0[i]);
        auto group1 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input1[i]);
        if(!c(output[i / GroupSize], op(*group0, *group1)))
        {
            auto result = std::to_string(output[i / GroupSize]);
            auto expected = opName + " {" + toString(*group0) + "}, {" + toString(*group1) +
                "} = " + std::to_string(op(*group0, *group1));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = std::equal_to<std::array<Result, GroupSize>>>
void checkUnaryGroupedResults(const std::array<Input, N>& input, const std::array<Result, N>& output,
    const std::function<std::array<Result, GroupSize>(const std::array<Input, GroupSize>&)>& op,
    const std::string& opName, const std::function<void(const std::string&, const std::string&)>& onError)
{
    static_assert(N >= GroupSize && N % GroupSize == 0, "The elements are not a multiple of the group size");
    Comparison c;
    for(std::size_t i = 0; i < N; i += GroupSize)
    {
        auto inputGroup = reinterpret_cast<const std::array<Input, GroupSize>*>(&input[i]);
        auto outputGroup = reinterpret_cast<const std::array<Input, GroupSize>*>(&output[i]);
        if(!c(*outputGroup, op(*inputGroup)))
        {
            auto result = toString(*outputGroup);
            auto expected = opName + " " + toString(*inputGroup) + " = " + toString(op(*inputGroup));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = std::equal_to<std::array<Result, GroupSize>>>
void checkBinaryGroupedResults(const std::array<Input, N>& input0, const std::array<Input, N>& input1,
    const std::array<Result, N>& output,
    const std::function<std::array<Result, GroupSize>(
        const std::array<Input, GroupSize>&, const std::array<Input, GroupSize>&)>& op,
    const std::string& opName, const std::function<void(const std::string&, const std::string&)>& onError)
{
    static_assert(N >= GroupSize && N % GroupSize == 0, "The elements are not a multiple of the group size");
    Comparison c;
    for(std::size_t i = 0; i < N; i += GroupSize)
    {
        auto inputGroup0 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input0[i]);
        auto inputGroup1 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input1[i]);
        auto outputGroup = reinterpret_cast<const std::array<Input, GroupSize>*>(&output[i]);
        if(!c(*outputGroup, op(*inputGroup0, *inputGroup1)))
        {
            auto result = toString(*outputGroup);
            auto expected = opName + " {" + toString(*inputGroup0) + "}, {" + toString(*inputGroup1) +
                "} = " + toString(op(*inputGroup0, *inputGroup1));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = std::equal_to<std::array<Result, GroupSize>>>
void checkTrinaryGroupedResults(const std::array<Input, N>& input0, const std::array<Input, N>& input1,
    const std::array<Input, N>& input2, const std::array<Result, N>& output,
    const std::function<std::array<Result, GroupSize>(const std::array<Input, GroupSize>&,
        const std::array<Input, GroupSize>&, const std::array<Input, GroupSize>&)>& op,
    const std::string& opName, const std::function<void(const std::string&, const std::string&)>& onError)
{
    static_assert(N >= GroupSize && N % GroupSize == 0, "The elements are not a multiple of the group size");
    Comparison c;
    for(std::size_t i = 0; i < N; i += GroupSize)
    {
        auto inputGroup0 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input0[i]);
        auto inputGroup1 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input1[i]);
        auto inputGroup2 = reinterpret_cast<const std::array<Input, GroupSize>*>(&input2[i]);
        auto outputGroup = reinterpret_cast<const std::array<Input, GroupSize>*>(&output[i]);
        if(!c(*outputGroup, op(*inputGroup0, *inputGroup1, *inputGroup2)))
        {
            auto result = toString(*outputGroup);
            auto expected = opName + " {" + toString(*inputGroup0) + "}, {" + toString(*inputGroup1) + "}, {" +
                toString(*inputGroup2) + "} = " + toString(op(*inputGroup0, *inputGroup1, *inputGroup2));
            onError(expected, result);
        }
    }
}

inline void compileBuffer(
    vc4c::Configuration& config, std::stringstream& buffer, const std::string& source, const std::string& options)
{
    config.outputMode = vc4c::OutputMode::BINARY;
    config.writeKernelInfo = true;
    std::istringstream input(source);
    vc4c::Compiler::compile(input, buffer, config, options);
}

template <std::size_t N, typename In, typename Out>
void copyConvert(const In& in, Out& out)
{
    if(out.size() < N)
        throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL, "Invalid container size for copy");
    auto base = reinterpret_cast<const typename Out::value_type*>(in.data());
    std::copy(base, base + N, out.data());
}

template <typename Input, typename Result, std::size_t VectorWidth, std::size_t LocalSize, std::size_t NumGroups = 1>
std::array<Result, VectorWidth * LocalSize * NumGroups> runEmulation(std::stringstream& codeBuffer,
    const std::vector<std::array<Input, VectorWidth * LocalSize * NumGroups>>& inputs,
    const std::string& kernelName = "test")
{
    using namespace vc4c::tools;

    std::vector<std::pair<uint32_t, vc4c::Optional<std::vector<uint32_t>>>> parameter;
    parameter.emplace_back(std::make_pair(
        0, std::vector<uint32_t>(VectorWidth * LocalSize * NumGroups * sizeof(Result) / sizeof(uint32_t))));
    for(const auto& input : inputs)
    {
        parameter.emplace_back(std::make_pair(
            0, std::vector<uint32_t>(VectorWidth * LocalSize * NumGroups * sizeof(Input) / sizeof(uint32_t))));
        copyConvert<VectorWidth * LocalSize * NumGroups * sizeof(Input) / sizeof(uint32_t)>(
            input, parameter.back().second.value());
    }

    WorkGroupConfig workGroups;
    workGroups.dimensions = 1;
    workGroups.localSizes[0] = LocalSize;
    workGroups.numGroups[0] = NumGroups;

    EmulationData data(codeBuffer, kernelName, parameter, workGroups);

    auto result = emulate(data);

    if(!result.executionSuccessful)
        throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL, "Kernel execution failed");

    std::array<Result, VectorWidth * LocalSize * NumGroups> output;
    copyConvert<VectorWidth * LocalSize * NumGroups>(result.results[0].second.value(), output);
    return output;
}

template <std::size_t ULP>
struct CompareULP
{
    bool operator()(float a, float b) const
    {
        auto delta = a * ULP * std::numeric_limits<float>::epsilon();
        return Test::Comparisons::inMaxDistance(a, b, delta);
    }
};

template <std::size_t N, std::size_t ULP>
struct CompareArrayULP
{
    bool operator()(const std::array<float, N>& a, const std::array<float, N>& b) const
    {
        for(std::size_t i = 0; i < N; ++i)
        {
            auto delta = a[i] * ULP * std::numeric_limits<float>::epsilon();
            if(!Test::Comparisons::inMaxDistance(a[i], b[i], delta))
                return false;
        }
        return true;
    }
};

#endif /* VC4C_TEST_EMULATION_HELPER_H */
