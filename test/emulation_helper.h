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

constexpr uint32_t maxExecutionCycles{1 << 16};

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

template <typename Distribution, unsigned InfPercentage = 10, unsigned NaNPercentage = 10>
struct InfNaNWrapper : public Distribution
{
    explicit InfNaNWrapper(double min, double max) : Distribution(min, max), infNaNDistribution(0.0, 1.0) {}

    static_assert(
        static_cast<float>(std::numeric_limits<double>::infinity()) == std::numeric_limits<float>::infinity(), "");
    static_assert(
        static_cast<float>(-std::numeric_limits<double>::infinity()) == -std::numeric_limits<float>::infinity(), "");

    template <typename RNG>
    double operator()(RNG& rng)
    {
        auto check = infNaNDistribution(rng);
        if(check < (static_cast<double>(InfPercentage / 2) / 100.0))
            return -std::numeric_limits<double>::infinity();
        if(check < (static_cast<double>(InfPercentage) / 100.0))
            return std::numeric_limits<double>::infinity();
        if(check < (static_cast<double>(InfPercentage + NaNPercentage) / 100.0))
            return std::numeric_limits<double>::quiet_NaN();
        return Distribution::operator()(rng);
    }

    std::uniform_real_distribution<double> infNaNDistribution;
};

template <typename T>
using InternalType = typename std::conditional<std::is_floating_point<T>::value, double, long>::type;

using InfNaNNormalDistribution = InfNaNWrapper<NormalDistribution<>>;
using InfNaNUniformDistribution = InfNaNWrapper<UniformDistribution<InternalType<float>>>;

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
    Distribution dis(
        static_cast<typename Distribution::result_type>(min), static_cast<typename Distribution::result_type>(max));

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

template <typename T>
struct CompareEqual;

template <typename Result, typename Input, std::size_t N, typename Comparison = CompareEqual<Result>>
void checkUnaryResults(const std::array<Input, N>& input, const std::array<Result, N>& output,
    const std::function<Result(Input)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Comparison c;
    for(std::size_t i = 0; i < N; ++i)
    {
        if(!c(output[i], op(input[i])))
        {
            auto result = Test::Formats::to_string(output[i]);
            auto expected = opName + " " + Test::Formats::to_string(input[i]) + " = " +
                Test::Formats::to_string(op(input[i])) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i)) : "") + c.difference(output[i], op(input[i]));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, typename Comparison = CompareEqual<Result>,
    typename Input2 = Input>
void checkBinaryResults(const std::array<Input, N>& input0, const std::array<Input2, N>& input1,
    const std::array<Result, N>& output, const std::function<Result(Input, Input2)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Comparison c;
    for(std::size_t i = 0; i < N; ++i)
    {
        if(!c(output[i], op(input0[i], input1[i])))
        {
            auto result = Test::Formats::to_string(output[i]);
            auto expected = Test::Formats::to_string(input0[i]) + " " + opName + " " +
                Test::Formats::to_string(input1[i]) + " = " + Test::Formats::to_string(op(input0[i], input1[i])) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i)) : "") +
                c.difference(output[i], op(input0[i], input1[i]));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, typename Comparison = CompareEqual<Result>,
    typename Input2 = Input, typename Input3 = Input>
void checkTernaryResults(const std::array<Input, N>& input0, const std::array<Input2, N>& input1,
    const std::array<Input3, N>& input2, const std::array<Result, N>& output,
    const std::function<Result(Input, Input2, Input3)>& op, const std::string& opName,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    Comparison c;
    for(std::size_t i = 0; i < N; ++i)
    {
        if(!c(output[i], op(input0[i], input1[i], input2[i])))
        {
            auto result = Test::Formats::to_string(output[i]);
            auto expected = Test::Formats::to_string(input0[i]) + " " + opName + " " +
                Test::Formats::to_string(input1[i]) + ", " + Test::Formats::to_string(input2[i]) + " = " +
                Test::Formats::to_string(op(input0[i], input1[i], input2[i])) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i)) : "") +
                c.difference(output[i], op(input0[i], input1[i], input2[i]));
            onError(expected, result);
        }
    }
}

template <typename C>
std::string toString(const C& container)
{
    return std::accumulate(container.begin(), container.end(), std::string{},
        [](const std::string s, const typename C::value_type v) -> std::string {
            return s + ", " + Test::Formats::to_string(v);
        });
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = CompareEqual<Result>>
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
            auto result = Test::Formats::to_string(output[i / GroupSize]);
            auto expected = opName + " " + toString(*group) + " = " + Test::Formats::to_string(op(*group)) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i) + " (group " +
                             Test::Formats::to_string(i / GroupSize) + ')') :
                         "") +
                c.difference(output[i / GroupSize], op(*group));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = CompareEqual<Result>>
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
            auto result = Test::Formats::to_string(output[i / GroupSize]);
            auto expected = opName + " {" + toString(*group0) + "}, {" + toString(*group1) +
                "} = " + Test::Formats::to_string(op(*group0, *group1)) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i) + " (group " +
                             Test::Formats::to_string(i / GroupSize) + ')') :
                         "") +
                c.difference(output[i / GroupSize], op(*group0, *group1));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = CompareEqual<std::array<Result, GroupSize>>>
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
            auto expected = opName + " " + toString(*inputGroup) + " = " + toString(op(*inputGroup)) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i) + " (group " +
                             Test::Formats::to_string(i / GroupSize) + ')') :
                         "") +
                c.difference(*outputGroup, op(*inputGroup));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = CompareEqual<std::array<Result, GroupSize>>>
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
                "} = " + toString(op(*inputGroup0, *inputGroup1)) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i) + " (group " +
                             Test::Formats::to_string(i / GroupSize) + ')') :
                         "") +
                c.difference(*outputGroup, op(*inputGroup0, *inputGroup1));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t N, std::size_t GroupSize = 16,
    typename Comparison = CompareEqual<std::array<Result, GroupSize>>>
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
                toString(*inputGroup2) + "} = " + toString(op(*inputGroup0, *inputGroup1, *inputGroup2)) +
                (N > 1 ? (" for element " + Test::Formats::to_string(i) + " (group " +
                             Test::Formats::to_string(i / GroupSize) + ')') :
                         "") +
                c.difference(*outputGroup, op(*inputGroup0, *inputGroup1, *inputGroup2));
            onError(expected, result);
        }
    }
}

template <typename Result, typename Input, std::size_t NumGroups, std::size_t InputGroupSize,
    std::size_t OutputGroupSize, std::size_t GroupSize = std::max(InputGroupSize, OutputGroupSize)>
void checkUnaryGroupedUnevenResults(const std::array<Input, GroupSize * NumGroups>& input,
    const std::array<Result, GroupSize * NumGroups>& output,
    const std::function<std::array<Result, OutputGroupSize>(const std::array<Input, InputGroupSize>&)>& op,
    const std::function<bool(const std::array<Result, OutputGroupSize>&, const std ::array<Result, OutputGroupSize>&)>&
        comp,
    const std::string& opName, const std::function<void(const std::string&, const std::string&)>& onError)
{
    for(std::size_t i = 0; i < NumGroups; ++i)
    {
        auto inputGroup = reinterpret_cast<const std::array<Input, InputGroupSize>*>(&input[i * InputGroupSize]);
        auto outputGroup = reinterpret_cast<const std::array<Input, OutputGroupSize>*>(&output[i * OutputGroupSize]);
        if(!comp(*outputGroup, op(*inputGroup)))
        {
            auto result = toString(*outputGroup);
            auto expected = opName + " " + toString(*inputGroup) + " = " + toString(op(*inputGroup)) +
                (" for group " + Test::Formats::to_string(i) + ')');
            onError(expected, result);
        }
    }
}

inline vc4c::CompilationData compileBuffer(
    vc4c::Configuration& config, const std::string& source, const std::string& options, const std::string& name = "")
{
    config.outputMode = vc4c::OutputMode::BINARY;
    config.writeKernelInfo = true;
    auto result = vc4c::Compiler::compile(
        vc4c::CompilationData{source.begin(), source.end(), vc4c::SourceType::OPENCL_C, name}, config, options);
    return result.first;
}

template <std::size_t N, typename In, typename Out>
void copyConvert(const In& in, Out& out)
{
    if(out.size() < N)
        throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL, "Invalid container size for copy");
    if(in.size() * sizeof(typename In::value_type) < N * sizeof(typename Out::value_type))
    {
        // special case for e.g. copying char/short to unsigned int
        auto dest = reinterpret_cast<typename In::value_type*>(out.data());
        std::copy(in.data(), in.data() + in.size(), dest);
    }
    else
    {
        auto base = reinterpret_cast<const typename Out::value_type*>(in.data());
        std::copy(base, base + N, out.data());
    }
}

template <typename Type, std::size_t VectorWidth, std::size_t LocalSize, std::size_t NumGroups>
constexpr std::size_t minBufferSize() noexcept
{
    // reserve at least 1 word, since otherwise buffers of less than a single word (e.g. single chars/shorts) have no
    // space to read from/write to
    return std::max(static_cast<std::size_t>(1), VectorWidth * LocalSize * NumGroups * sizeof(Type) / sizeof(uint32_t));
}

template <typename Input, typename Result, std::size_t VectorWidth, std::size_t LocalSize, std::size_t NumGroups = 1>
std::array<Result, VectorWidth * LocalSize * NumGroups> runEmulation(const vc4c::CompilationData& codeBuffer,
    const std::vector<std::array<Input, VectorWidth * LocalSize * NumGroups>>& inputs,
    const std::string& kernelName = "test")
{
    using namespace vc4c::tools;

    std::vector<std::pair<uint32_t, vc4c::Optional<std::vector<uint32_t>>>> parameter;
    parameter.emplace_back(
        std::make_pair(0, std::vector<uint32_t>(minBufferSize<Result, VectorWidth, LocalSize, NumGroups>())));
    for(const auto& input : inputs)
    {
        parameter.emplace_back(
            std::make_pair(0, std::vector<uint32_t>(minBufferSize<Input, VectorWidth, LocalSize, NumGroups>())));
        copyConvert<minBufferSize<Input, VectorWidth, LocalSize, NumGroups>()>(input, parameter.back().second.value());
    }

    WorkGroupConfig workGroups;
    workGroups.dimensions = 1;
    workGroups.localSizes[0] = LocalSize;
    workGroups.numGroups[0] = NumGroups;

    EmulationData data;
    data.module = codeBuffer;
    data.kernelName = kernelName;
    data.parameter = parameter;
    data.workGroup = workGroups;

    auto result = emulate(data);

    if(!result.executionSuccessful)
        throw vc4c::CompilationError(vc4c::CompilationStep::GENERAL, "Kernel execution failed");

    std::array<Result, VectorWidth * LocalSize * NumGroups> output{0};
    copyConvert<VectorWidth * LocalSize * NumGroups>(result.results[0].second.value(), output);
    return output;
}

template <typename T>
struct CompareEqual : public std::equal_to<T>
{
    std::string difference(T a, T b) const
    {
        return {};
    }
};

template <std::size_t ULP>
struct CompareULP
{
    bool operator()(float a, float b) const
    {
        if(std::isinf(a) && std::isinf(b) && std::signbit(a) == std::signbit(b))
            return true;
        if(std::isnan(a) && std::isnan(b))
            return true;
        auto delta = a * static_cast<float>(ULP) * std::numeric_limits<float>::epsilon();
        return Test::Comparisons::inMaxDistance(a, b, delta);
    }

    std::string difference(float a, float b) const
    {
        auto realDelta =
            static_cast<std::size_t>(std::abs(std::ceil(static_cast<double>(std::max(a, b) - std::min(a, b)) /
                (static_cast<double>(b) * static_cast<double>(std::numeric_limits<float>::epsilon())))));
        return " (error of " + std::to_string(realDelta) + ", allowed are " + std::to_string(ULP) + " ULP)";
    }
};

// ErrorProvider needs to have the function call operator with following signature: float(float,float)
template <typename ErrorProvider>
struct CompareAbsoluteError
{
    bool operator()(float a, float b) const
    {
        if(std::isinf(a) && std::isinf(b) && std::signbit(a) == std::signbit(b))
            return true;
        if(std::isnan(a) && std::isnan(b))
            return true;
        auto delta = ErrorProvider{}(a, b);
        return Test::Comparisons::inMaxDistance(a, b, delta);
    }

    std::string difference(float a, float b) const
    {
        auto error = std::max(a, b) - std::min(a, b);
        auto delta = ErrorProvider{}(a, b);
        return " (error of +/-" + std::to_string(error) + ", allowed is +/-" + std::to_string(delta) + ")";
    }
};

template <std::size_t N, std::size_t ULP>
struct CompareArrayULP
{
    bool operator()(const std::array<float, N>& a, const std::array<float, N>& b) const
    {
        for(std::size_t i = 0; i < N; ++i)
        {
            if(std::isinf(a[i]) && std::isinf(b[i]) && std::signbit(a[i]) == std::signbit(b[i]))
                continue;
            if(std::isnan(a[i]) && std::isnan(b[i]))
                continue;
            auto delta = a[i] * ULP * std::numeric_limits<float>::epsilon();
            if(!Test::Comparisons::inMaxDistance(a[i], b[i], delta))
                return false;
        }
        return true;
    }

    std::string difference(const std::array<float, N>& a, const std::array<float, N>& b) const
    {
        return {};
    }
};

#endif /* VC4C_TEST_EMULATION_HELPER_H */
