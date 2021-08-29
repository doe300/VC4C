/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#pragma once

#include "TestData.h"

#include <algorithm>
#include <cfenv>
#include <cmath>
#include <functional>
#include <iomanip>
#include <limits>
#include <numeric>
#include <random>
#include <sstream>
#include <stdexcept>

namespace test_data
{
    using ArgumentSetter = std::function<Result(std::size_t, TestRunner&)>;
    using ResultVerification = std::function<Result(TestRunner&)>;

    class TestData
    {
    public:
        std::string uniqueName;
        DataFilter filter;
        const std::string* sources;
        std::string compilationOptions;
        std::string kernelName;
        std::vector<ArgumentSetter> kernelArguments;
        WorkDimensions workDimensions;
        std::vector<ResultVerification> verifications;
    };

    template <typename T>
    static ArgumentSetter toScalarParameter(T val)
    {
        return [val](std::size_t index, TestRunner& runner) -> Result { return runner.setKernelArgument(index, val); };
    }

    template <typename T>
    static ArgumentSetter toVectorParameter(std::vector<T>&& val)
    {
        return [val{std::move(val)}](std::size_t index, TestRunner& runner) -> Result {
            return runner.setKernelArgument(index, true, true, val.data(), val.size() * sizeof(T));
        };
    }

    template <typename T>
    static ArgumentSetter toBufferParameter(std::vector<T>&& val)
    {
        return [val{std::move(val)}](std::size_t index, TestRunner& runner) -> Result {
            return runner.setKernelArgument(index, false, true, val.data(), val.size() * sizeof(T));
        };
    }

    template <typename T>
    std::vector<T> toRange(T start, T end, T step = 1)
    {
        auto comp = start < end ? [](T a, T b) { return a < b; } : [](T a, T b) { return a > b; };
        std::vector<T> out;
        for(T val = start; comp(val, end); val = static_cast<T>(val + step))
            out.push_back(val);
        return out;
    }

    template <typename R, typename T = R, typename Func = std::function<R(T)>>
    std::vector<R> transform(const std::vector<T>& arg, const Func& func)
    {
        std::vector<R> result(arg.size());
        for(std::size_t i = 0; i < arg.size(); ++i)
        {
            result[i] = func(arg[i]);
        }
        return result;
    }

    template <typename R, typename T = R, typename U = R, typename Func = std::function<R(T, U)>>
    std::vector<R> transform(const std::vector<T>& arg0, const std::vector<U>& arg1, const Func& func)
    {
        auto numElements = std::min(arg0.size(), arg1.size());
        std::vector<R> result(numElements);
        for(std::size_t i = 0; i < numElements; ++i)
        {
            result[i] = func(arg0[i], arg1[i]);
        }
        return result;
    }

    template <typename R, typename T = R, typename U = R, typename V = R, typename Func = std::function<R(T, U, V)>>
    std::vector<R> transform(
        const std::vector<T>& arg0, const std::vector<U>& arg1, const std::vector<V>& arg2, const Func& func)
    {
        auto numElements = std::min(std::min(arg0.size(), arg1.size()), arg2.size());
        std::vector<R> result(numElements);
        for(std::size_t i = 0; i < numElements; ++i)
        {
            result[i] = func(arg0[i], arg1[i], arg2[i]);
        }
        return result;
    }

    template <typename R, std::size_t GroupSize, typename T = R,
        typename Func = std::function<std::array<R, GroupSize>(const T*, std::size_t)>>
    std::vector<R> transform(const std::vector<T>& arg0, const Func& func)
    {
        std::vector<R> result;
        result.reserve(arg0.size());
        for(std::size_t i = 0; (i + GroupSize) <= arg0.size(); i += GroupSize)
        {
            auto tmp = func(arg0.data() + i, GroupSize);
            result.insert(result.end(), tmp.begin(), tmp.end());
        }
        return result;
    }

    template <typename R, std::size_t GroupSize, typename T = R, typename U = R,
        typename Func = std::function<std::array<R, GroupSize>(const T*, const U*, std::size_t)>>
    std::vector<R> transform(const std::vector<T>& arg0, const std::vector<U>& arg1, const Func& func)
    {
        auto numElements = std::min(arg0.size(), arg1.size());
        std::vector<R> result;
        result.reserve(numElements);
        for(std::size_t i = 0; (i + GroupSize) <= numElements; i += GroupSize)
        {
            auto tmp = func(arg0.data() + i, arg1.data() + i, GroupSize);
            result.insert(result.end(), tmp.begin(), tmp.end());
        }
        return result;
    }

    template <typename R, std::size_t GroupSize, typename T = R,
        typename Func = std::function<R(const T*, std::size_t)>>
    std::vector<R> reduce(const std::vector<T>& arg0, const Func& func)
    {
        auto numElements = arg0.size() / GroupSize;
        std::vector<R> result(numElements);
        for(std::size_t i = 0; i < numElements; ++i)
        {
            result[i] = func(arg0.data() + i * GroupSize, GroupSize);
        }
        return result;
    }

    template <typename R, std::size_t GroupSize, typename T = R, typename U = R,
        typename Func = std::function<R(const T*, const U*, std::size_t)>>
    std::vector<R> reduce(const std::vector<T>& arg0, const std::vector<U>& arg1, const Func& func)
    {
        auto numElements = std::min(arg0.size(), arg1.size()) / GroupSize;
        std::vector<R> result(numElements);
        for(std::size_t i = 0; i < numElements; ++i)
        {
            result[i] = func(arg0.data() + i * GroupSize, arg1.data() + i * GroupSize, GroupSize);
        }
        return result;
    }

    template <typename R, std::size_t GroupSize, typename T = R,
        typename Func = std::function<std::array<R, GroupSize>(const T*)>>
    std::vector<R> expand(const std::vector<T>& arg0, const Func& func)
    {
        auto numElements = arg0.size() * GroupSize;
        std::vector<R> result(numElements);
        for(std::size_t i = 0; i < numElements; i += GroupSize)
        {
            auto tmp = func(arg0.data() + i);
            std::copy(tmp.begin(), tmp.end(), result.begin() + i);
        }
        return result;
    }

    template <typename R>
    static std::function<R(float)> roundToZero(const std::function<R(float)>& func)
    {
        return [&](float in) -> R {
            auto origMode = fegetround();
            // emulate the VideoCore IV rounding mode, truncate to zero
            fesetround(FE_TOWARDZERO);
            auto tmp = func(in);
            fesetround(origMode);
            return tmp;
        };
    }

    template <typename R>
    static std::function<R(float, float)> roundToZero(const std::function<R(float, float)>& func)
    {
        return [&](float in1, float in2) -> R {
            auto origMode = fegetround();
            // emulate the VideoCore IV rounding mode, truncate to zero
            fesetround(FE_TOWARDZERO);
            auto tmp = func(in1, in2);
            fesetround(origMode);
            return tmp;
        };
    }

    // just to check that really the long- and double-versions are used
    template <typename T>
    struct UniformDistribution
    {
    };

    template <>
    struct UniformDistribution<int64_t> : public std::uniform_int_distribution<int64_t>
    {
        explicit UniformDistribution(int64_t min, int64_t max) : std::uniform_int_distribution<int64_t>(min, max) {}
    };

    template <>
    struct UniformDistribution<double> : public std::uniform_real_distribution<double>
    {
        explicit UniformDistribution(double min, double max) : std::uniform_real_distribution<double>(min, max) {}
    };

    template <typename T, typename InnerType = std::conditional_t<std::is_same<float, T>::value, double, int64_t>,
        typename Distribution = UniformDistribution<InnerType>>
    std::vector<T> toRandom(std::size_t numValues, bool allowNull, T minValue = std::numeric_limits<T>::lowest(),
        T maxValue = std::numeric_limits<T>::max())
    {
        std::vector<T> result(numValues, {});
        std::random_device rd;
        std::mt19937 gen(rd());
        // NOTE: double here allows for float::lowest() and float::max() to be used, see also
        // https://stackoverflow.com/a/36826730/8720655
        Distribution dis(static_cast<typename Distribution::result_type>(minValue),
            static_cast<typename Distribution::result_type>(maxValue));

        for(std::size_t i = 0; i < numValues; ++i)
        {
            T tmp;
            do
            {
                // e.g. to prevent division by zero
                tmp = static_cast<T>(dis(gen));
            } while(!allowNull && tmp == static_cast<T>(0));
            result[i] = tmp;
        }

        return result;
    }

    inline WorkDimensions toDimensions(uint32_t localSizeX, uint32_t localSizeY = 1, uint32_t localSizeZ = 1,
        uint32_t numGroupsX = 1, uint32_t numGroupsY = 1, uint32_t numGroupsZ = 1, uint32_t globalOffsetX = 0,
        uint32_t globalOffsetY = 0, uint32_t globalOffsetZ = 0)
    {
        WorkDimensions config;
        config.dimensions = 3;
        config.localSizes[0] = localSizeX;
        config.localSizes[1] = localSizeY;
        config.localSizes[2] = localSizeZ;
        config.numGroups[0] = numGroupsX;
        config.numGroups[1] = numGroupsY;
        config.numGroups[2] = numGroupsZ;
        config.globalOffsets[0] = globalOffsetX;
        config.globalOffsets[1] = globalOffsetY;
        config.globalOffsets[2] = globalOffsetZ;

        return config;
    }

    inline WorkDimensions calculateDimensions(std::size_t numElements, std::size_t vectorWidth = 16)
    {
        auto numWorkItems = numElements / vectorWidth;
        if((numElements % vectorWidth) != 0)
            ++numWorkItems;
        for(uint32_t groupSize = 12; groupSize > 0; --groupSize)
        {
            if(numWorkItems % groupSize == 0)
            {
                auto numGroups = static_cast<uint32_t>(numWorkItems / groupSize);
                return toDimensions(groupSize, 1, 1, numGroups, 1, 1);
            }
        }
        throw std::invalid_argument{"Work cannot be distributed across work-groups: " + std::to_string(numElements)};
    }

    template <typename T>
    T makePrintable(T val)
    {
        return val;
    }

    inline int32_t makePrintable(int8_t val)
    {
        // promote to integer, so they are not printed as characters, but as numbers
        return val;
    }

    inline uint32_t makePrintable(uint8_t val)
    {
        // promote to integer, so they are not printed as characters, but as numbers
        return val;
    }

    template <typename T>
    struct DefaultPrinter
    {
        static std::string print(T val)
        {
            return std::to_string(makePrintable(val));
        }
    };

    template <typename T>
    struct HexPrinter
    {
        static std::string print(T val)
        {
            std::ostringstream ss;
            ss << "0x" << std::setfill('0') << std::setw(sizeof(T) * 2) << std::hex << val;
            return ss.str();
        }
    };

    template <typename T, typename Printer = DefaultPrinter<T>>
    Result checkScalarEquals(T expected, T actual, std::string&& additionalInfo = "")
    {
        if(expected != actual)
            return Result{false,
                "Validation error: Got " + Printer::print(actual) + ", expected " + Printer::print(expected) +
                    std::move(additionalInfo)};
        return RESULT_OK;
    }

    template <typename T>
    bool isInMaxDistance(T expected, T actual, T maxDistance)
    {
        if(std::isnan(expected) && std::isnan(actual))
            return true;
        if(std::isinf(expected) && std::isinf(actual) && std::signbit(expected) == std::signbit(actual))
            return true;
        if(std::abs(expected) < std::numeric_limits<float>::min() &&
            std::abs(actual) < std::numeric_limits<float>::min())
            // truncate all denormal values to zero, also merge all positive and negative denormal/zero values
            return true;
        if(std::abs(expected) != 0.0f && std::abs(actual) != 0.0f && std::signbit(expected) == std::signbit(actual) &&
            std::abs(expected) <= std::numeric_limits<float>::min() &&
            std::abs(actual) <= std::numeric_limits<float>::min())
            // If both values are either the normal value closest to zero or a subnormal value and have the same sign,
            // consider them equal. This also applies for a maximum distance of zero!
            return true;
        if(std::fpclassify(maxDistance) == FP_SUBNORMAL)
            // Convert any subnormal maximum distance to the lowest normal value
            maxDistance = std::numeric_limits<float>::min();
        return ((actual - expected) * (actual > expected ? 1 : -1)) <= (maxDistance < 0 ? -maxDistance : maxDistance);
    }

    template <typename T>
    Result checkScalarEqualsUlp(
        T expected, T actual, std::size_t ulp, std::string&& additionalInfo = "", bool onlyError = false)
    {
        auto maxDistance = expected * static_cast<T>(ulp) * std::numeric_limits<T>::epsilon();
        if(!isInMaxDistance(expected, actual, maxDistance))
        {
            auto realDelta = static_cast<std::size_t>(
                std::abs(std::ceil(static_cast<double>(std::max(expected, actual) - std::min(expected, actual)) /
                    (static_cast<double>(expected) * static_cast<double>(std::numeric_limits<T>::epsilon())))));
            std::stringstream ss;
            if(onlyError)
                ss << realDelta << " ULP";
            else
            {
                ss << std::scientific << std::setprecision(std::numeric_limits<T>::max_digits10);
                ss << "Validation error: Got " << makePrintable(actual) << ", expected " << makePrintable(expected)
                   << " with an error of " << std::dec << realDelta << ", allowed are " << ulp << " ULP"
                   << additionalInfo;
            }
            return Result{false, ss.str()};
        }
        return RESULT_OK;
    }

    template <typename T, typename Func, typename Printer = DefaultPrinter<T>>
    ResultVerification checkParameterMatches(
        std::size_t index, std::size_t numValues, Func&& func, const std::string& expectedMessage)
    {
        return [index, numValues, func{std::forward<Func>(func)}, expectedMessage](TestRunner& runner) -> Result {
            std::vector<T> resultData(numValues);
            auto result = runner.getKernelArgument(index, resultData.data(), resultData.size() * sizeof(T));
            if(!result)
                return result;
            std::string wrongIndices;
            std::string actualValues;
            for(std::size_t i = 0; i < numValues; i++)
            {
                if(!func(resultData[i], i))
                {
                    if(!actualValues.empty())
                        actualValues.append(", ");
                    actualValues.append(Printer::print(resultData[i]));
                    if(!wrongIndices.empty())
                        wrongIndices.append(", ");
                    wrongIndices.append(std::to_string(i));
                }
            }
            if(!actualValues.empty() || !wrongIndices.empty())
            {
                std::stringstream ss;
                ss << std::scientific << std::setprecision(std::numeric_limits<T>::max_digits10);
                ss << "Validation error: Got {" << actualValues << "}, expected " << expectedMessage << " for indices {"
                   << wrongIndices << "} of parameter " << index;
                return Result{false, ss.str()};
            }
            return RESULT_OK;
        };
    }

    template <typename T>
    struct CompareEquals
    {
        using type = T;

        Result operator()(T a, T b, std::size_t index) const
        {
            return Result{a == b, ""};
        }

        std::string getAdditionalInfo() const
        {
            return {};
        }
    };

    template <std::size_t ULP>
    struct CompareULP
    {
        using type = float;

        Result operator()(float a, float b, std::size_t index) const
        {
            return checkScalarEqualsUlp(a, b, ULP, "", true);
        }

        std::string getAdditionalInfo() const
        {
            return "allowed are " + std::to_string(ULP) + " ULP";
        }
    };

    struct CompareAbsoluteError
    {
        using type = float;

        Result operator()(float a, float b, std::size_t index) const
        {
            if(!isInMaxDistance(a, b, allowedError))
            {
                std::stringstream ss;
                ss << std::scientific << std::setprecision(std::numeric_limits<float>::max_digits10);
                ss << "+/- " << (std::max(a, b) - std::min(a, b));
                return Result{false, ss.str()};
            }
            return RESULT_OK;
        }

        std::string getAdditionalInfo() const
        {
            std::stringstream ss;
            ss << std::scientific << std::setprecision(std::numeric_limits<float>::max_digits10);
            ss << "allowed difference is +/- " << allowedError;
            return ss.str();
        }

        const float allowedError;
    };

    struct CompareDynamicError
    {
        using type = float;

        Result operator()(float a, float b, std::size_t index) const
        {
            auto maxDistance = index < allowedErrors.size() ? allowedErrors[index] : 0.0f;
            if(!isInMaxDistance(a, b, maxDistance))
            {
                std::stringstream ss;
                ss << std::scientific << std::setprecision(std::numeric_limits<float>::max_digits10);
                ss << "+/- " << (std::max(a, b) - std::min(a, b));
                // print the allowed error here, since it differs for each element
                ss << ", allowed is +/- " << maxDistance;
                return Result{false, ss.str()};
            }
            return RESULT_OK;
        }

        std::string getAdditionalInfo() const
        {
            // TODO
            return "";
        }

        const std::vector<float> allowedErrors;
    };

    struct CompareDynamicULP
    {
        using type = float;

        Result operator()(float a, float b, std::size_t index) const
        {
            return checkScalarEqualsUlp(a, b, ULP, "", true);
        }

        std::string getAdditionalInfo() const
        {
            return "allowed are " + std::to_string(ULP) + " ULP";
        }

        std::size_t ULP;
    };

    template <typename Comparison, typename T = typename Comparison::type, typename Printer = DefaultPrinter<T>>
    ResultVerification checkParameter(std::size_t index, std::vector<T>&& expected, Comparison comp = {})
    {
        return [index, expected{std::move(expected)}, comp{std::move(comp)}](TestRunner& runner) -> Result {
            std::vector<T> resultData(expected.size());
            auto result = runner.getKernelArgument(index, resultData.data(), resultData.size() * sizeof(T));
            if(!result)
                return result;
            std::stringstream wrongIndices;
            wrongIndices << std::scientific << std::setprecision(std::numeric_limits<T>::max_digits10);
            std::stringstream expectedValues;
            expectedValues << std::scientific << std::setprecision(std::numeric_limits<T>::max_digits10);
            std::stringstream actualValues;
            actualValues << std::scientific << std::setprecision(std::numeric_limits<T>::max_digits10);
            bool noError = true;
            for(std::size_t i = 0; i < expected.size(); i++)
            {
                Result result;
                if(!(result = comp(expected[i], resultData[i], i)))
                {
                    if(!noError)
                    {
                        wrongIndices << ", ";
                        expectedValues << ", ";
                        actualValues << ", ";
                    }
                    noError = false;
                    expectedValues << Printer::print(expected[i]);
                    actualValues << Printer::print(resultData[i]);
                    if(!result.error.empty())
                        actualValues << " (" << result.error << ')';
                    wrongIndices << i;
                }
            }
            if(!noError)
            {
                std::stringstream ss;
                if(expected.size() == 1)
                    ss << "Validation error: Got " << actualValues.rdbuf() << ", expected " << expectedValues.rdbuf()
                       << " for index " << wrongIndices.rdbuf() << " of parameter " << index;
                else
                    ss << "Validation error: Got {" << actualValues.rdbuf() << "}, expected {" << expectedValues.rdbuf()
                       << "} for indices {" << wrongIndices.rdbuf() << "} of parameter " << index;
                auto info = comp.getAdditionalInfo();
                if(!info.empty())
                    ss << " (" << info << ')';
                return Result{false, ss.str()};
            }
            return RESULT_OK;
        };
    }

    template <typename T, typename Printer = DefaultPrinter<T>>
    ResultVerification checkParameterEquals(std::size_t index, std::vector<T>&& expected)
    {
        return checkParameter<CompareEquals<T>, typename CompareEquals<T>::type, Printer>(index, std::move(expected));
    }

    template <typename T, typename Func = std::function<Result(const std::vector<T>&)>>
    ResultVerification checkParameter(std::size_t index, std::size_t numEntries, Func&& predicate)
    {
        return [index, numEntries, predicate{std::forward<Func>(predicate)}](TestRunner& runner) -> Result {
            std::vector<T> resultData(numEntries);
            auto result = runner.getKernelArgument(index, resultData.data(), resultData.size() * sizeof(T));
            if(!result)
                return result;
            result = predicate(resultData);
            if(!result)
                result.error += " of parameter " + std::to_string(index);
            return result;
        };
    }

    void registerTest(TestData&& data);

    // The single TestData generators, roughly separated by category
    void registerGeneralTests();
    void registerArithmeticTests();
    void registerOpenCLCommonFunctionTests();
    void registerOpenCLGeometricFunctionTests();
    void registerOpenCLIntegerFunctionTests();
    void registerOpenCLRelationalFunctionTests();
    void registerMathTests();
    void registerMemoryTests();
    void registerTypeConversionTests();
    void registerVectorTests();

} // namespace test_data
