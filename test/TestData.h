/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#pragma once

#include <array>
#include <chrono>
#include <map>
#include <string>
#include <vector>

#ifndef NODISCARD
#if __cplusplus > 201402L
#define NODISCARD [[nodiscard]]
#else
#define NODISCARD __attribute__((warn_unused_result))
#endif
#endif

namespace test_data
{
    struct Result
    {
        bool wasSuccess;
        std::string error;

        inline operator bool() const noexcept
        {
            return wasSuccess;
        }
    };

    extern Result RESULT_OK;

    struct WorkDimensions
    {
        uint32_t dimensions = 3;
        std::array<uint32_t, 3> localSizes;
        std::array<uint32_t, 3> numGroups;
        std::array<uint32_t, 3> globalOffsets;
    };

    /**
     * Interface type for any piece of code which can execute a test (e.g. the emulator or the actual runtime).
     */
    struct TestRunner
    {
        virtual ~TestRunner() noexcept;

        /**
         * Compiles the sources from the given sourceCode string with the given options and stores the result
         * internally.
         *
         * NOTE: Implementations should cache the results of previous (successful) compilations for better performance.
         */
        NODISCARD virtual Result compile(const std::string& sourceCode, const std::string& options) = 0;

        /**
         * Selects the kernel with the given name as the "active" kernel to be used for all successive kernel-specific
         * calls until another kernel is selected.
         *
         * NOTE: Changing the "active" kernel disposes of all internal data structures used to hold data required for
         * the previous "active" kernel, e.g. argument buffers.
         */
        NODISCARD virtual Result selectKernel(const std::string& name) = 0;
        /**
         * Sets the kernel argument with the given index of the currently "active" kernel to the given binary value.
         *
         * NOTE: Implementations have to allocate and retain the necessary data structures for holding the argument data
         * until the "active" kernel is changed.
         */
        NODISCARD virtual Result setKernelArgument(
            std::size_t index, bool isLiteral, bool isVector, const void* byteData, std::size_t numBytes) = 0;
        template <typename T>
        NODISCARD Result setKernelArgument(std::size_t index, T scalarArg)
        {
            return setKernelArgument(index, true, false, &scalarArg, sizeof(scalarArg));
        }
        NODISCARD virtual Result setWorkDimensions(const WorkDimensions& dimensions) = 0;

        /**
         * Executes the configuration done so far in an implementation-specific way.
         */
        NODISCARD virtual Result execute() = 0;

        NODISCARD virtual Result getKernelArgument(std::size_t index, void* byteData, std::size_t bufferSize) = 0;
        template <typename T>
        NODISCARD Result getKernelArgument(std::size_t index, T& outputValue)
        {
            return getKernelArgument(index, &outputValue, sizeof(outputValue));
        }

        NODISCARD virtual Result validateFinish() = 0;
    };

    // Handle for the internal TestData structure
    class TestData;

    enum class DataFilter : uint32_t
    {
        NONE = 0,
        // The test uses "direct vector" parameter, which might not be supported by all runners
        VECTOR_PARAM = 0x1 << 0x1,
        // Focuses on integer arithmetic
        INT_ARITHMETIC = 0x1 << 0x2,
        // Focuses on float arithmetic
        FLOAT_ARITHMETIC = 0x1 << 0x3,
        // Uses 64-bit integers
        USES_LONG = 0x1 << 0x4,
        // Focuses on work-item/work-group functions
        WORK_GROUP = 0x1 << 0x5,
        // Focuses on memory access
        MEMORY_ACCESS = 0x1 << 0x6,
        // Runs a more complex calculation
        COMPLEX_KERNEL = 0x1 << 0x7,
        // Focuses on control flow (e.g. if-else, switches, loops)
        CONTROL_FLOW = 0x1 << 0x8,
        // Uses atomic_xxx standard library functions
        ATOMIC_FUNCTIONS = 0x1 << 0x9,
        // Uses async_xxx or barrier standard library functions
        ASYNC_BARRIER = 0x1 << 0xA,
        // Focuses on vector operations (e.g. folding, shuffling, rotation)
        VECTOR_OPERATIONS = 0x1 << 0xB,
        // Tests some corner cases (e.g. min/max values, Inf, NaN, etc.)
        CORNER_CASES = 0x1 << 0xC,
        // Tests some special type handling (e.g. literal vector parameters, structs, etc.)
        TYPE_HANDLING = 0x1 << 0xD,
        // Focuses on relational/comparison functions/operators
        COMPARISONS = 0x1 < 0xE,
        // Focuses on type conversions
        TYPE_CONVERSIONS = 0x1 < 0xF,
        // Disabled test, since the SPIR-V front-end does not support parts of it
        SPIRV_DISABLED = 0x40000000,
        // Disabled test, since it does not work right now
        DISABLED = 0x80000000,
        ALL = 0xFFFFFFFF
    };

    constexpr DataFilter operator|(DataFilter a, DataFilter b) noexcept
    {
        return static_cast<DataFilter>(
            static_cast<std::underlying_type_t<DataFilter>>(a) | static_cast<std::underlying_type_t<DataFilter>>(b));
    }

    constexpr DataFilter operator&(DataFilter a, DataFilter b) noexcept
    {
        return static_cast<DataFilter>(
            static_cast<std::underlying_type_t<DataFilter>>(a) & static_cast<std::underlying_type_t<DataFilter>>(b));
    }

    constexpr DataFilter operator~(DataFilter a) noexcept
    {
        return static_cast<DataFilter>(~static_cast<std::underlying_type_t<DataFilter>>(a));
    }

    constexpr DataFilter operator-(DataFilter a, DataFilter b) noexcept
    {
        return a & ~b;
    }

    constexpr DataFilter operator+(DataFilter a, DataFilter b) noexcept
    {
        return a | b;
    }

    std::map<std::string, const TestData*> getAllTests(DataFilter exclusionFilter = DataFilter::DISABLED);
    const TestData* getTest(const std::string& name);
    bool parseTestDataParameter(const std::string& param, std::vector<std::string>& enabledTests);
    NODISCARD Result execute(const TestData* key, TestRunner& runner);
} // namespace test_data
