
#include <atomic>
#include <chrono>
#include <cmath>
#include <iostream>
#include <limits>
#include <mutex>
#include <thread>
#include <vector>

union Converter
{
    float f;
    unsigned u;
};

static float divideWithError(float left, float right, unsigned maskBits)
{
    Converter c{1.0f / right};
    c.u |= maskBits;
    c.f = left * c.f;
    c.u |= maskBits;
    return c.f;
}

static std::mutex logMutex;

template <typename T>
static void testType(const std::string& typeName, T min, T max, std::atomic_uint64_t* numCalculations)
{
    for(int i = std::numeric_limits<T>::min(); i <= std::numeric_limits<T>::max(); ++i)
    {
        for(int k = min; k <= max; ++k)
        {
            if(k == 0)
                continue;
            auto floatResult = static_cast<T>(divideWithError(static_cast<float>(i), static_cast<float>(k), 3));
            // fix-up errors, e.g. 2a / a, could result in 1.9999... which is truncated to 1 instead of 2
            auto diff = static_cast<float>(i) - (static_cast<float>(floatResult) * static_cast<float>(k));
            auto increment = (i < 0) ^ (k < 0) ? -1 : 1;
            if(std::abs(diff) >= std::abs(static_cast<float>(k)))
                floatResult += increment;
            if(floatResult != (i / k))
            {
                std::lock_guard<std::mutex> guard{logMutex};
                std::cout << "Mismatch " << typeName << ": " << static_cast<int>(i) << '/' << static_cast<int>(k)
                          << " = " << static_cast<int>(floatResult) << " (expected " << static_cast<int>(i / k) << ")"
                          << std::endl;
            }
            ++(*numCalculations);
        }
    }
}

int main(int argc, char** argv)
{
    std::atomic_uint64_t counter{0};
    auto start = std::chrono::steady_clock::now();
    testType<char>("char", std::numeric_limits<char>::min(), std::numeric_limits<char>::max(), &counter);
    if(counter != (uint64_t{256} * uint64_t{256} - uint64_t{256} /* skip division by zero */))
        std::cout << "Wrong number of operations tested for char: " << counter.load() << std::endl;
    std::cout << "Testing for char took "
              << std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start).count()
              << "ms" << std::endl;

    counter = 0;
    start = std::chrono::steady_clock::now();
    testType<unsigned char>(
        "uchar", std::numeric_limits<unsigned char>::min(), std::numeric_limits<unsigned char>::max(), &counter);
    if(counter != (uint64_t{256} * uint64_t{256} - uint64_t{256} /* skip division by zero */))
        std::cout << "Wrong number of operations tested for uchar: " << counter.load() << std::endl;
    std::cout << "Testing for uchar took "
              << std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start).count()
              << "ms" << std::endl;

    std::vector<std::thread> threads;
    auto numThreads = std::thread::hardware_concurrency();
    threads.reserve(numThreads);

    {
        counter = 0;
        start = std::chrono::steady_clock::now();
        auto range = (static_cast<int64_t>(std::numeric_limits<short>::max()) -
                         static_cast<int64_t>(std::numeric_limits<short>::min())) /
            numThreads;
        auto startOffset = std::numeric_limits<short>::min();
        for(unsigned i = 0; i < numThreads; ++i)
        {
            threads.emplace_back(
                testType<short>, "short", startOffset, startOffset + static_cast<short>(range), &counter);
            startOffset += static_cast<short>(range);
        }
        for(auto& t : threads)
            t.join();
        if(counter != (uint64_t{65536} * uint64_t{65536} - uint64_t{65536} /* skip division by zero */))
            std::cout << "Wrong number of operations tested for short: " << counter.load() << std::endl;
        std::cout
            << "Testing for short took "
            << std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start).count()
            << "ms" << std::endl;
    }

    {
        counter = 0;
        start = std::chrono::steady_clock::now();
        auto range = (static_cast<uint64_t>(std::numeric_limits<unsigned short>::max()) -
                         static_cast<uint64_t>(std::numeric_limits<unsigned short>::min())) /
            numThreads;
        auto startOffset = std::numeric_limits<unsigned short>::min();
        for(unsigned i = 0; i < numThreads; ++i)
        {
            threads[i] = std::thread{testType<unsigned short>, "ushort", startOffset,
                startOffset + static_cast<unsigned short>(range), &counter};
            startOffset += static_cast<unsigned short>(range);
        }
        for(auto& t : threads)
            t.join();
        if(counter != (uint64_t{65536} * uint64_t{65536} - uint64_t{65536} /* skip division by zero */))
            std::cout << "Wrong number of operations tested for ushort: " << counter.load() << std::endl;
        std::cout
            << "Testing for ushort took "
            << std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start).count()
            << "ms" << std::endl;
    }

    return 0;
}