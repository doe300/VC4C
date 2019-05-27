/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PROFILER_H
#define PROFILER_H

#include <chrono>
#include <string>

namespace vc4c
{
#if DEBUG_MODE
#define PROFILE(func, ...)                                                                                             \
    profiler::ProfilingResult profile##func{#func, __FILE__, __LINE__, profiler::Clock::now()};                        \
    func(__VA_ARGS__);                                                                                                 \
    profiler::endFunctionCall(std::move(profile##func))

#define PROFILE_START(name)                                                                                            \
    profiler::ProfilingResult profile##name                                                                            \
    {                                                                                                                  \
        #name, __FILE__, __LINE__, profiler::Clock::now()                                                              \
    }
#define PROFILE_END(name) profiler::endFunctionCall(std::move(profile##name))

#define PROFILE_START_DYNAMIC(name)                                                                                    \
    profiler::ProfilingResult profile                                                                                  \
    {                                                                                                                  \
        name, __FILE__, __LINE__, profiler::Clock::now()                                                               \
    }
#define PROFILE_END_DYNAMIC(name) profiler::endFunctionCall(std::move(profile))

#define PROFILE_COUNTER(index, name, value) profiler::increaseCounter(index, name, value, __FILE__, __LINE__)
#define PROFILE_COUNTER_WITH_PREV(index, name, value, prevIndex)                                                       \
    profiler::increaseCounter(index, name, value, __FILE__, __LINE__, prevIndex)

#define PROFILE_RESULTS() profiler::dumpProfileResults()
#else
#define PROFILE(func, ...) func(__VA_ARGS__)

#define PROFILE_START(name)
#define PROFILE_END(name)

#define PROFILE_START_DYNAMIC(name)
#define PROFILE_END_DYNAMIC(name)

#define PROFILE_COUNTER(index, name, value)
#define PROFILE_COUNTER_WITH_PREV(index, name, value, prevIndex)

#define PROFILE_RESULTS()
#endif

    namespace profiler
    {
        using Clock = std::chrono::system_clock;
        using Duration = std::chrono::microseconds;

        struct ProfilingResult
        {
            std::string name;
            std::string fileName;
            std::size_t lineNumber;
            Clock::time_point startTime;
        };

        void endFunctionCall(ProfilingResult&& result);

        void dumpProfileResults(bool writeAsWarning = false);

        void increaseCounter(std::size_t index, std::string name, std::size_t value, std::string file, std::size_t line,
            std::size_t prevIndex = SIZE_MAX);

        /*
         * The following values are added to the sub counter index to get the absolute counter index.
         *
         * E.g. the second counter in the front-end is COUNTER_FRONTEND + 2
         */
        static constexpr std::size_t COUNTER_GENERAL = 0;
        static constexpr std::size_t COUNTER_FRONTEND = 10000;
        static constexpr std::size_t COUNTER_NORMALIZATION = 20000;
        static constexpr std::size_t COUNTER_OPTIMIZATION = 30000;
        static constexpr std::size_t COUNTER_BACKEND = 40000;
        static constexpr std::size_t COUNTER_EMULATOR = 100000;
    } // namespace profiler
} // namespace vc4c

#endif /* PROFILER_H */
