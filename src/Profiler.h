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
    static thread_local auto profileEntry##func =                                                                      \
        profiler::createEntry(reinterpret_cast<std::uintptr_t>(std::addressof(#func[0])), #func, __FILE__, __LINE__);  \
    auto profileStart##func = profiler::Clock::now();                                                                  \
    func(__VA_ARGS__);                                                                                                 \
    profiler::endFunctionCall(profileEntry##func, profileStart##func)

#define PROFILE_START(name)                                                                                            \
    static thread_local auto profileEntry##name =                                                                      \
        profiler::createEntry(reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), #name, __FILE__, __LINE__);  \
    auto profileStart##name = profiler::Clock::now()

#define PROFILE_END(name) profiler::endFunctionCall(profileEntry##name, profileStart##name)

#define PROFILE_START_DYNAMIC(name)                                                                                    \
    auto profileEntryDynamic = profiler::createEntry(std::hash<std::string>{}(name), name, __FILE__, __LINE__);        \
    auto profileStartDynamic = profiler::Clock::now()

#define PROFILE_END_DYNAMIC(name) profiler::endFunctionCall(profileEntryDynamic, profileStartDynamic)

#define PROFILE_SCOPE(name)                                                                                            \
    static thread_local auto profileEntry##name =                                                                      \
        profiler::createEntry(reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), #name, __FILE__, __LINE__);  \
    profiler::ProfilingScope profile##name(profileEntry##name)

#define PROFILE_COUNTER(index, name, value)                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        static thread_local auto constantCounter = profiler::createCounter(index, name, __FILE__, __LINE__);           \
        /* if the index is constant, reuse the statically queried counter, otherwise we need to requery the counter*/  \
        auto profilingCounter =                                                                                        \
            __builtin_constant_p(index) ? constantCounter : profiler::createCounter(index, name, __FILE__, __LINE__);  \
        profiler::increaseCounter(profilingCounter, value);                                                            \
    } while(false)

#define PROFILE_COUNTER_WITH_PREV(index, name, value, prevIndex)                                                       \
    do                                                                                                                 \
    {                                                                                                                  \
        static thread_local auto constantCounter =                                                                     \
            profiler::createCounter(index, name, __FILE__, __LINE__, prevIndex);                                       \
        /* if the index is constant, reuse the statically queried counter, otherwise we need to requery the counter*/  \
        auto profilingCounter = __builtin_constant_p(index) ?                                                          \
            constantCounter :                                                                                          \
            profiler::createCounter(index, name, __FILE__, __LINE__, prevIndex);                                       \
        profiler::increaseCounter(profilingCounter, value);                                                            \
    } while(false)

#define PROFILE_RESULTS() profiler::dumpProfileResults()

#define PROFILE_CREATE_THREAD_CACHE() profiler::startThreadCache()
#define PROFILE_FLUSH_THREAD_CACHE() profiler::flushThreadCache()
#else
#define PROFILE(func, ...) func(__VA_ARGS__)

#define PROFILE_START(name)
#define PROFILE_END(name)

#define PROFILE_START_DYNAMIC(name)
#define PROFILE_END_DYNAMIC(name)

#define PROFILE_SCOPE(name)

#define PROFILE_COUNTER(index, name, value)
#define PROFILE_COUNTER_WITH_PREV(index, name, value, prevIndex)

#define PROFILE_RESULTS()

#define PROFILE_CREATE_THREAD_CACHE()
#define PROFILE_FLUSH_THREAD_CACHE()
#endif

    namespace profiler
    {
        using Clock = std::chrono::system_clock;
        using Duration = std::chrono::microseconds;
        using HashKey = std::common_type<std::size_t, std::uintptr_t>::type;

        struct Entry;
        Entry* createEntry(HashKey key, std::string name, std::string fileName, std::size_t lineNumber);
        void endFunctionCall(Entry* entry, Clock::time_point startTime);

        void dumpProfileResults(bool writeAsWarning = false);

        struct Counter;
        Counter* createCounter(
            std::size_t index, std::string name, std::string file, std::size_t line, std::size_t prevIndex = SIZE_MAX);
        void increaseCounter(Counter* counter, std::size_t value);

        struct ProfilingScope
        {
            explicit ProfilingScope(Entry* entry) : entry(entry), start(Clock::now()) {}

            ~ProfilingScope()
            {
                endFunctionCall(entry, start);
            }

            Entry* entry;
            Clock::time_point start;
        };

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

        /**
         * Enables the in-thread cache of profile data
         */
        void startThreadCache();
        void flushThreadCache();

    } // namespace profiler
} // namespace vc4c

#endif /* PROFILER_H */
