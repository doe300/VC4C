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
#ifndef NDEBUG
#define PROFILE(func, ...)                                                                                             \
    [&]() {                                                                                                            \
        static_assert(__builtin_constant_p(#func), "");                                                                \
        static thread_local auto profileEntry##func = profiler::createEntry(                                           \
            reinterpret_cast<std::uintptr_t>(std::addressof(#func[0])), #func, __FILE__, __LINE__);                    \
        profiler::ProfilingScope profile##name{profileEntry##func};                                                    \
        return func(__VA_ARGS__);                                                                                      \
    }();

#define PROFILE_START(name)                                                                                            \
    static_assert(__builtin_constant_p(#name), "");                                                                    \
    static thread_local auto profileEntry##name =                                                                      \
        profiler::createEntry(reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), #name, __FILE__, __LINE__);  \
    auto profileStart##name = profiler::Clock::now()

#define PROFILE_END(name) profiler::endFunctionCall(profileEntry##name, profileStart##name)

#define PROFILE_END_EXTREMA(name, msg) profiler::endFunctionCall(profileEntry##name, profileStart##name, msg)

#define PROFILE_START_DYNAMIC(name)                                                                                    \
    auto profileEntryDynamic = profiler::createEntry(std::hash<std::string>{}(name), name, __FILE__, __LINE__);        \
    auto profileStartDynamic = profiler::Clock::now()

#define PROFILE_END_DYNAMIC(name) profiler::endFunctionCall(profileEntryDynamic, profileStartDynamic)

#define PROFILE_END_DYNAMIC_EXTREMA(name, msg) profiler::endFunctionCall(profileEntryDynamic, profileStartDynamic, msg)

#define PROFILE_SCOPE(name)                                                                                            \
    static_assert(__builtin_constant_p(#name), "");                                                                    \
    static thread_local auto profileEntry##name =                                                                      \
        profiler::createEntry(reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), #name, __FILE__, __LINE__);  \
    profiler::ProfilingScope profile##name(profileEntry##name)

#define PROFILE_SCOPE_EXTREMA(name, msg)                                                                               \
    static_assert(__builtin_constant_p(#name), "");                                                                    \
    static thread_local auto profileEntry##name =                                                                      \
        profiler::createEntry(reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), #name, __FILE__, __LINE__);  \
    profiler::ProfilingScope profile##name(profileEntry##name, msg)

#define PROFILE_COUNTER(base, name, value)                                                                             \
    static_assert(__builtin_constant_p(name), "");                                                                     \
    static thread_local auto profileCounter = profiler::createCounter(                                                 \
        reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), base, name, __FILE__, __LINE__);                   \
    profiler::increaseCounter(profileCounter, value);

#define PROFILE_COUNTER_SCOPE(base, name, value)                                                                       \
    do                                                                                                                 \
    {                                                                                                                  \
        static_assert(__builtin_constant_p(name), "");                                                                 \
        static thread_local auto profileCounter = profiler::createCounter(                                             \
            reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), base, name, __FILE__, __LINE__);               \
        profiler::increaseCounter(profileCounter, value);                                                              \
    } while(false)

#define PROFILE_COUNTER_DYNAMIC(base, name, value)                                                                     \
    auto profileCounterDynamic =                                                                                       \
        profiler::createCounter(std::hash<std::string>{}(name), base, name, __FILE__, __LINE__);                       \
    profiler::increaseCounter(profileCounterDynamic, value);

#define PROFILE_COUNTER_WITH_PREV(base, name, value)                                                                   \
    static_assert(__builtin_constant_p(name), "");                                                                     \
    static thread_local auto profileCounter2 = profiler::createCounter(                                                \
        reinterpret_cast<std::uintptr_t>(std::addressof(#name[0])), base, name, __FILE__, __LINE__, profileCounter);   \
    profiler::increaseCounter(profileCounter2, value);

#define PROFILE_COUNTER_DYNAMIC_WITH_PREV(base, name, value)                                                           \
    auto profileCounterDynamic2 = profiler::createCounter(                                                             \
        std::hash<std::string>{}(name), base, name, __FILE__, __LINE__, profileCounterDynamic);                        \
    profiler::increaseCounter(profileCounterDynamic2, value);

#define PROFILE_RESULTS() profiler::dumpProfileResults()

#define PROFILE_CREATE_THREAD_CACHE() profiler::startThreadCache()
#define PROFILE_FLUSH_THREAD_CACHE() profiler::flushThreadCache()
#else
#define PROFILE(func, ...) func(__VA_ARGS__)

#define PROFILE_START(name)
#define PROFILE_END(name)
#define PROFILE_END_EXTREMA(name, msg)

#define PROFILE_START_DYNAMIC(name)
#define PROFILE_END_DYNAMIC(name)
#define PROFILE_END_DYNAMIC_EXTREMA(name, msg)

#define PROFILE_SCOPE(name)
#define PROFILE_SCOPE_EXTREMA(name, msg)

#define PROFILE_COUNTER(base, name, value)
#define PROFILE_COUNTER_SCOPE(base, name, value)
#define PROFILE_COUNTER_DYNAMIC(base, name, value)
#define PROFILE_COUNTER_WITH_PREV(base, name, value)
#define PROFILE_COUNTER_DYNAMIC_WITH_PREV(base, name, value)

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
        void endFunctionCall(Entry* entry, Clock::time_point startTime, const std::string& message);

        void dumpProfileResults(bool writeAsWarning = false);

        struct Counter;
        Counter* createCounter(HashKey key, std::size_t baseIndex, std::string name, std::string file, std::size_t line,
            const Counter* prevCounter = nullptr);
        void increaseCounter(Counter* counter, std::size_t value);

        struct ProfilingScope
        {
            explicit ProfilingScope(Entry* entry) : entry(entry), start(Clock::now()) {}
            ProfilingScope(Entry* entry, const std::string& msg) : entry(entry), start(Clock::now()), message(msg) {}

            ~ProfilingScope()
            {
                endFunctionCall(entry, start, message);
            }

            Entry* entry;
            Clock::time_point start;
            std::string message;
        };

        /*
         * The following values are added to the sub counter index to get the absolute counter index.
         *
         * E.g. the second counter in the front-end is COUNTER_FRONTEND + 2
         */
        static constexpr std::size_t COUNTER_GENERAL = 0;
        static constexpr std::size_t COUNTER_FRONTEND = 10000000;
        static constexpr std::size_t COUNTER_NORMALIZATION = 20000000;
        static constexpr std::size_t COUNTER_OPTIMIZATION = 30000000;
        static constexpr std::size_t COUNTER_BACKEND = 40000000;
        static constexpr std::size_t COUNTER_EMULATOR = 100000000;

        /**
         * Enables the in-thread cache of profile data
         */
        void startThreadCache();
        void flushThreadCache();

    } // namespace profiler
} // namespace vc4c

#endif /* PROFILER_H */
