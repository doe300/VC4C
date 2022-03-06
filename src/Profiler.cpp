/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Profiler.h"

#include "log.h"

#include <atomic>
#include <chrono>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <sys/resource.h>
#include <sys/time.h>
#include <unistd.h>
#include <unordered_map>

// LCOV_EXCL_START

using namespace vc4c;

using Clock = std::chrono::system_clock;
using Duration = std::chrono::microseconds;

template <typename Counter>
struct ExtremeValue
{
    std::atomic<Counter> value{0};
    std::string message = "";

    ExtremeValue() = default;
    ExtremeValue(Counter counter, const std::string& msg) : value(counter), message(msg) {}
    ExtremeValue(const ExtremeValue&) = delete;
    ExtremeValue(ExtremeValue&& other) noexcept : value(other.value.load()), message(std::move(other.message)) {}

    ~ExtremeValue() noexcept = default;

    ExtremeValue& operator=(const ExtremeValue&) = delete;
    ExtremeValue& operator=(ExtremeValue&& other) noexcept
    {
        if(&other != this)
        {
            value = other.value.load();
            message = std::move(other.message);
        }
        return *this;
    }

    operator bool() const noexcept
    {
        return value != 0;
    }

    bool operator<(const ExtremeValue& other) const noexcept
    {
        return value.load() < other.value.load();
    }
};

struct profiler::Entry
{
    std::string name;
    std::atomic<Duration ::rep> duration;
    std::atomic<uint64_t> invocations;
    std::string fileName;
    std::size_t lineNumber;
    ExtremeValue<Duration::rep> extremeValue;

    Entry() = default;
    Entry(const Entry&) = delete;
    Entry(Entry&& other) noexcept :
        name(std::move(other.name)), duration(other.duration.load()), invocations(other.invocations.load()),
        fileName(std::move(other.fileName)), lineNumber(other.lineNumber), extremeValue(std::move(other.extremeValue))
    {
    }
    ~Entry() noexcept = default;

    Entry& operator=(const Entry&) = delete;
    Entry& operator=(Entry&& other) noexcept
    {
        if(&other != this)
        {
            name = std::move(other.name);
            duration = other.duration.load();
            invocations = other.invocations.load();
            fileName = std::move(other.fileName);
            lineNumber = other.lineNumber;
            extremeValue = std::move(other.extremeValue);
        }
        return *this;
    }

    bool operator<(const Entry& right) const noexcept
    {
        if(duration.load() == right.duration.load())
            return name > right.name;
        return duration.load() > right.duration.load();
    }
};

struct profiler::Counter
{
    std::size_t key;
    std::string name;
    std::atomic<uint64_t> count;
    std::size_t index;
    std::atomic<uint64_t> invocations;
    std::size_t prevCounter;
    std::string fileName;
    std::size_t lineNumber;

    Counter() = default;
    Counter(const Counter&) = delete;
    Counter(Counter&& other) noexcept :
        key(other.key), name(std::move(other.name)), count(other.count.load()), index(other.index),
        invocations(other.invocations.load()), prevCounter(other.prevCounter), fileName(std::move(other.fileName)),
        lineNumber(other.lineNumber)
    {
    }
    ~Counter() noexcept = default;

    Counter& operator=(const Counter&) = delete;
    Counter& operator=(Counter&& other) noexcept
    {
        if(&other != this)
        {
            key = other.key;
            name = std::move(other.name);
            count = other.count.load();
            index = other.index;
            invocations = other.invocations.load();
            prevCounter = other.prevCounter;
            fileName = std::move(other.fileName);
            lineNumber = other.lineNumber;
        }
        return *this;
    }

    bool operator<(const Counter& right) const noexcept
    {
        if(index == right.index)
            return name > right.name;
        return index < right.index;
    }
};

static std::unordered_map<profiler::HashKey, profiler::Entry> times;
static std::map<std::size_t, profiler::Counter> counters;
static std::mutex lockTimes;
static std::mutex lockCounters;

/**
 * Utility class to cache the profile result thread-locally to be written back at once at thread exit.
 *
 * This reduces the amount of profile result mutex locks required.
 */
struct ThreadResultCache
{
    ~ThreadResultCache()
    {
        // flush all thread-local data to the global data
        if(!localTimes.empty())
        {
            std::lock_guard<std::mutex> guard(lockTimes);
            for(auto& entry : localTimes)
            {
                auto it = times.find(entry.first);
                if(it != times.end())
                {
                    if(it->second.extremeValue < entry.second.extremeValue)
                        it->second.extremeValue = std::move(entry.second.extremeValue);
                    it->second.duration += entry.second.duration;
                    it->second.invocations += entry.second.invocations;
                }
                else
                    times.emplace(entry.first, std::move(entry.second));
            }
        }

        if(!localCounters.empty())
        {
            std::lock_guard<std::mutex> guard(lockCounters);
            for(auto& entry : localCounters)
            {
                auto it = counters.find(entry.first);
                if(it != counters.end())
                {
                    it->second.count += entry.second.count;
                    it->second.invocations += entry.second.invocations;
                    it->second.index = std::min(it->second.index, entry.second.index);
                }
                else
                    counters.emplace(entry.first, std::move(entry.second));
            }
        }
    }

    std::unordered_map<profiler::HashKey, profiler::Entry> localTimes;
    std::map<std::size_t, profiler::Counter> localCounters;
};

static thread_local std::unique_ptr<ThreadResultCache> threadCache;

profiler::Entry* profiler::createEntry(HashKey key, std::string name, std::string fileName, std::size_t lineNumber)
{
    if(threadCache)
    {
        auto& entry = threadCache->localTimes[key];
        entry.name = std::move(name);
        entry.fileName = std::move(fileName);
        entry.lineNumber = lineNumber;
        return &entry;
    }
    std::lock_guard<std::mutex> guard(lockTimes);
    auto& entry = times[key];
    entry.name = std::move(name);
    entry.fileName = std::move(fileName);
    entry.lineNumber = lineNumber;
    return &entry;
}

void profiler::endFunctionCall(Entry* entry, Clock::time_point startTime)
{
    entry->duration += std::chrono::duration_cast<Duration>(Clock::now() - startTime).count();
    ++entry->invocations;
}

void profiler::endFunctionCall(Entry* entry, Clock::time_point startTime, const std::string& message)
{
    endFunctionCall(entry, startTime);
    if(message.empty())
        return;

    auto duration = std::chrono::duration_cast<Duration>(Clock::now() - startTime).count();
    if(entry->extremeValue.value < duration)
    {
        ExtremeValue<Duration::rep> extrema{duration, message};
        if(entry->extremeValue < extrema)
            entry->extremeValue = std::move(extrema);
    }
}

static void printResourceUsage(bool writeAsWarning)
{
    auto logFunc = writeAsWarning ? logging::warn : logging::info;
    static const auto pageSizeInKb = static_cast<uint64_t>(sysconf(_SC_PAGESIZE)) / 1024;

    CPPLOG_LAZY_BLOCK(writeAsWarning ? logging::Level::WARNING : logging::Level::DEBUG, {
        rusage usage{};
        if(getrusage(RUSAGE_SELF, &usage) < 0)
        {
            logging::warn() << "Error gathering resource usage: " << strerror(errno) << logging::endl;
            return;
        }
        uint64_t virtualSize = 0;
        uint64_t residentSize = 0;
        uint64_t sharedSize = 0;
        {
            // htop uses this one, see https://github.com/hishamhm/htop/blob/master/linux/LinuxProcessList.c#L480
            std::ifstream fis{"/proc/self/statm"};
            fis >> virtualSize;
            fis >> residentSize;
            fis >> sharedSize;
            if(!fis)
            {
                logging::warn() << "Error reading memory usage: " << strerror(errno) << logging::endl;
                return;
            }
            // next ones are text segment, library (loaded *.so files) and data segment (incl. stack)
        }
        logFunc() << "Resource usage: " << logging::endl;
        logFunc() << "\tCPU time (user):   " << std::setfill(L' ') << std::setw(3) << usage.ru_utime.tv_sec << '.'
                  << std::setfill(L'0') << std::setw(6) << usage.ru_utime.tv_usec << " s" << logging::endl;
        logFunc() << "\tCPU time (kernel): " << std::setfill(L' ') << std::setw(3) << usage.ru_stime.tv_sec << '.'
                  << std::setfill(L'0') << std::setw(6) << usage.ru_stime.tv_usec << " s" << logging::endl;
        logFunc() << "\tVirtual RAM usage: " << std::setfill(L' ') << std::setw(6) << (virtualSize * pageSizeInKb)
                  << " kB" << logging::endl;
        logFunc() << "\tCurrent RAM usage: " << std::setfill(L' ') << std::setw(6) << (residentSize * pageSizeInKb)
                  << " kB" << logging::endl;
        logFunc() << "\tShared RAM usage:  " << std::setfill(L' ') << std::setw(6) << (sharedSize * pageSizeInKb)
                  << " kB" << logging::endl;
        logFunc() << "\tPeek RAM usage:    " << std::setfill(L' ') << std::setw(6) << usage.ru_maxrss << " kB"
                  << logging::endl;
        logFunc() << "\tPage faults (minor/major): " << usage.ru_minflt << '/' << usage.ru_majflt << logging::endl;
    });

    CPPLOG_LAZY_BLOCK(writeAsWarning ? logging::Level::WARNING : logging::Level::DEBUG, {
        std::ifstream fis{"/proc/meminfo"};
        std::string line;
        if(std::getline(fis, line))
            // first line is: MemTotal
            logFunc() << line << logging::endl;
        if(std::getline(fis, line) && std::getline(fis, line))
            // third line is: MemAvailable (free + file cache + buffers) -> everything which could be requested
            logFunc() << line << logging::endl;
    });
}

template <typename T>
struct SortPointerTarget
{
    bool operator()(const T* one, const T* other) const noexcept
    {
        return *one < *other;
    }
};

void profiler::dumpProfileResults(bool writeAsWarning)
{
    std::unique_lock<std::mutex> timesGuard(lockTimes, std::defer_lock);
    std::unique_lock<std::mutex> countersGuard(lockCounters, std::defer_lock);
    std::lock(timesGuard, countersGuard);
    logging::logLazy(writeAsWarning ? logging::Level::WARNING : logging::Level::DEBUG, [&]() {
        std::set<const Entry*, SortPointerTarget<const Entry>> entries;
        std::set<const Counter*, SortPointerTarget<const Counter>> counts;
        for(auto& entry : times)
        {
            // an invocation count of 0 can happen if we cleared the entries on a previous dump and the entry was called
            // afterwards
            if(entry.second.invocations != 0)
                entries.emplace(&entry.second);
        }
        for(const auto& count : counters)
        {
            // an invocation count of 0 can happen if we cleared the counters on a previous dump and the counter was
            // called afterwards
            if(count.second.invocations != 0)
                counts.emplace(&count.second);
        }

        auto logFunc = writeAsWarning ? logging::warn : logging::info;

        logFunc() << std::setfill(L' ') << logging::endl;
        logFunc() << "Profiling results for " << entries.size() << " functions:" << logging::endl;
        for(const Entry* entry : entries)
        {
            logFunc() << std::setw(40) << entry->name << std::setw(7)
                      << std::chrono::duration_cast<std::chrono::milliseconds>(Duration{entry->duration}).count()
                      << " ms" << std::setw(12) << entry->duration << " us" << std::setw(10) << entry->invocations
                      << " calls" << std::setw(12)
                      << static_cast<uint64_t>(entry->duration) / std::max(entry->invocations.load(), uint64_t{1})
                      << " us/call" << std::setw(64) << entry->fileName << "#" << entry->lineNumber;
            if(entry->extremeValue && entry->extremeValue.value.load() != entry->duration)
                logFunc() << std::setw(16) << "(maximum with " << std::setw(4)
                          << std::chrono::duration_cast<std::chrono::milliseconds>(Duration{entry->extremeValue.value})
                                 .count()
                          << " ms for '" << entry->extremeValue.message << "')";
            logFunc() << logging::endl;
        }

        logFunc() << logging::endl;
        logFunc() << "Profiling results for " << counts.size() << " counters:" << logging::endl;
        for(const Counter* counter : counts)
        {
            auto prevCount =
                counter->prevCounter == SIZE_MAX ? 0 : static_cast<int64_t>(counters[counter->prevCounter].count);
            logFunc() << std::setw(40) << counter->name << std::setw(9) << counter->count << " counts" << std::setw(7)
                      << counter->invocations << " calls" << std::setw(6)
                      << counter->count / std::max(counter->invocations.load(), uint64_t{1}) << " avg./call"
                      << std::setw(8) << (counter->prevCounter == SIZE_MAX ? "" : "diff") << std::setw(7)
                      << std::showpos
                      << (counter->prevCounter == SIZE_MAX ? 0 : static_cast<int64_t>(counter->count) - prevCount)
                      << " (" << std::setw(5) << std::showpos
                      << (counter->prevCounter == SIZE_MAX ?
                                 0 :
                                 static_cast<int>(100 *
                                     (-1.0 + static_cast<double>(counter->count) / static_cast<double>(prevCount))))
                      << std::noshowpos << "%)" << std::setw(64) << counter->fileName << "#" << counter->lineNumber
                      << logging::endl;
        }
    });
    // NOTE: we can't clear() the entries, since they are statically referenced by the profiling calls, so we just clear
    // the updated values
    for(auto& entry : times)
    {
        entry.second.duration = {};
        entry.second.invocations = 0;
        entry.second.extremeValue = {};
    }
    for(auto& entry : counters)
    {
        entry.second.count = 0;
        entry.second.invocations = 0;
    }
    printResourceUsage(writeAsWarning);
}

static profiler::Counter& allocateCounter(
    profiler::HashKey key, std ::map<std::size_t, profiler::Counter>& countersMap, std::size_t baseIndex)
{
    static std::atomic_size_t currentIndex{0};
    auto it = countersMap.find(key);
    if(it == countersMap.end())
    {
        auto nextIndex = currentIndex++;
        it = countersMap.emplace(key, profiler::Counter{}).first;
        it->second.index = baseIndex + nextIndex;
    }
    return it->second;
}

profiler::Counter* profiler::createCounter(HashKey key, std::size_t baseIndex, std::string name, std::string file,
    std::size_t line, const Counter* prevCounter)
{
    if(threadCache)
    {
        auto& entry = allocateCounter(key, threadCache->localCounters, baseIndex);
        entry.key = key;
        entry.name = std::move(name);
        entry.prevCounter = prevCounter ? prevCounter->key : SIZE_MAX;
        entry.fileName = std::move(file);
        entry.lineNumber = line;
        return &entry;
    }
    std::lock_guard<std::mutex> guard(lockCounters);
    auto& entry = allocateCounter(key, counters, baseIndex);
    entry.key = key;
    entry.name = std::move(name);
    entry.prevCounter = prevCounter ? prevCounter->key : SIZE_MAX;
    entry.fileName = std::move(file);
    entry.lineNumber = line;
    return &entry;
}

void profiler::increaseCounter(Counter* counter, std::size_t value)
{
    counter->count += value;
    ++counter->invocations;
}

uint64_t profiler::getCounterValue(const std::string& name)
{
    std::lock_guard<std::mutex> guard(lockCounters);
    for(const auto& counter : counters)
    {
        if(counter.second.name == name)
            return counter.second.count;
    }
    return 0;
}

void profiler::startThreadCache()
{
    threadCache = std::make_unique<ThreadResultCache>();
}

void profiler::flushThreadCache()
{
    threadCache.reset();
}

// LCOV_EXCL_STOP
