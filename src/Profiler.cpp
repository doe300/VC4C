/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Profiler.h"

#include "log.h"

#include <chrono>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <map>
#include <set>
#include <sys/resource.h>
#include <sys/time.h>
#include <unistd.h>
#include <unordered_map>

#ifdef MULTI_THREADED
#include <mutex>
#endif

// LCOV_EXCL_START

using namespace vc4c;

using Clock = std::chrono::system_clock;
using Duration = std::chrono::microseconds;

struct Entry
{
    std::string name;
    Duration duration;
    std::size_t invocations;
    std::string fileName;
    std::size_t lineNumber;

    bool operator<(const Entry& right) const
    {
        if(duration == right.duration)
            return name > right.name;
        return duration > right.duration;
    }
};

struct Counter
{
    std::string name;
    int64_t count;
    std::size_t index;
    std::size_t invocations;
    std::size_t prevCounter;
    std::string fileName;
    std::size_t lineNumber;

    bool operator<(const Counter& right) const
    {
        if(index == right.index)
            return name > right.name;
        return index < right.index;
    }
};

static std::unordered_map<std::string, Entry> times;
static std::map<std::size_t, Counter> counters;
#ifdef MULTI_THREADED
static std::mutex lockTimes;
static std::mutex lockCounters;
#endif

void profiler::endFunctionCall(ProfilingResult&& result)
{
#ifdef MULTI_THREADED
    std::lock_guard<std::mutex> guard(lockTimes);
#endif
    auto& entry = times[result.name];
    entry.name = std::move(result.name);
    entry.duration += std::chrono::duration_cast<Duration>(Clock::now() - result.startTime);
    entry.invocations += 1;
    entry.fileName = std::move(result.fileName);
    entry.lineNumber = result.lineNumber;
}

static void printResourceUsage(bool writeAsWarning)
{
    auto logFunc = writeAsWarning ? logging::warn : logging::info;
    static const auto pageSizeInKb = static_cast<uint64_t>(sysconf(_SC_PAGESIZE)) / 1024;
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

    CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
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

    {
        std::ifstream fis{"/proc/meminfo"};
        std::string line;
        if(std::getline(fis, line))
            // first line is: MemTotal
            logFunc() << line << logging::endl;
        if(std::getline(fis, line) && std::getline(fis, line))
            // third line is: MemAvailable (free + file cache + buffers) -> everything which could be requested
            logFunc() << line << logging::endl;
    }
}

void profiler::dumpProfileResults(bool writeAsWarning)
{
    logging::logLazy(writeAsWarning ? logging::Level::WARNING : logging::Level::DEBUG, [&]() {
#ifdef MULTI_THREADED
        std::lock_guard<std::mutex> guard(lockTimes);
#endif
        std::set<Entry> entries;
        std::set<Counter> counts;
        for(auto& entry : times)
        {
            entry.second.name = entry.first;
            entries.emplace(entry.second);
        }
        for(const auto& count : counters)
        {
            counts.emplace(count.second);
        }

        auto logFunc = writeAsWarning ? logging::warn : logging::info;

        logFunc() << std::setfill(L' ') << logging::endl;
        logFunc() << "Profiling results for " << entries.size() << " functions:" << logging::endl;
        for(const Entry& entry : entries)
        {
            logFunc() << std::setw(40) << entry.name << std::setw(7)
                      << std::chrono::duration_cast<std::chrono::milliseconds>(entry.duration).count() << " ms"
                      << std::setw(12) << entry.duration.count() << " us" << std::setw(10) << entry.invocations
                      << " calls" << std::setw(12)
                      << static_cast<std::size_t>(entry.duration.count()) / entry.invocations << " us/call"
                      << std::setw(64) << entry.fileName << "#" << entry.lineNumber << logging::endl;
        }

        logFunc() << logging::endl;
        logFunc() << "Profiling results for " << counts.size() << " counters:" << logging::endl;
        for(const Counter& counter : counts)
        {
            logFunc() << std::setw(40) << counter.name << std::setw(7) << counter.count << " counts" << std::setw(5)
                      << counter.invocations << " calls" << std::setw(6)
                      << static_cast<std::size_t>(counter.count) / counter.invocations << " avg./call" << std::setw(8)
                      << (counter.prevCounter == SIZE_MAX ? "" : "diff") << std::setw(7) << std::showpos
                      << (counter.prevCounter == SIZE_MAX ? 0 : counter.count - counters[counter.prevCounter].count)
                      << " (" << std::setw(5) << std::showpos
                      << (counter.prevCounter == SIZE_MAX ?
                                 0 :
                                 static_cast<int>(100 *
                                     (-1.0 +
                                         static_cast<double>(counter.count) /
                                             static_cast<double>(counters[counter.prevCounter].count))))
                      << std::noshowpos << "%)" << std::setw(64) << counter.fileName << "#" << counter.lineNumber
                      << logging::endl;
        }
    });
    times.clear();
    counters.clear();
    printResourceUsage(writeAsWarning);
}

void profiler::increaseCounter(const std::size_t index, std::string name, const std::size_t value, std::string file,
    const std::size_t line, const std::size_t prevIndex)
{
#ifdef MULTI_THREADED
    std::lock_guard<std::mutex> guard(lockCounters);
#endif
    auto& entry = counters[index];
    entry.index = index;
    entry.name = std::move(name);
    entry.count += value;
    entry.invocations += 1;
    entry.prevCounter = prevIndex;
    entry.fileName = std::move(file);
    entry.lineNumber = line;
}
// LCOV_EXCL_STOP
