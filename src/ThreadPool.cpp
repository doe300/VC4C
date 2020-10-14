/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ThreadPool.h"

#include "Profiler.h"
#include "log.h"

#include <sys/prctl.h>

using namespace vc4c;

ThreadPool::ThreadPool(const std::string& poolName, unsigned numThreads) : keepRunning(true)
{
#ifdef MULTI_THREADED
    workers.reserve(numThreads);
    for(unsigned i = 0; i < numThreads; ++i)
        workers.emplace_back([this, poolName]() { workerTask(poolName); });
#endif
}

ThreadPool::~ThreadPool()
{
#ifdef MULTI_THREADED
    keepRunning = false;

    // wait for all threads to end to not cause std::terminate to be issued
    queueCondition.notify_all();
    for(auto& worker : workers)
        worker.join();
#endif
}

std::future<void> ThreadPool::schedule(std::function<void()>&& func, logging::Logger* logger)
{
    std::packaged_task<void()> task{[func{std::move(func)}, logger]() {
        // Since this is unconditionally called for every task, the logger is only used for the tasks where is
        // explicitly set. In other words, the next task overwrites the logger to be used (possibly with a NULL pointer,
        // to use the global logger).
        logging::setThreadLogger(logger);
        func();
    }};
    auto fut = task.get_future();
#ifdef MULTI_THREADED
    {
        std::lock_guard<std::mutex> guard(queueMutex);
        taskQueue.emplace(std::move(task));
    }
    queueCondition.notify_one();
#else
    task();
#endif
    return fut;
}

void ThreadPool::workerTask(const std::string& poolName)
{
    prctl(PR_SET_NAME, poolName.data(), 0, 0, 0);
    PROFILE_CREATE_THREAD_CACHE();
    while(keepRunning)
    {
        std::packaged_task<void()> task;
        {
            std::unique_lock<std::mutex> lock(queueMutex);
            /*
             * There is a bug in TSAN which throws "double lock of a mutex" for the queueMutex.
             *
             * The actual desired queueCondition.wait_for(...) triggers the bug, while explicitly using
             * queueCondition.wait_until(...) with a std::chrono::system_clock::time_points avoids it.
             * Since this is also the identical (previous) behavior, we explicitly use this for now until
             * TSAN is fixed.
             *
             * TSAN bug: https://github.com/google/sanitizers/issues/1259
             */
            queueCondition.wait_until(lock, std::chrono::system_clock::now() + std::chrono::milliseconds{100},
                [&] { return !keepRunning || !taskQueue.empty(); });
            if(!keepRunning)
                break;
            if(taskQueue.empty())
                continue;
            task = std::move(taskQueue.front());
            taskQueue.pop();
        }

        // execute task outside of lock
        task();
    }
    PROFILE_FLUSH_THREAD_CACHE();
}
