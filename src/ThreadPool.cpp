/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ThreadPool.h"

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

std::future<void> ThreadPool::schedule(std::function<void()>&& func)
{
    std::packaged_task<void()> task{func};
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
    while(true)
    {
        std::packaged_task<void()> task;
        {
            std::unique_lock<std::mutex> lock(queueMutex);
            queueCondition.wait(lock, [&] { return !keepRunning || !taskQueue.empty(); });
            if(!keepRunning)
                return;
            task = std::move(taskQueue.front());
            taskQueue.pop();
        }

        // execute task outside of lock
        task();
    }
}
