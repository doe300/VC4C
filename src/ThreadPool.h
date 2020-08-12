/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_THREDAPOOL_H
#define VC4C_THREDAPOOL_H

#include <atomic>
#include <future>
#include <list>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

namespace logging
{
    class Logger;
} // namespace logging

namespace vc4c
{
    class ThreadPool
    {
    public:
        explicit ThreadPool(const std::string& poolName, unsigned numThreads = std::thread::hardware_concurrency());
        ThreadPool(const ThreadPool&) = delete;
        ThreadPool(ThreadPool&&) noexcept = delete;
        ~ThreadPool();

        ThreadPool& operator=(const ThreadPool&) = delete;
        ThreadPool& operator=(ThreadPool&&) noexcept = delete;

        std::future<void> schedule(std::function<void()>&& func, logging::Logger* logger = nullptr);

        template <typename T, typename Container = std::list<T>>
        void scheduleAll(
            const Container& c, const std::function<void(const T&)>& func, logging::Logger* logger = nullptr)
        {
            std::vector<std::future<void>> futures;
            futures.reserve(c.size());

            for(auto& elem : c)
                futures.emplace_back(schedule([&]() { func(elem); }, logger));

            for(auto& fut : futures)
                fut.get();
        }

    private:
        std::vector<std::thread> workers;
        std::atomic_bool keepRunning;
        std::mutex queueMutex;
        std::queue<std::packaged_task<void()>> taskQueue;
        std::condition_variable queueCondition;

        void workerTask(const std::string& poolName);
    };

} /* namespace vc4c */

#endif /* VC4C_THREDAPOOL_H */
