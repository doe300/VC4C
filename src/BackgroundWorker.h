/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef BACKGROUND_WORKER_H
#define BACKGROUND_WORKER_H

#include <exception>
#include <functional>
#include <list>
#include <vector>

#ifdef MULTI_THREADED
#include <dlfcn.h>
#include <mutex>
#include <sys/prctl.h>
#include <thread>
#endif

namespace vc4c
{
    /*
     * Container for code-executions which can be run in parallel
     *
     * Depending on the value of MULTI_THREADED, a background-worker executes the function in a background-thread or the
     * current one.
     *
     * BackgroundWorker also manage the throwing of exceptions from within the background thread:
     * If the function throws an exception (and MULTI_THREADED is set), it is caught (to not terminate the program) and
     * then re-thrown in the operator(), allowing it to be handled by the calling thread
     */
    struct BackgroundWorker
    {
    public:
        BackgroundWorker(const std::function<void()>& f, const std::string& name);

        void operator()();

        std::exception_ptr waitFor();

        std::string name;
        std::function<void()> functor;
        std::exception_ptr err;
#ifdef MULTI_THREADED
        std::thread runner;
#endif

        static void waitForAll(std::vector<BackgroundWorker>& worker);

        template <typename T, typename Container = std::list<T>>
        static void scheduleAll(const Container& c, const std::function<void(const T&)>& func, const std::string name)
        {
            auto it = c.begin();
#ifdef MULTI_THREADED
            // the maximum number of parallel running workers
            static constexpr unsigned maxRunning = 16;
            std::mutex containerGuard;
            const auto scheduler = [&]() {
                while(true)
                {
                    const T* item = nullptr;
                    {
                        std::lock_guard<std::mutex> guard(containerGuard);
                        if(it == c.end())
                            return;
                        item = &(*it);
                        ++it;
                    }
                    func(*item);
                }
            };
#else
            static constexpr unsigned maxRunning = 1;
            const auto scheduler = [&]() {
                while(true)
                {
                    if(it == c.end())
                        return;
                    const auto& item = *it;
                    ++it;
                    func(item);
                }
            };
#endif
            std::vector<BackgroundWorker> workers;
            workers.reserve(maxRunning);
            for(unsigned i = 0; i < std::min(maxRunning, static_cast<unsigned>(c.size())); ++i)
            {
                workers.emplace_back(scheduler, name);
                workers.back().operator()();
            }

            waitForAll(workers);
        }
    };

} /* namespace vc4c */

#endif /* BACKGROUND_WORKER_H */
