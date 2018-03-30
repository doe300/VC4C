/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef BACKGROUND_WORKER_H
#define BACKGROUND_WORKER_H

#include <exception>
#include <functional>
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
    };

} /* namespace vc4c */

#endif /* BACKGROUND_WORKER_H */
