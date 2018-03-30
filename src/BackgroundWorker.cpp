/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "BackgroundWorker.h"

#include "log.h"

using namespace vc4c;

BackgroundWorker::BackgroundWorker(const std::function<void()>& f, const std::string& name) :
    name(name), functor(f), err(nullptr)
#ifdef MULTI_THREADED
    ,
    runner()
#endif
{
#ifdef MULTI_THREADED
    // we need thread-support, so load the pthread library dynamically (if it is not yet loaded)
    void* handle = dlopen("libpthread.so.0", RTLD_GLOBAL | RTLD_LAZY);
    if(handle == nullptr)
    {
        throw std::runtime_error(std::string("Error loading pthread library: ") + dlerror());
    }
#endif
}

void BackgroundWorker::operator()()
{
    const auto f = [this]() -> void {
#ifdef MULTI_THREADED
        prctl(PR_SET_NAME, name.data(), 0, 0, 0);
#endif
        try
        {
            functor();
        }
        catch(const std::exception& e)
        {
            logging::error() << "Background worker threw error: " << e.what() << logging::endl;
            this->err = std::current_exception();
        }
    };
#ifdef MULTI_THREADED
    runner = std::thread(f);
#else
    f();
#endif
}

std::exception_ptr BackgroundWorker::waitFor()
{
#ifdef MULTI_THREADED
    if(runner.joinable())
        runner.join();
#endif
    return err;
}

void BackgroundWorker::waitForAll(std::vector<BackgroundWorker>& worker)
{
    std::exception_ptr err = nullptr;
    for(BackgroundWorker& w : worker)
    {
        std::exception_ptr tmp = w.waitFor();
        if(tmp)
            err = tmp;
    }
    // throwing the exception after all workers are finished solves the problem of terminating worker-threads
    //-> which crashes the program
    if(err)
        std::rethrow_exception(err);
}