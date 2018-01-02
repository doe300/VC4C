/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef BACKGROUND_WORKER_H
#define BACKGROUND_WORKER_H

#include "log.h"

#include <exception>
#include <functional>
#include <vector>

#ifdef MULTI_THREADED
#include <dlfcn.h>
#include <mutex>
#include <sys/prctl.h>
#include <thread>
#endif

namespace threading
{
	/*
	 * Container for code-executions which can be run in parallel
	 *
	 * Depending on the value of MULTI_THREADED, a background-worker executes the function in a background-thread or the current one.
	 *
	 * BackgroundWorker also manage the throwing of exceptions from within the background thread:
	 * If the function throws an exception (and MULTI_THREADED is set), it is caught (to not terminate the program) and then re-thrown
	 * in the operator(), allowing it to be handled by the calling thread
	 */
	struct BackgroundWorker
	{
	public:

		BackgroundWorker(const std::function<void()>& f, const std::string& name) : name(name), functor(f), err(nullptr)
#ifdef MULTI_THREADED
	, runner()
#endif
		{
#ifdef MULTI_THREADED
			//we need thread-support, so load the pthread library dynamically (if it is not yet loaded)
			void* handle = dlopen("libpthread.so.0", RTLD_GLOBAL | RTLD_LAZY);
			if(handle == nullptr)
			{
				throw std::runtime_error(std::string("Error loading pthread library: ") + dlerror());
			}
#endif
		}

		void operator()()
		{
			const auto f = [this]() -> void
			{
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

		std::exception_ptr waitFor() const
		{
#ifdef MULTI_THREADED
			if(runner.joinable())
				const_cast<std::thread&>(runner).join();
#endif
			return err;
		}
		std::string name;
		std::function<void()> functor;
		std::exception_ptr err;
#ifdef MULTI_THREADED
		std::thread runner;
#endif

		static void waitForAll(const std::vector<BackgroundWorker>& worker)
		{
			std::exception_ptr err = nullptr;
			for(const BackgroundWorker& w : worker)
			{
				std::exception_ptr tmp = w.waitFor();
				if(tmp)
					err = tmp;
			}
			//throwing the exception after all workers are finished solves the problem of terminating worker-threads
			//-> which crashes the program
			if(err)
				std::rethrow_exception(err);
		}
	};

} /* namespace threading */

#endif /* BACKGROUND_WORKER_H */
