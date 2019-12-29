/*
 * Contains functions to add signal handlers (if there are not already such handlers registered) to provide better error
 * messages on crashes.
 */
#include "CompilationError.h"
#include "log.h"

#include <cstdio>
#include <initializer_list>
#include <signal.h>

static const char* toName(int signal)
{
    switch(signal)
    {
    case SIGSEGV:
        return "SIGSEGV";
    case SIGILL:
        return "SIGILL";
    case SIGABRT:
        return "SIGABRT";
    case SIGFPE:
        return "SIGFPE";
    }
    return "(unknown)";
}

static void handleSignal(int signal)
{
    logging::error() << "Received signal: " << toName(signal) << logging::endl;
    vc4c::CompilationError::logBacktrace();

    // registering a signal handler overrides the default behavior (of crahsing the program)
    // to still abort correctly, we need to manually kill it
    exit(signal);
}

static auto action = []() {
    struct sigaction tmp;
    sigemptyset(&tmp.sa_mask);
    tmp.sa_flags = SA_RESTART | SA_SIGINFO | SA_ONSTACK;
    tmp.sa_handler = handleSignal;

    return tmp;
}();

static auto handlerDummy = []() {
    for(auto signal : {SIGSEGV, SIGILL, SIGABRT, SIGFPE})
    {
        struct sigaction oldAction;
        if(sigaction(signal, nullptr, &oldAction) == 0)
        {
            if(oldAction.sa_handler != nullptr || oldAction.sa_sigaction != nullptr)
                // already a listener registered, skip registering ours
                continue;
        }
        sigaction(signal, &action, nullptr);
    }
    return true;
}();
