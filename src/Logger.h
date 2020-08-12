/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LOGGER_H
#define VC4C_LOGGER_H

#include "logger.h"

#include <memory>

namespace vc4c
{
    /**
     * Storage of the thread-local logger instance.
     *
     * This is mainly used to be set by the global vc4c::setLogger(...) function for the entry thread (e.g. the thread
     * starting the compilation/emulation process). This logger instance is then used in that thread (until overwritten
     * by the next entry point) and also passed to all worker tasks in the thread pool (if active) started by this entry
     * point.
     *
     * This allows us to run different entry points in parallel and log to different outputs while still retaining the
     * multi-threaded and thread-pool nature of the normalization, optimization and code generation steps.
     */
    extern thread_local std::unique_ptr<logging::Logger> THREAD_LOGGER;

} // namespace vc4c

#endif /* VC4C_LOGGER_H */