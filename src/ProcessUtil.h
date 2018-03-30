/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PROCESSUTIL_H
#define PROCESSUTIL_H

#include <iostream>
#include <string>

namespace vc4c
{
    /*
     * Runs the command in a new child-process, passes the standard input/output/error streams, waits for the process to
     * finish and returns it status
     */
    int runProcess(const std::string& command, std::istream* stdin = nullptr, std::ostream* stdout = nullptr,
        std::ostream* stderr = nullptr);

} /* namespace vc4c */

#endif /* PROCESSUTIL_H */
