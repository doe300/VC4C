/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ProcessUtil.h"

#include "CompilationError.h"
#include "Profiler.h"
#include "log.h"

#include <array>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <sstream>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <thread>
#include <unistd.h>
#include <vector>

using namespace vc4c;

static constexpr int STD_IN = 0;
static constexpr int STD_OUT = 1;
static constexpr int STD_ERR = 2;

static constexpr int READ = 0;
static constexpr int WRITE = 1;

static constexpr int BUFFER_SIZE = 1024;

static void initPipe(std::array<int, 2>& fds)
{
    if(pipe(fds.data()) != 0)
        throw CompilationError(CompilationStep::GENERAL, "Error creating pipe", strerror(errno));
}

static void closePipe(int fd)
{
    if(close(fd) != 0)
        throw CompilationError(CompilationStep::GENERAL, "Error closing pipe", strerror(errno));
}

static void mapPipe(int pipe, int fd)
{
    if(dup2(pipe, fd) == -1)
        throw CompilationError(CompilationStep::GENERAL, "Error duplicating pipe", strerror(errno));
}

static std::vector<std::string> splitString(const std::string& input, const char delimiter)
{
    std::vector<std::string> result;
    std::stringstream in(input);
    std::string token;
    while(std::getline(in, token, delimiter))
    {
        result.push_back(token);
    }
    return result;
}

static void runChild(
    const std::string& command, std::array<std::array<int, 2>, 3>& pipes, bool hasStdIn, bool hasStdOut, bool hasStdErr)
{
    // map pipes into stdin/stdout/stderr
    // close pipes not used by child
    if(hasStdIn)
    {
        mapPipe(pipes[STD_IN][READ], STDIN_FILENO);
        closePipe(pipes[STD_IN][READ]);
        closePipe(pipes[STD_IN][WRITE]);
    }
    if(hasStdOut)
    {
        mapPipe(pipes[STD_OUT][WRITE], STDOUT_FILENO);
        closePipe(pipes[STD_OUT][READ]);
        closePipe(pipes[STD_OUT][WRITE]);
    }
    if(hasStdErr)
    {
        mapPipe(pipes[STD_ERR][WRITE], STDERR_FILENO);
        closePipe(pipes[STD_ERR][READ]);
        closePipe(pipes[STD_ERR][WRITE]);
    }

    // split command
    std::vector<std::string> parts = splitString(command, ' ');
    const std::string file = parts.at(0);
    std::array<char*, 128> args{};
    args.fill(nullptr);
    // man(3) exec: "The first argument, by convention, should point to the filename associated with the file being
    // executed"
    for(std::size_t i = 0; i < parts.size(); ++i)
    {
        args.at(i) = const_cast<char*>(parts[i].data());
    }

    execvp(file.data(), args.data());
}

static bool isChildFinished(pid_t pid, int* exitStatus, bool wait = false)
{
    int status = 0;
    int result = waitpid(pid, &status, WUNTRACED | (wait ? 0 : WNOHANG));
    if(result == -1)
    {
        throw CompilationError(CompilationStep::GENERAL, "Error retrieving child process information", strerror(errno));
    }
    if(result == 0)
        // child still running, no changes to status, returned because of WNOHANG
        return false;
    // check whether child terminated "normally" having an exit-code or was terminated by a signal
    if(WIFEXITED(status))
    {
        *exitStatus = WEXITSTATUS(status);
        return true;
    }
    if(WIFSIGNALED(status))
    {
        *exitStatus = WTERMSIG(status);
        return true;
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled case in retrieving child process information", std::to_string(result));
}

static int runReadOnlySubprocess(const std::string& command, std::ostream* output)
{
    auto fd = popen(command.data(), "r");
    if(fd == nullptr)
        throw CompilationError(CompilationStep::GENERAL, "Failed to run command", strerror(errno));
    if(output != nullptr)
    {
        std::array<char, 512> buffer;
        std::size_t numRead = 0;
        while((numRead = fread(buffer.data(), sizeof(char), buffer.size(), fd)) > 0)
        {
            output->write(buffer.data(), numRead);
        }
        if(ferror(fd))
        {
            auto err = ferror(fd);
            pclose(fd);
            throw CompilationError(CompilationStep::GENERAL, "Error reading from sub-process", strerror(err));
        }
    }
    return pclose(fd);
}

static int runWriteOnlySubprocess(const std::string& command, std::istream* input)
{
    auto fd = popen(command.data(), "w");
    if(fd == nullptr)
        throw CompilationError(CompilationStep::GENERAL, "Failed to run command", strerror(errno));
    if(input != nullptr)
    {
        std::array<char, 512> buffer;
        while(input->read(buffer.data(), buffer.size()))
        {
            fwrite(buffer.data(), sizeof(char), input->gcount(), fd);
        }
        if(ferror(fd))
        {
            auto err = ferror(fd);
            pclose(fd);
            throw CompilationError(CompilationStep::GENERAL, "Error writing into sub-process", strerror(err));
        }
    }
    return pclose(fd);
}

int vc4c::runProcess(const std::string& command, std::istream* stdin, std::ostream* stdout, std::ostream* stderr)
{
    /*
     * Simple version, only ONE of stdin, stdout or stderr is set.
     * Now we can simplify by using popen
     */
    if(static_cast<unsigned>(stdin != nullptr) + static_cast<unsigned>(stdout != nullptr) +
            static_cast<unsigned>(stderr != nullptr) <=
        1)
    {
        if(stderr != nullptr)
        {
            // redirect stderr to stdout, so we can read it
            // see
            // https://stackoverflow.com/questions/876239/how-can-i-redirect-and-append-both-stdout-and-stderr-to-a-file-with-bash
            return runReadOnlySubprocess(command + " 2>&1", stderr);
        }
        else if(stdout != nullptr)
        {
            return runReadOnlySubprocess(command, stderr);
        }
        return runWriteOnlySubprocess(command, stdin);
    }

    /*
     * For the general case:
     *
     * NOTE: not setting a stream to put the stdout of the child-process in doesn't currently work (hangs the
     * child-process)  so we always set an output-stream, even if we write to file. But since the stream will have no
     * content (is not written to), it has no impact
     */

    /*
     * See:
     * https://jineshkj.wordpress.com/2006/12/22/how-to-capture-stdin-stdout-and-stderr-of-child-program/
     * https://stackoverflow.com/questions/6171552/popen-simultaneous-read-and-write
     * https://www.linuxquestions.org/questions/programming-9/popen-read-and-write-both-how-201083/
     * https://stackoverflow.com/questions/29554036/waiting-for-popen-subprocess-to-terminate-before-reading?rq=1
     */

    std::array<std::array<int, 2>, 3> pipes{};

    if(stdin != nullptr)
        initPipe(pipes[STD_IN]);
    if(true)
        initPipe(pipes[STD_OUT]);
    if(stderr != nullptr)
        initPipe(pipes[STD_ERR]);

    pid_t pid = fork();
    if(pid == 0) // child
    {
        runChild(command, pipes, stdin != nullptr, true, stderr != nullptr);
        /*
         * Nothing below this line should be executed by child process. If so, it means that the exec function wasn't
         * successful, so lets exit:
         */
        throw CompilationError(CompilationStep::GENERAL, "Error executing the child process", strerror(errno));
    }

    std::array<char, BUFFER_SIZE> buffer{};
    ssize_t numBytes;
    int exitStatus = 0;
    bool childFinished = false;

    PROFILE_START(WriteToChildProcess);
    // write to stdin
    if(stdin != nullptr)
    {
        std::istream& in = *stdin;
        while(!(childFinished = childFinished || isChildFinished(pid, &exitStatus)))
        {
            in.read(buffer.data(), buffer.size());
            numBytes = in.gcount();
            write(pipes[STD_IN][WRITE], buffer.data(), in.gcount());
            if(numBytes != buffer.size())
                break;
        }
        closePipe(pipes[STD_IN][READ]);
        closePipe(pipes[STD_IN][WRITE]);
    }
    PROFILE_END(WriteToChildProcess);

    bool stdOutFinished = true;
    bool stdErrFinished = stderr != nullptr;

    int highestFD = std::max(pipes[STD_OUT][READ], pipes[STD_ERR][READ]);
    fd_set readDescriptors{};
    timeval timeout{};

    /*
     * While the child program has not yet finished and the output-streams are not read to the end (EOF, thus the child
     * process has also finished), check which output stream has data and read it
     */
    PROFILE_START(ReadFromChildProcess);
    while(!(stdOutFinished && stdErrFinished) || !(childFinished = childFinished || isChildFinished(pid, &exitStatus)))
    {
        FD_ZERO(&readDescriptors);
        if(true)
            FD_SET(pipes[STD_OUT][READ], &readDescriptors);
        if(stderr != nullptr)
            FD_SET(pipes[STD_ERR][READ], &readDescriptors);

        /*
         * We need to re-initialize the timeout every iteration, because the select man-page states:
         * On Linux, select() modifies timeout to reflect the amount of time not slept; most other implementations do
         * not do this. (POSIX.1-2001 permits either behavior.) This causes problems both when Linux code which reads
         * timeout is ported to other operating systems, and when code is ported to Linux that reuses a struct timeval
         * for multiple select()s in a loop without reinitializing it. Consider timeout to be undefined after select()
         * returns.
         */
        timeout.tv_sec = 0;
        // wait 1ms
        timeout.tv_usec = 1000;

        /*
         * "Those listed in readfds will be watched to see if characters become available for reading
         * [...] On exit, the sets are modified in place to indicate which file descriptors actually changed status."
         *
         * "nfds is the highest-numbered file descriptor in any of the three sets, plus 1.
         *
         * "On success, select() and pselect() return the number of file descriptors contained in the three returned
         * descriptor sets
         * [...] which may be zero if the timeout expires before anything interesting happens"
         */
        int selectStatus = select(highestFD + 1, &readDescriptors, nullptr, nullptr, &timeout);
        if(selectStatus == -1)
            throw CompilationError(
                CompilationStep::GENERAL, "Error waiting on child's output streams", strerror(errno));
        else if(selectStatus != 0)
        {
            // something happened
            if(FD_ISSET(pipes[STD_OUT][READ], &readDescriptors))
            {
                numBytes = read(pipes[STD_OUT][READ], buffer.data(), buffer.size());
                if(stdout != nullptr)
                    stdout->write(buffer.data(), numBytes);
                if(numBytes == 0)
                    // EOF
                    stdOutFinished = true;
            }
            if(stderr != nullptr && FD_ISSET(pipes[STD_ERR][READ], &readDescriptors))
            {
                numBytes = read(pipes[STD_ERR][READ], buffer.data(), buffer.size());
                stderr->write(buffer.data(), numBytes);
                if(numBytes == 0)
                    // EOF
                    stdErrFinished = true;
            }
        }
    }
    PROFILE_END(ReadFromChildProcess);

    if(true)
    {
        closePipe(pipes[STD_OUT][READ]);
        closePipe(pipes[STD_OUT][WRITE]);
    }

    if(stderr != nullptr)
    {
        closePipe(pipes[STD_ERR][READ]);
        closePipe(pipes[STD_ERR][WRITE]);
    }

    if(!childFinished)
    {
        PROFILE(isChildFinished, pid, &exitStatus, true);
    }
    return exitStatus;
}
