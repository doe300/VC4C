/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <vector>
#include <sstream>
#include <sys/wait.h>
#include <chrono>
#include <thread>
#include <sys/select.h>
#include <sys/time.h>

#include "log.h"
#include "ProcessUtil.h"
#include "CompilationError.h"
#include "Profiler.h"

using namespace vc4c;

static int STD_IN = 0;
static int STD_OUT = 1;
static int STD_ERR = 2;

static int READ = 0;
static int WRITE = 1;

static int BUFFER_SIZE = 1024;

static void initPipe(int fds[2])
{
	if(pipe(fds) != 0)
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
    while (std::getline(in, token, delimiter)) {
        result.push_back(token);
    }
    return result;
}

static void runChild(const std::string& command, int pipes[3][2], bool hasStdIn, bool hasStdOut, bool hasStdErr)
{
	//map pipes into stdin/stdout/stderr
	//close pipes not used by child
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

	//split command
	std::vector<std::string> parts = splitString(command, ' ');
	const std::string file = parts.at(0);
	char* args[128];
	//man(3) exec: "The first argument, by convention, should point to the filename associated with the file being executed"
	for(std::size_t i = 0; i < parts.size(); ++i)
	{
		args[i] = const_cast<char*>(parts.at(i).data());
	}
	args[parts.size()] = nullptr;

	execvp(file.data(), args);
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
		//child still running, no changes to status, returned because of WNOHANG
		return false;
	//check whether child terminated "normally" having an exit-code or was terminated by a signal
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
	throw CompilationError(CompilationStep::GENERAL, "Unhandled case in retrieving child process information", std::to_string(result));
}

int vc4c::runProcess(const std::string& command, std::istream* stdin, std::ostream* stdout, std::ostream* stderr)
{
	/*
	 * See:
	 * https://jineshkj.wordpress.com/2006/12/22/how-to-capture-stdin-stdout-and-stderr-of-child-program/
	 * https://stackoverflow.com/questions/6171552/popen-simultaneous-read-and-write
	 * https://www.linuxquestions.org/questions/programming-9/popen-read-and-write-both-how-201083/
	 * https://stackoverflow.com/questions/29554036/waiting-for-popen-subprocess-to-terminate-before-reading?rq=1
	 */

	int pipes[3][2];

	if(stdin != nullptr)
		initPipe(pipes[STD_IN]);
	if(stdout != nullptr)
		initPipe(pipes[STD_OUT]);
	if(stderr != nullptr)
		initPipe(pipes[STD_ERR]);

	pid_t pid = fork();
	if(pid == 0) //child
	{
		runChild(command, pipes, stdin != nullptr, stdout != nullptr, stderr != nullptr);
		/*
		 * Nothing below this line should be executed by child process. If so, it means that the exec function wasn't successful, so lets exit:
		 */
		throw CompilationError(CompilationStep::GENERAL, "Error executing the child process", strerror(errno));
	}

	char buffer[BUFFER_SIZE];
	int numBytes = -1;
	int exitStatus = 0;
	bool childFinished = false;

	PROFILE_START(WriteToChildProcess);
	//write to stdin
	if(stdin != nullptr)
	{
		std::istream& in = *stdin;
		while(!(childFinished = childFinished || isChildFinished(pid, &exitStatus)))
		{
			in.read(buffer, BUFFER_SIZE);
			numBytes = in.gcount();
			write(pipes[STD_IN][WRITE], buffer, in.gcount());
			if(numBytes != BUFFER_SIZE)
				break;
		}
		closePipe(pipes[STD_IN][READ]);
		closePipe(pipes[STD_IN][WRITE]);
	}
	PROFILE_END(WriteToChildProcess);

	bool stdOutFinished = stdout != nullptr;
	bool stdErrFinished = stderr != nullptr;

	int highestFD = std::max(pipes[STD_OUT][READ], pipes[STD_ERR][READ]);
	fd_set readDescriptors;
	//wait 1ms
	timeval timeout;
	timeout.tv_sec = 0;
	timeout.tv_usec = 1000;

	/*
	 * While the child program has not yet finished and the output-streams are not read to the end (EOF, thus the child process has also finished),
	 * check which output stream has data and read it
	 */
	PROFILE_START(ReadFromChildProcess);
	while(!(stdOutFinished && stdErrFinished) || !(childFinished = childFinished || isChildFinished(pid, &exitStatus)))
	{
		FD_ZERO(&readDescriptors);
		if(stdout != nullptr)
			FD_SET(pipes[STD_OUT][READ], &readDescriptors);
		if(stderr != nullptr)
			FD_SET(pipes[STD_ERR][READ], &readDescriptors);
		/*
		 * "Those listed in readfds will be watched to see if characters become available for reading
		 * [...] On exit, the sets are modified in place to indicate which file descriptors actually changed status."
		 *
		 * "nfds is the highest-numbered file descriptor in any of the three sets, plus 1.
		 *
		 * "On success, select() and pselect() return the number of file descriptors contained in the three returned descriptor sets
		 * [...] which may be zero if the timeout expires before anything interesting happens"
		 */
		int selectStatus = select(highestFD + 1, &readDescriptors, NULL, NULL, &timeout);
		if(selectStatus == -1)
			throw CompilationError(CompilationStep::GENERAL, "Error waiting on child's output streams", strerror(errno));
		else if(selectStatus != 0)
		{
			//something happened
			if(stdout != nullptr && FD_ISSET(pipes[STD_OUT][READ], &readDescriptors))
			{
				numBytes = read(pipes[STD_OUT][READ], buffer, BUFFER_SIZE);
				stdout->write(buffer, numBytes);
				if(numBytes == 0)
					//EOF
					stdOutFinished = true;
			}
			if(stderr != nullptr && FD_ISSET(pipes[STD_ERR][READ], &readDescriptors))
			{
				numBytes = read(pipes[STD_ERR][READ], buffer, BUFFER_SIZE);
				stderr->write(buffer, numBytes);
				if(numBytes == 0)
					//EOF
					stdErrFinished = true;
			}
		}
	}
	PROFILE_END(ReadFromChildProcess);

	if(stdout != nullptr)
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
