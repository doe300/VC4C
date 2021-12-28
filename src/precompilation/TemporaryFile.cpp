/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Precompiler.h"

#include "../Profiler.h"
#include "CompilationError.h"
#include "log.h"

#include <cerrno>
#include <cstring>
#include <fcntl.h>
#include <fstream>
#include <unistd.h>

using namespace vc4c;

static const std::string TEMP_FILE_TEMPLATE = "XXXXXX";

TemporaryFile::TemporaryFile(const std::string& fileTemplate, bool hasStaticLifetime) :
    fileName(fileTemplate), isStaticTemporary(hasStaticLifetime)
{
    PROFILE_COUNTER(profiler::COUNTER_FRONTEND, "TemporaryFile created", 1);
    // make sure, the format is as expected by mkstemp()
    // taken from: https://stackoverflow.com/questions/20446201/how-to-check-if-string-ends-with-txt#20446239
    if(fileName.size() < TEMP_FILE_TEMPLATE.size() ||
        fileName.compare(fileName.size() - TEMP_FILE_TEMPLATE.size(), TEMP_FILE_TEMPLATE.size(), TEMP_FILE_TEMPLATE) !=
            0)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Invalid template for temporary file", fileName);
    if(fileName.find("/tmp/") != 0)
        CPPLOG_LAZY(logging::Level::WARNING,
            log << "Temporary file is not created in /tmp/: " << fileTemplate << logging::endl);
    int fd = mkostemp(const_cast<char*>(fileName.data()), O_CREAT);
    if(fd < 0)
        throw CompilationError(
            CompilationStep::PRECOMPILATION, "Failed to create an unique temporary file", strerror(errno));
    // we don't need this file-descriptor anymore
    if(close(fd) < 0)
        throw CompilationError(
            CompilationStep::PRECOMPILATION, "Failed to close file-descriptor for temporary file", strerror(errno));
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Temporary file '" << fileName << "' created" << logging::endl);
}

TemporaryFile::TemporaryFile(const std::string& fileName, std::istream& data, bool hasStaticLifetime) :
    fileName(fileName), isStaticTemporary(hasStaticLifetime)
{
    PROFILE_COUNTER(profiler::COUNTER_FRONTEND, "TemporaryFile created", 1);
    if(fileName.find("/tmp/") != 0)
        CPPLOG_LAZY(
            logging::Level::WARNING, log << "Temporary file is not created in /tmp/: " << fileName << logging::endl);
    std::ofstream f(fileName, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    f << data.rdbuf();
    // XXX error-check (both streams?)
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Temporary file '" << fileName << "' created" << logging::endl);
}

TemporaryFile::TemporaryFile(const std::string& fileName, const std::vector<char>& data) : fileName(fileName)
{
    PROFILE_COUNTER(profiler::COUNTER_FRONTEND, "TemporaryFile created", 1);
    if(fileName.find("/tmp/") != 0)
        CPPLOG_LAZY(
            logging::Level::WARNING, log << "Temporary file is not created in /tmp/: " << fileName << logging::endl);
    std::ofstream f(fileName, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    f.write(data.data(), static_cast<std::streamsize>(data.size()));
    // XXX error-check stream
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Temporary file '" << fileName << "' created" << logging::endl);
}

TemporaryFile::TemporaryFile(TemporaryFile&& other) noexcept :
    fileName(other.fileName), isStaticTemporary(other.isStaticTemporary)
{
    const_cast<std::string&>(other.fileName) = "";
}

TemporaryFile::~TemporaryFile()
{
    if(fileName.empty())
        // e.g. via move-constructor
        return;
    // since C++ doesn't like exceptions in destructors, just print an error-message and continue
    if(remove(fileName.data()) < 0)
    {
        if(!isStaticTemporary)
            logging::error() << "Failed to remove temporary file: " << strerror(errno) << logging::endl;
    }
    if(!isStaticTemporary)
        // if this object has static lifetime, the logger might no longer exist when the destructor is called
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Temporary file '" << fileName << "' deleted" << logging::endl);
}

void TemporaryFile::openOutputStream(std::unique_ptr<std::ostream>& ptr) const
{
    ptr = std::make_unique<std::ofstream>(fileName, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
}
void TemporaryFile::openInputStream(std::unique_ptr<std::istream>& ptr) const
{
    ptr = std::make_unique<std::ifstream>(fileName, std::ios_base::in | std::ios_base::binary);
}
