/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Precompiler.h"

#include "log.h"

#include <cerrno>
#include <cstring>
#include <fcntl.h>
#include <fstream>
#include <unistd.h>

using namespace vc4c;

static const std::string TEMP_FILE_TEMPLATE = "XXXXXX";

TemporaryFile::TemporaryFile(const std::string& fileTemplate) : fileName(fileTemplate)
{
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

TemporaryFile::TemporaryFile(const std::string& fileName, std::istream& data) : fileName(fileName)
{
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
    if(fileName.find("/tmp/") != 0)
        CPPLOG_LAZY(
            logging::Level::WARNING, log << "Temporary file is not created in /tmp/: " << fileName << logging::endl);
    std::ofstream f(fileName, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    f.write(data.data(), data.size());
    // XXX error-check stream
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Temporary file '" << fileName << "' created" << logging::endl);
}

TemporaryFile::TemporaryFile(TemporaryFile&& other) noexcept : fileName(other.fileName)
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
        logging::error() << "Failed to remove temporary file: " << strerror(errno) << logging::endl;
    }
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Temporary file '" << fileName << "' deleted" << logging::endl);
}

void TemporaryFile::openOutputStream(std::unique_ptr<std::ostream>& ptr) const
{
    ptr.reset(new std::ofstream(fileName, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary));
}
void TemporaryFile::openInputStream(std::unique_ptr<std::istream>& ptr) const
{
    ptr.reset(new std::ifstream(fileName, std::ios_base::in | std::ios_base::binary));
}
