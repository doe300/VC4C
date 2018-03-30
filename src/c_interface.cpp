/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "../include/c_interface.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <string.h>

#include "../lib/cpplog/include/logger.h"
#include "CompilationError.h"
#include "Compiler.h"
#include "Precompiler.h"
#include "log.h"

using namespace vc4c;

const configuration DEFAULT_CONFIG = {MATH_TYPE_FAST, OUTPUT_BINARY, LOG_WARNING};

static CompilationErrorHandler errorCallback = NULL;
static void* callbackData = NULL;

int convert(const storage* in, storage* out, const configuration config, const char* options)
{
    // TODO allow to redirect log
    logging::LOGGER.reset(new logging::ColoredLogger(std::wcerr, static_cast<logging::Level>(config.log_level)));
    Configuration realConfig;
    realConfig.mathType = static_cast<MathType>(config.math_type);
    realConfig.outputMode = static_cast<OutputMode>(config.output_mode);
    realConfig.writeKernelInfo = true;

    std::unique_ptr<std::istream> is;
    if(in->is_file)
    {
        logging::debug() << "Compiling from source-file: " << in->file_name << logging::endl;
        is.reset(new std::ifstream(in->file_name, std::ios_base::in));
    }
    else
    {
        logging::debug() << "Compiling from input-string with " << in->data_length << " characters..." << logging::endl;
        const std::string s(in->data, in->data_length);
        is.reset(new std::istringstream(s, std::ios_base::in));
    }
    std::unique_ptr<std::ostream> os;
    if(out->is_file)
    {
        logging::debug() << "Compiling into file: " << out->file_name << logging::endl;
        os.reset(new std::ofstream(out->file_name, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary));
    }
    else
    {
        logging::debug() << "Compiling into buffer..." << logging::endl;
        os.reset(new std::ostringstream());
    }

    std::size_t bytesWritten = 0;
    try
    {
        const std::string optionsString(options == NULL ? "" : options);
        bytesWritten = Compiler::compile(*is.get(), *os.get(), realConfig, optionsString);
        logging::info() << "Compilation done, " << bytesWritten << " bytes written!" << logging::endl;
    }
    catch(CompilationError& err)
    {
        logging::severe() << err.what() << logging::endl;
        if(errorCallback != NULL)
        {
            errorCallback(err.what(), strlen(err.what()), callbackData);
        }
        bytesWritten = 0;
        return -15 /* CL_COMPILE_PROGRAM_FAILURE */;
    }

    if(!out->is_file)
    {
        if(out->data == nullptr)
        {
            out->data = static_cast<char*>(malloc(bytesWritten + 1));
            out->data_length = bytesWritten + 1;
        }
        else if(out->data_length < bytesWritten + 1)
        {
            out->data = static_cast<char*>(realloc(out->data, bytesWritten + 1));
            out->data_length = bytesWritten + 1;
        }
        memcpy(out->data, static_cast<std::ostringstream*>(os.get())->str().data(), bytesWritten);
        out->data[bytesWritten] = '\0';
    }

    return bytesWritten > 0 ? 0 /* CL_SUCCESS */ : -15 /* CL_COMPILE_PROGRAM_FAILURE */;
}

void setErrorHandler(CompilationErrorHandler errorHandler, void* userData)
{
    errorCallback = errorHandler;
    callbackData = userData;
}

int determineSourceType(const storage* in)
{
    std::unique_ptr<std::istream> is;
    if(in->is_file)
    {
        is.reset(new std::ifstream(in->file_name, std::ios_base::in));
    }
    else
    {
        const std::string s(in->data, in->data_length);
        is.reset(new std::istringstream(s, std::ios_base::in));
    }

    return static_cast<int>(Precompiler::getSourceType(*is.get()));
}
