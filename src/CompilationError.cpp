/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CompilationError.h"

#if !defined(NDEBUG) and defined(__GNUC__)
#include "log.h"

#include <array>
#include <cxxabi.h>
#include <execinfo.h>
#include <memory>
#endif

using namespace vc4c;

// LCOV_EXCL_START
static std::string to_string(const CompilationStep step)
{
    switch(step)
    {
    case CompilationStep::GENERAL:
        return "General";
    case CompilationStep::PRECOMPILATION:
        return "Pre-compilation";
    case CompilationStep::LINKER:
        return "Linker";
    case CompilationStep::SCANNER:
        return "Scanner";
    case CompilationStep::PARSER:
        return "Parser";
    case CompilationStep::LLVM_2_IR:
        return "Instruction Mapping";
    case CompilationStep::NORMALIZER:
        return "Normalizer";
    case CompilationStep::OPTIMIZER:
        return "Optimizer";
    case CompilationStep::LABEL_REGISTER_MAPPING:
        return "Label/Register Mapping";
    case CompilationStep::CODE_GENERATION:
        return "Code Generation";
    default:
        return "Other";
    }
}

CompilationError::CompilationError(const CompilationStep step, const std::string& message) :
    std::runtime_error((to_string(step) + ": ") + message)
{
    logBacktrace();
}

CompilationError::CompilationError(const CompilationStep step, const std::string& message, const std::string& object) :
    std::runtime_error((to_string(step) + ": ") + (message + ": ") + object)
{
    logBacktrace();
}

CompilationError::~CompilationError() noexcept = default;

#if !defined(NDEBUG) and defined(__GNUC__)
// adapted from here: https://stackoverflow.com/a/2526298/8720655
static void demangleAndPrint(char* line, int i)
{
    char* mangled_name = nullptr;
    char* offset_begin = nullptr;
    char* offset_end = nullptr;

    // find parentheses and +address offset surrounding mangled name
    for(char* p = line; *p != '\0'; ++p)
    {
        if(*p == '(')
        {
            mangled_name = p;
        }
        else if(*p == '+')
        {
            offset_begin = p;
        }
        else if(*p == ')')
        {
            offset_end = p;
            break;
        }
    }

    // if the line could be processed, attempt to demangle the symbol
    if(mangled_name && offset_begin && offset_end && mangled_name < offset_begin)
    {
        *mangled_name++ = '\0';
        *offset_begin++ = '\0';
        *offset_end++ = '\0';

        int status = 0;
        char* real_name = abi::__cxa_demangle(mangled_name, nullptr, nullptr, &status);

        // if demangling is successful, output the demangled function name
        if(status == 0)
        {
            logging::error() << " (" << i << ") " << line << " : " << real_name << "+" << offset_begin << offset_end
                             << logging::endl;
        }
        // otherwise, output the mangled function name
        else
        {
            logging::error() << " (" << i << ") " << line << " : " << mangled_name << "+" << offset_begin << offset_end
                             << logging::endl;
        }
        free(real_name);
    }
    // otherwise, print the whole line
    else
    {
        logging::error() << " (" << i << ") " << line << logging::endl;
    }
}
#endif

void CompilationError::logBacktrace()
{
#if !defined(NDEBUG) and defined(__GNUC__)
    std::array<void*, 256> funcPointers{};
    int numFuncs = backtrace(funcPointers.data(), funcPointers.size());

    char** strings = backtrace_symbols(funcPointers.data(), numFuncs);
    // skip first line (this function)
    for(int i = 1; i < numFuncs; i++)
    {
        demangleAndPrint(strings[i], i);
    }
    free(strings);
#endif
}
// LCOV_EXCL_STOP
