/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CompilationError.h"

using namespace vc4c;

static std::string to_sting(const CompilationStep step)
{
    switch(step)
    {
    case CompilationStep::GENERAL:
        return "General";
    case CompilationStep::PRECOMPILATION:
        return "Pre-compilation";
    case CompilationStep::SCANNER:
        return "Scanner";
    case CompilationStep::PARSER:
        return "Parser";
    case CompilationStep::LLVM_2_IR:
        return "Instruction Mapping";
    case CompilationStep::OPTIMIZER:
        return "Optimizer";
    case CompilationStep::LABEL_REGISTER_MAPPING:
        return "Label/Register Mapping";
    case CompilationStep::CODE_GENERATION:
        return "Code Generation";
    case CompilationStep::VERIFIER:
    	return "Verifier";
    default:
        return "Other";
    }
}

CompilationError::CompilationError(const CompilationStep step, const std::string& message) :
        std::runtime_error((to_sting(step) + ": ") + message)
{
}

CompilationError::CompilationError(const CompilationStep step, const std::size_t line, const std::string& message) :
std::runtime_error((to_sting(step) + ": In line ") + (std::to_string(line) + ": ") + message)
{

}

CompilationError::CompilationError(const CompilationStep step, const std::string& type, const std::string& message) :
std::runtime_error((to_sting(step) + ": ") + (type + ": ") + message)
{

}


CompilationError::~CompilationError()
{
}

