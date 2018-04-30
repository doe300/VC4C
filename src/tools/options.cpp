/*
 * Contains the public functions to access additional tools not part of the compiler itself, like the built-in emulator
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "tools.h"

#include "../optimization/Optimizer.h"
#include "log.h"

#include <stdexcept>

using namespace vc4c;
using namespace vc4c::tools;

static auto availableOptimizations = vc4c::optimizations::Optimizer::getPasses(OptimizationLevel::FULL);

bool tools::parseConfigurationParameter(Configuration& config, const std::string& arg)
{
    if(arg == "--hex")
    {
        config.outputMode = OutputMode::HEX;
        return true;
    }
    if(arg == "--bin")
    {
        config.outputMode = OutputMode::BINARY;
        return true;
    }
    if(arg == "--asm")
    {
        config.outputMode = OutputMode::ASSEMBLER;
        return true;
    }
    if(arg == "--fast-math")
    {
        config.mathType = MathType::FAST;
        return true;
    }
    if(arg == "--exact-math")
    {
        config.mathType = MathType::EXACT;
        return true;
    }
    if(arg == "--strict-math")
    {
        config.mathType = MathType::STRICT;
        return true;
    }
    if(arg == "--kernel-info")
    {
        config.writeKernelInfo = true;
        return true;
    }
    if(arg == "--no-kernel-info")
    {
        config.writeKernelInfo = false;
        return true;
    }
    if(arg == "--spirv")
    {
        config.frontend = Frontend::SPIR_V;
        return true;
    }
    if(arg == "--llvm")
    {
        config.frontend = Frontend::LLVM_IR;
        return true;
    }
    if(arg == "-O0")
    {
        config.optimizationLevel = OptimizationLevel::NONE;
        return true;
    }
    if(arg == "-O1")
    {
        config.optimizationLevel = OptimizationLevel::BASIC;
        return true;
    }
    if(arg == "-O2")
    {
        config.optimizationLevel = OptimizationLevel::MEDIUM;
        return true;
    }
    if(arg == "-O3")
    {
        config.optimizationLevel = OptimizationLevel::FULL;
        return true;
    }

    std::string passName;
    if(arg.find("--fno-") == 0)
    {
        passName = arg.substr(std::string("--fno-").size());
        if(availableOptimizations.find(passName) != availableOptimizations.end())
        {
            config.additionalDisabledOptimizations.emplace(passName);
            logging::debug() << "Disabling optimization: " << passName << logging::endl;
            return true;
        }

        std::cerr << "Cannot disable unknown optimization: " << passName << std::endl;
        return false;
    }
    else if(arg.find("--f") == 0)
    {
        passName = arg.substr(std::string("--f").size());
        if(passName.find('=') != std::string::npos)
        {
            // optimization parameter
            std::string value = passName.substr(passName.find('=') + 1);
            const std::string paramName = passName.substr(0, passName.find('='));
            int intValue;
            try
            {
                intValue = std::stoi(value);
            }
            catch(std::exception& e)
            {
                std::cerr << "Error converting optimization parameter for '" << paramName << ": " << e.what()
                          << std::endl;
                return false;
            }
            if(paramName == "combine-load-threshold")
                config.additionalOptions.combineLoadThreshold = intValue;
            else if(paramName == "accumulator-threshold")
                config.additionalOptions.accumulatorThreshold = intValue;
            else if(paramName == "replace-nop-threshold")
                config.additionalOptions.replaceNopThreshold = intValue;
            else if(paramName == "register-resolver-rounds")
                config.additionalOptions.registerResolverMaxRounds = intValue;
            else if(paramName == "move-constants-depth")
                config.additionalOptions.moveConstantsDepth = Optional<int>(intValue);
            else
            {
                std::cerr << "Cannot set unknown optimization parameter: " << paramName << " to " << value << std::endl;
                return false;
            }
            return true;
        }
        else if(availableOptimizations.find(passName) != availableOptimizations.end())
        {
            config.additionalEnabledOptimizations.emplace(passName);
            logging::debug() << "Enabling optimization: " << passName << logging::endl;
            return true;
        }
        std::cerr << "Cannot enable unknown optimization: " << passName << std::endl;
        return false;
    }
    return false;
}
