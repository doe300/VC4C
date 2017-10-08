/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <config.h>
#include <cstdlib>
#include <fstream>
#include <string.h>
#include <cstdio>
#include <unistd.h>

#include "Compiler.h"
#include "log.h"
#include "Profiler.h"

using namespace std;
using namespace vc4c;

/*
 * 
 */
int main(int argc, char** argv)
{
    
    if(argc < 3)
    {
        std::cerr << "Usage: vc4c [flags] [options] <source> <destination>" << std::endl;
        exit(1);
    }
    
    Configuration config;
    const std::string inputFile(argv[argc - 2]);
    const std::string outputFile(argv[argc - 1]);
    std::string options;
    
    for(int i = 1; i < argc - 2; ++i)
    {
        //flags
        if(strcmp("--hex", argv[i]) == 0)
            config.outputMode = OutputMode::HEX;
        else if(strcmp("--bin", argv[i]) == 0)
            config.outputMode = OutputMode::BINARY;
        else if(strcmp("--asm", argv[i]) == 0)
            config.outputMode = OutputMode::ASSEMBLER;
        else if(strcmp("--fast-math", argv[i]) == 0)
            config.mathType = MathType::FAST;
        else if(strcmp("--exact-math", argv[i]) == 0)
            config.mathType = MathType::EXACT;
        else if(strcmp("--strict-math", argv[i]) == 0)
            config.mathType = MathType::STRICT;
        else if(strcmp("--kernel-info", argv[i]) == 0)
            config.writeKernelInfo = true;
        else if(strcmp("--no-kernel-info", argv[i]) == 0)
            config.writeKernelInfo = false;
        else
        	options.append(argv[i]).append(" ");
    }
    
    std::cout << "Compiling '" << inputFile << "' into '" << outputFile << "' with options: " << options << std::endl;
    
#if DEBUG_MODE
    setLogger(std::wcout, true, LogLevel::DEBUG);
#else
    setLogger(std::wcout, true, LogLevel::WARNING);
#endif

    std::ifstream input(inputFile);
    std::ofstream output(outputFile, std::ios_base::out|std::ios_base::trunc|std::ios_base::binary);
    PROFILE_START(Compiler);
    Compiler::compile(input, output, config, options, inputFile);
    PROFILE_END(Compiler);
    
    PROFILE_RESULTS();
    return 0;
}
