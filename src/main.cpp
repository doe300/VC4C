/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Compiler.h"
#include "Precompiler.h"
#include "Profiler.h"
#include "config.h"
#include "log.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <unordered_map>
#include <sstream>
#include <unistd.h>

using namespace std;
using namespace vc4c;

/*
 * 
 */
int main(int argc, char** argv)
{
    
    if(argc < 3)
    {
        std::cerr << "Usage: vc4c [flags] [options] -o <destination> <sources>" << std::endl;
        std::cerr << "flags:" << std::endl;
        std::cerr << "\t--hex\t\t\tGenerate hex output (e.g. included in source-code)" << std::endl;
        std::cerr << "\t--bin\t\t\tGenerate binary output (as used by VC4CL run-time)" << std::endl;
        std::cerr << "\t--asm\t\t\tGenerate assembler output (for analysis only)" << std::endl;
        std::cerr << "options:" << std::endl;
        std::cerr << "\t--kernel-info\t\tWrite the kernel-info meta-data (as required by VC4CL run-time, default)" << std::endl;
        std::cerr << "\t--no-kernel-info\tDont write the kernel-info meta-data" << std::endl;
        std::cerr << "\t--spirv\t\t\tExplicitely use the SPIR-V front-end" << std::endl;
        std::cerr << "\t--llvm\t\t\tExplicitely use the LLVM-IR front-end" << std::endl;
        std::cerr << "\tany other option is passed to the pre-compiler" << std::endl;
        return 1;
    }
    
    Configuration config;
    std::vector<std::string> inputFiles;
    std::string outputFile;
    std::string options;
    
    int i = 1;
    for(; i < argc - 2; ++i)
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
        else if(strcmp("--spirv", argv[i]) == 0)
        	config.frontend = Frontend::SPIR_V;
        else if(strcmp("--llvm", argv[i]) == 0)
        	config.frontend = Frontend::LLVM_IR;
        else if(strcmp("-o", argv[i]) == 0)
        {
        	outputFile = argv[i+1];
        	//any further parameter is an input-file
        	i += 2;
        	break;
        }
        else
        	options.append(argv[i]).append(" ");
    }
    
    for(;i < argc; ++i)
    {
    	inputFiles.emplace_back(argv[i]);
    }

    if(inputFiles.empty())
    {
    	std::cerr << "No input file(s) specified, aborting!" << std::endl;
    	return 2;
    }
    if(outputFile.empty())
    {
    	std::cerr << "No output file specified, aborting!" << std::endl;
		return 3;
    }

    std::cout << "Compiling '" << to_string<std::string>(inputFiles, "', '") << "' into '" << outputFile << "' with options '" << options << "' ..." << std::endl;
    
#if DEBUG_MODE
    setLogger(std::wcout, true, LogLevel::DEBUG);
#else
    setLogger(std::wcout, true, LogLevel::WARNING);
#endif

    Optional<std::string> inputFile;
    std::unique_ptr<std::istream> input;
    //link if necessary
    if(inputFiles.size() > 1)
    {
    	std::vector<std::unique_ptr<std::istream>> fileStreams;
    	std::unordered_map<std::istream*, Optional<std::string>> inputs;
    	for(const std::string& file : inputFiles)
    	{
    		fileStreams.emplace_back(new std::ifstream(file));
    		inputs.emplace(fileStreams.back().get(), Optional<std::string>(file));
    	}
    	input.reset(new std::stringstream());
    	Precompiler::linkSourceCode(inputs, *reinterpret_cast<std::ostream*>(input.get()));
    }
    else
    {
		input.reset(new std::ifstream(inputFiles.at(0)));
		inputFile = inputFiles.at(0);
    }

    std::ofstream output(outputFile, std::ios_base::out|std::ios_base::trunc|std::ios_base::binary);
	PROFILE_START(Compiler);
	Compiler::compile(*input.get(), output, config, options, inputFile);
	PROFILE_END(Compiler);

    PROFILE_RESULTS();
    return 0;
}
