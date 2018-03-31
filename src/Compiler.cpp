/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Compiler.h"

#include "BackgroundWorker.h"
#include "Parser.h"
#include "Precompiler.h"
#include "Profiler.h"
#include "asm/CodeGenerator.h"
#include "log.h"
#include "logger.h"
#include "normalization/Normalizer.h"
#include "optimization/Optimizer.h"
#include "spirv/SPIRVParser.h"
#include "llvm/BitcodeReader.h"
#include "llvm/IRParser.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <fcntl.h>
#include <iterator>
#include <memory>
#include <sstream>
#include <unistd.h>
#include <vector>

#ifdef VERIFIER_HEADER
#include VERIFIER_HEADER
#endif

using namespace vc4c;

Parser::~Parser()
{
    // out-of-line virtual method definition
}

Compiler::Compiler(std::istream& stream, std::ostream& output) : input(stream), output(output), config()
{
    if(!input)
        // e.g. if pre-compilation failed
        throw CompilationError(CompilationStep::GENERAL, "Invalid input");
}

static std::unique_ptr<Parser> getParser(std::istream& stream)
{
    // determine which parser to use in which settings
    /*
     * Following options are possible:
     * - LLVM IR parser
     * - SPIR-V parser with binary input
     * - SPIR-V parser with text input
     */
    SourceType type = Precompiler::getSourceType(stream);
    switch(type)
    {
    case SourceType::LLVM_IR_TEXT:
        logging::info() << "Using LLVM-IR frontend..." << logging::endl;
#ifdef USE_LLVM_LIBRARY
        return std::unique_ptr<Parser>(new llvm2qasm::BitcodeReader(stream, SourceType::LLVM_IR_TEXT));
#else
        return std::unique_ptr<Parser>(new llvm2qasm::IRParser(stream));
#endif
    case SourceType::LLVM_IR_BIN:
#ifdef USE_LLVM_LIBRARY
        return std::unique_ptr<Parser>(new llvm2qasm::BitcodeReader(stream, SourceType::LLVM_IR_BIN));
#else
        throw CompilationError(
            CompilationStep::GENERAL, "LLVM-IR binary needs to be first converted to SPIR-V binary or LLVM-IR text!");
#endif
    case SourceType::SPIRV_TEXT:
        throw CompilationError(CompilationStep::GENERAL, "SPIR-V text needs to be first converted to SPIR-V binary!");
    case SourceType::SPIRV_BIN:
        logging::info() << "Using SPIR-V frontend..." << logging::endl;
        return std::unique_ptr<Parser>(new spirv2qasm::SPIRVParser(stream, false));
    case SourceType::OPENCL_C:
        throw CompilationError(CompilationStep::GENERAL, "OpenCL code needs to be first compiled with CLang!");
    case SourceType::QPUASM_BIN:
    case SourceType::QPUASM_HEX:
        throw CompilationError(CompilationStep::GENERAL, "Input code is already compiled machine-code!");
    case SourceType::UNKNOWN:
        throw CompilationError(CompilationStep::GENERAL, "Unrecognized source code type!");
    }
    return nullptr;
}

// register/instruction mapping
static void toMachineCode(qpu_asm::CodeGenerator& codeGen, Method& kernel)
{
    kernel.cleanLocals();
    const auto& instructions = codeGen.generateInstructions(kernel);
#ifdef VERIFIER_HEADER
    std::vector<uint64_t> hexData;
    hexData.reserve(instructions.size());
    for(const std::unique_ptr<qpu_asm::Instruction>& instr : instructions)
    {
        hexData.push_back(instr->toBinaryCode());
    }

    Validator v;
    v.OnMessage = [&instructions](const Message& msg) -> void {
        const Validator::Message& validatorMessage = dynamic_cast<const Validator::Message&>(msg);
        if(validatorMessage.Loc >= 0)
        {
            auto it = instructions.begin();
            std::advance(it, validatorMessage.Loc);
            logging::error() << "Validation-error '" << validatorMessage.Text << "' in: " << (*it)->toASMString()
                             << logging::endl;
            if(validatorMessage.RefLoc >= 0)
            {
                it = instructions.begin();
                std::advance(it, validatorMessage.RefLoc);
                logging::error() << "With reference to instruction: " << (*it)->toASMString() << logging::endl;
            }
        }
        throw CompilationError(CompilationStep::VERIFIER, msg.toString());
    };
    v.Instructions = &hexData;
    logging::info() << "Validation-output: " << logging::endl;
    v.Validate();
    fflush(stderr);
#endif
}

std::size_t Compiler::convert()
{
    Module module(config);

    std::unique_ptr<Parser> parser = getParser(input);
    PROFILE_START(Parser);
    parser->parse(module);
    PROFILE_END(Parser);

    normalization::Normalizer norm(config);
    optimizations::Optimizer opt(config);
    qpu_asm::CodeGenerator codeGen(module, config);

    PROFILE_START(Normalizer);
    norm.normalize(module);
    PROFILE_END(Normalizer);

    PROFILE_START(Optimizer);
    opt.optimize(module);
    PROFILE_END(Optimizer);

    PROFILE_START(SecondNormalizer);
    norm.adjust(module);
    PROFILE_END(SecondNormalizer);

    std::vector<BackgroundWorker> workers;
    workers.reserve(module.getKernels().size());
    for(Method* kernelFunc : module.getKernels())
    {
        auto f = [&codeGen, kernelFunc]() -> void { toMachineCode(codeGen, *kernelFunc); };
        workers.emplace(workers.end(), f, "Code Generator")->operator()();
    }
    BackgroundWorker::waitForAll(workers);

    // TODO could discard unused globals
    // since they are exported, they are still in the intermediate code, even if not used (e.g. optimized away)

    // code generation
    std::size_t bytesWritten = codeGen.writeOutput(output);
    output.flush();

    return bytesWritten;
}

Configuration& Compiler::getConfiguration()
{
    return config;
}

const Configuration& Compiler::getConfiguration() const
{
    return config;
}

std::size_t Compiler::compile(std::istream& input, std::ostream& output, const Configuration config,
    const std::string& options, const Optional<std::string>& inputFile)
{
    try
    {
        // pre-compilation
        TemporaryFile tmpFile;
        std::unique_ptr<std::istream> in;
        Precompiler::precompile(input, in, config, options, inputFile, tmpFile.fileName);

        if(in == nullptr ||
            (dynamic_cast<std::istringstream*>(in.get()) != nullptr &&
                dynamic_cast<std::istringstream*>(in.get())->str().empty()))
            // replace only when pre-compiled (and not just linked output to input, e.g. if source-type is output-type)
            tmpFile.openInputStream(in);

        // compilation
        Compiler conv(*in.get(), output);

        conv.getConfiguration() = config;
        std::size_t result = conv.convert();

        // clean-up
        std::wcout.flush();
        std::wcerr.flush();
        output.flush();

        logging::debug() << "Compilation complete: " << result << " bytes written" << logging::endl;

        return result;
    }
    catch(const CompilationError& e)
    {
        // log exception to log
        logging::error() << "Compiler threw exception: " << e.what() << logging::endl;
        // re-throw, so caller gets notified
        throw;
    }
}

std::unique_ptr<logging::Logger> logging::LOGGER(new logging::ColoredLogger(std::wcout, logging::Level::WARNING));

void vc4c::setLogger(std::wostream& outputStream, const bool coloredOutput, const LogLevel level)
{
    if(coloredOutput)
        logging::LOGGER.reset(new logging::ColoredLogger(outputStream, static_cast<logging::Level>(level)));
    else
        logging::LOGGER.reset(new logging::StreamLogger(outputStream, static_cast<logging::Level>(level)));
}
