/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LibClang.h"

#include "../Profiler.h"
#include "../helper.h"
#include "CompilationError.h"
#include "Precompiler.h"
#include "log.h"
#include "tool_paths.h"

#ifdef USE_LIBCLANG
#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/IR/Module.h"

#include <memory>

// TODO different LLVM/LibClang versions

// FIXME exits after/in precompilation
// FIXME correctly fix standard headers

using namespace vc4c;
using namespace vc4c::precompilation;

#if LLVM_LIBRARY_VERSION >= 100
static constexpr auto INPUT_LANGUAGE = clang::Language::OpenCL;
#else
static constexpr auto INPUT_LANGUAGE = clang::InputKind::Language::OpenCL;
#endif

extern std::unique_ptr<llvm::MemoryBuffer> fromInputStream(std::istream& stream, std::string& buffer);

struct LogConsumer : public clang::DiagnosticConsumer
{
    ~LogConsumer() override;

    static void convertToLog(std::wostream& out, const clang::Diagnostic& info)
    {
        llvm::SmallVector<char, 128> tmp;
        info.FormatDiagnostic(tmp);
        out << info.getLocation().printToString(info.getSourceManager()) << ": " << tmp.data() << logging::endl;
    }

    void HandleDiagnostic(clang::DiagnosticsEngine::Level DiagLevel, const clang::Diagnostic& Info) override
    {
        switch(DiagLevel)
        {
        case clang::DiagnosticsEngine::Level::Fatal:
        case clang::DiagnosticsEngine::Level::Error:
            logging::logLazy(logging::Level::ERROR, [&]() { convertToLog(logging::error(), Info); });
            ++NumErrors;
            break;
        case clang::DiagnosticsEngine::Level::Warning:
            logging::logLazy(logging::Level::WARNING, [&]() { convertToLog(logging::warn(), Info); });
            ++NumWarnings;
            break;
        case clang::DiagnosticsEngine::Level::Remark:
            logging::logLazy(logging::Level::INFO, [&]() { convertToLog(logging::info(), Info); });
            break;
        case clang::DiagnosticsEngine::Level::Note:
            logging::logLazy(logging::Level::DEBUG, [&]() { convertToLog(logging::debug(), Info); });
            break;
        default:; // ignore
        }
    }
};

LogConsumer::~LogConsumer() {}

static void dumpCompilationOptions(const clang::CompilerInvocation& invocation)
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Code gen options:" << logging::endl;
        logging::debug() << "\tModel: " << invocation.getCodeGenOpts().CodeModel << logging::endl;
        logging::debug() << "\t-cl-fp32-correctly-rounded-divide-sqrt: "
                         << invocation.getCodeGenOpts().CorrectlyRoundedDivSqrt << logging::endl;
        logging::debug() << "\tDebug pass: " << invocation.getCodeGenOpts().DebugPass << logging::endl;
        logging::debug() << "\tDisable LLVM passes: " << invocation.getCodeGenOpts().DisableLLVMPasses << logging::endl;
        logging::debug() << "\tOptimization level: " << invocation.getCodeGenOpts().OptimizationLevel << logging::endl;
        logging::debug() << "\tSize optimization level: " << invocation.getCodeGenOpts().OptimizeSize << logging::endl;
        logging::debug() << "\tLoop unrolling: " << invocation.getCodeGenOpts().UnrollLoops << logging::endl;
        logging::debug() << "\tLoop vectorization: " << invocation.getCodeGenOpts().VectorizeLoop << logging::endl;
        logging::debug() << "\tFloat ABI: " << invocation.getCodeGenOpts().FloatABI << logging::endl;
#if LLVM_LIBRARY_VERSION >= 100
        logging::debug() << "\tDenormal mode: " << llvm::denormalModeName(invocation.getCodeGenOpts().FPDenormalMode)
                         << logging::endl;
#else
        logging::debug() << "\tDenormal mode: " << invocation.getCodeGenOpts().FPDenormalMode << logging::endl;
#endif
        // TODO explicitly set this?
        logging::debug() << "\tPreferred vector width: " << invocation.getCodeGenOpts().PreferVectorWidth
                         << logging::endl;
        logging::debug() << "\tMain file: " << invocation.getCodeGenOpts().MainFileName << logging::endl;
    });

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Frontend options:" << logging::endl;
        logging::debug() << "\tOutput file: " << invocation.getFrontendOpts().OutputFile << logging::endl;
        logging::debug() << "\tInput files:" << logging::endl;
        for(const auto& file : invocation.getFrontendOpts().Inputs)
            if(file.isFile())
                logging::debug() << "\t\t" << file.getFile() << (file.isSystem() ? "(system)" : "")
                                 << (file.isPreprocessed() ? "(preprocessed)" : "") << (file.isEmpty() ? "(empty)" : "")
                                 << logging::endl;
            else
                logging::debug() << "\t\t"
                                 << "(buffer)" << (file.isSystem() ? "(system)" : "")
                                 << (file.isPreprocessed() ? "(preprocessed)" : "") << (file.isEmpty() ? "(empty)" : "")
                                 << logging::endl;
    });

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Header search options:" << logging::endl;
        logging::debug() << "\tSysroot: " << invocation.getHeaderSearchOpts().Sysroot << logging::endl;
        logging::debug() << "\tSystem includes:" << logging::endl;
        for(const auto& entry : invocation.getHeaderSearchOpts().SystemHeaderPrefixes)
            logging::debug() << "\t\t" << entry.Prefix << (entry.IsSystemHeader ? "(system)" : "") << logging::endl;
        logging::debug() << "\tUser includes:" << logging::endl;
        for(const auto& entry : invocation.getHeaderSearchOpts().UserEntries)
            logging::debug() << "\t\t" << entry.Path << logging::endl;
        logging::debug() << "\tCompiler resources: " << invocation.getHeaderSearchOpts().ResourceDir << logging::endl;
        logging::debug() << "\tModule mapping:" << logging::endl;
        for(const auto& entry : invocation.getHeaderSearchOpts().PrebuiltModuleFiles)
            logging::debug() << "\t\t" << entry.first << " -> " << entry.second << logging::endl;
        logging::debug() << "\tModule paths:" << logging::endl;
        for(const auto& entry : invocation.getHeaderSearchOpts().PrebuiltModulePaths)
            logging::debug() << "\t\t" << entry << logging::endl;
        logging::debug() << "\tModule format: " << invocation.getHeaderSearchOpts().ModuleFormat << logging::endl;
        logging::debug() << "\tBuiltin includes: " << invocation.getHeaderSearchOpts().UseBuiltinIncludes
                         << logging::endl;
        logging::debug() << "\tSystem includes: " << invocation.getHeaderSearchOpts().UseStandardSystemIncludes
                         << logging::endl;
    });

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Language options:" << logging::endl;
        logging::debug() << "\tOpenCL version: " << invocation.getLangOpts()->getOpenCLVersionTuple().getAsString()
                         << logging::endl;
        logging::debug() << "\tNative half type support: " << invocation.getLangOpts()->NativeHalfType << logging::endl;
    });

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Target option:" << logging::endl;
        logging::debug() << "\tTarget triple: " << invocation.getTargetOpts().Triple << logging::endl;
        logging::debug() << "\tCPU: " << invocation.getTargetOpts().CPU << logging::endl;
        logging::debug() << "\tFloat unit: " << invocation.getTargetOpts().FPMath << logging::endl;
        logging::debug() << "\tABI: " << invocation.getTargetOpts().ABI << logging::endl;
        logging::debug() << "\tOpenCL extensions: "
                         << to_string<std::string>(invocation.getTargetOpts().OpenCLExtensionsAsWritten)
                         << logging::endl;
    });
}

// code adapted from: http://fdiv.net/2012/08/15/compiling-code-clang-api
void precompilation::compileLibClang(const std::vector<std::string>& command, std::istream* inputStream,
    std::ostream* outputStream, const Optional<std::string>& inputFile, const Optional<std::string>& outputFile)
{
    std::vector<const char*> args;
    args.reserve(command.size());
    for(const auto& cmd : command)
        args.emplace_back(cmd.data());
    // args[0] is the compiler command
    args.erase(args.begin());

    llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> diagnosticOptions(new clang::DiagnosticOptions());
    llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs> diagnosticIds(new clang::DiagnosticIDs());
    clang::DiagnosticsEngine diagnostics(diagnosticIds, diagnosticOptions);

    std::shared_ptr<clang::CompilerInvocation> invocation(new clang::CompilerInvocation());

    // set default options
    clang::CompilerInvocation::setLangDefaults(*invocation->getLangOpts(), INPUT_LANGUAGE, llvm::Triple{},
        invocation->getPreprocessorOpts(), clang::LangStandard::lang_opencl12);

// read options from command line
#if LLVM_LIBRARY_VERSION >= 100
    if(!clang::CompilerInvocation::CreateFromArgs(*invocation.get(), args, diagnostics))
#else
    if(!clang::CompilerInvocation::CreateFromArgs(*invocation.get(), &args.front(), &args.back(), diagnostics))
#endif
    {
        // TODO here (and everywhere else) extract and log diagnostics
        throw CompilationError(CompilationStep::PRECOMPILATION,
            "Failed to create compiler invocation from command line", to_string<std::string>(command, " "));
    }

    // set custom options
    // keep value names in IR
    invocation->getCodeGenOpts().DiscardValueNames = 0;
    // keep lifetime markers
    invocation->getCodeGenOpts().DisableLifetimeMarkers = 0;
    // enable OpenCL kernel argument metadata
    invocation->getCodeGenOpts().EmitOpenCLArgMetadata = 1;
    // XXX depending on config/OpenCL options: NoInfsFPMath, NoSignedZeros, NoNaNsFPMath, UnsafeFPMath
    // flush denormal float values to zero
    invocation->getCodeGenOpts().FlushDenorm = 1;
    // -cl-uniform-work-group-size, force uniform work-group size (may already be implicitly set by -std=cl1.2)
    invocation->getCodeGenOpts().UniformWGSize = 1;
    // XXX can set LinkBitcodeFiles to directly link in bitcode files (e.g. standard-library)

    if(inputFile && invocation->getCodeGenOpts().MainFileName.empty())
    {
        invocation->getCodeGenOpts().MainFileName = *inputFile;
    }

    // re-direct input and output (if streams)
    std::pair<std::string, std::unique_ptr<llvm::MemoryBuffer>> inputBuffer;
    if(inputStream)
    {
        // rewrite from expecting input at stdin to using buffer
        for(auto& input : invocation->getFrontendOpts().Inputs)
        {
            if(input.isBuffer() || input.getFile() == "-")
            {
                inputBuffer.second = fromInputStream(*inputStream, inputBuffer.first);
                // XXX input kind always correct??
                input = clang::FrontendInputFile(inputBuffer.second.get(), INPUT_LANGUAGE);
                break;
            }
        }
    }
    else if(std::none_of(invocation->getFrontendOpts().Inputs.begin(), invocation->getFrontendOpts().Inputs.end(),
                [&](const clang::FrontendInputFile& file) -> bool {
                    return file.isFile() && file.getFile() == *inputFile;
                }))
    {
        // TODO why is this in the first place? The command clearly states the input file (and no stdin)??
        for(auto& input : invocation->getFrontendOpts().Inputs)
        {
            if(input.isBuffer() || input.getFile() == "-")
            {
                // XXX input kind always correct??
                input = clang::FrontendInputFile(*inputFile, INPUT_LANGUAGE);
            }
        }
    }

    // TODO use opencl_c module?? (See
    // https://github.com/llvm-mirror/clang/blob/f3b7928366f63b51ffc97e74f8afcff497c57e8d/lib/Headers/module.modulemap#L168)

    // TODO set OpenCL default headers include directory?? CLang internal or VC4CLStdLib??
    invocation->getHeaderSearchOpts().AddPath(
        "/home/daniel/workspace/VC4C/../VC4CLStdLib/include", clang::frontend::IncludeDirGroup::System, false, true);

    // TODO redirect output if stream (or copy MemoryBuffer to stream afterwards)

    dumpCompilationOptions(*invocation);

    // XXX can set PCHReader, make use to directly include pch??
    // TODO also directly link in stdlib module??
    // TODO also compile into buffer and re-use buffer (no temporary file!)
    clang::CompilerInstance instance;
    instance.setInvocation(invocation);

    instance.createDiagnostics(new LogConsumer());
    if(!instance.hasDiagnostics())
        throw CompilationError(CompilationStep::PRECOMPILATION, "compiler instance has no diagnostics set");

    auto commandIt = command.end();
    if((commandIt = std::find(command.begin(), command.end(), "-emit-pch")) != command.end())
    {
        ++commandIt;
        std::shared_ptr<clang::PCHBuffer> pchBuffer(new clang::PCHBuffer());
        std::vector<std::shared_ptr<clang::ModuleFileExtension>> extensions;
        auto consumer = std::make_unique<clang::PCHGenerator>(instance.getPreprocessor(), instance.getModuleCache(),
            *commandIt, /*isysroot=*/"", pchBuffer, extensions, /*AllowASTWithErrors=*/true);
        instance.getASTContext().setASTMutationListener(consumer->GetASTMutationListener());
        instance.setASTConsumer(std::move(consumer));
        clang::PreprocessOnlyAction action;
        PROFILE_START(EmitPCHAction);
        instance.ExecuteAction(action);
        PROFILE_END(EmitPCHAction);
    }
    else
    {
        PROFILE_START(EmitBCAction);
        clang::EmitBCAction action;
        if(!instance.ExecuteAction(action))
        {
            // TODO print diagnostics
            throw CompilationError(CompilationStep::PRECOMPILATION, "Error in precompilation - BETTER ERROR MESSAGE!");
        }
        PROFILE_END(EmitBCAction);
    }

    // TODO use module directly: action.takeModule()
}

#endif /* USE_LIBCLANG */
