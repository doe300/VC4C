/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 *
 * This code is largely adapted from mesa 3D licensed under MIT License, original source:
 * https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/gallium/frontends/clover/llvm/invocation.cpp
 */

#include "LLVMLibrary.h"

#include "../Profiler.h"
#include "../helper.h"
#include "CompilationError.h"
#include "FrontendCompiler.h"
#include "Precompiler.h"
#include "log.h"
#include "tool_paths.h"

#ifdef USE_CLANG_LIBRARY
#include "clang/Basic/TargetInfo.h"
#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Module.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <fstream>
#include <functional>
#include <memory>
#include <sstream>

using namespace vc4c;
using namespace vc4c::precompilation;

#if LLVM_LIBRARY_VERSION >= 100
static constexpr auto INPUT_LANGUAGE = clang::Language::OpenCL;
#else
static constexpr auto INPUT_LANGUAGE = clang::InputKind::OpenCL;
#endif

LCOV_EXCL_START
static void logDiagnostics(clang::TextDiagnosticBuffer& diagnostics, clang::SourceManager* sourceManager)
{
    logging::logLazy(logging::Level::WARNING, [&]() {
        if(diagnostics.getNumWarnings() > 0)
        {
            logging::warn() << "Warnings in precompilation:" << logging::endl;
            for(auto it = diagnostics.warn_begin(); it != diagnostics.warn_end(); ++it)
            {
                auto& log = logging::warn() << it->second;
                if(sourceManager)
                    log << " at " << it->first.printToString(*sourceManager);
                log << logging::endl;
            }
        }
    });
    if(diagnostics.getNumErrors() > 0)
    {
        logging::error() << "Errors in precompilation:" << logging::endl;
        for(auto it = diagnostics.err_begin(); it != diagnostics.err_end(); ++it)
        {
            auto& log = logging::error() << it->second;
            if(sourceManager)
                log << " at " << it->first.printToString(*sourceManager);
            log << logging::endl;
        }
    }
}
LCOV_EXCL_STOP

static std::unique_ptr<clang::CompilerInstance> createInstance(
    const std::vector<const char*>& args, clang::TextDiagnosticBuffer& diagnosticOutput)
{
    auto diagnosticsBuffer = new clang::TextDiagnosticBuffer();
    clang::DiagnosticsEngine diagnostics{new clang::DiagnosticIDs(), new clang::DiagnosticOptions(), diagnosticsBuffer,
        true /* take ownership of buffer */};

    auto instance = std::make_unique<clang::CompilerInstance>();
    auto success =
#if LLVM_LIBRARY_VERSION >= 100
        clang::CompilerInvocation::CreateFromArgs(instance->getInvocation(), args, diagnostics);
#else
        clang::CompilerInvocation::CreateFromArgs(
            instance->getInvocation(), args.data(), args.data() + args.size(), diagnostics);
#endif

    diagnosticsBuffer->FlushDiagnostics(diagnostics);
    logDiagnostics(*diagnosticsBuffer, nullptr);
    if(!success)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Failed to create clang compiler invocation");
    if(diagnostics.hasErrorOccurred())
        throw CompilationError(CompilationStep::PRECOMPILATION, "Error creating clang compiler invocation");

    instance->getTargetOpts().Triple = "spir-unknown-unknown";

#if LLVM_LIBRARY_VERSION >= 120
    clang::CompilerInvocation::setLangDefaults(instance->getLangOpts(), INPUT_LANGUAGE,
        llvm::Triple("spir-unknown-unknown"), instance->getPreprocessorOpts().Includes,
        clang::LangStandard::lang_opencl12);
#else
    clang::CompilerInvocation::setLangDefaults(instance->getLangOpts(), INPUT_LANGUAGE,
        llvm::Triple("spir-unknown-unknown"), instance->getPreprocessorOpts(), clang::LangStandard::lang_opencl12);
#endif

    instance->createDiagnostics(&diagnosticOutput, false /* don't take ownership of printer */);
    instance->setTarget(
        clang::TargetInfo::CreateTargetInfo(instance->getDiagnostics(), instance->getInvocation().TargetOpts));
    return instance;
}

static Optional<std::string> runClangLibraryCompilation(const std::vector<std::string>& command,
    const OpenCLData& inputData, PrecompilationConfig& config, clang::FrontendAction& action,
    clang::frontend::ActionKind actionKind)
{
    LLVMProfilerWrapper profiler{command.front()};

    std::vector<const char*> args;
    args.reserve(command.size());
    bool nextIsOutputFile = false;
    Optional<std::string> outputFilePath;
    bool includeDefaultHeader = false;
    for(const auto& cmd : command)
    {
        if(nextIsOutputFile)
        {
            nextIsOutputFile = false;
            if(cmd == "/dev/stdout")
                continue;
            args.emplace_back("-o");
            outputFilePath = cmd;
        }
        if(cmd == command.front() || cmd == "-cc1")
            continue;
        if(cmd == "-o")
            nextIsOutputFile = true;
        else if(cmd == "-finclude-default-header")
            // Including the header via the preprocessor option (or not including it twice) has significant performance
            // impact, so do it. This flag would basically anyway do the same thing, see https://reviews.llvm.org/D20444
            includeDefaultHeader = true;
        else
            args.emplace_back(cmd.data());
    }

    clang::TextDiagnosticBuffer diagnostics{};
    auto instance = createInstance(args, diagnostics);

    instance->getHeaderSearchOpts().UseBuiltinIncludes = true;
    instance->getHeaderSearchOpts().UseStandardSystemIncludes = true;
    instance->getHeaderSearchOpts().ResourceDir = CLANG_RESOURCE_DIR;
    instance->getHeaderSearchOpts().AddPath(CLANG_RESOURCE_DIR, clang::frontend::Angled, false, false);
    if(includeDefaultHeader)
        instance->getPreprocessorOpts().Includes.push_back("opencl-c.h");
    if(config.includeStdlibPCH)
    {
        // FIXME how to set the PCH path? Or is it already set via command line options?
        instance->getFrontendOpts().ASTMergeFiles.emplace_back(findStandardLibraryFiles().precompiledHeader);
    }

    if(std::find(command.begin(), command.end(), "-ffp-contract=off") != command.end())
        // Is overwritten by CompilerInvocation::setLangDefaults, so set again explicitly
        instance->getLangOpts().setDefaultFPContractMode(clang::LangOptions::FPM_Off);

    std::unique_ptr<llvm::MemoryBuffer> sourceBuffer;
    for(auto& input : instance->getFrontendOpts().Inputs)
    {
        if(input.getFile() == "-")
        {
            // replace with in-memory input
            sourceBuffer = loadLLVMBuffer(inputData);
            if(!sourceBuffer)
                throw CompilationError(CompilationStep::PRECOMPILATION,
                    "Failed to load OpenCL C source into LLVM buffer", inputData.to_string());
#if LLVM_LIBRARY_VERSION >= 120
            input = clang::FrontendInputFile(sourceBuffer->getMemBufferRef(), INPUT_LANGUAGE);
#else
            input = clang::FrontendInputFile(sourceBuffer.get(), INPUT_LANGUAGE);
#endif
        }
    }

    if(config.linkStdlibModule)
    {
        // Directly link in the standard library module, so we do not have to start another linker instance. Also this
        // way, the module is included before any optimizations are performed
        clang::CodeGenOptions::BitcodeFileToLink entry{};
        entry.Filename = findStandardLibraryFiles().llvmModule;
        entry.PropagateAttrs = true;
        entry.LinkFlags = llvm::Linker::Flags::LinkOnlyNeeded;
        instance->getCodeGenOpts().LinkBitcodeFiles.emplace_back(std::move(entry));
        config.linkedStandardLibrary = true; // no need to do it again
    }

    instance->getFrontendOpts().ProgramAction = actionKind;
    if(!instance->ExecuteAction(action))
    {
        logDiagnostics(diagnostics, &instance->getSourceManager());
        throw CompilationError(CompilationStep::PRECOMPILATION, "Error in precompilation");
    }

    logDiagnostics(diagnostics, &instance->getSourceManager());
    return outputFilePath;
}

template <SourceType ResultType, typename Result, typename SerializationFunc>
static void writeOutput(PrecompilationResult<ResultType>& output, const Optional<std::string>& outputFilePath,
    Result& result, const SerializationFunc& serializeResult,
    const std::function<bool(Result&, PrecompilationResult<ResultType>&)>& copyResult = nullptr)
{
    if(!result && !outputFilePath)
        throw CompilationError(CompilationStep::PRECOMPILATION, "No output for clang library compilation");
    if(outputFilePath && outputFilePath == output.getFilePath())
        return; // nothing to do
    else if(outputFilePath && !output)
        output = PrecompilationResult<ResultType>{*outputFilePath};
    else if(outputFilePath)
    {
        std::ifstream fis{*outputFilePath};
        output.inner().writeFrom(fis);
    }
    else if(copyResult && copyResult(result, output))
        return; // nothing further to do
    else
    {
        std::stringstream ss;
        if(!serializeResult(*result, ss))
            throw CompilationError(CompilationStep::PRECOMPILATION, "Failed to serialize clang library output");

        if(output)
            output.inner().writeFrom(ss);
        else
            output = PrecompilationResult<ResultType>{std::make_unique<RawCompilationData<ResultType>>(ss)};
    }
}

void precompilation::compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
    PrecompilationResult<SourceType::LLVM_IR_BIN>& output, PrecompilationConfig& config, LLVMPCHTag tag)
{
    clang::GeneratePCHAction action{};
    PROFILE_START(GeneratePCHAction);
    auto outputFilePath =
        runClangLibraryCompilation(command, inputData, config, action, clang::frontend::ActionKind::GeneratePCH);
    PROFILE_END(GeneratePCHAction);

    auto astUnit = action.takeCurrentASTUnit();
    writeOutput(output, outputFilePath, astUnit, [](clang::ASTUnit& ast, std::ostream& out) {
        llvm::raw_os_ostream wrapper(out);
        return ast.serialize(wrapper);
    });
}

void precompilation::compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
    PrecompilationResult<SourceType::LLVM_IR_BIN>& output, PrecompilationConfig& config, LLVMModuleTag tag)
{
    auto context = initializeLLVMContext();
    std::unique_ptr<clang::CodeGenAction> action = nullptr;
    clang::frontend::ActionKind kind;
    if(output.getFilePath())
    {
        // (additionally) writes a *.bc file
        action = std::make_unique<clang::EmitBCAction>(context.get());
        kind = clang::frontend::ActionKind::EmitBC;
    }
    else
    {
        // (only) creates a module in-memory
        action = std::make_unique<clang::EmitLLVMOnlyAction>(context.get());
        kind = clang::frontend::ActionKind::EmitLLVMOnly;
    }

    PROFILE_START(EmitBCAction);
    auto outputFilePath = runClangLibraryCompilation(command, inputData, config, *action, kind);
    PROFILE_END(EmitBCAction);

    auto module = action->takeModule();
    writeOutput<SourceType::LLVM_IR_BIN, std::unique_ptr<llvm::Module>>(
        output, outputFilePath, module,
        [](llvm::Module& module, std::ostream& out) {
            llvm::raw_os_ostream wrapper(out);
#if LLVM_LIBRARY_VERSION >= 70
            llvm::WriteBitcodeToFile(module, wrapper);
#else
            llvm::WriteBitcodeToFile(&module, wrapper);
#endif
            return true;
        },
        [&](std::unique_ptr<llvm::Module>& module, PrecompilationResult<SourceType::LLVM_IR_BIN>& output) {
            if(!output)
                output = PrecompilationResult<SourceType::LLVM_IR_BIN>{createLLVMCompilationData()};
            if(auto llvmData = dynamic_cast<LLVMCompilationData*>(&output.inner()))
            {
                llvmData->data.context = context;
                llvmData->data.module = std::move(module);
                return true;
            }
            return false;
        });
}

void precompilation::compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
    PrecompilationResult<SourceType::LLVM_IR_TEXT>& output, PrecompilationConfig& config, LLVMTextTag tag)
{
    std::unique_ptr<clang::CodeGenAction> action = nullptr;
    clang::frontend::ActionKind kind;
    if(output.getFilePath())
    {
        // (additionally) writes a *.ll file
        action = std::make_unique<clang::EmitLLVMAction>();
        kind = clang::frontend::ActionKind::EmitLLVM;
    }
    else
    {
        // (only) creates a module in-memory
        action = std::make_unique<clang::EmitLLVMOnlyAction>();
        kind = clang::frontend::ActionKind::EmitLLVMOnly;
    }

    PROFILE_START(EmitLLVMAction);
    auto outputFilePath = runClangLibraryCompilation(command, inputData, config, *action, kind);
    PROFILE_END(EmitLLVMAction);

    auto module = action->takeModule();
    writeOutput(output, outputFilePath, module, [](llvm::Module& module, std::ostream& out) {
        llvm::raw_os_ostream wrapper(out);
        module.print(wrapper, nullptr);
        return true;
    });
}

#else  /* USE_CLANG_LIBRARY */

using namespace vc4c;
using namespace vc4c::precompilation;

void precompilation::compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
    PrecompilationResult<SourceType::LLVM_IR_BIN>& output, PrecompilationConfig& config, LLVMPCHTag tag)
{
    throw CompilationError(
        CompilationStep::PRECOMPILATION, "Clang library precompilation is not enabled in this build");
}

void precompilation::compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
    PrecompilationResult<SourceType::LLVM_IR_BIN>& output, PrecompilationConfig& config, LLVMModuleTag tag)
{
    throw CompilationError(
        CompilationStep::PRECOMPILATION, "Clang library precompilation is not enabled in this build");
}

void precompilation::compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
    PrecompilationResult<SourceType::LLVM_IR_TEXT>& output, PrecompilationConfig& config, LLVMTextTag tag)
{
    throw CompilationError(
        CompilationStep::PRECOMPILATION, "Clang library precompilation is not enabled in this build");
}
#endif /* USE_CLANG_LIBRARY */
