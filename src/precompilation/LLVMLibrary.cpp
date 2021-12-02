/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LLVMLibrary.h"

#include "../Profiler.h"
#include "CompilationError.h"
#include "Precompiler.h"
#include "log.h"
#include "tool_paths.h"

#ifdef USE_LLVM_LIBRARY
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_os_ostream.h"

#include <memory>
#include <sstream>

using namespace vc4c;
using namespace vc4c::precompilation;

std::shared_ptr<llvm::LLVMContext> precompilation::initializeLLVMContext()
{
    auto context = std::make_shared<llvm::LLVMContext>();
    context->setDiagnosticHandlerCallBack([](const llvm::DiagnosticInfo& info, void* /* dummy */) {
        std::stringstream ss;
        llvm::raw_os_ostream os(ss);
        llvm::DiagnosticPrinterRawOStream dp(os);
        info.print(dp);

        switch(info.getSeverity())
        {
        case llvm::DiagnosticSeverity::DS_Warning:
            logging::warn() << "Warning in LLVM/clang: " << ss.str() << logging::endl;
            break;
        case llvm::DiagnosticSeverity::DS_Error:
            logging::warn() << "Error in LLVM/clang: " << ss.str() << logging::endl;
            break;
        default:
            logging::warn() << "Note/Remark in LLVM/clang: " << ss.str() << logging::endl;
        }
    });
    return context;
}

std::unique_ptr<llvm::MemoryBuffer> precompilation::loadLLVMBuffer(std::istream& stream)
{
    std::string buffer;
    // required, since LLVM cannot read from std::istreams
    if(auto sstream = dynamic_cast<std::istringstream*>(&stream))
        buffer = sstream->str();
    else if(auto sstream = dynamic_cast<std::stringstream*>(&stream))
        buffer = sstream->str();
    else
    {
        std::stringstream ss;
        ss << stream.rdbuf();
        buffer = ss.str();
    }
    return std::unique_ptr<llvm::MemoryBuffer>(llvm::MemoryBuffer::getMemBufferCopy(llvm::StringRef(buffer)));
}

LLVMModuleWithContext precompilation::loadLLVMModule(
    llvm::MemoryBuffer& buffer, SourceType sourceType, const std::shared_ptr<llvm::LLVMContext>& context)
{
    auto actualContext = context ? context : initializeLLVMContext();
    if(sourceType == SourceType::LLVM_IR_BIN)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading LLVM module from bit-code...");
        auto expected = llvm::parseBitcodeFile(buffer.getMemBufferRef(), *actualContext);
        if(!expected)
        {
#if LLVM_LIBRARY_VERSION >= 40
            std::string error = "";
            llvm::handleAllErrors(
                expected.takeError(), [&error](const llvm::ErrorInfoBase& base) { error = base.message(); });
            throw std::runtime_error("Error parsing LLVM module" + error);
#else
            throw std::system_error(expected.getError(), "Error parsing LLVM module");
#endif
        }
        else
        {
            auto module = std::unique_ptr<llvm::Module>(std::move(expected.get()));
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << " " << (module->getName().empty() ? "(unknown)" : module->getName().str()) << logging::endl);
            // expected.get() is either std::unique_ptr<llvm::Module> or llvm::Module*
            return {actualContext, std::move(module)};
        }
    }
    else if(sourceType == SourceType::LLVM_IR_TEXT)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading LLVM module from IR...");
        llvm::SMDiagnostic error;
        auto module = llvm::parseIR(buffer.getMemBufferRef(), error, *actualContext);
        if(!module)
            throw CompilationError(
                CompilationStep::PARSER, "Error parsing LLVM IR module", static_cast<std::string>(error.getMessage()));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << " " << (module->getName().empty() ? "(unknown)" : module->getName().str()) << logging::endl);
        return {actualContext, std::move(module)};
    }
    else
        throw CompilationError(CompilationStep::PARSER, "Unhandled source-type for reading LLVM bitcode",
            std::to_string(static_cast<unsigned>(sourceType)));
}

LLVMModuleWithContext precompilation::loadLLVMModule(
    TypedCompilationData<SourceType::LLVM_IR_BIN>&& data, const std::shared_ptr<llvm::LLVMContext>& context)
{
    if(auto llvmData = dynamic_cast<LLVMCompilationData*>(&data))
        return std::move(llvmData->data);

    if(auto stream = data.readStream())
    {
        auto buffer = loadLLVMBuffer(*stream);
        return loadLLVMModule(*buffer, data.getType(), context);
    }
    throw CompilationError(CompilationStep::PARSER, "Unhandled input data type for reading LLVM bitcode");
}

void precompilation::linkLLVMLibrary(
    std::vector<std::unique_ptr<TypedCompilationData<SourceType::LLVM_IR_BIN>>>&& sources,
    const std::string& userOptions, TypedCompilationData<SourceType::LLVM_IR_BIN>& output)
{
    unsigned flags = llvm::Linker::Flags::None;
    if(userOptions.find("-only-needed") != std::string::npos)
        flags |= llvm::Linker::LinkOnlyNeeded;
    if(userOptions.find("-override") != std::string::npos)
        flags |= llvm::Linker::OverrideFromSrc;

    // TODO take context from one of the inputs, if available?
    auto context = initializeLLVMContext();
    auto module = std::make_unique<llvm::Module>("link", *context);
    llvm::Linker linker(*module);

    for(auto& source : sources)
    {
        if(!linker.linkInModule(loadLLVMModule(std::move(*source), context).module, flags))
            // FIXME fails here and does not write any logging/diagnostics
            throw CompilationError(CompilationStep::LINKER, "Failed to link LLVM modules");
    }
    storeLLVMModule(std::move(module), context, output);
}

void precompilation::storeLLVMModule(std::unique_ptr<llvm::Module>&& module,
    const std::shared_ptr<llvm::LLVMContext>& context, TypedCompilationData<SourceType::LLVM_IR_BIN>& data)
{
    if(auto llvmData = dynamic_cast<LLVMCompilationData*>(&data))
    {
        llvmData->data = {context, std::move(module)};
    }
    else if(auto stream = data.writeStream())
    {
        llvm::raw_os_ostream out(*stream);
        llvm::WriteBitcodeToFile(*module, out);
    }
    else
        throw CompilationError(CompilationStep::PARSER, "Unhandled output data type for writing LLVM bitcode");
}

#endif /* USE_LLVM_LIBRARY */
