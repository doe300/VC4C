/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LLVMLibrary.h"

#include "../Profiler.h"
#include "../helper.h"
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
#include "llvm/Support/TimeProfiler.h"
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
        auto text = trim(ss.str());
        if(text.empty())
            return;

        switch(info.getSeverity())
        {
        case llvm::DiagnosticSeverity::DS_Warning:
            logging::warn() << "Warning in LLVM/clang: " << text << logging::endl;
            break;
        case llvm::DiagnosticSeverity::DS_Error:
            logging::error() << "Error in LLVM/clang: " << text << logging::endl;
            break;
        default:
            logging::info() << "Note/Remark in LLVM/clang: " << text << logging::endl;
        }
    });
#if DEBUG_MODE
    context->setDiscardValueNames(false);
#else
    context->setDiscardValueNames(true);
#endif
    return context;
}

static std::unique_ptr<llvm::MemoryBuffer> loadLLVMBuffer(std::istream& stream)
{
    // required, since LLVM cannot read from std::istreams
    std::string buffer = readIntoString(stream);
    return std::unique_ptr<llvm::MemoryBuffer>(llvm::MemoryBuffer::getMemBufferCopy(llvm::StringRef(buffer)));
}

std::unique_ptr<llvm::MemoryBuffer> precompilation::loadLLVMBuffer(const CompilationDataPrivate& data)
{
    if(auto rawData = data.getRawData())
    {
        auto buffer = llvm::MemoryBuffer::getMemBufferCopy(
            llvm::StringRef(reinterpret_cast<const char*>(rawData->data()), rawData->size()), "");
        if(!buffer)
            throw CompilationError(CompilationStep::PRECOMPILATION,
                "Failed to read in-memory buffer into LLVM MemoryBuffer", data.to_string());
        return buffer;
    }
    if(auto filePath = data.getFilePath())
    {
        auto buffer = llvm::MemoryBuffer::getFile(*filePath);
        if(!buffer)
            throw std::system_error(buffer.getError(), "Failed to read file into LLVM MemoryBuffer");
        return std::move(buffer.get());
    }
    std::stringstream ss;
    data.readInto(ss);
    return ::loadLLVMBuffer(ss);
}

LLVMModuleWithContext precompilation::loadLLVMModule(
    const llvm::MemoryBuffer& buffer, const std::shared_ptr<llvm::LLVMContext>& context, LLVMModuleTag tag)
{
    auto actualContext = context ? context : initializeLLVMContext();
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

LLVMModuleWithContext precompilation::loadLLVMModule(
    const llvm::MemoryBuffer& buffer, const std::shared_ptr<llvm::LLVMContext>& context, LLVMTextTag tag)
{
    auto actualContext = context ? context : initializeLLVMContext();
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading LLVM module from IR...");
    llvm::SMDiagnostic error;
    auto module = llvm::parseIR(buffer.getMemBufferRef(), error, *actualContext);
    if(!module)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Error parsing LLVM IR module",
            static_cast<std::string>(error.getMessage()));
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << " " << (module->getName().empty() ? "(unknown)" : module->getName().str()) << logging::endl);
    return {actualContext, std::move(module)};
}

template <typename TagType>
static LLVMModuleWithContext loadLLVMModuleDispatch(
    const TypedCompilationData<TagType::TYPE>& data, const std::shared_ptr<llvm::LLVMContext>& context)
{
    if(auto llvmData = dynamic_cast<const LLVMCompilationData*>(&data))
        return llvmData->data;
    if(auto buffer = loadLLVMBuffer(data))
        return loadLLVMModule(*buffer, context, TagType{});
    throw CompilationError(CompilationStep::PRECOMPILATION, "Unhandled input data type for reading LLVM bitcode");
}

LLVMModuleWithContext precompilation::loadLLVMModule(
    const LLVMIRData& data, const std::shared_ptr<llvm::LLVMContext>& context)
{
    return loadLLVMModuleDispatch<LLVMModuleTag>(data, context);
}

LLVMModuleWithContext precompilation::loadLLVMModule(
    const LLVMIRTextData& data, const std::shared_ptr<llvm::LLVMContext>& context)
{
    return loadLLVMModuleDispatch<LLVMTextTag>(data, context);
}

void precompilation::storeLLVMModule(
    std::unique_ptr<llvm::Module>&& module, const std::shared_ptr<llvm::LLVMContext>& context, LLVMIRData& data)
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
        throw CompilationError(CompilationStep::PRECOMPILATION, "Unhandled output data type for writing LLVM bitcode");
}

LLVMCompilationData::LLVMCompilationData(LLVMModuleWithContext&& data) : data(std::move(data)) {}
LLVMCompilationData::~LLVMCompilationData() = default;

Optional<std::string> LLVMCompilationData::getFilePath() const
{
    // TODO LLVM modules can be in-memory and on-disk, so return on-disk path??
    return {};
}

std::string LLVMCompilationData::to_string() const
{
    if(!data.module)
        return "(empty LLVM module)";
    auto name = data.module->getModuleIdentifier();
    return name.empty() ? name : "(LLVM module)";
}

void LLVMCompilationData::readInto(std::ostream& out) const
{
    if(!data.module)
        throw CompilationError(CompilationStep::PRECOMPILATION, "Cannot serialize NULL LLVM module");
    llvm::raw_os_ostream stream(out);
    llvm::WriteBitcodeToFile(*data.module, stream);
}

void LLVMCompilationData::writeFrom(std::istream& in)
{
    auto buffer = loadLLVMBuffer(in);
    data = loadLLVMModule(*buffer, data.context, LLVMModuleTag{});
}

std::unique_ptr<LLVMCompilationData> precompilation::createLLVMCompilationData()
{
    // Implement in source file, since std::unique_ptr constructor needs to fully know types and the LLVM types are only
    // forward-declared in the header but fully defined here
    return std::make_unique<LLVMCompilationData>(LLVMModuleWithContext{});
}

LLVMProfilerWrapper::LLVMProfilerWrapper(const std::string& command, std::chrono::microseconds granularity)
{
    // FIXME crashes after clang finished (and printed this), works for llvm library linking...
    // CPPLOG_LAZY_BLOCK(logging::Level::DEBUG,
    //     { llvm::timeTraceProfilerInitialize(static_cast<unsigned>(granularity.count()), command); });
}

LLVMProfilerWrapper::~LLVMProfilerWrapper()
{
    if(llvm::timeTraceProfilerEnabled())
    {
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            llvm::SmallString<16> string{};
            llvm::raw_svector_ostream os{string};
            llvm::timeTraceProfilerWrite(os);
            logging::debug() << "LLVM trace profile result: " << (string.empty() ? "(null)" : string.data())
                             << logging::endl;
        });
        llvm::timeTraceProfilerCleanup();
    }
}
#endif /* USE_LLVM_LIBRARY */
