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
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"
#if LLVM_LIBRARY_VERSION >= 90
#include "llvm/Support/TimeProfiler.h"
#endif

#include <fstream>
#include <memory>
#include <sstream>

using namespace vc4c;
using namespace vc4c::precompilation;

LCOV_EXCL_START
void LLVMModuleWithContext::dumpText(const std::string& fileName) const
{
    if(module)
    {
        std::ofstream fos{fileName};
        llvm::raw_os_ostream out(fos);
        module->print(out, nullptr);
    }
}
LCOV_EXCL_STOP

std::shared_ptr<llvm::LLVMContext> precompilation::initializeLLVMContext()
{
    auto context = std::make_shared<llvm::LLVMContext>();
    context->setDiagnosticHandlerCallBack([](const llvm::DiagnosticInfo& info, void* /* dummy */) {
        LCOV_EXCL_START
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
        LCOV_EXCL_STOP
    });
#ifndef NDEBUG
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
        LCOV_EXCL_START
#if LLVM_LIBRARY_VERSION >= 40
        std::string error = "";
        llvm::handleAllErrors(
            expected.takeError(), [&error](const llvm::ErrorInfoBase& base) { error = base.message(); });
        throw std::runtime_error("Error parsing LLVM module" + error);
#else
        throw std::system_error(expected.getError(), "Error parsing LLVM module");
#endif
        LCOV_EXCL_STOP
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

static void storeLLVMModule(
    const std::shared_ptr<llvm::Module>& module, const std::shared_ptr<llvm::LLVMContext>& context, LLVMIRData& data)
{
    if(auto llvmData = dynamic_cast<LLVMCompilationData*>(&data))
    {
        llvmData->data = {context, std::move(module)};
    }
    else if(auto stream = data.writeStream())
    {
        llvm::raw_os_ostream out(*stream);
#if LLVM_LIBRARY_VERSION >= 70
        llvm::WriteBitcodeToFile(*module, out);
#else
        llvm::WriteBitcodeToFile(module.get(), out);
#endif
    }
    else
        throw CompilationError(
            CompilationStep::PRECOMPILATION, "Unhandled output data type for writing LLVM bitcode", data.to_string());
}

void precompilation::disassembleLLVMLibrary(const LLVMIRData& input, LLVMIRTextData& output)
{
    auto module = loadLLVMModule(input, nullptr);
    if(auto stream = output.writeStream())
    {
        llvm::raw_os_ostream out(*stream);
        module.module->print(out, nullptr);
    }
    else
        throw CompilationError(
            CompilationStep::PRECOMPILATION, "Unhandled output data type for writing LLVM IR", output.to_string());
}

void precompilation::assembleLLVMLibrary(const LLVMIRTextData& input, LLVMIRData& output)
{
    auto module = loadLLVMModule(input, nullptr);
    storeLLVMModule(module.module, module.context, output);
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
#if LLVM_LIBRARY_VERSION >= 70
    llvm::WriteBitcodeToFile(*data.module, stream);
#else
    llvm::WriteBitcodeToFile(data.module.get(), stream);
#endif
}

void LLVMCompilationData::writeFrom(std::istream& in)
{
    auto buffer = ::loadLLVMBuffer(in);
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
#if LLVM_LIBRARY_VERSION >= 90
// FIXME crashes after clang finished (and printed this), works for llvm library linking...
// CPPLOG_LAZY_BLOCK(logging::Level::DEBUG,
//     { llvm::timeTraceProfilerInitialize(static_cast<unsigned>(granularity.count()), command); });
#endif
}

LLVMProfilerWrapper::~LLVMProfilerWrapper()
{
#if LLVM_LIBRARY_VERSION >= 90
    LCOV_EXCL_START
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
    LCOV_EXCL_STOP
#endif
}
#endif /* USE_LLVM_LIBRARY */
