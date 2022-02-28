/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LLVM_LIBRARY
#define VC4C_LLVM_LIBRARY

#include "../Optional.h"
#include "CompilationData.h"

#include <chrono>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace llvm
{
    class MemoryBuffer;
    class LLVMContext;
} // namespace llvm

namespace vc4c
{
    namespace precompilation
    {
        template <SourceType Type>
        class PrecompilationResult;

        struct PrecompilationConfig;

        struct LLVMModuleWithContext
        {
            // NOTE: the reference to the context needs to be stable
            std::shared_ptr<llvm::LLVMContext> context;
            std::shared_ptr<llvm::Module> module;

            void dumpText(const std::string& fileName = "/tmp/vc4c-llvm-module.ll") const;
        };

        std::shared_ptr<llvm::LLVMContext> initializeLLVMContext();

        struct LLVMPCHTag
        {
            static constexpr auto argument = "-emit-pch";
            static constexpr auto TYPE = SourceType::LLVM_IR_BIN; // TODO not actually true
        };

        struct LLVMModuleTag
        {
            static constexpr auto argument = "-emit-llvm-bc";
            static constexpr auto TYPE = SourceType::LLVM_IR_BIN;
        };

        struct LLVMTextTag
        {
            static constexpr auto argument = "-emit-llvm";
            static constexpr auto TYPE = SourceType::LLVM_IR_TEXT;
        };

        // NOTE: Requires clang library build option, implemented in ClangLibrary.cpp
        void compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
            PrecompilationResult<SourceType::LLVM_IR_BIN>& output, PrecompilationConfig& config, LLVMPCHTag tag);
        void compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
            PrecompilationResult<SourceType::LLVM_IR_BIN>& output, PrecompilationConfig& config, LLVMModuleTag tag);
        void compileClangLibrary(const std::vector<std::string>& command, const OpenCLData& inputData,
            PrecompilationResult<SourceType::LLVM_IR_TEXT>& output, PrecompilationConfig& config, LLVMTextTag tag);

        std::unique_ptr<llvm::MemoryBuffer> loadLLVMBuffer(const CompilationDataPrivate& data);
        LLVMModuleWithContext loadLLVMModule(
            const llvm::MemoryBuffer& buffer, const std::shared_ptr<llvm::LLVMContext>& context, LLVMModuleTag tag);
        LLVMModuleWithContext loadLLVMModule(
            const llvm::MemoryBuffer& buffer, const std::shared_ptr<llvm::LLVMContext>& context, LLVMTextTag tag);
        LLVMModuleWithContext loadLLVMModule(const LLVMIRData& data, const std::shared_ptr<llvm::LLVMContext>& context);
        LLVMModuleWithContext loadLLVMModule(
            const LLVMIRTextData& data, const std::shared_ptr<llvm::LLVMContext>& context);

        void disassembleLLVMLibrary(const LLVMIRData& input, LLVMIRTextData& output);
        void assembleLLVMLibrary(const LLVMIRTextData& input, LLVMIRData& output);

        struct LLVMCompilationData : public LLVMIRData
        {
            explicit LLVMCompilationData(LLVMModuleWithContext&& data);
            ~LLVMCompilationData() override;

            Optional<std::string> getFilePath() const override;
            std::string to_string() const override;
            void readInto(std::ostream& out) const override;
            void writeFrom(std::istream& in) override;

            LLVMModuleWithContext data;
        };

        std::unique_ptr<LLVMCompilationData> createLLVMCompilationData();

        /**
         * RAII wrapper around LLVM's internal profiling, allowing for easier usage.
         */
        struct LLVMProfilerWrapper
        {
            explicit LLVMProfilerWrapper(const std::string& command = CLANG_TOOL.defaultPath,
                std::chrono::microseconds granularity = std::chrono::microseconds{500});
            ~LLVMProfilerWrapper();
        };
    } // namespace precompilation
} // namespace vc4c

#endif /* VC4C_LLVM_LIBRARY */
