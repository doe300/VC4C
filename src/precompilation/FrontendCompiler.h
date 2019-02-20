/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_FRONTEND_COMPILER
#define VC4C_FRONTEND_COMPILER

#include "CompilationError.h"
#include "Optional.h"
#include "Precompiler.h"

#include <array>
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

namespace vc4c
{
    namespace precompilation
    {
        template <SourceType Type>
        struct PrecompilationResult : private NonCopyable
        {
            // XXX this is actually a variant between the members
            Optional<std::string> file;
            std::ostream* stream;

            explicit PrecompilationResult(std::ostream* s) : stream(s) {}
            explicit PrecompilationResult(const std::string& file) : file(file), stream(nullptr) {}
        };

        template <SourceType Type>
        struct PrecompilationSource : private NonCopyable
        {
            // XXX this is actually a variant between the members
            Optional<std::string> file;
            std::istream* stream;

            explicit PrecompilationSource(std::istream& s) : stream(&s) {}
            explicit PrecompilationSource(const std::string& file) : file(file), stream(nullptr) {}
            explicit PrecompilationSource(PrecompilationResult<Type>& res) :
                file(res.file), stream(dynamic_cast<std::istream*>(res.stream))
            {
                if(!file && !stream)
                    throw CompilationError(
                        CompilationStep::PRECOMPILATION, "Source has neither source file nor source stream!");
            }
        };

        template <SourceType OutType, SourceType InType>
        using PrecompilationStep =
            std::function<void(PrecompilationSource<InType>&&, const std::string&, PrecompilationResult<OutType>&)>;

        template <SourceType Type>
        using LinkStep = std::function<void(
            std::vector<PrecompilationSource<Type>>&&, const std::string&, PrecompilationResult<Type>&)>;

        template <SourceType OutType, SourceType InType, SourceType IntermediateType>
        constexpr PrecompilationStep<OutType, InType> chainSteps(
            const PrecompilationStep<IntermediateType, InType>& step1,
            const PrecompilationStep<OutType, IntermediateType>& step2)
        {
            return [step1, step2](PrecompilationSource<InType>&& in, const std::string& userOptions,
                       PrecompilationResult<OutType>& result) {
                TemporaryFile f;
                PrecompilationResult<IntermediateType> intermediateResult(f.fileName);
                step1(std::forward<PrecompilationSource<InType>>(in), userOptions, intermediateResult);
                return step2(PrecompilationSource<IntermediateType>(intermediateResult), userOptions, result);
            };
        }

        template <SourceType OutType, SourceType InType, typename FirstStep, typename... Steps>
        constexpr PrecompilationStep<OutType, InType> chainSteps(FirstStep step1, Steps... steps)
        {
            return chainSteps(step1, chainSteps(steps...));
        }

        using OpenCLSource = PrecompilationSource<SourceType::OPENCL_C>;
        using LLVMIRSource = PrecompilationSource<SourceType::LLVM_IR_BIN>;
        using LLVMIRTextSource = PrecompilationSource<SourceType::LLVM_IR_TEXT>;
        using SPIRVSource = PrecompilationSource<SourceType::SPIRV_BIN>;
        using SPIRVTextSource = PrecompilationSource<SourceType::SPIRV_TEXT>;
        using LLVMIRResult = PrecompilationResult<SourceType::LLVM_IR_BIN>;
        using LLVMIRTextResult = PrecompilationResult<SourceType::LLVM_IR_TEXT>;
        using SPIRVResult = PrecompilationResult<SourceType::SPIRV_BIN>;
        using SPIRVTextResult = PrecompilationResult<SourceType::SPIRV_TEXT>;

        void compileOpenCLWithPCH(OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result);
        void compileOpenCLWithDefaultHeader(
            OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result);
        void linkInStdlibModule(LLVMIRSource&& source, const std::string& userOptions, LLVMIRResult& result);
        void assembleLLVMIR(LLVMIRTextSource&& source, const std::string& userOptions, LLVMIRResult& result);
        void compileOpenCLToLLVMText(OpenCLSource&& source, const std::string& userOptions, LLVMIRTextResult& result);
        void compileLLVMToSPIRV(LLVMIRSource&& source, const std::string& userOptions, SPIRVResult& result);
        void assembleSPIRV(SPIRVTextSource&& source, const std::string& userOptions, SPIRVResult& result);
        void compileLLVMToSPIRVText(LLVMIRSource&& source, const std::string& userOptions, SPIRVTextResult& result);
        void disassembleSPIRV(SPIRVSource&& source, const std::string& userOptions, SPIRVTextResult& result);
        void linkLLVMModules(std::vector<LLVMIRSource>&& sources, const std::string& userOptions, LLVMIRResult& result);
        void linkSPIRVModules(std::vector<SPIRVSource>&& sources, const std::string& userOptions, SPIRVResult& result);
        void optimizeLLVMIR(LLVMIRSource&& source, const std::string& userOptions, LLVMIRResult& result);
        void optimizeLLVMText(LLVMIRTextSource&& source, const std::string& userOptions, LLVMIRTextResult& result);

        static const auto compileOpenCLAndLinkModule =
            chainSteps<SourceType::LLVM_IR_BIN, SourceType::OPENCL_C, SourceType::LLVM_IR_BIN>(
                compileOpenCLWithDefaultHeader, linkInStdlibModule);

        static const auto compileOpenCLToSPIRV =
            chainSteps<SourceType::SPIRV_BIN, SourceType::OPENCL_C, SourceType::LLVM_IR_BIN>(
                compileOpenCLWithPCH, compileLLVMToSPIRV);

        static const auto compileOpenCLToSPIRVText =
            chainSteps<SourceType::SPIRV_TEXT, SourceType::OPENCL_C, SourceType::LLVM_IR_BIN>(
                compileOpenCLWithPCH, compileLLVMToSPIRVText);

        /*
         * General version of compiling OpenCL C source to to LLVM binary module with the standard-library included.
         * Depending on the compilation options, the standard-library PCH is included or the standard-library module is
         * linked in.
         */
        void compileOpenCLToLLVMIR(OpenCLSource&& source, const std::string& userOptions, LLVMIRResult& result);
    } /* namespace precompilation */
} /* namespace vc4c */

#endif /* VC4C_FRONTEND_COMPILER */
