/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_BITCODEREADER_H
#define VC4C_BITCODEREADER_H

#include "../Parser.h"
#include "LLVMInstruction.h"
#include "Precompiler.h"
#include "tool_paths.h"

#include <iostream>
#include <memory>

#ifdef USE_LLVM_LIBRARY
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#endif

namespace llvm
{
    class Function;
    class Instruction;
    class Value;
    class Type;
    class ConstantExpr;
} /* namespace llvm */

namespace vc4c
{
    namespace llvm2qasm
    {
        // This list is only ever appended to the end and the size can be pre-calculated, so use a vector
        using LLVMInstructionList = FastAccessList<std::unique_ptr<LLVMInstruction>>;

#ifdef USE_LLVM_LIBRARY
        class BitcodeReader final : public Parser
        {
        public:
            explicit BitcodeReader(std::istream& stream, SourceType sourceType);
            ~BitcodeReader() override = default;

            void parse(Module& module) override;

        private:
            //"the lifetime of the LLVMContext needs to outlast the module"
            llvm::LLVMContext context;
            std::unique_ptr<llvm::Module> llvmModule;
            FastMap<const llvm::Function*, std::pair<Method*, LLVMInstructionList>> parsedFunctions;
            FastMap<const llvm::Value*, const Local*> localMap;
            // required to support recursive types
            FastMap<const llvm::Type*, DataType> typesMap;

            Method& parseFunction(Module& module, const llvm::Function& func);
            void parseFunctionBody(
                Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Function& func);
            void parseInstruction(
                Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Instruction& inst);

            DataType toDataType(
                Module& module, const llvm::Type* type, Optional<AddressSpace> overrideAddressSpace = {});
            Value parseInlineGetElementPtr(
                Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Value* pointerOperand);
            Value toValue(Method& method, const llvm::Value* val, LLVMInstructionList* instructions);
            Value toConstant(Module& module, const llvm::Value* val, Method* method = nullptr,
                LLVMInstructionList* instructions = nullptr);
            Value precalculateConstantExpression(
                Module& module, const llvm::ConstantExpr* expr, Method* method, LLVMInstructionList* instructions);

            CompoundConstant toConstantGlobal(Module& module, const llvm::Value* val);

            void extractKernelMetadata(Module& module, Method& kernel, const llvm::Function& func);
            void extractSPIRMetadata(
                Module& module, Method& kernel, const llvm::Function& func, const llvm::Module& llvmModule);
        };
#endif

    } /* namespace llvm2qasm */
} /* namespace vc4c */

#endif /* VC4C_BITCODEREADER_H */
