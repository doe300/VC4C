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
        using LLVMInstructionList = FastModificationList<std::unique_ptr<LLVMInstruction>>;

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

            DataType toDataType(const llvm::Type* type);
            Value toValue(Method& method, const llvm::Value* val);
            Value toConstant(Module& module, const llvm::Value* val);
            Value precalculateConstantExpression(Module& module, const llvm::ConstantExpr* expr);
        };
#endif

    } /* namespace llvm2qasm */
} /* namespace vc4c */

#endif /* VC4C_BITCODEREADER_H */
