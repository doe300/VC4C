/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_BITCODEREADER_H
#define VC4C_BITCODEREADER_H

#include "LLVMInstruction.h"
#include "../Parser.h"
#include "Precompiler.h"

#include <iostream>
#include <memory>

#ifdef LLVM_INCLUDE_PATH
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#endif

namespace llvm
{
	class Function;
	class Instruction;
	class Value;
} /* namespace llvm */

namespace vc4c
{
	namespace llvm2qasm
	{
		using LLVMInstructionList = FastModificationList<std::unique_ptr<LLVMInstruction>>;

#ifdef LLVM_INCLUDE_PATH
		class BitcodeReader: public Parser
		{
		public:
			explicit BitcodeReader(std::istream& stream, SourceType sourceType);
			~BitcodeReader() override = default;

			void parse(Module& module);
		private:
			//"the lifetime of the LLVMContext needs to outlast the module"
			llvm::LLVMContext context;
			std::unique_ptr<llvm::Module> llvmModule;
			FastMap<const llvm::Function*, std::pair<Method*, LLVMInstructionList>> parsedFunctions;
			//BasicBlocks in LLVM have no name
			FastMap<const llvm::Value*, Local*> labelMap;

			Method& parseFunction(Module& module, const llvm::Function& func);
			void parseFunctionBody(Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Function& func);
			void parseInstruction(Module& module, Method& method, LLVMInstructionList& instructions, const llvm::Instruction& inst);

			Value toValue(Method& method, const llvm::Value* val);
		};
#endif

	} /* namespace llvm2qasm */
} /* namespace vc4c */

#endif /* VC4C_BITCODEREADER_H */
