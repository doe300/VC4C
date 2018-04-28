/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef IR_PARSER_H
#define IR_PARSER_H

#include "../Parser.h"
#include "../performance.h"
#include "LLVMInstruction.h"
#include "Scanner.h"

#include <iostream>
#include <memory>

namespace vc4c
{
    namespace llvm2qasm
    {
        struct LLVMMethod
        {
            explicit LLVMMethod(Module& module) : method(new Method(module)), module(&module) {}
            LLVMMethod(LLVMMethod&& old) = default;
            LLVMMethod& operator=(LLVMMethod&& old) = default;
            std::unique_ptr<Method> method;
            FastModificationList<std::unique_ptr<LLVMInstruction>> instructions;
            FastMap<MetaDataType, std::string> metaDataMapping;
            Module* module;
        };

        class IRParser final : public Parser
        {
        public:
            explicit IRParser(std::istream& stream = std::cin);
            explicit IRParser(Scanner& scanner);
            ~IRParser() override = default;

            void parse(Module& module) override;

        private:
            Scanner scanner;
            std::vector<LLVMMethod> methods;
            std::vector<std::string> kernelIDs;
            std::vector<std::string> kernelNames;
            FastMap<std::string, std::vector<std::string>> metaData;
            FastMap<std::string, DataType> complexTypes;

            Module* module;
            Method* currentMethod;

            DataType parseType();
            std::vector<std::pair<Value, ParameterDecorations>> parseParameters();
            Value toValue(const Token& token, const DataType& type = TYPE_UNKNOWN);
            Value parseValue(bool withType = true, const DataType& typeArg = TYPE_UNKNOWN);
            DataType parseStructDefinition();
            std::vector<Value> parseIndices();

            bool parseGlobalData();
            bool parseMethod();

            void parseMethodBody(LLVMMethod& method);

            void parseInstruction(
                LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions);
            void parseAssignment(LLVMMethod& method,
                FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions, const Token& dest);
            void parseMethodCall(
                LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions);
            void parseStore(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions);
            void parseBranch(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions);
            void parseReturn(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions);
            void parseLabel(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions,
                const Token& label);
            void parseSwitch(LLVMMethod& method, FastModificationList<std::unique_ptr<LLVMInstruction>>& instructions);

            void parseMetaData();
            void extractKernelInfo();
            IndexOf* parseGetElementPtr(LLVMMethod& method, const std::string& destination);

            void mapInstructions(LLVMMethod& method) const;
        };
    } // namespace llvm2qasm
} // namespace vc4c

#endif /* IR_PARSER_H */
