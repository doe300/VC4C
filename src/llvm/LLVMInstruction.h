/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LLVMINSTRUCTION_H
#define LLVMINSTRUCTION_H

#include "../Locals.h"

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace vc4c
{
    class Method;

    namespace intermediate
    {
        enum class InstructionDecorations;
    } // namespace intermediate

    namespace llvm2qasm
    {
        /*!
         * http://llvm.org/docs/LangRef.html
         */
        class LLVMInstruction
        {
        public:
            explicit LLVMInstruction();
            virtual ~LLVMInstruction();

            virtual const Local* getDeclaredLocal() const;
            virtual std::vector<const Local*> getAllLocals() const;

            virtual bool mapInstruction(Method& method) const = 0;

            LLVMInstruction* setDecorations(intermediate::InstructionDecorations decorations);

        protected:
            intermediate::InstructionDecorations decorations;
        };

        class CallSite final : public LLVMInstruction
        {
        public:
            CallSite(Value&& dest, std::string&& methodName, std::vector<Value>&& args = {});
            CallSite(Value&& dest, const Method& method, std::vector<Value>&& args = {});
            CallSite(std::string&& methodName, DataType&& returnType, std::vector<Value>&& args = {});
            explicit CallSite(const Method& method, std::vector<Value>&& args = {});
            ~CallSite() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

            const std::vector<Value>& getArguments() const;

            const std::string& getMethodName() const;

        private:
            const Value dest;
            const std::string methodName;
            const std::vector<Value> arguments;
        };

        class Copy final : public LLVMInstruction
        {
        public:
            Copy(Value&& dest, Value&& orig, bool isLoadStore = false, bool isRead = false, bool isBitcast = false);
            ~Copy() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const Value orig;
            const bool isLoadStore;
            const bool isRead;
            const bool isBitcast;
        };

        class UnaryOperator : public LLVMInstruction
        {
        public:
            UnaryOperator(std::string&& opCode, Value&& dest, Value&& arg);
            ~UnaryOperator() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        protected:
            const Value dest;
            const std::string opCode;
            const Value arg;
        };

        class BinaryOperator final : public UnaryOperator
        {
        public:
            BinaryOperator(std::string&& opCode, Value&& dest, Value&& arg0, Value&& arg1);
            ~BinaryOperator() override = default;

            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value arg2;
        };

        class IndexOf final : public LLVMInstruction
        {
        public:
            IndexOf(Value&& dest, Value&& container, std::vector<Value>&& indices);
            ~IndexOf() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

            const Value getContainer() const;

        private:
            const Value dest;
            const Value container;
            const std::vector<Value> indices;
        };

        class Comparison final : public LLVMInstruction
        {
        public:
            Comparison(Value&& dest, std::string&& comp, Value&& op1, Value&& op2, bool isFloat);
            ~Comparison() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const std::string comp;
            const bool isFloat;
            const Value op1;
            const Value op2;
        };

        class ContainerInsertion final : public LLVMInstruction
        {
        public:
            ContainerInsertion(Value&& dest, Value&& container, Value&& newValue, Value&& index);
            ~ContainerInsertion() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const Value container;
            const Value newValue;
            const Value index;
        };

        class ContainerExtraction final : public LLVMInstruction
        {
        public:
            ContainerExtraction(Value&& dest, Value&& container, Value&& index);
            ~ContainerExtraction() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const Value container;
            const Value index;
        };

        class ValueReturn final : public LLVMInstruction
        {
        public:
            explicit ValueReturn();
            explicit ValueReturn(Value&& val);
            ~ValueReturn() override = default;

            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const bool hasValue;
            const Value val;
        };

        class ShuffleVector final : public LLVMInstruction
        {
        public:
            ShuffleVector(Value&& dest, Value&& v1, Value&& v2, Value&& mask);
            ~ShuffleVector() override = default;

            std::vector<const Local*> getAllLocals() const override;
            const Local* getDeclaredLocal() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const Value v1;
            const Value v2;
            const Value mask;
        };

        class LLVMLabel final : public LLVMInstruction
        {
        public:
            explicit LLVMLabel(Value&& label);
            ~LLVMLabel() override = default;

            bool mapInstruction(Method& method) const override;

        private:
            const Value label;
        };

        class PhiNode final : public LLVMInstruction
        {
        public:
            PhiNode(Value&& dest, std::vector<std::pair<Value, const Local*>>&& labels);
            ~PhiNode() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const std::vector<std::pair<Value, const Local*>> labels;
        };

        class Selection final : public LLVMInstruction
        {
        public:
            Selection(Value&& dest, Value&& cond, Value&& opt1, Value&& opt2);
            ~Selection() override = default;

            const Local* getDeclaredLocal() const override;
            std::vector<const Local*> getAllLocals() const override;

            bool mapInstruction(Method& method) const override;

        private:
            const Value dest;
            const Value cond;
            const Value opt1;
            const Value opt2;
        };

        class Branch final : public LLVMInstruction
        {
        public:
            explicit Branch(Value&& label);
            Branch(Value&& cond, Value&& thenLabel, Value&& elseLabel);
            ~Branch() override = default;

            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value thenLabel;
            const Value elseLabel;
            const Value cond;
        };

        class Switch final : public LLVMInstruction
        {
        public:
            Switch(Value&& cond, Value&& defaultLabel, FastMap<int, Value>&& cases);
            ~Switch() override = default;

            std::vector<const Local*> getAllLocals() const override;
            bool mapInstruction(Method& method) const override;

        private:
            const Value cond;
            const Value defaultLabel;
            const FastMap<int, Value> jumpLabels;
        };
    } // namespace llvm2qasm
} // namespace vc4c
#endif /* LLVMINSTRUCTION_H */
