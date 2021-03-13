/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LLVMINSTRUCTION_H
#define LLVMINSTRUCTION_H

#include "../Values.h"

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
        enum class InstructionDecorations : uint32_t;
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
            virtual ~LLVMInstruction() noexcept = 0;

            virtual bool mapInstruction(Method& method) = 0;

            LLVMInstruction* setDecorations(intermediate::InstructionDecorations decorations);

        protected:
            intermediate::InstructionDecorations decorations;
        };

        class CallSite final : public LLVMInstruction
        {
        public:
            CallSite(Value&& dest, std::string&& methodName, std::vector<Value>&& args = {});
            CallSite(Value&& dest, const Method& method, std::vector<Value>&& args, bool isVarArg);
            ~CallSite() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            std::string methodName;
            std::vector<Value> arguments;
        };

        class Copy final : public LLVMInstruction
        {
        public:
            Copy(Value&& dest, Value&& orig, bool isLoadStore = false, bool isRead = false, bool isBitcast = false);
            ~Copy() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            Value orig;
            bool isLoadStore;
            bool isRead;
            bool isBitcast;
        };

        class UnaryOperator : public LLVMInstruction
        {
        public:
            UnaryOperator(std::string&& opCode, Value&& dest, Value&& arg);
            ~UnaryOperator() noexcept override = default;

            bool mapInstruction(Method& method) override;

        protected:
            Value dest;
            std::string opCode;
            Value arg;
        };

        class BinaryOperator final : public UnaryOperator
        {
        public:
            BinaryOperator(std::string&& opCode, Value&& dest, Value&& arg0, Value&& arg1);
            ~BinaryOperator() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value arg2;
        };

        class IndexOf final : public LLVMInstruction
        {
        public:
            IndexOf(Value&& dest, Value&& container, std::vector<Value>&& indices);
            ~IndexOf() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            Value container;
            std::vector<Value> indices;
        };

        class Comparison final : public LLVMInstruction
        {
        public:
            Comparison(Value&& dest, std::string&& comp, Value&& op1, Value&& op2);
            ~Comparison() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            std::string comp;
            Value op1;
            Value op2;
        };

        class ContainerInsertion final : public LLVMInstruction
        {
        public:
            ContainerInsertion(Value&& dest, Value&& container, Value&& newValue, Value&& index);
            ~ContainerInsertion() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            Value container;
            Value newValue;
            Value index;
        };

        class ContainerExtraction final : public LLVMInstruction
        {
        public:
            ContainerExtraction(Value&& dest, Value&& container, Value&& index);
            ~ContainerExtraction() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            Value container;
            Value index;
        };

        class ValueReturn final : public LLVMInstruction
        {
        public:
            explicit ValueReturn();
            explicit ValueReturn(Value&& val);
            ~ValueReturn() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            bool hasValue;
            Value val;
        };

        class ShuffleVector final : public LLVMInstruction
        {
        public:
            ShuffleVector(Value&& dest, Value&& v1, Value&& v2, Value&& mask);
            ~ShuffleVector() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            Value v1;
            Value v2;
            Value mask;
        };

        class LLVMLabel final : public LLVMInstruction
        {
        public:
            explicit LLVMLabel(Value&& label);
            ~LLVMLabel() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value label;
        };

        class PhiNode final : public LLVMInstruction
        {
        public:
            PhiNode(Value&& dest, std::vector<std::pair<Value, const Local*>>&& labels);
            ~PhiNode() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            std::vector<std::pair<Value, const Local*>> labels;
        };

        class Selection final : public LLVMInstruction
        {
        public:
            Selection(Value&& dest, Value&& cond, Value&& opt1, Value&& opt2);
            ~Selection() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            Value cond;
            Value opt1;
            Value opt2;
        };

        class Branch final : public LLVMInstruction
        {
        public:
            explicit Branch(Value&& label);
            Branch(Value&& cond, Value&& thenLabel, Value&& elseLabel);
            ~Branch() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value thenLabel;
            Value elseLabel;
            Value cond;
        };

        class Switch final : public LLVMInstruction
        {
        public:
            Switch(Value&& cond, Value&& defaultLabel, FastMap<uint64_t, Value>&& cases);
            ~Switch() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value cond;
            Value defaultLabel;
            FastMap<uint64_t, Value> jumpLabels;
        };

        class LongConstant final : public LLVMInstruction
        {
        public:
            // Scalar constant
            LongConstant(Value&& dest, int64_t constant);
            // Vector constant
            LongConstant(Value&& dest, std::vector<Value>&& elements);
            ~LongConstant() noexcept override = default;

            bool mapInstruction(Method& method) override;

        private:
            Value dest;
            int64_t value;
            std::vector<Value> elements;
        };
    } // namespace llvm2qasm
} // namespace vc4c
#endif /* LLVMINSTRUCTION_H */
