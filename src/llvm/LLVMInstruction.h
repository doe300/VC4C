/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LLVMINSTRUCTION_H
#define LLVMINSTRUCTION_H

#include "../Locals.h"
#include "Token.h"

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
		ValueType toValueType(TokenType type);

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

		class CallSite: public LLVMInstruction
		{
		public:
			CallSite(const Local* dest, const std::string& methodName, const DataType& returnType, const std::vector<Value>& args = { });
			CallSite(const Local* dest, const Method& method, const std::vector<Value>& args = { });
			CallSite(const std::string& methodName, const DataType& returnType, const std::vector<Value>& args = { });
			explicit CallSite(const Method& method, const std::vector<Value>& args = { });
			~CallSite() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

			const std::vector<Value>& getArguments() const;

			const std::string& getMethodName() const;

		private:
			const Local* dest;
			const std::string methodName;
			const DataType returnType;
			const std::vector<Value> arguments;
		};

		class Copy: public LLVMInstruction
		{
		public:
			Copy(const Value& dest, const Value& orig, bool isLoadStore = false, bool isRead = false, bool isBitcast = false);
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

		class UnaryOperator: public LLVMInstruction
		{
		public:
			UnaryOperator(const std::string& opCode, const Value& dest, const Value& arg);
			~UnaryOperator() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		protected:
			const Value dest;
			const std::string opCode;
			const Value arg;
		};

		class BinaryOperator: public UnaryOperator
		{
		public:
			BinaryOperator(const std::string& opCode, const Value& dest, const Value& arg0, const Value& arg1);
			~BinaryOperator() override = default;

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Value arg2;

		};

		class IndexOf: public LLVMInstruction
		{
		public:
			IndexOf(const Value& dest, const Value& container, const std::vector<Value>& indices);
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

		class Comparison: public LLVMInstruction
		{
		public:
			Comparison(const Local* dest, const std::string& comp, const Value& op1, const Value& op2, bool isFloat);
			~Comparison() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Local* dest;
			const std::string comp;
			const bool isFloat;
			const Value op1;
			const Value op2;
		};

		class ContainerInsertion: public LLVMInstruction
		{
		public:
			ContainerInsertion(const Local* dest, const Value& container, const Value& newValue, const Value& index);
			~ContainerInsertion() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Local* dest;
			const Value container;
			const Value newValue;
			const Value index;
		};

		class ContainerExtraction: public LLVMInstruction
		{
		public:
			ContainerExtraction(const Local* dest, const Value& container, const Value& index);
			~ContainerExtraction() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Local* dest;
			const Value container;
			const Value index;
		};

		class ValueReturn: public LLVMInstruction
		{
		public:
			explicit ValueReturn();
			explicit ValueReturn(const Value& val);
			~ValueReturn() override = default;

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const bool hasValue;
			const Value val;
		};

		class ShuffleVector: public LLVMInstruction
		{
		public:
			ShuffleVector(const Value& dest, const Value& v1, const Value& v2, const Value& mask);
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

		class LLVMLabel: public LLVMInstruction
		{
		public:
			explicit LLVMLabel(const Local* label);
			~LLVMLabel() override = default;

			bool mapInstruction(Method& method) const override;
		private:
			const Local* label;
		};

		class PhiNode: public LLVMInstruction
		{
		public:
			PhiNode(const Local* dest, const std::vector<std::pair<Value, const Local*>>& labels);
			~PhiNode() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Local* dest;
			const std::vector<std::pair<Value, const Local*>> labels;
		};

		class Selection: public LLVMInstruction
		{
		public:
			Selection(const Local* dest, const Value& cond, const Value& opt1, const Value& opt2);
			~Selection() override = default;

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;

			bool mapInstruction(Method& method) const override;
		private:
			const Local* dest;
			const Value cond;
			const Value opt1;
			const Value opt2;
		};

		class Branch: public LLVMInstruction
		{
		public:
			explicit Branch(const Local* label);
			Branch(const Value& cond, const Local* thenLabel, const Local* elseLabel);
			~Branch() override = default;

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;
		private:
			const Local* thenLabel;
			const Local* elseLabel;
			const Value cond;
		};

		class Switch: public LLVMInstruction
		{
		public:
			Switch(const Value& cond, const std::string& defaultLabel, const std::map<int, std::string>& cases);
			~Switch() override = default;

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;
		private:
			const Value cond;
			const std::string defaultLabel;
			const std::map<int, std::string> jumpLabels;
		};
	} // namespace llvm2qasm
} // namespace vc4c
#endif /* LLVMINSTRUCTION_H */

