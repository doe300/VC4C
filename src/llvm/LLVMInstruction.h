/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LLVMINSTRUCTION_H
#define LLVMINSTRUCTION_H

#include <string>
#include <vector>
#include <utility>
#include <memory>
#include <map>

#include "Token.h"
#include "../Module.h"
#include "helper.h"
#include "../intermediate/IntermediateInstruction.h"

namespace vc4c
{

	namespace llvm2qasm
	{
		ValueType toValueType(const TokenType type);

		/*!
		 * http://llvm.org/docs/LangRef.html
		 */
		class LLVMInstruction
		{
		public:
			LLVMInstruction();
			virtual ~LLVMInstruction();

			virtual const Local* getDeclaredLocal() const;
			virtual std::vector<const Local*> getAllLocals() const;

			virtual bool mapInstruction(Method& method) const = 0;

			LLVMInstruction* setDecorations(const intermediate::InstructionDecorations decorations);

		protected:
			intermediate::InstructionDecorations decorations;
		};

		class CallSite: public LLVMInstruction
		{
		public:
			CallSite(const Local* dest, const std::string& methodName, const DataType& returnType, const std::vector<Value>& args = { });
			CallSite(const Local* dest, const Method& method, const std::vector<Value>& args = { });
			CallSite(const std::string& methodName, const DataType& returnType, const std::vector<Value>& args = { });
			CallSite(const Method& method, const std::vector<Value>& args = { });
			virtual ~CallSite();

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
			Copy(const Local* dest, const Value& orig, const Value& index = UNDEFINED_VALUE, const bool isRead = false);
			virtual ~Copy();

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Local* dest;
			const Value orig;
			const Value index;
			const bool isRead;
		};

		class UnaryOperator: public LLVMInstruction
		{
		public:
			UnaryOperator(const std::string& opCode, const Value& dest, const Value& arg);
			virtual ~UnaryOperator();

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
			BinaryOperator(const std::string& opCode, const Local* dest, const Value& arg0, const Value& arg1);
			virtual ~BinaryOperator();

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const Value arg2;

		};

		class IndexOf: public LLVMInstruction
		{
		public:
			IndexOf(const Local* dest, const Value& container, const std::vector<Value>& indices);
			virtual ~IndexOf();

			const Local* getDeclaredLocal() const override;
			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

			const Value getContainer() const;

		private:
			const Local* dest;
			const Value container;
			const std::vector<Value> indices;
		};

		class Comparison: public LLVMInstruction
		{
		public:
			Comparison(const Local* dest, const std::string& comp, const Value& op1, const Value& op2, const bool isFloat);
			virtual ~Comparison();

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
			virtual ~ContainerInsertion();

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
			virtual ~ContainerExtraction();

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
			ValueReturn();
			ValueReturn(const Value& val);
			virtual ~ValueReturn();

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;

		private:
			const bool hasValue;
			const Value val;
		};

		class ShuffleVector: public LLVMInstruction
		{
		public:
			ShuffleVector(const Local* dest, const Value& v1, const Value& v2, const Value& mask);
			virtual ~ShuffleVector();

			std::vector<const Local*> getAllLocals() const override;
			const Local* getDeclaredLocal() const override;
			bool mapInstruction(Method& method) const override;
		private:
			const Local* dest;
			const Value v1;
			const Value v2;
			const Value mask;
		};

		class LLVMLabel: public LLVMInstruction
		{
		public:
			LLVMLabel(const std::string& name);
			virtual ~LLVMLabel();

			bool mapInstruction(Method& method) const override;
		private:
			const std::string label;
		};

		class PhiNode: public LLVMInstruction
		{
		public:
			PhiNode(const Local* dest, const std::vector<std::pair<Value, const Local*>>& labels);
			virtual ~PhiNode();

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
			virtual ~Selection();

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
			Branch(const std::string& label);
			Branch(const Value& cond, const std::string& thenLabel, const std::string& elseLabel);
			virtual ~Branch();

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;
		private:
			const std::string thenLabel;
			const std::string elseLabel;
			const Value cond;
		};

		class Switch: public LLVMInstruction
		{
		public:
			Switch(const Value& cond, const std::string& defaultLabel, const std::map<int, std::string>& cases);
			virtual ~Switch();

			std::vector<const Local*> getAllLocals() const override;
			bool mapInstruction(Method& method) const override;
		private:
			const Value cond;
			const std::string defaultLabel;
			const std::map<int, std::string> jumpLabels;
		};
	}
}
#endif /* LLVMINSTRUCTION_H */

