/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTERMEDIATEINSTRUCTION_H
#define INTERMEDIATEINSTRUCTION_H

#include <map>

#include "../Module.h"
#include "helper.h"
#include "../asm/OpCodes.h"
#include "CompilationError.h"

namespace vc4c
{
	namespace qpu_asm
	{
		class Instruction;
	}

	namespace intermediate
	{
		std::pair<Register, Optional<SmallImmediate>> getInputValue(const Value& val, const FastMap<const Local*, Register>& registerMapping);

		/*
		 * Additional flags set for individual instructions
		 */
		enum class InstructionDecorations
		{
			//There are no decorations set for this instruction
			NONE = 0x0,
			//The result and parameters (floating-point values) are assumed to be non-NaN
			NO_NAN = 0x1,
			//The result and parameters (floating-point values) are assumed to be not +/- Inf
			NO_INF = 0x2,
			//The use of a reciprocal is allowed for this division
			ALLOW_RECIP = 0x4,
			//Implies NO_NAN, NO_INF and ALLOW_RECIP
			FAST_MATH = 0x8,
			//The conversion result needs be saturated within the limits of the result type
			SATURATED_CONVERSION = 0x10,
			//The result is the number of work-dimensions as of get_work_dim()
			BUILTIN_WORK_DIMENSIONS = 0x20,
			//The result is the local size in one of the dimensions as of get_local_size()
			BUILTIN_LOCAL_SIZE = 0x40,
			//The result is the local ID in one of the dimensions as of get_local_id()
			BUILTIN_LOCAL_ID = 0x80,
			//The result is the number of work-groups in one of the dimensions as of get_num_groups()
			BUILTIN_NUM_GROUPS = 0x100,
			//The result is the group-ID in one of the dimensions as of get_group_id()
			BUILTIN_GROUP_ID = 0x200,
			//The result is the global offset in one dimension as of get_global_offset()
			BUILTIN_GLOBAL_OFFSET = 0x400,
			//The result is the global size in one dimension as of get_global_size()
			BUILTIN_GLOBAL_SIZE = 0x800,
			//The result is the global id for one dimension (get_global_id())
			BUILTIN_GLOBAL_ID = 0x1000,
			//The result value is unsigned (signed by default)
			UNSIGNED_RESULT = 0x2000,
			//The result is a value of a PHI-node being set
			PHI_NODE = 0x4000,
			//The conditional branch depends on ALL flags, not just the flag of the first SIMD-element
			BRANCH_ON_ALL_ELEMENTS = 0x8000,
			//The instructions inserts a single element into a vector
			ELEMENT_INSERTION = 0x10000
		};

		std::string toString(const InstructionDecorations decoration);

		/*
		 * Converted to QPU instructions,
		 * but still with method-calls and typed locals
		 */
		class IntermediateInstruction : public LocalUser
		{
		public:
			IntermediateInstruction(Optional<Value> output = { }, ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET, Pack packMode = PACK_NOP);
			virtual ~IntermediateInstruction();

			FastMap<const Local*, LocalUser::Type> getUsedLocals() const override;
			void forUsedLocals(const std::function<void(const Local*, LocalUser::Type)>& consumer) const override;
			bool readsLocal(const Local* local) const override;
			bool writesLocal(const Local* local) const override;
			void replaceLocal(const Local* oldLocal, const Local* newLocal, const Type type) override;

			virtual std::string to_string() const = 0;
			virtual IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const = 0;
			virtual qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const = 0;
			/*
			 * Whether this intermediate instruction will map to an assembler instruction
			 */
			virtual bool mapsToASMInstruction() const;

			const Optional<Value>& getOutput() const;
			bool hasValueType(const ValueType type) const;

			const Optional<Value> getArgument(const std::size_t index) const;
			const std::vector<Value>& getArguments() const;
			void setArgument(const std::size_t index, const Value& arg);

			IntermediateInstruction* setOutput(const Optional<Value>& output);
			IntermediateInstruction* setSignaling(const Signaling signal);
			IntermediateInstruction* setPackMode(const Pack packMode);
			IntermediateInstruction* setCondition(const ConditionCode condition);
			IntermediateInstruction* setSetFlags(const SetFlag setFlags);
			IntermediateInstruction* setUnpackMode(const Unpack unpackMode);
			IntermediateInstruction* setDecorations(const InstructionDecorations decorations);

			bool firesSignal(bool ignoreImmediates = true) const;
			bool hasSideEffects() const;
			bool hasUnpackMode() const;
			bool hasPackMode() const;
			bool hasConditionalExecution() const;

			IntermediateInstruction* copyExtrasFrom(const IntermediateInstruction* src);
			virtual Optional<Value> precalculate(const std::size_t numIterations) const;

			Signaling signal;
			Unpack unpackMode;
			Pack packMode;
			ConditionCode conditional;
			SetFlag setFlags;
			InstructionDecorations decoration;
			bool canBeCombined;
		protected:
			const Value renameValue(Method& method, const Value& orig, const std::string& prefix) const;

			std::string createAdditionalInfoString() const;

			Optional<Value> getPrecalculatedValueForArg(const std::size_t argIndex, const std::size_t numIterations) const;

		private:
			Optional<Value> output;
			std::vector<Value> arguments;

			void removeAsUserFromValue(const Value& value, const LocalUser::Type type);
			void addAsUserToValue(const Value& value, LocalUser::Type type);
		};

		struct CombinedOperation;

		struct Operation: public IntermediateInstruction
		{
			Operation(const std::string& opCode, const Value& dest, const Value& arg0, const ConditionCode cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			Operation(const std::string& opCode, const Value& dest, const Value& arg0, const Value& arg1, const ConditionCode cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			virtual ~Operation();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			virtual bool mapsToASMInstruction() const;

			const Value getFirstArg() const;
			const Optional<Value> getSecondArg() const;
			Optional<Value> precalculate(const std::size_t numIterations) const override;

			std::string opCode;
			CombinedOperation* parent;
		};

		struct MethodCall: public IntermediateInstruction
		{
			MethodCall(const std::string& methodName, const std::vector<Value>& args = { });
			MethodCall(const Value& dest, const std::string& methodName, const std::vector<Value>& args = { });
			virtual ~MethodCall();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;

			const DataType getReturnType() const;

			bool matchesSignature(const Method& method) const;

			std::string methodName;
		};

		struct Return: public IntermediateInstruction
		{
			Return(const Value& val);
			Return();
			virtual ~Return();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			bool mapsToASMInstruction() const override;

			Optional<Value> getReturnValue() const;
		};

		struct MoveOperation: public IntermediateInstruction
		{
			MoveOperation(const Value& dest, const Value& arg, const ConditionCode cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			virtual ~MoveOperation();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			Operation* combineWith(const std::string& otherOpCode) const;
			virtual bool mapsToASMInstruction() const;
			Optional<Value> precalculate(const std::size_t numIterations) const override;

			void setSource(const Value& value);
			const Value getSource() const;
		};

		struct VectorRotation: public MoveOperation
		{
			VectorRotation(const Value& dest, const Value& src, const Value& offset, const ConditionCode cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			virtual ~VectorRotation();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			Operation* combineWith(const std::string& otherOpCode) const;
			Optional<Value> precalculate(const std::size_t numIterations) const override;

			const Value getOffset() const;
		};

		struct BranchLabel: public IntermediateInstruction
		{
		public:
			BranchLabel(const Local& label);
			virtual ~BranchLabel();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			bool mapsToASMInstruction() const override;

			const Local* getLabel() const;
		};

		struct Branch: public IntermediateInstruction
		{
			Branch(const Local* target, const ConditionCode condCode, const Value& cond);
			virtual ~Branch();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;

			const Local* getTarget() const;

			bool isUnconditional() const;
			const Value getCondition() const;
		};

		enum class DelayType
		{
			//one of the 3 delay-slots after a branch -> can never be optimized away
			BRANCH_DELAY,
			//waiting for result of a SFU call, can only be replaced by instructions not accessing SFU or r4
			WAIT_SFU,
			//waiting for TMU result, can only be replaced by instructions not accessing TMU or r4
			WAIT_TMU,
			//waiting for a register to be written, so it can be read again. Can be replaced by instructions not accessing this register
			WAIT_REGISTER,
			//delay-slots for thread-end. There is no code afterwards to replace them
			THREAD_END,
			//waiting for the UNIFORM address-register to be changed before reading an UNIFORM value
			WAIT_UNIFORM
		};

		struct Nop: public IntermediateInstruction
		{
		public:
			Nop(const DelayType type, const Signaling signal = Signaling::NO_SIGNAL);
			virtual ~Nop();

			std::string to_string() const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;

			DelayType type;
		};

		//names are according to LLVM names
		static const std::string COMP_EQ = "eq";
		static const std::string COMP_NEQ = "ne";
		static const std::string COMP_UNSIGNED_GT = "ugt";
		static const std::string COMP_UNSIGNED_GE = "uge";
		static const std::string COMP_UNSIGNED_LT = "ult";
		static const std::string COMP_UNSIGNED_LE = "ule";
		static const std::string COMP_SIGNED_GT = "sgt";
		static const std::string COMP_SIGNED_GE = "sge";
		static const std::string COMP_SIGNED_LT = "slt";
		static const std::string COMP_SIGNED_LE = "sle";

		//"ordered" -> no NaNs, "unordered", NaNs allowed
		static const std::string COMP_FALSE = "false";
		static const std::string COMP_TRUE = "true";

		static const std::string COMP_ORDERED_EQ = "oeq";
		static const std::string COMP_ORDERED_NEQ = "one";
		static const std::string COMP_ORDERED_GT = "ogt";
		static const std::string COMP_ORDERED_GE = "oge";
		static const std::string COMP_ORDERED_LT = "olt";
		static const std::string COMP_ORDERED_LE = "ole";
		static const std::string COMP_ORDERED = "ord";

		static const std::string COMP_UNORDERED_EQ = "ueq";
		static const std::string COMP_UNORDERED_NEQ = "une";
		static const std::string COMP_UNORDERED_GT = "ugt";
		static const std::string COMP_UNORDERED_GE = "uge";
		static const std::string COMP_UNORDERED_LT = "ult";
		static const std::string COMP_UNORDERED_LE = "ule";
		static const std::string COMP_UNORDERED = "uno";

		struct Comparison: public Operation
		{
		public:
			Comparison(const std::string& comp, const Value& dest, const Value& val0, const Value& val1);
			virtual ~Comparison();
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
		};

		struct CombinedOperation: public IntermediateInstruction
		{
		public:
			CombinedOperation(Operation* op1, Operation* op2);
			virtual ~CombinedOperation();

			FastMap<const Local*, LocalUser::Type> getUsedLocals() const override;
			void forUsedLocals(const std::function<void(const Local*, LocalUser::Type)>& consumer) const override;
			bool readsLocal(const Local* local) const override;
			bool writesLocal(const Local* local) const override;
			void replaceLocal(const Local* oldLocal, const Local* newLocal, const Type type) override;

			std::string to_string() const override;

			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			virtual bool mapsToASMInstruction() const;

			const Operation* getFirstOp() const;
			const Operation* getSecondOP() const;

			const std::unique_ptr<IntermediateInstruction> op1;
			const std::unique_ptr<IntermediateInstruction> op2;
		};

		struct LoadImmediate: public IntermediateInstruction
		{
		public:
			LoadImmediate(const Value& dest, const Literal& source, const ConditionCode& cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			virtual ~LoadImmediate();

			std::string to_string() const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			Optional<Value> precalculate(const std::size_t numIterations) const override;

			const Literal& getImmediate() const;
			void setImmediate(const Literal& value);
		};

		struct SemaphoreAdjustment: public IntermediateInstruction
		{
		public:
			SemaphoreAdjustment(const Semaphore semaphore, const bool increase, const ConditionCode& cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			virtual ~SemaphoreAdjustment();

			std::string to_string() const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;

			const Semaphore semaphore;
			const bool increase;
		};

		struct PhiNode: public IntermediateInstruction
		{
		public:
			PhiNode(const Value& dest, const std::vector<std::pair<Value, const Local*>>& labelPairs, const ConditionCode& cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);
			virtual ~PhiNode();

			std::string to_string() const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;

			FastMap<const Local*, Value> getValuesForLabels() const;
		};

		/*
		 * Taken from SPIR-V Scope
		 */
		enum class MemoryScope
		{
			//"Scope crosses multiple devices."
			CROSS_DEVICE = 0,
			//"Scope is the current device."
			DEVICE = 1,
			//"Scope is the current workgroup."
			WORK_GROUP = 2,
			//"Scope is the current subgroup."
			SUB_GROUP = 3,
			//"Scope is the current Invocation."
			INVOCATION = 4
		};

		/*
		 * Taken from SPIR-V MemorySemantics
		 */
		enum class MemorySemantics
		{
			NONE = 0x0,
			//"All memory operations provided in program order after this memory operation will execute after this memory operation."
			ACQUIRE = 0x2,
			//"All memory operations provided in program order before this memory operation will execute before this memory operation."
			RELEASE = 0x4,
			//"Has the properties of both Acquire and Release semantics. It is used for read-modify-write operations."
			ACQUIRE_RELEASE = 0x8,
			//"All observers will see this memory access in the same order with respect to other sequentially-consistent memory accesses from this invocation."
			SEQUENTIALLY_CONSISTENT = 0x10,
			//"Apply the memory-ordering constraints to subgroup memory."
			SUBGROUP_MEMORY = 0x80,
			//"Apply the memory-ordering constraints to Workgroup Storage Class memory."
			WORK_GROUP_MEMORY = 0x100,
			//"Apply the memory-ordering constraints to CrossWorkgroup Storage Class memory."
			CROSS_WORK_GROUP_MEMORY = 0x200,
			//"Apply the memory-ordering constraints to AtomicCounter Storage Class memory."
			ATOMIC_COUNTER_MEMORY = 0x400,
			//"Apply the memory-ordering constraints to image contents (types declared by OpTypeImage), or to accesses done through pointers to the Image Storage Class."
			IMAGE_MEMORY = 0x800
		};

		/*
		 * Instruction that prohibits re-ordering (and combination) of memory-accessing instructions across it.
		 */
		struct MemoryBarrier : public IntermediateInstruction
		{
		public:
			MemoryBarrier(const MemoryScope scope, const MemorySemantics semantics);
			virtual ~MemoryBarrier();

			std::string to_string() const override;
			qpu_asm::Instruction* convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const override;
			IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
			bool mapsToASMInstruction() const override;

			MemoryScope scope;
			MemorySemantics semantics;
		};

		using Instructions = FastModificationList<std::unique_ptr<IntermediateInstruction>>;
		using InstructionsIterator = FastModificationList<std::unique_ptr<IntermediateInstruction>>::iterator;
		using ConstInstructionsIterator = FastModificationList<std::unique_ptr<IntermediateInstruction>>::const_iterator;
	}
}


#endif /* INTERMEDIATEINSTRUCTION_H */

