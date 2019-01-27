/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTERMEDIATEINSTRUCTION_H
#define INTERMEDIATEINSTRUCTION_H

#include "../Method.h"
#include "../asm/OpCodes.h"
#include "CompilationError.h"
#include "Optional.h"

namespace vc4c
{
    namespace qpu_asm
    {
        struct DecoratedInstruction;
    } // namespace qpu_asm

    namespace intermediate
    {
        /*
         * Additional flags set for individual instructions
         */
        enum class InstructionDecorations
        {
            // There are no decorations set for this instruction
            NONE = 0,
            // The result and parameters (floating-point values) are assumed to be non-NaN
            NO_NAN = 1 << 0,
            // The result and parameters (floating-point values) are assumed to be not +/- Inf
            NO_INF = 1 << 1,
            // The use of a reciprocal is allowed for this division
            ALLOW_RECIP = 1 << 2,
            // Implies NO_NAN, NO_INF and ALLOW_RECIP
            FAST_MATH = 1 << 3,
            // The conversion result needs be saturated within the limits of the result type
            SATURATED_CONVERSION = 1 << 4,
            // The result is the number of work-dimensions as of get_work_dim()
            BUILTIN_WORK_DIMENSIONS = 1 << 5,
            // The result is the local size in one of the dimensions as of get_local_size()
            BUILTIN_LOCAL_SIZE = 1 << 6,
            // The result is the local ID in one of the dimensions as of get_local_id()
            BUILTIN_LOCAL_ID = 1 << 7,
            // The result is the number of work-groups in one of the dimensions as of get_num_groups()
            BUILTIN_NUM_GROUPS = 1 << 8,
            // The result is the group-ID in one of the dimensions as of get_group_id()
            BUILTIN_GROUP_ID = 1 << 9,
            // The result is the global offset in one dimension as of get_global_offset()
            BUILTIN_GLOBAL_OFFSET = 1 << 10,
            // The result is the global size in one dimension as of get_global_size()
            BUILTIN_GLOBAL_SIZE = 1 << 11,
            // The result is the global id for one dimension (get_global_id())
            BUILTIN_GLOBAL_ID = 1 << 12,
            // The result value is unsigned (signed by default)
            UNSIGNED_RESULT = 1 << 13,
            // The result is a value of a PHI-node being set
            PHI_NODE = 1 << 14,
            // The conditional branch depends on ALL flags, not just the flag of the first SIMD-element
            BRANCH_ON_ALL_ELEMENTS = 1 << 15,
            // The instructions inserts a single element into a vector
            ELEMENT_INSERTION = 1 << 16,
            // The instruction was already processed by auto-vectorization
            AUTO_VECTORIZED = 1 << 17,
            // The result of the instruction is the same for all work-items within a single work-group
            WORK_GROUP_UNIFORM_VALUE = 1 << 18,
            // The instruction calculates VPM read configuration
            VPM_READ_CONFIGURATION = 1 << 19,
            // The instruction calculates VPM write configuration
            VPM_WRITE_CONFIGURATION = 1 << 20
        };

        std::string toString(InstructionDecorations decoration);
        /*
         * Returns all decorations set in the given decorations which can be forwarded to a move of the output-value of
         * the decorated instruction E.g. if an output is unsigned, the value moved to another register is still
         * unsigned, same for all built-in flags. On the other side, a moved value is no longer a PHI-node instruction
         * or an element insertion
         */
        InstructionDecorations forwardDecorations(InstructionDecorations decorations);

        /*
         * Converted to QPU instructions,
         * but still with method-calls and typed locals
         */
        class IntermediateInstruction
        {
        public:
            explicit IntermediateInstruction(Optional<Value>&& output = {}, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET, Pack packMode = PACK_NOP);
            IntermediateInstruction(const IntermediateInstruction&) = delete;
            IntermediateInstruction(IntermediateInstruction&&) noexcept = delete;
            virtual ~IntermediateInstruction();

            IntermediateInstruction& operator=(const IntermediateInstruction&) = delete;
            IntermediateInstruction& operator=(IntermediateInstruction&&) = delete;

            virtual FastMap<const Local*, LocalUse::Type> getUsedLocals() const;
            virtual void forUsedLocals(const std::function<void(const Local*, LocalUse::Type)>& consumer) const;
            virtual bool readsLocal(const Local* local) const;
            virtual bool writesLocal(const Local* local) const;
            virtual void replaceLocal(
                const Local* oldLocal, const Local* newLocal, LocalUse::Type type = LocalUse::Type::BOTH);
            virtual void replaceLocal(
                const Local* oldLocal, const Value newLocal, LocalUse::Type type = LocalUse::Type::BOTH);

            /*
             * Whether this instructions reads the given register
             */
            bool readsRegister(Register reg) const;
            /*
             * Whether this instructions writes into the given register
             */
            bool writesRegister(Register reg) const;
            /*
             * Whether at least one of the operands of this instruction is a constant
             */
            bool readsLiteral() const;

            virtual std::string to_string() const = 0;
            /*
             * Copies this instruction for the given method, renaming all locals by appending the local-prefix specified
             *
             * This function is used for inlining instructions
             */
            virtual NODISCARD IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix) const = 0;
            /*
             * Converts the instruction to an equivalent assembler-instruction with the local-register- and
             * label-position-mappings resolved
             */
            virtual NODISCARD qpu_asm::DecoratedInstruction convertToAsm(
                const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const = 0;
            /*
             * Whether this intermediate instruction will map to an assembler instruction
             */
            virtual bool mapsToASMInstruction() const;

            /*
             * Whether this instruction is normalized (e.g. can be converted to assembler instructions)
             * Labels also count as normalized, although they do not map to assembler instructions.
             *
             * NOTE: Any non-normalized instruction needs to be replaced before entering the optimization-steps!
             */
            virtual bool isNormalized() const = 0;

            /*
             * Returns the output value, if any
             */
            const Optional<Value>& getOutput() const;
            /*
             * Whether the instruction has an output and the output has the given type
             */
            bool hasValueType(ValueType type) const;

            /*
             * Returns the argument for the given index
             */
            const Optional<Value> getArgument(std::size_t index) const;
            /*
             * Returns the argument for the given index.
             *
             * Throws an exception, if the index is not valid!
             */
            const Value& assertArgument(std::size_t index) const;
            /*
             * Lists all arguments/operands
             */
            const std::vector<Value>& getArguments() const;
            /*
             * Sets the argument for the given index to the value specified
             */
            void setArgument(std::size_t index, const Value& arg);
            void setArgument(std::size_t index, Value&& arg);

            IntermediateInstruction* setOutput(const Optional<Value>& output);
            IntermediateInstruction* setOutput(Optional<Value>&& output);
            IntermediateInstruction* setSignaling(Signaling signal);
            IntermediateInstruction* setPackMode(Pack packMode);
            IntermediateInstruction* setCondition(ConditionCode condition);
            IntermediateInstruction* setSetFlags(SetFlag setFlags);
            IntermediateInstruction* setUnpackMode(Unpack unpackMode);
            IntermediateInstruction* addDecorations(InstructionDecorations decorations);
            bool hasDecoration(InstructionDecorations deco) const;

            /*
             * Whether this instruction has any side-effects.
             *
             * Side-effects include:
             * - accessing a register with side-effects (e.g. reading/writing hardware-mutex)
             * - triggering a signal (excluding ALU_IMMEDIATE, LOAD_IMMEDIATE signals)
             * - branches, semaphores
             * - setting of ALU flags
             */
            virtual bool hasSideEffects() const;
            /*
             * Whether an unpack-mode is set
             */
            bool hasUnpackMode() const;
            /*
             * Whether a pack-mode is set
             */
            bool hasPackMode() const;
            /*
             * Whether the execution of this instruction depends on ALU flags
             */
            bool hasConditionalExecution() const;
            /*
             * Whether does the instruction set flag
             */
            bool doesSetFlag() const;

            /*
             * Copies all the extras (signal, pack-modes, etc.) from the given instruction.
             *
             * NOTE: This function throws errors on merging incompatible extras (e.g. different non-default pack-modes)
             */
            IntermediateInstruction* copyExtrasFrom(const IntermediateInstruction* src);
            /*
             * Tries to calculate the operation performed by this instruction and returns a constant value if
             * successful. The parameter numIterations determines the number of instructions providing the operands
             * (e.g. writing the local being read here) to use for determining the operand values
             *
             * NOTE: The constant value returned can be of value-type REGISTER, LITERAL, SMALL_IMMEDIATE or CONTAINER
             */
            virtual PrecalculatedValue precalculate(std::size_t numIterations = 1) const;

            bool replaceValue(const Value oldValue, const Value newValue, LocalUse::Type type);

            Signaling signal;
            Unpack unpackMode;
            Pack packMode;
            ConditionCode conditional;
            SetFlag setFlags;
            bool canBeCombined;
            InstructionDecorations decoration;

        protected:
            Value renameValue(Method& method, const Value& orig, const std::string& prefix) const;

            std::string createAdditionalInfoString() const;

            Optional<Value> getPrecalculatedValueForArg(std::size_t argIndex, std::size_t numIterations) const;

        private:
            Optional<Value> output;
            std::vector<Value> arguments;

            void removeAsUserFromValue(const Value& value, LocalUse::Type type);
            void addAsUserToValue(const Value& value, LocalUse::Type type);
        };

        struct CombinedOperation;

        struct Operation final : public IntermediateInstruction
        {
            Operation(const OpCode& opCode, const Value& dest, const Value& arg0, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            Operation(OpCode opCode, Value&& dest, Value&& arg0, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            Operation(const OpCode& opCode, const Value& dest, const Value& arg0, const Value& arg1,
                ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            Operation(OpCode opCode, Value&& dest, Value&& arg0, Value&& arg1, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);

            ~Operation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Value& getFirstArg() const;
            const Optional<Value> getSecondArg() const;
            PrecalculatedValue precalculate(std::size_t numIterations) const override;

            /**
             * Returns whether the operation "simply" calculates the arithmetic operation specified, without
             * side-effects or unpack or pack modes applied
             */
            bool isSimpleOperation() const;

            OpCode op;
            CombinedOperation* parent;
        };

        /*
         * Operation which cannot be mapped to VC4 machine code, but must be intrisified (e.g. integer
         * multiplications/divisions)
         */
        struct IntrinsicOperation : public IntermediateInstruction
        {
            IntrinsicOperation(std::string&& opCode, Value&& dest, Value&& arg0, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            IntrinsicOperation(std::string&& opCode, Value&& dest, Value&& arg0, Value&& arg1,
                ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            ~IntrinsicOperation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Value& getFirstArg() const;
            const Optional<Value> getSecondArg() const;

            std::string opCode;
        };

        struct MethodCall final : public IntermediateInstruction
        {
            explicit MethodCall(std::string&& methodName, std::vector<Value>&& args = {});
            MethodCall(Value&& dest, std::string&& methodName, std::vector<Value>&& args = {});
            ~MethodCall() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;

            const DataType getReturnType() const;

            bool matchesSignature(const Method& method) const;

            std::string methodName;
        };

        struct Return final : public IntermediateInstruction
        {
            explicit Return();
            explicit Return(Value&& val);
            ~Return() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            Optional<Value> getReturnValue() const;
        };

        struct MoveOperation : public IntermediateInstruction
        {
            MoveOperation(const Value& dest, const Value& arg, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            MoveOperation(
                Value&& dest, Value&& arg, ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            ~MoveOperation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            Operation* combineWith(const OpCode& otherOpCode) const;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            PrecalculatedValue precalculate(std::size_t numIterations) const override;

            void setSource(Value&& value);
            const Value& getSource() const;

            /**
             * Returns whether this move is "simple", i.e. copying the bits from the source to the destination without
             * triggering any side-effects, unpacking or packing any values.
             */
            virtual bool isSimpleMove() const;
        };

        struct VectorRotation final : public MoveOperation
        {
            VectorRotation(const Value& dest, const Value& src, const Value& offset, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            VectorRotation(Value&& dest, Value&& src, Value&& offset, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            ~VectorRotation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            Operation* combineWith(const std::string& otherOpCode) const;
            PrecalculatedValue precalculate(std::size_t numIterations) const override;

            const Value& getOffset() const;

            bool isSimpleMove() const override;
        };

        struct BranchLabel final : public IntermediateInstruction
        {
        public:
            explicit BranchLabel(const Local& label);
            ~BranchLabel() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Local* getLabel() const;
            Local* getLabel();
        };

        struct Branch final : public IntermediateInstruction
        {
            Branch(const Local* target, ConditionCode condCode, const Value& cond);
            ~Branch() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;
            bool hasSideEffects() const override;

            const Local* getTarget() const;

            bool isUnconditional() const;
            const Value& getCondition() const;
        };

        enum class DelayType : unsigned char
        {
            // one of the 3 delay-slots after a branch -> can never be optimized away
            BRANCH_DELAY,
            // waiting for result of a SFU call, can only be replaced by instructions not accessing SFU or r4
            WAIT_SFU,
            // waiting for TMU result, can only be replaced by instructions not accessing TMU or r4
            WAIT_TMU,
            // waiting for a register to be written, so it can be read again. Can be replaced by instructions not
            // accessing this register
            WAIT_REGISTER,
            // delay-slots for thread-end. There is no code afterwards to replace them
            THREAD_END,
            // waiting for the UNIFORM address-register to be changed before reading an UNIFORM value
            WAIT_UNIFORM,
            // waiting for a VPM operation (DMA read/write or VPM read) to finish. These types of nops can be removed,
            // after they are replaced
            WAIT_VPM
        };

        struct Nop final : public IntermediateInstruction
        {
        public:
            explicit Nop(DelayType type, Signaling signal = SIGNAL_NONE);
            ~Nop() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;

            DelayType type;
        };

        // names are according to LLVM names
        static constexpr const char* COMP_EQ = "eq";
        static constexpr const char* COMP_NEQ = "ne";
        static constexpr const char* COMP_UNSIGNED_GT = "ugt";
        static constexpr const char* COMP_UNSIGNED_GE = "uge";
        static constexpr const char* COMP_UNSIGNED_LT = "ult";
        static constexpr const char* COMP_UNSIGNED_LE = "ule";
        static constexpr const char* COMP_SIGNED_GT = "sgt";
        static constexpr const char* COMP_SIGNED_GE = "sge";
        static constexpr const char* COMP_SIGNED_LT = "slt";
        static constexpr const char* COMP_SIGNED_LE = "sle";

        //"ordered" -> no NaNs, "unordered", NaNs allowed
        // LLVM: "false: always yields false, regardless of operands."
        static constexpr const char* COMP_FALSE = "false";
        // LLVM: "true: always yields true, regardless of operands."
        static constexpr const char* COMP_TRUE = "true";

        // LLVM: "oeq: yields true if both operands are not a QNAN and op1 is equal to op2."
        static constexpr const char* COMP_ORDERED_EQ = "oeq";
        // LLVM: "one: yields true if both operands are not a QNAN and op1 is not equal to op2."
        static constexpr const char* COMP_ORDERED_NEQ = "one";
        // LLVM: "ogt: yields true if both operands are not a QNAN and op1 is greater than op2."
        static constexpr const char* COMP_ORDERED_GT = "ogt";
        // LLVM: "oge: yields true if both operands are not a QNAN and op1 is greater than or equal to op2."
        static constexpr const char* COMP_ORDERED_GE = "oge";
        // LLVM: "olt: yields true if both operands are not a QNAN and op1 is less than op2."
        static constexpr const char* COMP_ORDERED_LT = "olt";
        // LLVM: "ole: yields true if both operands are not a QNAN and op1 is less than or equal to op2."
        static constexpr const char* COMP_ORDERED_LE = "ole";
        // LLVM: "ord: yields true if both operands are not a QNAN."
        static constexpr const char* COMP_ORDERED = "ord";

        // LLVM: "ueq: yields true if either operand is a QNAN or op1 is equal to op2."
        static constexpr const char* COMP_UNORDERED_EQ = "ueq";
        // LLVM: "une: yields true if either operand is a QNAN or op1 is not equal to op2."
        static constexpr const char* COMP_UNORDERED_NEQ = "une";
        // LLVM: "ugt: yields true if either operand is a QNAN or op1 is greater than op2."
        static constexpr const char* COMP_UNORDERED_GT = "ugt";
        // LLVM: "uge: yields true if either operand is a QNAN or op1 is greater than or equal to op2."
        static constexpr const char* COMP_UNORDERED_GE = "uge";
        // LLVM: "ult: yields true if either operand is a QNAN or op1 is less than op2."
        static constexpr const char* COMP_UNORDERED_LT = "ult";
        // LLVM: "ule: yields true if either operand is a QNAN or op1 is less than or equal to op2."
        static constexpr const char* COMP_UNORDERED_LE = "ule";
        // LLVM: "uno: yields true if either operand is a QNAN."
        static constexpr const char* COMP_UNORDERED = "uno";

        struct Comparison final : public IntrinsicOperation
        {
        public:
            Comparison(std::string&& comp, Value&& dest, Value&& val0, Value&& val1);
            ~Comparison() override = default;

            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
        };

        struct CombinedOperation final : public IntermediateInstruction
        {
        public:
            CombinedOperation(Operation* op1, Operation* op2);
            ~CombinedOperation() override = default;

            FastMap<const Local*, LocalUse::Type> getUsedLocals() const override;
            void forUsedLocals(const std::function<void(const Local*, LocalUse::Type)>& consumer) const override;
            bool readsLocal(const Local* local) const override;
            bool writesLocal(const Local* local) const override;
            void replaceLocal(const Local* oldLocal, const Local* newLocal, LocalUse::Type type) override;

            std::string to_string() const override;

            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;
            bool hasSideEffects() const override;

            const Operation* getFirstOp() const;
            const Operation* getSecondOP() const;

            const std::unique_ptr<IntermediateInstruction> op1;
            const std::unique_ptr<IntermediateInstruction> op2;
        };

        /**
         * The VideoCore IV instruction set offers 3 different types of loads
         * (see Broadcom VideoCore IV specification, figure 5, page 33)
         *
         * Note: the constant values correspond with the flags specifying the type of load (bits 57/58)
         */
        enum class LoadType : unsigned char
        {
            /**
             * The default load instruction, replicates the 32-bit literal value across all 16 SIMD elements of the
             * output register(s).
             */
            REPLICATE_INT32 = 0,
            /**
             * The 32-bit literal value is split into upper and lower half. 1 bit per half is set per SIMD element. The
             * value is sign extended. These 4 different values can be set per element (here set for 2nd element):
             * xx0xxxxxxxxxxxxxyy0yyyyyyyyyyyyy -> 0x00000000 (0)
             * xx0xxxxxxxxxxxxxyy1yyyyyyyyyyyyy -> 0x00000001 (1)
             * xx1xxxxxxxxxxxxxyy0yyyyyyyyyyyyy -> 0xFFFFFFFE (-2)
             * xx1xxxxxxxxxxxxxyy1yyyyyyyyyyyyy -> 0xFFFFFFFF (-1)
             */
            PER_ELEMENT_SIGNED = 1,
            /**
             * The 32-bit literal value is split into upper and lower half. 1 bit per half is set per SIMD element. The
             * value is zero extended. These 4 different values can be set per element (here set for 2nd element):
             * xx0xxxxxxxxxxxxxyy0yyyyyyyyyyyyy -> 0x00000000 (0)
             * xx0xxxxxxxxxxxxxyy1yyyyyyyyyyyyy -> 0x00000001 (1)
             * xx1xxxxxxxxxxxxxyy0yyyyyyyyyyyyy -> 0x00000002 (2)
             * xx1xxxxxxxxxxxxxyy1yyyyyyyyyyyyy -> 0x00000003 (3)
             */
            PER_ELEMENT_UNSIGNED = 3
        };

        /**
         * NOTE: Load instructions can set flags per element (like any ALU instruction). This means, for the per-element
         * variants, the resulting flags can differ.
         *
         * NOTE: Conditional loads also apply per element (like any ALU instruction). This allows for some elements to
         * be overridden in the load, while other stay unchanged.
         */
        struct LoadImmediate final : public IntermediateInstruction
        {
        public:
            LoadImmediate(const Value& dest, const Literal& source, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            LoadImmediate(const Value& dest, uint32_t mask, LoadType type, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            ~LoadImmediate() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool isNormalized() const override;

            PrecalculatedValue precalculate(std::size_t numIterations) const override;

            Literal getImmediate() const;
            void setImmediate(const Literal& value);

            const LoadType type;
        };

        /*
         * Increments or decrements one of the 15 hardware semaphores
         *
         * Semaphore values are initially set to zero (i.e. decreasing the counter immediately stalls) and are reset
         * when the GPU is reset (e.g. shut-down and started again via the Mailbox interface). This means, that a kernel
         * has to make sure, all semaphores are reset to 0 at the end of its execution!
         *
         * Even the constructor does not suggest it, semaphore instructions can write up to two registers.
         * The value written to the registers is the lower word (similar to load immediate for 32-bit immediate
         * version).
         *
         * This means, that the value loaded consists of 27 unused bits (set to zero), the semaphore increment/decrement
         * flag as well as the semaphore index. So incrementing semaphore 7 will return 7, decrementing it will
         * return 23.
         *
         * Semaphore instruction MUST NOT write to any hardware register which can stall (e.g. TMU, SFU)
         *
         */
        struct SemaphoreAdjustment final : public IntermediateInstruction
        {
        public:
            SemaphoreAdjustment(Semaphore semaphore, bool increase);
            ~SemaphoreAdjustment() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool isNormalized() const override;
            bool hasSideEffects() const override;

            const Semaphore semaphore;
            const bool increase;
        };

        struct PhiNode final : public IntermediateInstruction
        {
        public:
            PhiNode(Value&& dest, std::vector<std::pair<Value, const Local*>>&& labelPairs,
                ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            ~PhiNode() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool isNormalized() const override;

            FastMap<const Local*, Value> getValuesForLabels() const;
        };

        /*
         * Taken from SPIR-V Scope
         */
        enum class MemoryScope : unsigned char
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
        enum class MemorySemantics : unsigned short
        {
            NONE = 0x0,
            //"All memory operations provided in program order after this memory operation will execute after this
            // memory operation."
            ACQUIRE = 0x2,
            //"All memory operations provided in program order before this memory operation will execute before this
            // memory operation."
            RELEASE = 0x4,
            //"Has the properties of both Acquire and Release semantics. It is used for read-modify-write operations."
            ACQUIRE_RELEASE = 0x8,
            //"All observers will see this memory access in the same order with respect to other sequentially-consistent
            // memory accesses from this invocation."
            SEQUENTIALLY_CONSISTENT = 0x10,
            //"Apply the memory-ordering constraints to subgroup memory."
            SUBGROUP_MEMORY = 0x80,
            //"Apply the memory-ordering constraints to Workgroup Storage Class memory."
            WORK_GROUP_MEMORY = 0x100,
            //"Apply the memory-ordering constraints to CrossWorkgroup Storage Class memory."
            CROSS_WORK_GROUP_MEMORY = 0x200,
            //"Apply the memory-ordering constraints to AtomicCounter Storage Class memory."
            ATOMIC_COUNTER_MEMORY = 0x400,
            //"Apply the memory-ordering constraints to image contents (types declared by OpTypeImage), or to accesses
            // done through pointers to the Image Storage Class."
            IMAGE_MEMORY = 0x800
        };

        /*
         * Instruction that prohibits re-ordering (and combination) of memory-accessing instructions across it.
         */
        struct MemoryBarrier final : public IntermediateInstruction
        {
        public:
            MemoryBarrier(MemoryScope scope, MemorySemantics semantics);
            ~MemoryBarrier() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            MemoryScope scope;
            MemorySemantics semantics;
        };

        /*
         * Instruction specifying the begin/end of the life-time of a stack allocation
         */
        struct LifetimeBoundary final : IntermediateInstruction
        {
        public:
            LifetimeBoundary(const Value& allocation, bool lifetimeEnd);
            ~LifetimeBoundary() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Value& getStackAllocation() const;

            bool isLifetimeEnd;
        };

        enum class MutexAccess : unsigned char
        {
            LOCK,
            RELEASE
        };

        /*
         * Instruction accessing (locking/unlocking) the hardware-mutex
         */
        struct MutexLock final : IntermediateInstruction
        {
        public:
            MutexLock(MutexAccess accessType);
            ~MutexLock() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool isNormalized() const override;
            bool hasSideEffects() const override;

            bool locksMutex() const;
            bool releasesMutex() const;

        private:
            MutexAccess accessType;
        };

        enum class MemoryOperation : unsigned char
        {
            // simple read from memory into a local register of a QPU
            READ,
            // simple write of a local register into memory
            WRITE,
            //(sized) copy of memory from one area into another
            COPY,
            // fills the destination area with a fixed copies of the source value
            FILL
        };

        /*
         * Instruction operating (reading/writing/copying/filling) on memory or VPM cache
         */
        struct MemoryInstruction final : IntermediateInstruction
        {
        public:
            MemoryInstruction(MemoryOperation op, Value&& dest, Value&& src, Value&& numEntries = Value(INT_ONE));

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(Method& method, const std::string& localPrefix) const override;
            bool isNormalized() const override;
            bool hasSideEffects() const override;

            const Value& getSource() const;
            const Value& getDestination() const;
            const Value& getNumEntries() const;

            bool accessesConstantGlobal() const;
            bool accessesStackAllocation() const;
            bool accessesLocalMemory() const;

            /*
             * Whether the source (address or local value) can be moved into VPM
             */
            bool canMoveSourceIntoVPM() const;
            /*
             * Whether the destination (address or local value) can be moved into VPM
             */
            bool canMoveDestinationIntoVPM() const;

            /*
             * Returns the element-type (e.g. type of area pointed to) for the source
             *
             * For local values, this returns the value-type, for pointers the pointed-to type.
             *
             * If sizedType is true, a type spanning the whole copied memory-area is returned for sized memory-copy
             * operations.
             */
            DataType getSourceElementType(bool sizedType = false) const;
            /*
             * Returns the element-type (e.g. type of area pointed to) for the destination
             *
             * For local values, this returns the value-type, for pointers the pointed-to type.
             *
             * If sizedType is true, a type spanning the whole copied memory-area is returned for sized memory-copy
             * operations. For the memory-fill operation, this also constructs a type spanning the complete area filled.
             */
            DataType getDestinationElementType(bool sizedType = false) const;

            /*
             * Returns the base-addresses for the accessed memory-areas
             */
            FastSet<const Local*> getMemoryAreas() const;

            const MemoryOperation op;
        };
    } // namespace intermediate
} // namespace vc4c

#endif /* INTERMEDIATEINSTRUCTION_H */
