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
        using InlineMapping = FastMap<const Local*, const Local*>;
        /*
         * Additional flags set for individual instructions
         */
        enum class InstructionDecorations : uint32_t
        {
            // There are no decorations set for this instruction
            NONE = 0u,
            // The result and parameters (floating-point values) are assumed to be non-NaN
            NO_NAN = 1u << 0u,
            // The result and parameters (floating-point values) are assumed to be not +/- Inf
            NO_INF = 1u << 1u,
            // The use of a reciprocal is allowed for this division
            ALLOW_RECIP = 1u << 2u,
            // Implies NO_NAN, NO_INF and ALLOW_RECIP
            FAST_MATH = 1u << 3u,
            // The conversion result needs be saturated within the limits of the result type
            SATURATED_CONVERSION = 1u << 4u,
            // The result is the number of work-dimensions as of get_work_dim()
            BUILTIN_WORK_DIMENSIONS = 1u << 5u,
            // The result is the local size in one of the dimensions as of get_local_size()
            BUILTIN_LOCAL_SIZE = 1u << 6u,
            // The result is the local ID in one of the dimensions as of get_local_id()
            BUILTIN_LOCAL_ID = 1u << 7u,
            // The result is the number of work-groups in one of the dimensions as of get_num_groups()
            BUILTIN_NUM_GROUPS = 1u << 8u,
            // The result is the group-ID in one of the dimensions as of get_group_id()
            BUILTIN_GROUP_ID = 1u << 9u,
            // The result is the global offset in one dimension as of get_global_offset()
            BUILTIN_GLOBAL_OFFSET = 1u << 10u,
            // The result is the global size in one dimension as of get_global_size()
            BUILTIN_GLOBAL_SIZE = 1u << 11u,
            // The result is the global id for one dimension (get_global_id())
            BUILTIN_GLOBAL_ID = 1u << 12u,
            // The result value is unsigned (signed by default)
            UNSIGNED_RESULT = 1u << 13u,
            // The result is a value of a PHI-node being set
            PHI_NODE = 1u << 14u,
            // The instructions inserts a single element into a vector
            ELEMENT_INSERTION = 1u << 16u,
            // The instruction was already processed by auto-vectorization
            AUTO_VECTORIZED = 1u << 17u,
            // The result of the instruction is the same for all work-items within a single work-group
            WORK_GROUP_UNIFORM_VALUE = 1u << 18u,
            // The instruction calculates VPM read configuration
            VPM_READ_CONFIGURATION = 1u << 19u,
            // The instruction calculates VPM write configuration
            VPM_WRITE_CONFIGURATION = 1u << 20u,
            // The behavior of the instruction in case of a signed integer overflow is undefined. Corresponds to LLVM
            // "nsw"
            SIGNED_OVERFLOW_IS_UB = 1u << 21u,
            // The behavior of the instruction of an unsigned overflow occurs. Corresponds to LLVM "nuw"
            UNSIGNED_OVERFLOW_IS_UB = 1u << 22u,
            // The division (signed or unsigned) or right shift (arithmetic or logical) is exact, e.g. there is no
            // remainder (for division) and no non-zero bits are shifted out of the value. I.e. for exact division (a /
            // b) * b == a and for shifts (a >> b) << b == a.
            EXACT_OPERATION = 1u << 23u,
            // An instruction which is located within a loop but does not depend on any calculate (or flag) done from
            // inside the loop. I.e. an instruction which calculates the same result for every loop iteration.
            // NOTE: The invariance marker does not distinguish between nested loops, so an instruction invariant for a
            // nested loop might not be invariant for its parent loop!
            LOOP_INVARIANT = 1u << 24u,
            // The instruction is part of the work-group-loop and not of the actual kernel body
            WORK_GROUP_LOOP = 1u << 25u
        };

        std::string toString(InstructionDecorations decoration);
        /**
         * Returns all decorations set in the given decorations which can be forwarded to a move of the output-value of
         * the decorated instruction E.g. if an output is unsigned, the value moved to another register is still
         * unsigned, same for all built-in flags. On the other side, a moved value is no longer a PHI-node instruction
         * or an element insertion
         */
        InstructionDecorations forwardDecorations(InstructionDecorations decorations);

        /**
         * Returns whether one of the decorations specified is one of the work-group builtins.
         *
         * This can be used to determine that the associated local can only have possible values.
         *
         * NOTE: This does only checks for the local IDs/sizes as well as the number of dimensions, if the additional
         * flag is set!
         */
        bool isGroupBuiltin(InstructionDecorations decorations, bool includeAll);

        /**
         * Types of side-effects an instruction can have
         *
         * This type is a bit-set for combination of side-effects!
         */
        enum class SideEffectType
        {
            NONE = 0,
            // Instruction reads a register which has side-effects, e.g. UNIFORM, TMU_OUT
            REGISTER_READ = 1u << 0u,
            // Instruction writes a register which has side-effects, e.g. SFU_XXX, VPM_IO
            REGISTER_WRITE = 1u << 1u,
            // Instructions writes flags
            FLAGS = 1u << 2u,
            // Instructions issues a signal with side-effects
            SIGNAL = 1u << 3u,
            // Instruction is a branch
            BRANCH = 1u << 4u,
            // Instruction writes a semaphore
            SEMAPHORE = 1u << 5u,
            // Instruction accesses memory
            MEMORY_ACCESS = 1u << 6u
        };

        /*
         * Converted to QPU instructions,
         * but still with method-calls and typed locals
         */
        class IntermediateInstruction
        {
        public:
            IntermediateInstruction(const IntermediateInstruction&) = delete;
            IntermediateInstruction(IntermediateInstruction&&) noexcept = delete;
            virtual ~IntermediateInstruction();

            IntermediateInstruction& operator=(const IntermediateInstruction&) = delete;
            IntermediateInstruction& operator=(IntermediateInstruction&&) = delete;

            bool operator==(const IntermediateInstruction& other) const;
            inline bool operator!=(const IntermediateInstruction& other) const
            {
                return !(*this == other);
            }

            virtual FastMap<const Local*, LocalUse::Type> getUsedLocals() const;
            virtual void forUsedLocals(
                const std::function<void(const Local*, LocalUse::Type, const IntermediateInstruction&)>& consumer)
                const;
            virtual bool readsLocal(const Local* local) const;
            virtual bool writesLocal(const Local* local) const;
            virtual void replaceLocal(
                const Local* oldLocal, const Local* newLocal, LocalUse::Type type = LocalUse::Type::BOTH);
            virtual void replaceLocal(
                const Local* oldLocal, const Value& newValue, LocalUse::Type type = LocalUse::Type::BOTH);

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
            /**
             * Whether this instruction reads any register
             */
            bool readsRegister() const;
            /*
             * Whether at least one of the operands of this instruction reads a local
             */
            bool readsLocal() const;
            /**
             * Returns ONE of the arguments that take the given register
             */
            const Value* findRegisterArgument(Register reg) const noexcept;
            /**
             * Returns ONE of the arguments that any register as input
             */
            const Value* findRegisterArgument() const noexcept;
            /**
             * Returns ONE of the arguments that takes a literal value
             */
            const Value* findLiteralArgument() const noexcept;
            /**
             * Returns ONE of the arguments that takes a local
             */
            const Value* findLocalArgument() const noexcept;

            virtual std::string to_string() const = 0;
            /*
             * Copies this instruction for the given method, renaming all locals by appending the local-prefix specified
             *
             * This function is used for inlining instructions
             */
            virtual NODISCARD IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const = 0;
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

            /**
             * Returns the register written by this instruction, if it writes a register
             */
            Optional<Register> checkOutputRegister() const noexcept;

            /**
             * Returns the local written by this instruction, if it writes a local
             */
            const Local* checkOutputLocal() const noexcept;

            /*
             * Returns the argument for the given index
             */
            Optional<Value> getArgument(std::size_t index) const;
            /*
             * Returns the argument for the given index.
             *
             * Throws an exception, if the index is not valid!
             */
            const Value& assertArgument(std::size_t index) const;
            /**
             * Returns the argument index of the given value, if it is read by this instruction
             */
            Optional<size_t> findArgument(const Value& val) const;
            /*
             * Lists all arguments/operands
             */
            const std::vector<Value>& getArguments() const;
            /**
             * Returns the other input value if the instruction takes exactly 2 input arguments and one of them is the
             * given value
             */
            Optional<Value> findOtherArgument(const Value& val) const;
            /*
             * Sets the argument for the given index to the value specified
             */
            void setArgument(std::size_t index, const Value& arg);
            void setArgument(std::size_t index, Value&& arg);

            IntermediateInstruction* setOutput(const Optional<Value>& output);
            IntermediateInstruction* setOutput(Optional<Value>&& output);
            IntermediateInstruction* addDecorations(InstructionDecorations decorations);
            bool hasDecoration(InstructionDecorations deco) const noexcept;

            /*
             * Whether this instruction has any side-effects.
             *
             * Side-effects include:
             * - accessing a register with side-effects (e.g. reading/writing hardware-mutex)
             * - triggering a signal (excluding ALU_IMMEDIATE, LOAD_IMMEDIATE signals)
             * - branches, semaphores
             * - setting of ALU flags
             */
            bool hasSideEffects() const;
            virtual SideEffectType getSideEffects() const;
            bool hasOtherSideEffects(SideEffectType ignoreEffects) const;

            /**
             * The signal that is fired, if any
             */
            Signaling getSignal() const;
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
            SetFlag getFlags() const;

            /*
             * Copies all the extras (signal, pack-modes, etc.) from the given instruction.
             *
             * NOTE: This function throws errors on merging incompatible extras (e.g. different non-default pack-modes)
             */
            virtual IntermediateInstruction* copyExtrasFrom(const IntermediateInstruction* src, bool skipSignal = false);
            /*
             * Tries to calculate the operation performed by this instruction and returns a constant value if
             * successful. The parameter numIterations determines the number of instructions providing the operands
             * (e.g. writing the local being read here) to use for determining the operand values
             *
             * NOTE: The constant value returned can be of value-type REGISTER, LITERAL, SMALL_IMMEDIATE or CONTAINER
             */
            virtual PrecalculatedValue precalculate(std::size_t numIterations = 1) const;

            bool replaceValue(const Value& oldValue, const Value& newValue, LocalUse::Type type);

            /* Determine constant instruction, such as
             * - load immediate instruction
             * - instruction whose all arguments are immediate value and which has output without side effect (its
             * result is immediate)
             */
            bool isConstantInstruction() const;

            InstructionDecorations decoration;

        protected:
            Signaling signal;
            Unpack unpackMode;
            Pack packMode;
            ConditionCode conditional;
            SetFlag setFlags;

            explicit IntermediateInstruction(Optional<Value>&& output = {});

            Value renameValue(
                Method& method, const Value& orig, const std::string& prefix, InlineMapping& localMapping) const;

            std::string createAdditionalInfoString() const;

            Optional<Value> getPrecalculatedValueForArg(std::size_t argIndex, std::size_t numIterations) const;

            virtual bool innerEquals(const IntermediateInstruction& other) const = 0;

        private:
            Optional<Value> output;
            std::vector<Value> arguments;

            void removeAsUserFromValue(const Value& value, LocalUse::Type type);
            void addAsUserToValue(const Value& value, LocalUse::Type type);
        };

        class SignalingInstruction : public IntermediateInstruction
        {
        public:
            SignalingInstruction* setSignaling(Signaling signal);

        protected:
            explicit SignalingInstruction(Signaling signal, Optional<Value>&& output = {});
        };

        class ExtendedInstruction : public SignalingInstruction
        {
        public:
            Pack getPackMode() const;
            ExtendedInstruction* setPackMode(Pack packMode);

            ConditionCode getCondition() const;
            ExtendedInstruction* setCondition(ConditionCode condition);

            ExtendedInstruction* setSetFlags(SetFlag setFlags);

        protected:
            explicit ExtendedInstruction(
                Signaling signal, ConditionCode cond, SetFlag setFlags, Pack packMode, Optional<Value>&& output = {});
        };

        class UnpackingInstruction : public ExtendedInstruction
        {
        public:
            Unpack getUnpackMode() const;
            UnpackingInstruction* setUnpackMode(Unpack unpackMode);

        protected:
            explicit UnpackingInstruction(Signaling signal, ConditionCode cond, SetFlag setFlags, Pack packMode,
                Unpack unpackMode, Optional<Value>&& output = {});
        };

        struct CombinedOperation;

        struct Operation final : public UnpackingInstruction
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
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
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

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        /*
         * Operation which cannot be mapped to VC4 machine code, but must be intrisified (e.g. integer
         * multiplications/divisions)
         */
        struct IntrinsicOperation : public IntermediateInstruction
        {
            IntrinsicOperation(std::string&& opCode, Value&& dest, Value&& arg0);
            IntrinsicOperation(std::string&& opCode, Value&& dest, Value&& arg0, Value&& arg1);
            ~IntrinsicOperation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Value& getFirstArg() const;
            const Optional<Value> getSecondArg() const;

            std::string opCode;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct MethodCall final : public IntermediateInstruction
        {
            explicit MethodCall(std::string&& methodName, std::vector<Value>&& args = {});
            MethodCall(Value&& dest, std::string&& methodName, std::vector<Value>&& args = {});
            ~MethodCall() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;

            const DataType getReturnType() const;

            bool matchesSignature(const Method& method) const;

            std::string methodName;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct Return final : public IntermediateInstruction
        {
            explicit Return();
            explicit Return(Value&& val);
            ~Return() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            Optional<Value> getReturnValue() const;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct MoveOperation : public UnpackingInstruction
        {
            MoveOperation(const Value& dest, const Value& arg, ConditionCode cond = COND_ALWAYS,
                SetFlag setFlags = SetFlag::DONT_SET);
            MoveOperation(
                Value&& dest, Value&& arg, ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            ~MoveOperation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
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

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        enum class RotationType : unsigned char
        {
            // Full vector rotation across all 16 SIMD elements, inputs must be accumulators
            // out[index] = input[(index + 16 - offset) % 16]
            // E.g. element_number << 2 -> [14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
            FULL,
            // Partial vector rotation within the four quads, inputs can be any register on physical file A
            // out[index] = input[(index / 4) * 4 + (index + 4 - offset) % 4]
            // e.g. element_number << 2 -> [2, 3, 0, 1, 6, 7, 4, 5, 11, 12, 8, 9, 15, 14, 12, 13]
            PER_QUAD,
            // This allows the usage of both full and partial vector rotation depending on the input register
            // NOTE: This is only valid if the offset is in [0, 3] for a single element which will be rotated from
            // element zero to element1, element2 or element3
            ANY
        };

        /**
         * A MUL ALU operation which does not calculate anything, but instead rotates the input vector
         *
         * Behavior:
         * - For full-range vector rotations, all inputs needs to be read from accumulators
         * - For per-quad vector rotation, inputs can also be located in physical registers
         * - In either case, the input register (physical or accumulator) must not be written in the instruction before
         * the vector rotation
         * - if rotated by r5 (e.g. dynamic offset), r5 must not be written in the instruction before the vector
         * rotation
         * - can output into any accumulator or physical register
         */
        struct VectorRotation final : public MoveOperation
        {
            VectorRotation(const Value& dest, const Value& src, const SmallImmediate& offset, RotationType type,
                ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            VectorRotation(Value&& dest, Value&& src, SmallImmediate&& offset, RotationType type,
                ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);
            ~VectorRotation() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            Operation* combineWith(const std::string& otherOpCode) const;
            PrecalculatedValue precalculate(std::size_t numIterations) const override;

            const SmallImmediate& getOffset() const;

            bool isSimpleMove() const override;
            bool isPerQuadRotationAllowed() const;
            bool isFullRotationAllowed() const;

            RotationType type;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct BranchLabel final : public IntermediateInstruction
        {
        public:
            explicit BranchLabel(const Local& label);
            ~BranchLabel() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Local* getLabel() const;
            Local* getLabel();

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct BranchCondition final : public SignalingInstruction
        {
            BranchCondition(const Value& cond, std::bitset<NATIVE_VECTOR_SIZE> elements = 0x1);
            ~BranchCondition() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;
            SideEffectType getSideEffects() const override;

            const Value& getBranchCondition() const;

            /**
             * The SIMD elements (defaults to element zero) which will be expanded to determine whether the branch is
             * taken. In other words: All other SIMD elements will be ignored (masked off) when the branch condition is
             * calculated.
             */
            std::bitset<NATIVE_VECTOR_SIZE> conditionalElements;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct Branch final : public SignalingInstruction
        {
            explicit Branch(const Local* target);
            Branch(const Local* target, BranchCond branchCond);
            ~Branch() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;
            SideEffectType getSideEffects() const override;

            const Local* getTarget() const;

            bool isUnconditional() const;

            BranchCond branchCondition;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
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

        struct Nop final : public SignalingInstruction
        {
        public:
            explicit Nop(DelayType type, Signaling signal = SIGNAL_NONE);
            ~Nop() override = default;

            std::string to_string() const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            bool isNormalized() const override;

            DelayType type;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
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

            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
        };

        struct CombinedOperation final : public IntermediateInstruction
        {
        public:
            CombinedOperation(Operation* op1, Operation* op2);
            ~CombinedOperation() override = default;

            FastMap<const Local*, LocalUse::Type> getUsedLocals() const override;
            void forUsedLocals(
                const std::function<void(const Local*, LocalUse::Type, const IntermediateInstruction&)>& consumer)
                const override;
            bool readsLocal(const Local* local) const override;
            bool writesLocal(const Local* local) const override;
            void replaceLocal(const Local* oldLocal, const Local* newLocal, LocalUse::Type type) override;

            std::string to_string() const override;

            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;
            SideEffectType getSideEffects() const override;

            const Operation* getFirstOp() const;
            const Operation* getSecondOP() const;

            const std::unique_ptr<IntermediateInstruction> op1;
            const std::unique_ptr<IntermediateInstruction> op2;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
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
        struct LoadImmediate final : public ExtendedInstruction
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
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool isNormalized() const override;

            PrecalculatedValue precalculate(std::size_t numIterations) const override;

            Literal getImmediate() const;
            void setImmediate(const Literal& value, DataType type = TYPE_INT32);

            const LoadType type;

            static SIMDVector toLoadedValues(uint32_t mask, LoadType type);
            static uint32_t fromLoadedValues(const SIMDVector& values, LoadType type);

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
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
        struct SemaphoreAdjustment final : public ExtendedInstruction
        {
        public:
            SemaphoreAdjustment(Semaphore semaphore, bool increase);
            ~SemaphoreAdjustment() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool isNormalized() const override;
            SideEffectType getSideEffects() const override;

            const Semaphore semaphore;
            const bool increase;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        struct PhiNode final : public IntermediateInstruction
        {
        public:
            PhiNode(Value&& dest, std::vector<std::pair<Value, const Local*>>&& labelPairs);
            ~PhiNode() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool isNormalized() const override;

            FastMap<const Local*, Value> getValuesForLabels() const;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
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
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            MemoryScope scope;
            MemorySemantics semantics;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
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
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool mapsToASMInstruction() const override;
            bool isNormalized() const override;

            const Value& getStackAllocation() const;

            bool isLifetimeEnd;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };

        enum class MutexAccess : unsigned char
        {
            LOCK,
            RELEASE
        };

        /*
         * Instruction accessing (locking/unlocking) the hardware-mutex
         */
        struct MutexLock final : SignalingInstruction
        {
        public:
            explicit MutexLock(MutexAccess accessType);
            ~MutexLock() override = default;

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool isNormalized() const override;
            SideEffectType getSideEffects() const override;

            bool locksMutex() const;
            bool releasesMutex() const;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;

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
            MemoryInstruction(MemoryOperation op, Value&& dest, Value&& src, Value&& numEntries = Value(INT_ONE),
                bool useMutex = true);

            std::string to_string() const override;
            qpu_asm::DecoratedInstruction convertToAsm(const FastMap<const Local*, Register>& registerMapping,
                const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const override;
            IntermediateInstruction* copyFor(
                Method& method, const std::string& localPrefix, InlineMapping& localMapping) const override;
            bool isNormalized() const override;
            SideEffectType getSideEffects() const override;

            const Value& getSource() const;
            const Value& getDestination() const;
            const Value& getNumEntries() const;

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
            const bool guardAccess;

        protected:
            bool innerEquals(const IntermediateInstruction& other) const override;
        };
    } // namespace intermediate
} // namespace vc4c

#endif /* INTERMEDIATEINSTRUCTION_H */
