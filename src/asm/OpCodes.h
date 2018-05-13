/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPCODES_H
#define OPCODES_H

#include "../Bitfield.h"
#include "Optional.h"

#include <algorithm>
#include <functional>
#include <limits>
#include <string>

namespace vc4c
{
    class DataType;
    struct Value;
    struct OpCode;
    enum class BranchCond : unsigned char;

    template <typename T,
        typename R = typename std::conditional<std::numeric_limits<T>::is_signed, int32_t, uint32_t>::type>
    R saturate(int64_t val)
    {
        return static_cast<int32_t>(std::min(std::max(val, static_cast<int64_t>(std::numeric_limits<T>::min())),
            static_cast<int64_t>(std::numeric_limits<T>::max())));
    }

    inline float saturate(double val)
    {
        return static_cast<float>(std::min(std::max(val, static_cast<double>(std::numeric_limits<float>::lowest())),
            static_cast<double>(std::numeric_limits<float>::max())));
    }

    /*!
     * The QPU keeps a set of N, Z and C flag bits per 16 SIMD element.
     * These flags are updated based on the result of the ADD ALU if the 'sf' bit is set.
     * If the sf bit is set and the ADD ALU executes a NOP or its condition code was NEVER,
     * flags are set based upon the result of the MUL ALU result.
     *
     * page 28
     */
    struct ConditionCode : public InstructionPart
    {
        explicit constexpr ConditionCode(unsigned char val) noexcept : InstructionPart(val) {}

        std::string to_string() const;
        ConditionCode invert() const;
        bool isInversionOf(ConditionCode other) const;
        BranchCond toBranchCondition() const;
    };

    /*
     * Instruction is never executed
     */
    constexpr ConditionCode COND_NEVER{0};
    /*
     * Instruction is always executed
     */
    constexpr ConditionCode COND_ALWAYS{1};
    /*
     * Execute instruction iff zero (Z) flag is set
     */
    constexpr ConditionCode COND_ZERO_SET{2};
    /*
     * Execute instruction iff zero (Z) flag is not set
     */
    constexpr ConditionCode COND_ZERO_CLEAR{3};
    /*
     * Execute instruction iff negative (N) flag is set
     *
     * NOTE: checks for negative flag set only work correctly on 32-bit values! Since for other values, the 31th bit may
     * not be set!
     */
    constexpr ConditionCode COND_NEGATIVE_SET{4};
    /*
     * Execute instruction iff negative (N) flag is not set
     *
     * NOTE: checks for negative flag set only work correctly on 32-bit values! Since for other values, the 31th bit may
     * not be set!
     */
    constexpr ConditionCode COND_NEGATIVE_CLEAR{5};
    /*
     * Execute instruction iff carry (C) flag is set
     */
    constexpr ConditionCode COND_CARRY_SET{6};
    /*
     * Execute instruction iff carry (C) flag is not set
     */
    constexpr ConditionCode COND_CARRY_CLEAR{7};

    /*!
     * The add_a, add_b, mul_a, and mul_b fields specify the input data for the A and B ports of the ADD and MUL
     * pipelines, respectively.
     *
     * page 28
     */
    enum class InputMultiplex : unsigned char
    {
        // use accumulator r0
        ACC0 = 0,
        // use accumulator r1
        ACC1 = 1,
        // use accumulator r2
        ACC2 = 2,
        // use accumulator r3
        ACC3 = 3,
        // use accumulator r4. Has special function, cannot be used for general-purpose
        ACC4 = 4,
        // use accumulator r5. Has special function, cannot be used for general-purpose
        ACC5 = 5,
        // use value from register file A
        REGA = 6,
        // use value from register file B
        REGB = 7
    };
    constexpr InputMultiplex MULTIPLEX_NONE{InputMultiplex::ACC0};
    constexpr InputMultiplex MULTIPLEX_IMMEDIATE{InputMultiplex::REGB};

    /*!
     * The 4-bit signaling field signal is connected to the 3d pipeline and is set to indicate one
     * of a number of conditions to the 3d hardware. Values from this field are also used to encode a 'BKPT'
     * instruction, and to encode Branches and Load Immediate instructions.
     *
     * page 29
     */
    struct Signaling : public InstructionPart
    {
        explicit constexpr Signaling(unsigned char val) noexcept : InstructionPart(val) {}

        std::string to_string() const;
        bool hasSideEffects() const;
        bool triggersReadOfR4() const;
    };

    /*
     * Software breakpoint
     */
    constexpr Signaling SIGNAL_SOFT_BREAK{0};
    /*
     * Trigger no signal
     */
    constexpr Signaling SIGNAL_NONE{1};
    /*
     * Last execution before thread switch
     */
    constexpr Signaling SIGNAL_SWITCH_THREAD{2};
    /*
     * Last execution
     */
    constexpr Signaling SIGNAL_END_PROGRAM{3};
    /*
     * Wait for scoreboard - stall until this QPU can safely access tile buffer
     *
     * "The explicit Wait for Scoreboard signal (4) is not required in most fragment shaders,
     * because the QPU will implicitly wait for the scoreboard on the first instruction that accesses the tile buffer."
     */
    constexpr Signaling SIGNAL_WAIT_FOR_SCORE{4};
    /*
     * Scoreboard unlock
     */
    constexpr Signaling SIGNAL_UNLOCK_SCORE{5};
    /*
     * Last Thread Switch
     */
    constexpr Signaling SIGNAL_THREAD_SWITCH_LAST{6};
    /*
     * Coverage load from tile buffer to r4
     */
    constexpr Signaling SIGNAL_LOAD_COVERAGE{7};
    /*
     * Color load from tile buffer to r4
     */
    constexpr Signaling SIGNAL_LOAD_COLOR{8};
    /*
     * Color load and program end
     */
    constexpr Signaling SIGNAL_LOAD_COLOR_END{9};
    /*
     * Trigger read data from TMU0 to r4
     */
    constexpr Signaling SIGNAL_LOAD_TMU0{10};
    /*
     * Trigger read data from TMU1 to r4
     */
    constexpr Signaling SIGNAL_LOAD_TMU1{11};
    /*
     * Alpha-mask load from tile buffer to r4
     */
    constexpr Signaling SIGNAL_LOAD_ALPHA{12};
    /*
     * ALU instruction with raddr_b specifying small immediate or vector rotate
     */
    constexpr Signaling SIGNAL_ALU_IMMEDIATE{13};
    /*
     * Load immediate instruction
     */
    constexpr Signaling SIGNAL_LOAD_IMMEDIATE{14};
    /*
     * Branch instruction
     */
    constexpr Signaling SIGNAL_BRANCH{15};

    /*!
     * ALU instructions can unpack their operands before executing the operation from packed storage-formats.
     *
     * Normally, the Pack and Unpack fields program the A register file pack/unpack blocks.
     * The A-regfile unpack block will convert packed 8 or 16 bit data to 32 bit values ready for use by the ALUs.
     * Similarly the a-regfile pack block allows the 32-bit ALU result to be packed back into the a-regfile as 8 or 16
     * bit data. As well as the a-regfile pack and unpack units, accumulator r4 has a more limited unpack unit which can
     * be used to unpack the color values returned by the tile buffer and texture unit. Finally, the mul ALU has the
     * ability to convert its floating point result to 8-bit color c: c = sat[round(f * 255)] (sat saturates to [255,
     * 0]) If the pm (MSB) bit is set, the unpack field programs the r4 unpack unit, and the pack field is used to
     * program the color conversion on the output of the mul unit (that is, enable the conversion and program which byte
     * in the destination regfile/accumulator to write the result to).
     *
     * page 31
     */
    struct Unpack : public InstructionPart
    {
        explicit constexpr Unpack(unsigned char val) noexcept : InstructionPart(val) {}

        std::string to_string() const;

        Optional<Value> unpack(const Value& val) const;
        bool handlesFloat(const OpCode& opCode) const;

        static const Unpack unpackTo32Bit(const DataType& type);
    };

    /*
     * Do not unpack data
     */
    constexpr Unpack UNPACK_NOP{0};
    /*
     * Float16 (lower half) -> float32 if any ALU consuming data executes float instruction, else signed int16 -> signed
     * int32
     */
    constexpr Unpack UNPACK_16A_32{1};
    /*
     * Float16 (upper half) -> float32 if any ALU consuming data executes float instruction, else signed int16 -> signed
     * int32
     */
    constexpr Unpack UNPACK_16B_32{2};
    /*
     * Replicate MSB (alpha) across word: result = {8d, 8d, 8d, 8d}
     */
    constexpr Unpack UNPACK_8888_32{3};
    /*
     * 8-bit color value (in range [0, 1.0]) from byte 0 (LSB) to 32 bit float if any ALU consuming data executes float
     * instruction, else unsigned int8 -> int32
     */
    constexpr Unpack UNPACK_8A_32{4};
    /*
     * 8-bit color value (in range [0, 1.0]) from byte 1 to 32 bit float if any ALU consuming data executes float
     * instruction, else unsigned int8 -> int32
     */
    constexpr Unpack UNPACK_8B_32{5};
    /*
     * 8-bit color value (in range [0, 1.0]) from byte 2 to 32 bit float if any ALU consuming data executes float
     * instruction, else unsigned int8 -> int32
     */
    constexpr Unpack UNPACK_8C_32{6};
    /*
     * 8-bit color value (in range [0, 1.0]) from byte 3 (MSB) to 32 bit float if any ALU consuming data executes float
     * instruction, else unsigned int8 -> int32
     */
    constexpr Unpack UNPACK_8D_32{7};
    constexpr Unpack UNPACK_SHORT_TO_INT_SEXT = UNPACK_16A_32;
    constexpr Unpack UNPACK_HALF_TO_FLOAT = UNPACK_16A_32;
    constexpr Unpack UNPACK_CHAR_TO_INT_ZEXT = UNPACK_8A_32;

    /*
     * 8-bit color value (in range [0, 1.0]) to 32 bit float
     */
    constexpr Unpack UNPACK_R4_COLOR0 = UNPACK_8A_32;
    constexpr Unpack UNPACK_R4_COLOR1 = UNPACK_8B_32;
    constexpr Unpack UNPACK_R4_COLOR2 = UNPACK_8C_32;
    constexpr Unpack UNPACK_R4_COLOR3 = UNPACK_8D_32;

    /*
     * ALU instructions can also pack their results back into packed storage-formats.
     */
    struct Pack : public InstructionPart
    {
        explicit constexpr Pack(unsigned char val) noexcept : InstructionPart(val) {}

        std::string to_string() const;

        Optional<Value> pack(const Value& val) const;
        bool handlesFloat(const OpCode& opCode) const;
    };

    /*
     * Do not pack data
     */
    constexpr Pack PACK_NOP{0};
    /*
     * Convert to 16 bit float if input was float result, else convert to int16 (no saturation, just take ls 16 bits)
     * and copy into lower half
     */
    constexpr Pack PACK_32_16A{1};
    /*
     * Convert to 16 bit float if input was float result, else convert to int16 (no saturation, just take ls 16 bits)
     * and copy into higher half
     */
    constexpr Pack PACK_32_16B{2};
    /*
     * Convert to 8-bit unsigned int (no saturation, just take LSB) and replicate across all bytes of 32-bit word
     */
    constexpr Pack PACK_32_8888{3};
    /*
     * Convert to 8-bit unsigned int (no saturation, just take LSB) and copy into byte 0 (LSB)
     */
    constexpr Pack PACK_32_8A{4};
    /*
     * Convert to 8-bit unsigned int (no saturation, just take LSB) and copy into byte 1
     */
    constexpr Pack PACK_32_8B{5};
    /*
     * Convert to 8-bit unsigned int (no saturation, just take LSB) and copy into byte 2
     */
    constexpr Pack PACK_32_8C{6};
    /*
     * Convert to 8-bit unsigned int (no saturation, just take LSB) and copy into byte 3 (MSB)
     */
    constexpr Pack PACK_32_8D{7};
    /*
     * Saturate (signed) 32-bit number (given overflow/carry flags)
     */
    constexpr Pack PACK_32_32{8};
    /*
     * Convert to 16 bit float if input was float result, else convert to signed 16 bit integer (with saturation) and
     * copy into lower half
     */
    constexpr Pack PACK_32_16A_S{9};
    /*
     * Convert to 16 bit float if input was float result, else convert to signed 16 bit integer (with saturation) and
     * copy into higher half
     */
    constexpr Pack PACK_32_16B_S{10};
    /*
     * Saturate to 8-bit unsigned int and replicate across all bytes of 32-bit word
     */
    constexpr Pack PACK_32_8888_S{11};
    /*
     * Saturate to 8-bit unsigned int and copy into byte 0 (LSB)
     */
    constexpr Pack PACK_32_8A_S{12};
    /*
     * Saturate to 8-bit unsigned int and copy into byte 1
     */
    constexpr Pack PACK_32_8B_S{13};
    /*
     * Saturate to 8-bit unsigned int and copy into byte 2
     */
    constexpr Pack PACK_32_8C_S{14};
    /*
     * Saturate to 8-bit unsigned int and copy into byte 3(MSB)
     */
    constexpr Pack PACK_32_8D_S{15};

    constexpr Pack PACK_INT_TO_SHORT_TRUNCATE = PACK_32_16A;
    constexpr Pack PACK_FLOAT_TO_HALF_TRUNCATE = PACK_32_16A;
    constexpr Pack PACK_INT_TO_SIGNED_SHORT_SATURATE = PACK_32_16A_S;
    constexpr Pack PACK_FLOAT_TO_HALF_SATURATE = PACK_32_16A_S;
    constexpr Pack PACK_INT_TO_CHAR_TRUNCATE = PACK_32_8A;
    constexpr Pack PACK_INT_TO_UNSIGNED_CHAR_SATURATE = PACK_32_8A_S;

    /*
     * Convert mul float result to 8-bit color in range [0, 1.0]
     */
    constexpr Pack PACK_MUL_COLOR0 = PACK_32_8A;
    constexpr Pack PACK_MUL_COLOR1 = PACK_32_8B;
    constexpr Pack PACK_MUL_COLOR2 = PACK_32_8C;
    constexpr Pack PACK_MUL_COLOR3 = PACK_32_8D;

    /*!
     * Flags are updated from the add ALU unless the add ALU performed a NOP
     * (or its condition code was NEVER) in which case flags are updated from the mul ALU
     *
     * page 27
     */
    enum class SetFlag : unsigned char
    {
        DONT_SET = 0,
        SET_FLAGS = 1
    };
    std::string toString(SetFlag flag);

    /*!
     * Write swap for add and multiply unit outputs
     *
     * page 27
     */
    enum class WriteSwap : unsigned char
    {
        /*
         * Add ALU writes to regfile A, mult to regfile B
         */
        DONT_SWAP = 0,
        /*
         * Add ALU writes to regfile B, mult to regfile A
         */
        SWAP = 1
    };

    /*
     * The operation-code being executed by ALU instructions.
     *
     * Most of the op-codes can only be executed on one of the ALUs, with a few exceptions.
     */
    struct OpCode
    {
        /*
         * The name of the op-code
         */
        const char* name;
        /*
         * The op-code for execution on the add ALU
         */
        unsigned char opAdd;
        /*
         * The op-code for execution on the mul ALU
         */
        unsigned char opMul;
        /*
         * The number of operands the operation takes
         */
        unsigned char numOperands;
        /*
         * Whether the operation accepts floating-point operands
         */
        bool acceptsFloat;
        /*
         * Whether the operation returns a floating-point value
         */
        bool returnsFloat;

        constexpr OpCode(const char* name, unsigned char opAdd, unsigned char opMul, unsigned char numOperands,
            bool acceptsFloat, bool returnsFloat) noexcept :
            name(name),
            opAdd(opAdd), opMul(opMul), numOperands(numOperands), acceptsFloat(acceptsFloat), returnsFloat(returnsFloat)
        {
        }

        bool operator==(const OpCode& right) const;
        bool operator!=(const OpCode& right) const;
        bool operator<(const OpCode& right) const;

        /*
         * Whether the op-code can be executed on the add ALU
         */
        constexpr bool runsOnAddALU() const
        {
            return opAdd != 0;
        }

        /*
         * Whether the op-code can be executed on the mul ALU
         */
        constexpr bool runsOnMulALU() const
        {
            return opMul != 0;
        }

        /*
         * Tries to calculate the operation for this op-code with the operands given
         */
        Optional<Value> calculate(const Optional<Value>& firstOperand, const Optional<Value>& secondOperand) const;
        Optional<Value> operator()(const Optional<Value>& firstOperand, const Optional<Value>& secondOperand) const;

        /*
         * Whether the operation is idempotent.
         *
         * Idempotent is defined as:
         * - for unary operations:
         *   applying the operation multiple times yields the same result as applying it once (e.g. fabs)
         * - for binary operations:
         *   for any operands, if both operands are the same value, the result is this value (e.g. and, or)
         *
         * These definitions are taken from:
         * https://en.wikipedia.org/wiki/Idempotence
         */
        bool isIdempotent() const;

        /*
         * Returns the op-code for the given op-code name.
         *
         * Throws an exception if the op-code could not be found.
         */
        static const OpCode& toOpCode(const std::string& name);
        /*
         * Returns the op-code for the given code and whether the code is for the add or the mul ALU
         */
        static const OpCode& toOpCode(unsigned char opcode, bool isMulALU);
        /*
         * Similar to #toOpCode, but returns OP_NUL of op-code is not a valid machine code instruction, instead of
         * throwing an exception
         */
        static const OpCode& findOpCode(const std::string& name);

        /*
         * Returns the left-identity value for the given op-code
         *
         * The left-identity is a value used as the left operand, which results in the result being the right operand
         * (e.g. 0 for additions, 1 for multiplications)
         */
        static Optional<Value> getLeftIdentity(const OpCode& code);
        /*
         * Returns the right-identity value for the given op-code
         *
         * The right-identity is a value used as the right operand, which results in the result being the left operand
         * (e.g. all-bits set for AND, 1 for multiplications)
         */
        static Optional<Value> getRightIdentity(const OpCode& code);
        /*
         * Returns the left absorbing element for the given op-code
         *
         * The left absorbing element is a value used as the left operand, which results in the result being the
         * absorbing element (e.g. 0 for multiplications and for and)
         */
        static Optional<Value> getLeftAbsorbingElement(const OpCode code);
        /*
         * Returns the right absorbing element for the given op-code
         *
         * The right absorbing element is a value used as the right operand, which results in the result being the
         * absorbing element (e.g. 0 for multiplications and for and)
         */
        static Optional<Value> getRightAbsorbingElement(const OpCode code);
    };

    /*
     * A no-op on both ALUs
     */
    static constexpr OpCode OP_NOP{"nop", 0, 0, 0, false, false};
    /*
     * Floating-point addition
     */
    static constexpr OpCode OP_FADD{"fadd", 1, 0, 2, true, true};
    /*
     * Floating-point subtraction
     */
    static constexpr OpCode OP_FSUB{"fsub", 2, 0, 2, true, true};
    /*
     * Floating-point minimum function
     */
    static constexpr OpCode OP_FMIN{"fmin", 3, 0, 2, true, true};
    /*
     * Floating-point maximum
     */
    static constexpr OpCode OP_FMAX{"fmax", 4, 0, 2, true, true};
    /*
     * Floating-point minimum of absolute values
     */
    static constexpr OpCode OP_FMINABS{"fminabs", 5, 0, 2, true, true};
    /*
     * Floating-point maximum of absolute values
     */
    static constexpr OpCode OP_FMAXABS{"fmaxabs", 6, 0, 2, true, true};
    /*
     * Converts floating-point to signed integer
     */
    static constexpr OpCode OP_FTOI{"ftoi", 7, 0, 1, true, false};
    /*
     * Converts Signed integer to floating-point
     */
    static constexpr OpCode OP_ITOF{"itof", 8, 0, 1, false, true};
    /*
     * Integer addition
     */
    static constexpr OpCode OP_ADD{"add", 12, 0, 2, false, false};
    /*
     * Integer subtraction
     */
    static constexpr OpCode OP_SUB{"sub", 13, 0, 2, false, false};
    /*
     * Integer right shift (unsigned)
     */
    static constexpr OpCode OP_SHR{"shr", 14, 0, 2, false, false};
    /*
     * Integer arithmetic right shift (signed)
     */
    static constexpr OpCode OP_ASR{"asr", 15, 0, 2, false, false};
    /*
     * Integer rotate right
     */
    static constexpr OpCode OP_ROR{"ror", 16, 0, 2, false, false};
    /*
     * Integer left shift
     */
    static constexpr OpCode OP_SHL{"shl", 17, 0, 2, false, false};
    /*
     * Integer minimum function
     */
    static constexpr OpCode OP_MIN{"min", 18, 0, 2, false, false};
    /*
     * Integer maximum function
     */
    static constexpr OpCode OP_MAX{"max", 19, 0, 2, false, false};
    /*
     * Bitwise AND
     */
    static constexpr OpCode OP_AND{"and", 20, 0, 2, false, false};
    /*
     * Bitwise OR
     */
    static constexpr OpCode OP_OR{"or", 21, 0, 2, false, false};
    /*
     * Bitwise XOR
     */
    static constexpr OpCode OP_XOR{"xor", 22, 0, 2, false, false};
    /*
     * Bitwise unary NOT
     */
    static constexpr OpCode OP_NOT{"not", 23, 0, 1, false, false};
    /*
     * Count leading zeroes
     */
    static constexpr OpCode OP_CLZ{"clz", 24, 0, 1, false, false};
    /*
     * Integer addition with saturation per 8-bit element
     */
    static constexpr OpCode OP_V8ADDS{"v8adds", 30, 6, 2, false, false};
    /*
     * Integer subtraction with saturation per 8-bit element
     */
    static constexpr OpCode OP_V8SUBS{"v8subs", 31, 7, 2, false, false};
    /*
     * Floating-point multiplication
     */
    static constexpr OpCode OP_FMUL{"fmul", 0, 1, 2, true, true};
    /*
     * 24-bit integer multiplication
     */
    static constexpr OpCode OP_MUL24{"mul24", 0, 2, 2, false, false};
    /*
     * Multiply two vectors of 8-bit values in the range [1.0, 0]
     */
    static constexpr OpCode OP_V8MULD{"v8muld", 0, 3, 2, false, false};
    /*
     * Integer minimum value per 8-bit element
     */
    static constexpr OpCode OP_V8MIN{"v8min", 0, 4, 2, false, false};
    /*
     * Integer maximum value per 8-bit element
     */
    static constexpr OpCode OP_V8MAX{"v8max", 0, 5, 2, false, false};

    /*!
     * Loads a 32-bit constant value into the output register
     *
     * The load immediate instructions can be used to write either a 32-bit immediate across the entire SIMD array,
     * or 16 individual 2-bit (signed or unsigned integer) values per-element.
     *
     * The encoding contains identical fields to the ALU instructions in the upper 32-bits,
     * while the lower 32 bits contain the immediate value(s) instead of the add and mul opcodes and read/mux fields.
     *
     * When a load immediate instruction is encountered, the processor feeds the immediate value into the add and
     * mul pipes and sets them to perform a 'mov'. The immediate value turns up at the output of the ALUs as if it
     * were just a normal arithmetic result and hence all of the write fields, conditions and modes (specified in the
     * upper 32-bits of the encoding) work just as they would for a normal ALU instruction.
     *
     * page 33
     */
    enum class OpLoad : unsigned char
    {
        /*
         * Write a 32-bit immediate across the entire SIMD array
         */
        LOAD_IMM_32 = 0b01110000,
        /*
         * Write 16 individual 2-bit (signed) values per-element
         */
        LOAD_SIGNED = 0b01110001,
        /*
         * Write 16 individual 2-bit (unsigned) values per-element
         */
        LOAD_UNSIGNED = 0b01110011
    };

    /*!
     * Increments/decrements a 4-bit counting semaphore.
     *
     * The dedicated semaphore instruction provides each QPU with access to one of 16 system wide 4-bit counting
     * semaphores. The semaphore accessed is selected by the 4-bit semaphore field. The semaphore is incremented if sa
     * is 0 and decremented if sa is 1. The QPU stalls if it is attempting to decrement a semaphore below 0 or increment
     * it above 15. The QPU may also stall briefly during arbitration access to the semaphore. The instruction otherwise
     * behaves like a 32-bit load immediate instruction, so the ALU outputs will not generally be useful.
     *
     * page 33
     */
    enum class OpSemaphore : unsigned char
    {
        SEMAPHORE = 0b01110100
    };

    /*
     * The semaphore values and their usages
     */
    enum class Semaphore
    {
        BARRIER_WORK_ITEM_0 = 0,
        BARRIER_WORK_ITEM_1 = 1,
        BARRIER_WORK_ITEM_2 = 2,
        BARRIER_WORK_ITEM_3 = 3,
        BARRIER_WORK_ITEM_4 = 4,
        BARRIER_WORK_ITEM_5 = 5,
        BARRIER_WORK_ITEM_6 = 6,
        BARRIER_WORK_ITEM_7 = 7,
        BARRIER_WORK_ITEM_8 = 8,
        BARRIER_WORK_ITEM_9 = 9,
        BARRIER_WORK_ITEM_10 = 10,
        BARRIER_WORK_ITEM_11 = 11,
        BARRIER_SFU_SLICE_0 = 12,
        BARRIER_SFU_SLICE_1 = 13,
        BARRIER_SFU_SLICE_2 = 14,
        BARRIER_SFU_SLICE_3 = 15
    };

    /*!
     * A branch-instruction moves the program counter (PC) to a new position in code
     *
     * QPU branches are conditional based on the status of the ALU flag bits across all 16 elements of the SIMD array.
     * If a branch condition is satisfied, a new program counter value is calculated as the sum of the (signed)
     * immediate field, the current PC+4 (if the rel bit is set) and the value read from the a register file SIMD
     * element 0 (if the reg bit is set).
     *
     * On branch instructions the link address (the current instruction plus four) appears at the output of the add and
     * mul ALUs (in the same way that immediates are passed through these units for load immediate instructions),
     * and therefore may be written to a register-file location to support branch-with-link functionality.
     *
     * For simplicity, the QPUs do not use branch prediction and never cancel the sequentially fetched instructions
     * when a branch is encountered. This means that three 'delay slot' instructions following a branch instruction
     * are always executed.
     *
     * page 34
     */
    enum class OpBranch : unsigned char
    {
        BRANCH = 15
    };

    /*
     * The branch-condition on which a conditional branch is taken.
     *
     * NOTE: The condition for a branch is dependent the flags of all SIMD-elements.
     */
    enum class BranchCond : unsigned char
    {
        /*
         * All Z flags set - &{Z[15:0]}
         */
        ALL_Z_SET = 0,
        /*
         * All Z flags clear - &{~Z[15:0]}
         */
        ALL_Z_CLEAR = 1,
        /*
         * Any Z flags set - |{Z[15:0]}
         */
        ANY_Z_SET = 2,
        /*
         * Any Z flags clear - |{~Z[15:0]}
         */
        ANY_Z_CLEAR = 3,
        /*
         * All N flags set - &{N[15:0]}
         */
        ALL_N_SET = 4,
        /*
         * All N flags clear - &{~N[15:0]}
         */
        ALL_N_CLEAR = 5,
        /*
         * Any N flags set - |{N[15:0]}
         */
        ANY_N_SET = 6,
        /*
         * Any N flags clear - |{~N[15:0]}
         */
        ANY_N_CLEAR = 7,
        /*
         * All C flags set - &{C[15:0]}
         */
        ALL_C_SET = 8,
        /*
         * All C flags clear - &{~C[15:0]}
         */
        ALL_C_CLEAR = 9,
        /*
         * Any C flags set - |{C[15:0]}
         */
        ANY_C_SET = 10,
        /*
         * Any C flags clear - |{~C[15:0]}
         */
        ANY_C_CLEAR = 11,
        // RESERVED 12 - 14
        /*
         * Always execute (unconditional)
         */
        ALWAYS = 15
    };
    std::string toString(BranchCond cond);

    /*
     * The way how the branch address is treated
     *
     * NOTE: Since the base address of the code block is not known at compile-time,
     * the branches taken by the machine code generated by this compiler are always relative.
     */
    enum class BranchRel : unsigned char
    {
        /*
         * Absolute branch, use the address as absolute address
         */
        BRANCH_ABSOLUTE = 0,
        /*
         * Relative branch, the address passed to the branch-condition is relative to PC+4 (adds PC+4 to target)
         */
        BRANCH_RELATIVE = 1
    };

    /*
     * The way to treat the register passed to the branch-instruction
     */
    enum class BranchReg : unsigned char
    {
        /*
         * Do not use the register-value passed
         */
        NONE = 0,
        /*
         * Add value of raddr_a (value read from SIMD element 0) to branch target.
         */
        BRANCH_REG = 1
    };

    /*
     * Type representing a register-address on a physical register-file
     */
    using Address = uint8_t;
} // namespace vc4c

#endif /* OPCODES_H */
