/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VALUES_H
#define VALUES_H

#include "BitMask.h"
#include "Bitfield.h"
#include "Optional.h"
#include "Types.h"
#include "Variant.h"
#include "config.h"
#include "performance.h"

#include <algorithm>
#include <array>

namespace vc4c
{
    /*
     * A physical register-file
     */
    enum class RegisterFile : unsigned char
    {
        NONE = 0,
        PHYSICAL_A = 1,
        PHYSICAL_B = 2,
        PHYSICAL_ANY = 3,
        ACCUMULATOR = 4,
        ANY = 7
    };

    std::string toString(RegisterFile file);
    /*
     * Whether the argument is fixed to a single register-file
     */
    CONST bool isFixed(RegisterFile file) noexcept;

    /*
     * Represents a single hardware-register.
     */
    struct Register
    {
        /*
         * The register-file of this register
         */
        RegisterFile file;
        /*
         * The physical register-number, as specified in the Broadcom VideoCore IV specification
         */
        unsigned char num;

        constexpr Register(RegisterFile file, unsigned char num) noexcept : file(file), num(num) {}

        std::string to_string(bool specialNames = true, bool readAccess = true) const;

        /*
         * Returns the accumulator-number for the physical register, if it represents an accumulator. Returns -1
         * otherwise.
         */
        int getAccumulatorNumber() const noexcept;

        bool operator<(Register right) const noexcept;
        bool operator==(Register right) const noexcept;
        bool operator!=(Register right) const noexcept
        {
            return !(*this == right);
        }

        /*
         * Whether this is a general purpose register.
         *
         * The register-numbers 0 - 31 in both physical register-files are general purpose registers.
         */
        bool isGeneralPurpose() const noexcept;

        /*
         * Whether this register is an accumulator.
         */
        bool isAccumulator() const noexcept;
        /*
         * Whether this register is a periphery-registers to access the tile-buffer (TLB)
         */
        bool isTileBuffer() const noexcept;
        /*
         * Whether this register is used to access the VPM
         */
        bool isVertexPipelineMemory() const noexcept;
        /*
         * Whether the register accesses the SFU
         */
        bool isSpecialFunctionsUnit() const noexcept;
        /*
         * Whether the register accesses the TMU periphery
         */
        bool isTextureMemoryUnit() const noexcept;
        /*
         * Whether reading this register has side-effects.
         *
         * Side-effects reading a register include:
         * - auto-incrementing some hardware-internal counter (e.g. reading UNIFORMs, reading from VPM)
         * - locking the hardware-mutex
         * - blocking until an operation has finished (e.g. reading VPM_WAIT)
         */
        bool hasSideEffectsOnRead() const noexcept;
        /*
         * Whether writing this register has side-effects.
         *
         * Side-effects writing a register include:
         * - writing to VPM/memory
         * - releasing the hardware-mutex
         * - writing configuration-registers for periphery (e.g. vpw_setup, tmu_address)
         * - triggering a host-interrupt
         */
        bool hasSideEffectsOnWrite() const noexcept;

        /*
         * Whether this register can be read
         */
        bool isReadable() const noexcept;
        /*
         * Whether this register can be written to
         */
        bool isWriteable() const noexcept;

        /*
         * Whether writing into this register triggers the result to appear in r4, e.g. by triggering an SFU calculation
         * or loading via TMU
         */
        bool triggersReadOfR4() const noexcept;

        /**
         * Returns whether this register is guaranteed to return an unsigned (positive) integer
         */
        bool isUnsignedInteger() const noexcept;

        /**
         * Returns the mask of (possible) non-zero bits the result might have when reading this register
         */
        BitMask getReadMask() const noexcept;

        /**
         * Returns the mask of bits used at all when writing this register, any bit not in this mask has no effect on
         * the behavior of this register
         */
        BitMask getWriteMask() const noexcept;

        static constexpr int INVALID_ACCUMULATOR{-1};
    };

    /*
     * UNIFORM registers, present on both physical register-files, used to read the kernel parameters and
     * image-configuration.
     *
     * 32-bit UNIFORM values are automatically replicated across all SIMD elements on read.
     */
    static constexpr Register REG_UNIFORM{RegisterFile::PHYSICAL_ANY, 32};
    /*
     * General-purpose accumulator 0
     *
     * NOTE: Full range vector rotation is only available if both inputs to the mul ALU are stored in the accumulators 0
     * to 3.
     */
    static constexpr Register REG_ACC0{RegisterFile::ACCUMULATOR, 32};
    /*
     * General-purpose accumulator 1
     *
     * NOTE: Full range vector rotation is only available if both inputs to the mul ALU are stored in the accumulators 0
     * to 3.
     */
    static constexpr Register REG_ACC1{RegisterFile::ACCUMULATOR, 33};
    /*
     * General-purpose accumulator 2
     *
     * NOTE: Full range vector rotation is only available if both inputs to the mul ALU are stored in the accumulators 0
     * to 3.
     */
    static constexpr Register REG_ACC2{RegisterFile::ACCUMULATOR, 34};
    /*
     * General-purpose accumulator 3
     *
     * NOTE: Full range vector rotation is only available if both inputs to the mul ALU are stored in the accumulators 0
     * to 3.
     */
    static constexpr Register REG_ACC3{RegisterFile::ACCUMULATOR, 35};
    /*
     * Varying input register
     *
     * Varying variables are pre-interpolated floating-point inputs for shaders, see specification section 6.
     *
     * "Reads of the VARYING_READ register will stall while empty, unless all of the varyings have been read for that
     * thread in which case the read returns undefined data." - Broadcom specification, section 6
     */
    static constexpr Register REG_VARYING{RegisterFile::PHYSICAL_ANY, 35};
    /*
     * "Accumulator 4", cannot be used as a "standard" accumulator.
     * Instead contains the result of some periphery-components (e.g. SFU, TMU)
     *
     * Similar to "normal" registers and accumulators, the previous result is available for successive reads of r4 and
     * r5 until it is overwritten by another periphery action writing into the register.
     *
     * NOTE: Reading from r4 offers some additional unpack-modes (see OpCodes.h)
     */
    static constexpr Register REG_SFU_OUT{RegisterFile::ACCUMULATOR, 36};
    /*
     * Again, "accumulator 4"
     *
     * "TMU read generates:
     *     9 clock stalls when it reads from TMU cache.
     *     12 clock stalls when it reads from V3D L2 cache.
     *     20 clock stalls when it reads directly from memory."
     * - see https://www.raspberrypi.org/forums/viewtopic.php?p=1143940#p1144081
     *
     * NOTE: These stalls are generated between writing the TMU S address and issuing the load signal, not for reading
     * the r4 output register!
     */
    static constexpr Register REG_TMU_OUT{RegisterFile::ACCUMULATOR, 36};
    /*
     * Writing to "accumulator 4" toggles the automatic swapping of TMUs (using TMU0 for QPUs 0/1 and TMU1 for QPUs 2/3
     * within a slice). Writing any value different from zero disables the TMU swapping and allows all QPUs to access
     * both TMUs by writing in their corresponding registers.
     *
     * NOTE: The value written is only taken from SIMD element 0.
     *
     * "If TMU_NOSWAP is written, the write must be three instructions before the first TMU write instruction"
     * - Broadcom specification, page 37
     */
    static constexpr Register REG_TMU_NOSWAP{RegisterFile::PHYSICAL_ANY, 36};
    /*
     * Write to "accumulator 5" to set a dynamic offset for vector-rotations.
     *
     * When any of the replication registers below are written, the replicated value can be read from this "accumulator
     * 5" register.
     *
     * Similar to "normal" registers and accumulators, the previous result is available for successive reads of r4 and
     * r5 until it is overwritten by another periphery action writing into the register.
     *
     * NOTE: The vector rotation offset is only taken from the bits [0:3], truncating/ignoring any upper bits
     */
    static constexpr Register REG_ACC5{RegisterFile::ACCUMULATOR, 37};
    /*
     * Writing to this register distributes the value from the first element of the quad (0, 4, 8, 12) to all other
     * elements of this quad
     */
    static constexpr Register REG_REPLICATE_QUAD{RegisterFile::PHYSICAL_A, 37};
    /*
     * Writing to this register distributes the value from the first element to all 15 other elements
     *
     * NOTE: Tests have shown that flags of the setting instruction are heeded, i.e. for conditional instruction setting
     * this register, the replicated value is only overwritten if the condition for the 0th element is met (similar to
     * any other kind of ALU register write).
     */
    static constexpr Register REG_REPLICATE_ALL{RegisterFile::PHYSICAL_B, 37};
    /*
     * When read, fills the vector-element with the corresponding element-number (e.g. [0, 1, 2, 3, 4, ...])
     */
    static constexpr Register REG_ELEMENT_NUMBER{RegisterFile::PHYSICAL_A, 38};
    /*
     * Reading this register returns the QPU-number across all vector-elements
     */
    static constexpr Register REG_QPU_NUMBER{RegisterFile::PHYSICAL_B, 38};
    /*
     * Writing a non-zero value triggers a host-interrupt.
     *
     * The interrupt is used by the kernel-driver to determine whether a kernel-execution has finished.
     */
    static constexpr Register REG_HOST_INTERRUPT{RegisterFile::PHYSICAL_ANY, 38};
    /*
     * The nop-register. Writing into this simply discards the values
     *
     * NOTE: The nop-register could be read to replicate the elements (12, 13, 14, 15) across all vector-elements
     */
    static constexpr Register REG_NOP{RegisterFile::PHYSICAL_ANY, 39};

    /*
     * Writing this register sets the memory-address the next UNIFORMs are read from
     *
     * "The uniform base pointer can be written (from SIMD element 0) by the processor to reset the stream,
     * there must be at least two nonuniform-accessing instructions following a pointer change before uniforms can be
     * accessed once more."
     * - Broadcom specification, page 22
     */
    static constexpr Register REG_UNIFORM_ADDRESS{RegisterFile::PHYSICAL_ANY, 40};

    /*
     * X pixel coordinates
     *
     * Originally a register for usage in shaders.
     *
     * Returns the constant pattern [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1] on read, cannot be written.
     */
    static constexpr Register REG_X_COORDS{RegisterFile::PHYSICAL_A, 41};

    /*
     * Y pixel coordinates
     *
     * Originally a register for usage in shaders.
     *
     * Returns the constant pattern [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1] on read, cannot be written.
     */
    static constexpr Register REG_Y_COORDS{RegisterFile::PHYSICAL_B, 41};

    /*
     * Multisample mask register
     *
     * In its original usage, this register stores multisample masks and is updated when loading new pixels.
     *
     * This register can be used outside of shaders register and has following properties:
     * - Like all general-purpose registers, writing and reading is per-element
     * - Also supports conditional write/read per-element
     * - Like the "physical" registers, there must be a delay between writing and reading the register
     * - NOTE: The register truncates the input to the 4 lowest bits, being able to only store the values [0,15]
     */
    static constexpr Register REG_MS_MASK{RegisterFile::PHYSICAL_A, 42};

    /*
     * Inversion flag register
     *
     * In its original usage, this register determines whether primitives face backwards
     *
     * This register can be used outside of shaders register and has following properties:
     * - NOTE: Only SIMD element 0 is written and the value is replicated across all elements
     * - Therefore, conditional write is only supported for element 0
     * - Supports conditional read per-element
     * - Like the "physical" registers, there must be a delay between writing and reading the register
     * - NOTE: The register only stores the lowest bit, being able to only store the values [0,1]
     */
    static constexpr Register REG_REV_FLAG{RegisterFile::PHYSICAL_B, 42};

    // VPM I/O, see specification section 7
    /*
     * Accesses the VPM (read and write)
     *
     * For VPM reads:
     * "After the read setup register is written, read data is available to read after a minimum latency of three QPU
     * instructions"
     * - Bradcom specification, page 56
     * "VPM read generates 5 clock stalls between "VPM generic block read setup" and the first VPM_READ read."
     * - see https://www.raspberrypi.org/forums/viewtopic.php?p=1143940#p1144081 and http://imrc.noip.me/blog/vc4/QV56/
     *
     * -> so this blocks always for at least 3/5 instructions (if no other instructions are inserted in between)
     *
     * "[...], but reads made too early or extra reads made beyond the number setup will return immediately with
     * undefined data."
     * - Bradcom specification, page 56
     * "VPM reads seem to block immediately if the FIFO is empty. No undefined data is returned when the reads are made
     * too early."
     * - http://maazl.de/project/vc4asm/doc/VideoCoreIV-addendum.html, section 7
     *
     * -> no need to insert delay, since reads will block, also the number of reads must match exactly
     *
     * For VPM writes:
     * "When writing vector data to the VPM, the contents of the write setup register are used for an indefinite number
     * of subsequent writes. The write address will wrap when incremented beyond a Y of 63, and writes to addresses
     * outside of the window of allocated VPM space will be masked. Up to two writes are queued in a FIFO, and writes
     * will stall the QPU when the FIFO is full."
     * - Bradcom specification, page 56
     *
     * -> QPU will stall automatically when FIFO full, no need for manual stalling
     * -> an "infinite" number of rows can be written with a single configuration
     */
    static constexpr Register REG_VPM_IO{RegisterFile::PHYSICAL_ANY, 48};
    /*
     * Reading this registers returns whether a DMA load operation is currently being executed by the VPM
     *
     * The value read from this register seems to be always the "unmapped I/O" (REG_NOP), independent of
     * the current status of any DMA process.
     */
    static constexpr Register REG_VPM_DMA_LOAD_BUSY{RegisterFile::PHYSICAL_A, 49};
    /*
     * Reading this registers returns whether a DMA write operation is currently being executed by the VPM
     *
     * The value read from this register seems to be always the "unmapped I/O" (REG_NOP), independent of
     * the current status of any DMA process.
     */
    static constexpr Register REG_VPM_DMA_STORE_BUSY{RegisterFile::PHYSICAL_B, 49};
    /*
     * Writing this register changes the VPM configuration to read values from memory/VPM
     */
    static constexpr Register REG_VPM_IN_SETUP{RegisterFile::PHYSICAL_A, 49};
    /*
     * Writing this register changes the VPM configuration to write values to memory/VPM
     */
    static constexpr Register REG_VPM_OUT_SETUP{RegisterFile::PHYSICAL_B, 49};
    /*
     * Reading this register stalls until the currently running DMA read operation has finished
     *
     * The value read from this register seems to be always the "unmapped I/O" (REG_NOP)
     */
    static constexpr Register REG_VPM_DMA_LOAD_WAIT{RegisterFile::PHYSICAL_A, 50};
    /*
     * Reading this register stalls until the currently running DMA write operation has finished
     *
     * The value read from this register seems to be always the "unmapped I/O" (REG_NOP)
     */
    static constexpr Register REG_VPM_DMA_STORE_WAIT{RegisterFile::PHYSICAL_B, 50};
    /*
     * Writes the memory-address to read data from, triggers a DMA read operation
     */
    static constexpr Register REG_VPM_DMA_LOAD_ADDR{RegisterFile::PHYSICAL_A, 50};
    /*
     * Writes the memory-address to write data into, triggers a DMA write operation
     */
    static constexpr Register REG_VPM_DMA_STORE_ADDR{RegisterFile::PHYSICAL_B, 50};

    /*
     * Reading this register locks the hardware-mutex (and blocks, if the mutex is already locked).
     * Writing this register releases a previously blocked hardware-mutex
     *
     * The value read from the register seems to be the constant value 0xFC2FF000
     */
    static constexpr Register REG_MUTEX{RegisterFile::PHYSICAL_ANY, 51};

    // Special Functions Unit
    /*
     * Writing this register triggers the SFU to approximate the reciprocal (1/x) of the floating-point value passed.
     * The result is available in r4 three instructions later, in which no SFU-register nor r4 can be touched!
     */
    static constexpr Register REG_SFU_RECIP{RegisterFile::PHYSICAL_ANY, 52};
    /*
     * Writing this register triggers the SFU to approximate the reciprocal square-root (1/sqrt(x)) of the
     * floating-point value passed. The result is available in r4 three instructions later, in which no SFU-register nor
     * r4 can be touched!
     */
    static constexpr Register REG_SFU_RECIP_SQRT{RegisterFile::PHYSICAL_ANY, 53};
    /*
     * Writing this register triggers the SFU to approximate the powr of two (2^x) of the floating-point value passed.
     * The result is available in r4 three instructions later, in which no SFU-register nor r4 can be touched!
     */
    static constexpr Register REG_SFU_EXP2{RegisterFile::PHYSICAL_ANY, 54};
    /*
     * Writing this register triggers the SFU to approximate the logarithm to the power of two (log2(x)) of the
     * floating-point value passed. The result is available in r4 three instructions later, in which no SFU-register nor
     * r4 can be touched!
     */
    static constexpr Register REG_SFU_LOG2{RegisterFile::PHYSICAL_ANY, 55};

    // Texture Memory Unit
    // if we leave TMU auto-swap enabled, we only need to write to TMU0, otherwise, we need to access both TMUs
    /*
     * Writing this register triggers a load from memory (RAM/L2) via the TMU0 into the TMU "receive" FIFO. To retrieve
     * the value from TMU "receive" FIFO, the corresponding signal must be issued.
     *
     * Depending on whether the T-coordinate was written before, this load is a texture load (if written) or a general
     * 32-bit load (otherwise).
     *
     * NOTE: Loads from TMU are element-wise, so with a single load, 16 values from 16 different memory-locations can be
     * read.
     * NOTE: For general loads, the address is automatically clamped to align 4 Byte!
     * NOTE: There is a delay of at least 8 instructions between writing the TMU address and issuing the TMU load
     * signal, which corresponds to the 8 cycles required to load the values from TMU cache into the TMU "receive" FIFO.
     * Loads directly from RAM take up to 20 cycles to be available.
     */
    static constexpr Register REG_TMU0_ADDRESS{RegisterFile::PHYSICAL_ANY, 56};
    /*
     * Same as above, this version is for reading image-coordinates
     */
    static constexpr Register REG_TMU0_COORD_S_U_X{RegisterFile::PHYSICAL_ANY, 56};
    /*
     * Writing this register sets the Y-coordinates for the next image-read via the TMU0
     */
    static constexpr Register REG_TMU0_COORD_T_V_Y{RegisterFile::PHYSICAL_ANY, 57};
    /*
     * Writing this register sets the border color to be used (depending on the clamp-mode) for out-of-range
     * coordinate-reads
     */
    static constexpr Register REG_TMU0_COORD_R_BORDER_COLOR{RegisterFile::PHYSICAL_ANY, 58};
    /*
     * Writing this register sets the LOD-bias for reading LOD-images
     */
    static constexpr Register REG_TMU0_COORD_B_LOD_BIAS{RegisterFile::PHYSICAL_ANY, 59};
    /*
     * Same as above, for TMU1
     *
     * NOTE: To manually access the second TMU, TMU-swapping needs to be disabled
     */
    static constexpr Register REG_TMU1_ADDRESS{RegisterFile::PHYSICAL_ANY, 60};
    /*
     * Same as above, for TMU1
     */
    static constexpr Register REG_TMU1_COORD_S_U_X{RegisterFile::PHYSICAL_ANY, 60};
    /*
     * Same as above, for TMU1
     */
    static constexpr Register REG_TMU1_COORD_T_V_Y{RegisterFile::PHYSICAL_ANY, 61};
    /*
     * Same as above, for TMU1
     */
    static constexpr Register REG_TMU1_COORD_R_BORDER_COLOR{RegisterFile::PHYSICAL_ANY, 62};
    /*
     * Same as above, for TMU1
     */
    static constexpr Register REG_TMU1_COORD_B_LOD_BIAS{RegisterFile::PHYSICAL_ANY, 63};

    /*
     * The arithmetic type of a literal value
     */
    enum class LiteralType : unsigned char
    {
        INTEGER,
        REAL,
        BOOL,
        // "literal type" indicating no literal present
        TOMBSTONE,
        // special version of "INTEGER" which indicates that the upper word are all ones
        LONG_LEADING_ONES,
    };

    struct SmallImmediate;

    /*
     * A literal value (a constant), directly used as operand.
     *
     * Literals cannot be handled by machine-code and must be converted before code-generation.
     * While "smaller" literal values can be directly used by ALU instructions in form of SmallImmediates,
     * "larger" values need to be loaded via a load-immediate instruction.
     */
    class Literal
    {
    public:
        LiteralType type;

        explicit constexpr Literal(optional_tombstone_tag) noexcept : type(LiteralType::TOMBSTONE), u() {}
        explicit constexpr Literal(int32_t integer) noexcept : type(LiteralType::INTEGER), i(integer) {}
        explicit constexpr Literal(uint32_t integer) noexcept : type(LiteralType::INTEGER), u(integer) {}
        explicit constexpr Literal(float real) noexcept : type(LiteralType::REAL), f(real) {}
        explicit constexpr Literal(bool flag) noexcept : type(LiteralType::BOOL), u(flag) {}
        ~Literal() = default;

        Literal(const Literal&) = default;
        Literal(Literal&&) noexcept = default;

        Literal& operator=(const Literal&) = default;
        Literal& operator=(Literal&&) noexcept = default;

        bool operator==(const Literal& other) const noexcept;
        inline bool operator!=(const Literal& other) const noexcept
        {
            return !(*this == other);
        }
        bool operator<(const Literal& other) const noexcept;

        std::string to_string() const;

        /*
         * Whether this literal represents the boolean value true
         */
        bool isTrue() const noexcept;
        /*
         * Bit-casts the stored value to a floating-point value
         */
        float real() const noexcept;
        /*
         * Bit-casts the stored value to a signed value
         */
        int32_t signedInt() const noexcept;
        /*
         * Bit-casts the stored value to an unsigned value
         */
        uint32_t unsignedInt() const noexcept;

        /*
         * Converts the stored value to a immediate-value which can be used in a load-immediate instruction.
         */
        uint32_t toImmediate() const noexcept;

        /*
         * A Literal is "undefined" if it is of TOMBSTONE type. the internal value can be any of the 2^32 possible
         * values
         */
        bool isUndefined() const noexcept;

        /**
         * Returns the mask of bits set
         */
        BitMask getBitMask() const noexcept;

    private:
        /*
         * The bit-wise representation of this literal
         */
        union {
            int32_t i;
            uint32_t u;
            float f;
        };

        static_assert(sizeof(int32_t) == sizeof(uint32_t) && sizeof(uint32_t) == sizeof(float),
            "Sizes of literal types do not match!");
    };

    /*
     * A literal value with undefined value, doubles as not-set literal for compact optionals
     */
    static constexpr Literal UNDEFINED_LITERAL{optional_tombstone_tag{}};

    template <>
    struct tombstone_traits<Literal>
    {
        static constexpr bool is_specialized = true;
        static constexpr Literal tombstone = Literal(optional_tombstone_tag{});

        static constexpr bool isTombstone(const Literal& val) noexcept
        {
            return val.type == LiteralType::TOMBSTONE;
        }
    };

    /**
     * Tries to convert the given 64-bit value to a literal.
     *
     * Returns an empty optional on failure
     */
    Optional<Literal> toLongLiteral(uint64_t val);

    /*!
     * A SmallImmediate value is a literal (constant) value which can be loaded directly into an ALU instruction.
     *
     * SmallImmediates are no "standard" integer values, but are mapped to represent some value (e.g. using a
     * SmallImmediate value of 16 actually loads the integer value -15). Additionally, some SmallImmediate values are
     * used to load floating-point constants as well as represent the offset for vector-rotations.
     *
     * "The 6 bit small immediate field encodes either an immediate integer/float value used in place of the register
     * file b input, or a vector rotation to apply to the mul ALU output, according to Table 5."
     * - Broadcom VideoCore IV specification, page 29
     */
    struct SmallImmediate : public InstructionPart<SmallImmediate>
    {
        /*
         * Creates an object with the given field-value (not the value actually loaded!)
         */
        explicit constexpr SmallImmediate(unsigned char val) noexcept : InstructionPart(val) {}

        std::string to_string() const;

        // the "real" values being loaded with this small immediate

        /*
         * Returns the integer-value represented by this object, if this object represents an integer value.
         *
         * The field-values 0 - 15 map to the integer values 0 - 15 correspondingly,
         * the field-values 16 - 31 map to the integer values -16 to -1.
         * So the range [-16, 15] can be represented by SmallImmediate objects.
         */
        Optional<int32_t> getIntegerValue() const noexcept;
        /*
         * Returns the floating-point value represented by this object, if this object represents a floating-point
         * value.
         *
         * The field-values 32 - 29 represent the floating-point constants 1.0, 2.0, 4.0, ..., 128.0,
         * the field-values 40 - 47 represent the constants 1/256.0, 1/128.0, ..., 1/2.
         * So every power of two from 1/256.0 to 128.0 can be represented by SmallImmediates.
         */
        Optional<float> getFloatingValue() const noexcept;
        /*
         * Returns whether the field-value represents a vector-rotation
         */
        bool isVectorRotation() const noexcept;
        /*
         * Returns the constant offset for vector-rotations, if this object represents one.
         *
         * The field-value of VECTOR_ROTATE_R5 represents a vector-rotation by the value stored in SIMD-element 0 of r5,
         * bits [3:0], while the field-values 49 - 63 represent vector-rotations by an offset of 1 - 15 upwards (so
         * element 0 moves to element 1 - 15)
         *
         * NOTE: vector-rotations can only be performed by the multiplication ALU.
         */
        Optional<unsigned char> getRotationOffset() const noexcept;

        /*
         * Returns the Literal value which is represented by this SmallImmediate object.
         *
         * For vector-rotation field-values, no value us returned.
         */
        Optional<Literal> toLiteral() const noexcept;

        /*
         * Tries to create a SmallImmediate object from the given integer-value.
         *
         * Returns a new object for an argument in the range [-16, 15] and an empty optional-value otherwise.
         */
        static Optional<SmallImmediate> fromInteger(signed char val) noexcept;
        /*
         * Creates a new object from the given constant vector-rotation offset.
         *
         * The given vector-rotation offset must lie in the range [1, 15]
         */
        static SmallImmediate fromRotationOffset(unsigned char offset);
    };

    template <>
    struct tombstone_traits<SmallImmediate>
    {
        static constexpr bool is_specialized = true;
        static constexpr SmallImmediate tombstone = SmallImmediate(255);

        static constexpr bool isTombstone(SmallImmediate val) noexcept
        {
            return val.value == 255;
        }
    };

    constexpr SmallImmediate VECTOR_ROTATE_R5{48};

    class Local;
    class SIMDVector;
    namespace intermediate
    {
        class IntermediateInstruction;
    } /* namespace intermediate */

    using LocalUser = intermediate::IntermediateInstruction;

    /*
     * The main type representing all values being operated on
     */
    struct Value
    {
        /*
         * Contains the data actually stored in this Value
         */
        Variant<Literal, Register, Local*, SmallImmediate, const SIMDVector*, VariantNamespace::monostate> data;
        /*
         * The data-type of the Value
         */
        DataType type;

        Value(const Literal& lit, DataType type) noexcept;
        Value(Register reg, DataType type) noexcept;
        Value(const SIMDVector* vector, DataType type);
        Value(const Value& val) = default;
        Value(Value&& val) noexcept = default;
        Value(Local* local, DataType type) noexcept;
        Value(DataType type) noexcept;
        Value(SmallImmediate immediate, DataType type) noexcept;
        ~Value() = default;

        Value& operator=(const Value& right) = default;
        Value& operator=(Value&& right) = default;

        bool operator==(const Value& other) const;
        inline bool operator!=(const Value& other) const
        {
            return !(*this == other);
        }

        /*
         * Returns a pointer to the data of the given type or nullptr if the data is not of the requested type.
         */
        Register* checkRegister() noexcept
        {
            return VariantNamespace::get_if<Register>(&data);
        }

        const Register* checkRegister() const noexcept
        {
            return VariantNamespace::get_if<Register>(&data);
        }

        Literal* checkLiteral() noexcept
        {
            return VariantNamespace::get_if<Literal>(&data);
        }

        const Literal* checkLiteral() const noexcept
        {
            return VariantNamespace::get_if<Literal>(&data);
        }

        SmallImmediate* checkImmediate() noexcept
        {
            return VariantNamespace::get_if<SmallImmediate>(&data);
        }

        const SmallImmediate* checkImmediate() const noexcept
        {
            return VariantNamespace::get_if<SmallImmediate>(&data);
        }

        Local* checkLocal() noexcept
        {
            auto loc = VariantNamespace::get_if<Local*>(&data);
            return loc ? *loc : nullptr;
        }

        const Local* checkLocal() const noexcept
        {
            auto loc = VariantNamespace::get_if<Local*>(&data);
            return loc ? *loc : nullptr;
        }

        const SIMDVector* checkVector() const noexcept
        {
            auto vec = VariantNamespace::get_if<const SIMDVector*>(&data);
            return vec ? *vec : nullptr;
        }

        /*
         * Whether this object has the given local
         */
        bool hasLocal(const Local* local) const;
        /*
         * Whether this object has the given register
         */
        bool hasRegister(Register reg) const;
        /*
         * Whether this object has the given literal value.
         *
         * This function also accepts, if this object contains a SmallImmediate with the same integer- or
         * floating-point-value (depending on the data-type) as the parameter
         */
        bool hasLiteral(const Literal& lit) const;
        /*
         * Whether this object contains the given SmallImmediate value.
         *
         * This function also accepts, if this object contains a Literal with the same integer- or floating-point-value
         * (depending on the data-type) as the parameter
         */
        bool hasImmediate(SmallImmediate immediate) const;

        /*
         * Whether this Value is undefined, e.g. by having the TYPE_UNDEFINED data-type
         */
        bool isUndefined() const;
        /*
         * Whether this object is a constant (scalar or composite) and has the literal-value zero in all its elements
         */
        bool isZeroInitializer() const;
        /*
         * Whether this Value represents a literal value (e.g. contains a Literal or SmallImmediate)
         */
        bool isLiteralValue() const noexcept;
        /*
         * Returns the Literal stored in this Value.
         *
         * This function also converts a stored SmallImmediate into the corresponding Literal value
         */
        Optional<Literal> getLiteralValue() const noexcept;

        std::string to_string(bool writeAccess = false, bool withLiterals = false) const;

        /*
         * Whether this Value can be written to.
         *
         * Constant values of any kind cannot be written to, neither can registers which are not writeable
         */
        bool isWriteable() const;
        /*
         * Whether this object can be read from.
         *
         * Almost all value-types can be read, except for some write-only registers
         */
        bool isReadable() const;
        /*
         * Returns this object, if it is writeable. Throws an exception otherwise
         */
        const Value& assertWriteable() const;
        Value& assertWriteable();
        /*
         * Returns this object if it is readable. Throws an exception otherwise
         */
        const Value& assertReadable() const;
        Value& assertReadable();

        /*
         * Wrapper for Local#getSingleWriter() for easier access
         */
        const LocalUser* getSingleWriter() const;

        /**
         * Return whether this value is guaranteed to be an unsigned (positive) integer.
         *
         * Unsigned integers are among others:
         * - positive integer constants
         * - unsigned registers (QPU number, element number)
         * - locals where all writes are decorated as unsigned
         */
        bool isUnsignedInteger() const;

        /**
         * Returns the constant value "contained" in this value, if any.
         *
         * A value is considered constant, if it matches one of the conditions:
         * - it is a literal value
         * - it is a SIMD vector
         * - it is a small immediate
         * - it is a constant register value
         * - it is a local with a single writer writing a constant value (only if transitive flag set)
         *
         * If the resulting Value is set, it is guaranteed to be either a literal value, a small immediate or a
         * register.
         */
        Optional<Value> getConstantValue(bool transitive = true) const;

        /*
         * Creates a zero-initializer Value for the given data-type.
         *
         * For scalar types, a simple INT_ZERO is returned, for compound types, a container containing the correct
         * amount of zero-elements is created
         */
        static Optional<Value> createZeroInitializer(DataType type);

        /**
         * Returns whether all SIMD elements of this value contain the same value.
         *
         * This is e.g. true for SIMDVectors with identical elements, literal values, SmallImmediates and some
         * registers.
         */
        bool isAllSame() const;

        /**
         * Returns the mask of (possible) non-zero bits in this value when read
         */
        BitMask getReadMask() const noexcept;

        /*
         * Returns the stored data of the given type, if it matches the stored type.
         * Throws error otherwise
         */
        Literal& literal()
        {
            return VariantNamespace::get<Literal>(data);
        }

        const Literal& literal() const
        {
            return VariantNamespace::get<Literal>(data);
        }

        Register& reg()
        {
            return VariantNamespace::get<Register>(data);
        }

        const Register& reg() const
        {
            return VariantNamespace::get<Register>(data);
        }

        Local*& local()
        {
            return VariantNamespace::get<Local*>(data);
        }

        Local* const& local() const
        {
            return VariantNamespace::get<Local*>(data);
        }

        SmallImmediate& immediate()
        {
            return VariantNamespace::get<SmallImmediate>(data);
        }

        const SmallImmediate& immediate() const
        {
            return VariantNamespace::get<SmallImmediate>(data);
        }

        const SIMDVector& vector() const
        {
            return *VariantNamespace::get<const SIMDVector*>(data);
        }
    };

    /*
     * The boolean-value true
     */
    const Value BOOL_TRUE(Literal(true), TYPE_BOOL);
    /*
     * The boolean-value false
     */
    const Value BOOL_FALSE(Literal(false), TYPE_BOOL);
    /*
     * The integer value of zero
     */
    const Value INT_ZERO(Literal(static_cast<uint32_t>(0)), TYPE_INT8);
    /*
     * The integer value of one
     */
    const Value INT_ONE(Literal(static_cast<uint32_t>(1)), TYPE_INT8);
    /*
     * The integer value of minus one
     */
    const Value INT_MINUS_ONE(Literal(static_cast<uint32_t>(0xFFFFFFFF)), TYPE_INT32);
    /*
     * The floating-point value of zero
     */
    const Value FLOAT_ZERO(Literal(0.0f), TYPE_FLOAT);
    /*
     * The floating-point value of one
     */
    const Value FLOAT_ONE(Literal(1.0f), TYPE_FLOAT);
    /*
     * The floating-point constant representing INF
     */
    const Value FLOAT_INF(Literal(static_cast<uint32_t>(0x7F800000)), TYPE_FLOAT);
    /*
     * The floating-point constant representing -INF
     */
    const Value FLOAT_NEG_INF(Literal(static_cast<uint32_t>(0xFF800000)), TYPE_FLOAT);
    /*
     * The floating-point constant representing NAN
     *
     * NOTE: There are different representations of a NaN!
     */
    const Value FLOAT_NAN(Literal(static_cast<uint32_t>(0x7FFFFFFF)), TYPE_FLOAT);
    /*
     * A undefined value
     */
    const Value UNDEFINED_VALUE(TYPE_UNKNOWN);
    /*
     * A constant Optional value containing no value
     */
    const Optional<Value> NO_VALUE;
    /*
     * All 32 bits are set
     */
    const Value VALUE_ALL_BITS_SET = INT_MINUS_ONE;

    /*
     * The Value representing the REG_UNIFORM register to read UNIFORMs
     */
    const Value UNIFORM_REGISTER(REG_UNIFORM, TYPE_INT32.toVectorType(16));
    /*
     * The Value representing the NOP-register
     */
    const Value NOP_REGISTER(REG_NOP, TYPE_UNKNOWN);
    /*
     * The Value representing the REG_ELEMENT_NUMBER
     */
    const Value ELEMENT_NUMBER_REGISTER(REG_ELEMENT_NUMBER, TYPE_INT8.toVectorType(16));
    /*
     * The element numbers (0, 1, 2, 3, ...) returned when querying the element-number register
     */
    extern const Value ELEMENT_NUMBERS;
    /*
     * The Value representing the r5 rotation offset register
     */
    const Value ROTATION_REGISTER(REG_ACC5, TYPE_INT8);

} /* namespace vc4c */

namespace std
{
    template <>
    struct hash<vc4c::Register> : public std::hash<unsigned char>
    {
        inline size_t operator()(const vc4c::Register& val) const noexcept
        {
            return std::hash<unsigned char>::operator()(static_cast<unsigned char>(val.file)) ^
                std::hash<unsigned char>::operator()(val.num);
        }
    };

    template <>
    struct hash<vc4c::Literal> : public std::hash<int32_t>
    {
        inline size_t operator()(const vc4c::Literal& lit) const noexcept
        {
            return std::hash<int32_t>::operator()(lit.signedInt());
        }
    };

    template <>
    struct hash<vc4c::SmallImmediate> : public std::hash<unsigned char>
    {
        inline size_t operator()(const vc4c::SmallImmediate& val) const noexcept
        {
            return std::hash<unsigned char>::operator()(val.value);
        }
    };

    template <>
    struct hash<vc4c::Value>
    {
        size_t operator()(const vc4c::Value& val) const noexcept;
    };
} /* namespace std */

#endif /* VALUES_H */
