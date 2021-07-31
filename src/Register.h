/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_REGISTER_H
#define VC4C_REGISTER_H

#include "BitMask.h"
#include "helper.h"

#include <string>

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
} /* namespace std */

#endif /* VC4C_REGISTER_H */
