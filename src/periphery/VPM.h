/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VPM_H
#define VC4C_VPM_H

#include "../Bitfield.h"
#include "../InstructionWalker.h"
#include "../Method.h"

namespace vc4c
{
    const Value VPM_IN_SETUP_REGISTER(REG_VPM_IN_SETUP, TYPE_INT32);
    const Value VPM_OUT_SETUP_REGISTER(REG_VPM_OUT_SETUP, TYPE_INT32);
    const Value VPM_DMA_LOAD_ADDR_REGISTER(REG_VPM_DMA_LOAD_ADDR, TYPE_VOID.toPointerType());
    const Value VPM_DMA_STORE_ADDR_REGISTER(REG_VPM_DMA_STORE_ADDR, TYPE_VOID.toPointerType());
    const Value VPM_DMA_LOAD_WAIT_REGISTER(REG_VPM_DMA_LOAD_WAIT, TYPE_UNKNOWN);
    const Value VPM_DMA_STORE_WAIT_REGISTER(REG_VPM_DMA_STORE_WAIT, TYPE_UNKNOWN);
    const Value VPM_IO_REGISTER(REG_VPM_IO, TYPE_UNKNOWN);

    namespace periphery
    {
        /*
         * Setup for writing from QPU to VPM
         *
         * see Broadcom spec, table 32
         */
        class VPWGenericSetup : private Bitfield<uint32_t>
        {
        public:
            VPWGenericSetup(uint8_t size, uint8_t stride, uint8_t address = 0) : Bitfield(0)
            {
                setAddress(address);
                setSize(size);
                setLaned(false);
                setHorizontal(true);
                setStride(stride);
                setID(0);
            }

            std::string to_string() const;

            /*
             * "Location of the first vector accessed.
             * The LS 1 or 2 bits select the Half-word or Byte sub-vector for 16 or 8-bit width. The LS 4 bits of the
             * 32-bit vector address are Y address if horizontal or X address if vertical. Horizontal 8-bit: ADDR[7:0] =
             * {Y[5:0], B[1:0]} Horizontal 16-bit: ADDR[6:0] = {Y[5:0], H[0]} Horizontal 32-bit: ADDR[5:0] = Y[5:0]"
             *
             * This is the address (in VPM) to write the first vector to. All consecutive writes write to this address
             * incremented by the type-width * stride. The meaning of the address depends on the type-width,
             * laned/packed and horizontal/vertical mode.
             *
             * Examples:
             * For packed and horizontal mode (default), an address of 7 for 8-bit values writes into the columns 12 to
             * 15 in the first row of the VPM The same values for vertical laned mode write into the third byte of the
             * third row into the columns 0 to 15
             *
             * NOTE:
             * In contrast to the VPM addressing for DMA operations the addressing for QPU-side accessing has a rather
             * wide granularity, allowing to address only blocks of 4 words (16 byte-sized elements)
             */
            BITFIELD_ENTRY(Address, uint8_t, 0, Byte)
            BITFIELD_ENTRY(WordRow, uint8_t, 0, Byte)
            BITFIELD_ENTRY(HalfWordRow, uint8_t, 1, Septuple)
            BITFIELD_ENTRY(HalfWordOffset, bool, 0, Bit)
            BITFIELD_ENTRY(ByteRow, uint8_t, 2, Sextuple)
            BITFIELD_ENTRY(ByteOffset, uint8_t, 0, Tuple)
            /*
             * "0,1,2,3 = 8-bit, 16-bit, 32-bit, reserved"
             *
             * NOTE: Writing into VPM always writes all 16 vector-elements, so depending on the size-value, 16, 32 or 64
             * bytes are written!
             */
            BITFIELD_ENTRY(Size, uint8_t, 8, Tuple)
            /*
             * "0,1 = Packed, Laned. Ignored for 32-bit width"
             *
             * In packed mode, writes into the VPM have consecutive addresses, e.g. | b1 | b2 | b3 | ... |.
             * In laned mode, every entry (byte, half-word, word) has an offset of 1 word to the previous, e.g. | b1 |
             * xx | xx | xx | b2 | xx | xx | xx | b3 | ... |.
             *
             * For now, only packed mode is used!
             */
            BITFIELD_ENTRY(Laned, bool, 10, Bit)
            /*
             * "0,1 = Vertical, Horizontal"
             *
             * For now, only horizontal mode is used!
             */
            BITFIELD_ENTRY(Horizontal, bool, 11, Bit)
            /*
             * "Stride. This is added to ADDR after every vector written. 0 => 64."
             *
             * This is the address increment between two consecutive writes (for the VPM address).
             * This increment considers the byte-/half-word offset as well as the row position.
             *
             * Example:
             * Writing 16 bytes horizontal and packed with a stride of 1 and an initial address of 0 results in
             * following new address: 0 + 1 = 1 -> (row 0, column 0) + 1 -> (row 0, column 4 (4 * 4 byte integer))
             *
             * NOTE:
             * If only one value (one 16-byte, 16-half-word or 16-word vector) should be written per row (16-words),
             * the stride has to be 4 for bytes, 2 for half-words and 1 for words to skip the remainder of the row until
             * to the beginning of the next.
             */
            BITFIELD_ENTRY(Stride, uint8_t, 12, Sextuple)
        private:
            //"Selects VPM generic block write setup register."
            BITFIELD_ENTRY(ID, uint8_t, 30, Tuple)
        };

        /*
         * Setup for DMA from VPM to RAM
         *
         * see Broadcom spec, table 34
         */
        class VPWDMASetup : private Bitfield<uint32_t>
        {
        public:
            VPWDMASetup(uint8_t mode, uint8_t depth, uint8_t units = 1) : Bitfield(0)
            {
                setMode(mode);
                setHorizontal(true);
                setDepth(depth);
                setUnits(units);
                setID(2);
            }

            std::string to_string() const;

            /*
             * "Mode, combining width with start Byte/Half-word offset for 8 and 16-bit widths.
             * 0: width = 32-bit
             * 1: Unused.
             * 2-3: width = 16-bit, Half-word offset (packed only) = MODEW[0]
             * 4-7: width = 8-bit, Byte offset (packed only) = MODEW[1:0]"
             *
             * Example:
             * For a base-address of 0 and a mode of 3 (16-bit width, half-word offset), the first element will be read
             * from the byte-address 2 (offset of 16 bit), same for a base-address of 0 and a mode of 6 (8-bit width, 2
             * bytes offset)
             *
             * NOTE:
             * The mode combines the element-size with the alignment (byte/half-word offset).
             * While the base alignment is 32-bit, the byte/half-word offset can be used to specify up to byte-wise
             * alignment (for byte types)
             */
            BITFIELD_ENTRY(Mode, uint8_t, 0, Triple)
            BITFIELD_ENTRY(HalfRowOffset, bool, 0, Bit)
            BITFIELD_ENTRY(ByteOffset, uint8_t, 0, Tuple)
            /*
             * "X,Y address of first 32-bit word in VPM to load to/store from. ADDRA[10:0] = {Y[6:0], X[3:0]}"
             *
             * This determines the base address (in VPM, in X and Y coordinates) to be written into memory
             * and allows to address data with a granularity of one word.
             *
             * Example:
             * Values read with a horizontal access-mode from a base-address-value of 23 addresses the seventh column of
             * the first row
             */
            BITFIELD_ENTRY(VPMBase, uint16_t, 3, Undecuple)
            BITFIELD_ENTRY(WordRow, uint8_t, 7, Septuple)
            BITFIELD_ENTRY(WordColumn, uint8_t, 3, Quadruple)
            /*
             * "0,1 = Vertical, Horizontal"
             *
             * For now, only horizontal mode is used!
             */
            BITFIELD_ENTRY(Horizontal, bool, 14, Bit)
            /*
             * "Row Length of 2D block in memory (0 => 128)"
             *
             * The length of a single row (in elements of the size given in Mode) to be written into memory.
             * Depending on the stride applied to successive values written into VPM, this may exceed the vector-width
             * of a single value (see examples).
             *
             * Examples:
             * To write 4 consecutive byte vectors of size n into RAM which have been written into VPM with a stride of
             * 4 (one byte-vector per row), the depth needs to be n (only write one n-element vector per row) and the
             * units need to be 4 (read 4 rows). To write 4 consecutive byte vectors of size 16 into RAM which have been
             * written into VPM with a stride of 1 (all 4 byte-vectors in a single row), the depth needs to be 64 to
             * write all 64 vector-elements (the whole row), while only one row is set in the units-field.
             *
             * NOTE:
             * As the example suggest, it is not possible (correct?!) to write several vectors packed into a single row
             * in VPM (see example 2) to consecutive memory space if the vector-width is not 16 (since the "dummy"
             * vector-elements) are written in between.
             */
            BITFIELD_ENTRY(Depth, uint8_t, 16, Septuple)
            /*
             * "Number of Rows of 2D block in memory (0 => 128)"
             *
             * The total number of rows (of length Depth * element-size) to be written into memory.
             *
             * NOTE:
             * Depending on the layout multiple vectors have been written to into VPM (see Depth), this is either the
             * number of vectors (for one-vector-per-row) or less (if several vectors are packed into a single row).
             */
            BITFIELD_ENTRY(Units, uint8_t, 23, Septuple)

        private:
            //"Write as 0"
            BITFIELD_ENTRY(laned, bool, 15, Bit)

            //"Selects VDW DMA basic setup"
            BITFIELD_ENTRY(ID, uint8_t, 30, Tuple)
        };

        /*
         * Setup DMA stride
         *
         * see Broadcom spec, table 35
         */
        class VPWStrideSetup : private Bitfield<uint32_t>
        {
        public:
            explicit VPWStrideSetup(uint16_t stride = 0) : Bitfield(0)
            {
                setStride(stride);
                setBlockMode(true);
                setID(3);
            }

            std::string to_string() const;

            /*
             * "Distance between last byte of a row and start of next row in memory, in bytes."
             * Addendum to the Broadcom documentation: "Unlike the documentation suggests the STRIDE field is 16 bits
             * wide."
             *
             * This is the distance in MEMORY between two consecutive rows (vectors) written (the distance between end
             * of one row and start of a new row). With a value of 0, the vectors v1, v2, v3, ... will be in consecutive
             * addresses: | v1 | v2 | v3 | ... | With a value of 1, there will be space of 1 byte between each vector
             * witten: | v1 | x | v2 | x | v3 | ... |
             */
            BITFIELD_ENTRY(Stride, uint16_t, 0, Short)
            /*
             * "0 = ‘row-row’ pitch in VPM is 1-row /1-column for horizontal/vertical mode.
             *  1 = rows are packed consecutively in VPM (into rows or columns)"
             *
             *  Don't know, currently always 0.
             */
            BITFIELD_ENTRY(BlockMode, bool, 16, Bit)
        private:
            //"Selects VDW DMA stride setup"
            BITFIELD_ENTRY(ID, uint8_t, 30, Tuple)
        };

        struct VPWSetup
        {
            union {
                uint32_t value;
                VPWGenericSetup genericSetup;
                VPWDMASetup dmaSetup;
                VPWStrideSetup strideSetup;
            };

            explicit VPWSetup(uint32_t val) : value(val) {}
            explicit VPWSetup(VPWGenericSetup generic) : genericSetup(generic) {}
            explicit VPWSetup(VPWDMASetup dma) : dmaSetup(dma) {}
            explicit VPWSetup(VPWStrideSetup stride) : strideSetup(stride) {}

            std::string to_string() const;

            operator uint32_t() const
            {
                return value;
            }

            bool isGenericSetup() const
            {
                return (value & 0xC0000000) == 0;
            }

            bool isDMASetup() const
            {
                return (value & 0xC0000000) == 0x80000000;
            }

            bool isStrideSetup() const
            {
                return (value & 0xC0000000) == 0xC0000000;
            }

            static VPWSetup fromLiteral(uint32_t val)
            {
                return VPWSetup(val);
            }
        };

        /*
         * Setup read from VPM to QPU
         *
         * see Broadcom spec, table 33
         */
        class VPRGenericSetup : private Bitfield<uint32_t>
        {
        public:
            VPRGenericSetup(uint8_t size, uint8_t stride, uint8_t numVectors = 1, uint8_t address = 0) : Bitfield(0)
            {
                setAddress(address);
                setSize(size);
                setLaned(false);
                setHorizontal(true);
                setStride(stride);
                setNumber(numVectors);
                setID(0);
            }

            std::string to_string() const;

            /*
             * "Location of the first vector accessed."
             *
             * This is the address (in VPM) of the first vector to be read by the following VPM_READ instructions.
             *
             * See VPWGenericSetup#Address for more detailed description
             */
            BITFIELD_ENTRY(Address, uint8_t, 0, Byte)
            BITFIELD_ENTRY(WordRow, uint8_t, 0, Byte)
            BITFIELD_ENTRY(HalfWordRow, uint8_t, 1, Septuple)
            BITFIELD_ENTRY(HalfWordOffset, bool, 0, Bit)
            BITFIELD_ENTRY(ByteRow, uint8_t, 2, Sextuple)
            BITFIELD_ENTRY(ByteOffset, uint8_t, 0, Tuple)
            /*
             * "0,1,2,3 = 8-bit, 16-bit, 32-bit, reserved"
             *
             * The size of one vector-element
             *
             * NOTE:
             * The QPU always reads a whole 16-element vector from VPM!
             */
            BITFIELD_ENTRY(Size, uint8_t, 8, Tuple)
            /*
             * "0,1 = Packed, Laned. Ignored for 32-bit width"
             *
             * Whether to read the data (for non-word sizes) packed or laned, see VPWGenericSetup#Laned.
             *
             * Only packed reading is used.
             */
            BITFIELD_ENTRY(Laned, bool, 10, Bit)
            /*
             * "0,1 = Vertical, Horizontal"
             *
             * Currently only horizontal reading is used.
             */
            BITFIELD_ENTRY(Horizontal, bool, 11, Bit)
            /*
             * "Stride. This is added to ADDR after every vector read. 0 => 64."
             *
             * See VPWGenericSetup#Stride for more details
             */
            BITFIELD_ENTRY(Stride, uint8_t, 12, Sextuple)
            /*
             * "Number of vectors to read (0 => 16)."
             *
             * The number of vectors to read from the VPM.
             *
             * NOTE:
             * This needs to equal to the number of VPM_READ instructions, otherwise the VPM_READ may block infinitely.
             */
            BITFIELD_ENTRY(Number, uint8_t, 20, Quadruple)

        private:
            //"Selects VPM generic block read setup."
            BITFIELD_ENTRY(ID, uint8_t, 30, Tuple)
        };

        /*
         * Setup for DMA from RAM to VPM
         *
         * see Broadcom spec, table 36
         */
        class VPRDMASetup : private Bitfield<uint32_t>
        {
        public:
            VPRDMASetup(
                uint8_t mode, uint8_t rowLength, uint8_t numRows = 1, uint8_t vpitch = 1, uint16_t address = 0) :
                Bitfield(0)
            {
                setAddress(address);
                setVertical(false);
                setVPitch(vpitch);
                setNumberRows(numRows);
                setRowLength(rowLength);
                setMPitch(0);
                setMode(mode);
                setID(1);
            }

            std::string to_string() const;

            /*
             * "X,Y address of first 32-bit word in VPM to load to /store from."
             *
             * The starting address (in VPM) to read the data from memory into.
             *
             * See VPWDMASetup#Address for more details
             */
            BITFIELD_ENTRY(Address, uint16_t, 0, Undecuple)
            BITFIELD_ENTRY(WordRow, uint8_t, 4, Septuple)
            BITFIELD_ENTRY(WordColumn, uint8_t, 0, Quadruple)
            /*
             * "0,1 = Horizontal, Vertical"
             * Currently all reads are horizontal.
             */
            BITFIELD_ENTRY(Vertical, bool, 11, Bit)
            /*
             * "Row-to-row pitch of 2D block when loaded into VPM memory. (0 => 16).
             *  Added to the Y address and Byte/Half-word sel after each row is loaded, for both horizontal and vertical
             * modes.
             *
             *  For 8-bit width, VPITCH is added to {Y[1:0], B[1:0]}.
             *  For 16-bit width, VPITCH is added to {Y[2:0], H[0]}.
             *  For 32-bit width, VPITCH is added to Y[3:0]."
             *
             * This is the distance of two consecutive elements loaded from memory.
             *
             * Example:
             * Reading byte vectors with horizontal mode, a VPitch value of 2 (byte-offset of 2) and a VPM base-address
             * of 0 reads the first 16 bytes into VPM row 0 columns 0 to 3 and the second 16 bytes into row 0 columns 8
             * to 11, the third 16 bytes into row 1 columns 0 to 3 and so on..
             */
            BITFIELD_ENTRY(VPitch, uint8_t, 12, Quadruple)
            /*
             * "Number of rows in 2D block in memory. (0 => 16)"
             *
             * Similar to VPWDMASetup#Units and #Depth, depending on the number of vectors read into a single row,
             * this is either the number of vectors read or a factor of that number.
             *
             */
            BITFIELD_ENTRY(NumberRows, uint8_t, 16, Quadruple)
            /*
             * "Row length of 2D block in memory. In units of width (8, 16 or 32 bits). (0 => 16)"
             *
             * Similar to VPMDMASetup#Depth, this is the number of elements (of the given type) to read into a single
             * row in VPM. This needs to be at least the element-count a single vector.
             *
             * NOTE:
             * Since the maximum value is 16, multiple vectors can only be packed into the VPM for smaller vector-sizes.
             */
            BITFIELD_ENTRY(RowLength, uint8_t, 20, Quadruple)
            /*
             * "Row-to-row pitch of 2D block in memory. If MPITCH is 0, selects MPITCHB from the extended pitch setup
             * register. Otherwise, pitch = 8*2^MPITCH bytes."
             *
             * This is the pitch between rows in MEMORY!
             * Always zero, to enable the extended stride setup.
             */
            BITFIELD_ENTRY(MPitch, uint8_t, 24, Quadruple)
            /*
             * "Mode, combining width with start Byte/Half-word sel for 8 and 16-bit widths.
             * 0: width = 32-bit
             * 1: selects VPM DMA extended memory stride setup format, defined separately.
             * 2-3: width = 16-bit, Half-word sel (packed only) = MODEW[0]
             * 4-7: width = 8-bit, Byte sel (packed only) = MODEW[1:0]"
             *
             * See VPWDMASetup#Mode for the meaning of the byte/half-word offsets.
             */
            BITFIELD_ENTRY(Mode, uint8_t, 28, Triple)
            BITFIELD_ENTRY(HalfRowOffset, bool, 28, Bit)
            BITFIELD_ENTRY(ByteOffset, uint8_t, 28, Tuple)
        private:
            //"Selects VDR DMA basic setup (in addition, bits[30:28] != 1)"
            BITFIELD_ENTRY(ID, uint8_t, 31, Bit)
        };

        /*
         * Setup DMA stride
         *
         * see Broadcom spec, table 37
         */
        class VPRStrideSetup : private Bitfield<uint32_t>
        {
        public:
            explicit VPRStrideSetup(uint16_t stride = 0) : Bitfield(0)
            {
                setPitch(stride);
                setID(9);
            }

            std::string to_string() const;

            /*
             * "Row-to-row pitch of 2D block in MEMORY, in bytes. Only used if MPITCH in VPM DMA Load basic setup is 0."
             *
             * This is the address distance (on the memory side) between two vectors loaded with the same instruction.
             *
             * NOTE: In contrast to VPWStrideSetup#Stride, this is NOT the distance between the end of one row and the
             * start of another row, but instead the distance between start of row to start of row.
             *
             * So for a stride of 0, the address is not incremented and the same row is read over and over again.
             * A stride of the physical size of the row reads the consecutive row in the next read instruction, etc.
             */
            BITFIELD_ENTRY(Pitch, uint16_t, 0, Tredecuple)
        private:
            //"Selects VDR DMA extended memory stride setup"
            BITFIELD_ENTRY(ID, uint8_t, 28, Quadruple)
        };

        struct VPRSetup
        {
            union {
                uint32_t value;
                VPRGenericSetup genericSetup;
                VPRDMASetup dmaSetup;
                VPRStrideSetup strideSetup;
            };

            explicit VPRSetup(uint32_t val) : value(val) {}
            explicit VPRSetup(VPRGenericSetup generic) : genericSetup(generic) {}
            explicit VPRSetup(VPRDMASetup dma) : dmaSetup(dma) {}
            explicit VPRSetup(VPRStrideSetup stride) : strideSetup(stride) {}

            std::string to_string() const;

            operator uint32_t() const
            {
                return value;
            }

            bool isGenericSetup() const
            {
                return (value & 0xC0000000) == 0;
            }

            bool isDMASetup() const
            {
                return ((value & 0x80000000) == 0x80000000) && ((value & 0x70000000) != 0x10000000);
            }

            bool isStrideSetup() const
            {
                return (value & 0xF0000000) == 0x90000000;
            }

            static VPRSetup fromLiteral(uint32_t val)
            {
                return VPRSetup(val);
            }
        };

        /*
         * Wraps around a load-immediate instruction and acts like a bit-field.
         *
         * It automatically writes the changes to the bit-field back to the load-instruction on destruction
         */
        template <typename T>
        struct SetupWrapper : public T
        {
        private:
            using Base = T;

        public:
            explicit SetupWrapper(intermediate::LoadImmediate* load) : T(0), load(load)
            {
                if(load != nullptr)
                    Base::value = load->getImmediate().toImmediate();
            }

            ~SetupWrapper()
            {
                if(load != nullptr)
                    load->setImmediate(Literal(Base::value));
            }

            inline void resetSetup() const
            {
                if(load != nullptr)
                    Base::value = load->getImmediate().toImmediate();
            }

            inline std::string to_string() const
            {
                return Base::to_string();
            }

        private:
            intermediate::LoadImmediate* load;
        };

        using VPWSetupWrapper = SetupWrapper<VPWSetup>;
        using VPRSetupWrapper = SetupWrapper<VPRSetup>;

        /*
         * Inserts a read from the memory located at addr into the value dest
         */
        NODISCARD InstructionWalker insertReadDMA(
            Method& method, InstructionWalker it, const Value& dest, const Value& addr, const bool useMutex = true);
        /*
         * Inserts write from the value src into the memory located at addr
         */
        NODISCARD InstructionWalker insertWriteDMA(
            Method& method, InstructionWalker it, const Value& src, const Value& addr, const bool useMutex = true);

        /*
         * Tries to find a combination of a vector of an integer-type and a number of vectors to match the given size in
         * bytes.
         *
         * E.g. for 64 bytes, the pair (int16, 1) is returned and an input of 6 bytes yields the pair (short3, 1)
         */
        std::pair<DataType, uint8_t> getBestVectorSize(const int64_t numBytes);

        enum class VPMUsage
        {
            /*
             * The area of the VPM to be used as cache for general DMA access.
             *
             * NOTE:
             * This area needs to always be at offset 0, its size is calculated to match the required scratch size
             */
            SCRATCH,
            /*
             * This area is used as storage for local memory areas which fit into VPM.
             */
            LOCAL_MEMORY,
            /*
             * This area is used to spill registers into.
             *
             * NOTE:
             * Its size needs include the spilled locals for all available QPUs!
             */
            REGISTER_SPILLING,
            /*
             * This area contains data from the QPUs stack.
             *
             * NOTE:
             * Its size needs include the spilled locals for all available QPUs!
             */
            STACK
        };

        /*
         * An area of the VPM used for a specific purpose (e.g. cache, register spilling, etc.)
         */
        struct VPMArea
        {
            // the usage type of this area of VPM
            const VPMUsage usageType;
            /*
             * The row-index in VPM of the first value belonging to this area.
             *
             * For simplicity (and to enable being accessed via DMA), all VPM areas start at a new row.
             */
            const unsigned char rowOffset;
            /*
             * The size of this area in rows in VPM.
             *
             * For simplicity (and the fact that the remainder of the last row would be padded anyway), all VPM areas
             * have an integral number of rows as size (a multiple of 64 Byte).
             */
            const unsigned char numRows;
            /*
             * The (optional) memory address this area is assigned to.
             *
             * This value can either set so this area is used as cache for the given address or as a replace (the memory
             * is lowered into VPM).
             */
            const Local* originalAddress;

            void checkAreaSize(unsigned requestedSize) const;

            bool operator<(const VPMArea& other) const;

            bool requiresSpacePerQPU() const;

            /*
             * Returns the default element-type for data stored in this VPM area, depends on the usage-type of the area.
             * For e.g. local-memory, this returns the element-type of the local memory assigned to this area,
             * for register-spilling area, this returns "int16", since all registers have this size.
             *
             * NOTE:
             * This type is not necessarily accurate (e.g. for scratch, which contains elements of all sizes)
             */
            DataType getElementType() const;

            /*
             * Returns the number of scalar elements of the given type that fit into a single row.
             * The data-type defaults to this area's element-type if undefined.
             *
             * NOTE:
             * The number of elements is the number of SCALAR elements!
             */
            uint8_t getElementsInRow(const DataType& elementType) const;

            /*
             * If we pack multiple values (value = byte/half-word/word vector of size 1 to 16) into a single row (e.g.
             * up to 4 for bytes and 2 for half-words), we cannot write them to DMA unless the vector-width is 16
             * elements.
             *
             * When writing into VPM, a QPU always writes vectors of 16 elements. Since the DMA configuration cannot set
             * a stride of less than a row, we would not be able to transfer the second, third, etc. value without
             * copying all the junk of the remaining (unset) vector-elements of the previous values.
             */
            bool canBeAccessedViaDMA() const;

            /*
             * If we need this VPM area to be transferable via DMA, we cannot pack multiple values into a single row.
             * 16-element vectors make the exception (can be transferred via DMA and packed to one row)
             *
             * See #canBeAccessedViaDMA
             */
            bool canBePackedIntoRow() const;

            /*
             * Generates a QPU-to-VPM write setup for accessing the base-address of this VPM area for elements of the
             * given data-type.
             *
             * If the data-type is set to unknown, the element-type of the local associated with this area is used
             */
            VPWGenericSetup toWriteSetup(const DataType& elementType) const;

            /*
             * Generates a VPM-to-RAM DMA write setup for storing the contents of the VPM area into RAM with the given
             * element-type and number of values of the given type.
             *
             * If the data-type is set to unknown, the element-type of the local associated with this area is used
             */
            VPWDMASetup toWriteDMASetup(const DataType& elementType, uint8_t numValues = 1) const;

            /*
             * Generates a VPM-to-QPU read setup for accessing the base-address of this VPM area for the given number of
             * elements of the given data-type.
             *
             * If the data-type is set to unknown, the default element-type of this area is used
             */
            VPRGenericSetup toReadSetup(const DataType& elementType, uint8_t numValues = 1) const;

            /*
             * Generates a RAM-to-VPM DMA read setup for loading the contents of a memory address into this VPM area
             * given the element-type and numbr of values of the given type.
             *
             * If the data-type is set to unknown, the default element-type of this area is used
             */
            VPRDMASetup toReadDMASetup(const DataType& elementType, uint8_t numValues = 1) const;

            std::string to_string() const;
        };

        /*
         * Object wrapping the VPM cache component
         *
         * "From the QPU perspective the window into the locally allocated portion of the VPM is a 2D array of 32-bit
         * words, 16 words wide with a maximum height of 64 words. The array is read and written as horizontal or
         * vertical 16-way vectors of 32, 16 or 8-bit data, with natural alignment. Thus horizontal 32-bit vectors start
         * in column 0 and vertical 32-bit vectors must start on a row multiple of 16.
         *
         * To access the VPM as 16-bit or 8-bit vectors, each 32-bit vector is simply split into 2x 16-bit or 4x 8-bit
         * sub-vectors. There are two alternative split modes supported for sub-vectors: ‘laned’, where each 32-bit word
         * is split into two 16-bit lanes or four 8-bit lanes; or ‘packed’, where the 16-bit or 8-bit sub-vector is
         * taken from the whole of eight or four successive 32-bit words."
         *
         * -Broadcom specification, pages 53+
         */
        class VPM : private NonCopyable
        {
        public:
            explicit VPM(unsigned totalVPMSize = VPM_DEFAULT_SIZE);

            const VPMArea& getScratchArea() const;
            const VPMArea* findArea(const Local* local);
            const VPMArea* addArea(
                const Local* local, const DataType& elementType, bool isStackArea, unsigned numStacks = NUM_QPUS);

            /*
             * The maximum number of vectors (of the given type) which can be cached in this VPM.
             *
             * On the hardware side, this is limited to 16 for reading and 64 for writing (see Broadcom spec, page 53)
             */
            unsigned getMaxCacheVectors(const DataType& type, bool writeAccess) const;

            /*
             * Inserts a read from VPM into a QPU register
             *
             * NOTE: the inAreaOffset is the offset in bytes
             */
            NODISCARD InstructionWalker insertReadVPM(Method& method, InstructionWalker it, const Value& dest,
                const VPMArea* area = nullptr, bool useMutex = true, const Value& inAreaOffset = INT_ZERO);
            /*
             * Inserts a write from a QPU register into VPM
             *
             * NOTE: the inAreaOffset is the offset in bytes
             */
            NODISCARD InstructionWalker insertWriteVPM(Method& method, InstructionWalker it, const Value& src,
                const VPMArea* area = nullptr, bool useMutex = true, const Value& inAreaOffset = INT_ZERO);

            /*
             * Inserts a read from RAM into VPM via DMA
             */
            NODISCARD InstructionWalker insertReadRAM(Method& method, InstructionWalker it, const Value& memoryAddress,
                const DataType& type, const VPMArea* area = nullptr, bool useMutex = true,
                const Value& inAreaOffset = INT_ZERO);
            /*
             * Inserts a write from VPM into RAM via DMA
             */
            NODISCARD InstructionWalker insertWriteRAM(Method& method, InstructionWalker it, const Value& memoryAddress,
                const DataType& type, const VPMArea* area = nullptr, bool useMutex = true,
                const Value& inAreaOffset = INT_ZERO);
            /*
             * Inserts a copy from RAM via DMA and VPM into RAM
             */
            NODISCARD InstructionWalker insertCopyRAM(Method& method, InstructionWalker it, const Value& destAddress,
                const Value& srcAddress, unsigned numBytes, const VPMArea* area = nullptr, bool useMutex = true);
            /*
             * Inserts a filling of a memory-area with a single value from VPM
             */
            NODISCARD InstructionWalker insertFillRAM(Method& method, InstructionWalker it, const Value& memoryAddress,
                const DataType& type, unsigned numCopies, const VPMArea* area = nullptr, bool useMutex = true);

            /*
             * Updates the maximum size used by the scratch area.
             * This can only be called until the scratch-area is locked!
             */
            void updateScratchSize(unsigned char requestedRows);

            /*
             * Since we can only access the VPM from QPU-side in vectors of 16 elements,
             * the type needs to be converted to a type with all element-types set to 16-element vectors
             * to correctly determine the amount of VPM cache required to store the data.
             */
            static DataType getVPMStorageType(const DataType& type);

        private:
            const unsigned maximumVPMSize;
            std::vector<std::shared_ptr<VPMArea>> areas;

            InstructionWalker insertLockMutex(InstructionWalker it, bool useMutex) const;
            InstructionWalker insertUnlockMutex(InstructionWalker it, bool useMutex) const;
        };

        /*
         * Helper container to store all instructions related to a VPM load or store operation
         */
        struct VPMInstructions
        {
            /*
             * Setup for accessing the VPM QPU-side
             *
             * NOTE: This needn't exist (e.g. for memory-copies)
             */
            Optional<InstructionWalker> genericVPMSetup;
            /*
             * Setup for accessing RAM via DMA
             *
             * NOTE: This needn't exist (e.g. for using VPM as cache)
             */
            Optional<InstructionWalker> dmaSetup;
            /*
             * Setup for the DMA stride
             *
             * NOTE: This needn't exist (e.g. for using VPM as cache)
             */
            Optional<InstructionWalker> strideSetup;
            /*
             * Accessing data in the VPM (actual VPM write/read instruction)
             *
             * NOTE: This needn't exist (e.g. for memory-copies)
             */
            Optional<InstructionWalker> vpmAccess;
            /*
             * Writing of the address for DMA accesses
             *
             * NOTE: This needn't exist (e.g. for using VPM as cache)
             */
            Optional<InstructionWalker> addressWrite;
            /*
             * Waiting for DMA operation to have finished
             *
             * NOTE: This needn't exist (e.g. for using VPM as cache)
             */
            Optional<InstructionWalker> dmaWait;
        };

        /*
         * Returns the instruction related to the current VPM access of the instruction given.
         *
         * This function looks within the same mutex-lock block at the preceding and following instructions to find
         * the instructions required for the given VPM access.
         */
        VPMInstructions findRelatedVPMInstructions(InstructionWalker anyVPMInstruction, bool isVPMRead);
    } // namespace periphery
} // namespace vc4c

#endif /* VC4C_VPM_H */
