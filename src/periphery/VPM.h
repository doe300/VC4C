/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VPM_H
#define VC4C_VPM_H

#include "../Bitfield.h"
#include "../InstructionWalker.h"
#include "../Module.h"

namespace vc4c
{
	const Value VPM_IN_SETUP_REGISTER(REG_VPM_IN_SETUP, TYPE_INT32);
	const Value VPM_OUT_SETUP_REGISTER(REG_VPM_OUT_SETUP, TYPE_INT32);
	const Value VPM_IN_ADDR_REGISTER(REG_VPM_IN_ADDR, TYPE_VOID.toPointerType());
	const Value VPM_OUT_ADDR_REGISTER(REG_VPM_OUT_ADDR, TYPE_VOID.toPointerType());
	const Value VPM_IN_WAIT_REGISTER(REG_VPM_IN_WAIT, TYPE_UNKNOWN);
	const Value VPM_OUT_WAIT_REGISTER(REG_VPM_OUT_WAIT, TYPE_UNKNOWN);
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

			/*
			 * "Location of the first vector accessed."
			 *
			 * This is the address (in VPM) to write the first vector to. All consecutive writes write to this address incremented by the type-width + stride
			 */
			BITFIELD_ENTRY(Address, uint8_t, 0, Byte)
			/*
			 * "0,1,2,3 = 8-bit, 16-bit, 32-bit, reserved"
			 */
			BITFIELD_ENTRY(Size, uint8_t, 8, Tuple)
			/*
			 * "0,1 = Packed, Laned. Ignored for 32-bit width"
			 *
			 * In packed mode, writes into the VPM have consecutive addresses, e.g. | b1 | b2 | b3 | ... |.
			 * In laned mode, every entry (byte, half-word, word) has an offset of 1 word to the previous, e.g. | b1 | xx | xx | xx | b2 | xx | xx | xx | b3 | ... |.
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
			 * In practice, this is the offset to the next n-elment 32-bit row in the width of the element type.
			 * So for now, setting this to 4 for bytes, 2 for half-words and 1 for words seems to be correct.
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

			/*
			 * "Mode, combining width with start Byte/Half-word offset for 8 and 16-bit widths.
			 * 0: width = 32-bit
			 * 1: Unused.
			 * 2-3: width = 16-bit, Half-word offset (packed only) = MODEW[0]
			 * 4-7: width = 8-bit, Byte offset (packed only) = MODEW[1:0]"
			 *
			 * The byte/half-word offsets determine how many bytes/half-words to skip in a 32-bit word.
			 * So a byte-offset of 2 for the word | b1 | b2 | b3 | b4 | would start at | b3 |.
			 * There is currently no need to set the offset to any other value than 0.
			 */
			BITFIELD_ENTRY(Mode, uint8_t, 0, Triple)
			/*
			 * "X,Y address of first 32-bit word in VPM to load to/store from. ADDRA[10:0] = {Y[6:0], X[3:0]}"
			 *
			 * This determines the address (in VPM, in X and Y coordinates) to be written into memory.
			 * For now, we do not use the VPM as cache for some other data, so we always write (and read) from the start (address 0).
			 */
			BITFIELD_ENTRY(VPMBase, uint16_t, 3, Undecuple)
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
			 *
			 * Currently a row represents a SIMD-vector, so this is the number of vector-elements.
			 */
			BITFIELD_ENTRY(Depth, uint8_t, 16, Septuple)
			/*
			 * "Number of Rows of 2D block in memory (0 => 128)"
			 *
			 * The total number of rows (of length Depth * element-size) to be written into memory.
			 * Since a row is a SIMD-vector, this equals the number of vectors to write
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

			/*
			 * "Distance between last byte of a row and start of next row in memory, in bytes."
			 * Addendum to the Broadcom documentation: "Unlike the documentation suggests the STRIDE field is 16 bits wide."
			 *
			 * This is the distance in memory between two consecutive rows (vectors) written.
			 * With a value of 0, the vectors v1, v2, v3, ... will be in consecutive addresses: | v1 | v2 | v3 | ... |
			 * With a value of 1, there will be space of the byte width of one vector (or one element??) between each vector: | v1 | xx | v2 | xx | v3 | ... |
			 * Since we write consecutive memory, this is always 0.
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
			union
			{
				uint32_t value;
				VPWGenericSetup genericSetup;
				VPWDMASetup dmaSetup;
				VPWStrideSetup strideSetup;
			};

			explicit VPWSetup(uint32_t val) : value(val) { }
			explicit VPWSetup(VPWGenericSetup generic) : genericSetup(generic) { }
			explicit VPWSetup(VPWDMASetup dma) : dmaSetup(dma) { }
			explicit VPWSetup(VPWStrideSetup stride) : strideSetup(stride) { }

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

			static VPWSetup fromLiteral(uint64_t val)
			{
				return VPWSetup(static_cast<uint32_t>(val));
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

			/*
			 * "Location of the first vector accessed."
			 *
			 * This is the address (in VPM) of the first vector to be read by the following VPM_READ instructions.
			 * Currently always 0.
			 */
			BITFIELD_ENTRY(Address, uint8_t, 0, Byte)
			/*
			 * "0,1,2,3 = 8-bit, 16-bit, 32-bit, reserved"
			 */
			BITFIELD_ENTRY(Size, uint8_t, 8, Tuple)
			/*
			 * "0,1 = Packed, Laned. Ignored for 32-bit width"
			 *
			 * Whether to read the data (for non-word sizes) packed or laned, see VPWGenericSetup#Laned.
			 * Only packed reading is used.
			 */
			BITFIELD_ENTRY(Laned, bool, 10, Bit)
			/*
			 * "0,1 = Vertical, Horizontal"
			 * Currently only horizontal reading is used.
			 */
			BITFIELD_ENTRY(Horizontal, bool, 11, Bit)
			/*
			 * "Stride. This is added to ADDR after every vector read. 0 => 64."
			 *
			 * Same as for VPW generic setup, this value is in the byte-size of the element-type and added to the address (in VPM).
			 * The result is currently the offset to the next 32-bit vector and therefore the size of a word in units of the type specified.
			 * So for bytes, this is 4, for half-words 2 and for words 1.
			 */
			BITFIELD_ENTRY(Stride, uint8_t, 12, Sextuple)
			/*
			 * "Number of vectors to read (0 => 16)."
			 *
			 * The number of vectors to read from the VPM. This needs to equal to the number of VPM_READ instructions,
			 * otherwise the VPM_READ may block infinitely.
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

			VPRDMASetup(uint8_t mode, uint8_t rowLength, uint8_t numRows = 1, uint8_t vpitch = 1, uint16_t address = 0) : Bitfield(0)
			{
				setAddress(address);
				setVertical(false);
				setVPitch(vpitch);
				setNumberRows(numRows);
				setRowLength(rowLength);
				setMPitch(0);
				setID(1);
			}

			/*
			 * "X,Y address of first 32-bit word in VPM to load to /store from."
			 *
			 * The starting address (in VPM) to read the data from memory into.
			 * For now, always 0.
			 */
			BITFIELD_ENTRY(Address, uint16_t, 0, Undecuple)
			/*
			 * "0,1 = Horizontal, Vertical"
			 * Currently all reads are horizontal.
			 */
			BITFIELD_ENTRY(Vertical, bool, 11, Bit)
			/*
			 * "Row-to-row pitch of 2D block when loaded into VPM memory. (0 => 16).
			 *  Added to the Y address and Byte/Half-word sel after each row is loaded, for both horizontal and vertical modes."
			 *
			 * This is the distance (in rows in VPM) of two consecutive elements loaded from memory.
			 * Since we only load consecutive memory with consecutive loads, this is always 1.
			 */
			BITFIELD_ENTRY(VPitch, uint8_t, 12, Quadruple)
			/*
			 * "Number of rows in 2D block in memory. (0 => 16)"
			 *
			 * Since a row is used for a single SIMD-vector, this is the number of vectors to read from memory to VPM.
			 */
			BITFIELD_ENTRY(NumberRows, uint8_t, 16, Quadruple)
			/*
			 * "Row length of 2D block in memory. In units of width (8, 16 or 32 bits). (0 => 16)"
			 *
			 * We use a row for a single SIMD-vector, so this is the number of vector-elements.
			 */
			BITFIELD_ENTRY(RowLength, uint8_t, 20, Quadruple)
			/*
			 * "Row-to-row pitch of 2D block in memory. If MPITCH is 0, selects MPITCHB from the extended pitch setup register. Otherwise, pitch = 8*2^MPITCH bytes."
			 *
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
			 * See VPWDMASetup#Mode for the meaning of the byte/half-word offsets. They are always zero for now.
			 */
			BITFIELD_ENTRY(Mode, uint8_t, 28, Triple)
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
				setStride(stride);
				setID(9);
			}

			/*
			 * "Row-to-row pitch of 2D block in memory, in bytes. Only used if MPITCH in VPM DMA Load basic setup is 0."
			 *
			 * This is the address distance (on the memory side) between two vectors loaded with the same instruction.
			 * Similar to VPWStrideSetup#Stride, this is currently always 0.
			 * A value of e.g. 1 would probably (untested) read the vectors in this fashion: | v1 | xx | v2 | xx | v3 | ... |, skipping every other vector (or element?)
			 */
			BITFIELD_ENTRY(Stride, uint16_t, 0, Tredecuple)
		private:
			//"Selects VDR DMA extended memory stride setup"
			BITFIELD_ENTRY(ID, uint8_t, 28, Quadruple)
		};

		struct VPRSetup
		{
			union
			{
				uint32_t value;
				VPRGenericSetup genericSetup;
				VPRDMASetup dmaSetup;
				VPRStrideSetup strideSetup;
			};

			explicit VPRSetup(uint32_t val) : value(val) { }
			explicit VPRSetup(VPRGenericSetup generic) : genericSetup(generic) { }
			explicit VPRSetup(VPRDMASetup dma) : dmaSetup(dma) { }
			explicit VPRSetup(VPRStrideSetup stride) : strideSetup(stride) { }

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
				return ((value & 0xC0000000) == 0x80000000) && ((value & 0x70000000) != 0x10000000);
			}

			bool isStrideSetup() const
			{
				return (value & 0xF0000000) == 0x90000000;
			}

			static VPRSetup fromLiteral(uint64_t val)
			{
				return VPRSetup(static_cast<uint32_t>(val));
			}
		};

		/*
		 * Wraps around a load-immediate instruction and acts like a bit-field.
		 *
		 * It automatically writes the changes to the bit-field back to the load-instruction on destruction
		 */
		template<typename T>
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
					load->setImmediate(Literal(static_cast<uint64_t>(Base::value)));
			}

			inline void resetSetup() const
			{
				if(load != nullptr)
					Base::value = load->getImmediate().toImmediate();
			}

		private:
			intermediate::LoadImmediate* load;
		};

		using VPWSetupWrapper = SetupWrapper<VPWSetup>;
		using VPRSetupWrapper = SetupWrapper<VPRSetup>;

		/*
		 * Inserts a read from the memory located at addr into the value dest
		 */
		InstructionWalker insertReadDMA(Method& method, InstructionWalker it, const Value& dest, const Value& addr, const bool useMutex = true);
		/*
		 * Inserts write from the value src into the memory located at addr
		 */
		InstructionWalker insertWriteDMA(Method& method, InstructionWalker it, const Value& src, const Value& addr, const bool useMutex = true);

		enum class VPMUsage
		{
			//the area of the VPM to be used as cache for general DMA access
			//This area needs to always be at offset 0, its size is calculated to match the required scratch size
			GENERAL_DMA,
			//part of the VPM used as cache for DMA access to specific memory regions
			SPECIFIC_DMA,
			//this area is used to spill registers into
			REGISTER_SPILLING
		};

		/*
		 * An area of the VPM used for a specific purpose (e.g. cache, register spilling, etc.)
		 */
		struct VPMArea
		{
			//the usage type of this area of VPM
			const VPMUsage usageType;
			//the base offset (from the start of the VPM) in bytes
			const unsigned baseOffset;
			//the size of this area (in bytes)
			const unsigned size;
			//the (optional) DMA address this area is assigned to (as DMA cache)
			const Local* dmaAddress;

			void checkAreaSize(unsigned requestedSize) const;

			bool operator<(const VPMArea& other) const;

			bool requiresSpacePerQPU() const;
			unsigned getTotalSize() const;
		};

		/*
		 * Object wrapping the VPM cache component
		 */
		class VPM : private NonCopyable
		{
		public:
			explicit VPM(unsigned totalVPMSize);

			const VPMArea& getScratchArea();
			const VPMArea* findArea(const Local* local);
			const VPMArea* addArea(const Local* local, unsigned requestedSize, bool alignToBack = false);

			/*
			 * The maximum number of vectors (of the given type) which can be cached in this VPM.
			 *
			 * On the hardware side, this is limited to 16 for reading and 64 for writing (see Broadcom spec, page 53)
			 */
			unsigned getMaxCacheVectors(const DataType& type, bool writeAccess) const;

			/*
			 * Inserts a read from VPM into a QPU register
			 */
			InstructionWalker insertReadVPM(InstructionWalker it, const Value& dest, const VPMArea* area = nullptr, bool useMutex = true);
			/*
			 * Inserts a write from a QPU register into VPM
			 */
			InstructionWalker insertWriteVPM(InstructionWalker it, const Value& src, const VPMArea* area = nullptr, bool useMutex = true);

			/*
			 * Inserts a read from RAM into VPM via DMA
			 */
			InstructionWalker insertReadRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, const VPMArea* area = nullptr, bool useMutex = true);
			/*
			 * Inserts a write from VPM into RAM via DMA
			 */
			InstructionWalker insertWriteRAM(InstructionWalker it, const Value& memoryAddress, const DataType& type, const VPMArea* area = nullptr, bool useMutex = true);
			/*
			 * Inserts a copy from RAM via DMA and VPM into RAM
			 */
			InstructionWalker insertCopyRAM(Method& method, InstructionWalker it, const Value& destAddress, const Value& srcAddress, unsigned numBytes, const VPMArea* area = nullptr, bool useMutex = true);
			/*
			 * Inserts a filling of a memory-area with a single value from VPM
			 */
			InstructionWalker insertFillRAM(Method& method, InstructionWalker it, const Value& memoryAddress, const DataType& type, unsigned numCopies, const VPMArea* area = nullptr, bool useMutex = true);

			/*
			 * Updates the maximum size used by the scratch area.
			 * This can only be called until the scratch-area is locked!
			 */
			void updateScratchSize(unsigned requestedSize);

		private:
			const unsigned maximumVPMSize;
			std::set<VPMArea> areas;
			//whether the scratch area is locked to a fixed size
			bool isScratchLocked;

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
