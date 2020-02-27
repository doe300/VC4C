/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TOOLS_EMULATOR_H
#define VC4C_TOOLS_EMULATOR_H

#include "../SIMDVector.h"
#include "../Values.h"
#include "../asm/OpCodes.h"
#include "../performance.h"
#include "config.h"
#include "tools.h"

#include <array>
#include <bitset>
#include <limits>
#include <queue>

namespace vc4c
{
    struct KernelUniforms;

    namespace qpu_asm
    {
        class Instruction;
        class ALUInstruction;
    } // namespace qpu_asm

    namespace tools
    {
        class QPU;

        using MemoryAddress = uint32_t;
        using Word = uint32_t;

        class Memory : private NonCopyable
        {
        public:
            /*
             * Use a direct buffer
             */
            explicit Memory(std::size_t size) : data(DirectBuffer(size, 0xDEADBEEF)) {}
            /*
             * Use a mapping of existing buffers
             *
             * The first element is the start "device address", the second element the buffer mapped for the part
             * [start "device address", start "device address"+ buffer.size())
             */
            explicit Memory(const std::map<uint32_t, std::reference_wrapper<std::vector<uint8_t>>>& buffers) :
                data(MappedBuffers(buffers))
            {
            }

            Word* getWordAddress(MemoryAddress address);
            const Word* getWordAddress(MemoryAddress address) const;

            Word readWord(MemoryAddress address) const;
            MemoryAddress incrementAddress(MemoryAddress address, DataType typeSize) const;

            MemoryAddress getMaximumAddress() const;
            void setUniforms(const std::vector<Word>& uniforms, MemoryAddress address);

        private:
            using DirectBuffer = std::vector<Word>;
            using MappedBuffers = std::map<uint32_t, std::reference_wrapper<std::vector<uint8_t>>>;
            Variant<DirectBuffer, MappedBuffers> data;
        };

        class Mutex : private NonCopyable
        {
        public:
            bool isLocked() const;
            NODISCARD bool lock(uint8_t qpu);
            void unlock(uint8_t qpu);

        private:
            bool locked{false};
            uint8_t lockOwner{255};
        };

        class Registers : private NonCopyable
        {
        public:
            explicit Registers(QPU& qpu) : qpu(qpu), hostInterrupt() {}

            void writeRegister(Register reg, const SIMDVector& val, std::bitset<16> elementMask, BitMask bitMask);
            std::pair<SIMDVector, bool> readRegister(Register reg, bool anyElementUsed);

            SIMDVector getInterruptValue() const;

            void clearReadCache();

        private:
            QPU& qpu;
            std::array<std::pair<SIMDVector, uint32_t>, 4 * 64> storageRegisters;
            Optional<SIMDVector> hostInterrupt;
            SortedMap<Register, SIMDVector> readCache;

            SIMDVector readStorageRegister(Register reg, bool anyElementUsed);
            void writeStorageRegister(Register reg, SIMDVector&& val, std::bitset<16> elementMask, BitMask bitMask);
            NODISCARD SortedMap<Register, SIMDVector>::iterator setReadCache(Register reg, const SIMDVector& val);
        };

        class UniformCache : private NonCopyable
        {
        public:
            UniformCache(QPU& qpu, Memory& memory, MemoryAddress uniformAddress) :
                qpu(qpu), memory(memory), uniformAddress(uniformAddress), lastAddressSetCycle(0)
            {
            }

            SIMDVector readUniform();
            void setUniformAddress(const SIMDVector& val);

        private:
            QPU& qpu;
            Memory& memory;
            MemoryAddress uniformAddress;
            uint32_t lastAddressSetCycle;
        };

        class TMUs : private NonCopyable
        {
        public:
            TMUs(QPU& qpu, Memory& memory) : qpu(qpu), tmuNoSwap(false), lastTMUNoSwap(0), memory(memory) {}

            std::pair<SIMDVector, bool> readTMU();
            bool hasValueOnR4() const;

            void setTMUNoSwap(const SIMDVector& swapVal);
            void setTMURegisterS(uint8_t tmu, const SIMDVector& val);
            void setTMURegisterT(uint8_t tmu, const SIMDVector& val);
            void setTMURegisterR(uint8_t tmu, const SIMDVector& val);
            void setTMURegisterB(uint8_t tmu, const SIMDVector& val);

            NODISCARD bool triggerTMURead(uint8_t tmu);

        private:
            QPU& qpu;
            bool tmuNoSwap;
            uint32_t lastTMUNoSwap;
            Memory& memory;
            std::queue<std::pair<SIMDVector, uint32_t>> tmu0RequestQueue;
            std::queue<std::pair<SIMDVector, uint32_t>> tmu0ResponseQueue;
            std::queue<std::pair<SIMDVector, uint32_t>> tmu1RequestQueue;
            std::queue<std::pair<SIMDVector, uint32_t>> tmu1ResponseQueue;

            void checkTMUWriteCycle() const;
            SIMDVector readMemoryAddress(const SIMDVector& address) const;
            uint8_t toRealTMU(uint8_t tmu) const;
        };

        class SFU : private NonCopyable
        {
        public:
            SIMDVector readSFU();
            bool hasValueOnR4() const;

            void startRecip(const SIMDVector& val);
            void startRecipSqrt(const SIMDVector& val);
            void startExp2(const SIMDVector& val);
            void startLog2(const SIMDVector& val);

            void incrementCycle();

        private:
            // FIXME is SFU calculation per QPU? Or do QPUs need to lock the SFU access?
            // XXX per QPU cycle??
            uint32_t lastSFUWrite{0};
            uint32_t currentCycle{0};
            Optional<SIMDVector> sfuResult{};
        };

        class VPM : private NonCopyable
        {
        public:
            explicit VPM(Memory& memory) :
                memory(memory), vpmReadSetup(0), vpmWriteSetup(0), dmaReadSetup(0), dmaWriteSetup(0),
                readStrideSetup(0), writeStrideSetup(0), lastDMAReadTrigger(0), lastDMAWriteTrigger(0), currentCycle(0),
                cache({})
            {
            }

            SIMDVector readValue();
            void writeValue(const SIMDVector& val);

            void setWriteSetup(const SIMDVector& val);
            void setReadSetup(const SIMDVector& val);

            void setDMAWriteAddress(const SIMDVector& val);
            void setDMAReadAddress(const SIMDVector& val);

            NODISCARD bool waitDMAWrite() const;
            NODISCARD bool waitDMARead() const;

            void incrementCycle();

            void dumpContents() const;

        private:
            Memory& memory;
            uint32_t vpmReadSetup;
            uint32_t vpmWriteSetup;
            uint32_t dmaReadSetup;
            uint32_t dmaWriteSetup;
            uint32_t readStrideSetup;
            uint32_t writeStrideSetup;
            uint32_t lastDMAReadTrigger;
            uint32_t lastDMAWriteTrigger;
            uint32_t currentCycle;

            std::array<std::array<Word, 16>, 64> cache;
        };

        class Semaphores : private NonCopyable
        {
        public:
            explicit Semaphores() : counter{}
            {
                counter.fill(0);
            }

            std::pair<SIMDVector, bool> increment(uint8_t index);
            std::pair<SIMDVector, bool> decrement(uint8_t index);

            void checkAllZero() const;

        private:
            std::array<uint8_t, 16> counter;
        };

        using ProgramCounter = uint32_t;

        using InstrumentationResults = FastMap<const qpu_asm::Instruction*, InstrumentationResult>;

        class QPU : private NonCopyable
        {
        public:
            QPU(uint8_t id, Mutex& mutex, SFU& sfu, VPM& vpm, Semaphores& semaphores, Memory& memory,
                MemoryAddress uniformAddress, InstrumentationResults& instrumentation) :
                ID(id),
                mutex(mutex), registers(*this), uniforms(*this, memory, uniformAddress), tmus(*this, memory), sfu(sfu),
                vpm(vpm), semaphores(semaphores), currentCycle(0), pc(0), instrumentation(instrumentation)
            {
            }

            const uint8_t ID;

            uint32_t getCurrentCycle() const;
            std::pair<SIMDVector, bool> readR4();

            NODISCARD bool execute(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction);

            const qpu_asm::Instruction* getCurrentInstruction(
                std::vector<qpu_asm::Instruction>::const_iterator firstInstruction) const;

        private:
            Mutex& mutex;
            Registers registers;
            UniformCache uniforms;
            TMUs tmus;
            SFU& sfu;
            VPM& vpm;
            Semaphores& semaphores;
            uint32_t currentCycle;
            VectorFlags flags;
            ProgramCounter pc;
            InstrumentationResults& instrumentation;

            friend class Registers;
            friend class UniformCache;
            friend class TMUs;
            friend class SFU;
            friend class VPM;

            NODISCARD bool executeALU(const qpu_asm::ALUInstruction* aluInst);
            void writeConditional(Register dest, const SIMDVector& in, ConditionCode cond, BitMask bitMask,
                const qpu_asm::ALUInstruction* addInst = nullptr, const qpu_asm::ALUInstruction* mulInst = nullptr);
            bool isConditionMet(BranchCond cond) const;
            NODISCARD bool executeSignal(Signaling signal);
            void setFlags(const SIMDVector& output, ConditionCode cond, const VectorFlags& newFlags);
        };

        std::vector<MemoryAddress> buildUniforms(Memory& memory, MemoryAddress baseAddress,
            const std::vector<MemoryAddress>& parameter, const WorkGroupConfig& config, MemoryAddress globalData,
            const KernelUniforms& uniformsUsed);
        bool emulate(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction, Memory& memory,
            const std::vector<MemoryAddress>& uniformAddresses, InstrumentationResults& instrumentation,
            uint32_t maxCycles = std::numeric_limits<uint32_t>::max());
        bool emulateTask(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction,
            const std::vector<MemoryAddress>& parameter, Memory& memory, MemoryAddress uniformBaseAddress,
            MemoryAddress globalData, const KernelUniforms& uniformsUsed, InstrumentationResults& instrumentation,
            uint32_t maxCycles = std::numeric_limits<uint32_t>::max());
    } // namespace tools
} // namespace vc4c

#endif /* VC4C_TOOLS_EMULATOR_H */
