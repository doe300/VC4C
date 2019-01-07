/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TOOLS_EMULATOR_H
#define VC4C_TOOLS_EMULATOR_H

#include "../Values.h"
#include "../asm/OpCodes.h"
#include "../performance.h"
#include "config.h"
#include "tools.h"

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
            explicit Memory(std::size_t size) : data(size)
            {
                data.resize(size, 0);
            }

            Word* getWordAddress(MemoryAddress address);

            Value readWord(MemoryAddress address) const;
            MemoryAddress incrementAddress(MemoryAddress address, const DataType& typeSize) const;

            MemoryAddress getMaximumAddress() const;
            void setUniforms(const std::vector<Word>& uniforms, MemoryAddress address);

        private:
            std::vector<Word> data;
        };

        class Mutex : private NonCopyable
        {
        public:
            explicit Mutex() : locked(false), lockOwner(255) {}

            bool isLocked() const;
            NODISCARD bool lock(uint8_t qpu);
            void unlock(uint8_t qpu);

        private:
            bool locked;
            uint8_t lockOwner;
        };

        class Registers : private NonCopyable
        {
        public:
            explicit Registers(QPU& qpu) : qpu(qpu), hostInterrupt(NO_VALUE) {}

            void writeRegister(Register reg, const Value& val, std::bitset<16> elementMask);
            std::pair<Value, bool> readRegister(Register reg);

            Value getInterruptValue() const;

            void clearReadCache();

        private:
            QPU& qpu;
            FastMap<Register, Value> storageRegisters;
            Optional<Value> hostInterrupt;
            OrderedMap<Register, Value> readCache;

            Value getActualValue(const Value& val);

            Value readStorageRegister(Register reg);
            void writeStorageRegister(Register reg, const Value& val, std::bitset<16> elementMask);
            void setReadCache(Register reg, const Value& val);
        };

        class UniformCache : private NonCopyable
        {
        public:
            UniformCache(QPU& qpu, Memory& memory, MemoryAddress uniformAddress) :
                qpu(qpu), memory(memory), uniformAddress(uniformAddress), lastAddressSetCycle(0)
            {
            }

            Value readUniform();
            void setUniformAddress(const Value& val);

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

            std::pair<Value, bool> readTMU();
            bool hasValueOnR4() const;

            void setTMUNoSwap(const Value& swapVal);
            void setTMURegisterS(uint8_t tmu, const Value& val);
            void setTMURegisterT(uint8_t tmu, const Value& val);
            void setTMURegisterR(uint8_t tmu, const Value& val);
            void setTMURegisterB(uint8_t tmu, const Value& val);

            NODISCARD bool triggerTMURead(uint8_t tmu);

        private:
            QPU& qpu;
            bool tmuNoSwap;
            uint32_t lastTMUNoSwap;
            Memory& memory;
            std::queue<std::pair<Value, uint32_t>> tmu0RequestQueue;
            std::queue<std::pair<Value, uint32_t>> tmu0ResponseQueue;
            std::queue<std::pair<Value, uint32_t>> tmu1RequestQueue;
            std::queue<std::pair<Value, uint32_t>> tmu1ResponseQueue;

            void checkTMUWriteCycle() const;
            Value readMemoryAddress(const Value& address) const;
            uint8_t toRealTMU(uint8_t tmu) const;
        };

        class SFU : private NonCopyable
        {
        public:
            explicit SFU() : lastSFUWrite(0), currentCycle(0), sfuResult(NO_VALUE) {}

            Value readSFU();
            bool hasValueOnR4() const;

            void startRecip(const Value& val);
            void startRecipSqrt(const Value& val);
            void startExp2(const Value& val);
            void startLog2(const Value& val);

            void incrementCycle();

        private:
            // FIXME is SFU calculation per QPU? Or do QPUs need to lock the SFU access?
            // XXX per QPU cycle??
            uint32_t lastSFUWrite;
            uint32_t currentCycle;
            Optional<Value> sfuResult;
        };

        class VPM : private NonCopyable
        {
        public:
            VPM(Memory& memory) : memory(memory), currentCycle(0)
            {
                vpmReadSetup.fill(0);
                vpmWriteSetup.fill(0);
                dmaReadSetup.fill(0);
                dmaWriteSetup.fill(0);
                readStrideSetup.fill(0);
                writeStrideSetup.fill(0);
                lastDMAReadTrigger.fill(0);
                lastDMAWriteTrigger.fill(0);
            }

            Value readValue(unsigned char qpu);
            void writeValue(unsigned char qpu, const Value& val);

            void setWriteSetup(unsigned char qpu, const Value& val);
            void setReadSetup(unsigned char qpu, const Value& val);

            void setDMAWriteAddress(unsigned char qpu, const Value& val);
            void setDMAReadAddress(unsigned char qpu, const Value& val);

            NODISCARD bool waitDMAWrite(unsigned char qpu) const;
            NODISCARD bool waitDMARead(unsigned char qpu) const;

            void incrementCycle();

            void dumpContents() const;

        private:
            Memory& memory;
            std::array<uint32_t, NUM_QPUS> vpmReadSetup;
            std::array<uint32_t, NUM_QPUS> vpmWriteSetup;
            std::array<uint32_t, NUM_QPUS> dmaReadSetup;
            std::array<uint32_t, NUM_QPUS> dmaWriteSetup;
            std::array<uint32_t, NUM_QPUS> readStrideSetup;
            std::array<uint32_t, NUM_QPUS> writeStrideSetup;
            std::array<uint32_t, NUM_QPUS> lastDMAReadTrigger;
            std::array<uint32_t, NUM_QPUS> lastDMAWriteTrigger;
            uint32_t currentCycle;

            std::array<std::array<Word, 16>, 64> cache;
        };

        class Semaphores : private NonCopyable
        {
        public:
            explicit Semaphores()
            {
                counter.fill(0);
            }

            std::pair<Value, bool> increment(uint8_t index);
            std::pair<Value, bool> decrement(uint8_t index);

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
            std::pair<Value, bool> readR4();

            NODISCARD bool execute(std::vector<std::unique_ptr<qpu_asm::Instruction>>::const_iterator firstInstruction);

            const qpu_asm::Instruction* getCurrentInstruction(
                std::vector<std::unique_ptr<qpu_asm::Instruction>>::const_iterator firstInstruction) const;

        private:
            Mutex& mutex;
            Registers registers;
            UniformCache uniforms;
            TMUs tmus;
            SFU& sfu;
            VPM& vpm;
            Semaphores& semaphores;
            uint32_t currentCycle;
            std::array<ElementFlags, vc4c::NATIVE_VECTOR_SIZE> flags;
            ProgramCounter pc;
            InstrumentationResults& instrumentation;

            friend class Registers;
            friend class UniformCache;
            friend class TMUs;
            friend class SFU;
            friend class VPM;

            NODISCARD bool executeALU(const qpu_asm::ALUInstruction* aluInst);
            void writeConditional(Register dest, const Value& in, ConditionCode cond,
                const qpu_asm::ALUInstruction* addInst = nullptr, const qpu_asm::ALUInstruction* mulInst = nullptr);
            bool isConditionMet(BranchCond cond) const;
            NODISCARD bool executeSignal(Signaling signal);
            void setFlags(const Value& output, ConditionCode cond, const VectorFlags& newFlags);
        };

        std::vector<MemoryAddress> buildUniforms(Memory& memory, MemoryAddress baseAddress,
            const std::vector<MemoryAddress>& parameter, const WorkGroupConfig& config, MemoryAddress globalData,
            const KernelUniforms& uniformsUsed);
        bool emulate(std::vector<std::unique_ptr<qpu_asm::Instruction>>::const_iterator firstInstruction,
            Memory& memory, const std::vector<MemoryAddress>& uniformAddresses, InstrumentationResults& instrumentation,
            uint32_t maxCycles = std::numeric_limits<uint32_t>::max());
        bool emulateTask(std::vector<std::unique_ptr<qpu_asm::Instruction>>::const_iterator firstInstruction,
            const std::vector<MemoryAddress>& parameter, Memory& memory, MemoryAddress uniformBaseAddress,
            MemoryAddress globalData, const KernelUniforms& uniformsUsed, InstrumentationResults& instrumentation,
            uint32_t maxCycles = std::numeric_limits<uint32_t>::max());
    } // namespace tools
} // namespace vc4c

#endif /* VC4C_TOOLS_EMULATOR_H */
