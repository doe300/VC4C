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
#include <atomic>
#include <bitset>
#include <cstdint>
#include <deque>
#include <future>
#include <limits>
#include <memory>
#include <mutex>
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
        class Slice;
        class QPU;
        class EmulationClock;

        template <typename T>
        using AsynchronousHandle = std::shared_ptr<std::promise<T>>;

        struct AsynchronousExecution
        {
            std::string name;
            std::function<bool(uint32_t)> func;

            bool operator()(uint32_t currentCycle) const
            {
                return func(currentCycle);
            }
        };

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
            AsynchronousExecution startMemoryRead(AsynchronousHandle<Word>&& handle, MemoryAddress address) const;
            MemoryAddress incrementAddress(MemoryAddress address, DataType typeSize) const;

            MemoryAddress getMaximumAddress() const;
            void assertAddressInMemory(MemoryAddress address, std::size_t numBytes) const;
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
            static constexpr uint8_t NO_OWNER = 255;
            std::atomic<std::uint8_t> lockedOwner{NO_OWNER};
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

        class UniformFifo : private NonCopyable
        {
        public:
            UniformFifo(EmulationClock& clock, QPU& qpu, Slice& slice, MemoryAddress uniformAddress) :
                clock(clock), qpu(qpu), slice(slice), uniformAddress(uniformAddress), lastAddressSetCycle(0), fifo()
            {
            }

            std::pair<SIMDVector, bool> readUniform();
            void setUniformAddress(const SIMDVector& val);

            void triggerFifoFill();

        private:
            EmulationClock& clock;
            QPU& qpu;
            Slice& slice;
            MemoryAddress uniformAddress;
            uint32_t lastAddressSetCycle;
            std::deque<std::future<Word>> fifo;
        };

        class TMUs : private NonCopyable
        {
        public:
            TMUs(EmulationClock& clock, QPU& qpu, Slice& slice) :
                clock(clock), qpu(qpu), tmuNoSwap(false), lastTMUNoSwap(0), slice(slice)
            {
            }

            void setTMUNoSwap(const SIMDVector& swapVal);
            void setTMURegisterS(uint8_t tmu, const SIMDVector& val);
            void setTMURegisterT(uint8_t tmu, const SIMDVector& val);
            void setTMURegisterR(uint8_t tmu, const SIMDVector& val);
            void setTMURegisterB(uint8_t tmu, const SIMDVector& val);

            NODISCARD bool triggerTMURead(uint8_t tmu, SIMDVector& r4Register);

        private:
            EmulationClock& clock;
            QPU& qpu;
            bool tmuNoSwap;
            uint32_t lastTMUNoSwap;
            Slice& slice;
            // Technically there are 2 FIFOs per TMU (request and response), but from a functional view, this makes no
            // difference, since we provide the response for a request immediately in the emulator
            std::queue<std::future<SIMDVector>> tmu0Queue;
            std::queue<std::future<SIMDVector>> tmu1Queue;

            void checkTMUWriteCycle() const;
            std::future<SIMDVector> readMemoryAddress(uint8_t tmu, const SIMDVector& address) const;
            uint8_t toRealTMU(uint8_t tmu) const;
        };

        class SFU : private NonCopyable
        {
        public:
            SFU(EmulationClock& clock) : clock(clock) {}

            void startRecip(const SIMDVector& val, SIMDVector& r4Register);
            void startRecipSqrt(const SIMDVector& val, SIMDVector& r4Register);
            void startExp2(const SIMDVector& val, SIMDVector& r4Register);
            void startLog2(const SIMDVector& val, SIMDVector& r4Register);

        private:
            EmulationClock& clock;

            void startOperation(SIMDVector&& result, SIMDVector& r4Register);
        };

        class VPM : private NonCopyable
        {
        public:
            explicit VPM(EmulationClock& clock, Memory& memory) :
                clock(clock), memory(memory), vpmReadSetup(0), vpmWriteSetup(0), dmaReadSetup(0), dmaWriteSetup(0),
                readStrideSetup(0), writeStrideSetup(0), lastDMAReadTrigger(0), lastDMAWriteTrigger(0), cache({})
            {
                // just some dummy data to simulate previous values
                std::for_each(cache.begin(), cache.end(), [](auto& entry) { entry.fill(0xDEADDEAD); });
            }

            SIMDVector readValue();
            void writeValue(const SIMDVector& val);

            void setWriteSetup(const SIMDVector& val);
            void setReadSetup(const SIMDVector& val);

            void setDMAWriteAddress(const SIMDVector& val);
            void setDMAReadAddress(const SIMDVector& val);

            NODISCARD bool waitDMAWrite() const;
            NODISCARD bool waitDMARead() const;

            void dumpContents() const;

        private:
            EmulationClock& clock;
            Memory& memory;
            uint32_t vpmReadSetup;
            uint32_t vpmWriteSetup;
            uint32_t dmaReadSetup;
            uint32_t dmaWriteSetup;
            uint32_t readStrideSetup;
            uint32_t writeStrideSetup;
            uint32_t lastDMAReadTrigger;
            uint32_t lastDMAWriteTrigger;

            std::array<std::array<Word, 16>, 64> cache;
        };

        class Semaphores : private NonCopyable
        {
        public:
            explicit Semaphores() : counter{}
            {
                counter.fill(0);
            }

            std::pair<SIMDVector, bool> increment(uint8_t index, uint8_t qpu);
            std::pair<SIMDVector, bool> decrement(uint8_t index, uint8_t qpu);

            void checkAllZero() const;

        private:
            std::mutex counterLock;
            std::array<uint8_t, 16> counter;
        };

        using ProgramCounter = uint32_t;

        template <unsigned NumElements, typename Element = Word>
        struct CacheLine
        {
            MemoryAddress baseAddress = 0;
            bool isSet = false;
            bool isBeingFilled = false;
            uint16_t lineNum;
            std::array<Element, NumElements> data{};
            uint32_t cycleWritten = 0;

            static constexpr auto LINE_SIZE = NumElements * sizeof(Element);

            constexpr bool containsAddress(MemoryAddress address) const noexcept
            {
                return isSet && baseAddress <= address && (baseAddress + LINE_SIZE) > address;
            }

            Element readElement(MemoryAddress address) const
            {
                return data.at((address - baseAddress) / sizeof(Element));
            }
        };

        class L2Cache
        {
        public:
            L2Cache(EmulationClock& clock, Memory& memory,
                std::vector<qpu_asm::Instruction>::const_iterator firstInstruction) :
                clock(clock),
                memory(memory), firstInstruction(firstInstruction)
            {
                cache.fill(CacheLine<16>{});
                for(uint16_t i = 0; i < cache.size(); ++i)
                    cache[i].lineNum = i;
            }

            AsynchronousExecution startCacheLineRead(
                AsynchronousHandle<std::array<Word, 16>>&& handle, MemoryAddress address);
            AsynchronousExecution startCacheLineRead(
                AsynchronousHandle<std::array<uint64_t, 8>>&& handle, ProgramCounter pc);

            void validateMemoryWord(MemoryAddress address, Word word) const;
            void validateInstruction(ProgramCounter pc, uint64_t instruction) const;

        private:
            EmulationClock& clock;
            Memory& memory;
            std::vector<qpu_asm::Instruction>::const_iterator firstInstruction;
            std::array<CacheLine<64 / 4>, 512> cache;

            // TODO remove after test
            friend class Slice;
        };

        class Slice
        {
        public:
            Slice(uint8_t id, EmulationClock& clock, L2Cache& l2Cache) : id(id), clock(clock), l2Cache(l2Cache)
            {
                tmu0Cache.fill(CacheLine<16>{});
                tmu1Cache.fill(CacheLine<16>{});
                uniformCache.fill(CacheLine<16>{});
                instructionCache.fill(CacheLine<8, uint64_t>{});
                for(uint16_t i = 0; i < tmu0Cache.size(); ++i)
                    tmu0Cache[i].lineNum = tmu1Cache[i].lineNum = uniformCache[i].lineNum =
                        instructionCache[i].lineNum = i;
            }

            AsynchronousExecution startUniformRead(AsynchronousHandle<MemoryAddress>&& handle, MemoryAddress address);
            AsynchronousExecution startTMURead(
                uint8_t tmuIndex, AsynchronousHandle<Word>&& handle, MemoryAddress address);
            std::pair<qpu_asm::Instruction, bool> readInstruction(ProgramCounter pc);

        private:
            uint8_t id;
            EmulationClock& clock;
            L2Cache& l2Cache;

            std::array<CacheLine<64 / 4>, 64> tmu0Cache;
            std::array<CacheLine<64 / 4>, 64> tmu1Cache;
            std::array<CacheLine<64 / 8, uint64_t>, 64> instructionCache;
            std::array<CacheLine<64 / 4>, 64> uniformCache;
        };

        using InstrumentationResults = FastAccessList<InstrumentationResult>;

        class QPU : private NonCopyable
        {
        public:
            QPU(uint8_t id, EmulationClock& clock, Slice& slice, Mutex& mutex, SFU& sfu, VPM& vpm,
                Semaphores& semaphores, MemoryAddress uniformAddress, InstrumentationResults& instrumentation) :
                ID(id),
                clock(clock), slice(slice), mutex(mutex), registers(*this),
                uniforms(clock, *this, slice, uniformAddress), tmus(clock, *this, slice), sfu(sfu), vpm(vpm),
                semaphores(semaphores), pc(0), lastInstruction{0xDEADDEAD, 0}, instrumentation(instrumentation),
                stopExecution(false)
            {
                // initially trigger the loading of the first UNIFORM values into the FIFO
                uniforms.triggerFifoFill();
            }

            const uint8_t ID;

            uint32_t getCurrentCycle() const;

            NODISCARD bool execute();

            qpu_asm::Instruction getCurrentInstruction() const;
            uint32_t getCurrentInstructionIndex() const;

            inline bool operator<(const QPU& other) const noexcept
            {
                return ID < other.ID;
            }

        private:
            EmulationClock& clock;
            Slice& slice;
            Mutex& mutex;
            Registers registers;
            UniformFifo uniforms;
            TMUs tmus;
            SFU& sfu;
            VPM& vpm;
            Semaphores& semaphores;
            VectorFlags flags;
            ProgramCounter pc;
            std::pair<ProgramCounter, uint64_t> lastInstruction;
            InstrumentationResults& instrumentation;
            SIMDVector lastR4Value;
            bool stopExecution;

            friend class Registers;
            friend class UniformFifo;
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
            const KernelUniforms& uniformsUsed, uint8_t workItemMergeFactor = 1);
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
