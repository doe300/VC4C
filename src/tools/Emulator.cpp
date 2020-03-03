/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Emulator.h"

#include "../GlobalValues.h"
#include "../Profiler.h"
#include "../asm/ALUInstruction.h"
#include "../asm/BranchInstruction.h"
#include "../asm/Instruction.h"
#include "../asm/KernelInfo.h"
#include "../asm/LoadInstruction.h"
#include "../asm/SemaphoreInstruction.h"
#include "../periphery/VPM.h"
#include "CompilationError.h"
#include "Compiler.h"

#include "log.h"

#include <cmath>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <numeric>
#include <random>

using namespace vc4c;
using namespace vc4c::tools;

extern void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo, StableList<Global>& globals,
    std::vector<qpu_asm::Instruction>& instructions);

std::size_t EmulationData::calcParameterSize() const
{
    return std::accumulate(parameter.begin(), parameter.end(), 0u,
               [](std::size_t size, const auto& pair) -> std::size_t {
                   return size + (pair.second ? pair.second->size() * sizeof(uint32_t) : sizeof(uint32_t));
               }) /
        sizeof(uint32_t);
}

tools::Word EmulationData::calcNumWorkItems() const
{
    return workGroup.localSizes[0] * workGroup.localSizes[1] * workGroup.localSizes[2] * workGroup.numGroups[0] *
        workGroup.numGroups[1] * workGroup.numGroups[2];
}

tools::Word* Memory::getWordAddress(MemoryAddress address)
{
    if(auto direct = VariantNamespace::get_if<DirectBuffer>(&data))
    {
        if(address >= direct->size() * sizeof(Word))
        {
            logging::warn() << "Buffer: [0, " << std::hex << direct->size() << std::dec << ")" << logging::endl;
            throw CompilationError(CompilationStep::GENERAL,
                "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
        }
        return direct->data() + (address / sizeof(Word));
    }
    auto& buffers = VariantNamespace::get<MappedBuffers>(data);
    if(address >= (buffers.rbegin()->first + buffers.rbegin()->second.get().size()))
    {
        logging::logLazy(logging::Level::WARNING, [&]() {
            for(const auto& buffer : buffers)
                logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                                << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
        });
        throw CompilationError(CompilationStep::GENERAL,
            "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
    }
    for(const auto& buf : buffers)
    {
        if(buf.first <= address && (buf.first + buf.second.get().size()) > address)
        {
            auto wordBoundsAddress = (address / sizeof(Word)) * sizeof(Word);
            return reinterpret_cast<Word*>(buf.second.get().data() + wordBoundsAddress - buf.first);
        }
    }
    logging::logLazy(logging::Level::WARNING, [&]() {
        for(const auto& buffer : buffers)
            logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                            << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
    });
    throw CompilationError(CompilationStep::GENERAL, "Address is not part of any buffer", std::to_string(address));
}

const tools::Word* Memory::getWordAddress(MemoryAddress address) const
{
    if(auto direct = VariantNamespace::get_if<DirectBuffer>(&data))
    {
        if(address >= direct->size() * sizeof(Word))
        {
            logging::warn() << "Buffer: [0, " << std::hex << direct->size() << std::dec << ")" << logging::endl;
            throw CompilationError(CompilationStep::GENERAL,
                "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
        }
        return direct->data() + (address / sizeof(Word));
    }
    auto& buffers = VariantNamespace::get<MappedBuffers>(data);
    if(address >= (buffers.rbegin()->first + buffers.rbegin()->second.get().size()))
    {
        logging::logLazy(logging::Level::WARNING, [&]() {
            for(const auto& buffer : buffers)
                logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                                << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
        });
        throw CompilationError(CompilationStep::GENERAL,
            "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
    }
    for(const auto& buf : buffers)
    {
        if(buf.first <= address && (buf.first + buf.second.get().size()) > address)
        {
            auto wordBoundsAddress = (address / sizeof(Word)) * sizeof(Word);
            return reinterpret_cast<const Word*>(buf.second.get().data() + wordBoundsAddress - buf.first);
        }
    }
    logging::logLazy(logging::Level::WARNING, [&]() {
        for(const auto& buffer : buffers)
            logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                            << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
    });
    throw CompilationError(CompilationStep::GENERAL, "Address is not part of any buffer", std::to_string(address));
}

tools::Word Memory::readWord(MemoryAddress address) const
{
    if(address % sizeof(Word) != 0)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading word from non-word-aligned memory location will be truncated to align with "
                   "word-boundaries: "
                << address << logging::endl);
    return *getWordAddress(address);
}

MemoryAddress Memory::incrementAddress(MemoryAddress address, DataType typeSize) const
{
    return address + typeSize.getInMemoryWidth();
}

MemoryAddress Memory::getMaximumAddress() const
{
    if(auto direct = VariantNamespace::get_if<DirectBuffer>(&data))
        return static_cast<MemoryAddress>(direct->size() * sizeof(Word));
    return VariantNamespace::get<MappedBuffers>(data).rbegin()->first +
        static_cast<Word>(VariantNamespace::get<MappedBuffers>(data).begin()->second.get().size());
}

void Memory::setUniforms(const std::vector<Word>& uniforms, MemoryAddress address)
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 10, "setUniforms", 1);
    std::size_t offset = address / sizeof(Word);
    std::copy_n(uniforms.begin(), uniforms.size(),
        VariantNamespace::get<DirectBuffer>(data).begin() + static_cast<std::vector<Word>::difference_type>(offset));
}

bool Mutex::isLocked() const
{
    return locked;
}

bool Mutex::lock(uint8_t qpu)
{
    if(locked && lockOwner == qpu)
        // we need to check for duplicate read in same instruction (e.g. or -, mutex_acq, mutex_acq)
        throw CompilationError(CompilationStep::GENERAL, "Double locked mutex!");
    if(locked && lockOwner != qpu)
    {
        // locked by another QPU
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 30, "waitOnMutex", 1);
        return false;
    }
    locked = true;
    lockOwner = qpu;
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 20, "lockMutex", 1);
    return true;
}

void Mutex::unlock(uint8_t qpu)
{
    if(!locked)
        throw CompilationError(CompilationStep::GENERAL, "Freeing mutex not previously locked!");
    if(lockOwner != qpu)
        throw CompilationError(CompilationStep::GENERAL, "Cannot free mutex locked by another QPU!");
    locked = false;
}

LCOV_EXCL_START
static std::string toRegisterWriteString(const SIMDVector& val, std::bitset<16> elementMask)
{
    std::vector<std::string> parts;
    parts.reserve(NATIVE_VECTOR_SIZE);
    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        if(elementMask.test(i))
            parts.emplace_back(val[i].to_string());
        else
            parts.emplace_back("-");
    }
    return " {" + to_string<std::string>(parts) + "}";
}
LCOV_EXCL_STOP

void Registers::writeRegister(Register reg, const SIMDVector& val, std::bitset<16> elementMask, BitMask bitMask)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Writing into register '" << reg.to_string(true, false)
            << "': " << toRegisterWriteString(val, elementMask) << logging::endl);
    if(reg.isGeneralPurpose())
        writeStorageRegister(reg, SIMDVector(val), elementMask, bitMask);
    else if(reg.isAccumulator())
    {
        if(reg.num == REG_TMU_NOSWAP.num)
            qpu.tmus.setTMUNoSwap(val);
        else if(reg.num == REG_REPLICATE_ALL.num)
            // the physical file A or B is important here!
            writeStorageRegister(reg, SIMDVector(val), elementMask, bitMask);
        else
            writeStorageRegister(Register{RegisterFile::ACCUMULATOR, reg.num}, SIMDVector(val), elementMask, bitMask);
    }
    else if(reg.num == REG_HOST_INTERRUPT.num)
    {
        if(hostInterrupt)
            throw CompilationError(
                CompilationStep::GENERAL, "Host interrupt was already triggered with", hostInterrupt->to_string(true));
        hostInterrupt = val;
    }
    else if(reg.num == REG_NOP.num)
        return;
    else if(reg.num == REG_UNIFORM_ADDRESS.num)
        qpu.uniforms.setUniformAddress(val);
    else if(reg.num == REG_MS_MASK.num)
        writeStorageRegister(reg, SIMDVector(val), elementMask, bitMask);
    else if(reg.num == REG_VPM_IO.num)
        qpu.vpm.writeValue(val);
    else if(reg == REG_VPM_IN_SETUP)
        qpu.vpm.setReadSetup(val);
    else if(reg == REG_VPM_OUT_SETUP)
        qpu.vpm.setWriteSetup(val);
    else if(reg == REG_VPM_DMA_LOAD_ADDR)
        qpu.vpm.setDMAReadAddress(val);
    else if(reg == REG_VPM_DMA_STORE_ADDR)
        qpu.vpm.setDMAWriteAddress(val);
    else if(reg.num == REG_MUTEX.num)
        qpu.mutex.unlock(qpu.ID);
    else if(reg.num == REG_SFU_RECIP.num)
        qpu.sfu.startRecip(val);
    else if(reg.num == REG_SFU_RECIP_SQRT.num)
        qpu.sfu.startRecipSqrt(val);
    else if(reg.num == REG_SFU_EXP2.num)
        qpu.sfu.startExp2(val);
    else if(reg.num == REG_SFU_LOG2.num)
        qpu.sfu.startLog2(val);
    else if(reg.num == REG_TMU0_COORD_S_U_X.num)
        qpu.tmus.setTMURegisterS(0, val);
    else if(reg.num == REG_TMU0_COORD_T_V_Y.num)
        qpu.tmus.setTMURegisterT(0, val);
    else if(reg.num == REG_TMU0_COORD_R_BORDER_COLOR.num)
        qpu.tmus.setTMURegisterR(0, val);
    else if(reg.num == REG_TMU0_COORD_B_LOD_BIAS.num)
        qpu.tmus.setTMURegisterB(0, val);
    else if(reg.num == REG_TMU1_COORD_S_U_X.num)
        qpu.tmus.setTMURegisterS(1, val);
    else if(reg.num == REG_TMU1_COORD_T_V_Y.num)
        qpu.tmus.setTMURegisterT(1, val);
    else if(reg.num == REG_TMU1_COORD_R_BORDER_COLOR.num)
        qpu.tmus.setTMURegisterR(1, val);
    else if(reg.num == REG_TMU1_COORD_B_LOD_BIAS.num)
        qpu.tmus.setTMURegisterB(1, val);
    else
        throw CompilationError(CompilationStep::GENERAL, "Write of invalid register", reg.to_string());

    // conditional write to periphery registers is not allowed
    if(!reg.isGeneralPurpose() && !reg.isAccumulator() && !elementMask.all())
        throw CompilationError(
            CompilationStep::GENERAL, "Conditional write to periphery registers is not allowed", reg.to_string());
}

std::pair<SIMDVector, bool> Registers::readRegister(Register reg, bool anyElementUsed)
{
    if(reg.isGeneralPurpose())
        return std::make_pair(readStorageRegister(reg, anyElementUsed), true);
    if(reg.file == RegisterFile::ACCUMULATOR && reg.num != REG_SFU_OUT.num)
        return std::make_pair(readStorageRegister(reg, anyElementUsed), true);
    switch(reg.num)
    {
    case REG_SFU_OUT.num:
    {
        auto it = readCache.find(REG_SFU_OUT);
        if(it != readCache.end())
            return std::make_pair(it->second, true);
        auto pair = qpu.readR4();
        ignoreReturnValue(setReadCache(REG_SFU_OUT, pair.first));
        return pair;
    }
    case REG_UNIFORM.num:
    {
        auto it = readCache.find(REG_UNIFORM);
        if(it == readCache.end())
            it = setReadCache(REG_UNIFORM, qpu.uniforms.readUniform());
        return std::make_pair(it->second, true);
    }
    case REG_VARYING.num:
    {
        // returns random floating-point values
        std::default_random_engine generator;
        std::uniform_real_distribution<float> distribution;
        return std::make_pair(
            SIMDVector({Literal(distribution(generator)), Literal(distribution(generator)),
                Literal(distribution(generator)), Literal(distribution(generator)), Literal(distribution(generator)),
                Literal(distribution(generator)), Literal(distribution(generator)), Literal(distribution(generator)),
                Literal(distribution(generator)), Literal(distribution(generator)), Literal(distribution(generator)),
                Literal(distribution(generator)), Literal(distribution(generator)), Literal(distribution(generator)),
                Literal(distribution(generator)), Literal(distribution(generator))}),

            true);
    }
    case REG_ELEMENT_NUMBER.num:

        if(reg == REG_ELEMENT_NUMBER)
            return std::make_pair(ELEMENT_NUMBERS.vector(), true);
        if(reg == REG_QPU_NUMBER)
            return std::make_pair(SIMDVector(Literal(qpu.ID)), true);
        // should never happen
        break;
    case REG_NOP.num:
        logging::warn() << "Reading NOP register" << logging::endl;
        return std::make_pair(SIMDVector{}, true);
    case REG_X_COORDS.num:
        if(reg == REG_X_COORDS)
            // returns fixed pattern
            return std::make_pair(SIMDVector({Literal(0u), Literal(1u), Literal(0u), Literal(1u), Literal(0u),
                                      Literal(1u), Literal(0u), Literal(1u), Literal(0u), Literal(1u), Literal(0u),
                                      Literal(1u), Literal(0u), Literal(1u), Literal(0u), Literal(1u)}),
                true);
        if(reg == REG_Y_COORDS)
            // returns fixed pattern
            return std::make_pair(SIMDVector({Literal(0u), Literal(0u), Literal(1u), Literal(1u), Literal(0u),
                                      Literal(0u), Literal(1u), Literal(1u), Literal(0u), Literal(0u), Literal(1u),
                                      Literal(1u), Literal(0u), Literal(0u), Literal(1u), Literal(1u)}),
                true);
        // should never happen
        break;
    case REG_MS_MASK.num:
        // both valid for REG_MS_MASK and REG_EV_FLAG
        return std::make_pair(readStorageRegister(reg, anyElementUsed), true);
    case REG_VPM_IO.num:
    {
        auto it = readCache.find(REG_VPM_IO);
        if(it == readCache.end())
            it = setReadCache(REG_VPM_IO, qpu.vpm.readValue());
        return std::make_pair(it->second, true);
    }
    case REG_VPM_DMA_LOAD_WAIT.num:
        if(reg == REG_VPM_DMA_LOAD_WAIT)
            return std::make_pair(SIMDVector{}, qpu.vpm.waitDMARead());
        if(reg == REG_VPM_DMA_STORE_WAIT)
            return std::make_pair(SIMDVector{}, qpu.vpm.waitDMAWrite());
        // should never happen
        break;
    case REG_MUTEX.num:
    {
        auto it = readCache.find(REG_MUTEX);
        if(it == readCache.end())
            it = setReadCache(
                REG_MUTEX, qpu.mutex.lock(qpu.ID) ? SIMDVector(Literal(true)) : SIMDVector(Literal(false)));
        return std::make_pair(it->second, it->second[0].isTrue());
    }
    }
    throw CompilationError(CompilationStep::GENERAL, "Read of invalid register", reg.to_string());
}

SIMDVector Registers::getInterruptValue() const
{
    if(hostInterrupt)
        return hostInterrupt.value();
    throw CompilationError(CompilationStep::GENERAL, "Host interrupt was not triggered!");
}

void Registers::clearReadCache()
{
    readCache.clear();
}

static constexpr uint8_t toIndex(Register reg) noexcept
{
    // we have 2 bits for the register file and need to store the files 1, 2 and 4. By subtracting 1, we can store all
    // of them in 2 bits
    return (static_cast<uint8_t>(reg.file) - 1) * 64 + reg.num;
}

SIMDVector Registers::readStorageRegister(Register reg, bool anyElementUsed)
{
    const auto& vec = storageRegisters.at(toIndex(reg));
    if(vec.first.isUndefined())
    {
        if(anyElementUsed)
            throw CompilationError(
                CompilationStep::GENERAL, "Reading from register not previously defined:", reg.to_string());
        else
            // for ALU operations which are not actually executed (e.g. flags do not match), we can return a dummy value
            return SIMDVector{};
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Reading from register '" << reg.to_string(true, true) << "': " << vec.first.to_string(true)
            << logging::endl);
    if(reg.isGeneralPurpose() && qpu.currentCycle - 1 <= vec.second)
        throw CompilationError(CompilationStep::GENERAL,
            "Physical register cannot be read in the next instruction after it was written", reg.to_string());
    return vec.first;
}

static SIMDVector toStorageValue(
    const SIMDVector& oldVal, const SIMDVector& newVal, std::bitset<16> elementMask, BitMask bitMask)
{
    if(elementMask.all())
        return newVal;
    if(elementMask.none())
        return oldVal;
    SIMDVector result;
    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        result[i] = elementMask.test(i) ? bitMask(newVal[i], oldVal[i]) : oldVal[i];
    }
    return result;
}

void Registers::writeStorageRegister(Register reg, SIMDVector&& val, std::bitset<16> elementMask, BitMask bitMask)
{
    if(reg == REG_MS_MASK)
    {
        // actual value is truncated to lowest 4 Bits per element
        auto truncate = [](Literal lit) -> Literal { return Literal(lit.unsignedInt() & 0xF); };
        val = val.transform(truncate);
    }
    if(reg == REG_REV_FLAG)
    {
        if(!elementMask.test(0))
            // if 0th element is not set, retain old value
            return;
        // actual value stored is truncated to lowest 1 Bit and replicated across all elements
        val = SIMDVector(Literal(val[0].unsignedInt() & 1));
    }
    auto& vec = storageRegisters.at(toIndex(reg)) = std::make_pair(
        toStorageValue(storageRegisters.at(toIndex(reg)).first, val, elementMask, bitMask), qpu.currentCycle);
    if(reg.num == REG_REPLICATE_ALL.num && elementMask.any())
    {
        // TODO if some flags are set, but not the 0th (or 0th, 4th, 8th and 12th), need to retain old value?
        // TODO or is conditional replication possible at all?
        // is not actually stored in the physical file A or B
        vec.first = SIMDVector{};
        storageRegisters.at(toIndex(REG_ACC5)).first = val;

        if(reg.file == RegisterFile::PHYSICAL_A)
        {
            // per-quad replication
            storageRegisters.at(toIndex(REG_ACC5)).first = SIMDVector({val[0], val[0], val[0], val[0], val[4], val[4],
                val[4], val[4], val[8], val[8], val[8], val[8], val[12], val[12], val[12], val[12]});
        }
        else if(reg.file == RegisterFile::PHYSICAL_B)
        {
            // across all elements replication
            storageRegisters.at(toIndex(REG_ACC5)).first = SIMDVector(val[0]);
        }
        else
            throw CompilationError(CompilationStep::GENERAL,
                "Failed to determine register-file for replication register", reg.to_string());
    }
}

SortedMap<Register, SIMDVector>::iterator Registers::setReadCache(Register reg, const SIMDVector& val)
{
    auto it = readCache.find(reg);
    if(it == readCache.end())
        return readCache.emplace(reg, val).first;
    it->second = val;
    return it;
}

SIMDVector UniformCache::readUniform()
{
    if(lastAddressSetCycle != 0 && lastAddressSetCycle + 2 > qpu.getCurrentCycle())
        // see Broadcom specification, page 22
        throw CompilationError(CompilationStep::GENERAL, "Reading UNIFORM within 2 cycles of last UNIFORM reset!");
    SIMDVector val(Literal(memory.readWord(uniformAddress)));
    // do not increment UNIFORM pointer for multiple reads in same instruction
    uniformAddress = memory.incrementAddress(uniformAddress, TYPE_INT32);
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading UNIFORM value: " << val.to_string(true) << logging::endl);
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 40, "UNIFORM read", 1);
    return val;
}

void UniformCache::setUniformAddress(const SIMDVector& val)
{
    // only first element is used, see Broadcom specification, page 22
    uniformAddress = val[0].toImmediate();
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Reset UNIFORM address to: " << uniformAddress << logging::endl);
    lastAddressSetCycle = qpu.getCurrentCycle();
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 50, "write UNIFORM address", 1);
}

std::pair<SIMDVector, bool> TMUs::readTMU()
{
    if(tmu0ResponseQueue.empty() && tmu1ResponseQueue.empty())
        throw CompilationError(CompilationStep::GENERAL, "Cannot read from empty TMU queue!");
    if(!tmu0ResponseQueue.empty() && !tmu1ResponseQueue.empty())
        // TODO or is there only one response queue? This would be more understandable from hardware point of view (for
        // ordering)
        logging::warn()
            << "Reading from r4 when both TMUs have queued responses is not yet tested and might give in wrong results"
            << logging::endl;

    // need to select the first triggered read in both queues
    std::queue<std::pair<SIMDVector, uint32_t>>* queue = nullptr;
    bool tmuFlag = false;
    if(!tmu0ResponseQueue.empty())
    {
        queue = &tmu0ResponseQueue;
        tmuFlag = false;
    }
    if(!tmu1ResponseQueue.empty())
    {
        if(queue == nullptr)
        {
            queue = &tmu1ResponseQueue;
            tmuFlag = true;
        }
        else if(tmu1ResponseQueue.front().second < queue->front().second)
        {
            queue = &tmu1ResponseQueue;
            tmuFlag = true;
        }
    }

    if(queue == nullptr)
        throw CompilationError(CompilationStep::GENERAL, "Illegal TMU response queue");

    auto front = queue->front();
    queue->pop();
    if(tmuFlag)
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 68, "TMU0 read", 1);
    else
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 69, "TMU1 read", 1);
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading from TMU: " << front.first.to_string(true) << logging::endl);
    return std::make_pair(front.first, true);
}

bool TMUs::hasValueOnR4() const
{
    return !tmu0ResponseQueue.empty() || !tmu1ResponseQueue.empty();
}

void TMUs::setTMUNoSwap(const SIMDVector& swapVal)
{
    // XXX or per-element?
    tmuNoSwap = swapVal[0].isTrue();
    lastTMUNoSwap = qpu.getCurrentCycle();
}

void TMUs::setTMURegisterS(uint8_t tmu, const SIMDVector& val)
{
    checkTMUWriteCycle();

    tmu = toRealTMU(tmu);
    auto& requestQueue = tmu == 1 ? tmu1RequestQueue : tmu0RequestQueue;

    if(requestQueue.size() >= 8)
        throw CompilationError(CompilationStep::GENERAL, "TMU request queue is full!");
    requestQueue.push(std::make_pair(readMemoryAddress(val), qpu.getCurrentCycle()));
}

void TMUs::setTMURegisterT(uint8_t tmu, const SIMDVector& val)
{
    checkTMUWriteCycle();
    throw CompilationError(CompilationStep::GENERAL, "Image reads via TMU are currently not supported!");
}

void TMUs::setTMURegisterR(uint8_t tmu, const SIMDVector& val)
{
    checkTMUWriteCycle();
    throw CompilationError(CompilationStep::GENERAL, "Image reads via TMU are currently not supported!");
}

void TMUs::setTMURegisterB(uint8_t tmu, const SIMDVector& val)
{
    checkTMUWriteCycle();
    throw CompilationError(CompilationStep::GENERAL, "Image reads via TMU are currently not supported!");
}

bool TMUs::triggerTMURead(uint8_t tmu)
{
    tmu = toRealTMU(tmu);

    auto& requestQueue = tmu == 1 ? tmu1RequestQueue : tmu0RequestQueue;
    auto& responseQueue = tmu == 1 ? tmu1ResponseQueue : tmu0ResponseQueue;

    if(requestQueue.empty())
        throw CompilationError(CompilationStep::GENERAL, "No data in TMU request queue to be read!");
    if(responseQueue.size() >= 8)
        throw CompilationError(CompilationStep::GENERAL, "TMU response queue is full!");

    auto val = requestQueue.front();
    if(tmu == 0)
        PROFILE_COUNTER(
            vc4c::profiler::COUNTER_EMULATOR + 65, "TMU0 read trigger", val.second + 9 <= qpu.getCurrentCycle());
    else
        PROFILE_COUNTER(
            vc4c::profiler::COUNTER_EMULATOR + 66, "TMU1 read trigger", val.second + 9 <= qpu.getCurrentCycle());
    if(val.second + 9 > qpu.getCurrentCycle())
        // block for at least 9 cycles
        return false;
    else if(val.second + 20 > qpu.getCurrentCycle())
        // blocks up to 20 cycles when reading from RAM
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Distance between triggering of TMU read and read is " << (qpu.getCurrentCycle() - val.second)
                << ", additional stalls may be introduced" << logging::endl);
    requestQueue.pop();
    responseQueue.push(std::make_pair(val.first, qpu.getCurrentCycle()));
    return true;
}

void TMUs::checkTMUWriteCycle() const
{
    // Broadcom specification, page 37
    if(lastTMUNoSwap + 3 > qpu.getCurrentCycle())
        throw CompilationError(CompilationStep::GENERAL, "Writing to TMU within 3 cycles of last TMU no-swap change!");
}

SIMDVector TMUs::readMemoryAddress(const SIMDVector& address) const
{
    SIMDVector res;
    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        // NOTE: For real TMU, a value of zero means to not load anything (for that vector element), but we allow the
        // offset of zero in the emulator
        if(address[i].isUndefined())
            throw CompilationError(
                CompilationStep::GENERAL, "Cannot read from undefined TMU address", address.to_string());
        else
            res[i] = Literal(memory.readWord(address[i].toImmediate()));
    }
    // XXX for cosmetic/correctness, this should print the rounded-down (to word boundaries) addresses
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Reading via TMU from memory address " << address.to_string(true) << ": " << res.to_string(true)
            << logging::endl);
    return res;
}

uint8_t TMUs::toRealTMU(uint8_t tmu) const
{
    bool upperHalf = (qpu.ID % 4) >= 2;
    return (upperHalf && !tmuNoSwap) ? tmu ^ 1 : tmu;
}

SIMDVector SFU::readSFU()
{
    if(lastSFUWrite + 2 > currentCycle)
        throw CompilationError(
            CompilationStep::GENERAL, "Reading of SFU result within 2 cycles of triggering SFU calculation!");
    if(!sfuResult)
        throw CompilationError(CompilationStep::GENERAL, "Cannot read empty SFU result!");
    auto val = sfuResult.value();
    sfuResult = {};
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 70, "SFU read", 1);
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading from SFU: " << val.to_string(true) << logging::endl);
    return val;
}

bool SFU::hasValueOnR4() const
{
    return sfuResult.has_value();
}

static SIMDVector calcSFU(const SIMDVector& in, const std::function<float(float)>& func)
{
    return in.transform([&](Literal in) -> Literal { return Literal(func(in.real())); });
}

static float addSFUError(float val)
{
    // assume 10 bits of SFU accuracy (half float)
    // -> convert from 23 bits accuracy to 10 bits accuracy
    return bit_cast<uint32_t, float>(bit_cast<float, uint32_t>(val) & 0xFFFFE000);
}

void SFU::startRecip(const SIMDVector& val)
{
    sfuResult = calcSFU(val, [](float f) -> float { return addSFUError(1.0f / f); });
    lastSFUWrite = currentCycle;
}

void SFU::startRecipSqrt(const SIMDVector& val)
{
    sfuResult = calcSFU(val, [](float f) -> float { return addSFUError(1.0f / std::sqrt(f)); });
    lastSFUWrite = currentCycle;
}

void SFU::startExp2(const SIMDVector& val)
{
    sfuResult = calcSFU(val, [](float f) -> float { return addSFUError(std::exp2(f)); });
    lastSFUWrite = currentCycle;
}

void SFU::startLog2(const SIMDVector& val)
{
    sfuResult = calcSFU(val, [](float f) -> float { return addSFUError(std::log2(f)); });
    lastSFUWrite = currentCycle;
}

void SFU::incrementCycle()
{
    ++currentCycle;
}

template <typename T>
static std::pair<uint32_t, uint32_t> toAddresses(T setup)
{
    switch(setup.getSize())
    {
    case 0: // Byte
        return std::make_pair(setup.getByteRow(), setup.getByteOffset() * 4);
    case 1: // Half-word
        return std::make_pair(setup.getHalfWordRow(), setup.getHalfWordOffset() * 8);
    case 2: // Word
        return std::make_pair(setup.getWordRow(), 0);
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled VPM type-size", std::to_string(setup.getSize()));
}

template <typename T>
static std::pair<uint32_t, uint32_t> toStride(T setup)
{
    switch(setup.getSize())
    {
    case 0: // Byte
        return std::make_pair(setup.getStride() % 4, setup.getStride() / 4);
    case 1: // Half-word
        return std::make_pair(setup.getStride() % 2, setup.getStride() / 2);
    case 2: // Word
        return std::make_pair(setup.getStride(), 0);
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled VPM type-size", std::to_string(setup.getSize()));
}

SIMDVector VPM::readValue()
{
    periphery::VPRSetup setup = periphery::VPRSetup::fromLiteral(vpmReadSetup);

    if(setup.value == 0)
        throw CompilationError(CompilationStep::GENERAL, "VPM generic setup was not previously set", setup.to_string());
    if(setup.genericSetup.getLaned())
        throw CompilationError(CompilationStep::GENERAL, "Laned access to VPM is not yet supported", setup.to_string());
    if(!setup.genericSetup.getHorizontal())
        throw CompilationError(
            CompilationStep::GENERAL, "Vertical access to VPM is not yet supported", setup.to_string());

    std::pair<uint32_t, uint32_t> addresses = toAddresses(setup.genericSetup);

    if(addresses.first >= cache.size())
        throw CompilationError(CompilationStep::GENERAL, "VPM address out of range", setup.to_string());

    SIMDVector result{};
    Word* dataPtr = &cache.at(addresses.first).at(addresses.second);
    switch(setup.genericSetup.getSize())
    {
    case 0: // Byte
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            result[i] = Literal((dataPtr[i / 4] >> ((i % 4) * 8)) & 0xFF);
        }
        break;
    }
    case 1: // Half-word
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            result[i] = Literal((dataPtr[i / 2] >> ((i % 2) * 16)) & 0xFFFF);
        }
        break;
    }
    case 2: // Word
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            result[i] = Literal(dataPtr[i]);
        }
        break;
    }
    }

    setup.genericSetup.setAddress(
        static_cast<uint8_t>(setup.genericSetup.getAddress() + setup.genericSetup.getStride()));
    setup.genericSetup.setNumber(static_cast<uint8_t>((16 + setup.genericSetup.getNumber() - 1) % 16));
    vpmReadSetup = setup.value;

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Read value from VPM: " << result.to_string(true) << logging::endl;
        logging::debug() << "New read setup is now: " << setup.to_string() << logging::endl;
    });

    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 80, "VPM read", 1);
    return result;
}

void VPM::writeValue(const SIMDVector& val)
{
    periphery::VPWSetup setup = periphery::VPWSetup::fromLiteral(vpmWriteSetup);

    if(setup.value == 0)
        throw CompilationError(CompilationStep::GENERAL, "VPM generic setup was not previously set", setup.to_string());
    if(setup.genericSetup.getLaned())
        throw CompilationError(CompilationStep::GENERAL, "Laned access to VPM is not yet supported", setup.to_string());
    if(!setup.genericSetup.getHorizontal())
        throw CompilationError(
            CompilationStep::GENERAL, "Vertical access to VPM is not yet supported", setup.to_string());

    std::pair<uint32_t, uint32_t> addresses = toAddresses(setup.genericSetup);

    if(addresses.first >= cache.size())
        throw CompilationError(CompilationStep::GENERAL, "VPM address out of range", setup.to_string());

    Word* dataPtr = &cache.at(addresses.first).at(addresses.second);
    switch(setup.genericSetup.getSize())
    {
    case 0: // Byte
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            dataPtr[i / 4] &= ~(0xFFu << ((i % 4u) * 8u));
            dataPtr[i / 4] |= static_cast<Word>(val[i].unsignedInt() & 0xFFu) << ((i % 4u) * 8u);
        }
        break;
    }
    case 1: // Half-word
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            dataPtr[i / 2] &= ~(0xFFFFu << ((i % 2u) * 16u));
            dataPtr[i / 2] |= static_cast<Word>(val[i].unsignedInt() & 0xFFFFu) << ((i % 2u) * 16u);
        }
        break;
    }
    case 2: // Word
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            dataPtr[i] = static_cast<Word>(val[i].unsignedInt());
        }
        break;
    }
    }

    setup.genericSetup.setAddress(
        static_cast<uint8_t>(setup.genericSetup.getAddress() + setup.genericSetup.getStride()));
    vpmWriteSetup = setup.value;

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Wrote value into VPM: " << val.to_string(true) << logging::endl;
        logging::debug() << "New write setup is now: " << setup.to_string() << logging::endl;
    });
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 90, "VPM written", 1);
}

void VPM::setWriteSetup(const SIMDVector& val)
{
    auto element0 = val[0];
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined VPM setup value", val.to_string());
    periphery::VPWSetup setup = periphery::VPWSetup::fromLiteral(element0.unsignedInt());
    if(setup.isDMASetup())
        dmaWriteSetup = setup.value;
    else if(setup.isGenericSetup())
        vpmWriteSetup = setup.value;
    else if(setup.isStrideSetup())
        writeStrideSetup = setup.value;
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Writing unknown VPM write setup", std::to_string(element0.unsignedInt()));
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Set VPM write setup: " << setup.to_string() << logging::endl);
}

void VPM::setReadSetup(const SIMDVector& val)
{
    auto element0 = val[0];
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined VPM setup value", val.to_string());
    periphery::VPRSetup setup = periphery::VPRSetup::fromLiteral(element0.unsignedInt());
    if(setup.isDMASetup())
        dmaReadSetup = setup.value;
    else if(setup.isGenericSetup())
        // TODO warn/error if there is still VPM read pending from previous setup. TODO or create VPM read queue like
        // for TMU?
        vpmReadSetup = setup.value;
    else if(setup.isStrideSetup())
        readStrideSetup = setup.value;
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Writing unknown VPM read setup", std::to_string(element0.unsignedInt()));
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Set VPM read setup: " << setup.to_string() << logging::endl);
}

void VPM::setDMAWriteAddress(const SIMDVector& val)
{
    auto element0 = val[0];
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined DMA setup value", val.to_string());
    periphery::VPWSetup setup = periphery::VPWSetup::fromLiteral(dmaWriteSetup);

    if(setup.value == 0)
        throw CompilationError(
            CompilationStep::GENERAL, "VPM DMA write setup was not previously set", setup.to_string());

    std::pair<uint32_t, uint32_t> vpmBaseAddress =
        std::make_pair(setup.dmaSetup.getWordRow(), setup.dmaSetup.getWordColumn());
    std::pair<uint32_t, uint32_t> sizes = std::make_pair(setup.dmaSetup.getUnits(), setup.dmaSetup.getDepth());
    if(!setup.dmaSetup.getHorizontal())
        // invert columns and rows
        sizes = std::make_pair(setup.dmaSetup.getDepth(), setup.dmaSetup.getUnits());
    uint32_t typeSize =
        setup.dmaSetup.getMode() >= 4 ? 1 /* Byte */ : setup.dmaSetup.getMode() >= 2 ? 2 /* Half-word */ : 4 /* Word */;
    uint32_t byteOffset =
        typeSize == 4 ? 0 : typeSize == 2 ? setup.dmaSetup.getHalfRowOffset() * 2 : setup.dmaSetup.getByteOffset();

    if(typeSize * sizes.second > sizeof(Word) * NATIVE_VECTOR_SIZE)
        throw CompilationError(
            CompilationStep::GENERAL, "Accessing more than a VPM row at once is not supported", setup.to_string());

    auto stride = periphery::VPWSetup::fromLiteral(writeStrideSetup).strideSetup.getStride();

    MemoryAddress address = static_cast<MemoryAddress>(element0.unsignedInt());

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Copying " << sizes.first << " rows with " << sizes.second << " elements of " << typeSize
            << " bytes each " << (setup.dmaSetup.getHorizontal() ? "horizontally" : "vertically")
            << " from VPM address " << vpmBaseAddress.first << "," << vpmBaseAddress.second << " into RAM at "
            << address << " with a memory stride of " << stride << logging::endl);

    if(vpmBaseAddress.first >= 64)
        throw CompilationError(
            CompilationStep::GENERAL, "VPM row address is out of range: ", std::to_string(vpmBaseAddress.first));

    if(setup.dmaSetup.getHorizontal())
    {
        for(uint32_t i = 0; i < sizes.first; ++i)
        {
            if(address + typeSize * sizes.second >= memory.getMaximumAddress())
                throw CompilationError(CompilationStep::GENERAL,
                    "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
            memcpy(reinterpret_cast<uint8_t*>(memory.getWordAddress(address)) + address % sizeof(Word),
                reinterpret_cast<uint8_t*>(&cache.at(vpmBaseAddress.first).at(vpmBaseAddress.second)) + byteOffset,
                typeSize * sizes.second);
            logging::debug() << "\tVPM row: "
                             << to_string<unsigned, std::array<unsigned, 16>>(cache.at(vpmBaseAddress.first))
                             << logging::endl;
            vpmBaseAddress.first += 1;
            // write stride is end-to-start, so add size of vector
            address += stride + (typeSize * sizes.second);
        }
    }
    else
    {
        for(uint32_t i = 0; i < sizes.second; ++i)
        {
            if(address + typeSize * sizes.first >= memory.getMaximumAddress())
                throw CompilationError(CompilationStep::GENERAL,
                    "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
            for(uint32_t k = 0; k < sizes.first; ++k)
            {
                memcpy(reinterpret_cast<uint8_t*>(memory.getWordAddress(address)) + address % sizeof(Word),
                    reinterpret_cast<uint8_t*>(&cache.at(vpmBaseAddress.first + k).at(vpmBaseAddress.second + i)) +
                        byteOffset,
                    typeSize);
                address += typeSize;
            }
            address += stride;
        }

        for(uint32_t i = 0; i < sizes.first; ++i)
        {
            logging::debug() << "\tVPM row: "
                             << to_string<unsigned, std::array<unsigned, 16>>(cache.at(vpmBaseAddress.first + i))
                             << logging::endl;
        }
    }

    lastDMAWriteTrigger = currentCycle;
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 100, "write DMA write address", 1);
}

void VPM::setDMAReadAddress(const SIMDVector& val)
{
    auto element0 = val[0];
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined DMA setup value", val.to_string());
    periphery::VPRSetup setup = periphery::VPRSetup::fromLiteral(dmaReadSetup);

    if(setup.value == 0)
        throw CompilationError(
            CompilationStep::GENERAL, "VPM DMA read setup was not previously set", setup.to_string());
    if(setup.dmaSetup.getVertical())
        throw CompilationError(
            CompilationStep::GENERAL, "Vertical access to VPM is not yet supported", setup.to_string());
    if(setup.dmaSetup.getMPitch() != 0)
        throw CompilationError(CompilationStep::GENERAL, "Memory pitch is not yet supported", setup.to_string());

    std::pair<uint32_t, uint32_t> vpmBaseAddress =
        std::make_pair(setup.dmaSetup.getWordRow(), setup.dmaSetup.getWordColumn());
    std::pair<uint32_t, uint32_t> sizes = std::make_pair(setup.dmaSetup.getNumberRows(),
        setup.dmaSetup.getRowLength() == 0 ? 16 /* 0 => 16 */ : setup.dmaSetup.getRowLength());
    uint32_t typeSize =
        setup.dmaSetup.getMode() >= 4 ? 1 /* Byte */ : setup.dmaSetup.getMode() >= 2 ? 2 /* Half-word */ : 4 /* Word */;
    uint32_t byteOffset =
        typeSize == 4 ? 0 : typeSize == 2 ? setup.dmaSetup.getHalfRowOffset() * 2 : setup.dmaSetup.getByteOffset();
    uint32_t vpitch = setup.dmaSetup.getVPitch() == 0 ? 16 : setup.dmaSetup.getVPitch();

    if(typeSize * sizes.second > sizeof(Word) * NATIVE_VECTOR_SIZE)
        throw CompilationError(
            CompilationStep::GENERAL, "Accessing more than a VPM row at once is not supported", setup.to_string());

    auto pitch = periphery::VPRSetup::fromLiteral(readStrideSetup).strideSetup.getPitch();

    MemoryAddress address = static_cast<MemoryAddress>(element0.unsignedInt());

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Copying " << sizes.first << " rows with " << sizes.second << " elements of " << typeSize
            << " bytes each from RAM address " << address << " into VPM at " << vpmBaseAddress.first << ","
            << vpmBaseAddress.second << " with byte-offset of " << byteOffset << " and a memory pitch of " << pitch
            << logging::endl);

    if(vpmBaseAddress.first >= 64)
        throw CompilationError(
            CompilationStep::GENERAL, "VPM row address is out of range: ", std::to_string(vpmBaseAddress.first));

    for(uint32_t i = 0; i < sizes.first; ++i)
    {
        memcpy(reinterpret_cast<uint8_t*>(&cache.at(vpmBaseAddress.first).at(vpmBaseAddress.second)) + byteOffset,
            reinterpret_cast<uint8_t*>(memory.getWordAddress(address)) + address % sizeof(Word),
            typeSize * sizes.second);
        logging::debug() << "\tVPM row: "
                         << to_string<unsigned, std::array<unsigned, 16>>(cache.at(vpmBaseAddress.first))
                         << logging::endl;
        vpmBaseAddress.first += static_cast<uint32_t>((vpitch * typeSize) / sizeof(Word));
        vpmBaseAddress.second += static_cast<uint32_t>((vpitch * typeSize) % sizeof(Word));
        // read pitch is start-to-start, so we don't have to add anything
        address += pitch;
    }

    lastDMAReadTrigger = currentCycle;
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 110, "write DMA read address", 1);
}

bool VPM::waitDMAWrite() const
{
    // XXX how many cycles?
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 120, "wait DMA write", lastDMAWriteTrigger + 12 < currentCycle);
    return lastDMAWriteTrigger + 12 < currentCycle;
}

bool VPM::waitDMARead() const
{
    // XXX how many cycles?
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 130, "wait DMA read", lastDMAReadTrigger + 12 < currentCycle);
    return lastDMAReadTrigger + 12 < currentCycle;
}

void VPM::incrementCycle()
{
    ++currentCycle;
}

LCOV_EXCL_START
void VPM::dumpContents() const
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "VPM contents:" << logging::endl;
        for(const auto& row : cache)
        {
            auto& s = logging::debug();
            for(auto word : row)
            {
                s << std::hex << std::setfill(L'0') << std::setw(8) << word << " ";
            }
            s << std::dec << logging::endl;
        }
        logging::debug() << logging::endl;
    });
}
LCOV_EXCL_STOP

std::pair<SIMDVector, bool> Semaphores::increment(uint8_t index)
{
    auto& cnt = counter.at(index);
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 140, "semaphore increment", cnt < 15);
    if(cnt == 15)
        return std::make_pair(SIMDVector(Literal(15)), false);
    ++cnt;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Semaphore " << static_cast<unsigned>(index) << " increased to: " << static_cast<unsigned>(cnt)
            << logging::endl);
    // Broadcom specification, page 33: "The instruction otherwise behaves like a 32-bit load immediate instruction, so
    // the ALU outputs will not generally be useful." -> It "loads" the lower part of the opcode
    return std::make_pair(SIMDVector(Literal(static_cast<uint32_t>(cnt))), true);
}

std::pair<SIMDVector, bool> Semaphores::decrement(uint8_t index)
{
    auto& cnt = counter.at(index);
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 150, "semaphore decrement", cnt > 0);
    if(cnt == 0)
        return std::make_pair(SIMDVector(Literal(0u)), false);
    --cnt;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Semaphore " << static_cast<unsigned>(index) << " decreased to: " << static_cast<unsigned>(cnt)
            << logging::endl);
    // Broadcom specification, page 33: "The instruction otherwise behaves like a 32-bit load immediate instruction, so
    // the ALU outputs will not generally be useful." -> It "loads" the lower part of the opcode
    return std::make_pair(SIMDVector(Literal((1u << 4) | static_cast<uint32_t>(cnt))), true);
}

void Semaphores::checkAllZero() const
{
    for(unsigned i = 0; i < counter.size(); ++i)
    {
        if(counter[i] != 0)
        {
            CPPLOG_LAZY(logging::Level::ERROR,
                log << "Semaphore " << i << " (" << counter[i] << ") is not reset to zero!" << logging::endl);
        }
    }
}

uint32_t QPU::getCurrentCycle() const
{
    return currentCycle;
}

std::pair<SIMDVector, bool> QPU::readR4()
{
    if(tmus.hasValueOnR4())
        return tmus.readTMU();
    if(sfu.hasValueOnR4())
        return std::make_pair(sfu.readSFU(), true);
    throw CompilationError(CompilationStep::GENERAL, "Cannot read from r4 without it being written!");
}

static Register toRegister(Address addr, bool isfileB)
{
    return Register{isfileB ? RegisterFile::PHYSICAL_B : RegisterFile::PHYSICAL_A, addr};
}

static VectorFlags generateImmediateFlags(const SIMDVector& vector)
{
    VectorFlags flags;
    for(uint8_t i = 0; i < vector.size(); ++i)
    {
        flags[i].carry = FlagStatus::CLEAR;
        flags[i].negative = vector[i].signedInt() < 0 ? FlagStatus::SET : FlagStatus::CLEAR;
        flags[i].overflow = FlagStatus::CLEAR;
        flags[i].zero = vector[i].unsignedInt() == 0 ? FlagStatus::SET : FlagStatus::CLEAR;
    }
    return flags;
}

bool QPU::execute(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction)
{
    const qpu_asm::Instruction* inst = &(*(firstInstruction + pc));
    ++instrumentation[inst].numExecutions;
    CPPLOG_LAZY(logging::Level::INFO,
        log << "QPU " << static_cast<unsigned>(ID) << " (0x" << std::hex << pc << std::dec
            << "): " << inst->toASMString() << logging::endl);
    ProgramCounter nextPC = pc;
    if(inst->getSig() == SIGNAL_END_PROGRAM)
        // end program
        return false;
    if(inst->getSig() == SIGNAL_NONE || executeSignal(inst->getSig()))
    {
        if(auto op = inst->as<qpu_asm::ALUInstruction>())
        {
            if(executeALU(op))
                ++nextPC;
            // otherwise the execution stalled and the PC stays the same
        }
        else if(auto br = inst->as<qpu_asm::BranchInstruction>())
        {
            if(isConditionMet(br->getBranchCondition()))
            {
                ++instrumentation[inst].numBranchTaken;
                int32_t offset = 4 /* Branch starts at PC + 4 */ +
                    (br->getImmediate() / static_cast<int32_t>(sizeof(uint64_t))) /* immediate offset is in bytes */;
                if(br->getAddRegister() == BranchReg::BRANCH_REG ||
                    br->getBranchRelative() == BranchRel::BRANCH_ABSOLUTE)
                    throw CompilationError(
                        CompilationStep::GENERAL, "This kind of branch is not yet implemented", br->toASMString());
                nextPC += static_cast<ProgramCounter>(offset);

                // see Broadcom specification, page 34
                registers.writeRegister(toRegister(br->getAddOut(), br->getWriteSwap() == WriteSwap::SWAP),
                    SIMDVector(Literal(pc + 4)), std::bitset<16>(0xFFFF), BITMASK_ALL);
                registers.writeRegister(toRegister(br->getMulOut(), br->getWriteSwap() == WriteSwap::DONT_SWAP),
                    SIMDVector(Literal(pc + 4)), std::bitset<16>(0xFFFF), BITMASK_ALL);
            }
            else
                // simply skip to next PC
                ++nextPC;
            PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 160, "branches taken",
                isConditionMet(br->getBranchCondition()) ? 1 : 0);
        }
        else if(auto load = inst->as<qpu_asm::LoadInstruction>())
        {
            auto type = load->getType();
            SIMDVector loadedValues;
            switch(type)
            {
            case OpLoad::LOAD_IMM_32:
                loadedValues = SIMDVector(Literal(load->getImmediateInt()));
                break;
            case OpLoad::LOAD_SIGNED:
                loadedValues = intermediate::LoadImmediate::toLoadedValues(
                    load->getImmediateInt(), intermediate::LoadType::PER_ELEMENT_SIGNED);
                break;
            case OpLoad::LOAD_UNSIGNED:
                loadedValues = intermediate::LoadImmediate::toLoadedValues(
                    load->getImmediateInt(), intermediate::LoadType::PER_ELEMENT_UNSIGNED);
                break;
            }

            auto flags = generateImmediateFlags(loadedValues);
            if(load->getPack().hasEffect())
                PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
            SIMDVector imm = load->getPack()(loadedValues, flags, false);
            auto mask = load->getPack().getMask();
            writeConditional(toRegister(load->getAddOut(), load->getWriteSwap() == WriteSwap::SWAP), imm,
                load->getAddCondition(), mask);
            writeConditional(toRegister(load->getMulOut(), load->getWriteSwap() == WriteSwap::DONT_SWAP), imm,
                load->getMulCondition(), mask);
            if(load->getSetFlag() == SetFlag::SET_FLAGS)
                setFlags(imm, load->getAddCondition() != COND_NEVER ? load->getAddCondition() : load->getMulCondition(),
                    flags);
            ++nextPC;
        }
        else if(auto semaphore = inst->as<qpu_asm::SemaphoreInstruction>())
        {
            bool dontStall = true;
            SIMDVector result{};
            if(semaphore->getAcquire())
                // NOTE: "acquire" is decrement, see SemaphoreInstruction#getAcquire() function documentation
                std::tie(result, dontStall) = semaphores.decrement(static_cast<uint8_t>(semaphore->getSemaphore()));
            else
                std::tie(result, dontStall) = semaphores.increment(static_cast<uint8_t>(semaphore->getSemaphore()));

            if(dontStall)
            {
                if(semaphore->getPack().hasEffect())
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
                result = semaphore->getPack()(result, {}, false);
                auto mask = semaphore->getPack().getMask();
                writeConditional(toRegister(semaphore->getAddOut(), semaphore->getWriteSwap() == WriteSwap::SWAP),
                    result, semaphore->getAddCondition(), mask);
                writeConditional(toRegister(semaphore->getMulOut(), semaphore->getWriteSwap() == WriteSwap::DONT_SWAP),
                    result, semaphore->getMulCondition(), mask);
                if(semaphore->getSetFlag() == SetFlag::SET_FLAGS)
                    setFlags(result,
                        semaphore->getAddCondition() != COND_NEVER ? semaphore->getAddCondition() :
                                                                     semaphore->getMulCondition(),
                        {});
                ++nextPC;
            }
            else
                ++instrumentation[inst].numStalls;
        }
        else
            throw CompilationError(CompilationStep::GENERAL, "Invalid assembler instruction", inst->toASMString());
    }

    // clear cache for registers already read this instruction
    registers.clearReadCache();

    ++currentCycle;
    pc = nextPC;
    return true;
}

const qpu_asm::Instruction* QPU::getCurrentInstruction(
    std::vector<qpu_asm::Instruction>::const_iterator firstInstruction) const
{
    return &(*(firstInstruction + pc));
}

static std::pair<SIMDVector, bool> toInputValue(Registers& registers, InputMultiplex mux, Address addressA,
    Address addressB, bool regBIsImmediate, bool anyElementExecuted)
{
    switch(mux)
    {
    case InputMultiplex::ACC0:
        return registers.readRegister(REG_ACC0, anyElementExecuted);
    case InputMultiplex::ACC1:
        return registers.readRegister(REG_ACC1, anyElementExecuted);
    case InputMultiplex::ACC2:
        return registers.readRegister(REG_ACC2, anyElementExecuted);
    case InputMultiplex::ACC3:
        return registers.readRegister(REG_ACC3, anyElementExecuted);
    case InputMultiplex::ACC4:
        return registers.readRegister(REG_SFU_OUT, anyElementExecuted);
    case InputMultiplex::ACC5:
        return registers.readRegister(REG_ACC5, anyElementExecuted);
    case InputMultiplex::REGA:
        return registers.readRegister(Register{RegisterFile::PHYSICAL_A, addressA}, anyElementExecuted);
    case InputMultiplex::REGB:
        if(regBIsImmediate)
            return std::make_pair(SIMDVector(*SmallImmediate{addressB}.toLiteral()), true);
        else
            return registers.readRegister(Register{RegisterFile::PHYSICAL_B, addressB}, anyElementExecuted);
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ALU input");
}

static std::pair<SIMDVector, bool> applyVectorRotation(std::pair<SIMDVector, bool>&& input,
    const qpu_asm::ALUInstruction* ins, Registers& registers, bool anyElementExecuted)
{
    if(!input.second)
        // if we stall, do not rotate
        return std::move(input);
    if(!ins->isVectorRotation())
        // no rotation set
        return std::move(input);
    SmallImmediate offset(ins->getInputB());
    if(!offset.isVectorRotation())
        return std::move(input);
    if(ins->getMulMultiplexA() == InputMultiplex::REGB || ins->getMulMultiplexB() == InputMultiplex::REGB)
        // XXX can't we actually?! See http://maazl.de/project/vc4asm/doc/VideoCoreIV-addendum.html
        throw CompilationError(CompilationStep::GENERAL, "Cannot read vector rotation offset", input.first.to_string());

    if(input.first.isUndefined() || input.first.getAllSame())
        return std::move(input);

    unsigned char distance;
    if(offset == VECTOR_ROTATE_R5)
        //"Mul output vector rotation is taken from accumulator r5, element 0, bits [3:0]"
        // - Broadcom Specification, page 30
        distance = static_cast<uint8_t>(registers.readRegister(REG_ACC5, anyElementExecuted).first[0].unsignedInt());
    else
        distance = offset.getRotationOffset().value();

    SIMDVector result(std::move(input.first));
    if(ins->isFullRangeRotation())
        result = std::move(result).rotate(distance & 0xF);
    else
        result = std::move(result).rotatePerQuad(distance & 0x3);

    PROFILE_COUNTER(
        vc4c::profiler::COUNTER_EMULATOR + 170, "vector rotations (full/total)", ins->isFullRangeRotation());
    return std::make_pair(result, true);
}

static bool isAnyElementExecuted(const VectorFlags& flags, ConditionCode code)
{
    return code == COND_ALWAYS ||
        std::any_of(flags.begin(), flags.end(), [=](ElementFlags flag) { return flag.matchesCondition(code); });
}

bool QPU::executeALU(const qpu_asm::ALUInstruction* aluInst)
{
    SIMDVector addIn0{};
    SIMDVector addIn1{};
    SIMDVector mulIn0{};
    SIMDVector mulIn1{};

    const OpCode& addCode = OpCode::toOpCode(aluInst->getAddition(), false);
    const OpCode& mulCode = OpCode::toOpCode(aluInst->getMultiplication(), true);

    // need to read both input before writing any registers
    if(aluInst->getAddCondition() != COND_NEVER && aluInst->getAddition() != OP_NOP.opAdd)
    {
        bool anyElementExecuting = isAnyElementExecuted(flags, aluInst->getAddCondition());

        bool addIn0NotStall = true;
        bool addIn1NotStall = true;
        std::tie(addIn0, addIn0NotStall) = toInputValue(registers, aluInst->getAddMultiplexA(), aluInst->getInputA(),
            aluInst->getInputB(), aluInst->getSig() == SIGNAL_ALU_IMMEDIATE, anyElementExecuting);
        if(addCode.numOperands > 1)
            std::tie(addIn1, addIn1NotStall) =
                toInputValue(registers, aluInst->getAddMultiplexB(), aluInst->getInputA(), aluInst->getInputB(),
                    aluInst->getSig() == SIGNAL_ALU_IMMEDIATE, anyElementExecuting);

        if(!addIn0NotStall || !addIn1NotStall)
        {
            // we stall on input, so do not calculate anything
            ++instrumentation[aluInst].numStalls;
            return false;
        }
    }

    if(aluInst->getMulCondition() != COND_NEVER && aluInst->getMultiplication() != OP_NOP.opMul)
    {
        bool anyElementExecuting = isAnyElementExecuted(flags, aluInst->getMulCondition());

        bool mulIn0NotStall = true;
        bool mulIn1NotStall = true;

        PROFILE_START(EmulateVectorRotation);
        std::tie(mulIn0, mulIn0NotStall) = applyVectorRotation(
            toInputValue(registers, aluInst->getMulMultiplexA(), aluInst->getInputA(), aluInst->getInputB(),
                aluInst->getSig() == SIGNAL_ALU_IMMEDIATE, anyElementExecuting),
            aluInst, registers, anyElementExecuting);
        if(mulCode.numOperands > 1)
            std::tie(mulIn1, mulIn1NotStall) = applyVectorRotation(
                toInputValue(registers, aluInst->getMulMultiplexB(), aluInst->getInputA(), aluInst->getInputB(),
                    aluInst->getSig() == SIGNAL_ALU_IMMEDIATE, anyElementExecuting),
                aluInst, registers, anyElementExecuting);
        PROFILE_END(EmulateVectorRotation);

        if(!mulIn0NotStall || !mulIn1NotStall)
        {
            // we stall on input, so do not calculate anything
            ++instrumentation[aluInst].numStalls;
            return false;
        }
    }

    if(aluInst->getAddCondition() != COND_NEVER && aluInst->getAddition() != OP_NOP.opAdd)
    {
        if(aluInst->getUnpack().hasEffect())
        {
            PROFILE_START(EmulateUnpack);
            if(aluInst->getUnpack().isUnpackFromR4())
            {
                if(aluInst->getAddMultiplexA() == InputMultiplex::ACC4)
                    addIn0 = aluInst->getUnpack()(addIn0, addCode.acceptsFloat);
                if(aluInst->getAddMultiplexB() == InputMultiplex::ACC4)
                    addIn1 = aluInst->getUnpack()(addIn1, addCode.acceptsFloat);
            }
            else
            {
                if(aluInst->getAddMultiplexA() == InputMultiplex::REGA)
                    addIn0 = aluInst->getUnpack()(addIn0, addCode.acceptsFloat);
                if(aluInst->getAddMultiplexB() == InputMultiplex::REGA)
                    addIn1 = aluInst->getUnpack()(addIn1, addCode.acceptsFloat);
            }
            PROFILE_END(EmulateUnpack);
        }

        PROFILE_START(EmulateOpcode);
        auto tmp = addCode(addIn0, addIn1);
        PROFILE_END(EmulateOpcode);
        if(!tmp.first)
            logging::error() << "Failed to emulate ALU operation: " << addCode.name << " with "
                             << addIn0.to_string(true) << " and " << addIn1.to_string(true) << logging::endl;
        // fall-through for errors above on purpose so the next instruction throws an exception
        auto result = std::move(tmp.first).value();
        auto mask = BITMASK_ALL;
        if(aluInst->getWriteSwap() == WriteSwap::DONT_SWAP && aluInst->getPack().hasEffect())
        {
            PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
            if(aluInst->getPack().supportsMulALU())
                throw CompilationError(CompilationStep::GENERAL, "Cannot apply mul pack mode on add result!");
            result = aluInst->getPack()(result, tmp.second, addCode.returnsFloat);
            mask = aluInst->getPack().getMask();
        }

        writeConditional(toRegister(aluInst->getAddOut(), aluInst->getWriteSwap() == WriteSwap::SWAP), result,
            aluInst->getAddCondition(), mask, aluInst, nullptr);
        if(aluInst->getSetFlag() == SetFlag::SET_FLAGS)
            setFlags(result, aluInst->getAddCondition(), tmp.second);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 180, "add instructions", 1);
    }
    if(aluInst->getMulCondition() != COND_NEVER && aluInst->getMultiplication() != OP_NOP.opMul)
    {
        if(aluInst->getUnpack().hasEffect())
        {
            PROFILE_START(EmulateUnpack);
            if(aluInst->getUnpack().isUnpackFromR4())
            {
                if(aluInst->getMulMultiplexA() == InputMultiplex::ACC4)
                    mulIn0 = aluInst->getUnpack()(mulIn0, mulCode.acceptsFloat);
                if(aluInst->getMulMultiplexB() == InputMultiplex::ACC4)
                    mulIn1 = aluInst->getUnpack()(mulIn1, mulCode.acceptsFloat);
            }
            else
            {
                if(aluInst->getMulMultiplexA() == InputMultiplex::REGA)
                    mulIn0 = aluInst->getUnpack()(mulIn0, mulCode.acceptsFloat);
                if(aluInst->getMulMultiplexB() == InputMultiplex::REGA)
                    mulIn1 = aluInst->getUnpack()(mulIn1, mulCode.acceptsFloat);
            }
            PROFILE_END(EmulateUnpack);
        }

        PROFILE_START(EmulateOpcode);
        auto tmp = mulCode(mulIn0, mulIn1);
        PROFILE_END(EmulateOpcode);
        if(!tmp.first)
            logging::error() << "Failed to emulate ALU operation: " << mulCode.name << " with "
                             << mulIn0.to_string(true) << " and " << mulIn1.to_string(true) << logging::endl;
        auto result = std::move(tmp.first).value();
        auto mask = BITMASK_ALL;
        if(aluInst->getWriteSwap() == WriteSwap::SWAP && aluInst->getPack().hasEffect())
        {
            PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
            if(!aluInst->getPack().supportsMulALU())
                throw CompilationError(CompilationStep::GENERAL, "Cannot apply add pack mode on mul result!");
            result = aluInst->getPack()(result, tmp.second, mulCode.returnsFloat);
            mask = aluInst->getPack().getMask();
        }

        // FIXME these might depend on flags of add ALU set in same instruction (which is wrong)
        writeConditional(toRegister(aluInst->getMulOut(), aluInst->getWriteSwap() == WriteSwap::DONT_SWAP), result,
            aluInst->getMulCondition(), mask, nullptr, aluInst);
        if(aluInst->getSetFlag() == SetFlag::SET_FLAGS &&
            isFlagSetByMulALU(aluInst->getAddition(), aluInst->getMultiplication()))
            setFlags(result, aluInst->getMulCondition(), tmp.second);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 190, "mul instructions", 1);
    }

    if(aluInst->getUnpack().hasEffect())
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 220, "values unpacked", 1);

    return true;
}

void QPU::writeConditional(Register dest, const SIMDVector& in, ConditionCode cond, BitMask bitMask,
    const qpu_asm::ALUInstruction* addInst, const qpu_asm::ALUInstruction* mulInst)
{
    if(cond == COND_ALWAYS)
    {
        registers.writeRegister(dest, in, std::bitset<16>(0xFFFF), bitMask);
        if(addInst)
            ++instrumentation[addInst].numAddALUExecuted;
        if(mulInst)
            ++instrumentation[mulInst].numMulALUExecuted;
        return;
    }
    else if(cond == COND_NEVER)
    {
        if(addInst)
            ++instrumentation[addInst].numAddALUSkipped;
        if(mulInst)
            ++instrumentation[mulInst].numMulALUSkipped;
        return;
    }

    std::bitset<16> elementMask{};
    if(in.isUndefined())
    {
        if(std::any_of(flags.begin(), flags.end(),
               [&](const ElementFlags& flag) -> bool { return flag.matchesCondition(cond); }))
        {
            // we would write an undefined value
            throw CompilationError(
                CompilationStep::GENERAL, "Cannot write an undefined value", dest.to_string(true, false));
        }
        // do not write register, but also don't return to allow instrumentation
    }
    else
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            if(flags[i].matchesCondition(cond))
                elementMask.set(i);
        }
        registers.writeRegister(dest, in, elementMask, bitMask);
    }

    if(addInst != nullptr)
    {
        if(elementMask.any())
            ++instrumentation[addInst].numAddALUExecuted;
        else
            ++instrumentation[addInst].numAddALUSkipped;
    }
    if(mulInst != nullptr)
    {
        if(elementMask.any())
            ++instrumentation[mulInst].numMulALUExecuted;
        else
            ++instrumentation[mulInst].numMulALUSkipped;
    }
}

bool QPU::isConditionMet(BranchCond cond) const
{
    ConditionCode singleCond = COND_NEVER;
    bool checkAll;
    switch(cond)
    {
    case BranchCond::ALL_C_CLEAR:
        checkAll = true;
        singleCond = COND_CARRY_CLEAR;
        break;
    case BranchCond::ALL_C_SET:
        checkAll = true;
        singleCond = COND_CARRY_SET;
        break;
    case BranchCond::ALL_N_CLEAR:
        checkAll = true;
        singleCond = COND_NEGATIVE_CLEAR;
        break;
    case BranchCond::ALL_N_SET:
        checkAll = true;
        singleCond = COND_NEGATIVE_SET;
        break;
    case BranchCond::ALL_Z_CLEAR:
        checkAll = true;
        singleCond = COND_ZERO_CLEAR;
        break;
    case BranchCond::ALL_Z_SET:
        checkAll = true;
        singleCond = COND_ZERO_SET;
        break;
    case BranchCond::ALWAYS:
        return true;
    case BranchCond::ANY_C_CLEAR:
        checkAll = false;
        singleCond = COND_CARRY_CLEAR;
        break;
    case BranchCond::ANY_C_SET:
        checkAll = false;
        singleCond = COND_CARRY_SET;
        break;
    case BranchCond::ANY_N_CLEAR:
        checkAll = false;
        singleCond = COND_NEGATIVE_CLEAR;
        break;
    case BranchCond::ANY_N_SET:
        checkAll = false;
        singleCond = COND_NEGATIVE_SET;
        break;
    case BranchCond::ANY_Z_CLEAR:
        checkAll = false;
        singleCond = COND_ZERO_CLEAR;
        break;
    case BranchCond::ANY_Z_SET:
        checkAll = false;
        singleCond = COND_ZERO_SET;
        break;
    default:
        throw CompilationError(CompilationStep::GENERAL, "Unhandled branch condition", toString(cond));
    }
    if(checkAll)
        return std::all_of(flags.begin(), flags.end(),
            [singleCond](ElementFlags flags) -> bool { return flags.matchesCondition(singleCond); });
    return std::any_of(flags.begin(), flags.end(),
        [singleCond](ElementFlags flags) -> bool { return flags.matchesCondition(singleCond); });
}

bool QPU::executeSignal(Signaling signal)
{
    if(signal == SIGNAL_ALU_IMMEDIATE || signal == SIGNAL_BRANCH || signal == SIGNAL_LOAD_IMMEDIATE ||
        signal == SIGNAL_NONE)
        // ignore
        return true;
    if(signal == SIGNAL_LOAD_TMU0)
        return tmus.triggerTMURead(0);
    else if(signal == SIGNAL_LOAD_TMU1)
        return tmus.triggerTMURead(1);
    else
        throw CompilationError(CompilationStep::GENERAL, "Unhandled signal", signal.to_string());
}

void QPU::setFlags(const SIMDVector& output, ConditionCode cond, const VectorFlags& newFlags)
{
    std::vector<std::string> parts;
    logging::logLazy(logging::Level::DEBUG, [&]() { parts.reserve(flags.size()); });
    for(uint8_t i = 0; i < flags.size(); ++i)
    {
        if(flags[i].matchesCondition(cond))
        {
            // only update flags for elements we actually write (where we actually calculate a result)
            flags[i] = newFlags[i];
            // do not set overflow flag, it is only valid for the one instruction
            flags[i].overflow = FlagStatus::UNDEFINED;

            logging::logLazy(logging::Level::DEBUG, [&]() { parts.emplace_back(flags[i].to_string()); });
        }
    }
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Setting flags: {" + to_string<std::string>(parts) << "}" << logging::endl);

    // TODO not completely correct, see http://maazl.de/project/vc4asm/doc/instructions.html
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 200, "flags set", 1);
}

std::vector<MemoryAddress> tools::buildUniforms(Memory& memory, MemoryAddress baseAddress,
    const std::vector<MemoryAddress>& parameter, const WorkGroupConfig& config, MemoryAddress globalData,
    const KernelUniforms& uniformsUsed)
{
    std::vector<MemoryAddress> res;

    Word numQPUs = config.localSizes[0] * config.localSizes[1] * config.localSizes[2];
    res.reserve(numQPUs);

    std::vector<Word> qpuUniforms;
    qpuUniforms.resize(uniformsUsed.countUniforms() + parameter.size());

    if((config.numGroups[0] > 1 || config.numGroups[1] > 1 || config.numGroups[2] > 1) &&
        !(uniformsUsed.getMaxGroupIDXUsed() && uniformsUsed.getMaxGroupIDYUsed() && uniformsUsed.getMaxGroupIDZUsed()))
        throw CompilationError(CompilationStep::GENERAL,
            "Emulator of multiple work-groups requires work-group-loop optimization to be enabled!");

    for(uint8_t q = 0; q < numQPUs; ++q)
    {
        std::array<Word, 3> localIDs = {q % config.localSizes[0], (q / config.localSizes[0]) % config.localSizes[1],
            (q / config.localSizes[0]) / config.localSizes[1]};

        std::size_t i = 0;
        if(uniformsUsed.getWorkDimensionsUsed())
            qpuUniforms[i++] = config.dimensions;
        if(uniformsUsed.getLocalSizesUsed())
            qpuUniforms[i++] = (config.localSizes[2] << 16) | (config.localSizes[1] << 8) | config.localSizes[0];
        if(uniformsUsed.getLocalIDsUsed())
            qpuUniforms[i++] = (localIDs[2] << 16) | (localIDs[1] << 8) | localIDs[0];
        if(uniformsUsed.getNumGroupsXUsed())
            qpuUniforms[i++] = config.numGroups[0];
        if(uniformsUsed.getNumGroupsYUsed())
            qpuUniforms[i++] = config.numGroups[1];
        if(uniformsUsed.getNumGroupsZUsed())
            qpuUniforms[i++] = config.numGroups[2];
        if(uniformsUsed.getGroupIDXUsed())
            qpuUniforms[i++] = 0; // is only set for single work-groups
        if(uniformsUsed.getGroupIDYUsed())
            qpuUniforms[i++] = 0; // is only set for single work-groups
        if(uniformsUsed.getGroupIDZUsed())
            qpuUniforms[i++] = 0; // is only set for single work-groups
        if(uniformsUsed.getGlobalOffsetXUsed())
            qpuUniforms[i++] = config.globalOffsets[0];
        if(uniformsUsed.getGlobalOffsetYUsed())
            qpuUniforms[i++] = config.globalOffsets[1];
        if(uniformsUsed.getGlobalOffsetZUsed())
            qpuUniforms[i++] = config.globalOffsets[2];
        if(uniformsUsed.getGlobalDataAddressUsed())
            qpuUniforms[i++] = globalData;
        for(auto param : parameter)
            qpuUniforms[i++] = param;
        if(uniformsUsed.getUniformAddressUsed())
            qpuUniforms[i++] = baseAddress;
        if(uniformsUsed.getMaxGroupIDXUsed())
            qpuUniforms[i++] = config.numGroups[0];
        if(uniformsUsed.getMaxGroupIDYUsed())
            qpuUniforms[i++] = config.numGroups[1];
        if(uniformsUsed.getMaxGroupIDZUsed())
            qpuUniforms[i++] = config.numGroups[2];

        memory.setUniforms(qpuUniforms, baseAddress);
        res.emplace_back(baseAddress);
        baseAddress += static_cast<Word>(qpuUniforms.size() * sizeof(Word));
    }

    return res;
}

static void emulateStep(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction, std::vector<QPU>& qpus,
    std::bitset<NATIVE_VECTOR_SIZE>& activeQPUs)
{
    for(unsigned i = 0; i < qpus.size(); ++i)
    {
        if(!activeQPUs.test(i))
            continue;
        try
        {
            bool continueRunning = qpus[i].execute(firstInstruction);
            if(!continueRunning)
                // this QPU has finished
                activeQPUs.reset(i);
        }
        catch(const std::exception&)
        {
            logging::error() << "Emulation threw exception execution in following instruction on QPU "
                             << static_cast<unsigned>(qpus[i].ID) << ": "
                             << qpus[i].getCurrentInstruction(firstInstruction)->toHexString(true) << logging::endl;
            // re-throw error
            throw;
        }
    }
}

bool tools::emulate(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction, Memory& memory,
    const std::vector<MemoryAddress>& uniformAddresses, InstrumentationResults& instrumentation, uint32_t maxCycles)
{
    if(uniformAddresses.size() > NUM_QPUS)
        throw CompilationError(CompilationStep::GENERAL, "Cannot use more than 12 QPUs!");

    Mutex mutex;
    // FIXME is SFU execution per QPU or need SFUs be locked?
    std::array<SFU, NUM_QPUS> sfus;
    VPM vpm(memory);
    Semaphores semaphores;

    std::vector<QPU> qpus;
    qpus.reserve(uniformAddresses.size());
    std::bitset<NATIVE_VECTOR_SIZE> activeQPUs = (1 << uniformAddresses.size()) - 1;
    uint8_t numQPU = 0;
    for(MemoryAddress uniformPointer : uniformAddresses)
    {
        qpus.emplace_back(numQPU, mutex, sfus.at(numQPU), vpm, semaphores, memory, uniformPointer, instrumentation);
        ++numQPU;
    }

    uint32_t cycle = 0;
    bool success = true;
    PROFILE_START(Emulation);
    while(activeQPUs.any())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Emulating cycle: " << cycle << logging::endl);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 250, "emulation cycles (utilization)", qpus.size());
        emulateStep(firstInstruction, qpus, activeQPUs);
        for(SFU& sfu : sfus)
            sfu.incrementCycle();
        vpm.incrementCycle();

        ++cycle;

        if(cycle == maxCycles)
        {
            logging::error() << "After the maximum number of execution cycles, following QPUs are still running: "
                             << logging::endl;
            for(const QPU& qpu : qpus)
                logging::error() << "QPU " << static_cast<unsigned>(qpu.ID) << ": "
                                 << qpu.getCurrentInstruction(firstInstruction)->toASMString() << logging::endl;
            success = false;
            break;
        }
    }
    PROFILE_END(Emulation);

    // Run some sanity checks
    semaphores.checkAllZero();
    if(mutex.isLocked())
    {
        CPPLOG_LAZY(logging::Level::ERROR, log << "Hardware mutex was not unlocked!" << logging::endl);
    }

    CPPLOG_LAZY(logging::Level::INFO,
        log << "Emulation " << (success ? "finished" : "timed out") << " for " << uniformAddresses.size()
            << " QPUs after " << cycle << " cycles" << logging::endl);

    vpm.dumpContents();
    return success;
}

bool tools::emulateTask(std::vector<qpu_asm::Instruction>::const_iterator firstInstruction,
    const std::vector<MemoryAddress>& parameter, Memory& memory, MemoryAddress uniformBaseAddress,
    MemoryAddress globalData, const KernelUniforms& uniformsUsed, InstrumentationResults& instrumentation,
    uint32_t maxCycles)
{
    WorkGroupConfig config;
    config.dimensions = 1;
    config.globalOffsets = {0, 0, 0};
    config.localSizes = {1, 1, 1};
    config.numGroups = {1, 1, 1};
    const auto uniformAddresses =
        buildUniforms(memory, uniformBaseAddress, parameter, config, globalData, uniformsUsed);
    return emulate(firstInstruction, memory, uniformAddresses, instrumentation, maxCycles);
}

static Memory fillMemory(const StableList<Global>& globalData, const EmulationData& settings,
    MemoryAddress& uniformBaseAddressOut, MemoryAddress& globalDataAddressOut,
    std::vector<MemoryAddress>& parameterAddressesOut)
{
    auto globalDataSize =
        std::accumulate(globalData.begin(), globalData.end(), 0u, [](unsigned u, const Global& global) -> unsigned {
            return u + global.initialValue.type.getInMemoryWidth() / (TYPE_INT32.getScalarBitCount() / 8);
        });
    auto size = globalDataSize + settings.calcParameterSize();
    // make sure to have enough space to align UNIFORMs
    while((size % 8) != 0)
        ++size;
    size += settings.calcNumWorkItems() * (16 + settings.parameter.size());
    Memory mem(size);

    MemoryAddress currentAddress = 0;
    globalDataAddressOut = currentAddress;

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Memory layout:" << logging::endl);

    for(const Global& global : globalData)
    {
        if(!global.initialValue.type.getArrayType() || global.initialValue.type.getElementType() != TYPE_INT32)
            throw CompilationError(
                CompilationStep::GENERAL, "Unhandled type of global data", global.initialValue.type.to_string());
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "\tGlobal '" << global.name << "' at offset 0x" << std::hex << currentAddress << std::dec
                << logging::endl);
        if(auto compound = global.initialValue.getCompound())
        {
            for(const auto& word : *compound)
            {
                *mem.getWordAddress(currentAddress) = word.getScalar()->unsignedInt();
                currentAddress += TYPE_INT32.getScalarBitCount() / 8;
            }
        }
        else if(auto lit = global.initialValue.getScalar())
        {
            *mem.getWordAddress(currentAddress) = lit->unsignedInt();
            currentAddress += TYPE_INT32.getScalarBitCount() / 8;
        }
        else
            throw CompilationError(
                CompilationStep::GENERAL, "Unhandled global data contents", global.initialValue.to_string());
    }

    parameterAddressesOut.reserve(settings.parameter.size());
    for(const auto& pair : settings.parameter)
    {
        tools::Word* addr = mem.getWordAddress(currentAddress);
        if(pair.second)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "\tParameter at offset 0x" << std::hex << currentAddress << std::dec << " with "
                    << pair.second->size() << " words" << logging::endl);
            parameterAddressesOut.emplace_back(currentAddress);
            std::copy_n(pair.second->data(), pair.second->size(), addr);
            currentAddress += static_cast<MemoryAddress>(pair.second->size() * sizeof(uint32_t));
        }
        else
            // value is directly read as input
            parameterAddressesOut.emplace_back(pair.first);
    }

    // align UNIFORMs to boundary of memory dump
    if(currentAddress % (sizeof(tools::Word) * 8) != 0)
        currentAddress +=
            static_cast<MemoryAddress>((sizeof(tools::Word) * 8) - (currentAddress % (sizeof(tools::Word) * 8)));

    uniformBaseAddressOut = currentAddress;

    return mem;
}

LCOV_EXCL_START
static void dumpMemory(const Memory& memory, const std::string& fileName, MemoryAddress uniformAddress, bool before)
{
    std::ofstream f(fileName, !before ? std::ios::app : std::ios::trunc);
    if(before)
        f << "Before: " << std::endl;
    else
        f << std::endl << "After: " << std::endl;
    MemoryAddress addr = 0;
    while(addr != memory.getMaximumAddress())
    {
        if(uniformAddress == addr)
            f << "Uniforms: " << std::endl;
        if(addr % (sizeof(tools::Word) * 8) == 0)
            f << std::hex << "0x" << addr << "\t";
        f << " " << std::hex << std::setfill('0') << std::setw(8) << memory.readWord(addr);
        if(addr % (sizeof(tools::Word) * 8) == (sizeof(tools::Word) * 7))
            f << std::endl;
        addr += static_cast<MemoryAddress>(sizeof(tools::Word));
    }
    f << std::endl;
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << std::dec << "Dumped " << addr << " words of memory into " << fileName << logging::endl);
}

std::string InstrumentationResult::to_string() const
{
    std::vector<std::string> parts;
    std::stringstream tmp;

    tmp << "execs: " << numExecutions;
    parts.emplace_back(tmp.str());
    tmp.str("");

    if(numAddALUExecuted + numAddALUSkipped > 0)
    {
        tmp << "add: " << numAddALUExecuted << "/" << numAddALUSkipped;
        parts.emplace_back(tmp.str());
        tmp.str("");
    }
    if(numMulALUExecuted + numMulALUSkipped > 0)
    {
        tmp << "mul: " << numMulALUExecuted << "/" << numMulALUSkipped;
        parts.emplace_back(tmp.str());
        tmp.str("");
    }
    if(numBranchTaken > 0)
    {
        tmp << "br: " << numBranchTaken;
        parts.emplace_back(tmp.str());
        tmp.str("");
    }
    if(numStalls > 0)
    {
        tmp << "stall: " << numStalls;
        parts.emplace_back(tmp.str());
        tmp.str("");
    }

    return vc4c::to_string<std::string>(parts);
}
LCOV_EXCL_STOP

EmulationResult tools::emulate(const EmulationData& data)
{
    qpu_asm::ModuleInfo module;
    StableList<Global> globals;
    std::vector<qpu_asm::Instruction> instructions;
    if(data.module.second != nullptr)
        extractBinary(*data.module.second, module, globals, instructions);
    else
    {
        std::ifstream f(data.module.first, std::ios_base::in | std::ios_base::binary);
        extractBinary(f, module, globals, instructions);
    }
    if(instructions.empty())
        throw CompilationError(CompilationStep::GENERAL, "Extracted module has no instructions!");
    if(module.kernelInfos.empty())
        throw CompilationError(CompilationStep::GENERAL, "Extracted module has no kernels!");

    auto kernelInfo = std::find_if(module.kernelInfos.begin(), module.kernelInfos.end(),
        [&data](const qpu_asm::KernelInfo& info) -> bool { return info.name == data.kernelName; });
    if(data.kernelName.empty() && module.kernelInfos.size() == 1)
        kernelInfo = module.kernelInfos.begin();
    if(kernelInfo == module.kernelInfos.end())
        throw CompilationError(CompilationStep::GENERAL, "Failed to find kernel-info for kernel", data.kernelName);
    // Count number of direct parameter words (e.g. also for literal vectors)
    auto numKernelWords = std::accumulate(kernelInfo->parameters.begin(), kernelInfo->parameters.end(), 0u,
        [](unsigned u, const qpu_asm::ParamInfo& param) -> unsigned { return u + param.getVectorElements(); });
    if(data.parameter.size() != numKernelWords)
        throw CompilationError(CompilationStep::GENERAL,
            "The number of parameters specified (" + std::to_string(data.parameter.size()) +
                ") does not match the number of kernel arguments (" +
                std::to_string(static_cast<unsigned>(kernelInfo->getParamCount())) + ')');

    MemoryAddress uniformAddress;
    MemoryAddress globalDataAddress;
    std::vector<MemoryAddress> paramAddresses;
    Memory mem(fillMemory(globals, data, uniformAddress, globalDataAddress, paramAddresses));

    auto uniformAddresses =
        buildUniforms(mem, uniformAddress, paramAddresses, data.workGroup, globalDataAddress, kernelInfo->uniformsUsed);

    if(!data.memoryDump.empty())
        dumpMemory(mem, data.memoryDump, uniformAddress, true);

    InstrumentationResults instrumentation;
    bool status = emulate(instructions.begin() +
            static_cast<std::vector<qpu_asm::Instruction>::difference_type>(
                (kernelInfo->getOffset() - module.kernelInfos.front().getOffset()).getValue()),
        mem, uniformAddresses, instrumentation, data.maxEmulationCycles);

    if(!data.memoryDump.empty())
        dumpMemory(mem, data.memoryDump, uniformAddress, false);

    EmulationResult result{data};
    result.executionSuccessful = status;

    result.results.reserve(data.parameter.size());
    for(std::size_t i = 0; i < data.parameter.size(); ++i)
    {
        if(!data.parameter[i].second)
            result.results.emplace_back(std::make_pair(data.parameter[i].first, Optional<std::vector<uint32_t>>{}));
        else
        {
            result.results.emplace_back(std::make_pair(paramAddresses[i], std::vector<uint32_t>{}));
            std::copy_n(mem.getWordAddress(paramAddresses[i]), data.parameter[i].second->size(),
                std::back_inserter(*result.results[i].second));
        }
    }

    // Map and dump instrumentation results
    std::unique_ptr<std::ofstream> dumpInstrumentation;
    if(!data.instrumentationDump.empty())
        dumpInstrumentation.reset(new std::ofstream(data.instrumentationDump));
    auto it = instructions.begin() +
        static_cast<std::vector<qpu_asm::Instruction>::difference_type>(
            (kernelInfo->getOffset() - module.kernelInfos.front().getOffset()).getValue());
    result.instrumentation.reserve(kernelInfo->getLength().getValue());
    while(true)
    {
        result.instrumentation.emplace_back(instrumentation[&(*it)]);
        if(dumpInstrumentation)
            *dumpInstrumentation << std::left << std::setw(80) << it->toASMString() << "//"
                                 << instrumentation[&(*it)].to_string() << std::endl;
        if(it->getSig() == SIGNAL_END_PROGRAM)
            break;
        ++it;
    }

    return result;
}

static std::vector<qpu_asm::Instruction> extractInstructions(const uint64_t* start, uint32_t numInstructions)
{
    std::vector<qpu_asm::Instruction> res;
    res.reserve(numInstructions);
    bool threadEndFound = false;
    for(std::size_t i = 0; i < numInstructions; ++i)
    {
        qpu_asm::Instruction instr(start[i]);
        if(!instr.isValidInstruction())
            throw CompilationError(CompilationStep::GENERAL, "Unrecognized instruction", std::to_string(start[i]));
        res.emplace_back(instr);
        if(instr.getSig() == SIGNAL_END_PROGRAM)
            threadEndFound = true;
    }
    if(!threadEndFound)
        throw CompilationError(CompilationStep::GENERAL, "Thread end instruction not found in code");
    return res;
}

LowLevelEmulationResult tools::emulate(const LowLevelEmulationData& data)
{
    auto instructions = extractInstructions(data.kernelAddress, data.numInstructions);
    Memory mem(data.buffers);

    InstrumentationResults instrumentation;
    bool status = emulate(instructions.begin(), mem, data.uniformAddresses, instrumentation, data.maxEmulationCycles);

    LowLevelEmulationResult result{data};
    result.executionSuccessful = status;

    // Map and dump instrumentation results
    std::unique_ptr<std::ofstream> dumpInstrumentation;
    if(!data.instrumentationDump.empty())
        dumpInstrumentation.reset(new std::ofstream(data.instrumentationDump));
    auto it = instructions.begin();
    result.instrumentation.reserve(data.numInstructions);
    while(true)
    {
        result.instrumentation.emplace_back(instrumentation[&(*it)]);
        if(dumpInstrumentation)
            *dumpInstrumentation << std::left << std::setw(80) << it->toASMString() << "//"
                                 << instrumentation[&(*it)].to_string() << std::endl;
        if(it->getSig() == SIGNAL_END_PROGRAM)
            break;
        ++it;
    }

    return result;
}
