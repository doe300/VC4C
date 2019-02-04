/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Emulator.h"

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

using namespace vc4c;
using namespace vc4c::tools;

extern std::vector<vc4c::Value> toLoadedValues(uint32_t mask, vc4c::intermediate::LoadType type);

extern void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo,
    ReferenceRetainingList<Global>& globals, std::vector<qpu_asm::Instruction>& instructions);

static Value ELEMENT_NUMBER(
    ContainerValue({Value(SmallImmediate(0), TYPE_INT8), Value(SmallImmediate(1), TYPE_INT8),
        Value(SmallImmediate(2), TYPE_INT8), Value(SmallImmediate(3), TYPE_INT8), Value(SmallImmediate(4), TYPE_INT8),
        Value(SmallImmediate(5), TYPE_INT8), Value(SmallImmediate(6), TYPE_INT8), Value(SmallImmediate(7), TYPE_INT8),
        Value(SmallImmediate(8), TYPE_INT8), Value(SmallImmediate(9), TYPE_INT8), Value(SmallImmediate(10), TYPE_INT8),
        Value(SmallImmediate(11), TYPE_INT8), Value(SmallImmediate(12), TYPE_INT8),
        Value(SmallImmediate(13), TYPE_INT8), Value(SmallImmediate(14), TYPE_INT8),
        Value(SmallImmediate(15), TYPE_INT8)}),
    TYPE_INT8);

std::size_t EmulationData::calcParameterSize() const
{
    return std::accumulate(parameter.begin(), parameter.end(), 0,
               [](std::size_t size,
                   const std::pair<uint32_t, vc4c::Optional<std::vector<uint32_t>>>& pair) -> std::size_t {
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
    if(VariantNamespace::holds_alternative<DirectBuffer>(data))
    {
        if(address >= VariantNamespace::get<DirectBuffer>(data).size() * sizeof(Word))
        {
            logging::warn() << "Buffer: [0, " << std::hex << VariantNamespace::get<DirectBuffer>(data).size()
                            << std::dec << ")" << logging::endl;
            throw CompilationError(CompilationStep::GENERAL,
                "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
        }
        return VariantNamespace::get<DirectBuffer>(data).data() + (address / sizeof(Word));
    }
    auto& buffers = VariantNamespace::get<MappedBuffers>(data);
    if(address >= (VariantNamespace::get<MappedBuffers>(data).rbegin()->first +
                      VariantNamespace::get<MappedBuffers>(data).rbegin()->second.get().size()))
    {
        logging::logLazy(logging::Level::WARNING, [&]() {
            for(const auto& buffer : buffers)
                logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                                << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
        });
        throw CompilationError(CompilationStep::GENERAL,
            "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
    }
    for(auto it = buffers.begin(); it != buffers.end(); ++it)
    {
        if(it->first <= address && (it->first + it->second.get().size()) > address)
        {
            auto wordBoundsAddress = (address / sizeof(Word)) * sizeof(Word);
            return reinterpret_cast<Word*>(it->second.get().data() + wordBoundsAddress - it->first);
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
    if(VariantNamespace::holds_alternative<DirectBuffer>(data))
    {
        if(address >= VariantNamespace::get<DirectBuffer>(data).size() * sizeof(Word))
        {
            logging::warn() << "Buffer: [0, " << std::hex << VariantNamespace::get<DirectBuffer>(data).size()
                            << std::dec << ")" << logging::endl;
            throw CompilationError(CompilationStep::GENERAL,
                "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
        }
        return VariantNamespace::get<DirectBuffer>(data).data() + (address / sizeof(Word));
    }
    auto& buffers = VariantNamespace::get<MappedBuffers>(data);
    if(address >= (VariantNamespace::get<MappedBuffers>(data).rbegin()->first +
                      VariantNamespace::get<MappedBuffers>(data).rbegin()->second.get().size()))
    {
        logging::logLazy(logging::Level::WARNING, [&]() {
            for(const auto& buffer : buffers)
                logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                                << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
        });
        throw CompilationError(CompilationStep::GENERAL,
            "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
    }
    for(auto it = buffers.begin(); it != buffers.end(); ++it)
    {
        if(it->first <= address && (it->first + it->second.get().size()) > address)
        {
            auto wordBoundsAddress = (address / sizeof(Word)) * sizeof(Word);
            return reinterpret_cast<const Word*>(it->second.get().data() + wordBoundsAddress - it->first);
        }
    }
    logging::logLazy(logging::Level::WARNING, [&]() {
        for(const auto& buffer : buffers)
            logging::warn() << "Buffer: [" << std::hex << buffer.first << ", "
                            << (buffer.first + buffer.second.get().size()) << std::dec << ")" << logging::endl;
    });
    throw CompilationError(CompilationStep::GENERAL, "Address is not part of any buffer", std::to_string(address));
}

Value Memory::readWord(MemoryAddress address) const
{
    if(address % sizeof(Word) != 0)
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Reading word from non-word-aligned memory location will be truncated to align with "
                   "word-boundaries: "
                << address << logging::endl);
    return Value(Literal(*getWordAddress(address)), TYPE_INT32);
}

MemoryAddress Memory::incrementAddress(MemoryAddress address, const DataType& typeSize) const
{
    return address + typeSize.getPhysicalWidth();
}

MemoryAddress Memory::getMaximumAddress() const
{
    if(VariantNamespace::holds_alternative<DirectBuffer>(data))
        return static_cast<MemoryAddress>(VariantNamespace::get<DirectBuffer>(data).size() * sizeof(Word));
    return VariantNamespace::get<MappedBuffers>(data).rbegin()->first +
        static_cast<Word>(VariantNamespace::get<MappedBuffers>(data).begin()->second.get().size());
}

void Memory::setUniforms(const std::vector<Word>& uniforms, MemoryAddress address)
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 10, "setUniforms", 1);
    std::size_t offset = address / sizeof(Word);
    std::copy_n(uniforms.begin(), uniforms.size(), VariantNamespace::get<DirectBuffer>(data).begin() + offset);
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

static std::string toRegisterWriteString(const Value& val, std::bitset<16> elementMask)
{
    if(elementMask.all())
        return val.to_string(true, true);
    std::vector<std::string> parts;
    parts.reserve(NATIVE_VECTOR_SIZE);
    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        const Value& element = val.hasContainer() ? val.container().elements[i] : val;
        if(elementMask.test(i))
            parts.push_back(element.to_string(false, true));
        else
            parts.push_back("-");
    }
    return (val.type.to_string() + " {") + to_string<std::string>(parts) + "}";
}

LiteralType getLiteralType(const DataType& type)
{
    if(type.isFloatingType())
        return LiteralType::REAL;
    if(type == TYPE_BOOL)
        return LiteralType::BOOL;
    return LiteralType::INTEGER;
}

void Registers::writeRegister(Register reg, const Value& val, std::bitset<16> elementMask)
{
    // set the types of the elements to the type of the container
    Value modifiedValue(val);
    if(modifiedValue.hasContainer())
    {
        for(unsigned i = 0; i < elementMask.size(); ++i)
        {
            if(elementMask.test(i))
            {
                modifiedValue.container().elements.at(i).type = modifiedValue.type.toVectorType(1);
                modifiedValue.container().elements.at(i).literal().type =
                    getLiteralType(modifiedValue.type.toVectorType(1));
            }
        }
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Writing into register '" << reg.to_string(true, false)
            << "': " << toRegisterWriteString(modifiedValue, elementMask) << logging::endl);
    if(reg.isGeneralPurpose())
        writeStorageRegister(reg, std::move(modifiedValue), elementMask);
    else if(reg.isAccumulator())
    {
        if(reg.num == REG_TMU_NOSWAP.num)
            qpu.tmus.setTMUNoSwap(getActualValue(modifiedValue));
        else if(reg.num == REG_REPLICATE_ALL.num)
            // the physical file A or B is important here!
            writeStorageRegister(reg, std::move(modifiedValue), elementMask);
        else
            writeStorageRegister(Register{RegisterFile::ACCUMULATOR, reg.num}, std::move(modifiedValue), elementMask);
    }
    else if(reg.num == REG_HOST_INTERRUPT.num)
    {
        if(hostInterrupt)
            throw CompilationError(
                CompilationStep::GENERAL, "Host interrupt was already triggered with", hostInterrupt->to_string());
        hostInterrupt = modifiedValue;
    }
    else if(reg.num == REG_NOP.num)
        return;
    else if(reg.num == REG_UNIFORM_ADDRESS.num)
        qpu.uniforms.setUniformAddress(getActualValue(modifiedValue));
    else if(reg.num == REG_VPM_IO.num)
        qpu.vpm.writeValue(getActualValue(modifiedValue));
    else if(reg == REG_VPM_IN_SETUP)
        qpu.vpm.setReadSetup(getActualValue(modifiedValue));
    else if(reg == REG_VPM_OUT_SETUP)
        qpu.vpm.setWriteSetup(getActualValue(modifiedValue));
    else if(reg == REG_VPM_DMA_LOAD_ADDR)
        qpu.vpm.setDMAReadAddress(getActualValue(modifiedValue));
    else if(reg == REG_VPM_DMA_STORE_ADDR)
        qpu.vpm.setDMAWriteAddress(getActualValue(modifiedValue));
    else if(reg.num == REG_MUTEX.num)
        qpu.mutex.unlock(qpu.ID);
    else if(reg.num == REG_SFU_RECIP.num)
        qpu.sfu.startRecip(getActualValue(modifiedValue));
    else if(reg.num == REG_SFU_RECIP_SQRT.num)
        qpu.sfu.startRecipSqrt(getActualValue(modifiedValue));
    else if(reg.num == REG_SFU_EXP2.num)
        qpu.sfu.startExp2(getActualValue(modifiedValue));
    else if(reg.num == REG_SFU_LOG2.num)
        qpu.sfu.startLog2(getActualValue(modifiedValue));
    else if(reg.num == REG_TMU0_COORD_S_U_X.num)
        qpu.tmus.setTMURegisterS(0, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU0_COORD_T_V_Y.num)
        qpu.tmus.setTMURegisterT(0, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU0_COORD_R_BORDER_COLOR.num)
        qpu.tmus.setTMURegisterR(0, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU0_COORD_B_LOD_BIAS.num)
        qpu.tmus.setTMURegisterB(0, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU1_COORD_S_U_X.num)
        qpu.tmus.setTMURegisterS(1, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU1_COORD_T_V_Y.num)
        qpu.tmus.setTMURegisterT(1, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU1_COORD_R_BORDER_COLOR.num)
        qpu.tmus.setTMURegisterR(1, getActualValue(modifiedValue));
    else if(reg.num == REG_TMU1_COORD_B_LOD_BIAS.num)
        qpu.tmus.setTMURegisterB(1, getActualValue(modifiedValue));
    else
        throw CompilationError(CompilationStep::GENERAL, "Write of invalid register", reg.to_string());

    // conditional write to periphery registers is not allowed
    if(!reg.isGeneralPurpose() && !reg.isAccumulator() && !elementMask.all())
        throw CompilationError(
            CompilationStep::GENERAL, "Conditional write to periphery registers is not allowed", reg.to_string());
}

std::pair<Value, bool> Registers::readRegister(Register reg)
{
    if(reg.isGeneralPurpose())
        return std::make_pair(readStorageRegister(reg), true);
    if(reg.file == RegisterFile::ACCUMULATOR && reg.num != REG_SFU_OUT.num)
        return std::make_pair(readStorageRegister(reg), true);
    if(reg.num == REG_SFU_OUT.num)
    {
        if(readCache.find(REG_SFU_OUT) != readCache.end())
            return std::make_pair(readCache.at(REG_SFU_OUT), true);
        auto pair = qpu.readR4();
        setReadCache(REG_SFU_OUT, pair.first);
        return pair;
    }
    if(reg.num == REG_UNIFORM.num)
    {
        if(readCache.find(REG_UNIFORM) == readCache.end())
            setReadCache(REG_UNIFORM, qpu.uniforms.readUniform());
        return std::make_pair(readCache.at(REG_UNIFORM), true);
    }
    if(reg == REG_ELEMENT_NUMBER)
        return std::make_pair(ELEMENT_NUMBER, true);
    if(reg == REG_QPU_NUMBER)
        return std::make_pair(Value(SmallImmediate(qpu.ID), TYPE_INT8), true);
    if(reg == REG_NOP)
    {
        logging::warn() << "Reading NOP register" << logging::endl;
        return std::make_pair(UNDEFINED_VALUE, true);
    }
    if(reg.num == REG_VPM_IO.num)
    {
        if(readCache.find(REG_VPM_IO) == readCache.end())
            setReadCache(REG_VPM_IO, qpu.vpm.readValue());
        return std::make_pair(readCache.at(REG_VPM_IO), true);
    }
    if(reg == REG_VPM_DMA_LOAD_WAIT)
        return std::make_pair(UNDEFINED_VALUE, qpu.vpm.waitDMARead());
    if(reg == REG_VPM_DMA_STORE_WAIT)
        return std::make_pair(UNDEFINED_VALUE, qpu.vpm.waitDMAWrite());
    if(reg.num == REG_MUTEX.num)
    {
        if(readCache.find(REG_MUTEX) == readCache.end())
            setReadCache(REG_MUTEX, qpu.mutex.lock(qpu.ID) ? BOOL_TRUE : BOOL_FALSE);
        return std::make_pair(readCache.at(REG_MUTEX), readCache.at(REG_MUTEX).getLiteralValue()->isTrue());
    }

    throw CompilationError(CompilationStep::GENERAL, "Read of invalid register", reg.to_string());
}

Value Registers::getInterruptValue() const
{
    if(hostInterrupt)
        return hostInterrupt.value();
    throw CompilationError(CompilationStep::GENERAL, "Host interrupt was not triggered!");
}

void Registers::clearReadCache()
{
    readCache.clear();
}

Value Registers::getActualValue(const Value& val)
{
    if(val.hasContainer())
        return val;
    if(val.hasLiteral())
        return val;
    if(val.hasRegister())
    {
        Value res = UNDEFINED_VALUE;
        bool dontBlock;
        std::tie(res, dontBlock) = readRegister(val.reg());
        if(!dontBlock)
            throw CompilationError(CompilationStep::GENERAL, "Unhandled case of blocking read", val.to_string());
        return res;
    }
    if(val.hasImmediate())
        return val;
    if(val.isUndefined())
        return val;
    throw CompilationError(CompilationStep::GENERAL, "Invalid value-type in emulator", val.to_string());
}

Value Registers::readStorageRegister(Register reg)
{
    if(storageRegisters.find(reg) == storageRegisters.end())
    {
        logging::warn() << "Reading from register not previously defined: " << reg.to_string() << logging::endl;
        return UNDEFINED_VALUE;
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Reading from register '" << reg.to_string(true, true)
            << "': " << storageRegisters.at(reg).to_string(true, true) << logging::endl);
    return storageRegisters.at(reg);
}

static Value toStorageValue(const Value& oldVal, const Value& newVal, std::bitset<16> elementMask)
{
    if(elementMask.all())
        return newVal;
    if(elementMask.none())
        return oldVal;
    Value result(ContainerValue(NATIVE_VECTOR_SIZE), newVal.type);

    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        const Value& newElement = newVal.hasContainer() ? newVal.container().elements[i] : newVal;
        const Value& oldElement = oldVal.hasContainer() ? oldVal.container().elements[i] : oldVal;
        if(elementMask.test(i))
            result.container().elements.push_back(newElement);
        else
            result.container().elements.push_back(oldElement);
    }
    return result;
}

void Registers::writeStorageRegister(Register reg, Value&& val, std::bitset<16> elementMask)
{
    if(storageRegisters.find(reg) == storageRegisters.end())
        storageRegisters.emplace(reg, val);
    else
        storageRegisters.at(reg) = toStorageValue(storageRegisters.at(reg), getActualValue(val), elementMask);
    if(reg.num == REG_REPLICATE_ALL.num && elementMask.any())
    {
        // is not actually stored in the physical file A or B
        storageRegisters.erase(reg);
        storageRegisters.emplace(REG_ACC5, val);

        const Value actualValue = getActualValue(val);
        if(actualValue.getLiteralValue() || actualValue.isUndefined())
        {
            // replicate same value across quads -> just store literal
            storageRegisters.at(REG_ACC5) = actualValue;
            return;
        }
        const auto& elements = actualValue.container().elements;
        if(reg.file == RegisterFile::PHYSICAL_A)
        {
            // per-quad replication
            storageRegisters.at(REG_ACC5) =
                Value(ContainerValue({elements[0], elements[0], elements[0], elements[0], elements[4], elements[4],
                          elements[4], elements[4], elements[8], elements[8], elements[8], elements[8], elements[12],
                          elements[12], elements[12], elements[12]}),
                    actualValue.type.toVectorType(16));
        }
        else if(reg.file == RegisterFile::PHYSICAL_B)
        {
            // across all elements replication
            storageRegisters.at(REG_ACC5) =
                Value(elements[0].getLiteralValue().value(), actualValue.type.toVectorType(16));
        }
        else
            throw CompilationError(CompilationStep::GENERAL,
                "Failed to determine register-file for replication register", reg.to_string());
    }
}

void Registers::setReadCache(Register reg, const Value& val)
{
    if(readCache.find(reg) == readCache.end())
        readCache.emplace(reg, val);
    else
        readCache.at(reg) = val;
}

Value UniformCache::readUniform()
{
    if(lastAddressSetCycle != 0 && lastAddressSetCycle + 2 > qpu.getCurrentCycle())
        // see Broadcom specification, page 22
        throw CompilationError(CompilationStep::GENERAL, "Reading UNIFORM within 2 cycles of last UNIFORM reset!");
    Value val = memory.readWord(uniformAddress);
    // do not increment UNIFORM pointer for multiple reads in same instruction
    uniformAddress = memory.incrementAddress(uniformAddress, TYPE_INT32);
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Reading UNIFORM value: " << val.to_string(false, true) << logging::endl);
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 40, "UNIFORM read", 1);
    return val;
}

void UniformCache::setUniformAddress(const Value& val)
{
    if(val.getLiteralValue())
    {
        uniformAddress = val.getLiteralValue()->toImmediate();
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Reset UNIFORM address to: " << uniformAddress << logging::endl);
    }
    else if(val.hasContainer())
        // see Broadcom specification, page 22
        setUniformAddress(val.container().elements[0]);
    else
        throw CompilationError(CompilationStep::GENERAL, "Invalid value to set as uniform address", val.to_string());
    lastAddressSetCycle = qpu.getCurrentCycle();
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 50, "write UNIFORM address", 1);
}

std::pair<Value, bool> TMUs::readTMU()
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
    std::queue<std::pair<Value, uint32_t>>* queue = nullptr;
    if(!tmu0ResponseQueue.empty())
        queue = &tmu0ResponseQueue;
    if(!tmu1ResponseQueue.empty())
    {
        if(queue == nullptr)
            queue = &tmu1ResponseQueue;
        else if(tmu1ResponseQueue.front().second < queue->front().second)
            queue = &tmu1ResponseQueue;
    }

    auto front = queue->front();
    queue->pop();
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 68, "TMU read", 1);
    return std::make_pair(front.first, true);
}

bool TMUs::hasValueOnR4() const
{
    return !tmu0ResponseQueue.empty() || !tmu1ResponseQueue.empty();
}

void TMUs::setTMUNoSwap(Value&& swapVal)
{
    if(swapVal.getLiteralValue())
        tmuNoSwap = swapVal.getLiteralValue()->isTrue();
    else if(swapVal.hasContainer())
        // XXX or per-element?
        setTMUNoSwap(std::move(swapVal.container().elements[0]));
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid value to set as uniform address", swapVal.to_string());
    lastTMUNoSwap = qpu.getCurrentCycle();
}

void TMUs::setTMURegisterS(uint8_t tmu, Value&& val)
{
    checkTMUWriteCycle();

    tmu = toRealTMU(tmu);
    auto& requestQueue = tmu == 1 ? tmu1RequestQueue : tmu0RequestQueue;

    if(requestQueue.size() >= 8)
        throw CompilationError(CompilationStep::GENERAL, "TMU request queue is full!");
    requestQueue.push(std::make_pair(readMemoryAddress(val), qpu.getCurrentCycle()));
}

void TMUs::setTMURegisterT(uint8_t tmu, Value&& val)
{
    checkTMUWriteCycle();
    throw CompilationError(CompilationStep::GENERAL, "Image reads via TMU are currently not supported!");
}

void TMUs::setTMURegisterR(uint8_t tmu, Value&& val)
{
    checkTMUWriteCycle();
    throw CompilationError(CompilationStep::GENERAL, "Image reads via TMU are currently not supported!");
}

void TMUs::setTMURegisterB(uint8_t tmu, Value&& val)
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
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 65, "TMU read trigger", val.second + 9 <= qpu.getCurrentCycle());
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

Value TMUs::readMemoryAddress(const Value& address) const
{
    Value res(ContainerValue(NATIVE_VECTOR_SIZE), TYPE_INT32);
    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        const Value& element = address.hasContainer() ? address.container().elements[i] : address;
        // TODO this is true for the real TMU, but we can have an offset of zero in the emulator
        //			if(element == INT_ZERO)
        //				//reading an address of zero disables TMU load for that element
        //				res.container.elements.push_back(UNDEFINED_VALUE);
        //			else
        if(element.isUndefined())
            throw CompilationError(
                CompilationStep::GENERAL, "Cannot read from undefined TMU address", address.to_string());
        else
            res.container().elements.push_back(memory.readWord(element.getLiteralValue()->toImmediate()));
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Reading via TMU from memory address " << address.to_string(false, true) << ": "
            << res.to_string(false, true) << logging::endl);
    return res;
}

uint8_t TMUs::toRealTMU(uint8_t tmu) const
{
    bool upperHalf = (qpu.ID % 4) >= 2;
    return (upperHalf && !tmuNoSwap) ? tmu ^ 1 : tmu;
}

Value SFU::readSFU()
{
    if(lastSFUWrite + 2 > currentCycle)
        throw CompilationError(
            CompilationStep::GENERAL, "Reading of SFU result within 2 cycles of triggering SFU calculation!");
    if(!sfuResult)
        throw CompilationError(CompilationStep::GENERAL, "Cannot read empty SFU result!");
    const Value val = sfuResult.value();
    sfuResult = NO_VALUE;
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 70, "SFU read", 1);
    return val;
}

bool SFU::hasValueOnR4() const
{
    return sfuResult.has_value();
}

static Value calcSFU(Value&& in, const std::function<float(float)>& func)
{
    if(in.getLiteralValue())
        return Value(Literal(func(static_cast<float>(in.getLiteralValue()->real()))), TYPE_FLOAT);
    if(in.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Cannot calculate SFU operation with undefined operand");
    if(in.hasContainer())
    {
        Value res(ContainerValue(in.container().elements.size()), in.type);
        for(Value& val : in.container().elements)
        {
            res.container().elements.push_back(calcSFU(std::move(val), func));
        }
        return res;
    }
    throw CompilationError(CompilationStep::GENERAL, "Invalid value to use as SFU-parameter", in.to_string());
}

void SFU::startRecip(Value&& val)
{
    sfuResult = calcSFU(std::move(val), [](float f) -> float { return 1.0f / f; });
    lastSFUWrite = currentCycle;
}

void SFU::startRecipSqrt(Value&& val)
{
    sfuResult = calcSFU(std::move(val), [](float f) -> float { return 1.0f / std::sqrt(f); });
    lastSFUWrite = currentCycle;
}

void SFU::startExp2(Value&& val)
{
    sfuResult = calcSFU(std::move(val), [](float f) -> float { return std::exp2(f); });
    lastSFUWrite = currentCycle;
}

void SFU::startLog2(Value&& val)
{
    sfuResult = calcSFU(std::move(val), [](float f) -> float { return std::log2(f); });
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

Value VPM::readValue()
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

    Value result = UNDEFINED_VALUE;
    Word* dataPtr = &cache.at(addresses.first).at(addresses.second);
    switch(setup.genericSetup.getSize())
    {
    case 0: // Byte
    {
        result = Value(ContainerValue(NATIVE_VECTOR_SIZE), TYPE_INT8.toVectorType(NATIVE_VECTOR_SIZE));
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            result.container().elements.push_back(Value(Literal((dataPtr[i / 4] >> ((i % 4) * 8)) & 0xFF), TYPE_INT8));
        }
        break;
    }
    case 1: // Half-word
    {
        result = Value(ContainerValue(NATIVE_VECTOR_SIZE), TYPE_INT16.toVectorType(NATIVE_VECTOR_SIZE));
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            result.container().elements.push_back(
                Value(Literal((dataPtr[i / 2] >> ((i % 2) * 16)) & 0xFFFF), TYPE_INT16));
        }
        break;
    }
    case 2: // Word
    {
        result = Value(ContainerValue(NATIVE_VECTOR_SIZE), TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE));
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            result.container().elements.push_back(Value(Literal(dataPtr[i]), TYPE_INT32));
        }
        break;
    }
    }

    setup.genericSetup.setAddress(
        static_cast<uint8_t>(setup.genericSetup.getAddress() + setup.genericSetup.getStride()));
    setup.genericSetup.setNumber(static_cast<uint8_t>((16 + setup.genericSetup.getNumber() - 1) % 16));
    vpmReadSetup = setup.value;

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Read value from VPM: " << result.to_string(false, true) << logging::endl;
        logging::debug() << "New read setup is now: " << setup.to_string() << logging::endl;
    });

    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 80, "VPM read", 1);
    return result;
}

void VPM::writeValue(Value&& val)
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
            const Value& element = val.hasContainer() ? val.container().elements[i] : val;
            dataPtr[i / 4] &= ~(0xFF << ((i % 4) * 8));
            if(element.getLiteralValue())
                // non-literal (e.g. undefined possible, simply skip writing element)
                dataPtr[i / 4] |= static_cast<Word>(element.getLiteralValue()->unsignedInt() & 0xFF) << ((i % 4) * 8);
        }
        break;
    }
    case 1: // Half-word
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            const Value& element = val.hasContainer() ? val.container().elements[i] : val;
            dataPtr[i / 2] &= ~(0xFFFF << ((i % 2) * 16));
            if(element.getLiteralValue())
                // non-literal (e.g. undefined possible, simply skip writing element)
                dataPtr[i / 2] |= static_cast<Word>(element.getLiteralValue()->unsignedInt() & 0xFFFF)
                    << ((i % 2) * 16);
        }
        break;
    }
    case 2: // Word
    {
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            const Value& element = val.hasContainer() ? val.container().elements[i] : val;
            if(element.getLiteralValue())
                // non-literal (e.g. undefined possible, simply skip writing element)
                dataPtr[i] = static_cast<Word>(element.getLiteralValue()->unsignedInt());
        }
        break;
    }
    }

    setup.genericSetup.setAddress(
        static_cast<uint8_t>(setup.genericSetup.getAddress() + setup.genericSetup.getStride()));
    vpmWriteSetup = setup.value;

    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Wrote value into VPM: " << val.to_string(true, true) << logging::endl;
        logging::debug() << "New write setup is now: " << setup.to_string() << logging::endl;
    });
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 90, "VPM written", 1);
}

void VPM::setWriteSetup(Value&& val)
{
    const Value& element0 = val.hasContainer() ? val.container().elements[0] : val;
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined VPM setup value", val.to_string());
    periphery::VPWSetup setup = periphery::VPWSetup::fromLiteral(element0.getLiteralValue()->unsignedInt());
    if(setup.isDMASetup())
        dmaWriteSetup = setup.value;
    else if(setup.isGenericSetup())
        vpmWriteSetup = setup.value;
    else if(setup.isStrideSetup())
        writeStrideSetup = setup.value;
    else
        throw CompilationError(CompilationStep::GENERAL, "Writing unknown VPM write setup",
            std::to_string(element0.getLiteralValue()->unsignedInt()));
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Set VPM write setup: " << setup.to_string() << logging::endl);
}

void VPM::setReadSetup(Value&& val)
{
    const Value& element0 = val.hasContainer() ? val.container().elements[0] : val;
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined VPM setup value", val.to_string());
    periphery::VPRSetup setup = periphery::VPRSetup::fromLiteral(element0.getLiteralValue()->unsignedInt());
    if(setup.isDMASetup())
        dmaReadSetup = setup.value;
    else if(setup.isGenericSetup())
        vpmReadSetup = setup.value;
    else if(setup.isStrideSetup())
        readStrideSetup = setup.value;
    else
        throw CompilationError(CompilationStep::GENERAL, "Writing unknown VPM read setup",
            std::to_string(element0.getLiteralValue()->unsignedInt()));
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Set VPM read setup: " << setup.to_string() << logging::endl);
}

void VPM::setDMAWriteAddress(Value&& val)
{
    const Value& element0 = val.hasContainer() ? val.container().elements[0] : val;
    if(element0.isUndefined())
        throw CompilationError(CompilationStep::GENERAL, "Undefined DMA setup value", val.to_string());
    periphery::VPWSetup setup = periphery::VPWSetup::fromLiteral(dmaWriteSetup);

    if(setup.value == 0)
        throw CompilationError(
            CompilationStep::GENERAL, "VPM DMA write setup was not previously set", setup.to_string());
    if(!setup.dmaSetup.getHorizontal())
        throw CompilationError(
            CompilationStep::GENERAL, "Vertical access to VPM is not yet supported", setup.to_string());

    std::pair<uint32_t, uint32_t> vpmBaseAddress =
        std::make_pair(setup.dmaSetup.getWordRow(), setup.dmaSetup.getWordColumn());
    std::pair<uint32_t, uint32_t> sizes = std::make_pair(setup.dmaSetup.getUnits(), setup.dmaSetup.getDepth());
    uint32_t typeSize =
        setup.dmaSetup.getMode() >= 4 ? 1 /* Byte */ : setup.dmaSetup.getMode() >= 2 ? 2 /* Half-word */ : 4 /* Word */;
    uint32_t byteOffset =
        typeSize == 4 ? 0 : typeSize == 2 ? setup.dmaSetup.getHalfRowOffset() * 2 : setup.dmaSetup.getByteOffset();

    if(typeSize * sizes.second > sizeof(Word) * NATIVE_VECTOR_SIZE)
        throw CompilationError(
            CompilationStep::GENERAL, "Accessing more than a VPM row at once is not supported", setup.to_string());

    auto stride = periphery::VPWSetup::fromLiteral(writeStrideSetup).strideSetup.getStride();

    MemoryAddress address = static_cast<MemoryAddress>(element0.getLiteralValue()->unsignedInt());

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Copying " << sizes.first << " rows with " << sizes.second << " elements of " << typeSize
            << " bytes each from VPM address " << vpmBaseAddress.first << "," << vpmBaseAddress.second
            << " into RAM at " << address << " with a memory stride of " << stride << logging::endl);

    if(vpmBaseAddress.first >= 64)
        throw CompilationError(
            CompilationStep::GENERAL, "VPM row address is out of range: ", std::to_string(vpmBaseAddress.first));

    for(uint32_t i = 0; i < sizes.first; ++i)
    {
        if(address + typeSize * sizes.second >= memory.getMaximumAddress())
            throw CompilationError(CompilationStep::GENERAL,
                "Memory address is out of bounds, consider using larger buffer", std::to_string(address));
        memcpy(reinterpret_cast<uint8_t*>(memory.getWordAddress(address)) + address % sizeof(Word),
            reinterpret_cast<uint8_t*>(&cache.at(vpmBaseAddress.first).at(vpmBaseAddress.second)) + byteOffset,
            typeSize * sizes.second);
        vpmBaseAddress.first += 1;
        // write stride is end-to-start, so add size of vector
        address += stride + (typeSize * sizes.second);
    }

    lastDMAWriteTrigger = currentCycle;
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 100, "write DMA write address", 1);
}

void VPM::setDMAReadAddress(Value&& val)
{
    const Value& element0 = val.hasContainer() ? val.container().elements[0] : val;
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

    MemoryAddress address = static_cast<MemoryAddress>(element0.getLiteralValue()->unsignedInt());

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

std::pair<Value, bool> Semaphores::increment(uint8_t index)
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 140, "semaphore increment", counter.at(index) < 15);
    if(counter.at(index) == 15)
        return std::make_pair(Value(SmallImmediate(15), TYPE_INT8), false);
    ++counter.at(index);
    return std::make_pair(Value(SmallImmediate(counter.at(index)), TYPE_INT8), true);
}

std::pair<Value, bool> Semaphores::decrement(uint8_t index)
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 150, "semaphore decrement", counter.at(0) > 0);
    if(counter.at(index) == 0)
        return std::make_pair(INT_ZERO, false);
    --counter.at(index);
    return std::make_pair(Value(SmallImmediate(counter.at(index)), TYPE_INT8), true);
}

uint32_t QPU::getCurrentCycle() const
{
    return currentCycle;
}

std::pair<Value, bool> QPU::readR4()
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

static ElementFlags generateImmediateFlags(Literal lit)
{
    ElementFlags flags;
    flags.carry = FlagStatus::CLEAR;
    flags.negative = lit.signedInt() < 0 ? FlagStatus::SET : FlagStatus::CLEAR;
    flags.overflow = FlagStatus::CLEAR;
    flags.zero = lit.unsignedInt() == 0 ? FlagStatus::SET : FlagStatus::CLEAR;
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
        if(inst->is<qpu_asm::ALUInstruction>())
        {
            if(executeALU(inst->as<qpu_asm::ALUInstruction>()))
                ++nextPC;
            // otherwise the execution stalled and the PC stays the same
        }
        else if(inst->is<qpu_asm::BranchInstruction>())
        {
            const auto br = inst->as<qpu_asm::BranchInstruction>();
            if(isConditionMet(br->getBranchCondition()))
            {
                ++instrumentation[inst].numBranchTaken;
                int32_t offset = 4 /* Branch starts at PC + 4 */ +
                    static_cast<int32_t>(br->getImmediate() / sizeof(uint64_t)) /* immediate offset is in bytes */;
                if(br->getAddRegister() == BranchReg::BRANCH_REG ||
                    br->getBranchRelative() == BranchRel::BRANCH_ABSOLUTE)
                    throw CompilationError(
                        CompilationStep::GENERAL, "This kind of branch is not yet implemented", br->toASMString());
                nextPC += offset;

                // see Broadcom specification, page 34
                registers.writeRegister(toRegister(br->getAddOut(), br->getWriteSwap() == WriteSwap::SWAP),
                    Value(Literal(pc + 4), TYPE_INT32), std::bitset<16>(0xFFFF));
                registers.writeRegister(toRegister(br->getMulOut(), br->getWriteSwap() == WriteSwap::DONT_SWAP),
                    Value(Literal(pc + 4), TYPE_INT32), std::bitset<16>(0xFFFF));
            }
            else
                // simply skip to next PC
                ++nextPC;
            PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 160, "branches taken",
                isConditionMet(br->getBranchCondition()) ? 1 : 0);
        }
        else if(inst->is<qpu_asm::LoadInstruction>())
        {
            const auto load = inst->as<qpu_asm::LoadInstruction>();
            auto type = load->getType();
            Value imm = Value(Literal(load->getImmediateInt()), TYPE_INT32);
            switch(type)
            {
            case OpLoad::LOAD_IMM_32:
                if(load->getPack().hasEffect())
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
                imm = load->getPack()(imm, generateImmediateFlags(Literal(load->getImmediateInt()))).value();
                break;
            case OpLoad::LOAD_SIGNED:
                imm = Value(ContainerValue(toLoadedValues(imm.getLiteralValue()->unsignedInt(),
                                vc4c::intermediate::LoadType::PER_ELEMENT_SIGNED)),
                    imm.type.toVectorType(16));
                break;
            case OpLoad::LOAD_UNSIGNED:
                imm = Value(ContainerValue(toLoadedValues(imm.getLiteralValue()->unsignedInt(),
                                vc4c::intermediate::LoadType::PER_ELEMENT_UNSIGNED)),
                    imm.type.toVectorType(16));
                break;
            }

            writeConditional(
                toRegister(load->getAddOut(), load->getWriteSwap() == WriteSwap::SWAP), imm, load->getAddCondition());
            writeConditional(toRegister(load->getMulOut(), load->getWriteSwap() == WriteSwap::DONT_SWAP), imm,
                load->getMulCondition());
            if(load->getSetFlag() == SetFlag::SET_FLAGS)
                setFlags(imm, load->getAddCondition() != COND_NEVER ? load->getAddCondition() : load->getMulCondition(),
                    generateImmediateFlags(Literal(load->getImmediateInt())));
            ++nextPC;
        }
        else if(inst->is<qpu_asm::SemaphoreInstruction>())
        {
            const auto semaphore = inst->as<qpu_asm::SemaphoreInstruction>();
            bool dontStall = true;
            Value result = UNDEFINED_VALUE;
            if(semaphore->getIncrementSemaphore())
                std::tie(result, dontStall) = semaphores.increment(static_cast<uint8_t>(semaphore->getSemaphore()));
            else
                std::tie(result, dontStall) = semaphores.decrement(static_cast<uint8_t>(semaphore->getSemaphore()));
            if(dontStall)
            {
                if(semaphore->getPack().hasEffect())
                    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
                result = semaphore->getPack()(result, {}).value();
                writeConditional(toRegister(semaphore->getAddOut(), semaphore->getWriteSwap() == WriteSwap::SWAP),
                    result, semaphore->getAddCondition());
                writeConditional(toRegister(semaphore->getMulOut(), semaphore->getWriteSwap() == WriteSwap::DONT_SWAP),
                    result, semaphore->getMulCondition());
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

static std::pair<Value, bool> toInputValue(
    Registers& registers, InputMultiplex mux, Address addressA, Address addressB, bool regBIsImmediate)
{
    switch(mux)
    {
    case InputMultiplex::ACC0:
        return registers.readRegister(REG_ACC0);
    case InputMultiplex::ACC1:
        return registers.readRegister(REG_ACC1);
    case InputMultiplex::ACC2:
        return registers.readRegister(REG_ACC2);
    case InputMultiplex::ACC3:
        return registers.readRegister(REG_ACC3);
    case InputMultiplex::ACC4:
        return registers.readRegister(REG_SFU_OUT);
    case InputMultiplex::ACC5:
        return registers.readRegister(REG_ACC5);
    case InputMultiplex::REGA:
        return registers.readRegister(Register{RegisterFile::PHYSICAL_A, addressA});
    case InputMultiplex::REGB:
        if(regBIsImmediate)
            return std::make_pair(Value(SmallImmediate(addressB),
                                      (SmallImmediate(addressB)).getFloatingValue() ? TYPE_FLOAT : TYPE_INT32),
                true);
        else
            return registers.readRegister(Register{RegisterFile::PHYSICAL_B, addressB});
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ALU input");
}

static std::pair<Value, bool> applyVectorRotation(std::pair<Value, bool>&& input, Signaling sig, InputMultiplex mux1,
    InputMultiplex mux2, Address regB, Registers& registers)
{
    if(!input.second)
        // if we stall, do not rotate
        return std::move(input);
    if(sig != SIGNAL_ALU_IMMEDIATE)
        // no rotation set
        return std::move(input);
    SmallImmediate offset(regB);
    if(!offset.isVectorRotation())
        return std::move(input);
    if(mux1 == InputMultiplex::REGB || mux2 == InputMultiplex::REGB)
        throw CompilationError(CompilationStep::GENERAL, "Cannot read vector rotation offset", input.first.to_string());

    if(input.first.hasLiteral() || (input.first.hasContainer() && input.first.container().isAllSame()))
        return std::move(input);

    unsigned char distance;
    if(offset == VECTOR_ROTATE_R5)
    {
        //"Mul output vector rotation is taken from accumulator r5, element 0, bits [3:0]"
        // - Broadcom Specification, page 30
        Value tmp = registers.readRegister(REG_ACC5).first;
        if(tmp.hasContainer())
            distance = 16 - static_cast<uint8_t>(tmp.container().elements.at(0).getLiteralValue()->unsignedInt() & 0xF);
        else
            distance = 16 - static_cast<uint8_t>(tmp.getLiteralValue()->unsignedInt() & 0xF);
    }
    else
        distance = 16 - offset.getRotationOffset().value();

    Value result(std::forward<Value>(input.first));

    std::rotate(result.container().elements.begin(), result.container().elements.begin() + distance,
        result.container().elements.end());

    PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 170, "vector rotations", 1);
    return std::make_pair(result, true);
}

bool QPU::executeALU(const qpu_asm::ALUInstruction* aluInst)
{
    Value addIn0 = UNDEFINED_VALUE;
    Value addIn1 = UNDEFINED_VALUE;
    Value mulIn0 = UNDEFINED_VALUE;
    Value mulIn1 = UNDEFINED_VALUE;

    const OpCode addCode = OpCode::toOpCode(aluInst->getAddition(), false);
    const OpCode mulCode = OpCode::toOpCode(aluInst->getMultiplication(), true);

    // need to read both input before writing any registers
    if(aluInst->getAddCondition() != COND_NEVER && aluInst->getAddition() != OP_NOP.opAdd)
    {
        bool addIn0NotStall = true;
        bool addIn1NotStall = true;
        std::tie(addIn0, addIn0NotStall) = toInputValue(registers, aluInst->getAddMultiplexA(), aluInst->getInputA(),
            aluInst->getInputB(), aluInst->getSig() == SIGNAL_ALU_IMMEDIATE);
        if(addCode.numOperands > 1)
            std::tie(addIn1, addIn1NotStall) = toInputValue(registers, aluInst->getAddMultiplexB(),
                aluInst->getInputA(), aluInst->getInputB(), aluInst->getSig() == SIGNAL_ALU_IMMEDIATE);

        if(!addIn0NotStall || !addIn1NotStall)
        {
            // we stall on input, so do not calculate anything
            ++instrumentation[aluInst].numStalls;
            return false;
        }
    }

    if(aluInst->getMulCondition() != COND_NEVER && aluInst->getMultiplication() != OP_NOP.opMul)
    {
        bool mulIn0NotStall = true;
        bool mulIn1NotStall = true;

        std::tie(mulIn0, mulIn0NotStall) =
            applyVectorRotation(toInputValue(registers, aluInst->getMulMultiplexA(), aluInst->getInputA(),
                                    aluInst->getInputB(), aluInst->getSig() == SIGNAL_ALU_IMMEDIATE),
                aluInst->getSig(), aluInst->getMulMultiplexA(), aluInst->getMulMultiplexB(), aluInst->getInputB(),
                registers);
        if(mulCode.numOperands > 1)
            std::tie(mulIn1, mulIn1NotStall) =
                applyVectorRotation(toInputValue(registers, aluInst->getMulMultiplexB(), aluInst->getInputA(),
                                        aluInst->getInputB(), aluInst->getSig() == SIGNAL_ALU_IMMEDIATE),
                    aluInst->getSig(), aluInst->getMulMultiplexA(), aluInst->getMulMultiplexB(), aluInst->getInputB(),
                    registers);

        if(!mulIn0NotStall || !mulIn1NotStall)
        {
            // we stall on input, so do not calculate anything
            ++instrumentation[aluInst].numStalls;
            return false;
        }
    }

    if(aluInst->getAddCondition() != COND_NEVER && aluInst->getAddition() != OP_NOP.opAdd)
    {
        if(addIn0.hasContainer() && addIn0.container().isUndefined())
            addIn0 = UNDEFINED_VALUE;
        if(addIn1.hasContainer() && addIn1.container().isUndefined())
            addIn1 = UNDEFINED_VALUE;

        if(aluInst->getUnpack().hasEffect())
        {
            if(aluInst->getUnpack().isUnpackFromR4())
            {
                if(aluInst->getAddMultiplexA() == InputMultiplex::ACC4)
                    addIn0 = aluInst->getUnpack()(addIn0).value();
                if(aluInst->getAddMultiplexB() == InputMultiplex::ACC4)
                    addIn1 = aluInst->getUnpack()(addIn1).value();
            }
            else
            {
                if(aluInst->getAddMultiplexA() == InputMultiplex::REGA)
                    addIn0 = aluInst->getUnpack()(addIn0).value();
                if(aluInst->getAddMultiplexB() == InputMultiplex::REGA)
                    addIn1 = aluInst->getUnpack()(addIn1).value();
            }
        }
        //"bit-cast" to correct type for displaying and pack-modes
        if(addCode.acceptsFloat)
        {
            addIn0.type = TYPE_FLOAT.toVectorType(addIn0.type.getVectorWidth());
            addIn1.type = TYPE_FLOAT.toVectorType(addIn1.type.getVectorWidth());
        }
        else if(!(addCode == OP_OR && addIn0 == addIn1)) // move leaves original types
        {
            addIn0.type =
                addIn0.type.isFloatingType() ? TYPE_INT32.toVectorType(addIn0.type.getVectorWidth()) : addIn0.type;
            addIn1.type =
                addIn1.type.isFloatingType() ? TYPE_INT32.toVectorType(addIn1.type.getVectorWidth()) : addIn1.type;
        }

        auto tmp = addCode(addIn0, addIn1);
        if(!tmp.first)
            logging::error() << "Failed to emulate ALU operation: " << addCode.name << " with "
                             << addIn0.to_string(false, true) << " and " << addIn1.to_string(false, true)
                             << logging::endl;
        Value result = tmp.first.value();
        if(aluInst->getWriteSwap() == WriteSwap::DONT_SWAP && aluInst->getPack().hasEffect())
        {
            PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
            if(aluInst->getPack().supportsMulALU())
                throw CompilationError(CompilationStep::GENERAL, "Cannot apply mul pack mode on add result!");
            result = aluInst->getPack()(result, tmp.second).value();
        }

        writeConditional(toRegister(aluInst->getAddOut(), aluInst->getWriteSwap() == WriteSwap::SWAP), result,
            aluInst->getAddCondition(), aluInst, nullptr);
        if(aluInst->getSetFlag() == SetFlag::SET_FLAGS)
            setFlags(result, aluInst->getAddCondition(), tmp.second);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 180, "add instructions", 1);
    }
    if(aluInst->getMulCondition() != COND_NEVER && aluInst->getMultiplication() != OP_NOP.opMul)
    {
        if(mulIn0.hasContainer() && mulIn0.container().isUndefined())
            mulIn0 = UNDEFINED_VALUE;
        if(mulIn1.hasContainer() && mulIn1.container().isUndefined())
            mulIn1 = UNDEFINED_VALUE;

        if(aluInst->getUnpack().hasEffect())
        {
            if(aluInst->getUnpack().isUnpackFromR4())
            {
                if(aluInst->getMulMultiplexA() == InputMultiplex::ACC4)
                    mulIn0 = aluInst->getUnpack()(mulIn0).value();
                if(aluInst->getMulMultiplexB() == InputMultiplex::ACC4)
                    mulIn1 = aluInst->getUnpack()(mulIn1).value();
            }
            else
            {
                if(aluInst->getMulMultiplexA() == InputMultiplex::REGA)
                    mulIn0 = aluInst->getUnpack()(mulIn0).value();
                if(aluInst->getMulMultiplexB() == InputMultiplex::REGA)
                    mulIn1 = aluInst->getUnpack()(mulIn1).value();
            }
        }

        //"bit-cast" to correct type for displaying and pack-modes
        if(mulCode.acceptsFloat)
        {
            mulIn0.type = TYPE_FLOAT.toVectorType(mulIn0.type.getVectorWidth());
            mulIn1.type = TYPE_FLOAT.toVectorType(mulIn1.type.getVectorWidth());
        }
        else if(!((mulCode == OP_V8MIN || mulCode == OP_V8MAX) && addIn0 == addIn1)) // move leaves original types
        {
            mulIn0.type =
                mulIn0.type.isFloatingType() ? TYPE_INT32.toVectorType(mulIn0.type.getVectorWidth()) : mulIn0.type;
            mulIn1.type =
                mulIn1.type.isFloatingType() ? TYPE_INT32.toVectorType(mulIn1.type.getVectorWidth()) : mulIn1.type;
        }

        auto tmp = mulCode(mulIn0, mulIn1);
        if(!tmp.first)
            logging::error() << "Failed to emulate ALU operation: " << mulCode.name << " with "
                             << mulIn0.to_string(false, true) << " and " << mulIn1.to_string(false, true)
                             << logging::endl;
        Value result = tmp.first.value();
        if(aluInst->getWriteSwap() == WriteSwap::SWAP && aluInst->getPack().hasEffect())
        {
            PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 210, "values packed", 1);
            if(!aluInst->getPack().supportsMulALU())
                throw CompilationError(CompilationStep::GENERAL, "Cannot apply add pack mode on mul result!");
            result = aluInst->getPack()(result, tmp.second).value();
        }

        // FIXME these might depend on flags of add ALU set in same instruction (which is wrong)
        writeConditional(toRegister(aluInst->getMulOut(), aluInst->getWriteSwap() == WriteSwap::DONT_SWAP), result,
            aluInst->getMulCondition(), nullptr, aluInst);
        if(aluInst->getSetFlag() == SetFlag::SET_FLAGS &&
            isFlagSetByMulALU(aluInst->getAddition(), aluInst->getMultiplication()))
            setFlags(result, aluInst->getMulCondition(), tmp.second);
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 190, "mul instructions", 1);
    }

    if(aluInst->getUnpack().hasEffect())
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 220, "values unpacked", 1);

    return true;
}

void QPU::writeConditional(Register dest, const Value& in, ConditionCode cond, const qpu_asm::ALUInstruction* addInst,
    const qpu_asm::ALUInstruction* mulInst)
{
    if(cond == COND_ALWAYS)
    {
        registers.writeRegister(dest, in, std::bitset<16>(0xFFFF));
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
    Value result(ContainerValue(NATIVE_VECTOR_SIZE), in.type);

    std::bitset<16> elementMask;

    for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
    {
        if((!in.hasContainer() || i < in.container().elements.size()))
        {
            if(flags[i].matchesCondition(cond))
                elementMask.set(i);
            Value element = in.hasContainer() ? in.container().elements[i] : in;
            element.type = element.type.toVectorType(1);
            result.container().elements.push_back(element);
        }
        else
            result.container().elements.push_back(UNDEFINED_VALUE);
    }

    registers.writeRegister(dest, result, elementMask);

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

    return true;
}

void QPU::setFlags(const Value& output, ConditionCode cond, const VectorFlags& newFlags)
{
    std::vector<std::string> parts;
    parts.reserve(flags.size());
    for(uint8_t i = 0; i < flags.size(); ++i)
    {
        if(flags[i].matchesCondition(cond))
        {
            // only update flags for elements we actually write (where we actually calculate a result)
            flags[i] = newFlags[i];
            // do not set overflow flag, it is only valid for the one instruction
            flags[i].overflow = FlagStatus::UNDEFINED;

            parts.push_back(flags[i].to_string());
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

    Word numQPUs = config.localSizes.at(0) * config.localSizes.at(1) * config.localSizes.at(2);
    Word numReruns = config.numGroups.at(0) * config.numGroups.at(1) * config.numGroups.at(2);
    res.reserve(numQPUs);

    std::array<Word, 3> groupIDs = {0, 0, 0};

    std::vector<Word> qpuUniforms;
    qpuUniforms.resize(uniformsUsed.countUniforms() + 1 /* re-run flag */ + parameter.size());

    for(uint8_t q = 0; q < numQPUs; ++q)
    {
        std::array<Word, 3> localIDs = {q % config.localSizes.at(0),
            (q / config.localSizes.at(0)) % config.localSizes.at(1),
            (q / config.localSizes.at(0)) / config.localSizes.at(1)};

        for(uint8_t g = 0; g < numReruns; ++g)
        {
            groupIDs = {g % config.numGroups.at(0), (g / config.numGroups.at(0)) % config.numGroups.at(1),
                (g / config.numGroups.at(0)) / config.numGroups.at(1)};

            std::size_t i = 0;
            if(uniformsUsed.getWorkDimensionsUsed())
                qpuUniforms[i++] = config.dimensions;
            if(uniformsUsed.getLocalSizesUsed())
                qpuUniforms[i++] =
                    (config.localSizes.at(2) << 16) | (config.localSizes.at(1) << 8) | config.localSizes.at(0);
            if(uniformsUsed.getLocalIDsUsed())
                qpuUniforms[i++] = (localIDs.at(2) << 16) | (localIDs.at(1) << 8) | localIDs.at(0);
            if(uniformsUsed.getNumGroupsXUsed())
                qpuUniforms[i++] = config.numGroups.at(0);
            if(uniformsUsed.getNumGroupsYUsed())
                qpuUniforms[i++] = config.numGroups.at(1);
            if(uniformsUsed.getNumGroupsZUsed())
                qpuUniforms[i++] = config.numGroups.at(2);
            if(uniformsUsed.getGroupIDXUsed())
                qpuUniforms[i++] = groupIDs.at(0);
            if(uniformsUsed.getGroupIDYUsed())
                qpuUniforms[i++] = groupIDs.at(1);
            if(uniformsUsed.getGroupIDZUsed())
                qpuUniforms[i++] = groupIDs.at(2);
            if(uniformsUsed.getGlobalOffsetXUsed())
                qpuUniforms[i++] = config.globalOffsets.at(0);
            if(uniformsUsed.getGlobalOffsetYUsed())
                qpuUniforms[i++] = config.globalOffsets.at(1);
            if(uniformsUsed.getGlobalOffsetZUsed())
                qpuUniforms[i++] = config.globalOffsets.at(2);
            if(uniformsUsed.getGlobalDataAddressUsed())
                qpuUniforms[i++] = globalData;
            for(auto param : parameter)
            {
                qpuUniforms[i++] = param;
            }
            qpuUniforms[i++] = (numReruns - 1) - g;

            memory.setUniforms(qpuUniforms, baseAddress);
            if(g == 0)
                res.push_back(baseAddress);

            baseAddress += static_cast<Word>(qpuUniforms.size() * sizeof(Word));
        }
    }

    return res;
}

static void emulateStep(
    std::vector<qpu_asm::Instruction>::const_iterator firstInstruction, ReferenceRetainingList<QPU>& qpus)
{
    auto it = qpus.begin();
    while(it != qpus.end())
    {
        try
        {
            const bool continueRunning = it->execute(firstInstruction);
            if(!continueRunning)
                // this QPU has finished
                it = qpus.erase(it);
            else
                ++it;
        }
        catch(const std::exception&)
        {
            logging::error() << "Emulation threw exception execution following instruction on QPU " << it->ID << ": "
                             << it->getCurrentInstruction(firstInstruction)->toHexString(true) << logging::endl;
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

    ReferenceRetainingList<QPU> qpus;
    uint8_t numQPU = 0;
    for(MemoryAddress uniformPointer : uniformAddresses)
    {
        qpus.emplace_back(numQPU, mutex, sfus.at(numQPU), vpm, semaphores, memory, uniformPointer, instrumentation);
        ++numQPU;
    }

    uint32_t cycle = 0;
    bool success = true;
    while(!qpus.empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Emulating cycle: " << cycle << logging::endl);
        emulateStep(firstInstruction, qpus);
        for(SFU& sfu : sfus)
            sfu.incrementCycle();
        vpm.incrementCycle();
        PROFILE_COUNTER(vc4c::profiler::COUNTER_EMULATOR + 250, "emulation cycles", 1);

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

static Memory fillMemory(const ReferenceRetainingList<Global>& globalData, const EmulationData& settings,
    MemoryAddress& uniformBaseAddressOut, MemoryAddress& globalDataAddressOut,
    std::vector<MemoryAddress>& parameterAddressesOut)
{
    auto globalDataSize =
        std::accumulate(globalData.begin(), globalData.end(), 0u, [](unsigned u, const Global& global) -> unsigned {
            return u + global.value.type.getPhysicalWidth() / (TYPE_INT32.getScalarBitCount() / 8);
        });
    auto size = globalDataSize + settings.calcParameterSize();
    // make sure to have enough space to align UNIFORMs
    while((size % 8) != 0)
        ++size;
    size += settings.calcNumWorkItems() * (16 + settings.parameter.size());
    Memory mem(size);

    MemoryAddress currentAddress = 0;
    globalDataAddressOut = currentAddress;

    for(const Global& global : globalData)
    {
        if(!global.value.type.getArrayType() || global.value.type.getElementType() != TYPE_INT32)
            throw CompilationError(
                CompilationStep::GENERAL, "Unhandled type of global data", global.value.type.to_string());
        for(const Value& word : global.value.container().elements)
        {
            *mem.getWordAddress(currentAddress) = word.getLiteralValue()->unsignedInt();
            currentAddress += TYPE_INT32.getScalarBitCount() / 8;
        }
    }

    parameterAddressesOut.reserve(settings.parameter.size());
    for(const auto& pair : settings.parameter)
    {
        tools::Word* addr = mem.getWordAddress(currentAddress);
        if(pair.second)
        {
            parameterAddressesOut.push_back(currentAddress);
            std::copy_n(pair.second->data(), pair.second->size(), addr);
            currentAddress += static_cast<MemoryAddress>(pair.second->size() * sizeof(uint32_t));
        }
        else
            // value is directly read as input
            parameterAddressesOut.push_back(pair.first);
    }

    // align UNIFORMs to boundary of memory dump
    if(currentAddress % (sizeof(tools::Word) * 8) != 0)
        currentAddress +=
            static_cast<MemoryAddress>((sizeof(tools::Word) * 8) - (currentAddress % (sizeof(tools::Word) * 8)));

    uniformBaseAddressOut = currentAddress;

    return mem;
}

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
        f << " " << std::hex << std::setfill('0') << std::setw(8)
          << memory.readWord(addr).getLiteralValue()->unsignedInt();
        if(addr % (sizeof(tools::Word) * 8) == (sizeof(tools::Word) * 7))
            f << std::endl;
        addr += sizeof(tools::Word);
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

EmulationResult tools::emulate(const EmulationData& data)
{
    qpu_asm::ModuleInfo module;
    ReferenceRetainingList<Global> globals;
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
    if(data.parameter.size() != kernelInfo->getParamCount())
        throw CompilationError(CompilationStep::GENERAL,
            "The number of parameters specified does not match the number of kernel arguments",
            std::to_string(static_cast<unsigned>(kernelInfo->getParamCount())));

    MemoryAddress uniformAddress;
    MemoryAddress globalDataAddress;
    std::vector<MemoryAddress> paramAddresses;
    Memory mem(fillMemory(globals, data, uniformAddress, globalDataAddress, paramAddresses));

    auto uniformAddresses =
        buildUniforms(mem, uniformAddress, paramAddresses, data.workGroup, globalDataAddress, kernelInfo->uniformsUsed);

    if(!data.memoryDump.empty())
        dumpMemory(mem, data.memoryDump, uniformAddress, true);

    InstrumentationResults instrumentation;
    bool status =
        emulate(instructions.begin() + (kernelInfo->getOffset() - module.kernelInfos.front().getOffset()).getValue(),
            mem, uniformAddresses, instrumentation, data.maxEmulationCycles);

    if(!data.memoryDump.empty())
        dumpMemory(mem, data.memoryDump, uniformAddress, false);

    EmulationResult result{data};
    result.executionSuccessful = status;

    result.results.reserve(data.parameter.size());
    for(std::size_t i = 0; i < data.parameter.size(); ++i)
    {
        if(!data.parameter[i].second)
            result.results.push_back(std::make_pair(data.parameter[i].first, Optional<std::vector<uint32_t>>{}));
        else
        {
            result.results.push_back(std::make_pair(paramAddresses[i], std::vector<uint32_t>{}));
            std::copy_n(mem.getWordAddress(paramAddresses[i]), data.parameter[i].second->size(),
                std::back_inserter(*result.results[i].second));
        }
    }

    // Map and dump instrumentation results
    std::unique_ptr<std::ofstream> dumpInstrumentation;
    if(!data.instrumentationDump.empty())
        dumpInstrumentation.reset(new std::ofstream(data.instrumentationDump));
    auto it = instructions.begin() + (kernelInfo->getOffset() - module.kernelInfos.front().getOffset()).getValue();
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
