/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CodeGenerator.h"

#include "../InstructionWalker.h"
#include "../Module.h"
#include "../Profiler.h"
#include "GraphColoring.h"
#include "KernelInfo.h"
#include "log.h"

#include <assert.h>
#include <climits>
#include <map>
#include <sstream>

using namespace vc4c;
using namespace vc4c::qpu_asm;
using namespace vc4c::intermediate;

CodeGenerator::CodeGenerator(const Module& module, const Configuration& config) : config(config), module(module) {}

static FastMap<const Local*, std::size_t> mapLabels(Method& method)
{
    logging::debug() << "-----" << logging::endl;
    FastMap<const Local*, std::size_t> labelsMap;
    // index is in bytes, so an increment of 1 instructions, increments by 8 bytes
    std::size_t index = 0;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        BranchLabel* label = it.isEndOfBlock() ? nullptr : it.get<BranchLabel>();
        if(label != nullptr)
        {
            logging::debug() << "Mapping label '" << label->getLabel()->name << "' to byte-position " << index
                             << logging::endl;
            labelsMap[label->getLabel()] = index;

            it.nextInMethod();
        }
        else if(!it.isEndOfBlock() && it.has() && !it->mapsToASMInstruction())
        {
            // an instruction which has no equivalent in machine code -> drop
            it.erase();
        }
        else
        {
            index += 8;
            it.nextInMethod();
        }
        if(it.isEndOfBlock() && !it.isEndOfMethod())
            // this handles empty basic blocks, so the index is not incremented in the next iteration
            it.nextInMethod();
    }
    logging::debug() << "Mapped " << labelsMap.size() << " labels to positions" << logging::endl;

    return labelsMap;
}

const FastModificationList<std::unique_ptr<qpu_asm::Instruction>>& CodeGenerator::generateInstructions(Method& method)
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND + 0, "CodeGeneration (before)", method.countInstructions());
#ifdef MULTI_THREADED
    instructionsLock.lock();
#endif
    auto& generatedInstructions = allInstructions[&method];
#ifdef MULTI_THREADED
    instructionsLock.unlock();
#endif

    // check and fix possible errors with register-association
    PROFILE_START(initializeLocalsUses);
    GraphColoring coloring(method, method.walkAllInstructions());
    PROFILE_END(initializeLocalsUses);
    PROFILE_START(colorGraph);
    std::size_t round = 0;
    while(round < config.additionalOptions.registerResolverMaxRounds && !coloring.colorGraph())
    {
        if(coloring.fixErrors())
            break;
        ++round;
    }
    if(round >= config.additionalOptions.registerResolverMaxRounds)
    {
        logging::warn() << "Register conflict resolver has exceeded its maximum rounds, there might still be errors!"
                        << logging::endl;
    }
    PROFILE_END(colorGraph);

    // create label-map + remove labels
    const auto labelMap = mapLabels(method);

    // IMPORTANT: DO NOT OPTIMIZE, RE-ORDER, COMBINE, INSERT OR REMOVE ANY INSTRUCTION AFTER THIS POINT!!!
    // otherwise, labels/branches will be wrong

    // map to registers
    PROFILE_START(toRegisterMap);
    PROFILE_START(toRegisterMapGraph);
    auto registerMapping = coloring.toRegisterMap();
    PROFILE_END(toRegisterMapGraph);
    PROFILE_END(toRegisterMap);

    logging::debug() << "-----" << logging::endl;
    std::size_t index = 0;

    std::string s = "";

    for(auto bb_it = method.begin(); bb_it != method.end(); ++bb_it)
    {
        auto it = bb_it->begin();
        if(it.getBasicBlock()->empty())
        {
            s = s.empty() ? bb_it->getLabel()->to_string() : s + "," + bb_it->getLabel()->to_string();
            continue;
        }

        auto label = dynamic_cast<intermediate::BranchLabel*>(it.get());
        assert(label != nullptr);
        it.nextInBlock();

        auto instr = it.get();
        if(instr->mapsToASMInstruction())
        {
            Instruction* mapped = instr->convertToAsm(registerMapping, labelMap, index);
            if(mapped != nullptr)
            {
                mapped->previousComment += s.empty() ? label->to_string() : s + ", " + label->to_string();
                s = "";
                generatedInstructions.emplace_back(mapped);
            }
            ++index;
        }

        it.nextInBlock();

        while(!it.isEndOfBlock())
        {
            auto instr = it.get();
            if(instr->mapsToASMInstruction())
            {
                Instruction* mapped = instr->convertToAsm(registerMapping, labelMap, index);
                if(mapped != nullptr)
                {
                    generatedInstructions.emplace_back(mapped);
                }
                ++index;
            }

            it.nextInBlock();
        }
    }

    logging::debug() << "-----" << logging::endl;
    index = 0;
    for(const auto& instr : generatedInstructions)
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << std::hex << index << " " << instr->toHexString(true) << logging::endl);
        index += 8;
    }
    logging::debug() << "Generated " << std::dec << generatedInstructions.size() << " instructions!" << logging::endl;

    PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_BACKEND + 1000, "CodeGeneration (after)",
        generatedInstructions.size(), vc4c::profiler::COUNTER_BACKEND + 0);
    return generatedInstructions;
}

std::size_t CodeGenerator::writeOutput(std::ostream& stream)
{
    ModuleInfo moduleInfo;

    std::size_t maxStackSize = 0;
    for(const auto& m : module)
        maxStackSize = std::max(maxStackSize, m->calculateStackSize());
    if(maxStackSize / sizeof(uint64_t) > std::numeric_limits<uint16_t>::max() || maxStackSize % sizeof(uint64_t) != 0)
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Stack-frame has unsupported size of", std::to_string(maxStackSize));
    moduleInfo.setStackFrameSize(Word(Byte(maxStackSize)));

    std::size_t numBytes = 0;
    // initial offset is zero
    std::size_t offset = 0;
    if(config.writeKernelInfo)
    {
        moduleInfo.kernelInfos.reserve(allInstructions.size());
        // generate kernel-infos
        for(const auto& pair : allInstructions)
        {
            moduleInfo.addKernelInfo(getKernelInfos(*pair.first, offset, pair.second.size()));
            offset += pair.second.size();
        }
        // add global offset (size of  header)
        std::ostringstream dummyStream;
        offset = moduleInfo.write(dummyStream, config.outputMode, module.globalData);

        for(KernelInfo& info : moduleInfo.kernelInfos)
            info.setOffset(info.getOffset() + Word(offset));
    }
    // prepend module header to output
    // also write, if writeKernelInfo is not set, since global-data is written in here too
    logging::debug() << "Writing module header..." << logging::endl;
    numBytes += moduleInfo.write(stream, config.outputMode, module.globalData) * sizeof(uint64_t);

    for(const auto& pair : allInstructions)
    {
        switch(config.outputMode)
        {
        case OutputMode::ASSEMBLER:
            for(const auto& instr : pair.second)
            {
                stream << instr->toASMString() << std::endl;
                numBytes += 0; // doesn't matter here, since the number of bytes is unused for assembler output
            }
            break;
        case OutputMode::BINARY:
            for(const auto& instr : pair.second)
            {
                const uint64_t binary = instr->toBinaryCode();
                stream.write(reinterpret_cast<const char*>(&binary), 8);
                numBytes += 8;
            }
            break;
        case OutputMode::HEX:
            for(const auto& instr : pair.second)
            {
                stream << instr->toHexString(true) << std::endl;
                numBytes += 8; // doesn't matter here, since the number of bytes is unused for hexadecimal output
            }
        }
    }
    stream.flush();
    return numBytes;
}
