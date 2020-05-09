/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CodeGenerator.h"

#if defined(VERIFIER_HEADER)
#include VERIFIER_HEADER
#endif

#include "../InstructionWalker.h"
#include "../Module.h"
#include "../Profiler.h"
#include "GraphColoring.h"
#include "KernelInfo.h"
#include "RegisterFixes.h"
#include "log.h"

#include <cassert>
#include <climits>
#include <map>
#include <sstream>

using namespace vc4c;
using namespace vc4c::qpu_asm;
using namespace vc4c::intermediate;

CodeGenerator::CodeGenerator(const Module& module, const Configuration& config) : config(config), module(module) {}

static FastMap<const Local*, std::size_t> mapLabels(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "-----" << logging::endl);
    FastMap<const Local*, std::size_t> labelsMap;
    labelsMap.reserve(method.size());
    // index is in bytes, so an increment of 1 instructions, increments by 8 bytes
    std::size_t index = 0;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(BranchLabel* label = it.isEndOfBlock() ? nullptr : it.get<BranchLabel>())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Mapping label '" << label->getLabel()->name << "' to byte-position " << index << logging::endl);
            labelsMap[label->getLabel()] = index;

            it.nextInMethod();
        }
        else if(!it.isEndOfBlock() && it.has() && !it->mapsToASMInstruction())
        {
            // an instruction which has no equivalent in machine code -> drop
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Dropping instruction not mapped to assembler: " << it->to_string() << logging::endl);
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
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Mapped " << labelsMap.size() << " labels to positions" << logging::endl);

    return labelsMap;
}

static FixupResult runRegisterFixupStep(const std::pair<std::string, RegisterFixupStep>& step, Method& method,
    const Configuration& config, std::unique_ptr<GraphColoring>& coloredGraph)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Running register fix-up step: " << step.first << "..." << logging::endl);
    PROFILE_START(runRegisterFixupStep);
    auto result = step.second(method, config, *coloredGraph);
    PROFILE_END(runRegisterFixupStep);
    return result;
}

const FastAccessList<DecoratedInstruction>& CodeGenerator::generateInstructions(Method& method)
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
    std::unique_ptr<GraphColoring> coloredGraph;
    std::size_t round = 0;
    auto stepIt = FIXUP_STEPS.begin();
    FixupResult lastResult = FixupResult::FIXES_APPLIED_RECREATE_GRAPH;
    while(round < config.additionalOptions.registerResolverMaxRounds && stepIt != FIXUP_STEPS.end())
    {
        if(!coloredGraph || lastResult == FixupResult::FIXES_APPLIED_RECREATE_GRAPH)
        {
            PROFILE_START(initializeLocalsUses);
            coloredGraph = std::make_unique<GraphColoring>(method, method.walkAllInstructions());
            PROFILE_END(initializeLocalsUses);
        }
        if(lastResult != FixupResult::NOTHING_FIXED)
        {
            // only recolor the graph if we did anything at all
            PROFILE_START(colorGraph);
            bool hasErrors = !coloredGraph->colorGraph();
            PROFILE_END(colorGraph);
            if(!hasErrors)
                // no more errors
                break;
        }
        lastResult = runRegisterFixupStep(*stepIt, method, config, coloredGraph);
        if(lastResult == FixupResult::ALL_FIXED)
            // all errors were fixed
            break;
        ++round;
        ++stepIt;
    }

    if(round >= config.additionalOptions.registerResolverMaxRounds || stepIt == FIXUP_STEPS.end())
    {
        logging::warn() << "Register conflict resolver has exceeded its maximum rounds, there might still be errors!"
                        << logging::endl;
        if(!coloredGraph || lastResult == FixupResult::FIXES_APPLIED_RECREATE_GRAPH)
        {
            PROFILE_START(initializeLocalsUses);
            coloredGraph = std::make_unique<GraphColoring>(method, method.walkAllInstructions());
            PROFILE_END(initializeLocalsUses);
        }
        PROFILE_START(colorGraph);
        auto hasError = coloredGraph->colorGraph();
        // the call to #toRegisterMap() below will fail anyway and has better error information
        (void) hasError;
        PROFILE_END(colorGraph);
    }

    // create label-map + remove labels
    const auto labelMap = mapLabels(method);

    // IMPORTANT: DO NOT OPTIMIZE, RE-ORDER, COMBINE, INSERT OR REMOVE ANY INSTRUCTION AFTER THIS POINT!!!
    // otherwise, labels/branches will be wrong

    // map to registers
    PROFILE_START(toRegisterMap);
    auto registerMapping = coloredGraph->toRegisterMap();
    PROFILE_END(toRegisterMap);

    CPPLOG_LAZY(logging::Level::DEBUG, log << "-----" << logging::endl);
    std::size_t index = 0;

    std::string s = "kernel " + method.name;

    generatedInstructions.reserve(method.countInstructions());
    for(const auto& bb : method)
    {
        if(bb.empty())
        {
            // show label comment for empty block with label comment for next block
            s = s.empty() ? bb.to_string() : (s + ",").append(bb.to_string());
            continue;
        }

        auto it = bb.begin();
        auto label = dynamic_cast<const intermediate::BranchLabel*>(it->get());
        assert(label != nullptr);
        ++it;

        auto instr = it->get();
        if(instr->mapsToASMInstruction())
        {
            DecoratedInstruction mapped(instr->convertToAsm(registerMapping, labelMap, index));
            mapped.previousComment += s.empty() ? label->to_string() : s + ", " + label->to_string();
            s = "";
            generatedInstructions.emplace_back(mapped);
            ++index;
        }
        ++it;

        while(it != bb.end())
        {
            auto instr = it->get();
            if(instr->mapsToASMInstruction())
            {
                DecoratedInstruction mapped(instr->convertToAsm(registerMapping, labelMap, index));
                generatedInstructions.emplace_back(mapped);
                ++index;
            }
            ++it;
        }
    }

    CPPLOG_LAZY(logging::Level::DEBUG, log << "-----" << logging::endl);
    index = 0;
    for(const auto& instr : generatedInstructions)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << std::hex << index << " " << instr.toHexString(true) << logging::endl);
        index += 8;
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generated " << std::dec << generatedInstructions.size() << " instructions!" << logging::endl);

    PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_BACKEND + 1000, "CodeGeneration (after)",
        generatedInstructions.size(), vc4c::profiler::COUNTER_BACKEND + 0);
    return generatedInstructions;
}

std::size_t CodeGenerator::writeOutput(std::ostream& stream)
{
    ModuleInfo moduleInfo;

    std::size_t maxStackSize = 0;
    for(const auto& m : module)
        maxStackSize = std::max(maxStackSize, m->calculateStackSize() * m->metaData.getWorkGroupSize());
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
        offset = moduleInfo.write(dummyStream, config.outputMode, module.globalData, Byte(maxStackSize));

        for(KernelInfo& info : moduleInfo.kernelInfos)
            info.setOffset(info.getOffset() + Word(offset));
    }
    // prepend module header to output
    // also write, if writeKernelInfo is not set, since global-data is written in here too
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Writing module header..." << logging::endl);
    numBytes += moduleInfo.write(stream, config.outputMode, module.globalData, Byte(maxStackSize)) * sizeof(uint64_t);

    for(const auto& pair : allInstructions)
    {
        switch(config.outputMode)
        {
        case OutputMode::ASSEMBLER:
            for(const auto& instr : pair.second)
            {
                stream << instr.toASMString() << std::endl;
                numBytes += 0; // doesn't matter here, since the number of bytes is unused for assembler output
            }
            break;
        case OutputMode::BINARY:
            for(const auto& instr : pair.second)
            {
                const uint64_t binary = instr.toBinaryCode();
                stream.write(reinterpret_cast<const char*>(&binary), 8);
                numBytes += 8;
            }
            break;
        case OutputMode::HEX:
            for(const auto& instr : pair.second)
            {
                stream << instr.toHexString(true) << std::endl;
                numBytes += 8; // doesn't matter here, since the number of bytes is unused for hexadecimal output
            }
        }
    }
    stream.flush();
    return numBytes;
}

// register/instruction mapping
void CodeGenerator::toMachineCode(Method& kernel)
{
    kernel.cleanLocals();
    const auto& instructions = generateInstructions(kernel);
#ifdef VERIFIER_HEADER
    LCOV_EXCL_START
    std::vector<uint64_t> hexData;
    hexData.reserve(instructions.size());
    for(const auto& instr : instructions)
    {
        hexData.push_back(instr.toBinaryCode());
    }

    Validator v;
    v.OnMessage = [&instructions, this](const Message& msg) -> void {
        const auto& validatorMessage = dynamic_cast<const Validator::Message&>(msg);
        if(validatorMessage.Loc >= 0)
        {
            auto it = instructions.begin();
            std::advance(it, validatorMessage.Loc);
            logging::error() << "Validation-error '" << validatorMessage.Text << "' in: " << it->toASMString()
                             << logging::endl;
            if(validatorMessage.RefLoc >= 0)
            {
                it = instructions.begin();
                std::advance(it, validatorMessage.RefLoc);
                logging::error() << "With reference to instruction: " << it->toASMString() << logging::endl;
            }
        }
        if(config.stopWhenVerificationFailed)
            throw CompilationError(CompilationStep::VERIFIER, "vc4asm verification error", msg.toString());
    };
    v.Instructions = &hexData;
    CPPLOG_LAZY(logging::Level::INFO, log << "Validation-output: " << logging::endl);
    v.Validate();
    fflush(stderr);
    LCOV_EXCL_STOP
#endif
}
