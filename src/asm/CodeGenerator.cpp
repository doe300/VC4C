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
#include "../optimization/Optimizer.h"
#include "../optimization/Peephole.h"
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

CodeGenerator::CodeGenerator(const Module& module, const Configuration& config) :
    CodeGenerator(module, FIXUP_STEPS, config)
{
}

CodeGenerator::CodeGenerator(
    const Module& module, const std::vector<RegisterFixupStep>& customSteps, const Configuration& config) :
    config(config),
    module(module), fixupSteps(customSteps)
{
}

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
            if(it->hasDecoration(InstructionDecorations::MANDATORY_DELAY))
                // if the instruction was inserted at a mandatory delay, we need to insert a NOP which takes some space
                index += 8;
            it.safeErase();
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

static FixupResult runRegisterFixupStep(const RegisterFixupStep& step, Method& method, const Configuration& config,
    std::unique_ptr<GraphColoring>& coloredGraph)
{
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Running register fix-up step: " << step.name << "..." << logging::endl);
    PROFILE_START_DYNAMIC(step.name);
    auto result = step(method, config, *coloredGraph);
    PROFILE_END_DYNAMIC(step.name);
    return result;
}

const FastAccessList<DecoratedInstruction>& CodeGenerator::generateInstructions(Method& method)
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_BACKEND, "CodeGeneration (before)", method.countInstructions());
    instructionsLock.lock();
    auto& generatedInstructions = allInstructions[&method];
    instructionsLock.unlock();

    // check and fix possible errors with register-association
    std::unique_ptr<GraphColoring> coloredGraph;
    auto stepIt = fixupSteps.begin();
    FixupResult lastResult = FixupResult::FIXES_APPLIED_RECREATE_GRAPH;
    while(stepIt != fixupSteps.end())
    {
        if(!coloredGraph || lastResult == FixupResult::FIXES_APPLIED_RECREATE_GRAPH)
        {
            PROFILE_SCOPE(initializeLocalsUses);
            coloredGraph = std::make_unique<GraphColoring>(method, method.walkAllInstructions());
        }
        if(lastResult != FixupResult::NOTHING_FIXED)
        {
            // only recolor the graph if we did anything at all
            PROFILE_SCOPE(colorGraph);
            bool hasErrors = !coloredGraph->colorGraph();
            if(!hasErrors)
                // no more errors
                break;
        }
        lastResult = runRegisterFixupStep(*stepIt, method, config, coloredGraph);
        if(lastResult == FixupResult::ALL_FIXED)
            // all errors were fixed
            break;
        ++stepIt;
    }

    if(stepIt == fixupSteps.end())
    {
        logging::warn() << "Register conflict resolver has exceeded its maximum rounds, there might still be errors!"
                        << logging::endl;
        if(!coloredGraph || lastResult == FixupResult::FIXES_APPLIED_RECREATE_GRAPH)
        {
            PROFILE_SCOPE(initializeLocalsUses);
            coloredGraph = std::make_unique<GraphColoring>(method, method.walkAllInstructions());
        }
        PROFILE_START(colorGraph);
        auto hasError = coloredGraph->colorGraph();
        // the call to #toRegisterMap() below will fail anyway and has better error information
        (void) hasError;
        PROFILE_END(colorGraph);
    }

    // map to registers
    PROFILE_START(toRegisterMap);
    auto registerMapping = coloredGraph->toRegisterMap();
    PROFILE_END(toRegisterMap);

    // run some peephole-optimizations
    if(optimizations::Optimizer::isEnabled(optimizations::PASS_PEEPHOLE_REMOVE, config))
        optimizations::removeObsoleteInstructions(module, method, config, registerMapping);
    if(optimizations::Optimizer::isEnabled(optimizations::PASS_PEEPHOLE_COMBINE, config))
        optimizations::combineRegisterMappedOperations(module, method, config, registerMapping);

    // create label-map + remove labels
    const auto labelMap = mapLabels(method);

    // IMPORTANT: DO NOT OPTIMIZE, RE-ORDER, COMBINE, INSERT OR REMOVE ANY INSTRUCTION AFTER THIS POINT!!!
    // otherwise, labels/branches will be wrong

    CPPLOG_LAZY(logging::Level::DEBUG, log << "-----" << logging::endl);
    std::size_t index = 0;

    std::string s = "kernel " + method.name;

    generatedInstructions.reserve(method.countInstructions());
    for(const auto& bb : method)
    {
        if(bb.empty())
        {
            // show label comment for empty block with label comment for next block
            s.append(s.empty() ? "" : ",").append(bb.to_string());
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

    PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_BACKEND, "CodeGeneration (after)", generatedInstructions.size());
    return generatedInstructions;
}

std::size_t CodeGenerator::writeOutput(std::ostream& stream)
{
    ModuleHeader moduleHeader;

    std::size_t maxStackSize = 0;
    for(const auto& m : module)
        maxStackSize = std::max(maxStackSize, m->calculateStackSize() * m->metaData.getMaximumInstancesCount());
    if(maxStackSize / sizeof(uint64_t) > std::numeric_limits<uint16_t>::max() || maxStackSize % sizeof(uint64_t) != 0)
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Stack-frame has unsupported size of", std::to_string(maxStackSize));
    moduleHeader.setStackFrameSize(maxStackSize / sizeof(uint64_t));

    std::size_t numBytes = 0;
    // initial offset is zero
    std::size_t offset = 0;
    if(config.writeKernelInfo)
    {
        moduleHeader.kernels.reserve(allInstructions.size());
        // generate kernel headers
        for(const auto& pair : allInstructions)
        {
            moduleHeader.addKernel(createKernelHeader(*pair.first, offset, pair.second.size()));
            offset += pair.second.size();
        }
        // add global offset (size of  header)
        std::ostringstream dummyStream;
        offset = writeModule(dummyStream, moduleHeader, config.outputMode, module.globalData, Byte(maxStackSize));

        for(auto& kernel : moduleHeader.kernels)
            kernel.setOffset(kernel.getOffset() + offset);
    }
    // prepend module header to output
    // also write, if writeKernelHeader is not set, since global-data is written in here too
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Writing module header..." << logging::endl);
    numBytes +=
        writeModule(stream, moduleHeader, config.outputMode, module.globalData, Byte(maxStackSize)) * sizeof(uint64_t);

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
