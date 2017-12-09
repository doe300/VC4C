/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CodeGenerator.h"

#include "../InstructionWalker.h"
#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "../Profiler.h"
#include "GraphColoring.h"
#include "KernelInfo.h"
#include "log.h"

#include <climits>
#include <map>
#include <sstream>

using namespace vc4c;
using namespace vc4c::qpu_asm;
using namespace vc4c::intermediate;

CodeGenerator::CodeGenerator(const Module& module, const Configuration& config) : config(config), module(module)
{
}

static InstructionWalker loadVectorParameter(const Parameter& param, Method& method, InstructionWalker it)
{
	//we need to load a UNIFORM per vector element into the particular vector element
	for(uint8_t i = 0; i < param.type.num; ++i)
	{
		//the first write to the parameter needs to unconditional, so the register allocator can find it
		if(i > 0)
		{
			it.emplace( new Operation(OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(SmallImmediate(i), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
			it.nextInBlock();
		}
		if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
		{
			it = insertSignExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false, i == 0 ? COND_ALWAYS : COND_ZERO_SET);
		}
		else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
		{
			it = insertZeroExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false, i == 0 ? COND_ALWAYS : COND_ZERO_SET);
		}
		else
		{
			it.emplace(new MoveOperation(param.createReference(), UNIFORM_REGISTER, i == 0 ? COND_ALWAYS : COND_ZERO_SET));
			it.nextInBlock();
		}
		//TODo improve performance by first putting together the vector, then zero/sign extending all elements?
	}
	return it;
}

static void generateStartSegment(Method& method)
{
    auto it = method.walkAllInstructions();
    if(!it.has<BranchLabel>() || BasicBlock::DEFAULT_BLOCK.compare(it.get<BranchLabel>()->getLabel()->name) != 0)
    {
    	it = method.emplaceLabel(it, new BranchLabel(*method.findOrCreateLocal(TYPE_LABEL, BasicBlock::DEFAULT_BLOCK)));
    }
	it.nextInBlock();
    
    /*
     * The first UNIFORMs are reserved for relaying information about the work-item and work-group
     * - work_dim: number of dimensions
     * - global_size: global number of work-items per dimension
     * - global_id: global id of this work-item per dimension
     * - local_size: local number of work-items in its work-group per dimension
     * - local_id: local id of this work-item within its work-group
     * - num_groups: global number of work-groups per dimension
     * - group_id: id of this work-group
     * - global_offset: global initial offset per dimension
     * - address of global data / to load the global data from
     * 
     */
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::WORK_DIMENSIONS)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::LOCAL_SIZES)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::LOCAL_IDS)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::NUM_GROUPS_X)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::NUM_GROUPS_Y)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
	it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::NUM_GROUPS_Z)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GROUP_ID_X)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GROUP_ID_Y)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
	it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GROUP_ID_Z)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_OFFSET_X)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_OFFSET_Y)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
	it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_OFFSET_Z)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
    it.emplace(new MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference(), UNIFORM_REGISTER));
    it.nextInBlock();
    
    //load arguments to locals (via reading from uniform)
    for(const Parameter& param : method.parameters)
    {
        //do the loading
    	//we need special treatment for non-scalar parameter (e.g. vectors), since they can't be read with just 1 UNIFORM
    	if(!param.type.isPointerType() && param.type.num != 1)
    	{
    		it = loadVectorParameter(param, method, it);
    	}
    	else if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
        {
            it = insertSignExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false);
        }
        else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
        {
            it = insertZeroExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false);
        }
        else
        {
            it.emplace(new MoveOperation(param.createReference(), UNIFORM_REGISTER));
            it.nextInBlock();
        }
    }

//    //write initial values to locals
//    for(const Local& local : method.readLocals())
//    {
//        if(local.value.hasType(ValueType::LITERAL))
//        {
//            logging::debug() << "Initalizing local " << local.name << " with value " << local.value.to_string() << logging::endl;
//            it.emplace(new LoadImmediate(Value(LocalRef{local.name}, local.value.type), local.value.literal));
//            ++it;
//        }
//    }
}

static void generateStopSegment(Method& method)
{
    //write interrupt for host
    //write QPU number finished (value must be NON-NULL, so we invert it -> the first 28 bits are always 1)
    method.appendToEnd(new Operation(OP_NOT, Value(REG_HOST_INTERRUPT, TYPE_INT8), Value(REG_QPU_NUMBER, TYPE_INT8)));
    IntermediateInstruction* nop = new Nop(DelayType::THREAD_END);
    //set signals to stop thread/program    
    nop->setSignaling(SIGNAL_END_PROGRAM);
    method.appendToEnd(nop);
    method.appendToEnd(new Nop(DelayType::THREAD_END));
    method.appendToEnd(new Nop(DelayType::THREAD_END));
}

static void extendBranches(Method& method)
{
    std::size_t num = 0;
    logging::debug() << "-----" << logging::endl;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
	{
    	Branch* branch = it.get<Branch>();
		if (branch != nullptr)
		{
			if(branch->hasConditionalExecution() || !branch->getCondition().hasLiteral(BOOL_TRUE.literal))
			{
				/*
				 * branch can only depend on scalar value
				 * -> set any not used vector-element (all except element 0) to a value where it doesn't influence the condition
				 *
				 * Using ELEMENT_NUMBER sets the vector-elements 1 to 15 to a non-zero value and 0 to either 0 (if condition was false) or 1 (if condition was true)
				 */
				//TODO can be skipped, if it is checked/guaranteed, that the last instruction setting flags is the boolean-selection for the given condition
				//but we need to check more than the last instructions, since there could be moves inserted by phi
				if(has_flag(branch->decoration, InstructionDecorations::BRANCH_ON_ALL_ELEMENTS))
					it.emplace(new Operation(OP_OR, NOP_REGISTER, branch->getCondition(), branch->getCondition(), COND_ALWAYS, SetFlag::SET_FLAGS));
				else
					it.emplace(new Operation(OP_OR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, branch->getCondition(), COND_ALWAYS, SetFlag::SET_FLAGS));
				it.nextInBlock();
			}
			++num;
			//go to next instruction
			it.nextInBlock();
			//insert 3 NOPs before
			it.emplace(new Nop(DelayType::BRANCH_DELAY));
			it.emplace(new Nop(DelayType::BRANCH_DELAY));
			it.emplace(new Nop(DelayType::BRANCH_DELAY));
		}
		it.nextInMethod();
	}
    logging::debug() << "Extended " << num << " branches" << logging::endl;
}

static FastMap<const Local*, std::size_t> mapLabels(Method& method)
{
    logging::debug() << "-----" << logging::endl;
    FastMap<const Local*, std::size_t> labelsMap;
    //index is in bytes, so an increment of 1 instructions, increments by 8 bytes
    std::size_t index = 0;
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
	{
    	BranchLabel* label = it.isEndOfBlock() ? nullptr : it.get<BranchLabel>();
		if (label != nullptr)
		{
			logging::debug() << "Mapping label '" << label->getLabel()->name << "' to byte-position " << index << logging::endl;
			labelsMap[label->getLabel()] = index;
			//we do not need the position of the label at all anymore
			it.erase();
		}
		else if(!it.isEndOfBlock() && it.has() && !it->mapsToASMInstruction())
		{
			//an instruction which has no equivalent in machine code -> drop
			it.erase();
		}
		else
		{
			index += 8;
			it.nextInMethod();
		}
		if(it.isEndOfBlock() && !it.isEndOfMethod())
			//this handles empty basic blocks, so the index is not incremented in the next iteration
			it.nextInMethod();
	}
    logging::debug() << "Mapped " << labelsMap.size() << " labels to positions" << logging::endl;

    return labelsMap;
}

const FastModificationList<std::unique_ptr<qpu_asm::Instruction>>& CodeGenerator::generateInstructions(Method& method)
{
	PROFILE_COUNTER(100000, "CodeGeneration (before)", method.countInstructions());
#ifdef MULTI_THREADED
	instructionsLock.lock();
#endif
    auto& generatedInstructions = allInstructions[&method];
#ifdef MULTI_THREADED
    instructionsLock.unlock();
#endif
    //prepend start segment
    generateStartSegment(method);
    //append end segment
    generateStopSegment(method);

    //expand branches (add 3 NOPs)
    extendBranches(method);

    //check and fix possible errors with register-association
    PROFILE_START(initializeLocalsUses);
	GraphColoring coloring(method, method.walkAllInstructions());
	PROFILE_END(initializeLocalsUses);
	PROFILE_START(colorGraph);
	std::size_t round = 0;
	while(round < REGISTER_RESOLVER_MAX_ROUNDS && !coloring.colorGraph())
	{
		if(coloring.fixErrors())
			break;
		++round;
	}
	if(round >= REGISTER_RESOLVER_MAX_ROUNDS)
	{
		logging::warn() << "Register conflict resolver has exceeded its maximum rounds, there might still be errors!" << logging::endl;
	}
	PROFILE_END(colorGraph);
    
    //create label-map + remove labels
    const auto labelMap = mapLabels(method);

    //IMPORTANT: DO NOT OPTIMIZE, RE-ORDER, COMBINE, INSERT OR REMOVE ANY INSTRUCTION AFTER THIS POINT!!!
    //otherwise, labels/branches will be wrong

    //map to registers
    PROFILE_START(toRegisterMap);
	PROFILE_START(toRegisterMapGraph);
	auto registerMapping = coloring.toRegisterMap();
	PROFILE_END(toRegisterMapGraph);
	PROFILE_END(toRegisterMap);

    logging::debug() << "-----" << logging::endl;
    std::size_t index = 0;
    method.forAllInstructions([&generatedInstructions, &index, &registerMapping, &labelMap](const IntermediateInstruction* instr) -> bool
	{
    	Instruction* mapped = instr->convertToAsm(registerMapping, labelMap, index);
		if (mapped != nullptr) {
			generatedInstructions.emplace_back(mapped);
		}
		++index;
		return true;
	});

    logging::debug() << "-----" << logging::endl;
    index = 0;
    for (const std::unique_ptr<Instruction>& instr : generatedInstructions) {
        logging::debug() << std::hex << index << ' ' << instr->toHexString(true) << logging::endl;
        index += 8;
    }
    logging::debug() << "Generated " << std::dec << generatedInstructions.size() << " instructions!" << logging::endl;

    PROFILE_COUNTER_WITH_PREV(1001000, "CodeGeneration (after)", generatedInstructions.size(), 100000);
    return generatedInstructions;
}

std::size_t CodeGenerator::writeOutput(std::ostream& stream)
{
	ModuleInfo moduleInfo;

	std::size_t maxStackSize = 0;
	for(const auto& m : module.methods)
		maxStackSize = std::max(maxStackSize, m->calculateStackSize());
	if(maxStackSize / sizeof(uint64_t) > std::numeric_limits<uint16_t>::max() || maxStackSize % sizeof(uint64_t) != 0)
		throw CompilationError(CompilationStep::CODE_GENERATION, "Stack-frame has unsupported size of", std::to_string(maxStackSize));
	moduleInfo.setStackFrameSize(maxStackSize / sizeof(uint64_t));


    std::size_t numBytes = 0;
    //initial offset is zero
    std::size_t offset = 0;
    if(config.writeKernelInfo)
    {
        moduleInfo.kernelInfos.reserve(allInstructions.size());
        //generate kernel-infos
        for(const auto& pair : allInstructions)
        {
        	moduleInfo.addKernelInfo(getKernelInfos(*pair.first, offset, pair.second.size()));
            offset += pair.second.size();
        }
        //add global offset (size of  header)
        std::ostringstream dummyStream;
        offset = moduleInfo.write(dummyStream, config.outputMode, module.globalData);

        for(KernelInfo& info : moduleInfo.kernelInfos)
        {
        	if(info.getOffset() + offset > std::numeric_limits<uint16_t>::max())
				throw CompilationError(CompilationStep::CODE_GENERATION, "Kernel-function has unsupported offset of", std::to_string(info.getOffset() + offset));
            info.setOffset(static_cast<uint16_t>(info.getOffset() + offset));
        }
    }
    //prepend module header to output
    //also write, if writeKernelInfo is not set, since global-data is written in here too
	logging::debug() << "Writing module header..." << logging::endl;
	numBytes += moduleInfo.write(stream, config.outputMode, module.globalData) * sizeof(uint64_t);

    for(const auto& pair : allInstructions)
    {
        switch (config.outputMode) {
        case OutputMode::ASSEMBLER:
            for (const std::unique_ptr<Instruction>& instr : pair.second) {
                stream << instr->toASMString() << std::endl;
                numBytes += 0; //doesn't matter here, since the number of bytes is unused for assembler output
            }
            break;
        case OutputMode::BINARY:
            for (const std::unique_ptr<Instruction>& instr : pair.second) {
                const uint64_t binary = instr->toBinaryCode();
                stream.write(reinterpret_cast<const char*>(&binary), 8);
                numBytes += 8;
            }
            break;
        case OutputMode::HEX:
            for (const std::unique_ptr<Instruction>& instr : pair.second) {
                stream << instr->toHexString(true) << std::endl;
                numBytes += 8; //doesn't matter here, since the number of bytes is unused for hexadecimal output
            }
        }
    }
    stream.flush();
    return numBytes;
}
