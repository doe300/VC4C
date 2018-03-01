/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Module.h"

#include "InstructionWalker.h"
#include "Profiler.h"
#include "intermediate/IntermediateInstruction.h"
#include "log.h"
#include "periphery/VPM.h"

using namespace vc4c;

const std::string BasicBlock::DEFAULT_BLOCK("%start_of_function");
const std::string BasicBlock::LAST_BLOCK("%end_of_function");

const std::string Method::WORK_DIMENSIONS("%work_dim");
const std::string Method::LOCAL_SIZES("%local_sizes");
const std::string Method::LOCAL_IDS("%local_ids");
const std::string Method::NUM_GROUPS_X("%num_groups_x");
const std::string Method::NUM_GROUPS_Y("%num_groups_y");
const std::string Method::NUM_GROUPS_Z("%num_groups_z");
const std::string Method::GROUP_ID_X("%group_id_x");
const std::string Method::GROUP_ID_Y("%group_id_y");
const std::string Method::GROUP_ID_Z("%group_id_z");
const std::string Method::GLOBAL_OFFSET_X("%global_offset_x");
const std::string Method::GLOBAL_OFFSET_Y("%global_offset_y");
const std::string Method::GLOBAL_OFFSET_Z("%global_offset_z");
const std::string Method::GLOBAL_DATA_ADDRESS("%global_data_address");
const std::string Method::GROUP_LOOP_SIZE("%group_loop_size");

bool KernelMetaData::isWorkGroupSizeSet() const
{
	return std::any_of(workGroupSizes.begin(), workGroupSizes.end(), [](uint32_t u )-> bool {return u > 0;});
}

BasicBlock::BasicBlock(Method& method, intermediate::BranchLabel* label) : method(method)
{
	instructions.emplace_back(label);
}

bool BasicBlock::empty() const
{
	return instructions.empty() || (instructions.size() == 1 && dynamic_cast<intermediate::BranchLabel*>(instructions.front().get()) != nullptr);
}

InstructionWalker BasicBlock::begin()
{
	return InstructionWalker(this, instructions.begin());
}

InstructionWalker BasicBlock::end()
{
	return InstructionWalker(this, instructions.end());
}

std::size_t BasicBlock::size() const
{
	return instructions.size();
}

bool BasicBlock::isLocallyLimited(InstructionWalker curIt, const Local* locale, const std::size_t threshold) const
{
	auto remainingUsers = locale->getUsers();

	int32_t usageRangeLeft = static_cast<int32_t>(threshold);
	//check whether the local is written in the instruction before (and this)
	//this happens e.g. for comparisons
	if(!curIt.isStartOfBlock())
	{
		remainingUsers.erase(curIt.copy().previousInBlock().get());
	}
	while(usageRangeLeft >= 0 && !curIt.isEndOfBlock())
	{
		remainingUsers.erase(curIt.get());
		--usageRangeLeft;
		curIt.nextInBlock();
	}

	return remainingUsers.empty();
}

const intermediate::BranchLabel* BasicBlock::getLabel() const
{
	if(dynamic_cast<intermediate::BranchLabel*>(instructions.front().get()) == nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Basic block does not start with a label", instructions.front()->to_string());
	return dynamic_cast<intermediate::BranchLabel*>(instructions.front().get());
}

void BasicBlock::forSuccessiveBlocks(const std::function<void(BasicBlock&)>& consumer) const
{
	InstructionWalker it = const_cast<BasicBlock*>(this)->begin();
	while(!it.isEndOfBlock())
	{
		if(it.has<intermediate::Branch>())
		{
			BasicBlock* next = method.findBasicBlock(it.get<intermediate::Branch>()->getTarget());
			if(next != nullptr)
				consumer(*next);
		}
		it.nextInBlock();
		if(fallsThroughToNextBlock())
		{
			BasicBlock* next = method.getNextBlockAfter(this);
			if(next != nullptr)
				consumer(*next);
		}
	}
}

void BasicBlock::forPredecessors(const std::function<void(InstructionWalker)>& consumer) const
{
	//TODO some more efficient way of doing this??!
	const Local* label = getLabel()->getLabel();
	BasicBlock* prevBlock = nullptr;
	bool prevBlockFound = false;
	for(BasicBlock& bb : method)
	{
		for(auto it = bb.begin(); !it.isEndOfBlock(); it.nextInBlock())
		{
			intermediate::Branch* br = it.get<intermediate::Branch>();
			if(br != nullptr && br->getTarget() == label)
				consumer(it);
		}
		if(&bb == this)
			prevBlockFound = true;
		if(!prevBlockFound)
			prevBlock = &bb;
	}
	if(prevBlockFound && prevBlock != nullptr && prevBlock->fallsThroughToNextBlock())
	{
		consumer(prevBlock->end().previousInBlock());
	}
}

bool BasicBlock::fallsThroughToNextBlock() const
{
	//if the last instruction of a basic block is not an unconditional branch to another block, the control-flow falls through to the next block
	InstructionWalker it = const_cast<BasicBlock*>(this)->end();
	do
	{
		it.previousInBlock();
	}
	while(it.has<intermediate::Nop>());
	const intermediate::Branch* lastBranch = dynamic_cast<const intermediate::Branch*>(it.get());
	const intermediate::Branch* secondLastBranch = nullptr;
	if(!it.isStartOfBlock())
	{
		if(lastBranch != nullptr && !lastBranch->isUnconditional())
			//skip writing/setting of condition for conditional jump
			it.previousInBlock();
		do
		{
			if(it.isStartOfBlock())
				//special handling for blocks with only label and branches
				break;
			it.previousInBlock();
		}
		while(it.has<intermediate::Nop>());
		secondLastBranch = dynamic_cast<const intermediate::Branch*>(it.get());
	}
	if(lastBranch != nullptr && lastBranch->isUnconditional())
	{
		return false;
	}
	//for either-there-or-there branches, we need to check the two last instructions and see if they cover all conditions
	if(lastBranch != nullptr && secondLastBranch != nullptr && lastBranch->getCondition() == secondLastBranch->getCondition() && lastBranch->conditional.isInversionOf(secondLastBranch->conditional))
	{
		return false;
	}
	return true;
}

Optional<InstructionWalker> BasicBlock::findWalkerForInstruction(const intermediate::IntermediateInstruction* instr, InstructionWalker start) const
{
	while(!start.isStartOfBlock())
	{
		if(!start.isEndOfBlock() && start.get() == instr)
		{
			return start;
		}
		start.previousInBlock();
	}
	return {};
}

Optional<InstructionWalker> BasicBlock::findLastSettingOfFlags(const InstructionWalker start) const
{
	InstructionWalker it = start.copy().previousInBlock();
	while(!it.isStartOfBlock())
	{
		if(it->setFlags == SetFlag::SET_FLAGS)
			return it;
		const intermediate::CombinedOperation* comb = it.get<intermediate::CombinedOperation>();
		if(comb != nullptr)
		{
			if(comb->op1 && comb->op1->setFlags == SetFlag::SET_FLAGS)
				return it;
			if(comb->op2 && comb->op2->setFlags == SetFlag::SET_FLAGS)
				return it;
		}
	}
	return {};
}

bool BasicBlock::isStartOfMethod() const
{
	return &method.basicBlocks.front() == this;
}


void BasicBlock::dumpInstructions() const {
	logging::debug() << "Basic block ----" << logging::endl;

	std::for_each(instructions.begin(), instructions.end(),[](const std::unique_ptr<intermediate::IntermediateInstruction>& instr){
		if(instr)
			logging::debug() << instr->to_string() << logging::endl;
	});
	logging::debug() << "Block end ----" << logging::endl;
}

Method::Method(const Module& module) : isKernel(false), name(), returnType(TYPE_UNKNOWN), vpm(new periphery::VPM(module.compilationConfig.availableVPMSize)), module(module)
{

}

Method::~Method()
{
	//makes sure, instructions are removed before locals (so usages are all zero)
	basicBlocks.clear();
}

const Local* Method::findLocal(const std::string& name) const
{
	if(locals.find(name) != locals.end())
		return &locals.at(name);
	return nullptr;
}

const Parameter* Method::findParameter(const std::string& name) const
{
    for(const Parameter& param : parameters)
    {
        if(param.name.compare(name) == 0)
            return &param;
    }
    return nullptr;
}

const Global* Method::findGlobal(const std::string& name) const
{
	return module.findGlobal(name);
}

const StackAllocation* Method::findStackAllocation(const std::string& name) const
{
	for(const StackAllocation& s : stackAllocations)
	{
		if(s.name.compare(name) == 0)
			return &s;
	}
	return nullptr;
}

const Local* Method::findOrCreateLocal(const DataType& type, const std::string& name)
{
	const Local* loc = findLocal(name);
	if(loc == nullptr)
		loc = findParameter(name);
	if(loc == nullptr)
		loc = findGlobal(name);
	if(loc == nullptr)
		loc = findStackAllocation(name);
	if(loc != nullptr)
		return loc;
	auto it = locals.emplace(name, Local(type, name));
	return &(it.first->second);
}

static bool removeUsagesInBasicBlock(Method& method, BasicBlock& bb, const Local* locale, OrderedMap<const LocalUser*, LocalUse>& remainingUsers, int& usageRangeLeft)
{
	InstructionWalker it = bb.begin();
	while(usageRangeLeft >= 0 && !it.isEndOfMethod())
	{
		remainingUsers.erase(it.get());
		--usageRangeLeft;
		if(it.has<intermediate::Branch>())
		{
			BasicBlock* successor = method.findBasicBlock(it.get<intermediate::Branch>()->getTarget());
			if(successor != nullptr && removeUsagesInBasicBlock(method, *successor, locale, remainingUsers, usageRangeLeft))
				return true;
		}
		it.nextInMethod();
	}
	return remainingUsers.empty();
}

bool Method::isLocallyLimited(InstructionWalker curIt, const Local* locale, const std::size_t threshold) const
{
	auto remainingUsers = locale->getUsers();

	int32_t usageRangeLeft = static_cast<int32_t>(threshold);
	//check whether the local is written in the instruction before (and this)
	//this happens e.g. for comparisons
	if(!curIt.isStartOfBlock())
	{
		remainingUsers.erase(curIt.copy().previousInBlock().get());
	}
	while(usageRangeLeft >= 0 && !curIt.isEndOfMethod())
	{
		remainingUsers.erase(curIt.get());
		--usageRangeLeft;
		const intermediate::Branch* branch = curIt.get<intermediate::Branch>();
		if(branch != nullptr)
		{
			BasicBlock* successor = const_cast<Method*>(this)->findBasicBlock(branch->getTarget());
			if(successor != nullptr && removeUsagesInBasicBlock(*const_cast<Method*>(this), *successor, locale, remainingUsers, usageRangeLeft))
				return true;
			if(branch->isUnconditional())
				//this branch jumps away unconditionally and the successor does not have all remaining usages within the remaining range, so we abort
				return false;
		}
		curIt.nextInMethod();
	}

	return remainingUsers.empty();
}

static std::size_t tmpIndex = 0;

const Value Method::addNewLocal(const DataType& type, const std::string& prefix, const std::string& postfix)
{
	const std::string name = createLocalName(prefix, postfix);
	if(findLocal(name) != nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Local with this name already exists", findLocal(name)->to_string());
	auto it = locals.emplace(name, Local(type, name));
    return it.first->second.createReference();
}

std::string Method::createLocalName(const std::string& prefix, const std::string& postfix)
{
	//prefix, postfix empty -> "%tmp.tmpIndex"
	//prefix empty -> "%postfix"
	//postfix empty -> "prefix.tmpIndex"
	//none empty -> "prefix.postfix"
	std::string localName;
	if((prefix.empty() || prefix == "%") && postfix.empty())
	{
		localName = std::string("%tmp.") + std::to_string(tmpIndex++);
	}
	else if((prefix.empty() || prefix == "%"))
	{
		if(postfix.at(0) == '%')
			//to prevent "%%xyz"
			localName = postfix;
		else
			localName = std::string("%") + postfix;
	}
	else if(postfix.empty())
	{
		localName = (prefix + ".") + std::to_string(tmpIndex++);
	}
	else
	{
		localName = (prefix + ".") + postfix;
	}
	return localName;
}

InstructionWalker Method::walkAllInstructions()
{
	return basicBlocks.front().begin();
}

void Method::forAllInstructions(const std::function<void(const intermediate::IntermediateInstruction*)>& consumer) const
{
	for(const BasicBlock& bb : basicBlocks)
	{
		for(const auto& instr : bb.instructions)
		{
			consumer(instr.get());
		}
	}
}

std::size_t Method::countInstructions() const
{
	std::size_t count = 0;
	for(const BasicBlock& bb : basicBlocks)
	{
		count += bb.instructions.size();
	}
	return count;
}

std::size_t Method::cleanEmptyInstructions()
{
	//TODO required??
	std::size_t num = 0;
	auto it = walkAllInstructions();
	while(!it.isEndOfMethod())
	{
		if(it.get() == nullptr)
		{
			it.erase();
			++num;
		}
		else
			it.nextInMethod();
	}
	return num;
}

void Method::appendToEnd(intermediate::IntermediateInstruction* instr)
{
	if(dynamic_cast<intermediate::BranchLabel*>(instr) != nullptr)
		basicBlocks.emplace_back(*this, dynamic_cast<intermediate::BranchLabel*>(instr));
	else
	{
		checkAndCreateDefaultBasicBlock();
		basicBlocks.back().instructions.emplace_back(instr);
	}
}
InstructionWalker Method::appendToEnd()
{
	checkAndCreateDefaultBasicBlock();
	return basicBlocks.back().end();
}

const OrderedMap<std::string, Local>& Method::readLocals() const
{
	return locals;
}

void Method::cleanLocals()
{
	PROFILE_COUNTER(7, "Clean locals (before)", locals.size());
#ifdef DEBUG_MODE
	//check duplicate locals
	FastSet<std::string> localNames;
	for(const Global& g : module.globalData)
	{
		if(!localNames.emplace(g.name).second)
			throw CompilationError(CompilationStep::GENERAL, "Duplicate global", g.to_string());
	}
	for(const Parameter& p : parameters)
	{
		if(!localNames.emplace(p.name).second)
			throw CompilationError(CompilationStep::GENERAL, "Duplicate parameter for method", p.to_string());
	}
#endif
	auto it = locals.begin();
	std::size_t numCleaned = 0;
	while(it != locals.end())
	{
#ifdef DEBUG_MODE
		if(!localNames.emplace(it->first).second)
			throw CompilationError(CompilationStep::GENERAL, "Local is already defined for method", it->second.to_string());
#endif
		if((*it).second.getUsers().empty())
		{
			it = locals.erase(it);
			++numCleaned;
		}
		else
			++it;
	}
	logging::debug() << "Cleaned " << numCleaned << " unused locals from method " << name << logging::endl;
	PROFILE_COUNTER_WITH_PREV(8, "Clean locals (after)", locals.size(), 7);
}

void Method::dumpInstructions() const
{
	for(const BasicBlock& bb : basicBlocks)
	{
		bb.dumpInstructions();
	}
}

RandomModificationList<BasicBlock>& Method::getBasicBlocks()
{
	return basicBlocks;
}

BasicBlock* Method::findBasicBlock(const Local* label)
{
	for(BasicBlock& bb : basicBlocks)
	{
		if(bb.begin().has<intermediate::BranchLabel>() && bb.begin().get<intermediate::BranchLabel>()->getLabel() == label)
			return &bb;
	}
	return nullptr;
}

InstructionWalker Method::emplaceLabel(InstructionWalker it, intermediate::BranchLabel* label)
{
	auto blockIt = basicBlocks.begin();
	while(blockIt != basicBlocks.end())
	{
		if(&(*blockIt) == it.basicBlock)
			break;
		++blockIt;
	}
	if(blockIt == basicBlocks.end())
		throw CompilationError(CompilationStep::GENERAL, "Failed to find basic block for instruction iterator");
	//1. insert new basic block after the current (or in front of it, if we emplace at the start of the basic block)
	bool isStartOfBlock = blockIt->begin() == it;
	if(!isStartOfBlock)
		++blockIt;
	BasicBlock& newBlock = *basicBlocks.emplace(blockIt, *this, label);
	//2. move all instructions beginning with it (inclusive) to the new basic block
	while(!isStartOfBlock && !it.isEndOfBlock())
	{
		newBlock.instructions.emplace_back(it.release());
		it.erase();
	}
	//3. return the begin() of the new basic block
	return newBlock.begin();
}

void Method::calculateStackOffsets()
{
	//TODO this could be greatly improved, by re-using space for other stack-allocations, when their life-times don't intersect (similar to register allocation)
	const std::size_t stackBaseOffset = getStackBaseOffset();

	//Simple version: reserve extra space for every stack-allocation
	std::size_t currentOffset = 0;
	for(auto it = stackAllocations.begin(); it != stackAllocations.end(); ++it)
	{
		if((stackBaseOffset + currentOffset) % it->alignment != 0)
			currentOffset += it->alignment - ((stackBaseOffset + currentOffset) % it->alignment);
		const_cast<std::size_t&>(it->offset) = currentOffset;
		currentOffset += it->size;
	}
}

std::size_t Method::calculateStackSize() const
{
	if(stackAllocations.empty())
		return 0;
	const StackAllocation* max = &(*stackAllocations.begin());
	for(const StackAllocation& s : stackAllocations)
	{
		if(s.offset + s.size > max->offset + max->size)
			max = &s;
	}
	std::size_t stackSize = max->offset + max->size;

	//make sure, stack-size is aligned to maximum stack entry alignment (for 2nd, 3rd, ... stack-frame)
	std::size_t maxAlignment = 1;
	if(!stackAllocations.empty())
		maxAlignment = stackAllocations.begin()->alignment;
	if(stackSize % maxAlignment != 0)
		stackSize += maxAlignment - (stackSize % maxAlignment);
	//align size of stack-frame to at least 8 bytes, so the code-block is aligned correctly
	if(stackSize % 8 != 0)
		stackSize += 8 - (stackSize % 8);
	return stackSize;
}

std::size_t Method::getStackBaseOffset() const
{
	std::size_t baseOffset = module.getGlobalDataOffset(nullptr).value();

	std::size_t maxAlignment = 1;
	if(!stackAllocations.empty())
		maxAlignment = stackAllocations.begin()->alignment;

	if((baseOffset % maxAlignment) != 0)
		baseOffset += maxAlignment - (baseOffset % maxAlignment);

	//align offset of stack-frame to at least 8 bytes
	if(baseOffset % 8 != 0)
		baseOffset += 8 - (baseOffset % 8);
	return baseOffset;
}

BasicBlock* Method::getNextBlockAfter(const BasicBlock* block)
{
	bool returnNext = false;
	for(BasicBlock& bb : basicBlocks)
	{
		if(returnNext)
			return &bb;
		if(&bb == block)
			returnNext = true;
	}
	return nullptr;
}

BasicBlock* Method::getPreviousBlock(const BasicBlock* block)
{
	bool returnNext = false;
	auto it = basicBlocks.rbegin();
	while(it != basicBlocks.rend())
	{
		if(returnNext)
			return &(*it);
		if(&(*it) == block)
			returnNext = true;
		--it;
	}
	return nullptr;
}

void Method::checkAndCreateDefaultBasicBlock()
{
	if(basicBlocks.empty())
	{
		// in case the input code does not always add a label to the start of a function
		basicBlocks.emplace_back(*this, new intermediate::BranchLabel(*findOrCreateLocal(TYPE_LABEL, BasicBlock::DEFAULT_BLOCK)));
	}
}

Module::Module(const Configuration& compilationConfig): compilationConfig(compilationConfig)
{

}

std::vector<Method*> Module::getKernels()
{
	std::vector<Method*> kernels;
	for(auto& method : methods)
		if(method->isKernel)
			kernels.push_back(method.get());
	return kernels;
}

Optional<unsigned int> Module::getGlobalDataOffset(const Local* local) const
{
	if(local != nullptr && !local->is<Global>())
		return {};
	unsigned int offset = 0;
	for(const Global& global : globalData)
	{
		const unsigned alignment = global.type.getPointerType().value()->getAlignment();
		if(offset % alignment != 0)
		{
			offset += alignment - (offset % alignment);
		}
		if(local == &global)
		{
			return offset;
		}
		offset += global.value.type.getPhysicalWidth();
	}
	if(local == nullptr)
		return offset;
	return {};
}

const Global* Module::findGlobal(const std::string& name) const
{
	for(const Global& global : globalData)
	{
		if(global.name.compare(name) == 0)
			return &global;
	}
	return nullptr;
}
