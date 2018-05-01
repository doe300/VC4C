/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Method.h"

#include "Module.h"
#include "Profiler.h"
#include "analysis/ControlFlowGraph.h"
#include "intermediate/IntermediateInstruction.h"
#include "periphery/VPM.h"

#include "log.h"

#include <atomic>

using namespace vc4c;

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

Method::Method(const Module& module) :
    isKernel(false), name(), returnType(TYPE_UNKNOWN),
    vpm(new periphery::VPM(module.compilationConfig.availableVPMSize)), module(module)
{
}

Method::~Method()
{
    // makes sure, instructions are removed before locals (so usages are all zero)
    basicBlocks.clear();
}

const Local* Method::findLocal(const std::string& name) const
{
    auto it = locals.find(name);
    if(it != locals.end())
        return &(it->second);
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

static bool removeUsagesInBasicBlock(const Method& method, const BasicBlock& bb, const Local* locale,
    OrderedMap<const LocalUser*, LocalUse>& remainingUsers, int& usageRangeLeft)
{
    auto it = bb.begin();
    while(usageRangeLeft >= 0 && !it.isEndOfMethod())
    {
        remainingUsers.erase(it.get());
        --usageRangeLeft;
        if(it.has<intermediate::Branch>())
        {
            const BasicBlock* successor = method.findBasicBlock(it.get<const intermediate::Branch>()->getTarget());
            if(successor != nullptr &&
                removeUsagesInBasicBlock(method, *successor, locale, remainingUsers, usageRangeLeft))
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
    // check whether the local is written in the instruction before (and this)
    // this happens e.g. for comparisons
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
            const BasicBlock* successor = findBasicBlock(branch->getTarget());
            if(successor != nullptr &&
                removeUsagesInBasicBlock(*this, *successor, locale, remainingUsers, usageRangeLeft))
                return true;
            if(branch->isUnconditional())
                // this branch jumps away unconditionally and the successor does not have all remaining usages within
                // the remaining range, so we abort
                return false;
        }
        curIt.nextInMethod();
    }

    return remainingUsers.empty();
}

static std::atomic_size_t tmpIndex{0};

const Value Method::addNewLocal(const DataType& type, const std::string& prefix, const std::string& postfix)
{
    const std::string name = createLocalName(prefix, postfix);
    if(findLocal(name) != nullptr)
        throw CompilationError(
            CompilationStep::GENERAL, "Local with this name already exists", findLocal(name)->to_string());
    auto it = locals.emplace(name, Local(type, name));
    return it.first->second.createReference();
}

std::string Method::createLocalName(const std::string& prefix, const std::string& postfix)
{
    // prefix, postfix empty -> "%tmp.tmpIndex"
    // prefix empty -> "%postfix"
    // postfix empty -> "prefix.tmpIndex"
    // none empty -> "prefix.postfix"
    std::string localName;
    if((prefix.empty() || prefix == "%") && postfix.empty())
    {
        localName = std::string("%tmp.") + std::to_string(tmpIndex++);
    }
    else if((prefix.empty() || prefix == "%"))
    {
        if(postfix[0] == '%')
            // to prevent "%%xyz"
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
    return begin()->begin();
}

void Method::forAllInstructions(const std::function<void(const intermediate::IntermediateInstruction*)>& consumer) const
{
    for(const BasicBlock& bb : *this)
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
    for(const BasicBlock& bb : *this)
    {
        count += bb.instructions.size();
    }
    return count;
}

std::size_t Method::cleanEmptyInstructions()
{
    // TODO required??
    std::size_t num = 0;
    auto it = walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(it.get() == nullptr)
        {
            it.erase();
            ++num;
            if(it.isEndOfBlock())
                it.nextInMethod();
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
    /*
     * Reset CFG since it might have changed.
     *
     * This will have no effect anyway most of the time, since appendToEnd() is called in front-end where there is no
     * CFG
     */
    if(dynamic_cast<intermediate::Branch*>(instr) != nullptr || dynamic_cast<intermediate::BranchLabel*>(instr))
        cfg.reset();
}
InstructionWalker Method::appendToEnd()
{
    checkAndCreateDefaultBasicBlock();
    // Invalidation of the CFG in this case is handled in InstructionWalker
    return basicBlocks.back().end();
}

const UnorderedMap<std::string, Local>& Method::readLocals() const
{
    return locals;
}

void Method::cleanLocals()
{
    PROFILE_COUNTER(vc4c::profiler::COUNTER_GENERAL + 7, "Clean locals (before)", locals.size());
#ifdef DEBUG_MODE
    // check duplicate locals
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
            throw CompilationError(
                CompilationStep::GENERAL, "Local is already defined for method", it->second.to_string());
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
    PROFILE_COUNTER_WITH_PREV(vc4c::profiler::COUNTER_GENERAL + 8, "Clean locals (after)", locals.size(),
        vc4c::profiler::COUNTER_GENERAL + 7);
}

void Method::dumpInstructions() const
{
    for(const BasicBlock& bb : *this)
    {
        bb.dumpInstructions();
    }
}

BasicBlock* Method::findBasicBlock(const Local* label)
{
    for(BasicBlock& bb : *this)
    {
        if(bb.begin().has<intermediate::BranchLabel>() &&
            bb.begin().get<intermediate::BranchLabel>()->getLabel() == label)
            return &bb;
    }
    return nullptr;
}

const BasicBlock* Method::findBasicBlock(const Local* label) const
{
    for(const BasicBlock& bb : *this)
    {
        if(bb.begin().has<intermediate::BranchLabel>() &&
            bb.begin().get<const intermediate::BranchLabel>()->getLabel() == label)
            return &bb;
    }
    return nullptr;
}

bool Method::removeBlock(BasicBlock& block, bool overwriteUsages)
{
    if(!overwriteUsages)
    {
        // check any usage
        // 1. check instructions inside block
        if(!block.empty())
            return false;
        // 2. check explicit jumps to this block
        unsigned count = 0;
        block.forPredecessors([&block, &count](InstructionWalker it) -> void {
            // only check for explicit jumps to this block, implicit "jumps" will just fall-through to the next block
            if(it.has<intermediate::Branch>() &&
                it.get<intermediate::Branch>()->getTarget() == block.getLabel()->getLabel())
                ++count;
        });
        if(count > 0)
            return false;
    }
    auto it = begin();
    while(it != end())
    {
        if(&(*it) == &block)
        {
            logging::debug() << "Removing basic block '" << block.getLabel()->to_string() << "' from function " << name
                             << logging::endl;
            basicBlocks.erase(it);
            return true;
        }
        ++it;
    }
    logging::warn() << "Basic block '" << block.getLabel()->to_string() << "' was not found in this function " << name
                    << logging::endl;
    return false;
}

BasicBlock& Method::createAndInsertNewBlock(BasicBlockList::iterator position, const std::string& labelName)
{
    auto newLabel = locals.emplace(labelName, Local(TYPE_LABEL, labelName));
    return *basicBlocks.emplace(position, *this, new intermediate::BranchLabel(newLabel.first->second));
}

InstructionWalker Method::emplaceLabel(InstructionWalker it, intermediate::BranchLabel* label)
{
    auto blockIt = begin();
    while(blockIt != end())
    {
        if(&(*blockIt) == it.basicBlock)
            break;
        ++blockIt;
    }
    if(blockIt == end())
        throw CompilationError(CompilationStep::GENERAL, "Failed to find basic block for instruction iterator");
    // 1. insert new basic block after the current (or in front of it, if we emplace at the start of the basic block)
    bool isStartOfBlock = blockIt->begin() == it;
    if(!isStartOfBlock)
        ++blockIt;
    BasicBlock& newBlock = *basicBlocks.emplace(blockIt, *this, label);
    // 2. move all instructions beginning with it (inclusive) to the new basic block
    while(!isStartOfBlock && !it.isEndOfBlock())
    {
        newBlock.instructions.emplace_back(it.release());
        it.erase();
    }
    // 3. return the begin() of the new basic block
    return newBlock.begin();
}

void Method::calculateStackOffsets()
{
    // TODO this could be greatly improved, by re-using space for other stack-allocations, when their life-times don't
    // intersect (similar to register allocation)
    const std::size_t stackBaseOffset = getStackBaseOffset();

    // Simple version: reserve extra space for every stack-allocation
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

    // make sure, stack-size is aligned to maximum stack entry alignment (for 2nd, 3rd, ... stack-frame)
    std::size_t maxAlignment = 1;
    if(!stackAllocations.empty())
        maxAlignment = stackAllocations.begin()->alignment;
    if(stackSize % maxAlignment != 0)
        stackSize += maxAlignment - (stackSize % maxAlignment);
    // align size of stack-frame to at least 8 bytes, so the code-block is aligned correctly
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

    // align offset of stack-frame to at least 8 bytes
    if(baseOffset % 8 != 0)
        baseOffset += 8 - (baseOffset % 8);
    return baseOffset;
}

ControlFlowGraph& Method::getCFG()
{
    if(!cfg)
    {
        logging::debug() << "CFG created/updated for function: " << name << logging::endl;
        cfg = std::make_shared<ControlFlowGraph>(ControlFlowGraph::createCFG(*this));
    }
    return *cfg.get();
}

BasicBlock* Method::getNextBlockAfter(const BasicBlock* block)
{
    bool returnNext = false;
    for(BasicBlock& bb : *this)
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
        basicBlocks.emplace_back(
            *this, new intermediate::BranchLabel(*findOrCreateLocal(TYPE_LABEL, BasicBlock::DEFAULT_BLOCK)));
    }
}
