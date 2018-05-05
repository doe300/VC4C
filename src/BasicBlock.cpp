/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "BasicBlock.h"

#include "InstructionWalker.h"
#include "analysis/ControlFlowGraph.h"
#include "intermediate/IntermediateInstruction.h"

#include <numeric>

#include "log.h"

using namespace vc4c;

const std::string BasicBlock::DEFAULT_BLOCK("%start_of_function");
const std::string BasicBlock::LAST_BLOCK("%end_of_function");

BasicBlock::BasicBlock(Method& method, intermediate::BranchLabel* label) : method(method)
{
    instructions.emplace_back(label);
    method.cfg.reset();
}

BasicBlock::~BasicBlock()
{
    method.cfg.reset();
}

bool BasicBlock::empty() const
{
    // no instructions
    return instructions.empty() ||
        // only the label
        (instructions.size() == 1 && dynamic_cast<intermediate::BranchLabel*>(instructions.front().get()) != nullptr) ||
        // only the label, the remaining instructions are empty
        (std::accumulate(instructions.begin(), instructions.end(), 0,
             [](unsigned tmp, const std::unique_ptr<intermediate::IntermediateInstruction>& instr) -> unsigned {
                 return tmp + (instr != nullptr);
             }) == 1);
}

InstructionWalker BasicBlock::begin()
{
    return InstructionWalker(this, instructions.begin());
}

ConstInstructionWalker BasicBlock::begin() const
{
    return ConstInstructionWalker(this, instructions.begin());
}

InstructionWalker BasicBlock::end()
{
    return InstructionWalker(this, instructions.end());
}

ConstInstructionWalker BasicBlock::end() const
{
    return ConstInstructionWalker(this, instructions.end());
}

std::size_t BasicBlock::size() const
{
    return instructions.size();
}

bool BasicBlock::isLocallyLimited(InstructionWalker curIt, const Local* locale, const std::size_t threshold) const
{
    auto remainingUsers = locale->getUsers();

    int32_t usageRangeLeft = static_cast<int32_t>(threshold);
    // check whether the local is written in the instruction before (and this)
    // this happens e.g. for comparisons
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
        throw CompilationError(
            CompilationStep::GENERAL, "Basic block does not start with a label", instructions.front()->to_string());
    return dynamic_cast<intermediate::BranchLabel*>(instructions.front().get());
}

intermediate::BranchLabel* BasicBlock::getLabel()
{
    if(dynamic_cast<intermediate::BranchLabel*>(instructions.front().get()) == nullptr)
        throw CompilationError(
            CompilationStep::GENERAL, "Basic block does not start with a label", instructions.front()->to_string());
    return dynamic_cast<intermediate::BranchLabel*>(instructions.front().get());
}

void BasicBlock::forSuccessiveBlocks(const std::function<void(BasicBlock&)>& consumer) const
{
    if(method.cfg)
    {
        // if we have a valid CFG, use it. This saves us from iterating all instructions
        method.cfg->assertNode(const_cast<BasicBlock*>(this))
            .forAllNeighbors(toFunction(&CFGRelation::isForwardRelation),
                [&consumer](const CFGNode* node, const CFGRelation& rel) -> void {
                    consumer(*const_cast<BasicBlock*>(node->key));
                });
    }
    else
    {
        ConstInstructionWalker it = begin();
        while(!it.isEndOfBlock())
        {
            if(it.has<intermediate::Branch>())
            {
                BasicBlock* next = method.findBasicBlock(it.get<const intermediate::Branch>()->getTarget());
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
}

void BasicBlock::forPredecessors(const std::function<void(InstructionWalker)>& consumer) const
{
    // TODO some more efficient way of doing this??!
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
    if(method.cfg)
    {
        // if we have a valid CFG, use it. This saves us from iterating all instructions
        const auto& node = method.cfg->assertNode(const_cast<BasicBlock*>(this));
        return std::any_of(node.getNeighbors().begin(), node.getNeighbors().end(),
            [](const std::pair<CFGNode*, CFGRelation>& neighbor) -> bool {
                return neighbor.second.isForwardRelation() && neighbor.second.isImplicit;
            });
    }
    // if the last instruction of a basic block is not an unconditional branch to another block, the control-flow falls
    // through to the next block
    ConstInstructionWalker it = end();
    do
    {
        it.previousInBlock();
    } while(it.has<intermediate::Nop>());
    const intermediate::Branch* lastBranch = it.get<const intermediate::Branch>();
    const intermediate::Branch* secondLastBranch = nullptr;
    if(!it.isStartOfBlock())
    {
        do
        {
            if(it.isStartOfBlock())
                // special handling for blocks with only label and branches
                break;
            it.previousInBlock();
        } while(!it.has<intermediate::Branch>());
        secondLastBranch = it.get<const intermediate::Branch>();
    }
    if(lastBranch != nullptr && lastBranch->isUnconditional())
    {
        return false;
    }
    // for either-there-or-there branches, we need to check the two last instructions and see if they cover all
    // conditions
    if(lastBranch != nullptr && secondLastBranch != nullptr &&
        lastBranch->getCondition() == secondLastBranch->getCondition() &&
        lastBranch->conditional.isInversionOf(secondLastBranch->conditional))
    {
        return false;
    }
    return true;
}

Optional<InstructionWalker> BasicBlock::findWalkerForInstruction(
    const intermediate::IntermediateInstruction* instr, InstructionWalker start) const
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
    return &(*method.begin()) == this;
}

void BasicBlock::dumpInstructions() const
{
    logging::debug() << "Basic block ----" << logging::endl;

    std::for_each(instructions.begin(), instructions.end(),
        [](const std::unique_ptr<intermediate::IntermediateInstruction>& instr) {
            if(instr)
                CPPLOG_LAZY(logging::Level::DEBUG, log << instr->to_string() << logging::endl);
        });
    logging::debug() << "Block end ----" << logging::endl;
}
