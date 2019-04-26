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

BasicBlock::BasicBlock(Method& method, intermediate::BranchLabel* label) : method(method), instructions()
{
    instructions.emplace_back(label);
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

InstructionWalker BasicBlock::walk()
{
    return InstructionWalker(this, instructions.begin());
}

ConstInstructionWalker BasicBlock::walk() const
{
    return ConstInstructionWalker(this, instructions.begin());
}

InstructionWalker BasicBlock::walkEnd()
{
    return InstructionWalker(this, instructions.end());
}

ConstInstructionWalker BasicBlock::walkEnd() const
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
            .forAllOutgoingEdges([&consumer](CFGNode& node, CFGEdge& edge) -> bool {
                consumer(*node.key);
                return true;
            });
    }
    else
    {
        for(const auto& inst : *this)
        {
            if(auto branch = dynamic_cast<const intermediate::Branch*>(inst.get()))
            {
                if(auto next = method.findBasicBlock(branch->getTarget()))
                    consumer(*next);
            }
            // TODO shouldn't this be outside of the loop?!
            if(fallsThroughToNextBlock())
            {
                if(auto next = method.getNextBlockAfter(this))
                    consumer(*next);
            }
        }
    }
}

void BasicBlock::forPredecessors(const std::function<void(InstructionWalker)>& consumer) const
{
    if(method.cfg)
    {
        // if we have a valid CFG, use it. This saves us from iterating all instructions
        auto& thisNode = method.cfg->assertNode(const_cast<BasicBlock*>(this));
        thisNode.forAllIncomingEdges([&](CFGNode& node, CFGEdge& edge) -> bool {
            consumer(edge.data.getPredecessor(edge.getOtherNode(thisNode).key));
            return true;
        });
    }
    else
    {
        // TODO some more efficient way of doing this??!
        // this function alone (including children) takes 30% of time for large basic blocks (according to perf on
        // clpeak/compute_sp_kernels). A large part of that the dynamic_cast to Branch (ca. 7%)
        // TODO don't check all instructions, but from the end only until all branch possibilities are found?!
        const Local* label = getLabel()->getLabel();
        BasicBlock* prevBlock = nullptr;
        bool prevBlockFound = false;
        for(BasicBlock& bb : method)
        {
            for(auto it = bb.walk(); !it.isEndOfBlock(); it.nextInBlock())
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
            consumer(prevBlock->walkEnd().previousInBlock());
        }
    }
}

bool BasicBlock::fallsThroughToNextBlock(bool useCFGIfAvailable) const
{
    if(useCFGIfAvailable && method.cfg)
    {
        // if we have a valid CFG, use it. This saves us from iterating all instructions
        const auto& node = method.cfg->assertNode(const_cast<BasicBlock*>(this));
        bool fallsThrough = false;
        node.forAllOutgoingEdges([&node, &fallsThrough](const CFGNode& n, const CFGEdge& edge) -> bool {
            if(edge.data.isImplicit(node.key))
                fallsThrough = true;
            return !fallsThrough;
        });
        return fallsThrough;
    }
    // if the last instruction of a basic block is not an unconditional branch to another block, the control-flow falls
    // through to the next block
    ConstInstructionWalker it = walkEnd();
    do
    {
        it.previousInBlock();
        if(it.get() && it->signal == SIGNAL_END_PROGRAM)
            return false;
    } while(it.get<const intermediate::Nop>());
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
        } while(!it.get<const intermediate::Branch>());
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
        if(auto comb = it.get<intermediate::CombinedOperation>())
        {
            if(comb->op1 && comb->op1->setFlags == SetFlag::SET_FLAGS)
                return it;
            if(comb->op2 && comb->op2->setFlags == SetFlag::SET_FLAGS)
                return it;
        }
        it.previousInBlock();
    }
    return {};
}

bool BasicBlock::isStartOfMethod() const
{
    return &(*method.begin()) == this;
}

void BasicBlock::dumpInstructions() const
{
    logging::logLazy(logging::Level::DEBUG, [&]() {
        logging::debug() << "Basic block ----" << logging::endl;
        std::for_each(instructions.begin(), instructions.end(),
            [](const std::unique_ptr<intermediate::IntermediateInstruction>& instr) {
                if(instr)
                    logging::debug() << instr->to_string() << logging::endl;
            });
        logging::debug() << "Block end ----" << logging::endl;
    });
}

std::string BasicBlock::to_string() const
{
    return "block " + getLabel()->getLabel()->name;
}