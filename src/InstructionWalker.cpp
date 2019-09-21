/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "InstructionWalker.h"

#include "BasicBlock.h"
#include "CompilationError.h"
#include "Method.h"
#include "analysis/ControlFlowGraph.h"

using namespace vc4c;

const InstructionWalker tombstone_traits<InstructionWalker>::tombstone;

bool InstructionVisitor::visit(const InstructionWalker& start) const
{
    InstructionWalker it(start);
    while(true)
    {
        auto res = op(it);
        switch(res)
        {
        case InstructionVisitResult::CONTINUE:
        {
            if(followJumps && it.get<intermediate::Branch>())
            {
                intermediate::Branch* jump = it.get<intermediate::Branch>();
                if(auto nextBlock = it.getBasicBlock()->method.findBasicBlock(jump->getTarget()))
                {
                    bool cont = visit(nextBlock->walk());
                    if(!cont)
                        return false;
                }
                if(jump->isUnconditional())
                    // the control-flow always jumps, destination is already processed
                    return true;
                // handle either-or-jumps, check previous instruction
                intermediate::Branch* prevJump = it.copy().previousInBlock().get<intermediate::Branch>();
                if(prevJump != nullptr && prevJump->getCondition() == jump->getCondition() &&
                    jump->conditional.isInversionOf(prevJump->conditional))
                    // the control-flow always jumps, both destinations are already processed
                    return true;
            }
            if(stopAtBlock)
            {
                it.nextInBlock();
                if(it.isEndOfBlock())
                    return true;
                continue;
            }
            else
            {
                it.nextInMethod();
                if(it.isEndOfMethod())
                    return true;
                continue;
            }
        }
        case InstructionVisitResult::STOP_BRANCH:
            // stop this branch, but continue with other branches
            return true;
        case InstructionVisitResult::STOP_ALL:
            // stop all
            return false;
        }
        throw CompilationError(CompilationStep::GENERAL, "Unhandled case of instruction visiting");
    }
}

bool InstructionVisitor::visitReverse(const InstructionWalker& start, ControlFlowGraph* blockGraph) const
{
    // FIXME has problems with instructionwalkers freed in Method::cleanEmptyInstructions() (e.g. TestEmulator)
    InstructionWalker it(start);
    while(true)
    {
        auto res = op(it);
        switch(res)
        {
        case InstructionVisitResult::CONTINUE:
        {
            if(!it.isStartOfBlock())
            {
                it.previousInBlock();
                continue;
            }
            else if(stopAtBlock)
            {
                return true;
            }
            else if(!followJumps)
            {
                it.previousInMethod();
                continue;
            }
            else // start of block and follow jumps -> follow jumps backwards
            {
                bool continueBranches = true;
                if(blockGraph != nullptr)
                {
                    // use pre-calculated graph of basic blocks
                    blockGraph->assertNode(it.getBasicBlock())
                        .forAllIncomingEdges(
                            [&continueBranches, this, blockGraph](const CFGNode& node, const CFGEdge& edge) -> bool {
                                // this makes sure, a STOP_ALL skips other predecessors
                                if(continueBranches)
                                    continueBranches = visitReverse(edge.data.getPredecessor(node.key), blockGraph);
                                return true;
                            });
                }
                else
                {
                    // re-calculate predecessor blocks
                    it.getBasicBlock()->forPredecessors([&continueBranches, this](InstructionWalker it) -> void {
                        // this makes sure, a STOP_ALL skips other predecessors
                        if(continueBranches)
                            continueBranches = visitReverse(it);
                    });
                }
                return continueBranches;
            }
        }
        case InstructionVisitResult::STOP_BRANCH:
            // stop this branch, but continue with other branches
            return true;
        case InstructionVisitResult::STOP_ALL:
            // stop all
            return false;
        }
        throw CompilationError(CompilationStep::GENERAL, "Unhandled case of instruction visiting");
    }
}

InstructionWalker::InstructionWalker() : basicBlock(nullptr), pos(nullptr) {}

InstructionWalker::InstructionWalker(BasicBlock* basicBlock, intermediate::InstructionsIterator pos) :
    basicBlock(basicBlock), pos(pos)
{
}

BasicBlock* InstructionWalker::getBasicBlock()
{
    return basicBlock;
}

const BasicBlock* InstructionWalker::getBasicBlock() const
{
    return basicBlock;
}

InstructionWalker& InstructionWalker::nextInBlock()
{
    ++pos;
    return *this;
}

InstructionWalker& InstructionWalker::previousInBlock()
{
    --pos;
    return *this;
}

InstructionWalker& InstructionWalker::nextInMethod()
{
    nextInBlock();
    if(isEndOfBlock())
    {
        if(auto tmp = basicBlock->method.getNextBlockAfter(basicBlock))
        {
            basicBlock = tmp;
            pos = basicBlock->instructions.begin();
        }
    }
    return *this;
}
InstructionWalker& InstructionWalker::previousInMethod()
{
    previousInBlock();
    if(isStartOfBlock())
    {
        if(auto tmp = basicBlock->method.getPreviousBlock(basicBlock))
        {
            basicBlock = tmp;
            pos = basicBlock->instructions.end();
            --pos;
        }
    }
    return *this;
}

InstructionWalker InstructionWalker::copy() const
{
    return InstructionWalker(basicBlock, pos);
}

bool InstructionWalker::operator==(const InstructionWalker& other) const
{
    return basicBlock == other.basicBlock && pos == other.pos;
}

bool InstructionWalker::isEndOfMethod() const
{
    if(basicBlock == nullptr)
        return true;
    if(!isEndOfBlock())
        return false;
    const auto& lastBlock = *(--basicBlock->method.end());
    return &lastBlock == basicBlock;
}

bool InstructionWalker::isStartOfMethod() const
{
    return isStartOfBlock() && basicBlock->isStartOfMethod();
}

bool InstructionWalker::isEndOfBlock() const
{
    if(basicBlock == nullptr)
        return true;
    return pos == basicBlock->instructions.end();
}

bool InstructionWalker::isStartOfBlock() const
{
    if(basicBlock == nullptr)
        return false;
    return pos == basicBlock->instructions.begin();
}

static inline void throwOnEnd(bool isEnd)
{
    if(isEnd)
        throw CompilationError(CompilationStep::GENERAL, "End of method or block reached!");
}

intermediate::IntermediateInstruction* InstructionWalker::get()
{
    throwOnEnd(isEndOfBlock());
    return (*pos).get();
}

const intermediate::IntermediateInstruction* InstructionWalker::get() const
{
    throwOnEnd(isEndOfBlock());
    return (*pos).get();
}

intermediate::IntermediateInstruction* InstructionWalker::release()
{
    throwOnEnd(isEndOfBlock());
    if(get<intermediate::BranchLabel>())
        basicBlock->method.updateCFGOnBlockRemoval(basicBlock);
    if(get<intermediate::Branch>())
    {
        // need to remove the branch from the block before triggering the CFG update
        std::unique_ptr<intermediate::IntermediateInstruction> tmp(pos->release());
        basicBlock->method.updateCFGOnBranchRemoval(
            *basicBlock, dynamic_cast<intermediate::Branch*>(tmp.get())->getTarget());
        return tmp.release();
    }
    return (*pos).release();
}

InstructionWalker& InstructionWalker::reset(intermediate::IntermediateInstruction* instr)
{
    throwOnEnd(isEndOfBlock());
    if(dynamic_cast<intermediate::BranchLabel*>(instr) != dynamic_cast<intermediate::BranchLabel*>((*pos).get()))
        throw CompilationError(CompilationStep::GENERAL, "Can't add labels into a basic block", instr->to_string());
    // if we reset the label with another label, the CFG dos not change
    if(get<intermediate::Branch>())
    {
        // need to remove the branch from the block before triggering the CFG update
        std::unique_ptr<intermediate::IntermediateInstruction> tmp(pos->release());
        basicBlock->method.updateCFGOnBranchRemoval(
            *basicBlock, dynamic_cast<intermediate::Branch*>(tmp.get())->getTarget());
    }
    (*pos).reset(instr);
    if(dynamic_cast<intermediate::Branch*>(instr))
        basicBlock->method.updateCFGOnBranchInsertion(*this);
    return *this;
}

InstructionWalker& InstructionWalker::erase()
{
    throwOnEnd(isEndOfBlock());
    if(get<intermediate::BranchLabel>())
        basicBlock->method.updateCFGOnBlockRemoval(basicBlock);
    if(get<intermediate::Branch>())
    {
        // need to remove the branch from the block before triggering the CFG update
        std::unique_ptr<intermediate::IntermediateInstruction> tmp(pos->release());
        basicBlock->method.updateCFGOnBranchRemoval(
            *basicBlock, dynamic_cast<intermediate::Branch*>(tmp.get())->getTarget());
    }
    pos = basicBlock->instructions.erase(pos);
    return *this;
}

InstructionWalker& InstructionWalker::emplace(intermediate::IntermediateInstruction* instr)
{
    if(isStartOfBlock())
        throw CompilationError(
            CompilationStep::GENERAL, "Can't emplace at the start of a basic block", instr->to_string());
    if(basicBlock == nullptr)
        throw CompilationError(
            CompilationStep::GENERAL, "Can't emplace into an iterator which is not associated with a basic block");
    if(dynamic_cast<intermediate::BranchLabel*>(instr) != nullptr)
        throw CompilationError(CompilationStep::GENERAL, "Can't add labels into a basic block", instr->to_string());
    pos = basicBlock->instructions.emplace(pos, instr);
    if(dynamic_cast<intermediate::Branch*>(instr))
        basicBlock->method.updateCFGOnBranchInsertion(*this);
    return *this;
}

ConstInstructionWalker::ConstInstructionWalker() : basicBlock(nullptr), pos(nullptr) {}

ConstInstructionWalker::ConstInstructionWalker(InstructionWalker it) : basicBlock(it.basicBlock), pos(it.pos) {}

ConstInstructionWalker::ConstInstructionWalker(
    const BasicBlock* basicBlock, intermediate::ConstInstructionsIterator pos) :
    basicBlock(basicBlock),
    pos(pos)
{
}

const BasicBlock* ConstInstructionWalker::getBasicBlock() const
{
    return basicBlock;
}

ConstInstructionWalker& ConstInstructionWalker::nextInBlock()
{
    ++pos;
    return *this;
}

ConstInstructionWalker& ConstInstructionWalker::previousInBlock()
{
    --pos;
    return *this;
}

ConstInstructionWalker& ConstInstructionWalker::nextInMethod()
{
    nextInBlock();
    if(isEndOfBlock())
    {
        if(auto tmp = basicBlock->method.getNextBlockAfter(basicBlock))
        {
            basicBlock = tmp;
            pos = basicBlock->instructions.begin();
        }
    }

    return *this;
}

bool InstructionWalker::replaceValueInBlock(
    const Value& oldValue, const Value& newValue, LocalUse::Type type, bool forward, bool stopWhenWritten)
{
    bool replaced = false;
    auto it = copy();
    if(forward)
    {
        it.nextInBlock();
        while(!it.isEndOfBlock())
        {
            replaced = it->replaceValue(oldValue, newValue, type);
            if(it->getOutput().has_value() && it->getOutput().value() == oldValue && stopWhenWritten)
                break;

            it.nextInBlock();
        }
    }
    else
    {
        it.previousInBlock();
        while(!it.isStartOfBlock())
        {
            replaced = it->replaceValue(oldValue, newValue, type);
            if(it->getOutput().has_value() && it->getOutput().value() == oldValue && stopWhenWritten)
                break;

            it.previousInBlock();
        }
    }

    return replaced;
}

ConstInstructionWalker& ConstInstructionWalker::previousInMethod()
{
    previousInBlock();
    if(isStartOfBlock())
    {
        if(auto tmp = basicBlock->method.getPreviousBlock(basicBlock))
        {
            basicBlock = tmp;
            pos = basicBlock->instructions.end();
            --pos;
        }
    }
    return *this;
}

ConstInstructionWalker ConstInstructionWalker::copy() const
{
    return ConstInstructionWalker(basicBlock, pos);
}

bool ConstInstructionWalker::operator==(const ConstInstructionWalker& other) const
{
    return basicBlock == other.basicBlock && pos == other.pos;
}

bool ConstInstructionWalker::isEndOfMethod() const
{
    if(basicBlock == nullptr)
        return true;
    const auto& lastBlock = *(--basicBlock->method.end());
    return isEndOfBlock() && &lastBlock == basicBlock;
}

bool ConstInstructionWalker::isStartOfMethod() const
{
    return isStartOfBlock() && basicBlock->isStartOfMethod();
}

bool ConstInstructionWalker::isEndOfBlock() const
{
    if(basicBlock == nullptr)
        return true;
    return pos == basicBlock->instructions.end();
}

bool ConstInstructionWalker::isStartOfBlock() const
{
    if(basicBlock == nullptr)
        return false;
    return pos == basicBlock->instructions.begin();
}

const intermediate::IntermediateInstruction* ConstInstructionWalker::get() const
{
    throwOnEnd(isEndOfBlock());
    return (*pos).get();
}
