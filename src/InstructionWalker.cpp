/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "InstructionWalker.h"

#include "CompilationError.h"
#include "analysis/ControlFlowGraph.h"

using namespace vc4c;

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
            if(followJumps && it.has<intermediate::Branch>())
            {
                intermediate::Branch* jump = it.get<intermediate::Branch>();
                BasicBlock* nextBlock = it.getBasicBlock()->method.findBasicBlock(jump->getTarget());
                if(nextBlock != nullptr)
                {
                    bool cont = visit(nextBlock->begin());
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
                        .forAllNeighbors(toFunction(&CFGRelation::isReverseRelation),
                            [&continueBranches, this, blockGraph](const CFGNode* node, const CFGRelation& rel) -> void {
                                // this makes sure, a STOP_ALL skips other predecessors
                                if(continueBranches)
                                    continueBranches = visitReverse(rel.predecessor, blockGraph);
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
        BasicBlock* tmp = basicBlock->method.getNextBlockAfter(basicBlock);
        if(tmp != nullptr)
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
        BasicBlock* tmp = basicBlock->method.getPreviousBlock(basicBlock);
        if(tmp != nullptr)
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
    const auto& lastBlock = *(--basicBlock->method.end());
    return isEndOfBlock() && &lastBlock == basicBlock;
}

bool InstructionWalker::isStartOfMethod() const
{
    return isStartOfBlock() && basicBlock->isStartOfMethod();
}

bool InstructionWalker::isEndOfBlock() const
{
    return pos == basicBlock->instructions.end();
}

bool InstructionWalker::isStartOfBlock() const
{
    return pos == basicBlock->instructions.begin();
}

static inline void throwOnEnd(bool isEnd)
{
    if(isEnd)
        throw CompilationError(CompilationStep::GENERAL, "End of method reached!");
}

intermediate::IntermediateInstruction* InstructionWalker::get()
{
    throwOnEnd(isEndOfMethod());
    return (*pos).get();
}

const intermediate::IntermediateInstruction* InstructionWalker::get() const
{
    throwOnEnd(isEndOfMethod());
    return (*pos).get();
}

intermediate::IntermediateInstruction* InstructionWalker::release()
{
    throwOnEnd(isEndOfMethod());
    if(has<intermediate::Branch>() || has<intermediate::BranchLabel>())
        basicBlock->method.cfg.reset();
    return (*pos).release();
}

InstructionWalker& InstructionWalker::reset(intermediate::IntermediateInstruction* instr)
{
    throwOnEnd(isEndOfMethod());
    if(dynamic_cast<intermediate::BranchLabel*>(instr) != dynamic_cast<intermediate::BranchLabel*>((*pos).get()))
        throw CompilationError(CompilationStep::GENERAL, "Can't add labels into a basic block", instr->to_string());
    if(has<intermediate::Branch>() || has<intermediate::BranchLabel>() ||
        dynamic_cast<intermediate::Branch*>(instr) != nullptr)
        basicBlock->method.cfg.reset();
    (*pos).reset(instr);
    return *this;
}

InstructionWalker& InstructionWalker::erase()
{
    throwOnEnd(isEndOfMethod());
    if(has<intermediate::Branch>() || has<intermediate::BranchLabel>())
        basicBlock->method.cfg.reset();
    pos = basicBlock->instructions.erase(pos);
    return *this;
}

InstructionWalker& InstructionWalker::emplace(intermediate::IntermediateInstruction* instr)
{
    if(isStartOfBlock())
        throw CompilationError(
            CompilationStep::GENERAL, "Can't emplace at the start of a basic block", instr->to_string());
    if(dynamic_cast<intermediate::BranchLabel*>(instr) != nullptr)
        throw CompilationError(CompilationStep::GENERAL, "Can't add labels into a basic block", instr->to_string());
    if(dynamic_cast<intermediate::Branch*>(instr) != nullptr)
        basicBlock->method.cfg.reset();
    pos = basicBlock->instructions.emplace(pos, instr);
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

bool InstructionWalker::replaceLocalInBlock(
    const Local* oldLocal, const Local* newLocal, LocalUse::Type type, bool forward, bool stopFlag)
{
    return replaceValueInBlock(oldLocal->createReference(), newLocal->createReference(), type, forward, stopFlag);
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
        BasicBlock* tmp = basicBlock->method.getNextBlockAfter(basicBlock);
        if(tmp != nullptr)
        {
            basicBlock = tmp;
            pos = basicBlock->instructions.begin();
        }
    }

    return *this;
}

bool InstructionWalker::replaceValueInBlock(
    const Value oldValue, const Value newValue, LocalUse::Type type, bool forward, bool stopWhenWritten)
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
        BasicBlock* tmp = basicBlock->method.getPreviousBlock(basicBlock);
        if(tmp != nullptr)
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
    const auto& lastBlock = *(--basicBlock->method.end());
    return isEndOfBlock() && &lastBlock == basicBlock;
}

bool ConstInstructionWalker::isStartOfMethod() const
{
    return isStartOfBlock() && basicBlock->isStartOfMethod();
}

bool ConstInstructionWalker::isEndOfBlock() const
{
    return pos == basicBlock->instructions.end();
}

bool ConstInstructionWalker::isStartOfBlock() const
{
    return pos == basicBlock->instructions.begin();
}

const intermediate::IntermediateInstruction* ConstInstructionWalker::get() const
{
    throwOnEnd(isEndOfMethod());
    return (*pos).get();
}

std::size_t vc4c::hash<vc4c::InstructionWalker>::operator()(vc4c::InstructionWalker const& it) const noexcept
{
    std::hash<const intermediate::IntermediateInstruction*> h;
    return h(it.get());
}

std::size_t vc4c::hash<vc4c::ConstInstructionWalker>::operator()(vc4c::ConstInstructionWalker const& it) const noexcept
{
    std::hash<const intermediate::IntermediateInstruction*> h;
    return h(it.get());
}

void vc4c::swap(BlockIterator& a, BlockIterator& b)
{
    BlockIterator tmp = b;
    b = a;
    a = tmp;
}

void vc4c::swap(MethodIterator& a, MethodIterator& b)
{
    MethodIterator tmp = b;
    b = a;
    a = tmp;
}
