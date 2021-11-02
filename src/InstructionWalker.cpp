/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "InstructionWalker.h"

#include "BasicBlock.h"
#include "CompilationError.h"
#include "Method.h"

using namespace vc4c;

const InstructionWalker tombstone_traits<InstructionWalker>::tombstone;

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
    if(!isEndOfBlock())
        // we need this check since for a linked list it seems that last->next == first, which would jump back to the
        // label of the block
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

std::unique_ptr<intermediate::IntermediateInstruction> InstructionWalker::release()
{
    throwOnEnd(isEndOfBlock());
    if(get<intermediate::BranchLabel>())
        basicBlock->method.updateCFGOnBlockRemoval(basicBlock);
    if(get<intermediate::Branch>())
    {
        // need to remove the branch from the block before triggering the CFG update
        auto tmp = std::move(*pos);
        basicBlock->method.updateCFGOnBranchRemoval(
            *basicBlock, dynamic_cast<intermediate::Branch*>(tmp.get())->getTargetLabels());
        return tmp;
    }
    return std::move(*pos);
}

intermediate::IntermediateInstruction& InstructionWalker::reset(
    std::unique_ptr<intermediate::IntermediateInstruction>&& instr)
{
    throwOnEnd(isEndOfBlock());
    if(dynamic_cast<intermediate::BranchLabel*>(instr.get()) != dynamic_cast<intermediate::BranchLabel*>((*pos).get()))
        throw CompilationError(CompilationStep::GENERAL, "Can't add labels into a basic block", instr->to_string());
    // if we reset the label with another label, the CFG dos not change
    if(get<intermediate::Branch>())
    {
        // need to remove the branch from the block before triggering the CFG update
        std::unique_ptr<intermediate::IntermediateInstruction> tmp(pos->release());
        basicBlock->method.updateCFGOnBranchRemoval(
            *basicBlock, dynamic_cast<intermediate::Branch*>(tmp.get())->getTargetLabels());
    }
    bool isBranch = dynamic_cast<intermediate::Branch*>(instr.get());
    (*pos) = std::move(instr);
    if(isBranch)
        basicBlock->method.updateCFGOnBranchInsertion(*this);
    return **pos;
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
            *basicBlock, dynamic_cast<intermediate::Branch*>(tmp.get())->getTargetLabels());
    }
    pos = basicBlock->instructions.erase(pos);
    return *this;
}

InstructionWalker& InstructionWalker::safeErase()
{
    if(!isEndOfBlock() && get() && get()->hasDecoration(intermediate::InstructionDecorations::MANDATORY_DELAY))
    {
        reset(std::make_unique<intermediate::Nop>(intermediate::DelayType::WAIT_REGISTER))
            .addDecorations(intermediate::InstructionDecorations::MANDATORY_DELAY);
        return nextInBlock();
    }
    return erase();
}

intermediate::IntermediateInstruction& InstructionWalker::emplace(
    std::unique_ptr<intermediate::IntermediateInstruction>&& instr)
{
    if(isStartOfBlock())
        throw CompilationError(
            CompilationStep::GENERAL, "Can't emplace at the start of a basic block", instr->to_string());
    if(basicBlock == nullptr)
        throw CompilationError(
            CompilationStep::GENERAL, "Can't emplace into an iterator which is not associated with a basic block");
    if(dynamic_cast<intermediate::BranchLabel*>(instr.get()))
        throw CompilationError(CompilationStep::GENERAL, "Can't add labels into a basic block", instr->to_string());
    pos = basicBlock->instructions.emplace(pos, std::move(instr));
    if(dynamic_cast<intermediate::Branch*>(pos->get()))
        basicBlock->method.updateCFGOnBranchInsertion(*this);
    return **pos;
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
    if(!isEndOfBlock())
        // we need this check since for a linked list it seems that last->next == first, which would jump back to the
        // label of the block
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
