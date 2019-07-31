
#include "PatternMatching.h"

#include "../Expression.h"
#include "../Profiler.h"

using namespace vc4c;
using namespace vc4c::pattern;

InstructionPattern ValuePattern::operator=(UnaryInstructionPattern&& unary) &&
{
    return InstructionPattern{std::move(*this), std::move(unary.operation), std::move(unary.firstArgument), anyValue(),
        std::move(unary.condition)};
}

InstructionPattern ValuePattern::operator=(BinaryInstructionPattern&& binary) &&
{
    return InstructionPattern{std::move(*this), std::move(binary.operation), std::move(binary.firstArgument),
        std::move(binary.secondArgument), std::move(binary.condition)};
}

static bool matchesValue(const Optional<Value>& val, const ValuePattern& pattern)
{
    if(VariantNamespace::get_if<Ignored>(&pattern.pattern))
        // accepts everything, even not set values
        return true;
    if(VariantNamespace::get_if<Placeholder<const Local*>>(&pattern.pattern))
        return val && val->checkLocal();
    if(VariantNamespace::get_if<Placeholder<Literal>>(&pattern.pattern))
        return val && val->getLiteralValue().has_value();
    if(VariantNamespace::get_if<Placeholder<Value>>(&pattern.pattern))
        return val.has_value();
    if(auto fixedValue = VariantNamespace::get_if<Value>(&pattern.pattern))
        return val == *fixedValue;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ValuePattern type");
}

static void updateMatch(const Optional<Value>& val, ValuePattern& pattern)
{
    if(auto local = VariantNamespace::get_if<Placeholder<const Local*>>(&pattern.pattern))
        local->get() = val->local();
    else if(auto literal = VariantNamespace::get_if<Placeholder<Literal>>(&pattern.pattern))
        literal->get() = val->getLiteralValue().value();
    else if(auto value = VariantNamespace::get_if<Placeholder<Value>>(&pattern.pattern))
        value->get() = val.value();
}

static bool matchesOperation(OpCode op, const OperationPattern& pattern)
{
    if(VariantNamespace::get_if<Ignored>(&pattern))
        // accepts everything, even not set operations
        return true;
    if(VariantNamespace::get_if<Placeholder<OpCode>>(&pattern))
        return true;
    if(auto code = VariantNamespace::get_if<OpCode>(&pattern))
        return op == *code;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled OperationPattern type");
}

static void updateMatch(OpCode op, OperationPattern& pattern)
{
    if(auto code = VariantNamespace::get_if<Placeholder<OpCode>>(&pattern))
        code->get() = op;
}

static bool matchesCondition(ConditionCode code, const ConditionPattern& pattern)
{
    if(VariantNamespace::get_if<Ignored>(&pattern))
        // accepts everything, even not set conditions
        return true;
    if(VariantNamespace::get_if<Placeholder<ConditionCode>>(&pattern))
        return true;
    if(auto cond = VariantNamespace::get_if<ConditionCode>(&pattern))
        return code == *cond;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ConditionPattern type");
}

static void updateMatch(ConditionCode code, ConditionPattern& pattern)
{
    if(auto cond = VariantNamespace::get_if<Placeholder<ConditionCode>>(&pattern))
        cond->get() = code;
}

static Optional<OpCode> determineOpCode(const intermediate::IntermediateInstruction* inst)
{
    if(auto op = dynamic_cast<const intermediate::Operation*>(inst))
        return op->op;
    if(dynamic_cast<const intermediate::VectorRotation*>(inst))
        return FAKEOP_ROTATE;
    if(dynamic_cast<const intermediate::MoveOperation*>(inst))
        return FAKEOP_MOV;
    if(dynamic_cast<const intermediate::LoadImmediate*>(inst))
        return FAKEOP_LDI;
    if(dynamic_cast<const intermediate::Branch*>(inst))
        return FAKEOP_BR;
    if(dynamic_cast<const intermediate::MutexLock*>(inst))
        return FAKEOP_MUTEX;

    return {};
}

static bool matchesOnly(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern)
{
    if(!inst)
        return false;

    // pack/unpack modes and signals with side-effects are not supported
    if(inst->hasPackMode() || inst->hasUnpackMode() || inst->signal.hasSideEffects())
        return false;

    if(!matchesValue(inst->getOutput(), pattern.output))
        return false;
    if(auto op = determineOpCode(inst))
    {
        if(!matchesOperation(*op, pattern.operation))
            return false;
    }
    else
        return false;
    if(!matchesValue(inst->getArgument(0), pattern.firstArgument))
        return false;
    if(!matchesValue(inst->getArgument(1), pattern.secondArgument))
        return false;
    if(!matchesCondition(inst->conditional, pattern.condition))
        return false;

    return true;
}

static void updateOnly(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern)
{
    updateMatch(inst->getOutput(), pattern.output);
    if(auto op = determineOpCode(inst))
        updateMatch(*op, pattern.operation);
    // TODO apply for commutative property of opcode, if given
    updateMatch(inst->getArgument(0), pattern.firstArgument);
    updateMatch(inst->getArgument(1), pattern.secondArgument);
    updateMatch(inst->conditional, pattern.condition);
}

bool pattern::matches(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern)
{
    PROFILE_START(PatternMatching);
    if(!matchesOnly(inst, pattern))
    {
        PROFILE_END(PatternMatching);
        return false;
    }

    updateOnly(inst, pattern);
    PROFILE_END(PatternMatching);
    return true;
}

bool pattern::matches(const Expression& expr, InstructionPattern& pattern)
{
    PROFILE_START(PatternMatching);

    // TODO also support arithmetic properties?

    // pack/unpack modes are not supported
    if(expr.packMode.hasEffect() || expr.unpackMode.hasEffect())
    {
        PROFILE_END(PatternMatching);
        return false;
    }
    if(!matchesOperation(expr.code, pattern.operation))
    {
        PROFILE_END(PatternMatching);
        return false;
    }
    if(!matchesValue(expr.arg0, pattern.firstArgument))
    {
        PROFILE_END(PatternMatching);
        return false;
    }
    if(!matchesValue(expr.arg1, pattern.secondArgument))
    {
        PROFILE_END(PatternMatching);
        return false;
    }

    updateMatch(expr.code, pattern.operation);
    updateMatch(expr.arg0, pattern.firstArgument);
    updateMatch(expr.arg1, pattern.secondArgument);

    PROFILE_END(PatternMatching);
    return true;
}

InstructionWalker pattern::search(InstructionWalker start, InstructionPattern& pattern)
{
    PROFILE_START(PatternMatching);
    while(!start.isEndOfBlock())
    {
        if(matches(start.get(), pattern))
        {
            PROFILE_END(PatternMatching);
            return start;
        }
        start.nextInBlock();
    }

    PROFILE_END(PatternMatching);
    return InstructionWalker{};
}

static InstructionWalker searchInnerCompact(InstructionWalker start, Pattern& pattern)
{
    // Check whether all pattern parts match consecutive instructions
    auto it = start;
    for(auto& part : pattern.parts)
    {
        if(it.isEndOfBlock())
            return InstructionWalker{};
        if(!matchesOnly(it.get(), part))
            return InstructionWalker{};
        it.nextInBlock();
    }

    // we matched so far, now update the values
    it = start;
    for(auto& part : pattern.parts)
    {
        updateOnly(it.get(), part);
        it.nextInBlock();
    }

    return start;
}

static InstructionWalker searchInnerGapped(InstructionWalker start, Pattern& pattern)
{
    // Check whether all pattern parts match any following instructions in the correct order
    FastSet<const Local*> gapWrittenLocals;
    auto it = start;
    for(auto& part : pattern.parts)
    {
        while(!it.isEndOfBlock() && !matchesOnly(it.get(), part))
        {
            // this instruction does not match - it is an unrelated gap
            // check for side-effects, determine written locals
            if(it.has() && (it->signal.hasSideEffects() || it->doesSetFlag()))
                // some side-effects in a gap instruction which don't allow for pattern to continue
                // XXX for flags, only abort if flags are actually used
                return InstructionWalker{};
            if(it.has())
                gapWrittenLocals.emplace(it->checkOutputLocal());

            it.nextInBlock();
        }
        if(it.isEndOfBlock())
            return InstructionWalker{};

        // this instruction matched, check for dependencies on locals written by one of the gap instructions
        // XXX can we remove/narrow this? E.g. only abort if local is over-written?!
        if(it.has() && std::any_of(gapWrittenLocals.begin(), gapWrittenLocals.end(), [&](const Local* loc) -> bool {
               return loc != nullptr && it->readsLocal(loc);
           }))
        {
            // matching instruction uses local written by gap instruction, abort
            // XXX this also aborts if we have two successive instructions matching, the first using locals written by
            // an unrelated instruction and the second not using them
            return InstructionWalker{};
        }

        it.nextInBlock();
    }

    // we matched so far, now update the values
    it = start;
    for(auto& part : pattern.parts)
    {
        // XXX if this gets slow, could above cache the matching instructions to skip need to re-check
        while(!it.isEndOfBlock() && !matchesOnly(it.get(), part))
            // skip gaps
            it.nextInBlock();

        updateOnly(it.get(), part);
        it.nextInBlock();
    }

    return start;
}

InstructionWalker pattern::search(InstructionWalker start, Pattern& pattern)
{
    if(pattern.parts.empty())
        return InstructionWalker{};

    PROFILE_START(PatternMatching);

    while(!start.isEndOfBlock())
    {
        if(matchesOnly(start.get(), pattern.parts.front()))
        {
            InstructionWalker it{};
            if(pattern.allowGaps)
                it = searchInnerGapped(start, pattern);
            else
                it = searchInnerCompact(start, pattern);
            // we found a match, return it
            if(!it.isEndOfBlock())
            {
                PROFILE_END(PatternMatching);
                return it;
            }
        }
        start.nextInBlock();
    }

    PROFILE_END(PatternMatching);
    return InstructionWalker{};
}
