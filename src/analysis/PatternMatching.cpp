
#include "PatternMatching.h"

#include "../Expression.h"
#include "../Profiler.h"

#include <unordered_map>

using namespace vc4c;
using namespace vc4c::pattern;

// TODO extend with proper new expression support

InstructionPattern ValuePattern::operator=(UnaryInstructionPattern&& unary) &&
{
    return InstructionPattern{std::move(*this), std::move(unary.operation), std::move(unary.firstArgument), anyValue(),
        std::move(unary.condition), std::move(unary.flags)};
}

InstructionPattern ValuePattern::operator=(BinaryInstructionPattern&& binary) &&
{
    return InstructionPattern{std::move(*this), std::move(binary.operation), std::move(binary.firstArgument),
        std::move(binary.secondArgument), std::move(binary.condition), std::move(binary.flags)};
}

InstructionPattern ValuePattern::operator=(vc4c::operators::OperationWrapper&& op) &&
{
    if(op.packMode.hasEffect() || op.unpackMode.hasEffect())
        throw CompilationError(CompilationStep::GENERAL, "(Un-)Pack modes are not yet supported for pattern matching!");
    if(op.signal.hasSideEffects())
        throw CompilationError(CompilationStep::GENERAL, "Signals are not yet supported for pattern matching!");
    if(op.decoration != intermediate::InstructionDecorations::NONE)
        throw CompilationError(
            CompilationStep::GENERAL, "Instruction decorations are not yet supported for pattern matching!");
    return InstructionPattern{std::move(*this), match(op.op), match(op.arg0), op.arg1 ? match(*op.arg1) : anyValue(),
        match(op.conditional), match(op.setFlags)};
}

using MatchCache = std::unordered_map<const void*, Variant<Value, OpCode, ConditionCode, SetFlag>>;

// The caches are tracked to be able to check whether two captures on the same local value actually capture the same
// value. Since (for multi-instruction patterns) a single instruction might be skipped, but the whole pattern still
// matches, we cannot immediately update the global cache. Therefore the newCache is per instruction and only merged
// into the previousCache if the instruction matches wholly.
static bool matchesCache(const Value& val, const void* ptr, const MatchCache& previousCache, const MatchCache& newCache)
{
    auto it = previousCache.find(ptr);
    if(it != previousCache.end() && VariantNamespace::get<Value>(it->second) != val)
        return false;

    it = newCache.find(ptr);
    if(it != newCache.end() && VariantNamespace::get<Value>(it->second) != val)
        return false;
    return true;
}

static bool matchesCache(OpCode code, const void* ptr, const MatchCache& previousCache, const MatchCache& newCache)
{
    auto it = previousCache.find(ptr);
    if(it != previousCache.end() && VariantNamespace::get<OpCode>(it->second) != code)
        return false;

    it = newCache.find(ptr);
    if(it != newCache.end() && VariantNamespace::get<OpCode>(it->second) != code)
        return false;
    return true;
}

static bool matchesCache(
    ConditionCode code, const void* ptr, const MatchCache& previousCache, const MatchCache& newCache)
{
    auto it = previousCache.find(ptr);
    if(it != previousCache.end() && VariantNamespace::get<ConditionCode>(it->second) != code)
        return false;

    it = newCache.find(ptr);
    if(it != newCache.end() && VariantNamespace::get<ConditionCode>(it->second) != code)
        return false;
    return true;
}

static bool matchesCache(SetFlag flag, const void* ptr, const MatchCache& previousCache, const MatchCache& newCache)
{
    auto it = previousCache.find(ptr);
    if(it != previousCache.end() && VariantNamespace::get<SetFlag>(it->second) != flag)
        return false;

    it = newCache.find(ptr);
    if(it != newCache.end() && VariantNamespace::get<SetFlag>(it->second) != flag)
        return false;
    return true;
}

static bool matchesValue(
    const Optional<Value>& val, const ValuePattern& pattern, const MatchCache& previousCache, MatchCache& newCache)
{
    if(VariantNamespace::get_if<Ignored>(&pattern.pattern))
        // accepts everything, even not set values
        return true;
    if(auto loc = VariantNamespace::get_if<Placeholder<const Local*>>(&pattern.pattern))
    {
        if(!(val & &Value::checkLocal))
            return false;
        auto ptr = &loc->get();
        if(!matchesCache(*val, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, *val);
        return true;
    }
    if(auto lit = VariantNamespace::get_if<Placeholder<Literal>>(&pattern.pattern))
    {
        if(!(val & &Value::getLiteralValue))
            return false;
        auto ptr = &lit->get();
        if(!matchesCache(*val, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, *val);
        return true;
    }
    if(auto value = VariantNamespace::get_if<Placeholder<Value>>(&pattern.pattern))
    {
        if(!val)
            return false;
        auto ptr = &value->get();
        if(!matchesCache(*val, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, *val);
        return true;
    }
    if(auto fixedValue = VariantNamespace::get_if<Value>(&pattern.pattern))
        return val == *fixedValue;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ValuePattern type");
}

static bool matchesValue(
    const SubExpression& sub, const ValuePattern& pattern, const MatchCache& previousCache, MatchCache& newCache)
{
    if(auto val = sub.checkValue())
        return matchesValue(val, pattern, previousCache, newCache);
    return false;
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

static void updateMatch(const SubExpression& sub, ValuePattern& pattern)
{
    if(auto val = sub.checkValue())
        updateMatch(val, pattern);
    // TODO do anything else for e.g. real sub-expressions??
}

static bool matchesOperation(
    OpCode op, const OperationPattern& pattern, const MatchCache& previousCache, MatchCache& newCache)
{
    if(VariantNamespace::get_if<Ignored>(&pattern))
        // accepts everything, even not set operations
        return true;
    if(auto code = VariantNamespace::get_if<Placeholder<OpCode>>(&pattern))
    {
        auto ptr = &code->get();
        if(!matchesCache(op, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, op);
        return true;
    }
    if(auto code = VariantNamespace::get_if<OpCode>(&pattern))
        return op == *code;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled OperationPattern type");
}

static void updateMatch(OpCode op, OperationPattern& pattern)
{
    if(auto code = VariantNamespace::get_if<Placeholder<OpCode>>(&pattern))
        code->get() = op;
}

static bool matchesCondition(
    ConditionCode code, const ConditionPattern& pattern, const MatchCache& previousCache, MatchCache& newCache)
{
    if(VariantNamespace::get_if<Ignored>(&pattern))
        // accepts everything, even not set conditions
        return true;
    if(auto cond = VariantNamespace::get_if<Placeholder<ConditionCode>>(&pattern))
    {
        auto ptr = &cond->get();
        if(!matchesCache(code, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, code);
        return true;
    }
    if(auto cond = VariantNamespace::get_if<InvertedCondition>(&pattern))
    {
        auto realCode = code.invert();
        auto ptr = &cond->cond.get();
        if(!matchesCache(realCode, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, realCode);
        return true;
    }
    if(auto cond = VariantNamespace::get_if<ConditionCode>(&pattern))
        return code == *cond;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ConditionPattern type");
}

static void updateMatch(ConditionCode code, ConditionPattern& pattern)
{
    if(auto cond = VariantNamespace::get_if<Placeholder<ConditionCode>>(&pattern))
        cond->get() = code;
    if(auto cond = VariantNamespace::get_if<InvertedCondition>(&pattern))
        cond->cond.get() = code.invert();
}

static bool matchesFlag(SetFlag flag, const FlagPattern& pattern, const MatchCache& previousCache, MatchCache& newCache)
{
    if(VariantNamespace::get_if<Ignored>(&pattern))
        // accepts everything, even not set conditions
        return true;
    if(auto state = VariantNamespace::get_if<Placeholder<SetFlag>>(&pattern))
    {
        auto ptr = &state->get();
        if(!matchesCache(flag, ptr, previousCache, newCache))
            return false;
        newCache.emplace(ptr, flag);
        return true;
    }
    if(auto state = VariantNamespace::get_if<SetFlag>(&pattern))
        return flag == *state;
    throw CompilationError(CompilationStep::GENERAL, "Unhandled ConditionPattern type");
}

static void updateMatch(SetFlag flag, FlagPattern& pattern)
{
    if(auto state = VariantNamespace::get_if<Placeholder<SetFlag>>(&pattern))
        state->get() = flag;
}

static Optional<OpCode> determineOpCode(const intermediate::IntermediateInstruction* inst)
{
    if(auto op = dynamic_cast<const intermediate::Operation*>(inst))
        return op->op;
    if(dynamic_cast<const intermediate::VectorRotation*>(inst))
        // TODO need to distinguish between full-range and per-quad?!
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

static bool matchesOnly(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern,
    const MatchCache& previousCache, MatchCache& newCache)
{
    if(!inst)
        return false;

    // pack/unpack modes and signals with side-effects are not supported
    if(inst->hasPackMode() || inst->hasUnpackMode() || inst->signal.hasSideEffects())
        return false;

    if(!matchesValue(inst->getOutput(), pattern.output, previousCache, newCache))
        return false;
    if(auto op = determineOpCode(inst))
    {
        if(!matchesOperation(*op, pattern.operation, previousCache, newCache))
            return false;
    }
    else
        return false;
    if(!matchesValue(inst->getArgument(0), pattern.firstArgument, previousCache, newCache))
        return false;
    if(!matchesValue(inst->getArgument(1), pattern.secondArgument, previousCache, newCache))
        return false;
    if(!matchesCondition(inst->conditional, pattern.condition, previousCache, newCache))
        return false;
    if(!matchesFlag(inst->setFlags, pattern.flags, previousCache, newCache))
        return false;

    return true;
}

static void updateOnly(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern)
{
    updateMatch(inst->getOutput(), pattern.output);
    if(auto op = determineOpCode(inst))
        updateMatch(*op, pattern.operation);
    // TODO apply for commutative property of opcode, if given
    // -> how to (for capture) decide, which way around to capture??
    updateMatch(inst->getArgument(0), pattern.firstArgument);
    updateMatch(inst->getArgument(1), pattern.secondArgument);
    updateMatch(inst->conditional, pattern.condition);
    updateMatch(inst->setFlags, pattern.flags);
}

bool pattern::matches(const intermediate::IntermediateInstruction* inst, InstructionPattern& pattern)
{
    PROFILE_START(PatternMatching);
    MatchCache cache{};
    if(!matchesOnly(inst, pattern, cache, cache))
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

    MatchCache cache{};

    // pack/unpack modes are not supported
    if(expr.packMode.hasEffect() || expr.unpackMode.hasEffect())
    {
        PROFILE_END(PatternMatching);
        return false;
    }
    if(!matchesOperation(expr.code, pattern.operation, cache, cache))
    {
        PROFILE_END(PatternMatching);
        return false;
    }
    if(!matchesValue(expr.arg0, pattern.firstArgument, cache, cache))
    {
        PROFILE_END(PatternMatching);
        return false;
    }
    if(!matchesValue(expr.arg1, pattern.secondArgument, cache, cache))
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
    MatchCache globalCache{};
    auto it = start;
    for(auto& part : pattern.parts)
    {
        if(it.isEndOfBlock())
            return InstructionWalker{};
        // we don't need to distinguish here between the caches, since any failure will immediately abort the whole
        // search
        if(!matchesOnly(it.get(), part, globalCache, globalCache))
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
    MatchCache globalCache{};
    auto it = start;
    FastAccessList<intermediate::IntermediateInstruction*> matchingInstructions;
    matchingInstructions.reserve(pattern.parts.size());
    for(auto& part : pattern.parts)
    {
        MatchCache localCache{};
        while(!it.isEndOfBlock() && !matchesOnly(it.get(), part, globalCache, localCache))
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
            // clear local cache, since all the content is wrong anyway
            localCache.clear();
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

        // merge local into global cache for the next instructions to check
        globalCache.insert(std::make_move_iterator(localCache.begin()), std::make_move_iterator(localCache.end()));
        localCache.clear();
        matchingInstructions.emplace_back(it.get());

        it.nextInBlock();
    }

    // we matched so far, now update the values
    if(matchingInstructions.size() != pattern.parts.size())
        return InstructionWalker{};

    for(unsigned i = 0; i < pattern.parts.size(); ++i)
        updateOnly(matchingInstructions[i], pattern.parts[i]);

    return start;
}

InstructionWalker pattern::search(InstructionWalker start, Pattern& pattern)
{
    if(pattern.parts.empty())
        return InstructionWalker{};

    PROFILE_START(PatternMatching);

    while(!start.isEndOfBlock())
    {
        MatchCache dummyCache{};
        if(matchesOnly(start.get(), pattern.parts.front(), dummyCache, dummyCache))
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
