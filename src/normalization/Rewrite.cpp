/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Rewrite.h"

#include "../InstructionWalker.h"
#include "../intermediate/operators.h"
#include "LiteralValues.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::operators;

static RegisterFile getFixedRegisterFile(const Value& val)
{
    if(auto reg = val.checkRegister())
    {
        if(reg->file == RegisterFile::PHYSICAL_A || reg->file == RegisterFile::PHYSICAL_B)
            return reg->file;
    }
    else if(val.isLiteralValue())
        return RegisterFile::PHYSICAL_B;
    else if(auto local = val.checkLocal())
    {
        for(const auto& user : local->getUsers())
        {
            if(user.second.readsLocal() && user.first->hasUnpackMode())
                return RegisterFile::PHYSICAL_A;
            if(user.second.writesLocal() && user.first->hasPackMode())
                return RegisterFile::PHYSICAL_A;
        }
    }

    return RegisterFile::ANY;
}

static NODISCARD InstructionWalker resolveRegisterConflicts(
    Method& method, InstructionWalker it, FastSet<Value>& fixedArgs)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Found instruction with conflicting fixed registers: " << it->to_string() << logging::endl);

    if(it->hasUnpackMode())
        // TODO if this instruction unpacks, then we need to insert the copy somewhere else
        // XXX can an instruction which unpacks take multiple parameters?
        throw CompilationError(CompilationStep::NORMALIZER, "Unhandled case of register conflicts", it->to_string());

    // otherwise, we can copy one of the arguments into a temporary
    auto& fixedArg =
        *std::find_if(fixedArgs.begin(), fixedArgs.end(), [](const Value& val) -> bool { return val.checkLocal(); });
    auto tmp = method.addNewLocal(fixedArg.type);
    auto cond = (check(it.get<intermediate::ExtendedInstruction>()) & &intermediate::ExtendedInstruction::getCondition)
                    .value_or(COND_ALWAYS);
    it.emplace(new intermediate::MoveOperation(tmp, fixedArg, cond));
    it.nextInBlock();
    it->replaceValue(fixedArg, tmp, LocalUse::Type::READER);
    return it;
}

InstructionWalker normalization::splitRegisterConflicts(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    auto fixedFiles = RegisterFile::NONE;
    FastSet<Value> fixedArgs;
    bool hasRegisterConflict = false;
    bool anyLocalArgument = false;
    for(const Value& arg : it->getArguments())
    {
        auto file = getFixedRegisterFile(arg);
        if(file != RegisterFile::ANY)
        {
            if(has_flag(fixedFiles, file))
                hasRegisterConflict = true;
            fixedArgs.emplace(arg);
            fixedFiles = add_flag(fixedFiles, file);
        }
        if(arg.checkLocal())
            anyLocalArgument = true;
    }
    if(hasRegisterConflict && fixedArgs.size() > 1 && anyLocalArgument)
        return resolveRegisterConflicts(method, it, fixedArgs);

    return it;
}

void normalization::extendBranches(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    // we only need to set the same flag once
    std::tuple<Value, std::bitset<NATIVE_VECTOR_SIZE>> lastSetFlags{UNDEFINED_VALUE, {}};
    while(!it.isEndOfMethod())
    {
        if(auto branchCond = it.get<intermediate::BranchCondition>())
        {
            // TODO can be skipped, if it is checked/guaranteed, that the last instruction setting flags is the
            // boolean-selection for the given condition  but we need to check more than the last instructions,
            // since there could be moves inserted by phi

            // skip setting of flags, if the previous setting wrote the same flags
            if(std::get<0>(lastSetFlags) != branchCond->getBranchCondition() ||
                std::get<1>(lastSetFlags) != branchCond->conditionalElements)
            {
                Value cond = branchCond->getBranchCondition();
                if(auto lit = cond.getLiteralValue())
                {
                    if(auto imm = normalization::toImmediate(*lit))
                        cond = Value(*imm, cond.type);
                    else
                        throw CompilationError(CompilationStep::NORMALIZER,
                            "Unhandled literal value in branch condition", branchCond->to_string());
                }
                /*
                 * branch usually only depends on scalar value
                 * -> set any not used vector-element (all except element 0) to a value where it doesn't influence
                 * the condition
                 *
                 * Using ELEMENT_NUMBER sets the vector-elements 1 to 15 to a non-zero value and 0 to either 0 (if
                 * condition was false) or 1 (if condition was true)
                 */
                if(branchCond->conditionalElements == 0x1)
                    // default case for simple jump on 0th element
                    assign(it, NOP_REGISTER) = (ELEMENT_NUMBER_REGISTER | cond, SetFlag::SET_FLAGS);
                else
                {
                    // more special case for jump on different element(s)
                    auto elementMask = it.getBasicBlock()->getMethod().addNewLocal(TYPE_INT8.toVectorType(16));
                    it.emplace(new intermediate::LoadImmediate(elementMask,
                        static_cast<uint32_t>((~branchCond->conditionalElements).to_ulong()),
                        intermediate::LoadType::PER_ELEMENT_UNSIGNED));
                    it.nextInBlock();
                    assign(it, NOP_REGISTER) = (elementMask | cond, SetFlag::SET_FLAGS);
                }
            }
            std::get<0>(lastSetFlags) = branchCond->getBranchCondition();
            std::get<1>(lastSetFlags) = branchCond->conditionalElements;
            it.erase();
            // to not skip the next instruction
            it.previousInBlock();
        }
        if(auto branch = it.get<intermediate::Branch>())
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Extending branch: " << branch->to_string() << logging::endl);
            // go to next instruction
            it.nextInBlock();
            // insert 3 NOPs before
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
        }
        else if(it.get() && it->doesSetFlag())
        {
            // any other instruction setting flags, need to re-set the branch-condition
            lastSetFlags = {UNDEFINED_VALUE, {}};
        }
        it.nextInMethod();
    }
}
