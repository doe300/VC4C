/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Rewrite.h"

#include "../InstructionWalker.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;

static RegisterFile getFixedRegisterFile(const Value& val)
{
    if(val.hasRegister())
    {
        if(val.reg().file == RegisterFile::PHYSICAL_A || val.reg().file == RegisterFile::PHYSICAL_B)
            return val.reg().file;
    }
    else if(val.isLiteralValue())
        return RegisterFile::PHYSICAL_B;
    else if(val.hasLocal())
    {
        for(const auto& user : val.local()->getUsers())
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
    logging::debug() << "Found instruction with conflicting fixed registers: " << it->to_string() << logging::endl;

    if(it->hasUnpackMode())
        // TODO if this instruction unpacks, then we need to insert the copy somewhere else
        // XXX can an instruction which unpacks take multiple parameters?
        throw CompilationError(CompilationStep::NORMALIZER, "Unhandled case of register conflicts", it->to_string());

    // otherwise, we can copy one of the arguments into a temporary
    auto& fixedArg =
        *std::find_if(fixedArgs.begin(), fixedArgs.end(), [](const Value& val) -> bool { return val.hasLocal(); });
    auto tmp = method.addNewLocal(fixedArg.type);
    it.emplace(new intermediate::MoveOperation(tmp, fixedArg, it->conditional));
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
        if(arg.hasLocal())
            anyLocalArgument = true;
    }
    if(hasRegisterConflict && fixedArgs.size() > 1 && anyLocalArgument)
        return resolveRegisterConflicts(method, it, fixedArgs);

    return it;
}