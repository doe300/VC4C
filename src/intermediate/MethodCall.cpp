/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../asm/Instruction.h"

using namespace vc4c;
using namespace vc4c::intermediate;

MethodCall::MethodCall(std::string&& methodName, std::vector<Value>&& args) :
    IntermediateInstruction(Optional<Value>{}), methodName(std::move(methodName))
{
    for(std::size_t i = 0; i < args.size(); ++i)
        setArgument(i, std::move(args[i]));
}

MethodCall::MethodCall(Value&& dest, std::string&& methodName, std::vector<Value>&& args) :
    IntermediateInstruction(std::move(dest)), methodName(std::move(methodName))
{
    for(std::size_t i = 0; i < args.size(); ++i)
        setArgument(i, std::move(args[i]));
}

std::string MethodCall::to_string() const
{
    std::string signature = (getReturnType() == TYPE_VOID ? "" : getOutput()->to_string(true) + " = ") +
        (getReturnType().to_string() + " ") + methodName + "(";
    if(!getArguments().empty())
    {
        for(const Value& arg : getArguments())
        {
            signature.append(arg.to_string()).append(", ");
        }
        signature = signature.substr(0, signature.length() - 2);
    }
    signature.append(")");

    return signature + createAdditionalInfoString();
}

IntermediateInstruction* MethodCall::copyFor(Method& method, const std::string& localPrefix) const
{
    std::vector<Value> newArgs;
    newArgs.reserve(getArguments().size());
    for(const Value& arg : getArguments())
    {
        newArgs.push_back(renameValue(method, arg, localPrefix));
    }
    if(getOutput())
        return (new MethodCall(
                    renameValue(method, getOutput().value(), localPrefix), std::string(methodName), std::move(newArgs)))
            ->copyExtrasFrom(this);
    else
        return (new MethodCall(std::string(methodName), std::move(newArgs)))->copyExtrasFrom(this);
}

qpu_asm::DecoratedInstruction MethodCall::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    throw CompilationError(CompilationStep::OPTIMIZER, "There should be no more function-calls", to_string());
}

bool MethodCall::isNormalized() const
{
    return false;
}

const DataType MethodCall::getReturnType() const
{
    if(!getOutput())
        return TYPE_VOID;
    return getOutput()->type;
}

bool MethodCall::matchesSignature(const Method& method) const
{
    if(methodName != method.name)
    {
        return false;
    }
    if(getArguments().size() != method.parameters.size())
    {
        return false;
    }
    if(!getReturnType().containsType(method.returnType))
    {
        return false;
    }
    for(std::size_t i = 0; i < method.parameters.size(); ++i)
    {
        if(!(method.parameters[i].type.containsType(assertArgument(i).type)))
        {
            return false;
        }
    }

    return true;
}

Return::Return(Value&& val) : IntermediateInstruction(Optional<Value>{})
{
    setArgument(0, std::move(val));
}

Return::Return() : IntermediateInstruction(Optional<Value>{}) {}

std::string Return::to_string() const
{
    return std::string("return ") + (getReturnValue() ? getReturnValue().to_string() : "") +
        createAdditionalInfoString();
}

IntermediateInstruction* Return::copyFor(Method& method, const std::string& localPrefix) const
{
    throw CompilationError(CompilationStep::OPTIMIZER, "Return should never be inlined in calling method", to_string());
}

qpu_asm::DecoratedInstruction Return::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    throw CompilationError(
        CompilationStep::LABEL_REGISTER_MAPPING, "There should be no more returns at this point", to_string());
}

bool Return::mapsToASMInstruction() const
{
    return false;
}

bool Return::isNormalized() const
{
    return false;
}

Optional<Value> Return::getReturnValue() const
{
    return getArgument(0);
}
