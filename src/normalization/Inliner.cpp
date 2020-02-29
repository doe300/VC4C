/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Inliner.h"

#include "../Module.h"
#include "../Profiler.h"
#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/TypeConversions.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;

static const Method* matchSignatures(
    const std::vector<std::unique_ptr<Method>>& methods, const intermediate::MethodCall* callSignature)
{
    for(const auto& m : methods)
    {
        if(callSignature->matchesSignature(*m))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found method matching " << m->returnType.to_string() << ' ' << m->name << " with "
                    << m->parameters.size() << " arguments" << logging::endl);
            return m.get();
        }
    }
    return nullptr;
}

static Method& inlineMethod(const std::string& localPrefix, const std::vector<std::unique_ptr<Method>>& methods,
    const FastMap<std::string, std::string>& functionAliases, Method& currentMethod)
{
    auto it = currentMethod.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        // Find all method calls
        if(auto call = it.get<intermediate::MethodCall>())
        {
            // search for method with matching signature
            auto calledMethod = matchSignatures(methods, call);
            if(!calledMethod)
            {
                // if not find directly, try aliasing
                auto aliasIt = functionAliases.find(call->methodName);
                if(aliasIt != functionAliases.end())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Using alias '" << aliasIt->second << "' for call-site: " << call->to_string()
                            << logging::endl);
                    // we need to rewrite the call-site function name, since this is checked in
                    // CallSite#matchesSignature(...)
                    call->methodName = aliasIt->second;
                    calledMethod = matchSignatures(methods, call);
                }
            }
            if(calledMethod)
            {
                const std::size_t numInstructions = currentMethod.countInstructions();
                // recursively search for used methods
                const std::string newLocalPrefix = localPrefix +
                    (!(call->getReturnType() == TYPE_VOID) ?
                            call->getOutput()->local()->name :
                            std::string("%") + (calledMethod->name + ".") + std::to_string(rand())) +
                    '.';
                const Local* methodEndLabel = currentMethod.createLocal(TYPE_LABEL, newLocalPrefix + "after");
                inlineMethod(newLocalPrefix, methods, functionAliases, const_cast<Method&>(*calledMethod));
                // at this point, the called method has already inlined all other methods

                intermediate::InlineMapping mapping;
                // the number of instructions is a good guess for the number of locals used
                mapping.reserve(calledMethod->countInstructions());
                // Starting at lowest level (here), insert in parent
                // map parameters to arguments
                for(std::size_t i = 0; i < call->getArguments().size(); ++i)
                {
                    auto callArg = call->assertArgument(i);
                    const Parameter& param = calledMethod->parameters.at(i);
                    auto ref = currentMethod.createLocal(param.type, newLocalPrefix + param.name)->createReference();
                    mapping.emplace(&param, ref.local());
                    if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
                        it = intermediate::insertSignExtension(it, currentMethod, callArg, ref, true);
                    else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
                        it = intermediate::insertZeroExtension(it, currentMethod, callArg, ref, true);
                    else
                    {
                        it.emplace(new intermediate::MoveOperation(ref, callArg));
                        it.nextInMethod();
                    }
                    if(ref.checkLocal() && callArg.checkLocal() && callArg.type.getPointerType())
                        ref.local()->set(ReferenceData(*callArg.local()->getBase(false), 0));
                }
                // add parameters and locals to locals of parent
                for(const Parameter& arg : calledMethod->parameters)
                {
                    if(mapping.find(&arg) == mapping.end())
                        mapping.emplace(&arg, currentMethod.createLocal(arg.type, newLocalPrefix + arg.name));
                }
                // insert instructions
                calledMethod->forAllInstructions([&](const intermediate::IntermediateInstruction& instr) -> void {
                    if(auto ret = dynamic_cast<const intermediate::Return*>(&instr))
                    {
                        if(auto retVal = ret->getReturnValue())
                        {
                            // prefix locals with destination of call
                            // map return-value to destination
                            if(auto retLoc = retVal->checkLocal())
                            {
                                auto it = mapping.find(retLoc);
                                if(it != mapping.end())
                                    retVal->local() = const_cast<Local*>(it->second);
                                else
                                    retVal->local() = const_cast<Local*>(
                                        currentMethod.createLocal(retVal->type, newLocalPrefix + retLoc->name));
                            }
                            it.emplace(new intermediate::MoveOperation(call->getOutput().value(), *retVal));
                            it.nextInMethod();
                        }
                        // after each return, jump to label after call-site (since there may be several return
                        // statements in a method)
                        it.emplace(new intermediate::Branch(methodEndLabel, COND_ALWAYS, BOOL_TRUE));
                    }
                    else
                    {
                        // prefix locals with destination of call
                        // copy instructions
                        if(dynamic_cast<const intermediate::BranchLabel*>(&instr) != nullptr)
                            it = currentMethod.emplaceLabel(it,
                                dynamic_cast<intermediate::BranchLabel*>(
                                    instr.copyFor(currentMethod, newLocalPrefix, mapping)));
                        else
                            it.emplace(instr.copyFor(currentMethod, newLocalPrefix, mapping));
                    }
                    it.nextInMethod();
                });
                if(it.get() != call)
                {
                    throw CompilationError(CompilationStep::OPTIMIZER, "Method call expected, got", it->to_string());
                }
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Function body for " << call->to_string() << " inlined, added "
                        << (currentMethod.countInstructions() - 1 - numInstructions) << " instructions"
                        << logging::endl);
                // replace method-call from parent with label to jump to (for returns)
                it = it.erase();
                auto copyIt = it.copy().previousInMethod();
                it = currentMethod.emplaceLabel(it, new intermediate::BranchLabel(*methodEndLabel));

                // fix-up to immediately remove branches from return to %end_of_function when consecutive instructions
                if(copyIt.get<intermediate::Branch>() &&
                    copyIt.get<intermediate::Branch>()->getTarget() == methodEndLabel)
                    copyIt.erase();
            }
        }
        it.nextInMethod();
    }

    return currentMethod;
}

void normalization::inlineMethods(const Module& module, Method& kernel, const Configuration& config)
{
    CPPLOG_LAZY(logging::Level::INFO, log << "-----" << logging::endl);
    CPPLOG_LAZY(logging::Level::INFO, log << "Inlining functions for kernel: " << kernel.name << logging::endl);
    // Starting at kernel
    inlineMethod("", module.methods, module.functionAliases, kernel);
    CPPLOG_LAZY(logging::Level::INFO, log << "-----" << logging::endl);
}
