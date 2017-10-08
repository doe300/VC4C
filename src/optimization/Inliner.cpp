/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Inliner.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/Helper.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::optimizations;

static const Method* matchSignatures(const std::vector<std::unique_ptr<Method>>& methods, const intermediate::MethodCall* callSignature)
{
    for(const auto& m : methods)
    {
        if(callSignature->matchesSignature(*m.get()))
        {
            logging::debug() << "Found method matching " << m->returnType.to_string() << ' ' << m->name << " with " << m->parameters.size() << " arguments" << logging::endl;
            return m.get();
        }
    }
    return nullptr;
}

static Method& inlineMethod(const std::string& localPrefix, const std::vector<std::unique_ptr<Method>>& methods, Method& currentMethod)
{
	auto it = currentMethod.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        //Find all method calls
        intermediate::MethodCall* call = it.get<intermediate::MethodCall>();
        if(call != nullptr)
        {
            //search for method with matching signature
            const Method* calledMethod = matchSignatures(methods, call);
            if(calledMethod != nullptr)
            {
                const std::size_t numInstructions = currentMethod.countInstructions();
                //recursively search for used methods
                const std::string newLocalPrefix = localPrefix + (!(call->getReturnType() == TYPE_VOID) ? call->getOutput().get().local->name : std::string("%") + (calledMethod->name + ".") + std::to_string(rand())) + '.';
                const Local* methodEndLabel = currentMethod.findOrCreateLocal(TYPE_LABEL, newLocalPrefix + "after");
                inlineMethod(newLocalPrefix, methods, const_cast<Method&>(*calledMethod));
                //at this point, the called method has already inlined all other methods
            
                //Starting at lowest level (here), insert in parent
                //map parameters to arguments
                for(std::size_t i = 0; i < call->getArguments().size(); ++i)
                {
                	const Parameter& param = calledMethod->parameters.at(i);
                    const Value ref = currentMethod.findOrCreateLocal(param.type, newLocalPrefix + param.name)->createReference();
                    if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
					{
						it = intermediate::insertSignExtension(it, currentMethod, call->getArgument(i), ref);
					}
					else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
					{
						it = intermediate::insertZeroExtension(it, currentMethod, call->getArgument(i), ref);
					}
					else
					{
						it.emplace(new intermediate::MoveOperation(ref, call->getArgument(i)));
						it.nextInMethod();
					}
                }
                //add parameters and locals to locals of parent
                for(const Parameter& arg : calledMethod->parameters)
                {
                	currentMethod.findOrCreateLocal(arg.type, newLocalPrefix + arg.name);
                }
                //TODO maybe this is not needed at all? Since IntermediateInstruction#copyFor already creates all used locals
                for(const auto& pair : calledMethod->readLocals())
                {
                	currentMethod.findOrCreateLocal(pair.second.type, newLocalPrefix + pair.second.name);
                }
                //insert instructions
                calledMethod->forAllInstructions([&it, &currentMethod, &methodEndLabel, &newLocalPrefix, &call](const intermediate::IntermediateInstruction* instr) -> void
                {
                    const intermediate::Return* ret = dynamic_cast<const intermediate::Return*>(instr);
                    if(ret != nullptr)
                    {
                        if(ret->getReturnValue())
                        {
                            //prefix locals with destination of call
                            //map return-value to destination
                            Value retVal(ret->getReturnValue().get());
                            if(retVal.hasType(ValueType::LOCAL))
                            {
                                retVal.local = const_cast<Local*>(currentMethod.findOrCreateLocal(retVal.type, newLocalPrefix + retVal.local->name));
                            }
                            it.emplace(new intermediate::MoveOperation(call->getOutput(), retVal));
                            it.nextInMethod();
                        }
                        //after each return, jump to label after call-site (since there may be several return statements in a method)
                        it.emplace(new intermediate::Branch(methodEndLabel, COND_ALWAYS, BOOL_TRUE));
                    }
                    else
                    {
                        //prefix locals with destination of call
                        //copy instructions
                    	if(dynamic_cast<const intermediate::BranchLabel*>(instr) != nullptr)
                    		it = currentMethod.emplaceLabel(it, dynamic_cast<intermediate::BranchLabel*>(instr->copyFor(currentMethod, newLocalPrefix)));
                    	else
                    		it.emplace(instr->copyFor(currentMethod, newLocalPrefix));
                    }
                    it.nextInMethod();
                });
                if(it.get() != call)
                {
                    throw CompilationError(CompilationStep::OPTIMIZER, "Method call expected, got", it->to_string());
                }
                logging::debug() << "Function body for " << call->to_string() << " inlined, added " << (currentMethod.countInstructions() - 1 - numInstructions) << " instructions" << logging::endl;
                //replace method-call from parent with label to jump to (for returns)
                it = it.erase();
                it = currentMethod.emplaceLabel(it, new intermediate::BranchLabel(*methodEndLabel));
            }
        }
        it.nextInMethod();
    }
    
    return currentMethod;
}

void optimizations::inlineMethods(const Module& module, Method& kernel, const Configuration& config)
{
    logging::info() << "-----" << logging::endl;
    logging::info() << "Inlining functions for kernel: " << kernel.name << logging::endl;
    //Starting at kernel
    inlineMethod("", module.methods, kernel);
    logging::info() << "-----" << logging::endl;
}
