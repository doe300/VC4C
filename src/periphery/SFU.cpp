/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SFU.h"

#include "../intermediate/operators.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::operators;

InstructionWalker periphery::insertSFUCall(const Register sfuReg, InstructionWalker it, const Value& arg)
{
    // TODO need to synchronize SFU ?? (per slice!)
    // Also need to include the reading of r4. And if this is enclosed in mutex, the NOPs are no longer replaced?
    // 1. move argument to SFU register
    assign(it, Value(sfuReg, TYPE_FLOAT)) = arg;
    // 2. wait 2 instructions / don't touch r4
    nop(it, intermediate::DelayType::WAIT_SFU);
    nop(it, intermediate::DelayType::WAIT_SFU);
    return it;
}

Optional<Value> periphery::precalculateSFU(Register sfuReg, const Value& input)
{
    std::function<Value(const Value&)> elemFunc;
    switch(sfuReg.num)
    {
    case REG_SFU_EXP2.num:
        elemFunc = [](const Value& val) -> Value {
            return Value(Literal(std::exp2(val.getLiteralValue()->real())), TYPE_FLOAT);
        };
        break;
    case REG_SFU_LOG2.num:
        elemFunc = [](const Value& val) -> Value {
            return Value(Literal(std::log2(val.getLiteralValue()->real())), TYPE_FLOAT);
        };
        break;
    case REG_SFU_RECIP.num:
        elemFunc = [](const Value& val) -> Value {
            return Value(Literal(1.0f / val.getLiteralValue()->real()), TYPE_FLOAT);
        };
        break;
    case REG_SFU_RECIP_SQRT.num:
        elemFunc = [](const Value& val) -> Value {
            return Value(Literal(1.0f / std::sqrt(val.getLiteralValue()->real())), TYPE_FLOAT);
        };
        break;
    default:
        throw CompilationError(CompilationStep::GENERAL, "Invalid SFU input register", sfuReg.to_string());
    }

    if(input.getLiteralValue())
        return elemFunc(input);
    if(input.hasContainer())
    {
        Value result(ContainerValue(input.type.getVectorWidth()), input.type);
        for(const auto& elem : input.container().elements)
            result.container().elements.emplace_back(elemFunc(elem));
        return result;
    }
    return NO_VALUE;
}
