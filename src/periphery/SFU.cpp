/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SFU.h"

#include "../SIMDVector.h"
#include "../intermediate/operators.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::periphery;
using namespace vc4c::operators;

InstructionWalker periphery::insertSFUCall(const Register sfuReg, InstructionWalker it, const Value& arg)
{
    // 1. move argument to SFU register
    assign(it, Value(sfuReg, TYPE_FLOAT)) = arg;
    // 2. wait 2 instructions / don't touch r4
    nop(it, intermediate::DelayType::WAIT_SFU);
    nop(it, intermediate::DelayType::WAIT_SFU);
    return it;
}

Optional<Value> periphery::precalculateSFU(Register sfuReg, const Value& input)
{
    std::function<Literal(const Literal&)> elemFunc;
    switch(sfuReg.num)
    {
    case REG_SFU_EXP2.num:
        elemFunc = [](const Literal& val) -> Literal { return Literal(std::exp2(val.real())); };
        break;
    case REG_SFU_LOG2.num:
        elemFunc = [](const Literal& val) -> Literal { return Literal(std::log2(val.real())); };
        break;
    case REG_SFU_RECIP.num:
        elemFunc = [](const Literal& val) -> Literal { return Literal(1.0f / val.real()); };
        break;
    case REG_SFU_RECIP_SQRT.num:
        elemFunc = [](const Literal& val) -> Literal { return Literal(1.0f / std::sqrt(val.real())); };
        break;
    default:
        throw CompilationError(CompilationStep::GENERAL, "Invalid SFU input register", sfuReg.to_string());
    }

    if(auto lit = input.getLiteralValue())
        return Value(elemFunc(*lit), TYPE_FLOAT);
    if(auto vector = input.checkVector())
    {
        SIMDVector result = vector->transform(elemFunc);
        return SIMDVectorHolder::storeVector(std::move(result), input.type, vector->getStorage());
    }
    return NO_VALUE;
}
