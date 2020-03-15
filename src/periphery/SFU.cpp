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

Literal periphery::precalculateSFUExp2(Literal in)
{
    return Literal(std::exp2(in.real()));
}

Literal periphery::precalculateSFULog2(Literal in)
{
    return Literal(std::log2(in.real()));
}

Literal periphery::precalculateSFURecip(Literal in)
{
    if(in.real() == 0.0f)
        return Literal(std::numeric_limits<float>::infinity());
    return Literal(1.0f / in.real());
}

Literal periphery::precalculateSFURecipSqrt(Literal in)
{
    if(in.real() == 0.0f)
        return Literal(std::numeric_limits<float>::infinity());
    return Literal(1.0f / std::sqrt(in.real()));
}

Optional<Value> periphery::precalculateSFU(Register sfuReg, const Value& input)
{
    std::function<Literal(const Literal&)> elemFunc;
    switch(sfuReg.num)
    {
    case REG_SFU_EXP2.num:
        elemFunc = precalculateSFUExp2;
        break;
    case REG_SFU_LOG2.num:
        elemFunc = precalculateSFULog2;
        break;
    case REG_SFU_RECIP.num:
        elemFunc = precalculateSFURecip;
        break;
    case REG_SFU_RECIP_SQRT.num:
        elemFunc = precalculateSFURecipSqrt;
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
