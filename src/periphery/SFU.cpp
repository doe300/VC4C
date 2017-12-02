/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SFU.h"

using namespace vc4c;
using namespace vc4c::periphery;

InstructionWalker periphery::insertSFUCall(const Register sfuReg, InstructionWalker it, const Value& arg, const ConditionCode cond, const SetFlag setFlags)
{
    //TODO need to synchronize SFU ?? (per slice!)
	//Also need to include the reading of r4. And if this is enclosed in mutex, the NOPs are no longer replaced?
    //1. move argument to SFU register
    it.emplace( new intermediate::MoveOperation(Value(sfuReg, TYPE_FLOAT), arg, cond, setFlags));
    it.nextInBlock();
    //2. wait 2 instructions / don't touch r4
    it.emplace( new intermediate::Nop(intermediate::DelayType::WAIT_SFU));
    it.nextInBlock();
    it.emplace( new intermediate::Nop(intermediate::DelayType::WAIT_SFU));
    it.nextInBlock();
    return it;
}
