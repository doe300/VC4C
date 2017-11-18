/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SemaphoreInstruction.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

SemaphoreInstruction::SemaphoreInstruction(const Pack pack, const ConditionCode condAdd, const ConditionCode condMul, 
                                           const SetFlag sf, const WriteSwap ws, const Address addOut, const Address mulOut, 
                                           const bool increment, const Semaphore semaphore)
{
    setEntry(OpSemaphore::SEMAPHORE, 57, MASK_Septuple);
    setPack(pack);
    setAddCondition(condAdd);
    setMulCondition(condMul);
    setSetFlag(sf);
    setWriteSwap(ws);
    setAddOut(addOut);
    setMulOut(mulOut);
    setIncrementSemaphore(increment);
    setSemaphore(semaphore);
}

std::string SemaphoreInstruction::toASMString() const
{
    std::string result(toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag(), UNPACK_NOP, getPack()));
    if(getIncrementSemaphore())
        return std::string("sacq") + (result + " ") + (toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") + std::to_string(static_cast<unsigned char>(getSemaphore()));
    return std::string("srel") + (result + " ") + (toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", ") + std::to_string(static_cast<unsigned char>(getSemaphore()));
}
