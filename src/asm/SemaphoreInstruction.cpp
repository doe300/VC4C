/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SemaphoreInstruction.h"

using namespace vc4c;
using namespace vc4c::qpu_asm;

SemaphoreInstruction::SemaphoreInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf,
    WriteSwap ws, Address addOut, Address mulOut, bool increment, Semaphore semaphore)
{
    setEntry(OpSemaphore::SEMAPHORE, 57, MASK_Septuple);
    setPack(pack);
    setAddCondition(condAdd);
    setMulCondition(condMul);
    setSetFlag(sf);
    setWriteSwap(ws);
    setAddOut(addOut);
    setMulOut(mulOut);
    setAcquire(!increment);
    setSemaphore(semaphore);
}

LCOV_EXCL_START
std::string SemaphoreInstruction::toASMString() const
{
    std::string s;
    std::string result(toExtrasString(SIGNAL_NONE, getAddCondition(), getSetFlag(), UNPACK_NOP, getPack()));
    auto cmd = getAcquire() ? "sacq" : "srel";
    return cmd + result + " " + toOutputRegister(getWriteSwap() == WriteSwap::DONT_SWAP, getAddOut()) + ", " +
        std::to_string(static_cast<unsigned char>(getSemaphore()));
}
LCOV_EXCL_STOP
