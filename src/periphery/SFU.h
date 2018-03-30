/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_SFU_H
#define VC4C_SFU_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace periphery
    {
        /*
         * Inserts a SFU call to the given SFU register at the position specified.
         * Also inserts the nop-instructions required to wait for the result being available.
         */
        InstructionWalker insertSFUCall(Register sfuReg, InstructionWalker it, const Value& arg,
            ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);

    } /* namespace periphery */
} /* namespace vc4c */

#endif /* VC4C_SFU_H */
