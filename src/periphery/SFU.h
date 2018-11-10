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
        NODISCARD InstructionWalker insertSFUCall(Register sfuReg, InstructionWalker it, const Value& arg);

        /*
         * Tries to convert the constant result for the SFU call and the given input
         */
        Optional<Value> precalculateSFU(Register sfuReg, const Value& input);

    } /* namespace periphery */
} /* namespace vc4c */

#endif /* VC4C_SFU_H */
