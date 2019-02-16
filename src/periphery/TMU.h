/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TMU_H
#define VC4C_TMU_H

#include "../Method.h"
#include "../asm/OpCodes.h"

namespace vc4c
{
    namespace periphery
    {
        const Value TMU_READ_REGISTER(REG_TMU_OUT, TYPE_UNKNOWN);

        struct TMU
        {
            const Register s_coordinate;
            const Register t_coordinate;
            const Register r_border_color;
            const Register b_lod_bias;
            const Signaling signal;

            inline Value getAddress(DataType type) const
            {
                return Value(s_coordinate, type);
            }

            inline Value getXCoord(DataType type = TYPE_FLOAT) const
            {
                return Value(s_coordinate, type);
            }

            inline Value getYCoord(DataType type = TYPE_FLOAT) const
            {
                return Value(t_coordinate, type);
            }
        };

        extern const TMU TMU0;
        extern const TMU TMU1;

        /*
         * TMU
         *
         * Every QPU has access to 2 TMUs (TMU_0 and TMU_1) and can queue up to 4 requests to each of the TMUs.
         * The VC4 has 2 TMUs shared by all QPUs on a slice. Nevertheless, querying memory via a TMU does not require a
         * mutex-lock. The TMU memory-lookup is per-element, so element X will return the value from the address  given
         * by element X, write 0 per element to disable.
         */

        /*
         * Perform a general memory lookup via the TMU.
         *
         * Reads the required number of 32-bit vectors from the given address via the TMU and performs the necessary
         * type conversions and vector rotations (for non 32-bit types).
         */
        NODISCARD InstructionWalker insertReadVectorFromTMU(
            Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu = TMU0);

        /*
         * Inserts a read via TMU from the given image-parameter at the coordinates x, y (y optional), which need to be
         * converted to [0, 1] prior to this call and stores the result in dest.
         */
        NODISCARD InstructionWalker insertReadTMU(Method& method, InstructionWalker it, const Value& image,
            const Value& dest, const Value& xCoord, const Optional<Value>& yCoord = NO_VALUE, const TMU& tmu = TMU0);

    } /* namespace periphery */
} /* namespace vc4c */

#endif /* VC4C_TMU_H */
