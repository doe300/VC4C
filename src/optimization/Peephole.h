/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_PEEPHOLE_OPTIMIZATIONS_H
#define VC4C_PEEPHOLE_OPTIMIZATIONS_H

#include "../performance.h"

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;
    struct Register;
    class Local;

    namespace optimizations
    {
        /**
         * Removes useless instructions after the register-mapping is done.
         *
         * The optimizations removes among others:
         * - non-necessary NOPs inserted to split read-after-write where the local is mapped to an accumulator
         * - "simple" moves where source and destination are mapped to the same register, if delay is not required
         *
         * NOTE: In contrast to most other optimization steps, this is run AFTER the locals have been mapped to
         * registers!
         *
         * NOTE: This optimization MUST NOT make any changes that required re-building the register-mapping!
         */
        void removeObsoleteInstructions(const Module& module, Method& kernel, const Configuration& config,
            const FastMap<const Local*, Register>& registerMap);

        /*
         * Combine ALU-instructions which (can) use different ALUs into a single instruction accessing both ALUs.
         * There are two types of instruction-pairs which can be combined:
         * - The instructions do not depend on one another, their relative order unimportant (e.g. output of first
         * instruction is not read in second one)
         * - Both instructions write to the same output but with inverted conditions (see example)
         *
         * This optimization is very similar to #combineOperations() with the difference that at this point the
         * register-mapping is already fix, allowing us to more accurately check for conflicts, but disallows any
         * rewrite of operations that would change the register association.
         *
         * Example (source taken from #combineSelectionWithZero):
         *   %5 = %11 (ifz)
         *   %5 = xor %11, %11 (ifzc)
         *
         * is converted to:
         *   %5 = xor %11, %11 (ifzc) and %5 = v8min %11, %11 (ifz)
         *
         * NOTE: As of this point, the instruction-type CombinedInstruction can occur within a basic block!
         * Also, only moves and ALU instructions are combined at the moment
         */
        void combineRegisterMappedOperations(const Module& module, Method& kernel, const Configuration& config,
            const FastMap<const Local*, Register>& registerMap);

    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_PEEPHOLE_OPTIMIZATIONS_H */
