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

    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_PEEPHOLE_OPTIMIZATIONS_H */
