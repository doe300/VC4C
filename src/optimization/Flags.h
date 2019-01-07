/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_OPTIMIZATION_FLAGS
#define VC4C_OPTIMIZATION_FLAGS

#include "config.h"

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;

    namespace optimizations
    {
        /*
         * Removes setting of flags where it is guaranteed that the flags are set always or never.
         * Also removes the setting of flags, if they are never used.
         *
         * All succeeding conditional instructions based on these flags are either also removed or made unconditional,
         * depending on whether they require the flags to be set or cleared.
         *
         * Example:
         *   - = xor 0, 1 (setf)
         *   %1 = %2 (ifz)
         *   %1 = %3 (ifzc)
         *
         * becomes:
         *   %1 = %3
         *
         * Also:
         *   %1 = xor 0, %2 (setf)
         *   [...]
         *   - = xor 0, %4 (setf)
         *
         * becomes:
         *   %1 = xor 0, %2
         *   [...]
         *   - = xor 0, %4 (setf)
         *
         * And:
         *   - = xor 0, 1 (setf)
         *   [...]
         *   - = xor 0, %4 (setf)
         *
         * becomes:
         *   [...]
         *   - = xor 0, %4 (setf)
         */
        bool removeUselessFlags(const Module& module, Method& method, const Configuration& config);
    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_OPTIMIZATION_FLAGS */
