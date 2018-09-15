/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_INSTRUCTION_SCHEDULER_H
#define VC4C_INSTRUCTION_SCHEDULER_H

#include "../analysis/DependencyGraph.h"
#include "config.h"

namespace vc4c
{
    class Method;
    class Module;

    namespace intermediate
    {
        class IntermediateInstruction;
    }

    namespace optimizations
    {
        bool reorderInstructions(const Module& module, Method& kernel, const Configuration& config);

    } /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_INSTRUCTION_SCHEDULER_H */
