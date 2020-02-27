/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_COMPARISONS_H
#define VC4C_COMPARISONS_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace intrinsics
    {
        // relational operators
        bool intrinsifyComparison(Method& method, InstructionWalker it);
    } // namespace intrinsics
} // namespace vc4c

#endif /* VC4C_COMPARISONS_H */
