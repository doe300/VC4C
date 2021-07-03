/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LONG_OPERATIONS_H
#define VC4C_LONG_OPERATIONS_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace normalization
    {
        std::pair<Value, Value> getLowerAndUpperWords(const Value& longValue);
        void lowerLongOperation(Module& module, Method& method, InstructionWalker it, const Configuration& config);
    } // namespace normalization
} // namespace vc4c

#endif /* VC4C_LONG_OPERATIONS_H */