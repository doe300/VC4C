/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPERATORS_H
#define OPERATORS_H

#include "../InstructionWalker.h"

namespace vc4c
{
	namespace intermediate
	{
		InstructionWalker intrinsifySignedIntegerMultiplication(Method& method, InstructionWalker it, Operation& op);
		InstructionWalker intrinsifyUnsignedIntegerMultiplication(Method& method, InstructionWalker it, Operation& op);
		InstructionWalker intrinsifySignedIntegerDivision(Method& method, InstructionWalker it, Operation& op, bool useRemainder = false);
		InstructionWalker intrinsifyUnsignedIntegerDivision(Method& method, InstructionWalker it, Operation& op, bool useRemainder = false);

		InstructionWalker intrinsifyFloatingDivision(Method& method, InstructionWalker it, Operation& op);
	} // namespace intermediate
} // namespace vc4c

#endif /* OPERATORS_H */

