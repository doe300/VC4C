/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMPARISONS_H
#define COMPARISONS_H

#include "../InstructionWalker.h"

namespace vc4c
{
	namespace intermediate
	{
		//LLVM relational operators
		InstructionWalker intrinsifyIntegerRelation(Method& method, InstructionWalker it, const Comparison* comp);
		InstructionWalker intrinsifyFloatingRelation(Method& method, InstructionWalker it, const Comparison* comp);

		InstructionWalker insertIsNegative(InstructionWalker it, const Value& src, Value& dest);
	} // namespace intermediate
} // namespace vc4c

#endif /* COMPARISONS_H */

