/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMPARISONS_H
#define COMPARISONS_H

#include "../Module.h"
#include "../InstructionWalker.h"

namespace vc4c
{

	namespace intermediate
	{
		//LLVM relational operators
		InstructionWalker intrinsifyIntegerRelation(Method& method, InstructionWalker it, const Comparison* comp);
		InstructionWalker intrinsifyFloatingRelation(Method& method, InstructionWalker it, const Comparison* comp);

		InstructionWalker insertIsNegative(InstructionWalker it, const Value& src, Value& dest);
	}
}

#endif /* COMPARISONS_H */

