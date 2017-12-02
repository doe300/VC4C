/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TYPE_CONVERSIONS_H
#define TYPE_CONVERSIONS_H

#include "../InstructionWalker.h"

namespace vc4c
{
	namespace intermediate
	{
		InstructionWalker insertZeroExtension(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS, SetFlag setFlags =
				SetFlag::DONT_SET);
		InstructionWalker insertSignExtension(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS, SetFlag setFlags =
				SetFlag::DONT_SET);
		InstructionWalker insertSaturation(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool isSigned);
		InstructionWalker insertTruncate(InstructionWalker it, Method& method, const Value& src, const Value& dest);
	} /* namespace intermediate */

} /* namespace vc4c */

#endif /* TYPE_CONVERSIONS_H */
