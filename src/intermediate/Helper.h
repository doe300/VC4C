/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_HELPER_H
#define INSTRUCTION_HELPER_H

#include "../InstructionWalker.h"

namespace vc4c
{
	namespace intermediate
	{
		enum class Direction
		{
			UP, DOWN
		};
		InstructionWalker insertVectorRotation(InstructionWalker it, const Value& src, const Value& offset, const Value& dest, Direction direction = Direction::UP);

		InstructionWalker insertReplication(InstructionWalker it, const Value& src, const Value& dest, bool useDestionation = true);

		InstructionWalker insertVectorExtraction(InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest);
		InstructionWalker insertVectorInsertion(InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& value);
		InstructionWalker insertVectorShuffle(InstructionWalker it, Method& method, const Value& destination, const Value& source0, const Value& source1, const Value& mask);

		InstructionWalker insertSFUCall(Register sfuReg, InstructionWalker it, const Value& arg, ConditionCode cond = COND_ALWAYS, SetFlag setFlags = SetFlag::DONT_SET);

		InstructionWalker insertZeroExtension(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS, SetFlag setFlags =
				SetFlag::DONT_SET);
		InstructionWalker insertSignExtension(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool allowLiteral, ConditionCode conditional = COND_ALWAYS, SetFlag setFlags =
				SetFlag::DONT_SET);
		InstructionWalker insertSaturation(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool isSigned);

		InstructionWalker insertMakePositive(InstructionWalker it, Method& method, const Value& src, Value& dest);
		InstructionWalker insertInvertSign(InstructionWalker it, Method& method, const Value& src, Value& dest, ConditionCode cond = COND_ALWAYS);

		InstructionWalker insertCalculateIndices(InstructionWalker it, Method& method, const Value& container, const Value& dest, const std::vector<Value>& indices, bool firstIndexIsElement = false);
	} // namespace intermediate
} // namespace vc4c

#endif /* INSTRUCTION_HELPER_H */

