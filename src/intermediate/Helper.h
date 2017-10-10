/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_HELPER_H
#define INSTRUCTION_HELPER_H

#include "../Module.h"
#include "../InstructionWalker.h"

namespace vc4c
{
	namespace intermediate
	{
		enum class Direction
		{
			UP, DOWN
		};
		InstructionWalker insertVectorRotation(InstructionWalker it, const Value& src, const Value& offset, const Value& dest, const Direction direction = Direction::UP);

		InstructionWalker insertReplication(InstructionWalker it, const Value& src, const Value& dest, const bool useDestionation = true);

		InstructionWalker insertVectorExtraction(InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest);
		InstructionWalker insertVectorInsertion(InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& value);
		InstructionWalker insertVectorShuffle(InstructionWalker it, Method& method, const Value& destination, const Value& source0, const Value& source1, const Value& mask);

		InstructionWalker insertSFUCall(const Register sfuReg, InstructionWalker it, const Value& arg, const ConditionCode cond = COND_ALWAYS, const SetFlag setFlags = SetFlag::DONT_SET);

		InstructionWalker insertZeroExtension(InstructionWalker it, Method& method, const Value& src, const Value& dest, const ConditionCode conditional = COND_ALWAYS, const SetFlag setFlags =
				SetFlag::DONT_SET);
		InstructionWalker insertSignExtension(InstructionWalker it, Method& method, const Value& src, const Value& dest, const ConditionCode conditional = COND_ALWAYS, const SetFlag setFlags =
				SetFlag::DONT_SET);
		InstructionWalker insertSaturation(InstructionWalker it, Method& method, const Value& src, const Value& dest, bool isSigned);

		InstructionWalker insertMakePositive(InstructionWalker it, Method& method, const Value& src, Value& dest);
		InstructionWalker insertInvertSign(InstructionWalker it, Method& method, const Value& src, Value& dest, const ConditionCode cond = COND_ALWAYS);

		InstructionWalker insertCalculateIndices(InstructionWalker it, Method& method, const Value& container, const Value& dest, const std::vector<Value>& indices, const bool firstIndexIsElement = false);
	}
}

#endif /* INSTRUCTION_HELPER_H */

