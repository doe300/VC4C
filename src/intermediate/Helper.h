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

		/*
		 * After this function returns, dest will contain the positive value of src (either src or it's tow's compliment)
		 * and writeIsNegative will return whether the src was negative (-1 if negative, 0 otherwise)
		 */
		InstructionWalker insertMakePositive(InstructionWalker it, Method& method, const Value& src, Value& dest, Value& writeIsNegative);
		/*
		 * Restores the original sign to the value in src and writes into dest according to the value of sign.
		 * Sign is -1 to restore a negative value and 0 to restore a positive value.
		 *
		 * NOTE: src is required to be unsigned!
		 */
		InstructionWalker insertRestoreSign(InstructionWalker it, Method& method, const Value& src, Value& dest, const Value& sign);

		InstructionWalker insertCalculateIndices(InstructionWalker it, Method& method, const Value& container, const Value& dest, const std::vector<Value>& indices, bool firstIndexIsElement = false);

		InstructionWalker insertByteSwap(InstructionWalker it, Method& method, const Value& src, const Value& dest);
	} // namespace intermediate
} // namespace vc4c

#endif /* INSTRUCTION_HELPER_H */

