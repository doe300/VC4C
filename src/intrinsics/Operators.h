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

		/*
		 * Implementations for on-host calculations
		 */

		Literal asr(const DataType& type, const Literal& left, const Literal& right);
		Literal clz(const DataType& type, const Literal& val);
		/*
		 * OpSMod: "Signed modulo operation of Operand 1 modulo Operand 2. The sign of a non-0 result comes from Operand 2."
		 */
		Literal smod(const DataType& type, const Literal& numerator, const Literal& denominator);
		/*
		 * OpSRem: "Signed remainder operation of Operand 1 divided by Operand 2. The sign of a non-0 result comes from Operand 1."
		 */
		Literal srem(const DataType& type, const Literal& numerator, const Literal& denominator);
		/*
		 * OpFMod: "Floating-point remainder operation of Operand 1 divided by Operand 2. The sign of a non-0 result comes from Operand 2."
		 */
		Literal fmod(const DataType& type, const Literal& numerator, const Literal& denominator);
		/*
		 * OpFRem: "Floating-point remainder operation of Operand 1 divided by Operand 2. The sign of a non-0 result comes from	Operand 1."
		 */
		Literal frem(const DataType& type, const Literal& numerator, const Literal& denominator);

	} // namespace intermediate
} // namespace vc4c

#endif /* OPERATORS_H */

