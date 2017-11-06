/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTERMEDIATE_TMU_H
#define INTERMEDIATE_TMU_H

#include "../Bitfield.h"
#include "../Types.h"
#include "../Module.h"

namespace vc4c
{
	namespace periphery
	{
		const Value TMU_READ_REGISTER(REG_TMU_OUT, TYPE_UNKNOWN);
		const Value TMU_GENERAL_READ_ADDRESS(REG_TMU_ADDRESS, TYPE_INT32.toVectorType(16).toPointerType());
		const Value TMU_COORD_S_REGISTER(REG_TMU_COORD_S_U_X, TYPE_FLOAT.toVectorType(16));
		const Value TMU_COORD_T_REGISTER(REG_TMU_COORD_T_V_Y, TYPE_FLOAT.toVectorType(16));

		/*
		 * Perform a general 32-bit memory lookup via the TMU.
		 *
		 * Actually, 16 separate 32-bit memory loads are performed for the 16 elements of the address-vector.
		 */
		InstructionWalker insertGeneralReadTMU(InstructionWalker it, const Value& dest, const Value& addr);

		/*
		 * Inserts a read via TMU from the given image-parameter at the coordinates x, y (y optional), which need to be converted to [0, 1] prior to this call
		 * and stores the result in dest.
		 */
		InstructionWalker insertReadTMU(Method& method, InstructionWalker it, const Value& image, const Value& dest, const Value& xCoord, const Optional<Value>& yCoord = NO_VALUE);

	} /* namespace periphery */
} /* namespace vc4c */

#endif /* INTERMEDIATE_TMU_H */
