/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTRINSICS_H
#define INTRINSICS_H

#include <config.h>
#include "../Module.h"
#include "../InstructionWalker.h"

#include <string>
#include <memory>

namespace vc4c
{

	namespace optimizations
	{
		InstructionWalker intrinsify(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
	}
}
#endif /* INTRINSICS_H */

