/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INTRINSICS_H
#define INTRINSICS_H

#include "../InstructionWalker.h"
#include "config.h"

#include <memory>
#include <string>

namespace vc4c
{
	namespace optimizations
	{
		InstructionWalker intrinsify(const Module& module, Method& method, InstructionWalker it, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c
#endif /* INTRINSICS_H */

