/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_OPTIMIZATION_CONTROLFLOW_H
#define VC4C_OPTIMIZATION_CONTROLFLOW_H

#include "../Module.h"

namespace vc4c
{
	namespace optimizations
	{
		/*
		 * Tries to find loops which then can be vectorized by combining multiple iterations into one.
		 *
		 * NOTE: Currently only works with "standard" for-range loops
		 */
		void vectorizeLoops(const Module& module, Method& method, const Configuration& config);

	} /* namespace optimizations */
} /* namespace vc4c */

#endif /* VC4C_OPTIMIZATION_CONTROLFLOW_H */
