/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INLINER_H
#define INLINER_H

#include <config.h>
#include "../Module.h"

#include <vector>
#include <string>

namespace vc4c
{
	namespace optimizations
	{
		void inlineMethods(const Module& module, Method& kernel, const Configuration& config);
	}
}

#endif /* INLINER_H */

