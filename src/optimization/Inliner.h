/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INLINER_H
#define INLINER_H

#include "config.h"

namespace vc4c
{
	class Method;
	class Module;

	namespace optimizations
	{
		void inlineMethods(const Module& module, Method& kernel, const Configuration& config);
	} // namespace optimizations
} // namespace vc4c

#endif /* INLINER_H */

