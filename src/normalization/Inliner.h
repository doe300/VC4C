/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INLINER_H
#define INLINER_H

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace normalization
    {
        void inlineMethods(const Module& module, Method& kernel, const Configuration& config);
    } // namespace normalization
} // namespace vc4c

#endif /* INLINER_H */
