/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_OPTIMIZATION_VECTORIZER
#define VC4C_OPTIMIZATION_VECTORIZER

namespace vc4c
{
    class Method;
    class Module;
    struct Configuration;

    namespace optimizations
    {
        /*
         * Tries to find loops which then can be vectorized by combining multiple iterations into one.
         *
         * NOTE: Currently only works with "standard" for-range loops and needs to be enabled explicitly in the
         * Configuration
         */
        bool vectorizeLoops(const Module& module, Method& method, const Configuration& config);

    } // namespace optimizations

} // namespace vc4c

#endif /* VC4C_OPTIMIZATION_VECTORIZER */