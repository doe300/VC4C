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

        /**
         * Tries to find and improve vector folding.
         *
         * To fold a vector in OpenCL, often element-wise binary operations are inserted which are then passes through
         * LLVM and then also converted 1:1 to IR code similar to:
         *   %tmp0 = extractelement %vector, 1
         *   %out0 = add %tmp0, %vector
         *   %tmp1 = extractelement %vector, 2
         *   %out1 = add %tmp1, %out0
         *   %tmp1 = extractelement %vector, 3
         *   %out1 = add %tmp2, %out1
         *   [...]
         *
         * The above can be simplified to by applying hierarchical vector folding (see VectorHelper.h
         * #insertFoldVector())
         *
         */
        bool compactVectorFolding(const Module& module, Method& method, const Configuration& config);

        /**
         * Tries to find element-wise (single elements or masks) copies from and to the same vectors and combines them.
         *
         * Combines for example this:
         *   register - = loadui <1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0> (setf )
         *   %out = %in (ifzc)
         *   register - = loadui <0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0> (setf )
         *   %out = %in (ifzc)
         *
         * to this:
         *   register - = loadui <1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0> (setf )
         *   %out = %in (ifzc)
         */
        bool combineVectorElementCopies(const Module& module, Method& method, const Configuration& config);

    } // namespace optimizations

} // namespace vc4c

#endif /* VC4C_OPTIMIZATION_VECTORIZER */