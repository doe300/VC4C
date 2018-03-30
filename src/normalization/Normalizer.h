/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_NORMALIZER_H

#include "config.h"

#include <functional>

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;

    namespace normalization
    {
        /*
         * A normalization step runs a single type of normalization (e.g. in-lining, intrinsic replacements, etc.)
         * on one instruction at a time.
         *
         * NOTE: The normalization steps are run in parallel (on different kernel-functions) in an undefined order and
         * MUST not:
         * - require any existing order of normalization steps
         * - modify any non-local memory
         */
        using NormalizationStep =
            std::function<void(Module& module, Method& method, InstructionWalker it, const Configuration& config)>;

        class Normalizer
        {
        public:
            explicit Normalizer(const Configuration& config = {}) : config(config){};

            /*
             * Runs the normalization steps on all kernels in the module
             *
             * Depending on the build configuration, the normalization steps are run in parallel
             */
            void normalize(Module& module) const;

        private:
            Configuration config;

            /*
             * Runs all registered normalization steps on the given method.
             *
             * After this function has returned, it is guaranteed, that all remaining instructions within the method are
             * normalized (e.g. return true for #isNormalized()).
             */
            void normalizeMethod(Module& module, Method& method) const;
        };
    } /* namespace normalization */
} /* namespace vc4c */

#define VC4C_NORMALIZER_H
#endif /* VC4C_NORMALIZER_H */