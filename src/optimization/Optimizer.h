/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include "config.h"

#include <functional>
#include <vector>

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;

    namespace optimizations
    {
        /*
         * An OptimizationPass usually walks over all instructions within a single method
         */
        struct OptimizationPass
        {
        public:
            /*
             * NOTE: Optimizations can be run in parallel, so no static or global variables can be set.
             * The optimizations are only run in parallel for different methods, so any access to the method is
             * thread-safe
             */
            using Pass = std::function<void(const Module&, Method&, const Configuration&)>;

            OptimizationPass(const std::string& name, const Pass pass);

            void operator()(const Module& module, Method& method, const Configuration& config) const;

            std::string name;

        private:
            Pass pass;
        };

        /*
         * An OptimizationStep handles a single instruction per invocation
         */
        struct OptimizationStep
        {
        public:
            /*
             * NOTE: Optimizations can be run in parallel, so no static or global variables can be set.
             * The optimizations are only run in parallel for different methods, so any access to the method is
             * thread-safe
             */
            using Step =
                std::function<InstructionWalker(const Module&, Method&, InstructionWalker, const Configuration&)>;

            OptimizationStep(const std::string& name, const Step step);

            InstructionWalker operator()(
                const Module& module, Method& method, InstructionWalker it, const Configuration& config) const;

            std::string name;

        private:
            Step step;
        };

        class Optimizer
        {
        public:
            explicit Optimizer(const Configuration& config);

            void optimize(Module& module) const;

        private:
            Configuration config;
            std::vector<OptimizationPass> passes;
        };

    } // namespace optimizations
} // namespace vc4c
#endif /* OPTIMIZER_H */
