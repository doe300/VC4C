/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include "config.h"

#include <functional>
#include <map>
#include <set>
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
            using Pass = std::function<bool(const Module&, Method&, const Configuration&)>;

            OptimizationPass(const std::string& name, const std::string& parameterName, const Pass& pass,
                const std::string& description);

            bool operator()(const Module& module, Method& method, const Configuration& config) const;

            const std::string name;
            const std::string parameterName;
            const std::string description;

        private:
            const Pass pass;
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

            OptimizationStep(const std::string& name, const Step& step);

            InstructionWalker operator()(
                const Module& module, Method& method, InstructionWalker it, const Configuration& config) const;

            const std::string name;

        private:
            const Step step;
        };

        class Optimizer
        {
        public:
            explicit Optimizer(const Configuration& config);

            void optimize(Module& module) const;

            /*
             * The complete list of all optimization passes available to be used
             *
             * NOTE: The order of the passes is the order of execution!
             */
            static const std::vector<OptimizationPass> ALL_PASSES;

            /*
             * Returns the list of enabled passes when using the specific optimization level
             */
            static std::set<std::string> getPasses(OptimizationLevel level);

        private:
            Configuration config;
            std::vector<const OptimizationPass*> passes;
        };

    } // namespace optimizations
} // namespace vc4c
#endif /* OPTIMIZER_H */
