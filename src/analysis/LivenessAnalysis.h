/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LIVENESS_ANALYSIS
#define VC4C_LIVENESS_ANALYSIS

#include "../performance.h"
#include "Analysis.h"

#include <memory>

namespace vc4c
{
    class Local;

    namespace analysis
    {
        /*
         * Analyses the liveness (the life-time range) of locals within a single basic block
         *
         * In short: A local is "live" if its current value is used in the future!
         *
         * The liveness analysis provides following information:
         * - live-range of locals (only for locals not living outside of the basic block)
         * - getStartResult() returns all locals used within this basic block (more accurate: all locals, where their
         * previous value is used in this block/which are not overwritten before use)
         *
         * Also called Live Variable Analysis (https://en.wikipedia.org/wiki/Live_variable_analysis)
         */
        class LivenessAnalysis : public LocalAnalysis<AnalysisDirection::BACKWARD, FastSet<const Local*>,
                                     std::pair<FastSet<const Local*>, FastMap<const Local*, ConditionCode>>>
        {
        public:
            // The optional parameter are the locals assumed to be live at the end of the block
            explicit LivenessAnalysis(FastSet<const Local*>&& outgoingLiveLocals = {});

        private:
            /*
             * For an instruction reading a, b and writing c:
             *
             * - c's liveness ends (any previous value of c is now dead)
             * - a's, b's livenesses begin (they need to be live to be read)
             * - any other live local remains live
             */
            static FastSet<const Local*> analyzeLiveness(const intermediate::IntermediateInstruction* instr,
                const FastSet<const Local*>& nextResult,
                std::pair<FastSet<const Local*>, FastMap<const Local*, ConditionCode>>& cache);

            static std::string to_string(const FastSet<const Local*>& liveLocals);
        };

        /**
         * Analyzes the liveness (the life-time range) of locals across all basic blocks
         *
         * See LivenessAnalysis for detailed description of the liveness.
         *
         * Runs the LivenessAnalysis for every block taking into account all locals that are live at the end and start
         * of all blocks, recursively.
         *
         * The result will contain all locals for a given instruction that are live at that instruction, whether
         * actually used in the corresponding block or not.
         */
        class GlobalLivenessAnalysis
        {
        public:
            explicit GlobalLivenessAnalysis() = default;

            void operator()(Method& method);

            inline const LivenessAnalysis& getLocalAnalysis(const BasicBlock& block) const
            {
                return *results.at(&block);
            }
            void dumpResults(const Method& method) const;

        private:
            FastMap<const BasicBlock*, std::unique_ptr<LivenessAnalysis>> results;
        };

        /*
         * Analyses the usage of locals in granularity of basic blocks.
         *
         * Each basic block is analyzed and following information is extracted:
         * - the locals read by this block (which are written externally)
         * - the locals written to by this block (and with usages externally)
         */
        class LocalUsageAnalysis : public GlobalAnalysis<FastSet<const Local*>>
        {
        public:
            explicit LocalUsageAnalysis();

        private:
            /*
             * The initial set of locals contains all locals read by this block (before they are possibly overwritten)
             *
             * The final set of locals contains all locals written in this block where there are usages outside of this
             * block
             */
            static std::pair<FastSet<const Local*>, FastSet<const Local*>> analyzeLocalUsage(const BasicBlock& block);

            static std::string to_string(const FastSet<const Local*>& locals);
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_LIVENESS_ANALYSIS */
