/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LIVENESS_ANALYSIS
#define VC4C_LIVENESS_ANALYSIS

#include "../performance.h"
#include "../tools/SmallSet.h"
#include "Analysis.h"

#include <memory>

namespace vc4c
{
    class Local;

    namespace analysis
    {
        struct LivenessChanges
        {
            tools::SmallSortedPointerSet<const Local*> removedLocals{};
            tools::SmallSortedPointerSet<const Local*> addedLocals{};
        };

        struct LivenessAnalysisCache
        {
            FastSet<const Local*> conditionalWrites;
            FastMap<const Local*, ConditionCode> conditionalReads;
            FastSet<const Local*> liveLocals;
            FastSet<const Local*> conditionalPhiWrites;
        };

        /**
         * Dummy local which indicated the r5/replicate registers.
         *
         * NOTE: This pointer is only valid for the liveness analysis and all derived analysis algorithms!
         */
        extern const Local* FAKE_REPLICATE_REGISTER;

        /**
         * Analyses the changes in the liveness of locals within a basic block
         *
         * See LivenessAnalysis below for details.
         *
         * This is a helper analysis and probably won't be useful on its own.
         */
        class LivenessChangesAnalysis
            : public LocalAnalysis<AnalysisDirection::BACKWARD, LivenessChanges, LivenessAnalysisCache, const bool>
        {
        public:
            explicit LivenessChangesAnalysis();

        private:
            /*
             * For an instruction reading a, b and writing c:
             *
             * - c's liveness ends (any previous value of c is now dead)
             * - a's, b's livenesses begin (they need to be live to be read)
             * - any other live local remains live
             */
            static LivenessChanges analyzeLivenessChanges(const intermediate::IntermediateInstruction* instr,
                const LivenessChanges& previousChanges, LivenessAnalysisCache& cache, const bool& trackR5Usage);

            static std::string to_string(const LivenessChanges& changes);
        };

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
                                     LivenessAnalysisCache, const bool>
        {
        public:
            // The optional parameter are the locals assumed to be live at the end of the block
            explicit LivenessAnalysis(FastSet<const Local*>&& outgoingLiveLocals = {});

            /**
             * Analyzes the live locals inside the block and returns the live locals at the beginning of the block.
             *
             * If the trackR5Usage flag is set to true, the usages of the R5/replicate register will be tracked under
             * the dummy local FAKE_REPLICATE_REGISTER.
             *
             * The optional parameter outgoingLiveLocals takes the locals that are live at the end of the block (i.e.
             * that are live at the beginning of the successor blocks). If this parameter is not set, only locals used
             * inside this block are considered.
             *
             * NOTE: Usage of this function directly over the "normal" analyze operator() is highly recommended, iff
             * only the live locals at the beginning of the block are of interest.
             *
             * Returns the live locals at the beginning of the block
             */
            static FastSet<const Local*> analyzeIncomingLiveLocals(
                const BasicBlock& block, bool trackR5Usage, FastSet<const Local*>&& outgoingLiveLocals = {});

            /**
             * Analyzes the live locals inside the block using the given precalculated liveness changes.
             *
             * The behavior of calling this function is identical to calling the default analyze operator().
             * The only difference is, that this function does not recalculate the liveness changes, but uses the given
             * changes instead.
             */
            void analyzeWithChanges(const BasicBlock& block, const LivenessChangesAnalysis& analysis);

            /**
             * Similar to #analyzeWithChanges, but only takes the given new additional outgoing locals as input and
             * processes their lifetimes.
             *
             * NOTE: Via this method, only new live locals can be added, none can be removed. I.e. the given locals are
             * all added to the already live locals!
             *
             * NOTE: Before calling this function, the #analyzeWithChanges or the analyze operator() needs to be called
             * first!
             */
            void updateWithChanges(const BasicBlock& block, const LivenessChangesAnalysis& analysis,
                FastSet<const Local*>&& outgoingLiveLocals);

        private:
            /*
             * For an instruction reading a, b and writing c:
             *
             * - c's liveness ends (any previous value of c is now dead)
             * - a's, b's livenesses begin (they need to be live to be read)
             * - any other live local remains live
             */
            static FastSet<const Local*> analyzeLiveness(const intermediate::IntermediateInstruction* instr,
                const FastSet<const Local*>& nextResult, LivenessAnalysisCache& cache, const bool& trackR5Usage);

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
            explicit GlobalLivenessAnalysis(bool trackR5) : trackR5Usage(trackR5) {}

            void operator()(Method& method);

            inline const LivenessAnalysis& getLocalAnalysis(const BasicBlock& block) const
            {
                return *results.at(&block);
            }
            inline const LivenessChangesAnalysis& getChanges(const BasicBlock& block) const
            {
                return changes.at(&block);
            }
            void dumpResults(const Method& method) const;

        private:
            bool trackR5Usage;
            FastMap<const BasicBlock*, std::unique_ptr<LivenessAnalysis>> results;
            FastMap<const BasicBlock*, LivenessChangesAnalysis> changes;
        };

        /**
         * A single usage-range of a local inside a single basic block
         *
         * The usage range starts with:
         * - the first writer to that local or if the local is not written before its first read,
         * - the start of the block
         *
         * The usage range ends with:
         * - the last read of that local or if not all reads of that local are within this block,
         * - the end if the block
         */
        struct LocalUsageRange
        {
            const Local* local;
            std::size_t startIndex;
            std::size_t endIndex;
            std::size_t numAccesses;
            std::size_t maxUnaccessedRange;

            constexpr bool startsWithBlock() const noexcept
            {
                return startIndex == 0;
            }

            inline bool endsWithBlock(const BasicBlock& block) const noexcept
            {
                return endIndex == block.size();
            }

            constexpr std::size_t size() const noexcept
            {
                return endIndex - startIndex + std::size_t{1} /* the end instruction itself */;
            }

            bool operator<(const LocalUsageRange& other) const noexcept;

            std::string to_string() const;
        };

        /**
         * A single usage share of a local across the whole kernel
         */
        struct LocalUtilization
        {
            const Local* local;
            std::size_t numInstructions;
            std::size_t numAccesses;
            std::size_t numLoops;
            // Calculated from the fields above, this indicates the rating for splitting/spilling this local. This field
            // also determines the natural ordering. Locals with bigger ratings should be preferred to be split/spilled.
            std::size_t rating;

            bool operator<(const LocalUtilization& other) const noexcept;

            std::string to_string() const;
        };

        /**
         * Using the results of the given or a newly created global liveness analysis, determines the access ranges of
         * the locals as the blocks they are accessed in, the first and last accesses as well as the number of accesses.
         *
         * The result of this analysis can be used as basis to deciding which locals to split (and where) or to spill
         * into VPM/RAM.
         */
        class LocalUsageRangeAnalysis
        {
        public:
            explicit LocalUsageRangeAnalysis(const GlobalLivenessAnalysis* globalLiveness = nullptr) :
                livenessAnalysis(globalLiveness)
            {
            }

            void operator()(Method& method);

            inline const SortedSet<LocalUsageRange>& getRanges(const BasicBlock& block) const
            {
                return detailedRanges.at(&block);
            }

            void dumpResults(const Method& method) const;

            inline const SortedSet<LocalUtilization>& getOverallUsages() const
            {
                return overallUsages;
            }

        private:
            const GlobalLivenessAnalysis* livenessAnalysis;
            SortedSet<LocalUtilization> overallUsages;
            FastMap<const BasicBlock*, SortedSet<LocalUsageRange>> detailedRanges;
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_LIVENESS_ANALYSIS */
