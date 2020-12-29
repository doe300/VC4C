/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_FLAGS_ANALYSIS
#define VC4C_FLAGS_ANALYSIS

#include "Analysis.h"

#include "../InstructionWalker.h"
#include "../asm/OpCodes.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../performance.h"

#include <bitset>

namespace vc4c
{
    namespace analysis
    {
        /*
         * Analyses the statically/dynamically set flags for all elements of flags-setting operations.
         *
         * The information obtained from this analysis can be used e.g. to combine setting of the same flags.
         */
        class StaticFlagsAnalysis
            : public LocalAnalysis<AnalysisDirection::FORWARD, Optional<VectorFlags>, InstructionWalker, BasicBlock&>
        {
        public:
            explicit StaticFlagsAnalysis();

            static std::string to_string(const Optional<VectorFlags>& flags);

            /*
             * Analyzes the static/dynamic setting of flags for an instruction with the set-flags bit set.
             *
             * The returned FlagStatus is interpreted as such:
             * - SET - the flag will always be set (can be determined at compile-time)
             * - CLEAR - the flag will never be set (can be determined at compile-time)
             * - UNDEFINED - the flag cannot be determined at compile time and is set dynamically
             *
             * NOTE: Since the actual analysis currently does not do much, this function should be called directly
             */
            static Optional<VectorFlags> analyzeStaticFlags(const intermediate::IntermediateInstruction* inst,
                InstructionWalker it, bool allowNoFlagsSetter = false);

        private:
            static Optional<VectorFlags> analyzeStaticFlagsWrapper(const intermediate::IntermediateInstruction* inst,
                const Optional<VectorFlags>& previousFlags, InstructionWalker& it, BasicBlock& block);
        };

        /**
         * Extracted information about a comparison, giving information about the original comparison
         */
        struct ComparisonInfo
        {
            // the name of the comparison, one of the intermediate::COMP_XXX constants
            std::string name;
            // the left operand of the original comparison
            Value leftOperand;
            // the right operand of the original comparison
            Value rightOperand;
            // the (optional) local storing the boolean result value of the comparison
            const Local* result;
            // the flags which indicate the comparison, i.e. for an "ult" comparison, the flags indicating the left
            // operand being less then the right operand seen as unsigned integers
            ConditionCode condition;

            std::string to_string() const;
        };

        /**
         * Tries to extract the original comparison information for the comparison setting the flags used in the given
         * conditional instruction.
         */
        Optional<ComparisonInfo> getComparison(
            const intermediate::IntermediateInstruction* conditionalInstruction, InstructionWalker searchStart);

        /**
         * Tries to extract the original comparison information for the comparison writing into the given result local.
         */
        Optional<ComparisonInfo> getComparison(const Local* comparisonResult, InstructionWalker searchStart);
    } // namespace analysis
} // namespace vc4c

#endif /* VC4C_FLAGS_ANALYSIS */
