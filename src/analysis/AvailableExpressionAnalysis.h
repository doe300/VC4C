/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_AVAILABLE_EXPRESSION_ANALYSIS
#define VC4C_AVAILABLE_EXPRESSION_ANALYSIS

#include "../performance.h"
#include "Analysis.h"

namespace vc4c
{
    namespace analysis
    {
        /*
         * Maps the available locals and the available expression writing into the given local for a given point in
         * the program
         */
        using AvailableExpressions = FastMap<const Local*, const intermediate::IntermediateInstruction*>;

        /*
         * Analyses the available expressions within a single basic block.
         *
         * An available expression at a given point in the program is an expression that already has been calculated
         * (and whose result has not yet been overridden) and therefore does not need to be recalculated again.
         *
         * See also: https://en.wikipedia.org/wiki/Available_expression
         *
         */
        class AvailableExpressionAnalysis : public LocalAnalysis<AnalysisDirection::FORWARD, AvailableExpressions>
        {
        public:
            explicit AvailableExpressionAnalysis();

        private:
            /*
             * For an instruction reading a, b and writing c:
             *
             * - the available expression for c is re-set to the current instruction
             */
            static AvailableExpressions analyzeAvailableExpressions(
                const intermediate::IntermediateInstruction* instr, const AvailableExpressions& previousExpressions);
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_AVAILABLE_EXPRESSION_ANALYSIS */