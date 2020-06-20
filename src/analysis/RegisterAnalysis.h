/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_REGISTER_ANALYSIS
#define VC4C_REGISTER_ANALYSIS

#include "Analysis.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../performance.h"

#include <bitset>

namespace vc4c
{
    namespace analysis
    {
        using UsedElements = FastMap<const Local*, std::bitset<NATIVE_VECTOR_SIZE>>;

        /*
         * Tracks locals which are read conditionally to determine the used elements
         */
        using ConditionalUsages = FastMap<const Local*, ConditionCode>;

        /*
         * Analyses the SIMD elements used for each local.
         *
         * The information obtained from this analysis can be used e.g. to combine multiple locals into different
         * elements of the same registers.
         */
        class UsedElementsAnalysis : public LocalAnalysis<AnalysisDirection::BACKWARD, UsedElements, ConditionalUsages>
        {
        public:
            explicit UsedElementsAnalysis();

            static std::string to_string(const UsedElements& registerElements);

        private:
            /*
             * For every instruction, following will be done:
             * - the read SIMD elements are added to the values
             * - any local write clears the register usage
             */
            static UsedElements analyzeUsedSIMDElements(const intermediate::IntermediateInstruction* inst,
                const UsedElements& nextValues, ConditionalUsages& cache);
        };
    } // namespace analysis
} // namespace vc4c

#endif /* VC4C_REGISTER_ANALYSIS */
