/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_AVAILABLE_EXPRESSION_ANALYSIS
#define VC4C_AVAILABLE_EXPRESSION_ANALYSIS

#include "../asm/OpCodes.h"
#include "../performance.h"
#include "Analysis.h"

namespace vc4c
{
    namespace analysis
    {
        struct Expression;
    }

    template <>
    struct hash<analysis::Expression>
    {
        size_t operator()(const analysis::Expression& expr) const noexcept;
    };

    namespace analysis
    {
        /**
         * An expression is an abstraction of an ALU operation (or load) where only the inputs and the type of operation
         * is considered.
         *
         * Expressions might not have any side-effects or conditional execution!
         */
        struct Expression
        {
            OpCode code;
            Value arg0;
            Optional<Value> arg1;
            Unpack unpackMode = UNPACK_NOP;
            Pack packMode = PACK_NOP;
            intermediate::InstructionDecorations deco;

            static Optional<Expression> createExpression(const intermediate::IntermediateInstruction& instr);

            bool operator==(const Expression& other) const;

            std::string to_string() const;
        };

        /*
         * Maps the available locals and the available expression writing into the given local for a given point in
         * the program
         */
        using AvailableExpressions = FastMap<Expression, const intermediate::IntermediateInstruction*>;

        /*
         * Analyses the available expressions within a single basic block.
         *
         * An available expression at a given point in the program is an expression that already has been calculated
         * (and whose result has not yet been overridden) and therefore does not need to be recalculated again.
         *
         * See also: https://en.wikipedia.org/wiki/Available_expression
         *
         */
        class AvailableExpressionAnalysis : public LocalAnalysis<AnalysisDirection::FORWARD, AvailableExpressions,
                                                FastMap<const Local*, FastSet<const Expression*>>>
        {
        public:
            explicit AvailableExpressionAnalysis();

        private:
            /*
             * For an instruction reading a, b and writing c:
             *
             * - the available expression for c is re-set to the current instruction
             */
            static AvailableExpressions analyzeAvailableExpressions(const intermediate::IntermediateInstruction* instr,
                const AvailableExpressions& previousExpressions,
                FastMap<const Local*, FastSet<const Expression*>>& cache);

            static std::string to_string(const AvailableExpressions& expressions);
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_AVAILABLE_EXPRESSION_ANALYSIS */