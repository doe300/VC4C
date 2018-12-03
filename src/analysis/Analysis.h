/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LOCAL_ANALYSIS
#define VC4C_LOCAL_ANALYSIS

#include "../BasicBlock.h"
#include "../InstructionWalker.h"
#include "log.h"

#include <functional>
#include <unordered_map>

namespace vc4c
{
    namespace analysis
    {
        enum class AnalysisDirection
        {
            FORWARD,
            BACKWARD
        };

        template <typename V, typename C>
        using DefaultLocalTransferFunction =
            std::function<V(const intermediate::IntermediateInstruction*, const V&, C&)>;
        template <typename V>
        using DumpFunction = std::function<std::string(const V&)>;

        /*
         * Template for local analyses (within a single basic block) traversing the block to create the analysis
         * result
         *
         * Adapted from here: https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/15/Slides15.pdf
         */
        template <AnalysisDirection D, typename V, typename C = void*, typename F = DefaultLocalTransferFunction<V, C>>
        class LocalAnalysis
        {
        public:
            static constexpr AnalysisDirection Direction = D;
            using Values = V;
            using Cache = C;
            using TransferFunction = F;

            /*
             * Analyses the given basic block and fills the internal result store
             *
             * NOTE: One instance of a LocalAnalysis can only analyze a single basic block!
             */
            void operator()(const BasicBlock& block)
            {
                results.reserve(block.size());
                if(Direction == AnalysisDirection::FORWARD)
                    analyzeForward(block);
                else
                    analyzeBackward(block);

                resultAtStart = &results.at(block.begin()->get());
                if(!block.empty())
                    resultAtEnd = &results.at((--block.end())->get());
            }

            const Values& getResult(const intermediate::IntermediateInstruction* instr) const
            {
                return results.at(instr);
            }

            const Values& getStartResult() const
            {
                return *resultAtStart;
            }

            const Values& getEndResult() const
            {
                return *resultAtEnd;
            }

            void dumpResults(const BasicBlock& block) const
            {
                for(const auto& inst : block)
                {
                    if(inst)
                        logging::debug() << inst->to_string() << " : " << dumpFunction(getResult(inst.get()))
                                         << logging::endl;
                }
            }

        protected:
            LocalAnalysis(
                TransferFunction&& transferFunction, DumpFunction<Values>&& dumpFunction, Values&& initialValue = {}) :
                transferFunction(std::forward<TransferFunction>(transferFunction)),
                dumpFunction(std::forward<DumpFunction<Values>>(dumpFunction)),
                initialValue(std::forward<Values>(initialValue))
            {
            }

            const TransferFunction transferFunction;
            const DumpFunction<Values> dumpFunction;
            std::unordered_map<const intermediate::IntermediateInstruction*, Values> results;
            const Values initialValue;
            const Values* resultAtStart = nullptr;
            const Values* resultAtEnd = nullptr;

        private:
            void analyzeForward(const BasicBlock& block)
            {
                Cache c;
                const auto* prevVal = &initialValue;
                for(const auto& inst : block)
                {
                    if(inst)
                    {
                        auto pos = results.emplace(
                            inst.get(), std::forward<Values>(transferFunction(inst.get(), *prevVal, c)));
                        prevVal = &(pos.first->second);
                    }
                }
            }

            void analyzeBackward(const BasicBlock& block)
            {
                Cache c;
                const auto* prevVal = &initialValue;
                auto it = block.end();
                do
                {
                    --it;
                    if(*it)
                    {
                        auto pos =
                            results.emplace(it->get(), std::forward<Values>(transferFunction(it->get(), *prevVal, c)));
                        prevVal = &(pos.first->second);
                    }
                } while(it != block.begin());
            }
        };

        /*
         * The default template for a transfer function retrieving information about a basic block
         *
         * The first return value is the initial value (before the block executes), the second the final value (after
         * the block executes)
         */
        template <typename V>
        using DefaultGlobalTransferFunction = std::function<std::pair<V, V>(const BasicBlock&)>;

        /*
         * Template for global analyses
         *
         * A global analysis analyzes a basic block as a whole and retrieves information about pre- and post-conditions
         * of the single blocks
         *
         */
        template <typename V, typename F = DefaultGlobalTransferFunction<V>>
        class GlobalAnalysis
        {
        public:
            using Values = V;
            using TransferFunction = F;

            /*
             * Analyses the given method and fills the internal result store
             */
            void operator()(const Method& method)
            {
                results.reserve(method.size());
                for(const BasicBlock& block : method)
                {
                    results.emplace(&block, std::forward<std::pair<Values, Values>>(transferFunction(block)));
                }
            }

            const Values& getInitialResult(const BasicBlock& block) const
            {
                return results.at(&block).first;
            }

            const Values& getFinalResult(const BasicBlock& block) const
            {
                return results.at(&block).second;
            }

            void dumpResults(const Method& method) const
            {
                for(const BasicBlock& block : method)
                {
                    logging::debug() << block.getLabel()->to_string()
                                     << " (in) : " << dumpFunction(getInitialResult(block)) << logging::endl;
                    logging::debug() << block.getLabel()->to_string()
                                     << " (out) : " << dumpFunction(getFinalResult(block)) << logging::endl;
                }
            }

        protected:
            GlobalAnalysis(TransferFunction&& transferFunction, DumpFunction<Values>&& dumpFunction) :
                transferFunction(std::forward<TransferFunction>(transferFunction)),
                dumpFunction(std::forward<DumpFunction<Values>>(dumpFunction))
            {
            }

            const TransferFunction transferFunction;
            const DumpFunction<Values> dumpFunction;
            std::unordered_map<const BasicBlock*, std::pair<Values, Values>> results;
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_LOCAL_ANALYSIS */