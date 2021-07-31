/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LOCAL_ANALYSIS
#define VC4C_LOCAL_ANALYSIS

#include "../BasicBlock.h"
#include "../Method.h"
#include "../helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "log.h"

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

        template <typename V>
        using DumpFunction = FunctionPointer<std::string(const V&)>;

        /*
         * Template for local analyses (within a single basic block) traversing the block to create the analysis
         * result
         *
         * Adapted from here: https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/15/Slides15.pdf
         */
        template <AnalysisDirection D, typename V, typename C = void*, typename... Args>
        class LocalAnalysis
        {
        public:
            static constexpr AnalysisDirection Direction = D;
            using Values = V;
            using Cache = C;
            using AdditionalArgs = std::tuple<Args...>;
            using TransferFunction =
                FunctionPointer<V(const intermediate::IntermediateInstruction*, const V&, C&, Args&...)>;

            /*
             * Analyses the given basic block and fills the internal result store
             *
             * NOTE: One instance of a LocalAnalysis can only analyze a single basic block!
             */
            void operator()(const BasicBlock& block, Args&... args)
            {
                results.reserve(block.size());
                if(Direction == AnalysisDirection::FORWARD)
                    analyzeForward(block, args...);
                else
                    analyzeBackward(block, args...);

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
                logging::logLazy(logging::Level::DEBUG, [&]() {
                    for(const auto& inst : block)
                    {
                        if(inst)
                            logging::debug()
                                << inst->to_string() << " : " << dumpFunction(getResult(inst.get())) << logging::endl;
                    }
                });
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
            void analyzeForward(const BasicBlock& block, Args&... args)
            {
                Cache c;
                const auto* prevVal = &initialValue;
                for(const auto& inst : block)
                {
                    if(inst)
                    {
                        auto pos = results.emplace(
                            inst.get(), std::forward<Values>(transferFunction(inst.get(), *prevVal, c, args...)));
                        prevVal = &(pos.first->second);
                    }
                }
            }

            void analyzeBackward(const BasicBlock& block, Args&... args)
            {
                Cache c;
                const auto* prevVal = &initialValue;
                auto it = block.end();
                do
                {
                    --it;
                    if(*it)
                    {
                        auto pos = results.emplace(
                            it->get(), std::forward<Values>(transferFunction(it->get(), *prevVal, c, args...)));
                        prevVal = &(pos.first->second);
                    }
                } while(it != block.begin());
            }

            template <typename, typename, typename...>
            friend class CombinedLocalAnalysis;
        };

        /*
         * The default template for a transfer function retrieving information about a basic block
         *
         * The first return value is the initial value (before the block executes), the second the final value (after
         * the block executes)
         */
        template <typename V, typename... Args>
        using DefaultGlobalTransferFunction = FunctionPointer<std::pair<V, V>(const BasicBlock&, Args&...)>;

        /*
         * Template for global analyses
         *
         * A global analysis analyzes a basic block as a whole and retrieves information about pre- and post-conditions
         * of the single blocks
         *
         */
        template <typename V, typename... Args>
        class GlobalAnalysis
        {
        public:
            using Values = V;
            using TransferFunction = DefaultGlobalTransferFunction<V, Args...>;

            /*
             * Analyses the given method and fills the internal result store
             */
            void operator()(const Method& method, Args&... args)
            {
                results.reserve(method.size());
                for(const BasicBlock& block : method)
                {
                    results.emplace(&block, std::forward<std::pair<Values, Values>>(transferFunction(block, args...)));
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
                logging::logLazy(logging::Level::DEBUG, [&]() {
                    for(const BasicBlock& block : method)
                    {
                        logging::debug() << block.to_string() << " (in) : " << dumpFunction(getInitialResult(block))
                                         << logging::endl;
                        logging::debug() << block.to_string() << " (out) : " << dumpFunction(getFinalResult(block))
                                         << logging::endl;
                    }
                });
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

        template <typename AnalysisA, typename AnalysisB, typename... Args>
        class CombinedLocalAnalysis
        {
            static_assert(AnalysisA::Direction == AnalysisB::Direction, "");

        public:
            CombinedLocalAnalysis(AnalysisA&& first, AnalysisB&& second) : a(first), b(second) {}

            /*
             * Analyses the given basic block and fills the internal result store
             *
             * NOTE: One instance of a LocalAnalysis can only analyze a single basic block!
             */
            void operator()(const BasicBlock& block, Args&... args)
            {
                a.results.reserve(block.size());
                b.results.reserve(block.size());
                if(AnalysisA::Direction == AnalysisDirection::FORWARD)
                    analyzeForward(block, args...);
                else
                    analyzeBackward(block, args...);

                a.resultAtStart = &a.results.at(block.begin()->get());
                b.resultAtStart = &b.results.at(block.begin()->get());
                if(!block.empty())
                {
                    a.resultAtEnd = &a.results.at((--block.end())->get());
                    b.resultAtEnd = &b.results.at((--block.end())->get());
                }
            }

            std::pair<const typename AnalysisA::Values*, const typename AnalysisB::Values*> getResult(
                const intermediate::IntermediateInstruction* instr) const
            {
                return std::make_pair(&a.results.at(instr), &b.results.at(instr));
            }

            std::pair<const typename AnalysisA::Values*, const typename AnalysisB::Values*> getStartResult() const
            {
                return std::make_pair(a.resultAtStart, b.resultAtStart);
            }

            std::pair<const typename AnalysisA::Values*, const typename AnalysisB::Values*> getEndResult() const
            {
                return std::make_pair(a.resultAtEnd, b.resultAtEnd);
            }

        private:
            AnalysisA a;
            AnalysisB b;

            void analyzeForward(const BasicBlock& block, Args&... args)
            {
                typename AnalysisA::Cache cacheA;
                typename AnalysisB::Cache cacheB;
                const auto* prevValA = &a.initialValue;
                const auto* prevValB = &b.initialValue;
                for(const auto& inst : block)
                {
                    if(inst)
                    {
                        auto posA = a.results.emplace(inst.get(),
                            std::forward<typename AnalysisA::Values>(
                                a.transferFunction(inst.get(), *prevValA, cacheA, args...)));
                        prevValA = &(posA.first->second);
                        auto posB = b.results.emplace(inst.get(),
                            std::forward<typename AnalysisB::Values>(
                                b.transferFunction(inst.get(), *prevValB, cacheB, *prevValA)));
                        prevValB = &(posB.first->second);
                    }
                }
            }

            void analyzeBackward(const BasicBlock& block, Args&... args)
            {
                typename AnalysisA::Cache cacheA;
                typename AnalysisB::Cache cacheB;
                const auto* prevValA = &a.initialValue;
                const auto* prevValB = &b.initialValue;
                auto it = block.end();
                do
                {
                    --it;
                    if(*it)
                    {
                        auto posA = a.results.emplace(it->get(),
                            std::forward<typename AnalysisA::Values>(
                                a.transferFunction(it->get(), *prevValA, cacheA, args...)));
                        prevValA = &(posA.first->second);
                        auto posB = b.results.emplace(it->get(),
                            std::forward<typename AnalysisB::Values>(
                                b.transferFunction(it->get(), *prevValB, cacheB, *prevValA)));
                        prevValB = &(posB.first->second);
                    }
                } while(it != block.begin());
            }
        };
    } /* namespace analysis */
} /* namespace vc4c */

#endif /* VC4C_LOCAL_ANALYSIS */
