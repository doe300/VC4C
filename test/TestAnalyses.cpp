/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestAnalyses.h"

#include "CompilerInstance.h"
#include "Precompiler.h"
#include "analysis/ControlFlowGraph.h"
#include "analysis/DataDependencyGraph.h"
#include "analysis/DominatorTree.h"
#include "analysis/FlagsAnalysis.h"
#include "analysis/ValueRange.h"
#include "analysis/WorkItemAnalysis.h"
#include "intermediate/Helper.h"
#include "intermediate/operators.h"
#include "intrinsics/Comparisons.h"
#include "normalization/LiteralValues.h"

using namespace vc4c;
using namespace vc4c::analysis;
using namespace vc4c::operators;

static const Configuration config{};

static constexpr auto KERNEL_NESTED_LOOPS = R"(
__kernel void test(__global int16* in, __global int16* out, uint count) {
    uint gid = get_global_id(0);
    for(uint x = 0; x < 64; ++x) {
        int16 val = in[gid * x];
        if(val.x == 0) {
            // will jump to tail (which does the increment), does not add a back edge
            continue;
        }
        if(val.x == 1) {
            // adds a second exit edge
            break;
        }
        for(uint y = 0; y < count; ++y) {
            // single node loop
            out[x * 64 + y] = val;
        }
    }
}
)";

static constexpr auto KERNEL_LOOP_BARRIER = R"(
__kernel void test(__global int16* in, __global int16* out) {
    __local int16 buffer[16];
    uint lid = get_local_id(0);
    buffer[lid] = 0;
    for(uint i = 0; i < 32; ++i) {
        buffer[lid] += in[lid * i];
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    if(lid == 0) {
        int16 result = 0;
        for(uint i = 0; i < get_local_size(0); ++i) {
            // single node loop
            result += buffer[i];
        }
        out[0] = result;
    }
}
)";

TestAnalyses::TestAnalyses(const Configuration& config)
{
    // TEST_ADD(TestAnalyses::testAvailableExpressions);
    TEST_ADD(TestAnalyses::testControlFlowGraph);
    // FIXME randomly fails for e.g. loop inclusions
    // TEST_ADD(TestAnalyses::testControlFlowLoops);
    TEST_ADD(TestAnalyses::testDataDependency);
    TEST_ADD(TestAnalyses::testDependency);
    TEST_ADD(TestAnalyses::testDominatorTree);
    // TODO
    // TEST_ADD(TestAnalyses::testInterference);
    // TEST_ADD(TestAnalyses::testLifetime);
    // TEST_ADD(TestAnalyses::testLiveness);
    // TEST_ADD(TestAnalyses::testRegister);
    // TEST_ADD(TestAnalyses::testValueRange);
    TEST_ADD(TestAnalyses::testStaticFlags);
    TEST_ADD(TestAnalyses::testIntegerComparisonDetection);
    TEST_ADD(TestAnalyses::testActiveWorkItems);
}

void TestAnalyses::testAvailableExpressions() {}

void TestAnalyses::testControlFlowGraph()
{
    Configuration configCopy(config);
    configCopy.additionalEnabledOptimizations.emplace("loop-work-groups");
    configCopy.additionalEnabledOptimizations.emplace("reorder-blocks");
    configCopy.additionalEnabledOptimizations.emplace("simplify-branches");
    configCopy.additionalEnabledOptimizations.emplace("merge-blocks");

    // first kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 8+ nodes, depending on clang version
        TEST_ASSERT(8 <= numNodes);
        FastSet<const CFGEdge*> backEdges;
        FastSet<const CFGEdge*> implicitEdges;
        FastSet<const CFGEdge*> workGroupEdges;
        cfg.forAllNodes([&](const CFGNode& node) {
            node.forAllOutgoingEdges([&](const CFGNode&, const CFGEdge& edge) -> bool {
                if(edge.data.isBackEdge(node.key))
                    backEdges.emplace(&edge);
                if(edge.data.isImplicit(node.key))
                    implicitEdges.emplace(&edge);
                if(edge.data.isWorkGroupLoop)
                    workGroupEdges.emplace(&edge);
                return true;
            });
        });
        // 2 back edges (2 for loops)
        TEST_ASSERT_EQUALS(2u, backEdges.size());
        // at least 1 implicit edge
        TEST_ASSERT(1u <= implicitEdges.size());
        // no work-group edges (they are only created by optimizations which are not yet run)
        TEST_ASSERT_EQUALS(0u, workGroupEdges.size());

        // Run the optimization steps and do some more checks
        instance.optimize();

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 8+ nodes, depending on the clang compiler
        TEST_ASSERT(8u <= numNodes);
        backEdges.clear();
        implicitEdges.clear();
        workGroupEdges.clear();
        cfg.forAllNodes([&](const CFGNode& node) {
            node.forAllOutgoingEdges([&](const CFGNode&, const CFGEdge& edge) -> bool {
                if(edge.data.isBackEdge(node.key))
                    backEdges.emplace(&edge);
                if(edge.data.isImplicit(node.key))
                    implicitEdges.emplace(&edge);
                if(edge.data.isWorkGroupLoop)
                    workGroupEdges.emplace(&edge);
                return true;
            });
        });
        // 6 back edges (2 for the inner loops, 3 for the work-group loops, 1 for work-group barrier loops)
        TEST_ASSERT_EQUALS(6u, backEdges.size());
        // 19+ implicit edges (into first loop, 2 out of if-blocks, out of second loop, 2 for work-group loop, some for
        // work-group barrier loops)
        TEST_ASSERT(19u <= implicitEdges.size());
        // 3 work-group edges (one per dimension)
        TEST_ASSERT_EQUALS(3u, workGroupEdges.size());
    }

    // second kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 35+ nodes
        TEST_ASSERT(35u <= numNodes);
        FastSet<const CFGEdge*> backEdges;
        FastSet<const CFGEdge*> implicitEdges;
        FastSet<const CFGEdge*> workGroupEdges;
        cfg.forAllNodes([&](const CFGNode& node) {
            node.forAllOutgoingEdges([&](const CFGNode&, const CFGEdge& edge) -> bool {
                if(edge.data.isBackEdge(node.key))
                    backEdges.emplace(&edge);
                if(edge.data.isImplicit(node.key))
                    implicitEdges.emplace(&edge);
                if(edge.data.isWorkGroupLoop)
                    workGroupEdges.emplace(&edge);
                return true;
            });
        });
        // 2 back edges (2 for loops)
        TEST_ASSERT_EQUALS(2u, backEdges.size());
        // 2+ implicit edges (into and out of the function calls, 1 inside barrier())
        TEST_ASSERT(2u <= implicitEdges.size());
        // no work-group edges (they are only created by optimizations which are not yet run)
        TEST_ASSERT_EQUALS(0u, workGroupEdges.size());

        // Run the optimization steps and do some more checks
        instance.optimize();

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 35+ nodes (some added for the work-group loop, some merged)
        TEST_ASSERT(35u <= numNodes);
        backEdges.clear();
        implicitEdges.clear();
        workGroupEdges.clear();
        cfg.forAllNodes([&](const CFGNode& node) {
            node.forAllOutgoingEdges([&](const CFGNode&, const CFGEdge& edge) -> bool {
                if(edge.data.isBackEdge(node.key))
                    backEdges.emplace(&edge);
                if(edge.data.isImplicit(node.key))
                    implicitEdges.emplace(&edge);
                if(edge.data.isWorkGroupLoop)
                    workGroupEdges.emplace(&edge);
                return true;
            });
        });
        // 6 back edges (3 for the inner loops, 3 for the work-group loops, 1 for work-group barrier loops)
        TEST_ASSERT_EQUALS(6u, backEdges.size());
        // 34+ implicit edges (into first loop, out of if-block, out of second loop, 2 for work-group loop, some for
        // if-then-after blocks, a lot for barrier(), 5 for work-group barrier loops)
        TEST_ASSERT(34u <= implicitEdges.size());
        // 3 work-group edges (one per dimension)
        TEST_ASSERT_EQUALS(3u, workGroupEdges.size());
    }
}

void TestAnalyses::testControlFlowLoops()
{
    Configuration configCopy(config);
    configCopy.additionalEnabledOptimizations.emplace("loop-work-groups");
    configCopy.additionalEnabledOptimizations.emplace("reorder-blocks");
    configCopy.additionalEnabledOptimizations.emplace("simplify-branches");
    configCopy.additionalEnabledOptimizations.emplace("merge-blocks");

    // first kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto loops = kernel->getCFG().findLoops(true, false);
        // 2 loops (2 for-loops)
        TEST_ASSERT_EQUALS(2u, loops.size());
        // the outer loop contains the inner, so we only have 1 innermost loop
        auto innerLoops = kernel->getCFG().findLoops(false);
        TEST_ASSERT_EQUALS(1u, innerLoops.size());
        TEST_ASSERT_EQUALS(1u, innerLoops.begin()->size());

        bool firstLoopIsOuter;
        if(loops.front().size() > loops.back().size())
        {
            TEST_ASSERT(loops.front().includes(loops.back()));
            TEST_ASSERT(!loops.back().includes(loops.front()));
            firstLoopIsOuter = true;
        }
        else
        {
            TEST_ASSERT(loops.back().includes(loops.front()));
            TEST_ASSERT(!loops.front().includes(loops.back()));
            firstLoopIsOuter = false;
        }

        // both loops have a single predecessor
        TEST_ASSERT(!!loops.front().findPredecessor());
        TEST_ASSERT(!!loops.back().findPredecessor());
        // the also have a single successor
        TEST_ASSERT(!!loops.front().findSuccessor());
        TEST_ASSERT(!!loops.back().findSuccessor());
        // every loop has a header and a tail
        TEST_ASSERT(!!loops.front().getHeader());
        TEST_ASSERT(!!loops.front().getTail());
        TEST_ASSERT(!!loops.back().getHeader());
        TEST_ASSERT(!!loops.back().getTail());
        // neither is a work-group loop (since the optimization did not yet run)
        TEST_ASSERT(!loops.front().isWorkGroupLoop());
        TEST_ASSERT(!loops.back().isWorkGroupLoop());
        // loops equal themselves, but not each other
        TEST_ASSERT(loops.front() == loops.front());
        TEST_ASSERT(loops.back() == loops.back());
        TEST_ASSERT(loops.back() != loops.front());
        TEST_ASSERT(loops.front() != loops.back());

        // the small/inner loop has only 1 node
        auto& innerLoop = firstLoopIsOuter ? loops.back() : loops.front();
        TEST_ASSERT_EQUALS(1u, innerLoop.size());
        TEST_ASSERT_EQUALS(innerLoop.getHeader(), innerLoop.getTail());

        auto inclusionTree = createLoopInclusionTree(loops);
        TEST_ASSERT_EQUALS(2u, inclusionTree->getNodes().size());
        TEST_ASSERT_EQUALS(
            0u, inclusionTree->assertNode(&(firstLoopIsOuter ? loops.front() : loops.back())).getLongestPathToRoot());
        TEST_ASSERT_EQUALS(
            1u, inclusionTree->assertNode(&(firstLoopIsOuter ? loops.back() : loops.front())).getLongestPathToRoot());
        TEST_ASSERT_EQUALS(inclusionTree->assertNode(&loops.front()).findRoot(12),
            inclusionTree->assertNode(&loops.back()).findRoot(12));

        // both loops are simple enough so we should be able to detect the induction variable
        auto dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);
        for(auto& loop : loops)
        {
            auto inductionVars = loop.findInductionVariables(*dataDependencies, true);
            TEST_ASSERT_EQUALS(1u, inductionVars.size());
            auto& inductionVar = inductionVars[0];
            TEST_ASSERT(!!inductionVar.local);
            TEST_ASSERT(!!inductionVar.initialAssignment);
            TEST_ASSERT(!!inductionVar.inductionStep);
            TEST_ASSERT(!!inductionVar.repeatCondition);
            auto checkedVar = loop.checkInductionVariable(inductionVar.local, true);
            TEST_ASSERT(!!checkedVar);
            TEST_ASSERT_EQUALS(inductionVar.initialAssignment, checkedVar->initialAssignment);
            TEST_ASSERT_EQUALS(inductionVar.inductionStep, checkedVar->inductionStep);
            TEST_ASSERT_EQUALS(inductionVar.repeatCondition.value().conditionResult,
                checkedVar->repeatCondition.value().conditionResult);
            if(&loop == &innerLoop)
            {
                // for the inner loop we can't determine the range or iteration count, since the upper limit is
                // non-constant
                TEST_ASSERT_EQUALS(0_lit, inductionVar.getLowerBound());
                TEST_ASSERT(!inductionVar.getUpperBound());
                TEST_ASSERT(!inductionVar.getRange());
                TEST_ASSERT(!inductionVar.getIterationCount());
            }
            else
            {
                // for the outer loop we should be able to determine the iteration count
                TEST_ASSERT_EQUALS(0_lit, inductionVar.getLowerBound());
                // The upper bound might be 63 or 64 (with <= or < comparison respectively), depending on the LLVM
                // version
                TEST_ASSERT(!!inductionVar.getUpperBound());
                TEST_ASSERT(!!inductionVar.getRange());
                TEST_ASSERT_EQUALS(64u, inductionVar.getIterationCount().value());
            }

            // there are some constant address offset/index calculations
            auto invariants = loop.findLoopInvariants();
            TEST_ASSERT(!invariants.empty());

            // neither the initial assignment (is outside of the loop) nor the induction step (depends on induction
            // variable) are invariant
            TEST_ASSERT(std::find_if(invariants.begin(), invariants.end(), [&](InstructionWalker it) -> bool {
                return it.get() == inductionVar.initialAssignment || it.get() == inductionVar.inductionStep;
            }) == invariants.end());

            // any constant instruction in the loop is a loop-invariant instruction
            for(auto& block : loop)
            {
                auto it = block->key->walk();
                while(!it.isEndOfBlock())
                {
                    if(it.has() && it->isConstantInstruction())
                        TEST_ASSERT(invariants.find(it) != invariants.end());
                    it.nextInBlock();
                }
            }
        }

        // Run the optimization steps and do some more checks
        instance.optimize();

        loops = kernel->getCFG().findLoops(true, false);
        // 6 loops (2 for-loops, 3 for the work-group loop, 1 for work-group barrier)
        TEST_ASSERT_EQUALS(6u, loops.size());
        // the outer loops contain the inner, so we only have 2 innermost loops
        innerLoops = kernel->getCFG().findLoops(false);
        TEST_ASSERT_EQUALS(2u, innerLoops.size());
        // work-group loop has 2 blocks, inner actual loop has 1, the order is nondeterministic
        if(innerLoops.front().size() == 1)
        {
            TEST_ASSERT_EQUALS(2u, innerLoops.back().size());
        }
        else
        {
            TEST_ASSERT_EQUALS(2u, innerLoops.front().size());
            TEST_ASSERT_EQUALS(1u, innerLoops.back().size());
        }

        // sort by descending size, so we can easier handle includes
        std::sort(loops.begin(), loops.end(),
            [](const ControlFlowLoop& l1, const ControlFlowLoop& l2) -> bool { return l1.size() > l2.size(); });
        for(auto it1 = loops.begin(); it1 != loops.end(); ++it1)
        {
            for(auto it2 = it1 + 1; it2 != loops.end(); ++it2)
            {
                TEST_ASSERT(!it1->isWorkGroupLoop() || it1->includes(*it2));
                TEST_ASSERT(!it2->includes(*it1));
            }
        }
        // all loops have a single predecessor and successor, as well as a head and tail
        FastAccessList<const ControlFlowLoop*> workGroupLoops;
        for(std::size_t i = 0; i < loops.size(); ++i)
        {
            auto& loop = loops[i];
            auto numPredecessors = 1u;
            // the inner work-group loops have multiple predecessors (the start of CFG and the tails of the outer
            // work-group loops)
            if(i == 1) // second (y) work-group loop, has first (z) work-group loop as predecessor
                numPredecessors = 2u;
            if(i == 2) // third (x) work-group loop, as other two (z, y) work-group loops as predecessors
                numPredecessors = 3u;

            TEST_ASSERT_EQUALS(numPredecessors, loop.findPredecessors().size());
            TEST_ASSERT_EQUALS(numPredecessors == 1, !!loop.findPredecessor());
            TEST_ASSERT_EQUALS(1u, loop.findSuccessors().size());
            TEST_ASSERT(!!loop.getHeader());
            TEST_ASSERT(!!loop.getTail());
            if(loop.isWorkGroupLoop())
                workGroupLoops.emplace_back(&loop);
        }
        // 3 work-group loops (one per dimension)
        TEST_ASSERT_EQUALS(3u, workGroupLoops.size());
        // all work-group loops have the same header
        auto header = workGroupLoops.front()->getHeader();
        for(const auto& loop : workGroupLoops)
        {
            TEST_ASSERT_EQUALS(header, loop->getHeader());
        }

        // loops equal themselves, but not each other
        for(const auto& l1 : loops)
        {
            for(const auto& l2 : loops)
            {
                if(&l1 == &l2)
                {
                    TEST_ASSERT(l1 == l2);
                    TEST_ASSERT(l2 == l1);
                }
                else
                {
                    TEST_ASSERT(l1 != l2);
                    TEST_ASSERT(l2 != l1);
                }
            }
        }

        inclusionTree = createLoopInclusionTree(loops);
        TEST_ASSERT_EQUALS(6u, inclusionTree->getNodes().size());
        // since we sorted the loops in order of their inclusion, the longest path smaller than their index
        auto& base = inclusionTree->assertNode(&loops.front());
        for(unsigned i = 0; i < loops.size(); ++i)
        {
            auto& node = inclusionTree->assertNode(&loops[i]);
            TEST_ASSERT(i >= node.getLongestPathToRoot());
            TEST_ASSERT_EQUALS(&base, node.findRoot(12));
        }

        // both inner loops are simple enough so we should be able to detect the induction variable
        dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);
        for(auto& loop : loops)
        {
            if(loop.isWorkGroupLoop())
                continue;
            auto inductionVars = loop.findInductionVariables(*dataDependencies, true);
            if(inductionVars.size() != 1)
                continue;
            auto& inductionVar = inductionVars[0];
            TEST_ASSERT(!!inductionVar.local);
            TEST_ASSERT(!!inductionVar.initialAssignment);
            TEST_ASSERT(!!inductionVar.inductionStep);
            auto checkedVar = loop.checkInductionVariable(inductionVar.local, true);
            TEST_ASSERT(!!checkedVar);
            TEST_ASSERT_EQUALS(inductionVar.initialAssignment, checkedVar->initialAssignment);
            TEST_ASSERT_EQUALS(inductionVar.inductionStep, checkedVar->inductionStep);
            if(loop.size() == 1)
            {
                // for the inner loop we can't determine the range or iteration count, since the upper limit is
                // non-constant
                TEST_ASSERT_EQUALS(0_lit, inductionVar.getLowerBound());
                TEST_ASSERT(!inductionVar.getUpperBound());
                TEST_ASSERT(!inductionVar.getRange());
                TEST_ASSERT(!inductionVar.getIterationCount());
            }
            else
            {
                // for the outer loop we should be able to determine the iteration count
                TEST_ASSERT_EQUALS(0_lit, inductionVar.getLowerBound());
                // The upper bound might be 63 or 64 (with <= or < comparison respectively), depending on the LLVM
                // version
                TEST_ASSERT(!!inductionVar.getUpperBound());
                TEST_ASSERT(!!inductionVar.getRange());
                TEST_ASSERT_EQUALS(64u, inductionVar.getIterationCount().value());
            }

            // there are some constant address offset/index calculations
            auto invariants = loop.findLoopInvariants();
            TEST_ASSERT(!invariants.empty());

            // neither the initial assignment (is outside of the loop) nor the induction step (depends on induction
            // variable) are invariant
            TEST_ASSERT(std::find_if(invariants.begin(), invariants.end(), [&](InstructionWalker it) -> bool {
                return it.get() == inductionVar.initialAssignment || it.get() == inductionVar.inductionStep;
            }) == invariants.end());

            // any constant instruction in the loop is a loop-invariant instruction
            for(auto& block : loop)
            {
                auto it = block->key->walk();
                while(!it.isEndOfBlock())
                {
                    if(it.has() && it->isConstantInstruction())
                        TEST_ASSERT(invariants.find(it) != invariants.end());
                    it.nextInBlock();
                }
            }
        }
    }

    // second kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto loops = kernel->getCFG().findLoops(true, false);
        // 2 loops (2 for-loops)
        TEST_ASSERT_EQUALS(2u, loops.size());
        // the 2 loops do not contain each other
        auto innerLoops = kernel->getCFG().findLoops(false);
        TEST_ASSERT_EQUALS(2u, innerLoops.size());

        // loops do not include each other
        TEST_ASSERT(!loops.front().includes(loops.back()));
        TEST_ASSERT(!loops.back().includes(loops.front()));

        // both loops have a single predecessor
        TEST_ASSERT(!!loops.front().findPredecessor());
        TEST_ASSERT(!!loops.back().findPredecessor());
        // the also have a single successor
        TEST_ASSERT(!!loops.front().findSuccessor());
        TEST_ASSERT(!!loops.back().findSuccessor());
        // every loop has a header and a tail
        TEST_ASSERT(!!loops.front().getHeader());
        TEST_ASSERT(!!loops.front().getTail());
        TEST_ASSERT(!!loops.back().getHeader());
        TEST_ASSERT(!!loops.back().getTail());
        // neither is a work-group loop (since the optimization did not yet run)
        TEST_ASSERT(!loops.front().isWorkGroupLoop());
        TEST_ASSERT(!loops.back().isWorkGroupLoop());
        // loops equal themselves, but not each other
        TEST_ASSERT(loops.front() == loops.front());
        TEST_ASSERT(loops.back() == loops.back());
        TEST_ASSERT(loops.back() != loops.front());
        TEST_ASSERT(loops.front() != loops.back());

        auto inclusionTree = createLoopInclusionTree(loops);
        TEST_ASSERT_EQUALS(2u, inclusionTree->getNodes().size());
        TEST_ASSERT_EQUALS(0u, inclusionTree->assertNode(&loops.front()).getLongestPathToRoot());
        TEST_ASSERT_EQUALS(0u, inclusionTree->assertNode(&loops.back()).getLongestPathToRoot());

        // Run the optimization steps and do some more checks
        instance.optimize();

        loops = kernel->getCFG().findLoops(true, false);
        // 6 loops (2 for-loops, 3 for the work-group loop, 1 for work-group synchronization)
        TEST_ASSERT_EQUALS(6u, loops.size());
        // the 3 non work-group loops do not contain each other
        innerLoops = kernel->getCFG().findLoops(false);
        TEST_ASSERT_EQUALS(3u, innerLoops.size());

        // sort by descending size, so we can easier handle includes
        std::sort(loops.begin(), loops.end(),
            [](const ControlFlowLoop& l1, const ControlFlowLoop& l2) -> bool { return l1.size() > l2.size(); });
        for(auto it1 = loops.begin(); it1 != loops.end(); ++it1)
        {
            for(auto it2 = it1 + 1; it2 != loops.end(); ++it2)
            {
                if(it1->isWorkGroupLoop())
                {
                    TEST_ASSERT(it1->includes(*it2));
                }
                else
                {
                    TEST_ASSERT(!it1->includes(*it2));
                }
                TEST_ASSERT(!it2->includes(*it1));
            }
        }
        // all loops have a single predecessor and successor, as well as a head and tail
        FastAccessList<const ControlFlowLoop*> workGroupLoops;
        for(std::size_t i = 0; i < loops.size(); ++i)
        {
            auto& loop = loops[i];
            auto numPredecessors = 1u;
            // the inner work-group loops have multiple predecessors (the start of CFG and the tails of the outer
            // work-group loops)
            if(i == 1) // second (y) work-group loop, has first (z) work-group loop as predecessor
                numPredecessors = 2u;
            if(i == 2) // third (x) work-group loop, as other two (z, y) work-group loops as predecessors
                numPredecessors = 3u;

            TEST_ASSERT_EQUALS(numPredecessors, loop.findPredecessors().size());
            TEST_ASSERT_EQUALS(numPredecessors == 1, !!loop.findPredecessor());
            TEST_ASSERT_EQUALS(1u, loop.findSuccessors().size());
            TEST_ASSERT(!!loop.findSuccessor());
            TEST_ASSERT(!!loop.getHeader());
            TEST_ASSERT(!!loop.getTail());
            if(loop.isWorkGroupLoop())
                workGroupLoops.emplace_back(&loop);
        }
        // 3 work-group loops (one per dimension)
        TEST_ASSERT_EQUALS(3u, workGroupLoops.size());
        // all work-group loops have the same header
        auto header = workGroupLoops.front()->getHeader();
        for(const auto& loop : workGroupLoops)
        {
            TEST_ASSERT_EQUALS(header, loop->getHeader());
        }

        // loops equal themselves, but not each other
        for(const auto& l1 : loops)
        {
            for(const auto& l2 : loops)
            {
                if(&l1 == &l2)
                {
                    TEST_ASSERT(l1 == l2);
                    TEST_ASSERT(l2 == l1);
                }
                else
                {
                    TEST_ASSERT(l1 != l2);
                    TEST_ASSERT(l2 != l1);
                }
            }
        }

        inclusionTree = createLoopInclusionTree(loops);
        TEST_ASSERT_EQUALS(6u, inclusionTree->getNodes().size());
        auto& base = inclusionTree->assertNode(&loops.front());
        for(unsigned i = 0; i < loops.size(); ++i)
        {
            auto& node = inclusionTree->assertNode(&loops[i]);
            auto longestPath =
                loops[i].isWorkGroupLoop() ? i : 3u /* all for-loops are contained in the 3 work-group loops */;
            TEST_ASSERT_EQUALS(longestPath, node.getLongestPathToRoot());
            TEST_ASSERT_EQUALS(&base, node.findRoot(12));
        }
    }
}

void TestAnalyses::testDataDependency()
{
    // first kernel
    {
        CompilerInstance instance{config};
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();

        auto dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);

        // The start of control flow has no incoming dependencies
        auto& start = dataDependencies->assertNode(cfg.getStartOfControlFlow().key);
        TEST_ASSERT_EQUALS(0u, start.getAllIncomingDependencies().size());
        TEST_ASSERT(!start.getAllOutgoingDependencies().empty());
        // The end of control flow has no incoming or outgoing dependencies, so it has no dependency node at all
        TEST_ASSERT_EQUALS(nullptr, dataDependencies->findNode(cfg.getEndOfControlFlow().key));

        // The single inner-most loop has the induction variable as dependency to its dominator/preceding node and
        // itself
        auto loops = cfg.findLoops(false);
        TEST_ASSERT_EQUALS(1u, loops.size());
        auto preheader = loops[0].findPredecessor();
        auto header = loops[0].getHeader();
        TEST_ASSERT(!!preheader);
        TEST_ASSERT(!!header);
        auto& preheaderNode = dataDependencies->assertNode(preheader->key);
        auto& headerNode = dataDependencies->assertNode(header->key);
        TEST_ASSERT(!headerNode.getAllIncomingDependencies().empty());
        auto edge = preheaderNode.getEdge(&headerNode);
        TEST_ASSERT(!!edge);
        auto preheaderDependencies = edge->data.at(preheader->key);
        edge = headerNode.getEdge(&headerNode);
        TEST_ASSERT(!!edge);
        auto loopDependencies = edge->data.at(headerNode.key);
        TEST_ASSERT(!preheaderDependencies.empty());
        TEST_ASSERT(!loopDependencies.empty());
        auto inductionVars = loops[0].findInductionVariables(*dataDependencies, false);
        TEST_ASSERT_EQUALS(1u, inductionVars.size());
        auto preheaderIt = preheaderDependencies.end();
        auto loopIt = loopDependencies.end();
        for(auto it = preheaderDependencies.begin(); it != preheaderDependencies.end(); ++it)
        {
            if(it->first == inductionVars[0].local)
            {
                preheaderIt = it;
                break;
            }
        }
        for(auto it = loopDependencies.begin(); it != loopDependencies.end(); ++it)
        {
            if(it->first == inductionVars[0].local)
            {
                loopIt = it;
                break;
            }
        }
        TEST_ASSERT(preheaderIt != preheaderDependencies.end());
        TEST_ASSERT(loopIt != loopDependencies.end());
        TEST_ASSERT_EQUALS(preheaderIt->second, loopIt->second);

        auto deps = headerNode.getAllIncomingDependencies();
        TEST_ASSERT(deps.find(loopIt->first) != deps.end());
        deps = headerNode.getAllOutgoingDependencies();
        TEST_ASSERT(deps.find(loopIt->first) != deps.end());
        deps = preheaderNode.getAllOutgoingDependencies();
        TEST_ASSERT(deps.find(loopIt->first) != deps.end());
    }

    // second kernel
    {
        CompilerInstance instance{config};
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();

        auto dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);

        // The start of control flow has no incoming dependencies
        auto& start = dataDependencies->assertNode(cfg.getStartOfControlFlow().key);
        TEST_ASSERT_EQUALS(0u, start.getAllIncomingDependencies().size());
        TEST_ASSERT(!start.getAllOutgoingDependencies().empty());
        // The end of control flow has no incoming or outgoing dependencies, so it has no dependency node at all
        TEST_ASSERT_EQUALS(nullptr, dataDependencies->findNode(cfg.getEndOfControlFlow().key));

        // The 2 (or 3 depending on loop unrolling) inner-most loop have the induction variable as dependency to their
        // dominator/preceding node and themselves
        auto loops = cfg.findLoops(false);
        TEST_ASSERT(loops.size() >= 2u && loops.size() <= 3u);
        for(auto& loop : loops)
        {
            auto preheader = loop.findPredecessor();
            auto header = loop.getHeader();
            TEST_ASSERT(!!preheader);
            TEST_ASSERT(!!header);
            auto& preheaderNode = dataDependencies->assertNode(preheader->key);
            auto& headerNode = dataDependencies->assertNode(header->key);
            TEST_ASSERT(!headerNode.getAllIncomingDependencies().empty());
            auto edge = preheaderNode.getEdge(&headerNode);
            TEST_ASSERT(!!edge);
            auto preheaderDependencies = edge->data.at(preheader->key);
            auto loopNode = loop.begin();
            while(loopNode != loop.end() &&
                (edge = headerNode.getEdge(&dataDependencies->assertNode((*loopNode)->key))) == nullptr)
            {
                // the in-loop dependency does not necessarily originate from the header node (e.g. it does not for
                // barriers)
                ++loopNode;
            }
            TEST_ASSERT(!!edge);
            auto loopDependencies = edge->data.at(edge->getOtherNode(headerNode).key);
            TEST_ASSERT(!preheaderDependencies.empty());
            TEST_ASSERT(!loopDependencies.empty());
            auto inductionVars = loop.findInductionVariables(*dataDependencies, false);
            TEST_ASSERT_EQUALS(1u, inductionVars.size());
            auto preheaderIt = preheaderDependencies.end();
            auto loopIt = loopDependencies.end();
            for(auto it = preheaderDependencies.begin(); it != preheaderDependencies.end(); ++it)
            {
                if(it->first == inductionVars[0].local)
                {
                    preheaderIt = it;
                    break;
                }
            }
            for(auto it = loopDependencies.begin(); it != loopDependencies.end(); ++it)
            {
                if(it->first == inductionVars[0].local)
                {
                    loopIt = it;
                    break;
                }
            }
            TEST_ASSERT(preheaderIt != preheaderDependencies.end());
            TEST_ASSERT(loopIt != loopDependencies.end());
            TEST_ASSERT_EQUALS(preheaderIt->second, loopIt->second);

            auto deps = headerNode.getAllIncomingDependencies();
            TEST_ASSERT(deps.find(loopIt->first) != deps.end());
            deps = dataDependencies->assertNode((*loopNode)->key).getAllOutgoingDependencies();
            TEST_ASSERT(deps.find(loopIt->first) != deps.end());
            deps = preheaderNode.getAllOutgoingDependencies();
            TEST_ASSERT(deps.find(loopIt->first) != deps.end());
        }
    }
}

void TestAnalyses::testDependency() {}

void TestAnalyses::testDominatorTree()
{
    Configuration configCopy(config);
    configCopy.additionalEnabledOptimizations.emplace("loop-work-groups");
    configCopy.additionalEnabledOptimizations.emplace("reorder-blocks");
    configCopy.additionalEnabledOptimizations.emplace("simplify-branches");
    configCopy.additionalEnabledOptimizations.emplace("merge-blocks");

    // first kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        auto tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());

        // the start of the control flow dominates all nodes (except itself)
        auto* start = &tree->assertNode(&cfg.getStartOfControlFlow());
        TEST_ASSERT_EQUALS(0u, start->getDominators().size());
        TEST_ASSERT_EQUALS(numNodes - 1, start->getDominatedNodes().size());
        // the end of the control flow dominates no nodes
        auto* end = &tree->assertNode(&cfg.getEndOfControlFlow());
        TEST_ASSERT(!end->getDominators().empty());
        TEST_ASSERT_EQUALS(0u, end->getDominatedNodes().size());

        for(auto& node : tree->getNodes())
        {
            if(&node.second != start)
            {
                TEST_ASSERT(start->dominates(node.second));
                TEST_ASSERT(!start->isDominatedBy(node.second));
            }
        }

        tree = DominatorTree::createPostdominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());

        // the start of the control flow post-dominates no nodes
        start = &tree->assertNode(&cfg.getStartOfControlFlow());
        TEST_ASSERT(!start->getDominators().empty());
        TEST_ASSERT_EQUALS(0u, start->getDominatedNodes().size());
        // the end of the control flow post-dominates all nodes (except itself)
        end = &tree->assertNode(&cfg.getEndOfControlFlow());
        TEST_ASSERT_EQUALS(0u, end->getDominators().size());
        TEST_ASSERT_EQUALS(numNodes - 1, end->getDominatedNodes().size());

        for(auto& node : tree->getNodes())
        {
            if(&node.second != end)
            {
                TEST_ASSERT(end->dominates(node.second));
                TEST_ASSERT(!end->isDominatedBy(node.second));
            }
        }

        // Run the optimization steps and do some more checks
        instance.optimize();

        numNodes = cfg.getNodes().size();
        tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());
        tree = DominatorTree::createPostdominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());
    }

    // second kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        auto tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());

        // // the start of the control flow dominates all nodes (except itself)
        auto* start = &tree->assertNode(&cfg.getStartOfControlFlow());
        TEST_ASSERT_EQUALS(0u, start->getDominators().size());
        TEST_ASSERT_EQUALS(numNodes - 1, start->getDominatedNodes().size());
        // the end of the control flow dominates no nodes
        auto* end = &tree->assertNode(&cfg.getEndOfControlFlow());
        TEST_ASSERT(!end->getDominators().empty());
        TEST_ASSERT_EQUALS(0u, end->getDominatedNodes().size());

        for(auto& node : tree->getNodes())
        {
            if(&node.second != start)
            {
                TEST_ASSERT(start->dominates(node.second));
                TEST_ASSERT(!start->isDominatedBy(node.second));
            }
        }

        tree = DominatorTree::createPostdominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());

        // the start of the control flow post-dominates no nodes
        start = &tree->assertNode(&cfg.getStartOfControlFlow());
        TEST_ASSERT(!start->getDominators().empty());
        TEST_ASSERT_EQUALS(0u, start->getDominatedNodes().size());
        // the end of the control flow post-dominates all nodes (except itself)
        end = &tree->assertNode(&cfg.getEndOfControlFlow());
        TEST_ASSERT_EQUALS(0u, end->getDominators().size());
        TEST_ASSERT_EQUALS(numNodes - 1, end->getDominatedNodes().size());

        for(auto& node : tree->getNodes())
        {
            if(&node.second != end)
            {
                TEST_ASSERT(end->dominates(node.second));
                TEST_ASSERT(!end->isDominatedBy(node.second));
            }
        }

        // Run the optimization steps and do some more checks
        instance.optimize();

        numNodes = cfg.getNodes().size();
        tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());
        tree = DominatorTree::createPostdominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());
    }
}

void TestAnalyses::testStaticFlags()
{
    using namespace vc4c::intermediate;
    using namespace vc4c::operators;
    using analysis::StaticFlagsAnalysis;

    Module module{config};
    Method method(module);
    InstructionWalker it = method.createAndInsertNewBlock(method.begin(), "%dummy").walkEnd();

    // can be detected - constant flags
    {
        // test with simple scalar constant
        auto out = method.addNewLocal(TYPE_INT32);
        it.emplace(std::make_unique<LoadImmediate>(out, Literal(-12345)));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::SET, FlagStatus::CLEAR, FlagStatus::CLEAR}), elem);
        }

        // test with differing constant vector
        it.reset(std::make_unique<LoadImmediate>(out, 0x32100123, intermediate::LoadType::PER_ELEMENT_SIGNED));
        auto vector = LoadImmediate::toLoadedValues(0x32100123, intermediate::LoadType::PER_ELEMENT_SIGNED);
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(uint8_t i = 0; i < vector.size(); ++i)
        {
            TEST_ASSERT_EQUALS(vector[i].signedInt() == 0 ? FlagStatus::SET : FlagStatus::CLEAR, (*flags)[i].zero);
            TEST_ASSERT_EQUALS(vector[i].signedInt() < 0 ? FlagStatus::SET : FlagStatus::CLEAR, (*flags)[i].negative);
            TEST_ASSERT_EQUALS(FlagStatus::CLEAR, (*flags)[i].carry);
            TEST_ASSERT_EQUALS(FlagStatus::CLEAR, (*flags)[i].overflow);
        }

        it.nextInBlock();
    }

    // can be detected - basic boolean writing based on other flags
    {
        auto input = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        auto cond = assignNop(it) = as_unsigned{input} == as_unsigned{13_val};
        auto booleanValue = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, booleanValue) = (BOOL_FALSE, cond.invert());
        it.emplace(std::make_unique<MoveOperation>(NOP_REGISTER, booleanValue, COND_ALWAYS, SetFlag::SET_FLAGS));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::UNDEFINED, FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::CLEAR}), elem);
        }
        it.nextInBlock();
    }

    // can be detected - OR'ing boolean values
    {
        auto inputA = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        auto cond = assignNop(it) = as_unsigned{inputA} == as_unsigned{13_val};
        auto booleanA = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, booleanA) = (BOOL_FALSE, cond.invert());

        auto inputB = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        cond = assignNop(it) = as_unsigned{inputB} != as_unsigned{112_val};
        auto booleanB = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, booleanB) = (BOOL_FALSE, cond.invert());

        it.emplace(std::make_unique<Operation>(OP_OR, NOP_REGISTER, booleanA, booleanB));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::UNDEFINED, FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::CLEAR}), elem);
        }

        it.nextInBlock();
    }

    // can be detected - AND'ing boolean values
    {
        auto inputA = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        auto cond = assignNop(it) = as_unsigned{inputA} == as_unsigned{13_val};
        auto booleanA = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, booleanA) = (BOOL_FALSE, cond.invert());

        auto inputB = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        cond = assignNop(it) = as_unsigned{inputB} != as_unsigned{112_val};
        auto booleanB = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, booleanB) = (BOOL_FALSE, cond.invert());

        it.emplace(std::make_unique<Operation>(OP_AND, NOP_REGISTER, booleanA, booleanB));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::UNDEFINED, FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::CLEAR}), elem);
        }

        it.nextInBlock();
    }

    // can be detected - Or'ing boolean value with element number register (e.g. for branch flags)
    {
        auto input = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        auto cond = assignNop(it) = as_unsigned{input} == as_unsigned{13_val};
        auto booleanValue = assign(it, TYPE_BOOL) = (BOOL_TRUE, cond);
        assign(it, booleanValue) = (BOOL_FALSE, cond.invert());
        it.emplace(std::make_unique<Operation>(
            OP_OR, NOP_REGISTER, booleanValue, ELEMENT_NUMBER_REGISTER, COND_ALWAYS, SetFlag::SET_FLAGS));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it);
        TEST_ASSERT(!!flags);
        for(uint8_t i = 0; i < flags->size(); ++i)
        {
            TEST_ASSERT_EQUALS(i == 0 ? FlagStatus::UNDEFINED : FlagStatus::CLEAR, (*flags)[i].zero);
            TEST_ASSERT_EQUALS(FlagStatus::CLEAR, (*flags)[i].negative);
            TEST_ASSERT_EQUALS(FlagStatus::CLEAR, (*flags)[i].carry);
            TEST_ASSERT_EQUALS(FlagStatus::CLEAR, (*flags)[i].overflow);
        }

        it.nextInBlock();
    }

    // can be detected - flags output of some min/max instructions
    {
        auto input = assign(it, TYPE_INT16) = UNIFORM_REGISTER;

        it.emplace(std::make_unique<Operation>(OP_MAX, NOP_REGISTER, input, 13_val));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // can never be negative or zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.reset(std::make_unique<Operation>(OP_MIN, NOP_REGISTER, input, Value(Literal(-13), TYPE_INT16)));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // is always negative and never zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::SET, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.emplace(std::make_unique<Operation>(OP_FMAX, NOP_REGISTER, input, Value(Literal(-13.4f), TYPE_FLOAT)));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // may be negative and zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::UNDEFINED, FlagStatus::UNDEFINED, FlagStatus::UNDEFINED, FlagStatus::CLEAR}),
                elem);
        }

        it.emplace(std::make_unique<Operation>(OP_FMAXABS, NOP_REGISTER, input, Value(Literal(-13.4f), TYPE_FLOAT)));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // can never be negative or zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.emplace(std::make_unique<Operation>(OP_FMINABS, NOP_REGISTER, input, FLOAT_ZERO));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // is always zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::SET, FlagStatus::CLEAR, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.nextInBlock();
    }

    // can not be detected - flag set based on parameter
    {
        auto input = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        it.emplace(std::make_unique<MoveOperation>(NOP_REGISTER, input));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!flags);

        it.nextInBlock();
    }

    // can not be detected - flag based set on conditionally written local
    {
        // test with almost "setting boolean based on other flags", but with not zero/one values
        auto input = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        auto cond = assignNop(it) = as_unsigned{input} == as_unsigned{13_val};
        auto booleanValue = assign(it, TYPE_BOOL) = (32_val, cond);
        assign(it, booleanValue) = (4_val, cond.invert());
        it.emplace(std::make_unique<MoveOperation>(NOP_REGISTER, booleanValue, COND_ALWAYS, SetFlag::SET_FLAGS));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it);
        TEST_ASSERT(!flags);

        // test with multiple conditional writes
        cond = assignNop(it) = as_signed{input} > as_signed{32_val};
        assign(it, booleanValue) = (14_val, cond.invert());
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it);
        TEST_ASSERT(!flags);

        it.nextInBlock();
    }

    // can not be detected - instruction does not set flags
    {
        auto input = assign(it, TYPE_INT16) = UNIFORM_REGISTER;
        it.emplace(std::make_unique<MoveOperation>(NOP_REGISTER, input));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it);
        TEST_ASSERT(!flags);
    }
}

static Value getSourceValue(const Value& val)
{
    auto src = intermediate::getSourceValue(val);
    if(auto constant = src.getConstantValue())
        return *constant;
    if(auto writer = dynamic_cast<const intermediate::Operation*>(src.getSingleWriter()))
    {
        // revert the sign extension
        if(writer->op == OP_ASR)
        {
            auto offset = writer->assertArgument(1).getConstantValue();
            if(auto secondWriter =
                    dynamic_cast<const intermediate::Operation*>(writer->getFirstArg().getSingleWriter()))
            {
                if(secondWriter->op == OP_SHL && secondWriter->assertArgument(1) == offset)
                    return secondWriter->getFirstArg();
            }
        }
    }
    return src;
}

using ComparisonFunc = ComparisonWrapper (*)(const Value&, const Value&);

template <typename T, operators::ComparisonWrapper (*func)(T&&, T&&)>
static ComparisonFunc wrap()
{
    return static_cast<ComparisonFunc>(
        [](const Value& left, const Value& right) -> ComparisonWrapper { return func(T{left}, T{right}); });
}

template <typename T>
struct TestEntry
{
    T inputComparison;
    Value inputLeftOperand;
    Value inputRightOperand;
    const char* expectedComparison;
    Value expectedLeftOperand;
    Value expectedRightOperand;
    ConditionCode expectedCondition;
    std::bitset<NATIVE_VECTOR_SIZE> elementMask{0xFFFF};
};

void TestAnalyses::testIntegerComparisonDetection()
{
    using namespace vc4c::intermediate;
    Module module{config};
    Method m(module);
    auto& block = m.createAndInsertNewBlock(m.begin(), "dummyLabel");
    auto it = block.walkEnd();

    auto inInt0 = m.addNewLocal(TYPE_INT32, "%in.int");
    auto inInt1 = m.addNewLocal(TYPE_INT32, "%in.int");
    auto inShort0 = m.addNewLocal(TYPE_INT16, "%in.short");
    auto inShort1 = m.addNewLocal(TYPE_INT16, "%in.short");
    auto inChar0 = m.addNewLocal(TYPE_INT16, "%in.char");
    auto inChar1 = m.addNewLocal(TYPE_INT16, "%in.char");
    auto inFloat0 = m.addNewLocal(TYPE_FLOAT, "%in.float");
    auto inFloat1 = m.addNewLocal(TYPE_FLOAT, "%in.float");

    // Test front-end comparisons

    using StringEntry = TestEntry<const char*>;
    std::vector<StringEntry> tests = {

        // general 32-bit comparisons
        StringEntry{COMP_EQ, inInt0, inInt1, COMP_EQ, inInt0, inInt1, COND_ZERO_SET},
        StringEntry{COMP_NEQ, inInt0, inInt1, COMP_NEQ, inInt0, inInt1, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GE, inInt0, inInt1, COMP_UNSIGNED_GE, inInt0, inInt1, COND_ZERO_SET},
        StringEntry{COMP_UNSIGNED_GT, inInt0, inInt1, COMP_UNSIGNED_LT, inInt1, inInt0, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_LT, inInt0, inInt1, COMP_UNSIGNED_LT, inInt0, inInt1, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_LE, inInt0, inInt1, COMP_UNSIGNED_GE, inInt1, inInt0, COND_ZERO_SET},
        StringEntry{COMP_SIGNED_GE, inInt0, inInt1, COMP_SIGNED_LE, inInt1, inInt0, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GT, inInt0, inInt1, COMP_SIGNED_GT, inInt0, inInt1, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LT, inInt0, inInt1, COMP_SIGNED_GT, inInt1, inInt0, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LE, inInt0, inInt1, COMP_SIGNED_LE, inInt0, inInt1, COND_CARRY_CLEAR},
        // 32-bit comparisons with power of 2
        StringEntry{COMP_EQ, inInt0, 256_val, COMP_EQ, inInt0, 256_val, COND_ZERO_SET},
        StringEntry{COMP_NEQ, inInt0, 256_val, COMP_NEQ, inInt0, 256_val, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GE, inInt0, 256_val, COMP_UNSIGNED_GE, inInt0, 256_val, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GT, inInt0, 256_val, COMP_UNSIGNED_LT, 256_val, inInt0, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_LT, inInt0, 256_val, COMP_UNSIGNED_LT, inInt0, 256_val, COND_ZERO_SET},
        StringEntry{COMP_UNSIGNED_LE, inInt0, 256_val, COMP_UNSIGNED_GE, 256_val, inInt0, COND_ZERO_SET},
        StringEntry{COMP_SIGNED_GE, inInt0, 256_val, COMP_SIGNED_LE, 256_val, inInt0, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GT, inInt0, 256_val, COMP_SIGNED_GT, inInt0, 256_val, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LT, inInt0, 256_val, COMP_SIGNED_GT, 256_val, inInt0, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LE, inInt0, 256_val, COMP_SIGNED_LE, inInt0, 256_val, COND_CARRY_CLEAR},
        // 32-bit comparisons with 2^x-1
        StringEntry{COMP_EQ, inInt0, 511_val, COMP_EQ, inInt0, 511_val, COND_ZERO_SET},
        StringEntry{COMP_NEQ, inInt0, 511_val, COMP_NEQ, inInt0, 511_val, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GE, inInt0, 511_val, COMP_UNSIGNED_GE, inInt0, 511_val, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GT, inInt0, 511_val, COMP_UNSIGNED_LE, 512_val, inInt0, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_LT, inInt0, 511_val, COMP_UNSIGNED_LT, inInt0, 511_val, COND_ZERO_SET},
        StringEntry{COMP_UNSIGNED_LE, inInt0, 511_val, COMP_UNSIGNED_GT, 512_val, inInt0, COND_ZERO_SET},
        StringEntry{COMP_SIGNED_GE, inInt0, 511_val, COMP_SIGNED_LE, 511_val, inInt0, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GT, inInt0, 511_val, COMP_SIGNED_GT, inInt0, 511_val, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LT, inInt0, 511_val, COMP_SIGNED_GT, 511_val, inInt0, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LE, inInt0, 511_val, COMP_SIGNED_LE, inInt0, 511_val, COND_CARRY_CLEAR},
        // 32-bit comparison with 0
        StringEntry{COMP_EQ, inInt0, 0_val, COMP_EQ, inInt0, 0_val, COND_ZERO_SET},
        StringEntry{COMP_NEQ, inInt0, 0_val, COMP_NEQ, inInt0, 0_val, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GT, inInt0, 0_val, COMP_NEQ, inInt0, 0_val, COND_ZERO_CLEAR},
        StringEntry{COMP_SIGNED_GE, inInt0, 0_val, COMP_SIGNED_LE, 0_val, inInt0, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GT, inInt0, 0_val, COMP_SIGNED_GT, inInt0, 0_val, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LT, inInt0, 0_val, COMP_SIGNED_GT, 0_val, inInt0, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LE, inInt0, 0_val, COMP_SIGNED_LE, inInt0, 0_val, COND_CARRY_CLEAR},
        // general 16-bit comparisons
        StringEntry{COMP_EQ, inShort0, inShort1, COMP_EQ, inShort0, inShort1, COND_ZERO_SET},
        StringEntry{COMP_NEQ, inShort0, inShort1, COMP_NEQ, inShort0, inShort1, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GE, inShort0, inShort1, COMP_SIGNED_LE, inShort1, inShort0, COND_CARRY_CLEAR},
        StringEntry{COMP_UNSIGNED_GT, inShort0, inShort1, COMP_SIGNED_GT, inShort0, inShort1, COND_CARRY_SET},
        StringEntry{COMP_UNSIGNED_LT, inShort0, inShort1, COMP_SIGNED_GT, inShort1, inShort0, COND_CARRY_SET},
        StringEntry{COMP_UNSIGNED_LE, inShort0, inShort1, COMP_SIGNED_LE, inShort0, inShort1, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GE, inShort0, inShort1, COMP_SIGNED_LE, inShort1, inShort0, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GT, inShort0, inShort1, COMP_SIGNED_GT, inShort0, inShort1, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LT, inShort0, inShort1, COMP_SIGNED_GT, inShort1, inShort0, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LE, inShort0, inShort1, COMP_SIGNED_LE, inShort0, inShort1, COND_CARRY_CLEAR},
        // general 8-bit comparisons
        StringEntry{COMP_EQ, inChar0, inChar1, COMP_EQ, inChar0, inChar1, COND_ZERO_SET},
        StringEntry{COMP_NEQ, inChar0, inChar1, COMP_NEQ, inChar0, inChar1, COND_ZERO_CLEAR},
        StringEntry{COMP_UNSIGNED_GE, inChar0, inChar1, COMP_SIGNED_LE, inChar1, inChar0, COND_CARRY_CLEAR},
        StringEntry{COMP_UNSIGNED_GT, inChar0, inChar1, COMP_SIGNED_GT, inChar0, inChar1, COND_CARRY_SET},
        StringEntry{COMP_UNSIGNED_LT, inChar0, inChar1, COMP_SIGNED_GT, inChar1, inChar0, COND_CARRY_SET},
        StringEntry{COMP_UNSIGNED_LE, inChar0, inChar1, COMP_SIGNED_LE, inChar0, inChar1, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GE, inChar0, inChar1, COMP_SIGNED_LE, inChar1, inChar0, COND_CARRY_CLEAR},
        StringEntry{COMP_SIGNED_GT, inChar0, inChar1, COMP_SIGNED_GT, inChar0, inChar1, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LT, inChar0, inChar1, COMP_SIGNED_GT, inChar1, inChar0, COND_CARRY_SET},
        StringEntry{COMP_SIGNED_LE, inChar0, inChar1, COMP_SIGNED_LE, inChar0, inChar1, COND_CARRY_CLEAR},
        // TODO float and (u)long comparisons
    };

    for(auto& test : tests)
    {
        auto out = m.addNewLocal(TYPE_BOOL, std::string("%out.") + test.inputComparison);
        auto& compInst = it.emplace(std::make_unique<Comparison>(
            test.inputComparison, Value(out), Value(test.inputLeftOperand), Value(test.inputRightOperand)));
        auto comparisonString = it->to_string();
        auto checkIt = it.copy().previousInBlock();
        TEST_ASSERT(intrinsics::intrinsifyComparison(m, typeSafe(it, compInst)));

        auto comp = analysis::getComparison(out.local(), checkIt);
        if(!comp)
            TEST_ASSERT_EQUALS(comparisonString, "");
        TEST_ASSERT_EQUALS(test.expectedComparison, comp->name);
        auto leftOperand = ::getSourceValue(comp->leftOperand);
        leftOperand = leftOperand.getConstantValue().value_or(leftOperand);
        TEST_ASSERT_EQUALS(test.expectedLeftOperand, leftOperand);
        auto rightOperand = ::getSourceValue(comp->rightOperand);
        rightOperand = rightOperand.getConstantValue().value_or(rightOperand);
        TEST_ASSERT_EQUALS(test.expectedRightOperand, rightOperand);
        if(test.expectedCondition != comp->condition)
            TEST_ASSERT_EQUALS(test.expectedCondition.to_string(), comp->condition.to_string());
        TEST_ASSERT_EQUALS(out.local(), comp->result);
        TEST_ASSERT_EQUALS(test.elementMask, comp->elementMask);

        if(test.expectedLeftOperand.getLiteralValue() || test.expectedRightOperand.getLiteralValue())
        {
            // second test with literal values mapped to small immediate values/loads
            for(auto litIt = it; !litIt.isEndOfBlock(); litIt.nextInBlock())
                litIt = normalization::handleImmediate(module, m, litIt, config);

            comp = analysis::getComparison(out.local(), checkIt);
            if(!comp)
                TEST_ASSERT_EQUALS(comparisonString, "");
            TEST_ASSERT_EQUALS(test.expectedComparison, comp->name);
            auto leftOperand = ::getSourceValue(comp->leftOperand);
            leftOperand = leftOperand.getConstantValue().value_or(leftOperand);
            if(!leftOperand.getLiteralValue() ||
                test.expectedLeftOperand.getLiteralValue() != *leftOperand.getLiteralValue())
                TEST_ASSERT_EQUALS(test.expectedLeftOperand, leftOperand);
            auto rightOperand = ::getSourceValue(comp->rightOperand);
            rightOperand = rightOperand.getConstantValue().value_or(rightOperand);
            if(!rightOperand.getLiteralValue() ||
                test.expectedRightOperand.getLiteralValue() != *rightOperand.getLiteralValue())
                TEST_ASSERT_EQUALS(test.expectedRightOperand, rightOperand);
            if(test.expectedCondition != comp->condition)
                TEST_ASSERT_EQUALS(test.expectedCondition.to_string(), comp->condition.to_string());
            TEST_ASSERT_EQUALS(out.local(), comp->result);
            TEST_ASSERT_EQUALS(test.elementMask, comp->elementMask);
        }

        it.nextInBlock();
    }

    // Test internal comparison helpers

    using FuncEntry = TestEntry<ComparisonFunc>;
    std::vector<FuncEntry> operatorTests = {
        FuncEntry{
            wrap < as_float, operators::operator==>(), inFloat0, inFloat1, COMP_EQ, inFloat0, inFloat1, COND_ZERO_SET},
        FuncEntry{wrap < as_signed, operators::operator==>(), inInt0, inInt1, COMP_EQ, inInt0, inInt1, COND_ZERO_SET},
        FuncEntry{wrap < as_unsigned, operators::operator==>(), inInt0, inInt1, COMP_EQ, inInt0, inInt1, COND_ZERO_SET},
        FuncEntry{wrap < as_float, operators::operator!=>(), inFloat0, inFloat1, COMP_EQ, inFloat0, inFloat1,
            COND_ZERO_CLEAR},
        FuncEntry{wrap < as_signed, operators::operator!=>(), inInt0, inInt1, COMP_EQ, inInt0, inInt1, COND_ZERO_CLEAR},
        FuncEntry{
            wrap < as_unsigned, operators::operator!=>(), inInt0, inInt1, COMP_EQ, inInt0, inInt1, COND_ZERO_CLEAR},
        FuncEntry{wrap < as_signed, operators::operator> /**/>(), inInt0, inInt1, COMP_SIGNED_GT, inInt0, inInt1,
            COND_CARRY_SET},
        FuncEntry{wrap < as_signed, operators::operator>=>(), inInt0, inInt1, COMP_SIGNED_GT, inInt1, inInt0,
            COND_CARRY_CLEAR},
        FuncEntry{
            wrap < as_signed, operators::operator<>(), inInt0, inInt1, COMP_SIGNED_GT, inInt1, inInt0, COND_CARRY_SET},
        FuncEntry{wrap < as_signed, operators::operator<= /**/>(), inInt0, inInt1, COMP_SIGNED_GT, inInt0, inInt1,
            COND_CARRY_CLEAR},
        // TODO float comparison operators
    };

    for(auto& test : operatorTests)
    {
        auto cond = assignNop(it) = test.inputComparison(test.inputLeftOperand, test.inputRightOperand);
        auto checkIt = it.copy().previousInBlock();
        auto dummyOutput = assign(it, TYPE_INT32) = (NOP_REGISTER, cond);
        (void) dummyOutput;

        auto comp = analysis::getComparison(it.copy().previousInBlock().get(), checkIt);
        if(!comp)
            TEST_ASSERT_EQUALS(test.inputLeftOperand.to_string() + " " + test.expectedComparison + " " +
                    test.inputRightOperand.to_string(),
                "");
        TEST_ASSERT_EQUALS(test.expectedComparison, comp->name);
        auto leftOperand = ::getSourceValue(comp->leftOperand);
        leftOperand = leftOperand.getConstantValue().value_or(leftOperand);
        TEST_ASSERT_EQUALS(test.expectedLeftOperand, leftOperand);
        auto rightOperand = ::getSourceValue(comp->rightOperand);
        rightOperand = rightOperand.getConstantValue().value_or(rightOperand);
        TEST_ASSERT_EQUALS(test.expectedRightOperand, rightOperand);
        if(test.expectedCondition != cond)
            // NOTE: This check is different than for the source-language comparison checks above
            TEST_ASSERT_EQUALS(test.expectedCondition.to_string(), cond.to_string());
        TEST_ASSERT_EQUALS(nullptr, comp->result);
        TEST_ASSERT_EQUALS(test.elementMask, comp->elementMask);
    }

    // Non-standard comparisons

    {
        // comparison with elem_num on single element only
        auto startIt = it.copy().previousInBlock();
        auto expectedCond = assignNop(it) = selectSIMDElement(13);
        it.emplace(std::make_unique<MoveOperation>(NOP_REGISTER, NOP_REGISTER, expectedCond));

        auto comp = analysis::getComparison(it.get(), startIt);
        TEST_ASSERT(!!comp);
        TEST_ASSERT_EQUALS(COMP_EQ, comp->name);
        TEST_ASSERT_EQUALS(ELEMENT_NUMBER_REGISTER, comp->leftOperand);
        TEST_ASSERT_EQUALS(13_lit, comp->rightOperand.getLiteralValue());
        TEST_ASSERT_EQUALS(nullptr, comp->result);
        TEST_ASSERT_EQUALS(expectedCond, comp->condition);
        TEST_ASSERT_EQUALS(std::bitset<NATIVE_VECTOR_SIZE>{0xFFFF}, comp->elementMask);

        it.nextInBlock();
    }
}

void TestAnalyses::testActiveWorkItems()
{
    Configuration configCopy(config);
    configCopy.additionalEnabledOptimizations.emplace("loop-work-groups");
    configCopy.additionalEnabledOptimizations.emplace("reorder-blocks");
    configCopy.additionalEnabledOptimizations.emplace("simplify-branches");
    configCopy.additionalEnabledOptimizations.emplace("merge-blocks");

    // first kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss;
        ss << "__attribute__((reqd_work_group_size(7,1,1)))\n";
        ss << KERNEL_NESTED_LOOPS;
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        auto items = determineActiveWorkItems(*kernel, cfg);
        TEST_ASSERT_EQUALS(numNodes, items.size());

        // all blocks have the same limit
        for(const auto& item : items)
        {
            TEST_ASSERT_EQUALS(WorkItemCondition::LOCAL_ID_X, item.second.condition);
            TEST_ASSERT_EQUALS(7u, item.second.activeElements.size());
            TEST_ASSERT(item.second.inactiveElements.empty());
        }

        // Run the optimization steps and do some more checks
        instance.optimize();

        numNodes = cfg.getNodes().size();
        items = determineActiveWorkItems(*kernel, cfg);
        TEST_ASSERT_EQUALS(numNodes, items.size());

        // all blocks have either a single local ID scalar (within barrier) or the same global local ID X limit
        for(const auto& item : items)
        {
            if(item.second.condition == WorkItemCondition::LOCAL_ID_X)
            {
                TEST_ASSERT_EQUALS(7u, item.second.activeElements.size());
                TEST_ASSERT(item.second.inactiveElements.empty());
            }
            else if(item.second.condition == WorkItemCondition::LOCAL_ID_SCALAR)
            {
                TEST_ASSERT(item.second.activeElements.size() == 1u || item.second.inactiveElements.size() == 1u);
            }
        }
    }

    // second kernel
    {
        CompilerInstance instance{configCopy};
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        instance.precompileAndParseInput(CompilationData{ss});
        instance.normalize();

        TEST_ASSERT_EQUALS(1u, instance.module.getKernels().size());
        auto kernel = instance.module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        auto items = determineActiveWorkItems(*kernel, cfg);
        // we have items for: all barrier nodes, the local-ID 0 only blocks
        TEST_ASSERT(!items.empty());

        // all blocks have either a single local ID scalar (within barrier) or the same global local ID X limit
        for(const auto& item : items)
        {
            if(item.second.condition == WorkItemCondition::LOCAL_ID_X)
            {
                if(!item.second.activeElements.empty())
                {
                    TEST_ASSERT_EQUALS(1u, item.second.activeElements.size());
                    TEST_ASSERT_EQUALS(0u, *item.second.activeElements.begin());
                    TEST_ASSERT(item.second.inactiveElements.empty());
                }
                else
                {
                    TEST_ASSERT_EQUALS(1u, item.second.inactiveElements.size());
                    TEST_ASSERT_EQUALS(0u, *item.second.inactiveElements.begin());
                    TEST_ASSERT(item.second.activeElements.empty());
                }
            }
            else if(item.second.condition == WorkItemCondition::LOCAL_ID_SCALAR)
            {
                TEST_ASSERT(item.second.activeElements.size() == 1u || item.second.inactiveElements.size() == 1u);
            }
        }

        // Run the optimization steps and do some more checks
        instance.optimize();

        numNodes = cfg.getNodes().size();
        items = determineActiveWorkItems(*kernel, cfg);
        // we have items for: all barrier nodes, the local-ID 0 only blocks
        TEST_ASSERT(!items.empty());

        // all blocks have either a single local ID scalar (within barrier) or the same global local ID X limit
        for(const auto& item : items)
        {
            if(item.second.condition == WorkItemCondition::LOCAL_ID_X)
            {
                if(!item.second.activeElements.empty())
                {
                    TEST_ASSERT_EQUALS(1u, item.second.activeElements.size());
                    TEST_ASSERT_EQUALS(0u, *item.second.activeElements.begin());
                    TEST_ASSERT(item.second.inactiveElements.empty());
                }
                else
                {
                    TEST_ASSERT_EQUALS(1u, item.second.inactiveElements.size());
                    TEST_ASSERT_EQUALS(0u, *item.second.inactiveElements.begin());
                    TEST_ASSERT(item.second.activeElements.empty());
                }
            }
            else if(item.second.condition == WorkItemCondition::LOCAL_ID_SCALAR)
            {
                TEST_ASSERT(item.second.activeElements.size() == 1u || item.second.inactiveElements.size() == 1u);
            }
        }
    }
}
