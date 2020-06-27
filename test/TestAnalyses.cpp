/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestAnalyses.h"

#include "analysis/ControlFlowGraph.h"
#include "analysis/DataDependencyGraph.h"
#include "analysis/DominatorTree.h"
#include "analysis/FlagsAnalysis.h"
#include "intermediate/operators.h"

using namespace vc4c;
using namespace vc4c::analysis;

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

TestAnalyses::TestAnalyses(const Configuration& config) : TestCompilationHelper(config)
{
    TEST_ADD(TestAnalyses::testAvailableExpressions);
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
        Module module(configCopy);
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 10 nodes
        TEST_ASSERT_EQUALS(10, numNodes);
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
        // 3 implicit edges (into and out of the get_global_id() function call)
        TEST_ASSERT_EQUALS(3u, implicitEdges.size());
        // no work-group edges (they are only created by optimizations which are not yet run)
        TEST_ASSERT_EQUALS(0u, workGroupEdges.size());

        // Modifying the clone leaves the original CFG intact
        auto copy = cfg.clone();
        copy->getNodes().begin()->second.erase();
        TEST_ASSERT_EQUALS(numNodes - 1, copy->getNodes().size());
        TEST_ASSERT_EQUALS(numNodes, cfg.getNodes().size());

        // Run the optimization steps and do some more checks
        optimize(module);

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 10 nodes (some added for the work-group loop, some merged)
        TEST_ASSERT_EQUALS(10, numNodes);
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
        // 5 back edges (2 for the inner loops + 3 for the work-group loops)
        TEST_ASSERT_EQUALS(5u, backEdges.size());
        // 6 implicit edges (into first loop, 2 * out of if-blocks, out of second loop, 2 * for work-group loop)
        TEST_ASSERT_EQUALS(6u, implicitEdges.size());
        // 3 work-group edges (one per dimension)
        TEST_ASSERT_EQUALS(3u, workGroupEdges.size());
    }

    // second kernel
    {
        Module module(configCopy);
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 10 nodes
        TEST_ASSERT_EQUALS(40, numNodes);
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
        // 4 implicit edges (into and out of the function calls)
        TEST_ASSERT_EQUALS(7u, implicitEdges.size());
        // no work-group edges (they are only created by optimizations which are not yet run)
        TEST_ASSERT_EQUALS(0u, workGroupEdges.size());

        // Modifying the clone leaves the original CFG intact
        auto copy = cfg.clone();
        copy->getNodes().begin()->second.erase();
        TEST_ASSERT_EQUALS(numNodes - 1, copy->getNodes().size());
        TEST_ASSERT_EQUALS(numNodes, cfg.getNodes().size());

        // Run the optimization steps and do some more checks
        optimize(module);

        // This kernel has:
        // A start of the CFG
        TEST_THROWS_NOTHING(cfg.getStartOfControlFlow());
        // An end of the CFG
        TEST_ASSERT_EQUALS(BasicBlock::LAST_BLOCK, cfg.getEndOfControlFlow().key->getLabel()->getLabel()->name);
        // 40 nodes (some added for the work-group loop, some merged)
        TEST_ASSERT_EQUALS(40, numNodes);
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
        // 5 back edges (2 for the inner loops + 3 for the work-group loops)
        TEST_ASSERT_EQUALS(5u, backEdges.size());
        // 18 implicit edges (into first loop, out of if-block, out of second loop, 2 * for work-group loop +  a lot for
        // if-then-after blocks)
        TEST_ASSERT_EQUALS(18u, implicitEdges.size());
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
        Module module(configCopy);
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
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

        auto inclusionTree = createLoopInclusingTree(loops);
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
            TEST_ASSERT(!!inductionVars[0].local);
            TEST_ASSERT(!!inductionVars[0].initialAssignment);
            TEST_ASSERT(!!inductionVars[0].inductionStep);
            // TODO TEST_ASSERT(!!inductionVars[0].repeatCondition);

            // there are some constant address offset/index calculations
            auto invariants = loop.findLoopInvariants();
            TEST_ASSERT(!invariants.empty());

            // neither the initial assignment (is outside of the loop) nor the induction step (depends on induction
            // variable) are invariant
            TEST_ASSERT(std::find_if(invariants.begin(), invariants.end(), [&](InstructionWalker it) -> bool {
                return it.get() == inductionVars[0].initialAssignment || it.get() == inductionVars[0].inductionStep;
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
        optimize(module);

        loops = kernel->getCFG().findLoops(true, false);
        // 5 loops (2 for-loops, 3 for the work-group loop)
        TEST_ASSERT_EQUALS(5, loops.size());
        // the outer loops contains the inner, so we only have 1 innermost loop
        innerLoops = kernel->getCFG().findLoops(false);
        TEST_ASSERT_EQUALS(1u, innerLoops.size());
        TEST_ASSERT_EQUALS(1u, innerLoops.begin()->size());

        // sort by descending size, so we can easier handle includes
        std::sort(loops.begin(), loops.end(),
            [](const ControlFlowLoop& l1, const ControlFlowLoop& l2) -> bool { return l1.size() > l2.size(); });
        for(auto it1 = loops.begin(); it1 != loops.end(); ++it1)
        {
            for(auto it2 = it1 + 1; it2 != loops.end(); ++it2)
            {
                TEST_ASSERT(it1->includes(*it2));
                TEST_ASSERT(!it2->includes(*it1));
            }
        }
        // all loops have a single predecessor and successor, as well as a head and tail
        FastAccessList<const ControlFlowLoop*> workGroupLoops;
        for(std::size_t i = 0; i < loops.size(); ++i)
        {
            auto& loop = loops[i];
            auto numPredecessors = 1;
            // the inner work-group loops have multiple predecessors (the start of CFG and the tails of the outer
            // work-group loops)
            if(i == 1) // second (y) work-group loop, has first (z) work-group loop as predecessor
                numPredecessors = 2;
            if(i == 2) // third (x) work-group loop, as other two (z, y) work-group loops as predecessors
                numPredecessors = 3;

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

        inclusionTree = createLoopInclusingTree(loops);
        TEST_ASSERT_EQUALS(5u, inclusionTree->getNodes().size());
        // since we sorted the loops in order of their inclusion, the longest path is their index
        auto& base = inclusionTree->assertNode(&loops.front());
        for(unsigned i = 0; i < loops.size(); ++i)
        {
            auto& node = inclusionTree->assertNode(&loops[i]);
            TEST_ASSERT_EQUALS(i, node.getLongestPathToRoot());
            TEST_ASSERT_EQUALS(&base, node.findRoot(12));

            if(i > 2)
                TEST_ASSERT_EQUALS(&inclusionTree->assertNode(&loops[i - 2]), node.findRoot(2));
        }

        // both inner loops are simple enough so we should be able to detect the induction variable
        dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);
        for(auto& loop : loops)
        {
            if(loop.isWorkGroupLoop())
                continue;
            auto inductionVars = loop.findInductionVariables(*dataDependencies, true);
            TEST_ASSERT_EQUALS(1u, inductionVars.size());
            TEST_ASSERT(!!inductionVars[0].local);
            TEST_ASSERT(!!inductionVars[0].initialAssignment);
            TEST_ASSERT(!!inductionVars[0].inductionStep);
            // TODO TEST_ASSERT(!!inductionVars[0].repeatCondition);

            // there are some constant address offset/index calculations
            auto invariants = loop.findLoopInvariants();
            TEST_ASSERT(!invariants.empty());

            // neither the initial assignment (is outside of the loop) nor the induction step (depends on induction
            // variable) are invariant
            TEST_ASSERT(std::find_if(invariants.begin(), invariants.end(), [&](InstructionWalker it) -> bool {
                return it.get() == inductionVars[0].initialAssignment || it.get() == inductionVars[0].inductionStep;
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
        Module module(configCopy);
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
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

        auto inclusionTree = createLoopInclusingTree(loops);
        TEST_ASSERT_EQUALS(2u, inclusionTree->getNodes().size());
        TEST_ASSERT_EQUALS(0u, inclusionTree->assertNode(&loops.front()).getLongestPathToRoot());
        TEST_ASSERT_EQUALS(0u, inclusionTree->assertNode(&loops.back()).getLongestPathToRoot());

        // Run the optimization steps and do some more checks
        optimize(module);

        loops = kernel->getCFG().findLoops(true, false);
        // 5 loops (2 for-loops, 3 for the work-group loop)
        TEST_ASSERT_EQUALS(5, loops.size());
        // the 2 non work-group loops do not contain each other
        innerLoops = kernel->getCFG().findLoops(false);
        TEST_ASSERT_EQUALS(2u, innerLoops.size());

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
            auto numPredecessors = 1;
            // the inner work-group loops have multiple predecessors (the start of CFG and the tails of the outer
            // work-group loops)
            if(i == 1) // second (y) work-group loop, has first (z) work-group loop as predecessor
                numPredecessors = 2;
            if(i == 2) // third (x) work-group loop, as other two (z, y) work-group loops as predecessors
                numPredecessors = 3;

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

        inclusionTree = createLoopInclusingTree(loops);
        TEST_ASSERT_EQUALS(5u, inclusionTree->getNodes().size());
        auto& base = inclusionTree->assertNode(&loops.front());
        for(unsigned i = 0; i < loops.size(); ++i)
        {
            auto& node = inclusionTree->assertNode(&loops[i]);
            auto longestPath =
                loops[i].isWorkGroupLoop() ? i : 3u /* both for-loops are contained in the 3 work-group loops */;
            TEST_ASSERT_EQUALS(longestPath, node.getLongestPathToRoot());
            TEST_ASSERT_EQUALS(&base, node.findRoot(12));
        }
    }
}

void TestAnalyses::testDataDependency()
{
    // first kernel
    {
        Module module(config);
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
        auto& cfg = kernel->getCFG();

        auto dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);

        // The start of control flow has no incoming dependencies
        auto& start = dataDependencies->assertNode(cfg.getStartOfControlFlow().key);
        TEST_ASSERT_EQUALS(0, start.getAllIncomingDependencies().size());
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
        Module module(config);
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
        auto& cfg = kernel->getCFG();

        auto dataDependencies = analysis::DataDependencyGraph::createDependencyGraph(*kernel);
        TEST_ASSERT(!!dataDependencies);

        // The start of control flow has no incoming dependencies
        auto& start = dataDependencies->assertNode(cfg.getStartOfControlFlow().key);
        TEST_ASSERT_EQUALS(0, start.getAllIncomingDependencies().size());
        TEST_ASSERT(!start.getAllOutgoingDependencies().empty());
        // The end of control flow has no incoming or outgoing dependencies, so it has no dependency node at all
        TEST_ASSERT_EQUALS(nullptr, dataDependencies->findNode(cfg.getEndOfControlFlow().key));

        // The 2 inner-most loop has the induction variable as dependency to its dominator/preceding node and themselves
        auto loops = cfg.findLoops(false);
        TEST_ASSERT_EQUALS(2u, loops.size());
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
            edge = headerNode.getEdge(&headerNode);
            TEST_ASSERT(!!edge);
            auto loopDependencies = edge->data.at(headerNode.key);
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
            deps = headerNode.getAllOutgoingDependencies();
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
        Module module(configCopy);
        std::stringstream ss(KERNEL_NESTED_LOOPS);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        auto tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());

        // // the start of the control flow dominates all nodes (except itself)
        auto& start = tree->assertNode(&cfg.getStartOfControlFlow());
        TEST_ASSERT_EQUALS(0, start.getDominators().size());
        TEST_ASSERT_EQUALS(numNodes - 1, start.getDominatedNodes().size());
        // the end of the control flow dominates no nodes
        auto& end = tree->assertNode(&cfg.getEndOfControlFlow());
        TEST_ASSERT(end.getDominators().size() > 4);
        TEST_ASSERT_EQUALS(0, end.getDominatedNodes().size());

        for(auto& node : tree->getNodes())
        {
            if(&node.second != &start)
            {
                TEST_ASSERT(start.dominates(node.second));
                TEST_ASSERT(!start.isDominatedBy(node.second));
            }
        }

        // Run the optimization steps and do some more checks
        optimize(module);

        numNodes = cfg.getNodes().size();
        tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());
    }

    // second kernel
    {
        Module module(configCopy);
        std::stringstream ss(KERNEL_LOOP_BARRIER);
        precompileAndParse(module, ss, "");
        normalize(module);

        TEST_ASSERT_EQUALS(1, module.getKernels().size());
        auto kernel = module.getKernels()[0];
        auto& cfg = kernel->getCFG();
        auto numNodes = cfg.getNodes().size();

        auto tree = DominatorTree::createDominatorTree(cfg);
        TEST_ASSERT(!!tree);
        TEST_ASSERT_EQUALS(numNodes, tree->getNodes().size());

        // // the start of the control flow dominates all nodes (except itself)
        auto& start = tree->assertNode(&cfg.getStartOfControlFlow());
        TEST_ASSERT_EQUALS(0, start.getDominators().size());
        TEST_ASSERT_EQUALS(numNodes - 1, start.getDominatedNodes().size());
        // the end of the control flow dominates no nodes
        auto& end = tree->assertNode(&cfg.getEndOfControlFlow());
        TEST_ASSERT(end.getDominators().size() > 4);
        TEST_ASSERT_EQUALS(0, end.getDominatedNodes().size());

        for(auto& node : tree->getNodes())
        {
            if(&node.second != &start)
            {
                TEST_ASSERT(start.dominates(node.second));
                TEST_ASSERT(!start.isDominatedBy(node.second));
            }
        }

        // Run the optimization steps and do some more checks
        optimize(module);

        numNodes = cfg.getNodes().size();
        tree = DominatorTree::createDominatorTree(cfg);
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
        it.emplace(new LoadImmediate(out, Literal(-12345)));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::SET, FlagStatus::CLEAR, FlagStatus::CLEAR}), elem);
        }

        // test with differing constant vector
        it.reset(new LoadImmediate(out, 0x32100123, intermediate::LoadType::PER_ELEMENT_SIGNED));
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
        it.emplace(new MoveOperation(NOP_REGISTER, booleanValue, COND_ALWAYS, SetFlag::SET_FLAGS));
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

        it.emplace(new Operation(OP_OR, NOP_REGISTER, booleanA, booleanB));
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

        it.emplace(new Operation(OP_AND, NOP_REGISTER, booleanA, booleanB));
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
        it.emplace(
            new Operation(OP_OR, NOP_REGISTER, booleanValue, ELEMENT_NUMBER_REGISTER, COND_ALWAYS, SetFlag::SET_FLAGS));
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

        it.emplace(new Operation(OP_MAX, NOP_REGISTER, input, 13_val));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // can never be negative or zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.reset(new Operation(OP_MIN, NOP_REGISTER, input, Value(Literal(-13), TYPE_INT16)));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // is always negative and never zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::SET, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.emplace(new Operation(OP_FMAX, NOP_REGISTER, input, Value(Literal(-13.4f), TYPE_FLOAT)));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // may be negative and zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::UNDEFINED, FlagStatus::UNDEFINED, FlagStatus::UNDEFINED, FlagStatus::CLEAR}),
                elem);
        }

        it.emplace(new Operation(OP_FMAXABS, NOP_REGISTER, input, Value(Literal(-13.4f), TYPE_FLOAT)));
        flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it, true);
        TEST_ASSERT(!!flags);
        for(auto elem : *flags)
        {
            // can never be negative or zero
            TEST_ASSERT_EQUALS(
                (ElementFlags{FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::UNDEFINED, FlagStatus::CLEAR}), elem);
        }

        it.emplace(new Operation(OP_FMINABS, NOP_REGISTER, input, FLOAT_ZERO));
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
        it.emplace(new MoveOperation(NOP_REGISTER, input));
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
        it.emplace(new MoveOperation(NOP_REGISTER, booleanValue, COND_ALWAYS, SetFlag::SET_FLAGS));
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
        it.emplace(new MoveOperation(NOP_REGISTER, input));
        auto flags = StaticFlagsAnalysis::analyzeStaticFlags(it.get(), it);
        TEST_ASSERT(!flags);
    }
}
