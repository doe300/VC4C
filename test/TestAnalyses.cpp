/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestAnalyses.h"

#include "analysis/ControlFlowGraph.h"

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
    TEST_ADD(TestAnalyses::testDataDependency);
    TEST_ADD(TestAnalyses::testDependency);
    TEST_ADD(TestAnalyses::testInterference);
    TEST_ADD(TestAnalyses::testLifetime);
    TEST_ADD(TestAnalyses::testLiveness);
    TEST_ADD(TestAnalyses::testRegister);
    TEST_ADD(TestAnalyses::testValueRange);
}

// TODO how to test these?? How to get access to code/analyses

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
        cfg.updateBackEdges();
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
        cfg.updateBackEdges();
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
        cfg.updateBackEdges();
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
        cfg.updateBackEdges();
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
        // 5 implicit edges (into first loop, out of if-block, out of second loop, 2 * for work-group loop)
        TEST_ASSERT_EQUALS(5u, implicitEdges.size());
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

        auto inclusionTree = createLoopInclusingTree(loops);
        TEST_ASSERT_EQUALS(2u, inclusionTree->getNodes().size());
        TEST_ASSERT_EQUALS(
            0u, inclusionTree->assertNode(&(firstLoopIsOuter ? loops.front() : loops.back())).getLongestPathToRoot());
        TEST_ASSERT_EQUALS(
            1u, inclusionTree->assertNode(&(firstLoopIsOuter ? loops.back() : loops.front())).getLongestPathToRoot());
        TEST_ASSERT_EQUALS(inclusionTree->assertNode(&loops.front()).findRoot(12),
            inclusionTree->assertNode(&loops.back()).findRoot(12));

        // Run the optimization steps and do some more checks
        optimize(module);

        loops = kernel->getCFG().findLoops(true, false);
        // 5 loops (2 for-loops, 3 for the work-group loop)
        TEST_ASSERT_EQUALS(5, loops.size());
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
        for(const auto& loop : loops)
        {
            TEST_ASSERT(!!loop.findPredecessor());
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
        // since we sorted the loops in order of their inclusion, the longest path is their index
        auto& base = inclusionTree->assertNode(&loops.front());
        for(unsigned i = 0; i < loops.size(); ++i)
        {
            auto& node = inclusionTree->assertNode(&loops[i]);
            TEST_ASSERT_EQUALS(i, node.getLongestPathToRoot());
            TEST_ASSERT_EQUALS(&base, node.findRoot(12));
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
        for(const auto& loop : loops)
        {
            TEST_ASSERT(!!loop.findPredecessor());
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
void TestAnalyses::testDataDependency() {}
void TestAnalyses::testDependency() {}
void TestAnalyses::testInterference() {}
void TestAnalyses::testLifetime() {}
void TestAnalyses::testLiveness() {}
void TestAnalyses::testRegister() {}
void TestAnalyses::testValueRange() {}