/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestGraph.h"

#include "Graph.h"

using namespace vc4c;

using UndirectedNode = vc4c::Node<int, int, Directionality::UNDIRECTED>;
using UndirectedEdge = UndirectedNode::EdgeType;
using UndirectedGraph = vc4c::Graph<int, UndirectedNode>;

using DirectedNode = vc4c::Node<int, int, Directionality::DIRECTED>;
using DirectedEdge = DirectedNode::EdgeType;
using DirectedGraph = vc4c::Graph<int, DirectedNode>;

using BidirectionalNode = vc4c::Node<int, int, Directionality::BIDIRECTIONAL>;
using BidirectionalEdge = BidirectionalNode::EdgeType;
using BidirectionalGraph = vc4c::Graph<int, BidirectionalNode>;

TestGraph::TestGraph()
{
    TEST_ADD(TestGraph::testAssertNode);
    TEST_ADD(TestGraph::testFindNode);
    TEST_ADD(TestGraph::testEraseNode);
    TEST_ADD(TestGraph::testFindSink);
    TEST_ADD(TestGraph::testFindSource);

    TEST_ADD(TestGraph::testAddEdge);
    TEST_ADD(TestGraph::testFindEdge);
    TEST_ADD(TestGraph::testRemoveNeighbor);
    TEST_ADD(TestGraph::testGetSingleNeighbor);
    TEST_ADD(TestGraph::testGetDirectedNeighbors);
    TEST_ADD(TestGraph::testGetBidirectionalNeighbors);
    TEST_ADD(TestGraph::testLoop);
    TEST_ADD(TestGraph::testAdjacent);
    TEST_ADD(TestGraph::testForEdges);

    TEST_ADD(TestGraph::testEdgeNodes);
    TEST_ADD(TestGraph::testDirection);
}

void TestGraph::testAssertNode()
{
    UndirectedGraph graph;
    TEST_THROWS(graph.assertNode(7), CompilationError);

    graph.getOrCreateNode(7);
    TEST_ASSERT_EQUALS(7, graph.assertNode(7).key);
}

void TestGraph::testFindNode()
{
    UndirectedGraph graph;
    TEST_ASSERT_EQUALS(nullptr, graph.findNode(42));

    graph.getOrCreateNode(42);
    TEST_ASSERT(graph.findNode(42) != nullptr);
    TEST_ASSERT_EQUALS(42, graph.findNode(42)->key);
}

void TestGraph::testEraseNode()
{
    UndirectedGraph graph;
    graph.getOrCreateNode(42);
    TEST_ASSERT(graph.findNode(42) != nullptr);

    graph.eraseNode(42);
    TEST_ASSERT_EQUALS(nullptr, graph.findNode(42));
}

void TestGraph::testFindSink()
{
    DirectedGraph graph;
    TEST_ASSERT_EQUALS(nullptr, graph.findSink());

    auto& n = graph.getOrCreateNode(42);
    TEST_ASSERT(graph.findSink() != nullptr);

    auto& m = graph.getOrCreateNode(43);
    n.getOrCreateEdge(&m);
    TEST_ASSERT_EQUALS(&m, graph.findSink());

    graph.eraseNode(m.key);
    TEST_ASSERT_EQUALS(&n, graph.findSink());

    graph.getOrCreateNode(11).addEdge(&graph.getOrCreateNode(12), 9);

    int counter = 0;
    graph.forAllSinks([&](const DirectedNode&) -> bool {
        ++counter;
        return true;
    });
    TEST_ASSERT_EQUALS(2, counter);
}

void TestGraph::testFindSource()
{
    DirectedGraph graph;
    TEST_ASSERT_EQUALS(nullptr, graph.findSource());

    auto& n = graph.getOrCreateNode(42);
    TEST_ASSERT(graph.findSource() != nullptr);
    TEST_ASSERT_EQUALS(&n, graph.findSource());

    graph.getOrCreateNode(11).addEdge(&graph.getOrCreateNode(12), 9);

    int counter = 0;
    graph.forAllSources([&](const DirectedNode&) -> bool {
        ++counter;
        return true;
    });
    TEST_ASSERT_EQUALS(2, counter);
}

void TestGraph::testAddEdge()
{
    UndirectedGraph graph;
    auto& n = graph.getOrCreateNode(42);
    TEST_ASSERT_EQUALS(0, n.getEdgesSize());

    auto& m = graph.getOrCreateNode(12);
    n.addEdge(&m, 11);
    TEST_THROWS(n.addEdge(&m, 111), CompilationError)
}

void TestGraph::testFindEdge()
{
    UndirectedGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    TEST_ASSERT_EQUALS(nullptr, n.findEdge(7));
    auto e = n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(e, n.findEdge(7));
    TEST_ASSERT_EQUALS(e, m.findEdge(7));
}

void TestGraph::testRemoveNeighbor()
{
    UndirectedGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    auto e = n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(e, n.findEdge(7));
    TEST_ASSERT_EQUALS(e, m.findEdge(7));

    n.removeAsNeighbor(&m);
    TEST_ASSERT_EQUALS(nullptr, n.findEdge(7));
    TEST_ASSERT_EQUALS(nullptr, m.findEdge(7));

    e = n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(e, n.findEdge(7));
    TEST_ASSERT_EQUALS(e, m.findEdge(7));

    n.removeEdge(*e);
    TEST_ASSERT_EQUALS(nullptr, n.findEdge(7));
    TEST_ASSERT_EQUALS(nullptr, m.findEdge(7));
}

void TestGraph::testGetSingleNeighbor()
{
    UndirectedGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(&m, n.getSingleNeighbor(7));
    TEST_ASSERT_EQUALS(&n, m.getSingleNeighbor(7));
}

void TestGraph::testGetDirectedNeighbors()
{
    DirectedGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(&m, n.getSingleSuccessor());
    TEST_ASSERT_EQUALS(&n, m.getSinglePredecessor());
}

void TestGraph::testGetBidirectionalNeighbors()
{
    BidirectionalGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    n.addEdge(&m, 7)->addInput(m);
    TEST_ASSERT_EQUALS(&m, n.getSingleSuccessor());
    TEST_ASSERT_EQUALS(&n, m.getSinglePredecessor());
}

void TestGraph::testLoop()
{
    DirectedGraph graph;
    auto& n = graph.getOrCreateNode(1);
    n.addEdge(&n, 7);
    TEST_ASSERT_EQUALS(&n, n.getSingleSuccessor());
    TEST_ASSERT_EQUALS(&n, n.getSinglePredecessor());
}

void TestGraph::testAdjacent()
{
    UndirectedGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    TEST_ASSERT_EQUALS(false, n.isAdjacent(&m));
    TEST_ASSERT_EQUALS(false, m.isAdjacent(&n));

    n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(true, n.isAdjacent(&m));
    TEST_ASSERT_EQUALS(true, m.isAdjacent(&n));
}

void TestGraph::testForEdges()
{
    BidirectionalGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    n.addEdge(&m, 7);
    n.addEdge(&n, 1);
    int count = 0;
    n.forAllOutgoingEdges([&](const BidirectionalNode&, const BidirectionalEdge&) -> bool {
        ++count;
        return true;
    });
    TEST_ASSERT_EQUALS(2, count);
}

void TestGraph::testEdgeNodes()
{
    BidirectionalGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    auto e = n.addEdge(&m, 7);
    e->addInput(m);
    TEST_ASSERT(e->isInput(n));
    TEST_ASSERT(e->isInput(m));

    TEST_ASSERT_EQUALS(&m, &e->getOtherNode(n));
    TEST_ASSERT_EQUALS(&n, &e->getOtherNode(m));
}

void TestGraph::testDirection()
{
    BidirectionalGraph graph;
    auto& n = graph.getOrCreateNode(1);
    auto& m = graph.getOrCreateNode(2);
    auto e = n.addEdge(&m, 7);
    TEST_ASSERT_EQUALS(Direction::FIRST_TO_SECOND, e->getDirection());

    e->addInput(m);
    TEST_ASSERT_EQUALS(Direction::BOTH, e->getDirection());
}