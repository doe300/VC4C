/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestInternalLibs.h"
#include "Graph.h"

using intGraph = vc4c::Graph<int, vc4c::Node<int, int>>;

void TestInternalLibs::testGraph() {
  auto graph = intGraph();
  graph.getOrCreateNode(1).addNeighbor(&graph.getOrCreateNode(1), 0);
  graph.getOrCreateNode(1).addNeighbor(&graph.getOrCreateNode(2), 1);
  graph.getOrCreateNode(2).addNeighbor(&graph.getOrCreateNode(1), 2);
  graph.getOrCreateNode(3).addNeighbor(&graph.getOrCreateNode(1), 3);
  graph.getOrCreateNode(4).addNeighbor(&graph.getOrCreateNode(1), 4);
  graph.getOrCreateNode(4).addNeighbor(&graph.getOrCreateNode(2), 4);

  graph.erase(1);

  TEST_ASSERT_EQUALS(3, graph.size());
  TEST_ASSERT_EQUALS(1, graph.at(4).getNeighbors().size());
  TEST_ASSERT_EQUALS(0, graph.at(3).getNeighbors().size());
  TEST_ASSERT_EQUALS(0, graph.at(2).getNeighbors().size());
  TEST_ASSERT_EQUALS(1, graph.at(2).getPointee().size());

  graph.at(4).erase(&graph.at(2));
  TEST_ASSERT_EQUALS(0, graph.at(4).getNeighbors().size());

  graph.getOrCreateNode(4).addNeighbor(&graph.getOrCreateNode(2), 5);
  TEST_ASSERT_EQUALS(1, graph.at(4).getNeighbors().size());
  TEST_ASSERT_EQUALS(2, graph.at(4).getNeighbors().find(&graph.at(2))->first->key);

  graph.getOrCreateNode(4).addNeighbor(&graph.getOrCreateNode(2), 6);
  TEST_ASSERT_EQUALS(2, graph.at(4).getNeighbors().size());
}

TestInternalLibs::TestInternalLibs() {
  TEST_ADD(TestInternalLibs::testGraph);
}

TestInternalLibs::~TestInternalLibs()
{

}
