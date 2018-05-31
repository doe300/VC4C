/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_GRAPH
#define VC4C_TEST_GRAPH

#include "cpptest.h"

class TestGraph : public Test::Suite
{
public:
    TestGraph();

    void testAssertNode();
    void testFindNode();
    void testEraseNode();
    void testFindSink();
    void testFindSource();

    void testAddEdge();
    void testFindEdge();
    void testRemoveNeighbor();
    void testGetSingleNeighbor();
    void testGetDirectedNeighbors();
    void testGetBidirectionalNeighbors();
    void testLoop();
    void testAdjacent();
    void testForEdges();

    void testEdgeNodes();
    void testDirection();
};

#endif /* VC4C_TEST_GRAPH */
