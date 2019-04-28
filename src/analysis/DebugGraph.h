/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef DEBUG_GRAPH_H
#define DEBUG_GRAPH_H

#include "../Graph.h"

#include <fstream>
#include <functional>

#ifdef DEBUG_MODE
// LCOV_EXCL_START

namespace vc4c
{
    void printEdge(std::ofstream& file, uintptr_t id1, uintptr_t id2, bool weakEdge, Direction direction,
        const std::string& edgeLabel);
    void printNode(std::ofstream& file, uintptr_t ID, const std::string& name);

    namespace detail
    {
        /*
         * This construct is required to not throw assertions for directed/not directed edges.
         *
         * This would be fixed by C++17 if constexpr
         */
        template <typename NodeType>
        using ForAllEdgesFunc = std::function<bool(const NodeType& neighbor, const typename NodeType::EdgeType& edge)>;

        template <typename NodeType, Directionality Direction>
        struct ForAllEdgesFuncWrapper
        {
            void operator()(const NodeType& node, ForAllEdgesFunc<NodeType> func) const;
        };

        template <typename NodeType>
        struct ForAllEdgesFuncWrapper<NodeType, Directionality::DIRECTED>
        {
            void operator()(const NodeType& node, ForAllEdgesFunc<NodeType> func) const
            {
                node.forAllOutgoingEdges(func);
            }
        };

        template <typename NodeType>
        struct ForAllEdgesFuncWrapper<NodeType, Directionality::BIDIRECTIONAL>
        {
            void operator()(const NodeType& node, ForAllEdgesFunc<NodeType> func) const
            {
                node.forAllOutgoingEdges(func);
            }
        };

        template <typename NodeType>
        struct ForAllEdgesFuncWrapper<NodeType, Directionality::UNDIRECTED>
        {
            void operator()(const NodeType& node, ForAllEdgesFunc<NodeType> func) const
            {
                node.forAllEdges(func);
            }
        };
    } /* namespace detail */

    /*
     * Generates a Graphviz (http://graphviz.org/) graph out of the colored graph/basic block graph
     *
     * Generate SVG with: "sfdp -Tsvg <input>.dot -o <output>.svg"
     */
    template <typename Key, typename Relation, Directionality Direction>
    class DebugGraph
    {
    public:
        template <typename U>
        using NameFunc = std::function<std::string(const U&)>;

        using DefaultNodeType = Node<Key, Relation, Direction>;

        explicit DebugGraph(const std::string& fileName, std::size_t numNodes) : file(fileName)
        {
            processedNodes.reserve(numNodes);
            // strict: at most one edge can connect two nodes, multiple same connections are merged (including their
            // attributes)  graph: undirected graph, digraph: directed graph
            if(Direction != Directionality::UNDIRECTED)
                file << "strict digraph {" << std::endl;
            else
                file << "strict graph {" << std::endl;
            file << "concentrate=true" << std::endl;
            // global graph settings: draw edges as splines and remove overlap between edges and nodes, draw nodes over
            // edges
            file << "graph [splines=true, overlap=\"prism\", outputorder=\"edgesfirst\"];" << std::endl;
            // fill background of nodes white
            file << "node [style=\"filled\", fillcolor=\"white\"];" << std::endl;
        }

        ~DebugGraph()
        {
            file << "}" << std::endl;
            file.flush();
            file.close();
        }

        template <typename NodeType = DefaultNodeType>
        void addNodeWithNeighbors(const NodeType& node, const NameFunc<Key>& nameFunc = NodeType::to_string,
            const std::function<bool(const Relation&)>& weakEdgeFunc = [](const Relation& r) -> bool { return false; },
            const NameFunc<Relation>& edgeLabelFunc = [](const Relation& r) -> std::string { return ""; })
        {
            printNode(file, reinterpret_cast<uintptr_t>(&node), nameFunc(node.key));
            if(Direction != Directionality::UNDIRECTED)
            {
                detail::ForAllEdgesFuncWrapper<NodeType, Direction>{}(
                    node, [&, this](const NodeType& neighbor, const typename NodeType::EdgeType& edge) -> bool {
                        // checking for duplicate edges is not required on directed graph, since every two nodes are
                        // only connected one edge, which is only output to one of the nodes
                        printEdge(file, reinterpret_cast<uintptr_t>(&node), reinterpret_cast<uintptr_t>(&neighbor),
                            weakEdgeFunc(edge.data), edge.getDirection(), edgeLabelFunc(edge.data));
                        return true;
                    });
            }
            else
            {
                processedNodes.emplace(reinterpret_cast<const DefaultNodeType*>(&node));
                detail::ForAllEdgesFuncWrapper<NodeType, Direction>{}(
                    node, [&, this](const NodeType& neighbor, const typename NodeType::EdgeType& edge) -> bool {
                        // making sure every edge is printed just once is not necessary because of the "strict"
                        // keyword, but it saves a lot of processing time in Graphviz
                        if(processedNodes.find(reinterpret_cast<const DefaultNodeType*>(&neighbor)) !=
                            processedNodes.end())
                            return true;

                        printEdge(file, reinterpret_cast<uintptr_t>(&node), reinterpret_cast<uintptr_t>(&neighbor),
                            weakEdgeFunc(edge.data), Direction::NONE, edgeLabelFunc(edge.data));
                        return true;
                    });
            }
        }

        template <typename G = Graph<Key, DefaultNodeType>>
        static void dumpGraph(const G& graph, const std::string& fileName,
            const NameFunc<Key>& nameFunc = G::NodeType::to_string,
            const std::function<bool(const Relation&)>& weakEdgeFunc = [](const Relation& r) -> bool { return false; },
            const NameFunc<Relation>& edgeLabelFunc = [](const Relation& r) -> std::string { return ""; })
        {
            DebugGraph<Key, Relation, Direction> debugGraph(fileName, graph.getNodes().size());
            for(const auto& node : graph.getNodes())
            {
                debugGraph.addNodeWithNeighbors(node.second, nameFunc, weakEdgeFunc, edgeLabelFunc);
            }
        }

    private:
        std::ofstream file;
        FastSet<const DefaultNodeType*> processedNodes;
    };

} /* namespace vc4c */
// LCOV_EXCL_STOP
#endif /* DEBUG_MODE */
#endif /* DEBUG_GRAPH_H */
