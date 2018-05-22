/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_GRAPH_H
#define VC4C_GRAPH_H

#include "CompilationError.h"
#include "performance.h"

#include <functional>
#include <type_traits>

namespace vc4c
{
    struct empty_base
    {
    };

    /*
     * The possible directionality of the graph, whether edges can be directed or not
     */
    enum class Directionality
    {
        UNDIRECTED,
        DIRECTED,
        BIDIRECTIONAL
    };

    template <typename Key, typename Relation, Directionality Direction, typename Base = empty_base>
    class Node;

    template <typename NodeType, typename Relation, Directionality Direction>
    class Edge;

    template <typename Key, typename NodeType>
    class Graph;

    /*
     * A node in a graph, general base-class maintaining the list of edges to neighboring nodes
     */
    template <typename Key, typename Relation, Directionality Direction, typename Base>
    class Node : public Base
    {
    public:
        using RelationType = Relation;
        using NodeType = Node<Key, Relation, Direction, Base>;
        using EdgeType = Edge<NodeType, Relation, Direction>;
        using GraphType = Graph<Key, NodeType>;

        template <typename... Args>
        explicit Node(GraphType& graph, const Key& key, Args&&... args) :
            Base(std::forward<Args&&>(args)...), key(key), graph(graph)
        {
        }
        template <typename... Args>
        explicit Node(GraphType& graph, Key&& key, Args&&... args) :
            Base(std::forward<Args&&>(args)...), key(std::move(key)), graph(graph)
        {
        }

        Node(const Node&) = delete;
        Node(Node&&) noexcept = delete;

        Node& operator=(const Node&) = delete;
        Node& operator=(Node&&) noexcept = delete;

        void erase()
        {
            graph.eraseNode(key);
        }

        /*!
         * Adds the given neighbor with the given relation.
         * Multiple calls to this method do not override the previous association.
         */
        EdgeType* addEdge(Node* neighbor, Relation&& relation)
        {
            if(isAdjacent(neighbor))
                throw CompilationError(CompilationStep::GENERAL, "Nodes are already adjacent!");
            return graph.createEdge(this, neighbor, std::forward<Relation&&>(relation));
        }

        EdgeType& getOrCreateEdge(Node* neighbor, Relation&& defaultRelation = {})
        {
            auto it = edges.find(neighbor);
            if(it != edges.end())
                return *it->second;
            return *addEdge(neighbor, std::forward<Relation&&>(defaultRelation));
        }

        EdgeType* findEdge(const Relation& relation)
        {
            for(auto& pair : edges)
            {
                if(pair.second->data == relation)
                    return pair.second;
            }
            return nullptr;
        }

        const EdgeType* findEdge(const Relation& relation) const
        {
            for(const auto& pair : edges)
            {
                if(pair.second->data == relation)
                    return pair.second;
            }
            return nullptr;
        }

        EdgeType* findEdge(const std::function<bool(const Relation&)>& predicate)
        {
            for(auto& pair : edges)
            {
                if(predicate(pair.second->data))
                    return pair.second;
            }
            return nullptr;
        }

        const EdgeType* findEdge(const std::function<bool(const Relation&)>& predicate) const
        {
            for(const auto& pair : edges)
            {
                if(predicate(pair.second->data))
                    return pair.second;
            }
            return nullptr;
        }

        void removeEdge(EdgeType& edge)
        {
            graph.eraseEdge(edge);
        }

        void removeAsNeighbor(Node* neighbor)
        {
            auto it = edges.find(neighbor);
            if(it == edges.end())
                throw CompilationError(CompilationStep::GENERAL, "Node was not neighbor of this node!");
            removeEdge(*it->second);
        }

        /*
         * Returns the single neighbor with the given relation.
         * Returns nullptr otherwise, if there is no or more than one neighbor with this relation.
         */
        inline const Node* getSingleNeighbor(const Relation relation) const
        {
            return getSingleNeighbor([&relation](const Relation& rel) -> bool { return rel == relation; });
        }

        inline Node* getSingleNeighbor(const Relation relation)
        {
            return getSingleNeighbor([&relation](const Relation& rel) -> bool { return rel == relation; });
        }

        /*
         * Returns the single neighbor where the relation matches the given predicate.
         * Returns nullptr otherwise, if there is no or more than one neighbor with this relation.
         */
        const Node* getSingleNeighbor(const std::function<bool(const Relation&)>& relation) const
        {
            static_assert(Direction == Directionality::UNDIRECTED,
                "For directed graphs, incoming and outgoing edges need to be handled differently!");
            Node* singleNeighbor = nullptr;
            for(const auto& pair : edges)
            {
                if(relation(pair.second->data))
                {
                    if(singleNeighbor != nullptr)
                        // multiple neighbors
                        return nullptr;
                    singleNeighbor = pair.first;
                }
            }
            return singleNeighbor;
        }

        Node* getSingleNeighbor(const std::function<bool(const Relation&)>& relation)
        {
            static_assert(Direction == Directionality::UNDIRECTED,
                "For directed graphs, incoming and outgoing edges need to be handled differently!");
            Node* singleNeighbor = nullptr;
            for(auto& pair : edges)
            {
                if(relation(pair.second->data))
                {
                    if(singleNeighbor != nullptr)
                        // multiple neighbors
                        return nullptr;
                    singleNeighbor = pair.first;
                }
            }
            return singleNeighbor;
        }

        Node* getSinglePredecessor()
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have predecessors!");
            Node* singlePredecessor = nullptr;
            for(auto& edge : edges)
            {
                if(edge.second->isOutput(*this))
                {
                    if(singlePredecessor != nullptr)
                        // multiple predecessors
                        return nullptr;
                    singlePredecessor = edge.first;
                }
            }
            return singlePredecessor;
        }

        const Node* getSinglePredecessor() const
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have predecessors!");
            const Node* singlePredecessor = nullptr;
            for(auto& edge : edges)
            {
                if(edge.second->isOutput(*this))
                {
                    if(singlePredecessor != nullptr)
                        // multiple predecessors
                        return nullptr;
                    singlePredecessor = edge.first;
                }
            }
            return singlePredecessor;
        }

        Node* getSingleSuccessor()
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have successors!");
            Node* singleSuccessor = nullptr;
            for(auto& edge : edges)
            {
                if(edge.second->isInput(*this))
                {
                    if(singleSuccessor != nullptr)
                        // multiple successors
                        return nullptr;
                    singleSuccessor = edge.first;
                }
            }
            return singleSuccessor;
        }

        const Node* getSingleSuccessor() const
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have successors!");
            const Node* singleSuccessor = nullptr;
            for(auto& edge : edges)
            {
                if(edge.second->isInput(*this))
                {
                    if(singleSuccessor != nullptr)
                        // multiple successors
                        return nullptr;
                    singleSuccessor = edge.first;
                }
            }
            return singleSuccessor;
        }

        bool isAdjacent(NodeType* node) const
        {
            return edges.find(node) != edges.end();
        }

        inline bool isAdjacent(const NodeType* node) const
        {
            return isAdjacent(const_cast<NodeType*>(node));
        }

        /*
         * Executes the given predicate for all neighbors until it becomes false
         */
        void forAllEdges(const std::function<bool(NodeType&, EdgeType&)>& predicate)
        {
            static_assert(Direction == Directionality::UNDIRECTED,
                "For directed graphs, incoming and outgoing edges need to be handled differently!");
            for(auto& pair : edges)
            {
                if(!predicate(*pair.first, *pair.second))
                    return;
            }
        }

        void forAllEdges(const std::function<bool(const NodeType&, const EdgeType&)>& predicate) const
        {
            static_assert(Direction == Directionality::UNDIRECTED,
                "For directed graphs, incoming and outgoing edges need to be handled differently!");
            for(const auto& pair : edges)
            {
                if(!predicate(*pair.first, *pair.second))
                    return;
            }
        }

        /*
         * Executes the predicate for all incoming edges, until it becomes false
         */
        void forAllIncomingEdges(const std::function<bool(NodeType&, EdgeType&)>& predicate)
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have incoming edges!");
            for(auto& edge : edges)
            {
                if(edge.second->isOutput(*this))
                {
                    if(!predicate(*edge.first, *edge.second))
                        return;
                }
            }
        }

        void forAllIncomingEdges(const std::function<bool(const NodeType&, const EdgeType&)>& predicate) const
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have incoming edges!");
            for(const auto& edge : edges)
            {
                if(edge.second->isOutput(*this))
                {
                    if(!predicate(*edge.first, *edge.second))
                        return;
                }
            }
        }

        /*
         * Executes the predicate for all outgoing edges, until it becomes false
         */
        void forAllOutgoingEdges(const std::function<bool(NodeType&, EdgeType&)>& predicate)
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have outgoing edges!");
            for(auto& edge : edges)
            {
                if(edge.second->isInput(*this))
                {
                    if(!predicate(*edge.first, *edge.second))
                        return;
                }
            }
        }

        void forAllOutgoingEdges(const std::function<bool(const NodeType&, const EdgeType&)>& predicate) const
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Only directed graphs have outgoing edges!");
            for(const auto& edge : edges)
            {
                if(edge.second->isInput(*this))
                {
                    if(!predicate(*edge.first, *edge.second))
                        return;
                }
            }
        }

        std::size_t getEdgesSize() const
        {
            return edges.size();
        }

        Key key;

    protected:
        GraphType& graph;
        FastMap<NodeType*, EdgeType*> edges;

        friend GraphType;
    };

    /*
     * The actual direction an edge is pointing.
     */
    enum class Direction
    {
        NONE = 0,
        FIRST_TO_SECOND = 1,
        SECOND_TO_FIRST = 2,
        BOTH = 3
    };

    /*
     * An edge represents the connection between two nodes.
     *
     * Edges store additional content specifying the type of relation/connection between the nodes connected by the edge
     *
     * If the edge is directional, then the edge points from the first node to the second node
     *
     */
    template <typename Node, typename Relation, Directionality Direction>
    class Edge
    {
    public:
        using NodeType = Node;

        Edge(NodeType& first, NodeType& second, Relation&& data) :
            data(std::move(data)), first(first), second(second), firstInput(true), secondInput(false)
        {
        }

        Edge(const Edge&) = delete;
        Edge(Edge&&) noexcept = delete;

        Edge& operator=(const Edge&) = delete;
        Edge& operator=(Edge&&) noexcept = delete;

        bool operator==(const Edge& other) const
        {
            return &first == &other.first && &second == &other.second;
        }

        Node& getInput()
        {
            static_assert(Direction == Directionality::DIRECTED, "Input is only available for directed edges!");
            return first;
        }

        const Node& getInput() const
        {
            static_assert(Direction == Directionality::DIRECTED, "Input is only available for directed edges!");
            return first;
        }

        NodeType& getOutput()
        {
            static_assert(Direction == Directionality::DIRECTED, "Output is only available for directed edges!");
            return second;
        }

        const NodeType& getOutput() const
        {
            static_assert(Direction == Directionality::DIRECTED, "Output is only available for directed edges!");
            return second;
        }

        bool isInput(const Node& node) const
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Input is only available for directed edges!");
            return (&node == &first && firstInput) || (&node == &second && secondInput);
        }

        bool isOutput(const Node& node) const
        {
            static_assert(Direction != Directionality::UNDIRECTED, "Output is only available for directed edges!");
            return (&node == &second && firstInput) || (&node == &first && secondInput);
        }

        FastSet<NodeType*> getNodes()
        {
            static_assert(Direction == Directionality::UNDIRECTED,
                "For directed edges, the input and output have different meaning!");
            return FastSet<NodeType*>{&first, &second};
        }

        NodeType& getOtherNode(const NodeType& oneNode)
        {
            if(&first == &oneNode)
                return second;
            return first;
        }

        Edge& addInput(const NodeType& node)
        {
            static_assert(Direction == Directionality::BIDIRECTIONAL, "Can only add input for bidirectional graphs!");
            if(&node == &first)
                firstInput = true;
            else if(&node == &second)
                secondInput = true;
            else
                throw CompilationError(CompilationStep::GENERAL, "Node is not a part of this edge!");
            return *this;
        }

        enum Direction getDirection() const
        {
            if(Direction == Directionality::UNDIRECTED)
                return Direction::NONE;
            if(Direction == Directionality::DIRECTED)
                return Direction::FIRST_TO_SECOND;
            if(firstInput && secondInput)
                return Direction::BOTH;
            if(firstInput)
                return Direction::FIRST_TO_SECOND;
            return Direction::SECOND_TO_FIRST;
        }

        static constexpr Directionality Directed = Direction;

        Relation data;

    protected:
        NodeType& first;
        NodeType& second;
        bool firstInput;
        bool secondInput;

        friend NodeType;
        friend typename NodeType::GraphType;
        friend struct hash<Edge<Node, Relation, Directed>>;
    };

    template <typename Node, typename Relation, Directionality Direction>
    struct hash<Edge<Node, Relation, Direction>>
    {
        using EdgeType = Edge<Node, Relation, Direction>;
        inline std::size_t operator()(const EdgeType& edge) const
        {
            std::hash<Node*> h;
            return h(&edge.first) ^ h(&edge.second);
        }
    };

    /*
     * General base type for graphs of any kind.
     *
     * A graph contains nodes containing the object being represented as well as some arbitrary additional information.
     * Additionally, the object-type of the relations between the nodes can be specified allowing for extra data being
     * stored in them.
     *
     * NOTE: The fact whether the graph is directed or not must be managed by the user.
     * E.g. for an undirected graph, a relationship must be added to both nodes taking place in it.
     */
    template <typename Key, typename NodeType>
    class Graph
    {
    public:
        using RelationType = typename NodeType::RelationType;
        using EdgeType = typename NodeType::EdgeType;

        explicit Graph() = default;
        Graph(const Graph&) = delete;
        Graph(Graph&&) noexcept = delete;

        Graph& operator=(const Graph&) = delete;
        Graph& operator=(Graph&&) noexcept = delete;

        /*
         * Returns the node for the given key
         *
         * If such a node does not exist yet, a new node is created with the given additional initial payload
         */
        template <typename... Args>
        NodeType& getOrCreateNode(Key key, Args&&... initialPayload)
        {
            auto it = nodes.find(key);
            if(it == nodes.end())
            {
                return nodes
                    .emplace(std::piecewise_construct_t{}, std::forward_as_tuple(key),
                        std::forward_as_tuple(*this, key, std::forward<Args&&>(initialPayload)...))
                    .first->second;
            }
            return it->second;
        }

        /*
         * Guarantees a node for the given key to exist within the graph and returns it.
         * Throws a compilation-error otherwise
         */
        NodeType& assertNode(const Key& key)
        {
            if(nodes.find(key) == nodes.end())
            {
                throw CompilationError(CompilationStep::GENERAL, "Failed to find graph-node for key");
            }
            return nodes.at(key);
        }

        const NodeType& assertNode(const Key& key) const
        {
            if(nodes.find(key) == nodes.end())
            {
                throw CompilationError(CompilationStep::GENERAL, "Failed to find graph-node for key");
            }
            return nodes.at(key);
        }

        NodeType* findNode(const Key& key)
        {
            auto it = nodes.find(key);
            if(it == nodes.end())
                return nullptr;
            return &it->second;
        }

        const NodeType* findNode(const Key& key) const
        {
            auto it = nodes.find(key);
            if(it == nodes.end())
                return nullptr;
            return &it->second;
        }

        void eraseNode(const Key& key)
        {
            auto it = nodes.find(key);
            if(it == nodes.end())
                throw CompilationError(CompilationStep::GENERAL, "Failed to find graph-node for key");
            for(auto& edge : it->second.edges)
            {
                edge.second->getOtherNode(it->second).edges.erase(&it->second);
                edges.erase(*edge.second);
            }
            nodes.erase(it);
        }

        /*
         * Finds a sink in this graph (a node without outgoing edges)
         */
        NodeType* findSink()
        {
            static_assert(EdgeType::Directed != Directionality::UNDIRECTED, "Can only find sinks in directed graphs!");
            for(auto& pair : nodes)
            {
                bool hasOutgoingEdges = false;
                pair.second.forAllOutgoingEdges([&](const NodeType&, const EdgeType&) -> bool {
                    hasOutgoingEdges = true;
                    return false;
                });
                if(!hasOutgoingEdges)
                    return &pair.second;
            }
            return nullptr;
        }

        const FastMap<Key, NodeType>& getNodes() const
        {
            return nodes;
        }

        void clear()
        {
            nodes.clear();
            edges.clear();
        }

    protected:
        FastMap<Key, NodeType> nodes;
        FastSet<EdgeType> edges;

        EdgeType* createEdge(NodeType* first, NodeType* second, RelationType&& relation)
        {
            EdgeType& edge =
                const_cast<EdgeType&>(*(edges.emplace(*first, *second, std::forward<RelationType&&>(relation)).first));
            first->edges.emplace(second, &edge);
            second->edges.emplace(first, &edge);
            return &edge;
        }

        void eraseEdge(EdgeType& edge)
        {
            edge.first.edges.erase(&edge.second);
            edge.second.edges.erase(&edge.first);
            edges.erase(edge);
        }

        friend NodeType;
    };
} // namespace vc4c

#endif /* VC4C_GRAPH_H */
