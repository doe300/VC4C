/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_GRAPH_H
#define VC4C_GRAPH_H

#include "CompilationError.h"
#include "Optional.h"
#include "performance.h"

#include <functional>
#include <type_traits>

namespace vc4c
{
    /*
     * A node for in a graph, general base-class maintaining the list of neighbors and their relations
     */
    template <typename K, typename R>
    struct Node : private NonCopyable
    {
        using NeighborsType = FastMap<Node*, R>;
        using KeyType = K;

        const K key;

        explicit Node(const K key) : key(key) {}

        typename NeighborsType::value_type& emplace(Node * neighbor, R relation)
        {
            neighbor->pointee.emplace(this);
            return * neighbors.emplace(neighbor, relation).first;
        }

        /*!
         * Adds the given neighbor with the given relation.
         * Multiple calls to this method do not override the previous association.
         */
        void addNeighbor(Node* neighbor, R relation)
        {
            neighbor->pointee.emplace(this);
            neighbors.emplace(neighbor, relation).first;
        }

        typename NeighborsType::value_type& getOrCreateNeighbor(Node* neighbor, R defaultRelation = {})
        {
            auto it = neighbors.find(neighbor);
            if(it != neighbors.end())
                return *it;
            return emplace(neighbor, defaultRelation);
        }

        const NeighborsType& getNeighbors() const
        {
            return neighbors;
        }

        NeighborsType& getNeighbors()
        {
            return neighbors;
        }

        /*
         * Executes the given consumer for all neighbors with the given relation
         */
        void forAllNeighbors(const R relation, const std::function<void(const Node*)>& consumer) const
        {
            for(const auto& pair : neighbors)
                if(pair.second == relation)
                    consumer(pair.first);
        }

        /*
         * Executes the given consumer for all neighbors, where the relation to this node match the given predicate
         */
        void forAllNeighbors(const std::function<bool(const R&)>& relation,
            const std::function<void(const Node*, const R&)>& consumer) const
        {
            for(const auto& pair : neighbors)
                if(relation(pair.second))
                    consumer(pair.first, pair.second);
        }

        /*
         * Returns the single neighbor with the given relation.
         * Returns nullptr otherwise, if there is no or more than one neighbor with this relation.
         */
        const Node* getSingleNeighbor(const R relation) const
        {
            const Node* singleNeighbor = nullptr;
            for(const auto& pair : neighbors)
            {
                if(pair.second == relation)
                {
                    if(singleNeighbor != nullptr)
                        // multiple neighbors
                        return nullptr;
                    singleNeighbor = pair.first;
                }
            }
            return singleNeighbor;
        }

        /*
         * Returns the single neighbor where the relation matches the given predicate.
         * Returns nullptr otherwise, if there is no or more than one neighbor with this relation.
         */
        const Node* getSingleNeighbor(const std::function<bool(const R&)>& relation) const
        {
            const Node* singleNeighbor = nullptr;
            for(const auto& pair : neighbors)
            {
                if(relation(pair.second))
                {
                    if(singleNeighbor != nullptr)
                        // multiple neighbors
                        return nullptr;
                    singleNeighbor = pair.first;
                }
            }
            return singleNeighbor;
        }

        static std::string to_string(const K& key)
        {
            return key.to_string();
        }

        /*
         * erase all references to this node
         */
        void eraseReference()
        {
            for (auto it = pointee.begin(); it != pointee.end(); ++it)
            {
                (*it)->neighbors.erase(this);
                (*it)->pointee.erase(this);
            }
        }

        /*
         * Erase edge to a node
         */
        void erase(Node* node)
        {
          node->getPointee().erase(this);
          neighbors.erase(node);
        }

        std::set<Node*> getPointee()
        {
          return pointee;
        }

    protected:
        // TODO find better way, so the map is to the actual (child) type
        FastMap<Node*, R> neighbors;
        std::set<Node*> pointee;
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
    template <typename K, typename NodeType>
    struct Graph : public FastMap<K, NodeType>
    {
        using Base = FastMap<K, NodeType>;
        using Node = typename Base::value_type;

        /*
         * Returns the node for the given key
         *
         * If such a node does not exist yet, a new node is created with the given additional initial payload
         */
        template <typename... Args>
        NodeType& getOrCreateNode(const K& key, const Args... initialPayload)
        {
            if(Base::find(key) == Base::end())
            {
                return Base::emplace(key, NodeType(key, initialPayload...)).first->second;
            }
            return Base::at(key);
        }

        /*
         * Guarantees a node for the given key to exist within the graph and returns it.
         * Throws a compilation-error otherwise
         */
        NodeType& assertNode(const K& key)
        {
            if(Base::find(key) == Base::end())
            {
                throw CompilationError(CompilationStep::GENERAL, "Failed to find graph-node for key");
            }
            return Base::at(key);
        }

        void erase(const K& key)
        {
            auto base = Base::find(key);
            if (base != Base::end())
            {
                auto & node = base->second;
                node.eraseReference();
            }

            Base::erase(key);
        }
    };
} // namespace vc4c

#endif /* VC4C_GRAPH_H */
