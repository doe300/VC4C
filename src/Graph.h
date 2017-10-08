/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_GRAPH_H
#define VC4C_GRAPH_H

#include <type_traits>

#include "performance.h"
#include "CompilationError.h"

namespace vc4c
{
	template<typename K, typename R>
	struct Node
	{
		const K key;

		Node(const K key) : key(key)
		{
		}

		/*!
		 * Adds the given neighbor with the given relation.
		 * Multiple calls to this method do not override the previous association.
		 */
		void addNeighbor(Node* neighbor, const R relation)
		{
			neighbors.emplace(neighbor, relation);
		}

		const FastMap<Node*, R>& getNeighbors() const
		{
			return neighbors;
		}

		void forAllNeighbors(const R relation, const std::function<void(const Node*)>& consumer) const
		{
			for(const auto& pair : neighbors)
				if(pair.second == relation)
					consumer(pair.first);
		}

	protected:
		//TODO find better way, so the map is to the actual (child) type
		FastMap<Node*, R> neighbors;
	};

	template<typename K, typename NodeType>
	struct Graph : public FastMap<K, NodeType>
	{
		using Base = FastMap<K, NodeType>;

		template<typename... Args>
		NodeType& getOrCreateNode(const K& key, const Args... initialPayload)
		{
			if(Base::find(key) == Base::end())
			{
				return Base::emplace(key, NodeType(key, initialPayload...)).first->second;
			}
			return Base::at(key);
		}

		NodeType& assertNode(const K& key)
		{
			if(Base::find(key) == Base::end())
			{
				throw CompilationError(CompilationStep::GENERAL, "Failed to find graph-node for key");
			}
			return Base::at(key);
		}
	};
}


#endif /* VC4C_GRAPH_H */
