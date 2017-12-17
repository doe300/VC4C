/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_GRAPH_H
#define VC4C_GRAPH_H

#include "CompilationError.h"
#include "helper.h"
#include "performance.h"

#include <functional>
#include <type_traits>

namespace vc4c
{
	template<typename K, typename R>
	struct Node : private NonCopyable
	{
		using NeighborsType = FastMap<Node*, R>;

		const K key;

		explicit Node(const K key) : key(key) { }

		/*!
		 * Adds the given neighbor with the given relation.
		 * Multiple calls to this method do not override the previous association.
		 */
		void addNeighbor(Node* neighbor, const R relation)
		{
			neighbors.emplace(neighbor, relation);
		}

		const NeighborsType& getNeighbors() const
		{
			return neighbors;
		}

		NeighborsType& getNeighbors()
		{
			return neighbors;
		}

		void forAllNeighbors(const R relation, const std::function<void(const Node*)>& consumer) const
		{
			for(const auto& pair : neighbors)
				if(pair.second == relation)
					consumer(pair.first);
		}

		static std::string to_string(const K& key)
		{
			return key.to_string();
		}

	protected:
		//TODO find better way, so the map is to the actual (child) type
		FastMap<Node*, R> neighbors;
	};

	template<typename K, typename NodeType>
	struct Graph : public FastMap<K, NodeType>
	{
		using Base = FastMap<K, NodeType>;
		using Node = typename Base::value_type;

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
} // namespace vc4c


#endif /* VC4C_GRAPH_H */
