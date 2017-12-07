/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PERFORMANCE_H
#define PERFORMANCE_H

#include <list>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace vc4c
{
	template<typename T>
	struct hash : public std::hash<T> {};

	enum class AccessType
	{
		RANDOM_ACCESS = 1,
		SEQUENTIAL = 2
	};

	enum class InsertRemoveType
	{
		FRONT = 1,
		END = 2,
		ARBITRARY_POSITION = 3
	};

	enum class OrderType
	{
		ORDERED = 1,
		UNORDERED = 2
	};

	////
	// General types
	////

	template<typename T, AccessType A, InsertRemoveType IR>
	class PerformanceList;

	template<typename T, OrderType O, typename H = vc4c::hash<T>>
	class PerformanceSet;

	template<typename K, typename V, OrderType O, typename C = std::less<K>>
	class PerformanceMap;

	////
	// Specializations
	////

	template<typename T>
	class PerformanceList<T, AccessType::RANDOM_ACCESS, InsertRemoveType::END> : public std::vector<T> {};
	template<typename T>
	class PerformanceList<T, AccessType::SEQUENTIAL, InsertRemoveType::ARBITRARY_POSITION> : public std::list<T> {};

	template<typename T, typename H>
	class PerformanceSet<T, OrderType::UNORDERED, H> : public std::unordered_set<T, H> {};
	template<typename T, typename H>
	class PerformanceSet<T, OrderType::ORDERED, H> : public std::set<T, H> {};

	template<typename K, typename V, typename C>
	class PerformanceMap<K, V, OrderType::UNORDERED, C> : public std::unordered_map<K, V, C> {};

	template<typename K, typename V, typename C>
	class PerformanceMap<K, V, OrderType::ORDERED, C> : public std::map<K, V, C> {};

	////
	// Easier names
	////

	template<typename T>
	using RandomAccessList = PerformanceList<T, AccessType::RANDOM_ACCESS, InsertRemoveType::END>;
	template<typename T>
	using RandomModificationList = PerformanceList<T, AccessType::SEQUENTIAL, InsertRemoveType::ARBITRARY_POSITION>;
	template<typename T>
	using FastAccessList = RandomAccessList<T>;
	template<typename T>
	using FastModificationList = RandomModificationList<T>;
	/*!
	 * A list type in which the references (and pointers) to the elements are not modified (e.g. stay valid even after insertions, modifications)
	 */
	template<typename T>
	using ReferenceRetainingList = RandomModificationList<T>;

	template<typename T, typename H = vc4c::hash<T>>
	using UnorderedSet = PerformanceSet<T, OrderType::UNORDERED, H>;
	template<typename T, typename C = std::less<T>>
	using OrderedSet = PerformanceSet<T, OrderType::ORDERED, C>;
	template<typename T>
	using FastSet = UnorderedSet<T>;

	template<typename K, typename V, typename H = vc4c::hash<K>>
	using UnorderedMap = PerformanceMap<K, V, OrderType::UNORDERED, H>;
	template<typename K, typename V, typename C = std::less<K>>
	using OrderedMap = PerformanceMap<K, V, OrderType::ORDERED, C>;
	template<typename K, typename V>
	using FastMap = UnorderedMap<K, V>;
} // namespace vc4c


#endif /* PERFORMANCE_H */
