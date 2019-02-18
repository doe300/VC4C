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
    /*!
     * A list type which allows fast read-access to arbitrary elements in the container (e.g. by using a constant
     * offset)
     */
    template <typename T>
    using FastAccessList = std::vector<T>;
    /*!
     * A list type which allows fast insertion/deletion and reordering of arbitrary elements in the container (e.g. by
     * simply manipulating pointers to the next/previous elements)
     */
    template <typename T>
    using FastModificationList = std::list<T>;
    /*!
     * A list-type which is stored compactly in memory providing better cache behavior and little to no memory overhead
     */
    template <typename T>
    using CompactList = std::vector<T>;
    /*!
     * A list type in which the references (and pointers) to the elements are not modified (e.g. stay valid even after
     * insertions, modifications)
     */
    template <typename T>
    using StableList = std::list<T>;

    /*!
     * A set type where the elements are sorted according to their (natural or explicit) order
     */
    template <typename T, typename C = std::less<T>>
    using SortedSet = std::set<T, C>;
    /*!
     * A set type which allows fast lookup and insertion of elements
     */
    template <typename T, typename H = std::hash<T>>
    using FastSet = std::unordered_set<T, H>;

    /*!
     * A map type where the entries are sorted according to the (natural or explicit) order of the keys
     */
    template <typename K, typename V, typename C = std::less<K>>
    using SortedMap = std::map<K, V, C>;
    /*!
     * A map type providing fast lookup and insertion of elements
     */
    template <typename K, typename V, typename H = std::hash<K>>
    using FastMap = std::unordered_map<K, V, H>;
} // namespace vc4c

#endif /* PERFORMANCE_H */
