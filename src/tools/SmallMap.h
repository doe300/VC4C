/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#pragma once

#include "../Variant.h"

#include <algorithm>
#include <array>
#include <map>
#include <type_traits>

namespace vc4c
{
    namespace tools
    {
        /**
         * Container similar to a map, but with a fixed size that is allocated inline in the object.
         *
         * NOTE: In contrast to std::map, inserting or deleting elements might invalidate iterators!
         */
        template <typename K, typename V, std::size_t N, typename C = std::less<K>>
        class FixedSortedPointerMap
        {
            // For now, only support pointers as keys and take nullptr as "not set"
            static_assert(std::is_pointer<K>::value, "The key must be a pointer type");

            // For now only support stateless comparators, so we do not need to store the objects
            static_assert(std::is_empty<C>::value, "Only stateless comparators are supported");

            // Since we create pairs (and therefore values) for entries which do not exist, they need to be default
            // constructible
            static_assert(std::is_default_constructible<V>::value, "The value type needs to be default constructible");

            // The marker for an entry that is not set, is sorted to the end of the array
            static constexpr auto TOMBSTONE = std::numeric_limits<uintptr_t>::max();

            using Container = std::array<std::pair<K, V>, N>;
            using Element = typename Container::value_type;

            struct ElementComparator
            {
                C comp;
                bool operator()(const Element& e1, const Element& e2) const
                {
                    return comp(e1.first, e2.first);
                }
            };

            struct ElementWithKeyComparator
            {
                C comp;
                bool operator()(const Element& e1, const K& k2) const
                {
                    return comp(e1.first, k2);
                }
            };

        public:
            using key_type = K;
            using mapped_type = V;
            using value_type = typename Container::value_type;
            using key_compare = C;
            using reference = typename Container::reference;
            using const_reference = typename Container::const_reference;
            using pointer = typename Container::pointer;
            using const_pointer = typename Container::const_pointer;
            using iterator = typename Container::iterator;
            using const_iterator = typename Container::const_iterator;
            using difference_type = typename Container::difference_type;
            using size_type = typename Container::size_type;

            explicit FixedSortedPointerMap()
            {
                // need to set all pointers initially to TOMBSTONE value
                clear();
            }

            iterator begin() noexcept
            {
                return data.begin();
            }

            const_iterator begin() const noexcept
            {
                return data.begin();
            }

            iterator end() noexcept
            {
                return findInner(reinterpret_cast<K>(TOMBSTONE));
            }

            const_iterator end() const noexcept
            {
                return findInner(reinterpret_cast<K>(TOMBSTONE));
            }

            bool empty() const noexcept
            {
                // if the first element is a tombstone, all are, since tombstones are sorted to the end
                return data[0].first == reinterpret_cast<K>(TOMBSTONE);
            }

            bool full() const noexcept
            {
                // if the first element is not a tombstone, none are, since tombstones are sorted to the end
                return data.back().first != reinterpret_cast<K>(TOMBSTONE);
            }

            size_type size() const noexcept
            {
                return findInner(reinterpret_cast<K>(TOMBSTONE)) - begin();
            }

            size_type max_size() const noexcept
            {
                return data.size();
            }

            mapped_type& operator[](const key_type& key)
            {
                auto it = find(key);
                if(it == data.end())
                    // key not found and array is full
                    throw std::out_of_range{"Fixed pointer map is full, can't insert key"};
                if(it->first == reinterpret_cast<K>(TOMBSTONE))
                {
                    // insert new element
                    *it = std::make_pair(key, V{});
                    sort();
                    return find(key)->second;
                }
                return it->second;
            }

            mapped_type& at(const key_type& key)
            {
                auto it = findInner(key);
                if(it != data.end() && it->first == key)
                    return it->second;
                throw std::out_of_range{"Key not in fixed pointer map"};
            }

            const mapped_type& at(const key_type& key) const
            {
                auto it = findInner(key);
                if(it != data.end() && it->first == key)
                    return it->second;
                throw std::out_of_range{"Key not in fixed pointer map"};
            }

            void erase(const_iterator position)
            {
                *const_cast<iterator>(position) = std::make_pair(reinterpret_cast<K>(TOMBSTONE), V{});
                sort();
            }

            size_type erase(const key_type& key)
            {
                auto it = findInner(key);
                if(it != data.end() && it->first == key)
                {
                    erase(it);
                    return 1;
                }
                return 0;
            }

            void clear()
            {
                data.fill(std::make_pair(reinterpret_cast<K>(TOMBSTONE), V{}));
            }

            template <typename... Args>
            std::pair<iterator, bool> emplace(Args&&... args)
            {
                value_type tmp{std::forward<Args>(args)...};
                auto it = find(tmp.first);
                if(it == data.end())
                    // key not found and array is full
                    throw std::out_of_range{"Fixed pointer map is full, can't insert key"};
                if(it->first == reinterpret_cast<K>(TOMBSTONE))
                {
                    auto key = tmp.first;
                    *it = std::move(tmp);
                    sort();
                    return std::make_pair(find(key), true);
                }
                return std::make_pair(it, false);
            }

            iterator find(const key_type& key)
            {
                auto it = findInner(key);
                if(it == data.end() || it->first == key)
                    return it;
                return end();
            }

            const_iterator find(const key_type& key) const
            {
                auto it = findInner(key);
                if(it == data.end() || it->first == key)
                    return it;
                return end();
            }

        private:
            Container data;

            void sort()
            {
                std::sort(data.begin(), data.end(), ElementComparator{});
            }

            iterator findInner(const key_type& key)
            {
                return std::lower_bound(data.begin(), data.end(), key, ElementWithKeyComparator{});
            }

            const_iterator findInner(const key_type& key) const
            {
                return std::lower_bound(data.begin(), data.end(), key, ElementWithKeyComparator{});
            }
        };

        /**
         * Container similar to a std::map, but stores the first few elements in-line.
         *
         * This container should be preferred when the object usually contains a small number of elements to avoid
         * allocating memory on the heap.
         *
         * NOTE: In contrast to std::map, inserting or deleting elements might invalidate iterators!
         *
         * NOTE: The NULL pointer is considered a marker for a "not set" entry in the small map and can therefore
         * not be used as valid value!
         */
        template <typename K, typename V>
        class SmallSortedPointerMap
        {
            // For now, only support pointers as keys and take nullptr as "not set"
            static_assert(std::is_pointer<K>::value, "The key must be a pointer type");

            // The number of elements in the small version
            static constexpr auto SmallSize = sizeof(std::map<K, V>) / sizeof(std::pair<K, V>);
            static_assert(SmallSize > 0, "Element size too big, the small version has no elements");

            using Comparator = std::less<K>;
            using SmallContainer = FixedSortedPointerMap<K, V, SmallSize, Comparator>;
            using BigContainer = std::map<K, V, Comparator>;
            static_assert(sizeof(SmallContainer) <= sizeof(BigContainer), "Small container is too big");

            struct ConstIterator
            {
                using value_type = typename BigContainer::value_type;
                using difference_type = ptrdiff_t;
                using pointer = value_type*;
                using reference = value_type&;
                using iterator_category = std::bidirectional_iterator_tag;

                bool isBig;
                typename SmallContainer::const_iterator smallIt;
                typename BigContainer::const_iterator bigIt;

                bool operator==(const ConstIterator& other) const noexcept
                {
                    return isBig == other.isBig && (isBig ? bigIt == other.bigIt : smallIt == other.smallIt);
                }

                bool operator!=(const ConstIterator& other) const noexcept
                {
                    return !(*this == other);
                }

                const std::pair<const K, V>& operator*() const noexcept
                {
                    return isBig ? *bigIt : reinterpret_cast<const std::pair<const K, V>&>(*smallIt);
                }

                const std::pair<const K, V>* operator->() const noexcept
                {
                    return isBig ? &*bigIt : reinterpret_cast<const std::pair<const K, V>*>(&*smallIt);
                }

                ConstIterator& operator++() noexcept
                {
                    if(isBig)
                        ++bigIt;
                    else
                        ++smallIt;
                    return *this;
                }

                ConstIterator operator++(int) noexcept
                {
                    auto copy = *this;
                    if(isBig)
                        ++bigIt;
                    else
                        ++smallIt;
                    return copy;
                }

                ConstIterator& operator--() noexcept
                {
                    if(isBig)
                        --bigIt;
                    else
                        --smallIt;
                    return *this;
                }

                ConstIterator operator--(int) noexcept
                {
                    auto copy = *this;
                    if(isBig)
                        --bigIt;
                    else
                        --smallIt;
                    return copy;
                }
            };

            struct Iterator
            {
                using value_type = typename BigContainer::value_type;
                using difference_type = ptrdiff_t;
                using pointer = value_type*;
                using reference = value_type&;
                using iterator_category = std::bidirectional_iterator_tag;

                bool isBig;
                typename SmallContainer::iterator smallIt;
                typename BigContainer::iterator bigIt;

                bool operator==(const Iterator& other) const noexcept
                {
                    return isBig == other.isBig && (isBig ? bigIt == other.bigIt : smallIt == other.smallIt);
                }

                bool operator!=(const Iterator& other) const noexcept
                {
                    return !(*this == other);
                }

                std::pair<const K, V>& operator*() noexcept
                {
                    return isBig ? *bigIt : reinterpret_cast<std::pair<const K, V>&>(*smallIt);
                }

                std::pair<const K, V>* operator->() noexcept
                {
                    return isBig ? &*bigIt : reinterpret_cast<std::pair<const K, V>*>(&*smallIt);
                }

                Iterator& operator++() noexcept
                {
                    if(isBig)
                        ++bigIt;
                    else
                        ++smallIt;
                    return *this;
                }

                Iterator operator++(int) noexcept
                {
                    auto copy = *this;
                    if(isBig)
                        ++bigIt;
                    else
                        ++smallIt;
                    return copy;
                }

                Iterator& operator--() noexcept
                {
                    if(isBig)
                        --bigIt;
                    else
                        --smallIt;
                    return *this;
                }

                Iterator operator--(int) noexcept
                {
                    auto copy = *this;
                    if(isBig)
                        --bigIt;
                    else
                        --smallIt;
                    return copy;
                }

                operator ConstIterator() const noexcept
                {
                    return ConstIterator{isBig, smallIt, bigIt};
                }
            };

        public:
            using key_type = typename BigContainer::key_type;
            using mapped_type = typename BigContainer::mapped_type;
            using value_type = typename BigContainer::value_type;
            using key_compare = typename BigContainer::key_compare;
            using reference = typename BigContainer::reference;
            using const_reference = typename BigContainer::const_reference;
            using pointer = typename BigContainer::pointer;
            using const_pointer = typename BigContainer::const_pointer;
            using iterator = Iterator;
            using const_iterator = ConstIterator;
            using difference_type = typename BigContainer::difference_type;
            using size_type = typename BigContainer::size_type;

            explicit SmallSortedPointerMap() : data(SmallContainer{}) {}

            iterator begin() noexcept
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return Iterator{false, small->begin(), {}};
                return Iterator{true, {}, VariantNamespace::get<BigContainer>(data).begin()};
            }

            const_iterator begin() const noexcept
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return ConstIterator{false, small->begin(), {}};
                return ConstIterator{true, {}, VariantNamespace::get<BigContainer>(data).begin()};
            }

            iterator end() noexcept
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return Iterator{false, small->end(), {}};
                return Iterator{true, {}, VariantNamespace::get<BigContainer>(data).end()};
            }

            const_iterator end() const noexcept
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return ConstIterator{false, small->end(), {}};
                return ConstIterator{true, {}, VariantNamespace::get<BigContainer>(data).end()};
            }

            bool empty() const noexcept
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return small->empty();
                return VariantNamespace::get<BigContainer>(data).empty();
            }

            size_type size() const noexcept
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return small->size();
                return VariantNamespace::get<BigContainer>(data).size();
            }

            mapped_type& operator[](const key_type& key)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                {
                    auto it = small->find(key);
                    if(it != small->end())
                        return it->second;
                    if(!small->full())
                        return (*small)[key];
                    useBigContainer();
                }
                return VariantNamespace::get<BigContainer>(data)[key];
            }

            mapped_type& at(const key_type& key)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return small->at(key);
                return VariantNamespace::get<BigContainer>(data).at(key);
            }

            const mapped_type& at(const key_type& key) const
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return small->at(key);
                return VariantNamespace::get<BigContainer>(data).at(key);
            }

            void erase(const_iterator position)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    small->erase(position.smallIt);
                else
                    VariantNamespace::get<BigContainer>(data).erase(position.bigIt);
            }

            size_type erase(const key_type& key)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return small->erase(key);
                return VariantNamespace::get<BigContainer>(data).erase(key);
            }

            void clear()
            {
                data = SmallContainer{};
            }

            template <typename... Args>
            std::pair<iterator, bool> emplace(Args&&... args)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                {
                    if(!small->full())
                    {
                        auto tmp = small->emplace(std::forward<Args>(args)...);
                        return std::make_pair(Iterator{false, tmp.first, {}}, tmp.second);
                    }
                    else
                        // TODO if the element is already in there, this wastes resources
                        useBigContainer();
                }
                auto tmp = VariantNamespace::get<BigContainer>(data).emplace(std::forward<Args>(args)...);
                return std::make_pair(Iterator{true, {}, tmp.first}, tmp.second);
            }

            iterator find(const key_type& key)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return Iterator{false, small->find(key), {}};
                return Iterator{true, {}, VariantNamespace::get<BigContainer>(data).find(key)};
            }

            const_iterator find(const key_type& key) const
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                    return ConstIterator{false, small->find(key), {}};
                return ConstIterator{true, {}, VariantNamespace::get<BigContainer>(data).find(key)};
            }

            /**
             * The number of elements stored inline without requiring heap allocations.
             */
            constexpr size_type small_size()
            {
                return SmallSize;
            }

        private:
            Variant<SmallContainer, BigContainer> data;

            void useBigContainer()
            {
                // copy all entries and insert them back to the big container
                auto tmp = VariantNamespace::get<SmallContainer>(data);
                data = BigContainer{tmp.begin(), tmp.end()};
            }
        };

        // just to make sure we don't have too much overhead
        static_assert(sizeof(FixedSortedPointerMap<void*, void*, 4>) == sizeof(std::array<std::pair<void*, void*>, 4>),
            "FixedPointerMap wastes space");
        static_assert(sizeof(SmallSortedPointerMap<void*, void*>) == (sizeof(void*) + sizeof(std::map<void*, void*>)),
            "SmallSortedPointerMap wastes space");

        static_assert(std::is_trivially_destructible<FixedSortedPointerMap<void*, unsigned, 4>>::value, "");

    } // namespace tools
} // namespace vc4c
