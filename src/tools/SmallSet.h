/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#pragma once

#include "../Variant.h"

#include <algorithm>
#include <array>
#include <set>
#include <stdexcept>
#include <type_traits>

namespace vc4c
{
    namespace tools
    {
        /**
         * Container similar to a set, but with a fixed size that is allocated inline in the object.
         *
         * NOTE: In contrast to std::set, inserting or deleting elements might invalidate iterators!
         */
        template <typename T, std::size_t N, typename C = std::less<T>>
        class FixedSortedPointerSet
        {
            // For now, only support pointers as keys and take nullptr as "not set"
            static_assert(std::is_pointer<T>::value, "The type must be a pointer type");

            // For now only support stateless comparators, so we do not need to store the objects
            static_assert(std::is_empty<C>::value, "Only stateless comparators are supported");

            // The marker for an entry that is not set, is sorted to the end of the array
            static constexpr auto TOMBSTONE = std::numeric_limits<uintptr_t>::max();

            using Container = std::array<T, N>;

        public:
            using key_type = T;
            using value_type = typename Container::value_type;
            using key_compare = C;
            using value_compare = key_compare;
            using reference = typename Container::reference;
            using const_reference = typename Container::const_reference;
            using pointer = typename Container::pointer;
            using const_pointer = typename Container::const_pointer;
            using iterator = typename Container::iterator;
            using const_iterator = typename Container::const_iterator;
            using difference_type = typename Container::difference_type;
            using size_type = typename Container::size_type;

            explicit FixedSortedPointerSet()
            {
                // need to set all pointers initially to TOMBSTONE value
                clear();
            }

            FixedSortedPointerSet(std::initializer_list<key_type> init) : FixedSortedPointerSet()
            {
                std::copy(init.begin(), init.end(), data.begin());
                sort();
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
                return findInner(reinterpret_cast<T>(TOMBSTONE));
            }

            const_iterator end() const noexcept
            {
                return findInner(reinterpret_cast<T>(TOMBSTONE));
            }

            bool empty() const noexcept
            {
                // if the first element is a tombstone, all are, since tombstones are sorted to the end
                return data[0] == reinterpret_cast<T>(TOMBSTONE);
            }

            bool full() const noexcept
            {
                // if the last element is not a tombstone, none are, since tombstones are sorted to the end
                return data.back() != reinterpret_cast<T>(TOMBSTONE);
            }

            size_type size() const noexcept
            {
                return static_cast<size_type>(findInner(reinterpret_cast<T>(TOMBSTONE)) - begin());
            }

            size_type max_size() const noexcept
            {
                return data.size();
            }

            void erase(const_iterator position)
            {
                *const_cast<iterator>(position) = reinterpret_cast<T>(TOMBSTONE);
                sort();
            }

            size_type erase(const key_type& key)
            {
                auto it = findInner(key);
                if(it != data.end() && *it == key)
                {
                    erase(it);
                    return 1;
                }
                return 0;
            }

            void clear()
            {
                data.fill(reinterpret_cast<T>(TOMBSTONE));
            }

            std::pair<iterator, bool> emplace(key_type arg)
            {
                auto it = find(arg);
                if(it == data.end())
                    // key not found and array is full
                    throw std::out_of_range{"Fixed pointer set is full, can't insert key"};
                if(*it == reinterpret_cast<T>(TOMBSTONE))
                {
                    *it = arg;
                    sort();
                    return std::make_pair(find(arg), true);
                }
                return std::make_pair(it, false);
            }

            iterator find(const key_type& key)
            {
                auto it = findInner(key);
                if(it == data.end() || *it == key)
                    return it;
                return end();
            }

            const_iterator find(const key_type& key) const
            {
                auto it = findInner(key);
                if(it == data.end() || *it == key)
                    return it;
                return end();
            }

            bool operator==(const FixedSortedPointerSet& other) const
            {
                return data == other.data;
            }

            bool operator!=(const FixedSortedPointerSet& other) const
            {
                return data != other.data;
            }

        private:
            Container data;

            void sort()
            {
                std::sort(data.begin(), data.end(), key_compare{});
            }

            iterator findInner(const key_type& key)
            {
                return std::lower_bound(data.begin(), data.end(), key, key_compare{});
            }

            const_iterator findInner(const key_type& key) const
            {
                return std::lower_bound(data.begin(), data.end(), key, key_compare{});
            }
        };

        /**
         * Container similar to a std::set, but stores the first few elements in-line.
         *
         * This container should be preferred when the object usually contains a small number of elements to avoid
         * allocating memory on the heap.
         *
         * NOTE: In contrast to std::set, inserting or deleting elements might invalidate iterators!
         *
         * NOTE: The NULL pointer is considered a marker for a "not set" entry in the small set and can therefore
         * not be used as valid value!
         */
        template <typename T>
        class SmallSortedPointerSet
        {
            // For now, only support pointers as keys and take nullptr as "not set"
            static_assert(std::is_pointer<T>::value, "The key must be a pointer type");

            // The number of elements in the small version
            static constexpr auto SmallSize = sizeof(std::set<T>) / sizeof(T);
            static_assert(SmallSize > 0, "Element size too big, the small version has no elements");

            using Comparator = std::less<T>;
            using SmallContainer = FixedSortedPointerSet<T, SmallSize, Comparator>;
            using BigContainer = std::set<T, Comparator>;
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

                const T& operator*() const noexcept
                {
                    return isBig ? *bigIt : reinterpret_cast<const T&>(*smallIt);
                }

                const T* operator->() const noexcept
                {
                    return isBig ? &*bigIt : reinterpret_cast<const T*>(&*smallIt);
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

                const T& operator*() noexcept
                {
                    return isBig ? *bigIt : reinterpret_cast<const T&>(*smallIt);
                }

                const T* operator->() noexcept
                {
                    return isBig ? &*bigIt : reinterpret_cast<const T*>(&*smallIt);
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
            using value_type = typename BigContainer::value_type;
            using key_compare = typename BigContainer::key_compare;
            using value_compare = typename BigContainer::value_compare;
            using reference = typename BigContainer::reference;
            using const_reference = typename BigContainer::const_reference;
            using pointer = typename BigContainer::pointer;
            using const_pointer = typename BigContainer::const_pointer;
            using iterator = Iterator;
            using const_iterator = ConstIterator;
            using difference_type = typename BigContainer::difference_type;
            using size_type = typename BigContainer::size_type;

            explicit SmallSortedPointerSet() : data(SmallContainer{}) {}

            SmallSortedPointerSet(std::initializer_list<key_type> init)
            {
                if(init.size() <= small_size())
                    data = SmallContainer{init};
                else
                    data = BigContainer{init};
            }

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

            std::pair<iterator, bool> emplace(key_type arg)
            {
                if(auto small = VariantNamespace::get_if<SmallContainer>(&data))
                {
                    if(!small->full())
                    {
                        auto tmp = small->emplace(arg);
                        return std::make_pair(Iterator{false, tmp.first, {}}, tmp.second);
                    }
                    else
                        // TODO if the element is already in there, this wastes resources
                        useBigContainer();
                }
                auto tmp = VariantNamespace::get<BigContainer>(data).emplace(arg);
                return std::make_pair(Iterator{true, {}, tmp.first}, tmp.second);
            }

            iterator insert(const_iterator position, const value_type& val)
            {
                return emplace(val).first;
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

            bool operator==(const SmallSortedPointerSet& other) const
            {
                return data == other.data;
            }

            bool operator!=(const SmallSortedPointerSet& other) const
            {
                return data != other.data;
            }

        private:
            Variant<SmallContainer, BigContainer> data;

            void useBigContainer()
            {
                // copy all entries and insert them back to the big container
                auto tmp = VariantNamespace::get<SmallContainer>(data);
                // NOTE: () on purpose to select range constructor and not the initializer list!
                data = BigContainer(tmp.begin(), tmp.end());
            }
        };

        // just to make sure we don't have too much overhead
        static_assert(
            sizeof(FixedSortedPointerSet<void*, 4>) == sizeof(std::array<void*, 4>), "FixedPointerSet wastes space");
        static_assert(sizeof(SmallSortedPointerSet<void*>) == (sizeof(void*) + sizeof(std::set<void*>)),
            "SmallSortedPointerSet wastes space");

        static_assert(std::is_trivially_destructible<FixedSortedPointerSet<void*, 4>>::value, "");

    } // namespace tools
} // namespace vc4c
