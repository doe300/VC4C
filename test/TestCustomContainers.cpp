/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestCustomContainers.h"

#include "tools/SmallMap.h"
#include "tools/SmallSet.h"

using namespace vc4c::tools;

TestCustomContainers::TestCustomContainers()
{
    TEST_ADD(TestCustomContainers::testFixedSortedPointerMap);
    TEST_ADD(TestCustomContainers::testSmallSortedPointerMap);

    TEST_ADD(TestCustomContainers::testFixedSortedPointerSet);
    TEST_ADD(TestCustomContainers::testSmallSortedPointerSet);
}

TestCustomContainers::~TestCustomContainers() = default;

static auto SOME_POINTER = reinterpret_cast<void*>(0xFFF33F);
static auto OTHER_POINTER = reinterpret_cast<void*>(0xF22FF33F);
static auto NEXT_POINTER = reinterpret_cast<void*>(0xF22FF332);
static auto NOT_FOUND_POINTER = reinterpret_cast<void*>(0xFF1F33F);
static auto SOME_MORE_POINTER = reinterpret_cast<void*>(0xF233F);
static auto ANOTHER_POINTER = reinterpret_cast<void*>(0x11FF33F);
static auto EVEN_MORE_POINTER = reinterpret_cast<void*>(0xF11AF);
static auto AND_SOME_MORE_POINTER = reinterpret_cast<void*>(0x1F11AF);
static auto MAYBE_LAST_POINTER = reinterpret_cast<void*>(0x1ABCDEF);

template <typename T, typename U>
static bool hasSameMapContent(const T& first, const U& second);
template <typename T, typename U>
static bool hasSameSetContent(const T& first, const U& second);

void TestCustomContainers::testFixedSortedPointerMap()
{
    std::map<void*, unsigned> reference = {{SOME_POINTER, 12}, {OTHER_POINTER, 13}};
    FixedSortedPointerMap<void*, unsigned, 3> map0 = {{SOME_POINTER, 12}, {OTHER_POINTER, 13}};

    TEST_ASSERT_EQUALS(3, map0.max_size());
    TEST_ASSERT(!map0.full());
    TEST_ASSERT(hasSameMapContent(reference, map0));

    auto refInsert = reference.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(refInsert.second);
    TEST_ASSERT_EQUALS(51, refInsert.first->second);
    auto insert0 = map0.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(insert0.second);
    TEST_ASSERT_EQUALS(51, insert0.first->second);
    TEST_ASSERT(reference.find(NEXT_POINTER) != reference.end());
    TEST_ASSERT(map0.find(NEXT_POINTER) != map0.end());

    refInsert = reference.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(!refInsert.second);
    TEST_ASSERT_EQUALS(51, refInsert.first->second);
    insert0 = map0.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(!insert0.second);
    TEST_ASSERT_EQUALS(51, insert0.first->second);

    TEST_ASSERT(hasSameMapContent(reference, map0));
    TEST_ASSERT(map0.full());
    TEST_ASSERT(reference.find(NEXT_POINTER) != reference.end());
    TEST_ASSERT(map0.find(NEXT_POINTER) != map0.end());

    // test removal
    TEST_ASSERT_EQUALS(1, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(1, map0.erase(NEXT_POINTER));
    TEST_ASSERT(hasSameMapContent(reference, map0));

    // test removal of non-element
    TEST_ASSERT_EQUALS(0, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(0, map0.erase(NEXT_POINTER));

    TEST_ASSERT(hasSameMapContent(reference, map0));
    TEST_ASSERT(!map0.full());

    TEST_ASSERT(reference.emplace(NEXT_POINTER, 13).second);
    TEST_ASSERT(map0.emplace(NEXT_POINTER, 13).second);
    TEST_THROWS(map0.emplace(NOT_FOUND_POINTER, 113), std::out_of_range);

    TEST_ASSERT_EQUALS(13, reference[NEXT_POINTER]);
    TEST_ASSERT_EQUALS(13, map0[NEXT_POINTER]);
    TEST_THROWS(map0[NOT_FOUND_POINTER] = 113, std::out_of_range);

    TEST_ASSERT(hasSameMapContent(reference, map0));
    TEST_ASSERT(map0.full());

    reference.at(NEXT_POINTER) = 15;
    map0.at(NEXT_POINTER) = 15;
    TEST_ASSERT_EQUALS(15, reference.at(NEXT_POINTER));
    TEST_ASSERT_EQUALS(15, map0.at(NEXT_POINTER));
    TEST_THROWS(reference.at(NOT_FOUND_POINTER) = 113, std::out_of_range);
    TEST_THROWS(map0.at(NOT_FOUND_POINTER) = 113, std::out_of_range);

    TEST_ASSERT(hasSameMapContent(reference, map0));
    TEST_ASSERT(map0.full());

    reference.erase(reference.find(NEXT_POINTER));
    map0.erase(map0.find(NEXT_POINTER));
    TEST_ASSERT(!map0.full());

    reference[NEXT_POINTER] = 14;
    map0[NEXT_POINTER] = 14;
    TEST_ASSERT_EQUALS(14, reference[NEXT_POINTER]);
    TEST_ASSERT_EQUALS(14, map0[NEXT_POINTER]);
    TEST_ASSERT(map0.full());
}

void TestCustomContainers::testSmallSortedPointerMap()
{
    std::map<void*, unsigned> reference = {{SOME_POINTER, 12}, {OTHER_POINTER, 13}};
    SmallSortedPointerMap<void*, unsigned> map0 = {{SOME_POINTER, 12}, {OTHER_POINTER, 13}};

    TEST_ASSERT_EQUALS(3, map0.small_size());
    TEST_ASSERT(hasSameMapContent(reference, map0));

    TEST_ASSERT_EQUALS(12, reference[SOME_POINTER]);
    TEST_ASSERT_EQUALS(13, reference[OTHER_POINTER]);
    TEST_ASSERT_EQUALS(12, map0[SOME_POINTER]);
    TEST_ASSERT_EQUALS(13, map0[OTHER_POINTER]);

    reference.erase(reference.find(SOME_POINTER));
    map0.erase(map0.find(SOME_POINTER));
    reference[SOME_POINTER] = 12;
    map0[SOME_POINTER] = 12;

    auto refInsert = reference.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(refInsert.second);
    TEST_ASSERT_EQUALS(51, refInsert.first->second);
    auto insert0 = map0.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(insert0.second);
    TEST_ASSERT_EQUALS(51, insert0.first->second);

    refInsert = reference.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(!refInsert.second);
    TEST_ASSERT_EQUALS(51, refInsert.first->second);
    insert0 = map0.emplace(NEXT_POINTER, 51u);
    TEST_ASSERT(!insert0.second);
    TEST_ASSERT_EQUALS(51, insert0.first->second);

    TEST_ASSERT(hasSameMapContent(reference, map0));

    // test removal
    TEST_ASSERT_EQUALS(1, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(1, map0.erase(NEXT_POINTER));
    TEST_ASSERT(hasSameMapContent(reference, map0));

    // test removal of non-element
    TEST_ASSERT_EQUALS(0, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(0, map0.erase(NEXT_POINTER));

    TEST_ASSERT(hasSameMapContent(reference, map0));

    TEST_ASSERT(reference.emplace(NEXT_POINTER, 13).second);
    TEST_ASSERT(map0.emplace(NEXT_POINTER, 13).second);
    TEST_ASSERT_EQUALS(13, reference[NEXT_POINTER]);
    TEST_ASSERT_EQUALS(13, map0[NEXT_POINTER]);

    TEST_ASSERT(hasSameMapContent(reference, map0));

    reference.at(NEXT_POINTER) = 15;
    map0.at(NEXT_POINTER) = 15;
    TEST_ASSERT_EQUALS(15, reference.at(NEXT_POINTER));
    TEST_ASSERT_EQUALS(15, map0.at(NEXT_POINTER));
    TEST_THROWS(reference.at(NOT_FOUND_POINTER) = 113, std::out_of_range);
    TEST_THROWS(map0.at(NOT_FOUND_POINTER) = 113, std::out_of_range);

    TEST_ASSERT(hasSameMapContent(reference, map0));

    reference.erase(reference.find(NEXT_POINTER));
    map0.erase(map0.find(NEXT_POINTER));

    reference[NEXT_POINTER] = 14;
    map0[NEXT_POINTER] = 14;
    TEST_ASSERT_EQUALS(14, reference[NEXT_POINTER]);
    TEST_ASSERT_EQUALS(14, map0[NEXT_POINTER]);

    TEST_ASSERT(reference.emplace(SOME_MORE_POINTER, 42).second);
    TEST_ASSERT(map0.emplace(SOME_MORE_POINTER, 42).second);
    reference[ANOTHER_POINTER] = 144;
    map0[ANOTHER_POINTER] = 144;
    TEST_ASSERT_EQUALS(144, reference[ANOTHER_POINTER]);
    TEST_ASSERT_EQUALS(144, map0[ANOTHER_POINTER]);

    TEST_ASSERT(hasSameMapContent(reference, map0));
    TEST_ASSERT(map0.size() > map0.small_size());

    reference.clear();
    map0.clear();
    TEST_ASSERT(reference.empty());
    TEST_ASSERT(map0.empty());

    reference[SOME_POINTER] = 12;
    map0[SOME_POINTER] = 12;

    reference.emplace(NEXT_POINTER, 13);
    map0.emplace(NEXT_POINTER, 13);

    reference.erase(reference.find(SOME_POINTER));
    map0.erase(map0.find(SOME_POINTER));
    reference.erase(NEXT_POINTER);
    map0.erase(NEXT_POINTER);
    TEST_ASSERT(reference.empty());
    TEST_ASSERT(map0.empty());

    TEST_ASSERT(reference.end() == reference.find(SOME_POINTER));
    TEST_ASSERT(map0.end() == map0.find(SOME_POINTER));

    reference[SOME_POINTER] = 12;
    map0[SOME_POINTER] = 12;

    TEST_ASSERT(reference.begin() == reference.find(SOME_POINTER));
    TEST_ASSERT(map0.begin() == map0.find(SOME_POINTER));
    TEST_ASSERT_EQUALS(12, reference.at(SOME_POINTER));
    TEST_ASSERT_EQUALS(12, map0.at(SOME_POINTER));

    TEST_ASSERT(hasSameMapContent(reference, map0));
}

void TestCustomContainers::testFixedSortedPointerSet()
{
    std::set<void*> reference = {SOME_POINTER, OTHER_POINTER};
    FixedSortedPointerSet<void*, 3> set0 = {SOME_POINTER, OTHER_POINTER};

    TEST_ASSERT_EQUALS(3, set0.max_size());
    TEST_ASSERT(!set0.full());
    TEST_ASSERT(hasSameSetContent(reference, set0));

    auto refInsert = reference.emplace(NEXT_POINTER);
    TEST_ASSERT(refInsert.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *refInsert.first);
    auto insert0 = set0.emplace(NEXT_POINTER);
    TEST_ASSERT(insert0.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *insert0.first);
    TEST_ASSERT(reference.find(NEXT_POINTER) != reference.end());
    TEST_ASSERT(set0.find(NEXT_POINTER) != set0.end());

    refInsert = reference.emplace(NEXT_POINTER);
    TEST_ASSERT(!refInsert.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *refInsert.first);
    insert0 = set0.emplace(NEXT_POINTER);
    TEST_ASSERT(!insert0.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *insert0.first);

    TEST_ASSERT(hasSameSetContent(reference, set0));
    TEST_ASSERT(set0.full());
    TEST_ASSERT(reference.find(NEXT_POINTER) != reference.end());
    TEST_ASSERT(set0.find(NEXT_POINTER) != set0.end());

    // test removal
    TEST_ASSERT_EQUALS(1, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(1, set0.erase(NEXT_POINTER));
    TEST_ASSERT(hasSameSetContent(reference, set0));

    // test removal of non-element
    TEST_ASSERT_EQUALS(0, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(0, set0.erase(NEXT_POINTER));

    TEST_ASSERT(hasSameSetContent(reference, set0));
    TEST_ASSERT(!set0.full());

    TEST_ASSERT(reference.emplace(NEXT_POINTER).second);
    TEST_ASSERT(set0.emplace(NEXT_POINTER).second);
    TEST_THROWS(set0.emplace(NOT_FOUND_POINTER), std::out_of_range);

    TEST_ASSERT(hasSameSetContent(reference, set0));
    TEST_ASSERT(set0.full());

    reference.erase(reference.find(NEXT_POINTER));
    set0.erase(set0.find(NEXT_POINTER));
    TEST_ASSERT(!set0.full());
}

void TestCustomContainers::testSmallSortedPointerSet()
{
    std::set<void*> reference = {SOME_POINTER, OTHER_POINTER};
    SmallSortedPointerSet<void*> set0 = {SOME_POINTER, OTHER_POINTER};

    TEST_ASSERT_EQUALS(6, set0.small_size());
    TEST_ASSERT(hasSameSetContent(reference, set0));

    reference.erase(reference.find(SOME_POINTER));
    set0.erase(set0.find(SOME_POINTER));
    TEST_ASSERT(reference.emplace(SOME_POINTER).second);
    TEST_ASSERT(set0.emplace(SOME_POINTER).second);

    auto refInsert = reference.emplace(NEXT_POINTER);
    TEST_ASSERT(refInsert.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *refInsert.first);
    auto insert0 = set0.emplace(NEXT_POINTER);
    TEST_ASSERT(insert0.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *insert0.first);

    refInsert = reference.emplace(NEXT_POINTER);
    TEST_ASSERT(!refInsert.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *refInsert.first);
    insert0 = set0.emplace(NEXT_POINTER);
    TEST_ASSERT(!insert0.second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *insert0.first);

    TEST_ASSERT(hasSameSetContent(reference, set0));

    // test removal
    TEST_ASSERT_EQUALS(1, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(1, set0.erase(NEXT_POINTER));
    TEST_ASSERT(hasSameSetContent(reference, set0));

    // test removal of non-element
    TEST_ASSERT_EQUALS(0, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(0, set0.erase(NEXT_POINTER));

    TEST_ASSERT(hasSameSetContent(reference, set0));

    TEST_ASSERT(reference.emplace(NEXT_POINTER).second);
    TEST_ASSERT(set0.emplace(NEXT_POINTER).second);
    TEST_ASSERT_EQUALS(NEXT_POINTER, *reference.find(NEXT_POINTER));
    TEST_ASSERT_EQUALS(NEXT_POINTER, *set0.find(NEXT_POINTER));

    TEST_ASSERT(hasSameSetContent(reference, set0));

    reference.erase(reference.find(NEXT_POINTER));
    set0.erase(set0.find(NEXT_POINTER));

    TEST_ASSERT(reference.emplace(NEXT_POINTER).second);
    TEST_ASSERT(set0.emplace(NEXT_POINTER).second);
    TEST_ASSERT(reference.emplace(SOME_MORE_POINTER).second);
    TEST_ASSERT(set0.emplace(SOME_MORE_POINTER).second);
    TEST_ASSERT(reference.emplace(EVEN_MORE_POINTER).second);
    TEST_ASSERT(set0.emplace(EVEN_MORE_POINTER).second);
    TEST_ASSERT(reference.emplace(AND_SOME_MORE_POINTER).second);
    TEST_ASSERT(set0.emplace(AND_SOME_MORE_POINTER).second);
    TEST_ASSERT(reference.emplace(MAYBE_LAST_POINTER).second);
    TEST_ASSERT(set0.emplace(MAYBE_LAST_POINTER).second);

    TEST_ASSERT_EQUALS(reference.size(), set0.size());
    TEST_ASSERT(hasSameSetContent(reference, set0));
    TEST_ASSERT(set0.size() > set0.small_size());

    reference.clear();
    set0.clear();
    TEST_ASSERT(reference.empty());
    TEST_ASSERT(set0.empty());

    reference.emplace(SOME_POINTER);
    set0.emplace(SOME_POINTER);

    reference.emplace(NEXT_POINTER);
    set0.emplace(NEXT_POINTER);

    reference.erase(reference.find(SOME_POINTER));
    set0.erase(set0.find(SOME_POINTER));
    reference.erase(NEXT_POINTER);
    set0.erase(NEXT_POINTER);
    TEST_ASSERT(reference.empty());
    TEST_ASSERT(set0.empty());

    TEST_ASSERT(reference.end() == reference.find(SOME_POINTER));
    TEST_ASSERT(set0.end() == set0.find(SOME_POINTER));

    reference.emplace(SOME_POINTER);
    set0.emplace(SOME_POINTER);

    TEST_ASSERT(reference.begin() == reference.find(SOME_POINTER));
    TEST_ASSERT(set0.begin() == set0.find(SOME_POINTER));
    TEST_ASSERT_EQUALS(SOME_POINTER, *reference.find(SOME_POINTER));
    TEST_ASSERT_EQUALS(SOME_POINTER, *set0.find(SOME_POINTER));

    TEST_ASSERT(hasSameSetContent(reference, set0));
}

template <typename T, typename U>
static bool hasSameMapContent(const T& first, const U& second)
{
    auto firstIt = first.begin();
    auto secondIt = second.begin();
    for(; firstIt != first.end(); ++firstIt, ++secondIt)
    {
        if(firstIt->first != secondIt->first || firstIt->second != secondIt->second)
            return false;
        if(first.at(firstIt->first) != firstIt->second)
            return false;
        if(second.at(secondIt->first) != secondIt->second)
            return false;
        if(first.find(firstIt->first) == first.end())
            return false;
        if(second.find(secondIt->first) == second.end())
            return false;
        if(first.find(firstIt->first) != firstIt)
            return false;
        if(second.find(secondIt->first) != secondIt)
            return false;
    }

    if(12 != first.find(SOME_POINTER)->second)
        return false;
    if(12 != second.find(SOME_POINTER)->second)
        return false;

    if(first.end() != first.find(NOT_FOUND_POINTER))
        return false;
    if(second.end() != second.find(NOT_FOUND_POINTER))
        return false;

    if(first.empty())
        return false;
    if(second.empty())
        return false;

    if(first.size() != second.size())
        return false;
    return true;
}

template <typename T, typename U>
static bool hasSameSetContent(const T& first, const U& second)
{
    auto firstIt = first.begin();
    auto secondIt = second.begin();
    for(; firstIt != first.end(); ++firstIt, ++secondIt)
    {
        if(*firstIt != *secondIt)
            return false;
        if(first.find(*firstIt) == first.end())
            return false;
        if(second.find(*secondIt) == second.end())
            return false;
        if(first.find(*firstIt) != firstIt)
            return false;
        if(second.find(*secondIt) != secondIt)
            return false;
    }

    if(SOME_POINTER != *first.find(SOME_POINTER))
        return false;
    if(SOME_POINTER != *second.find(SOME_POINTER))
        return false;

    if(first.end() != first.find(NOT_FOUND_POINTER))
        return false;
    if(second.end() != second.find(NOT_FOUND_POINTER))
        return false;

    if(first.empty())
        return false;
    if(second.empty())
        return false;

    if(first.size() != second.size())
        return false;
    return true;
}
