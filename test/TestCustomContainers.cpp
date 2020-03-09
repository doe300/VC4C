/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestCustomContainers.h"

#include "tools/SmallMap.h"

using namespace vc4c::tools;

TestCustomContainers::TestCustomContainers()
{
    TEST_ADD(TestCustomContainers::testFixedSortedPointerMap);
    TEST_ADD(TestCustomContainers::testSmallSortedPointerMap);
}

TestCustomContainers::~TestCustomContainers() = default;

static auto SOME_POINTER = reinterpret_cast<void*>(0xFFF33F);
static auto OTHER_POINTER = reinterpret_cast<void*>(0xF22FF33F);
static auto NEXT_POINTER = reinterpret_cast<void*>(0xF22FF332);
static auto NOT_FOUND_POINTER = reinterpret_cast<void*>(0xFF1F33F);

template <typename T, typename U>
static bool hasSameContent(const T& first, const U& second);

void TestCustomContainers::testFixedSortedPointerMap()
{
    std::map<void*, unsigned> reference = {{SOME_POINTER, 12}, {OTHER_POINTER, 13}};
    FixedSortedPointerMap<void*, unsigned, 3> map0;
    for(auto i : reference)
        map0.emplace(i);

    TEST_ASSERT_EQUALS(3, map0.max_size());
    TEST_ASSERT(!map0.full());
    TEST_ASSERT(hasSameContent(reference, map0));

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

    TEST_ASSERT(hasSameContent(reference, map0));
    TEST_ASSERT(map0.full());
    TEST_ASSERT(reference.find(NEXT_POINTER) != reference.end());
    TEST_ASSERT(map0.find(NEXT_POINTER) != map0.end());

    // test removal
    TEST_ASSERT_EQUALS(1, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(1, map0.erase(NEXT_POINTER));
    TEST_ASSERT(hasSameContent(reference, map0));

    // test removal of non-element
    TEST_ASSERT_EQUALS(0, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(0, map0.erase(NEXT_POINTER));

    TEST_ASSERT(hasSameContent(reference, map0));
    TEST_ASSERT(!map0.full());

    TEST_ASSERT(reference.emplace(NEXT_POINTER, 13).second);
    TEST_ASSERT(map0.emplace(NEXT_POINTER, 13).second);
    TEST_THROWS(map0.emplace(NOT_FOUND_POINTER, 113), std::out_of_range);

    TEST_ASSERT_EQUALS(13, reference[NEXT_POINTER]);
    TEST_ASSERT_EQUALS(13, map0[NEXT_POINTER]);
    TEST_THROWS(map0[NOT_FOUND_POINTER] = 113, std::out_of_range);

    TEST_ASSERT(hasSameContent(reference, map0));
    TEST_ASSERT(map0.full());

    reference.at(NEXT_POINTER) = 15;
    map0.at(NEXT_POINTER) = 15;
    TEST_ASSERT_EQUALS(15, reference.at(NEXT_POINTER));
    TEST_ASSERT_EQUALS(15, map0.at(NEXT_POINTER));
    TEST_THROWS(reference.at(NOT_FOUND_POINTER) = 113, std::out_of_range);
    TEST_THROWS(map0.at(NOT_FOUND_POINTER) = 113, std::out_of_range);

    TEST_ASSERT(hasSameContent(reference, map0));
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

static auto SOME_MORE_POINTER = reinterpret_cast<void*>(0xF233F);
static auto ANOTHER_POINTER = reinterpret_cast<void*>(0x11FF33F);

void TestCustomContainers::testSmallSortedPointerMap()
{
    std::map<void*, unsigned> reference = {{SOME_POINTER, 12}, {OTHER_POINTER, 13}};
    SmallSortedPointerMap<void*, unsigned> map0;
    for(auto i : reference)
        map0.emplace(i);

    TEST_ASSERT_EQUALS(3, map0.small_size());
    TEST_ASSERT(hasSameContent(reference, map0));

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

    TEST_ASSERT(hasSameContent(reference, map0));

    // test removal
    TEST_ASSERT_EQUALS(1, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(1, map0.erase(NEXT_POINTER));
    TEST_ASSERT(hasSameContent(reference, map0));

    // test removal of non-element
    TEST_ASSERT_EQUALS(0, reference.erase(NEXT_POINTER));
    TEST_ASSERT_EQUALS(0, map0.erase(NEXT_POINTER));

    TEST_ASSERT(hasSameContent(reference, map0));

    TEST_ASSERT(reference.emplace(NEXT_POINTER, 13).second);
    TEST_ASSERT(map0.emplace(NEXT_POINTER, 13).second);
    TEST_ASSERT_EQUALS(13, reference[NEXT_POINTER]);
    TEST_ASSERT_EQUALS(13, map0[NEXT_POINTER]);

    TEST_ASSERT(hasSameContent(reference, map0));

    reference.at(NEXT_POINTER) = 15;
    map0.at(NEXT_POINTER) = 15;
    TEST_ASSERT_EQUALS(15, reference.at(NEXT_POINTER));
    TEST_ASSERT_EQUALS(15, map0.at(NEXT_POINTER));
    TEST_THROWS(reference.at(NOT_FOUND_POINTER) = 113, std::out_of_range);
    TEST_THROWS(map0.at(NOT_FOUND_POINTER) = 113, std::out_of_range);

    TEST_ASSERT(hasSameContent(reference, map0));

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

    TEST_ASSERT(hasSameContent(reference, map0));
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

    TEST_ASSERT(hasSameContent(reference, map0));
}

template <typename T, typename U>
static bool hasSameContent(const T& first, const U& second)
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