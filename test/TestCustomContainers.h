/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_CUSTOM_CONTAINERS_H
#define VC4C_TEST_CUSTOM_CONTAINERS_H

#include "cpptest.h"

class TestCustomContainers : public Test::Suite
{
public:
    TestCustomContainers();
    ~TestCustomContainers() override;

    void testFixedSortedPointerMap();
    void testSmallSortedPointerMap();
};

#endif /* VC4C_TEST_CUSTOM_CONTAINERS_H */
