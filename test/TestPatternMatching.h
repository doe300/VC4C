#pragma once

#include "cpptest.h"

class TestPatternMatching : public Test::Suite
{
public:
    TestPatternMatching();

    void testInstructionMatch();
    void testExpressionMatch();
    void testSingleSearch();
    void testConsecutiveSearch();
    void testGappedSearch();
};