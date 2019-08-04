#ifndef VC4C_TEST_PATTERN_H
#define VC4C_TEST_PATTERN_H 1

#include "cpptest.h"

class TestPatternMatching : public Test::Suite
{
public:
    TestPatternMatching();
    ~TestPatternMatching() override;

    void testInstructionMatch();
    void testExpressionMatch();
    void testSingleSearch();
    void testConsecutiveSearch();
    void testGappedSearch();
};
#endif /* VC4C_TEST_PATTERN_H */
