/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_RELATIONAL_FUNCTIONS_H
#define VC4C_TEST_RELATIONAL_FUNCTIONS_H

#include "cpptest.h"

#include "config.h"

class TestRelationalFunctions : public Test::Suite
{
public:
    TestRelationalFunctions(const vc4c::Configuration& config = {});
    ~TestRelationalFunctions() override;

    void testIsEqual();
    void testIsEqualScalar();
    void testIsNotEqual();
    void testIsNotEqualScalar();
    void testIsGreater();
    void testIsGreaterScalar();
    void testIsGreaterEqual();
    void testIsGreaterEqualScalar();
    void testIsLess();
    void testIsLessScalar();
    void testIsLessEqual();
    void testIsLessEqualScalar();
    void testIsLessGreater();
    void testIsLessGreaterScalar();
    void testIsFinite();
    void testIsFiniteScalar();
    void testIsInf();
    void testIsInfScalar();
    void testIsNaN();
    void testIsNaNScalar();
    void testIsNormal();
    void testIsNormalScalar();
    void testIsOrdered();
    void testIsOrderedScalar();
    void testIsUnordered();
    void testIsUnorderedScalar();
    void testSignbit();
    void testSignbitScalar();
    void testIntAny();
    void testShortAny();
    void testCharAny();
    void testIntAll();
    void testShortAll();
    void testCharAll();
    void testBitselectInt();
    void testBitselectShort();
    void testBitselectChar();
    void testSelectInt();
    void testSelectShort();
    void testSelectChar();
    void testSelectIntScalar();
    void testSelectShortScalar();
    void testSelectCharScalar();

private:
    vc4c::Configuration config;

    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_RELATIONAL_FUNCTIONS_H */
