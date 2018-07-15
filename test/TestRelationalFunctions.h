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

    void testIsEqual();
    void testIsNotEqual();
    void testIsGreater();
    void testIsGreaterEqual();
    void testIsLess();
    void testIsLessEqual();
    void testIsLessGreater();
    void testIsFinite();
    void testIsInf();
    void testIsNaN();
    void testIsNormal();
    void testIsOrdered();
    void testIsUnordered();
    void testSignbit();
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

private:
    vc4c::Configuration config;

    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_RELATIONAL_FUNCTIONS_H */