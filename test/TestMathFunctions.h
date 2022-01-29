/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_STDLIB_H
#define VC4C_TEST_STDLIB_H

#include "cpptest.h"

#include "config.h"

class TestMathFunctions : public Test::Suite
{
public:
    TestMathFunctions(const vc4c::Configuration& config = {});

    void testAcos();
    void testAcosh();
    void testAcosPi();
    void testAsin();
    void testAsinh();
    void testAsinPi();
    void testAtan();
    void testAtan2();
    void testAtanh();
    void testAtanPi();
    void testAtan2Pi();
    void testCbrt();
    void testCeil();
    void testCopysign();
    void testCos();
    void testCosh();
    void testCosPi();
    void testErfc();
    void testErf();
    void testExp();
    void testExp2();
    void testExp10();
    void testExpm1();
    void testFabs();
    void testFdim();
    void testFloor();
    void testFma();
    void testFmax();
    void testFmin();
    void testFmod();
    void testFract();
    void testFrexp();
    void testHypot();
    void testIlogb();
    void testLdexp();
    void testLgamma();
    void testLgammaR();
    void testLog();
    void testLog2();
    void testLog10();
    void testLog1p();
    void testLogb();
    void testMad();
    void testMaxMag();
    void testMinMag();
    void testModf();
    void testNan();
    void testNextafter();
    void testPow();
    void testPown();
    void testPowr();
    void testRemainder();
    void testRemquo();
    void testRint();
    void testRootn();
    void testRound();
    void testRsqrt();
    void testSin();
    void testSinCos();
    void testSinh();
    void testSinPi();
    void testSqrt();
    void testTan();
    void testTanh();
    void testTanPi();
    void testTgamma();
    void testTrunc();

private:
    void onMismatch(const std::string& expected, const std::string& result);

    vc4c::Configuration config;
};

#endif /* VC4C_TEST_STDLIB_H */
